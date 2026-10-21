//! M-G1 production wiring for `--gc=generational` (see
//! `docs/gc/production-integration-plan.md`).
//!
//! Thin emitted routines that compose the existing generational components
//! (root steps, TLAB allocation, source access, initialization publish,
//! builtin descriptors, root storage) into the entry points the direct
//! backend calls. M-G1 is an integration scaffold: one mutator, Young pages
//! prepared at startup, no collection. Exhausting the heap is an honest OOM.
//!
//! All state lives in one anonymous mapping whose address is kept in a
//! single data cell: the `Runtime` block at offset 0, then the page table,
//! object-start bitmaps, forward tables and cells, the mark-record arena and
//! the root storage. The heap payload is a second mapping. Internal routines
//! expect the runtime pointer in `V3` (rbx), which no component touches.
use crate::gc_abi::{
    ABI_VERSION, AllocationInto, AllocationPage, AllocationRequest, AllocationResult, ForwardCell,
    ForwardTable, GuardedMarkPrepare, HEAP_PAGE_BYTES, MarkArena, MarkRecord, MetadataLeaseRequest,
    MutatorContext, ObjectHeader, PAGE_READY, PhaseSnapshot, ROOT_SEGMENT_ACTIVE, RootCell,
    RootChange, RootHandle, RootRequest, RootSegment, RootStorage, RootStorageSpec,
    SourceAccessRequest,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

mod alloc;
mod collect;
mod enums;
mod roots;
mod startup;

/// Young pages prepared at startup (64 MiB of payload), split into two
/// semispaces of `HALF` pages: allocation fills one, a collection evacuates
/// the live objects into the other and recycles the first.
pub(crate) const PAGES: u64 = 512;
const HALF: u64 = PAGES / 2;
/// Forward cells per page: one per smallest object (32-byte header plus
/// 16-byte minimum payload), so a page can never exhaust its table first.
const CELLS_PER_PAGE: u64 = HEAP_PAGE_BYTES / 48;
/// Object-start bitmap words per page (one bit per 16-byte granule).
const STARTS_WORDS: u64 = HEAP_PAGE_BYTES / 16 / 64;
/// Distinct identity range per page (granules per page).
const IDENTITY_STRIDE: u64 = HEAP_PAGE_BYTES / 16;
/// Frame root cells (LIFO) plus the allocation scratch root.
const ROOT_CELLS: u64 = 1 << 20;
/// Root journal entries; M-G1 has no consumer and drains on FLUSH.
const JOURNAL_ENTRIES: u64 = 4096;
/// Largest payload one page can hold.
pub(crate) const MAX_PAYLOAD: u64 = HEAP_PAGE_BYTES - size_of::<ObjectHeader>() as u64;
/// Bytes the print path copies out of the heap per `write`.
pub(crate) const COPY_OUT_BYTES: u64 = 4096;
/// Synthetic owner metadata-lease token for readiness publication; it is
/// only format-checked and never transferred.
const PUBLISH_LEASE_TOKEN: u64 = (1 << 32) | 1;

const ROOT_RESERVE: u64 = 0;
const ROOT_STORE: u64 = 1;
const ROOT_READ_RAW: u64 = 3;
const ROOT_FLUSH: i32 = 3;
const ALLOC_REFILL: i32 = 1;
const ALLOC_METADATA_REFILL: i32 = 4;
const ALLOC_ROOT_FLUSH: i32 = 5;
const SOURCE_ENTER: u64 = 0;
const SOURCE_EXIT: u64 = 1;

/// Color bits of every reference while the adopted phase stays initial.
pub(super) fn color() -> u64 {
    u64::from(PhaseSnapshot::initial().color())
}

/// The M-G1 runtime block, at offset 0 of the metadata mapping.
#[repr(C)]
struct Runtime {
    ctx: MutatorContext,
    builtins: [u64; 25],
    heap_base: u64,
    pages: u64,
    next_page: u64,
    segment: u64,
    journal: u64,
    cell_sp: u64,
    alloc_size: u64,
    adopt_slot: u64,
    bind_page: u64,
    pending_ref: u64,
    pending_page: u64,
    pending_cell: u64,
    published: u64,
    objects: u64,
    verified: u64,
    pages_used: u64,
    space_base: u64,
    space_end: u64,
    collecting: u64,
    collections: u64,
    scan_index: u64,
    scan_old: u64,
    saved_size: u64,
    saved_elements: u64,
    saved_tag: u64,
    saved_descriptor: u64,
    from_base: u64,
    from_end: u64,
    from_marks: u64,
    alloc_tag: u64,
    cs_page: u64,
    cs_off: u64,
    cs_field: u64,
    cs_count: u64,
    forward_cell: u64,
    scan_new: u64,
    old_top: u64,
    old_base: u64,
    old_from: u64,
    old_from_end: u64,
    full: u64,
    full_collections: u64,
    old_scan: u64,
    promoted: u64,
    remset_len: u64,
    rs_index: u64,
    rs_write: u64,
    marks: MarkArena,
    request: AllocationRequest,
    into: AllocationInto,
    result: AllocationResult,
    alloc_root: RootRequest,
    frame_root: RootRequest,
    spec: RootStorageSpec,
    access: SourceAccessRequest,
    publish: GuardedMarkPrepare,
    /// Native bounce buffer: heap-string bytes are copied here before a
    /// blocking `write`, so no managed address reaches the kernel.
    copy_out: [u8; COPY_OUT_BYTES as usize],
}

const _: () = assert!(size_of::<u64>() * 25 == 200); // BuiltinDescriptors
const _: () = assert!(offset_of!(MutatorContext, abi_version) == 0);
const _: () = assert!(offset_of!(MutatorContext, state) == 4);
const _: () = assert!(size_of::<RootCell>() == 32);
const _: () = assert!(size_of::<AllocationPage>() == 112);

const fn round_up(value: u64, align: u64) -> u64 {
    value.div_ceil(align) * align
}

/// Byte offsets inside the metadata mapping.
struct Layout {
    pages: u64,
    starts: u64,
    tables: u64,
    cells: u64,
    marks: u64,
    roots: u64,
    roots_bytes: u64,
    remset: u64,
    total: u64,
}

const fn layout() -> Layout {
    let pages = round_up(size_of::<Runtime>() as u64, 4096);
    let starts = pages + PAGES * size_of::<AllocationPage>() as u64;
    let tables = starts + PAGES * STARTS_WORDS * 8;
    let cells = round_up(tables + PAGES * size_of::<ForwardTable>() as u64, 64);
    let marks = cells + PAGES * CELLS_PER_PAGE * size_of::<ForwardCell>() as u64;
    let roots = round_up(
        marks + PAGES * CELLS_PER_PAGE * size_of::<MarkRecord>() as u64,
        64,
    );
    let roots_bytes = size_of::<RootStorage>() as u64
        + ROOT_CELLS * size_of::<RootCell>() as u64
        + JOURNAL_ENTRIES * size_of::<RootChange>() as u64;
    let remset = round_up(roots + roots_bytes, 64);
    let total = round_up(remset + REMSET_ENTRIES * 8, 4096);
    Layout {
        pages,
        starts,
        tables,
        cells,
        marks,
        roots,
        roots_bytes,
        remset,
        total,
    }
}

const L: Layout = layout();
/// End of the Young pages (heap offset); the Old region follows.
pub(super) const YOUNG_END: u64 = (PAGES + 1) * HEAP_PAGE_BYTES;
/// Old generation: two semispaces of `OLD_HALF`. Promotion bump-allocates
/// into the current half; a full collection copies every live object into
/// the other half.
pub(super) const OLD_HALF: u64 = 64 << 20;
pub(super) const OLD_BYTES: u64 = 2 * OLD_HALF;
/// A Young collection may promote at most one Young semispace.
pub(super) const YOUNG_SEMI_BYTES: u64 = (PAGES / 2) * HEAP_PAGE_BYTES;
pub(super) const HEAP_BYTES: u64 = YOUNG_END + OLD_BYTES;
/// Remembered-set capacity: Old fields that hold Young payload addresses.
pub(super) const REMSET_ENTRIES: u64 = 1 << 20;
const _: () = assert!(L.total < i32::MAX as u64);
const _: () = assert!(L.pages >= size_of::<Runtime>() as u64);
const _: () = assert!(L.pages < L.starts && L.starts < L.tables && L.tables < L.cells);
const _: () = assert!(L.cells < L.marks && L.marks < L.roots && L.roots < L.total);
const _: () =
    assert!(L.starts.is_multiple_of(8) && L.tables.is_multiple_of(8) && L.cells.is_multiple_of(8));
const _: () =
    assert!(L.marks.is_multiple_of(8) && L.roots.is_multiple_of(8) && L.total.is_multiple_of(4096));
const _: () = assert!(HEAP_BYTES.is_multiple_of(4096));
// One forward cell per smallest possible object on a page.
const _: () = assert!(CELLS_PER_PAGE * 48 <= HEAP_PAGE_BYTES);

fn d(offset: usize) -> i32 {
    i32::try_from(offset).expect("runtime offset fits disp32")
}
fn imm(value: u64) -> i32 {
    i32::try_from(value).expect("layout constant fits imm32")
}

const RT_CTX: usize = offset_of!(Runtime, ctx);
const RT_BUILTINS: usize = offset_of!(Runtime, builtins);
const RT_HEAP_BASE: usize = offset_of!(Runtime, heap_base);
const RT_PAGES: usize = offset_of!(Runtime, pages);
const RT_NEXT_PAGE: usize = offset_of!(Runtime, next_page);
const RT_SEGMENT: usize = offset_of!(Runtime, segment);
const RT_JOURNAL: usize = offset_of!(Runtime, journal);
const RT_CELL_SP: usize = offset_of!(Runtime, cell_sp);
const RT_ALLOC_SIZE: usize = offset_of!(Runtime, alloc_size);
const RT_ADOPT_SLOT: usize = offset_of!(Runtime, adopt_slot);
const RT_BIND_PAGE: usize = offset_of!(Runtime, bind_page);
const RT_PENDING_REF: usize = offset_of!(Runtime, pending_ref);
const RT_PENDING_PAGE: usize = offset_of!(Runtime, pending_page);
const RT_PENDING_CELL: usize = offset_of!(Runtime, pending_cell);
const RT_PUBLISHED: usize = offset_of!(Runtime, published);
const RT_OBJECTS: usize = offset_of!(Runtime, objects);
const RT_VERIFIED: usize = offset_of!(Runtime, verified);
const RT_MARKS: usize = offset_of!(Runtime, marks);
const RT_PAGES_USED: usize = offset_of!(Runtime, pages_used);
const RT_SPACE_BASE: usize = offset_of!(Runtime, space_base);
const RT_SPACE_END: usize = offset_of!(Runtime, space_end);
const RT_COLLECTING: usize = offset_of!(Runtime, collecting);
const RT_COLLECTIONS: usize = offset_of!(Runtime, collections);
const RT_SCAN_INDEX: usize = offset_of!(Runtime, scan_index);
const RT_SCAN_OLD: usize = offset_of!(Runtime, scan_old);
const RT_SAVED_SIZE: usize = offset_of!(Runtime, saved_size);
const RT_SAVED_ELEMENTS: usize = offset_of!(Runtime, saved_elements);
const RT_SAVED_TAG: usize = offset_of!(Runtime, saved_tag);
const RT_SAVED_DESCRIPTOR: usize = offset_of!(Runtime, saved_descriptor);
const RT_FROM_BASE: usize = offset_of!(Runtime, from_base);
const RT_FROM_END: usize = offset_of!(Runtime, from_end);
const RT_FROM_MARKS: usize = offset_of!(Runtime, from_marks);
const RT_ALLOC_TAG: usize = offset_of!(Runtime, alloc_tag);
const RT_CS_PAGE: usize = offset_of!(Runtime, cs_page);
const RT_CS_OFF: usize = offset_of!(Runtime, cs_off);
const RT_CS_FIELD: usize = offset_of!(Runtime, cs_field);
const RT_CS_COUNT: usize = offset_of!(Runtime, cs_count);
const RT_FORWARD_CELL: usize = offset_of!(Runtime, forward_cell);
const RT_SCAN_NEW: usize = offset_of!(Runtime, scan_new);
const RT_OLD_TOP: usize = offset_of!(Runtime, old_top);
const RT_OLD_BASE: usize = offset_of!(Runtime, old_base);
const RT_OLD_FROM: usize = offset_of!(Runtime, old_from);
const RT_OLD_FROM_END: usize = offset_of!(Runtime, old_from_end);
const RT_FULL: usize = offset_of!(Runtime, full);
const RT_FULL_COLLECTIONS: usize = offset_of!(Runtime, full_collections);
const RT_OLD_SCAN: usize = offset_of!(Runtime, old_scan);
const RT_PROMOTED: usize = offset_of!(Runtime, promoted);
const RT_REMSET_LEN: usize = offset_of!(Runtime, remset_len);
const RT_RS_INDEX: usize = offset_of!(Runtime, rs_index);
const RT_RS_WRITE: usize = offset_of!(Runtime, rs_write);
const RT_REQUEST: usize = offset_of!(Runtime, request);
const RT_INTO: usize = offset_of!(Runtime, into);
const RT_RESULT: usize = offset_of!(Runtime, result);
const RT_ALLOC_ROOT: usize = offset_of!(Runtime, alloc_root);
const RT_FRAME_ROOT: usize = offset_of!(Runtime, frame_root);
const RT_SPEC: usize = offset_of!(Runtime, spec);
const RT_ACCESS: usize = offset_of!(Runtime, access);
const RT_PUBLISH: usize = offset_of!(Runtime, publish);

const HANDLE_CELL: usize = offset_of!(RootRequest, handle) + offset_of!(RootHandle, cell);
const HANDLE_GENERATION: usize =
    offset_of!(RootRequest, handle) + offset_of!(RootHandle, reuse_generation);
const HANDLE_SCOPE: usize = offset_of!(RootRequest, handle) + offset_of!(RootHandle, scope);
const REQUEST_VALUE: usize = offset_of!(RootRequest, value);
/// Every M-G1 segment uses scope 1; it is never retired.
const SEGMENT_SCOPE: u64 = 1;

/// Stats readable by the backend's `--gc-log` report, as offsets into the
/// runtime block.
pub(crate) struct StatOffsets {
    pub collections: i32,
    pub full_collections: i32,
    pub promoted: i32,
    pub allocated_bytes: i32,
    pub pages_used: i32,
    pub objects: i32,
    pub published: i32,
}

/// Offset of the copy-out buffer inside the runtime block.
pub(crate) fn copy_out_offset() -> i32 {
    d(offset_of!(Runtime, copy_out))
}

pub(crate) fn stat_offsets() -> StatOffsets {
    StatOffsets {
        collections: d(RT_COLLECTIONS),
        full_collections: d(RT_FULL_COLLECTIONS),
        promoted: d(RT_PROMOTED),
        allocated_bytes: d(RT_CTX + offset_of!(MutatorContext, allocated_bytes)),
        pages_used: d(RT_PAGES_USED),
        objects: d(RT_OBJECTS),
        published: d(RT_VERIFIED),
    }
}

/// Entry points and leaf subroutines, all created by the backend.
pub(crate) struct Labels<T> {
    /// `() -> ()`: map and prepare the runtime; preserves rbx.
    pub startup: T,
    /// Legacy `gc_alloc` ABI: V7 = payload bytes, V6 = legacy tag ->
    /// V0 = payload address. Clobbers the caller-saved registers.
    pub alloc: T,
    /// V0 = frame slot address. Roots the raw address (or null) the slot
    /// holds in a fresh LIFO cell and leaves the cell pointer in the slot.
    /// Preserves every register.
    pub root_adopt: T,
    /// Clear and pop the top LIFO cell. Preserves every register.
    pub root_pop: T,
    /// V0 = payload address (or null), V1 = frame slot address. Stores
    /// through the slot's cell. Preserves every register.
    pub root_store: T,
    /// V0 = cell pointer -> V0 = payload address (or null), via READ_RAW.
    /// Preserves every other register.
    pub root_load: T,
    /// Publish the pending object, then walk every used page and count
    /// objects / published headers into the runtime stats. Clobbers the
    /// caller-saved registers.
    pub verify: T,
    /// `(V7 = bytes) -> V0 = zeroed mapping or 0` (backend supplied).
    pub map: T,
    step: T,
    tlab_page: T,
    alloc_into: T,
    source_access: T,
    publish: T,
    builtin_prepare: T,
    root_storage_prepare: T,
    publish_pending: T,
    alloc_raw: T,
    collect: T,
    forward: T,
    remset_roots: T,
    /// Heap-field load: V0 = reference bits -> V0 = payload address.
    /// Preserves every other register.
    pub field_load: T,
    /// Heap-field store (legacy `gc_store_pointer` ABI): V7 = field
    /// address, V6 = payload address or null.
    pub field_store: T,
    /// Legacy `gc_deep_equal` ABI: V7, V6 = payload addresses -> V0 bool.
    pub deep_equal: T,
    /// V0 = address of a native cell holding a payload address (or null):
    /// evacuate its referent and rewrite the cell. Called from `literals`.
    pub forward_cell_entry: T,
    /// Backend routine visiting every collection-literal cell through
    /// `forward_cell_entry` (just `ret` when there are none).
    pub literals: T,
    zero: T,
    drain: T,
    fail: T,
    oom: T,
    too_large: T,
    old_full: T,
}

/// Labels the backend already calls, bound to M-G routines under the flag.
pub(crate) struct Backend<T> {
    pub field_store: T,
    pub deep_equal: T,
    pub literals: T,
}

impl<T: Copy> Labels<T> {
    /// `map` and `alloc` are backend labels: the mapping leaf and the
    /// existing allocation entry every site already calls.
    pub(crate) fn new(
        mut create: impl FnMut() -> T,
        map: T,
        alloc: T,
        backend: Backend<T>,
    ) -> Self {
        Self {
            startup: create(),
            alloc,
            root_adopt: create(),
            root_pop: create(),
            root_store: create(),
            root_load: create(),
            verify: create(),
            map,
            step: create(),
            tlab_page: create(),
            alloc_into: create(),
            source_access: create(),
            publish: create(),
            builtin_prepare: create(),
            root_storage_prepare: create(),
            publish_pending: create(),
            alloc_raw: create(),
            collect: create(),
            forward: create(),
            remset_roots: create(),
            field_load: create(),
            field_store: backend.field_store,
            deep_equal: backend.deep_equal,
            forward_cell_entry: create(),
            literals: backend.literals,
            zero: create(),
            drain: create(),
            fail: create(),
            oom: create(),
            too_large: create(),
            old_full: create(),
        }
    }
}

pub(crate) const FAIL_TEXT: &[u8] = b"klassic gc: --gc=generational runtime invariant violated\n";
pub(crate) const OOM_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational live data exceeds a young semispace)\n";
pub(crate) const OLD_FULL_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational live data exceeds an Old semispace)\n";
pub(crate) const TOO_LARGE_TEXT: &[u8] =
    b"klassic gc: object larger than one --gc=generational page (M-G1 limit)\n";

/// Data the routines read: the cell holding the runtime pointer and the
/// three diagnostics above.
pub(crate) struct Data<D> {
    pub runtime: D,
    pub fail_text: D,
    pub oom_text: D,
    pub too_large_text: D,
    pub old_full_text: D,
}

fn load_runtime<E: PortableAsm>(a: &mut E, runtime: E::DataLabel) {
    a.mov_data_addr(R::V3, runtime);
    a.load_ptr_disp32(R::V3, R::V3, 0);
}

fn lea_rt<E: PortableAsm>(a: &mut E, dst: R, offset: usize) {
    a.mov_reg_reg(dst, R::V3);
    a.add_reg_imm32(dst, d(offset));
}

fn store_rt_imm<E: PortableAsm>(a: &mut E, offset: usize, value: u64) {
    a.mov_imm64(R::V0, value);
    a.store_ptr_disp32(R::V3, d(offset), R::V0);
}

/// Store `V3 + field` into the runtime block at `offset`.
fn store_rt_addr<E: PortableAsm>(a: &mut E, offset: usize, field: usize) {
    lea_rt(a, R::V0, field);
    a.store_ptr_disp32(R::V3, d(offset), R::V0);
}

/// `status = step(ctx, request, op)`, draining the journal and retrying on
/// FLUSH; any other non-OK status is a fatal invariant failure.
fn root_step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, request: usize, op: u64) {
    let retry = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(retry);
    lea_rt(a, R::V7, RT_CTX);
    lea_rt(a, R::V6, request);
    a.mov_imm64(R::V2, op);
    a.call_label(l.step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm8(R::V0, ROOT_FLUSH as i8);
    a.jcc_label(C::NotEqual, l.fail);
    a.call_label(l.drain);
    a.jmp_label(retry);
    a.bind_text_label(done);
}

/// Tag bit marking an immortal pointer outside the managed heap (static
/// enum constants in native data). Heap references always have bit 0
/// clear (their metadata is a phase color), so the tag is unambiguous; such
/// values are carried verbatim and never traced or moved.
pub(super) const EXTERNAL: i32 = 1;

/// `value` (a payload address or null) -> reference bits, in place.
fn encode<E: PortableAsm>(a: &mut E, value: R, scratch: R) {
    let null = a.create_text_label();
    let external = a.create_text_label();
    a.test_reg_reg(value, value);
    a.jcc_label(C::Equal, null);
    a.load_ptr_disp32(scratch, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(value, scratch);
    a.jcc_label(C::Below, external);
    a.mov_imm64(scratch, HEAP_BYTES);
    a.cmp_reg_reg(value, scratch);
    a.jcc_label(C::AboveOrEqual, external);
    a.shl_reg_imm8(value, 16);
    a.mov_imm64(scratch, color());
    a.or_reg_reg(value, scratch);
    a.jmp_label(null);
    a.bind_text_label(external);
    a.load_ptr_disp32(scratch, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(value, scratch);
    a.mov_imm64(scratch, EXTERNAL as u64);
    a.or_reg_reg(value, scratch);
    a.bind_text_label(null);
}

/// Reference bits (or null, or an external tagged pointer) -> address.
fn decode<E: PortableAsm>(a: &mut E, value: R, scratch: R) {
    let null = a.create_text_label();
    let external = a.create_text_label();
    a.test_reg_reg(value, value);
    a.jcc_label(C::Equal, null);
    a.bt_reg_imm8(value, 0);
    a.jcc_label(C::Below, external);
    a.shr_reg_imm8(value, 16);
    a.load_ptr_disp32(scratch, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(value, scratch);
    a.jmp_label(null);
    a.bind_text_label(external);
    a.mov_imm64(scratch, !1);
    a.and_reg_reg(value, scratch);
    a.bind_text_label(null);
}

/// Emit every M-G1 routine except `map`.
pub(crate) fn emit<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    data: &Data<E::DataLabel>,
    stderr_fd: u64,
    stress: bool,
) {
    emit_leaves(a, l);
    startup::emit_startup(a, l, data.runtime);
    alloc::emit_alloc(a, l, data.runtime, stress);
    collect::emit_collect(a, l);
    enums::emit_enums(a, l, data.runtime);
    emit_publish_pending(a, l);
    roots::emit_roots(a, l, data.runtime);
    roots::emit_verify(a, l, data.runtime);
    // drain: consumed = published (the journal has no consumer in M-G1).
    a.bind_text_label(l.drain);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_JOURNAL));
    a.load_ptr_disp32(
        R::V1,
        R::V0,
        d(offset_of!(crate::gc_abi::BufferCursor, published)),
    );
    a.store_ptr_disp32(
        R::V0,
        d(offset_of!(crate::gc_abi::BufferCursor, consumed)),
        R::V1,
    );
    a.ret();
    for (label, text, len) in [
        (l.fail, data.fail_text, FAIL_TEXT.len()),
        (l.oom, data.oom_text, OOM_TEXT.len()),
        (l.too_large, data.too_large_text, TOO_LARGE_TEXT.len()),
        (l.old_full, data.old_full_text, OLD_FULL_TEXT.len()),
    ] {
        a.bind_text_label(label);
        a.plat_write_data(stderr_fd, text, len);
        a.plat_exit(1);
    }
}

fn emit_leaves<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    use crate::gc_codegen::{descriptors, heap, root_lifetime, root_storage, roots};
    a.bind_text_label(l.step);
    roots::emit_step(a);
    a.ret();
    a.bind_text_label(l.tlab_page);
    heap::emit_tlab_page_locked(a);
    a.ret();
    a.bind_text_label(l.alloc_into);
    root_lifetime::alloc_into::emit_alloc_into(a);
    a.ret();
    a.bind_text_label(l.source_access);
    root_lifetime::source_access::emit_source_access(a);
    a.ret();
    a.bind_text_label(l.publish);
    root_lifetime::initialization::emit_publish(a);
    a.ret();
    a.bind_text_label(l.builtin_prepare);
    descriptors::emit_builtin_prepare(a);
    a.ret();
    a.bind_text_label(l.root_storage_prepare);
    root_storage::emit_prepare(a);
    a.ret();
}

fn source_access<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    a.load_ptr_disp32(R::V7, R::V3, d(RT_PENDING_CELL));
    lea_rt(a, R::V6, RT_ACCESS);
    a.mov_imm64(R::V2, op);
    a.call_label(l.source_access);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}

/// ENTER the pending object's source guard, publish readiness, EXIT.
fn emit_publish_pending<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    a.bind_text_label(l.publish_pending);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PENDING_REF));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    let access = |field: usize| d(RT_ACCESS + field);
    let publish = |field: usize| d(RT_PUBLISH + field);
    let lease = |field: usize| d(RT_PUBLISH + offset_of!(GuardedMarkPrepare, lease) + field);
    lea_rt(a, R::V1, RT_CTX);
    a.store_ptr_disp32(
        R::V3,
        access(offset_of!(SourceAccessRequest, context)),
        R::V1,
    );
    a.store_ptr_disp32(
        R::V3,
        publish(offset_of!(GuardedMarkPrepare, reference)),
        R::V0,
    );
    a.shr_reg_imm8(R::V0, 16);
    a.store_ptr_disp32(
        R::V3,
        access(offset_of!(SourceAccessRequest, source_offset)),
        R::V0,
    );
    a.load_ptr_disp32(R::V1, R::V3, d(RT_PENDING_PAGE));
    a.store_ptr_disp32(R::V3, publish(offset_of!(GuardedMarkPrepare, page)), R::V1);
    a.store_ptr_disp32(R::V3, lease(offset_of!(MetadataLeaseRequest, page)), R::V1);
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(
        R::V3,
        access(offset_of!(SourceAccessRequest, incarnation)),
        R::V1,
    );
    a.store_ptr_disp32(
        R::V3,
        lease(offset_of!(MetadataLeaseRequest, incarnation)),
        R::V1,
    );
    a.mov_imm64(R::V1, PUBLISH_LEASE_TOKEN);
    a.store_ptr_disp32(R::V3, lease(offset_of!(MetadataLeaseRequest, token)), R::V1);
    lea_rt(a, R::V1, RT_ACCESS);
    a.store_ptr_disp32(R::V3, publish(offset_of!(GuardedMarkPrepare, scope)), R::V1);
    source_access(a, l, SOURCE_ENTER);
    lea_rt(a, R::V7, RT_PUBLISH);
    a.call_label(l.publish);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    source_access(a, l, SOURCE_EXIT);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_PENDING_REF), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PUBLISHED));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_PUBLISHED), R::V0);
    a.bind_text_label(done);
    a.ret();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_color_encodes_valid_references() {
        let reference = crate::gc_abi::encode_ref(HEAP_PAGE_BYTES + 32, color() as u16)
            .expect("initial-phase color is valid reference metadata");
        assert_eq!(reference.bits(), ((HEAP_PAGE_BYTES + 32) << 16) | color());
    }
}
