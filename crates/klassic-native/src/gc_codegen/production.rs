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
mod roots;

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
const ROOT_CELLS: u64 = 1 << 16;
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
fn color() -> u64 {
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
    from_base: u64,
    from_end: u64,
    from_marks: u64,
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
    let total = round_up(roots + roots_bytes, 4096);
    Layout {
        pages,
        starts,
        tables,
        cells,
        marks,
        roots,
        roots_bytes,
        total,
    }
}

const L: Layout = layout();
const HEAP_BYTES: u64 = (PAGES + 1) * HEAP_PAGE_BYTES;
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
const RT_FROM_BASE: usize = offset_of!(Runtime, from_base);
const RT_FROM_END: usize = offset_of!(Runtime, from_end);
const RT_FROM_MARKS: usize = offset_of!(Runtime, from_marks);
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
    zero: T,
    drain: T,
    fail: T,
    oom: T,
    too_large: T,
}

impl<T: Copy> Labels<T> {
    /// `map` and `alloc` are backend labels: the mapping leaf and the
    /// existing allocation entry every site already calls.
    pub(crate) fn new(mut create: impl FnMut() -> T, map: T, alloc: T) -> Self {
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
            zero: create(),
            drain: create(),
            fail: create(),
            oom: create(),
            too_large: create(),
        }
    }
}

pub(crate) const FAIL_TEXT: &[u8] = b"klassic gc: --gc=generational runtime invariant violated\n";
pub(crate) const OOM_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational live data exceeds a young semispace)\n";
pub(crate) const TOO_LARGE_TEXT: &[u8] =
    b"klassic gc: object larger than one --gc=generational page (M-G1 limit)\n";

/// Data the routines read: the cell holding the runtime pointer and the
/// three diagnostics above.
pub(crate) struct Data<D> {
    pub runtime: D,
    pub fail_text: D,
    pub oom_text: D,
    pub too_large_text: D,
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

/// `value` (a payload address or null) -> reference bits, in place.
fn encode<E: PortableAsm>(a: &mut E, value: R, scratch: R) {
    let null = a.create_text_label();
    a.test_reg_reg(value, value);
    a.jcc_label(C::Equal, null);
    a.load_ptr_disp32(scratch, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(value, scratch);
    a.shl_reg_imm8(value, 16);
    a.mov_imm64(scratch, color());
    a.or_reg_reg(value, scratch);
    a.bind_text_label(null);
}

/// Reference bits (or null) -> payload address, in place.
fn decode<E: PortableAsm>(a: &mut E, value: R, scratch: R) {
    let null = a.create_text_label();
    a.test_reg_reg(value, value);
    a.jcc_label(C::Equal, null);
    a.shr_reg_imm8(value, 16);
    a.load_ptr_disp32(scratch, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(value, scratch);
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
    emit_startup(a, l, data.runtime);
    alloc::emit_alloc(a, l, data.runtime, stress);
    collect::emit_collect(a, l);
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

fn emit_startup<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    a.bind_text_label(l.startup);
    a.push_reg(R::V3);
    // Metadata mapping; the runtime block sits at its start.
    a.mov_imm64(R::V7, L.total);
    a.call_label(l.map);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.oom);
    a.mov_reg_reg(R::V3, R::V0);
    a.mov_data_addr(R::V1, runtime);
    a.store_ptr_disp32(R::V1, 0, R::V3);
    // Heap mapping: offset 0 is never a page, so page i starts at (i+1) pages.
    a.mov_imm64(R::V7, HEAP_BYTES);
    a.call_label(l.map);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.oom);
    a.store_ptr_disp32(R::V3, d(RT_HEAP_BASE), R::V0);
    // The single mutator: ABI version, Running (0), thread 1, initial phase.
    store_rt_imm(a, RT_CTX, u64::from(ABI_VERSION));
    store_rt_imm(a, RT_CTX + offset_of!(MutatorContext, thread_id), 1);
    // Page table: every page READY, owning its start bitmap and forward table.
    let page = |field: usize| d(field);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    lea_rt(a, R::V8, L.pages as usize);
    lea_rt(a, R::V10, L.starts as usize);
    lea_rt(a, R::V11, L.tables as usize);
    lea_rt(a, R::V2, L.cells as usize);
    a.mov_imm64(R::V6, HEAP_PAGE_BYTES);
    a.mov_imm64(R::V7, 1);
    a.mov_imm64(R::V9, 0);
    let fill = a.create_text_label();
    a.bind_text_label(fill);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, heap_base)), R::V1);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, start)), R::V6);
    a.store_ptr_disp32(
        R::V8,
        page(offset_of!(AllocationPage, allocated_top)),
        R::V6,
    );
    a.mov_reg_reg(R::V0, R::V6);
    a.add_reg_imm32(R::V0, imm(HEAP_PAGE_BYTES));
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, limit)), R::V0);
    a.mov_reg_reg(R::V0, R::V9);
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, incarnation)), R::V0);
    a.store_ptr_disp32(
        R::V8,
        page(offset_of!(AllocationPage, identity_base)),
        R::V7,
    );
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, run_head)), R::V9);
    a.mov_imm64(R::V0, PAGE_READY);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, state)), R::V0);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, starts)), R::V10);
    a.mov_imm64(R::V0, STARTS_WORDS);
    a.store_ptr_disp32(R::V8, page(offset_of!(AllocationPage, starts_words)), R::V0);
    a.store_ptr_disp32(
        R::V8,
        page(offset_of!(AllocationPage, forward_table)),
        R::V11,
    );
    a.store_ptr_disp32(R::V11, d(offset_of!(ForwardTable, cells)), R::V2);
    a.mov_imm64(R::V0, CELLS_PER_PAGE);
    a.store_ptr_disp32(R::V11, d(offset_of!(ForwardTable, capacity)), R::V0);
    a.add_reg_imm32(R::V8, d(size_of::<AllocationPage>()));
    a.add_reg_imm32(R::V10, imm(STARTS_WORDS * 8));
    a.add_reg_imm32(R::V11, d(size_of::<ForwardTable>()));
    a.add_reg_imm32(R::V2, imm(CELLS_PER_PAGE * size_of::<ForwardCell>() as u64));
    a.add_reg_imm32(R::V6, imm(HEAP_PAGE_BYTES));
    a.add_reg_imm32(R::V7, imm(IDENTITY_STRIDE));
    a.add_reg_imm32(R::V9, 1);
    a.cmp_reg_imm32(R::V9, imm(PAGES));
    a.jcc_label(C::Below, fill);
    store_rt_addr(a, RT_PAGES, L.pages as usize);
    // Mark-record arena covering every possible object.
    store_rt_addr(
        a,
        RT_MARKS + offset_of!(MarkArena, records),
        L.marks as usize,
    );
    store_rt_imm(
        a,
        RT_MARKS + offset_of!(MarkArena, capacity),
        HALF * CELLS_PER_PAGE,
    );
    store_rt_imm(a, RT_SPACE_END, HALF);
    // Builtin descriptors; raw bytes (id 1) is the only one M-G1 allocates.
    lea_rt(a, R::V7, RT_BUILTINS);
    a.call_label(l.builtin_prepare);
    // Root storage: one unregistered segment owned by thread 1, scope 1.
    store_rt_imm(a, RT_SPEC + offset_of!(RootStorageSpec, owner), 1);
    store_rt_imm(a, RT_SPEC + offset_of!(RootStorageSpec, cells), ROOT_CELLS);
    store_rt_imm(
        a,
        RT_SPEC + offset_of!(RootStorageSpec, entries),
        JOURNAL_ENTRIES,
    );
    store_rt_imm(
        a,
        RT_SPEC + offset_of!(RootStorageSpec, bytes),
        L.roots_bytes,
    );
    lea_rt(a, R::V7, L.roots as usize);
    lea_rt(a, R::V6, RT_SPEC);
    a.call_label(l.root_storage_prepare);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(
        a,
        R::V1,
        L.roots as usize + offset_of!(RootStorage, segment),
    );
    a.store_ptr_disp32(R::V3, d(RT_SEGMENT), R::V1);
    a.mov_imm64(R::V0, SEGMENT_SCOPE);
    a.store_ptr_disp32(R::V1, d(offset_of!(RootSegment, scope)), R::V0);
    a.mov_imm64(R::V0, ROOT_SEGMENT_ACTIVE);
    a.store_ptr_disp32(R::V1, d(offset_of!(RootSegment, state)), R::V0);
    a.store_ptr_disp32(
        R::V3,
        d(RT_ALLOC_ROOT + offset_of!(RootRequest, segment)),
        R::V1,
    );
    a.store_ptr_disp32(
        R::V3,
        d(RT_FRAME_ROOT + offset_of!(RootRequest, segment)),
        R::V1,
    );
    store_rt_addr(
        a,
        RT_JOURNAL,
        L.roots as usize + offset_of!(RootStorage, journal),
    );
    // Allocation requests: raw-bytes descriptor, result, mark arena.
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, descriptor),
        RT_BUILTINS + 8,
    );
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, output),
        RT_RESULT,
    );
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, marks),
        RT_MARKS,
    );
    store_rt_addr(
        a,
        RT_INTO + offset_of!(AllocationInto, allocation),
        RT_REQUEST,
    );
    store_rt_addr(a, RT_INTO + offset_of!(AllocationInto, root), RT_ALLOC_ROOT);
    // Cell 0 is the allocation scratch root; frame roots use cells 1...
    root_step(a, l, RT_ALLOC_ROOT, ROOT_RESERVE);
    a.pop_reg(R::V3);
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
