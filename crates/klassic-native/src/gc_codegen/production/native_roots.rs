//! Independently owned strong native roots. The caller's frame may disappear
//! after publication. Storage is registered against the actual creator context
//! and participates in the ordinary root/journal/relocation inventories.
//! Handles are unique, owner-affine, serial checked before backing is touched.
use super::*;
use crate::gc_abi::{BufferCursor, RootLease, RootLifeRequest};

const CAPACITY: usize = 128;
#[repr(C)]
struct Entry {
    backing: u64,
    serial: u64,
    holder: u64,
    context: u64,
    stage: u64,
}
#[repr(C)]
pub(super) struct State {
    serial: u64,
    active: u64,
    entries: [Entry; CAPACITY],
}
#[repr(C)]
pub(super) struct Owner {
    caller: u64,
    output: u64,
    value: u64,
    entry: u64,
    backing: u64,
    root: RootRequest,
}
#[repr(C)]
struct Backing {
    spec: RootStorageSpec,
    life: RootLifeRequest,
    lease: RootLease,
    storage: RootStorage,
    cell: crate::gc_abi::RootCell,
    changes: [RootChange; 4],
}
const BASE: usize = offset_of!(Runtime, native_roots);
const SERIAL: usize = BASE + offset_of!(State, serial);
pub(super) const ACTIVE: usize = BASE + offset_of!(State, active);
const ENTRIES: usize = BASE + offset_of!(State, entries);
const OWN: usize = owner_state::offset(offset_of!(OwnerState, native_roots));
const CALLER: usize = OWN + offset_of!(Owner, caller);
const OUTPUT: usize = OWN + offset_of!(Owner, output);
const VALUE: usize = OWN + offset_of!(Owner, value);
const ENTRY: usize = OWN + offset_of!(Owner, entry);
const BACKING: usize = OWN + offset_of!(Owner, backing);
const ROOT: usize = OWN + offset_of!(Owner, root);
const STORAGE: usize = offset_of!(Backing, storage);
const LIFE: usize = offset_of!(Backing, life);
const LEASE: usize = offset_of!(Backing, lease);

fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, label: E::TextLabel) {
    a.bind_text_label(label);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    load_rt(a, R::V1, CALLER);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, CALLER, R::V7);
    store_rt(a, OUTPUT, R::V6);
    a.push_reg(R::V0);
    a.call_label(l.publish_pending);
    quiescence::poll(a, l);
    a.pop_reg(R::V0);
}
fn finish<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V1, 0);
    for off in (0..size_of::<Owner>()).step_by(8) {
        store_rt(a, OWN + off, R::V1);
    }
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}
fn backing<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V8, BACKING);
}
fn put<E: PortableAsm>(a: &mut E, field: usize, value: R) {
    backing(a);
    a.store_ptr_disp32(R::V8, d(field), value);
}
fn life<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    load_rt(a, R::V6, BACKING);
    a.add_reg_imm32(R::V6, d(LIFE));
    lea_rt(a, R::V7, registered_roots::REGISTRY);
    a.mov_imm64(R::V2, op);
    a.call_label(l.root_life_step);
}
fn require_ok<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
/// Only table metadata is inspected until every stale/copied/foreign check
/// succeeds; an unmapped former backing is never dereferenced by stale input.
fn lookup<E: PortableAsm>(a: &mut E, invalid: E::TextLabel) {
    load_rt(a, R::V7, CALLER);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    lea_rt(a, R::V1, ENTRIES);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V7, 8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_imm32(R::V0, CAPACITY as i32);
    a.jcc_label(C::Above, invalid);
    a.sub_reg_imm8(R::V0, 1);
    const _: () = assert!(size_of::<Entry>() == 40);
    a.mov_reg_reg(R::V2, R::V0);
    a.shl_reg_imm8(R::V0, 5);
    a.shl_reg_imm8(R::V2, 3);
    a.add_reg_reg(R::V0, R::V2);
    a.add_reg_reg(R::V1, R::V0);
    a.load_ptr_disp32(R::V0, R::V1, d(offset_of!(Entry, holder)));
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V7, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V2, R::V1, d(offset_of!(Entry, serial)));
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::NotEqual, invalid);
    lea_rt(a, R::V0, RT_CTX);
    a.load_ptr_disp32(R::V2, R::V1, d(offset_of!(Entry, context)));
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::NotEqual, invalid);
    store_rt(a, ENTRY, R::V1);
    a.load_ptr_disp32(R::V0, R::V1, d(offset_of!(Entry, backing)));
    store_rt(a, BACKING, R::V0);
}
fn stage<E: PortableAsm>(a: &mut E, value: u64) {
    load_rt(a, R::V8, ENTRY);
    a.mov_imm64(R::V0, value);
    a.store_ptr_disp32(R::V8, d(offset_of!(Entry, stage)), R::V0);
}
fn request<E: PortableAsm>(a: &mut E) {
    backing(a);
    a.add_reg_imm32(R::V8, d(STORAGE + offset_of!(RootStorage, segment)));
    store_rt(a, ROOT + offset_of!(RootRequest, segment), R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootSegment, scope)));
    store_rt(a, ROOT + HANDLE_SCOPE, R::V0);
    a.load_ptr_disp32(R::V8, R::V8, d(offset_of!(RootSegment, cells)));
    store_rt(a, ROOT + HANDLE_CELL, R::V8);
    a.load_ptr_disp32(
        R::V0,
        R::V8,
        d(offset_of!(crate::gc_abi::RootCell, reuse_generation)),
    );
    store_rt(a, ROOT + HANDLE_GENERATION, R::V0);
}
fn create<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.native_root_new_actor); // V0=frame RootCell, V7=empty 24-byte handle
    a.call_label(l.root_load_actor);
    encode(a, R::V0, R::V1);
    store_rt(a, VALUE, R::V0);
    let invalid = a.create_text_label();
    let retry = a.create_text_label();
    let resource = a.create_text_label();
    let dispose = a.create_text_label();
    let exhausted_locked = a.create_text_label();
    let done = a.create_text_label();
    // Validate before OS work. No safepoint occurs between resolving the
    // source and admission; the actual Running owner retains its phase.
    load_rt(a, R::V7, CALLER);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    for off in [0, 8, 16] {
        a.load_ptr_disp32(R::V1, R::V7, off);
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.mov_imm64(R::V7, size_of::<Backing>() as u64);
    a.call_label(l.metadata_map);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, resource);
    store_rt(a, BACKING, R::V0);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    put(
        a,
        offset_of!(Backing, spec) + offset_of!(RootStorageSpec, owner),
        R::V0,
    );
    for (off, value) in [
        (offset_of!(RootStorageSpec, cells), 1),
        (offset_of!(RootStorageSpec, entries), 4),
        (
            offset_of!(RootStorageSpec, bytes),
            (size_of::<Backing>() - STORAGE) as u64,
        ),
    ] {
        a.mov_imm64(R::V0, value);
        put(a, offset_of!(Backing, spec) + off, R::V0);
    }
    load_rt(a, R::V6, BACKING);
    a.mov_reg_reg(R::V7, R::V6);
    a.add_reg_imm32(R::V7, d(STORAGE));
    a.call_label(l.root_storage_prepare);
    require_ok(a, l);
    worker::lock(a, l);
    for field in [RT_PHASE_OPEN, RT_RETIRE_STAGE] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, retry);
    }
    load_rt(a, R::V0, SERIAL);
    a.mov_imm64(R::V1, u64::MAX);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, exhausted_locked);
    lea_rt(a, R::V8, ENTRIES);
    a.mov_imm64(R::V6, 1);
    let find = a.create_text_label();
    let found = a.create_text_label();
    a.bind_text_label(find);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Entry, backing)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, found);
    a.add_reg_imm32(R::V8, d(size_of::<Entry>()));
    a.add_reg_imm32(R::V6, 1);
    a.cmp_reg_imm32(R::V6, CAPACITY as i32 + 1);
    a.jcc_label(C::Below, find);
    a.mov_imm64(R::V0, 6);
    a.jmp_label(dispose);
    a.bind_text_label(found);
    store_rt(a, ENTRY, R::V8);
    // Reserve the slot under the same ticket as real region registration.
    load_rt(a, R::V0, BACKING);
    a.store_ptr_disp32(R::V8, d(offset_of!(Entry, backing)), R::V0);
    a.push_reg(R::V6);
    a.push_reg(R::V6);
    a.add_reg_imm32(R::V0, d(STORAGE));
    put(a, LIFE + offset_of!(RootLifeRequest, registration), R::V0);
    lea_rt(a, R::V0, RT_CTX);
    put(a, LIFE + offset_of!(RootLifeRequest, context), R::V0);
    load_rt(a, R::V0, BACKING);
    a.add_reg_imm32(R::V0, d(LEASE));
    put(a, LIFE + offset_of!(RootLifeRequest, lease), R::V0);
    life(a, l, 0);
    require_ok(a, l);
    copy::bump(a, SERIAL, R::V0);
    load_rt(a, R::V8, ENTRY);
    load_rt(a, R::V0, SERIAL);
    a.store_ptr_disp32(R::V8, d(offset_of!(Entry, serial)), R::V0);
    load_rt(a, R::V7, CALLER);
    a.store_ptr_disp32(R::V8, d(offset_of!(Entry, holder)), R::V7);
    a.store_ptr_disp32(R::V7, 16, R::V0);
    lea_rt(a, R::V0, RT_CTX);
    a.store_ptr_disp32(R::V8, d(offset_of!(Entry, context)), R::V0);
    lea_rt(a, R::V0, ENTRIES);
    a.store_ptr_disp32(R::V7, 0, R::V0);
    a.pop_reg(R::V6);
    a.pop_reg(R::V6);
    a.store_ptr_disp32(R::V7, 8, R::V6);
    stage(a, 1);
    copy::bump(a, ACTIVE, R::V0);
    copy::bump(
        a,
        owner_state::offset(offset_of!(OwnerState, native_handles)),
        R::V0,
    );
    worker::unlock(a, l);
    backing(a);
    a.add_reg_imm32(R::V8, d(STORAGE + offset_of!(RootStorage, segment)));
    store_rt(a, ROOT + offset_of!(RootRequest, segment), R::V8);
    root_step(a, l, ROOT, ROOT_RESERVE);
    load_rt(a, R::V0, VALUE);
    store_rt(a, ROOT + REQUEST_VALUE, R::V0);
    root_step(a, l, ROOT, ROOT_STORE);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(exhausted_locked);
    a.mov_imm64(R::V0, 6);
    a.jmp_label(dispose);
    a.bind_text_label(retry);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(dispose);
    worker::unlock(a, l);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    load_rt(a, R::V7, BACKING);
    a.mov_imm64(R::V6, size_of::<Backing>() as u64);
    a.call_label(l.metadata_native_release);
    require_ok(a, l);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.jmp_label(done);
    a.bind_text_label(resource);
    a.mov_imm64(R::V0, 6);
    a.bind_text_label(done);
    finish(a);
}

fn load<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.native_root_load_actor); // V7=handle, V6=empty own frame RootCell
    let invalid_locked = a.create_text_label();
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    worker::lock(a, l);
    lookup(a, invalid_locked);
    load_rt(a, R::V8, ENTRY);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Entry, stage)));
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid_locked);
    // Authenticate the output before any handle construction or mutation.
    load_rt(a, R::V8, OUTPUT);
    load_rt(a, R::V2, RT_SEGMENT);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(RootSegment, cells)));
    a.mov_reg_reg(R::V0, R::V8);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid_locked);
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, 31);
    a.jcc_label(C::NotEqual, invalid_locked);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(RootSegment, used)));
    a.shl_reg_imm8(R::V1, 5);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, invalid_locked);
    roots::frame_handle(a, R::V8, R::V2);
    worker::unlock(a, l);
    root_step(a, l, RT_FRAME_ROOT, ROOT_READ_RAW);
    load_rt(a, R::V0, RT_FRAME_ROOT + REQUEST_VALUE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    request(a);
    root_step(a, l, ROOT, ROOT_READ_RAW);
    load_rt(a, R::V0, ROOT + REQUEST_VALUE);
    load_rt(a, R::V10, ROOT + HANDLE_CELL);
    a.call_label(l.field_load_actor);
    encode(a, R::V0, R::V1);
    store_rt(a, RT_FRAME_ROOT + REQUEST_VALUE, R::V0);
    root_step(a, l, RT_FRAME_ROOT, ROOT_STORE);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(invalid_locked);
    worker::unlock(a, l);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    finish(a);
}
fn release<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.native_root_release_actor);
    let invalid = a.create_text_label();
    let pending = a.create_text_label();
    let drain = a.create_text_label();
    let retired = a.create_text_label();
    let cleaner = a.create_text_label();
    let unmap = a.create_text_label();
    let failed = a.create_text_label();
    let done = a.create_text_label();
    let unlocked_done = a.create_text_label();
    worker::lock(a, l);
    lookup(a, invalid);
    load_rt(a, R::V8, ENTRY);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Entry, stage)));
    for (value, target) in [(2, drain), (3, retired), (4, cleaner), (5, unmap)] {
        a.cmp_reg_imm8(R::V0, value);
        a.jcc_label(C::Equal, target);
    }
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    worker::unlock(a, l);
    request(a);
    store_rt_imm(a, ROOT + REQUEST_VALUE, 0);
    // Actual SATB/admission protects the overwritten value before clearing.
    root_step(a, l, ROOT, ROOT_STORE);
    worker::lock(a, l);
    stage(a, 2);
    a.bind_text_label(drain);
    backing(a);
    a.mov_reg_reg(R::V0, R::V8);
    a.add_reg_imm32(R::V0, d(STORAGE));
    a.call_label(l.root_journal_drain_region);
    backing(a);
    a.load_ptr_disp32(
        R::V0,
        R::V8,
        d(STORAGE + offset_of!(RootStorage, journal) + offset_of!(BufferCursor, published)),
    );
    a.load_ptr_disp32(
        R::V1,
        R::V8,
        d(STORAGE + offset_of!(RootStorage, journal) + offset_of!(BufferCursor, consumed)),
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, pending);
    life(a, l, 3);
    require_ok(a, l);
    stage(a, 3);
    a.bind_text_label(retired);
    life(a, l, 4);
    require_ok(a, l);
    // This one-cell region was cleared through real tracked publication and
    // its journal acknowledged. Inspect the actual deletion range: null has
    // no graph work to enqueue. Only then acknowledge its real claim.
    backing(a);
    a.add_reg_imm32(R::V8, d(offset_of!(Backing, cell)));
    a.atomic_load_acquire(R::V0, R::V8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    backing(a);
    a.load_ptr_disp32(R::V0, R::V8, d(LIFE + offset_of!(RootLifeRequest, begin)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V8, d(LIFE + offset_of!(RootLifeRequest, end)));
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.store_ptr_disp32(R::V8, d(LEASE + offset_of!(RootLease, cursor)), R::V0);
    life(a, l, 5);
    require_ok(a, l);
    stage(a, 4);
    a.bind_text_label(cleaner);
    life(a, l, 6);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, pending);
    require_ok(a, l);
    load_rt(a, R::V7, BACKING);
    a.add_reg_imm32(R::V7, d(LIFE));
    a.call_label(l.root_clear_chunk);
    require_ok(a, l);
    life(a, l, 7);
    require_ok(a, l);
    stage(a, 5);
    a.bind_text_label(unmap);
    worker::unlock(a, l);
    load_rt(a, R::V7, BACKING);
    a.mov_imm64(R::V6, size_of::<Backing>() as u64);
    a.call_label(l.metadata_native_release);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, failed);
    // The retired registration, all readers and its journal are gone before
    // OS release. Recycling only the table entry cannot revive old serials.
    worker::lock(a, l);
    load_rt(a, R::V8, ENTRY);
    a.mov_imm64(R::V0, 0);
    for off in (0..size_of::<Entry>()).step_by(8) {
        a.store_ptr_disp32(R::V8, d(off), R::V0);
    }
    load_rt(a, R::V7, CALLER);
    for off in [0, 8, 16] {
        a.store_ptr_disp32(R::V7, off, R::V0);
    }
    load_rt(a, R::V0, ACTIVE);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, ACTIVE, R::V0);
    let handles = owner_state::offset(offset_of!(OwnerState, native_handles));
    load_rt(a, R::V0, handles);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, handles, R::V0);
    worker::request_work(a);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.jmp_label(unlocked_done);
    a.bind_text_label(failed);
    a.mov_imm64(R::V0, 6); // Still owns unmapped-pending storage; retryable.
    a.bind_text_label(unlocked_done);
    finish(a);
}
/// Internal scoped-pin adapter: authenticate the actual unique registration;
/// resolve retains its own independent root, and does not poll after the raw
/// result. These private entries do not expose an unrooted public load API.
fn internal<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, resolve: bool) {
    begin(
        a,
        l,
        if resolve {
            l.native_root_resolve
        } else {
            l.native_root_validate
        },
    );
    worker::lock(a, l);
    let invalid = a.create_text_label();
    let finish_label = a.create_text_label();
    lookup(a, invalid);
    if resolve {
        load_rt(a, R::V8, ENTRY);
        a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Entry, stage)));
        a.cmp_reg_imm8(R::V0, 1);
        a.jcc_label(C::NotEqual, invalid);
    }
    worker::unlock(a, l);
    if resolve {
        request(a);
        root_step(a, l, ROOT, ROOT_READ_RAW);
        load_rt(a, R::V0, ROOT + REQUEST_VALUE);
        load_rt(a, R::V10, ROOT + HANDLE_CELL);
        a.call_label(l.field_load_actor);
    } else {
        a.mov_imm64(R::V0, 0);
    }
    a.mov_imm64(R::V2, 0);
    a.jmp_label(finish_label);
    a.bind_text_label(invalid);
    worker::unlock(a, l);
    a.mov_imm64(R::V0, 0);
    a.mov_imm64(R::V2, 3);
    a.bind_text_label(finish_label);
    finish(a);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    create(a, l);
    internal(a, l, false);
    internal(a, l, true);
    load(a, l);
    release(a, l);
}

#[cfg(test)]
pub(crate) fn reader_probe(
    g: &mut crate::NativeCodeGenerator,
    handle: crate::DataLabel,
    request: Option<crate::DataLabel>,
) -> crate::DataLabel {
    use crate::gc_abi::RootRegistration;
    let operation = if request.is_some() { 2 } else { 1 };
    let rt = g.gc_gen.take().unwrap();
    let request = if let Some(request) = request {
        request
    } else {
        g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
        let request = g
            .asm
            .data_label_with_i64s(&vec![0; size_of::<RootLifeRequest>() / 8]);
        let lease = g
            .asm
            .data_label_with_i64s(&vec![0; size_of::<RootLease>() / 8]);
        g.mov_data_addr(R::V6, request);
        g.mov_data_addr(R::V0, lease);
        g.store_ptr_disp32(R::V6, d(offset_of!(RootLifeRequest, lease)), R::V0);
        g.mov_data_addr(R::V7, handle);
        g.load_ptr_disp32(R::V0, R::V7, 8);
        g.sub_reg_imm8(R::V0, 1);
        g.mov_reg_reg(R::V1, R::V0);
        g.shl_reg_imm8(R::V0, 5);
        g.shl_reg_imm8(R::V1, 3);
        g.add_reg_reg(R::V0, R::V1);
        g.load_ptr_disp32(R::V1, R::V7, 0);
        g.add_reg_reg(R::V0, R::V1);
        g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(Entry, backing)));
        g.add_reg_imm32(R::V0, d(STORAGE));
        g.store_ptr_disp32(R::V6, d(offset_of!(RootLifeRequest, registration)), R::V0);
        g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, scope)));
        g.store_ptr_disp32(R::V6, d(offset_of!(RootLifeRequest, scope)), R::V0);
        request
    };
    worker::lock(g, &rt.labels);
    g.mov_data_addr(R::V6, request);
    lea_rt(g, R::V7, registered_roots::REGISTRY);
    g.mov_imm64(R::V2, operation);
    g.call_label(rt.labels.root_life_step);
    require_ok(g, &rt.labels);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    request
}
