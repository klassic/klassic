//! Incremental metadata-slot growth. Tokens keep their index/generation identity;
//! no entry pointer is carried outside a ticket. Mutations mirror into the
//! candidate until atomic publication. OS mappings are owned and charged until
//! actual release, including retry after a failed release.
use super::*;
#[repr(C)]
pub(super) struct State {
    stage: u64,
    buffer: u64,
    capacity: u64,
    cursor: u64,
    tail: u64,
    old: u64,
    old_bytes: u64,
    error: u64,
    failures: u64,
    grown: u64,
    copied: u64,
    released: u64,
    #[cfg(test)]
    paused: u64,
}
const G: usize = BASE + offset_of!(super::State, growth);
const STAGE: usize = G + offset_of!(State, stage);
const BUFFER: usize = G + offset_of!(State, buffer);
const CAP: usize = G + offset_of!(State, capacity);
const COPY: usize = G + offset_of!(State, cursor);
const TAIL: usize = G + offset_of!(State, tail);
const OLD: usize = G + offset_of!(State, old);
const OLD_BYTES: usize = G + offset_of!(State, old_bytes);
const ERROR: usize = G + offset_of!(State, error);
const FAILURES: usize = G + offset_of!(State, failures);
const GROWN: usize = G + offset_of!(State, grown);
const COPIED: usize = G + offset_of!(State, copied);
const RELEASED: usize = G + offset_of!(State, released);
// 0 idle, 1 map requested, 2 map in flight, 3 copying old slots,
// 4 initialize free tail/publish, 5 old mapping retained, 6 release in flight.

pub(super) fn retry<E: PortableAsm>(a: &mut E, release: E::TextLabel) {
    a.push_reg(R::V0);
    load_rt(a, R::V0, STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.pop_reg(R::V0);
    a.jcc_label(C::NotEqual, release);
}
pub(super) fn after_unlock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V0);
    let done = a.create_text_label();
    lea_rt(a, R::V0, offset_of!(Runtime, worker_view));
    a.cmp_reg_reg(R::OwnerContext, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.call_label(l.lease_grow_service);
    a.bind_text_label(done);
    a.pop_reg(R::V0);
}
/// Called under the ticket only when no free slot exists. Return 2 while the
/// actual copy/OS work is pending, or 6 after a failed map / address exhaustion.
pub(super) fn request<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    step(a, l);
    let pending = a.create_text_label();
    let failed = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    // step may have just published a new free list.
    load_rt(a, R::V0, HEAD);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    load_rt(a, R::V0, ERROR);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, failed);
    load_rt(a, R::V0, SIZE);
    a.mov_imm64(R::V1, u32::MAX as u64 / 2);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, failed);
    a.shl_reg_imm8(R::V0, 1);
    store_rt(a, CAP, R::V0);
    store_rt_imm(a, STAGE, 1);
    a.bind_text_label(pending);
    worker::request_work(a);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(failed);
    store_rt_imm(a, ERROR, 0);
    a.mov_imm64(R::V0, 6);
    a.bind_text_label(done);
}
/// Preserve the entry pointer and every register. A mirrored token is only a
/// metadata copy; the published table remains the sole owner of its one lease.
pub(super) fn mirror<E: PortableAsm>(a: &mut E) {
    for r in [R::V0, R::V1, R::V9, R::V10] {
        a.push_reg(r);
    }
    let done = a.create_text_label();
    load_rt(a, R::V0, STAGE);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Below, done);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Above, done);
    load_rt(a, R::V9, ENTRIES);
    a.mov_reg_reg(R::V1, R::V8);
    a.sub_reg_reg(R::V1, R::V9);
    load_rt(a, R::V10, BUFFER);
    a.add_reg_reg(R::V10, R::V1);
    for off in (0..size_of::<MetadataLeaseSlot>()).step_by(8) {
        a.load_ptr_disp32(R::V0, R::V8, d(off));
        a.store_ptr_disp32(R::V10, d(off), R::V0);
    }
    a.bind_text_label(done);
    for r in [R::V10, R::V9, R::V1, R::V0] {
        a.pop_reg(r);
    }
}
/// V8=index -> byte offset V1, preserving V8.
fn offset<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V1, R::V8);
    a.shl_reg_imm8(R::V1, 5);
}
/// One old entry copy or one free-tail entry per ticket hold, plus constant
/// publication metadata. The candidate is pre-touched outside the ticket.
pub(super) fn step<E: PortableAsm>(a: &mut E, _l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    let done = a.create_text_label();
    let tail = a.create_text_label();
    let commit = a.create_text_label();
    #[cfg(test)]
    {
        load_rt(a, R::V0, G + offset_of!(State, paused));
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    load_rt(a, R::V0, STAGE);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Equal, tail);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::NotEqual, done);
    load_rt(a, R::V8, COPY);
    load_rt(a, R::V0, SIZE);
    a.cmp_reg_reg(R::V8, R::V0);
    let copying = a.create_text_label();
    a.jcc_label(C::Below, copying);
    store_rt_imm(a, STAGE, 4);
    a.jmp_label(tail);
    a.bind_text_label(copying);
    offset(a);
    load_rt(a, R::V9, ENTRIES);
    a.add_reg_reg(R::V9, R::V1);
    load_rt(a, R::V10, BUFFER);
    a.add_reg_reg(R::V10, R::V1);
    for off in (0..size_of::<MetadataLeaseSlot>()).step_by(8) {
        a.load_ptr_disp32(R::V0, R::V9, d(off));
        a.store_ptr_disp32(R::V10, d(off), R::V0);
    }
    copy::bump(a, COPY, R::V0);
    copy::bump(a, COPIED, R::V0);
    a.jmp_label(done);
    a.bind_text_label(tail);
    load_rt(a, R::V8, TAIL);
    load_rt(a, R::V0, CAP);
    a.cmp_reg_reg(R::V8, R::V0);
    a.jcc_label(C::AboveOrEqual, commit);
    offset(a);
    load_rt(a, R::V10, BUFFER);
    a.add_reg_reg(R::V10, R::V1);
    a.mov_reg_reg(R::V0, R::V8);
    a.add_reg_imm32(R::V0, 2);
    load_rt(a, R::V1, CAP);
    a.cmp_reg_reg(R::V0, R::V1);
    let chained = a.create_text_label();
    a.jcc_label(C::Below, chained);
    a.jcc_label(C::Equal, chained);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(chained);
    a.store_ptr_disp32(R::V10, d(offset_of!(MetadataLeaseSlot, next_free)), R::V0);
    copy::bump(a, TAIL, R::V0);
    a.jmp_label(done);
    a.bind_text_label(commit);
    // Every slot borrower is ticket-local. Outstanding leases own tokens,
    // not slot addresses, so movement/retirement do not pin this mapping.
    load_rt(a, R::V8, CAP);
    a.sub_reg_imm8(R::V8, 1);
    offset(a);
    load_rt(a, R::V10, BUFFER);
    a.add_reg_reg(R::V10, R::V1);
    load_rt(a, R::V0, HEAD);
    a.store_ptr_disp32(R::V10, d(offset_of!(MetadataLeaseSlot, next_free)), R::V0);
    load_rt(a, R::V0, SIZE);
    a.mov_reg_reg(R::V8, R::V0);
    offset(a);
    store_rt(a, OLD_BYTES, R::V1);
    load_rt(a, R::V0, ENTRIES);
    store_rt(a, OLD, R::V0);
    load_rt(a, R::V0, SIZE);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, HEAD, R::V0);
    load_rt(a, R::V0, BUFFER);
    store_rt(a, ENTRIES, R::V0);
    load_rt(a, R::V0, CAP);
    store_rt(a, SIZE, R::V0);
    store_rt_imm(a, BUFFER, 0);
    load_rt(a, R::V0, OLD);
    lea_rt(a, R::V1, layout(a).metadata_lease_slots as usize);
    a.cmp_reg_reg(R::V0, R::V1);
    let retire = a.create_text_label();
    a.jcc_label(C::NotEqual, retire);
    store_rt_imm(a, OLD, 0);
    store_rt_imm(a, OLD_BYTES, 0);
    store_rt_imm(a, STAGE, 0);
    let published = a.create_text_label();
    a.jmp_label(published);
    a.bind_text_label(retire);
    store_rt_imm(a, STAGE, 5);
    a.bind_text_label(published);
    copy::bump(a, GROWN, R::V0);
    worker::request_work(a);
    a.bind_text_label(done);
    quiescence::restore(a);
}
fn enter_native<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let worker = a.create_text_label();
    lea_rt(a, R::V0, offset_of!(Runtime, worker_view));
    a.cmp_reg_reg(R::OwnerContext, R::V0);
    a.jcc_label(C::Equal, worker);
    a.call_label(l.native_enter_actor);
    a.bind_text_label(worker);
}
fn exit_native<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let worker = a.create_text_label();
    lea_rt(a, R::V0, offset_of!(Runtime, worker_view));
    a.cmp_reg_reg(R::OwnerContext, R::V0);
    a.jcc_label(C::Equal, worker);
    a.call_label(l.native_exit_actor);
    a.bind_text_label(worker);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.lease_grow_service);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    let quick_done = a.create_text_label();
    let work = a.create_text_label();
    // A nonbinding hint avoids a second collector ticket on ordinary weak
    // operations. Actual ownership and every parameter are rechecked locked.
    lea_rt(a, R::V1, STAGE);
    a.atomic_load_acquire(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, work);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::NotEqual, quick_done);
    a.bind_text_label(work);
    quiescence::save(a);
    worker::lock_raw(a, l);
    let done = a.create_text_label();
    let release = a.create_text_label();
    let leave_native = a.create_text_label();
    load_rt(a, R::V0, STAGE);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::Equal, release);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, done);
    store_rt_imm(a, STAGE, 2);
    load_rt(a, R::V8, CAP);
    offset(a);
    a.mov_reg_reg(R::V7, R::V1);
    worker::unlock_raw(a, l);
    enter_native(a, l);
    a.call_label(l.metadata_map);
    let map_failed = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, map_failed);
    a.mov_reg_reg(R::V10, R::V0);
    // Fault/commit each native page outside the collector ticket. OS mappings
    // are prezeroed, so these writes do not initialize unpublished GC values.
    a.mov_reg_reg(R::V9, R::V0);
    load_rt(a, R::V8, CAP);
    offset(a);
    a.add_reg_reg(R::V1, R::V10);
    a.mov_imm64(R::V0, 0);
    let touch = a.create_text_label();
    a.bind_text_label(touch);
    a.store_ptr_disp32(R::V9, 0, R::V0);
    a.add_reg_imm32(R::V9, 4096);
    a.cmp_reg_reg(R::V9, R::V1);
    a.jcc_label(C::Below, touch);
    worker::lock_raw(a, l);
    store_rt(a, BUFFER, R::V10);
    store_rt_imm(a, COPY, 0);
    load_rt(a, R::V0, SIZE);
    store_rt(a, TAIL, R::V0);
    store_rt_imm(a, STAGE, 3);
    worker::request_work(a);
    worker::unlock_raw(a, l);
    a.jmp_label(leave_native);
    a.bind_text_label(map_failed);
    worker::lock_raw(a, l);
    store_rt_imm(a, ERROR, 6);
    store_rt_imm(a, STAGE, 0);
    copy::bump(a, FAILURES, R::V0);
    worker::unlock_raw(a, l);
    a.jmp_label(leave_native);
    a.bind_text_label(release);
    store_rt_imm(a, STAGE, 6);
    load_rt(a, R::V7, OLD);
    load_rt(a, R::V6, OLD_BYTES);
    worker::unlock_raw(a, l);
    enter_native(a, l);
    a.call_label(l.metadata_native_release);
    worker::lock_raw(a, l);
    let retained = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, retained);
    store_rt_imm(a, OLD, 0);
    store_rt_imm(a, OLD_BYTES, 0);
    store_rt_imm(a, STAGE, 0);
    copy::bump(a, RELEASED, R::V0);
    let released = a.create_text_label();
    a.jmp_label(released);
    a.bind_text_label(retained);
    store_rt_imm(a, STAGE, 5);
    copy::bump(a, FAILURES, R::V0);
    worker::request_work(a);
    a.bind_text_label(released);
    worker::unlock_raw(a, l);
    a.bind_text_label(leave_native);
    exit_native(a, l);
    quiescence::restore(a);
    a.jmp_label(quick_done);
    a.bind_text_label(done);
    worker::unlock_raw(a, l);
    quiescence::restore(a);
    a.bind_text_label(quick_done);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.ret();
}

#[cfg(test)]
pub(super) fn metric_offsets() -> [usize; 12] {
    [
        SIZE, STAGE, COPY, TAIL, FAILURES, GROWN, COPIED, RELEASED, OLD, BUFFER, ERROR, ENTRIES,
    ]
}
#[cfg(test)]
pub(super) fn pause<E: PortableAsm>(a: &mut E, paused: bool) {
    store_rt_imm(a, G + offset_of!(State, paused), u64::from(paused));
}
