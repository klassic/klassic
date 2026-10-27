//! Planned Old evacuation from worker completion, before capacity exhaustion.
//! Feedback chooses occupancy/allocation readiness for one owner. Only
//! fixed frontier metadata is checked here. A stale hint rearms the worker.
use super::{Labels, Runtime, load_rt, store_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

/// Request kinds: 1 is a worker-ready policy hint; 2 is capacity exhaustion.
pub(super) const CAPACITY: u64 = 2;
pub(super) const REQUEST: usize = offset_of!(Runtime, old_mark.request_full);
pub(super) const CHECKS: usize = offset_of!(Runtime, old_mark.cycle_checks);
pub(super) const RETRIES: usize = offset_of!(Runtime, old_mark.cycle_retries);
pub(super) const PLANNED: usize = offset_of!(Runtime, old_mark.planned_cycles);

fn pending<E: PortableAsm>(a: &mut E, busy: E::TextLabel) {
    for field in [
        super::RT_M_OBJ,
        super::RT_MSTACK_LEN,
        super::mark_queue::count(false),
    ] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, busy);
    }
    super::registered_roots::pending(a, super::registered_roots::OLD, busy);
}
fn rearm<E: PortableAsm>(a: &mut E) {
    super::copy::bump(a, RETRIES, R::V0);
    a.mov_imm64(R::V0, 0);
    store_rt(a, REQUEST, R::V0);
    store_rt(a, super::RT_MARK_DONE, R::V0);
    super::worker::request_work(a);
}
fn eligible<E: PortableAsm>(a: &mut E, skip: E::TextLabel) {
    for flag in [
        super::RT_WORKER_ALIVE,
        super::RT_MARK_ACTIVE,
        super::RT_MARK_DONE,
    ] {
        load_rt(a, R::V0, flag);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, skip);
    }
    for flag in [super::RT_PHASE_OPEN, super::RT_RETIRE_STAGE] {
        load_rt(a, R::V0, flag);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, skip);
    }
    super::old_policy::eligible(a, skip);
}

/// Inline at allocation's cycle poll, before the Young readiness path. On
/// success returns from that entry after opening actual full relocation.
pub(super) fn poll<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(_) = &l.worker else { return };
    let skip = a.create_text_label();
    let release = a.create_text_label();
    let busy = a.create_text_label();
    eligible(a, skip);
    super::worker::lock(a, l);
    eligible(a, release);
    a.call_label(l.publish_pending);
    super::copy::bump(a, CHECKS, R::V0);
    pending(a, busy);
    super::old_policy::accepted(a);
    a.mov_imm64(R::V0, 1);
    store_rt(a, REQUEST, R::V0);
    super::worker::unlock(a, l);
    // collect waits for any Young mark outside the moving pause, and rechecks
    // this request under the lock before closing Old or changing Old addresses.
    a.call_label(l.collect);
    a.ret();
    a.bind_text_label(busy);
    rearm(a);
    a.bind_text_label(release);
    super::worker::unlock(a, l);
    super::worker::wake_if_pending(a, l);
    a.bind_text_label(skip);
}

/// Called under collect's lock after prior cleanup and any Young wait. A
/// planned cycle must not manufacture readiness by draining Old on the owner.
/// Capacity exhaustion retains its existing cooperative fallback separately.
pub(super) fn recheck<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    let busy = a.create_text_label();
    load_rt(a, R::V0, REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm8(R::V0, CAPACITY as i8);
    a.jcc_label(C::Equal, done);
    pending(a, busy);
    a.jmp_label(done);
    a.bind_text_label(busy);
    rearm(a);
    a.bind_text_label(done);
}

/// Consume only a request that actually selected a full Old move.
pub(super) fn consume<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    load_rt(a, R::V0, REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    let clear = a.create_text_label();
    a.cmp_reg_imm8(R::V0, CAPACITY as i8);
    a.jcc_label(C::Equal, clear);
    super::copy::bump(a, PLANNED, R::V0);
    a.bind_text_label(clear);
    a.mov_imm64(R::V0, 0);
    store_rt(a, REQUEST, R::V0);
    a.bind_text_label(done);
}
