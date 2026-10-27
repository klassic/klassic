//! Cycle start from worker-completed Old or Young snapshots.
//!
//! Shared feedback selects the used-page threshold. The sole owner rechecks
//! fixed-count frontier metadata under the lock; it never traces merely to
//! make a completion hint true. Old's snapshot/producers remain active across
//! Young evacuation; full Old selection has its own occupancy/credit policy.
use super::{Labels, load_rt, store_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

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
    super::start_policy::below(a, skip);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.mark_cycle_poll);
    let legacy = a.create_text_label();
    load_rt(a, R::V0, super::cooperative_old::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, legacy);
    a.call_label(l.cooperative_old_poll);
    a.ret();
    a.bind_text_label(legacy);
    let Some(_) = &l.worker else {
        a.ret();
        return;
    };
    super::old_cycle::poll(a, l);
    super::young_mark::poll(a, l);
    let skip = a.create_text_label();
    let release = a.create_text_label();
    let pending = a.create_text_label();
    eligible(a, skip);
    super::worker::lock(a, l);
    eligible(a, release);
    // Previous initialization is complete at this allocation safepoint.
    a.call_label(l.publish_pending);
    super::copy::bump(a, super::RT_CYCLE_CHECKS, R::V0);
    load_rt(a, R::V0, super::RT_M_OBJ);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    super::registered_roots::pending(a, super::registered_roots::OLD, pending);
    load_rt(a, R::V0, super::mark_queue::count(false));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    load_rt(a, R::V0, super::RT_MSTACK_LEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    super::copy::bump(a, super::RT_CYCLE_READY, R::V0);
    super::worker::unlock(a, l);
    // There is one owner: no user barrier runs between this check and collect's
    // reacquisition. Young collection does not close Old marking or drain it;
    // it captures and relocates retained Old receivers along with its roots.
    a.call_label(l.collect);
    a.jmp_label(skip);
    a.bind_text_label(pending);
    super::copy::bump(a, super::RT_CYCLE_RETRIES, R::V0);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_MARK_DONE, R::V0);
    super::worker::request_work(a);
    a.bind_text_label(release);
    super::worker::unlock(a, l);
    super::worker::wake_if_pending(a, l);
    a.bind_text_label(skip);
    a.ret();
}
