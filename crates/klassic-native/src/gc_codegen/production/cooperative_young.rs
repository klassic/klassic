//! Ordinary allocation/poll demand for authenticated fixed-bank Young rounds.
//! Independent allocation credit prevents occupied pages replaying empty cycles.
//! A request carries the actually observed Young epoch across ticket release;
//! only a real quorum-ready start consumes its window. No moving permission.
use super::*;

#[repr(C)]
pub(super) struct State {
    allocation_base: u64,
    start_requests: u64,
    end_requests: u64,
    polls: u64,
}
const BASE: usize = offset_of!(Runtime, cooperative_young);
pub(super) const ALLOCATION_BASE: usize = BASE + offset_of!(State, allocation_base);
pub(super) const START_REQUESTS: usize = BASE + offset_of!(State, start_requests);
pub(super) const END_REQUESTS: usize = BASE + offset_of!(State, end_requests);
pub(super) const POLLS: usize = BASE + offset_of!(State, polls);

pub(super) fn begin_cycle<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, old_policy::ALLOCATED);
    store_rt(a, ALLOCATION_BASE, R::V0);
}

/// Ticket held. The shared feedback threshold and actual fresh bytes both
/// participate; neither a completion hint nor a request spends this credit.
pub(super) fn not_due<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, skip: E::TextLabel) {
    start_policy::below(a, skip);
    load_rt(a, R::V0, old_policy::ALLOCATED);
    load_rt(a, R::V1, ALLOCATION_BASE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, start_policy::TARGET);
    a.shl_reg_imm8(R::V1, 17);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, skip);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.cooperative_young_poll);
    quiescence::save(a);
    let done = a.create_text_label();
    load_rt(a, R::V0, cooperative_old::YOUNG_ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    #[cfg(test)]
    {
        // Existing explicit-round fixtures suppress automatic Young demand.
        // Production has no suppression field or branch.
        load_rt(a, R::V0, young_mark::AUTO_DISABLED);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    let normal_physical = a.create_text_label();
    load_rt(a, R::V0, moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, normal_physical);
    for f in [RT_PHASE_OPEN, RT_RETIRE_STAGE, moving_round::DEMAND] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    a.bind_text_label(normal_physical);
    owner_state::bind_running(a, l);
    let slow = a.create_text_label();
    let active_hint = a.create_text_label();
    load_rt(a, R::V0, young_mark::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, active_hint);
    start_policy::below(a, done);
    load_rt(a, R::V0, old_policy::ALLOCATED);
    load_rt(a, R::V1, ALLOCATION_BASE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, slow); // independently sampled hints cannot reject
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, start_policy::TARGET);
    a.shl_reg_imm8(R::V1, 17);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.jmp_label(slow);
    a.bind_text_label(active_hint);
    load_rt(a, R::V0, young_mark::DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.bind_text_label(slow);
    worker::lock(a, l);
    let release = a.create_text_label();
    let service = a.create_text_label();
    let active = a.create_text_label();
    let start = a.create_text_label();
    let end = a.create_text_label();
    for field in [quiescence::REQUEST, phase_round::RESUMING] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, service);
    }
    journal_snapshot::lifetime(a, l);
    copy::bump(a, POLLS, R::V0);
    load_rt(a, R::V0, young_mark::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, active);
    not_due(a, l, release);
    a.jmp_label(start);
    a.bind_text_label(active);
    a.call_label(l.publish_pending);
    let no_debt = a.create_text_label();
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no_debt);
    worker::request_work(a);
    a.jmp_label(release);
    a.bind_text_label(no_debt);
    load_rt(a, R::V0, young_mark::DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, release);
    a.jmp_label(end);
    for (label, entry, counter, is_start) in [
        (start, l.young_start_observed_actor, START_REQUESTS, true),
        (end, l.young_end_observed_actor, END_REQUESTS, false),
    ] {
        a.bind_text_label(label);
        #[cfg(not(test))]
        let _ = is_start;
        load_rt(a, R::V0, young_mark::EPOCH);
        store_rt(a, phase_round::REQUESTED_CYCLE, R::V0);
        copy::bump(a, counter, R::V0);
        worker::unlock(a, l);
        #[cfg(test)]
        if is_start && let Some(hook) = l.young_scheduler_request_hook {
            a.call_label(hook);
        }
        load_rt(a, R::V0, phase_round::REQUESTED_CYCLE);
        a.call_label(entry);
        a.jmp_label(done);
    }
    a.bind_text_label(service);
    worker::unlock(a, l);
    a.call_label(l.owner_checkpoint_poll);
    a.jmp_label(done);
    a.bind_text_label(release);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.bind_text_label(done);
    quiescence::restore(a);
    a.ret();
}
