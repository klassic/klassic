//! Allocation and safepoint demand for explicitly enrolled moving owners.
//! A request retains its observed physical bank across ticket release. Only
//! an actual bank flip consumes fresh allocation credit; end checks retry.
use super::*;
#[repr(C)]
pub(in super::super) struct State {
    enabled: u64,
    allocation_base: u64,
    polls: u64,
    starts: u64,
    ends: u64,
    stale_starts: u64,
    stale_ends: u64,
}
const BASE: usize = offset_of!(Runtime, moving_round.scheduler);
pub(in super::super) const ENABLED: usize = BASE + offset_of!(State, enabled);
pub(in super::super) const ALLOCATION_BASE: usize = BASE + offset_of!(State, allocation_base);
pub(in super::super) const POLLS: usize = BASE + offset_of!(State, polls);
pub(in super::super) const START_REQUESTS: usize = BASE + offset_of!(State, starts);
pub(in super::super) const END_REQUESTS: usize = BASE + offset_of!(State, ends);
pub(in super::super) const STALE_STARTS: usize = BASE + offset_of!(State, stale_starts);
pub(in super::super) const STALE_ENDS: usize = BASE + offset_of!(State, stale_ends);
pub(in super::super) fn begin_cycle<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, old_policy::ALLOCATED);
    store_rt(a, ALLOCATION_BASE, R::V0);
}
pub(in super::super) fn not_due<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    done: E::TextLabel,
) {
    start_policy::below(a, done);
    load_rt(a, R::V0, old_policy::ALLOCATED);
    load_rt(a, R::V1, ALLOCATION_BASE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, start_policy::TARGET);
    a.shl_reg_imm8(R::V1, 17);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
}
pub(in super::super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.moving_auto_enable_actor);
    quiescence::save(a);
    a.call_label(l.moving_enable_actor);
    owner_state::bind_running(a, l);
    worker::lock(a, l);
    begin_cycle(a);
    store_rt_imm(a, ENABLED, 1);
    worker::unlock(a, l);
    quiescence::restore(a);
    a.ret();

    a.bind_text_label(l.moving_poll_actor);
    quiescence::save(a);
    let done = a.create_text_label();
    load_rt(a, R::V0, ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    owner_state::bind_running(a, l);
    worker::lock(a, l);
    let release = a.create_text_label();
    let service = a.create_text_label();
    let end = a.create_text_label();
    let open = a.create_text_label();
    let wait_worker = a.create_text_label();
    let idle = a.create_text_label();
    for field in [quiescence::REQUEST, phase_round::RESUMING] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, service);
    }
    copy::bump(a, POLLS, R::V0);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, open);
    load_rt(a, R::V0, RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, idle);
    a.cmp_reg_imm32(R::V0, retire::DONE as i32);
    a.jcc_label(C::Equal, end);
    a.jmp_label(wait_worker);
    a.bind_text_label(open);
    // These are hints only. A real end rendezvous still checks the complete
    // ledger/frontiers after every owner ACK. Avoid a quorum per allocation
    // while the independent worker plainly still has transport/copy work.
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, wait_worker);
    load_rt(a, R::V0, RT_WORKER_DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, end);
    a.bind_text_label(wait_worker);
    worker::request_work(a);
    a.jmp_label(release);
    a.bind_text_label(idle);
    load_rt(a, R::V0, DEMAND);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, release);
    let old_only = a.create_text_label();
    let requested_start = a.create_text_label();
    // An actual planned/capacity Old request is independent of fresh Young
    // occupancy. It must progress even when no more application bytes arrive.
    load_rt(a, R::V0, old_cycle::REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, requested_start);
    not_due(a, l, old_only);
    a.bind_text_label(requested_start);
    for (label, entry, field, counter) in [
        (
            None,
            l.moving_start_observed_actor,
            REQUESTED_BANK,
            START_REQUESTS,
        ),
        (
            Some(end),
            l.moving_end_observed_actor,
            REQUESTED_END_BANK,
            END_REQUESTS,
        ),
    ] {
        if let Some(label) = label {
            a.bind_text_label(label);
        }
        load_rt(a, R::V0, allocation_records::REVISION);
        store_rt(a, field, R::V0);
        copy::bump(a, counter, R::V0);
        store_rt_imm(
            a,
            SCHEDULER_KIND,
            if counter == START_REQUESTS { 1 } else { 2 },
        );
        worker::unlock(a, l);
        #[cfg(test)]
        if let Some(hook) = l.moving_scheduler_request_hook {
            a.call_label(hook);
        }
        load_rt(a, R::V0, field);
        a.call_label(entry);
        #[cfg(test)]
        if let Some(hook) = l.moving_scheduler_return_hook {
            a.call_label(hook);
        }
        a.jmp_label(done);
    }
    a.bind_text_label(old_only);
    worker::unlock(a, l);
    a.call_label(l.cooperative_old_only_poll);
    a.jmp_label(done);
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
