//! Allocation/poll-driven nonmoving Old scheduling for an explicitly joined
//! cooperative heap. Enabling enrolls owners that poll on allocation/safepoints.
//! Dual enrollment also schedules fixed-bank Young rounds with independent
//! allocation credit. Physical movement remains separate; enrolled heaps
//! reject legacy collect before any physical mutation.
use super::*;
use crate::gc_abi::HandshakeState;
#[repr(C)]
pub(super) struct State {
    enabled: u64,
    young_enabled: u64,
    bank: u64,
    snapshot: u64,
    allocation_base: u64,
    start_requests: u64,
    end_requests: u64,
    polls: u64,
}
const BASE: usize = offset_of!(Runtime, cooperative_old);
pub(super) const ENABLED: usize = BASE + offset_of!(State, enabled);
pub(super) const YOUNG_ENABLED: usize = BASE + offset_of!(State, young_enabled);
pub(super) const BANK: usize = BASE + offset_of!(State, bank);
pub(super) const SNAPSHOT: usize = BASE + offset_of!(State, snapshot);
pub(super) const ALLOCATION_BASE: usize = BASE + offset_of!(State, allocation_base);
pub(super) const START_REQUESTS: usize = BASE + offset_of!(State, start_requests);
pub(super) const END_REQUESTS: usize = BASE + offset_of!(State, end_requests);
pub(super) const POLLS: usize = BASE + offset_of!(State, polls);
pub(super) fn branch_enabled<E: PortableAsm>(a: &mut E, target: E::TextLabel) {
    load_rt(a, R::V0, ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, target);
}
/// Under the ticket. The current actual cycle consumes this allocation window.
pub(super) fn begin_cycle<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, old_policy::ALLOCATED);
    store_rt(a, ALLOCATION_BASE, R::V0);
}
/// Under ticket: old policy threshold in pages and a fresh allocation window
/// prevent repeated empty snapshots above a permanently occupied threshold.
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
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, stress: bool) {
    enable(a, l, stress, false);
    enable(a, l, stress, true);
    cooperative_young::emit(a, l);

    a.bind_text_label(l.cooperative_old_poll);
    quiescence::save(a);
    let done = a.create_text_label();
    load_rt(a, R::V0, ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    let normal_physical = a.create_text_label();
    load_rt(a, R::V0, moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, normal_physical);
    a.call_label(l.moving_poll_actor);
    a.jmp_label(done);
    a.bind_text_label(normal_physical);
    owner_state::bind_running(a, l);
    a.call_label(l.cooperative_young_poll);
    let old_body = a.create_text_label();
    a.jmp_label(old_body);
    a.bind_text_label(l.cooperative_old_only_poll);
    quiescence::save(a);
    owner_state::bind_running(a, l);
    a.bind_text_label(old_body);
    // Lock-free hints only: all eligibility is rechecked under the ticket.
    // Active unfinished marking and sub-window allocation stay on the fast path.
    let slow = a.create_text_label();
    let hint_active = a.create_text_label();
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, hint_active);
    start_policy::below(a, done);
    load_rt(a, R::V0, old_policy::ALLOCATED);
    load_rt(a, R::V1, ALLOCATION_BASE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, slow); // racing hint samples cannot reject a valid heap
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, start_policy::TARGET);
    a.shl_reg_imm8(R::V1, 17);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.jmp_label(slow);
    a.bind_text_label(hint_active);
    load_rt(a, R::V0, RT_MARK_DONE);
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
    copy::bump(a, POLLS, R::V0);
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, active);
    not_due(a, l, release);
    a.jmp_label(start);
    a.bind_text_label(active);
    // A completion hint does not certify transferred root journals. Publish
    // this actual initializer at the safepoint and let the independent worker
    // pay existing debt before asking peers to suspend TLABs for an end check.
    a.call_label(l.publish_pending);
    let no_debt = a.create_text_label();
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no_debt);
    worker::request_work(a);
    a.jmp_label(release);
    a.bind_text_label(no_debt);
    load_rt(a, R::V0, RT_MARK_DONE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, release);
    a.jmp_label(end);
    for (label, entry, counter, is_start) in [
        (start, l.old_start_observed_actor, START_REQUESTS, true),
        (end, l.old_end_observed_actor, END_REQUESTS, false),
    ] {
        a.bind_text_label(label);
        #[cfg(not(test))]
        let _ = is_start;
        load_rt(a, R::V0, RT_MARK_CYCLES);
        store_rt(a, phase_round::REQUESTED_CYCLE, R::V0);
        copy::bump(a, counter, R::V0);
        worker::unlock(a, l);
        #[cfg(test)]
        if is_start && let Some(hook) = l.scheduler_request_hook {
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

fn enable<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, stress: bool, dual: bool) {
    a.bind_text_label(if dual {
        l.dual_mark_enable
    } else {
        l.cooperative_old_enable
    });
    quiescence::save(a);
    owner_state::bind_running(a, l);
    worker::lock(a, l);
    if stress || l.worker.is_none() {
        a.jmp_label(l.fail);
    }
    for field in [
        ENABLED,
        quiescence::REQUEST,
        phase_round::RESUMING,
        phase_round::KIND,
        RT_MARK_ACTIVE,
        RT_MARK_VALID,
        RT_PHASE_OPEN,
        RT_RETIRE_STAGE,
        young_mark::ACTIVE,
    ] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.and_reg_imm32(R::V0, 255);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    native_io::require_phase_clear(a, l);
    load_rt(a, R::V0, RT_WORKER_ALIVE);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V0,
        participants::HANDSHAKE + offset_of!(HandshakeState, members),
    );
    load_rt(
        a,
        R::V1,
        participants::HANDSHAKE + offset_of!(HandshakeState, native),
    );
    a.sub_reg_reg(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 1); // caller is the sole Running bootstrap owner
    a.jcc_label(C::NotEqual, l.fail);
    quiescence::prepare(a, l);
    // Transfer the last sole-consumer backlog before publishing independent
    // consumption. Other enrolled owners are already sealed BlockedNative.
    a.call_label(l.root_journal_seal_owner);
    root_work::require_empty(a, l);
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, BANK, R::V0);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    store_rt(a, SNAPSHOT, R::V0);
    begin_cycle(a);
    cooperative_young::begin_cycle(a);
    store_rt_imm(a, YOUNG_ENABLED, u64::from(dual));
    lea_rt(a, R::V1, ENABLED);
    a.mov_imm64(R::V0, 1);
    a.atomic_store_release(R::V1, R::V0);
    worker::unlock(a, l);
    quiescence::restore(a);
    a.ret();
}
