//! Real moving rendezvous before physical bank mutation. Start reuses the
//! concurrent Young/Old mark engines; end checks never drain a parked heap.
//! Source lifetime and captured journal validation are in journal.
use super::*;
use crate::gc_abi::{HandshakeState, RootRegistry};
#[path = "moving_admission.rs"]
pub(super) mod admission;
#[path = "moving_journal.rs"]
pub(super) mod journal;
#[path = "moving_native.rs"]
pub(super) mod native;
#[path = "moving_scheduler.rs"]
pub(super) mod scheduler;
#[repr(C)]
pub(super) struct State {
    scheduler: scheduler::State,
    enabled: u64,
    current_bank: u64,
    source_bank: u64,
    source_phase: u64,
    closed_bank: u64,
    minimum_phase: u64,
    target_phase: u64,
    coordinator: u64,
    demand: u64,
    starts: u64,
    start_expected: u64,
    start_acknowledged: u64,
    captured_debt: u64,
    opened_debt: u64,
    closes: u64,
    cleanups: u64,
    retries: u64,
    no_move_round: u64,
    aborted_starts: u64,
    native_tail_bytes: u64,
    native_tail_waits: u64,
}
#[repr(C)]
pub(super) struct Owner {
    requested_bank: u64,
    requested_end_bank: u64,
    scheduler_kind: u64,
    stale_starts: u64,
    stale_ends: u64,
    pub(super) fresh_reserved: u64,
    pub(super) fresh_bank: u64,
    pub(super) adoptions: u64,
}
const BASE: usize = offset_of!(Runtime, moving_round);
pub(super) const ENABLED: usize = BASE + offset_of!(State, enabled);
pub(super) const CURRENT_BANK: usize = BASE + offset_of!(State, current_bank);
pub(super) const SOURCE_BANK: usize = BASE + offset_of!(State, source_bank);
pub(super) const SOURCE_PHASE: usize = BASE + offset_of!(State, source_phase);
pub(super) const CLOSED_BANK: usize = BASE + offset_of!(State, closed_bank);
pub(super) const MINIMUM_PHASE: usize = BASE + offset_of!(State, minimum_phase);
pub(super) const TARGET_PHASE: usize = BASE + offset_of!(State, target_phase);
const COORDINATOR: usize = BASE + offset_of!(State, coordinator);
pub(super) const DEMAND: usize = BASE + offset_of!(State, demand);
pub(super) const START_EXPECTED: usize = BASE + offset_of!(State, start_expected);
pub(super) const START_ACKNOWLEDGED: usize = BASE + offset_of!(State, start_acknowledged);
pub(super) const OPENED_DEBT: usize = BASE + offset_of!(State, opened_debt);
pub(super) const CAPTURED_DEBT: usize = BASE + offset_of!(State, captured_debt);
pub(super) const STARTS: usize = BASE + offset_of!(State, starts);
pub(super) const CLOSES: usize = BASE + offset_of!(State, closes);
pub(super) const CLEANUPS: usize = BASE + offset_of!(State, cleanups);
pub(super) const RETRIES: usize = BASE + offset_of!(State, retries);
const NO_MOVE_ROUND: usize = BASE + offset_of!(State, no_move_round);
pub(super) const ABORTED_STARTS: usize = BASE + offset_of!(State, aborted_starts);
pub(super) const NATIVE_TAIL_BYTES: usize = BASE + offset_of!(State, native_tail_bytes);
pub(super) const NATIVE_TAIL_WAITS: usize = BASE + offset_of!(State, native_tail_waits);
const OWN: usize = owner_state::offset(offset_of!(OwnerState, moving_round));
pub(super) const REQUESTED_BANK: usize = OWN + offset_of!(Owner, requested_bank);
pub(super) const REQUESTED_END_BANK: usize = OWN + offset_of!(Owner, requested_end_bank);
pub(super) const SCHEDULER_KIND: usize = OWN + offset_of!(Owner, scheduler_kind);
pub(super) const OWN_STALE_STARTS: usize = OWN + offset_of!(Owner, stale_starts);
pub(super) const OWN_STALE_ENDS: usize = OWN + offset_of!(Owner, stale_ends);
pub(super) const ADOPTIONS: usize = OWN + offset_of!(Owner, adoptions);
fn hs(field: usize) -> usize {
    participants::HANDSHAKE + field
}
pub(super) fn branch_enabled<E: PortableAsm>(a: &mut E, target: E::TextLabel) {
    load_rt(a, R::V0, ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, target);
}
fn request<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V1, quiescence::REQUEST);
    a.atomic_load_acquire(R::V0, R::V1);
}
fn release_request<E: PortableAsm>(a: &mut E, value: u64) {
    lea_rt(a, R::V1, quiescence::REQUEST);
    a.mov_imm64(R::V0, value);
    a.atomic_store_release(R::V1, R::V0);
}
pub(super) fn prepare_peer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    store_rt(a, phase_round::PHASE, R::V0);
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, phase_round::BANK, R::V0);
    // Own constructor scratch is cleared before ACK; a retry with an already
    // empty scratch must not create another 0->0 publication indefinitely.
    let empty = a.create_text_label();
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.atomic_load_acquire_u32(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Equal, empty);
    load_rt(a, R::V0, RT_ALLOC_ROOT + HANDLE_CELL);
    a.atomic_load_acquire(R::V0, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    collect::clear_scratch(a, l);
    a.bind_text_label(empty);
}
pub(super) fn require_coordinator<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, active)));
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, phase_round::KIND);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm32(R::V0, 7);
    a.jcc_label(C::Above, l.fail);
    load_rt(a, R::V0, COORDINATOR);
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, l.fail);
    request(a);
    load_rt(a, R::V1, hs(offset_of!(HandshakeState, generation)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, expected)));
    load_rt(a, R::V1, hs(offset_of!(HandshakeState, acknowledged)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
}
pub(super) fn resume_peer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    native_io::adoption::private_closed(a, l);
    let retained_page = a.create_text_label();
    load_rt(a, R::V0, phase_round::OWN_KIND);
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::NotEqual, retained_page);
    load_rt(a, R::V0, RT_REQUEST + offset_of!(AllocationRequest, page));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(retained_page);
    for f in [
        RT_CTX + offset_of!(MutatorContext, tlab_start),
        RT_CTX + offset_of!(MutatorContext, tlab_top),
        RT_CTX + offset_of!(MutatorContext, tlab_end),
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    load_rt(a, R::V1, phase_round::PHASE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, phase_round::BANK);
    let same = a.create_text_label();
    load_rt(a, R::V1, phase_round::OWN_KIND);
    a.cmp_reg_imm32(R::V1, 5);
    a.jcc_label(C::NotEqual, same);
    let unchanged = a.create_text_label();
    let check_bank = a.create_text_label();
    load_rt(a, R::V1, NO_MOVE_ROUND);
    load_rt(a, R::V2, hs(offset_of!(HandshakeState, generation)));
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Equal, unchanged);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    a.jmp_label(check_bank);
    a.bind_text_label(unchanged);
    load_rt(a, R::V1, CLOSED_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V1, phase_round::PHASE);
    load_rt(a, R::V2, TARGET_PHASE);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(same);
    a.bind_text_label(check_bank);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    load_rt(a, R::V1, TARGET_PHASE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.atomic_store_release(R::V1, R::V0);
    allocation_records::adopt(a, l);
    old_tlab::restore(a, l);
    copy::bump(a, ADOPTIONS, R::V0);
}
fn finish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    store_rt(a, TARGET_PHASE, R::V0);
    store_rt(a, cooperative_old::SNAPSHOT, R::V0);
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, cooperative_old::BANK, R::V0);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, expected)));
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, phase_round::RESUMING, R::V0);
    participants::finish(a, l);
    allocation_records::adopt(a, l);
    old_tlab::restore(a, l);
    copy::bump(a, ADOPTIONS, R::V0);
    store_rt_imm(a, phase_round::OWN_KIND, 0);
    store_rt_imm(a, phase_round::KIND, 0);
    store_rt_imm(a, COORDINATOR, 0);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, generation)));
    store_rt(a, quiescence::COMPLETED, R::V0);
    release_request(a, 0);
    worker::request_work(a);
}
fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, kind: u64) {
    if kind == 5 {
        quiescence::prepare(a, l);
    } else {
        quiescence::prepare_old(a, l);
    }
    participants::step(a, l, 6);
    store_rt_imm(a, phase_round::KIND, kind);
    if kind == 5 {
        store_rt_imm(a, NO_MOVE_ROUND, 0);
    }
    store_rt(a, COORDINATOR, R::OwnerContext);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, generation)));
    lea_rt(a, R::V1, quiescence::REQUEST);
    a.atomic_store_release(R::V1, R::V0);
    store_rt_imm(a, phase_round::OWN_KIND, kind);
    prepare_peer(a, l);
    participants::step(a, l, 1);
    let wait = a.create_text_label();
    let ready = a.create_text_label();
    a.bind_text_label(wait);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, acknowledged)));
    load_rt(a, R::V1, hs(offset_of!(HandshakeState, expected)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, ready);
    a.jcc_label(C::Above, l.fail);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.call_label(l.worker.as_ref().unwrap().hooks.relax);
    worker::lock(a, l);
    a.jmp_label(wait);
    a.bind_text_label(ready);
    copy_claims::require_empty(a, l);
    worker_view::require_quiescent(a, l);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    scheduler::emit(a, l);
    a.bind_text_label(l.moving_enable_actor);
    quiescence::save(a);
    owner_state::bind_running(a, l);
    a.call_label(l.dual_mark_enable);
    worker::lock(a, l);
    load_rt(a, R::V0, ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, CURRENT_BANK, R::V0);
    store_rt(a, CLOSED_BANK, R::V0);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    store_rt(a, MINIMUM_PHASE, R::V0);
    store_rt(a, TARGET_PHASE, R::V0);
    store_rt_imm(a, ENABLED, 1);
    worker::unlock(a, l);
    quiescence::restore(a);
    a.ret();

    let start_dispatch = a.create_text_label();
    a.bind_text_label(l.moving_start_observed_actor);
    quiescence::save(a);
    owner_state::bind_running(a, l);
    store_rt(a, REQUESTED_BANK, R::V0);
    a.jmp_label(start_dispatch);
    a.bind_text_label(l.moving_start_actor);
    quiescence::save(a);
    owner_state::bind_running(a, l);
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, REQUESTED_BANK, R::V0);
    a.bind_text_label(start_dispatch);
    let start_retry = a.create_text_label();
    let release = a.create_text_label();
    let service = a.create_text_label();
    let marks = a.create_text_label();
    let young_start = a.create_text_label();
    let young_end = a.create_text_label();
    let old_end = a.create_text_label();
    a.bind_text_label(start_retry);
    worker::lock(a, l);
    load_rt(a, R::V0, ENABLED);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    for f in [quiescence::REQUEST, phase_round::RESUMING] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, service);
    }
    load_rt(a, R::V0, allocation_records::REVISION);
    load_rt(a, R::V1, REQUESTED_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    let current_request = a.create_text_label();
    a.jcc_label(C::Equal, current_request);
    copy::bump(a, scheduler::STALE_STARTS, R::V0);
    copy::bump(a, OWN_STALE_STARTS, R::V0);
    a.jmp_label(release);
    a.bind_text_label(current_request);
    store_rt(a, DEMAND, R::V0);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    load_rt(a, R::V1, RT_RETIRE_STAGE);
    a.or_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, marks);
    worker::unlock(a, l);
    a.call_label(l.moving_end_actor);
    a.jmp_label(start_retry);
    a.bind_text_label(marks);
    let old_present = a.create_text_label();
    load_rt(a, R::V0, weak::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, old_present);
    for f in [RT_MARK_ACTIVE, RT_MARK_VALID] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, old_present);
    }
    let needs_old_start = a.create_text_label();
    load_rt(a, R::V0, old_cycle::REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, needs_old_start);
    pool::must_full(a, l, needs_old_start);
    a.jmp_label(old_present);
    a.bind_text_label(needs_old_start);
    worker::unlock(a, l);
    a.call_label(l.old_start_actor);
    a.jmp_label(start_retry);
    a.bind_text_label(old_present);
    load_rt(a, R::V0, young_mark::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, young_end);
    load_rt(a, R::V0, young_mark::VALID);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, young_start);
    let ready_to_move = a.create_text_label();
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, ready_to_move);
    load_rt(a, R::V0, old_cycle::REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, old_end);
    pool::must_full(a, l, old_end);
    a.bind_text_label(ready_to_move);
    native_io::require_phase_clear(a, l);
    #[cfg(test)]
    if let Some(hook) = l.moving_selected_hook {
        a.call_label(hook);
    }
    begin(a, l, 5);
    // Final source/capacity selection uses the actual closed producer quorum.
    // If late allocation made Full necessary, reopen this unchanged bank and
    // finish Old through its real asynchronous end protocol before retrying.
    let move_now = a.create_text_label();
    let needs_old_end = a.create_text_label();
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    let check_full = a.create_text_label();
    a.jcc_label(C::NotEqual, check_full);
    load_rt(a, R::V0, weak::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, move_now);
    load_rt(a, R::V0, RT_MARK_VALID);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, move_now);
    a.bind_text_label(check_full);
    load_rt(a, R::V0, old_cycle::REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, needs_old_end);
    pool::must_full(a, l, needs_old_end);
    a.jmp_label(move_now);
    a.bind_text_label(needs_old_end);
    store_rt_imm(a, old_cycle::REQUEST, old_cycle::CAPACITY);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, generation)));
    store_rt(a, NO_MOVE_ROUND, R::V0);
    copy::bump(a, ABORTED_STARTS, R::V0);
    finish(a, l);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.jmp_label(start_retry);
    a.bind_text_label(move_now);
    for (source, target) in [
        (hs(offset_of!(HandshakeState, expected)), START_EXPECTED),
        (
            hs(offset_of!(HandshakeState, acknowledged)),
            START_ACKNOWLEDGED,
        ),
        (root_work::PENDING, CAPTURED_DEBT),
    ] {
        load_rt(a, R::V0, source);
        store_rt(a, target, R::V0);
    }
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, SOURCE_BANK, R::V0);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    store_rt(a, SOURCE_PHASE, R::V0);
    store_rt_imm(a, CLOSED_BANK, 0);
    a.call_label(l.collect_moving);
    worker::lock(a, l);
    load_rt(a, R::V0, root_work::PENDING);
    store_rt(a, OPENED_DEBT, R::V0);
    store_rt_imm(a, DEMAND, 0);
    scheduler::begin_cycle(a);
    copy::bump(a, STARTS, R::V0);
    finish(a, l);
    a.jmp_label(release);
    for (label, entry, active) in [
        (young_start, l.young_start_actor, None),
        (young_end, l.young_end_actor, Some(young_mark::DONE)),
        (old_end, l.old_end_actor, Some(RT_MARK_DONE)),
    ] {
        a.bind_text_label(label);
        if let Some(done) = active {
            load_rt(a, R::V0, done);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, service);
            // A done hint does not close outstanding producer journals. Let
            // the worker transport them before requesting another mark quorum.
            lea_rt(a, R::V1, root_work::PENDING);
            a.atomic_load_acquire(R::V0, R::V1);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, service);
        }
        worker::unlock(a, l);
        a.call_label(entry);
        a.jmp_label(start_retry);
    }
    a.bind_text_label(service);
    worker::request_work(a);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.call_label(l.owner_checkpoint_poll);
    a.call_label(l.worker.as_ref().unwrap().hooks.relax);
    a.jmp_label(start_retry);
    a.bind_text_label(release);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    quiescence::restore(a);
    a.ret();

    let end_dispatch = a.create_text_label();
    a.bind_text_label(l.moving_end_observed_actor);
    quiescence::save(a);
    owner_state::bind_running(a, l);
    store_rt(a, REQUESTED_END_BANK, R::V0);
    a.jmp_label(end_dispatch);
    a.bind_text_label(l.moving_end_actor);
    quiescence::save(a);
    owner_state::bind_running(a, l);
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, REQUESTED_END_BANK, R::V0);
    a.bind_text_label(end_dispatch);
    worker::lock(a, l);
    let end_release = a.create_text_label();
    let end_service = a.create_text_label();
    let cleanup = a.create_text_label();
    let pending = a.create_text_label();
    for f in [quiescence::REQUEST, phase_round::RESUMING] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, end_service);
    }
    load_rt(a, R::V0, allocation_records::REVISION);
    load_rt(a, R::V1, REQUESTED_END_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    let current_end = a.create_text_label();
    let stale_end = a.create_text_label();
    a.jcc_label(C::Equal, current_end);
    a.bind_text_label(stale_end);
    copy::bump(a, scheduler::STALE_ENDS, R::V0);
    copy::bump(a, OWN_STALE_ENDS, R::V0);
    a.jmp_label(end_release);
    a.bind_text_label(current_end);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, cleanup);
    begin(a, l, 6);
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    collect::relocation_pending(a, pending);
    old_policy::observe(a, l);
    tenuring::observe(a);
    capacity::observe(a);
    start_policy::observe(a);
    for (total, part) in [
        (RT_PROMOTED, RT_W_PROMOTED),
        (RT_UNMARKED, RT_W_UNMARKED),
        (young_mark::UNMARKED, young_mark::WORKER_UNMARKED),
    ] {
        load_rt(a, R::V0, part);
        copy::add_bytes(a, total, R::V0);
        store_rt_imm(a, part, 0);
    }
    phase::relocating(a, l, crate::gc_abi::Phase::Retire);
    retire::begin(a, l);
    for f in [RT_PHASE_OPEN, RT_RY_LO, RT_RY_HI, RT_RO_LO, RT_RO_HI] {
        store_rt_imm(a, f, 0);
    }
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, CLOSED_BANK, R::V0);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    store_rt(a, MINIMUM_PHASE, R::V0);
    store_rt_imm(a, young_mark::VALID, 0);
    store_rt_imm(a, RT_MARK_VALID, 0);
    copy::bump(a, CLOSES, R::V0);
    finish(a, l);
    a.jmp_label(end_release);
    a.bind_text_label(pending);
    copy::bump(a, RETRIES, R::V0);
    store_rt_imm(a, RT_WORKER_DONE, 0);
    worker::request_work(a);
    finish(a, l);
    a.jmp_label(end_release);
    a.bind_text_label(cleanup);
    load_rt(a, R::V0, RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, end_release);
    a.cmp_reg_imm32(R::V0, retire::DONE as i32);
    a.jcc_label(C::NotEqual, end_service);
    begin(a, l, 7);
    retire::complete(a, l);
    copy::bump(a, CLEANUPS, R::V0);
    finish(a, l);
    a.jmp_label(end_release);
    a.bind_text_label(end_service);
    worker::request_work(a);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.call_label(l.owner_checkpoint_poll);
    a.call_label(l.worker.as_ref().unwrap().hooks.relax);
    // A newer handshake can race between an observed request and this
    // entry's ticket. After servicing it, classify the captured request
    // against the actual revision before returning. Otherwise a stale end
    // is silently dropped without the stale-request receipt.
    worker::lock(a, l);
    load_rt(a, R::V0, allocation_records::REVISION);
    load_rt(a, R::V1, REQUESTED_END_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, stale_end);
    a.bind_text_label(end_release);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    quiescence::restore(a);
    a.ret();
}
