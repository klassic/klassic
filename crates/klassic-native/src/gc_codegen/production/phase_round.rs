//! Actual fixed-bank Young/Old phase rounds. Enrolled owners retain initialized
//! journals across ACK/start/reopen for independent worker admission. Final
//! closure still requires real zero obligations and empty graph frontiers.
//! Dual enrollment admits Young rounds with actual queue/debt closure.
//! Legacy owners self-handoff; moving-bank coordination remains separate.
use super::*;
use crate::gc_abi::HandshakeState;
#[repr(C)]
pub(super) struct State {
    young_starts: u64,
    young_ends: u64,
    young_end_checks: u64,
    young_end_retries: u64,
    young_stale_starts: u64,
    young_stale_ends: u64,
    kind: u64,
    coordinator: u64,
    resuming: u64,
    resume_waits: u64,
    starts: u64,
    ends: u64,
    end_checks: u64,
    end_retries: u64,
    stale_starts: u64,
    stale_ends: u64,
    closed_publications: u64,
    closed_acknowledged: u64,
    closed_worker: u64,
}
#[repr(C)]
pub(super) struct Owner {
    pub(super) tlab: old_tlab::State,
    kind: u64,
    phase: u64,
    bank: u64,
    adoptions: u64,
    requested_cycle: u64,
    coordinations: u64,
}
const BASE: usize = offset_of!(Runtime, phase_round);
pub(super) const KIND: usize = BASE + offset_of!(State, kind);
const COORDINATOR: usize = BASE + offset_of!(State, coordinator);
pub(super) const RESUMING: usize = BASE + offset_of!(State, resuming);
pub(super) const RESUME_WAITS: usize = BASE + offset_of!(State, resume_waits);
pub(super) const STARTS: usize = BASE + offset_of!(State, starts);
pub(super) const CLOSED_PUBLICATIONS: usize = BASE + offset_of!(State, closed_publications);
pub(super) const CLOSED_ACKNOWLEDGED: usize = BASE + offset_of!(State, closed_acknowledged);
pub(super) const CLOSED_WORKER: usize = BASE + offset_of!(State, closed_worker);
pub(super) const END_CHECKS: usize = BASE + offset_of!(State, end_checks);
pub(super) const END_RETRIES: usize = BASE + offset_of!(State, end_retries);
pub(super) const ENDS: usize = BASE + offset_of!(State, ends);
pub(super) const STALE_STARTS: usize = BASE + offset_of!(State, stale_starts);
pub(super) const STALE_ENDS: usize = BASE + offset_of!(State, stale_ends);
pub(super) const Y_STARTS: usize = BASE + offset_of!(State, young_starts);
pub(super) const Y_ENDS: usize = BASE + offset_of!(State, young_ends);
pub(super) const Y_CHECKS: usize = BASE + offset_of!(State, young_end_checks);
pub(super) const Y_RETRIES: usize = BASE + offset_of!(State, young_end_retries);
pub(super) const Y_STALE_STARTS: usize = BASE + offset_of!(State, young_stale_starts);
pub(super) const Y_STALE_ENDS: usize = BASE + offset_of!(State, young_stale_ends);
const OWN: usize = owner_state::offset(offset_of!(OwnerState, phase_round));
pub(super) const OWN_KIND: usize = OWN + offset_of!(Owner, kind);
pub(super) const PHASE: usize = OWN + offset_of!(Owner, phase);
pub(super) const BANK: usize = OWN + offset_of!(Owner, bank);
pub(super) const ADOPTIONS: usize = OWN + offset_of!(Owner, adoptions);
pub(super) const REQUESTED_CYCLE: usize = OWN + offset_of!(Owner, requested_cycle);
pub(super) const COORDINATIONS: usize = OWN + offset_of!(Owner, coordinations);
fn hs(field: usize) -> usize {
    participants::HANDSHAKE + field
}
fn request<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V1, quiescence::REQUEST);
    a.atomic_load_acquire(R::V0, R::V1);
}
fn release_request<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V1, quiescence::REQUEST);
    a.atomic_store_release(R::V1, R::V0);
}
/// Called only for an unacknowledged actual Running participant, under ticket.
pub(super) fn prepare_peer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, KIND);
    store_rt(a, OWN_KIND, R::V0);
    let done = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    let fixed = a.create_text_label();
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Below, fixed);
    a.jcc_label(C::Equal, fixed);
    moving_round::prepare_peer(a, l);
    a.jmp_label(done);
    a.bind_text_label(fixed);
    let authorized = a.create_text_label();
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Below, authorized);
    load_rt(a, R::V0, cooperative_old::YOUNG_ENABLED);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(authorized);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    store_rt(a, PHASE, R::V0);
    load_rt(a, R::V0, allocation_records::REVISION);
    store_rt(a, BANK, R::V0);
    let legacy = a.create_text_label();
    let prepared = a.create_text_label();
    // Detaching keeps participating, but its frame storage may already be
    // retired. Its producer/TLAB closure happened before entering this state.
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.atomic_load_acquire_u32(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Equal, prepared);
    load_rt(a, R::V0, cooperative_old::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, legacy);
    journal_snapshot::lifetime(a, l);
    // Empty scratch needs no new 0->0 journal obligation on each retry.
    // A full startup ring can require at most one actual pressure record for
    // this one clear; no region list or total backlog is traversed here.
    load_rt(a, R::V0, RT_ALLOC_ROOT + HANDLE_CELL);
    a.atomic_load_acquire(R::V0, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, prepared);
    collect::clear_scratch(a, l);
    a.jmp_label(prepared);
    a.bind_text_label(legacy);
    collect::clear_scratch(a, l);
    a.call_label(l.root_journal_seal_owner);
    a.bind_text_label(prepared);
    // Own handoff can yield. A different round must not inherit this ACK.
    request(a);
    load_rt(a, R::V1, hs(offset_of!(HandshakeState, generation)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.bind_text_label(done);
}
/// Called under ticket just before the real participant resume. The last
/// decrement becomes visible only after that resume releases the same ticket.
pub(super) fn resume_peer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    #[cfg(test)]
    if let Some(hook) = l.phase_resume_hook {
        a.call_label(hook);
    }
    load_rt(a, R::V0, OWN_KIND);
    let done = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    let fixed = a.create_text_label();
    let adopted = a.create_text_label();
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Below, fixed);
    a.jcc_label(C::Equal, fixed);
    moving_round::resume_peer(a, l);
    a.jmp_label(adopted);
    a.bind_text_label(fixed);
    native_io::adoption::adopt_snapshot(a, l, PHASE, BANK, ADOPTIONS);
    old_tlab::restore(a, l);
    a.bind_text_label(adopted);
    load_rt(a, R::V0, RESUMING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, RESUMING, R::V0);
    let clear = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, clear);
    load_rt(a, R::V0, cooperative_old::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, clear);
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, clear);
    // The final peer makes retained journal work eligible again. Publish a
    // new request under the ticket; deliver its wake after real resume/unlock.
    worker::request_work(a);
    a.bind_text_label(clear);
    store_rt_imm(a, OWN_KIND, 0);
    a.bind_text_label(done);
}
/// Reject a foreign phase publisher before it can touch transport or metadata.
pub(super) fn require_coordinator<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, KIND);
    let legacy = a.create_text_label();
    let done = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, legacy);
    let fixed = a.create_text_label();
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Below, fixed);
    a.jcc_label(C::Equal, fixed);
    moving_round::require_coordinator(a, l);
    a.jmp_label(done);
    a.bind_text_label(fixed);
    load_rt(a, R::V0, COORDINATOR);
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, allocation_records::REVISION);
    load_rt(a, R::V1, BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    request(a);
    load_rt(a, R::V1, hs(offset_of!(HandshakeState, generation)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.jmp_label(done);
    a.bind_text_label(legacy);
    load_rt(a, R::V0, RESUMING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    request(a);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(done);
}
pub(super) fn publish_begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    load_rt(a, R::V0, KIND);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    participants::begin(a, l);
    a.bind_text_label(done);
}
pub(super) fn publish_finish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    load_rt(a, R::V0, KIND);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    participants::finish(a, l);
    a.bind_text_label(done);
}
fn finish<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    start: bool,
    closed: bool,
    young: bool,
) {
    if closed {
        root_work::require_empty(a, l);
    } else {
        require_legacy_handoff(a, l);
    }
    for (source, target) in [
        (root_work::PUBLISHED, CLOSED_PUBLICATIONS),
        (root_work::ACKNOWLEDGED, CLOSED_ACKNOWLEDGED),
        (
            if young {
                young_mark::WORKER_UNITS
            } else {
                RT_DM_WORKER
            },
            CLOSED_WORKER,
        ),
    ] {
        lea_rt(a, R::V1, source);
        a.atomic_load_acquire(R::V0, R::V1);
        store_rt(a, target, R::V0);
    }
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, expected)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1); // coordinator already adopts its own publications
    store_rt(a, RESUMING, R::V0);
    participants::finish(a, l);
    old_tlab::restore(a, l);
    copy::bump(a, COORDINATIONS, R::V0);
    store_rt_imm(a, OWN_KIND, 0);
    copy::bump(
        a,
        if start {
            if young { Y_STARTS } else { STARTS }
        } else if closed {
            if young { Y_ENDS } else { ENDS }
        } else {
            if young { Y_RETRIES } else { END_RETRIES }
        },
        R::V0,
    );
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, generation)));
    store_rt(a, quiescence::COMPLETED, R::V0);
    store_rt_imm(a, KIND, 0);
    store_rt_imm(a, COORDINATOR, 0);
    a.mov_imm64(R::V0, 0);
    release_request(a);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for (label, observed, start, young) in [
        (l.old_start_actor, l.old_start_observed_actor, true, false),
        (l.old_end_actor, l.old_end_observed_actor, false, false),
        (
            l.young_start_actor,
            l.young_start_observed_actor,
            true,
            true,
        ),
        (l.young_end_actor, l.young_end_observed_actor, false, true),
    ] {
        let cycle = if young {
            young_mark::EPOCH
        } else {
            RT_MARK_CYCLES
        };
        let active = if young {
            young_mark::ACTIVE
        } else {
            RT_MARK_ACTIVE
        };
        let locked = a.create_text_label();
        a.bind_text_label(observed);
        #[cfg(test)]
        if young
            && start
            && let Some(hook) = l.young_observed_entry_hook
        {
            a.call_label(hook);
        }
        quiescence::save(a);
        owner_state::bind_running(a, l);
        store_rt(a, REQUESTED_CYCLE, R::V0);
        #[cfg(test)]
        if !start && let Some(hook) = l.weak_end_enter_hook {
            a.call_label(hook);
        }
        worker::lock(a, l);
        a.jmp_label(locked);
        a.bind_text_label(label);
        quiescence::save(a);
        owner_state::bind_running(a, l);
        #[cfg(test)]
        if !start && let Some(hook) = l.weak_end_enter_hook {
            a.call_label(hook);
        }
        worker::lock(a, l);
        load_rt(a, R::V0, cycle);
        store_rt(a, REQUESTED_CYCLE, R::V0);
        a.jmp_label(locked);
        let retry = a.create_text_label();
        let wait_resume = a.create_text_label();
        let begin = a.create_text_label();
        let complete = a.create_text_label();
        let waiting = a.create_text_label();
        let release = a.create_text_label();
        let stale = a.create_text_label();
        a.bind_text_label(retry);
        worker::lock(a, l);
        a.bind_text_label(locked);
        if young {
            load_rt(a, R::V0, cooperative_old::YOUNG_ENABLED);
            a.cmp_reg_imm8(R::V0, 1);
            a.jcc_label(C::NotEqual, l.fail);
        }
        request(a);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, wait_resume);
        load_rt(a, R::V0, RESUMING);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, begin);
        copy::bump(a, RESUME_WAITS, R::V0);
        a.jmp_label(wait_resume); // previous real peers must adopt before next begin
        a.bind_text_label(wait_resume);
        worker::unlock(a, l);
        a.call_label(l.owner_checkpoint_poll);
        if let Some(w) = &l.worker {
            a.call_label(w.hooks.relax);
        }
        a.jmp_label(retry);
        a.bind_text_label(begin);
        load_rt(a, R::V0, cycle);
        load_rt(a, R::V1, REQUESTED_CYCLE);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Below, l.fail); // no request may name an unobserved future cycle
        a.jcc_label(C::NotEqual, stale);
        load_rt(a, R::V0, active);
        if start {
            // A competing real coordinator may already have started the cycle.
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, release);
        } else {
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, release);
        }
        native_io::require_phase_clear(a, l);
        round_eligibility(a, l, start, young);
        if start && !young {
            let delayed = a.create_text_label();
            load_rt(a, R::V0, cooperative_old::ENABLED);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, delayed);
            a.call_label(l.mark_prepare_begin);
            a.bind_text_label(delayed);
        }
        quiescence::prepare_old(a, l);
        participants::step(a, l, 6);
        store_rt_imm(
            a,
            KIND,
            if young {
                if start { 3 } else { 4 }
            } else if start {
                1
            } else {
                2
            },
        );
        store_rt(a, COORDINATOR, R::OwnerContext);
        load_rt(a, R::V0, hs(offset_of!(HandshakeState, generation)));
        release_request(a);
        prepare_peer(a, l);
        participants::step(a, l, 1);
        copy::bump(a, quiescence::OWN_ACKS, R::V0);
        a.bind_text_label(waiting);
        load_rt(a, R::V0, hs(offset_of!(HandshakeState, acknowledged)));
        load_rt(a, R::V1, hs(offset_of!(HandshakeState, expected)));
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Equal, complete);
        a.jcc_label(C::Above, l.fail);
        copy::bump(a, quiescence::RETRIES, R::V0);
        worker::unlock(a, l);
        if let Some(w) = &l.worker {
            a.call_label(w.hooks.relax);
        }
        worker::lock(a, l);
        a.jmp_label(waiting);
        a.bind_text_label(complete);
        require_legacy_handoff(a, l);
        load_rt(a, R::V0, hs(offset_of!(HandshakeState, expected)));
        store_rt(a, quiescence::EXPECTED, R::V0);
        store_rt(a, quiescence::ACKNOWLEDGED, R::V0);
        if start {
            if young {
                store_rt_imm(a, young_mark::VALID, 0);
                cooperative_young::begin_cycle(a);
                young_mark::start_ready(a, l);
            } else {
                let ready = a.create_text_label();
                load_rt(a, R::V0, cooperative_old::ENABLED);
                a.test_reg_reg(R::V0, R::V0);
                a.jcc_label(C::Equal, ready);
                store_rt_imm(a, RT_MARK_VALID, 0);
                a.call_label(l.mark_prepare_begin);
                cooperative_old::begin_cycle(a);
                a.bind_text_label(ready);
                mark::start_ready(a, l);
            }
            finish(a, l, true, false, young);
        } else if young {
            young_end_check(a, l);
        } else {
            end_check(a, l);
        }
        a.jmp_label(release);
        a.bind_text_label(stale);
        copy::bump(
            a,
            if young {
                if start { Y_STALE_STARTS } else { Y_STALE_ENDS }
            } else if start {
                STALE_STARTS
            } else {
                STALE_ENDS
            },
            R::V0,
        );
        a.bind_text_label(release);
        worker::unlock(a, l);
        worker::wake_if_pending(a, l);
        quiescence::restore(a);
        a.ret();
    }
}

/// Fixed-count termination check. No receiver/queue/root traversal is done
/// with peers parked: incomplete work reopens ConcurrentMark and resumes them.
fn end_check<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let pending = a.create_text_label();
    let done = a.create_text_label();
    copy::bump(a, END_CHECKS, R::V0);
    phase::marking(a, l, crate::gc_abi::Phase::MarkEndCheck);
    satb_buffer::pending(a, false, pending);
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    for f in [RT_M_OBJ, RT_MSTACK_LEN, mark_queue::count(false)] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, pending);
    }
    registered_roots::pending(a, registered_roots::OLD, pending);
    root_work::require_empty(a, l);
    weak::seal(a, false, pending);
    store_rt_imm(a, RT_MARK_ACTIVE, 0);
    store_rt_imm(a, RT_MARK_DONE, 0);
    store_rt_imm(a, RT_MARK_VALID, 1);
    phase::marking(a, l, crate::gc_abi::Phase::Select);
    finish(a, l, false, true, false);
    a.jmp_label(done);
    a.bind_text_label(pending);
    store_rt_imm(a, RT_MARK_DONE, 0);
    phase::marking(a, l, crate::gc_abi::Phase::ConcurrentMark);
    worker::request_work(a);
    finish(a, l, false, false, false);
    a.bind_text_label(done);
}
fn require_legacy_handoff<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let retained = a.create_text_label();
    cooperative_old::branch_enabled(a, retained);
    root_work::require_empty(a, l);
    a.bind_text_label(retained);
}
/// A generation-specific rendezvous cannot publish another generation or movement.
pub(super) fn allow_stage<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    young: Option<crate::gc_abi::Phase>,
    old: Option<crate::gc_abi::Phase>,
) {
    load_rt(a, R::V0, KIND);
    let fixed = a.create_text_label();
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Below, fixed);
    a.jcc_label(C::Equal, fixed);
    let accepted = a.create_text_label();
    use crate::gc_abi::Phase as P;
    if matches!(
        (young, old),
        (
            Some(P::RelocateStart | P::ConcurrentRelocate),
            Some(P::Idle | P::ConcurrentMark | P::RelocateStart | P::ConcurrentRelocate)
        )
    ) {
        a.cmp_reg_imm32(R::V0, 5);
        a.jcc_label(C::Equal, accepted);
    }
    if matches!(
        (young, old),
        (
            Some(P::Retire),
            Some(P::Idle | P::ConcurrentMark | P::Retire)
        )
    ) {
        a.cmp_reg_imm32(R::V0, 6);
        a.jcc_label(C::Equal, accepted);
    }
    if matches!(
        (young, old),
        (Some(P::Idle), Some(P::Idle | P::ConcurrentMark))
    ) {
        a.cmp_reg_imm32(R::V0, 7);
        a.jcc_label(C::Equal, accepted);
    }
    a.jmp_label(l.fail);
    a.bind_text_label(accepted);
    let moving_done = a.create_text_label();
    a.jmp_label(moving_done);
    a.bind_text_label(fixed);
    load_rt(a, R::V0, KIND);
    let done = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    use crate::gc_abi::Phase;
    let stage = young.or(old);
    let base = if young.is_some() && old.is_none() {
        2
    } else {
        0
    };
    let expected = match stage {
        Some(Phase::MarkStart | Phase::ConcurrentMark) => base + 1,
        Some(Phase::MarkEndCheck | Phase::Select) => base + 2,
        _ => 0,
    };
    if young.is_some() && old.is_some() {
        a.jmp_label(l.fail);
    }
    if stage == Some(Phase::ConcurrentMark) {
        a.cmp_reg_imm32(R::V0, base + 1);
        a.jcc_label(C::Equal, done);
        a.cmp_reg_imm32(R::V0, base + 2);
    } else {
        a.cmp_reg_imm32(R::V0, expected);
    }
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(done);
    a.bind_text_label(moving_done);
}

fn round_eligibility<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    start: bool,
    young: bool,
) {
    use crate::gc_abi::Phase;
    if young {
        load_rt(a, R::V0, cooperative_old::YOUNG_ENABLED);
        a.cmp_reg_imm8(R::V0, 1);
        a.jcc_label(C::NotEqual, l.fail);
    }
    let strict = a.create_text_label();
    let stages = a.create_text_label();
    load_rt(a, R::V0, cooperative_old::YOUNG_ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, strict);
    journal_snapshot::lifetime(a, l);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.mov_reg_reg(R::V1, R::V0);
    native_io::adoption::dual_stable(a, l);
    load_rt(a, R::V2, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    if young {
        a.shr_reg_imm8(R::V2, 4);
    }
    a.and_reg_imm32(R::V2, 15);
    journal_snapshot::stable_stage(a, l);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    if !young {
        a.shr_reg_imm8(R::V0, 4);
    }
    a.and_reg_imm32(R::V0, 15);
    a.jmp_label(stages);
    a.bind_text_label(strict);
    load_rt(a, R::V0, young_mark::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.and_reg_imm32(R::V0, 255);
    a.mov_reg_reg(R::V2, R::V0);
    a.and_reg_imm32(R::V2, 15);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    a.shr_reg_imm8(R::V0, 4);
    a.bind_text_label(stages);
    if start {
        let initial = a.create_text_label();
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, initial);
        a.cmp_reg_imm8(R::V0, Phase::Select as i8);
        a.jcc_label(C::NotEqual, l.fail);
        load_rt(a, R::V0, cooperative_old::ENABLED);
        a.cmp_reg_imm8(R::V0, 1);
        a.jcc_label(C::NotEqual, l.fail);
        load_rt(
            a,
            R::V0,
            if young {
                young_mark::VALID
            } else {
                RT_MARK_VALID
            },
        );
        a.cmp_reg_imm8(R::V0, 1);
        a.jcc_label(C::NotEqual, l.fail);
        a.bind_text_label(initial);
    } else {
        a.cmp_reg_imm8(R::V0, Phase::ConcurrentMark as i8);
        a.jcc_label(C::NotEqual, l.fail);
    }
    for f in [RT_PHASE_OPEN, RT_RETIRE_STAGE] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
}

fn young_end_check<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    use crate::gc_abi::Phase;
    let pending = a.create_text_label();
    let done = a.create_text_label();
    copy::bump(a, Y_CHECKS, R::V0);
    phase::young_marking(a, l, Phase::MarkEndCheck);
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    young_mark::pending(a, pending);
    root_work::require_empty(a, l);
    weak::seal(a, true, pending);
    young_mark::seal_ready(a, l);
    finish(a, l, false, true, true);
    a.jmp_label(done);
    a.bind_text_label(pending);
    store_rt_imm(a, young_mark::DONE, 0);
    phase::young_marking(a, l, Phase::ConcurrentMark);
    worker::request_work(a);
    finish(a, l, false, false, true);
    a.bind_text_label(done);
}
