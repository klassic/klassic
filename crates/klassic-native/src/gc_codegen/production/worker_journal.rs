//! Independent, bounded journal transport for enrolled nonmoving heaps.
//! The worker holds the collector ticket, uses shared control storage and
//! retains a real inventory reader lease across quanta. It never borrows an
//! owner's pending token or producer scratch. Unpublished objects defer the
//! record before any journal claim, so their producer remains free to publish.
use super::*;
use crate::gc_abi::{
    BufferCursor, OBJECT_PUBLISHED, RootJournalRequest, RootLease, RootRegistration, RootRegistry,
    RootWalk,
};

#[repr(C)]
pub(super) struct State {
    walk: RootWalk,
    request: RootJournalRequest,
    lease: RootLease,
    change: RootChange,
    retry: u64,
    pass_progress: u64,
    pub(super) entries: u64,
    pub(super) regions: u64,
    pub(super) deferred: u64,
    pub(super) claimed_skips: u64,
}
const BASE: usize = offset_of!(Runtime, worker_journal);
const WALK: usize = BASE + offset_of!(State, walk);
const REQUEST: usize = BASE + offset_of!(State, request);
const LEASE: usize = BASE + offset_of!(State, lease);
const CHANGE: usize = BASE + offset_of!(State, change);
pub(super) const RETRY: usize = BASE + offset_of!(State, retry);
const PASS_PROGRESS: usize = BASE + offset_of!(State, pass_progress);
const REG: usize = WALK + offset_of!(RootWalk, lease) + offset_of!(RootLease, registration);

fn advance<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    lea_rt(a, R::V7, registered_roots::REGISTRY);
    lea_rt(a, R::V6, WALK);
    a.mov_imm64(R::V2, op);
    a.call_label(l.root_walk_step);
    let valid = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, valid);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(valid);
}
fn next<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    // Journal discovery claims no generation's cell-scan completion.
    store_rt_imm(a, WALK + offset_of!(RootWalk, mask), 0);
    store_rt_imm(a, WALK + offset_of!(RootWalk, completed_mask), 0);
    load_rt(a, R::V0, WALK + offset_of!(RootWalk, limit));
    store_rt(a, WALK + offset_of!(RootWalk, cursor), R::V0);
    advance(a, l, 1);
}
/// Stop visits at most one remaining inventory link per ticket hold. The
/// worker cannot return to its OS caller with a retained reader lease.
pub(super) fn shutdown<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, pending: E::TextLabel) {
    let empty = a.create_text_label();
    load_rt(a, R::V0, WALK + offset_of!(RootWalk, active));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    next(a, l);
    a.jmp_label(pending);
    a.bind_text_label(empty);
}
fn step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    lea_rt(a, R::V7, registered_roots::REGISTRY);
    lea_rt(a, R::V6, REQUEST);
    a.mov_imm64(R::V2, op);
    a.call_label(l.root_journal_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.worker_journal_step);
    let done = a.create_text_label();
    let visit = a.create_text_label();
    let advance_region = a.create_text_label();
    let phase_blocked = a.create_text_label();
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    store_rt_imm(a, RETRY, 0);
    #[cfg(test)]
    if let Some(gate) = l.journal_worker_gate {
        a.call_label(gate);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, done);
    }
    load_rt(a, R::V0, cooperative_old::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    let moving = a.create_text_label();
    let admitted = a.create_text_label();
    moving_round::branch_enabled(a, moving);
    // Phase publication and own-list sealing keep their existing exclusion.
    for field in [quiescence::REQUEST, phase_round::RESUMING, RT_PHASE_OPEN] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, phase_blocked);
    }
    a.jmp_label(admitted);
    a.bind_text_label(moving);
    load_rt(a, R::V0, phase_round::KIND);
    a.cmp_reg_imm32(R::V0, 5);
    let no_start = a.create_text_label();
    a.jcc_label(C::NotEqual, no_start);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, phase_blocked);
    a.bind_text_label(no_start);
    a.bind_text_label(admitted);
    let dual = a.create_text_label();
    load_rt(a, R::V0, cooperative_old::YOUNG_ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, dual);
    load_rt(a, R::V0, young_mark::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.bind_text_label(dual);
    load_rt(a, R::V0, WALK + offset_of!(RootWalk, active));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, visit);
    lea_rt(a, R::V0, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    store_rt_imm(a, PASS_PROGRESS, 0);
    advance(a, l, 0);
    load_rt(a, R::V0, WALK + offset_of!(RootWalk, active));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.bind_text_label(visit);
    copy::bump(a, BASE + offset_of!(State, regions), R::V0);
    load_rt(a, R::V0, REG);
    a.call_label(l.worker_journal_consume_region);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, advance_region);
    store_rt_imm(a, PASS_PROGRESS, 1);
    store_rt_imm(a, RETRY, 1);
    a.bind_text_label(advance_region);
    next(a, l);
    load_rt(a, R::V0, WALK + offset_of!(RootWalk, active));
    load_rt(a, R::V1, RETRY);
    a.or_reg_reg(R::V0, R::V1);
    store_rt(a, RETRY, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    // A last empty region must not forget earlier actual ACK progress in
    // this sweep. Retry remaining debt without a timed park; a whole sweep
    // with no admission may park rather than spin on an unready/busy entry.
    load_rt(a, R::V0, PASS_PROGRESS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    store_rt_imm(a, RETRY, 1);
    a.jmp_label(done);
    a.bind_text_label(phase_blocked);
    // A phase edge temporarily excludes this consumer, not its ready debt.
    // Parking here can repeatedly miss the short eligible window between
    // peer resumption and the next end check on a single CPU.
    lea_rt(a, R::V1, root_work::PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    store_rt_imm(a, RETRY, 1);
    a.bind_text_label(done);
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
    a.ret();
    consume_region(a, l);
}

/// V0 is a registered region retained by a real inventory reader or a queued
/// retirement Node. Ticket held. One actual journal record; V0 reports progress.
fn consume_region<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.worker_journal_consume_region);
    let empty = a.create_text_label();
    let deferred = a.create_text_label();
    let claimed = a.create_text_label();
    let done = a.create_text_label();
    let saved = [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11];
    for r in saved {
        a.push_reg(r);
    }
    a.mov_reg_reg(R::V8, R::V0);
    a.load_ptr_disp32(
        R::V0,
        R::V8,
        d(offset_of!(RootRegistration, journal_serial)),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, claimed);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootRegistration, journal_end)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V9, R::V8, d(offset_of!(RootRegistration, segment)));
    a.load_ptr_disp32(R::V9, R::V9, d(offset_of!(RootSegment, journal)));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, empty);
    a.load_ptr_disp32(R::V10, R::V9, d(offset_of!(BufferCursor, consumed)));
    a.mov_reg_reg(R::V0, R::V9);
    a.add_reg_imm32(R::V0, d(offset_of!(BufferCursor, published)));
    a.atomic_load_acquire(R::V0, R::V0);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Above, l.fail);
    a.jcc_label(C::Equal, empty);
    a.sub_reg_reg(R::V0, R::V10);
    a.load_ptr_disp32(R::V1, R::V9, d(offset_of!(BufferCursor, capacity)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    // A stable preflight read is safe: every enrolled consumer holds this
    // ticket and the producer cannot reuse the unacknowledged ring entry.
    a.mov_reg_reg(R::V0, R::V10);
    a.div_unsigned(R::V1);
    a.shl_reg_imm8(R::V2, 5);
    a.load_ptr_disp32(R::V9, R::V9, d(offset_of!(BufferCursor, entries)));
    a.add_reg_reg(R::V9, R::V2);
    for (field, current) in [
        (offset_of!(RootChange, scope), None),
        (
            offset_of!(RootChange, phase),
            Some(registered_roots::REGISTRY + offset_of!(RootRegistry, phase)),
        ),
    ] {
        a.load_ptr_disp32(R::V0, R::V9, d(field));
        if let Some(current) = current {
            load_rt(a, R::V1, current);
        } else {
            a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(RootRegistration, scope)));
        }
        if field == offset_of!(RootChange, phase) {
            journal_snapshot::validate(a, l);
        } else {
            a.cmp_reg_reg(R::V0, R::V1);
            a.jcc_label(C::NotEqual, l.fail);
        }
    }
    for field in [
        offset_of!(RootChange, previous),
        offset_of!(RootChange, next),
    ] {
        let ready = a.create_text_label();
        a.load_ptr_disp32(R::V0, R::V9, d(field));
        fields::offset(a, R::V0, R::V2, ready);
        load_rt(a, R::V1, RT_HEAP_BASE);
        a.add_reg_reg(R::V1, R::V2);
        a.add_reg_imm32(
            R::V1,
            d(offset_of!(ObjectHeader, state)) - size_of::<ObjectHeader>() as i32,
        );
        a.atomic_load_acquire(R::V0, R::V1);
        a.cmp_reg_imm8(R::V0, OBJECT_PUBLISHED as i8);
        a.jcc_label(C::NotEqual, deferred);
        a.bind_text_label(ready);
    }
    store_rt(
        a,
        REQUEST + offset_of!(RootJournalRequest, registration),
        R::V8,
    );
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(RootRegistration, scope)));
    store_rt(a, REQUEST + offset_of!(RootJournalRequest, scope), R::V0);
    store_rt_addr(a, REQUEST + offset_of!(RootJournalRequest, lease), LEASE);
    store_rt_addr(a, REQUEST + offset_of!(RootJournalRequest, output), CHANGE);
    store_rt_imm(a, REQUEST + offset_of!(RootJournalRequest, capacity), 1);
    step(a, l, 0); // Real kind-4 storage claim; no unlock until ACK.
    step(a, l, 1);
    if let Some(w) = &l.worker {
        for field in [
            offset_of!(RootChange, previous),
            offset_of!(RootChange, next),
        ] {
            let next = a.create_text_label();
            load_rt(a, R::V0, CHANGE + field);
            fields::offset(a, R::V0, R::V2, next);
            load_rt(a, R::V0, RT_MARK_ACTIVE);
            a.test_reg_reg(R::V0, R::V0);
            let young = a.create_text_label();
            a.jcc_label(C::Equal, young);
            a.call_label(w.mark_ref);
            store_rt_imm(a, RT_MARK_DONE, 0);
            a.bind_text_label(young);
            a.call_label(w.young.reference);
            a.bind_text_label(next);
        }
    }
    step(a, l, 2); // Both graph admissions precede real ACK and ledger debit.
    copy::bump(a, BASE + offset_of!(State, entries), R::V0);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(deferred);
    copy::bump(a, BASE + offset_of!(State, deferred), R::V0);
    a.jmp_label(empty);
    a.bind_text_label(claimed);
    copy::bump(a, BASE + offset_of!(State, claimed_skips), R::V0);
    a.bind_text_label(empty);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();
}
