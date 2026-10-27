//! Actual journal handoff for every registered region of the executing owner.
//! Foreign BlockedNative regions may be passed only with no pending journal,
//! private producer/access, TLAB or live claim. Their entries are never dropped.
//!
//! The owner holds the collector lock and has no private producer/access guard.
//! Full seals require initialization publication; pressure retains its token.
//! New handoffs pin each copied record with a real registered claim until both
//! sides have been offered to the active production mark queues. Already-owned
//! records have a separate validated owner-only pressure completion path.
//! One record is copied per claim; lock holds contain at most SLICE records.
//! Native boundaries seal initialized journals; pressure consumes just one
//! record without yielding inside its caller's scan/copy. Initializing values
//! stay owned by the actual pending allocation and its verified black stamps.
//! Multi-region seals retain a real discovery cursor; empty region transitions
//! and record admissions both cost a slice unit. This does not admit concurrent
//! owners or arbitrary deferred producers across moving phases.
use super::*;
use crate::gc_codegen::production::{OwnerState, lea_rt, load_rt, owner_state, store_rt};
#[path = "root_journal_inventory.rs"]
mod inventory;
#[path = "root_journal_native.rs"]
mod native;
#[path = "root_journal_owner.rs"]
mod owner;
#[path = "root_journal_protected.rs"]
mod protected;
use crate::gc_abi::{RootChange, RootJournalRequest, RootLease, RootRegistration};

#[repr(C)]
pub(super) struct State {
    request: RootJournalRequest,
    lease: RootLease,
    change: RootChange,
    remaining: u64,
    pub(super) entries: u64,
    pub(super) old_offers: u64,
    pub(super) young_offers: u64,
    pub(super) yields: u64,
    owner_locked: u64,
    pressure: u64,
    pub(super) pending_offers: u64,
    pub(super) fast_entries: u64,
    inventory: crate::gc_abi::RootWalk,
    pub(super) regions: u64,
    own_cursor: u64,
    own_previous: u64,
    own_scope: u64,
}
const BASE: usize = owner_state::offset(offset_of!(OwnerState, root_journal));
const REQUEST: usize = BASE + offset_of!(State, request);
const LEASE: usize = BASE + offset_of!(State, lease);
const CHANGE: usize = BASE + offset_of!(State, change);
const REMAINING: usize = BASE + offset_of!(State, remaining);
pub(super) const SLICE: u64 = 16;
pub(super) const OWNER_LOCKED: usize = BASE + offset_of!(State, owner_locked);
const PRESSURE: usize = BASE + offset_of!(State, pressure);

fn step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    lea_rt(a, R::V7, registered_roots::REGISTRY);
    lea_rt(a, R::V6, REQUEST);
    a.mov_imm64(R::V2, op);
    a.call_label(l.root_journal_step);
}
fn check<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    protected::emit(a, l);
    let common = a.create_text_label();
    let multiple = a.create_text_label();
    owner::emit(a, l, common);
    a.bind_text_label(l.root_journal_seal);
    store_rt_imm(a, PRESSURE, 0);
    store_rt_imm(a, REMAINING, SLICE);
    lea_rt(a, R::V0, registered_roots::registration(a));
    load_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, inventory),
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, multiple);
    a.load_ptr_disp32(
        R::V1,
        R::V0,
        d(offset_of!(RootRegistration, inventory_next)),
    );
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, common);
    a.bind_text_label(multiple);
    inventory::emit(a, l, common);
    a.bind_text_label(l.root_journal_pressure);
    lea_rt(a, R::V0, registered_roots::registration(a));
    a.bind_text_label(l.root_journal_pressure_region);
    a.mov_imm64(R::V1, 1);
    store_rt(a, PRESSURE, R::V1);
    a.mov_imm64(R::V1, SLICE);
    store_rt(a, REMAINING, R::V1);
    a.bind_text_label(common);
    store_rt(
        a,
        REQUEST + offset_of!(RootJournalRequest, registration),
        R::V0,
    );
    load_rt(a, R::V0, OWNER_LOCKED);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    let empty = a.create_text_label();
    // Most phase edges have no owner writes between them. Avoid building a
    // request or entering the generic claim leaf for that empty transport.
    for field in [
        offset_of!(RootRegistration, journal_serial),
        offset_of!(RootRegistration, journal_end),
    ] {
        load_rt(
            a,
            R::V0,
            REQUEST + offset_of!(RootJournalRequest, registration),
        );
        a.load_ptr_disp32(R::V0, R::V0, d(field));
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(
        a,
        R::V0,
        REQUEST + offset_of!(RootJournalRequest, registration),
    );
    a.load_ptr_disp32(R::V1, R::V0, d(offset_of!(RootRegistration, context)));
    lea_rt(a, R::V2, RT_CTX);
    a.cmp_reg_reg(R::V1, R::V2);
    let owned = a.create_text_label();
    a.jcc_label(C::Equal, owned);
    native::empty_excluded(a, l);
    a.jmp_label(empty);
    a.bind_text_label(owned);
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, segment)));
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootSegment, journal)));
    // A prepared read-only region may have no journal. It has no producer
    // obligation; existing claimed serial/end must still have been zero.
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    a.load_ptr_disp32(
        R::V1,
        R::V0,
        d(offset_of!(crate::gc_abi::BufferCursor, published)),
    );
    a.load_ptr_disp32(
        R::V0,
        R::V0,
        d(offset_of!(crate::gc_abi::BufferCursor, consumed)),
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, empty);
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    let allow_pending = a.create_text_label();
    load_rt(a, R::V0, PRESSURE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, allow_pending);
    load_rt(a, R::V0, RT_PENDING_REF);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(allow_pending);
    for field in [
        RT_CTX + offset_of!(MutatorContext, private_producers),
        RT_CTX + offset_of!(MutatorContext, access_epoch),
    ] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    store_rt_addr(a, REQUEST + offset_of!(RootJournalRequest, lease), LEASE);
    store_rt_addr(a, REQUEST + offset_of!(RootJournalRequest, output), CHANGE);
    store_rt_imm(a, REQUEST + offset_of!(RootJournalRequest, capacity), 1);
    load_rt(
        a,
        R::V0,
        REQUEST + offset_of!(RootJournalRequest, registration),
    );
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, scope)));
    store_rt(a, REQUEST + offset_of!(RootJournalRequest, scope), R::V0);
    let again = a.create_text_label();
    let done = a.create_text_label();
    let claimed = a.create_text_label();
    a.bind_text_label(again);
    step(a, l, 0);
    a.cmp_reg_imm8(R::V0, 2); // EMPTY: no outstanding ring or lease obligation.
    a.jcc_label(C::Equal, done);
    check(a, l);
    load_rt(a, R::V0, REMAINING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, claimed);
    // The real claim pins this entry across unlock. The worker can finish
    // already-published graph jobs but cannot mutate the owner's phase/journal.
    yield_slice(a, l);
    a.bind_text_label(claimed);
    step(a, l, 1);
    check(a, l);
    for (entry, current) in [
        (
            CHANGE + offset_of!(RootChange, scope),
            REQUEST + offset_of!(RootJournalRequest, scope),
        ),
        (
            CHANGE + offset_of!(RootChange, phase),
            RT_CTX + offset_of!(MutatorContext, adopted_phase),
        ),
    ] {
        load_rt(a, R::V0, entry);
        load_rt(a, R::V1, current);
        if entry == CHANGE + offset_of!(RootChange, phase) {
            journal_snapshot::validate(a, l);
        } else {
            a.cmp_reg_reg(R::V0, R::V1);
            a.jcc_label(C::NotEqual, l.fail);
        }
    }
    if let Some(w) = &l.worker {
        for field in [
            offset_of!(RootChange, previous),
            offset_of!(RootChange, next),
        ] {
            let next = a.create_text_label();
            let young = a.create_text_label();
            load_rt(a, R::V0, CHANGE + field);
            fields::offset(a, R::V0, R::V2, next);
            let ready = a.create_text_label();
            let pending_young = a.create_text_label();
            let pending_done = a.create_text_label();
            load_rt(a, R::V1, RT_PENDING_REF);
            a.shr_reg_imm8(R::V1, 16);
            a.cmp_reg_reg(R::V2, R::V1);
            a.jcc_label(C::NotEqual, ready);
            // Do not publish or inspect an incomplete payload. The real
            // allocation token remains live and phase adoption requires its
            // publication. Every active generation must already own its black
            // membership; an unknown initializing value is never queued.
            load_rt(a, R::V0, RT_MARK_ACTIVE);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, pending_young);
            mark::test_mark(a, R::V2, R::V6, R::V7);
            a.jcc_label(C::Equal, l.fail);
            a.bind_text_label(pending_young);
            load_rt(a, R::V0, young_mark::ACTIVE);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, pending_done);
            generation::test(a, R::V2, generation::YOUNG);
            a.jcc_label(C::NotEqual, pending_done);
            young_mark::test(a, R::V2, R::V6, R::V7);
            a.jcc_label(C::Equal, l.fail);
            a.bind_text_label(pending_done);
            copy::bump(a, BASE + offset_of!(State, pending_offers), R::V0);
            a.jmp_label(next);
            a.bind_text_label(ready);
            load_rt(a, R::V0, RT_MARK_ACTIVE);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, young);
            let known = a.create_text_label();
            mark::test_mark(a, R::V2, R::V6, R::V7);
            a.jcc_label(C::NotEqual, known);
            a.call_label(w.mark_ref); // V2 survives; actual queue/epoch admission.
            store_rt_imm(a, RT_MARK_DONE, 0);
            a.bind_text_label(known);
            copy::bump(a, BASE + offset_of!(State, old_offers), R::V0);
            a.bind_text_label(young);
            load_rt(a, R::V0, young_mark::ACTIVE);
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::Equal, next);
            a.call_label(w.young.reference);
            copy::bump(a, BASE + offset_of!(State, young_offers), R::V0);
            a.bind_text_label(next);
        }
        worker::request_work(a);
    }
    // Only now may the producer reuse the ring slot and storage lease.
    step(a, l, 2);
    check(a, l);
    copy::bump(a, BASE + offset_of!(State, entries), R::V0);
    load_rt(a, R::V0, PRESSURE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    load_rt(a, R::V0, REMAINING);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, REMAINING, R::V0);
    a.jmp_label(again);
    a.bind_text_label(done);
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
    a.bind_text_label(empty);
    a.ret();
}

/// Retains the current real storage claim/cursor while the worker runs.
fn yield_slice<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    copy::bump(a, BASE + offset_of!(State, yields), R::V0);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.relax);
    }
    worker::lock(a, l);
    store_rt_imm(a, REMAINING, SLICE);
}
