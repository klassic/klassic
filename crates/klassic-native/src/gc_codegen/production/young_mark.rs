//! Young-only marking with its own epoch, queue and root/slot cursors.
//! Old objects are not traversed: their Young edges enter through remembered
//! slots. An allocation resource wait precedes relocation and never traces
//! while holding the collector's moving-phase pause.
use super::{Labels as RuntimeLabels, Runtime, load_rt, store_rt};
use crate::gc_abi::{MarkRecord, ObjectHeader, Phase};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

#[repr(C)]
pub(super) struct State {
    pub active: u64,
    pub done: u64,
    pub valid: u64,
    pub epoch: u64,
    pub marked: u64,
    pub len: u64,
    pub object: u64,
    pub field: u64,
    pub count: u64,
    pub root: u64,
    pub root_bound: u64,
    pub remset: u64,
    pub remset_bound: u64,
    pub remset_base: u64,
    pub old_index: u64,
    pub old_bound: u64,
    pub turn: u64,
    pub worker_units: u64,
    pub waits: u64,
    pub unmarked: u64,
    pub worker_unmarked: u64,
    #[cfg(test)]
    pub auto_disabled: u64,
}
macro_rules! field {
    ($($name:ident: $member:ident),* $(,)?) => { $(
        pub(super) const $name: usize = offset_of!(Runtime, young_mark.$member);
    )* };
}
field! { ACTIVE:active, DONE:done, VALID:valid, EPOCH:epoch, MARKED:marked,
LEN:len, OBJECT:object, FIELD:field, COUNT:count, ROOT:root, ROOT_BOUND:root_bound,
REMSET:remset, REMSET_BOUND:remset_bound, REMSET_BASE:remset_base,
OLD_INDEX:old_index, OLD_BOUND:old_bound, TURN:turn, WORKER_UNITS:worker_units, WAITS:waits, UNMARKED:unmarked, WORKER_UNMARKED:worker_unmarked }
#[cfg(test)]
pub(super) const AUTO_DISABLED: usize = offset_of!(Runtime, young_mark.auto_disabled);

pub(super) struct Labels<T> {
    pub start: T,
    pub reference: T,
    pub trace: T,
}
impl<T> Labels<T> {
    pub fn new(create: &mut impl FnMut() -> T) -> Self {
        Self {
            start: create(),
            reference: create(),
            trace: create(),
        }
    }
}
const RECORD: i32 = offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32;
const STAMP: i32 = offset_of!(MarkRecord, young_epoch) as i32;

/// Same register/flag contract as the Old epoch check, independently stamped.
pub(super) fn test<E: PortableAsm>(a: &mut E, off: R, epoch: R, record: R) {
    let no = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, epoch, EPOCH);
    a.test_reg_reg(epoch, epoch);
    a.jcc_label(C::Equal, no);
    load_rt(a, record, super::RT_HEAP_BASE);
    a.add_reg_reg(record, off);
    a.load_ptr_disp32(record, record, RECORD);
    a.test_reg_reg(record, record);
    a.jcc_label(C::Equal, no);
    a.add_reg_imm32(record, STAMP);
    a.atomic_load_acquire(record, record);
    a.cmp_reg_reg(record, epoch);
    a.jcc_label(C::NotEqual, no);
    a.mov_imm64(record, 1);
    a.jmp_label(done);
    a.bind_text_label(no);
    a.mov_imm64(record, 0);
    a.bind_text_label(done);
    a.test_reg_reg(record, record);
}
fn set<E: PortableAsm>(a: &mut E, off: R) {
    load_rt(a, R::V6, super::RT_HEAP_BASE);
    a.add_reg_reg(R::V6, off);
    a.load_ptr_disp32(R::V6, R::V6, RECORD);
    a.add_reg_imm32(R::V6, STAMP);
    load_rt(a, R::V7, EPOCH);
    a.atomic_store_release(R::V6, R::V7);
}
pub(super) fn alloc_black<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    load_rt(a, R::V2, ACTIVE);
    a.test_reg_reg(R::V2, R::V2);
    let active = a.create_text_label();
    a.jcc_label(C::NotEqual, active);
    load_rt(a, R::V2, super::moving_round::ENABLED);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V2, VALID);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, done);
    for f in [super::RT_PHASE_OPEN, super::RT_RETIRE_STAGE] {
        load_rt(a, R::V2, f);
        a.test_reg_reg(R::V2, R::V2);
        a.jcc_label(C::NotEqual, done);
    }
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, done);
    set(a, R::V2);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    load_rt(a, R::V0, super::RT_HEAP_BASE);
    a.add_reg_reg(R::V0, R::V2);
    a.load_ptr_disp32(R::V0, R::V0, super::copy::PAYLOAD);
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    let positive = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, positive);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(positive);
    a.add_reg_imm32(R::V0, super::copy::HEADER);
    super::copy::add_bytes(a, super::capacity::SEALED, R::V0);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.jmp_label(done);
    a.bind_text_label(active);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, done);
    set(a, R::V2);
    super::capacity::protect(a, true);
    a.bind_text_label(done);
}
/// Only fixed-count frontier metadata, plus the current retained Old receiver.
pub(super) fn pending<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    for f in [
        OBJECT,
        LEN,
        super::mark_queue::count(true),
        super::mark_queue::discovery_frozen(),
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, pending);
    }
    super::registered_roots::pending(a, super::registered_roots::YOUNG, pending);
    for (index, bound) in [(REMSET, REMSET_BOUND), (OLD_INDEX, OLD_BOUND)] {
        load_rt(a, R::V0, index);
        load_rt(a, R::V1, bound);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Below, pending);
    }
    let done = a.create_text_label();
    load_rt(a, R::V0, super::RT_M_OBJ);
    super::fields::young_offset(a, R::V0, R::V2, done);
    test(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::Equal, pending);
    a.bind_text_label(done);
}
/// Allocation stalls here *before* the moving-phase lock/pause. Only the
/// worker traces; the owner does metadata checks and yields. Failed-spawn
/// production never starts this lane; explicit harness starts must finish it.
pub(super) fn wait<E: PortableAsm>(a: &mut E, l: &RuntimeLabels<E::TextLabel>) {
    if l.worker.is_none() {
        return;
    }
    let done = a.create_text_label();
    let again = a.create_text_label();
    let busy = a.create_text_label();
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.call_label(l.publish_pending);
    a.bind_text_label(again);
    super::worker::lock(a, l);
    a.call_label(l.root_journal_seal);
    pending(a, busy);
    load_rt(a, R::V0, super::root_work::PENDING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, busy);
    super::weak::seal(a, true, busy);
    seal_ready(a, l);
    super::worker::unlock(a, l);
    a.jmp_label(done);
    a.bind_text_label(busy);
    load_rt(a, R::V0, super::RT_WORKER_ALIVE);
    a.test_reg_reg(R::V0, R::V0);
    let assisted = a.create_text_label();
    a.jcc_label(C::NotEqual, assisted);
    load_rt(a, R::V0, super::weak::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(l.worker.as_ref().unwrap().young.trace);
    a.bind_text_label(assisted);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    store_rt(a, super::RT_BUDGET, R::V0);
    super::weak::work(a, l);
    a.mov_imm64(R::V0, 0);
    store_rt(a, DONE, R::V0);
    super::copy::bump(a, WAITS, R::V0);
    super::worker::request_work(a);
    super::worker::unlock(a, l);
    super::worker::wake_if_pending(a, l);
    a.call_label(l.worker.as_ref().unwrap().hooks.relax);
    a.jmp_label(again);
    a.bind_text_label(done);
}

pub(super) fn trigger<E: PortableAsm>(a: &mut E, l: &RuntimeLabels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let done = a.create_text_label();
    #[cfg(test)]
    {
        load_rt(a, R::V0, AUTO_DISABLED);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    load_rt(a, R::V0, super::RT_WORKER_ALIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    for f in [ACTIVE, VALID, super::RT_PHASE_OPEN, super::RT_RETIRE_STAGE] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    super::start_policy::below(a, done);
    let started = a.create_text_label();
    load_rt(a, R::V0, super::RT_MP_STATE);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, started);
    a.call_label(w.mark_start);
    a.bind_text_label(started);
    a.call_label(w.young.start);
    a.bind_text_label(done);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &RuntimeLabels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    a.bind_text_label(w.young.reference);
    let known = a.create_text_label();
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, known);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, known);
    test(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::NotEqual, known);
    set(a, R::V2);
    super::capacity::protect(a, false);
    super::copy::bump(a, MARKED, R::V0);
    a.mov_imm64(R::V0, 0);
    store_rt(a, DONE, R::V0);
    a.mov_reg_reg(R::V0, R::V2);
    a.shl_reg_imm8(R::V0, 16);
    super::colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    a.push_reg(R::V2);
    a.push_reg(R::V2);
    super::colours::uncertify_white(a);
    a.pop_reg(R::V2);
    a.pop_reg(R::V2);
    a.mov_reg_reg(R::V8, R::V0);
    load_rt(a, R::V0, LEN);
    let spill = a.create_text_label();
    let retained = a.create_text_label();
    load_rt(a, R::V1, super::mark_queue::capacity(true));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, spill);
    a.mov_reg_reg(R::V1, R::V0);
    a.shl_reg_imm8(R::V1, 3);
    load_rt(a, R::V6, super::mark_queue::entries(true));
    a.add_reg_reg(R::V1, R::V6);
    a.store_ptr_disp32(R::V1, 0, R::V8);
    super::mark_queue::mirror_address(a, true, R::V1);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, LEN, R::V0);
    a.jmp_label(retained);
    a.bind_text_label(spill);
    super::mark_queue::spill(a, l, true);
    a.bind_text_label(retained);
    a.bind_text_label(known);
    a.ret();
    super::young_trace::emit(a, l);
    a.bind_text_label(w.young.start);
    for f in [ACTIVE, VALID, super::RT_PHASE_OPEN, super::RT_RETIRE_STAGE] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    super::collect::clear_scratch(a, l);
    super::worker::lock(a, l);
    start_ready(a, l);
    super::worker::unlock(a, l);
    a.ret();
}

/// A sealed Young snapshot must cover every copied from-space Young object.
/// Actor-private counters are folded at retirement, like the Old diagnostics.
pub(super) fn check_copy<E: PortableAsm>(a: &mut E, worker: bool) {
    let done = a.create_text_label();
    load_rt(a, R::V2, VALID);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, done);
    a.mov_reg_reg(R::V2, R::V0);
    load_rt(a, R::V1, super::RT_HEAP_BASE);
    a.sub_reg_reg(R::V2, R::V1);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, done);
    test(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::NotEqual, done);
    super::copy::bump(a, if worker { WORKER_UNMARKED } else { UNMARKED }, R::V2);
    a.bind_text_label(done);
}

/// Young completion drives an early cycle even if Old is still tracing.
/// When this lane is active, every path returns from the enclosing poll entry;
/// otherwise execution falls through to the older Old-only readiness path.
pub(super) fn poll<E: PortableAsm>(a: &mut E, l: &RuntimeLabels<E::TextLabel>) {
    let Some(_) = &l.worker else {
        return;
    };
    let old = a.create_text_label();
    let skip = a.create_text_label();
    let pending_label = a.create_text_label();
    let release = a.create_text_label();
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, old);
    for f in [DONE, super::RT_WORKER_ALIVE] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, skip);
    }
    for f in [super::RT_PHASE_OPEN, super::RT_RETIRE_STAGE] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, skip);
    }
    super::start_policy::below(a, skip);
    super::worker::lock(a, l);
    a.call_label(l.publish_pending);
    super::copy::bump(a, super::RT_CYCLE_CHECKS, R::V0);
    pending(a, pending_label);
    super::copy::bump(a, super::RT_CYCLE_READY, R::V0);
    super::worker::unlock(a, l);
    a.call_label(l.collect);
    a.ret();
    a.bind_text_label(pending_label);
    super::copy::bump(a, super::RT_CYCLE_RETRIES, R::V0);
    a.mov_imm64(R::V0, 0);
    store_rt(a, DONE, R::V0);
    super::worker::request_work(a);
    a.jmp_label(release);
    a.bind_text_label(release);
    super::worker::unlock(a, l);
    super::worker::wake_if_pending(a, l);
    a.bind_text_label(skip);
    a.ret();
    a.bind_text_label(old);
}

/// A prior relocation may have consumed all headroom while retiring, before a
/// new mark could start. Start the missing snapshot as an allocation resource
/// wait after prior cleanup, releasing the moving-phase lock until it finishes.
/// Tiny forced stress cycles retain the direct-copy fallback.
pub(super) fn ensure_for_collection<E: PortableAsm>(a: &mut E, l: &RuntimeLabels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let done = a.create_text_label();
    let needed = a.create_text_label();
    for f in [ACTIVE, VALID] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    load_rt(a, R::V0, super::weak::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, needed);
    #[cfg(test)]
    {
        load_rt(a, R::V0, AUTO_DISABLED);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    for f in [ACTIVE, VALID] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    load_rt(a, R::V0, super::RT_WORKER_ALIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, super::old_cycle::REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, needed);
    super::start_policy::below(a, done);
    a.bind_text_label(needed);
    super::worker::unlock(a, l);
    let started = a.create_text_label();
    load_rt(a, R::V0, super::RT_MP_STATE);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, started);
    a.call_label(w.mark_start);
    a.bind_text_label(started);
    a.call_label(w.young.start);
    wait(a, l);
    super::worker::lock(a, l);
    a.bind_text_label(done);
}

/// Actual coordinator holds the ticket; all participating owners are quiescent.
pub(super) fn start_ready<E: PortableAsm>(a: &mut E, l: &RuntimeLabels<E::TextLabel>) {
    for f in [
        super::mark_queue::count(true),
        super::mark_queue::active_page(true),
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(a, R::V0, EPOCH);
    a.mov_imm64(R::V1, u64::MAX);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, l.fail);
    super::capacity::begin(a);
    super::start_policy::begin_mark(a, l);
    super::phase::young_marking(a, l, Phase::MarkStart);
    super::mark_queue::freeze_old(a);
    super::registered_roots::begin(a, l, super::registered_roots::YOUNG);
    a.mov_imm64(R::V0, 0);
    for f in [DONE, VALID, LEN, OBJECT, FIELD, COUNT, REMSET, OLD_INDEX] {
        store_rt(a, f, R::V0);
    }
    a.mov_imm64(R::V0, 1);
    store_rt(a, ACTIVE, R::V0);
    super::copy::bump(a, EPOCH, R::V0);
    load_rt(a, R::V0, super::RT_REMSET_BASE);
    a.add_reg_reg(R::V0, R::V3); // The remembered-set selector is a runtime offset.
    store_rt(a, REMSET_BASE, R::V0);
    load_rt(a, R::V0, super::RT_REMSET_LEN);
    store_rt(a, REMSET_BOUND, R::V0);
    load_rt(a, R::V0, super::RT_MSTACK_LEN);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, OLD_BOUND, R::V0);
    if l.raw_roots {
        // Native shadow slots need an eager snapshot while the owner holds
        // the collector lock. RootCell backends retain budgeted discovery.
        a.mov_imm64(R::V0, 2);
        store_rt(a, super::RT_LIT_MARK, R::V0);
        a.call_label(l.literals);
        a.mov_imm64(R::V0, 0);
        store_rt(a, super::RT_LIT_MARK, R::V0);
    }
    super::phase::young_marking(a, l, Phase::ConcurrentMark);
    super::worker::request_work(a);
}

/// Real debt/frontiers already closed under the collector ticket.
pub(super) fn seal_ready<E: PortableAsm>(a: &mut E, l: &RuntimeLabels<E::TextLabel>) {
    super::registered_roots::finish(a, l, super::registered_roots::YOUNG);
    super::capacity::seal(a, l);
    super::start_policy::end_mark(a, l);
    a.mov_imm64(R::V0, 0);
    store_rt(a, ACTIVE, R::V0);
    store_rt(a, DONE, R::V0);
    a.mov_imm64(R::V0, 1);
    store_rt(a, VALID, R::V0);
    super::phase::young_marking(a, l, Phase::Select);
}
