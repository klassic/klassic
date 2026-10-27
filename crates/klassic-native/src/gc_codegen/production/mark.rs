//! Concurrent Old marking, independent of Young copying (worker builds only).
//!
//! Per-object epoch stamps avoid heap-wide clearing before a snapshot. Normal
//! allocation consumes metadata Ready to capture roots and visit external roots (AArch64 raw
//! shadow slots). On x86 the bound includes permanent literal RootCells; the
//! worker discovers those and frame roots and traces their graph in quanta.
//! Root stores/pops protect overwritten values before mutation. Objects allocated while
//! marking are marked at allocation (`alloc_black`), and `field_store`
//! marks the reference it overwrites (SATB pre-barrier, under the
//! collector lock). A full Old move cooperatively finishes remaining mark
//! work with bounded lock holds. When a completed mark is valid, remembered
//! fields whose referent is unmarked are dropped instead of evacuated, and
//! copying an unmarked object is counted as an invariant violation. Marking
//! pauses across a Young-only relocation: its queue and active receiver are
//! relocated as strong roots. A full Old move first finishes the Old mark.
use super::{
    Labels, RT_BUDGET, RT_HEAP_BASE, RT_LIT_MARK, RT_M_COUNT, RT_M_FIELD, RT_M_OBJ, RT_MARK_ACTIVE,
    RT_MARK_CYCLES, RT_MARK_DONE, RT_MARK_DRAINS, RT_MARK_VALID, RT_MARKED, RT_MSTACK_LEN,
    RT_PHASE_OPEN,
    copy::{DESCRIPTOR, PAYLOAD, bump},
    load_rt, store_rt,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

/// Current Old-cycle membership, retained by object metadata through copies.
/// NotEqual means marked. Preserves off and V0; clobbers bit/word and V1.
pub(super) fn test_mark<E: PortableAsm>(a: &mut E, off: R, bit: R, word: R) {
    super::mark_epoch::test(a, off, bit, word);
}

/// In `alloc_raw`, V0 = the new object's bits (preserved): while marking,
/// a new object is born marked. Clobbers V1, V2, V6..V9.
pub(super) fn alloc_black<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker.is_none() {
        return;
    }
    let skip = a.create_text_label();
    load_rt(a, R::V2, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V2, R::V2);
    let tracked = a.create_text_label();
    a.jcc_label(C::NotEqual, tracked);
    // Fresh identities remain live while the selected census is valid.
    // Weak loads must distinguish them from pre-snapshot white objects.
    load_rt(a, R::V2, RT_MARK_VALID);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, skip);
    a.bind_text_label(tracked);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    super::mark_epoch::set(a, R::V2, R::V6, R::V7);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.bind_text_label(skip);
    super::young_mark::alloc_black(a);
}

/// In `alloc` (V3 = runtime): start a mark cycle once enough of the
/// semispace is in use and no phase or cycle is open.
pub(super) fn trigger<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    super::cooperative_old::branch_enabled(a, done);
    super::mark_prepare::trigger(a, l);
    super::young_mark::trigger(a, l);
    a.bind_text_label(done);
}

/// Before full Old relocation (lock held, before the flip): finish an open
/// mark cycle cooperatively if work remains, then validate it. The caller
/// waits for completion; lock holds are bounded and it returns holding the lock.
pub(super) fn drain<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let none = a.create_text_label();
    let drained = a.create_text_label();
    let pending = a.create_text_label();
    let again = a.create_text_label();
    let weak_busy = a.create_text_label();
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, none);
    // Initialization completed before entering this allocation safepoint.
    // Seal its pending header before changing the adopted snapshot.
    a.call_label(l.publish_pending);
    super::satb_buffer::flush(a, l, true);
    super::phase::marking(a, l, crate::gc_abi::Phase::MarkEndCheck);
    super::satb_buffer::pending(a, false, pending);
    load_rt(a, R::V0, RT_M_OBJ);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    super::registered_roots::pending(a, super::registered_roots::OLD, pending);
    load_rt(a, R::V0, super::mark_queue::count(false));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    load_rt(a, R::V0, RT_MSTACK_LEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, drained);
    a.bind_text_label(pending);
    bump(a, RT_MARK_DRAINS, R::V0);
    super::drain::start_mark(a);
    a.bind_text_label(again);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    store_rt(a, RT_BUDGET, R::V0);
    a.call_label(w.mark_loop);
    super::drain::record_mark_slice(a, l);
    let complete = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, complete);
    super::drain::yield_mark(a, l);
    a.jmp_label(again);
    a.bind_text_label(complete);
    super::drain::finish_mark(a);
    a.bind_text_label(drained);
    // The inventory must be exhausted even when every captured region is empty.
    super::registered_roots::finish(a, l, super::registered_roots::OLD);
    super::root_work::require_empty(a, l);
    super::weak::seal(a, false, weak_busy);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_MARK_ACTIVE, R::V0);
    store_rt(a, RT_MARK_DONE, R::V0);
    a.mov_imm64(R::V0, 1);
    store_rt(a, RT_MARK_VALID, R::V0);
    super::phase::marking(a, l, crate::gc_abi::Phase::Select);
    a.jmp_label(none);
    a.bind_text_label(weak_busy);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    store_rt(a, RT_BUDGET, R::V0);
    super::weak::work(a, l);
    super::drain::yield_mark(a, l);
    a.jmp_label(pending);
    a.bind_text_label(none);
}

pub(super) fn emit_mark<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::young_mark::emit(a, l);
    super::mark_cycle::emit(a, l);
    super::mark_prepare::emit(a, l);
    a.bind_text_label(l.mark_end);
    drain(a, l);
    a.ret();
    let Some(w) = &l.worker else {
        return;
    };
    // mark_ref: V2 = payload offset (preserved). Mark it; if it was not
    // marked, push it. Caller holds the lock. Clobbers V0, V1, V6..V9.
    let known = a.create_text_label();
    a.bind_text_label(w.mark_ref);
    test_mark(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::NotEqual, known);
    super::mark_epoch::set(a, R::V2, R::V6, R::V7);
    super::old_footprint::admit(a);
    bump(a, RT_MARKED, R::V0);
    load_rt(a, R::V0, RT_MSTACK_LEN);
    let spill = a.create_text_label();
    let retained = a.create_text_label();
    load_rt(a, R::V1, super::mark_queue::capacity(false));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, spill);
    a.mov_reg_reg(R::V1, R::V0);
    a.shl_reg_imm8(R::V1, 3);
    load_rt(a, R::V6, super::mark_queue::entries(false));
    a.add_reg_reg(R::V1, R::V6);
    a.mov_reg_reg(R::V6, R::V2);
    a.shl_reg_imm8(R::V6, 16);
    super::colours::load_current(a, R::V7);
    a.or_reg_reg(R::V6, R::V7);
    a.store_ptr_disp32(R::V1, 0, R::V6);
    super::mark_queue::mirror_address(a, false, R::V1);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, RT_MSTACK_LEN, R::V0);
    a.jmp_label(retained);
    a.bind_text_label(spill);
    super::mark_queue::spill(a, l, false);
    a.bind_text_label(retained);
    // A newly retained Old job is also a strong Young root when applicable.
    a.call_label(w.young.reference);
    a.bind_text_label(known);
    a.ret();

    emit_mark_loop(a, l, w.mark_loop, w.mark_ref);
    emit_mark_start(a, l, w.mark_start);
}

/// mark_loop: trace graph work and discover roots until both are empty
/// (V0 = 1), or the shared budget runs out (V0 = 0). Caller holds the lock.
fn emit_mark_loop<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    entry: E::TextLabel,
    mark_ref: E::TextLabel,
) {
    let top = a.create_text_label();
    let fields = a.create_text_label();
    let field_loop = a.create_text_label();
    let next_field = a.create_text_label();
    let empty = a.create_text_label();
    let paused = a.create_text_label();
    let roots = a.create_text_label();
    let fields_done = a.create_text_label();
    a.bind_text_label(entry);
    #[cfg(test)]
    if let Some(gate) = l.phase_mark_gate {
        let proceed = a.create_text_label();
        a.call_label(gate);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, proceed);
        a.ret(); // no consumed budget or claimed completion while gated
        a.bind_text_label(proceed);
    }

    // Queue receivers are rewritten by relocation before tracing may resume.
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, paused);
    a.bind_text_label(top);
    // A suspended object's fields are a live frontier even when the queue
    // is empty. Resume that cursor before taking another object or root.
    load_rt(a, R::V0, RT_M_OBJ);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, field_loop);
    super::mark_queue::finish_active(a, l, false);
    let spill = a.create_text_label();
    let popped = a.create_text_label();
    load_rt(a, R::V0, RT_MSTACK_LEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, spill);
    super::scan::spend(a, paused);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, RT_MSTACK_LEN, R::V0);
    super::mark_queue::slot(a, false, R::V0, R::V0);
    a.load_ptr_disp32(R::V2, R::V0, 0);
    a.jmp_label(popped);
    a.bind_text_label(spill);
    load_rt(a, R::V0, super::mark_queue::count(false));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, roots);
    load_rt(a, R::V0, super::mark_queue::discovery_frozen());
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, roots);
    super::scan::spend(a, paused);
    super::mark_queue::take(a, l, false, roots);
    a.bind_text_label(popped);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.mov_reg_reg(R::V6, R::V2);
    a.shr_reg_imm8(R::V6, 16);
    a.add_reg_reg(R::V1, R::V6);
    a.load_ptr_disp32(R::V0, R::V1, DESCRIPTOR);
    a.load_ptr_disp32(R::V6, R::V1, PAYLOAD);
    a.shr_reg_imm8(R::V6, 3);
    store_rt(a, RT_M_COUNT, R::V6);
    a.mov_imm64(R::V7, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, fields);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, top);
    a.mov_imm64(R::V7, 1);
    a.bind_text_label(fields);
    store_rt(a, RT_M_OBJ, R::V2);
    store_rt(a, RT_M_FIELD, R::V7);
    a.bind_text_label(field_loop);
    load_rt(a, R::V7, RT_M_FIELD);
    load_rt(a, R::V6, RT_M_COUNT);
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::AboveOrEqual, fields_done);
    super::scan::spend(a, paused);
    a.shl_reg_imm8(R::V7, 3);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V7, R::V1);
    load_rt(a, R::V1, RT_M_OBJ);
    a.shr_reg_imm8(R::V1, 16);
    a.add_reg_reg(R::V7, R::V1);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    super::fields::offset(a, R::V0, R::V2, next_field);
    for reg in [R::V10, R::V10, R::V7, R::V0] {
        a.push_reg(reg);
    }
    a.call_label(mark_ref);
    a.pop_reg(R::V0);
    a.pop_reg(R::V10);
    super::colours::heal_marked_slot(a);
    a.pop_reg(R::V10);
    a.pop_reg(R::V10);
    a.bind_text_label(next_field);
    load_rt(a, R::V7, RT_M_FIELD);
    a.add_reg_imm32(R::V7, 1);
    store_rt(a, RT_M_FIELD, R::V7);
    a.jmp_label(field_loop);
    a.bind_text_label(fields_done);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_M_OBJ, R::V0);
    a.jmp_label(top);
    a.bind_text_label(roots);
    super::mark_roots::discover(a, l, mark_ref, top, empty, paused);
    a.bind_text_label(empty);
    super::satb_buffer::pending(a, false, paused);
    load_rt(a, R::V0, super::mark_queue::count(false));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, paused);
    a.mov_imm64(R::V0, 1);
    a.ret();
    a.bind_text_label(paused);
    a.mov_imm64(R::V0, 0);
    a.ret();
}

/// mark_start (mutator, from `alloc`): the snapshot pause.
fn emit_mark_start<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, entry: E::TextLabel) {
    a.bind_text_label(entry);
    for flag in [RT_MARK_ACTIVE, RT_PHASE_OPEN, super::RT_RETIRE_STAGE] {
        load_rt(a, R::V0, flag);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    super::mark_prepare::ensure(a, l);
    // The allocation result is private scratch, not a snapshot root.
    // Publish its initialization and empty it before activating root SATB.
    super::collect::clear_scratch(a, l);
    super::worker::lock(a, l);
    start_ready(a, l);
    super::worker::unlock(a, l);
    a.ret();
}

/// Actual prepared snapshot start with the collector ticket held. Leaves it
/// held so a coordinator may publish its scheduling gate before worker tracing.
pub(super) fn start_ready<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for f in [
        super::mark_queue::count(false),
        super::mark_queue::active_page(false),
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(a, R::V0, super::RT_MP_STATE);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, RT_MARK_CYCLES);
    a.mov_imm64(R::V1, u64::MAX);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, l.fail); // Never reuse epoch zero or an earlier epoch.
    load_rt(a, R::V0, super::RT_NEXT_OBJECT_ID);
    store_rt(a, super::old_mark::OBJECT_BOUND, R::V0);
    super::phase::marking(a, l, crate::gc_abi::Phase::MarkStart);
    a.mov_imm64(R::V0, 0);
    for field in [
        super::RT_MP_STATE,
        RT_MSTACK_LEN,
        RT_MARK_DONE,
        RT_MARK_VALID,
        RT_M_OBJ,
        RT_M_FIELD,
        RT_M_COUNT,
        super::old_footprint::TRACE,
        super::old_footprint::BLACK,
    ] {
        store_rt(a, field, R::V0);
    }
    a.mov_imm64(R::V0, 1);
    store_rt(a, RT_MARK_ACTIVE, R::V0);
    bump(a, RT_MARK_CYCLES, R::V0);
    super::mark_roots::start(a, l);
    a.mov_imm64(R::V0, 1);
    store_rt(a, RT_LIT_MARK, R::V0);
    a.call_label(l.literals);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_LIT_MARK, R::V0);
    super::phase::marking(a, l, crate::gc_abi::Phase::ConcurrentMark);
    super::worker::request_work(a);
}
