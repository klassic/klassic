//! Test-only emission helpers for the machine-code harness in
//! `gc_barrier_harness_tests.rs`. They read runtime state that the harness
//! program reports (mark bits, remembered-set contents, counters), so the
//! tests observe the real emitted collector rather than a model of it.
use super::{
    HEAP_BYTES, Labels, RT_COLLECTIONS, RT_FULL_COLLECTIONS, RT_HEAP_BASE, RT_MARK_ACTIVE,
    RT_MARK_CYCLES, RT_MARKED, RT_PHASE_OPEN, RT_REMSET_BASE, RT_REMSET_DROPPED, RT_REMSET_FLIPPED,
    RT_REMSET_FLIPS, RT_REMSET_KEPT, RT_REMSET_LEN, RT_UNMARKED, RT_WORKER_SCANNED, YOUNG_END, d,
    mark,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

/// A runtime counter or flag the harness can report.
#[derive(Clone, Copy)]
pub(crate) enum Counter {
    Collections,
    FullCollections,
    Marked,
    MarkCycles,
    MarkActive,
    Unmarked,
    PhaseOpen,
    RemsetLen,
    RemsetDropped,
    RemsetFlips,
    RemsetFlipped,
    RemsetKept,
    WorkerScanned,
}

pub(crate) fn counter_offset(counter: Counter) -> i32 {
    d(match counter {
        Counter::Collections => RT_COLLECTIONS,
        Counter::FullCollections => RT_FULL_COLLECTIONS,
        Counter::Marked => RT_MARKED,
        Counter::MarkCycles => RT_MARK_CYCLES,
        Counter::MarkActive => RT_MARK_ACTIVE,
        Counter::Unmarked => RT_UNMARKED,
        Counter::PhaseOpen => RT_PHASE_OPEN,
        Counter::RemsetLen => RT_REMSET_LEN,
        Counter::RemsetDropped => RT_REMSET_DROPPED,
        Counter::RemsetFlips => RT_REMSET_FLIPS,
        Counter::RemsetFlipped => RT_REMSET_FLIPPED,
        Counter::RemsetKept => RT_REMSET_KEPT,
        Counter::WorkerScanned => RT_WORKER_SCANNED,
    })
}

/// The labels a harness program calls.
pub(crate) struct HarnessLabels<T> {
    pub alloc: T,
    pub field_store: T,
    pub field_load: T,
    pub root_adopt: T,
    pub root_load: T,
    pub root_pop: T,
    pub root_store: T,
    pub mark_start: T,
    pub mark_loop: T,
    pub lock: T,
    pub unlock: T,
}

pub(crate) fn harness_labels<T: Copy>(l: &Labels<T>) -> HarnessLabels<T> {
    let w = l.worker.as_ref().expect("the harness needs a worker build");
    HarnessLabels {
        alloc: l.alloc,
        field_store: l.field_store,
        field_load: l.field_load,
        root_adopt: l.root_adopt,
        root_load: l.root_load,
        root_pop: l.root_pop,
        root_store: l.root_store,
        mark_start: w.mark_start,
        mark_loop: w.mark_loop,
        lock: w.lock,
        unlock: w.unlock,
    }
}

/// V0 = payload address -> V0 = 1 if its mark bit is set, else 0
/// (V3 = runtime; clobbers V1, V2, V6, V8).
pub(crate) fn emit_marked<E: PortableAsm>(a: &mut E) {
    let unset = a.create_text_label();
    let end = a.create_text_label();
    a.mov_reg_reg(R::V2, R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(R::V2, R::V0);
    mark::test_mark(a, R::V2, R::V6, R::V8);
    a.jcc_label(C::Equal, unset);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(end);
    a.bind_text_label(unset);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(end);
}

/// V0 = payload address -> V0 = 1 if it lies in the Old generation.
pub(crate) fn emit_is_old<E: PortableAsm>(a: &mut E) {
    let no = a.create_text_label();
    let end = a.create_text_label();
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, no);
    a.mov_imm64(R::V1, YOUNG_END);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, no);
    a.mov_imm64(R::V1, HEAP_BYTES);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, no);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(end);
    a.bind_text_label(no);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(end);
}

/// V0 = entry `index` of the current remembered set (V3 = runtime;
/// clobbers V1).
pub(crate) fn emit_current_entry<E: PortableAsm>(a: &mut E, index: u32) {
    a.load_ptr_disp32(R::V1, R::V3, d(RT_REMSET_BASE));
    a.add_reg_reg(R::V1, R::V3);
    a.load_ptr_disp32(R::V0, R::V1, (index * 8) as i32);
}

/// Let `mark_loop` run until its stack is empty (V3 = runtime; clobbers V0).
pub(crate) fn emit_unbounded_budget<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V0, u64::MAX);
    a.store_ptr_disp32(R::V3, d(super::RT_BUDGET), R::V0);
}
