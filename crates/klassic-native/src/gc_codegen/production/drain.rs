//! Cooperative mark/relocation end and measured work accounting, one owner.
use super::{Labels, load_rt, store_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

#[repr(C)]
pub(super) struct Stats {
    pub(super) mark_slices: u64,
    pub(super) mark_yields: u64,
    pub(super) mark_max_units: u64,
    pub(super) worker_mark_units: u64,
    pub(super) mark_worker_units: u64,
    pub(super) mark_worker_start: u64,
    pub(super) relocate_slices: u64,
    pub(super) relocate_yields: u64,
    pub(super) relocate_max_units: u64,
    pub(super) worker_relocate_units: u64,
    pub(super) relocate_worker_units: u64,
    pub(super) relocate_worker_start: u64,
    pub(super) end_checks: u64,
    pub(super) end_retries: u64,
    pub(super) cycle_checks: u64,
    pub(super) cycle_retries: u64,
    pub(super) ready_cycles: u64,
}

pub(super) fn start_mark<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, super::RT_DM_WORKER);
    store_rt(a, super::RT_DM_START, R::V0);
}

/// V0 = consumed units, V1 = remaining budget. The caller retains the loop's
/// done flag on the stack; a larger remaining value violates this contract.
fn used<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V1, super::RT_BUDGET);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, l.fail);
    a.sub_reg_reg(R::V0, R::V1);
}

pub(super) fn record_mark_slice<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let recorded = a.create_text_label();
    a.push_reg(R::V0);
    used(a, l);
    load_rt(a, R::V2, super::RT_DM_MAX);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, recorded);
    a.jcc_label(C::Equal, recorded);
    store_rt(a, super::RT_DM_MAX, R::V0);
    a.bind_text_label(recorded);
    super::copy::bump(a, super::RT_DM_SLICES, R::V9);
    a.pop_reg(R::V0);
}

pub(super) fn record_worker_mark<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V0);
    used(a, l);
    load_rt(a, R::V1, super::RT_DM_WORKER);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, super::RT_DM_WORKER, R::V0);
    a.pop_reg(R::V0);
}

pub(super) fn yield_mark<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::copy::bump(a, super::RT_DM_YIELDS, R::V9);
    super::worker::unlock(a, l);
    super::worker::wake_if_pending(a, l);
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.relax);
    }
    super::worker::lock(a, l);
}

pub(super) fn finish_mark<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, super::RT_DM_WORKER);
    load_rt(a, R::V2, super::RT_DM_START);
    a.sub_reg_reg(R::V1, R::V2);
    load_rt(a, R::V0, super::RT_DM_ASSISTED);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, super::RT_DM_ASSISTED, R::V0);
}

/// Quiescent relocation finish retains the shared frontier between bounded
/// owner and worker turns. A zero-work incomplete scan is an unpublished Old
/// slot with no mutator able to finish it at this boundary: reject, never retire.
pub(super) fn relocate<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let again = a.create_text_label();
    let complete = a.create_text_label();
    load_rt(a, R::V0, super::RT_DR_WORKER);
    store_rt(a, super::RT_DR_START, R::V0);
    a.bind_text_label(again);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    store_rt(a, super::RT_BUDGET, R::V0);
    a.call_label(l.cheney);
    a.push_reg(R::V0);
    used(a, l);
    load_rt(a, R::V2, super::RT_DR_MAX);
    a.cmp_reg_reg(R::V0, R::V2);
    let recorded = a.create_text_label();
    a.jcc_label(C::Below, recorded);
    a.jcc_label(C::Equal, recorded);
    store_rt(a, super::RT_DR_MAX, R::V0);
    a.bind_text_label(recorded);
    super::copy::bump(a, super::RT_DR_SLICES, R::V9);
    a.mov_reg_reg(R::V2, R::V0);
    a.pop_reg(R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, complete);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, l.fail);
    super::copy::bump(a, super::RT_DR_YIELDS, R::V9);
    super::worker::unlock(a, l);
    super::worker::wake_if_pending(a, l);
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.relax);
    }
    super::worker::lock(a, l);
    a.jmp_label(again);
    a.bind_text_label(complete);
    load_rt(a, R::V1, super::RT_DR_WORKER);
    load_rt(a, R::V2, super::RT_DR_START);
    a.sub_reg_reg(R::V1, R::V2);
    load_rt(a, R::V0, super::RT_DR_ASSISTED);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, super::RT_DR_ASSISTED, R::V0);
}

pub(super) fn record_worker_relocation<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V0);
    used(a, l);
    load_rt(a, R::V1, super::RT_DR_WORKER);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, super::RT_DR_WORKER, R::V0);
    a.pop_reg(R::V0);
}
