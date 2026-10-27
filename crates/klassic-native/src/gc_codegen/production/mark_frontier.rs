//! Preserve the Old snapshot across Young-only copying, under the collector
//! lock. Tracing is suspended while relocation rewrites these strong roots.
//! Old owns the epoch/queue; Young copying traces roots and remembered slots.
use super::{Labels, load_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
#[path = "old_spill_frontier.rs"]
mod spill;

/// Resolve a logical Old frontier index under the ticket: zero is the active
/// receiver; positive i is queue[i-1]. Clobbers dst/V1 and flags, preserves V2.
/// Callers retain the index, never this slot address, across a helper that can
/// yield or replace the queue backing. Growth changes this one resolver.
pub(super) fn old_slot<E: PortableAsm>(a: &mut E, index: R, dst: R) {
    let queue = a.create_text_label();
    let done = a.create_text_label();
    a.mov_reg_reg(dst, index);
    a.test_reg_reg(dst, dst);
    a.jcc_label(C::NotEqual, queue);
    super::lea_rt(a, dst, super::RT_M_OBJ);
    a.jmp_label(done);
    a.bind_text_label(queue);
    a.sub_reg_imm8(dst, 1);
    a.shl_reg_imm8(dst, 3);
    load_rt(a, R::V1, super::mark_queue::entries(false));
    a.add_reg_reg(dst, R::V1);
    a.bind_text_label(done);
}

/// Only a full Old move ends the Old snapshot. Empty Old work frontiers do
/// not close its producers: root/store SATB and black allocation remain active
/// across any number of Young collections until Old itself must move.
pub(super) fn before_flip<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let keep = a.create_text_label();
    let full = a.create_text_label();
    load_rt(a, R::V0, super::old_cycle::REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, full);
    super::pool::must_full(a, l, full);
    a.jmp_label(keep);
    a.bind_text_label(full);
    super::weak::ensure_old(a, l);
    a.call_label(l.mark_end);
    a.bind_text_label(keep);
}

/// Constant-count end check; late SATB appends extend the relocation frontier.
pub(super) fn pending<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    let done = a.create_text_label();
    load_rt(a, R::V0, super::RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, super::RT_M_RELOCATE_INDEX);
    load_rt(a, R::V1, super::RT_MSTACK_LEN);
    // Index zero belongs to the active receiver, even if it is null.
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, pending);
    a.jcc_label(C::Equal, pending);
    spill::pending(a, pending);
    a.bind_text_label(done);
}

/// One unit per retained receiver/queue slot, including null active receiver.
/// The queue cannot pop during relocation. Its length can grow through SATB.
pub(super) fn scan<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    forward: E::TextLabel,
    paused: E::TextLabel,
) {
    let done = a.create_text_label();
    let top = a.create_text_label();
    let next = a.create_text_label();
    load_rt(a, R::V0, super::RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.bind_text_label(top);
    load_rt(a, R::V0, super::RT_M_RELOCATE_INDEX);
    load_rt(a, R::V1, super::RT_MSTACK_LEN);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, done);
    super::scan::spend(a, paused);
    old_slot(a, R::V0, R::V0);
    a.load_ptr_disp32(R::V1, R::V0, 0);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, next);
    load_rt(a, R::V0, super::RT_M_RELOCATE_INDEX);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    a.mov_reg_reg(R::V0, R::V1);
    a.call_label(forward);
    // A late admission can have stamped the old source after a white copy
    // won its forwarding CAS. This queued job also owns the destination.
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    super::mark_epoch::set(a, R::V2, R::V6, R::V7);
    a.shl_reg_imm8(R::V2, 16);
    super::colours::load_current(a, R::V1);
    a.or_reg_reg(R::V2, R::V1);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    let queued = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, queued);
    super::mark_queue::rebind_active_old(a, l);
    a.bind_text_label(queued);
    old_slot(a, R::V0, R::V0);
    a.store_ptr_disp32(R::V0, 0, R::V2);
    // The receiver lives outside queue backing and has no candidate mirror.
    super::lea_rt(a, R::V1, super::RT_M_OBJ);
    a.cmp_reg_reg(R::V0, R::V1);
    let mirrored = a.create_text_label();
    a.jcc_label(C::Equal, mirrored);
    super::mark_queue::mirror_address(a, false, R::V0);
    a.bind_text_label(mirrored);
    a.bind_text_label(next);
    super::copy::bump(a, super::RT_M_RELOCATE_INDEX, R::V0);
    a.jmp_label(top);
    a.bind_text_label(done);
    spill::scan(a, l, forward, paused);
}
