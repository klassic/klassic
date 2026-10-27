//! Exact live obligations, credited before actual journal release and retired
//! after actual ACK/lease release. Includes alloc_into's direct publication.
//! This counter is not a queue certificate: closure still excludes producers,
//! hands off journals and completes the real graph/root frontiers. No polling,
//! native calls or allocation is introduced at a raw publication boundary.
use super::*;
#[repr(C)]
pub(super) struct State {
    pending: u64,
    published: u64,
    acknowledged: u64,
}
const BASE: usize = offset_of!(Runtime, root_work);
pub(super) const PENDING: usize = BASE + offset_of!(State, pending);
pub(super) const PUBLISHED: usize = BASE + offset_of!(State, published);
pub(super) const ACKNOWLEDGED: usize = BASE + offset_of!(State, acknowledged);

pub(super) fn publish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    change(a, l, false, None);
}
/// Positive count of actual retired cells, before their region can disappear.
pub(super) fn publish_retired<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, count: R) {
    change(a, l, false, Some(count));
}
pub(super) fn ack<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, count: Option<R>) {
    change(a, l, true, count);
}
fn change<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, remove: bool, count: Option<R>) {
    let saved = [R::V0, R::V1, R::V6, R::V7, R::V8, R::V9];
    for r in saved {
        a.push_reg(r);
    }
    if let Some(count) = count {
        a.mov_reg_reg(R::V9, count);
    } else {
        a.mov_imm64(R::V9, 1);
    }
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, l.fail);
    adjust(a, l, PENDING, remove);
    adjust(a, l, if remove { ACKNOWLEDGED } else { PUBLISHED }, false);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
}
/// V9 is the positive count. Every arithmetic failure rejects before wrapping.
fn adjust<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, field: usize, remove: bool) {
    lea_rt(a, R::V7, field);
    let retry = a.create_text_label();
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V7);
    a.mov_reg_reg(R::V8, R::V0);
    a.mov_reg_reg(R::V6, R::V0);
    if remove {
        a.cmp_reg_reg(R::V6, R::V9);
        a.jcc_label(C::Below, l.fail);
        a.sub_reg_reg(R::V6, R::V9);
    } else {
        a.add_reg_reg(R::V6, R::V9);
        a.cmp_reg_reg(R::V6, R::V8);
        a.jcc_label(C::Below, l.fail);
    }
    a.atomic_compare_exchange(R::V7, R::V6);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, retry);
}
/// After producer exclusion and actual handoff/frontier completion, under the
/// collector ticket. A positive count must never be relabelled as termination.
pub(super) fn require_empty<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    lea_rt(a, R::V1, PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}

/// A zero exact ledger skips journal inventory entirely at a phase edge.
/// Nonzero work must be handed off by the real consumer before publication.
pub(super) fn seal<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::root_retirement::drain_deletions(a, l);
    let empty = a.create_text_label();
    lea_rt(a, R::V1, PENDING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    a.call_label(l.root_journal_seal);
    require_empty(a, l);
    a.bind_text_label(empty);
}
