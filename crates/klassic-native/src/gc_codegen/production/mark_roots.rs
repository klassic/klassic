//! Root SATB and resumable discovery for the production mark cycle.
//!
//! Each lane leases a stable inventory cursor and captures one region bound
//! without traversing cells. List transitions and cell visits are budgeted.
//! Root stores/pops mark their old value before mutation, so a queued root
//! may disappear or be reused before discovery without losing the snapshot.
//! New strong roots are protected before publication of current mark bits.
//! Reader leases end only after acknowledged discovery. Concurrent attach and
//! retirement deletion/journal handoff remain separate work.
use super::{HANDLE_CELL, Labels, RT_MARK_ACTIVE, RT_MARK_DONE, load_rt, store_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

/// RootRequest is runtime-owned and its handle has already been prepared.
/// Do not hold a private access guard: this may wait for the collector lock.
pub(super) fn before_store<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, request: usize) {
    let Some(w) = &l.worker else {
        return;
    };
    let done = a.create_text_label();
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    for previous in [true, false] {
        let next = a.create_text_label();
        if previous {
            load_rt(a, R::V0, request + HANDLE_CELL);
            a.atomic_load_acquire(R::V0, R::V0);
        } else {
            // A newly published strong root may lie beyond the captured
            // discovery bound. Protect it before certifying current mark bits.
            load_rt(a, R::V0, request + super::REQUEST_VALUE);
        }
        super::fields::offset(a, R::V0, R::V2, next);
        let protected = a.create_text_label();
        // The same single-owner epoch/metadata lifetime rule as heap stores:
        // a protected value need not acquire the collector lock again.
        super::mark::test_mark(a, R::V2, R::V6, R::V7);
        a.jcc_label(C::NotEqual, protected);
        super::worker::lock(a, l);
        a.call_label(w.mark_ref);
        if !previous {
            certify_value(a, request);
        }
        a.mov_imm64(R::V0, 0);
        store_rt(a, RT_MARK_DONE, R::V0);
        super::worker::request_work(a);
        super::worker::unlock(a, l);
        super::worker::wake_if_pending(a, l);
        a.jmp_label(next);
        a.bind_text_label(protected);
        if !previous {
            certify_value(a, request);
        }
        a.bind_text_label(next);
    }
    a.bind_text_label(done);
}

/// V2 is the canonical offset protected by either branch, before publication.
fn certify_value<E: PortableAsm>(a: &mut E, request: usize) {
    a.shl_reg_imm8(R::V2, 16);
    super::colours::load_current(a, R::V0);
    a.or_reg_reg(R::V0, R::V2);
    store_rt(a, request + super::REQUEST_VALUE, R::V0);
}

/// O(1) root boundary capture. Cell zero is the private allocation scratch,
/// which mark_start publishes and empties before activating root SATB.
pub(super) fn start<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::registered_roots::begin(a, l, super::registered_roots::OLD);
}

/// Emit the empty-mark-stack branch: discover one root per budget unit,
/// queue its referent, then return to the common graph/root work loop.
/// The caller holds the collector lock. An owner may mutate cells atomically
/// after its root SATB step has queued their previous values under that lock.
pub(super) fn discover<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    mark_ref: E::TextLabel,
    top: E::TextLabel,
    empty: E::TextLabel,
    paused: E::TextLabel,
) {
    let next = a.create_text_label();
    super::registered_roots::prepare(a, l, super::registered_roots::OLD, top, empty, paused);
    super::registered_roots::capture(a, l, super::registered_roots::OLD);
    a.mov_reg_reg(R::V1, R::V10);
    super::fields::offset(a, R::V0, R::V2, next);
    for reg in [R::V10, R::V10, R::V1, R::V0] {
        a.push_reg(reg);
    }
    a.call_label(mark_ref);
    a.pop_reg(R::V0);
    a.pop_reg(R::V10);
    super::colours::heal_marked_slot(a);
    a.pop_reg(R::V10);
    a.pop_reg(R::V10);
    a.bind_text_label(next);
    super::registered_roots::acknowledge(a, l, super::registered_roots::OLD);
    a.jmp_label(top);
}
