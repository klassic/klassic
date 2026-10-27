//! Budgeted relocation through leased registered-region inventory.
//!
//! Root reads already resolve through the load barrier. New publications
//! resolve their referents before publishing the current color. The segment
//! remains process-owned; exact-word CAS preserves concurrent owner stores/pop/reuse.
use super::Labels;
use crate::portable_asm::{PortableAsm, Reg as R};

pub(super) fn start<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::native_completion::capture(a, l);
    super::registered_roots::begin(a, l, super::registered_roots::RELOCATION);
}

/// Called under the collector lock, before the heap chains. Every cell,
/// including null/inactive cells, costs one unit. `paused` cannot mean done.
pub(super) fn scan<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    forward: E::TextLabel,
    paused: E::TextLabel,
) {
    let top = a.create_text_label();
    let next = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(top);
    super::registered_roots::prepare(a, l, super::registered_roots::RELOCATION, top, done, paused);
    super::registered_roots::capture(a, l, super::registered_roots::RELOCATION);
    super::colours::offset(a, R::V0, R::V1, next);
    a.push_reg(R::V10);
    a.push_reg(R::V0);
    a.call_label(forward);
    a.mov_reg_reg(R::V1, R::V0);
    a.pop_reg(R::V0);
    a.pop_reg(R::V10);
    heal(a);
    a.bind_text_label(next);
    super::registered_roots::acknowledge(a, l, super::registered_roots::RELOCATION);
    super::copy::bump(a, super::RT_R_ROOT_WORK, R::V0);
    a.jmp_label(top);
    a.bind_text_label(done);
    super::registered_roots::finish(a, l, super::registered_roots::RELOCATION);
}

/// V0 = observed word, V1 = resolved reference, V10 = retained cell.
/// A collector heal never overwrites a newer owner publication.
pub(super) fn heal<E: PortableAsm>(a: &mut E) {
    for reg in [R::V0, R::V2, R::V6, R::V7] {
        a.push_reg(reg);
    }
    a.mov_reg_reg(R::V0, R::V1);
    a.and_reg_imm32(R::V0, -0x10000);
    super::colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    // Refreshing a nonmoving Old root's remap color must not also certify
    // a white referent in the still-active shared snapshot.
    super::colours::uncertify_white(a);
    a.mov_reg_reg(R::V1, R::V0);
    super::fields::forget_nonnull(a, R::V1, R::V0);
    for reg in [R::V7, R::V6, R::V2, R::V0] {
        a.pop_reg(reg);
    }
    a.atomic_compare_exchange(R::V10, R::V1);
}
