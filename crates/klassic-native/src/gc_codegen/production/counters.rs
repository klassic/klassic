//! Contested allocation/publication accounting. Saturate rather than wrapping
//! a long-lived diagnostic/policy total. Never lock around an allocation.
use super::*;

pub(super) fn add<E: PortableAsm>(a: &mut E, field: usize, delta: R) {
    emit(a, field, Some(delta), None);
}
pub(super) fn increment<E: PortableAsm>(a: &mut E, field: usize) {
    emit(a, field, None, None);
}
pub(super) fn add_capped<E: PortableAsm>(a: &mut E, field: usize, delta: R, cap: u64) {
    emit(a, field, Some(delta), Some(cap));
}
/// Preserve every register (except flags), including an allocation result.
fn emit<E: PortableAsm>(a: &mut E, field: usize, delta: Option<R>, cap: Option<u64>) {
    let saved = [R::V0, R::V1, R::V6, R::V7, R::V8, R::V9];
    for r in saved {
        a.push_reg(r);
    }
    if let Some(delta) = delta {
        a.mov_reg_reg(R::V9, delta);
    } else {
        a.mov_imm64(R::V9, 1);
    }
    lea_rt(a, R::V7, field);
    let retry = a.create_text_label();
    let sized = a.create_text_label();
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V7);
    a.mov_reg_reg(R::V8, R::V0);
    a.mov_reg_reg(R::V6, R::V0);
    a.add_reg_reg(R::V6, R::V9);
    a.jcc_label(C::AboveOrEqual, sized);
    a.mov_imm64(R::V6, u64::MAX);
    a.bind_text_label(sized);
    if let Some(cap) = cap {
        let bounded = a.create_text_label();
        a.mov_imm64(R::V1, cap);
        a.cmp_reg_reg(R::V6, R::V1);
        a.jcc_label(C::Below, bounded);
        a.mov_reg_reg(R::V6, R::V1);
        a.bind_text_label(bounded);
    }
    a.atomic_compare_exchange(R::V7, R::V6);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, retry);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
}
