//! Young admission is independent of Old protection and remembered membership.
use super::young_mark as y;
use super::{Labels, load_rt, store_rt};
use crate::gc_abi::YOUNG_MARK_MASK;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

/// V2 = valid managed offset, preserved. Change V0/V1/V6..V9. A matching
/// Young stamp already owns an obligation; unknown values recheck under lock.
pub(super) fn protect<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let done = a.create_text_label();
    load_rt(a, R::V0, y::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, done);
    y::test(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::NotEqual, done);
    a.push_reg(R::V2);
    a.push_reg(R::V2);
    super::worker::lock(a, l);
    a.call_label(w.young.reference);
    super::worker::request_work(a);
    super::worker::unlock(a, l);
    super::worker::wake_if_pending(a, l);
    a.pop_reg(R::V2);
    a.pop_reg(R::V2);
    a.bind_text_label(done);
}
pub(super) fn field_store<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker.is_none() {
        return;
    }
    let done = a.create_text_label();
    load_rt(a, R::V0, y::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    for previous in [true, false] {
        let next = a.create_text_label();
        if previous {
            a.load_ptr_disp32(R::V0, R::V7, 0);
            super::fields::young_offset(a, R::V0, R::V2, next);
        } else {
            a.mov_reg_reg(R::V0, R::V6);
            super::enums::heap_offset(a, R::V0, R::V2, next);
        }
        for r in [R::V6, R::V7, R::V8, R::V9] {
            a.push_reg(r);
        }
        protect(a, l);
        for r in [R::V9, R::V8, R::V7, R::V6] {
            a.pop_reg(r);
        }
        a.bind_text_label(next);
    }
    a.bind_text_label(done);
}
pub(super) fn root_store<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, request: usize) {
    if l.worker.is_none() {
        return;
    }
    let done = a.create_text_label();
    load_rt(a, R::V0, y::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    for previous in [true, false] {
        let next = a.create_text_label();
        if previous {
            load_rt(a, R::V0, request + super::HANDLE_CELL);
            a.atomic_load_acquire(R::V0, R::V0);
        } else {
            load_rt(a, R::V0, request + super::REQUEST_VALUE);
        }
        super::fields::young_offset(a, R::V0, R::V2, next);
        protect(a, l);
        if !previous {
            load_rt(a, R::V0, request + super::REQUEST_VALUE);
            super::colours::load_current(a, R::V1);
            a.and_reg_imm32(R::V1, i32::from(YOUNG_MARK_MASK));
            a.and_reg_imm32(R::V0, !i32::from(YOUNG_MARK_MASK));
            a.or_reg_reg(R::V0, R::V1);
            store_rt(a, request + super::REQUEST_VALUE, R::V0);
        }
        a.bind_text_label(next);
    }
    a.bind_text_label(done);
}
/// The referent is Young-protected. Only its Young mark certificate changes.
pub(super) fn heal<E: PortableAsm>(a: &mut E) {
    super::colours::load_current(a, R::V1);
    a.and_reg_imm32(R::V1, i32::from(YOUNG_MARK_MASK));
    a.mov_reg_reg(R::V2, R::V0);
    a.and_reg_imm32(R::V2, !i32::from(YOUNG_MARK_MASK));
    a.or_reg_reg(R::V1, R::V2);
    a.atomic_compare_exchange(R::V10, R::V1);
}
