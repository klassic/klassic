//! Captured references retain exactly one actual source bank until ACK. The
//! source closes only after the ledger is empty; later records are canonical
//! references in the current bank, including Retire -> Idle publications.
use super::*;
fn life<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, allocation_records::REVISION);
    load_rt(a, R::V1, CURRENT_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    let closed = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V1, CLOSED_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, closed);
    load_rt(a, R::V1, SOURCE_BANK);
    a.add_reg_imm32(R::V1, 1);
    a.jcc_label(C::Equal, l.fail);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, RT_RY_LO);
    load_rt(a, R::V1, RT_RY_HI);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.jmp_label(done);
    a.bind_text_label(closed);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    load_rt(a, R::V1, TARGET_PHASE);
    a.xor_reg_reg(R::V0, R::V1);
    a.and_reg_imm32(R::V0, 0x1c00);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(done);
}
pub(in super::super) fn lifetime<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    life(a, l);
    quiescence::restore(a);
}
pub(in super::super) fn validate<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    a.mov_reg_reg(R::V8, R::V0);
    a.mov_reg_reg(R::V9, R::V1);
    life(a, l);
    a.mov_reg_reg(R::V0, R::V8);
    a.and_reg_imm32(R::V0, 0xe000);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    for shift in [0, 4] {
        let valid = a.create_text_label();
        a.mov_reg_reg(R::V0, R::V8);
        a.shr_reg_imm8(R::V0, shift);
        a.and_reg_imm32(R::V0, 15);
        for stage in [0, 2, 4, 6, 7] {
            a.cmp_reg_imm8(R::V0, stage);
            a.jcc_label(C::Equal, valid);
        }
        a.jmp_label(l.fail);
        a.bind_text_label(valid);
    }
    a.mov_reg_reg(R::V0, R::V8);
    a.shr_reg_imm8(R::V0, 16);
    a.mov_reg_reg(R::V1, R::V9);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    load_rt(a, R::V1, MINIMUM_PHASE);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    let valid = a.create_text_label();
    a.mov_reg_reg(R::V0, R::V8);
    a.xor_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 0x1c00);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, valid);
    load_rt(a, R::V0, CLOSED_BANK);
    load_rt(a, R::V1, CURRENT_BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::V0, R::V8);
    load_rt(a, R::V1, SOURCE_PHASE);
    a.xor_reg_reg(R::V0, R::V1);
    a.and_reg_imm32(R::V0, 0x1c00);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(valid);
    quiescence::restore(a);
}
/// After a real empty-ledger Retire rendezvous no subsequent producer can
/// publish an old-source reference. Current-bank entries may remain at Idle.
pub(in super::super) fn require_closed<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    lifetime(a, l);
    load_rt(a, R::V0, CLOSED_BANK);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
}
