//! Native-state root backing preparation using a target-provided map entry.
//! Input V7=reserved charge, V6=immutable spec; map takes V7=bytes and returns
//! V0=zeroed mapping or null, clobbering portable volatile registers. No commit
//! or registration occurs here. The caller excludes concurrent ownership changes.
use crate::portable_asm::{Condition, PortableAsm, Reg};

pub(crate) fn emit_prepare<E: PortableAsm>(a: &mut E, quantum: u32, map: E::TextLabel) {
    assert!(quantum.is_power_of_two() && (8..=1 << 30).contains(&quantum));
    use crate::gc_abi::{MetadataCharge as Q, RootStorage, RootStorageSpec as S};
    use std::mem::{offset_of, size_of};
    let invalid = a.create_text_label();
    let os_error = a.create_text_label();
    let done = a.create_text_label();
    let initialize = a.create_text_label();
    a.push_reg(Reg::V5);
    a.mov_reg_reg(Reg::V5, Reg::V4);
    a.sub_reg_imm8(Reg::V4, 16);
    for reg in [Reg::V7, Reg::V6] {
        a.test_reg_reg(reg, reg);
        a.jcc_label(Condition::Equal, invalid);
        a.mov_reg_reg(Reg::V0, reg);
        a.and_reg_imm32(Reg::V0, 7);
        a.test_reg_reg(Reg::V0, Reg::V0);
        a.jcc_label(Condition::NotEqual, invalid);
    }
    a.store_rbp_slot(8, Reg::V7);
    a.store_rbp_slot(16, Reg::V6);
    a.load_ptr_disp32(Reg::V0, Reg::V7, offset_of!(Q, stage) as i32);
    a.cmp_reg_imm8(Reg::V0, 1);
    a.jcc_label(Condition::NotEqual, invalid);
    a.load_ptr_disp32(Reg::V0, Reg::V7, offset_of!(Q, allocation) as i32);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::NotEqual, invalid);
    a.load_ptr_disp32(Reg::V0, Reg::V7, offset_of!(Q, owner) as i32);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::Equal, invalid);
    a.and_reg_imm32(Reg::V0, 7);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::NotEqual, invalid);
    a.load_ptr_disp32(Reg::V8, Reg::V7, offset_of!(Q, bytes) as i32);
    a.test_reg_reg(Reg::V8, Reg::V8);
    a.jcc_label(Condition::Equal, invalid);
    a.mov_reg_reg(Reg::V0, Reg::V8);
    a.and_reg_imm32(Reg::V0, (quantum - 1) as i32);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::NotEqual, invalid);
    a.load_ptr_disp32(Reg::V0, Reg::V6, offset_of!(S, bytes) as i32);
    a.cmp_reg_reg(Reg::V0, Reg::V8);
    a.jcc_label(Condition::NotEqual, invalid);
    a.load_ptr_disp32(Reg::V0, Reg::V6, offset_of!(S, owner) as i32);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::Equal, invalid);
    a.mov_imm64(Reg::V0, u32::MAX as u64);
    for (reg, offset) in [
        (Reg::V9, offset_of!(S, cells)),
        (Reg::V10, offset_of!(S, entries)),
    ] {
        a.load_ptr_disp32(reg, Reg::V6, offset as i32);
        a.test_reg_reg(reg, reg);
        a.jcc_label(Condition::Equal, invalid);
        a.cmp_reg_reg(reg, Reg::V0);
        a.jcc_label(Condition::Above, invalid);
    }
    a.add_reg_reg(Reg::V9, Reg::V10);
    a.shl_reg_imm8(Reg::V9, 5);
    a.add_reg_imm32(Reg::V9, size_of::<RootStorage>() as i32);
    a.cmp_reg_reg(Reg::V8, Reg::V9);
    a.jcc_label(Condition::Below, invalid);
    a.mov_reg_reg(Reg::V7, Reg::V8);
    a.call_label(map);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::Equal, os_error);
    a.load_rbp_slot(Reg::V7, 8);
    a.store_ptr_disp32(Reg::V7, offset_of!(Q, allocation) as i32, Reg::V0);
    a.mov_reg_reg(Reg::V7, Reg::V0);
    a.load_rbp_slot(Reg::V6, 16);
    a.call_label(initialize);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(Reg::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(os_error);
    a.mov_imm64(Reg::V0, 2);
    a.bind_text_label(done);
    a.leave();
    a.ret();
    a.bind_text_label(initialize);
    super::root_storage::emit_prepare(a);
    a.ret();
}

/// V7=owned charge -> OK/INVALID/OS_ERROR. Caller excludes concurrent attach.
/// The target unmap entry takes V7=base/V6=bytes and returns a boolean in V0.
/// Only successful disposal clears allocation; budget release remains separate.
pub(crate) fn emit_dispose<E: PortableAsm>(a: &mut E, quantum: u32, unmap: E::TextLabel) {
    use crate::gc_abi::{MetadataCharge as Q, RootStorage};
    use std::mem::{offset_of, size_of};
    assert!(quantum.is_power_of_two() && (8..=1 << 30).contains(&quantum));
    let invalid = a.create_text_label();
    let os_error = a.create_text_label();
    let done = a.create_text_label();
    let check = a.create_text_label();
    a.push_reg(Reg::V5);
    a.mov_reg_reg(Reg::V5, Reg::V4);
    a.sub_reg_imm8(Reg::V4, 16);
    a.test_reg_reg(Reg::V7, Reg::V7);
    a.jcc_label(Condition::Equal, invalid);
    a.mov_reg_reg(Reg::V0, Reg::V7);
    a.and_reg_imm32(Reg::V0, 7);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::NotEqual, invalid);
    a.store_rbp_slot(8, Reg::V7);
    a.load_ptr_disp32(Reg::V0, Reg::V7, offset_of!(Q, stage) as i32);
    a.cmp_reg_imm8(Reg::V0, 1);
    a.jcc_label(Condition::Below, invalid);
    a.cmp_reg_imm8(Reg::V0, 2);
    a.jcc_label(Condition::Above, invalid);
    for (offset, mask) in [
        (offset_of!(Q, owner), 7),
        (offset_of!(Q, bytes), (quantum - 1) as i32),
        (offset_of!(Q, allocation), (quantum - 1) as i32),
    ] {
        a.load_ptr_disp32(Reg::V0, Reg::V7, offset as i32);
        a.test_reg_reg(Reg::V0, Reg::V0);
        a.jcc_label(Condition::Equal, invalid);
        a.and_reg_imm32(Reg::V0, mask);
        a.test_reg_reg(Reg::V0, Reg::V0);
        a.jcc_label(Condition::NotEqual, invalid);
    }
    a.load_ptr_disp32(Reg::V0, Reg::V7, offset_of!(Q, bytes) as i32);
    a.cmp_reg_imm32(Reg::V0, size_of::<RootStorage>() as i32);
    a.jcc_label(Condition::Below, invalid);
    a.load_ptr_disp32(Reg::V7, Reg::V7, offset_of!(Q, allocation) as i32);
    a.call_label(check);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::NotEqual, invalid);
    a.load_rbp_slot(Reg::V9, 8);
    a.load_ptr_disp32(Reg::V6, Reg::V9, offset_of!(Q, bytes) as i32);
    a.load_ptr_disp32(Reg::V7, Reg::V9, offset_of!(Q, allocation) as i32);
    a.call_label(unmap);
    a.test_reg_reg(Reg::V0, Reg::V0);
    a.jcc_label(Condition::Equal, os_error);
    a.load_rbp_slot(Reg::V7, 8);
    a.mov_imm64(Reg::V0, 0);
    a.store_ptr_disp32(Reg::V7, offset_of!(Q, allocation) as i32, Reg::V0);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(Reg::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(os_error);
    a.mov_imm64(Reg::V0, 2);
    a.bind_text_label(done);
    a.leave();
    a.ret();
    a.bind_text_label(check);
    super::root_storage::emit_can_dispose(a);
    a.ret();
}
