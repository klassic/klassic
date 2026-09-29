//! Private descriptor bootstrap in native state, outside locks/source guards.
//! V7=reserved MetadataCharge, V6=capacity. Mapping is target-provided and zeroed.
//! Success still requires budget commit before publication. Failures after map
//! retain ownership for disposal and budget abort; no external callbacks occur.
use super::descriptors::Builtins;
use crate::gc_abi::{DescriptorRegistry, MetadataCharge};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

#[repr(C)]
struct Storage {
    registry: DescriptorRegistry,
    builtins: Builtins,
}
const _: () = assert!(size_of::<Storage>() == 216);

pub(crate) fn emit_prepare<E: PortableAsm>(a: &mut E, quantum: u32, map: E::TextLabel) {
    assert!(quantum.is_power_of_two() && (8..=1 << 30).contains(&quantum));
    let invalid = a.create_text_label();
    let os_error = a.create_text_label();
    let done = a.create_text_label();
    let prepare = a.create_text_label();
    let register = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 32);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V0, R::V7);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.store_rbp_slot(8, R::V7);
    a.store_rbp_slot(16, R::V6);
    a.load_ptr_disp32(R::V0, R::V7, offset_of!(MetadataCharge, stage) as i32);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V7, offset_of!(MetadataCharge, allocation) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V7, offset_of!(MetadataCharge, owner) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.cmp_reg_imm8(R::V6, 5);
    a.jcc_label(C::Below, invalid);
    a.mov_imm64(R::V0, 0x1_0000_0000);
    a.cmp_reg_reg(R::V6, R::V0);
    a.jcc_label(C::Above, invalid);
    a.load_ptr_disp32(R::V8, R::V7, offset_of!(MetadataCharge, bytes) as i32);
    a.mov_reg_reg(R::V0, R::V8);
    a.and_reg_imm32(R::V0, (quantum - 1) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_reg_reg(R::V0, R::V6);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_imm32(R::V0, size_of::<Storage>() as i32);
    a.cmp_reg_reg(R::V8, R::V0);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V7, R::V8);
    a.call_label(map);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, os_error);
    a.store_rbp_slot(24, R::V0);
    a.load_rbp_slot(R::V7, 8);
    a.store_ptr_disp32(R::V7, offset_of!(MetadataCharge, allocation) as i32, R::V0);
    a.mov_reg_reg(R::V7, R::V0);
    a.add_reg_imm32(R::V0, size_of::<Storage>() as i32);
    a.store_ptr_disp32(R::V7, offset_of!(DescriptorRegistry, slots) as i32, R::V0);
    a.load_rbp_slot(R::V0, 16);
    a.store_ptr_disp32(
        R::V7,
        offset_of!(DescriptorRegistry, capacity) as i32,
        R::V0,
    );
    a.add_reg_imm32(R::V7, offset_of!(Storage, builtins) as i32);
    a.call_label(prepare);
    for offset in [
        offset_of!(Builtins, raw),
        offset_of!(Builtins, record),
        offset_of!(Builtins, list),
    ] {
        a.load_rbp_slot(R::V7, 24);
        a.mov_reg_reg(R::V6, R::V7);
        a.add_reg_imm32(R::V6, (offset_of!(Storage, builtins) + offset) as i32);
        a.call_label(register);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(os_error);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    a.leave();
    a.ret();
    a.bind_text_label(prepare);
    super::descriptors::emit_builtin_prepare(a);
    a.ret();
    a.bind_text_label(register);
    super::descriptors::emit_register(a);
    a.ret();
}
