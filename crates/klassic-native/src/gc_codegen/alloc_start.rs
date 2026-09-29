//! Compiler allocation entry: shape resolution, output-root reservation, TLAB
//! attempt and empty-root cancellation. Same contract as gc_alloc_start.h.
use crate::gc_abi::{AllocationShape, AllocationStart, RootCell, RootHandle, RootRequest};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

#[allow(dead_code)]
pub(crate) fn emit_start<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let full = a.create_text_label();
    let exhausted = a.create_text_label();
    let done = a.create_text_label();
    let shape = a.create_text_label();
    let reserve = a.create_text_label();
    let allocate = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    // ctx, query, root, shape, AllocationInto (two words), result.
    a.sub_reg_imm8(R::V4, 64);
    for r in [R::V7, R::V6] {
        a.test_reg_reg(r, r);
        a.jcc_label(C::Equal, invalid);
        a.mov_reg_reg(R::V0, r);
        a.and_reg_imm32(R::V0, 7);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.store_rbp_slot(8, R::V7);
    a.store_rbp_slot(16, R::V6);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(AllocationStart, root) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.store_rbp_slot(24, R::V0);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V7, R::V6, offset_of!(AllocationStart, registry) as i32);
    a.load_ptr_disp32(R::V6, R::V6, offset_of!(AllocationStart, shape) as i32);
    a.store_rbp_slot(32, R::V6);
    a.call_label(shape);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.load_rbp_slot(R::V7, 8);
    a.load_rbp_slot(R::V6, 24);
    a.mov_imm64(R::V2, 0);
    a.call_label(reserve);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, full);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::Equal, exhausted);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_rbp_slot(R::V0, 32);
    a.load_ptr_disp32(R::V0, R::V0, offset_of!(AllocationShape, allocation) as i32);
    a.store_rbp_slot(48, R::V0);
    a.load_rbp_slot(R::V0, 24);
    a.store_rbp_slot(40, R::V0);
    a.load_rbp_slot(R::V7, 8);
    a.mov_reg_reg(R::V6, R::V5);
    a.sub_reg_imm8(R::V6, 48);
    a.call_label(allocate);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    // Failed allocation cannot have published a value to the reserved cell.
    a.load_rbp_slot(R::V7, 24);
    a.load_ptr_disp32(
        R::V7,
        R::V7,
        (offset_of!(RootRequest, handle) + offset_of!(RootHandle, cell)) as i32,
    );
    a.mov_imm64(R::V1, 0);
    a.store_ptr_disp32(R::V7, offset_of!(RootCell, active) as i32, R::V1);
    a.jmp_label(done);
    a.bind_text_label(full);
    a.mov_imm64(R::V0, 7);
    a.jmp_label(done);
    a.bind_text_label(exhausted);
    a.mov_imm64(R::V0, 6);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    a.leave();
    a.ret();
    a.bind_text_label(shape);
    super::descriptors::emit_allocation_shape(a);
    a.ret();
    a.bind_text_label(reserve);
    super::roots::emit_step(a);
    a.ret();
    a.bind_text_label(allocate);
    super::root_lifetime::alloc_into::emit_alloc_into(a);
    a.ret();
}
