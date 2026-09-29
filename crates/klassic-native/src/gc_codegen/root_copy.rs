//! Same-owner root assignment; source/destination requests never change.
use super::*;
use crate::gc_abi::RootRequest;
const REQUEST: i32 = 160;

/// V7=context, V6=source request, V2=destination request.
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let step = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(SEG, R::V2);
    let mut b = Emit { a };
    for slot in [G, Q, SEG] {
        b.pointer(local(slot), invalid);
    }
    for i in 0..size_of::<RootRequest>() / 8 {
        b.set(local(REQUEST - i as i32 * 8), field(Q, i * 8, 0));
    }
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, REQUEST);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(step);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, done);
    b.set(
        local(REG),
        local(REQUEST - offset_of!(RootRequest, value) as i32),
    );
    for i in 0..size_of::<RootRequest>() / 8 {
        b.set(local(REQUEST - i as i32 * 8), field(SEG, i * 8, 0));
    }
    b.set(
        local(REQUEST - offset_of!(RootRequest, value) as i32),
        local(REG),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, REQUEST);
    b.a.mov_imm64(R::V2, 1);
    b.a.call_label(step);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(step);
    crate::gc_codegen::roots::emit_step(b.a);
    b.a.ret();
}
