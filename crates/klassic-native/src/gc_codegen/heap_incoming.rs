//! Bounded canonical heap incoming-reference slice, gc_heap_incoming.h.
use super::*;
use crate::gc_abi::{DescriptorScan as D, HeapIncoming as T, ObjectHeader as H};
const SCAN: i32 = 176;
const OFFSET: i32 = 184;
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let commit = a.create_text_label();
    let done = a.create_text_label();
    let descriptor = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 80);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }
    b.pointer(local(Q), invalid);
    b.pointer(q!(registry), invalid);
    b.branch(q!(observed), n(0), C::NotEqual, invalid);
    b.branch(q!(target_start), n(0), C::Equal, invalid);
    b.branch(q!(target_start), q!(target_limit), C::AboveOrEqual, invalid);
    b.branch(
        q!(target_limit),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    for v in [q!(target_start), q!(target_limit)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, invalid);
    }
    super::barrier::guarded_object(&mut b, q!(scope), q!(page), q!(phase), invalid);
    for i in 0..8 {
        b.set(local(SCAN - i * 8), n(0));
    }
    b.set(local(SCAN), field(REG, offset_of!(H, descriptor_id), 0));
    b.set(
        local(SCAN - offset_of!(D, payload_bytes) as i32),
        field(REG, offset_of!(H, payload_bytes), 0),
    );
    b.set(local(SCAN - offset_of!(D, next_offset) as i32), q!(cursor));
    b.set(local(SCAN - offset_of!(D, slot_budget) as i32), n(1));
    b.set(local(SCAN - offset_of!(D, output_capacity) as i32), n(1));
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, OFFSET);
    b.a.store_rbp_slot(SCAN - offset_of!(D, output) as i32, R::V0);
    b.value(q!(registry));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, SCAN);
    b.a.call_label(descriptor);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(2), C::Equal, invalid);
    b.branch(
        local(SCAN - offset_of!(D, written) as i32),
        n(0),
        C::Equal,
        commit,
    );
    b.set(local(SEG), add(add(local(REG), n(32)), local(OFFSET)));
    b.set(local(USED), field(SEG, 0, 8));
    super::barrier::reference(&mut b, local(USED), invalid);
    b.branch(local(USED), n(0), C::Equal, commit);
    b.branch(shr(local(USED), 16), q!(target_start), C::Below, commit);
    b.branch(
        shr(local(USED), 16),
        q!(target_limit),
        C::AboveOrEqual,
        commit,
    );
    b.set(q!(cursor), local(OFFSET));
    b.set(q!(observed), local(USED));
    b.a.mov_imm64(R::V0, 3);
    b.a.jmp_label(done);
    b.a.bind_text_label(commit);
    b.set(q!(cursor), local(SCAN - offset_of!(D, next_offset) as i32));
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(descriptor);
    crate::gc_codegen::descriptors::emit_scan(b.a);
    b.a.ret();
}
