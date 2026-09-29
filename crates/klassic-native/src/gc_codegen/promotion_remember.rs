//! Bounded private-destination remset handoff, gc_promotion_remember.h.
use super::*;
use crate::gc_abi::{
    DescriptorScan as D, PromotionRemember as P, RememberedInventory as I,
    RememberedInventoryRequest as T, RememberedJob as J, RememberedRegistration as N,
    RememberedSet as S,
};
const SCAN: i32 = 176;
const JOB: i32 = 224;
const REQUEST: i32 = 248;
const OFFSET: i32 = 256;
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let commit = a.create_text_label();
    let done = a.create_text_label();
    let inventory = a.create_text_label();
    let descriptor = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 32);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(P, $f), 0)
        };
    }
    macro_rules! s {
        ($f:ident) => {
            field(SEG, offset_of!(S, $f), 0)
        };
    }
    b.pointer(local(Q), invalid);
    b.pointer(q!(registry), invalid);
    for (base, bytes) in [
        (SCAN, size_of::<D>()),
        (JOB, size_of::<J>()),
        (REQUEST, size_of::<T>()),
    ] {
        for i in 0..bytes / 8 {
            b.set(local(base - i as i32 * 8), n(0));
        }
    }
    b.set(local(REQUEST), q!(registration));
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, JOB);
    b.a.store_rbp_slot(REQUEST - offset_of!(T, job) as i32, R::V0);
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, REQUEST);
    b.a.mov_imm64(R::V2, 8);
    b.a.call_label(inventory);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.set(local(REG), q!(registration));
    b.set(local(SEG), field(REG, offset_of!(N, set), 0));
    b.branch(q!(owner), add(s!(start), n(32)), C::Below, invalid);
    b.branch(q!(owner), s!(limit), C::Above, invalid);
    b.branch(and(q!(owner), n(15)), n(0), C::NotEqual, invalid);
    b.branch(
        q!(payload_bytes),
        sub(s!(limit), q!(owner)),
        C::Above,
        invalid,
    );
    b.branch(
        q!(phase),
        field(G, offset_of!(I, epoch), 0),
        C::NotEqual,
        wait,
    );
    b.branch(
        s!(epoch),
        field(G, offset_of!(I, epoch), 0),
        C::NotEqual,
        wait,
    );
    b.set(
        local(SCAN - offset_of!(D, descriptor_id) as i32),
        q!(descriptor_id),
    );
    b.set(
        local(SCAN - offset_of!(D, payload_bytes) as i32),
        q!(payload_bytes),
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
    b.set(
        local(JOB - offset_of!(J, offset) as i32),
        add(q!(owner), local(OFFSET)),
    );
    b.set(
        local(JOB - offset_of!(J, incarnation) as i32),
        s!(incarnation),
    );
    b.set(local(JOB - offset_of!(J, phase) as i32), q!(phase));
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, REQUEST);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(inventory);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.a.bind_text_label(commit);
    b.set(q!(cursor), local(SCAN - offset_of!(D, next_offset) as i32));
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(wait);
    b.a.mov_imm64(R::V0, 3);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(inventory);
    super::remembered_inventory::emit_inventory(b.a);
    b.a.ret();
    b.a.bind_text_label(descriptor);
    crate::gc_codegen::descriptors::emit_scan(b.a);
    b.a.ret();
}
