//! Claim/receipt commit after all returned children have acquired protection.
use super::*;
use crate::gc_abi::{MarkRequest, MarkScan};

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let finish = a.create_text_label();
    let call = a.create_text_label();
    let success = a.create_text_label();
    let done = a.create_text_label();
    let step = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(local(OP), n(1), C::Above, invalid);
    b.set(local(SEG), field(Q, offset_of!(MarkScan, job), 0));
    b.pointer(local(SEG), invalid);
    b.branch(
        field(Q, offset_of!(MarkScan, capacity), 0),
        n(1),
        C::Below,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(MarkScan, capacity), 0),
        n(256),
        C::Above,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(MarkScan, examined), 0),
        field(Q, offset_of!(MarkScan, capacity), 0),
        C::Above,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(MarkScan, written), 0),
        field(Q, offset_of!(MarkScan, examined), 0),
        C::Above,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(MarkRequest, index), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(MarkRequest, serial), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.set(local(REG), field(SEG, offset_of!(MarkRequest, cursor), 0));
    b.branch(
        field(Q, offset_of!(MarkScan, next_cursor), 0),
        local(REG),
        C::Below,
        invalid,
    );
    b.branch(local(OP), n(0), C::Equal, finish);
    b.branch(
        field(Q, offset_of!(MarkScan, next_cursor), 0),
        local(REG),
        C::Equal,
        invalid,
    );
    b.branch(
        and(field(Q, offset_of!(MarkScan, next_cursor), 0), n(7)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.set(local(OP), n(3));
    b.a.jmp_label(call);
    b.a.bind_text_label(finish);
    b.set(local(OP), n(2));
    b.a.bind_text_label(call);
    b.set(
        field(SEG, offset_of!(MarkRequest, cursor), 0),
        field(Q, offset_of!(MarkScan, next_cursor), 0),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, SEG);
    b.a.load_rbp_slot(R::V2, OP);
    b.a.call_label(step);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::Equal, success);
    b.set(field(SEG, offset_of!(MarkRequest, cursor), 0), local(REG));
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(success);
    b.set(field(Q, offset_of!(MarkScan, job), 0), n(0));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(step);
    super::mark::emit_mark_step(b.a);
    b.a.ret();
}
