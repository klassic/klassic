//! Bounded private cleanup followed by source retention and admission release.
use super::*;
use crate::gc_abi::{ForwardCell, RelocationCancellation, RelocationCopy, RelocationDiscard};
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(RelocationCancellation, $f), 0)
    };
}
macro_rules! discard {
    ($f:ident) => {
        field(REG, offset_of!(RelocationDiscard, $f), 0)
    };
}
macro_rules! copy {
    ($f:ident) => {
        field(SEG, offset_of!(RelocationCopy, $f), 0)
    };
}
macro_rules! src {
    ($f:ident,$a:expr) => {
        field(CTX, offset_of!(ForwardCell, $f), $a)
    };
}
fn gate_call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, op: u64) {
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, NEXT);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
#[allow(dead_code)]
pub(crate) fn emit_relocation_cancel<A: PortableAsm>(a: &mut A) {
    let status: [_; 3] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let fresh = a.create_text_label();
    let controls = a.create_text_label();
    let inputs = a.create_text_label();
    let staged = a.create_text_label();
    let reopen = a.create_text_label();
    let release = a.create_text_label();
    let cleared = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let gate = a.create_text_label();
    let cleanup = a.create_text_label();
    let forward = a.create_text_label();
    let access = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 80);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    for (slot, offset) in [(NEXT, 144), (PREV, 192)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, offset);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    b.branch(req!(stage), n(3), C::Above, invalid);
    b.branch(req!(owner), n(0), C::Equal, fresh);
    b.branch(req!(owner), local(Q), C::NotEqual, invalid);
    b.a.jmp_label(controls);
    b.a.bind_text_label(fresh);
    b.branch(req!(stage), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(controls);
    b.pointer(req!(target), invalid);
    b.pointer(req!(discard), invalid);
    b.set(local(REG), req!(discard));
    b.pointer(discard!(source), invalid);
    b.pointer(discard!(copy), invalid);
    b.branch(req!(owner), n(0), C::NotEqual, inputs);
    for value in [discard!(owner), discard!(cursor), discard!(bytes)] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.a.bind_text_label(inputs);
    b.set(local(CTX), discard!(source));
    b.set(local(SEG), discard!(copy));
    b.branch(req!(serial), n(0), C::Equal, invalid);
    b.branch(req!(serial), n(u64::MAX >> 2), C::Above, invalid);
    b.branch(copy!(ticket), req!(serial), C::NotEqual, invalid);
    b.branch(src!(incarnation, 0), n(0), C::Equal, invalid);
    b.branch(
        copy!(source_incarnation),
        src!(incarnation, 0),
        C::NotEqual,
        invalid,
    );
    b.branch(src!(source_offset, 0), n(48), C::Below, invalid);
    b.branch(
        and(src!(source_offset, 0), n(15)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        src!(source_offset, 0),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    for (offset, value) in [(0, req!(permit)), (8, req!(serial)), (16, req!(epoch))] {
        b.set(field(NEXT, offset, 0), value);
    }
    gate_call(&mut b, gate, 5);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.branch(req!(stage), n(0), C::NotEqual, staged);
    b.value(req!(target));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.call_label(cleanup);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(1), C::Equal, invalid);
    b.set(req!(owner), local(Q));
    b.branch(local(STATE), n(0), C::Equal, cleared);
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(cleared);
    b.set(req!(stage), n(1));
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(staged);
    b.branch(discard!(owner), local(REG), C::NotEqual, invalid);
    b.branch(discard!(cursor), discard!(bytes), C::NotEqual, invalid);
    b.branch(req!(stage), n(2), C::Equal, reopen);
    b.branch(req!(stage), n(3), C::Equal, release);
    for (offset, value) in [
        (0, src!(source_offset, 0)),
        (8, src!(incarnation, 0)),
        (16, req!(serial)),
        (24, n(0)),
    ] {
        b.set(field(PREV, offset, 0), value);
    }
    b.a.load_rbp_slot(R::V7, CTX);
    b.a.load_rbp_slot(R::V6, PREV);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(forward);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(req!(stage), n(2));
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(reopen);
    for (offset, value) in [
        (0, n(0)),
        (8, src!(source_offset, 0)),
        (16, src!(incarnation, 0)),
        (24, n(0)),
        (32, n(0)),
    ] {
        b.set(field(PREV, offset, 0), value);
    }
    b.a.load_rbp_slot(R::V7, CTX);
    b.a.load_rbp_slot(R::V6, PREV);
    b.a.mov_imm64(R::V2, 4);
    b.a.call_label(access);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(req!(stage), n(3));
    b.a.jmp_label(status[2]);
    b.a.bind_text_label(release);
    b.branch(src!(word, 1), n(3), C::NotEqual, invalid);
    b.branch(and(src!(access, 1), n(1 << 63)), n(0), C::NotEqual, invalid);
    gate_call(&mut b, gate, 1);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(req!(stage), n(4));
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(gate);
    super::relocation_gate::emit_relocation_gate(b.a);
    b.a.ret();
    b.a.bind_text_label(cleanup);
    super::relocate_discard::emit_relocate_discard(b.a);
    b.a.ret();
    b.a.bind_text_label(forward);
    super::forward::emit_forward_step(b.a);
    b.a.ret();
    b.a.bind_text_label(access);
    super::source_access::emit_source_access(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
