//! Weak admission around mark termination; gc_weak_mark_step_locked.
use super::*;
use crate::gc_abi::{MarkPhaseControl as Mark, WeakLoadGate as Gate, WeakMarkControl as Owner};
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let wait = a.create_text_label();
    let begin = a.create_text_label();
    let call = a.create_text_label();
    let outcome = a.create_text_label();
    let begun = a.create_text_label();
    let release = a.create_text_label();
    let retry = a.create_text_label();
    let selected = a.create_text_label();
    let advanced = a.create_text_label();
    let result = a.create_text_label();
    let done = a.create_text_label();
    let phase = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(2), C::Above, invalid);
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.set(local(REG), field(Q, offset_of!(Owner, gate), 0));
    b.pointer(local(REG), invalid);
    b.set(local(SEG), field(Q, offset_of!(Owner, mark), 0));
    b.pointer(local(SEG), invalid);
    b.branch(
        field(Q, offset_of!(Owner, stage), 0),
        n(3),
        C::Above,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(Gate, registry), 0),
        local(G),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(Gate, handshake), 0),
        field(SEG, offset_of!(Mark, handshake), 0),
        C::NotEqual,
        invalid,
    );
    b.pointer(field(REG, offset_of!(Gate, pool), 0), invalid);
    b.branch(
        field(REG, offset_of!(Gate, closed), 0),
        n(1),
        C::Above,
        invalid,
    );
    b.branch(local(OP), n(0), C::Equal, begin);
    b.branch(
        field(REG, offset_of!(Gate, closed), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(Q, offset_of!(Owner, stage), 0),
        local(OP),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(Mark, state), 0),
        local(OP),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(call);
    b.a.bind_text_label(begin);
    for off in [
        offset_of!(Owner, stage),
        offset_of!(Owner, processor),
        offset_of!(Owner, scan),
    ] {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, invalid);
    }
    b.branch(
        field(REG, offset_of!(Gate, closed), 0),
        n(0),
        C::NotEqual,
        wait,
    );
    b.set(field(REG, offset_of!(Gate, closed), 0), n(1));
    b.a.bind_text_label(call);
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, SEG);
    b.a.load_rbp_slot(R::V2, OP);
    b.a.call_label(phase);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(OP), n(0), C::NotEqual, outcome);
    b.branch(local(STATE), n(0), C::Equal, begun);
    b.set(field(REG, offset_of!(Gate, closed), 0), n(0));
    b.a.jmp_label(result);
    b.a.bind_text_label(begun);
    b.set(field(Q, offset_of!(Owner, stage), 0), n(1));
    b.a.jmp_label(result);
    b.a.bind_text_label(outcome);
    b.branch(local(OP), n(2), C::Equal, release);
    b.branch(local(STATE), n(0), C::Equal, advanced);
    b.a.jmp_label(result);
    b.a.bind_text_label(advanced);
    b.set(field(Q, offset_of!(Owner, stage), 0), n(2));
    b.a.jmp_label(result);
    b.a.bind_text_label(release);
    b.branch(local(STATE), n(5), C::Equal, retry);
    b.branch(local(STATE), n(6), C::Equal, selected);
    b.a.jmp_label(result);
    b.a.bind_text_label(retry);
    b.set(field(Q, offset_of!(Owner, stage), 0), n(0));
    b.set(field(REG, offset_of!(Gate, closed), 0), n(0));
    b.a.jmp_label(result);
    b.a.bind_text_label(selected);
    b.set(field(Q, offset_of!(Owner, stage), 0), n(3));
    b.a.bind_text_label(result);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(wait);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(phase);
    super::mark_phase::emit_mark_phase(b.a);
    b.a.ret();
}
