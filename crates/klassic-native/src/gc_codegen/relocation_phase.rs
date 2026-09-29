//! Two adoption boundaries keep relocation admission closed until actors agree.
use super::*;
use crate::gc_abi::{HandshakeState, RelocationGate, RelocationPhaseControl};
fn call<A: PortableAsm>(b: &mut Emit<'_, A>, target: A::TextLabel, base: i32, op: u64) {
    b.a.load_rbp_slot(R::V7, base);
    b.a.mov_imm64(R::V6, 0);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(target);
}
fn validate<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(RelocationPhaseControl, $f), 0)
        };
    }
    macro_rules! h {
        ($f:ident) => {
            field(SEG, offset_of!(HandshakeState, $f), 0)
        };
    }
    macro_rules! p {
        () => {
            field(G, offset_of!(RootRegistry, phase), 0)
        };
    }
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.set(local(SEG), q!(handshake));
    b.pointer(local(SEG), invalid);
    b.set(local(REG), q!(gate));
    b.pointer(local(REG), invalid);
    b.branch(q!(generation), n(1), C::Above, invalid);
    b.branch(and(p!(), n(0xe000)), n(0), C::NotEqual, invalid);
    b.branch(and(p!(), n(15)), n(7), C::Above, invalid);
    b.branch(and(shr(p!(), 4), n(15)), n(7), C::Above, invalid);
    b.branch(h!(active), n(1), C::Above, invalid);
    b.branch(h!(native), h!(members), C::Above, invalid);
    let empty = b.a.create_text_label();
    let head_ok = b.a.create_text_label();
    b.branch(h!(members), n(0), C::Equal, empty);
    b.branch(h!(head), n(0), C::Equal, invalid);
    b.a.jmp_label(head_ok);
    b.a.bind_text_label(empty);
    b.branch(h!(head), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(head_ok);
    b.branch(h!(acknowledged), h!(expected), C::Above, invalid);
    let inactive = b.a.create_text_label();
    let valid = b.a.create_text_label();
    b.branch(h!(active), n(0), C::Equal, inactive);
    b.branch(h!(generation), n(0), C::Equal, invalid);
    b.branch(
        h!(expected),
        sub(h!(members), h!(native)),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(valid);
    b.a.bind_text_label(inactive);
    b.branch(h!(expected), n(0), C::NotEqual, invalid);
    b.branch(h!(acknowledged), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(valid);
    b.set(local(KIND), and(p!(), n(15)));
    b.set(local(USED), and(shr(p!(), 4), n(15)));
    let phases = b.a.create_text_label();
    b.branch(q!(generation), n(0), C::Equal, phases);
    b.set(local(KIND), and(shr(p!(), 4), n(15)));
    b.set(local(USED), and(p!(), n(15)));
    b.a.bind_text_label(phases);
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let status: [_; 6] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[2];
    let wait = status[1];
    let begin = a.create_text_label();
    let checked = a.create_text_label();
    let release = a.create_text_label();
    let publish = a.create_text_label();
    let hs = a.create_text_label();
    let gate = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(RelocationPhaseControl, $f), 0)
        };
    }
    macro_rules! h {
        ($f:ident) => {
            field(SEG, offset_of!(HandshakeState, $f), 0)
        };
    }
    macro_rules! p {
        () => {
            field(G, offset_of!(RootRegistry, phase), 0)
        };
    }
    macro_rules! epoch {
        () => {
            field(REG, offset_of!(RelocationGate, epoch), 0)
        };
    }
    b.branch(local(OP), n(2), C::Above, invalid);
    validate(&mut b, invalid);
    b.branch(local(OP), n(0), C::Equal, begin);
    b.branch(q!(state), local(OP), C::NotEqual, invalid);
    b.branch(q!(rendezvous), n(0), C::Equal, invalid);
    b.branch(q!(gate_epoch), n(0), C::Equal, invalid);
    b.branch(q!(phase), p!(), C::NotEqual, status[4]);
    b.branch(q!(rendezvous), h!(generation), C::NotEqual, status[4]);
    b.branch(q!(gate_epoch), epoch!(), C::NotEqual, status[4]);
    b.branch(h!(active), n(0), C::Equal, invalid);
    b.branch(local(KIND), add(local(OP), n(4)), C::NotEqual, invalid);
    b.branch(h!(acknowledged), h!(expected), C::NotEqual, wait);
    b.a.jmp_label(checked);
    b.a.bind_text_label(begin);
    let reusable = b.a.create_text_label();
    b.branch(q!(state), n(0), C::Equal, reusable);
    b.branch(q!(state), n(3), C::NotEqual, invalid);
    b.a.bind_text_label(reusable);
    b.branch(local(KIND), n(4), C::NotEqual, invalid);
    b.branch(h!(active), n(0), C::NotEqual, wait);
    b.branch(local(USED), n(5), C::AboveOrEqual, wait);
    b.a.bind_text_label(checked);
    call(&mut b, gate, REG, 3);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(2), C::Equal, wait);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.branch(epoch!(), n(u64::MAX), C::Equal, status[3]);
    b.branch(local(OP), n(2), C::Equal, release);
    b.set(local(USED), sub(n(2), local(OP)));
    b.branch(
        h!(generation),
        sub(n(u64::MAX), local(USED)),
        C::Above,
        status[3],
    );
    b.branch(
        shr(p!(), 16),
        sub(n(u64::MAX >> 16), local(USED)),
        C::Above,
        status[3],
    );
    b.branch(local(OP), n(0), C::Equal, publish);
    call(&mut b, hs, SEG, 7);
    b.a.bind_text_label(publish);
    call(&mut b, hs, SEG, 6);
    for generation in 0..2 {
        let next = b.a.create_text_label();
        b.branch(q!(generation), n(generation), C::NotEqual, next);
        let shift = generation * 4;
        b.set(
            p!(),
            add(
                and(add(p!(), n(0x10000)), n(!(15u64 << shift))),
                if shift == 0 {
                    add(local(OP), n(5))
                } else {
                    shl(add(local(OP), n(5)), shift)
                },
            ),
        );
        let no_flip = b.a.create_text_label();
        b.branch(local(OP), n(0), C::NotEqual, no_flip);
        b.value(p!());
        b.a.mov_imm64(R::V1, 1 << (10 + generation));
        b.a.xor_reg_reg(R::V0, R::V1);
        b.a.store_rbp_slot(STATE, R::V0);
        b.set(p!(), local(STATE));
        b.a.bind_text_label(no_flip);
        b.a.jmp_label(next);
        b.a.bind_text_label(next);
    }
    b.set(q!(state), add(local(OP), n(1)));
    b.set(q!(phase), p!());
    b.set(q!(rendezvous), h!(generation));
    b.set(q!(gate_epoch), epoch!());
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(release);
    call(&mut b, gate, REG, 4);
    call(&mut b, hs, SEG, 7);
    b.set(q!(state), n(3));
    b.set(q!(gate_epoch), epoch!());
    b.a.jmp_label(status[5]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(hs);
    crate::gc_codegen::handshake::emit_step_locked(b.a);
    b.a.ret();
    b.a.bind_text_label(gate);
    super::relocation_gate::emit_relocation_gate(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}

/// A separate fresh control adopts Retire without reopening move admission.
#[allow(dead_code)]
pub(crate) fn emit_retire<A: PortableAsm>(a: &mut A) {
    let status: [_; 7] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[2];
    let wait = status[1];
    let begin = a.create_text_label();
    let checked = a.create_text_label();
    let release = a.create_text_label();
    let hs = a.create_text_label();
    let gate = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(RelocationPhaseControl, $f), 0)
        };
    }
    macro_rules! h {
        ($f:ident) => {
            field(SEG, offset_of!(HandshakeState, $f), 0)
        };
    }
    macro_rules! p {
        () => {
            field(G, offset_of!(RootRegistry, phase), 0)
        };
    }
    macro_rules! epoch {
        () => {
            field(REG, offset_of!(RelocationGate, epoch), 0)
        };
    }
    b.branch(local(OP), n(1), C::Above, invalid);
    validate(&mut b, invalid);
    b.branch(local(OP), n(0), C::Equal, begin);
    b.branch(q!(state), n(1), C::NotEqual, invalid);
    b.branch(q!(rendezvous), n(0), C::Equal, invalid);
    b.branch(q!(gate_epoch), n(0), C::Equal, invalid);
    b.branch(q!(phase), p!(), C::NotEqual, status[4]);
    b.branch(q!(rendezvous), h!(generation), C::NotEqual, status[4]);
    b.branch(q!(gate_epoch), epoch!(), C::NotEqual, status[4]);
    b.branch(h!(active), n(0), C::Equal, invalid);
    b.branch(local(KIND), n(7), C::NotEqual, invalid);
    b.branch(h!(acknowledged), h!(expected), C::NotEqual, wait);
    b.a.jmp_label(checked);
    b.a.bind_text_label(begin);
    for field in [q!(state), q!(rendezvous), q!(phase), q!(gate_epoch)] {
        b.branch(field, n(0), C::NotEqual, invalid);
    }
    b.branch(local(KIND), n(6), C::NotEqual, invalid);
    b.branch(h!(active), n(0), C::NotEqual, wait);
    b.branch(local(USED), n(5), C::AboveOrEqual, wait);
    b.a.bind_text_label(checked);
    call(&mut b, gate, REG, 3);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(2), C::Equal, wait);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.branch(local(OP), n(1), C::Equal, release);
    b.branch(h!(generation), n(u64::MAX), C::Equal, status[3]);
    b.branch(shr(p!(), 16), n(u64::MAX >> 16), C::Equal, status[3]);
    call(&mut b, hs, SEG, 6);
    for generation in 0..2 {
        let next = b.a.create_text_label();
        b.branch(q!(generation), n(generation), C::NotEqual, next);
        let shift = generation * 4;
        b.set(
            p!(),
            add(
                and(add(p!(), n(0x10000)), n(!(15u64 << shift))),
                n(7 << shift),
            ),
        );
        b.a.bind_text_label(next);
    }
    b.set(q!(state), n(1));
    b.set(q!(phase), p!());
    b.set(q!(rendezvous), h!(generation));
    b.set(q!(gate_epoch), epoch!());
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(release);
    call(&mut b, hs, SEG, 7);
    b.set(q!(state), n(2));
    b.a.jmp_label(status[6]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(hs);
    crate::gc_codegen::handshake::emit_step_locked(b.a);
    b.a.ret();
    b.a.bind_text_label(gate);
    super::relocation_gate::emit_relocation_gate(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
