//! Prepared mark-start/end boundaries with uninterrupted phase adoption.
use super::*;
use crate::gc_abi::{HandshakeState, MarkEndRequest, MarkPhaseControl, MarkQueue, RootAudit};
fn handshake<A: PortableAsm>(b: &mut Emit<'_, A>, core: A::TextLabel, op: u64) {
    b.a.load_rbp_slot(R::V7, SEG);
    b.a.mov_imm64(R::V6, 0);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(core);
}
fn snapshot<A: PortableAsm>(b: &mut Emit<'_, A>) {
    let old = b.a.create_text_label();
    let done = b.a.create_text_label();
    b.branch(
        field(Q, offset_of!(MarkPhaseControl, generation), 0),
        n(1),
        C::Equal,
        old,
    );
    b.set(
        field(G, offset_of!(RootRegistry, phase), 0),
        add(
            and(
                add(field(G, offset_of!(RootRegistry, phase), 0), n(0x10000)),
                n(!15u64),
            ),
            local(STATE),
        ),
    );
    b.a.jmp_label(done);
    b.a.bind_text_label(old);
    b.set(
        field(G, offset_of!(RootRegistry, phase), 0),
        add(
            and(
                add(field(G, offset_of!(RootRegistry, phase), 0), n(0x10000)),
                n(!0xf0u64),
            ),
            shl(local(STATE), 4),
        ),
    );
    b.a.bind_text_label(done);
}
#[allow(dead_code)]
pub(crate) fn emit_mark_phase<A: PortableAsm>(a: &mut A) {
    let status: [_; 8] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[2];
    let wait = status[1];
    let begin = a.create_text_label();
    let start = a.create_text_label();
    let activate = a.create_text_label();
    let publish = a.create_text_label();
    let release = a.create_text_label();
    let commit = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let hs_core = a.create_text_label();
    let end_core = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 96);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    a.mov_reg_reg(R::V0, R::V5);
    a.add_reg_imm32(R::V0, -144);
    a.store_rbp_slot(PREV, R::V0);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(MarkPhaseControl, $f), 0)
        };
    }
    macro_rules! h {
        ($f:ident) => {
            field(SEG, offset_of!(HandshakeState, $f), 0)
        };
    }
    macro_rules! phase {
        () => {
            field(G, offset_of!(RootRegistry, phase), 0)
        };
    }
    macro_rules! queue {
        ($f:ident) => {
            field(REG, offset_of!(MarkQueue, $f), 0)
        };
    }
    b.branch(local(OP), n(4), C::Above, invalid);
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.set(local(SEG), q!(handshake));
    b.pointer(local(SEG), invalid);
    b.set(local(LEASE), q!(audit));
    b.pointer(local(LEASE), invalid);
    b.branch(q!(generation), n(1), C::Above, invalid);
    b.branch(and(phase!(), n(0xe000)), n(0), C::NotEqual, invalid);
    b.branch(and(phase!(), n(15)), n(7), C::Above, invalid);
    b.branch(and(shr(phase!(), 4), n(15)), n(7), C::Above, invalid);
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
    b.set(local(REG), field(LEASE, offset_of!(RootAudit, queue), 0));
    b.pointer(local(REG), invalid);
    b.branch(queue!(epoch), n(0), C::Equal, invalid);
    b.branch(queue!(generation), q!(generation), C::NotEqual, invalid);
    b.set(local(KIND), and(phase!(), n(15)));
    let selected = b.a.create_text_label();
    b.branch(q!(generation), n(0), C::Equal, selected);
    b.set(local(KIND), and(shr(phase!(), 4), n(15)));
    b.a.bind_text_label(selected);
    b.branch(local(OP), n(0), C::Equal, begin);
    b.branch(local(OP), n(3), C::Equal, start);
    b.set(local(STATE), n(1));
    let check_state = b.a.create_text_label();
    b.branch(local(OP), n(1), C::Equal, check_state);
    b.set(local(STATE), n(4));
    b.branch(local(OP), n(4), C::Equal, check_state);
    b.set(local(STATE), n(5));
    b.branch(q!(state), n(5), C::Equal, check_state);
    b.set(local(STATE), n(2));
    b.a.bind_text_label(check_state);
    b.branch(q!(state), local(STATE), C::NotEqual, invalid);
    b.branch(q!(rendezvous), n(0), C::Equal, invalid);
    b.branch(q!(epoch), n(0), C::Equal, invalid);
    b.branch(q!(rendezvous), h!(generation), C::NotEqual, status[4]);
    b.branch(q!(phase), phase!(), C::NotEqual, status[4]);
    b.branch(q!(epoch), queue!(epoch), C::NotEqual, status[4]);
    b.branch(h!(active), n(0), C::Equal, invalid);
    b.branch(h!(acknowledged), h!(expected), C::NotEqual, wait);
    b.branch(local(OP), n(2), C::Equal, release);
    b.branch(local(OP), n(4), C::Equal, activate);
    b.branch(local(KIND), n(3), C::NotEqual, invalid);
    b.branch(q!(outcome), n(0), C::NotEqual, invalid);
    b.branch(h!(generation), n(u64::MAX), C::Equal, status[3]);
    b.branch(shr(phase!(), 16), n(u64::MAX >> 16), C::Equal, status[3]);
    b.set(
        field(PREV, offset_of!(MarkEndRequest, handshake), 0),
        local(SEG),
    );
    b.set(
        field(PREV, offset_of!(MarkEndRequest, audit), 0),
        local(LEASE),
    );
    b.set(
        field(PREV, offset_of!(MarkEndRequest, generation), 0),
        q!(rendezvous),
    );
    b.set(
        field(PREV, offset_of!(MarkEndRequest, remembered), 0),
        q!(remembered),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, PREV);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(end_core);
    b.a.store_rbp_slot(NEXT, R::V0);
    b.branch(local(NEXT), n(1), C::Equal, invalid);
    b.set(local(STATE), n(2));
    let decided = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::NotEqual, decided);
    b.set(local(STATE), n(4));
    b.a.bind_text_label(decided);
    b.a.bind_text_label(publish);
    handshake(&mut b, hs_core, 7);
    handshake(&mut b, hs_core, 6);
    snapshot(&mut b);
    b.set(q!(state), n(2));
    let state_set = b.a.create_text_label();
    b.branch(local(OP), n(4), C::NotEqual, state_set);
    b.set(q!(state), n(5));
    b.a.bind_text_label(state_set);
    b.set(q!(outcome), local(STATE));
    b.a.jmp_label(commit);
    b.a.bind_text_label(release);
    let outcome = b.a.create_text_label();
    b.branch(q!(outcome), n(2), C::Equal, outcome);
    b.branch(q!(outcome), n(4), C::NotEqual, invalid);
    b.a.bind_text_label(outcome);
    b.branch(local(KIND), q!(outcome), C::NotEqual, invalid);
    let releasing = b.a.create_text_label();
    b.branch(q!(state), n(5), C::NotEqual, releasing);
    b.branch(q!(outcome), n(2), C::NotEqual, invalid);
    b.a.bind_text_label(releasing);
    b.set(local(STATE), q!(state));
    handshake(&mut b, hs_core, 7);
    b.set(q!(state), n(3));
    b.branch(local(STATE), n(5), C::Equal, status[7]);
    b.branch(q!(outcome), n(2), C::Equal, status[5]);
    b.a.jmp_label(status[6]);
    b.a.bind_text_label(begin);
    let reusable = b.a.create_text_label();
    b.branch(q!(state), n(0), C::Equal, reusable);
    b.branch(q!(state), n(3), C::NotEqual, invalid);
    b.a.bind_text_label(reusable);
    b.branch(local(KIND), n(2), C::NotEqual, invalid);
    b.branch(h!(active), n(0), C::NotEqual, wait);
    b.branch(h!(generation), n(u64::MAX - 2), C::Above, status[3]);
    b.branch(
        shr(phase!(), 16),
        n((u64::MAX >> 16) - 2),
        C::Above,
        status[3],
    );
    handshake(&mut b, hs_core, 6);
    b.set(local(STATE), n(3));
    snapshot(&mut b);
    b.set(q!(state), n(1));
    b.set(q!(outcome), n(0));
    b.set(q!(epoch), queue!(epoch));
    b.a.jmp_label(commit);
    b.a.bind_text_label(start);
    let start_reusable = b.a.create_text_label();
    b.branch(q!(state), n(0), C::Equal, start_reusable);
    b.branch(q!(state), n(3), C::NotEqual, invalid);
    b.a.bind_text_label(start_reusable);
    b.branch(local(KIND), n(1), C::NotEqual, invalid);
    b.branch(h!(active), n(0), C::NotEqual, wait);
    b.branch(h!(generation), n(u64::MAX - 2), C::Above, status[3]);
    b.branch(shr(phase!(), 16), n(u64::MAX >> 16), C::Equal, status[3]);
    handshake(&mut b, hs_core, 6);
    b.set(q!(state), n(4));
    b.set(q!(outcome), n(0));
    b.set(q!(epoch), queue!(epoch));
    b.a.jmp_label(commit);
    b.a.bind_text_label(activate);
    b.branch(local(KIND), n(1), C::NotEqual, invalid);
    b.branch(q!(outcome), n(0), C::NotEqual, invalid);
    b.branch(h!(generation), n(u64::MAX), C::Equal, status[3]);
    b.branch(shr(phase!(), 16), n(u64::MAX >> 16), C::Equal, status[3]);
    b.set(local(STATE), n(2));
    b.a.jmp_label(publish);
    b.a.bind_text_label(commit);
    b.set(q!(rendezvous), h!(generation));
    b.set(q!(phase), phase!());
    b.a.jmp_label(status[0]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(hs_core);
    crate::gc_codegen::handshake::emit_step_locked(b.a);
    b.a.ret();
    b.a.bind_text_label(end_core);
    super::mark_end::emit_mark_end(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
