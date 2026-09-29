//! Fair Young/Old relocation admission, gc_relocation_turn.h.
use super::*;
use crate::gc_abi::{RelocationCoordinator as T, RelocationGate, RelocationTurn as U};
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let status: [_; 5] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[2];
    let wait = status[1];
    let ok = status[0];
    let inactive = a.create_text_label();
    let validated = a.create_text_label();
    let acquire = a.create_text_label();
    let release = a.create_text_label();
    let chosen = a.create_text_label();
    let done = a.create_text_label();
    let gate = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! g {
        ($f:ident) => {
            field(G, offset_of!(T, $f), 0)
        };
    }
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(U, $f), 0)
        };
    }
    b.branch(local(OP), n(3), C::Above, invalid);
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(q!(generation), n(1), C::Above, invalid);
    b.branch(field(G, offset_of!(T, pending), 0), n(1), C::Above, invalid);
    b.branch(
        field(G, offset_of!(T, pending) + 8, 0),
        n(1),
        C::Above,
        invalid,
    );
    b.branch(g!(active), n(2), C::Above, invalid);
    b.branch(g!(turn), n(1), C::Above, invalid);
    b.branch(g!(active), n(0), C::Equal, inactive);
    b.branch(g!(serial), n(0), C::Equal, invalid);
    b.pointer(g!(gate), invalid);
    b.branch(g!(gate_epoch), n(2), C::Below, invalid);
    b.branch(g!(turn), sub(n(2), g!(active)), C::NotEqual, invalid);
    b.a.jmp_label(validated);
    b.a.bind_text_label(inactive);
    b.branch(g!(gate), n(0), C::NotEqual, invalid);
    b.branch(g!(gate_epoch), n(0), C::NotEqual, invalid);
    b.a.bind_text_label(validated);
    b.branch(local(OP), n(2), C::AboveOrEqual, release);
    b.branch(q!(coordinator), n(0), C::NotEqual, invalid);
    b.branch(q!(serial), n(0), C::NotEqual, invalid);
    b.set(
        local(REG),
        add(
            local(G),
            add(n(offset_of!(T, pending) as u64), shl(q!(generation), 3)),
        ),
    );
    b.set(
        local(SEG),
        add(
            local(G),
            add(
                n(offset_of!(T, pending) as u64),
                shl(sub(n(1), q!(generation)), 3),
            ),
        ),
    );
    b.branch(local(OP), n(1), C::Equal, acquire);
    b.set(field(REG, 0, 0), n(1));
    b.a.jmp_label(ok);
    b.a.bind_text_label(acquire);
    b.branch(g!(active), n(0), C::NotEqual, wait);
    b.branch(field(REG, 0, 0), n(0), C::Equal, status[4]);
    b.branch(field(SEG, 0, 0), n(0), C::Equal, chosen);
    b.branch(g!(turn), q!(generation), C::NotEqual, wait);
    b.a.bind_text_label(chosen);
    b.pointer(q!(gate), invalid);
    b.set(local(NEXT), q!(gate));
    b.a.load_rbp_slot(R::V7, NEXT);
    b.a.mov_imm64(R::V6, 0);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(gate);
    b.a.cmp_reg_imm8(R::V0, 2);
    b.a.jcc_label(C::Equal, wait);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.branch(g!(serial), n(u64::MAX), C::Equal, status[3]);
    b.branch(
        field(NEXT, offset_of!(RelocationGate, epoch), 0),
        n(u64::MAX),
        C::Equal,
        status[3],
    );
    b.set(g!(active), add(q!(generation), n(1)));
    b.set(g!(turn), sub(n(1), q!(generation)));
    b.set(field(REG, 0, 0), n(0));
    b.set(g!(gate), q!(gate));
    b.set(
        g!(gate_epoch),
        add(field(NEXT, offset_of!(RelocationGate, epoch), 0), n(1)),
    );
    b.set(g!(serial), add(g!(serial), n(1)));
    b.set(q!(coordinator), local(G));
    b.set(q!(serial), g!(serial));
    b.a.jmp_label(ok);
    b.a.bind_text_label(release);
    b.branch(q!(coordinator), local(G), C::NotEqual, invalid);
    b.branch(q!(serial), n(0), C::Equal, invalid);
    b.branch(q!(serial), g!(serial), C::NotEqual, invalid);
    b.branch(g!(active), add(q!(generation), n(1)), C::NotEqual, invalid);
    b.branch(q!(gate), g!(gate), C::NotEqual, invalid);
    b.set(local(NEXT), g!(gate));
    b.branch(
        field(NEXT, offset_of!(RelocationGate, epoch), 0),
        g!(gate_epoch),
        C::NotEqual,
        invalid,
    );
    b.a.load_rbp_slot(R::V7, NEXT);
    b.a.mov_imm64(R::V6, 0);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(gate);
    b.a.cmp_reg_imm8(R::V0, 2);
    b.a.jcc_label(C::Equal, wait);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    let finish = b.a.create_text_label();
    b.branch(local(OP), n(2), C::Equal, finish);
    b.branch(g!(gate_epoch), n(u64::MAX), C::Equal, status[3]);
    b.a.load_rbp_slot(R::V7, NEXT);
    b.a.mov_imm64(R::V6, 0);
    b.a.mov_imm64(R::V2, 4);
    b.a.call_label(gate);
    b.set(g!(gate_epoch), add(g!(gate_epoch), n(1)));
    b.a.jmp_label(ok);
    b.a.bind_text_label(finish);
    b.set(g!(active), n(0));
    b.set(g!(gate), n(0));
    b.set(g!(gate_epoch), n(0));
    b.set(q!(coordinator), n(0));
    b.set(q!(serial), n(0));
    b.a.jmp_label(ok);
    for (i, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(gate);
    super::relocation_gate::emit_relocation_gate(b.a);
    b.a.ret();
}
