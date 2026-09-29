//! Read-only, rendezvous-bound mark termination; matching gc_mark_end.h.
use super::*;
use crate::gc_abi::{
    BufferConsumer, BufferCursor, HandshakeState, MarkEndRequest, MarkQueue, RememberedInventory,
    RootAudit, ThreadRegistration,
};
fn audit_call<A: PortableAsm>(
    b: &mut Emit<'_, A>,
    core: A::TextLabel,
    op: u64,
    status: &[A::TextLabel; 4],
) {
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(core);
    b.a.store_rbp_slot(KIND, R::V0);
    b.branch(local(KIND), n(5), C::Equal, status[3]);
    b.branch(local(KIND), n(4), C::Equal, status[2]);
    b.branch(local(KIND), n(2), C::NotEqual, status[1]);
}
#[allow(dead_code)]
pub(crate) fn emit_mark_end<A: PortableAsm>(a: &mut A) {
    emit_mode(a, false);
}
#[allow(dead_code)]
pub(crate) fn emit_retire_inventory<A: PortableAsm>(a: &mut A) {
    emit_mode(a, true);
}
fn emit_mode<A: PortableAsm>(a: &mut A, retire: bool) {
    let status: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let wait = status[2];
    let done = a.create_text_label();
    let after = a.create_text_label();
    let audit_core = a.create_text_label();
    let mark_core = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for _ in 0..3 {
        a.sub_reg_imm8(R::V4, 96);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    // Fixed 192-byte private copy; CHECK/OWNER never mutate the caller's audit.
    a.mov_reg_reg(R::V0, R::V5);
    a.add_reg_imm32(R::V0, -288);
    a.store_rbp_slot(LEASE, R::V0);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(MarkEndRequest, $f), 0)
        };
    }
    macro_rules! h {
        ($f:ident) => {
            field(SEG, offset_of!(HandshakeState, $f), 0)
        };
    }
    macro_rules! p {
        ($f:ident) => {
            field(REG, offset_of!(ThreadRegistration, $f), 0)
        };
    }
    macro_rules! g {
        ($f:ident) => {
            field(G, offset_of!(RootRegistry, $f), 0)
        };
    }
    b.branch(local(OP), n(0), C::NotEqual, invalid);
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.set(local(SEG), q!(handshake));
    b.pointer(local(SEG), invalid);
    b.set(local(OP), q!(audit));
    b.pointer(local(OP), invalid);
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
    b.branch(q!(generation), n(0), C::Equal, invalid);
    b.branch(q!(generation), h!(generation), C::NotEqual, status[3]);
    b.branch(h!(active), n(0), C::Equal, wait);
    b.branch(h!(acknowledged), h!(expected), C::NotEqual, wait);
    for off in [
        offset_of!(RootRegistry, retiring),
        offset_of!(RootRegistry, leases),
        offset_of!(RootRegistry, walkers),
    ] {
        b.branch(field(G, off, 0), n(0), C::NotEqual, wait);
    }
    for off in (0..std::mem::size_of::<RootAudit>()).step_by(8) {
        b.set(field(LEASE, off, 0), field(OP, off, 0));
    }
    audit_call(&mut b, audit_core, 2, &status);
    b.value(field(LEASE, offset_of!(RootAudit, queue), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.mov_imm64(R::V6, 0);
    b.a.mov_imm64(R::V2, 4);
    b.a.call_label(mark_core);
    b.a.store_rbp_slot(KIND, R::V0);
    b.branch(local(KIND), n(5), C::Equal, wait);
    b.branch(local(KIND), n(4), C::NotEqual, invalid);
    b.set(local(NEXT), q!(remembered));
    super::remembered_inventory::valid(&mut b, NEXT, invalid);
    b.set(local(REG), field(LEASE, offset_of!(RootAudit, queue), 0));
    let remembered_done = b.a.create_text_label();
    if !retire {
        b.branch(
            field(REG, offset_of!(MarkQueue, generation), 0),
            n(0),
            C::NotEqual,
            remembered_done,
        );
    }
    b.branch(
        field(NEXT, offset_of!(RememberedInventory, last_phase), 0),
        g!(phase),
        C::Above,
        status[3],
    );
    b.branch(
        and(
            field(NEXT, offset_of!(RememberedInventory, epoch), 0),
            n(0x1000),
        ),
        and(g!(phase), n(0x1000)),
        C::NotEqual,
        status[3],
    );
    for off in [
        offset_of!(RememberedInventory, unflipped),
        offset_of!(RememberedInventory, pending),
        offset_of!(RememberedInventory, claims),
        offset_of!(RememberedInventory, walkers),
    ] {
        b.branch(field(NEXT, off, 0), n(0), C::NotEqual, wait);
    }
    b.a.bind_text_label(remembered_done);
    b.set(local(PREV), n(0));
    b.set(local(USED), n(0));
    b.set(local(STATE), n(0));
    b.set(local(REG), h!(head));
    let visit = b.a.create_text_label();
    let end = b.a.create_text_label();
    let identity_ok = b.a.create_text_label();
    b.a.bind_text_label(visit);
    b.branch(local(REG), n(0), C::Equal, end);
    b.pointer(local(REG), invalid);
    b.branch(local(USED), h!(members), C::AboveOrEqual, invalid);
    b.branch(p!(registry), local(SEG), C::NotEqual, invalid);
    b.branch(p!(registered), n(1), C::NotEqual, invalid);
    b.branch(p!(previous), local(PREV), C::NotEqual, invalid);
    b.branch(p!(identity), n(0), C::Equal, invalid);
    b.branch(p!(identity), h!(last_identity), C::Above, invalid);
    b.branch(local(PREV), n(0), C::Equal, identity_ok);
    b.branch(
        p!(identity),
        field(PREV, offset_of!(ThreadRegistration, identity), 0),
        C::AboveOrEqual,
        invalid,
    );
    b.a.bind_text_label(identity_ok);
    b.set(local(CTX), p!(context));
    b.pointer(local(CTX), invalid);
    b.branch(
        p!(acknowledged_generation),
        h!(generation),
        C::Above,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        p!(identity),
        C::NotEqual,
        invalid,
    );
    b.set(
        local(KIND),
        field(CTX, offset_of!(MutatorContext, state), 4),
    );
    let native = b.a.create_text_label();
    let held = b.a.create_text_label();
    let state_ok = b.a.create_text_label();
    b.branch(local(KIND), n(2), C::Equal, native);
    b.branch(local(KIND), n(1), C::Equal, held);
    b.branch(local(KIND), n(3), C::NotEqual, invalid);
    b.a.bind_text_label(held);
    b.branch(
        p!(acknowledged_generation),
        h!(generation),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, adopted_phase), 8),
        g!(phase),
        C::NotEqual,
        invalid,
    );
    b.a.jmp_label(state_ok);
    b.a.bind_text_label(native);
    b.set(local(STATE), add(local(STATE), n(1)));
    b.a.bind_text_label(state_ok);
    b.branch(
        field(CTX, offset_of!(MutatorContext, private_producers), 8),
        n(0),
        C::NotEqual,
        wait,
    );
    b.set(field(LEASE, offset_of!(RootAudit, context), 0), local(CTX));
    audit_call(&mut b, audit_core, 3, &status);
    if !retire {
        for base in [
            offset_of!(MutatorContext, satb_consumer),
            offset_of!(MutatorContext, remembered_consumer),
        ] {
            for off in [
                offset_of!(BufferConsumer, serial),
                offset_of!(BufferConsumer, end),
            ] {
                b.branch(field(CTX, base + off, 0), n(0), C::NotEqual, wait);
            }
        }
        for base in [
            offset_of!(MutatorContext, satb),
            offset_of!(MutatorContext, remembered),
        ] {
            b.set(
                local(NEXT),
                field(CTX, base + offset_of!(BufferCursor, published), 8),
            );
            b.set(
                local(KIND),
                field(CTX, base + offset_of!(BufferCursor, consumed), 8),
            );
            b.branch(local(KIND), local(NEXT), C::Above, invalid);
            b.branch(
                local(NEXT),
                field(CTX, base + offset_of!(BufferCursor, top), 0),
                C::Above,
                invalid,
            );
            b.branch(local(KIND), local(NEXT), C::NotEqual, wait);
            b.branch(
                local(NEXT),
                field(CTX, base + offset_of!(BufferCursor, top), 0),
                C::NotEqual,
                wait,
            );
        }
    }
    b.set(local(PREV), local(REG));
    b.set(local(REG), p!(next));
    b.set(local(USED), add(local(USED), n(1)));
    b.a.jmp_label(visit);
    b.a.bind_text_label(end);
    b.branch(local(USED), h!(members), C::NotEqual, invalid);
    b.branch(local(STATE), h!(native), C::NotEqual, invalid);
    b.a.jmp_label(status[0]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(audit_core);
    if retire {
        super::root_audit::emit_root_retire_audit(b.a);
    } else {
        super::root_audit::emit_root_audit(b.a);
    }
    b.a.ret();
    b.a.bind_text_label(mark_core);
    super::mark::emit_mark_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
