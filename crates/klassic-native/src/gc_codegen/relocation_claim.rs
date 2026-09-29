//! Acquire admission before claiming a source; release every losing attempt.
use super::*;
use crate::gc_abi::{ForwardCell, RelocationClaim};
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(RelocationClaim, $f), 0)
    };
}
fn gate_call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, op: u64) {
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
#[allow(dead_code)]
pub(crate) fn emit_relocation_claim<A: PortableAsm>(a: &mut A) {
    let status: [_; 7] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let won = a.create_text_label();
    let release = a.create_text_label();
    let gate = a.create_text_label();
    let claim = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 64);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    for (slot, offset) in [(REG, 144), (SEG, 176)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, offset);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    b.pointer(req!(permit), invalid);
    b.pointer(req!(source), invalid);
    b.branch(req!(ticket), n(0), C::NotEqual, invalid);
    b.branch(req!(epoch), n(0), C::NotEqual, invalid);
    b.branch(req!(source_offset), n(0), C::Equal, invalid);
    b.branch(and(req!(source_offset), n(15)), n(0), C::NotEqual, invalid);
    b.branch(
        req!(source_offset),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    b.branch(req!(incarnation), n(0), C::Equal, invalid);
    b.set(local(CTX), req!(source));
    b.branch(
        field(CTX, offset_of!(ForwardCell, source_offset), 0),
        req!(source_offset),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(ForwardCell, incarnation), 0),
        req!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.set(field(REG, 0, 0), req!(permit));
    b.set(field(REG, 8, 0), n(0));
    b.set(field(REG, 16, 0), n(0));
    gate_call(&mut b, gate, 0);
    b.branch(local(STATE), n(2), C::Equal, status[6]);
    b.branch(local(STATE), n(3), C::Equal, status[5]);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.set(local(USED), n(5));
    b.branch(field(REG, 8, 0), n(u64::MAX >> 2), C::Above, release);
    for (offset, value) in [
        (0, req!(source_offset)),
        (8, req!(incarnation)),
        (16, field(REG, 8, 0)),
        (24, n(0)),
    ] {
        b.set(field(SEG, offset, 0), value);
    }
    b.a.load_rbp_slot(R::V7, CTX);
    b.a.load_rbp_slot(R::V6, SEG);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(claim);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::Equal, won);
    b.set(local(USED), n(1));
    for (returned, mapped) in [(3, 2), (4, 3), (5, 4)] {
        let next = b.a.create_text_label();
        b.branch(local(STATE), n(returned), C::NotEqual, next);
        b.set(local(USED), n(mapped));
        b.a.jmp_label(release);
        b.a.bind_text_label(next);
    }
    b.a.bind_text_label(release);
    gate_call(&mut b, gate, 1);
    b.branch(local(STATE), n(0), C::NotEqual, invalid);
    b.a.load_rbp_slot(R::V0, USED);
    b.a.jmp_label(done);
    b.a.bind_text_label(won);
    b.set(req!(ticket), field(REG, 8, 0));
    b.set(req!(epoch), field(REG, 16, 0));
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
    b.a.bind_text_label(claim);
    super::forward::emit_forward_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}

/// Admit only sources marked in the captured generation epoch.
#[allow(dead_code)]
pub(crate) fn emit_marked_relocation_claim<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::{MarkQueue, MarkRecord, MarkedRelocation};
    const _: () = assert!(offset_of!(MarkedRelocation, claim) == 0);
    let bad = a.create_text_label();
    let stale = a.create_text_label();
    let unmarked = a.create_text_label();
    let claim = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.pointer(local(G), bad);
    b.pointer(local(Q), bad);
    b.set(local(REG), field(Q, offset_of!(MarkedRelocation, queue), 0));
    b.set(
        local(SEG),
        field(Q, offset_of!(MarkedRelocation, record), 0),
    );
    b.pointer(local(REG), bad);
    b.pointer(local(SEG), bad);
    let epoch = field(Q, offset_of!(MarkedRelocation, epoch), 0);
    b.branch(epoch.clone(), n(0), C::Equal, bad);
    let generation = field(REG, offset_of!(MarkQueue, generation), 0);
    b.branch(generation.clone(), n(1), C::Above, bad);
    b.branch(
        field(REG, offset_of!(MarkQueue, epoch), 0),
        n(0),
        C::Equal,
        bad,
    );
    b.branch(
        field(SEG, offset_of!(MarkRecord, object_id), 0),
        n(0),
        C::Equal,
        bad,
    );
    b.branch(
        field(REG, offset_of!(MarkQueue, epoch), 0),
        epoch.clone(),
        C::NotEqual,
        stale,
    );
    b.set(local(CTX), add(local(SEG), shl(generation, 3)));
    let stamp = field(CTX, offset_of!(MarkRecord, young_epoch), 0);
    b.branch(stamp.clone(), epoch.clone(), C::Above, stale);
    b.branch(stamp, epoch, C::NotEqual, unmarked);
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, Q);
    b.a.call_label(claim);
    b.a.jmp_label(done);
    for (label, code) in [(bad, 1), (stale, 8), (unmarked, 7)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(claim);
    emit_relocation_claim(b.a);
    b.a.ret();
}
