//! Transactional request-ledger and prepared MarkStart admission.
use super::*;
use crate::gc_abi::{CycleLedger, CycleMarkStart, CycleTicket, MarkPhaseControl};
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let done = a.create_text_label();
    let cycle = a.create_text_label();
    let phase = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 96);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    for slot in [G, Q, OP] {
        b.pointer(local(slot), bad);
    }
    b.set(
        local(REG),
        field(OP, offset_of!(CycleMarkStart, control), 0),
    );
    b.pointer(local(REG), bad);
    b.pointer(field(OP, offset_of!(CycleMarkStart, registry), 0), bad);
    let kind = field(Q, offset_of!(CycleTicket, kind), 0);
    b.branch(kind.clone(), n(1), C::Above, bad);
    b.branch(
        field(REG, offset_of!(MarkPhaseControl, generation), 0),
        kind.clone(),
        C::NotEqual,
        bad,
    );
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 208);
    b.a.store_rbp_slot(PREV, R::V0);
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 160);
    b.a.store_rbp_slot(NEXT, R::V0);
    for off in (0..std::mem::size_of::<CycleLedger>()).step_by(8) {
        b.set(field(PREV, off, 0), field(G, off, 0));
    }
    for off in (0..std::mem::size_of::<CycleTicket>()).step_by(8) {
        b.set(field(NEXT, off, 0), field(Q, off, 0));
    }
    b.a.load_rbp_slot(R::V7, PREV);
    b.a.load_rbp_slot(R::V6, NEXT);
    b.a.mov_imm64(R::V2, 1);
    b.a.call_label(cycle);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, done);
    b.value(field(OP, offset_of!(CycleMarkStart, registry), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.mov_imm64(R::V2, 3);
    b.a.call_label(phase);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::NotEqual, done);
    b.set(local(REG), add(local(G), shl(kind, 3)));
    let admitted = field(NEXT, offset_of!(CycleTicket, cycle), 0);
    b.set(
        field(REG, offset_of!(CycleLedger, started), 0),
        admitted.clone(),
    );
    b.set(field(Q, offset_of!(CycleTicket, cycle), 0), admitted);
    b.set(field(Q, offset_of!(CycleTicket, ledger), 0), local(G));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(cycle);
    super::cycle::emit_cycle_step(b.a);
    b.a.ret();
    b.a.bind_text_label(phase);
    super::mark_phase::emit_mark_phase(b.a);
    b.a.ret();
}
