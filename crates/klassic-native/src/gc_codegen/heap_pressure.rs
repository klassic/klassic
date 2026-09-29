//! Soft target and reserve-aware mutator allocation forecast.
use super::*;
use crate::gc_abi::HeapPool;

#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let limit_ready = a.create_text_label();
    let no_headroom = a.create_text_label();
    let forecast = a.create_text_label();
    let done = a.create_text_label();
    let pressure = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    let hard = field(G, offset_of!(HeapPool, hard_max), 0);
    let soft = field(G, offset_of!(HeapPool, soft_max), 0);
    let reserve = field(G, offset_of!(HeapPool, relocation_reserve), 0);
    let committed = field(G, offset_of!(HeapPool, committed), 0);
    let pending = field(G, offset_of!(HeapPool, pending), 0);
    b.branch(hard.clone(), n(0), C::Equal, invalid);
    for value in [soft.clone(), reserve.clone(), committed.clone()] {
        b.branch(value, hard.clone(), C::Above, invalid);
    }
    b.branch(
        pending.clone(),
        sub(hard.clone(), committed.clone()),
        C::Above,
        invalid,
    );
    b.set(local(REG), sub(hard, reserve));
    b.branch(soft.clone(), local(REG), C::AboveOrEqual, limit_ready);
    b.set(local(REG), soft);
    b.a.bind_text_label(limit_ready);
    b.set(local(STATE), add(committed, pending));
    b.branch(local(STATE), local(REG), C::AboveOrEqual, no_headroom);
    b.set(local(STATE), sub(local(REG), local(STATE)));
    b.a.jmp_label(forecast);
    b.a.bind_text_label(no_headroom);
    b.set(local(STATE), n(0));
    b.a.bind_text_label(forecast);
    b.a.load_rbp_slot(R::V7, Q);
    b.a.load_rbp_slot(R::V6, STATE);
    b.a.load_rbp_slot(R::V2, OP);
    b.a.call_label(pressure);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(pressure);
    super::allocation_sample::emit_pressure(b.a);
    b.a.ret();
}
