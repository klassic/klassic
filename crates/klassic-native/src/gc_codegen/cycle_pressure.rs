//! Coherent pressure forecast -> coalescible cycle request, gc_cycle_pressure.h.
use super::*;
use crate::gc_abi::{CyclePressure, CycleTicket};
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let quiet = a.create_text_label();
    let done = a.create_text_label();
    let forecast = a.create_text_label();
    let request = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    for slot in [G, Q, OP] {
        b.pointer(local(slot), bad);
    }
    b.branch(
        field(Q, offset_of!(CycleTicket, kind), 0),
        n(1),
        C::Above,
        bad,
    );
    for off in [
        offset_of!(CycleTicket, ledger),
        offset_of!(CycleTicket, cycle),
    ] {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, bad);
    }
    b.value(field(OP, offset_of!(CyclePressure, heap), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(field(OP, offset_of!(CyclePressure, sample), 0));
    b.a.mov_reg_reg(R::V6, R::V0);
    b.value(field(OP, offset_of!(CyclePressure, horizon_ns), 0));
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.call_label(forecast);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::Equal, quiet);
    b.branch(local(STATE), n(1), C::NotEqual, bad);
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, Q);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(request);
    b.a.jmp_label(done);
    b.a.bind_text_label(quiet);
    b.a.mov_imm64(R::V0, 4);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(forecast);
    super::heap_pressure::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(request);
    super::cycle::emit_cycle_step(b.a);
    b.a.ret();
}
