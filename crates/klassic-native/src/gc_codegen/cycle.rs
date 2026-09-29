//! Nonwrapping, request-time cycle tickets; matching gc_cycle.h.
use super::*;
use crate::gc_abi::{CycleLedger, CycleTicket};
#[allow(dead_code)]
pub(crate) fn emit_cycle_step<A: PortableAsm>(a: &mut A) {
    let status: [_; 5] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[2];
    let existing = a.create_text_label();
    let start = a.create_text_label();
    let issue = a.create_text_label();
    let finish = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(CycleTicket, $f), 0)
        };
    }
    macro_rules! lane {
        ($f:ident) => {
            field(REG, offset_of!(CycleLedger, $f), 0)
        };
    }
    b.branch(local(OP), n(3), C::Above, invalid);
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(q!(kind), n(1), C::Above, invalid);
    for i in 0..2 {
        let requested = field(G, offset_of!(CycleLedger, requested) + i * 8, 0);
        let started = field(G, offset_of!(CycleLedger, started) + i * 8, 0);
        let completed = field(G, offset_of!(CycleLedger, completed) + i * 8, 0);
        b.branch(completed.clone(), started.clone(), C::Above, invalid);
        b.branch(sub(started.clone(), completed), n(1), C::Above, invalid);
        b.branch(started.clone(), requested.clone(), C::Above, invalid);
        b.branch(sub(requested, started), n(1), C::Above, invalid);
    }
    b.set(local(REG), add(local(G), shl(q!(kind), 3)));
    b.branch(local(OP), n(1), C::Above, existing);
    b.branch(q!(ledger), n(0), C::NotEqual, invalid);
    b.branch(q!(cycle), n(0), C::NotEqual, invalid);
    b.branch(local(OP), n(1), C::Equal, start);
    b.branch(lane!(started), n(u64::MAX), C::Equal, status[3]);
    b.set(lane!(requested), add(lane!(started), n(1)));
    b.set(q!(cycle), lane!(requested));
    b.a.jmp_label(issue);
    b.a.bind_text_label(start);
    b.branch(lane!(started), lane!(completed), C::NotEqual, status[1]);
    b.branch(lane!(requested), lane!(started), C::Equal, status[4]);
    b.set(lane!(started), add(lane!(started), n(1)));
    b.set(q!(cycle), lane!(started));
    b.a.bind_text_label(issue);
    b.set(q!(ledger), local(G));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(existing);
    b.branch(q!(ledger), local(G), C::NotEqual, invalid);
    b.branch(q!(cycle), n(0), C::Equal, invalid);
    b.branch(q!(cycle), lane!(requested), C::Above, invalid);
    b.branch(local(OP), n(2), C::Equal, finish);
    b.branch(q!(cycle), lane!(completed), C::Above, status[1]);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(finish);
    b.branch(q!(cycle), lane!(started), C::NotEqual, invalid);
    b.branch(lane!(started), lane!(completed), C::Equal, invalid);
    b.set(lane!(completed), lane!(started));
    b.a.jmp_label(status[0]);
    for (code, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
