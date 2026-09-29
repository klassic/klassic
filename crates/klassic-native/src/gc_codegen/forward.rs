//! Atomic forwarding ownership/publication; caller obligations in gc_forward.h.
use super::*;
use crate::gc_abi::{ForwardCell, ForwardRequest, MAX_OFFSET};
macro_rules! cell {
    ($f:ident) => {
        field(G, offset_of!(ForwardCell, $f), 0)
    };
}
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(ForwardRequest, $f), 0)
    };
}
fn destination<A: PortableAsm>(b: &mut Emit<'_, A>, invalid: A::TextLabel) {
    b.branch(shr(local(USED), 16), n(0), C::Equal, invalid);
    b.branch(
        shr(local(USED), 16),
        cell!(source_offset),
        C::Equal,
        invalid,
    );
    b.branch(
        and(local(USED), n(0xf0000 | 0xcf)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(and(local(USED), n(0x30)), n(0), C::Equal, invalid);
    for mask in [0x300, 0xc00, 0xf000] {
        b.set(local(PREV), and(local(USED), n(mask)));
        b.branch(local(PREV), n(0), C::Equal, invalid);
        b.branch(
            and(local(PREV), sub(local(PREV), n(1))),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
}
fn cas<A: PortableAsm>(b: &mut Emit<'_, A>, expected: E, desired: E) {
    b.value(desired);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.value(expected);
    b.a.load_rbp_slot(R::V11, G);
    b.a.add_reg_imm32(R::V11, offset_of!(ForwardCell, word) as i32);
    b.a.atomic_compare_exchange(R::V11, R::V2);
}
#[allow(dead_code)]
pub(crate) fn emit_forward_step<A: PortableAsm>(a: &mut A) {
    let status: [_; 6] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let read = a.create_text_label();
    let owner = a.create_text_label();
    let claim = a.create_text_label();
    let publish = a.create_text_label();
    let inspect = a.create_text_label();
    let observed = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(3), C::Above, invalid);
    b.branch(cell!(source_offset), n(0), C::Equal, invalid);
    b.branch(cell!(source_offset), n(MAX_OFFSET), C::Above, invalid);
    b.branch(and(cell!(source_offset), n(15)), n(0), C::NotEqual, invalid);
    b.branch(cell!(incarnation), n(0), C::Equal, invalid);
    b.branch(
        req!(source_offset),
        cell!(source_offset),
        C::NotEqual,
        invalid,
    );
    b.branch(req!(incarnation), cell!(incarnation), C::NotEqual, invalid);
    b.branch(local(OP), n(1), C::Equal, read);
    b.branch(req!(ticket), n(0), C::Equal, invalid);
    b.branch(req!(ticket), n(u64::MAX >> 2), C::Above, invalid);
    b.branch(local(OP), n(2), C::Equal, publish);
    b.branch(req!(destination), n(0), C::NotEqual, invalid);
    b.branch(local(OP), n(0), C::Equal, claim);
    b.set(local(NEXT), n(3));
    b.a.jmp_label(owner);
    b.a.bind_text_label(publish);
    b.set(local(USED), req!(destination));
    destination(&mut b, invalid);
    b.set(local(NEXT), add(req!(destination), n(2)));
    b.a.bind_text_label(owner);
    b.branch(
        field(G, offset_of!(ForwardCell, access), 8),
        n(crate::gc_abi::SOURCE_CLOSED),
        C::NotEqual,
        invalid,
    );
    b.set(local(STATE), add(shl(req!(ticket), 2), n(1)));
    cas(&mut b, local(STATE), local(NEXT));
    b.a.store_rbp_slot(USED, R::V0);
    b.branch(local(USED), local(STATE), C::NotEqual, invalid);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(claim);
    cas(&mut b, n(0), add(shl(req!(ticket), 2), n(1)));
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, status[0]);
    b.a.jmp_label(observed);
    b.a.bind_text_label(read);
    b.branch(req!(ticket), n(0), C::NotEqual, invalid);
    b.branch(req!(destination), n(0), C::NotEqual, invalid);
    b.value(field(G, offset_of!(ForwardCell, word), 8));
    b.a.bind_text_label(observed);
    b.a.store_rbp_slot(STATE, R::V0);
    b.branch(local(STATE), n(0), C::Equal, status[2]);
    b.branch(and(local(STATE), n(3)), n(1), C::NotEqual, inspect);
    b.branch(shr(local(STATE), 2), n(0), C::Equal, invalid);
    b.a.jmp_label(status[3]);
    b.a.bind_text_label(inspect);
    b.branch(local(STATE), n(3), C::Equal, status[4]);
    b.branch(and(local(STATE), n(3)), n(2), C::NotEqual, invalid);
    b.set(local(USED), and(local(STATE), n(!3)));
    destination(&mut b, invalid);
    b.branch(local(OP), n(0), C::Equal, status[5]);
    b.set(req!(destination), local(USED));
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
