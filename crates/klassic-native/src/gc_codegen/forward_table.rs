//! Metadata-only bounded lookup. Lifetime/append contract: gc_forward_table.h.
use super::*;
use crate::gc_abi::{ForwardCell, ForwardLookup, ForwardTable, MAX_OFFSET};
macro_rules! table {
    ($f:ident) => {
        field(G, offset_of!(ForwardTable, $f), 0)
    };
}
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(ForwardLookup, $f), 0)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_forward_lookup<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<ForwardCell>() == 32);
    }
    let status: [_; 3] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let search = a.create_text_label();
    let lower = a.create_text_label();
    let upper = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.branch(req!(cell), n(0), C::NotEqual, invalid);
    b.branch(req!(source_offset), n(0), C::Equal, invalid);
    b.branch(req!(source_offset), n(MAX_OFFSET), C::Above, invalid);
    b.branch(and(req!(source_offset), n(15)), n(0), C::NotEqual, invalid);
    b.branch(req!(incarnation), n(0), C::Equal, invalid);
    b.branch(table!(cells), n(0), C::Equal, invalid);
    b.branch(and(table!(cells), n(7)), n(0), C::NotEqual, invalid);
    b.branch(table!(capacity), n(0), C::Equal, invalid);
    b.branch(table!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        table!(cells),
        sub(n(u64::MAX), shl(table!(capacity), 5)),
        C::Above,
        invalid,
    );
    b.set(local(PREV), n(0));
    b.set(local(NEXT), field(G, offset_of!(ForwardTable, used), 8));
    b.branch(local(NEXT), table!(capacity), C::Above, invalid);
    b.a.bind_text_label(search);
    b.branch(local(PREV), local(NEXT), C::AboveOrEqual, status[2]);
    b.set(
        local(USED),
        add(local(PREV), shr(sub(local(NEXT), local(PREV)), 1)),
    );
    b.set(local(REG), add(table!(cells), shl(local(USED), 5)));
    b.branch(
        field(REG, offset_of!(ForwardCell, source_offset), 0),
        req!(source_offset),
        C::Below,
        lower,
    );
    b.branch(
        field(REG, offset_of!(ForwardCell, source_offset), 0),
        req!(source_offset),
        C::Above,
        upper,
    );
    b.branch(
        field(REG, offset_of!(ForwardCell, incarnation), 0),
        req!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.set(req!(cell), local(REG));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(lower);
    b.set(local(PREV), add(local(USED), n(1)));
    b.a.jmp_label(search);
    b.a.bind_text_label(upper);
    b.set(local(NEXT), local(USED));
    b.a.jmp_label(search);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
