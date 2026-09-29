//! Bounded incoming-forwarding audit; no heap access or retirement authorization.
use super::*;
use crate::gc_abi::{ForwardCell, ForwardIncoming, ForwardTable};
macro_rules! t {
    ($f:ident) => {
        field(G, offset_of!(ForwardTable, $f), 0)
    };
}
macro_rules! q {
    ($f:ident) => {
        field(Q, offset_of!(ForwardIncoming, $f), 0)
    };
}
macro_rules! cell {
    ($f:ident,$a:expr) => {
        field(SEG, offset_of!(ForwardCell, $f), $a)
    };
}
#[allow(dead_code)]
pub(crate) fn emit_forward_incoming<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let loop_label = a.create_text_label();
    let next = a.create_text_label();
    let more = a.create_text_label();
    let wait = a.create_text_label();
    let commit = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.pointer(t!(cells), invalid);
    b.branch(t!(capacity), n(0), C::Equal, invalid);
    b.branch(t!(capacity), n(u32::MAX as u64), C::Above, invalid);
    b.branch(
        add(t!(cells), shl(t!(capacity), 5)),
        t!(cells),
        C::Below,
        invalid,
    );
    b.branch(q!(incarnation), n(0), C::Equal, invalid);
    b.branch(q!(used), t!(capacity), C::Above, invalid);
    b.branch(
        q!(used),
        field(G, offset_of!(ForwardTable, used), 8),
        C::NotEqual,
        invalid,
    );
    b.branch(q!(cursor), q!(used), C::Above, invalid);
    b.branch(q!(budget), n(0), C::Equal, invalid);
    b.branch(q!(budget), n(256), C::Above, invalid);
    b.branch(q!(source_offset), n(0), C::NotEqual, invalid);
    b.branch(q!(target_start), n(0), C::Equal, invalid);
    b.branch(q!(target_start), q!(target_limit), C::AboveOrEqual, invalid);
    b.branch(
        q!(target_limit),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    for v in [q!(target_start), q!(target_limit)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, invalid);
    }
    b.set(local(PREV), q!(cursor));
    b.set(local(NEXT), n(0));
    b.set(local(USED), n(0));
    b.set(local(OP), n(0));
    b.a.bind_text_label(loop_label);
    b.branch(local(PREV), q!(used), C::Equal, commit);
    b.branch(local(NEXT), q!(budget), C::Equal, more);
    b.set(local(SEG), add(t!(cells), shl(local(PREV), 5)));
    b.branch(cell!(source_offset, 0), n(0), C::Equal, invalid);
    b.branch(
        cell!(source_offset, 0),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    b.branch(
        and(cell!(source_offset, 0), n(15)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(cell!(incarnation, 0), q!(incarnation), C::NotEqual, invalid);
    let first = b.a.create_text_label();
    b.branch(local(PREV), n(0), C::Equal, first);
    b.set(local(REG), sub(local(SEG), n(32)));
    b.branch(
        field(REG, offset_of!(ForwardCell, source_offset), 0),
        cell!(source_offset, 0),
        C::AboveOrEqual,
        invalid,
    );
    b.a.bind_text_label(first);
    b.set(local(STATE), cell!(word, 8));
    b.set(local(NEXT), add(local(NEXT), n(1)));
    b.branch(local(STATE), n(0), C::Equal, next);
    b.branch(local(STATE), n(3), C::Equal, next);
    let published = b.a.create_text_label();
    b.branch(and(local(STATE), n(3)), n(1), C::NotEqual, published);
    b.branch(shr(local(STATE), 2), n(0), C::NotEqual, wait);
    b.a.bind_text_label(published);
    b.branch(and(local(STATE), n(3)), n(2), C::NotEqual, invalid);
    b.set(local(STATE), and(local(STATE), n(!3u64)));
    b.branch(local(STATE), n(0), C::Equal, invalid);
    super::barrier::reference(&mut b, local(STATE), invalid);
    b.set(local(STATE), shr(local(STATE), 16));
    b.branch(local(STATE), cell!(source_offset, 0), C::Equal, invalid);
    b.branch(local(STATE), q!(target_start), C::Below, next);
    b.branch(local(STATE), q!(target_limit), C::AboveOrEqual, next);
    b.set(local(USED), cell!(source_offset, 0));
    b.set(local(OP), n(3));
    b.a.jmp_label(commit);
    b.a.bind_text_label(next);
    b.set(local(PREV), add(local(PREV), n(1)));
    b.a.jmp_label(loop_label);
    b.a.bind_text_label(more);
    b.set(local(OP), n(2));
    b.a.jmp_label(commit);
    b.a.bind_text_label(wait);
    b.set(local(OP), n(4));
    b.a.bind_text_label(commit);
    b.set(q!(cursor), local(PREV));
    b.set(q!(examined), local(NEXT));
    b.set(q!(source_offset), local(USED));
    b.value(local(OP));
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}
