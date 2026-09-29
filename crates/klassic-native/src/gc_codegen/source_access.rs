//! Source-local all-actor admission/drain; gc_source_access.h contract.
use super::*;
use crate::gc_abi::{
    ABI_VERSION, ForwardCell, MAX_OFFSET, MutatorContext as M, SOURCE_CLOSED, SourceAccessRequest,
};
macro_rules! cell {
    ($f:ident) => {
        field(G, offset_of!(ForwardCell, $f), 0)
    };
}
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(SourceAccessRequest, $f), 0)
    };
}
fn cas<A: PortableAsm>(b: &mut Emit<'_, A>, desired: E) {
    b.value(desired);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.load_rbp_slot(R::V11, G);
    b.a.add_reg_imm32(R::V11, offset_of!(ForwardCell, access) as i32);
    b.a.atomic_compare_exchange(R::V11, R::V2);
    b.a.store_rbp_slot(USED, R::V0);
}
#[allow(dead_code)]
pub(crate) fn emit_source_access<A: PortableAsm>(a: &mut A) {
    let status: [_; 4] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let wait = status[2];
    let collector = a.create_text_label();
    let leave = a.create_text_label();
    let reopen = a.create_text_label();
    let check = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(4), C::Above, invalid);
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
    b.branch(local(OP), n(1), C::Above, collector);
    b.branch(req!(ticket), n(0), C::NotEqual, invalid);
    b.pointer(req!(context), invalid);
    b.set(local(CTX), req!(context));
    b.branch(
        field(CTX, offset_of!(M, abi_version), 4),
        n(ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(M, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(M, state), 4),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.set(local(PREV), field(CTX, offset_of!(M, access_epoch), 8));
    b.branch(local(OP), n(1), C::Equal, leave);
    b.branch(req!(held), n(0), C::NotEqual, invalid);
    b.branch(local(PREV), n(0), C::NotEqual, invalid);
    b.set(local(NEXT), field(G, offset_of!(ForwardCell, word), 8));
    let admitted = b.a.create_text_label();
    b.branch(local(NEXT), n(0), C::Equal, admitted);
    b.branch(local(NEXT), n(3), C::NotEqual, wait);
    b.a.bind_text_label(admitted);
    b.set(local(STATE), field(G, offset_of!(ForwardCell, access), 8));
    b.branch(local(STATE), n(SOURCE_CLOSED), C::AboveOrEqual, wait);
    b.branch(local(STATE), n(SOURCE_CLOSED - 1), C::Equal, status[3]);
    cas(&mut b, add(local(STATE), n(1)));
    b.branch(local(USED), local(STATE), C::NotEqual, wait);
    b.set(
        field(CTX, offset_of!(M, access_epoch), 8),
        cell!(incarnation),
    );
    b.set(req!(held), local(G));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(leave);
    b.branch(req!(held), local(G), C::NotEqual, invalid);
    b.branch(local(PREV), cell!(incarnation), C::NotEqual, invalid);
    b.set(local(STATE), field(G, offset_of!(ForwardCell, access), 8));
    b.branch(
        and(local(STATE), n(!SOURCE_CLOSED)),
        n(0),
        C::Equal,
        invalid,
    );
    cas(&mut b, sub(local(STATE), n(1)));
    b.branch(local(USED), local(STATE), C::NotEqual, wait);
    b.set(field(CTX, offset_of!(M, access_epoch), 8), n(0));
    b.set(req!(held), n(0));
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(collector);
    b.branch(req!(context), n(0), C::NotEqual, invalid);
    b.branch(req!(held), n(0), C::NotEqual, invalid);
    b.set(local(NEXT), field(G, offset_of!(ForwardCell, word), 8));
    b.branch(local(OP), n(4), C::Equal, reopen);
    b.branch(req!(ticket), n(0), C::Equal, invalid);
    b.branch(req!(ticket), n(u64::MAX >> 2), C::Above, invalid);
    b.branch(
        local(NEXT),
        add(shl(req!(ticket), 2), n(1)),
        C::NotEqual,
        invalid,
    );
    b.set(local(STATE), field(G, offset_of!(ForwardCell, access), 8));
    b.branch(local(OP), n(3), C::Equal, check);
    b.branch(local(STATE), n(SOURCE_CLOSED), C::AboveOrEqual, status[0]);
    cas(&mut b, add(local(STATE), n(SOURCE_CLOSED)));
    b.branch(local(USED), local(STATE), C::NotEqual, wait);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(check);
    b.branch(local(STATE), n(SOURCE_CLOSED), C::NotEqual, wait);
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(reopen);
    b.branch(req!(ticket), n(0), C::NotEqual, invalid);
    b.branch(local(NEXT), n(3), C::NotEqual, invalid);
    b.set(local(STATE), n(SOURCE_CLOSED));
    cas(&mut b, n(0));
    b.branch(local(USED), local(STATE), C::NotEqual, invalid);
    b.a.jmp_label(status[0]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
