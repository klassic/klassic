//! FINISH then native notification, with a retry independent of freed backing.
use super::*;
use crate::gc_abi::RootFinishNotification as Notice;
fn notice(off: usize) -> E {
    field(REG, off, 0)
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let success = a.create_text_label();
    let notify = a.create_text_label();
    let life = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(REG, R::V2);
    let mut b = Emit { a };
    b.pointer(local(REG), invalid);
    b.branch(notice(offset_of!(Notice, stage)), n(2), C::Above, invalid);
    b.branch(notice(offset_of!(Notice, stage)), n(2), C::Equal, success);
    b.pointer(notice(offset_of!(Notice, context)), invalid);
    b.branch(notice(offset_of!(Notice, event)), n(0), C::Equal, invalid);
    b.branch(
        and(notice(offset_of!(Notice, event)), n(3)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(notice(offset_of!(Notice, notify)), n(0), C::Equal, invalid);
    b.branch(notice(offset_of!(Notice, stage)), n(1), C::Equal, notify);
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.pointer(
        field(Q, offset_of!(RootLifeRequest, registration), 0),
        invalid,
    );
    b.set(
        local(NEXT),
        field(Q, offset_of!(RootLifeRequest, registration), 0),
    );
    b.branch(
        field(NEXT, offset_of!(RootRegistration, context), 0),
        notice(offset_of!(Notice, context)),
        C::NotEqual,
        invalid,
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, Q);
    b.a.mov_imm64(R::V2, 7);
    b.a.call_label(life);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, done);
    b.set(notice(offset_of!(Notice, stage)), n(1));
    b.a.bind_text_label(notify);
    b.value(notice(offset_of!(Notice, notify)));
    b.a.mov_reg_reg(R::V10, R::V0);
    b.value(notice(offset_of!(Notice, event)));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.call_native_unary(R::V10);
    b.a.store_rbp_slot(STATE, R::V0);
    b.set(notice(offset_of!(Notice, last_error)), local(STATE));
    b.branch(local(STATE), n(0), C::Equal, success);
    b.a.mov_imm64(R::V0, 4);
    b.a.jmp_label(done);
    b.a.bind_text_label(success);
    b.set(notice(offset_of!(Notice, stage)), n(2));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(life);
    super::emit_life_locked(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
