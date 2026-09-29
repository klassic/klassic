//! Stable per-object Young survival accounting, matching gc_survivor_age.h.
use super::*;
use crate::gc_abi::MarkRecord;

#[allow(dead_code)]
pub(crate) fn emit_age<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let prior = a.create_text_label();
    let checked = a.create_text_label();
    let stamp = a.create_text_label();
    let decide = a.create_text_label();
    let promote = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(G, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.pointer(local(Q), invalid);
    b.branch(
        field(Q, offset_of!(MarkRecord, object_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(local(G), n(0), C::Equal, invalid);
    b.branch(local(OP), n(0), C::Equal, invalid);
    b.branch(local(OP), n(crate::gc_abi::MAX_AGE), C::Above, invalid);
    let age = field(Q, offset_of!(MarkRecord, age), 0);
    let epoch = field(Q, offset_of!(MarkRecord, age_epoch), 0);
    b.branch(age.clone(), n(crate::gc_abi::MAX_AGE), C::Above, invalid);
    b.branch(epoch.clone(), n(0), C::NotEqual, prior);
    b.branch(age.clone(), n(0), C::NotEqual, invalid);
    b.a.jmp_label(checked);
    b.a.bind_text_label(prior);
    b.branch(age.clone(), n(0), C::Equal, invalid);
    b.a.bind_text_label(checked);
    b.branch(local(G), epoch.clone(), C::Below, invalid);
    b.branch(local(G), epoch.clone(), C::Equal, decide);
    b.branch(age.clone(), n(crate::gc_abi::MAX_AGE), C::Equal, stamp);
    b.set(age.clone(), add(age.clone(), n(1)));
    b.a.bind_text_label(stamp);
    b.set(epoch, local(G));
    b.a.bind_text_label(decide);
    b.branch(age, local(OP), C::AboveOrEqual, promote);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(promote);
    b.a.mov_imm64(R::V0, 1);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
}

/// Match the completed Young mark epoch before applying survival aging.
#[allow(dead_code)]
pub(crate) fn emit_marked_age<A: PortableAsm>(a: &mut A) {
    use crate::gc_abi::MarkQueue;
    let invalid = a.create_text_label();
    let unmarked = a.create_text_label();
    let done = a.create_text_label();
    let age = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(
        field(G, offset_of!(MarkQueue, generation), 0),
        n(0),
        C::NotEqual,
        invalid,
    );
    let epoch = field(G, offset_of!(MarkQueue, epoch), 0);
    let marked = field(Q, offset_of!(MarkRecord, young_epoch), 0);
    b.branch(epoch.clone(), n(0), C::Equal, invalid);
    b.branch(
        field(Q, offset_of!(MarkRecord, object_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(local(OP), n(0), C::Equal, invalid);
    b.branch(local(OP), n(crate::gc_abi::MAX_AGE), C::Above, invalid);
    b.branch(marked.clone(), epoch.clone(), C::Above, invalid);
    b.branch(marked, epoch.clone(), C::NotEqual, unmarked);
    b.value(epoch);
    b.a.mov_reg_reg(R::V6, R::V0);
    b.a.load_rbp_slot(R::V7, Q);
    b.a.load_rbp_slot(R::V2, OP);
    b.a.call_label(age);
    b.a.jmp_label(done);
    b.a.bind_text_label(unmarked);
    b.a.mov_imm64(R::V0, 3);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(age);
    emit_age(b.a);
    b.a.ret();
}
