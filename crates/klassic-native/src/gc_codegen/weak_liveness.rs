//! Read-only canonical weak liveness observation; gc_weak_liveness.h.
use super::*;
use crate::gc_abi::{AllocationPage, GuardedMarkPrepare as T, MarkRecord};
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let live = a.create_text_label();
    let dead = a.create_text_label();
    let wait = a.create_text_label();
    let selected = a.create_text_label();
    let old = a.create_text_label();
    let compare = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    a.store_rbp_slot(OP, R::V6);
    a.store_rbp_slot(104, R::V2);
    let mut b = Emit { a };
    b.pointer(local(Q), invalid);
    b.branch(local(OP), n(1), C::Above, invalid);
    b.branch(local(104), n(0), C::Equal, invalid);
    let phase = field(Q, offset_of!(T, phase), 0);
    super::remembered::phase(&mut b, phase.clone(), invalid);
    b.set(local(STATE), and(phase.clone(), n(15)));
    b.branch(local(OP), n(0), C::Equal, selected);
    b.set(local(STATE), and(shr(phase, 4), n(15)));
    b.a.bind_text_label(selected);
    b.branch(local(STATE), n(4), C::NotEqual, invalid);
    super::mark_prepare::validate_state(&mut b, invalid, true);
    b.branch(local(KIND), n(1), C::Equal, wait);
    b.branch(local(OP), n(1), C::Equal, old);
    b.set(local(SEG), field(Q, offset_of!(T, page), 0));
    b.branch(
        field(SEG, offset_of!(AllocationPage, generation), 0),
        n(1),
        C::Equal,
        live,
    );
    b.set(
        local(STATE),
        field(NEXT, offset_of!(MarkRecord, young_epoch), 0),
    );
    b.a.jmp_label(compare);
    b.a.bind_text_label(old);
    b.set(
        local(STATE),
        field(NEXT, offset_of!(MarkRecord, old_epoch), 0),
    );
    b.a.bind_text_label(compare);
    b.branch(local(STATE), local(104), C::Above, invalid);
    b.branch(local(STATE), local(104), C::Equal, live);
    b.a.jmp_label(dead);
    for (label, result) in [(live, 0), (dead, 1), (invalid, 2), (wait, 3)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, result);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
