//! Canonical generation filtering and protected mark transfer; gc_mark_route.h.
use super::*;
use crate::gc_abi::{AllocationPage, GuardedMarkPrepare as P, GuardedMarkRoute as T};
#[allow(dead_code)]
pub(crate) fn emit_route<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let skip = a.create_text_label();
    let wait = a.create_text_label();
    let nonnull = a.create_text_label();
    let selected = a.create_text_label();
    let commit = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    b.pointer(local(Q), bad);
    b.set(local(OP), field(Q, offset_of!(T, generation), 0));
    b.branch(local(OP), n(1), C::Above, bad);
    b.set(local(Q), add(local(Q), n(offset_of!(T, prepare) as u64)));
    macro_rules! p {
        ($f:ident) => {
            field(Q, offset_of!(P, $f), 0)
        };
    }
    super::remembered::phase(&mut b, p!(phase), bad);
    b.set(local(STATE), and(p!(phase), n(15)));
    b.branch(local(OP), n(0), C::Equal, selected);
    b.set(local(STATE), and(shr(p!(phase), 4), n(15)));
    b.a.bind_text_label(selected);
    b.branch(local(STATE), n(1), C::Below, bad);
    b.branch(local(STATE), n(3), C::Above, bad);
    b.branch(p!(reference), n(0), C::NotEqual, nonnull);
    for off in (0..std::mem::size_of::<P>()).step_by(8) {
        if off != offset_of!(P, phase) && off != offset_of!(P, reference) {
            b.branch(field(Q, off, 0), n(0), C::NotEqual, bad);
        }
    }
    b.a.jmp_label(skip);
    b.a.bind_text_label(nonnull);
    super::mark_prepare::validate_state(&mut b, bad, true);
    b.branch(local(KIND), n(1), C::Equal, wait);
    b.branch(local(OP), n(1), C::Equal, commit);
    b.set(local(SEG), p!(page));
    b.branch(
        field(SEG, offset_of!(AllocationPage, generation), 0),
        n(1),
        C::Equal,
        skip,
    );
    b.a.bind_text_label(commit);
    super::mark_prepare::transfer(&mut b);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(wait);
    b.a.mov_imm64(R::V0, 3);
    b.a.jmp_label(done);
    b.a.bind_text_label(skip);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}
