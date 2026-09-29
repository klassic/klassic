//! Retained remembered claim to guarded output root, gc_remembered_load.h.
use super::*;
use crate::gc_abi::{
    AllocationPage, GuardedLoad as L, RememberedJob as J, RememberedLoad as T,
    RememberedRegistration as N, RememberedSet as S,
};
#[allow(dead_code)]
pub(crate) fn emit_load<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let done = a.create_text_label();
    let core = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(T, $f), 0)
        };
    }
    macro_rules! l {
        ($f:ident) => {
            field(Q, offset_of!(T, load) + offset_of!(L, $f), 0)
        };
    }
    macro_rules! r {
        ($f:ident) => {
            field(REG, offset_of!(N, $f), 0)
        };
    }
    macro_rules! s {
        ($f:ident) => {
            field(SEG, offset_of!(S, $f), 0)
        };
    }
    macro_rules! job {
        ($f:ident) => {
            field(LEASE, offset_of!(J, $f), 0)
        };
    }
    b.pointer(local(Q), bad);
    b.set(local(REG), q!(registration));
    b.pointer(local(REG), bad);
    b.set(local(LEASE), q!(claim));
    b.pointer(local(LEASE), bad);
    b.branch(q!(source_owner), n(0), C::Equal, bad);
    super::barrier::reference(&mut b, q!(source_owner), bad);
    b.set(local(NEXT), l!(page));
    b.pointer(local(NEXT), bad);
    b.pointer(r!(registry), bad);
    b.branch(r!(identity), n(0), C::Equal, bad);
    b.set(local(SEG), r!(set));
    b.pointer(local(SEG), bad);
    b.branch(job!(serial), n(0), C::Equal, bad);
    b.branch(job!(serial), s!(active_serial), C::NotEqual, bad);
    b.branch(job!(offset), s!(active_offset), C::NotEqual, bad);
    b.branch(job!(incarnation), s!(incarnation), C::NotEqual, bad);
    b.branch(job!(phase), s!(epoch), C::NotEqual, bad);
    b.branch(r!(epoch), s!(epoch), C::NotEqual, bad);
    b.set(local(STATE), shr(q!(source_owner), 16));
    b.branch(local(STATE), s!(start), C::Below, bad);
    b.branch(local(STATE), s!(limit), C::Above, bad);
    b.branch(local(STATE), s!(limit), C::Equal, bad);
    b.branch(l!(byte_offset), sub(s!(limit), local(STATE)), C::Above, bad);
    b.branch(l!(byte_offset), sub(s!(limit), local(STATE)), C::Equal, bad);
    b.branch(and(l!(byte_offset), n(7)), n(0), C::NotEqual, bad);
    b.branch(
        add(local(STATE), l!(byte_offset)),
        job!(offset),
        C::NotEqual,
        bad,
    );
    super::remembered::phase(&mut b, l!(phase), bad);
    b.branch(l!(phase), job!(phase), C::Below, bad);
    b.branch(
        and(l!(phase), n(0x1000)),
        and(job!(phase), n(0x1000)),
        C::NotEqual,
        bad,
    );
    b.branch(and(l!(phase), n(15)), n(1), C::Below, bad);
    b.branch(and(l!(phase), n(15)), n(3), C::Above, bad);
    b.branch(
        field(NEXT, offset_of!(AllocationPage, generation), 0),
        n(1),
        C::NotEqual,
        bad,
    );
    b.value(add(local(Q), n(offset_of!(T, load) as u64)));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.call_label(core);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    super::barrier::emit_guarded_load(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
