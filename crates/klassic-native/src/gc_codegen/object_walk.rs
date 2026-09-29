//! Authenticated one-key page enumeration, gc_object_walk.h.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, ForwardCell as F, ForwardTable as T, MetadataLeaseRequest as L,
    ObjectWalk as W,
};
const TEMP: i32 = 136;
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let end = a.create_text_label();
    let wait = a.create_text_label();
    let metadata = a.create_text_label();
    let state_ok = a.create_text_label();
    let commit = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 32);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(W, $f), 0)
        };
    }
    macro_rules! p {
        ($f:ident) => {
            field(SEG, offset_of!(P, $f), 0)
        };
    }
    macro_rules! t {
        ($f:ident) => {
            field(REG, offset_of!(T, $f), 0)
        };
    }
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.pointer(q!(lease), invalid);
    b.branch(q!(source_offset), n(0), C::NotEqual, invalid);
    b.set(local(LEASE), q!(lease));
    b.branch(
        field(LEASE, offset_of!(L, page), 0),
        q!(page),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(LEASE, offset_of!(L, incarnation), 0),
        q!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.set(local(TEMP), n(0));
    b.set(local(TEMP - 8), n(0));
    b.set(local(TEMP - 16), field(LEASE, offset_of!(L, token), 0));
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, TEMP);
    b.a.mov_imm64(R::V2, 2);
    b.a.call_label(metadata);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.branch(local(TEMP), q!(page), C::NotEqual, invalid);
    b.branch(local(TEMP - 8), q!(incarnation), C::NotEqual, invalid);
    b.set(local(SEG), local(TEMP));
    b.set(local(STATE), field(SEG, offset_of!(P, state), 8));
    for state in [1, 5, 7] {
        b.branch(local(STATE), n(state), C::Equal, state_ok);
    }
    b.branch(local(STATE), n(8), C::NotEqual, invalid);
    b.a.bind_text_label(state_ok);
    b.branch(p!(start), n(0), C::Equal, invalid);
    b.branch(p!(start), p!(limit), C::AboveOrEqual, invalid);
    b.branch(p!(limit), n(crate::gc_abi::MAX_OFFSET), C::Above, invalid);
    for v in [p!(start), p!(limit)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, invalid);
    }
    b.pointer(q!(table), invalid);
    b.branch(q!(table), p!(forward_table), C::NotEqual, invalid);
    b.set(local(REG), q!(table));
    b.pointer(t!(cells), invalid);
    b.branch(t!(capacity), n(0), C::Equal, invalid);
    b.branch(t!(capacity), n(u32::MAX as u64), C::Above, invalid);
    const {
        assert!(size_of::<F>() == 32);
    }
    b.branch(
        t!(cells),
        sub(n(u64::MAX), shl(t!(capacity), 5)),
        C::Above,
        invalid,
    );
    b.branch(q!(used), t!(capacity), C::Above, invalid);
    b.branch(q!(cursor), q!(used), C::Above, invalid);
    b.set(local(USED), field(REG, offset_of!(T, used), 8));
    b.branch(local(USED), t!(capacity), C::Above, invalid);
    b.branch(local(USED), q!(used), C::NotEqual, wait);
    b.branch(q!(cursor), q!(used), C::Equal, end);
    b.set(local(NEXT), add(t!(cells), shl(q!(cursor), 5)));
    b.set(local(STATE), field(SEG, offset_of!(P, allocated_top), 8));
    b.set(local(USED), field(NEXT, offset_of!(F, source_offset), 0));
    b.branch(local(STATE), p!(start), C::Below, invalid);
    b.branch(local(STATE), p!(limit), C::Above, invalid);
    for v in [local(STATE), local(USED)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, invalid);
    }
    b.branch(local(USED), add(p!(start), n(32)), C::Below, invalid);
    b.branch(local(USED), local(STATE), C::AboveOrEqual, invalid);
    b.branch(
        field(NEXT, offset_of!(F, incarnation), 0),
        q!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.branch(q!(cursor), n(0), C::Equal, commit);
    b.set(local(PREV), sub(local(NEXT), n(32)));
    b.branch(
        field(PREV, offset_of!(F, source_offset), 0),
        local(USED),
        C::AboveOrEqual,
        invalid,
    );
    b.a.bind_text_label(commit);
    b.set(q!(source_offset), local(USED));
    b.set(q!(cursor), add(q!(cursor), n(1)));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(end);
    b.a.mov_imm64(R::V0, 1);
    b.a.jmp_label(done);
    b.a.bind_text_label(wait);
    b.a.mov_imm64(R::V0, 3);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 2);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
}
