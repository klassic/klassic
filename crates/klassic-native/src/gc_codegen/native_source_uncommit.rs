//! SourceUncommit-bound native payload discard; metadata remains independently leased.
use super::*;
use crate::gc_abi::{AllocationPage as P, HeapPool as H, SourceUncommit as T};
macro_rules! h {
    ($f:ident) => {
        field(G, offset_of!(H, $f), 0)
    };
}
macro_rules! q {
    ($f:ident) => {
        field(Q, offset_of!(T, $f), 0)
    };
}
macro_rules! p {
    ($f:ident) => {
        field(SEG, offset_of!(P, $f), 0)
    };
}
fn page_bytes(v: E) -> E {
    sub(shl(v.clone(), 7), shl(v, 4))
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<P>() == 112);
    }
    let invalid = a.create_text_label();
    let failure = a.create_text_label();
    let done = a.create_text_label();
    let scan = a.create_text_label();
    let tail = a.create_text_label();
    let check = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(local(OP), n(0), C::Equal, invalid);
    b.branch(h!(heap_base), n(0), C::Equal, invalid);
    b.branch(and(h!(heap_base), n(15)), n(0), C::NotEqual, invalid);
    b.pointer(h!(pages), invalid);
    b.branch(h!(page_count), n(0), C::Equal, invalid);
    b.branch(
        h!(page_count),
        n(crate::gc_abi::MAX_OFFSET >> 17),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(
        h!(pages),
        sub(n(u64::MAX), page_bytes(h!(page_count))),
        C::Above,
        invalid,
    );
    b.branch(
        h!(heap_base),
        sub(n(u64::MAX), shl(add(h!(page_count), n(1)), 17)),
        C::Above,
        invalid,
    );
    b.branch(q!(stage), n(1), C::NotEqual, invalid);
    b.branch(q!(owner), local(G), C::NotEqual, invalid);
    b.branch(q!(committed_mapping), n(1), C::NotEqual, invalid);
    b.branch(q!(incarnation), n(0), C::Equal, invalid);
    b.branch(q!(page_count), n(0), C::Equal, invalid);
    b.branch(q!(page_count), h!(page_count), C::Above, invalid);
    b.branch(
        q!(head_index),
        sub(h!(page_count), q!(page_count)),
        C::Above,
        invalid,
    );
    b.set(local(REG), add(h!(pages), page_bytes(q!(head_index))));
    b.set(local(SEG), local(REG));
    b.set(local(STATE), q!(head_index));
    b.set(local(USED), add(q!(head_index), q!(page_count)));
    b.branch(p!(identity_base), n(0), C::Equal, invalid);
    b.branch(p!(generation), n(1), C::Above, invalid);
    b.a.bind_text_label(scan);
    b.set(local(PREV), n(crate::gc_abi::PAGE_RUN_TAIL));
    b.set(local(NEXT), n(0));
    b.branch(local(STATE), q!(head_index), C::NotEqual, tail);
    b.set(local(PREV), n(crate::gc_abi::PAGE_EVACUATED));
    b.set(local(NEXT), local(Q));
    b.a.bind_text_label(tail);
    b.branch(
        field(SEG, offset_of!(P, state), 8),
        local(PREV),
        C::NotEqual,
        invalid,
    );
    b.branch(p!(owner), local(NEXT), C::NotEqual, invalid);
    b.branch(p!(heap_base), h!(heap_base), C::NotEqual, invalid);
    b.branch(p!(run_head), q!(head_index), C::NotEqual, invalid);
    b.branch(
        p!(start),
        shl(add(local(STATE), n(1)), 17),
        C::NotEqual,
        invalid,
    );
    b.branch(
        p!(limit),
        shl(add(local(USED), n(1)), 17),
        C::NotEqual,
        invalid,
    );
    b.branch(p!(incarnation), q!(incarnation), C::NotEqual, invalid);
    b.branch(
        p!(identity_base),
        field(REG, offset_of!(P, identity_base), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        p!(generation),
        field(REG, offset_of!(P, generation), 0),
        C::NotEqual,
        invalid,
    );
    b.set(local(STATE), add(local(STATE), n(1)));
    b.branch(local(STATE), local(USED), C::Equal, check);
    b.set(local(SEG), add(local(SEG), n(112)));
    b.a.jmp_label(scan);
    b.a.bind_text_label(check);
    b.set(
        local(LEASE),
        add(h!(heap_base), shl(add(q!(head_index), n(1)), 17)),
    );
    b.set(local(CTX), shl(q!(page_count), 17));
    b.a.load_rbp_slot(R::V10, OP);
    b.a.load_rbp_slot(R::V7, LEASE);
    b.a.load_rbp_slot(R::V6, CTX);
    b.a.call_native_binary(R::V10);
    b.a.and_reg_imm32(R::V0, 255);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, failure);
    b.set(q!(committed_mapping), n(0));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(failure);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}
