//! Evacuated payload accounting transaction, retaining source metadata/range.
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
        field(REG, offset_of!(P, $f), 0)
    };
}
fn page_bytes(v: E) -> E {
    add(add(shl(v.clone(), 6), shl(v.clone(), 5)), shl(v, 4))
}
#[allow(dead_code)]
pub(crate) fn emit_source_uncommit<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<P>() == 112);
    }
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let finish = a.create_text_label();
    let key = a.create_text_label();
    let pressure = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(1), C::Above, invalid);
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
    b.branch(and(h!(heap_base), n(15)), n(0), C::NotEqual, invalid);
    b.branch(
        h!(heap_base),
        sub(n(u64::MAX), shl(add(h!(page_count), n(1)), 17)),
        C::Above,
        invalid,
    );
    b.branch(h!(hard_max), n(0), C::Equal, invalid);
    b.branch(h!(hard_max), shl(h!(page_count), 17), C::Above, invalid);
    b.branch(h!(soft_max), h!(hard_max), C::Above, invalid);
    b.branch(h!(relocation_reserve), h!(hard_max), C::Above, invalid);
    b.branch(h!(committed), h!(hard_max), C::Above, invalid);
    b.branch(
        h!(pending),
        sub(h!(hard_max), h!(committed)),
        C::Above,
        invalid,
    );
    b.branch(q!(page_count), n(0), C::Equal, invalid);
    b.branch(q!(page_count), h!(page_count), C::Above, invalid);
    b.branch(
        q!(head_index),
        sub(h!(page_count), q!(page_count)),
        C::Above,
        invalid,
    );
    b.branch(q!(incarnation), n(0), C::Equal, invalid);
    b.branch(q!(stage), local(OP), C::NotEqual, invalid);
    b.branch(q!(committed_mapping), n(0), C::NotEqual, invalid);
    b.set(local(PREV), n(0));
    b.set(local(NEXT), n(0));
    b.branch(local(OP), n(0), C::Equal, key);
    b.set(local(PREV), local(G));
    b.set(local(NEXT), local(Q));
    b.a.bind_text_label(key);
    b.branch(q!(owner), local(PREV), C::NotEqual, invalid);
    b.set(local(USED), shl(q!(page_count), 17));
    b.branch(h!(committed), local(USED), C::Below, invalid);
    b.set(local(REG), add(h!(pages), page_bytes(q!(head_index))));
    b.branch(p!(heap_base), h!(heap_base), C::NotEqual, invalid);
    b.branch(p!(run_head), q!(head_index), C::NotEqual, invalid);
    b.branch(
        p!(start),
        shl(add(q!(head_index), n(1)), 17),
        C::NotEqual,
        invalid,
    );
    b.branch(
        p!(limit),
        shl(add(add(q!(head_index), q!(page_count)), n(1)), 17),
        C::NotEqual,
        invalid,
    );
    b.branch(p!(incarnation), q!(incarnation), C::NotEqual, invalid);
    b.branch(p!(identity_base), n(0), C::Equal, invalid);
    b.branch(p!(generation), n(1), C::Above, invalid);
    b.branch(
        field(REG, offset_of!(P, state), 8),
        n(7),
        C::NotEqual,
        invalid,
    );
    b.branch(p!(owner), local(NEXT), C::NotEqual, invalid);
    b.branch(local(OP), n(1), C::Equal, finish);
    b.set(p!(owner), local(Q));
    b.set(q!(owner), local(G));
    b.set(q!(stage), n(1));
    b.set(q!(committed_mapping), n(1));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(finish);
    b.set(h!(committed), sub(h!(committed), local(USED)));
    b.set(h!(pressure), n(0));
    b.branch(
        h!(soft_max),
        add(h!(committed), h!(pending)),
        C::AboveOrEqual,
        pressure,
    );
    b.set(h!(pressure), n(1));
    b.a.bind_text_label(pressure);
    b.set(p!(owner), n(0));
    b.set(
        field(REG, offset_of!(P, state), 8),
        n(crate::gc_abi::PAGE_UNCOMMITTED),
    );
    b.set(q!(stage), n(2));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}
