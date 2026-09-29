//! Bounded uncommitted-source metadata retirement; gc_source_retire.h.
use super::*;
use crate::gc_abi::{AllocationPage as P, HeapPool as H, SourceRetire as T};
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
pub(crate) fn emit_source_retire<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<P>() == 112);
    }
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let step = a.create_text_label();
    let complete = a.create_text_label();
    let more = a.create_text_label();
    let wait = a.create_text_label();
    let key = a.create_text_label();

    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(1), C::Above, invalid);
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
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
    b.branch(
        h!(metadata_committed),
        page_bytes(h!(page_count)),
        C::Below,
        invalid,
    );
    b.branch(h!(metadata_committed), h!(metadata_max), C::Above, invalid);
    b.branch(
        h!(metadata_pending),
        sub(h!(metadata_max), h!(metadata_committed)),
        C::Above,
        invalid,
    );
    b.set(local(USED), shl(q!(page_count), 10));
    b.branch(
        sub(h!(metadata_committed), page_bytes(h!(page_count))),
        local(USED),
        C::Below,
        invalid,
    );
    b.branch(q!(stage), local(OP), C::NotEqual, invalid);
    b.set(local(PREV), n(0));
    b.set(local(NEXT), n(0));
    b.set(local(KIND), n(crate::gc_abi::PAGE_UNCOMMITTED));
    b.branch(local(OP), n(0), C::Equal, key);
    b.set(local(PREV), local(G));
    b.set(local(NEXT), local(Q));
    b.set(local(KIND), n(crate::gc_abi::PAGE_RELEASING));
    b.a.bind_text_label(key);
    b.branch(q!(owner), local(PREV), C::NotEqual, invalid);
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
    b.branch(p!(owner), local(NEXT), C::NotEqual, invalid);
    b.branch(
        field(REG, offset_of!(P, state), 8),
        local(KIND),
        C::NotEqual,
        invalid,
    );
    b.branch(p!(metadata_leases), n(0), C::NotEqual, wait);
    b.branch(local(OP), n(1), C::Equal, step);
    for value in [q!(cursor), q!(starts), q!(forward_table)] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.pointer(p!(starts), invalid);
    b.branch(p!(starts), sub(n(u64::MAX), local(USED)), C::Above, invalid);
    b.branch(p!(starts_words), shr(local(USED), 3), C::NotEqual, invalid);
    b.pointer(p!(forward_table), invalid);
    b.set(q!(owner), local(G));
    b.set(q!(stage), n(1));
    b.set(q!(cursor), n(1));
    b.set(q!(starts), p!(starts));
    b.set(q!(forward_table), p!(forward_table));
    b.set(p!(owner), local(Q));
    b.set(p!(starts), n(0));
    b.set(p!(starts_words), n(0));
    b.set(p!(forward_table), n(0));
    b.set(
        field(REG, offset_of!(P, state), 8),
        n(crate::gc_abi::PAGE_RELEASING),
    );
    b.a.jmp_label(more);
    b.a.bind_text_label(step);
    b.branch(q!(cursor), n(0), C::Equal, invalid);
    b.branch(q!(cursor), q!(page_count), C::Above, invalid);
    for value in [p!(starts), p!(starts_words), p!(forward_table)] {
        b.branch(value, n(0), C::NotEqual, invalid);
    }
    b.branch(q!(starts), n(0), C::NotEqual, wait);
    b.branch(q!(forward_table), n(0), C::NotEqual, wait);
    b.branch(q!(cursor), q!(page_count), C::Equal, complete);
    b.set(local(STATE), add(q!(head_index), q!(cursor)));
    b.set(local(SEG), add(h!(pages), page_bytes(local(STATE))));
    b.branch(
        field(SEG, offset_of!(P, heap_base), 0),
        h!(heap_base),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(P, run_head), 0),
        q!(head_index),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(P, start), 0),
        shl(add(local(STATE), n(1)), 17),
        C::NotEqual,
        invalid,
    );
    for off in [
        offset_of!(P, limit),
        offset_of!(P, incarnation),
        offset_of!(P, identity_base),
        offset_of!(P, generation),
    ] {
        b.branch(field(SEG, off, 0), field(REG, off, 0), C::NotEqual, invalid);
    }
    for off in [
        offset_of!(P, owner),
        offset_of!(P, metadata_leases),
        offset_of!(P, starts),
        offset_of!(P, starts_words),
        offset_of!(P, forward_table),
    ] {
        b.branch(field(SEG, off, 0), n(0), C::NotEqual, invalid);
    }
    b.branch(
        field(SEG, offset_of!(P, state), 8),
        n(crate::gc_abi::PAGE_RUN_TAIL),
        C::NotEqual,
        invalid,
    );
    b.set(
        field(SEG, offset_of!(P, state), 8),
        n(crate::gc_abi::PAGE_FREE),
    );
    b.set(q!(cursor), add(q!(cursor), n(1)));
    b.branch(q!(cursor), q!(page_count), C::Below, more);
    b.a.bind_text_label(complete);
    b.set(
        h!(metadata_committed),
        sub(h!(metadata_committed), local(USED)),
    );
    b.set(p!(owner), n(0));
    b.set(
        field(REG, offset_of!(P, state), 8),
        n(crate::gc_abi::PAGE_FREE),
    );
    b.set(q!(stage), n(2));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    for (label, code) in [(invalid, 1), (more, 2), (wait, 3)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, code);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
