//! Unpublished committed-run cleanup transaction; gc_page_release.h contract.
use super::*;
use crate::gc_abi::{AllocationPage as P, ForwardTable as FT, HeapPool as H, PageRelease as T};
macro_rules! heap {
    ($f:ident) => {
        field(G, offset_of!(H, $f), 0)
    };
}
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(T, $f), 0)
    };
}
macro_rules! head {
    ($f:ident) => {
        field(REG, offset_of!(P, $f), 0)
    };
}
macro_rules! page {
    ($f:ident) => {
        field(SEG, offset_of!(P, $f), 0)
    };
}
fn page_bytes(v: E) -> E {
    add(add(shl(v.clone(), 6), shl(v.clone(), 5)), shl(v, 4))
}
#[allow(dead_code)]
pub(crate) fn emit_page_release<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<P>() == 112);
    }
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let success = a.create_text_label();
    let key = a.create_text_label();
    let pages = a.create_text_label();
    let tail = a.create_text_label();
    let state = a.create_text_label();
    let zero = a.create_text_label();
    let next = a.create_text_label();
    let transfer = a.create_text_label();
    let finish = a.create_text_label();
    let freeing = a.create_text_label();
    let pressure = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    b.branch(local(OP), n(1), C::Above, invalid);
    b.pointer(heap!(pages), invalid);
    b.branch(heap!(page_count), n(0), C::Equal, invalid);
    b.branch(
        heap!(page_count),
        n(crate::gc_abi::MAX_OFFSET >> 17),
        C::AboveOrEqual,
        invalid,
    );
    b.set(local(USED), page_bytes(heap!(page_count)));
    b.branch(
        heap!(pages),
        sub(n(u64::MAX), local(USED)),
        C::Above,
        invalid,
    );
    b.branch(and(heap!(heap_base), n(15)), n(0), C::NotEqual, invalid);
    b.branch(
        heap!(heap_base),
        sub(n(u64::MAX), shl(add(heap!(page_count), n(1)), 17)),
        C::Above,
        invalid,
    );
    b.branch(heap!(hard_max), n(0), C::Equal, invalid);
    b.branch(
        heap!(hard_max),
        shl(heap!(page_count), 17),
        C::Above,
        invalid,
    );
    b.branch(heap!(soft_max), heap!(hard_max), C::Above, invalid);
    b.branch(
        heap!(relocation_reserve),
        heap!(hard_max),
        C::Above,
        invalid,
    );
    b.branch(heap!(committed), heap!(hard_max), C::Above, invalid);
    b.branch(
        heap!(pending),
        sub(heap!(hard_max), heap!(committed)),
        C::Above,
        invalid,
    );
    b.branch(heap!(metadata_committed), local(USED), C::Below, invalid);
    b.branch(
        heap!(metadata_committed),
        heap!(metadata_max),
        C::Above,
        invalid,
    );
    b.branch(
        heap!(metadata_pending),
        sub(heap!(metadata_max), heap!(metadata_committed)),
        C::Above,
        invalid,
    );
    b.branch(req!(page_count), n(0), C::Equal, invalid);
    b.branch(req!(page_count), heap!(page_count), C::Above, invalid);
    b.branch(
        req!(head_index),
        sub(heap!(page_count), req!(page_count)),
        C::Above,
        invalid,
    );
    b.branch(req!(incarnation), n(0), C::Equal, invalid);
    b.set(local(LEASE), shl(req!(page_count), 17));
    b.set(local(STATE), shr(local(LEASE), 7));
    b.branch(heap!(committed), local(LEASE), C::Below, invalid);
    b.branch(
        sub(heap!(metadata_committed), local(USED)),
        local(STATE),
        C::Below,
        invalid,
    );
    b.branch(req!(stage), local(OP), C::NotEqual, invalid);
    for v in [req!(starts), req!(forward_table), req!(committed_mapping)] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    b.set(local(KIND), n(0));
    b.branch(local(OP), n(0), C::Equal, key);
    b.set(local(KIND), local(G));
    b.a.bind_text_label(key);
    b.branch(req!(owner), local(KIND), C::NotEqual, invalid);
    b.set(local(REG), add(heap!(pages), page_bytes(req!(head_index))));
    b.set(local(CTX), add(req!(head_index), req!(page_count)));
    b.branch(head!(identity_base), n(0), C::Equal, invalid);
    b.branch(head!(generation), n(1), C::Above, invalid);
    b.set(local(PREV), req!(head_index));
    b.set(local(SEG), local(REG));
    b.a.bind_text_label(pages);
    b.branch(page!(heap_base), heap!(heap_base), C::NotEqual, invalid);
    b.branch(page!(run_head), req!(head_index), C::NotEqual, invalid);
    b.branch(
        page!(start),
        shl(add(local(PREV), n(1)), 17),
        C::NotEqual,
        invalid,
    );
    b.branch(
        page!(limit),
        shl(add(local(CTX), n(1)), 17),
        C::NotEqual,
        invalid,
    );
    b.branch(page!(incarnation), req!(incarnation), C::NotEqual, invalid);
    b.branch(
        page!(identity_base),
        head!(identity_base),
        C::NotEqual,
        invalid,
    );
    b.branch(page!(generation), head!(generation), C::NotEqual, invalid);
    for v in [page!(owner), page!(metadata_leases)] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    b.branch(
        field(SEG, offset_of!(P, allocated_top), 8),
        page!(start),
        C::NotEqual,
        invalid,
    );
    b.branch(local(PREV), req!(head_index), C::NotEqual, tail);
    b.set(local(KIND), n(3));
    b.branch(local(OP), n(0), C::Equal, state);
    b.set(local(KIND), n(6));
    b.a.jmp_label(state);
    b.a.bind_text_label(tail);
    b.set(local(KIND), n(4));
    b.a.bind_text_label(state);
    b.branch(
        field(SEG, offset_of!(P, state), 8),
        local(KIND),
        C::NotEqual,
        invalid,
    );
    b.branch(local(OP), n(0), C::NotEqual, zero);
    b.branch(local(PREV), req!(head_index), C::Equal, next);
    b.a.bind_text_label(zero);
    for v in [page!(starts), page!(starts_words), page!(forward_table)] {
        b.branch(v, n(0), C::NotEqual, invalid);
    }
    b.a.bind_text_label(next);
    b.set(local(PREV), add(local(PREV), n(1)));
    b.set(local(SEG), add(local(SEG), n(112)));
    b.branch(local(PREV), local(CTX), C::Below, pages);
    b.branch(local(OP), n(0), C::NotEqual, finish);
    b.pointer(head!(starts), invalid);
    b.branch(
        head!(starts_words),
        shr(local(STATE), 3),
        C::NotEqual,
        invalid,
    );
    b.branch(
        head!(starts),
        sub(n(u64::MAX), local(STATE)),
        C::Above,
        invalid,
    );
    b.branch(head!(forward_table), n(0), C::Equal, transfer);
    b.pointer(head!(forward_table), invalid);
    b.set(local(NEXT), head!(forward_table));
    b.pointer(field(NEXT, offset_of!(FT, cells), 0), invalid);
    b.branch(
        field(NEXT, offset_of!(FT, capacity), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(NEXT, offset_of!(FT, capacity), 0),
        n(u32::MAX as u64),
        C::Above,
        invalid,
    );
    b.branch(
        field(NEXT, offset_of!(FT, cells), 0),
        sub(
            n(u64::MAX),
            shl(field(NEXT, offset_of!(FT, capacity), 0), 5),
        ),
        C::Above,
        invalid,
    );
    b.branch(
        field(NEXT, offset_of!(FT, used), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(transfer);
    b.set(req!(owner), local(G));
    b.set(req!(starts), head!(starts));
    b.set(req!(forward_table), head!(forward_table));
    b.set(req!(committed_mapping), n(1));
    b.set(req!(stage), n(1));
    b.set(head!(starts), n(0));
    b.set(head!(starts_words), n(0));
    b.set(head!(forward_table), n(0));
    b.set(field(REG, offset_of!(P, state), 8), n(6));
    b.a.jmp_label(success);
    b.a.bind_text_label(finish);
    b.set(heap!(committed), sub(heap!(committed), local(LEASE)));
    b.set(
        heap!(metadata_committed),
        sub(heap!(metadata_committed), local(STATE)),
    );
    b.set(local(PREV), req!(head_index));
    b.set(local(SEG), local(REG));
    b.a.bind_text_label(freeing);
    b.set(field(SEG, offset_of!(P, state), 8), n(0));
    b.set(local(PREV), add(local(PREV), n(1)));
    b.set(local(SEG), add(local(SEG), n(112)));
    b.branch(local(PREV), local(CTX), C::Below, freeing);
    b.set(heap!(pressure), n(0));
    b.branch(
        add(heap!(committed), heap!(pending)),
        heap!(soft_max),
        C::Below,
        pressure,
    );
    b.branch(
        add(heap!(committed), heap!(pending)),
        heap!(soft_max),
        C::Equal,
        pressure,
    );
    b.set(heap!(pressure), n(1));
    b.a.bind_text_label(pressure);
    b.set(req!(stage), n(2));
    b.a.bind_text_label(success);
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}
