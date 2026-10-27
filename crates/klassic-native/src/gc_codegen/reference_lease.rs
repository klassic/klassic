//! Strong source reference -> run-head metadata lease; gc_reference_lease.h contract.
use super::*;
use crate::gc_abi::{AllocationPage as P, HeapPool as H, MetadataLeasePool as M, ReferenceLease};
use std::mem::size_of;
macro_rules! heap {
    ($f:ident) => {
        field(LEASE, offset_of!(H, $f), 0)
    };
}
macro_rules! head {
    (state) => {
        field(REG, offset_of!(P, state), 8)
    };
    ($f:ident) => {
        field(REG, offset_of!(P, $f), 0)
    };
}
macro_rules! page {
    ($f:ident) => {
        field(SEG, offset_of!(P, $f), 0)
    };
}
fn table_bytes(value: E) -> E {
    // Layout checked here so pointer indexing cannot silently change stride.
    const {
        assert!(size_of::<P>() == 112);
    }
    add(
        add(shl(value.clone(), 6), shl(value.clone(), 5)),
        shl(value, 4),
    )
}
fn reference() -> E {
    field(Q, offset_of!(ReferenceLease, reference), 0)
}
#[allow(dead_code)]
pub(crate) fn emit_reference_lease<A: PortableAsm>(a: &mut A) {
    emit_reference_lease_tracked(a, &mut |_| {}, &mut |_| {});
}
pub(crate) fn emit_reference_lease_tracked<A: PortableAsm>(
    a: &mut A,
    changed: &mut impl FnMut(&mut A),
    full: &mut impl FnMut(&mut A),
) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let core = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 48);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    for off in [8, 16, 24] {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, invalid);
    }
    b.set(local(LEASE), field(G, offset_of!(M, heap), 0));
    b.pointer(local(LEASE), invalid);
    b.set(local(USED), shr(reference(), 16));
    b.branch(local(USED), n(0), C::Equal, invalid);
    b.branch(
        and(reference(), n(0xf0000 | 0xcf)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(and(reference(), n(0x30)), n(0), C::Equal, invalid);
    for mask in [0x300, 0xc00, 0xf000] {
        b.set(local(PREV), and(reference(), n(mask)));
        b.branch(local(PREV), n(0), C::Equal, invalid);
        b.branch(
            and(local(PREV), sub(local(PREV), n(1))),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.pointer(heap!(pages), invalid);
    b.branch(heap!(page_count), n(0), C::Equal, invalid);
    b.branch(
        heap!(page_count),
        n(crate::gc_abi::MAX_OFFSET >> 17),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(
        add(heap!(pages), table_bytes(heap!(page_count))),
        heap!(pages),
        C::Below,
        invalid,
    );
    b.branch(and(heap!(heap_base), n(15)), n(0), C::NotEqual, invalid);
    b.set(local(NEXT), shl(add(heap!(page_count), n(1)), 17));
    b.branch(
        add(heap!(heap_base), local(NEXT)),
        heap!(heap_base),
        C::Below,
        invalid,
    );
    b.set(local(STATE), shr(local(USED), 17));
    b.branch(local(STATE), n(0), C::Equal, invalid);
    b.branch(local(STATE), heap!(page_count), C::Above, invalid);
    b.set(local(STATE), sub(local(STATE), n(1)));
    b.set(local(SEG), add(heap!(pages), table_bytes(local(STATE))));
    b.branch(page!(run_head), local(STATE), C::Above, invalid);
    b.set(local(KIND), page!(run_head));
    b.set(local(REG), add(heap!(pages), table_bytes(local(KIND))));
    let live = b.a.create_text_label();
    b.branch(head!(state), n(1), C::Equal, live);
    b.branch(head!(state), n(5), C::Equal, live);
    b.branch(head!(state), n(7), C::Equal, live);
    b.branch(head!(state), n(8), C::NotEqual, invalid);
    b.a.bind_text_label(live);
    b.branch(head!(run_head), local(KIND), C::NotEqual, invalid);
    b.branch(head!(heap_base), heap!(heap_base), C::NotEqual, invalid);
    b.branch(
        head!(start),
        shl(add(local(KIND), n(1)), 17),
        C::NotEqual,
        invalid,
    );
    b.branch(head!(limit), head!(start), C::Below, invalid);
    b.branch(head!(limit), head!(start), C::Equal, invalid);
    b.branch(head!(limit), local(NEXT), C::Above, invalid);
    b.branch(
        and(head!(limit), n((1 << 17) - 1)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(head!(incarnation), n(0), C::Equal, invalid);
    b.branch(head!(identity_base), n(0), C::Equal, invalid);
    b.branch(head!(generation), n(1), C::Above, invalid);
    b.branch(page!(heap_base), heap!(heap_base), C::NotEqual, invalid);
    b.branch(
        page!(start),
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
    let same = b.a.create_text_label();
    b.branch(local(STATE), local(KIND), C::Equal, same);
    b.branch(
        field(SEG, offset_of!(P, state), 8),
        n(4),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(same);
    b.set(local(NEXT), field(REG, offset_of!(P, allocated_top), 8));
    b.branch(local(NEXT), head!(start), C::Below, invalid);
    b.branch(local(NEXT), head!(limit), C::Above, invalid);
    b.branch(and(local(NEXT), n(15)), n(0), C::NotEqual, invalid);
    b.branch(local(USED), add(head!(start), n(32)), C::Below, invalid);
    b.branch(local(USED), local(NEXT), C::AboveOrEqual, invalid);
    b.pointer(head!(starts), invalid);
    b.branch(
        head!(starts_words),
        shr(sub(head!(limit), head!(start)), 10),
        C::Below,
        invalid,
    );
    b.branch(head!(starts_words), n(u64::MAX / 8), C::Above, invalid);
    b.branch(
        add(head!(starts), shl(head!(starts_words), 3)),
        head!(starts),
        C::Below,
        invalid,
    );
    b.set(local(PREV), shr(sub(local(USED), head!(start)), 4));
    b.set(local(NEXT), add(head!(starts), shl(shr(local(PREV), 6), 3)));
    b.value(and(local(PREV), n(63)));
    b.a.mov_reg_reg(R::V1, R::V0);
    b.a.mov_imm64(R::V2, 1);
    b.a.shl_reg_count(R::V2);
    b.a.load_rbp_slot(R::V0, NEXT);
    b.a.atomic_load_acquire(R::V0, R::V0);
    b.a.and_reg_reg(R::V0, R::V2);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, invalid);
    // Stack temporary prevents partial output on core pool failure.
    b.a.mov_reg_reg(R::V0, R::V5);
    b.a.sub_reg_imm32(R::V0, 144);
    b.a.store_rbp_slot(CTX, R::V0);
    b.set(field(CTX, 0, 0), local(REG));
    b.set(field(CTX, 8, 0), head!(incarnation));
    b.set(field(CTX, 16, 0), n(0));
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, CTX);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(core);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, done);
    for off in [0, 8, 16] {
        b.set(field(Q, 8 + off, 0), field(CTX, off, 0));
    }
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(core);
    super::metadata::emit_metadata_step_tracked(b.a, changed, full);
    b.a.ret();
    b.a.bind_text_label(after);
}
