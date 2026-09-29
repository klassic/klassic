//! One-page metadata enumeration, matching gc_heap_walk.h.
use super::*;
use crate::gc_abi::{AllocationPage as P, HeapPool as H, HeapWalk as W, MetadataLeasePool as M};
const TEMP: i32 = 136;
fn bytes(v: E) -> E {
    const {
        assert!(size_of::<P>() == 112);
    }
    add(add(shl(v.clone(), 6), shl(v.clone(), 5)), shl(v, 4))
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let end = a.create_text_label();
    let wait = a.create_text_label();
    let skip = a.create_text_label();
    let advance = a.create_text_label();
    let tail = a.create_text_label();
    let acquire = a.create_text_label();
    let metadata = a.create_text_label();
    let success = a.create_text_label();
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
    macro_rules! h {
        ($f:ident) => {
            field(LEASE, offset_of!(H, $f), 0)
        };
    }
    macro_rules! p {
        ($f:ident) => {
            field(SEG, offset_of!(P, $f), 0)
        };
    }
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(q!(pool), local(G), C::NotEqual, invalid);
    b.set(local(LEASE), field(G, offset_of!(M, heap), 0));
    b.pointer(local(LEASE), invalid);
    b.branch(q!(heap), local(LEASE), C::NotEqual, invalid);
    for off in [0, 8, 16] {
        b.branch(
            field(Q, offset_of!(W, lease) + off, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
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
        sub(n(u64::MAX), bytes(h!(page_count))),
        C::Above,
        invalid,
    );
    b.branch(q!(pages), h!(pages), C::NotEqual, invalid);
    b.branch(q!(page_count), h!(page_count), C::NotEqual, invalid);
    b.branch(q!(cursor), h!(page_count), C::Above, invalid);
    b.branch(q!(cursor), h!(page_count), C::Equal, end);
    b.set(local(SEG), add(h!(pages), bytes(q!(cursor))));
    b.set(local(STATE), field(SEG, offset_of!(P, state), 8));
    b.branch(local(STATE), n(8), C::Above, invalid);
    for state in [2, 6] {
        b.branch(local(STATE), n(state), C::Equal, wait);
    }
    for state in [0, 3, 4] {
        b.branch(local(STATE), n(state), C::Equal, skip);
    }
    b.a.jmp_label(acquire);
    b.a.bind_text_label(skip);
    b.branch(p!(metadata_leases), n(0), C::NotEqual, invalid);
    b.branch(local(STATE), n(4), C::Equal, tail);
    b.branch(local(STATE), n(0), C::Equal, advance);
    b.branch(
        field(SEG, offset_of!(P, allocated_top), 8),
        p!(start),
        C::NotEqual,
        wait,
    );
    b.a.jmp_label(advance);
    b.a.bind_text_label(tail);
    b.branch(p!(run_head), q!(cursor), C::AboveOrEqual, invalid);
    b.branch(p!(incarnation), n(0), C::Equal, invalid);
    b.set(local(REG), add(h!(pages), bytes(p!(run_head))));
    b.branch(
        field(REG, offset_of!(P, run_head), 0),
        p!(run_head),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(REG, offset_of!(P, incarnation), 0),
        p!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.set(local(STATE), field(REG, offset_of!(P, state), 8));
    for state in [0, 4] {
        b.branch(local(STATE), n(state), C::Equal, invalid);
    }
    b.branch(local(STATE), n(8), C::Above, invalid);
    b.a.bind_text_label(advance);
    b.set(q!(cursor), add(q!(cursor), n(1)));
    b.a.mov_imm64(R::V0, 1);
    b.a.jmp_label(done);
    b.a.bind_text_label(acquire);
    b.set(local(TEMP), local(SEG));
    b.set(local(TEMP - 8), p!(incarnation));
    b.set(local(TEMP - 16), n(0));
    b.a.load_rbp_slot(R::V7, G);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, TEMP);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(metadata);
    b.a.cmp_reg_imm8(R::V0, 0);
    b.a.jcc_label(C::Equal, success);
    b.a.add_reg_imm32(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(success);
    for i in 0..3 {
        b.set(
            field(Q, offset_of!(W, lease) + i * 8, 0),
            local(TEMP - i as i32 * 8),
        );
    }
    b.set(q!(cursor), add(q!(cursor), n(1)));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(end);
    b.a.mov_imm64(R::V0, 2);
    b.a.jmp_label(done);
    b.a.bind_text_label(wait);
    b.a.mov_imm64(R::V0, 6);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 3);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
}
