//! Admit a bounded prepared tail without moving live lease tokens.
use super::*;
use crate::gc_abi::{MetadataLeasePool as P, MetadataLeaseSlot as S};
#[allow(dead_code)]
pub(crate) fn emit_grow<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let not_full = a.create_text_label();
    let head_ok = a.create_text_label();
    let validate = a.create_text_label();
    let link = a.create_text_label();
    let tail = a.create_text_label();
    let store = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.pointer(local(G), bad);
    macro_rules! p {
        ($f:ident) => {
            field(G, offset_of!(P, $f), 0)
        };
    }
    b.pointer(p!(heap), bad);
    b.pointer(p!(slots), bad);
    b.set(local(USED), p!(capacity));
    b.branch(local(USED), n(0), C::Equal, bad);
    b.branch(local(USED), n(u32::MAX as u64), C::Above, bad);
    b.branch(local(Q), local(USED), C::Below, bad);
    b.branch(local(Q), local(USED), C::Equal, bad);
    b.branch(local(Q), n(u32::MAX as u64), C::Above, bad);
    b.branch(sub(local(Q), local(USED)), n(256), C::Above, bad);
    b.branch(p!(slots), sub(n(u64::MAX), shl(local(Q), 5)), C::Above, bad);
    b.branch(p!(active), local(USED), C::Above, bad);
    b.branch(p!(free_head), local(USED), C::Above, bad);
    b.branch(p!(active), local(USED), C::Below, not_full);
    b.branch(p!(free_head), n(0), C::NotEqual, bad);
    b.a.jmp_label(head_ok);
    b.a.bind_text_label(not_full);
    b.branch(p!(free_head), n(0), C::Equal, bad);
    b.a.bind_text_label(head_ok);
    b.set(local(STATE), local(USED));
    b.a.bind_text_label(validate);
    b.set(local(REG), add(p!(slots), shl(local(STATE), 5)));
    for offset in [
        offset_of!(S, page),
        offset_of!(S, incarnation),
        offset_of!(S, generation),
        offset_of!(S, next_free),
    ] {
        b.branch(field(REG, offset, 0), n(0), C::NotEqual, bad);
    }
    b.set(local(STATE), add(local(STATE), n(1)));
    b.branch(local(STATE), local(Q), C::Below, validate);
    b.set(local(STATE), local(USED));
    b.a.bind_text_label(link);
    b.set(local(REG), add(p!(slots), shl(local(STATE), 5)));
    b.set(local(STATE), add(local(STATE), n(1)));
    b.branch(local(STATE), local(Q), C::Equal, tail);
    b.set(local(NEXT), add(local(STATE), n(1)));
    b.a.jmp_label(store);
    b.a.bind_text_label(tail);
    b.set(local(NEXT), p!(free_head));
    b.a.bind_text_label(store);
    b.set(field(REG, offset_of!(S, next_free), 0), local(NEXT));
    b.branch(local(STATE), local(Q), C::Below, link);
    b.set(p!(free_head), add(local(USED), n(1)));
    b.set(p!(capacity), local(Q));
    b.a.mov_imm64(R::V0, 0);
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.a.mov_imm64(R::V0, 1);
    b.a.bind_text_label(done);
    b.a.leave();
}
