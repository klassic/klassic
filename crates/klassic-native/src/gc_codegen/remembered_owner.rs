//! Metadata-only predecessor search for a retained claim; gc_remembered_owner.h.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, ForwardCell as F, ForwardTable as T, MAX_OFFSET, RememberedJob as J,
    RememberedOwner as O, RememberedRegistration as N, RememberedSet as S,
};
#[allow(dead_code)]
pub(crate) fn emit_owner<A: PortableAsm>(a: &mut A) {
    let status: [_; 3] = std::array::from_fn(|_| a.create_text_label());
    let bad = status[1];
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(Q, R::V7);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(O, $f), 0)
        };
    }
    macro_rules! s {
        ($f:ident) => {
            field(SEG, offset_of!(S, $f), 0)
        };
    }
    macro_rules! p {
        ($f:ident) => {
            field(REG, offset_of!(P, $f), 0)
        };
    }
    macro_rules! j {
        ($f:ident) => {
            field(LEASE, offset_of!(J, $f), 0)
        };
    }
    macro_rules! t {
        ($f:ident) => {
            field(G, offset_of!(T, $f), 0)
        };
    }
    b.pointer(local(Q), bad);
    for out in [q!(owner), q!(byte_offset), q!(cell)] {
        b.branch(out, n(0), C::NotEqual, bad);
    }
    b.set(local(G), q!(registration));
    b.pointer(local(G), bad);
    b.set(local(LEASE), q!(claim));
    b.pointer(local(LEASE), bad);
    b.set(local(REG), q!(page));
    b.pointer(local(REG), bad);
    b.pointer(field(G, offset_of!(N, registry), 0), bad);
    b.branch(field(G, offset_of!(N, identity), 0), n(0), C::Equal, bad);
    b.set(local(SEG), field(G, offset_of!(N, set), 0));
    b.pointer(local(SEG), bad);
    b.branch(s!(incarnation), n(0), C::Equal, bad);
    b.branch(j!(serial), n(0), C::Equal, bad);
    for (left, right) in [
        (j!(serial), s!(active_serial)),
        (j!(offset), s!(active_offset)),
        (j!(incarnation), s!(incarnation)),
        (j!(phase), s!(epoch)),
        (field(G, offset_of!(N, epoch), 0), s!(epoch)),
    ] {
        b.branch(left, right, C::NotEqual, bad);
    }
    super::remembered::phase(&mut b, j!(phase), bad);
    b.branch(shr(j!(phase), 16), n(0), C::Equal, bad);
    b.branch(s!(start), n(0), C::Equal, bad);
    b.branch(s!(start), s!(limit), C::AboveOrEqual, bad);
    b.branch(s!(limit), n(MAX_OFFSET), C::Above, bad);
    for v in [s!(start), s!(limit)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, bad);
    }
    b.branch(j!(offset), s!(start), C::Below, bad);
    b.branch(j!(offset), s!(limit), C::AboveOrEqual, bad);
    b.branch(and(j!(offset), n(7)), n(0), C::NotEqual, bad);
    for (left, right) in [
        (p!(start), s!(start)),
        (p!(limit), s!(limit)),
        (p!(incarnation), s!(incarnation)),
        (p!(generation), n(1)),
    ] {
        b.branch(left, right, C::NotEqual, bad);
    }
    let live = b.a.create_text_label();
    b.set(local(STATE), field(REG, offset_of!(P, state), 8));
    b.branch(local(STATE), n(1), C::Equal, live);
    b.branch(local(STATE), n(5), C::Equal, live);
    b.branch(local(STATE), n(7), C::Equal, live);
    b.branch(local(STATE), n(8), C::NotEqual, bad);
    b.a.bind_text_label(live);
    b.set(local(CTX), field(REG, offset_of!(P, allocated_top), 8));
    b.branch(local(CTX), p!(start), C::Below, bad);
    b.branch(local(CTX), p!(limit), C::Above, bad);
    b.branch(and(local(CTX), n(15)), n(0), C::NotEqual, bad);
    b.pointer(p!(forward_table), bad);
    b.pointer(p!(starts), bad);
    b.branch(
        p!(starts_words),
        shr(add(sub(p!(limit), p!(start)), n(1023)), 10),
        C::Below,
        bad,
    );
    b.branch(p!(starts_words), n(u64::MAX / 8), C::Above, bad);
    b.branch(
        p!(starts),
        sub(n(u64::MAX), shl(p!(starts_words), 3)),
        C::Above,
        bad,
    );
    b.set(local(G), p!(forward_table));
    b.pointer(t!(cells), bad);
    b.branch(t!(capacity), n(0), C::Equal, bad);
    b.branch(t!(capacity), n(u32::MAX as u64), C::Above, bad);
    b.branch(
        t!(cells),
        sub(n(u64::MAX), shl(t!(capacity), 5)),
        C::Above,
        bad,
    );
    b.set(local(USED), field(G, offset_of!(T, used), 8));
    b.branch(local(USED), t!(capacity), C::Above, bad);
    b.branch(j!(offset), add(p!(start), n(32)), C::Below, status[2]);
    b.branch(j!(offset), local(CTX), C::AboveOrEqual, status[2]);
    b.set(local(PREV), n(0));
    b.set(local(NEXT), local(USED));
    let search = b.a.create_text_label();
    let found = b.a.create_text_label();
    let upper = b.a.create_text_label();
    b.a.bind_text_label(search);
    b.branch(local(PREV), local(NEXT), C::AboveOrEqual, found);
    b.set(
        local(STATE),
        add(local(PREV), shr(sub(local(NEXT), local(PREV)), 1)),
    );
    b.set(local(SEG), add(t!(cells), shl(local(STATE), 5)));
    b.branch(
        field(SEG, offset_of!(F, source_offset), 0),
        j!(offset),
        C::Above,
        upper,
    );
    b.set(local(PREV), add(local(STATE), n(1)));
    b.a.jmp_label(search);
    b.a.bind_text_label(upper);
    b.set(local(NEXT), local(STATE));
    b.a.jmp_label(search);
    b.a.bind_text_label(found);
    b.branch(local(PREV), n(0), C::Equal, status[2]);
    b.set(local(SEG), add(t!(cells), shl(sub(local(PREV), n(1)), 5)));
    b.set(local(STATE), field(SEG, offset_of!(F, source_offset), 0));
    b.branch(local(STATE), add(p!(start), n(32)), C::Below, bad);
    b.branch(local(STATE), j!(offset), C::Above, bad);
    b.branch(and(local(STATE), n(15)), n(0), C::NotEqual, bad);
    b.branch(
        field(SEG, offset_of!(F, incarnation), 0),
        p!(incarnation),
        C::NotEqual,
        bad,
    );
    let last = b.a.create_text_label();
    b.branch(local(PREV), local(USED), C::AboveOrEqual, last);
    b.set(local(NEXT), add(t!(cells), shl(local(PREV), 5)));
    b.branch(
        field(NEXT, offset_of!(F, incarnation), 0),
        p!(incarnation),
        C::NotEqual,
        bad,
    );
    b.set(local(OP), field(NEXT, offset_of!(F, source_offset), 0));
    b.branch(and(local(OP), n(15)), n(0), C::NotEqual, bad);
    b.branch(local(OP), add(local(STATE), n(32)), C::Below, bad);
    b.branch(local(OP), add(local(STATE), n(32)), C::Equal, bad);
    b.branch(local(OP), p!(limit), C::AboveOrEqual, bad);
    b.branch(
        j!(offset),
        sub(local(OP), n(32)),
        C::AboveOrEqual,
        status[2],
    );
    b.a.bind_text_label(last);
    b.set(local(STATE), shr(sub(local(STATE), p!(start)), 4));
    b.set(local(OP), and(local(STATE), n(63)));
    b.set(local(KIND), n(1));
    let mask = b.a.create_text_label();
    let mask_done = b.a.create_text_label();
    b.a.bind_text_label(mask);
    b.branch(local(OP), n(0), C::Equal, mask_done);
    b.set(local(KIND), shl(local(KIND), 1));
    b.set(local(OP), sub(local(OP), n(1)));
    b.a.jmp_label(mask);
    b.a.bind_text_label(mask_done);
    let word = E::Mem(
        Box::new(add(p!(starts), shl(shr(local(STATE), 6), 3))),
        0,
        8,
    );
    b.branch(and(word, local(KIND)), n(0), C::Equal, bad);
    b.set(q!(owner), field(SEG, offset_of!(F, source_offset), 0));
    b.set(q!(byte_offset), sub(j!(offset), q!(owner)));
    b.set(q!(cell), local(SEG));
    b.a.jmp_label(status[0]);
    for (i, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, i as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
}
