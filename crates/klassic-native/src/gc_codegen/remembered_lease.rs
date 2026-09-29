//! Claimed slot to Old run metadata, without a GcRef; gc_remembered_lease.h.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, HeapPool as H, MetadataLeasePool as M, MetadataLeaseRequest,
    RememberedInventoryRequest as Check, RememberedJob as J, RememberedLease as L,
    RememberedRegistration as N, RememberedSet as S,
};
#[allow(dead_code)]
pub(crate) fn emit_lease<A: PortableAsm>(a: &mut A) {
    let bad = a.create_text_label();
    let done = a.create_text_label();
    let inventory = a.create_text_label();
    let metadata = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 64);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    macro_rules! q {
        ($f:ident) => {
            field(Q, offset_of!(L, $f), 0)
        };
    }
    macro_rules! s {
        ($f:ident) => {
            field(SEG, offset_of!(S, $f), 0)
        };
    }
    macro_rules! j {
        ($f:ident) => {
            field(NEXT, offset_of!(J, $f), 0)
        };
    }
    macro_rules! h {
        ($f:ident) => {
            field(CTX, offset_of!(H, $f), 0)
        };
    }
    macro_rules! p {
        ($f:ident) => {
            field(REG, offset_of!(P, $f), 0)
        };
    }
    b.pointer(local(G), bad);
    b.pointer(local(Q), bad);
    for off in (0..std::mem::size_of::<MetadataLeaseRequest>()).step_by(8) {
        b.branch(
            field(Q, offset_of!(L, lease) + off, 0),
            n(0),
            C::NotEqual,
            bad,
        );
    }
    b.set(local(REG), q!(registration));
    b.pointer(local(REG), bad);
    b.set(local(NEXT), q!(claim));
    b.pointer(local(NEXT), bad);
    for (slot, displacement) in [(LEASE, 128), (PREV, 160)] {
        b.a.mov_reg_reg(R::V0, R::V5);
        b.a.sub_reg_imm32(R::V0, displacement);
        b.a.store_rbp_slot(slot, R::V0);
    }
    b.set(field(PREV, offset_of!(Check, registration), 0), local(REG));
    b.set(field(PREV, offset_of!(Check, job), 0), local(NEXT));
    b.set(field(PREV, offset_of!(Check, phase), 0), n(0));
    b.value(field(REG, offset_of!(N, registry), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, PREV);
    b.a.mov_imm64(R::V2, 8);
    b.a.call_label(inventory);
    b.a.store_rbp_slot(KIND, R::V0);
    b.branch(local(KIND), n(0), C::NotEqual, bad);
    b.set(local(SEG), field(REG, offset_of!(N, set), 0));
    b.branch(j!(serial), n(0), C::Equal, bad);
    for (job, set) in [
        (offset_of!(J, serial), offset_of!(S, active_serial)),
        (offset_of!(J, offset), offset_of!(S, active_offset)),
        (offset_of!(J, phase), offset_of!(S, epoch)),
        (offset_of!(J, incarnation), offset_of!(S, incarnation)),
    ] {
        b.branch(field(NEXT, job, 0), field(SEG, set, 0), C::NotEqual, bad);
    }
    b.set(local(CTX), field(G, offset_of!(M, heap), 0));
    b.pointer(local(CTX), bad);
    b.pointer(h!(pages), bad);
    b.branch(h!(page_count), n(0), C::Equal, bad);
    b.branch(
        h!(page_count),
        n(crate::gc_abi::MAX_OFFSET >> 17),
        C::AboveOrEqual,
        bad,
    );
    const {
        assert!(std::mem::size_of::<P>() == 112);
    }
    let bytes = |v: E| add(add(shl(v.clone(), 6), shl(v.clone(), 5)), shl(v, 4));
    b.branch(
        add(h!(pages), bytes(h!(page_count))),
        h!(pages),
        C::Below,
        bad,
    );
    b.branch(and(h!(heap_base), n(15)), n(0), C::NotEqual, bad);
    b.set(local(USED), shl(add(h!(page_count), n(1)), 17));
    b.branch(
        add(h!(heap_base), local(USED)),
        h!(heap_base),
        C::Below,
        bad,
    );
    for field in [s!(start), s!(limit)] {
        b.branch(and(field, n((1 << 17) - 1)), n(0), C::NotEqual, bad);
    }
    b.branch(s!(limit), local(USED), C::Above, bad);
    b.set(local(STATE), shr(s!(start), 17));
    b.branch(local(STATE), n(0), C::Equal, bad);
    b.branch(local(STATE), h!(page_count), C::Above, bad);
    b.set(local(STATE), sub(local(STATE), n(1)));
    b.set(local(REG), add(h!(pages), bytes(local(STATE))));
    b.branch(p!(run_head), local(STATE), C::NotEqual, bad);
    b.branch(p!(heap_base), h!(heap_base), C::NotEqual, bad);
    b.branch(p!(start), s!(start), C::NotEqual, bad);
    b.branch(p!(limit), s!(limit), C::NotEqual, bad);
    b.branch(p!(generation), n(1), C::NotEqual, bad);
    b.branch(p!(incarnation), s!(incarnation), C::NotEqual, bad);
    b.branch(p!(identity_base), n(0), C::Equal, bad);
    b.set(
        field(LEASE, offset_of!(MetadataLeaseRequest, page), 0),
        local(REG),
    );
    b.set(
        field(LEASE, offset_of!(MetadataLeaseRequest, incarnation), 0),
        s!(incarnation),
    );
    b.set(
        field(LEASE, offset_of!(MetadataLeaseRequest, token), 0),
        n(0),
    );
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(metadata);
    b.a.store_rbp_slot(KIND, R::V0);
    b.branch(local(KIND), n(0), C::NotEqual, done);
    for off in (0..std::mem::size_of::<MetadataLeaseRequest>()).step_by(8) {
        b.set(
            field(Q, offset_of!(L, lease) + off, 0),
            field(LEASE, off, 0),
        );
    }
    b.a.jmp_label(done);
    b.a.bind_text_label(bad);
    b.set(local(KIND), n(1));
    b.a.bind_text_label(done);
    b.a.load_rbp_slot(R::V0, KIND);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(inventory);
    super::remembered_inventory::emit_inventory(b.a);
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
