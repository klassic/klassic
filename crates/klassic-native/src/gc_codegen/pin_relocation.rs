//! Selected-page authentication before a marked object claims relocation.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, MarkQueue, MarkedRelocation, MetadataLeaseRequest as L, ObjectHeader,
    PinRelocationClaim as Request, RelocationClaim,
};
use std::mem::size_of;
fn request(off: usize) -> E {
    field(Q, off, 0)
}
fn claim(off: usize) -> E {
    field(REG, offset_of!(MarkedRelocation, claim) + off, 0)
}
#[allow(dead_code)]
pub(crate) fn emit_claim<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let closed = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let selected = a.create_text_label();
    let marked = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    let mut b = Emit { a };
    b.pointer(local(Q), invalid);
    b.set(local(REG), request(offset_of!(Request, work)));
    b.pointer(local(REG), invalid);
    b.set(
        local(SEG),
        field(REG, offset_of!(MarkedRelocation, queue), 0),
    );
    b.pointer(local(SEG), invalid);
    b.set(local(LEASE), request(offset_of!(Request, lease)));
    b.set(local(CTX), request(offset_of!(Request, selection)));
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.a.load_rbp_slot(R::V2, CTX);
    b.a.call_label(selected);
    b.a.cmp_reg_imm32(R::V0, 2);
    b.a.jcc_label(C::Equal, closed);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.set(local(STATE), field(LEASE, offset_of!(L, page), 0));
    b.branch(
        field(STATE, offset_of!(P, state), 8),
        n(crate::gc_abi::PAGE_CLOSED),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(STATE, offset_of!(P, generation), 0),
        n(1),
        C::Above,
        invalid,
    );
    b.branch(
        field(SEG, offset_of!(MarkQueue, generation), 0),
        field(STATE, offset_of!(P, generation), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        claim(offset_of!(RelocationClaim, incarnation)),
        field(LEASE, offset_of!(L, incarnation), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(STATE, offset_of!(P, start), 0),
        n(u64::MAX - size_of::<ObjectHeader>() as u64),
        C::Above,
        invalid,
    );
    b.branch(
        claim(offset_of!(RelocationClaim, source_offset)),
        add(
            field(STATE, offset_of!(P, start), 0),
            n(size_of::<ObjectHeader>() as u64),
        ),
        C::Below,
        invalid,
    );
    b.branch(
        claim(offset_of!(RelocationClaim, source_offset)),
        field(STATE, offset_of!(P, allocated_top), 8),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(
        claim(offset_of!(RelocationClaim, source_offset)),
        field(STATE, offset_of!(P, limit), 0),
        C::AboveOrEqual,
        invalid,
    );
    b.value(request(offset_of!(Request, movers)));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, REG);
    b.a.call_label(marked);
    b.a.jmp_label(done);
    for (label, status) in [(invalid, 1), (closed, 6)] {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(selected);
    super::pin_registry::emit_selection(b.a);
    b.a.ret();
    b.a.bind_text_label(marked);
    super::relocation_claim::emit_marked_relocation_claim(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
