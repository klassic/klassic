//! Bounded counted-pending-reference forwarding; see gc_pending_follow.h.
use super::*;
use crate::gc_abi::AllocationPage;

fn call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, g: E, q: i32, op: u64) {
    b.value(g);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, q);
    b.a.mov_imm64(R::V2, op);
    b.a.call_label(label);
    b.a.store_rbp_slot(PREV, R::V0);
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let status: [_; 8] = std::array::from_fn(|_| a.create_text_label());
    let acquire = a.create_text_label();
    let metadata = a.create_text_label();
    let lookup = a.create_text_label();
    let forward = a.create_text_label();
    let existing = a.create_text_label();
    let ready = a.create_text_label();
    let nonnull = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for bytes in [112, 112] {
        a.sub_reg_imm8(R::V4, bytes);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    // Three disjoint stack controls: ReferenceLease, ForwardLookup, ForwardRequest.
    for (slot, offset) in [(REG, 144), (SEG, 176), (LEASE, 208)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, offset);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    for slot in [G, Q, OP] {
        b.pointer(local(slot), status[1]);
    }
    for off in [0, 8, 16] {
        b.branch(field(OP, off, 0), n(0), C::NotEqual, status[7]);
    }
    b.branch(field(Q, 0, 0), n(0), C::NotEqual, nonnull);
    for off in [8, 16, 24] {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, status[1]);
    }
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(nonnull);
    for off in [8, 16, 24] {
        b.branch(field(Q, off, 0), n(0), C::NotEqual, existing);
    }
    call(&mut b, acquire, local(G), Q, 0);
    b.branch(local(PREV), n(2), C::Equal, status[5]);
    b.branch(local(PREV), n(3), C::Equal, status[6]);
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    b.a.jmp_label(ready);
    b.a.bind_text_label(existing);
    for off in [0, 8, 16] {
        b.set(field(REG, off, 0), field(Q, off + 8, 0));
    }
    call(&mut b, metadata, local(G), REG, 2);
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    for off in [0, 8] {
        b.branch(
            field(REG, off, 0),
            field(Q, off + 8, 0),
            C::NotEqual,
            status[1],
        );
    }
    b.a.bind_text_label(ready);
    b.set(local(NEXT), field(Q, 8, 0));
    b.pointer(
        field(NEXT, offset_of!(AllocationPage, forward_table), 0),
        status[1],
    );
    b.set(field(SEG, 0, 0), shr(field(Q, 0, 0), 16));
    b.set(field(SEG, 8, 0), field(Q, 16, 0));
    b.set(field(SEG, 16, 0), n(0));
    call(
        &mut b,
        lookup,
        field(NEXT, offset_of!(AllocationPage, forward_table), 0),
        SEG,
        0,
    );
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    for off in [0, 8] {
        b.set(field(LEASE, off, 0), field(SEG, off, 0));
    }
    for off in [16, 24] {
        b.set(field(LEASE, off, 0), n(0));
    }
    call(&mut b, forward, field(SEG, 16, 0), LEASE, 1);
    b.branch(local(PREV), n(3), C::Equal, status[2]);
    for result in [2, 4] {
        b.branch(local(PREV), n(result), C::Equal, status[0]);
    }
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    b.set(field(REG, 0, 0), field(LEASE, 24, 0));
    for off in [8, 16, 24] {
        b.set(field(REG, off, 0), n(0));
    }
    call(&mut b, acquire, local(G), REG, 0);
    b.branch(local(PREV), n(2), C::Equal, status[5]);
    b.branch(local(PREV), n(3), C::Equal, status[6]);
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    for off in [0, 8, 16] {
        b.set(field(OP, off, 0), field(Q, off + 8, 0));
    }
    for off in [0, 8, 16, 24] {
        b.set(field(Q, off, 0), field(REG, off, 0));
    }
    call(&mut b, metadata, local(G), OP, 1);
    b.branch(local(PREV), n(0), C::NotEqual, status[7]);
    b.a.jmp_label(status[3]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(acquire);
    super::reference_lease::emit_reference_lease(b.a);
    b.a.ret();
    b.a.bind_text_label(metadata);
    super::metadata::emit_metadata_step(b.a);
    b.a.ret();
    b.a.bind_text_label(lookup);
    super::forward_table::emit_forward_lookup(b.a);
    b.a.ret();
    b.a.bind_text_label(forward);
    super::forward::emit_forward_step(b.a);
    b.a.ret();
}
