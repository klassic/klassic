//! Bounded root-to-root forwarding handoff; see gc_root_follow.h.
use super::*;
use crate::gc_abi::{AllocationPage, RootFollow, RootRequest};

fn call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, g: E, q: i32, op: u64) {
    b.value(g);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.mov_imm64(R::V2, op);
    b.a.load_rbp_slot(R::V6, q);
    b.a.call_label(label);
    b.a.store_rbp_slot(PREV, R::V0);
}

#[allow(dead_code)]
pub(crate) fn emit_root_follow<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<RootFollow>() == 72);
        assert!(std::mem::size_of::<RootRequest>() == 40);
        assert!(std::mem::size_of::<crate::gc_abi::ReferenceLease>() == 32);
        assert!(std::mem::size_of::<crate::gc_abi::ForwardLookup>() == 24);
        assert!(std::mem::size_of::<crate::gc_abi::ForwardRequest>() == 32);
    }
    let status: [_; 8] = std::array::from_fn(|_| a.create_text_label());
    let root = a.create_text_label();
    let acquire = a.create_text_label();
    let metadata = a.create_text_label();
    let lookup = a.create_text_label();
    let forward = a.create_text_label();
    let release = a.create_text_label();
    let adopted = a.create_text_label();
    let wait = a.create_text_label();
    let terminal = a.create_text_label();
    let cleanup = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    for bytes in [112, 112, 16] {
        a.sub_reg_imm8(R::V4, bytes);
    }
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    for (slot, offset) in [(REG, 152), (SEG, 184), (LEASE, 208), (USED, 240)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, offset);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    b.branch(local(OP), n(1), C::Above, status[1]);
    b.set(
        local(KIND),
        add(local(Q), n(offset_of!(RootFollow, cleanup) as u64)),
    );
    b.set(local(STATE), n(0));
    b.branch(local(OP), n(1), C::Equal, cleanup);
    for off in [0, 8, 16] {
        b.branch(field(KIND, off, 0), n(0), C::NotEqual, status[7]);
    }
    b.set(local(CTX), field(Q, offset_of!(RootFollow, context), 0));
    b.pointer(local(CTX), status[1]);
    b.branch(
        field(
            Q,
            offset_of!(RootFollow, root) + offset_of!(RootRequest, value),
            0,
        ),
        n(0),
        C::NotEqual,
        status[1],
    );
    for off in (0..40).step_by(8) {
        b.set(
            field(REG, off, 0),
            field(Q, offset_of!(RootFollow, root) + off, 0),
        );
    }
    call(&mut b, root, local(CTX), REG, 3);
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    b.branch(field(REG, 32, 0), n(0), C::Equal, status[0]);
    b.set(field(SEG, 0, 0), field(REG, 32, 0));
    for off in [8, 16, 24] {
        b.set(field(SEG, off, 0), n(0));
    }
    call(&mut b, acquire, local(G), SEG, 0);
    b.branch(local(PREV), n(2), C::Equal, status[5]);
    b.branch(local(PREV), n(3), C::Equal, status[6]);
    b.branch(local(PREV), n(0), C::NotEqual, status[1]);
    for off in [0, 8, 16] {
        b.set(field(KIND, off, 0), field(SEG, off + 8, 0));
    }
    b.set(local(NEXT), field(SEG, 8, 0));
    b.set(local(STATE), n(1));
    b.pointer(
        field(NEXT, offset_of!(AllocationPage, forward_table), 0),
        release,
    );
    b.set(field(LEASE, 0, 0), shr(field(REG, 32, 0), 16));
    b.set(field(LEASE, 8, 0), field(SEG, 16, 0));
    b.set(field(LEASE, 16, 0), n(0));
    call(
        &mut b,
        lookup,
        field(NEXT, offset_of!(AllocationPage, forward_table), 0),
        LEASE,
        0,
    );
    b.branch(local(PREV), n(0), C::NotEqual, release);
    for off in [0, 8] {
        b.set(field(USED, off, 0), field(LEASE, off, 0));
    }
    for off in [16, 24] {
        b.set(field(USED, off, 0), n(0));
    }
    call(&mut b, forward, field(LEASE, 16, 0), USED, 1);
    b.branch(local(PREV), n(3), C::Equal, wait);
    b.branch(local(PREV), n(2), C::Equal, terminal);
    b.branch(local(PREV), n(4), C::Equal, terminal);
    b.branch(local(PREV), n(0), C::NotEqual, release);
    b.set(field(REG, 32, 0), field(USED, 24, 0));
    call(&mut b, root, local(CTX), REG, 1);
    b.branch(local(PREV), n(0), C::Equal, adopted);
    let flush = b.a.create_text_label();
    let exhausted = b.a.create_text_label();
    b.branch(local(PREV), n(3), C::Equal, flush);
    b.branch(local(PREV), n(4), C::Equal, exhausted);
    b.a.jmp_label(release);
    for (label, result) in [
        (wait, 2),
        (terminal, 0),
        (adopted, 3),
        (flush, 4),
        (exhausted, 6),
    ] {
        b.a.bind_text_label(label);
        b.set(local(STATE), n(result));
        b.a.jmp_label(release);
    }
    b.a.bind_text_label(release);
    b.a.bind_text_label(cleanup);
    call(&mut b, metadata, local(G), KIND, 1);
    b.branch(local(PREV), n(0), C::NotEqual, status[7]);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.jmp_label(done);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(root);
    crate::gc_codegen::roots::emit_step(b.a);
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
    b.a.bind_text_label(after);
}
