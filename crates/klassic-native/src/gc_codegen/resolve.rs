//! One bounded forwarding/access step; gc_resolve.h owns the handoff contract.
use super::*;
use crate::gc_abi::{
    AllocationPage as P, MutatorContext as M, ResolveRequest as Qr, SourceAccessRequest as S,
};
macro_rules! page {
    ($f:ident) => {
        field(G, offset_of!(P, $f), 0)
    };
}
macro_rules! req {
    ($f:ident) => {
        field(Q, offset_of!(Qr, $f), 0)
    };
}
macro_rules! scope {
    ($f:ident) => {
        field(Q, offset_of!(Qr, scope) + offset_of!(S, $f), 0)
    };
}
fn call<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, g: E, q: i32, op: E) {
    b.value(g);
    b.a.mov_reg_reg(R::V7, R::V0);
    b.value(op);
    b.a.mov_reg_reg(R::V2, R::V0);
    b.a.load_rbp_slot(R::V6, q);
    b.a.call_label(label);
}
#[allow(dead_code)]
pub(crate) fn emit_resolve_access<A: PortableAsm>(a: &mut A) {
    const {
        assert!(std::mem::size_of::<S>() == 40);
        assert!(std::mem::size_of::<crate::gc_abi::ForwardRequest>() == 32);
        assert!(std::mem::size_of::<crate::gc_abi::ForwardLookup>() == 24);
    }
    let status: [_; 5] = std::array::from_fn(|_| a.create_text_label());
    let invalid = status[1];
    let lookup = a.create_text_label();
    let forward = a.create_text_label();
    let access = a.create_text_label();
    let entry = a.create_text_label();
    let guard = a.create_text_label();
    let resolved = a.create_text_label();
    let copy = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    // Three independent stack temporaries: lookup[24], forwarding[32], scope[40].
    for (slot, offset) in [(REG, 144), (SEG, 176), (LEASE, 216)] {
        a.mov_reg_reg(R::V0, R::V5);
        a.sub_reg_imm32(R::V0, offset);
        a.store_rbp_slot(slot, R::V0);
    }
    let mut b = Emit { a };
    b.branch(local(OP), n(1), C::Above, invalid);
    b.branch(req!(destination), n(0), C::NotEqual, invalid);
    b.branch(scope!(ticket), n(0), C::NotEqual, invalid);
    b.set(local(CTX), scope!(context));
    b.pointer(local(CTX), invalid);
    b.branch(
        field(CTX, offset_of!(M, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(M, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(M, state), 4),
        n(0),
        C::NotEqual,
        invalid,
    );
    let key = b.a.create_text_label();
    b.branch(local(OP), n(0), C::NotEqual, key);
    for off in [
        offset_of!(S, source_offset),
        offset_of!(S, incarnation),
        offset_of!(S, held),
    ] {
        b.branch(
            field(Q, offset_of!(Qr, scope) + off, 0),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.branch(
        field(CTX, offset_of!(M, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.a.bind_text_label(key);
    b.set(local(STATE), shr(req!(reference), 16));
    b.branch(local(STATE), n(0), C::Equal, invalid);
    b.branch(
        and(req!(reference), n(0xf0000 | 0xcf)),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.branch(and(req!(reference), n(0x30)), n(0), C::Equal, invalid);
    for mask in [0x300, 0xc00, 0xf000] {
        b.set(local(PREV), and(req!(reference), n(mask)));
        b.branch(local(PREV), n(0), C::Equal, invalid);
        b.branch(
            and(local(PREV), sub(local(PREV), n(1))),
            n(0),
            C::NotEqual,
            invalid,
        );
    }
    b.set(local(PREV), field(G, offset_of!(P, state), 8));
    b.set(local(NEXT), field(G, offset_of!(P, allocated_top), 8));
    let live = b.a.create_text_label();
    b.branch(local(PREV), n(1), C::Equal, live);
    b.branch(local(PREV), n(5), C::Equal, live);
    b.branch(local(PREV), n(7), C::Equal, live);
    b.branch(local(PREV), n(8), C::NotEqual, invalid);
    b.a.bind_text_label(live);
    let enter = b.a.create_text_label();
    b.branch(local(OP), n(0), C::Equal, enter);
    b.branch(local(PREV), n(7), C::AboveOrEqual, invalid);
    b.a.bind_text_label(enter);
    b.branch(page!(incarnation), n(0), C::Equal, invalid);
    b.branch(page!(start), n(0), C::Equal, invalid);
    b.branch(page!(start), page!(limit), C::AboveOrEqual, invalid);
    b.branch(
        page!(limit),
        n(crate::gc_abi::MAX_OFFSET),
        C::Above,
        invalid,
    );
    for v in [page!(start), page!(limit), local(NEXT)] {
        b.branch(and(v, n(15)), n(0), C::NotEqual, invalid);
    }
    b.branch(local(NEXT), page!(start), C::Below, invalid);
    b.branch(local(NEXT), page!(limit), C::Above, invalid);
    b.branch(local(STATE), add(page!(start), n(32)), C::Below, invalid);
    b.branch(local(STATE), local(NEXT), C::AboveOrEqual, invalid);
    b.pointer(page!(forward_table), invalid);
    b.set(field(REG, 0, 0), local(STATE));
    b.set(field(REG, 8, 0), page!(incarnation));
    b.set(field(REG, 16, 0), n(0));
    call(&mut b, lookup, page!(forward_table), REG, n(0));
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, invalid);
    b.set(local(USED), field(REG, 16, 0));
    for off in (0..40).step_by(8) {
        b.set(
            field(LEASE, off, 0),
            field(Q, offset_of!(Qr, scope) + off, 0),
        );
    }
    b.branch(local(OP), n(0), C::Equal, entry);
    b.branch(scope!(source_offset), local(STATE), C::NotEqual, invalid);
    b.branch(
        scope!(incarnation),
        page!(incarnation),
        C::NotEqual,
        invalid,
    );
    b.branch(scope!(held), local(USED), C::NotEqual, invalid);
    b.a.jmp_label(guard);
    b.a.bind_text_label(entry);
    b.set(field(SEG, 0, 0), local(STATE));
    b.set(field(SEG, 8, 0), page!(incarnation));
    b.set(field(SEG, 16, 0), n(0));
    b.set(field(SEG, 24, 0), n(0));
    call(&mut b, forward, local(USED), SEG, n(1));
    b.a.store_rbp_slot(PREV, R::V0);
    b.branch(local(PREV), n(0), C::Equal, resolved);
    b.branch(
        field(G, offset_of!(P, state), 8),
        n(7),
        C::AboveOrEqual,
        invalid,
    );
    b.branch(local(PREV), n(3), C::Equal, status[2]);
    let source = b.a.create_text_label();
    b.branch(local(PREV), n(2), C::Equal, source);
    b.branch(local(PREV), n(4), C::NotEqual, invalid);
    b.a.bind_text_label(source);
    b.set(field(LEASE, offset_of!(S, source_offset), 0), local(STATE));
    b.set(
        field(LEASE, offset_of!(S, incarnation), 0),
        page!(incarnation),
    );
    b.a.bind_text_label(guard);
    call(&mut b, access, local(USED), LEASE, local(OP));
    b.a.store_rbp_slot(PREV, R::V0);
    b.branch(local(PREV), n(3), C::Equal, status[4]);
    b.branch(local(PREV), n(2), C::Equal, status[2]);
    b.branch(local(PREV), n(0), C::NotEqual, invalid);
    b.branch(local(OP), n(0), C::Equal, copy);
    b.set(field(LEASE, offset_of!(S, source_offset), 0), n(0));
    b.set(field(LEASE, offset_of!(S, incarnation), 0), n(0));
    b.a.bind_text_label(copy);
    for off in (0..40).step_by(8) {
        b.set(
            field(Q, offset_of!(Qr, scope) + off, 0),
            field(LEASE, off, 0),
        );
    }
    b.a.jmp_label(status[0]);
    b.a.bind_text_label(resolved);
    b.set(req!(destination), field(SEG, 24, 0));
    b.a.jmp_label(status[3]);
    for (value, label) in status.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, value as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(lookup);
    super::forward_table::emit_forward_lookup(b.a);
    b.a.ret();
    b.a.bind_text_label(forward);
    super::forward::emit_forward_step(b.a);
    b.a.ret();
    b.a.bind_text_label(access);
    super::source_access::emit_source_access(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
