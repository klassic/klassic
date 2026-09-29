//! Native heap-mutex dispatch for rooted pin acquisition and sticky cancellation.
use super::*;
use crate::gc_abi::{
    MetadataLeaseRequest, PinRuntime as Runtime, RootAccess, RootFollow, RootPin as Pin,
};
fn owner(off: usize) -> E {
    field(Q, off, 0)
}
fn step<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel) {
    b.a.load_rbp_slot(R::V7, Q);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
}
fn native<A: PortableAsm>(b: &mut Emit<'_, A>, off: usize) {
    b.value(field(G, off, 0));
    b.a.mov_reg_reg(R::V10, R::V0);
    b.value(field(G, offset_of!(Runtime, heap_lock), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.call_native_unary(R::V10);
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let ret: [_; 12] = std::array::from_fn(|_| a.create_text_label());
    let dynamic = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let exit = a.create_text_label();
    let finish = a.create_text_label();
    let locked = a.create_text_label();
    let cancel = a.create_text_label();
    let cleanup = a.create_text_label();
    let unlock = a.create_text_label();
    let owner_steps: [_; 5] = std::array::from_fn(|_| a.create_text_label());
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(OP, R::V2);
    let mut b = Emit { a };
    let invalid = ret[4];
    b.pointer(local(G), invalid);
    b.pointer(local(Q), invalid);
    b.branch(
        owner(offset_of!(Pin, self_)),
        local(Q),
        C::NotEqual,
        invalid,
    );
    b.pointer(owner(offset_of!(Pin, registry)), invalid);
    b.branch(local(OP), n(1), C::Above, invalid);
    b.branch(
        owner(offset_of!(Pin, registry)),
        field(G, offset_of!(Runtime, pages), 0),
        C::NotEqual,
        invalid,
    );
    for off in [
        offset_of!(Runtime, heap_lock),
        offset_of!(Runtime, lock),
        offset_of!(Runtime, unlock),
    ] {
        b.branch(field(G, off, 0), n(0), C::Equal, invalid);
    }
    let stage = owner(offset_of!(Pin, stage));
    let cancelled = owner(offset_of!(Pin, cancel));
    b.branch(stage.clone(), n(1), C::Below, invalid);
    b.branch(stage.clone(), n(6), C::Above, invalid);
    b.branch(cancelled.clone(), n(1), C::Above, invalid);
    let validated = b.a.create_text_label();
    b.branch(local(OP), n(0), C::Equal, validated);
    b.set(cancelled.clone(), n(1));
    b.a.bind_text_label(validated);
    b.branch(stage.clone(), n(6), C::Equal, ret[8]);
    b.branch(stage.clone(), n(2), C::Equal, exit);
    const FOLLOW: usize = offset_of!(Pin, root) + offset_of!(RootAccess, follow);
    b.set(local(CTX), owner(FOLLOW + offset_of!(RootFollow, context)));
    b.pointer(local(CTX), invalid);
    b.branch(
        field(CTX, offset_of!(MutatorContext, abi_version), 4),
        n(crate::gc_abi::ABI_VERSION as u64),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, thread_id), 0),
        n(0),
        C::Equal,
        invalid,
    );
    b.branch(
        field(CTX, offset_of!(MutatorContext, access_epoch), 8),
        n(0),
        C::NotEqual,
        invalid,
    );
    b.set(
        local(USED),
        field(CTX, offset_of!(MutatorContext, state), 4),
    );
    let valid_state = b.a.create_text_label();
    b.branch(local(USED), n(0), C::Equal, valid_state);
    b.branch(local(USED), n(2), C::NotEqual, invalid);
    b.a.bind_text_label(valid_state);
    let not_ready = b.a.create_text_label();
    b.branch(stage.clone(), n(3), C::NotEqual, not_ready);
    b.branch(cancelled.clone(), n(0), C::Equal, ret[1]);
    b.a.bind_text_label(not_ready);
    b.branch(stage.clone(), n(5), C::Equal, finish);
    let not_resume = b.a.create_text_label();
    b.branch(cancelled.clone(), n(0), C::NotEqual, not_resume);
    b.branch(stage.clone(), n(1), C::NotEqual, not_resume);
    b.branch(local(USED), n(2), C::Equal, ret[11]);
    b.a.bind_text_label(not_resume);
    b.branch(local(USED), n(0), C::Equal, locked);
    b.branch(cancelled.clone(), n(0), C::Equal, invalid);
    b.a.bind_text_label(locked);
    native(&mut b, offset_of!(Runtime, lock));
    b.branch(cancelled.clone(), n(0), C::NotEqual, cancel);
    b.branch(stage.clone(), n(4), C::Equal, cancel);
    let enter = b.a.create_text_label();
    b.branch(stage, n(1), C::Equal, enter);
    b.set(local(STATE), n(4));
    b.a.jmp_label(unlock);
    b.a.bind_text_label(enter);
    b.branch(
        owner(FOLLOW + offset_of!(RootFollow, cleanup) + offset_of!(MetadataLeaseRequest, token)),
        n(0),
        C::NotEqual,
        cleanup,
    );
    step(&mut b, owner_steps[0]);
    b.a.jmp_label(unlock);
    b.a.bind_text_label(cleanup);
    step(&mut b, owner_steps[2]);
    b.a.jmp_label(unlock);
    b.a.bind_text_label(cancel);
    step(&mut b, owner_steps[3]);
    b.a.jmp_label(unlock);
    b.a.bind_text_label(unlock);
    native(&mut b, offset_of!(Runtime, unlock));
    b.a.jmp_label(dynamic);
    b.a.bind_text_label(exit);
    step(&mut b, owner_steps[1]);
    b.branch(cancelled, n(0), C::Equal, dynamic);
    b.branch(local(STATE), n(1), C::Equal, ret[0]);
    b.a.jmp_label(dynamic);
    b.a.bind_text_label(finish);
    b.branch(local(USED), n(2), C::Equal, ret[11]);
    step(&mut b, owner_steps[4]);
    b.a.jmp_label(dynamic);
    for (status, label) in ret.into_iter().enumerate() {
        b.a.bind_text_label(label);
        b.a.mov_imm64(R::V0, status as u64);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(dynamic);
    b.a.load_rbp_slot(R::V0, STATE);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    for (op, label) in owner_steps.into_iter().enumerate() {
        b.a.bind_text_label(label);
        super::root_pin::emit_step(b.a, op as u64);
        b.a.ret();
    }
    b.a.bind_text_label(after);
}
