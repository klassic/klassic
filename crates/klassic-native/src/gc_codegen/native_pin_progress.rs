//! Bounded pin shutdown with worker polling and native boundary adoption.
use super::*;
use crate::gc_abi::{
    MetadataCharge, NativePin as Pin, NativePinRuntime as Runtime, RootPin, ThreadRegistration,
    WorkerRequest,
};
fn owner(off: usize) -> E {
    field(Q, off, 0)
}
fn runtime(off: usize) -> E {
    field(G, off, 0)
}
fn close<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel) {
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, Q);
    b.a.call_label(label);
}
fn worker<A: PortableAsm>(b: &mut Emit<'_, A>, label: A::TextLabel, op: E) {
    b.set(local(OP), op);
    super::native_pin_close::callback(b, false, false);
    b.value(runtime(offset_of!(Runtime, registry)));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.load_rbp_slot(R::V6, LEASE);
    b.a.load_rbp_slot(R::V2, OP);
    b.a.call_label(label);
    b.a.store_rbp_slot(STATE, R::V0);
    super::native_pin_close::callback(b, false, true);
    b.a.load_rbp_slot(R::V0, STATE);
}
#[allow(dead_code)]
pub(crate) fn emit<A: PortableAsm>(a: &mut A) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let after = a.create_text_label();
    let direct = a.create_text_label();
    let context = a.create_text_label();
    let action = a.create_text_label();
    let enter = a.create_text_label();
    let boundary = a.create_text_label();
    let mapped = a.create_text_label();
    let more = a.create_text_label();
    let close_entry = a.create_text_label();
    let worker_entry = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(LEASE, R::V2);
    let mut b = Emit { a };
    super::native_pin_close::validate_runtime(&mut b, invalid);
    b.branch(owner(offset_of!(Pin, stage)), n(6), C::Equal, direct);
    b.pointer(owner(offset_of!(Pin, charge)), invalid);
    b.set(local(NEXT), owner(offset_of!(Pin, charge)));
    b.branch(
        field(NEXT, offset_of!(MetadataCharge, owner), 0),
        runtime(offset_of!(Runtime, heap)),
        C::NotEqual,
        invalid,
    );
    b.pointer(owner(offset_of!(Pin, context)), invalid);
    b.set(local(CTX), owner(offset_of!(Pin, context)));
    b.pointer(local(LEASE), invalid);
    b.pointer(
        field(LEASE, offset_of!(WorkerRequest, handshake), 0),
        invalid,
    );
    b.pointer(
        field(LEASE, offset_of!(WorkerRequest, participant), 0),
        invalid,
    );
    b.set(
        local(NEXT),
        field(LEASE, offset_of!(WorkerRequest, participant), 0),
    );
    b.branch(
        field(NEXT, offset_of!(ThreadRegistration, context), 0),
        local(CTX),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(NEXT, offset_of!(ThreadRegistration, registry), 0),
        field(LEASE, offset_of!(WorkerRequest, handshake), 0),
        C::NotEqual,
        invalid,
    );
    b.branch(
        field(NEXT, offset_of!(ThreadRegistration, registered), 0),
        n(1),
        C::NotEqual,
        invalid,
    );
    b.branch(owner(offset_of!(Pin, stage)), n(3), C::NotEqual, context);
    b.branch(
        owner(offset_of!(Pin, pin) + offset_of!(RootPin, stage)),
        n(2),
        C::Equal,
        direct,
    );
    b.a.bind_text_label(context);
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
    b.branch(local(USED), n(2), C::Above, invalid);
    b.branch(local(USED), n(2), C::Equal, action);
    worker(&mut b, worker_entry, n(0));
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::NotEqual, mapped);
    b.a.bind_text_label(action);
    close(&mut b, close_entry);
    b.a.cmp_reg_imm32(R::V0, 12);
    b.a.jcc_label(C::Equal, enter);
    b.a.cmp_reg_imm32(R::V0, 11);
    b.a.jcc_label(C::NotEqual, done);
    b.set(local(OP), n(2));
    b.a.jmp_label(boundary);
    b.a.bind_text_label(enter);
    b.set(local(OP), n(1));
    b.a.bind_text_label(boundary);
    worker(&mut b, worker_entry, local(OP));
    b.a.bind_text_label(mapped);
    b.a.test_reg_reg(R::V0, R::V0);
    b.a.jcc_label(C::Equal, more);
    b.a.cmp_reg_imm32(R::V0, 3);
    b.a.jcc_label(C::Equal, invalid);
    b.a.cmp_reg_imm32(R::V0, 7);
    b.a.jcc_label(C::Above, invalid);
    b.a.add_reg_imm32(R::V0, 16);
    b.a.jmp_label(done);
    b.a.bind_text_label(direct);
    close(&mut b, close_entry);
    b.a.jmp_label(done);
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 4);
    b.a.jmp_label(done);
    b.a.bind_text_label(more);
    b.a.mov_imm64(R::V0, 0);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.jmp_label(after);
    b.a.bind_text_label(close_entry);
    super::native_pin_close::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(worker_entry);
    super::worker::emit_worker_step(b.a);
    b.a.ret();
    b.a.bind_text_label(after);
}
