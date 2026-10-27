//! Native unary ABI over the actual scoped-pin owner. Caller-owned requests
//! and scopes survive pending cleanup; native callbacks never bypass release.
use super::*;
use crate::gc_abi::{
    ScopedPinApi as Api, ScopedPinCall as Call, ScopedPinScope as Scope, ScopedPinToken as Token,
};

pub(super) const API: usize = offset_of!(Runtime, pin_api);
const ADDRESS: usize = offset_of!(Call, address);
const IDENTITY: usize = offset_of!(Call, identity);
const OP: usize = offset_of!(Call, operation);
const TOKEN: usize = offset_of!(Call, token);
const STAGE: usize = offset_of!(Scope, stage);
const OWN_TOKEN: usize = offset_of!(Scope, token);

pub(super) fn startup<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for (off, value) in [
        (
            offset_of!(Api, version),
            crate::gc_abi::SCOPED_PIN_ABI_VERSION,
        ),
        (offset_of!(Api, token_bytes), size_of::<Token>() as u64),
        (offset_of!(Api, call_bytes), size_of::<Call>() as u64),
        (offset_of!(Api, scope_bytes), size_of::<Scope>() as u64),
    ] {
        store_rt_imm(a, API + off, value);
    }
    for (off, label) in [
        (offset_of!(Api, call), l.scoped_pin_abi_call),
        (offset_of!(Api, scope), l.scoped_pin_abi_scope),
    ] {
        a.mov_text_addr(R::V0, label);
        store_rt(a, API + off, R::V0);
    }
}
fn pointer<E: PortableAsm>(a: &mut E, r: R, bad: E::TextLabel) {
    a.test_reg_reg(r, r);
    a.jcc_label(C::Equal, bad);
    a.mov_reg_reg(R::V0, r);
    a.and_reg_imm32(R::V0, 7);
    a.jcc_label(C::NotEqual, bad);
}
fn request<E: PortableAsm>(a: &mut E) {
    a.load_rbp_slot(R::V9, 8);
}
fn load<E: PortableAsm>(a: &mut E, r: R, off: usize) {
    request(a);
    a.load_ptr_disp32(r, R::V9, d(off));
}
fn store<E: PortableAsm>(a: &mut E, off: usize, r: R) {
    request(a);
    a.store_ptr_disp32(R::V9, d(off), r);
}
fn set<E: PortableAsm>(a: &mut E, off: usize, n: u64) {
    a.mov_imm64(R::V0, n);
    store(a, off, R::V0);
}
fn context<E: PortableAsm>(a: &mut E, bad: E::TextLabel) {
    load(a, R::OwnerContext, offset_of!(Call, context));
    pointer(a, R::OwnerContext, bad);
    context_word(a, offset_of!(MutatorContext, abi_version));
    a.cmp_reg_imm32(R::V0, crate::gc_abi::ABI_VERSION as i32);
    a.jcc_label(C::NotEqual, bad);
    a.load_ptr_disp32(
        R::V3,
        R::OwnerContext,
        d(offset_of!(OwnerState, shared_runtime)),
    );
}
fn context_word<E: PortableAsm>(a: &mut E, off: usize) {
    a.mov_reg_reg(R::V1, R::OwnerContext);
    a.add_reg_imm32(R::V1, d(off));
    a.atomic_load_acquire_u32(R::V0, R::V1);
}
fn source_cell<E: PortableAsm>(a: &mut E, bad: E::TextLabel) {
    // Authenticate the borrowed cell's actual creator-frame range before
    // entering the trusted root actor. This never dereferences an out-of-range
    // source or substitutes a native backing cell for the creator's frame.
    load(a, R::V0, offset_of!(Call, source));
    load_rt(a, R::V8, RT_SEGMENT);
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(RootSegment, cells)));
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, bad);
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, 31);
    a.jcc_label(C::NotEqual, bad);
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(RootSegment, used)));
    a.shl_reg_imm8(R::V1, 5);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, bad);
}
fn emit_call<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.scoped_pin_abi_call);
    a.native_entry_begin(16);
    let invalid_input = a.create_text_label();
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    pointer(a, R::V7, invalid_input);
    a.store_rbp_slot(8, R::V7);
    set(a, ADDRESS, 0);
    set(a, IDENTITY, 0);
    context(a, invalid);
    load(a, R::V7, TOKEN);
    pointer(a, R::V7, invalid);
    load(a, R::V6, OP);
    a.cmp_reg_imm8(R::V6, 8);
    a.jcc_label(C::Above, invalid);
    context_word(a, offset_of!(MutatorContext, state));
    let native = a.create_text_label();
    let checked = a.create_text_label();
    for op in [4, 5] {
        a.cmp_reg_imm8(R::V6, op);
        a.jcc_label(C::Equal, native);
    }
    a.test_reg_reg(R::V0, R::V0); // Actual Running, never a forged adoption.
    a.jcc_label(C::NotEqual, invalid);
    a.jmp_label(checked);
    a.bind_text_label(native);
    a.cmp_reg_imm8(R::V0, 2); // Actual BlockedNative.
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(checked);
    let view = a.create_text_label();
    let end_running = a.create_text_label();
    a.cmp_reg_imm8(R::V6, 7);
    a.jcc_label(C::Equal, view);
    a.cmp_reg_imm8(R::V6, 8);
    a.jcc_label(C::Equal, end_running);
    let dispatched = a.create_text_label();
    let entries = [
        l.scoped_pin_new_actor,
        l.scoped_pin_step_actor,
        l.scoped_pin_release_actor,
        l.scoped_pin_native_enter_actor,
        l.scoped_pin_native_exit_actor,
        l.scoped_pin_callback_enter_actor,
        l.scoped_pin_callback_return_actor,
    ];
    for (op, entry) in entries.into_iter().enumerate() {
        let next = a.create_text_label();
        a.cmp_reg_imm8(R::V6, op as i8);
        a.jcc_label(C::NotEqual, next);
        if op == 0 {
            source_cell(a, invalid);
        }
        load(a, R::V0, offset_of!(Call, source));
        a.call_label(entry);
        a.jmp_label(dispatched);
        a.bind_text_label(next);
    }
    a.jmp_label(invalid);
    a.bind_text_label(view);
    a.load_ptr_disp32(R::V0, R::V7, d(offset_of!(Token, stage)));
    a.cmp_reg_imm8(R::V0, 3);
    a.jcc_label(C::NotEqual, invalid);
    a.call_label(l.scoped_pin_step_actor); // Authenticate actual root serial/holder.
    a.jmp_label(dispatched);
    a.bind_text_label(end_running);
    a.load_ptr_disp32(R::V0, R::V7, d(offset_of!(Token, uses)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.call_label(l.scoped_pin_end_running_actor);
    a.bind_text_label(dispatched);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    load(a, R::V7, TOKEN);
    a.load_ptr_disp32(R::V1, R::V7, d(offset_of!(Token, stage)));
    a.cmp_reg_imm8(R::V1, 3);
    a.jcc_label(C::NotEqual, done);
    a.load_ptr_disp32(R::V1, R::V7, d(offset_of!(Token, address)));
    store(a, ADDRESS, R::V1);
    a.load_ptr_disp32(R::V1, R::V7, d(offset_of!(Token, identity)));
    store(a, IDENTITY, R::V1);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    a.native_entry_end();
    a.ret();
    a.bind_text_label(invalid_input);
    a.mov_imm64(R::V0, 3);
    a.native_entry_end();
    a.ret();
}
fn call<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    set(a, OP, op);
    a.load_rbp_slot(R::V7, 8);
    a.mov_text_addr(R::V11, l.scoped_pin_abi_call);
    a.call_native_unary(R::V11);
}
fn emit_scope<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.scoped_pin_abi_scope);
    a.native_entry_begin(16);
    let invalid_input = a.create_text_label();
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    pointer(a, R::V7, invalid_input);
    a.store_rbp_slot(8, R::V7);
    context(a, invalid);
    // Scope owns its embedded token at this exact stable address.
    a.load_rbp_slot(R::V0, 8);
    a.add_reg_imm32(R::V0, d(OWN_TOKEN));
    store(a, TOKEN, R::V0);
    lea_rt(a, R::V0, API);
    store(a, offset_of!(Scope, api), R::V0);
    let prepare = a.create_text_label();
    let invoke = a.create_text_label();
    let cleanup = a.create_text_label();
    let complete = a.create_text_label();
    let failed = a.create_text_label();
    load(a, R::V0, STAGE);
    for (stage, label) in [(1, prepare), (2, invoke), (4, cleanup), (5, complete)] {
        a.cmp_reg_imm8(R::V0, stage);
        a.jcc_label(C::Equal, label);
    }
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid); // No callback/scope reentrancy at stage 3.
    load(a, R::V0, offset_of!(Scope, callback));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    call(a, l, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, done);
    set(a, STAGE, 1);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(prepare);
    call(a, l, 1);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, done);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, failed);
    set(a, STAGE, 2);
    a.bind_text_label(invoke);
    load(a, R::V0, offset_of!(Scope, callback));
    a.test_reg_reg(R::V0, R::V0);
    let callback_valid = a.create_text_label();
    a.jcc_label(C::NotEqual, callback_valid);
    a.mov_imm64(R::V0, 3);
    a.jmp_label(failed);
    a.bind_text_label(callback_valid);
    call(a, l, 3);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, failed);
    set(a, STAGE, 3);
    load(a, R::V11, offset_of!(Scope, callback));
    a.load_rbp_slot(R::V7, 8);
    a.call_native_unary(R::V11); // Actual Native; body may perform managed callbacks.
    store(a, offset_of!(Scope, result), R::V0);
    set(a, STAGE, 4); // Body cannot be invoked again during cleanup retries.
    a.jmp_label(cleanup);
    a.bind_text_label(failed);
    store(a, offset_of!(Scope, error), R::V0);
    set(a, STAGE, 4);
    a.bind_text_label(cleanup);
    // A body may return in Native, or leave a managed callback Running on
    // its error path. Normalize the real state rather than overwriting it.
    load(a, R::OwnerContext, offset_of!(Call, context));
    context_word(a, offset_of!(MutatorContext, state));
    let running = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, running);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, invalid);
    call(a, l, 4);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.bind_text_label(running);
    let unpin = a.create_text_label();
    load(a, R::V7, TOKEN);
    a.load_ptr_disp32(R::V0, R::V7, d(offset_of!(Token, uses)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, unpin);
    call(a, l, 8); // Each remaining scope-owned raw use ends through the real owner.
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    load(a, R::V7, TOKEN);
    a.load_ptr_disp32(R::V0, R::V7, d(offset_of!(Token, uses)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, unpin);
    // One Running-use end per scope step, independently of native nesting
    // depth. No raw address escapes while the body has ended and cleanup is
    // still owned. Its result/root/lease survive the next actual safepoint.
    set(a, ADDRESS, 0);
    set(a, IDENTITY, 0);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(unpin);
    call(a, l, 2);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    set(a, STAGE, 5);
    a.bind_text_label(complete);
    load(a, R::V0, offset_of!(Scope, error));
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    // The scalar callback result and unique token survive all retry statuses.
    a.load_rbp_slot(R::V9, 8);
    a.store_ptr_disp32(R::V9, d(offset_of!(Scope, last_status)), R::V0);
    a.native_entry_end();
    a.ret();
    a.bind_text_label(invalid_input);
    a.mov_imm64(R::V0, 3);
    a.native_entry_end();
    a.ret();
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_call(a, l);
    emit_scope(a, l);
}
