//! Actual-owner native adapter bootstrap and root-only boundary dispatch.
//! Compiler sleep and standalone string output consume this same native ABI.
use super::*;
use crate::gc_abi::{NativeApiBinding as Binding, NativeApiCall as Call};

pub(super) const BINDING: usize = owner_state::offset(offset_of!(OwnerState, native_api));
const REQUEST: usize = owner_state::offset(offset_of!(OwnerState, native_api_request));
pub(super) fn initialize<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    store_rt_imm(a, BINDING + offset_of!(Binding, version), 1);
    store_rt_imm(
        a,
        BINDING + offset_of!(Binding, bytes),
        size_of::<Binding>() as u64,
    );
    store_rt_addr(a, BINDING + offset_of!(Binding, self_), BINDING);
    lea_rt(a, R::V0, RT_CTX);
    store_rt(a, BINDING + offset_of!(Binding, context), R::V0);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    store_rt(a, BINDING + offset_of!(Binding, identity), R::V0);
    lea_rt(a, R::V0, pin_abi::API);
    store_rt(a, BINDING + offset_of!(Binding, pins), R::V0);
    a.mov_text_addr(R::V0, l.native_api_call);
    store_rt(a, BINDING + offset_of!(Binding, call), R::V0);
}
fn pointer<E: PortableAsm>(a: &mut E, r: R, bad: E::TextLabel) {
    a.test_reg_reg(r, r);
    a.jcc_label(C::Equal, bad);
    a.mov_reg_reg(R::V0, r);
    a.and_reg_imm32(R::V0, 7);
    a.jcc_label(C::NotEqual, bad);
}
fn word32<E: PortableAsm>(a: &mut E, off: usize) {
    a.mov_reg_reg(R::V1, R::OwnerContext);
    a.add_reg_imm32(R::V1, d(off));
    a.atomic_load_acquire_u32(R::V0, R::V1);
}
/// Recover the real registry binding before exposing any API pointer. This
/// checks actual registration identity/context rather than trusting a copied
/// ABI prefix or caller-filled immutable-looking fields.
fn owner<E: PortableAsm>(a: &mut E, bad: E::TextLabel) {
    pointer(a, R::OwnerContext, bad);
    word32(a, offset_of!(MutatorContext, abi_version));
    a.cmp_reg_imm32(R::V0, ABI_VERSION as i32);
    a.jcc_label(C::NotEqual, bad);
    word32(a, offset_of!(MutatorContext, state));
    let state_ok = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, state_ok);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::NotEqual, bad);
    a.bind_text_label(state_ok);
    load_rt(
        a,
        R::V3,
        owner_state::offset(offset_of!(OwnerState, shared_runtime)),
    );
    pointer(a, R::V3, bad);
    use crate::gc_abi::ThreadRegistration as T;
    load_rt(a, R::V0, participants::OWNER + offset_of!(T, registered));
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, bad);
    load_rt(a, R::V0, participants::OWNER + offset_of!(T, context));
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, bad);
    load_rt(a, R::V0, participants::OWNER + offset_of!(T, registry));
    lea_rt(a, R::V1, participants::HANDSHAKE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, bad);
    load_rt(a, R::V0, participants::OWNER + offset_of!(T, identity));
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, bad);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, bad);
}
fn validate<E: PortableAsm>(a: &mut E, bad: E::TextLabel) {
    lea_rt(a, R::V8, BINDING);
    for (off, value) in [
        (offset_of!(Binding, version), 1),
        (offset_of!(Binding, bytes), size_of::<Binding>() as u64),
    ] {
        a.load_ptr_disp32(R::V0, R::V8, d(off));
        a.cmp_reg_imm32(R::V0, value as i32);
        a.jcc_label(C::NotEqual, bad);
    }
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Binding, self_)));
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, bad);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Binding, context)));
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, bad);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Binding, identity)));
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, bad);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Binding, pins)));
    lea_rt(a, R::V1, pin_abi::API);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, bad);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.native_api_bootstrap);
    a.native_entry_begin(16);
    let bad = a.create_text_label();
    let done = a.create_text_label();
    a.mov_reg_reg(R::OwnerContext, R::V7);
    owner(a, bad);
    validate(a, bad);
    lea_rt(a, R::V0, BINDING);
    a.jmp_label(done);
    a.bind_text_label(bad);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
    a.native_entry_end();
    a.ret();

    a.bind_text_label(l.native_api_call);
    a.native_entry_begin(16);
    let bad = a.create_text_label();
    let done = a.create_text_label();
    pointer(a, R::V7, bad);
    a.store_rbp_slot(8, R::V7);
    a.load_ptr_disp32(R::V8, R::V7, d(offset_of!(Call, binding)));
    pointer(a, R::V8, bad);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Binding, self_)));
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, bad);
    a.load_ptr_disp32(R::OwnerContext, R::V8, d(offset_of!(Binding, context)));
    owner(a, bad);
    a.load_rbp_slot(R::V7, 8);
    a.load_ptr_disp32(R::V8, R::V7, d(offset_of!(Call, binding)));
    lea_rt(a, R::V1, BINDING);
    a.cmp_reg_reg(R::V8, R::V1);
    a.jcc_label(C::NotEqual, bad);
    validate(a, bad);
    a.load_rbp_slot(R::V7, 8);
    a.load_ptr_disp32(R::V6, R::V7, d(offset_of!(Call, operation)));
    for (op, state, entry) in [
        (0, 0, l.native_enter_actor),
        (1, 2, l.native_exit_actor),
        (2, 0, l.poll_actor),
    ] {
        let next = a.create_text_label();
        a.cmp_reg_imm8(R::V6, op);
        a.jcc_label(C::NotEqual, next);
        word32(a, offset_of!(MutatorContext, state));
        a.cmp_reg_imm8(R::V0, state);
        a.jcc_label(C::NotEqual, bad);
        a.call_label(entry);
        a.mov_imm64(R::V0, 0);
        a.jmp_label(done);
        a.bind_text_label(next);
    }
    a.cmp_reg_imm8(R::V6, 4);
    a.jcc_label(C::NotEqual, bad);
    lea_rt(a, R::V0, thread_api::API);
    a.jmp_label(done);
    a.bind_text_label(bad);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    a.native_entry_end();
    a.ret();
}

/// Compiler adapter with only native scalar/buffer inputs. Caller has bound
/// its actual owner and saved live portable registers; stack is native aligned.
pub(super) fn compiler_bootstrap<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.mov_reg_reg(R::V7, R::OwnerContext);
    a.mov_text_addr(R::V11, l.native_api_bootstrap);
    a.call_native_unary(R::V11);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, REQUEST + offset_of!(Call, binding), R::V0);
}
pub(super) fn compiler_transition<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    enter: bool,
) {
    store_rt_imm(
        a,
        REQUEST + offset_of!(Call, operation),
        if enter { 0 } else { 1 },
    );
    lea_rt(a, R::V7, REQUEST);
    a.mov_text_addr(R::V11, l.native_api_call);
    a.call_native_unary(R::V11);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
