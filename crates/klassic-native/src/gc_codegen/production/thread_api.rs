//! Native-callable, allocator-backed thread lifecycle. Tokens retain exclusive
//! native backing across pending cleanup and OS failure; TLS values are never
//! treated as context pointers before actual association-list authentication.
use super::*;
use crate::gc_abi::{NativeThreadApi as Api, NativeThreadCall as Call, NativeThreadToken as Token};
pub(super) const API: usize = offset_of!(Runtime, thread_api);
const OWN_TOKEN: usize = owner_state::offset(offset_of!(OwnerState, native_thread_token));
pub(super) fn startup<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for (off, value) in [
        (offset_of!(Api, version), 1),
        (offset_of!(Api, bytes), size_of::<Api>() as u64),
        (offset_of!(Api, token_bytes), size_of::<Token>() as u64),
        (offset_of!(Api, call_bytes), size_of::<Call>() as u64),
    ] {
        store_rt_imm(a, API + off, value);
    }
    store_rt_addr(a, API + offset_of!(Api, self_), API);
    store_rt(a, API + offset_of!(Api, runtime), R::V3);
    a.mov_text_addr(R::V0, l.thread_api_call);
    store_rt(a, API + offset_of!(Api, call), R::V0);
}
fn pointer<E: PortableAsm>(a: &mut E, r: R, bad: E::TextLabel) {
    a.test_reg_reg(r, r);
    a.jcc_label(C::Equal, bad);
    a.mov_reg_reg(R::V0, r);
    a.and_reg_imm32(R::V0, 7);
    a.jcc_label(C::NotEqual, bad);
}
fn load<E: PortableAsm>(a: &mut E, r: R, off: usize) {
    a.load_rbp_slot(R::V9, 8);
    a.load_ptr_disp32(r, R::V9, d(off));
}
fn put<E: PortableAsm>(a: &mut E, off: usize, r: R) {
    a.load_rbp_slot(R::V9, 8);
    a.store_ptr_disp32(R::V9, d(off), r);
}
fn token<E: PortableAsm>(a: &mut E) {
    load(a, R::V8, offset_of!(Call, token));
}
fn get<E: PortableAsm>(a: &mut E, r: R, off: usize) {
    token(a);
    a.load_ptr_disp32(r, R::V8, d(off));
}
fn set<E: PortableAsm>(a: &mut E, off: usize, r: R) {
    token(a);
    a.store_ptr_disp32(R::V8, d(off), r);
}
fn stage<E: PortableAsm>(a: &mut E, value: u64) {
    a.mov_imm64(R::V0, value);
    set(a, offset_of!(Token, stage), R::V0);
}
fn local<E: PortableAsm>(a: &mut E, off: i32) {
    a.mov_reg_reg(R::V7, R::V5);
    a.sub_reg_imm32(R::V7, off);
}
fn word<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.atomic_load_acquire_u32(R::V0, R::V1);
}
fn owned<E: PortableAsm>(a: &mut E, bad: E::TextLabel) {
    token(a);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Token, self_)));
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, bad);
    get(a, R::V0, offset_of!(Token, runtime));
    a.cmp_reg_reg(R::V0, R::V3);
    a.jcc_label(C::NotEqual, bad);
    get(a, R::V0, offset_of!(Token, thread));
    a.load_rbp_slot(R::V1, 16);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, bad);
    get(a, R::OwnerContext, offset_of!(Token, context));
    pointer(a, R::OwnerContext, bad);
    load_rt(
        a,
        R::V0,
        owner_state::offset(offset_of!(OwnerState, shared_runtime)),
    );
    a.cmp_reg_reg(R::V0, R::V3);
    a.jcc_label(C::NotEqual, bad);
    load_rt(a, R::V0, OWN_TOKEN);
    token(a);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, bad);
}
fn empty<E: PortableAsm>(a: &mut E, bad: E::TextLabel) {
    for off in (0..size_of::<Token>()).step_by(8) {
        get(a, R::V0, off);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, bad);
    }
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.thread_api_call);
    a.native_entry_begin(64);
    let bad = a.create_text_label();
    let busy = a.create_text_label();
    let done = a.create_text_label();
    let shutdown = a.create_text_label();
    let current = a.create_text_label();
    let fresh = a.create_text_label();
    let progress = a.create_text_label();
    let ready = a.create_text_label();
    let detach = a.create_text_label();
    let clear = a.create_text_label();
    let dispose = a.create_text_label();
    let publish = a.create_text_label();
    let success = a.create_text_label();
    pointer(a, R::V7, bad);
    a.store_rbp_slot(8, R::V7);
    a.mov_imm64(R::V0, 0);
    put(a, offset_of!(Call, context), R::V0);
    put(a, offset_of!(Call, binding), R::V0);
    load(a, R::V8, offset_of!(Call, api));
    pointer(a, R::V8, bad);
    a.load_ptr_disp32(R::V3, R::V8, d(offset_of!(Api, runtime)));
    pointer(a, R::V3, bad);
    lea_rt(a, R::V1, API);
    a.cmp_reg_reg(R::V8, R::V1);
    a.jcc_label(C::NotEqual, bad);
    for (off, value) in [
        (offset_of!(Api, version), 1),
        (offset_of!(Api, bytes), size_of::<Api>() as u64),
    ] {
        a.load_ptr_disp32(R::V0, R::V8, d(off));
        a.cmp_reg_imm32(R::V0, value as i32);
        a.jcc_label(C::NotEqual, bad);
    }
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Api, self_)));
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, bad);
    // Raw resource/lookup tickets do not borrow any caller's owner identity.
    a.mov_imm64(R::OwnerContext, 0);
    load(a, R::V0, offset_of!(Call, operation));
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Above, bad);
    a.jcc_label(C::Equal, shutdown);
    // An empty-token cleanup needs no OS resource or creator identity.
    let identify = a.create_text_label();
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, identify);
    load(a, R::V8, offset_of!(Call, token));
    pointer(a, R::V8, bad);
    get(a, R::V0, offset_of!(Token, stage));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, identify);
    empty(a, bad);
    a.jmp_label(success);
    a.bind_text_label(identify);
    local(a, 16);
    a.call_label(l.tls_query_thread);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.load_rbp_slot(R::V0, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, bad);
    load(a, R::V0, offset_of!(Call, operation));
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Equal, current);
    load(a, R::V8, offset_of!(Call, token));
    pointer(a, R::V8, bad);
    get(a, R::V0, offset_of!(Token, stage));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, fresh);
    owned(a, bad);
    load(a, R::V0, offset_of!(Call, operation));
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, detach);
    a.jmp_label(progress);

    a.bind_text_label(fresh);
    empty(a, bad);
    load(a, R::V0, offset_of!(Call, operation));
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, success);
    a.call_label(l.tls_initialize);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    local(a, 24);
    a.call_label(l.tls_prepare_attach);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.load_rbp_slot(R::V0, 24);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, busy);
    // Dynamic capacities are validated before any ownership or mapping. Their
    // u32 bounds also prove that the native byte arithmetic cannot overflow.
    for (off, save) in [
        (offset_of!(Call, cells), 32),
        (offset_of!(Call, entries), 40),
    ] {
        load(a, R::V0, off);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, bad);
        a.mov_imm64(R::V1, u32::MAX as u64);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Above, bad);
        a.store_rbp_slot(save, R::V0);
    }
    a.load_rbp_slot(R::V7, 32);
    a.load_rbp_slot(R::V0, 40);
    a.add_reg_reg(R::V7, R::V0);
    a.shl_reg_imm8(R::V7, 5);
    a.add_reg_imm32(R::V7, size_of::<RootStorage>() as i32);
    a.store_rbp_slot(48, R::V7);
    a.add_reg_imm32(R::V7, d(offset_of!(OwnerState, roots)));
    a.call_label(l.metadata_map);
    a.test_reg_reg(R::V0, R::V0);
    let mapped = a.create_text_label();
    a.jcc_label(C::NotEqual, mapped);
    a.mov_imm64(R::V0, 6);
    a.jmp_label(done);
    a.bind_text_label(mapped);
    a.mov_reg_reg(R::OwnerContext, R::V0);
    set(a, offset_of!(Token, context), R::V0);
    token(a);
    store_rt(a, OWN_TOKEN, R::V8);
    a.mov_reg_reg(R::V0, R::V8);
    set(a, offset_of!(Token, self_), R::V0);
    set(a, offset_of!(Token, runtime), R::V3);
    a.load_rbp_slot(R::V0, 16);
    set(a, offset_of!(Token, thread), R::V0);
    stage(a, 1);
    for (off, save) in [
        (offset_of!(RootStorageSpec, cells), 32),
        (offset_of!(RootStorageSpec, entries), 40),
        (offset_of!(RootStorageSpec, bytes), 48),
    ] {
        a.load_rbp_slot(R::V0, save);
        store_rt(a, RT_SPEC + off, R::V0);
    }
    a.mov_reg_reg(R::V7, R::OwnerContext);
    a.call_label(l.attach_actor);
    a.bind_text_label(progress);
    get(a, R::V0, offset_of!(Token, stage));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, ready);
    a.call_label(l.owner_checkpoint_poll);
    a.call_label(l.tls_initialize);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.call_label(l.tls_install_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    stage(a, 2);
    a.jmp_label(publish);
    a.bind_text_label(ready);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, detach);
    local(a, 24);
    a.call_label(l.tls_current);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.load_rbp_slot(R::V0, 24);
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, bad);
    a.jmp_label(publish);

    a.bind_text_label(detach);
    get(a, R::V0, offset_of!(Token, stage));
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::Equal, dispose);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Equal, clear);
    a.cmp_reg_imm32(R::V0, 3);
    let stepping = a.create_text_label();
    a.jcc_label(C::Equal, stepping);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::Below, bad);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Above, bad);
    word(a);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, bad);
    a.call_label(l.detach_begin_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    stage(a, 3);
    a.bind_text_label(stepping);
    a.call_label(l.detach_step_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    stage(a, 4);
    a.bind_text_label(clear);
    a.mov_reg_reg(R::V7, R::OwnerContext);
    a.call_label(l.tls_clear_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    stage(a, 5);
    a.bind_text_label(dispose);
    a.mov_reg_reg(R::V7, R::OwnerContext);
    a.mov_imm64(R::OwnerContext, 0);
    a.call_label(l.detach_dispose_actor);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    for off in (0..size_of::<Token>()).step_by(8) {
        a.mov_imm64(R::V0, 0);
        set(a, off, R::V0);
    }
    a.jmp_label(success);

    a.bind_text_label(current);
    a.call_label(l.tls_initialize);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    local(a, 24);
    a.call_label(l.tls_current);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.load_rbp_slot(R::OwnerContext, 24);
    a.test_reg_reg(R::OwnerContext, R::OwnerContext);
    a.jcc_label(C::Equal, success);
    a.bind_text_label(publish);
    put(a, offset_of!(Call, context), R::OwnerContext);
    a.mov_reg_reg(R::V7, R::OwnerContext);
    a.mov_text_addr(R::V11, l.native_api_bootstrap);
    a.call_native_unary(R::V11);
    put(a, offset_of!(Call, binding), R::V0);
    a.jmp_label(success);
    a.bind_text_label(shutdown);
    a.call_label(l.tls_destroy);
    a.jmp_label(done);
    a.bind_text_label(busy);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(bad);
    a.mov_imm64(R::V0, 3);
    a.jmp_label(done);
    a.bind_text_label(success);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
    a.native_entry_end();
    a.ret();
}
