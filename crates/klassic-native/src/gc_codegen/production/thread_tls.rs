//! OS TLS lifetime for explicit production owners. Resource/borrow counts cover
//! actual OS calls; no key can be deleted while a context or lookup retains it.
//! Query returns an opaque OS value: callers must authenticate a context before
//! dereferencing it. Linux rejects an occupied GS on a fresh install.
use super::*;
use crate::gc_abi::ThreadRegistration;
#[path = "thread_tls_lookup.rs"]
mod lookup;
#[repr(C)]
pub(super) struct State {
    key: u64,
    pub(super) stage: u64,
    pub(super) users: u64,
    borrowed: u64,
    head: u64,
    pub(super) readers: u64,
}
const BASE: usize = offset_of!(Runtime, thread_tls);
const KEY: usize = BASE + offset_of!(State, key);
const STAGE: usize = BASE + offset_of!(State, stage);
const USERS: usize = BASE + offset_of!(State, users);
const BORROWS: usize = BASE + offset_of!(State, borrowed);
const HEAD: usize = BASE + offset_of!(State, head);
pub(super) const READERS: usize = BASE + offset_of!(State, readers);
const PREV: usize = owner_state::offset(offset_of!(OwnerState, native_tls_prev));
const NEXT: usize = owner_state::offset(offset_of!(OwnerState, native_tls_next));
pub(super) const THREAD: usize = owner_state::offset(offset_of!(OwnerState, native_tls_thread));
pub(super) const OWNER: usize = owner_state::offset(offset_of!(OwnerState, native_tls));
fn frame<E: PortableAsm>(a: &mut E) {
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm32(R::V4, 64);
    a.store_rbp_slot(8, R::V7);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
}
fn end<E: PortableAsm>(a: &mut E) {
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.leave();
    a.ret();
}
fn os<E: PortableAsm>(a: &mut E, entry: E::TextLabel) {
    a.mov_reg_reg(R::V7, R::V5);
    a.sub_reg_imm32(R::V7, 40); // key -40, value -32, OS thread token -24
    a.mov_text_addr(R::V11, entry);
    // Internal callers can have an odd number of saved words. Native OS
    // entries still require exact platform alignment; keep their request in
    // this stable frame while aligning only the temporary call stack.
    a.mov_reg_reg(R::V0, R::V4);
    a.store_rbp_slot(56, R::V0);
    a.and_reg_imm32(R::V0, -16);
    a.mov_reg_reg(R::V4, R::V0);
    a.push_reg(R::V3);
    a.push_reg(R::V3);
    a.call_native_unary(R::V11);
    a.pop_reg(R::V3);
    a.pop_reg(R::V3);
    a.load_rbp_slot(R::V1, 56);
    a.mov_reg_reg(R::V4, R::V1);
}
fn counter<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, field: usize, add: bool) {
    load_rt(a, R::V0, field);
    if add {
        a.add_reg_imm32(R::V0, 1);
        a.jcc_label(C::Equal, l.fail);
    } else {
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, l.fail);
        a.sub_reg_imm8(R::V0, 1);
    }
    store_rt(a, field, R::V0);
}
fn key<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, KEY);
    a.store_rbp_slot(40, R::V0);
    a.mov_imm64(R::V0, 0);
    a.store_rbp_slot(32, R::V0);
    a.store_rbp_slot(24, R::V0);
}
fn ready<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    load_rt(a, R::V0, STAGE);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, pending);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    lifecycle(a, l, false);
    lifecycle(a, l, true);
    query(a, l, false);
    query(a, l, true);
    install(a, l);
    clear(a, l);
    lookup::emit(a, l);
}
fn lifecycle<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, deleting: bool) {
    a.bind_text_label(if deleting {
        l.tls_destroy
    } else {
        l.tls_initialize
    });
    frame(a);
    let done = a.create_text_label();
    let pending = a.create_text_label();
    let already = a.create_text_label();
    let failed = a.create_text_label();
    let publish = a.create_text_label();
    worker::lock_raw(a, l);
    load_rt(a, R::V0, STAGE);
    a.cmp_reg_imm32(R::V0, if deleting { 0 } else { 2 });
    a.jcc_label(C::Equal, already);
    a.cmp_reg_imm32(R::V0, if deleting { 2 } else { 0 });
    a.jcc_label(C::NotEqual, pending);
    if deleting {
        load_rt(a, R::V0, USERS);
        load_rt(a, R::V1, BORROWS);
        a.or_reg_reg(R::V0, R::V1);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, pending);
    }
    store_rt_imm(a, STAGE, if deleting { 3 } else { 1 });
    key(a);
    worker::unlock_raw(a, l);
    os(
        a,
        if deleting {
            l.tls_os.delete
        } else {
            l.tls_os.create
        },
    );
    a.store_rbp_slot(16, R::V0);
    worker::lock_raw(a, l);
    load_rt(a, R::V0, STAGE);
    a.cmp_reg_imm32(R::V0, if deleting { 3 } else { 1 });
    a.jcc_label(C::NotEqual, l.fail);
    a.load_rbp_slot(R::V0, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, failed);
    if deleting {
        store_rt_imm(a, KEY, 0);
    } else {
        a.load_rbp_slot(R::V0, 40);
        store_rt(a, KEY, R::V0);
    }
    store_rt_imm(a, STAGE, if deleting { 0 } else { 2 });
    a.jmp_label(publish);
    a.bind_text_label(failed);
    store_rt_imm(a, STAGE, if deleting { 2 } else { 0 });
    a.bind_text_label(publish);
    a.load_rbp_slot(R::V0, 16);
    a.jmp_label(done);
    a.bind_text_label(already);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    worker::unlock_raw(a, l);
    end(a);
}
/// V7 is native u64 output storage; status 0/2/6. No context is dereferenced.
fn query<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, thread: bool) {
    a.bind_text_label(if thread {
        l.tls_query_thread
    } else {
        l.tls_query
    });
    frame(a);
    if thread {
        // Cleanup cannot depend on obtaining a new OS TLS key. This native
        // identity leaf never reads an association or takes a collector ticket.
        os(a, l.tls_os.identity);
        let done = a.create_text_label();
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
        a.load_rbp_slot(R::V1, 8);
        a.load_rbp_slot(R::V2, 24);
        a.store_ptr_disp32(R::V1, 0, R::V2);
        a.bind_text_label(done);
        end(a);
        return;
    }
    let pending = a.create_text_label();
    let no_value = a.create_text_label();
    let done = a.create_text_label();
    worker::lock_raw(a, l);
    ready(a, pending);
    counter(a, l, BORROWS, true);
    key(a);
    worker::unlock_raw(a, l);
    os(a, l.tls_os.get);
    a.store_rbp_slot(16, R::V0);
    worker::lock_raw(a, l);
    counter(a, l, BORROWS, false);
    a.load_rbp_slot(R::V0, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, no_value);
    a.load_rbp_slot(R::V1, 8);
    a.load_rbp_slot(R::V0, 32);
    a.store_ptr_disp32(R::V1, 0, R::V0);
    a.bind_text_label(no_value);
    a.load_rbp_slot(R::V0, 16);
    a.jmp_label(done);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    worker::unlock_raw(a, l);
    end(a);
}
/// Running owner, initialized key. 0 installed/idempotent; 2 pending/occupied
/// foreign TLS; 3 wrong thread; 6 OS failure. No foreign GS/TLS value is changed.
fn install<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.tls_install_actor);
    frame(a);
    owner_state::bind_running(a, l);
    a.call_label(l.publish_pending);
    quiescence::poll(a, l);
    let pending = a.create_text_label();
    let duplicate = a.create_text_label();
    let inspect = a.create_text_label();
    let busy = a.create_text_label();
    let invalid = a.create_text_label();
    let finish = a.create_text_label();
    let failed = a.create_text_label();
    let done = a.create_text_label();
    worker::lock_raw(a, l);
    ready(a, pending);
    load_rt(a, R::V0, OWNER);
    a.store_rbp_slot(48, R::V0);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, inspect);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    store_rt_imm(a, OWNER, 1);
    counter(a, l, USERS, true);
    a.bind_text_label(inspect);
    counter(a, l, BORROWS, true);
    key(a);
    worker::unlock_raw(a, l);
    os(a, l.tls_os.get);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, finish);
    a.load_rbp_slot(R::V1, 48);
    a.cmp_reg_imm32(R::V1, 2);
    a.jcc_label(C::Equal, duplicate);
    a.load_rbp_slot(R::V0, 32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, busy);
    a.store_rbp_slot(32, R::OwnerContext);
    os(a, l.tls_os.set);
    a.jmp_label(finish);
    a.bind_text_label(duplicate);
    a.load_rbp_slot(R::V0, 32);
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, invalid);
    a.load_rbp_slot(R::V0, 24);
    load_rt(a, R::V1, THREAD);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(finish);
    a.bind_text_label(busy);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(finish);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(finish);
    a.store_rbp_slot(16, R::V0);
    worker::lock_raw(a, l);
    counter(a, l, BORROWS, false);
    a.load_rbp_slot(R::V0, 48);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, done);
    a.load_rbp_slot(R::V0, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, failed);
    a.load_rbp_slot(R::V0, 24);
    store_rt(a, THREAD, R::V0);
    store_rt_imm(a, OWNER, 2);
    link(a);
    a.jmp_label(done);
    a.bind_text_label(failed);
    store_rt_imm(a, OWNER, 0);
    counter(a, l, USERS, false);
    a.jmp_label(done);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.store_rbp_slot(16, R::V0);
    a.bind_text_label(done);
    a.load_rbp_slot(R::V0, 16);
    worker::unlock_raw(a, l);
    end(a);
}
/// V7 retains a Detached context. The calling OS thread must still own its TLS
/// association. Failed/wrong-thread clear retains that association and backing.
fn clear<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.tls_clear_actor);
    frame(a);
    a.mov_reg_reg(R::OwnerContext, R::V7);
    load_rt(
        a,
        R::V3,
        owner_state::offset(offset_of!(OwnerState, shared_runtime)),
    );
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.atomic_load_acquire_u32(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V0,
        participants::OWNER + offset_of!(ThreadRegistration, registered),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    let pending = a.create_text_label();
    let already = a.create_text_label();
    let invalid = a.create_text_label();
    let finish = a.create_text_label();
    let retained = a.create_text_label();
    let done = a.create_text_label();
    worker::lock_raw(a, l);
    load_rt(a, R::V0, OWNER);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, already);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, pending);
    ready(a, l.fail);
    store_rt_imm(a, OWNER, 3);
    counter(a, l, BORROWS, true);
    key(a);
    worker::unlock_raw(a, l);
    os(a, l.tls_os.get);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, finish);
    a.load_rbp_slot(R::V0, 32);
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, invalid);
    a.load_rbp_slot(R::V0, 24);
    load_rt(a, R::V1, THREAD);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_imm64(R::V0, 0);
    a.store_rbp_slot(32, R::V0);
    os(a, l.tls_os.set);
    a.jmp_label(finish);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(finish);
    a.store_rbp_slot(16, R::V0);
    worker::lock_raw(a, l);
    counter(a, l, BORROWS, false);
    a.load_rbp_slot(R::V0, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, retained);
    store_rt_imm(a, OWNER, 0);
    store_rt_imm(a, THREAD, 0);
    unlink(a);
    counter(a, l, USERS, false);
    a.jmp_label(done);
    a.bind_text_label(retained);
    store_rt_imm(a, OWNER, 2);
    a.jmp_label(done);
    a.bind_text_label(already);
    a.mov_imm64(R::V0, 0);
    a.store_rbp_slot(16, R::V0);
    a.jmp_label(done);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.store_rbp_slot(16, R::V0);
    a.bind_text_label(done);
    a.load_rbp_slot(R::V0, 16);
    worker::unlock_raw(a, l);
    end(a);
}

// The list contains only actual installed associations. Removing a node keeps
// its next word intact until all traversals finish; disposal observes READERS.
// Insertion/removal each touch a constant number of native words under ticket.
fn link<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, HEAD);
    store_rt(a, NEXT, R::V1);
    store_rt_imm(a, PREV, 0);
    let empty = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, empty);
    a.store_ptr_disp32(
        R::V1,
        d(offset_of!(OwnerState, native_tls_prev)),
        R::OwnerContext,
    );
    a.bind_text_label(empty);
    store_rt(a, HEAD, R::OwnerContext);
}
fn unlink<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, PREV);
    load_rt(a, R::V2, NEXT);
    let head = a.create_text_label();
    let successor = a.create_text_label();
    let done = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, head);
    a.store_ptr_disp32(R::V1, d(offset_of!(OwnerState, native_tls_next)), R::V2);
    a.jmp_label(successor);
    a.bind_text_label(head);
    store_rt(a, HEAD, R::V2);
    a.bind_text_label(successor);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, done);
    a.store_ptr_disp32(R::V2, d(offset_of!(OwnerState, native_tls_prev)), R::V1);
    a.bind_text_label(done);
    // NEXT is a retained traversal edge, not an active membership flag.
    store_rt_imm(a, PREV, 0);
}
