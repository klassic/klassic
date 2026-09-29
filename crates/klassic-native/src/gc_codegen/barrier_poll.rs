//! Finite cancel/cleanup -> owner rendezvous -> rearm, without an OS wait loop.
use super::*;
use crate::gc_abi::{RootedLoadAdmission as A, RootedStoreRuntime as D, ThreadRegistration};
const WORKER: i32 = 144;
fn invoke<B: PortableAsm>(
    b: &mut Emit<'_, B>,
    load: B::TextLabel,
    store: B::TextLabel,
    op: u64,
    next: bool,
) {
    let is_store = b.a.create_text_label();
    let done = b.a.create_text_label();
    b.a.load_rbp_slot(R::V7, G);
    b.a.load_rbp_slot(R::V6, Q);
    b.a.mov_imm64(R::V2, op);
    b.branch(local(KIND), n(1), C::Equal, is_store);
    b.a.call_label(load);
    b.a.jmp_label(done);
    b.a.bind_text_label(is_store);
    b.a.call_label(store);
    b.a.bind_text_label(done);
    b.a.store_rbp_slot(if next { NEXT } else { STATE }, R::V0);
}
fn native<B: PortableAsm>(b: &mut Emit<'_, B>, offset: usize) {
    b.value(field(G, offset, 0));
    b.a.mov_reg_reg(R::V10, R::V0);
    b.value(field(G, offset_of!(D, registry_lock), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.call_native_unary(R::V10);
}
#[allow(dead_code)]
pub(crate) fn emit<B: PortableAsm>(a: &mut B) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    let terminal = a.create_text_label();
    let after_cleanup = a.create_text_label();
    let load_next = a.create_text_label();
    let store_next = a.create_text_label();
    let load = a.create_text_label();
    let store = a.create_text_label();
    let worker = a.create_text_label();
    let unlock = a.create_text_label();
    let resumed = a.create_text_label();
    let rearmed = a.create_text_label();
    let results: Vec<_> = (0..8).map(|_| a.create_text_label()).collect();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 32);
    a.store_rbp_slot(G, R::V7);
    a.store_rbp_slot(Q, R::V6);
    a.store_rbp_slot(KIND, R::V2);
    let mut b = Emit { a };
    b.pointer(local(G), invalid);
    b.branch(local(KIND), n(1), C::Above, invalid);
    invoke(&mut b, load_next, store_next, 0, true);
    b.branch(local(NEXT), n(44), C::Equal, invalid);
    for status in [35, 36, 42, 43] {
        b.branch(local(NEXT), n(status), C::Equal, terminal);
    }
    invoke(&mut b, load, store, 1, false);
    b.branch(local(STATE), n(44), C::Equal, invalid);
    invoke(&mut b, load, store, 0, true);
    for status in [35, 36, 42, 43] {
        b.branch(local(NEXT), n(status), C::Equal, terminal);
    }
    b.value(local(NEXT));
    b.a.jmp_label(done);
    b.a.bind_text_label(terminal);
    b.set(local(CTX), field(Q, 0, 0));
    b.pointer(local(CTX), invalid);
    b.set(local(SEG), field(G, offset_of!(D, admission), 0));
    b.pointer(local(SEG), invalid);
    for field_offset in [
        offset_of!(D, registry_lock),
        offset_of!(D, lock),
        offset_of!(D, unlock),
    ] {
        b.branch(field(G, field_offset, 0), n(0), C::Equal, invalid);
    }
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
    b.pointer(field(SEG, offset_of!(A, registry), 0), invalid);
    b.pointer(field(SEG, offset_of!(A, participant), 0), invalid);
    b.set(local(WORKER), field(SEG, offset_of!(A, handshake), 0));
    b.set(local(WORKER - 8), field(SEG, offset_of!(A, participant), 0));
    native(&mut b, offset_of!(D, lock));
    b.set(local(REG), field(SEG, offset_of!(A, participant), 0));
    b.set(local(STATE), n(3));
    b.branch(
        field(REG, offset_of!(ThreadRegistration, context), 0),
        local(CTX),
        C::NotEqual,
        unlock,
    );
    b.value(field(SEG, offset_of!(A, registry), 0));
    b.a.mov_reg_reg(R::V7, R::V0);
    b.a.mov_reg_reg(R::V6, R::V5);
    b.a.sub_reg_imm32(R::V6, WORKER);
    b.a.mov_imm64(R::V2, 0);
    b.a.call_label(worker);
    b.a.store_rbp_slot(STATE, R::V0);
    b.a.bind_text_label(unlock);
    native(&mut b, offset_of!(D, unlock));
    b.branch(local(STATE), n(0), C::Equal, resumed);
    for i in 1..8 {
        b.branch(local(STATE), n(i), C::Equal, results[i as usize]);
    }
    b.a.jmp_label(invalid);
    b.a.bind_text_label(resumed);
    b.branch(local(NEXT), n(36), C::Equal, after_cleanup);
    b.value(local(NEXT));
    b.a.jmp_label(done);
    b.a.bind_text_label(after_cleanup);
    invoke(&mut b, load, store, 2, false);
    b.branch(local(STATE), n(32), C::Equal, rearmed);
    b.branch(local(STATE), n(37), C::Equal, rearmed);
    b.value(local(STATE));
    b.a.jmp_label(done);
    b.a.bind_text_label(rearmed);
    b.a.mov_imm64(R::V0, 80);
    b.a.jmp_label(done);
    for (i, status) in [44, 81, 82, 44, 83, 84, 85, 86].into_iter().enumerate() {
        b.a.bind_text_label(results[i]);
        b.a.mov_imm64(R::V0, status);
        b.a.jmp_label(done);
    }
    b.a.bind_text_label(invalid);
    b.a.mov_imm64(R::V0, 44);
    b.a.bind_text_label(done);
    b.a.leave();
    b.a.ret();
    b.a.bind_text_label(load_next);
    super::rooted_load::emit_next(b.a);
    b.a.ret();
    b.a.bind_text_label(store_next);
    super::rooted_store::emit_next(b.a);
    b.a.ret();
    b.a.bind_text_label(load);
    super::rooted_store_dispatch::emit_load(b.a);
    b.a.ret();
    b.a.bind_text_label(store);
    super::rooted_store_dispatch::emit(b.a);
    b.a.ret();
    b.a.bind_text_label(worker);
    super::worker::emit_worker_step(b.a);
    b.a.ret();
}
