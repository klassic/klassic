//! Real Linux threads allocate their own contexts exclusively through the
//! public ABI. No owner prefix, registration or TLS ownership is prefilled.
use super::super::*;
use super::{ThreadApiFixture, thread_api_call, thread_api_fixture, thread_api_token_empty};
use crate::gc_codegen::threads;
use crate::{DataLabel, NativeCodeGenerator, TextLabel};
const STACK_BYTES: u64 = 65536;
const TID: i32 = 24;
const STACK: i32 = 32;
const DONE: i32 = 48;
const READY: i32 = 56;
const ROOT: i32 = 64;
const VALUE: i32 = 72;
const ATTACH: i32 = 80;
const DETACH: i32 = 88;
const IDENTITY: i32 = 96;
const OS_TID: i32 = 104;
const CREATOR: i32 = 112;
const BOOTSTRAP: i32 = 120;
const ADMIT: i32 = 128;

pub(crate) struct PublicApiThread {
    control: DataLabel,
    pub(crate) wrong: ThreadApiFixture,
    join: TextLabel,
    unmap: TextLabel,
}
fn control(g: &mut NativeCodeGenerator, r: R) {
    g.load_rbp_slot(r, 8);
}
fn api_retry(g: &mut NativeCodeGenerator, f: ThreadApiFixture, op: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 20_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    thread_api_call(g, f, op);
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn start_public_api_thread(g: &mut NativeCodeGenerator) -> PublicApiThread {
    start_public_api_thread_impl(g, false, false)
}
pub(crate) fn start_inherited_public_api_thread(g: &mut NativeCodeGenerator) -> PublicApiThread {
    start_public_api_thread_impl(g, true, false)
}
pub(crate) fn start_owned_public_api_thread(g: &mut NativeCodeGenerator) -> PublicApiThread {
    start_public_api_thread_impl(g, false, true)
}
fn start_public_api_thread_impl(
    g: &mut NativeCodeGenerator,
    inherited: bool,
    owned: bool,
) -> PublicApiThread {
    use crate::gc_abi::{
        NativeApiBinding as Binding, NativeThreadApi as Api, NativeThreadCall as Call,
        NativeThreadToken as Token,
    };
    let f = thread_api_fixture(g);
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let c = g.asm.data_label_with_i64s(&[0; 17]);
    let wrong_call = g.asm.data_label_with_i64s(&vec![0; size_of::<Call>() / 8]);
    g.mov_data_addr(R::V8, f.call);
    g.mov_data_addr(R::V9, wrong_call);
    for off in (0..size_of::<Call>()).step_by(8) {
        g.load_ptr_disp32(R::V0, R::V8, d(off));
        g.store_ptr_disp32(R::V9, d(off), R::V0);
    }
    let wrong = ThreadApiFixture {
        call: wrong_call,
        token: f.token,
    };
    let start = g.create_text_label();
    let join = g.create_text_label();
    let map = g.create_text_label();
    let unmap = g.create_text_label();
    let entry = g.create_text_label();
    let body = g.create_text_label();
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (fail, alloc, adopt, publish, load, relax) = (
        l.fail,
        l.alloc_actor,
        l.root_adopt_actor,
        l.publish_pending,
        l.root_load_actor,
        l.worker.as_ref().unwrap().hooks.relax,
    );
    g.jmp_label(body);
    g.bind_text_label(start);
    if owned {
        threads::emit_linux_owned_thread_start(g);
    } else {
        threads::emit_linux_worker_start(g);
    }
    g.bind_text_label(join);
    threads::emit_linux_worker_join(g);
    g.bind_text_label(map);
    threads::emit_linux_stack_alloc(g);
    g.bind_text_label(unmap);
    threads::emit_linux_stack_free(g);
    g.bind_text_label(entry);
    g.push_reg(R::V5);
    g.mov_reg_reg(R::V5, R::V4);
    g.sub_reg_imm32(R::V4, 2048);
    g.store_rbp_slot(8, R::V7);
    threads::emit_linux_thread_context(g, crate::Reg::R12); // initially empty
    g.mov_data_addr(R::V0, f.call);
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(Call, api)));
    g.load_ptr_disp32(R::V3, R::V0, d(offset_of!(Api, runtime)));
    g.asm.mov_imm64(crate::Reg::Rax, 186);
    g.asm.syscall();
    control(g, R::V1);
    g.store_ptr_disp32(R::V1, OS_TID, R::V0);
    if owned {
        // Deliberately postpone all runtime lookup until the parent releases
        // its real association/context. The launcher must not need that node.
        let wait = g.create_text_label();
        g.bind_text_label(wait);
        control(g, R::V1);
        g.add_reg_imm32(R::V1, ADMIT);
        g.atomic_load_acquire(R::V0, R::V1);
        g.test_reg_reg(R::V0, R::V0);
        let ready = g.create_text_label();
        g.jcc_label(C::NotEqual, ready);
        g.call_label(relax);
        g.jmp_label(wait);
        g.bind_text_label(ready);
        super::tls_query_value(g);
        control(g, R::V1);
        g.store_ptr_disp32(R::V1, 40, R::V0);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
    }
    if inherited {
        // Current must refuse the parent's inherited association; fresh Attach
        // then authenticates it and clears only this new OS thread's slot.
        thread_api_call(g, f, 3);
        control(g, R::V1);
        g.store_ptr_disp32(R::V1, 40, R::V0);
        g.cmp_reg_imm32(R::V0, 3);
        g.jcc_label(C::NotEqual, fail);
        super::thread_api_outputs_zero(g, f);
        thread_api_token_empty(g, f);
    }
    api_retry(g, f, 0);
    control(g, R::V1);
    g.store_ptr_disp32(R::V1, ATTACH, R::V0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V9, f.call);
    g.load_ptr_disp32(R::OwnerContext, R::V9, d(offset_of!(Call, context)));
    g.test_reg_reg(R::OwnerContext, R::OwnerContext);
    g.jcc_label(C::Equal, fail);
    control(g, R::V1);
    g.store_ptr_disp32(R::V1, 16, R::OwnerContext); // actual VM FS slot
    g.load_ptr_disp32(R::V0, R::V9, d(offset_of!(Call, binding)));
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(Binding, identity)));
    g.store_ptr_disp32(R::V1, IDENTITY, R::V0);
    g.mov_data_addr(R::V8, f.token);
    g.load_ptr_disp32(R::V0, R::V8, d(offset_of!(Token, thread)));
    g.store_ptr_disp32(R::V1, CREATOR, R::V0);
    // Actual managed code on this admitted owner creates a precise frame root.
    g.mov_imm64(R::V7, 24);
    g.mov_imm64(R::V6, 1);
    g.call_label(alloc);
    g.mov_imm64(R::V1, 84);
    g.store_ptr_disp32(R::V0, 0, R::V1);
    control(g, R::V1);
    g.add_reg_imm32(R::V1, ROOT);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.mov_reg_reg(R::V0, R::V1);
    g.call_label(adopt);
    g.call_label(publish);
    super::thread_api_boundary(g, f, 0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    control(g, R::V1);
    g.add_reg_imm32(R::V1, READY);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    let wait = g.create_text_label();
    let resume = g.create_text_label();
    g.bind_text_label(wait);
    control(g, R::V1);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, resume);
    g.call_label(relax);
    g.jmp_label(wait);
    g.bind_text_label(resume);
    thread_api_call(g, f, 3); // actual per-OS-thread association after Full
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V9, f.call);
    g.load_ptr_disp32(R::V0, R::V9, d(offset_of!(Call, context)));
    g.cmp_reg_reg(R::V0, R::OwnerContext);
    g.jcc_label(C::NotEqual, fail);
    super::thread_api_boundary(g, f, 1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    control(g, R::V1);
    g.load_ptr_disp32(R::V0, R::V1, ROOT);
    g.call_label(load);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    control(g, R::V1);
    g.store_ptr_disp32(R::V1, VALUE, R::V0);
    // No saved VM register/custom FS binding may outlive context release.
    threads::emit_linux_thread_unbind(g);
    api_retry(g, f, 2);
    control(g, R::V1);
    g.store_ptr_disp32(R::V1, DETACH, R::V0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    thread_api_token_empty(g, f);
    control(g, R::V1);
    g.add_reg_imm32(R::V1, DONE);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    g.leave();
    g.ret();
    g.bind_text_label(body);
    g.mov_imm64(R::V7, STACK_BYTES);
    g.call_label(map);
    g.cmp_reg_imm32(R::V0, -4095);
    g.jcc_label(C::AboveOrEqual, fail);
    g.mov_data_addr(R::V9, c);
    g.store_ptr_disp32(R::V9, STACK, R::V0);
    g.mov_reg_reg(R::V1, R::V9);
    g.add_reg_imm32(R::V1, 8);
    g.store_ptr_disp32(R::V9, 8, R::V1);
    g.asm.mov_reg_reg(crate::Reg::Rdi, crate::Reg::Rax);
    g.asm
        .add_reg_imm32(crate::Reg::Rdi, STACK_BYTES as i32 + 4096);
    g.asm.mov_reg_reg(crate::Reg::Rsi, crate::Reg::R9);
    g.asm.add_reg_imm32(crate::Reg::Rsi, 8);
    g.asm.lea_text_addr(crate::Reg::Rdx, entry);
    g.asm.mov_reg_reg(crate::Reg::Rcx, crate::Reg::R9);
    g.asm.mov_reg_reg(crate::Reg::R8, crate::Reg::R9);
    g.asm.add_reg_imm32(crate::Reg::R8, TID);
    if owned {
        g.asm.mov_reg_reg(crate::Reg::R9, crate::Reg::Rcx);
        g.asm.add_reg_imm32(crate::Reg::R9, BOOTSTRAP);
    }
    g.call_label(start);
    g.cmp_reg_imm32(R::V0, 0);
    g.jcc_label(C::LessEqual, fail);
    PublicApiThread {
        control: c,
        wrong,
        join,
        unmap,
    }
}
pub(crate) fn public_api_thread_admit(g: &mut NativeCodeGenerator, t: &PublicApiThread) {
    g.mov_data_addr(R::V1, t.control);
    g.add_reg_imm32(R::V1, ADMIT);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
}
pub(crate) fn public_api_thread_wait(g: &mut NativeCodeGenerator, t: &PublicApiThread, done: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, poll, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.poll_actor,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 30_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let ready = g.create_text_label();
    g.bind_text_label(again);
    g.call_label(poll);
    g.mov_data_addr(R::V1, t.control);
    g.add_reg_imm32(R::V1, if done { DONE } else { READY });
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, ready);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(ready);
}
pub(crate) fn public_api_thread_go(g: &mut NativeCodeGenerator, t: &PublicApiThread) {
    g.mov_data_addr(R::V1, t.control);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
}
pub(crate) fn public_api_thread_join(g: &mut NativeCodeGenerator, t: &PublicApiThread) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (enter, exit, fail) = (l.native_enter_actor, l.native_exit_actor, l.fail);
    g.call_label(enter);
    g.mov_data_addr(R::V7, t.control);
    g.add_reg_imm32(R::V7, TID);
    g.call_label(t.join);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_data_addr(R::V7, t.control);
    g.load_ptr_disp32(R::V7, R::V7, STACK);
    g.mov_imm64(R::V6, STACK_BYTES);
    g.call_label(t.unmap);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.call_label(exit);
}
pub(crate) fn public_api_thread_metric(
    g: &mut NativeCodeGenerator,
    t: &PublicApiThread,
    field: usize,
) {
    let fields = [
        ATTACH, DETACH, VALUE, OS_TID, CREATOR, IDENTITY, 16, 40, BOOTSTRAP,
    ];
    g.mov_data_addr(R::V1, t.control);
    g.load_ptr_disp32(R::V0, R::V1, fields[field]);
}
