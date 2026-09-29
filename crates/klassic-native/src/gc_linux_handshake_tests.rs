//! A libc-free owner running the shared protocol on a real raw-clone thread.
//! Its parent test orchestrates requests; every worker instruction and every
//! lock/notification/park primitive below is emitted, including TLS lookup.
use super::{finish_code, generator};
use crate::gc_abi::{HandshakeState, ThreadRegistration};
use crate::gc_codegen::{context, handshake, threads};
use crate::{Condition as C, NativeCodeGenerator, Reg as R, TextLabel};
use std::mem::offset_of;

// C fixture NativeOwnerControl has nine u64 fields, with static offset checks.
const HANDSHAKE: i32 = 0;
const REGISTRATION: i32 = 8;
const LOCK: i32 = 16;
const COORDINATOR_EVENT: i32 = 24;
const OWNER_EVENT: i32 = 32;
const STOP: i32 = 40;
const PROGRESS: i32 = 48;
const TLS_OBSERVED: i32 = 56;
const ENTRY_SP: i32 = 64;

fn call_checked(g: &mut NativeCodeGenerator, target: TextLabel, fail: TextLabel) {
    g.asm.call_label(target);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, fail);
}
fn control_call(g: &mut NativeCodeGenerator, field: i32, target: TextLabel, fail: TextLabel) {
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, field);
    call_checked(g, target, fail);
}
fn step(g: &mut NativeCodeGenerator, operation: u64, target: TextLabel) {
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, HANDSHAKE);
    g.asm.load_ptr_disp32(R::Rsi, R::Rbx, REGISTRATION);
    g.asm.mov_imm64(R::Rdx, operation);
    g.asm.call_label(target);
}

pub(super) fn worker_code() -> Vec<u8> {
    let mut g = generator();
    let lock = g.asm.create_text_label();
    let unlock = g.asm.create_text_label();
    let notify = g.asm.create_text_label();
    let park = g.asm.create_text_label();
    let hs_step = g.asm.create_text_label();
    let transition = g.asm.create_text_label();
    let fail = g.asm.create_text_label();
    let poll = g.asm.create_text_label();
    let running = g.asm.create_text_label();
    let wait = g.asm.create_text_label();
    let relock = g.asm.create_text_label();
    let detach = g.asm.create_text_label();
    // RBX is control, R12 snapshots the generation, R13 keeps ABI alignment.
    g.asm.store_ptr_disp32(R::Rdi, ENTRY_SP, R::Rsp);
    for reg in [R::Rbx, R::R12, R::R13] {
        g.asm.push_reg(reg);
    }
    g.asm.mov_reg_reg(R::Rbx, R::Rdi);
    threads::emit_linux_thread_context(&mut g, R::Rax);
    g.asm.store_ptr_disp32(R::Rbx, TLS_OBSERVED, R::Rax);
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, REGISTRATION);
    g.asm.load_ptr_disp32(
        R::Rdi,
        R::Rdi,
        offset_of!(ThreadRegistration, context) as i32,
    );
    g.asm.cmp_reg_reg(R::Rax, R::Rdi);
    g.asm.jcc_label(C::NotEqual, fail);
    control_call(&mut g, LOCK, lock, fail);
    step(&mut g, 0, hs_step); // register fresh context
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, fail);
    control_call(&mut g, COORDINATOR_EVENT, notify, fail);

    g.asm.bind_text_label(poll);
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, HANDSHAKE);
    g.asm
        .load_ptr_disp32(R::Rax, R::Rdi, offset_of!(HandshakeState, active) as i32);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Equal, running);
    step(&mut g, 1, hs_step); // first or duplicate acknowledgement
    g.asm.cmp_reg_imm8(R::Rax, 4);
    g.asm.jcc_label(C::Equal, wait);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, fail);
    control_call(&mut g, COORDINATOR_EVENT, notify, fail);
    g.asm.jmp_label(wait);

    g.asm.bind_text_label(running);
    step(&mut g, 2, hs_step); // resume before test work / detach
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, fail);
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, STOP);
    g.asm.load_u32_ptr_disp32(R::Rax, R::Rdi, 0);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, detach);
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, HANDSHAKE);
    g.asm.load_ptr_disp32(
        R::R12,
        R::Rdi,
        offset_of!(HandshakeState, generation) as i32,
    );
    control_call(&mut g, LOCK, unlock, fail);
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, PROGRESS);
    g.asm.store_ptr_disp32(R::Rdi, 0, R::R12); // release; outside registry lock
    control_call(&mut g, LOCK, lock, fail);
    control_call(&mut g, COORDINATOR_EVENT, notify, fail);
    // A request may have started while progress was published outside the lock.
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, HANDSHAKE);
    g.asm
        .load_ptr_disp32(R::Rax, R::Rdi, offset_of!(HandshakeState, active) as i32);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, poll);
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, STOP);
    g.asm.load_u32_ptr_disp32(R::Rax, R::Rdi, 0);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, detach);

    g.asm.bind_text_label(wait);
    // Reset only this owner's event, while still holding the registry lock.
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, OWNER_EVENT);
    g.asm.mov_imm64(R::Rax, 0);
    g.asm.store_u32_ptr_disp32(R::Rdi, 0, R::Rax);
    control_call(&mut g, LOCK, unlock, fail);
    g.asm.load_ptr_disp32(R::Rdi, R::Rbx, OWNER_EVENT);
    g.asm.mov_imm64(R::Rsi, 0);
    g.asm.call_label(park);
    for status in [0, -11, -4] {
        g.asm.cmp_reg_imm8(R::Rax, status);
        g.asm.jcc_label(C::Equal, relock);
    }
    g.asm.jmp_label(fail);
    g.asm.bind_text_label(relock);
    control_call(&mut g, LOCK, lock, fail);
    g.asm.jmp_label(poll);

    g.asm.bind_text_label(detach);
    threads::emit_linux_thread_context(&mut g, R::Rdi);
    g.asm.mov_imm64(R::Rsi, 0);
    g.asm.mov_imm64(R::Rdx, 3);
    call_checked(&mut g, transition, fail);
    step(&mut g, 5, hs_step);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, fail);
    control_call(&mut g, COORDINATOR_EVENT, notify, fail);
    control_call(&mut g, LOCK, unlock, fail);
    for reg in [R::R13, R::R12, R::Rbx] {
        g.asm.pop_reg(reg);
    }
    g.asm.ret(); // raw clone trampoline exits and publishes kernel clear-tid
    g.asm.bind_text_label(fail);
    g.asm.mov_imm64(R::Rdi, 91);
    g.asm.mov_imm64(R::Rax, 231); // fail the whole test immediately, never hang
    g.asm.syscall();
    g.asm.bytes(&[0x0f, 0x0b]);

    g.asm.bind_text_label(lock);
    threads::emit_linux_registry_lock(&mut g);
    g.asm.bind_text_label(unlock);
    threads::emit_linux_registry_unlock(&mut g);
    g.asm.bind_text_label(notify);
    threads::emit_linux_registry_notify(&mut g);
    g.asm.bind_text_label(park);
    threads::emit_linux_worker_park(&mut g);
    g.asm.bind_text_label(hs_step);
    handshake::emit_step_locked(&mut g);
    g.asm.ret();
    g.asm.bind_text_label(transition);
    context::emit_transition_locked(&mut g);
    g.asm.ret();
    finish_code(g)
}
