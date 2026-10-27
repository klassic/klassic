//! Owned Linux production worker stack/TLS and kernel-clear-tid joining.
//! The worker uses raw syscalls only; its private FS base is not libc TLS.
#[cfg(test)]
use super::production::{linux_worker_offset, worker_slot_identity_offset};
use super::{
    linux_clock,
    production::{WorkerHooks, worker_slot_linux_offset, worker_slot_runtime_offset},
    threads,
};
use crate::{Condition as C, NativeCodeGenerator, Reg as R, TextLabel};
use std::mem::offset_of;

pub(crate) const STACK_BYTES: u64 = 256 << 10;
#[cfg(test)]
#[derive(Clone, Copy)]
pub(crate) enum Fault {
    Clone,
    Map,
    Protect,
    Join,
    Free,
    CloneCleanup,
    Bootstrap,
    SecondClone,
    SecondBootstrap,
}

#[repr(C)]
pub(crate) struct State {
    pub stack: u64,
    pub tls: [u64; 2],
    pub tid: u64, // kernel accesses exactly the low u32
    pub observed_tls: u64,
    pub entry_sp: u64,
    pub joins: u64,
    pub releases: u64,
    /// Diagnostic address only; never dereference it after release.
    pub last_stack: u64,
    pub bootstrap: u64,
}
// Test introspection retains absolute offsets for slot zero. Production OS
// leaves instead address the selected stable slot through OwnerContext/R12.
#[cfg(test)]
fn field(member: usize) -> i32 {
    linux_worker_offset() + member as i32
}
fn slot_field(member: usize) -> i32 {
    worker_slot_linux_offset() + member as i32
}
#[cfg(test)]
pub(crate) fn last_stack_offset() -> i32 {
    field(offset_of!(State, last_stack))
}
#[cfg(test)]
pub(crate) fn bootstrap_offset() -> i32 {
    field(offset_of!(State, bootstrap))
}
#[cfg(test)]
pub(crate) fn offsets() -> [i32; 6] {
    [
        offset_of!(State, stack),
        offset_of!(State, tid),
        offset_of!(State, observed_tls),
        offset_of!(State, entry_sp),
        offset_of!(State, joins),
        offset_of!(State, releases),
    ]
    .map(field)
}

pub(crate) fn emit(g: &mut NativeCodeGenerator, h: &WorkerHooks<TextLabel>, entry: TextLabel) {
    let [stack, tid, observed, entry_sp, joins, releases] = [
        offset_of!(State, stack),
        offset_of!(State, tid),
        offset_of!(State, observed_tls),
        offset_of!(State, entry_sp),
        offset_of!(State, joins),
        offset_of!(State, releases),
    ]
    .map(slot_field);
    let tls = slot_field(offset_of!(State, tls));
    let alloc = g.asm.create_text_label();
    let start = g.asm.create_text_label();
    let join = g.asm.create_text_label();
    let free = g.asm.create_text_label();
    let callback = g.asm.create_text_label();
    let failed = g.asm.create_text_label();
    let rollback = g.asm.create_text_label();
    let invalid = g.asm.create_text_label();
    let allocation_failed = g.asm.create_text_label();
    g.asm.bind_text_label(h.spawn);
    #[cfg(test)]
    if g.gc_test.is_some_and(|t| t.no_worker_thread) {
        g.asm.mov_imm64(R::Rax, 0);
        g.asm.ret();
    }
    g.asm.mov_imm64(R::Rdi, STACK_BYTES);
    g.asm.call_label(alloc);
    g.asm.cmp_reg_imm32(R::Rax, -4095);
    g.asm.jcc_label(C::AboveOrEqual, allocation_failed);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Equal, allocation_failed);
    g.asm.store_ptr_disp32(R::R12, stack, R::Rax);
    g.asm
        .store_ptr_disp32(R::R12, slot_field(offset_of!(State, last_stack)), R::Rax);
    g.asm.mov_reg_reg(R::Rdi, R::Rax);
    g.asm.add_reg_imm32(R::Rdi, (STACK_BYTES + 4096) as i32);
    g.asm.mov_reg_reg(R::Rsi, R::R12);
    g.asm.add_reg_imm32(R::Rsi, tls);
    g.asm.store_ptr_disp32(R::Rsi, 0, R::Rsi);
    g.asm.store_ptr_disp32(R::Rsi, 8, R::R12);
    g.asm.lea_text_addr(R::Rdx, callback);
    g.asm.mov_reg_reg(R::Rcx, R::R12);
    g.asm.mov_reg_reg(R::R8, R::R12);
    g.asm.add_reg_imm32(R::R8, tid);
    g.asm.mov_reg_reg(R::R9, R::R12);
    g.asm
        .add_reg_imm32(R::R9, slot_field(offset_of!(State, bootstrap)));
    g.asm.call_label(start);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Less, rollback);
    g.asm.jcc_label(C::Equal, invalid);
    // Child bootstrap touches only its OS association and the retained status
    // word, so it cannot need a collector ticket held by the spawning thread.
    let bootstrap_wait = g.asm.create_text_label();
    let bootstrap_failed = g.asm.create_text_label();
    let bootstrap_ready = g.asm.create_text_label();
    g.asm.bind_text_label(bootstrap_wait);
    g.asm
        .load_ptr_disp32(R::Rax, R::R12, slot_field(offset_of!(State, bootstrap)));
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Less, bootstrap_failed);
    g.asm.jcc_label(C::NotEqual, bootstrap_ready);
    g.asm.mov_imm64(R::Rax, 24); // sched_yield, no heap access or GC callback
    g.asm.syscall();
    g.asm.jmp_label(bootstrap_wait);
    g.asm.bind_text_label(bootstrap_failed);
    g.asm.mov_reg_reg(R::Rdi, R::R12);
    g.asm.add_reg_imm32(R::Rdi, tid);
    g.asm.call_label(join);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, invalid);
    bump(g, joins);
    g.asm.jmp_label(rollback);
    g.asm.bind_text_label(bootstrap_ready);
    // Parent-tid is the receipt that prevents a not-yet-started child from
    // looking already terminated to the join fast path.
    g.asm.load_u32_ptr_disp32(R::Rcx, R::R12, tid);
    g.asm.test_reg_reg(R::Rcx, R::Rcx);
    g.asm.jcc_label(C::Equal, invalid);
    g.asm.mov_imm64(R::Rax, 1);
    g.asm.ret();
    g.asm.bind_text_label(rollback);
    g.asm.load_ptr_disp32(R::Rdi, R::R12, stack);
    g.asm.mov_imm64(R::Rsi, STACK_BYTES + 8192);
    g.asm.call_label(free);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, invalid);
    g.asm.store_ptr_disp32(R::R12, stack, R::Rax);
    bump(g, releases);
    g.asm.bind_text_label(failed);
    g.asm.mov_imm64(R::Rax, 0);
    g.asm.ret();

    if let Some(label) = h.join {
        g.asm.bind_text_label(label);
        g.asm.load_ptr_disp32(R::Rax, R::R12, stack);
        g.asm.test_reg_reg(R::Rax, R::Rax);
        g.asm.jcc_label(C::Equal, invalid);
        g.asm.mov_reg_reg(R::Rdi, R::R12);
        g.asm.add_reg_imm32(R::Rdi, tid);
        g.asm.call_label(join);
        g.asm.test_reg_reg(R::Rax, R::Rax);
        g.asm.jcc_label(C::NotEqual, invalid);
        bump(g, joins);
        g.asm.load_ptr_disp32(R::Rdi, R::R12, stack);
        g.asm.mov_imm64(R::Rsi, STACK_BYTES + 8192);
        g.asm.call_label(free);
        g.asm.test_reg_reg(R::Rax, R::Rax);
        g.asm.jcc_label(C::NotEqual, invalid);
        g.asm.store_ptr_disp32(R::R12, stack, R::Rax);
        bump(g, releases);
        g.asm.ret();
    }
    g.asm.bind_text_label(callback);
    // start's trampoline passes this worker's stable slot, never a shared
    // singleton. Bind both common registers before any collector operation.
    g.asm.mov_reg_reg(R::R12, R::Rdi);
    g.asm
        .load_ptr_disp32(R::Rbx, R::R12, worker_slot_runtime_offset());
    g.asm.load_fs_u64(R::Rax, 0);
    g.asm.mov_reg_reg(R::Rcx, R::R12);
    g.asm.add_reg_imm32(R::Rcx, tls);
    g.asm.cmp_reg_reg(R::Rax, R::Rcx);
    g.asm.jcc_label(C::NotEqual, invalid);
    threads::emit_linux_thread_context(g, R::Rax);
    g.asm.cmp_reg_reg(R::Rax, R::R12);
    g.asm.jcc_label(C::NotEqual, invalid);
    g.asm.store_ptr_disp32(R::R12, observed, R::Rax);
    g.asm.store_ptr_disp32(R::R12, entry_sp, R::Rsp);
    g.asm.push_reg(R::Rdi); // align before the common worker call
    g.asm.call_label(entry);
    g.asm.pop_reg(R::Rdi);
    g.asm.ret(); // start's trampoline exits only this thread

    g.asm.bind_text_label(alloc);
    emit_alloc(g);
    g.asm.bind_text_label(start);
    emit_start(g);
    g.asm.bind_text_label(join);
    emit_join(g);
    g.asm.bind_text_label(free);
    #[cfg(test)]
    if matches!(
        g.linux_worker_fault,
        Some(Fault::Free | Fault::CloneCleanup)
    ) {
        g.asm.add_reg_imm32(R::Rdi, 1); // real munmap EINVAL, retain the mapping
    }
    threads::emit_linux_stack_free(g);
    linux_clock::emit(g, h.clock);
    emit_waits(g, h);
    g.asm.bind_text_label(invalid);
    g.emit_runtime_error_text("GC worker lifecycle failed\n");
    g.asm.bind_text_label(allocation_failed);
    // The allocation helper has attempted rollback; failure is terminal, so
    // no surviving collector can lose an unreleased guard reservation.
    g.emit_runtime_error_text("GC worker stack allocation failed\n");
}
fn bump(g: &mut NativeCodeGenerator, offset: i32) {
    g.asm.load_ptr_disp32(R::Rax, R::R12, offset);
    g.asm.add_reg_imm32(R::Rax, 1);
    g.asm.store_ptr_disp32(R::R12, offset, R::Rax);
}
fn emit_waits(g: &mut NativeCodeGenerator, h: &WorkerHooks<TextLabel>) {
    for (label, op, value) in [(h.park, 128, None), (h.unpark, 129, Some(i32::MAX as u64))] {
        g.asm.bind_text_label(label);
        if let Some(count) = value {
            g.asm.mov_imm64(R::Rdx, count);
        } else {
            g.asm.mov_reg_reg(R::Rdx, R::Rsi);
        }
        g.asm.mov_imm64(R::Rsi, op);
        if value.is_none() {
            g.asm.sub_reg_imm32(R::Rsp, 16);
            g.asm.mov_imm64(R::Rax, 1);
            g.asm.store_ptr_disp32(R::Rsp, 0, R::Rax);
            g.asm.mov_imm64(R::Rax, 0);
            g.asm.store_ptr_disp32(R::Rsp, 8, R::Rax);
            g.asm.mov_reg_reg(R::R10, R::Rsp);
        } else {
            g.asm.mov_imm64(R::R10, 0);
        }
        g.asm.mov_imm64(R::Rax, 202);
        g.asm.syscall();
        if value.is_none() {
            g.asm.add_reg_imm32(R::Rsp, 16);
        }
        g.asm.ret();
    }
    g.asm.bind_text_label(h.relax);
    for reg in [R::Rax, R::Rcx, R::R11] {
        g.asm.push_reg(reg);
    }
    g.asm.mov_imm64(R::Rax, 24);
    g.asm.syscall();
    for reg in [R::R11, R::Rcx, R::Rax] {
        g.asm.pop_reg(reg);
    }
    g.asm.ret();
}

fn emit_alloc(g: &mut NativeCodeGenerator) {
    #[cfg(test)]
    match g.linux_worker_fault {
        Some(Fault::Map) => {
            threads::emit_linux_stack_map_failure(g);
            return;
        }
        Some(Fault::Protect) => {
            threads::emit_linux_stack_protect_failure(g);
            return;
        }
        _ => {}
    }
    threads::emit_linux_stack_alloc(g);
}
fn emit_start(g: &mut NativeCodeGenerator) {
    #[cfg(test)]
    {
        if matches!(g.linux_worker_fault, Some(Fault::Bootstrap)) {
            threads::emit_linux_owned_thread_start_failure(g);
            return;
        }
        if matches!(
            g.linux_worker_fault,
            Some(Fault::Clone | Fault::CloneCleanup)
        ) {
            threads::emit_linux_worker_start_failure(g);
            return;
        }
        if matches!(
            g.linux_worker_fault,
            Some(Fault::SecondClone | Fault::SecondBootstrap)
        ) {
            let normal = g.asm.create_text_label();
            g.asm
                .load_ptr_disp32(R::Rax, R::R12, worker_slot_identity_offset());
            g.asm.cmp_reg_imm32(R::Rax, 1);
            g.asm.jcc_label(C::NotEqual, normal);
            if matches!(g.linux_worker_fault, Some(Fault::SecondClone)) {
                threads::emit_linux_worker_start_failure(g);
            } else {
                threads::emit_linux_owned_thread_start_failure(g);
            }
            g.asm.bind_text_label(normal);
        }
    }
    threads::emit_linux_owned_thread_start(g);
}
fn emit_join(g: &mut NativeCodeGenerator) {
    #[cfg(test)]
    if matches!(g.linux_worker_fault, Some(Fault::Join)) {
        g.asm.mov_imm64(R::Rsi, 0x7fff); // unsupported futex operation, actual kernel error
        g.asm.mov_imm64(R::Rdx, 0);
        g.asm.mov_imm64(R::R10, 0);
        g.asm.mov_imm64(R::Rax, 202);
        g.asm.syscall();
        g.asm.ret();
        return;
    }
    threads::emit_linux_worker_join(g);
}
