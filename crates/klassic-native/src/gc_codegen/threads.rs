//! Linux x86-64 worker bootstrap, custom TLS and kernel-clear-tid join.
//! No libc or external linker. These leaf entry points use the SysV ABI.
use crate::{Condition, NativeCodeGenerator, Reg};

// CLONE_VM | FS | FILES | SIGHAND | THREAD | SYSVSEM | SETTLS |
// PARENT_SETTID | CHILD_CLEARTID. Kernel clear-tid uses a shared futex wake.
const GC_CLONE_FLAGS: u64 = 0x003d_0f00;

/// `(aligned_stack_end, tls, entry, context, tid_u32*) -> tid_or_negative_errno`.
/// TLS is `{ self_pointer: u64, context_pointer: u64 }`, owned by this runtime;
/// the child must never call libc using this custom FS base. The parent owns
/// stack/TLS/context/clear-tid memory through successful join. Stack is writable
/// with room for bootstrap words and call frames; caller supplies guard pages.
#[allow(dead_code)]
pub(crate) fn emit_linux_worker_start(out: &mut NativeCodeGenerator) {
    emit_start(out, GC_CLONE_FLAGS);
}

fn emit_start(out: &mut NativeCodeGenerator, flags: u64) {
    let a = &mut out.asm;
    let parent = a.create_text_label();
    // The new stack contains the entry and rooted worker context. Parent and
    // child return from clone on different stacks; no Rust/C frame is copied.
    a.sub_reg_imm32(Reg::Rdi, 16);
    a.store_ptr_disp32(Reg::Rdi, 0, Reg::Rdx);
    a.store_ptr_disp32(Reg::Rdi, 8, Reg::Rcx);
    a.mov_reg_reg(Reg::R9, Reg::Rsi);
    a.mov_reg_reg(Reg::Rsi, Reg::Rdi);
    a.mov_reg_reg(Reg::Rdx, Reg::R8); // parent_tid
    a.mov_reg_reg(Reg::R10, Reg::R8); // child clear_tid
    a.mov_reg_reg(Reg::R8, Reg::R9); // TLS base
    a.mov_imm64(Reg::Rdi, flags);
    a.mov_imm64(Reg::Rax, 56); // clone
    a.syscall();
    a.cmp_reg_imm8(Reg::Rax, 0);
    a.jcc_label(Condition::NotEqual, parent); // error also returns here
    a.pop_reg(Reg::Rax); // entry
    a.pop_reg(Reg::Rdi); // context, stack is again 16-byte aligned before call
    a.call_reg(Reg::Rax);
    a.mov_imm64(Reg::Rdi, 0);
    a.mov_imm64(Reg::Rax, 60); // exit THIS thread, never exit_group
    a.syscall();
    a.bytes(&[0x0f, 0x0b]); // exit cannot return
    a.bind_text_label(parent);
    a.ret();
}

/// `(tid_u32*) -> 0_or_negative_errno`. Context/stack must remain allocated.
/// Wait for Linux's clear-tid publication; tolerate EAGAIN and EINTR. This
/// function belongs in a blocked-native/worker state, not a heap-access guard.
#[allow(dead_code)]
pub(crate) fn emit_linux_worker_join(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let retry = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(retry);
    a.load_u32_ptr_disp32(Reg::Rdx, Reg::Rdi, 0); // acquire, exact kernel word width
    a.test_reg_reg(Reg::Rdx, Reg::Rdx);
    a.jcc_label(Condition::Equal, done);
    a.mov_imm64(Reg::Rax, 202); // futex
    a.mov_imm64(Reg::Rsi, 0); // FUTEX_WAIT, not PRIVATE (kernel clear-tid wake)
    a.mov_imm64(Reg::R10, 0); // no timeout
    a.syscall();
    a.cmp_reg_imm8(Reg::Rax, 0);
    a.jcc_label(Condition::Equal, retry);
    a.cmp_reg_imm8(Reg::Rax, -11); // EAGAIN: value changed before wait
    a.jcc_label(Condition::Equal, retry);
    a.cmp_reg_imm8(Reg::Rax, -4); // EINTR
    a.jcc_label(Condition::Equal, retry);
    a.ret(); // propagate errors instead of spinning forever
    a.bind_text_label(done);
    a.mov_imm64(Reg::Rax, 0);
    a.ret();
}

/// Load this native worker's context without a safepoint or foreign TLS lookup.
#[allow(dead_code)]
pub(crate) fn emit_linux_thread_context(out: &mut NativeCodeGenerator, dst: Reg) {
    out.asm.load_fs_u64(dst, 8);
}

/// `(usable_bytes) -> mapping_base_or_negative_errno`. Linux x86-64 base pages
/// are 4096 bytes. Require a nonzero page multiple; reserve two extra PROT_NONE
/// guard pages and make only the interior writable. Stack end is base + 4096 +
/// usable_bytes. Release base/usable_bytes+8192 only after successful join.
#[allow(dead_code)]
pub(crate) fn emit_linux_stack_alloc(out: &mut NativeCodeGenerator) {
    emit_stack_alloc(out, 0x20022, 3);
}

fn emit_stack_alloc(out: &mut NativeCodeGenerator, map_flags: u64, protect: u64) {
    let a = &mut out.asm;
    let invalid = a.create_text_label();
    let overflow = a.create_text_label();
    let finish = a.create_text_label();
    let cleanup = a.create_text_label();
    a.push_reg(Reg::Rbx);
    a.push_reg(Reg::R12);
    a.test_reg_reg(Reg::Rdi, Reg::Rdi);
    a.jcc_label(Condition::Equal, invalid);
    a.mov_reg_reg(Reg::Rax, Reg::Rdi);
    a.and_reg_imm32(Reg::Rax, 4095);
    a.jcc_label(Condition::NotEqual, invalid);
    a.mov_imm64(Reg::Rax, 0x7fff_ffff_ffff_d000);
    a.cmp_reg_reg(Reg::Rdi, Reg::Rax);
    a.jcc_label(Condition::Above, overflow);
    a.mov_reg_reg(Reg::R12, Reg::Rdi);
    a.add_reg_imm32(Reg::R12, 8192);
    a.mov_reg_reg(Reg::Rsi, Reg::R12);
    a.mov_imm64(Reg::Rdi, 0);
    a.mov_imm64(Reg::Rdx, 0); // PROT_NONE
    a.mov_imm64(Reg::R10, map_flags); // MAP_STACK | PRIVATE | ANONYMOUS
    a.mov_imm64(Reg::R8, u64::MAX); // fd = -1
    a.mov_imm64(Reg::R9, 0);
    a.mov_imm64(Reg::Rax, 9); // mmap
    a.syscall();
    a.cmp_reg_imm32(Reg::Rax, -4095);
    a.jcc_label(Condition::AboveOrEqual, finish);
    a.mov_reg_reg(Reg::Rbx, Reg::Rax);
    a.mov_reg_reg(Reg::Rdi, Reg::Rax);
    a.add_reg_imm32(Reg::Rdi, 4096);
    a.mov_reg_reg(Reg::Rsi, Reg::R12);
    a.sub_reg_imm32(Reg::Rsi, 8192);
    a.mov_imm64(Reg::Rdx, protect); // PROT_READ | PROT_WRITE
    a.mov_imm64(Reg::Rax, 10); // mprotect
    a.syscall();
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::NotEqual, cleanup);
    a.mov_reg_reg(Reg::Rax, Reg::Rbx);
    a.jmp_label(finish);
    a.bind_text_label(cleanup);
    a.mov_reg_reg(Reg::R8, Reg::Rax); // preserve the original error
    a.mov_reg_reg(Reg::Rdi, Reg::Rbx);
    a.mov_reg_reg(Reg::Rsi, Reg::R12);
    a.mov_imm64(Reg::Rax, 11); // munmap the failed reservation
    a.syscall();
    a.mov_reg_reg(Reg::Rax, Reg::R8);
    a.jmp_label(finish);
    a.bind_text_label(invalid);
    a.mov_imm64(Reg::Rax, (-22_i64) as u64);
    a.jmp_label(finish);
    a.bind_text_label(overflow);
    a.mov_imm64(Reg::Rax, (-75_i64) as u64);
    a.bind_text_label(finish);
    a.pop_reg(Reg::R12);
    a.pop_reg(Reg::Rbx);
    a.ret();
}

/// `(mapping_base, mapping_bytes) -> 0_or_negative_errno`.
#[allow(dead_code)]
pub(crate) fn emit_linux_stack_free(out: &mut NativeCodeGenerator) {
    out.asm.mov_imm64(Reg::Rax, 11);
    out.asm.syscall();
    out.asm.ret();
}

/// `(aligned_u32_word*, expected_u32) -> 0_or_negative_errno`.
/// Compare and sleep is atomic in the kernel. Callers must recheck their
/// acquire predicate on return (including spurious wakes, EAGAIN and EINTR).
/// Never park while holding a heap-access guard. Private futexes are suitable
/// here; kernel clear-tid join above deliberately uses the shared operation.
#[allow(dead_code)]
pub(crate) fn emit_linux_worker_park(out: &mut NativeCodeGenerator) {
    emit_linux_park_body(out);
    out.asm.ret();
}

pub(crate) fn emit_linux_park_body(out: &mut NativeCodeGenerator) {
    out.asm.mov_reg_reg(Reg::Rdx, Reg::Rsi);
    out.asm.mov_imm64(Reg::Rsi, 128); // FUTEX_WAIT_PRIVATE
    out.asm.mov_imm64(Reg::R10, 0);
    out.asm.mov_imm64(Reg::Rax, 202);
    out.asm.syscall();
}

/// `(aligned_u32_word*, max_waiters) -> woken_count_or_negative_errno`.
/// Publish the predicate with release semantics before waking. A zero result
/// is valid: a waiter racing with publication must observe the changed word.
#[allow(dead_code)]
pub(crate) fn emit_linux_worker_wake(out: &mut NativeCodeGenerator) {
    out.asm.mov_reg_reg(Reg::Rdx, Reg::Rsi);
    out.asm.mov_imm64(Reg::Rsi, 129); // FUTEX_WAKE_PRIVATE
    out.asm.mov_imm64(Reg::Rax, 202);
    out.asm.syscall();
    out.asm.ret();
}

/// `(aligned_u32_lock*) -> 0_or_negative_errno`. Zero is unlocked, one is
/// locked. Acquire by exact-width CAS; contention sleeps on a private futex.
/// This is for registry metadata, never an object-access or barrier fast path.
/// The caller owns lock storage until all possible users have joined.
#[allow(dead_code)]
pub(crate) fn emit_linux_registry_lock(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let retry = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(retry);
    a.mov_imm64(Reg::Rax, 0);
    a.mov_imm64(Reg::Rdx, 1);
    a.lock_cmpxchg_u32(Reg::Rdi, Reg::Rdx);
    a.jcc_label(Condition::Equal, done);
    a.mov_imm64(Reg::Rax, 202);
    a.mov_imm64(Reg::Rsi, 128); // FUTEX_WAIT_PRIVATE(lock, 1)
    a.mov_imm64(Reg::R10, 0);
    a.syscall();
    for status in [0, -11, -4] {
        // wake, EAGAIN, EINTR
        a.cmp_reg_imm8(Reg::Rax, status);
        a.jcc_label(Condition::Equal, retry);
    }
    a.ret(); // failure: caller does not own the lock
    a.bind_text_label(done);
    a.ret(); // successful CMPXCHG returns zero, with acquire ordering
}

/// `(owned_u32_lock*) -> 0_or_negative_errno`. Release and wake one contender.
/// Ownership has ended even if the wake syscall fails. Callers must diagnose
/// such errors; silently continuing could leave a waiter asleep indefinitely.
#[allow(dead_code)]
pub(crate) fn emit_linux_registry_unlock(out: &mut NativeCodeGenerator) {
    emit_linux_word_signal(out, 0);
}

/// `(single_waiter_u32_event*) -> 0_or_negative_errno`. Sticky binary event.
/// Under the registry lock, a waiter checks its predicate and clears its OWN
/// event before unlocking and FUTEX_WAIT(0). The notifier sets it under that
/// same lock. A wake before park therefore returns EAGAIN instead of being lost.
/// Each owner and the coordinator need separate events: sharing an event among
/// predicate waiters permits one waiter to consume another's notification.
#[allow(dead_code)]
pub(crate) fn emit_linux_registry_notify(out: &mut NativeCodeGenerator) {
    emit_linux_word_signal(out, 1);
}

fn emit_linux_word_signal(out: &mut NativeCodeGenerator, value: u64) {
    let a = &mut out.asm;
    let done = a.create_text_label();
    a.mov_imm64(Reg::Rax, value);
    a.store_u32_ptr_disp32(Reg::Rdi, 0, Reg::Rax); // release on x86 TSO
    a.mov_imm64(Reg::Rdx, 1);
    a.mov_imm64(Reg::Rsi, 129); // FUTEX_WAKE_PRIVATE
    a.mov_imm64(Reg::Rax, 202);
    a.syscall();
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Less, done);
    a.mov_imm64(Reg::Rax, 0);
    a.bind_text_label(done);
    a.ret();
}

#[cfg(test)]
pub(crate) fn emit_linux_worker_start_failure(out: &mut NativeCodeGenerator) {
    // CLONE_THREAD without CLONE_SIGHAND is an intentional EINVAL injection.
    emit_start(out, 0x0001_0000);
}

#[cfg(test)]
pub(crate) fn emit_linux_stack_map_failure(out: &mut NativeCodeGenerator) {
    emit_stack_alloc(out, 2, 3); // no anonymous flag and fd = -1: EBADF
}

#[cfg(test)]
pub(crate) fn emit_linux_stack_protect_failure(out: &mut NativeCodeGenerator) {
    emit_stack_alloc(out, 0x20022, 0x8000_0003); // invalid protection: EINVAL
}
