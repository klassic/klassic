//! Win64 worker leaf entry points and OS-owned TLS/events. These are exercised
//! separately before connection to the default collector. No CRT is required.
use crate::{Condition, ImportSymbol as I, NativeCodeGenerator, Reg, TextLabel};

/// Immutable launch metadata, owned by the parent through join. `context` points
/// to a worker payload (or the common MutatorContext), not a second context ABI.
/// The entry is `DWORD WINAPI entry(void *context)`. A process-owned TLS index
/// must outlive every worker using it; allocate/free it outside concurrent use.
#[repr(C)]
#[allow(dead_code)]
pub(crate) struct WorkerLaunch {
    pub entry: u64,
    pub context: u64,
    pub tls_index: u32,
    pub reserved: u32,
}

/// `(launch*, stack_reservation_bytes) -> HANDLE_or_NULL`, using native Win64
/// register arguments. On NULL, GetLastError remains available to the caller.
/// Thread startup may precede this function's return. The caller must join even
/// if child TLS initialization fails, and inspect the child's exit code.
#[allow(dead_code)]
pub(crate) fn emit_worker_start(out: &mut NativeCodeGenerator, thunk: TextLabel) {
    let a = &mut out.asm;
    a.sub_reg_imm32(Reg::Rsp, 56); // aligned shadow space + two stack arguments
    a.mov_reg_reg(Reg::R9, Reg::Rcx);
    a.lea_text_addr(Reg::R8, thunk);
    a.mov_imm64(Reg::Rcx, 0);
    a.mov_imm64(Reg::Rax, 0x10000); // STACK_SIZE_PARAM_IS_A_RESERVATION
    a.store_ptr_disp32(Reg::Rsp, 32, Reg::Rax);
    a.mov_imm64(Reg::Rax, 0);
    a.store_ptr_disp32(Reg::Rsp, 40, Reg::Rax);
    a.call_import(I::CreateThread);
    a.add_reg_imm32(Reg::Rsp, 56);
    a.ret();
}

/// OS thread callback: initialize TLS, call the entry, clear TLS, then return
/// the DWORD result to Windows' thread wrapper (which exits this thread only).
/// TLS failure bypasses the entry and returns GetLastError as the exit code.
#[allow(dead_code)]
pub(crate) fn emit_worker_thunk(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let error = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(Reg::Rbx);
    a.sub_reg_imm32(Reg::Rsp, 48); // shadow + saved entry result
    a.mov_reg_reg(Reg::Rbx, Reg::Rcx);
    a.load_u32_ptr_disp32(Reg::Rcx, Reg::Rbx, 16);
    a.load_ptr_disp32(Reg::Rdx, Reg::Rbx, 8);
    a.call_import(I::TlsSetValue);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, error);
    a.load_ptr_disp32(Reg::Rax, Reg::Rbx, 0);
    a.load_ptr_disp32(Reg::Rcx, Reg::Rbx, 8);
    a.call_reg(Reg::Rax);
    a.store_ptr_disp32(Reg::Rsp, 32, Reg::Rax);
    a.load_u32_ptr_disp32(Reg::Rcx, Reg::Rbx, 16);
    a.mov_imm64(Reg::Rdx, 0);
    a.call_import(I::TlsSetValue);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, error);
    a.load_u32_ptr_disp32(Reg::Rax, Reg::Rsp, 32);
    a.jmp_label(done);
    a.bind_text_label(error);
    a.call_import(I::GetLastError);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.bind_text_label(done);
    a.add_reg_imm32(Reg::Rsp, 48);
    a.pop_reg(Reg::Rbx);
    a.ret();
}

/// `(thread_handle, exit_code_u32*) -> Windows_error_or_0`. Closes the handle
/// only after successful wait and exit-code retrieval. On failure the caller
/// retains ownership and may retry. Call outside heap-access guards.
#[allow(dead_code)]
pub(crate) fn emit_worker_join(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let error = a.create_text_label();
    let waited = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(Reg::Rbx);
    a.push_reg(Reg::R12);
    a.sub_reg_imm32(Reg::Rsp, 40);
    a.mov_reg_reg(Reg::Rbx, Reg::Rcx);
    a.mov_reg_reg(Reg::R12, Reg::Rdx);
    a.mov_imm64(Reg::Rdx, u32::MAX as u64);
    a.call_import(I::WaitForSingleObject);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, waited);
    a.mov_imm64(Reg::Rcx, u32::MAX as u64);
    a.cmp_reg_reg(Reg::Rax, Reg::Rcx);
    a.jcc_label(Condition::Equal, error); // WAIT_FAILED
    a.mov_imm64(Reg::Rax, 6); // non-thread handle / unexpected wait result
    a.jmp_label(done);
    a.bind_text_label(waited);
    a.mov_reg_reg(Reg::Rcx, Reg::Rbx);
    a.mov_reg_reg(Reg::Rdx, Reg::R12);
    a.call_import(I::GetExitCodeThread);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, error);
    a.mov_reg_reg(Reg::Rcx, Reg::Rbx);
    a.call_import(I::CloseHandle);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, error);
    a.mov_imm64(Reg::Rax, 0);
    a.jmp_label(done);
    a.bind_text_label(error);
    a.call_import(I::GetLastError);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.bind_text_label(done);
    a.add_reg_imm32(Reg::Rsp, 40);
    a.pop_reg(Reg::R12);
    a.pop_reg(Reg::Rbx);
    a.ret();
}

/// One auto-reset event per worker: SetEvent remembers a notification made
/// before WaitForSingleObject. Notifications may coalesce; the worker must
/// recheck its acquire predicate before/after waiting. Do not share one event
/// among workers needing a broadcast; signal each worker's event instead.
#[allow(dead_code)]
pub(crate) fn emit_event_create(out: &mut NativeCodeGenerator) {
    for reg in [Reg::Rcx, Reg::Rdx, Reg::R8, Reg::R9] {
        out.asm.mov_imm64(reg, 0);
    }
    emit_import_leaf(out, I::CreateEventA, false);
}

/// Emit native Win64 arguments/results with aligned shadow space. APIs returning
/// DWORD/BOOL are explicitly zero-extended; their upper RAX bits are unspecified.
/// TlsGetValue changes last-error state; capture API errors before TLS lookup.
#[allow(dead_code)]
pub(crate) fn emit_import_leaf(out: &mut NativeCodeGenerator, api: I, result_u32: bool) {
    assert!(matches!(
        api,
        I::TlsAlloc
            | I::TlsFree
            | I::TlsGetValue
            | I::TlsSetValue
            | I::CreateEventA
            | I::SetEvent
            | I::WaitForSingleObject
            | I::CloseHandle
    ));
    out.asm.sub_reg_imm32(Reg::Rsp, 40);
    out.asm.call_import(api);
    if result_u32 {
        out.asm.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    }
    out.asm.add_reg_imm32(Reg::Rsp, 40);
    out.asm.ret();
}

/// `(nanoseconds_u64*) -> Windows_error_or_0`. QPC/QPF scratch lives on this
/// thread's stack. Compute floor(counter * 1e9 / frequency) with a 128-bit
/// product, checking division overflow; publish output only on success.
pub(crate) fn emit_monotonic_ns(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let unsupported = a.create_text_label();
    let invalid = a.create_text_label();
    let overflow = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(Reg::Rbx);
    a.sub_reg_imm32(Reg::Rsp, 48);
    a.mov_reg_reg(Reg::Rbx, Reg::Rcx);
    a.mov_reg_reg(Reg::Rcx, Reg::Rsp);
    a.add_reg_imm32(Reg::Rcx, 32);
    a.call_import(I::QueryPerformanceCounter);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, unsupported);
    a.mov_reg_reg(Reg::Rcx, Reg::Rsp);
    a.add_reg_imm32(Reg::Rcx, 40);
    a.call_import(I::QueryPerformanceFrequency);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, unsupported);
    a.load_ptr_disp32(Reg::Rax, Reg::Rsp, 32);
    a.cmp_reg_imm8(Reg::Rax, 0);
    a.jcc_label(Condition::Less, invalid);
    a.load_ptr_disp32(Reg::Rcx, Reg::Rsp, 40);
    a.cmp_reg_imm8(Reg::Rcx, 0);
    a.jcc_label(Condition::LessEqual, invalid);
    a.mov_imm64(Reg::R8, 1_000_000_000);
    a.mul_reg(Reg::R8);
    a.cmp_reg_reg(Reg::Rdx, Reg::Rcx);
    a.jcc_label(Condition::AboveOrEqual, overflow);
    a.div_reg(Reg::Rcx);
    a.store_ptr_disp32(Reg::Rbx, 0, Reg::Rax);
    a.mov_imm64(Reg::Rax, 0);
    a.jmp_label(done);
    a.bind_text_label(unsupported);
    a.mov_imm64(Reg::Rax, 50); // ERROR_NOT_SUPPORTED
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(Reg::Rax, 13); // ERROR_INVALID_DATA
    a.jmp_label(done);
    a.bind_text_label(overflow);
    a.mov_imm64(Reg::Rax, 534); // ERROR_ARITHMETIC_OVERFLOW
    a.bind_text_label(done);
    a.add_reg_imm32(Reg::Rsp, 48);
    a.pop_reg(Reg::Rbx);
    a.ret();
}
