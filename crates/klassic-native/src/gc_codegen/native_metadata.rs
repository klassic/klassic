//! Linux/Windows x86-64 native metadata backing without a C runtime or external linker.
//! The owner reserves/commits the charge separately and calls outside locks and
//! source guards, in the appropriate blocked-native state. ABI: NativeMetadataOps.
use crate::{Condition, NativeCodeGenerator, Reg};

/// `(bytes) -> zeroed_page_aligned_pointer_or_null`; bytes is a page multiple.
#[allow(dead_code)]
pub(crate) fn emit_linux_map(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let fail = a.create_text_label();
    a.test_reg_reg(Reg::Rdi, Reg::Rdi);
    a.jcc_label(Condition::Equal, fail);
    a.mov_reg_reg(Reg::Rax, Reg::Rdi);
    a.and_reg_imm32(Reg::Rax, 4095);
    a.jcc_label(Condition::NotEqual, fail);
    a.mov_reg_reg(Reg::Rsi, Reg::Rdi);
    a.mov_imm64(Reg::Rdi, 0);
    a.mov_imm64(Reg::Rdx, 3); // PROT_READ | PROT_WRITE
    a.mov_imm64(Reg::R10, 0x22); // MAP_PRIVATE | MAP_ANONYMOUS
    a.mov_imm64(Reg::R8, u64::MAX); // fd=-1
    a.mov_imm64(Reg::R9, 0);
    a.mov_imm64(Reg::Rax, 9); // mmap
    a.syscall();
    a.mov_imm64(Reg::Rdx, (-4095_i64) as u64);
    a.cmp_reg_reg(Reg::Rax, Reg::Rdx);
    a.jcc_label(Condition::AboveOrEqual, fail);
    a.ret();
    a.bind_text_label(fail);
    a.mov_imm64(Reg::Rax, 0);
    a.ret();
}

/// `(base, bytes) -> bool`; failed release preserves the owner's charge.
#[allow(dead_code)]
pub(crate) fn emit_linux_unmap(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let fail = a.create_text_label();
    for reg in [Reg::Rdi, Reg::Rsi] {
        a.test_reg_reg(reg, reg);
        a.jcc_label(Condition::Equal, fail);
        a.mov_reg_reg(Reg::Rax, reg);
        a.and_reg_imm32(Reg::Rax, 4095);
        a.jcc_label(Condition::NotEqual, fail);
    }
    a.mov_imm64(Reg::Rax, 11); // munmap
    a.syscall();
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::NotEqual, fail);
    a.mov_imm64(Reg::Rax, 1);
    a.ret();
    a.bind_text_label(fail);
    a.mov_imm64(Reg::Rax, 0);
    a.ret();
}

/// Win64 `(bytes) -> pointer_or_null`, reserving and committing zeroed storage.
#[allow(dead_code)]
pub(crate) fn emit_windows_map(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let fail = a.create_text_label();
    a.test_reg_reg(Reg::Rcx, Reg::Rcx);
    a.jcc_label(Condition::Equal, fail);
    a.mov_reg_reg(Reg::Rax, Reg::Rcx);
    a.and_reg_imm32(Reg::Rax, 4095);
    a.jcc_label(Condition::NotEqual, fail);
    a.mov_reg_reg(Reg::Rdx, Reg::Rcx);
    a.mov_imm64(Reg::Rcx, 0);
    a.mov_imm64(Reg::R8, 0x3000); // MEM_RESERVE | MEM_COMMIT
    a.mov_imm64(Reg::R9, 4); // PAGE_READWRITE
    a.sub_reg_imm32(Reg::Rsp, 40); // aligned Win64 shadow space
    a.call_import(crate::ImportSymbol::VirtualAlloc);
    a.add_reg_imm32(Reg::Rsp, 40);
    a.ret();
    a.bind_text_label(fail);
    a.mov_imm64(Reg::Rax, 0);
    a.ret();
}

/// Win64 `(base, bytes) -> bool`; MEM_RELEASE requires a zero OS size argument.
#[allow(dead_code)]
pub(crate) fn emit_windows_unmap(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let fail = a.create_text_label();
    for reg in [Reg::Rcx, Reg::Rdx] {
        a.test_reg_reg(reg, reg);
        a.jcc_label(Condition::Equal, fail);
        a.mov_reg_reg(Reg::Rax, reg);
        a.and_reg_imm32(Reg::Rax, 4095);
        a.jcc_label(Condition::NotEqual, fail);
    }
    a.mov_imm64(Reg::Rdx, 0);
    a.mov_imm64(Reg::R8, 0x8000); // MEM_RELEASE
    a.sub_reg_imm32(Reg::Rsp, 40);
    a.call_import(crate::ImportSymbol::VirtualFree);
    a.add_reg_imm32(Reg::Rsp, 40);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax); // BOOL's upper bits are unspecified
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, fail);
    a.mov_imm64(Reg::Rax, 1); // normalize even noncanonical nonzero BOOL values
    a.ret();
    a.bind_text_label(fail);
    a.mov_imm64(Reg::Rax, 0);
    a.ret();
}

/// Linux SysV `(reserved_charge, RootStorageSpec) -> OK/INVALID/OS_ERROR`.
/// Caller owns the charge and runs outside locks/guards, in native state when
/// attached. Maps and initializes without C callbacks; never commits/registers.
/// Store mapping ownership before initialization so any failure remains cleanable.
#[allow(dead_code)]
pub(crate) fn emit_linux_roots_prepare(out: &mut NativeCodeGenerator) {
    let map = out.asm.create_text_label();
    super::native_roots::emit_prepare(out, 4096, map);
    out.asm.bind_text_label(map);
    emit_linux_map(out);
}

/// Win64 `(reserved_charge, RootStorageSpec) -> OK/INVALID/OS_ERROR`.
/// The portable core uses rdi/rsi, which Win64 callers require preserved.
#[allow(dead_code)]
pub(crate) fn emit_windows_roots_prepare(out: &mut NativeCodeGenerator) {
    let core = out.asm.create_text_label();
    let map = out.asm.create_text_label();
    out.asm.push_reg(Reg::Rdi);
    out.asm.push_reg(Reg::Rsi);
    out.asm.sub_reg_imm8(Reg::Rsp, 8); // align the private portable call
    out.asm.mov_reg_reg(Reg::Rdi, Reg::Rcx);
    out.asm.mov_reg_reg(Reg::Rsi, Reg::Rdx);
    out.asm.call_label(core);
    out.asm.add_reg_imm32(Reg::Rsp, 8);
    out.asm.pop_reg(Reg::Rsi);
    out.asm.pop_reg(Reg::Rdi);
    out.asm.ret();
    out.asm.bind_text_label(core);
    super::native_roots::emit_prepare(out, 4096, map);
    out.asm.bind_text_label(map);
    out.asm.mov_reg_reg(Reg::Rcx, Reg::Rdi);
    emit_windows_map(out);
}

/// Linux SysV `(owned_charge) -> OK/INVALID/OS_ERROR`.
#[allow(dead_code)]
pub(crate) fn emit_linux_roots_dispose(out: &mut NativeCodeGenerator) {
    let unmap = out.asm.create_text_label();
    super::native_roots::emit_dispose(out, 4096, unmap);
    out.asm.bind_text_label(unmap);
    emit_linux_unmap(out);
}

/// Win64 counterpart, preserving the portable core's rdi/rsi arguments.
#[allow(dead_code)]
pub(crate) fn emit_windows_roots_dispose(out: &mut NativeCodeGenerator) {
    let core = out.asm.create_text_label();
    let unmap = out.asm.create_text_label();
    out.asm.push_reg(Reg::Rdi);
    out.asm.push_reg(Reg::Rsi);
    out.asm.sub_reg_imm8(Reg::Rsp, 8);
    out.asm.mov_reg_reg(Reg::Rdi, Reg::Rcx);
    out.asm.call_label(core);
    out.asm.add_reg_imm32(Reg::Rsp, 8);
    out.asm.pop_reg(Reg::Rsi);
    out.asm.pop_reg(Reg::Rdi);
    out.asm.ret();
    out.asm.bind_text_label(core);
    super::native_roots::emit_dispose(out, 4096, unmap);
    out.asm.bind_text_label(unmap);
    out.asm.mov_reg_reg(Reg::Rcx, Reg::Rdi);
    out.asm.mov_reg_reg(Reg::Rdx, Reg::Rsi);
    emit_windows_unmap(out);
}

/// SysV private descriptor bootstrap `(reserved charge, capacity) -> status`.
#[allow(dead_code)]
pub(crate) fn emit_linux_builtin_registry_prepare(out: &mut NativeCodeGenerator) {
    let map = out.asm.create_text_label();
    super::native_builtin_registry::emit_prepare(out, 4096, map);
    out.asm.bind_text_label(map);
    emit_linux_map(out);
}

#[allow(dead_code)]
pub(crate) fn emit_windows_builtin_registry_prepare(out: &mut NativeCodeGenerator) {
    let core = out.asm.create_text_label();
    let map = out.asm.create_text_label();
    out.asm.push_reg(Reg::Rdi);
    out.asm.push_reg(Reg::Rsi);
    out.asm.sub_reg_imm8(Reg::Rsp, 8); // align the private portable call
    out.asm.mov_reg_reg(Reg::Rdi, Reg::Rcx);
    out.asm.mov_reg_reg(Reg::Rsi, Reg::Rdx);
    out.asm.call_label(core);
    out.asm.add_reg_imm32(Reg::Rsp, 8);
    out.asm.pop_reg(Reg::Rsi);
    out.asm.pop_reg(Reg::Rdi);
    out.asm.ret();
    out.asm.bind_text_label(core);
    super::native_builtin_registry::emit_prepare(out, 4096, map);
    out.asm.bind_text_label(map);
    out.asm.mov_reg_reg(Reg::Rcx, Reg::Rdi);
    emit_windows_map(out);
}

/// SysV private startup preparation `(request, owner) -> status`.
#[allow(dead_code)]
pub(crate) fn emit_linux_startup_prepare(out: &mut NativeCodeGenerator) {
    let map = out.asm.create_text_label();
    super::root_lifetime::native_startup::emit_prepare(out, 4096, map);
    out.asm.bind_text_label(map);
    emit_linux_map(out);
}

#[allow(dead_code)]
pub(crate) fn emit_windows_startup_prepare(out: &mut NativeCodeGenerator) {
    let core = out.asm.create_text_label();
    let map = out.asm.create_text_label();
    out.asm.push_reg(Reg::Rdi);
    out.asm.push_reg(Reg::Rsi);
    out.asm.sub_reg_imm8(Reg::Rsp, 8); // align the private portable call
    out.asm.mov_reg_reg(Reg::Rdi, Reg::Rcx);
    out.asm.mov_reg_reg(Reg::Rsi, Reg::Rdx);
    out.asm.call_label(core);
    out.asm.add_reg_imm32(Reg::Rsp, 8);
    out.asm.pop_reg(Reg::Rsi);
    out.asm.pop_reg(Reg::Rdi);
    out.asm.ret();
    out.asm.bind_text_label(core);
    super::root_lifetime::native_startup::emit_prepare(out, 4096, map);
    out.asm.bind_text_label(map);
    out.asm.mov_reg_reg(Reg::Rcx, Reg::Rdi);
    emit_windows_map(out);
}

/// SysV `(base, bytes) -> bool`, retaining the exclusively owned reservation.
/// Partial protection failure retains the caller's receipt for retry.
#[allow(dead_code)]
pub(crate) fn emit_linux_decommit(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let fail = a.create_text_label();
    for reg in [Reg::Rdi, Reg::Rsi] {
        a.test_reg_reg(reg, reg);
        a.jcc_label(Condition::Equal, fail);
        a.mov_reg_reg(Reg::Rax, reg);
        a.and_reg_imm32(Reg::Rax, 4095);
        a.jcc_label(Condition::NotEqual, fail);
    }
    a.mov_reg_reg(Reg::Rax, Reg::Rdi);
    a.add_reg_reg(Reg::Rax, Reg::Rsi);
    a.cmp_reg_reg(Reg::Rax, Reg::Rdi);
    a.jcc_label(Condition::Below, fail);
    a.mov_imm64(Reg::Rdx, 0);
    a.mov_imm64(Reg::Rax, 10); // mprotect(PROT_NONE)
    a.syscall();
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::NotEqual, fail);
    a.mov_imm64(Reg::Rdx, 4);
    a.mov_imm64(Reg::Rax, 28); // madvise(MADV_DONTNEED)
    a.syscall();
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::NotEqual, fail);
    a.mov_imm64(Reg::Rax, 1);
    a.ret();
    a.bind_text_label(fail);
    a.mov_imm64(Reg::Rax, 0);
    a.ret();
}

/// Win64 `(base, bytes) -> bool`; MEM_DECOMMIT retains the virtual reservation.
#[allow(dead_code)]
pub(crate) fn emit_windows_decommit(out: &mut NativeCodeGenerator) {
    let a = &mut out.asm;
    let fail = a.create_text_label();
    for reg in [Reg::Rcx, Reg::Rdx] {
        a.test_reg_reg(reg, reg);
        a.jcc_label(Condition::Equal, fail);
        a.mov_reg_reg(Reg::Rax, reg);
        a.and_reg_imm32(Reg::Rax, 4095);
        a.jcc_label(Condition::NotEqual, fail);
    }
    a.mov_reg_reg(Reg::Rax, Reg::Rcx);
    a.add_reg_reg(Reg::Rax, Reg::Rdx);
    a.cmp_reg_reg(Reg::Rax, Reg::Rcx);
    a.jcc_label(Condition::Below, fail);
    a.mov_imm64(Reg::R8, 0x4000);
    a.sub_reg_imm32(Reg::Rsp, 40);
    a.call_import(crate::ImportSymbol::VirtualFree);
    a.add_reg_imm32(Reg::Rsp, 40);
    a.mov_u32_reg_reg(Reg::Rax, Reg::Rax);
    a.test_reg_reg(Reg::Rax, Reg::Rax);
    a.jcc_label(Condition::Equal, fail);
    a.mov_imm64(Reg::Rax, 1);
    a.ret();
    a.bind_text_label(fail);
    a.mov_imm64(Reg::Rax, 0);
    a.ret();
}
