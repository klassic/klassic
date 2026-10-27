//! Concurrent-safe Linux clock leaf, with per-call timespec storage.
use crate::{Condition as C, NativeCodeGenerator, PlatformSyscall, Reg as R, TextLabel};

/// Nanoseconds in RAX, or zero on syscall/representation failure. Every other
/// general register survives. No shared data scratch or managed pointer.
pub(crate) fn emit(out: &mut NativeCodeGenerator, label: TextLabel) {
    let a = &mut out.asm;
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(label);
    for r in [R::Rcx, R::Rdx, R::Rdi, R::Rsi, R::R8, R::R11] {
        a.push_reg(r);
    }
    a.sub_reg_imm32(R::Rsp, 16);
    a.mov_reg_reg(R::Rsi, R::Rsp);
    a.mov_imm64(R::Rdi, out.platform.clock_monotonic());
    a.mov_imm64(
        R::Rax,
        out.platform.syscall_number(PlatformSyscall::ClockGettime),
    );
    a.syscall();
    a.test_reg_reg(R::Rax, R::Rax);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::Rax, R::Rsp, 0);
    a.mov_imm64(R::R8, 1_000_000_000);
    a.mul_reg(R::R8);
    a.test_reg_reg(R::Rdx, R::Rdx);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::Rdx, R::Rsp, 8);
    a.cmp_reg_reg(R::Rdx, R::R8);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.add_reg_reg(R::Rax, R::Rdx);
    a.jcc_label(C::Below, invalid);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::Rax, 0);
    a.bind_text_label(done);
    a.add_reg_imm32(R::Rsp, 16);
    for r in [R::R11, R::R8, R::Rsi, R::Rdi, R::Rdx, R::Rcx] {
        a.pop_reg(r);
    }
    a.ret();
}
