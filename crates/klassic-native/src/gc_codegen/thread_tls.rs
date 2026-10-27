//! Native OS TLS leaves used by the direct thread-lifecycle layer. Linux owns
//! GS only while explicitly installed; libc's FS is untouched. Win64 uses a
//! private TlsAlloc key. Every OS call has native alignment/nonvolatile handling.
use crate::gc_codegen::production::ThreadTlsOsLabels;
use crate::portable_asm::PortableAsm;
use crate::{Condition as C, ImportSymbol as I, NativeCodeGenerator, Reg as R, TextLabel};

pub(crate) fn emit(g: &mut NativeCodeGenerator, l: &ThreadTlsOsLabels<TextLabel>) {
    for (op, label) in [
        (0, l.create),
        (1, l.get),
        (2, l.set),
        (3, l.delete),
        (4, l.identity),
    ] {
        g.asm.bind_text_label(label);
        g.native_entry_begin(64); // private locals above Win64 shadow space
        g.asm.mov_reg_reg(R::Rbx, R::Rdi); // native request, callee saved
        let failed = g.asm.create_text_label();
        let done = g.asm.create_text_label();
        if op == 4 {
            if g.is_windows {
                g.asm.call_import(I::GetCurrentThreadId);
                g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
            } else {
                g.asm.mov_imm64(R::Rax, 186); // gettid; no TLS key/value lookup
                g.asm.syscall();
            }
            g.asm.cmp_reg_imm8(R::Rax, 0);
            g.asm.jcc_label(C::LessEqual, failed);
            g.asm.store_ptr_disp32(R::Rbx, 16, R::Rax);
        } else if g.is_windows {
            if op != 0 {
                g.asm.load_ptr_disp32(R::Rcx, R::Rbx, 0);
            }
            if op == 2 {
                g.asm.load_ptr_disp32(R::Rdx, R::Rbx, 8);
            }
            g.asm.call_import(match op {
                0 => I::TlsAlloc,
                1 => I::TlsGetValue,
                2 => I::TlsSetValue,
                _ => I::TlsFree,
            });
            match op {
                0 => {
                    g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
                    g.asm.mov_imm64(R::R10, u64::from(u32::MAX));
                    g.asm.cmp_reg_reg(R::Rax, R::R10);
                    g.asm.jcc_label(C::Equal, failed);
                    g.asm.store_ptr_disp32(R::Rbx, 0, R::Rax);
                }
                1 => {
                    // TlsGetValue returns NULL for both unbound and error;
                    // it clears last-error for the former. Read immediately.
                    g.asm.store_ptr_disp32(R::Rbx, 8, R::Rax);
                    let value = g.asm.create_text_label();
                    g.asm.test_reg_reg(R::Rax, R::Rax);
                    g.asm.jcc_label(C::NotEqual, value);
                    g.asm.call_import(I::GetLastError);
                    g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
                    g.asm.test_reg_reg(R::Rax, R::Rax);
                    g.asm.jcc_label(C::NotEqual, failed);
                    g.asm.bind_text_label(value);
                    g.asm.call_import(I::GetCurrentThreadId);
                    g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
                    g.asm.test_reg_reg(R::Rax, R::Rax);
                    g.asm.jcc_label(C::Equal, failed);
                    g.asm.store_ptr_disp32(R::Rbx, 16, R::Rax);
                }
                _ => {
                    g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
                    g.asm.test_reg_reg(R::Rax, R::Rax);
                    g.asm.jcc_label(C::Equal, failed);
                }
            }
        } else if op == 1 || op == 2 {
            if op == 1 {
                g.asm.mov_reg_reg(R::Rsi, R::Rbx);
                g.asm.add_reg_imm32(R::Rsi, 8);
            } else {
                g.asm.load_ptr_disp32(R::Rsi, R::Rbx, 8);
            }
            g.asm
                .mov_imm64(R::Rdi, if op == 1 { 0x1004 } else { 0x1001 });
            g.asm.mov_imm64(R::Rax, 158); // arch_prctl GET_GS / SET_GS
            g.asm.syscall();
            g.asm.cmp_reg_imm8(R::Rax, 0);
            g.asm.jcc_label(C::Less, failed);
            if op == 1 {
                g.asm.mov_imm64(R::Rax, 186); // real Linux gettid, not a runtime identity
                g.asm.syscall();
                g.asm.cmp_reg_imm8(R::Rax, 0);
                g.asm.jcc_label(C::LessEqual, failed);
                g.asm.store_ptr_disp32(R::Rbx, 16, R::Rax);
            }
        } else if op == 0 {
            // GS has no process-wide allocated key to create/delete.
            g.asm.mov_imm64(R::Rax, 0);
            g.asm.store_ptr_disp32(R::Rbx, 0, R::Rax);
        }
        g.asm.mov_imm64(R::Rax, 0);
        g.asm.jmp_label(done);
        g.asm.bind_text_label(failed);
        g.asm.mov_imm64(R::Rax, 6);
        g.asm.bind_text_label(done);
        g.native_entry_end();
        g.asm.ret();
    }
}
