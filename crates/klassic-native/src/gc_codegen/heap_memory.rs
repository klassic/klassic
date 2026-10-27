//! Physical heap reservation/accessibility leaves, without a C runtime.
//! Portable inputs: V7=address (or reserve length), V6=length; preserve V3.
use super::production::MemoryLabels;
use crate::{Condition as C, NativeCodeGenerator, Reg as R, TextLabel};
pub(crate) fn emit(g: &mut NativeCodeGenerator, l: &MemoryLabels<TextLabel>) {
    if g.is_windows {
        windows(g, l);
    } else {
        linux(g, l);
    }
}
fn linux(g: &mut NativeCodeGenerator, l: &MemoryLabels<TextLabel>) {
    let a = &mut g.asm;
    let fail = a.create_text_label();
    a.bind_text_label(l.reserve);
    a.mov_reg_reg(R::Rsi, R::Rdi);
    if g.gc_large_pages != crate::GcLargePages::Off {
        a.test_reg_reg(R::Rsi, R::Rsi);
        a.jcc_label(C::Equal, fail);
        a.add_reg_imm32(R::Rsi, 2 << 20);
    }
    a.mov_imm64(R::Rdi, 0);
    a.mov_imm64(R::Rdx, 0); // PROT_NONE: payload pages are inaccessible.
    a.mov_imm64(R::R10, 0x22); // MAP_PRIVATE | MAP_ANONYMOUS
    a.mov_imm64(R::R8, u64::MAX);
    a.mov_imm64(R::R9, 0);
    a.mov_imm64(R::Rax, 9);
    a.syscall();
    a.mov_imm64(R::Rdx, (-4095_i64) as u64);
    a.cmp_reg_reg(R::Rax, R::Rdx);
    a.jcc_label(C::AboveOrEqual, fail);
    if g.gc_large_pages != crate::GcLargePages::Off {
        super::linux_large_pages::reserved(g);
    }
    let a = &mut g.asm;
    a.ret();
    a.bind_text_label(fail);
    a.mov_imm64(R::Rax, 0);
    a.ret();
    a.bind_text_label(l.commit);
    let failed = a.create_text_label();
    if g.gc_large_pages == crate::GcLargePages::Required {
        a.mov_reg_reg(R::Rax, R::Rdi);
        a.or_reg_reg(R::Rax, R::Rsi);
        a.and_reg_imm32(R::Rax, (2 << 20) - 1);
        a.jcc_label(C::NotEqual, failed);
        a.test_reg_reg(R::Rsi, R::Rsi);
        a.jcc_label(C::Equal, failed);
    }
    a.mov_imm64(R::Rdx, 3); // mprotect(PROT_READ | PROT_WRITE)
    a.mov_imm64(R::Rax, 10);
    a.syscall();
    a.test_reg_reg(R::Rax, R::Rax);
    a.jcc_label(C::NotEqual, failed);
    if g.gc_large_pages == crate::GcLargePages::Required {
        super::linux_large_pages::collapse(g, failed);
    }
    let a = &mut g.asm;
    a.mov_imm64(R::Rax, 1);
    a.ret();
    a.bind_text_label(failed);
    a.mov_imm64(R::Rax, 0);
    a.ret();
    a.bind_text_label(l.decommit);
    super::native_metadata::emit_linux_decommit(g);
    g.asm.bind_text_label(l.release);
    let failed = g.asm.create_text_label();
    #[cfg(test)]
    if g.gc_test.is_some_and(|t| t.no_worker_thread) {
        let unlocked = g.asm.create_text_label();
        super::production::test_support::metadata_release_ticket_check(g);
        g.asm.jcc_label(C::Equal, unlocked);
        g.emit_runtime_error_text("test: metadata OS release retained the collector ticket\n");
        g.asm.bind_text_label(unlocked);
    }
    g.asm.mov_imm64(R::Rax, 11);
    g.asm.syscall();
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, failed);
    g.asm.mov_imm64(R::Rax, 1);
    g.asm.ret();
    g.asm.bind_text_label(failed);
    g.asm.mov_imm64(R::Rax, 0);
    g.asm.ret();
}
fn windows(g: &mut NativeCodeGenerator, l: &MemoryLabels<TextLabel>) {
    use crate::ImportSymbol as I;
    let a = &mut g.asm;
    for (entry, reserve) in [(l.reserve, true), (l.commit, false)] {
        a.bind_text_label(entry);
        a.push_reg(R::Rbp);
        a.mov_reg_reg(R::Rbp, R::Rsp);
        a.and_reg_imm32(R::Rsp, -16);
        a.sub_reg_imm32(R::Rsp, 32);
        if reserve {
            a.mov_reg_reg(R::Rdx, R::Rdi);
            a.mov_imm64(R::Rcx, 0);
        } else {
            a.mov_reg_reg(R::Rcx, R::Rdi);
            a.mov_reg_reg(R::Rdx, R::Rsi);
        }
        a.mov_imm64(R::R8, if reserve { 0x2000 } else { 0x1000 });
        a.mov_imm64(R::R9, if reserve { 1 } else { 4 });
        a.call_import(I::VirtualAlloc);
        if !reserve {
            let failed = a.create_text_label();
            a.cmp_reg_reg(R::Rax, R::Rdi);
            a.mov_imm64(R::Rax, 0);
            a.jcc_label(C::NotEqual, failed);
            a.mov_imm64(R::Rax, 1);
            a.bind_text_label(failed);
        }
        a.leave();
        a.ret();
    }
    a.bind_text_label(l.decommit);
    a.push_reg(R::Rbp);
    a.mov_reg_reg(R::Rbp, R::Rsp);
    a.and_reg_imm32(R::Rsp, -16);
    a.sub_reg_imm32(R::Rsp, 32);
    a.mov_reg_reg(R::Rcx, R::Rdi);
    a.mov_reg_reg(R::Rdx, R::Rsi);
    a.mov_imm64(R::R8, 0x4000); // MEM_DECOMMIT, retain the reservation.
    a.call_import(I::VirtualFree);
    a.mov_u32_reg_reg(R::Rax, R::Rax);
    let failed = a.create_text_label();
    a.test_reg_reg(R::Rax, R::Rax);
    a.jcc_label(C::Equal, failed);
    a.mov_imm64(R::Rax, 1);
    a.bind_text_label(failed);
    a.leave();
    a.ret();
    a.bind_text_label(l.release);
    a.push_reg(R::Rbp);
    a.mov_reg_reg(R::Rbp, R::Rsp);
    a.and_reg_imm32(R::Rsp, -16);
    a.sub_reg_imm32(R::Rsp, 32);
    a.mov_reg_reg(R::Rcx, R::Rdi);
    a.mov_imm64(R::Rdx, 0);
    a.mov_imm64(R::R8, 0x8000); // MEM_RELEASE releases the complete reservation.
    a.call_import(I::VirtualFree);
    a.mov_u32_reg_reg(R::Rax, R::Rax);
    let failed = a.create_text_label();
    a.test_reg_reg(R::Rax, R::Rax);
    a.jcc_label(C::Equal, failed);
    a.mov_imm64(R::Rax, 1);
    a.bind_text_label(failed);
    a.leave();
    a.ret();
}
