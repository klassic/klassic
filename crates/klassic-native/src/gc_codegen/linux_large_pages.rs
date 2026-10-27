//! Linux THP requests and synchronous commit-time collapse. No libc dependency.
use super::production::large_pages as state;
use crate::{Condition as C, NativeCodeGenerator, Reg as R, TextLabel};
const HUGE: u64 = 2 << 20;
const GC_PAGE: u64 = 128 << 10;
fn d(n: usize) -> i32 {
    n as i32
}
fn bump(g: &mut NativeCodeGenerator, field: usize) {
    g.asm.load_ptr_disp32(R::Rdx, R::Rbx, d(field));
    g.asm.add_reg_imm32(R::Rdx, 1);
    g.asm.store_ptr_disp32(R::Rbx, d(field), R::Rdx);
}
/// After mmap: RAX=actual mapping, before returning the logical heap base.
pub(super) fn reserved(g: &mut NativeCodeGenerator) {
    g.asm
        .store_ptr_disp32(R::Rbx, d(state::RESERVATION_BASE), R::Rax);
    g.asm.mov_imm64(R::Rdx, g.gc_max_heap + GC_PAGE + HUGE);
    g.asm
        .store_ptr_disp32(R::Rbx, d(state::RESERVATION_BYTES), R::Rdx);
    // Keep the logical guard immediately before a 2-MiB-aligned payload.
    g.asm.add_reg_imm32(R::Rax, (GC_PAGE + HUGE - 1) as i32);
    g.asm.and_reg_imm32(R::Rax, -(HUGE as i32));
    g.asm.sub_reg_imm32(R::Rax, GC_PAGE as i32);
    g.asm.mov_reg_reg(R::R8, R::Rax);
    g.asm.mov_reg_reg(R::Rdi, R::Rax);
    g.asm.add_reg_imm32(R::Rdi, GC_PAGE as i32);
    g.asm.mov_imm64(R::Rsi, g.gc_max_heap);
    #[cfg(test)]
    if g.gc_large_pages_fault {
        g.asm.add_reg_imm32(R::Rdi, 1);
    }
    g.asm.mov_imm64(R::Rdx, 14); // MADV_HUGEPAGE: eligibility, not proof of backing.
    g.asm.mov_imm64(R::Rax, 28);
    g.asm.syscall();
    let failed = g.asm.create_text_label();
    let done = g.asm.create_text_label();
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, failed);
    g.asm.mov_imm64(R::Rdx, g.gc_max_heap);
    g.asm
        .store_ptr_disp32(R::Rbx, d(state::ADVISED_BYTES), R::Rdx);
    g.asm.jmp_label(done);
    g.asm.bind_text_label(failed);
    bump(g, state::ADVICE_FAILURES);
    g.asm.bind_text_label(done);
    g.asm.mov_reg_reg(R::Rax, R::R8);
}
/// After successful mprotect: RDI/RSI=aligned newly committed range.
/// Returns by fallthrough on success; branches to the ordinary false return
/// on failure. No accessible tag/allocation top has been published yet.
pub(super) fn collapse(g: &mut NativeCodeGenerator, failed: TextLabel) {
    g.asm.mov_reg_reg(R::R8, R::Rdi);
    g.asm.mov_reg_reg(R::R9, R::Rsi);
    g.asm.mov_imm64(R::Rdx, 0);
    let next = g.asm.create_text_label();
    g.asm.bind_text_label(next);
    // MADV_COLLAPSE needs at least one physically backed page per huge span.
    g.asm.store_ptr_disp32(R::R8, 0, R::Rdx);
    g.asm.add_reg_imm32(R::R8, HUGE as i32);
    g.asm.sub_reg_imm32(R::R9, HUGE as i32);
    g.asm.jcc_label(C::NotEqual, next);
    #[cfg(test)]
    if g.gc_large_pages_fault {
        g.asm.add_reg_imm32(R::Rdi, 1);
    }
    g.asm.mov_imm64(R::Rdx, 25); // MADV_COLLAPSE (Linux >=6.1), checked synchronously.
    g.asm.mov_imm64(R::Rax, 28);
    g.asm.syscall();
    let success = g.asm.create_text_label();
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Equal, success);
    bump(g, state::COLLAPSE_FAILURES);
    g.asm.jmp_label(failed);
    g.asm.bind_text_label(success);
    g.asm
        .load_ptr_disp32(R::Rdx, R::Rbx, d(state::COLLAPSED_BYTES));
    g.asm.add_reg_reg(R::Rdx, R::Rsi);
    g.asm
        .store_ptr_disp32(R::Rbx, d(state::COLLAPSED_BYTES), R::Rdx);
}
