//! Win64 OS leaves for the production single-worker generational collector.
//! Normal completion stops and joins the worker before releasing both handles.
//! Fatal errors retain process-wide termination without attempting collector work.
use super::production::{WorkerHooks, windows_worker_offsets};
use crate::{Condition as C, ImportSymbol as I, NativeCodeGenerator, Reg as R, TextLabel};

fn prologue(g: &mut NativeCodeGenerator) {
    g.asm.push_reg(R::Rbp);
    g.asm.mov_reg_reg(R::Rbp, R::Rsp);
    g.asm.and_reg_imm32(R::Rsp, -16);
    g.asm.sub_reg_imm32(R::Rsp, 48); // shadow and two CreateThread stack args
}
fn finish(g: &mut NativeCodeGenerator) {
    g.asm.leave();
    g.asm.ret();
}
pub(crate) fn emit(
    g: &mut NativeCodeGenerator,
    h: &WorkerHooks<TextLabel>,
    entry: TextLabel,
) -> TextLabel {
    let (event, thread) = windows_worker_offsets();
    let failed = g.asm.create_text_label();
    let rollback = g.asm.create_text_label();
    let invalid = g.asm.create_text_label();
    let callback = g.asm.create_text_label();
    g.asm.bind_text_label(h.spawn);
    #[cfg(test)]
    if g.gc_test.is_some_and(|t| t.no_worker_thread) {
        g.asm.mov_imm64(R::Rax, 0);
        g.asm.ret();
    }
    prologue(g);
    for r in [R::Rcx, R::Rdx, R::R8, R::R9] {
        g.asm.mov_imm64(r, 0);
    }
    g.asm.call_import(I::CreateEventA); // auto-reset, initially nonsignaled
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Equal, failed);
    g.asm.store_ptr_disp32(R::Rbx, event, R::Rax);
    g.asm.mov_imm64(R::Rcx, 0);
    g.asm.mov_imm64(R::Rdx, 256 << 10);
    g.asm.lea_text_addr(R::R8, callback);
    g.asm.mov_imm64(R::R9, 0);
    g.asm.mov_imm64(R::Rax, 0x10000); // stack size is a reservation
    g.asm.store_ptr_disp32(R::Rsp, 32, R::Rax);
    g.asm.mov_imm64(R::Rax, 0);
    g.asm.store_ptr_disp32(R::Rsp, 40, R::Rax);
    g.asm.call_import(I::CreateThread);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Equal, rollback);
    g.asm.store_ptr_disp32(R::Rbx, thread, R::Rax);
    g.asm.mov_imm64(R::Rax, 1);
    finish(g);
    g.asm.bind_text_label(rollback);
    g.asm.load_ptr_disp32(R::Rcx, R::Rbx, event);
    g.asm.call_import(I::CloseHandle);
    g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Equal, invalid);
    g.asm.mov_imm64(R::Rax, 0);
    g.asm.store_ptr_disp32(R::Rbx, event, R::Rax);
    g.asm.bind_text_label(failed);
    g.asm.mov_imm64(R::Rax, 0);
    finish(g);

    g.asm.bind_text_label(h.park);
    prologue(g);
    // Sequence mismatch avoids sleeping through work already observed after
    // unlock. An auto-reset event retains a signal arriving before the wait.
    let awake = g.asm.create_text_label();
    g.asm.load_u32_ptr_disp32(R::Rax, R::Rdi, 0);
    g.asm.mov_u32_reg_reg(R::Rdx, R::Rsi);
    g.asm.cmp_reg_reg(R::Rax, R::Rdx);
    g.asm.jcc_label(C::NotEqual, awake);
    g.asm.load_ptr_disp32(R::Rcx, R::Rbx, event);
    g.asm.mov_imm64(R::Rdx, 1000); // relative milliseconds, including idle work
    g.asm.call_import(I::WaitForSingleObject);
    g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Equal, awake);
    g.asm.cmp_reg_imm32(R::Rax, 258); // WAIT_TIMEOUT
    g.asm.jcc_label(C::NotEqual, invalid);
    g.asm.bind_text_label(awake);
    finish(g);

    g.asm.bind_text_label(h.unpark);
    prologue(g);
    let absent = g.asm.create_text_label();
    g.asm.load_ptr_disp32(R::Rcx, R::Rbx, event);
    g.asm.test_reg_reg(R::Rcx, R::Rcx);
    g.asm.jcc_label(C::Equal, absent); // event/thread creation failed
    g.asm.call_import(I::SetEvent);
    g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::Equal, invalid);
    g.asm.bind_text_label(absent);
    finish(g);

    g.asm.bind_text_label(h.relax);
    let yield_leaf = g.asm.create_text_label();
    g.asm.bytes(&[0x9c]); // pushfq: the hook preserves flags as well as GPRs
    g.asm.push_reg(R::Rax);
    g.asm.call_label(yield_leaf);
    g.asm.pop_reg(R::Rax);
    g.asm.bytes(&[0x9d]); // popfq
    g.asm.ret();
    g.asm.bind_text_label(yield_leaf);
    g.asm.push_reg(R::Rbp);
    g.asm.mov_reg_reg(R::Rbp, R::Rsp);
    g.emit_win_shim_save_registers();
    g.asm.sub_reg_imm32(R::Rsp, 96);
    for xmm in 0..6_u8 {
        // movdqu [rbp-disp32],xmm: protect volatile SIMD state too.
        g.asm.bytes(&[0xf3, 0x0f, 0x7f, 0x85 | (xmm << 3)]);
        g.asm.bytes(&(-96_i32 - i32::from(xmm) * 16).to_le_bytes());
    }
    g.emit_win_shim_align_stack(32);
    g.asm.mov_imm64(R::Rcx, 0);
    g.asm.call_import(I::Sleep); // yield the time slice to a ready lock holder
    for xmm in 0..6_u8 {
        g.asm.bytes(&[0xf3, 0x0f, 0x6f, 0x85 | (xmm << 3)]);
        g.asm.bytes(&(-96_i32 - i32::from(xmm) * 16).to_le_bytes());
    }
    g.emit_win_shim_epilogue();

    g.asm
        .bind_text_label(h.join.expect("Windows worker join leaf"));
    prologue(g);
    g.asm.load_ptr_disp32(R::Rcx, R::Rbx, thread);
    g.asm.mov_imm64(R::Rdx, u32::MAX as u64); // INFINITE, thread becomes signaled at termination
    g.asm.call_import(I::WaitForSingleObject);
    g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
    g.asm.test_reg_reg(R::Rax, R::Rax);
    g.asm.jcc_label(C::NotEqual, invalid);
    for field in [thread, event] {
        g.asm.load_ptr_disp32(R::Rcx, R::Rbx, field);
        g.asm.call_import(I::CloseHandle);
        g.asm.mov_u32_reg_reg(R::Rax, R::Rax);
        g.asm.test_reg_reg(R::Rax, R::Rax);
        g.asm.jcc_label(C::Equal, invalid);
        g.asm.mov_imm64(R::Rax, 0);
        g.asm.store_ptr_disp32(R::Rbx, field, R::Rax);
    }
    finish(g);

    // The common collector uses its own register convention. Returning to
    // the Windows thread trampoline must restore the complete native ABI.
    g.asm.bind_text_label(callback);
    g.asm.push_reg(R::Rbp);
    let saved = [R::Rbx, R::Rdi, R::Rsi, R::R12, R::R13, R::R14, R::R15];
    for reg in saved {
        g.asm.push_reg(reg);
    }
    g.asm.sub_reg_imm32(R::Rsp, 168); // ten full SIMD registers + alignment
    for xmm in 6..16_u8 {
        callback_xmm(g, xmm, false);
    }
    g.asm.sub_reg_imm32(R::Rsp, 32);
    g.asm.call_label(entry);
    g.asm.add_reg_imm32(R::Rsp, 32);
    for xmm in 6..16_u8 {
        callback_xmm(g, xmm, true);
    }
    g.asm.add_reg_imm32(R::Rsp, 168);
    for reg in saved.into_iter().rev() {
        g.asm.pop_reg(reg);
    }
    g.asm.pop_reg(R::Rbp);
    g.asm.mov_imm64(R::Rax, 0);
    g.asm.ret();
    g.asm.bind_text_label(h.clock);
    g.asm.call_label(g.windows_runtime().win_monotonic_ns);
    g.asm.ret();
    g.asm.bind_text_label(invalid);
    g.emit_runtime_error_text("GC worker notification failed\n");
    callback
}

fn callback_xmm(g: &mut NativeCodeGenerator, xmm: u8, load: bool) {
    // movdqu xmm,[rsp+disp32] or its store inverse; REX.R addresses XMM8..15.
    g.asm.bytes(&[0xf3]);
    if xmm >= 8 {
        g.asm.bytes(&[0x44]);
    }
    g.asm.bytes(&[
        0x0f,
        if load { 0x6f } else { 0x7f },
        0x84 | ((xmm & 7) << 3),
        0x24,
    ]);
    g.asm.bytes(&(i32::from(xmm - 6) * 16).to_le_bytes());
}
