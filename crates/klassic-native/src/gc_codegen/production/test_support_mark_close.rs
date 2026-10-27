//! Real all-native close: each producer seals itself, the last producer is
//! delayed, then the actual Old end drains/certifies and owners adopt Select.
use super::*;
const PENDING_BEFORE: i32 = 96;
const CHECKED_AFTER: i32 = 104;
fn count_ready(g: &mut crate::NativeCodeGenerator, ready: crate::DataLabel) {
    g.mov_data_addr(R::V7, ready);
    let again = g.create_text_label();
    g.bind_text_label(again);
    g.atomic_load_acquire(R::V0, R::V7);
    g.mov_reg_reg(R::V8, R::V0);
    g.mov_reg_reg(R::V6, R::V0);
    g.add_reg_imm32(R::V6, 1);
    g.atomic_compare_exchange(R::V7, R::V6);
    g.cmp_reg_reg(R::V0, R::V8);
    g.jcc_label(C::NotEqual, again);
}
fn wake(g: &mut crate::NativeCodeGenerator, word: crate::DataLabel) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.mov_data_addr(R::V7, word);
    g.asm.mov_imm64(X::Rsi, 129); // FUTEX_WAKE_PRIVATE
    g.asm.mov_imm64(X::Rdx, i32::MAX as u64);
    g.asm.mov_imm64(X::Rax, 202);
    g.asm.syscall();
    g.cmp_reg_imm32(R::V0, 0);
    g.jcc_label(C::Less, fail);
}
fn wait_word(
    g: &mut crate::NativeCodeGenerator,
    word: crate::DataLabel,
    expected: u64,
    native: bool,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (fail, relax, clock) = (
        l.fail,
        l.worker.as_ref().unwrap().hooks.relax,
        l.worker.as_ref().unwrap().hooks.clock,
    );
    let again = g.create_text_label();
    let done = g.create_text_label();
    let expired = g.create_text_label();
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let timeout = g.asm.data_label_with_i64s(&[120, 0]);
    if !native {
        // This deliberately delayed producer stays Running until it really
        // seals itself. A scheduling-dependent iteration budget is invalid.
        g.call_label(clock);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, expired);
        g.mov_imm64(R::V1, 120_000_000_000);
        g.add_reg_reg(R::V0, R::V1);
        g.jcc_label(C::Below, expired);
        g.store_rbp_slot(88, R::V0);
    }
    g.bind_text_label(again);
    g.mov_data_addr(R::V7, word);
    g.atomic_load_acquire(R::V0, R::V7);
    g.cmp_reg_imm32(R::V0, expected as i32);
    g.jcc_label(C::Equal, done);
    if native {
        // Only actual BlockedNative owners park. The low predicate word is
        // compared by the kernel; publication-before-park returns EAGAIN.
        g.asm.mov_reg_reg(X::Rdx, X::Rax);
        g.mov_data_addr(R::V0, timeout);
        g.asm.mov_reg_reg(X::R10, X::Rax);
        g.asm.mov_imm64(X::Rsi, 128); // FUTEX_WAIT_PRIVATE
        g.asm.mov_imm64(X::Rax, 202);
        g.asm.syscall();
        for status in [0, -11, -4] {
            g.cmp_reg_imm32(R::V0, status);
            g.jcc_label(C::Equal, again);
        }
        g.jmp_label(expired);
    } else {
        g.call_label(clock);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, expired);
        g.load_rbp_slot(R::V1, 88);
        g.cmp_reg_reg(R::V0, R::V1);
        g.jcc_label(C::AboveOrEqual, expired);
        g.call_label(relax);
        g.jmp_label(again);
    }
    g.bind_text_label(expired);
    let message = b"native close wait timed out or failed\n";
    let text = g.asm.data_label_with_bytes(message);
    g.emit_write_data(2, text, message.len());
    g.jmp_label(fail);
    g.bind_text_label(done);
}
pub(super) fn child(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    iterations: u64,
    ready: crate::DataLabel,
    gate: crate::DataLabel,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (enter, exit, fail) = (l.native_enter_actor, l.native_exit_actor, l.fail);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, (owners + 1) as i32);
    let ordinary = g.create_text_label();
    g.jcc_label(C::NotEqual, ordinary);
    wait_word(g, ready, owners - 1, false);
    lea_rt(g, R::V1, root_work::PENDING);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, fail);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, PENDING_BEFORE, R::V0);
    g.bind_text_label(ordinary);
    graph::call_owner(g, enter);
    count_ready(g, ready); // only after this actual producer is closed/sealed
    wake(g, ready);
    wait_word(g, gate, 1, true);
    graph::call_owner(g, exit);
    graph::validate_closed(g, iterations);
    g.load_rbp_slot(R::V1, 8);
    g.mov_imm64(R::V0, 1);
    g.store_ptr_disp32(R::V1, CHECKED_AFTER, R::V0);
}
pub(super) fn coordinator(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    ready: crate::DataLabel,
    gate: crate::DataLabel,
) {
    // Parent has its own native scope while waiting for actual child entries.
    // Parent frame differs from child; preserve its ordinary slot88 value.
    g.load_rbp_slot(R::V0, 88);
    g.push_reg(R::V0);
    g.push_reg(R::V0);
    wait_word(g, ready, owners, true);
    g.pop_reg(R::V0);
    g.pop_reg(R::V0);
    g.store_rbp_slot(88, R::V0);
    let runtime = g.gc_gen.take().unwrap();
    g.call_label(runtime.labels.native_exit_actor);
    load_rt(
        g,
        R::V0,
        participants::HANDSHAKE + offset_of!(crate::gc_abi::HandshakeState, native),
    );
    g.store_rbp_slot(1504, R::V0);
    worker::lock(g, &runtime.labels);
    root_work::require_empty(g, &runtime.labels);
    g.call_label(runtime.labels.mark_end); // real drain/frontier finish/Select
    worker::unlock(g, &runtime.labels);
    g.call_label(runtime.labels.native_enter_actor);
    g.mov_data_addr(R::V1, gate);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    g.gc_gen = Some(runtime);
    wake(g, gate);
}
pub(crate) fn closed_owner_metric(
    g: &mut crate::NativeCodeGenerator,
    p: &ParallelActors,
    i: usize,
    after: bool,
) {
    g.mov_data_addr(R::V1, p.controls[i]);
    g.load_ptr_disp32(
        R::V0,
        R::V1,
        if after { CHECKED_AFTER } else { PENDING_BEFORE },
    );
}
