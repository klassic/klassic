//! Real worker scheduling latch and observations of retained journal records.
use super::super::*;
use crate::gc_abi::{BufferCursor, RootRegistry};

pub(crate) fn captured_journal_gate(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let state = g.asm.data_label_with_i64s(&[0]);
    let gate = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.journal_worker_gate = Some(gate);
    g.jmp_label(body);
    g.bind_text_label(gate);
    g.mov_data_addr(R::V0, state);
    g.atomic_load_acquire(R::V0, R::V0);
    g.ret();
    g.bind_text_label(body);
    state
}
pub(crate) fn captured_journal_release(g: &mut crate::NativeCodeGenerator, gate: crate::DataLabel) {
    let runtime = g.gc_gen.take().unwrap();
    worker_journal_gate_release(g, gate);
    worker::lock(g, &runtime.labels);
    worker::request_work(g);
    worker::unlock(g, &runtime.labels);
    worker::wake_if_pending(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
fn worker_journal_gate_release(g: &mut crate::NativeCodeGenerator, gate: crate::DataLabel) {
    g.mov_data_addr(R::V1, gate);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
}
pub(crate) fn captured_journal_phase(g: &mut crate::NativeCodeGenerator) {
    load_rt(
        g,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
}
pub(crate) fn captured_journal_check_lifetime(g: &mut crate::NativeCodeGenerator) {
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    journal_snapshot::lifetime(g, &runtime.labels);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
pub(crate) fn captured_journal_fault(g: &mut crate::NativeCodeGenerator, kind: u64) {
    if kind == 8 {
        load_rt(g, R::V0, allocation_records::REVISION);
        g.add_reg_imm32(R::V0, 1);
        store_rt(g, allocation_records::REVISION, R::V0);
        return;
    }
    if kind == 9 {
        load_rt(
            g,
            R::V0,
            registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
        );
        g.mov_imm64(R::V1, 0x400);
        g.xor_reg_reg(R::V0, R::V1);
        store_rt(
            g,
            registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
            R::V0,
        );
        return;
    }
    load_rt(g, R::V1, RT_JOURNAL);
    g.load_ptr_disp32(R::V0, R::V1, d(offset_of!(BufferCursor, consumed)));
    g.load_ptr_disp32(R::V6, R::V1, d(offset_of!(BufferCursor, capacity)));
    g.load_ptr_disp32(R::V10, R::V1, d(offset_of!(BufferCursor, entries)));
    g.div_unsigned(R::V6);
    g.shl_reg_imm8(R::V2, 5);
    g.add_reg_reg(R::V10, R::V2);
    // The oldest actual entry was captured before this cycle's start.
    g.load_ptr_disp32(R::V0, R::V10, d(offset_of!(RootChange, phase)));
    if kind <= 5 {
        g.mov_imm64(
            R::V1,
            [1, 0x100, 0x400, 0x1000, 0x2000, 0x50][kind as usize],
        );
        g.or_reg_reg(R::V0, R::V1);
    } else {
        captured_journal_phase(g);
        if kind == 6 {
            g.and_reg_imm32(R::V0, !255);
            g.add_reg_imm32(R::V0, (crate::gc_abi::Phase::Select as i32) << 4);
        } else {
            g.add_reg_imm32(R::V0, 0x10000);
        }
    }
    g.store_ptr_disp32(R::V10, d(offset_of!(RootChange, phase)), R::V0);
}

pub(crate) fn captured_tlab_metric(g: &mut crate::NativeCodeGenerator, kind: usize) {
    let base = owner_state::offset(
        offset_of!(OwnerState, phase_round) + offset_of!(phase_round::Owner, tlab),
    );
    let fields = [
        RT_CTX + offset_of!(MutatorContext, tlab_top),
        RT_REQUEST + offset_of!(AllocationRequest, page),
        base + offset_of!(old_tlab::State, suspends),
        base + offset_of!(old_tlab::State, restores),
        RT_NEXT_PAGE,
    ];
    load_rt(g, R::V0, fields[kind]);
}
pub(crate) fn captured_tlab_fault(g: &mut crate::NativeCodeGenerator, kind: u64) {
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    old_tlab::suspend(g, &runtime.labels);
    if kind == 4 {
        store_rt_imm(g, RT_REQUEST + offset_of!(AllocationRequest, page), 0);
    } else {
        load_rt(g, R::V2, RT_REQUEST + offset_of!(AllocationRequest, page));
        let field = [
            offset_of!(AllocationPage, owner),
            offset_of!(AllocationPage, incarnation),
            offset_of!(AllocationPage, allocated_top),
            offset_of!(AllocationPage, state),
        ][kind as usize];
        g.load_ptr_disp32(R::V0, R::V2, d(field));
        g.add_reg_imm32(R::V0, if kind == 2 { 16 } else { 1 });
        g.store_ptr_disp32(R::V2, d(field), R::V0);
    }
    old_tlab::restore(g, &runtime.labels);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
