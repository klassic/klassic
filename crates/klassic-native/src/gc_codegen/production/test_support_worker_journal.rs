//! Observe actual independent worker transport; waits never consume journals,
//! poll owners, perform phase transitions or run mutator graph work.
use super::super::*;

pub(crate) fn worker_journal_counter(g: &mut crate::NativeCodeGenerator, kind: usize) {
    if kind == 4 {
        load_rt(g, R::V0, RT_MARK_DONE);
        return;
    }
    let base = offset_of!(Runtime, worker_journal);
    let fields = [
        offset_of!(worker_journal::State, entries),
        offset_of!(worker_journal::State, regions),
        offset_of!(worker_journal::State, deferred),
        offset_of!(worker_journal::State, claimed_skips),
    ];
    load_rt(g, R::V0, base + fields[kind]);
}
pub(crate) fn worker_journal_wait(g: &mut crate::NativeCodeGenerator, kind: usize, at_least: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let w = l.worker.as_ref().unwrap();
    let (clock, relax, fail) = (w.hooks.clock, w.hooks.relax, l.fail);
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1992, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    // A real acquire/release ticket, never the worker entry or consumer leaf.
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    worker_journal_counter(g, kind);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
    g.cmp_reg_imm32(R::V0, at_least as i32);
    g.jcc_label(C::AboveOrEqual, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1992);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}

/// Pause the real allocating owner after alloc_into released a journal entry
/// and before it installs pending_ref/black membership. The actual independent
/// worker must observe and defer it. No synthetic mark/cycle/work credit.
pub(crate) fn worker_journal_before_black(
    g: &mut crate::NativeCodeGenerator,
    gate: Option<crate::DataLabel>,
) {
    let hook = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.alloc_before_black_hook = Some(hook);
    g.jmp_label(body);
    g.bind_text_label(hook);
    // Isolate this wait's frame scratch from the interrupted allocation.
    g.push_reg(R::V5);
    g.mov_reg_reg(R::V5, R::V4);
    g.sub_reg_imm32(R::V4, 2048);
    if let Some(gate) = gate {
        g.mov_data_addr(R::V1, gate);
        g.mov_imm64(R::V0, 1);
        g.atomic_store_release(R::V1, R::V0);
    }
    worker_journal_wait(g, 2, 1);
    worker_journal_wait(g, 4, 1);
    load_rt(
        g,
        R::V0,
        RT_RESULT + offset_of!(AllocationResult, reference),
    );
    g.shr_reg_imm8(R::V0, 16);
    load_rt(g, R::V1, RT_HEAP_BASE);
    g.add_reg_reg(R::V0, R::V1);
    g.load_ptr_disp32(
        R::V1,
        R::V0,
        d(offset_of!(ObjectHeader, mark_record)) - size_of::<ObjectHeader>() as i32,
    );
    g.add_reg_imm32(R::V1, d(offset_of!(crate::gc_abi::MarkRecord, old_epoch)));
    g.atomic_load_acquire(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    load_rt(
        g,
        R::V0,
        registered_roots::registration(g)
            + offset_of!(crate::gc_abi::RootRegistration, journal_serial),
    );
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    // An absent allocation token must not be used as a readiness certificate.
    load_rt(g, R::V0, RT_PENDING_REF);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.leave();
    g.ret();
    g.bind_text_label(body);
}

pub(crate) fn worker_journal_gate(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let state = g.asm.data_label_with_i64s(&[0]);
    let gate = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.phase_mark_gate = Some(gate);
    g.jmp_label(body);
    g.bind_text_label(gate);
    g.mov_data_addr(R::V0, state);
    g.atomic_load_acquire(R::V0, R::V0);
    g.ret();
    g.bind_text_label(body);
    state
}
pub(crate) fn worker_journal_open_gate(g: &mut crate::NativeCodeGenerator, gate: crate::DataLabel) {
    g.mov_data_addr(R::V1, gate);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
}

pub(crate) fn worker_journal_fast_probe(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.root_journal_protected;
    g.call_label(entry);
}

/// A real independent kind-4 claim retained by this fixture's control, not a
/// forged registration field. Caller holds the owner ticket across publication
/// and claim; COPY/ACK later authenticate that exact serial and storage lease.
pub(crate) fn worker_journal_hold(
    g: &mut crate::NativeCodeGenerator,
    region: super::JournalRegion,
) -> crate::DataLabel {
    use crate::gc_abi::{RootJournalRequest, RootLease, RootRegistration};
    let runtime = g.gc_gen.take().unwrap();
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let request = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<RootJournalRequest>() / 8]);
    let lease = g
        .asm
        .data_label_with_i64s(&vec![0; size_of::<RootLease>() / 8]);
    let output = g.asm.data_label_with_i64s(&[0; 4]);
    g.mov_data_addr(R::V6, request);
    g.mov_data_addr(R::V0, region.storage);
    g.store_ptr_disp32(
        R::V6,
        d(offset_of!(RootJournalRequest, registration)),
        R::V0,
    );
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, scope)));
    g.store_ptr_disp32(R::V6, d(offset_of!(RootJournalRequest, scope)), R::V0);
    for (field, address) in [
        (offset_of!(RootJournalRequest, lease), lease),
        (offset_of!(RootJournalRequest, output), output),
    ] {
        g.mov_data_addr(R::V0, address);
        g.store_ptr_disp32(R::V6, d(field), R::V0);
    }
    g.mov_imm64(R::V0, 1);
    g.store_ptr_disp32(R::V6, d(offset_of!(RootJournalRequest, capacity)), R::V0);
    lea_rt(g, R::V7, registered_roots::REGISTRY);
    g.mov_imm64(R::V2, 0);
    g.call_label(runtime.labels.root_journal_step);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, runtime.labels.fail);
    g.gc_gen = Some(runtime);
    request
}
pub(crate) fn worker_journal_finish_hold(
    g: &mut crate::NativeCodeGenerator,
    request: crate::DataLabel,
) {
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    // This fixture uses Idle; neither generation has tracing admission duties.
    for field in [RT_MARK_ACTIVE, young_mark::ACTIVE] {
        load_rt(g, R::V0, field);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, runtime.labels.fail);
    }
    for op in [1, 2] {
        lea_rt(g, R::V7, registered_roots::REGISTRY);
        g.mov_data_addr(R::V6, request);
        g.mov_imm64(R::V2, op);
        g.call_label(runtime.labels.root_journal_step);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, runtime.labels.fail);
    }
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
