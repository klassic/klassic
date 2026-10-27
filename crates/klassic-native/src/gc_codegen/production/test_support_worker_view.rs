//! Observe an actual worker quantum without executing its tracing on the caller.
use super::*;

pub(crate) fn other_owner_cycle(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    moving: bool,
) {
    select(g, actor);
    if moving {
        let l = g.gc_gen.as_ref().unwrap().labels.collect;
        g.call_label(l);
    } else {
        one_explicit_old_cycle(g);
    }
    restore(g);
}

pub(crate) fn worker_view_metric(g: &mut crate::NativeCodeGenerator, kind: usize) {
    let base = offset_of!(Runtime, worker_view);
    let fields = [
        base + offset_of!(OwnerState, ctx) + offset_of!(MutatorContext, adopted_phase),
        base + offset_of!(OwnerState, ctx) + offset_of!(MutatorContext, thread_id),
        base + offset_of!(OwnerState, segment),
        base + offset_of!(OwnerState, participant)
            + offset_of!(crate::gc_abi::ThreadRegistration, registered),
        RT_CTX + offset_of!(MutatorContext, adopted_phase),
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
        base + offset_of!(OwnerState, ctx) + offset_of!(MutatorContext, tlab_top),
        base + owner_state::raw(root_journal::OWNER_LOCKED),
        RT_WORKER_COPIES,
    ];
    load_rt(g, R::V0, fields[kind]);
}

/// Only requests/wakes the real worker and observes its published phase cache.
/// Mutator lock calls must not perform this refresh on the worker's behalf.
pub(crate) fn worker_view_wait(g: &mut crate::NativeCodeGenerator) {
    wait(g, false);
}
pub(crate) fn worker_view_wait_relocation(g: &mut crate::NativeCodeGenerator) {
    wait(g, true);
}
fn wait(g: &mut crate::NativeCodeGenerator, moving: bool) {
    let runtime = g.gc_gen.take().unwrap();
    let w = runtime.labels.worker.as_ref().unwrap();
    g.call_label(w.hooks.clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    worker::lock(g, &runtime.labels);
    if moving {
        load_rt(g, R::V0, RT_WORKER_DONE);
        g.cmp_reg_imm8(R::V0, 1);
    } else {
        worker_view_metric(g, 0);
        g.mov_reg_reg(R::V6, R::V0);
        worker_view_metric(g, 5);
        g.cmp_reg_reg(R::V0, R::V6);
    }
    g.jcc_label(C::Equal, done);
    worker::request_work(g);
    worker::unlock(g, &runtime.labels);
    worker::wake_if_pending(g, &runtime.labels);
    g.call_label(w.hooks.clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, runtime.labels.fail);
    g.call_label(w.hooks.relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

/// V0 points to a real Native owner's RootCell. Observe after actual worker
/// completion under the ticket; do not enter that owner's root/load APIs.
pub(crate) fn worker_view_native_root(g: &mut crate::NativeCodeGenerator, kind: u64) {
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    g.atomic_load_acquire(R::V0, R::V0);
    if kind == 0 {
        g.and_reg_imm32(R::V0, 65535);
    } else {
        decode(g, R::V0, R::V1);
        if kind == 2 {
            g.load_ptr_disp32(
                R::V0,
                R::V0,
                d(offset_of!(ObjectHeader, mark_record)) - size_of::<ObjectHeader>() as i32,
            );
            g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
        } else if kind == 3 {
            g.mov_reg_reg(R::V8, R::V0);
            g.mov_imm64(R::V9, 0);
            let again = g.create_text_label();
            let done = g.create_text_label();
            g.bind_text_label(again);
            g.test_reg_reg(R::V8, R::V8);
            g.jcc_label(C::Equal, done);
            g.load_ptr_disp32(R::V0, R::V8, 8);
            g.cmp_reg_imm32(R::V0, 91);
            g.jcc_label(C::NotEqual, runtime.labels.fail);
            g.add_reg_imm32(R::V9, 1);
            g.cmp_reg_imm32(R::V9, 1025);
            g.jcc_label(C::Above, runtime.labels.fail);
            g.load_ptr_disp32(R::V0, R::V8, 0);
            decode(g, R::V0, R::V1);
            g.mov_reg_reg(R::V8, R::V0);
            g.jmp_label(again);
            g.bind_text_label(done);
            g.mov_reg_reg(R::V0, R::V9);
        }
    }
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

/// Coherent observation by the actual caller, after the worker drops its real
/// ticket. Acquiring here must never create or complete a worker epoch.
pub(crate) fn worker_epoch_sample(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    let rt = g.gc_gen.take().unwrap();
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let data = g.asm.data_label_with_i64s(&[0; 7]);
    quiescence::save(g);
    worker::lock(g, &rt.labels);
    g.mov_data_addr(R::V2, data);
    for (i, field) in [
        worker_view::ACTIVE,
        worker_view::ENTERED,
        worker_view::COMPLETED,
        worker_view::PHASE,
        worker_view::BANK,
        offset_of!(Runtime, worker_view)
            + offset_of!(OwnerState, ctx)
            + offset_of!(MutatorContext, adopted_phase),
        allocation_records::REVISION,
    ]
    .into_iter()
    .enumerate()
    {
        load_rt(g, R::V0, field);
        g.store_ptr_disp32(R::V2, i as i32 * 8, R::V0);
    }
    worker::unlock(g, &rt.labels);
    quiescence::restore(g);
    g.gc_gen = Some(rt);
    data
}
pub(crate) fn worker_epoch_metric(
    g: &mut crate::NativeCodeGenerator,
    data: crate::DataLabel,
    field: usize,
) {
    g.mov_data_addr(R::V1, data);
    g.load_ptr_disp32(R::V0, R::V1, field as i32 * 8);
}

/// Inject invalid captured state only after the actual OS worker acquired
/// its real epoch. No test-manufactured phase, bank, quorum or admission bit.
pub(crate) fn worker_epoch_fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let hook = g.create_text_label();
    let after = g.create_text_label();
    let rt = g.gc_gen.take().unwrap();
    g.jmp_label(after);
    g.bind_text_label(hook);
    quiescence::save(g);
    let entered = g.create_text_label();
    load_rt(g, R::V0, worker_view::ACTIVE);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, entered);
    // The matching release accepted the injected invalid state. Stop here
    // successfully so a later guard cannot hide this missing rejection.
    g.emit_exit_success();
    g.bind_text_label(entered);
    match mode {
        0 | 1 => {
            let field = if mode == 0 {
                worker_view::PHASE
            } else {
                worker_view::BANK
            };
            load_rt(g, R::V0, field);
            g.add_reg_imm32(R::V0, if mode == 0 { 0x10000 } else { 1 });
            store_rt(g, field, R::V0);
        }
        2 => store_rt_imm(g, worker_view::ACTIVE, 0),
        3 => store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, access_epoch), 1),
        4 => {
            allocation_records::advance(g, &rt.labels);
            // An illegal advance must fail at the actual bank boundary. If
            // that guard is removed, exit successfully before another guard
            // could mask the missing rejection.
            g.emit_exit_success();
        }
        _ => unreachable!(),
    }
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(after);
    g.gc_gen = Some(rt);
    g.gc_gen.as_mut().unwrap().labels.worker_epoch_hook = Some(hook);
}

/// Exhaust the real worker's monotonic boundary identity while it is excluded
/// by the actual caller's ticket. Only a real subsequent worker can reject it.
pub(crate) fn worker_epoch_exhaust(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    worker_view::require_quiescent(g, &rt.labels);
    g.mov_imm64(R::V0, u64::MAX);
    store_rt(g, worker_view::ENTERED, R::V0);
    store_rt(g, worker_view::COMPLETED, R::V0);
    worker::request_work(g);
    worker::unlock(g, &rt.labels);
    worker::wake_if_pending(g, &rt.labels);
    let w = rt.labels.worker.as_ref().unwrap();
    g.call_label(w.hooks.clock);
    g.mov_imm64(R::V1, 1_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let accepted = g.create_text_label();
    g.bind_text_label(again);
    worker::lock(g, &rt.labels);
    load_rt(g, R::V0, worker_view::ENTERED);
    worker::unlock(g, &rt.labels);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, accepted);
    g.call_label(w.hooks.clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, accepted);
    g.call_label(w.hooks.relax);
    g.jmp_label(again);
    g.bind_text_label(accepted);
    // No rejection (including no actual worker execution) must fail the test.
    g.emit_exit_success();
    g.gc_gen = Some(rt);
}
