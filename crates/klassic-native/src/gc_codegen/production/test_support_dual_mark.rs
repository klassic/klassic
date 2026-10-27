//! Actual shared-heap Young/Old rounds, with real peers servicing handshakes.
use super::*;

/// At a real last-peer resume, exercise the phase-excluded journal step and
/// the actual resume transition under its existing ticket. The nested hook
/// returns immediately; the outer resume then sees OWN_KIND zero. The real
/// participant resume/ACK still belongs to the ordinary checkpoint caller.
pub(crate) fn observe_resume_transport(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    let mut rt = g.gc_gen.take().unwrap();
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let data = g.asm.data_label_with_i64s(&[0; 7]);
    let hook = g.create_text_label();
    let after = g.create_text_label();
    let done = g.create_text_label();
    rt.labels.phase_resume_hook = Some(hook);
    g.jmp_label(after);
    g.bind_text_label(hook);
    quiescence::save(g);
    g.mov_data_addr(R::V1, data);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    load_rt(g, R::V0, phase_round::RESUMING);
    g.cmp_reg_imm8(R::V0, 1);
    g.jcc_label(C::NotEqual, done);
    g.store_ptr_disp32(R::V1, 40, R::V0);
    lea_rt(g, R::V2, root_work::PENDING);
    g.atomic_load_acquire(R::V0, R::V2);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.store_ptr_disp32(R::V1, 8, R::V0);
    load_rt(g, R::V0, phase_round::OWN_KIND);
    g.store_ptr_disp32(R::V1, 48, R::V0);
    g.mov_imm64(R::V0, 1);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.call_label(rt.labels.worker_journal_step);
    load_rt(g, R::V0, worker_journal::RETRY);
    g.mov_data_addr(R::V1, data);
    g.store_ptr_disp32(R::V1, 16, R::V0);
    g.cmp_reg_imm8(R::V0, 1);
    g.jcc_label(C::NotEqual, rt.labels.fail);
    load_rt(g, R::V0, RT_WORK_SEQ);
    g.store_ptr_disp32(R::V1, 24, R::V0);
    phase_round::resume_peer(g, &rt.labels);
    load_rt(g, R::V0, RT_WORK_SEQ);
    g.mov_data_addr(R::V1, data);
    g.store_ptr_disp32(R::V1, 32, R::V0);
    g.load_ptr_disp32(R::V2, R::V1, 24);
    g.cmp_reg_reg(R::V0, R::V2);
    g.jcc_label(C::Below, rt.labels.fail);
    g.jcc_label(C::Equal, rt.labels.fail);
    g.bind_text_label(done);
    quiescence::restore(g);
    g.ret();
    g.bind_text_label(after);
    g.gc_gen = Some(rt);
    data
}

pub(crate) fn resume_transport_metric(
    g: &mut crate::NativeCodeGenerator,
    data: crate::DataLabel,
    i: i32,
) {
    g.mov_data_addr(R::V0, data);
    g.load_ptr_disp32(R::V0, R::V0, i * 8);
}

pub(crate) fn run_parallel_dual_mark_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
    gate: crate::DataLabel,
) -> ParallelActors {
    run_graph(g, actors, iterations, master, GraphMode::RunningDual(gate))
}

pub(crate) fn dual_mark_enable(g: &mut crate::NativeCodeGenerator) {
    let label = g.gc_gen.as_ref().unwrap().labels.dual_mark_enable;
    call_owner(g, label);
}

pub(crate) fn dual_mark_entry(g: &mut crate::NativeCodeGenerator, young: bool, start: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let label = match (young, start) {
        (true, true) => l.young_start_actor,
        (true, false) => l.young_end_actor,
        (false, true) => l.old_start_actor,
        (false, false) => l.old_end_actor,
    };
    call_owner(g, label);
}

pub(crate) fn dual_mark_iteration(
    g: &mut crate::NativeCodeGenerator,
    iterations: u64,
    gate: crate::DataLabel,
) {
    let skip = g.create_text_label();
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, skip);
    g.load_rbp_slot(R::V0, 16);
    g.cmp_reg_imm32(R::V0, (iterations / 4) as i32);
    g.jcc_label(C::NotEqual, skip);
    dual_mark_entry(g, false, true);
    dual_mark_entry(g, true, true);
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    for field in [RT_MARK_ACTIVE, young_mark::ACTIVE] {
        load_rt(g, R::V0, field);
        g.cmp_reg_imm8(R::V0, 1);
        g.jcc_label(C::NotEqual, fail);
    }
    crate::gc_codegen::production::test_support::worker_journal_open_gate(g, gate);
    g.bind_text_label(skip);
    let poll = g.gc_gen.as_ref().unwrap().labels.poll_actor;
    call_owner(g, poll);
}

pub(crate) fn dual_mark_finish(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    arrival: crate::DataLabel,
) {
    super::super::scheduling::schedule_arrive(g, arrival);
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (poll, relax) = (l.poll_actor, l.worker.as_ref().unwrap().hooks.relax);
    let again = g.create_text_label();
    let done = g.create_text_label();
    let peer = g.create_text_label();
    let waiting = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, poll); // Early finishers still ACK real rounds.
    g.mov_data_addr(R::V1, arrival);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, owners as i32);
    g.jcc_label(C::NotEqual, waiting);
    load_rt(g, R::V0, phase_round::Y_ENDS);
    load_rt(g, R::V1, RT_MARK_ACTIVE);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, peer);
    g.test_reg_reg(R::V1, R::V1);
    g.jcc_label(C::NotEqual, peer);
    load_rt(g, R::V0, phase_round::RESUMING);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, peer);
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    let unlock_retry = g.create_text_label();
    for field in [quiescence::REQUEST, phase_round::RESUMING, RT_MARK_ACTIVE] {
        load_rt(g, R::V0, field);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, unlock_retry);
    }
    cooperative_old::not_due(g, &runtime.labels, done);
    g.bind_text_label(unlock_retry);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
    g.bind_text_label(peer);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, waiting);
    dual_mark_entry(g, true, false);
    dual_mark_entry(g, false, false);
    g.bind_text_label(waiting);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    let runtime = g.gc_gen.take().unwrap();
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

pub(crate) fn dual_mark_metric(g: &mut crate::NativeCodeGenerator, kind: usize) {
    let fields = [
        phase_round::Y_STARTS,
        phase_round::Y_ENDS,
        phase_round::Y_CHECKS,
        phase_round::Y_RETRIES,
        young_mark::ACTIVE,
        young_mark::VALID,
        young_mark::EPOCH,
        young_mark::WORKER_UNITS,
        young_mark::MARKED,
        RT_MARK_ACTIVE,
        RT_MARK_VALID,
        RT_DM_WORKER,
        allocation_records::REVISION,
        phase_round::Y_STALE_STARTS,
        phase_round::Y_STALE_ENDS,
        participants::HANDSHAKE + offset_of!(crate::gc_abi::HandshakeState, generation),
    ];
    load_rt(g, R::V0, fields[kind]);
}

/// Observe real Young completion; never runs graph work or journal consumption.
pub(crate) fn dual_mark_wait(g: &mut crate::NativeCodeGenerator, debt_entries: Option<u64>) {
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
    if let Some(n) = debt_entries {
        load_rt(
            g,
            R::V0,
            offset_of!(Runtime, worker_journal) + offset_of!(worker_journal::State, entries),
        );
        g.cmp_reg_imm32(R::V0, n as i32);
    } else {
        load_rt(g, R::V0, young_mark::DONE);
        g.cmp_reg_imm8(R::V0, 1);
    }
    g.jcc_label(C::AboveOrEqual, done);
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

pub(crate) fn dual_gate_close(g: &mut crate::NativeCodeGenerator, gate: crate::DataLabel) {
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    g.mov_data_addr(R::V1, gate);
    g.mov_imm64(R::V0, 0);
    g.atomic_store_release(R::V1, R::V0);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}

pub(crate) fn dual_native_fault(g: &mut crate::NativeCodeGenerator, actor: TestActor, kind: u64) {
    use crate::gc_abi::RootRegistry;
    select(g, actor);
    match kind {
        0 => {
            load_rt(g, R::V0, allocation_records::REVISION);
            g.add_reg_imm32(R::V0, 2);
            store_rt(g, allocation_records::REVISION, R::V0);
        }
        1 => {
            load_rt(
                g,
                R::V0,
                registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
            );
            store_rt(g, RT_CTX + offset_of!(MutatorContext, adopted_phase), R::V0);
        }
        5 => store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, private_producers), 1),
        6 => store_rt_imm(g, young_mark::ACTIVE, 0),
        7 | 14 => store_rt_imm(g, young_mark::EPOCH, 0),
        8 => store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, tlab_top), 8),
        10 => store_rt_imm(g, young_mark::VALID, 0),
        _ => {
            load_rt(
                g,
                R::V0,
                registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
            );
            if kind == 3 {
                g.and_reg_imm32(R::V0, !15);
                g.add_reg_imm32(R::V0, 5);
            } else if kind == 4 {
                g.shl_reg_imm8(R::V0, 48);
                g.shr_reg_imm8(R::V0, 48);
            } else {
                g.mov_imm64(
                    R::V1,
                    match kind {
                        2 => 0x400,
                        9 => 0x1000,
                        11 => 0x100,
                        12 => 0x200,
                        _ => 0x2000,
                    },
                );
                g.xor_reg_reg(R::V0, R::V1);
            }
            store_rt(
                g,
                registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
                R::V0,
            );
        }
    }
    let label = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
    call_owner(g, label);
    restore(g);
}

pub(crate) fn dual_mark_observed(g: &mut crate::NativeCodeGenerator, start: bool, epoch: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = if start {
        l.young_start_observed_actor
    } else {
        l.young_end_observed_actor
    };
    g.mov_imm64(R::V0, epoch);
    call_owner(g, entry);
}

pub(crate) fn dual_young_gate(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    let state = crate::gc_codegen::production::test_support::worker_journal_gate(g);
    let labels = &mut g.gc_gen.as_mut().unwrap().labels;
    labels.young_mark_gate = labels.phase_mark_gate.take();
    state
}
