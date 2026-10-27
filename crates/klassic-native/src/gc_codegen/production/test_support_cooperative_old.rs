//! Real allocation-driven Old scheduler fixtures and request-order latches.
use super::*;
pub(crate) fn run_parallel_scheduled_old_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    graph::run_graph(g, actors, iterations, master, GraphMode::ScheduledOld)
}
pub(super) fn schedule_arrive(g: &mut crate::NativeCodeGenerator, word: crate::DataLabel) {
    g.mov_data_addr(R::V7, word);
    let retry = g.create_text_label();
    g.bind_text_label(retry);
    g.atomic_load_acquire(R::V0, R::V7);
    g.mov_reg_reg(R::V8, R::V0);
    g.mov_reg_reg(R::V6, R::V0);
    g.add_reg_imm32(R::V6, 1);
    g.atomic_compare_exchange(R::V7, R::V6);
    g.cmp_reg_reg(R::V0, R::V8);
    g.jcc_label(C::NotEqual, retry);
}
pub(super) fn scheduled_arrival(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    ready: crate::DataLabel,
) {
    schedule_arrive(g, ready);
    let again = g.create_text_label();
    let done = g.create_text_label();
    let relax = g
        .gc_gen
        .as_ref()
        .unwrap()
        .labels
        .worker
        .as_ref()
        .unwrap()
        .hooks
        .relax;
    g.bind_text_label(again);
    g.mov_data_addr(R::V1, ready);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, owners as i32);
    g.jcc_label(C::Equal, done);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(super) fn scheduled_finish(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    iterations: u64,
    done_word: crate::DataLabel,
    dual: bool,
) {
    schedule_arrive(g, done_word);
    let again = g.create_text_label();
    let release = g.create_text_label();
    let done = g.create_text_label();
    let (poll, relax) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.poll_actor, l.worker.as_ref().unwrap().hooks.relax)
    };
    g.bind_text_label(again);
    call_owner(g, poll); // ordinary safepoint scheduling; no selected coordinator
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    g.mov_data_addr(R::V1, done_word);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, owners as i32);
    g.jcc_label(C::NotEqual, release);
    for field in [quiescence::REQUEST, phase_round::RESUMING, RT_MARK_ACTIVE] {
        load_rt(g, R::V0, field);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, release);
    }
    if dual {
        load_rt(g, R::V0, young_mark::ACTIVE);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, release);
        let old_not_due = g.create_text_label();
        cooperative_old::not_due(g, &runtime.labels, old_not_due);
        g.jmp_label(release);
        g.bind_text_label(old_not_due);
        cooperative_young::not_due(g, &runtime.labels, done);
    } else {
        // No allocation remains and no unconsumed start window may be abandoned.
        cooperative_old::not_due(g, &runtime.labels, done);
    }
    g.bind_text_label(release);
    worker::unlock(g, &runtime.labels);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
    graph::validate_closed(g, iterations);
}
pub(crate) fn scheduled_old_metric(g: &mut crate::NativeCodeGenerator, which: usize) {
    let fields = [
        cooperative_old::ENABLED,
        cooperative_old::ALLOCATION_BASE,
        cooperative_old::START_REQUESTS,
        cooperative_old::END_REQUESTS,
        cooperative_old::POLLS,
        phase_round::STALE_STARTS,
        phase_round::STALE_ENDS,
        RT_MARK_CYCLES,
        old_policy::ALLOCATED,
        cooperative_old::BANK,
        allocation_records::REVISION,
    ];
    load_rt(g, R::V0, fields[which]);
}
pub(crate) fn phase_owner_coordinations(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    load_rt(g, R::V0, phase_round::COORDINATIONS);
    restore(g);
}

/// Delay an actually chosen cycle-0 request while other ordinary allocators
/// start and finish that cycle. Service actual peer requests while delayed.
pub(crate) fn delay_scheduler_request(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    delay_request(g, false)
}
pub(crate) fn delay_young_scheduler_request(
    g: &mut crate::NativeCodeGenerator,
) -> crate::DataLabel {
    delay_request(g, true)
}
fn delay_request(g: &mut crate::NativeCodeGenerator, young: bool) -> crate::DataLabel {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let state = g.asm.data_label_with_i64s(&[0; 5]);
    let hook = g.create_text_label();
    let body = g.create_text_label();
    if young {
        g.gc_gen
            .as_mut()
            .unwrap()
            .labels
            .young_scheduler_request_hook = Some(hook);
    } else {
        g.gc_gen.as_mut().unwrap().labels.scheduler_request_hook = Some(hook);
    }
    g.jmp_label(body);
    g.bind_text_label(hook);
    let done = g.create_text_label();
    let target = g.create_text_label();
    let wait = g.create_text_label();
    let target_wait = g.create_text_label();
    let target_release = g.create_text_label();
    let target_seen = g.create_text_label();
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (poll, relax, clock, fail) = (
        l.owner_checkpoint_poll,
        l.worker.as_ref().unwrap().hooks.relax,
        l.worker.as_ref().unwrap().hooks.clock,
        l.fail,
    );
    load_rt(g, R::V0, phase_round::REQUESTED_CYCLE);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.call_label(clock);
    g.mov_imm64(R::V1, 120_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(88, R::V0);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 3);
    g.jcc_label(C::Equal, target);
    g.bind_text_label(wait);
    g.call_label(poll);
    g.mov_data_addr(R::V1, state);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 88);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(wait);
    g.bind_text_label(target);
    g.mov_data_addr(R::V1, state);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    g.bind_text_label(target_wait);
    g.call_label(poll);
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    for f in [
        quiescence::REQUEST,
        if young {
            young_mark::ACTIVE
        } else {
            RT_MARK_ACTIVE
        },
    ] {
        load_rt(g, R::V0, f);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, target_release);
    }
    load_rt(
        g,
        R::V0,
        if young {
            young_mark::EPOCH
        } else {
            RT_MARK_CYCLES
        },
    );
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, target_seen);
    g.bind_text_label(target_release);
    worker::unlock(g, &runtime.labels);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 88);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(target_wait);
    g.bind_text_label(target_seen);
    g.mov_data_addr(R::V1, state);
    g.store_ptr_disp32(R::V1, 8, R::V0);
    load_rt(g, R::V0, phase_round::REQUESTED_CYCLE);
    g.store_ptr_disp32(R::V1, 16, R::V0);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
    g.bind_text_label(done);
    g.ret();
    g.bind_text_label(body);
    if young {
        // Observe the actual argument at the admission boundary, after the
        // scheduler reloads its saved token. Other owners' stale counters
        // cannot witness this particular delayed request.
        let entry = g.create_text_label();
        let after = g.create_text_label();
        let done = g.create_text_label();
        g.gc_gen.as_mut().unwrap().labels.young_observed_entry_hook = Some(entry);
        g.jmp_label(after);
        g.bind_text_label(entry);
        quiescence::save(g);
        g.mov_reg_reg(R::V2, R::V0);
        load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
        g.cmp_reg_imm32(R::V0, 3);
        g.jcc_label(C::NotEqual, done);
        g.mov_data_addr(R::V1, state);
        g.load_ptr_disp32(R::V0, R::V1, 8);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, done);
        g.load_ptr_disp32(R::V0, R::V1, 32);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, done);
        g.store_ptr_disp32(R::V1, 24, R::V2);
        g.mov_imm64(R::V0, 1);
        g.store_ptr_disp32(R::V1, 32, R::V0);
        g.bind_text_label(done);
        quiescence::restore(g);
        g.ret();
        g.bind_text_label(after);
    }
    state
}
pub(crate) fn scheduler_delay_metric(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
    observed: bool,
) {
    g.mov_data_addr(R::V1, state);
    g.load_ptr_disp32(R::V0, R::V1, if observed { 8 } else { 16 });
}
pub(crate) fn delayed_young_entry_metric(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
    seen: bool,
) {
    g.mov_data_addr(R::V1, state);
    g.load_ptr_disp32(R::V0, R::V1, if seen { 32 } else { 24 });
}
pub(crate) fn cooperative_enable(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.cooperative_old_enable;
    call_owner(g, entry);
}
pub(crate) fn observed_old_entry(g: &mut crate::NativeCodeGenerator, start: bool, cycle: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let entry = if start {
        l.old_start_observed_actor
    } else {
        l.old_end_observed_actor
    };
    g.mov_imm64(R::V0, cycle);
    call_owner(g, entry);
}
pub(crate) fn one_explicit_old_cycle(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (start, end, relax) = (
        l.old_start_actor,
        l.old_end_actor,
        l.worker.as_ref().unwrap().hooks.relax,
    );
    call_owner(g, start);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, end);
    load_rt(g, R::V0, RT_MARK_ACTIVE);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}

pub(crate) fn enable_cooperative_other(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    cooperative_enable(g);
    restore(g);
}
pub(crate) fn cooperative_private_fault(g: &mut crate::NativeCodeGenerator) {
    store_rt_imm(g, RT_CTX + offset_of!(MutatorContext, private_producers), 1);
    cooperative_enable(g);
}
pub(crate) fn legacy_collect_entry(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.collect;
    g.call_label(entry);
}
/// Observe actual collection-call state at the real invariant exit. This
/// distinguishes rejection at entry from a later failure after mutation.
pub(crate) fn observe_legacy_collect_failure(g: &mut crate::NativeCodeGenerator) {
    g.gc_log = true;
    let hook = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.invariant_hook = Some(hook);
    g.jmp_label(body);
    g.bind_text_label(hook);
    let name = b"collect_active=";
    let data = g.asm.data_label_with_bytes(name);
    g.emit_write_data(1, data, name.len());
    load_rt(g, R::V7, mutator_time::ACTIVE);
    g.mov_imm64(R::V6, 1);
    g.mov_imm64(R::V2, 0);
    g.call_label(g.print_i64);
    g.emit_write_data(1, g.newline, 1);
    g.ret();
    g.bind_text_label(body);
}
