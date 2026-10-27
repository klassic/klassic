//! Ordinary allocations and safepoints, without selected Young coordinators.
use super::*;

pub(crate) fn young_scheduler_minimum_target(g: &mut crate::NativeCodeGenerator) {
    store_rt_imm(g, start_policy::TARGET, start_policy::MIN);
}

pub(crate) fn run_parallel_scheduled_dual_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    run_graph(g, actors, iterations, master, GraphMode::ScheduledDual)
}

pub(crate) fn young_scheduler_metric(g: &mut crate::NativeCodeGenerator, kind: usize) {
    let fields = [
        cooperative_young::ALLOCATION_BASE,
        cooperative_young::START_REQUESTS,
        cooperative_young::END_REQUESTS,
        cooperative_young::POLLS,
        old_policy::ALLOCATED,
        start_policy::MARK_SAMPLES,
        start_policy::MARK_UNITS,
        start_policy::DURATION_NS,
        start_policy::MARK_NS,
        capacity::SEALED,
        capacity::TRACE,
        capacity::BLACK,
    ];
    load_rt(g, R::V0, fields[kind]);
}

/// Drain real scheduler demand using only ordinary safepoints. No chosen
/// coordinator/graph consumer runs here. The deadline bounds this observation.
pub(crate) fn finish_scheduled_primary(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (poll, clock, relax, fail) = (
        l.poll_actor,
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let release = g.create_text_label();
    let done = g.create_text_label();
    let old_not_due = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, poll);
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    for field in [
        quiescence::REQUEST,
        phase_round::RESUMING,
        RT_MARK_ACTIVE,
        young_mark::ACTIVE,
    ] {
        load_rt(g, R::V0, field);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, release);
    }
    cooperative_old::not_due(g, &runtime.labels, old_not_due);
    g.jmp_label(release);
    g.bind_text_label(old_not_due);
    cooperative_young::not_due(g, &runtime.labels, done);
    g.bind_text_label(release);
    worker::unlock(g, &runtime.labels);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
}
