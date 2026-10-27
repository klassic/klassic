//! Actual ordinary allocations/polls initiate and finish moving rounds. No
//! chosen logical owner issues the collector's start or end entries.
//! Waits reject stalled real work, rather than treating wall time spent on
//! 22,000 peer allocations/marking as a collector latency guarantee. Only
//! allocation, consumed work and actual phase progress refresh the deadline.
use super::*;
const PROGRESS: [usize; 7] = [
    old_policy::ALLOCATED,
    root_work::ACKNOWLEDGED,
    young_mark::WORKER_UNITS,
    RT_DM_WORKER,
    allocation_records::REVISION,
    quiescence::COMPLETED,
    RT_RETIRE_STAGE,
];
fn begin_wait(g: &mut crate::NativeCodeGenerator, slot: i32, clock: crate::TextLabel) {
    for (i, field) in PROGRESS.into_iter().enumerate() {
        load_rt(g, R::V0, field);
        g.store_rbp_slot(slot - 8 * (i as i32 + 1), R::V0);
    }
    g.call_label(clock);
    g.mov_imm64(R::V1, 30_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(slot, R::V0);
}
fn reject_stalled(
    g: &mut crate::NativeCodeGenerator,
    slot: i32,
    clock: crate::TextLabel,
    timeout: crate::TextLabel,
) {
    let advanced = g.create_text_label();
    let done = g.create_text_label();
    for (i, field) in PROGRESS.into_iter().enumerate() {
        load_rt(g, R::V0, field);
        g.load_rbp_slot(R::V1, slot - 8 * (i as i32 + 1));
        g.cmp_reg_reg(R::V0, R::V1);
        g.jcc_label(C::NotEqual, advanced);
    }
    g.call_label(clock);
    g.load_rbp_slot(R::V1, slot);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, timeout);
    g.jmp_label(done);
    g.bind_text_label(advanced);
    begin_wait(g, slot, clock);
    g.bind_text_label(done);
}
/// Exercise an already expired fixture deadline without a thirty-second sleep.
/// Ordinary idle polling cannot refresh it; a real allocation can.
pub(crate) fn moving_wait_probe(g: &mut crate::NativeCodeGenerator, allocate: bool) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, poll, alloc, publish, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.poll_actor,
        l.alloc,
        l.publish_pending,
        l.fail,
    );
    begin_wait(g, 1856, clock);
    g.mov_imm64(R::V0, 0);
    g.store_rbp_slot(1856, R::V0);
    g.call_label(poll);
    if allocate {
        g.mov_imm64(R::V7, 16);
        g.mov_imm64(R::V6, 1);
        g.call_label(alloc);
        g.mov_imm64(R::V1, 0);
        g.store_ptr_disp32(R::V0, 0, R::V1);
        g.store_ptr_disp32(R::V0, 8, R::V1);
        g.call_label(publish);
    }
    reject_stalled(g, 1856, clock, fail);
}
pub(crate) fn run_parallel_scheduled_moving_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    graph::run_graph(g, actors, iterations, master, GraphMode::ScheduledMoving)
}
pub(super) fn finish(g: &mut crate::NativeCodeGenerator) {
    if let Some(hook) = g
        .gc_gen
        .as_ref()
        .unwrap()
        .labels
        .moving_scheduler_finish_hook
    {
        g.call_label(hook);
    }
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (poll, clock, relax, fail) = (
        l.poll_actor,
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    begin_wait(g, 1856, clock);
    let again = g.create_text_label();
    let release = g.create_text_label();
    let closed = g.create_text_label();
    let done = g.create_text_label();
    let timeout = g.create_text_label();
    g.bind_text_label(again);
    graph::call_owner(g, poll);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    for field in [
        quiescence::REQUEST,
        phase_round::RESUMING,
        moving_round::DEMAND,
        RT_PHASE_OPEN,
        RT_RETIRE_STAGE,
        RT_MARK_ACTIVE,
        young_mark::ACTIVE,
    ] {
        load_rt(g, R::V0, field);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, release);
    }
    moving_round::scheduler::not_due(g, &rt.labels, closed);
    g.bind_text_label(release);
    worker::unlock(g, &rt.labels);
    reject_stalled(g, 1856, clock, timeout);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(closed);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    g.jmp_label(done);
    g.bind_text_label(timeout);
    for (name, field) in [
        (
            "finish_owner",
            RT_CTX + offset_of!(MutatorContext, thread_id),
        ),
        ("finish_pending", RT_PENDING_REF),
        ("finish_debt", root_work::PENDING),
        ("finish_allocated", old_policy::ALLOCATED),
        ("finish_base", moving_round::scheduler::ALLOCATION_BASE),
        ("finish_target", start_policy::TARGET),
        ("finish_young", young_mark::ACTIVE),
        ("finish_young_done", young_mark::DONE),
        ("finish_demand", moving_round::DEMAND),
        (
            "finish_start_requests",
            moving_round::scheduler::START_REQUESTS,
        ),
        (
            "finish_journal_entries",
            offset_of!(Runtime, worker_journal) + offset_of!(worker_journal::State, entries),
        ),
        (
            "finish_journal_deferred",
            offset_of!(Runtime, worker_journal) + offset_of!(worker_journal::State, deferred),
        ),
    ] {
        let data = g.asm.data_label_with_bytes(format!("{name}=").as_bytes());
        g.emit_write_data(1, data, name.len() + 1);
        load_rt(g, R::V0, field);
        g.push_reg(R::V3);
        g.push_reg(R::OwnerContext);
        g.mov_reg_reg(R::V7, R::V0);
        g.mov_imm64(R::V6, 1);
        g.mov_imm64(R::V2, 0);
        g.call_label(g.print_i64);
        g.pop_reg(R::OwnerContext);
        g.pop_reg(R::V3);
        g.emit_write_data(1, g.newline, 1);
    }
    g.jmp_label(fail);
    g.bind_text_label(done);
}
pub(crate) fn moving_scheduler_metric(g: &mut crate::NativeCodeGenerator, which: usize) {
    let field = [
        moving_round::scheduler::ALLOCATION_BASE,
        moving_round::scheduler::POLLS,
        moving_round::scheduler::START_REQUESTS,
        moving_round::scheduler::END_REQUESTS,
        moving_round::scheduler::STALE_STARTS,
        moving_round::scheduler::STALE_ENDS,
        old_policy::ALLOCATED,
    ][which];
    load_rt(g, R::V0, field);
}

/// Hold an actual observed request while other real owners change the bank.
/// The delayed owner still services every real quorum through its own poll.
pub(crate) fn delay_moving_scheduler(g: &mut crate::NativeCodeGenerator, start: bool) {
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let state = g.asm.data_label_with_i64s(&[0, 0]);
    let hook = g.create_text_label();
    let returned = g.create_text_label();
    let finishing = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen
        .as_mut()
        .unwrap()
        .labels
        .moving_scheduler_request_hook = Some(hook);
    g.gc_gen
        .as_mut()
        .unwrap()
        .labels
        .moving_scheduler_return_hook = Some(returned);
    g.gc_gen
        .as_mut()
        .unwrap()
        .labels
        .moving_scheduler_finish_hook = Some(finishing);
    g.jmp_label(body);
    g.bind_text_label(hook);
    let done = g.create_text_label();
    let timeout = g.create_text_label();
    let bad_return = g.create_text_label();
    let target = g.create_text_label();
    let again = g.create_text_label();
    let seen = g.create_text_label();
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (poll, clock, relax, fail) = (
        l.owner_checkpoint_poll,
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    let ordinary_poll = l.poll_actor;
    let field = if start {
        moving_round::REQUESTED_BANK
    } else {
        moving_round::REQUESTED_END_BANK
    };
    let bank = if start { 1 } else { 2 };
    load_rt(g, R::V0, moving_round::SCHEDULER_KIND);
    g.cmp_reg_imm32(R::V0, if start { 1 } else { 2 });
    g.jcc_label(C::NotEqual, done);
    load_rt(g, R::V0, field);
    g.cmp_reg_imm32(R::V0, bank);
    g.jcc_label(C::NotEqual, done);
    begin_wait(g, 1984, clock);
    // Delay the first actual request. A chosen owner can finish this bank
    // through allocation admission without ever making a scheduler request;
    // other requesters must not wait for that event before progressing.
    load_rt(g, R::V6, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.mov_data_addr(R::V7, state);
    g.mov_imm64(R::V0, 0);
    g.atomic_compare_exchange(R::V7, R::V6);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.bind_text_label(target);
    if !start {
        // Ensure another real collection is demanded even if adaptive feedback
        // has consumed all fresh credit by this late end request. Peers perform
        // the public capacity request through their ordinary scheduler polls.
        let rt = g.gc_gen.take().unwrap();
        worker::lock(g, &rt.labels);
        store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
        worker::unlock(g, &rt.labels);
        g.gc_gen = Some(rt);
    }
    g.bind_text_label(again);
    g.call_label(poll);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    load_rt(g, R::V0, moving_round::CURRENT_BANK);
    g.cmp_reg_imm32(R::V0, bank);
    g.jcc_label(C::Above, seen);
    worker::unlock(g, &rt.labels);
    reject_stalled(g, 1984, clock, timeout);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(seen);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, 104, R::V0);
    load_rt(g, R::V0, field);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, 96, R::V0);
    let stale = if start {
        moving_round::OWN_STALE_STARTS
    } else {
        moving_round::OWN_STALE_ENDS
    };
    load_rt(g, R::V0, stale);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, 88, R::V0);
    g.mov_imm64(R::V0, 1);
    g.store_ptr_disp32(R::V1, 72, R::V0);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    if !start {
        // Force the observed-end/service ordering using the peers' next real
        // handshake. This owner has observed a newer open bank and withholds
        // its next ACK until the stale end entry services that handshake.
        // No handshake state or completion receipt is manufactured here.
        let pending_handshake = g.create_text_label();
        g.bind_text_label(pending_handshake);
        lea_rt(g, R::V1, quiescence::REQUEST);
        g.atomic_load_acquire(R::V0, R::V1);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, done);
        reject_stalled(g, 1984, clock, timeout);
        g.call_label(relax);
        g.jmp_label(pending_handshake);
    }
    g.bind_text_label(done);
    g.ret();
    g.bind_text_label(returned);
    let ignored = g.create_text_label();
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.mov_data_addr(R::V1, state);
    g.atomic_load_acquire(R::V1, R::V1);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, ignored);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V0, R::V1, 72);
    g.cmp_reg_imm32(R::V0, 1);
    g.jcc_label(C::NotEqual, ignored);
    g.load_ptr_disp32(R::V1, R::V1, 88);
    g.add_reg_imm32(R::V1, 1);
    load_rt(g, R::V0, stale);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, bad_return);
    g.load_rbp_slot(R::V1, 8);
    g.mov_imm64(R::V0, 2);
    g.store_ptr_disp32(R::V1, 72, R::V0);
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, 8);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    g.bind_text_label(ignored);
    g.ret();
    g.bind_text_label(finishing);
    // A peer must not leave Running between the delayed request's selection
    // and its capacity demand. Keep actual ordinary polls alive until the
    // selected owner has returned through the verified stale branch. This
    // controls only fixture exit; no GC quorum/phase/epoch is written here.
    quiescence::save(g);
    begin_wait(g, 1920, clock);
    let finish_again = g.create_text_label();
    let finish_done = g.create_text_label();
    g.bind_text_label(finish_again);
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, 8);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, finish_done);
    graph::call_owner(g, ordinary_poll);
    reject_stalled(g, 1920, clock, timeout);
    g.call_label(relax);
    g.jmp_label(finish_again);
    g.bind_text_label(finish_done);
    quiescence::restore(g);
    g.ret();
    for (label, text) in [
        (timeout, "moving delayed request exceeded its deadline\n"),
        (
            bad_return,
            "moving delayed request did not take its stale branch\n",
        ),
    ] {
        g.bind_text_label(label);
        let data = g.asm.data_label_with_bytes(text.as_bytes());
        g.emit_write_data(2, data, text.len());
        if label == timeout {
            for (name, field) in [
                (
                    "delayed_owner",
                    RT_CTX + offset_of!(MutatorContext, thread_id),
                ),
                ("delayed_bank", moving_round::CURRENT_BANK),
                ("delayed_observed", field),
                ("delayed_kind", moving_round::SCHEDULER_KIND),
                ("delayed_debt", root_work::PENDING),
                ("delayed_pending", RT_PENDING_REF),
                ("delayed_allocated", old_policy::ALLOCATED),
                (
                    "delayed_allocation_base",
                    moving_round::scheduler::ALLOCATION_BASE,
                ),
                ("delayed_target", start_policy::TARGET),
                ("delayed_young_done", young_mark::DONE),
                ("delayed_worker_alive", RT_WORKER_ALIVE),
                ("delayed_young_enabled", cooperative_old::YOUNG_ENABLED),
                ("delayed_journal_retry", worker_journal::RETRY),
                (
                    "delayed_journal_entries",
                    offset_of!(Runtime, worker_journal)
                        + offset_of!(worker_journal::State, entries),
                ),
                (
                    "delayed_journal_deferred",
                    offset_of!(Runtime, worker_journal)
                        + offset_of!(worker_journal::State, deferred),
                ),
                ("delayed_phase_kind", phase_round::KIND),
                (
                    "delayed_start_requests",
                    moving_round::scheduler::START_REQUESTS,
                ),
                ("delayed_open", RT_PHASE_OPEN),
                ("delayed_retiring", RT_RETIRE_STAGE),
                ("delayed_old", RT_MARK_ACTIVE),
                ("delayed_young", young_mark::ACTIVE),
                ("delayed_demand", old_cycle::REQUEST),
                ("delayed_quorum", quiescence::REQUEST),
                ("delayed_resuming", phase_round::RESUMING),
            ] {
                let data = g.asm.data_label_with_bytes(format!("{name}=").as_bytes());
                g.emit_write_data(1, data, name.len() + 1);
                load_rt(g, R::V0, field);
                g.push_reg(R::V3);
                g.push_reg(R::OwnerContext);
                g.mov_reg_reg(R::V7, R::V0);
                g.mov_imm64(R::V6, 1);
                g.mov_imm64(R::V2, 0);
                g.call_label(g.print_i64);
                g.pop_reg(R::OwnerContext);
                g.pop_reg(R::V3);
                g.emit_write_data(1, g.newline, 1);
            }
        }
        g.jmp_label(fail);
    }
    g.bind_text_label(body);
}
pub(crate) fn moving_owner_stale(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    start: bool,
) {
    select(g, actor);
    load_rt(
        g,
        R::V0,
        if start {
            moving_round::OWN_STALE_STARTS
        } else {
            moving_round::OWN_STALE_ENDS
        },
    );
    restore(g);
}

pub(crate) fn moving_idle_polls(g: &mut crate::NativeCodeGenerator) {
    let poll = g.gc_gen.as_ref().unwrap().labels.poll_actor;
    let again = g.create_text_label();
    g.mov_imm64(R::V6, 1024);
    g.bind_text_label(again);
    graph::call_owner(g, poll);
    g.sub_reg_imm8(R::V6, 1);
    g.jcc_label(C::NotEqual, again);
}
pub(crate) fn moving_owner_tlab_metric(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    restored: bool,
) {
    select(g, actor);
    let base = owner_state::offset(
        offset_of!(OwnerState, phase_round) + offset_of!(phase_round::Owner, tlab),
    );
    load_rt(
        g,
        R::V0,
        base + if restored {
            offset_of!(old_tlab::State, restores)
        } else {
            offset_of!(old_tlab::State, suspends)
        },
    );
    restore(g);
}
