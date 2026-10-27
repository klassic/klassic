//! Shared managed cycles accessed with the explicit owner root/field entries.
//! No raw observation or transient field address crosses a GC safepoint here.
use super::*;

pub(crate) fn call_owner(g: &mut crate::NativeCodeGenerator, label: crate::TextLabel) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.push_reg(R::V3);
    g.mov_imm64(R::V3, 123); // every boundary must derive its actual heap
    g.call_label(label);
    g.cmp_reg_imm32(R::V3, 123);
    g.jcc_label(C::NotEqual, fail);
    g.pop_reg(R::V3);
}
fn root(g: &mut crate::NativeCodeGenerator, slot: i32) {
    let label = g.gc_gen.as_ref().unwrap().labels.root_adopt_actor;
    g.store_rbp_slot(slot, R::V0);
    g.mov_reg_reg(R::V0, R::V5);
    g.add_reg_imm32(R::V0, -slot);
    call_owner(g, label);
}
fn load(g: &mut crate::NativeCodeGenerator, slot: i32) {
    let label = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    g.load_rbp_slot(R::V0, slot);
    call_owner(g, label);
}
fn store(g: &mut crate::NativeCodeGenerator, slot: i32) {
    let label = g.gc_gen.as_ref().unwrap().labels.root_store_actor;
    g.mov_reg_reg(R::V1, R::V5);
    g.add_reg_imm32(R::V1, -slot);
    call_owner(g, label);
}
fn pop(g: &mut crate::NativeCodeGenerator) {
    let label = g.gc_gen.as_ref().unwrap().labels.root_pop_actor;
    call_owner(g, label);
}
fn field_store(g: &mut crate::NativeCodeGenerator) {
    let label = g.gc_gen.as_ref().unwrap().labels.field_store_actor;
    call_owner(g, label);
}
fn field_load(g: &mut crate::NativeCodeGenerator) {
    let label = g.gc_gen.as_ref().unwrap().labels.field_load_actor;
    g.atomic_load_acquire(R::V0, R::V10);
    call_owner(g, label);
}
fn hub(g: &mut crate::NativeCodeGenerator) {
    let label = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    g.load_rbp_slot(R::V0, 8);
    g.load_ptr_disp32(R::V0, R::V0, HUB_ROOT);
    call_owner(g, label);
}

pub(super) fn iteration(g: &mut crate::NativeCodeGenerator, owners: u64, iterations: u64) {
    let labels = &g.gc_gen.as_ref().unwrap().labels;
    let (alloc, publish, fail) = (labels.alloc_actor, labels.publish_pending, labels.fail);
    g.mov_imm64(R::V7, 16);
    g.mov_imm64(R::V6, 1);
    call_owner(g, alloc);
    load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.shl_reg_imm8(R::V1, 32);
    g.load_rbp_slot(R::V2, 16);
    g.or_reg_reg(R::V1, R::V2);
    g.store_ptr_disp32(R::V0, 0, R::V1);
    root(g, 32);
    // The next allocation finishes this leaf's initialization first.
    g.mov_imm64(R::V7, 16);
    g.mov_imm64(R::V6, 2);
    call_owner(g, alloc);
    root(g, 40);
    load(g, 32);
    g.mov_reg_reg(R::V6, R::V0);
    load(g, 40);
    g.mov_reg_reg(R::V7, R::V0);
    field_store(g); // parent.leaf
    hub(g);
    g.mov_reg_reg(R::V6, R::V0);
    g.add_reg_imm32(R::V7, 8);
    field_store(g); // parent.hub, a real cycle once the hub slot is published
    g.call_label(publish); // both reference fields are initialized before escape
    hub(g);
    load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.sub_reg_imm8(R::V1, 2);
    g.shl_reg_imm8(R::V1, 3);
    g.mov_reg_reg(R::V7, R::V0);
    g.add_reg_reg(R::V7, R::V1);
    load(g, 40);
    g.mov_reg_reg(R::V6, R::V0);
    field_store(g); // release-publish this parent's initialized fields
    // Exercise actual owned root replacement, null read and LIFO pop.
    g.mov_imm64(R::V0, 0);
    root(g, 48);
    load(g, 32);
    g.store_rbp_slot(72, R::V0);
    store(g, 48);
    load(g, 48);
    g.load_rbp_slot(R::V1, 72);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    g.mov_imm64(R::V0, 0);
    store(g, 48);
    load(g, 48);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    pop(g);
    // Read the next owner's actual shared slot and traverse its parent/leaf.
    hub(g);
    g.store_rbp_slot(56, R::V0);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.sub_reg_imm8(R::V0, 1);
    g.mov_imm64(R::V6, owners);
    g.div_unsigned(R::V6);
    g.mov_reg_reg(R::V1, R::V2);
    g.add_reg_imm32(R::V1, 2);
    g.store_rbp_slot(64, R::V1);
    g.shl_reg_imm8(R::V2, 3);
    g.load_rbp_slot(R::V10, 56);
    g.add_reg_reg(R::V10, R::V2);
    field_load(g);
    let empty = g.create_text_label();
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, empty);
    g.store_rbp_slot(80, R::V0);
    g.mov_reg_reg(R::V10, R::V0);
    field_load(g);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, fail);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    g.mov_reg_reg(R::V1, R::V0);
    g.shr_reg_imm8(R::V1, 32);
    g.load_rbp_slot(R::V2, 64);
    g.cmp_reg_reg(R::V1, R::V2);
    g.jcc_label(C::NotEqual, fail);
    g.shl_reg_imm8(R::V0, 32);
    g.shr_reg_imm8(R::V0, 32);
    g.cmp_reg_imm32(R::V0, iterations as i32);
    g.jcc_label(C::AboveOrEqual, fail);
    g.load_rbp_slot(R::V10, 80);
    g.add_reg_imm32(R::V10, 8);
    field_load(g);
    g.load_rbp_slot(R::V1, 56);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V0, R::V1, GRAPH_READS);
    g.add_reg_imm32(R::V0, 1);
    g.store_ptr_disp32(R::V1, GRAPH_READS, R::V0);
    g.bind_text_label(empty);
    pop(g);
    pop(g);
}

/// Input master contains a RootCell pointer belonging to the primary context.
/// Every owner gets its own real LIFO RootCell for that same managed hub.
pub(crate) fn run_parallel_actor_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    run_graph(g, actors, iterations, master, GraphMode::Idle)
}
pub(crate) fn run_parallel_checkpoint_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    run_graph(g, actors, iterations, master, GraphMode::Checkpoint)
}
pub(crate) fn run_parallel_old_mark_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    run_graph(g, actors, iterations, master, GraphMode::OldMark)
}
pub(crate) fn run_parallel_closed_old_mark_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    run_graph(g, actors, iterations, master, GraphMode::CloseOldMark)
}
pub(super) fn run_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
    mode: GraphMode,
) -> ParallelActors {
    let checkpoint = mode.checkpoint();
    let old_mark = mode.old_mark();
    let labels = &g.gc_gen.as_ref().unwrap().labels;
    let (read, adopt) = (labels.root_load, labels.root_adopt_actor);
    let mut roots = Vec::new();
    for actor in actors {
        g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
        let cell = g.asm.data_label_with_i64s(&[0]);
        g.mov_data_addr(R::V0, master);
        g.load_ptr_disp32(R::V0, R::V0, 0);
        g.call_label(read);
        g.mov_data_addr(R::V1, cell);
        g.store_ptr_disp32(R::V1, 0, R::V0);
        select(g, *actor);
        g.mov_data_addr(R::V0, cell);
        call_owner(g, adopt);
        if old_mark
            || matches!(
                mode,
                GraphMode::ScheduledOld
                    | GraphMode::ScheduledDual
                    | GraphMode::RunningDual(_)
                    | GraphMode::Moving(..)
                    | GraphMode::ScheduledMoving
            )
        {
            let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
            call_owner(g, enter);
        }
        restore(g);
        roots.push(cell);
    }
    if !checkpoint
        && !old_mark
        && !matches!(
            mode,
            GraphMode::Moving(..)
                | GraphMode::ScheduledMoving
                | GraphMode::RunningOld
                | GraphMode::ScheduledOld
                | GraphMode::ScheduledDual
                | GraphMode::RunningDual(_)
        )
    {
        store_rt_imm(g, RT_COLLECTING, 77);
    } // scratch isolation sentinel
    if old_mark {
        let runtime = g.gc_gen.take().unwrap();
        mark_prepare::ensure(g, &runtime.labels);
        collect::clear_scratch(g, &runtime.labels);
        worker::lock(g, &runtime.labels);
        mark::start_ready(g, &runtime.labels);
        g.gc_gen = Some(runtime);
    }
    if matches!(mode, GraphMode::RunningDual(_)) {
        // These fixtures choose their real Young end after all allocations.
        // Bootstrap promotion may have enabled automatic Young demand; its
        // ordinary end poll must not silently close this explicit snapshot.
        crate::gc_codegen::production::test_support::set_young_auto(g, false);
        let enable = g.gc_gen.as_ref().unwrap().labels.dual_mark_enable;
        call_owner(g, enable);
    }
    if matches!(mode, GraphMode::ScheduledDual) {
        crate::gc_codegen::production::test_support::set_young_auto(g, true);
        store_rt_imm(g, start_policy::TARGET, start_policy::MIN);
        let enable = g.gc_gen.as_ref().unwrap().labels.dual_mark_enable;
        call_owner(g, enable);
    }
    if matches!(mode, GraphMode::ScheduledOld) {
        store_rt_imm(g, start_policy::TARGET, start_policy::MIN);
        let enable = g.gc_gen.as_ref().unwrap().labels.cooperative_old_enable;
        call_owner(g, enable);
    }
    if let GraphMode::Moving(state, ..) = mode {
        let enable = g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
        call_owner(g, enable);
        moving::start_preserved_old(g, state);
    }
    if matches!(mode, GraphMode::ScheduledMoving) {
        store_rt_imm(g, start_policy::TARGET, start_policy::MIN);
        let enable = g.gc_gen.as_ref().unwrap().labels.moving_auto_enable_actor;
        call_owner(g, enable);
    }
    run_parallel_impl(g, actors, iterations, Some(&roots), mode)
}
pub(crate) fn parallel_graph_reads(
    g: &mut crate::NativeCodeGenerator,
    p: &ParallelActors,
    i: usize,
) {
    g.mov_data_addr(R::V0, p.controls[i]);
    g.load_ptr_disp32(R::V0, R::V0, GRAPH_READS);
}
pub(crate) fn primary_copy_guard(g: &mut crate::NativeCodeGenerator, reset: bool) {
    load_rt(g, R::V0, RT_COLLECTING);
    if reset {
        g.push_reg(R::V0);
        g.push_reg(R::V0);
        store_rt_imm(g, RT_COLLECTING, 0);
        g.pop_reg(R::V0);
        g.pop_reg(R::V0);
    }
}

pub(super) fn checkpoint_iteration(g: &mut crate::NativeCodeGenerator, iterations: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (request, poll) = (l.checkpoint_actor, l.poll_actor);
    let skip = g.create_text_label();
    g.load_rbp_slot(R::V0, 16);
    g.cmp_reg_imm32(R::V0, (iterations / 2) as i32);
    g.jcc_label(C::NotEqual, skip);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, skip);
    call_owner(g, request);
    g.bind_text_label(skip);
    call_owner(g, poll);
}
pub(super) fn checkpoint_finish(g: &mut crate::NativeCodeGenerator) {
    let poll = g.gc_gen.as_ref().unwrap().labels.poll_actor;
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
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, poll);
    lea_rt(g, R::V1, quiescence::COMPLETED);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn checkpoint_metric(g: &mut crate::NativeCodeGenerator, which: u64) {
    use crate::gc_abi::HandshakeState as HS;
    let f = match which {
        0 => quiescence::COMPLETED,
        1 => quiescence::EXPECTED,
        2 => quiescence::ACKNOWLEDGED,
        3 => participants::HANDSHAKE + offset_of!(HS, active),
        4 => participants::HANDSHAKE + offset_of!(HS, native),
        5 => quiescence::OWN_ACKS,
        6 => native_io::ENTRIES,
        7 => native_io::EXITS,
        _ => RT_CTX + offset_of!(MutatorContext, tlab_top),
    };
    load_rt(g, R::V0, f);
}

/// Last peer remains Running until the real coordinator has observed an
/// incomplete quorum. No timeout/sleep or fabricated ACK drives completion.
pub(super) fn delay_last_peer(g: &mut crate::NativeCodeGenerator, owners: u64) {
    use crate::gc_abi::HandshakeState as HS;
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
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
    let done = g.create_text_label();
    let wait = g.create_text_label();
    let retry = g.create_text_label();
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, owners as i32 + 1);
    g.jcc_label(C::NotEqual, done);
    g.bind_text_label(wait);
    lea_rt(g, R::V1, quiescence::REQUEST);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, retry);
    lea_rt(
        g,
        R::V1,
        participants::HANDSHAKE + offset_of!(HS, acknowledged),
    );
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, owners as i32 - 1);
    g.jcc_label(C::NotEqual, retry);
    lea_rt(g, R::V1, quiescence::RETRIES);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, retry);
    g.load_rbp_slot(R::V1, 8);
    g.mov_imm64(R::V0, owners - 1);
    g.store_ptr_disp32(R::V1, DELAYED_ACKS, R::V0);
    lea_rt(g, R::V1, quiescence::COMPLETED);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, DELAYED_COMPLETE, R::V0);
    g.jmp_label(done);
    g.bind_text_label(retry);
    g.call_label(relax);
    g.jmp_label(wait);
    g.bind_text_label(done);
}
pub(crate) fn delayed_checkpoint_metric(
    g: &mut crate::NativeCodeGenerator,
    p: &ParallelActors,
    which: u64,
) {
    if which == 0 {
        load_rt(g, R::V0, quiescence::RETRIES);
    } else {
        g.mov_data_addr(R::V0, *p.controls.last().unwrap());
        g.load_ptr_disp32(
            R::V0,
            R::V0,
            if which == 1 {
                DELAYED_ACKS
            } else {
                DELAYED_COMPLETE
            },
        );
    }
}

/// Each real owner observes real worker credit, then allocates the latter half
/// of its graph. The worker had no snapshot work before the parent's gate.
pub(super) fn old_worker_iteration(g: &mut crate::NativeCodeGenerator, iterations: u64) {
    let skip = g.create_text_label();
    let again = g.create_text_label();
    let seen = g.create_text_label();
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let fail = l.fail;
    let relax = l.worker.as_ref().unwrap().hooks.relax;
    g.load_rbp_slot(R::V0, 16);
    g.cmp_reg_imm32(R::V0, (iterations / 2).saturating_sub(1) as i32);
    g.jcc_label(C::NotEqual, skip);
    g.mov_imm64(R::V0, 100000);
    g.store_rbp_slot(88, R::V0);
    g.bind_text_label(again);
    lea_rt(g, R::V1, RT_DM_WORKER);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, seen);
    g.call_label(relax);
    g.load_rbp_slot(R::V0, 88);
    g.sub_reg_imm8(R::V0, 1);
    g.store_rbp_slot(88, R::V0);
    g.jcc_label(C::Equal, fail);
    g.jmp_label(again);
    g.bind_text_label(seen);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, OLD_WORKER, R::V0);
    g.bind_text_label(skip);
}
pub(crate) fn parallel_old_worker(
    g: &mut crate::NativeCodeGenerator,
    p: &ParallelActors,
    i: usize,
) {
    g.mov_data_addr(R::V1, p.controls[i]);
    g.load_ptr_disp32(R::V0, R::V1, OLD_WORKER);
}

pub(super) fn validate_closed(g: &mut crate::NativeCodeGenerator, iterations: u64) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    hub(g);
    load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.sub_reg_imm8(R::V1, 2);
    g.shl_reg_imm8(R::V1, 3);
    g.mov_reg_reg(R::V10, R::V0);
    g.add_reg_reg(R::V10, R::V1);
    field_load(g);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, fail);
    g.mov_reg_reg(R::V10, R::V0);
    field_load(g);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, fail);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.shl_reg_imm8(R::V1, 32);
    g.add_reg_imm32(R::V1, (iterations - 1) as i32);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
}

pub(crate) fn run_parallel_running_old_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    run_graph(g, actors, iterations, master, GraphMode::RunningOld)
}
pub(super) fn running_old_iteration(g: &mut crate::NativeCodeGenerator, iterations: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (start, end, poll) = (l.old_start_actor, l.old_end_actor, l.poll_actor);
    for (at, entry) in [(iterations / 4, start), (iterations * 3 / 4, end)] {
        let skip = g.create_text_label();
        load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
        g.cmp_reg_imm32(R::V0, 2);
        g.jcc_label(C::NotEqual, skip);
        g.load_rbp_slot(R::V0, 16);
        g.cmp_reg_imm32(R::V0, at as i32);
        g.jcc_label(C::NotEqual, skip);
        call_owner(g, entry);
        if at == iterations / 4 && g.gc_gen.as_ref().unwrap().labels.phase_mark_gate.is_some() {
            // N=1 has no intervening mutator: root discovery is actually
            // pending, with no synthetic queued job or completion hint.
            call_owner(g, end);
        }

        g.bind_text_label(skip);
    }
    call_owner(g, poll);
    if g.gc_gen.as_ref().unwrap().labels.phase_mark_gate.is_some() {
        return;
    }
    let skip = g.create_text_label();
    let waiting = g.create_text_label();
    let seen = g.create_text_label();
    g.load_rbp_slot(R::V0, 16);
    g.cmp_reg_imm32(R::V0, (iterations / 2) as i32);
    g.jcc_label(C::NotEqual, skip);
    g.bind_text_label(waiting);
    call_owner(g, poll); // a fast peer must service the actual start request
    lea_rt(g, R::V1, RT_DM_WORKER);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, seen);
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
    g.call_label(relax);
    g.jmp_label(waiting);
    g.bind_text_label(seen);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, OLD_WORKER, R::V0);
    g.bind_text_label(skip);
}
pub(super) fn running_old_finish(g: &mut crate::NativeCodeGenerator, iterations: u64) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (poll, relax, end) = (
        l.poll_actor,
        l.worker.as_ref().unwrap().hooks.relax,
        l.old_end_actor,
    );
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, poll);
    lea_rt(g, R::V1, phase_round::ENDS);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    let peer = g.create_text_label();
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, peer);
    call_owner(g, end);
    g.bind_text_label(peer);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
    validate_closed(g, iterations);
}

pub(crate) fn phase_round_metric(g: &mut crate::NativeCodeGenerator, which: usize) {
    let fields = [
        phase_round::KIND,
        phase_round::RESUMING,
        phase_round::STARTS,
        phase_round::ENDS,
        phase_round::CLOSED_PUBLICATIONS,
        phase_round::CLOSED_ACKNOWLEDGED,
        phase_round::CLOSED_WORKER,
        phase_round::END_CHECKS,
        phase_round::END_RETRIES,
        allocation_records::REVISION,
    ];
    lea_rt(g, R::V1, fields[which]);
    g.atomic_load_acquire(R::V0, R::V1);
}
pub(crate) fn phase_owner_adoptions(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    load_rt(g, R::V0, phase_round::ADOPTIONS);
    restore(g);
}

/// Deterministically retain the last real Parked peer across the coordinator's
/// next-round attempt. No peer cache, ACK or debt value is fabricated.
pub(crate) fn delay_phase_resume(g: &mut crate::NativeCodeGenerator, owners: u64) {
    let hook = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.phase_resume_hook = Some(hook);
    g.jmp_label(body);
    g.bind_text_label(hook);
    let done = g.create_text_label();
    let again = g.create_text_label();
    let observed = g.create_text_label();
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, owners as i32 + 1);
    g.jcc_label(C::NotEqual, done);
    load_rt(g, R::V0, phase_round::OWN_KIND);
    g.cmp_reg_imm32(R::V0, 1);
    g.jcc_label(C::NotEqual, done);
    // Observe the actual old cache before this owner adopts ConcurrentMark.
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, 112, R::V0);
    let runtime = g.gc_gen.take().unwrap();
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
    g.call_label(clock);
    g.mov_imm64(R::V1, 120_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(88, R::V0);
    g.bind_text_label(again);
    lea_rt(g, R::V1, phase_round::RESUME_WAITS);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, observed);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 88);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(observed);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, 120, R::V0);
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
    g.bind_text_label(done);
    g.ret();
    g.bind_text_label(body);
}
pub(crate) fn delayed_phase_resume_metric(
    g: &mut crate::NativeCodeGenerator,
    p: &ParallelActors,
    count: bool,
) {
    g.mov_data_addr(R::V1, *p.controls.last().unwrap());
    g.load_ptr_disp32(R::V0, R::V1, if count { 120 } else { 112 });
}

pub(crate) fn old_phase_entry_fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let field = match mode {
        0 => RT_COLLECTING,
        1 => RT_CTX + offset_of!(MutatorContext, private_producers),
        2 => RT_CTX + offset_of!(MutatorContext, access_epoch),
        3 => young_mark::ACTIVE,
        _ => unreachable!(),
    };
    store_rt_imm(g, field, 1);
    let entry = g.gc_gen.as_ref().unwrap().labels.old_start_actor;
    call_owner(g, entry);
}
pub(crate) fn old_phase_start_other(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    let entry = g.gc_gen.as_ref().unwrap().labels.old_start_actor;
    call_owner(g, entry);
    restore(g);
}

/// Keep actual worker root/graph work pending through the first real end check.
/// A gated slice returns incomplete with its budget untouched, never credit.
pub(crate) fn force_first_old_end_retry(g: &mut crate::NativeCodeGenerator) {
    let gate = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.phase_mark_gate = Some(gate);
    g.jmp_label(body);
    g.bind_text_label(gate);
    load_rt(g, R::V0, phase_round::END_CHECKS);
    g.ret();
    g.bind_text_label(body);
}

#[path = "test_support_dual_mark.rs"]
mod dual_mark;
pub(crate) use dual_mark::*;

#[path = "test_support_young_scheduler.rs"]
mod young_scheduling;
pub(crate) use young_scheduling::*;

#[path = "test_support_relocate_pages.rs"]
mod relocate_page_tests;
pub(crate) use relocate_page_tests::*;
