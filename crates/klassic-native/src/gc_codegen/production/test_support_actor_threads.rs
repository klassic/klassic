//! Real Linux threads executing the explicit production allocation entry.
//! Idle checkpoints and native-return Old-mark adoption share this driver.
//! Running-peer and moving phase adoption remain separate integration steps.
use super::*;
use crate::gc_codegen::threads;
use crate::{Condition as XC, Reg as X};
const STACK_BYTES: u64 = 65536;
const TLS: i32 = 8;
const TID: i32 = 24;
const STACK: i32 = 32;
const RESULTS: i32 = 40;
const OBSERVED_TID: i32 = 48;
const HUB_ROOT: i32 = 56;
const GRAPH_READS: i32 = 64;
const DELAYED_ACKS: i32 = 72;
const DELAYED_COMPLETE: i32 = 80;
const OLD_WORKER: i32 = 88;

#[derive(Clone, Copy)]
enum GraphMode {
    Idle,
    Satb(crate::DataLabel, crate::DataLabel, u64),
    WeakRace(crate::DataLabel, bool, bool),
    Moving(crate::DataLabel, bool, u64, bool),
    Checkpoint,
    OldMark,
    CloseOldMark,
    RunningOld,
    RunningDual(crate::DataLabel),
    ScheduledOld,
    ScheduledDual,
    ScheduledMoving,
    CopyRoot(crate::DataLabel, crate::DataLabel, u64),
    NativeCycles(u64, bool, bool),
    NativeMarks(u64, bool, u64),
    NativeChurn(bool),
    NativeLeases(bool, crate::DataLabel),
    NativeTransfers(bool, crate::DataLabel),
}
impl GraphMode {
    fn native_lease(self) -> Option<(bool, crate::DataLabel, bool)> {
        match self {
            Self::NativeLeases(full, requests) => Some((full, requests, false)),
            Self::NativeTransfers(full, requests) => Some((full, requests, true)),
            _ => None,
        }
    }
    fn checkpoint(self) -> bool {
        matches!(self, Self::Checkpoint)
    }
    fn old_mark(self) -> bool {
        matches!(self, Self::OldMark | Self::CloseOldMark)
    }
}
pub(crate) struct ParallelActors {
    controls: Vec<crate::DataLabel>,
    results: crate::DataLabel,
    ready: crate::DataLabel,
    count: u64,
    iterations: u64,
}
/// Guarded stacks and custom TLS are owned until kernel clear-tid join. Only
/// syscalls and emitted GC code execute on these threads; no libc is called.
pub(crate) fn run_parallel_actors(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
) -> ParallelActors {
    run_parallel_impl(g, actors, iterations, None, GraphMode::Idle)
}
fn run_parallel_impl(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    hub_roots: Option<&[crate::DataLabel]>,
    mode: GraphMode,
) -> ParallelActors {
    let moving = matches!(mode, GraphMode::Moving(..) | GraphMode::ScheduledMoving);
    let satb = matches!(mode, GraphMode::Satb(..));
    let copying = matches!(mode, GraphMode::CopyRoot(..));
    let native_cycles = matches!(
        mode,
        GraphMode::NativeCycles(..)
            | GraphMode::NativeMarks(..)
            | GraphMode::NativeChurn(..)
            | GraphMode::NativeLeases(..)
            | GraphMode::NativeTransfers(..)
    );
    let checkpoint = mode.checkpoint();
    let old_mark = mode.old_mark();
    let close_old = matches!(mode, GraphMode::CloseOldMark);
    let running_old = matches!(mode, GraphMode::RunningOld);
    let dual = matches!(mode, GraphMode::RunningDual(_));
    let scheduled_old = matches!(mode, GraphMode::ScheduledOld | GraphMode::ScheduledDual);
    let scheduled_dual = matches!(mode, GraphMode::ScheduledDual);
    let labels = &g.gc_gen.as_ref().unwrap().labels;
    let (alloc, publish, fail, relax, clock) = (
        labels.alloc_actor,
        labels.publish_pending,
        labels.fail,
        labels.worker.as_ref().unwrap().hooks.relax,
        labels.worker.as_ref().unwrap().hooks.clock,
    );
    let start = g.create_text_label();
    let join = g.create_text_label();
    let map = g.create_text_label();
    let unmap = g.create_text_label();
    let entry = g.create_text_label();
    let body = g.create_text_label();
    let child_done = g.create_text_label();
    g.jmp_label(body);
    g.bind_text_label(start);
    threads::emit_linux_worker_start(g);
    g.bind_text_label(join);
    threads::emit_linux_worker_join(g);
    g.bind_text_label(map);
    threads::emit_linux_stack_alloc(g);
    g.bind_text_label(unmap);
    threads::emit_linux_stack_free(g);
    let count = actors.len() as u64 * iterations;
    // Earlier reports append byte strings. Kernel clear-tid/futex storage
    // and native control words require their own alignment afterward.
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let results = g.asm.data_label_with_i64s(&vec![0; count as usize * 3]);
    let ready = g.asm.data_label_with_i64s(&[0]);
    let gate = g.asm.data_label_with_i64s(&[0]);
    let closing_ready = g.asm.data_label_with_i64s(&[0]);
    let closing_gate = g.asm.data_label_with_i64s(&[0]);
    let controls: Vec<_> = actors
        .iter()
        .map(|_| g.asm.data_label_with_i64s(&[0; 17]))
        .collect();
    g.bind_text_label(entry);
    g.asm.push_reg(X::Rbp);
    g.asm.mov_reg_reg(X::Rbp, X::Rsp);
    // Explicit fixture locals include bounded-wait deadlines at slot 1984.
    // Reserve their actual storage before a nested owner entry pushes returns.
    g.asm.sub_reg_imm32(X::Rsp, 2048);
    g.store_rbp_slot(8, R::V7); // SysV RDI = control
    g.asm.load_fs_u64(X::R12, 8);
    load_rt(
        g,
        R::V3,
        owner_state::offset(offset_of!(OwnerState, shared_runtime)),
    );
    g.asm.mov_imm64(X::Rax, 186); // real kernel thread identity
    g.asm.syscall();
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, OBSERVED_TID, R::V0);
    // Every child reaches this barrier before the parent releases allocations.
    g.mov_data_addr(R::V7, ready);
    let arrival = g.create_text_label();
    g.bind_text_label(arrival);
    g.atomic_load_acquire(R::V0, R::V7);
    g.mov_reg_reg(R::V8, R::V0);
    g.mov_reg_reg(R::V6, R::V0);
    g.add_reg_imm32(R::V6, 1);
    g.atomic_compare_exchange(R::V7, R::V6);
    g.cmp_reg_reg(R::V0, R::V8);
    g.jcc_label(C::NotEqual, arrival);
    let waiting = g.create_text_label();
    g.bind_text_label(waiting);
    g.mov_data_addr(R::V7, gate);
    g.atomic_load_acquire(R::V0, R::V7);
    g.test_reg_reg(R::V0, R::V0);
    let released = g.create_text_label();
    g.jcc_label(C::NotEqual, released);
    g.call_label(relax);
    g.jmp_label(waiting);
    g.bind_text_label(released);
    if let GraphMode::WeakRace(state, young, gate_first) = mode {
        weak_race::child(g, state, young, gate_first, closing_ready, closing_gate);
        g.jmp_label(child_done);
    }
    if let GraphMode::Satb(state, marker_gate, seal) = mode {
        satb_parallel::child(g, actors.len() as u64, state, marker_gate, seal);
        g.jmp_label(child_done);
    }
    if let GraphMode::Moving(state, true, cycles, _) = mode {
        moving::native_start(g, actors.len() as u64, state, closing_ready, cycles);
    } else if old_mark || scheduled_old || dual || copying || native_cycles || moving {
        let exit = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
        graph::call_owner(g, exit);
    }
    if let Some((_, _, transfer)) = mode.native_lease() {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        let release = if transfer {
            l.metadata_lease_retarget_actor
        } else {
            l.metadata_lease_release_actor
        };
        g.load_rbp_slot(R::V1, 8);
        g.load_ptr_disp32(R::V0, R::V1, HUB_ROOT);
        g.load_ptr_disp32(R::V7, R::V1, 128);
        graph::call_owner(g, release);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
    }
    if scheduled_old
        || dual
        || matches!(
            mode,
            GraphMode::Moving(_, false, ..) | GraphMode::ScheduledMoving
        )
    {
        scheduling::scheduled_arrival(g, actors.len() as u64, closing_ready);
    }
    if checkpoint || (running_old && actors.len() > 1) {
        graph::delay_last_peer(g, actors.len() as u64);
    }
    g.mov_imm64(R::V0, 0);
    g.store_rbp_slot(16, R::V0);
    if moving {
        g.store_rbp_slot(96, R::V0);
    }
    let again = g.create_text_label();
    g.bind_text_label(again);
    if let GraphMode::CopyRoot(root, _, _) = mode {
        copying::iteration(g, root);
    } else if native_cycles {
        completed::iteration(
            g,
            matches!(
                mode,
                GraphMode::NativeCycles(_, _, true)
                    | GraphMode::NativeMarks(..)
                    | GraphMode::NativeChurn(..)
                    | GraphMode::NativeLeases(..)
                    | GraphMode::NativeTransfers(..)
            ),
        );
    } else if hub_roots.is_some() {
        graph::iteration(g, actors.len() as u64, iterations);
    } else {
        g.mov_imm64(R::V7, 16);
        g.mov_imm64(R::V6, 1);
        g.call_label(alloc); // full production entry, not alloc_raw or a substitute
        load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
        g.shl_reg_imm8(R::V1, 32);
        g.load_rbp_slot(R::V2, 16);
        g.or_reg_reg(R::V1, R::V2);
        g.store_ptr_disp32(R::V0, 0, R::V1);
        g.load_rbp_slot(R::V1, 8);
        g.load_ptr_disp32(R::V1, R::V1, RESULTS);
        g.shl_reg_imm8(R::V2, 3);
        g.mov_reg_reg(R::V6, R::V2);
        g.shl_reg_imm8(R::V6, 1);
        g.add_reg_reg(R::V2, R::V6);
        g.add_reg_reg(R::V1, R::V2);
        g.store_ptr_disp32(R::V1, 0, R::V0);
        g.load_ptr_disp32(
            R::V2,
            R::V0,
            offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
        );
        g.store_ptr_disp32(R::V1, 16, R::V2);
        g.load_ptr_disp32(R::V2, R::V2, offset_of!(MarkRecord, object_id) as i32);
        g.store_ptr_disp32(R::V1, 8, R::V2);
    }
    if let GraphMode::Moving(state, delayed, cycles, full) = mode {
        moving::iteration(g, iterations, state, delayed, cycles, full);
    }
    if checkpoint {
        graph::checkpoint_iteration(g, iterations);
    }
    if old_mark {
        graph::old_worker_iteration(g, iterations);
    }
    if running_old {
        graph::running_old_iteration(g, iterations);
    }
    if let GraphMode::RunningDual(gate) = mode {
        graph::dual_mark_iteration(g, iterations, gate);
    }
    if matches!(mode, GraphMode::NativeChurn(..)) {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        let (enter, exit) = (l.native_enter_actor, l.native_exit_actor);
        graph::call_owner(g, enter);
        graph::call_owner(g, exit);
    }
    g.load_rbp_slot(R::V0, 16);
    g.add_reg_imm32(R::V0, 1);
    g.store_rbp_slot(16, R::V0);
    g.call_label(relax); // exercise real scheduling between independent owners
    g.cmp_reg_imm32(R::V0, iterations as i32);
    g.jcc_label(C::Below, again);
    g.call_label(publish);
    if checkpoint {
        graph::checkpoint_finish(g);
    }
    if running_old {
        graph::running_old_finish(g, iterations);
    }
    if dual {
        graph::dual_mark_finish(g, actors.len() as u64, closing_gate);
    }
    if scheduled_old {
        scheduling::scheduled_finish(
            g,
            actors.len() as u64,
            iterations,
            closing_gate,
            scheduled_dual,
        );
    }
    if close_old {
        closing::child(
            g,
            actors.len() as u64,
            iterations,
            closing_ready,
            closing_gate,
        );
    }
    if matches!(mode, GraphMode::ScheduledMoving) {
        moving_scheduling::finish(g);
        graph::validate_closed(g, iterations);
    } else if let GraphMode::Moving(state, ..) = mode {
        moving::finish(g);
        moving::finish_preserved_old(g, state);
    }
    if copying || native_cycles || moving {
        let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
        graph::call_owner(g, enter);
    }
    g.bind_text_label(child_done);
    g.asm.mov_reg_reg(X::Rsp, X::Rbp);
    g.asm.pop_reg(X::Rbp);
    g.ret();
    g.bind_text_label(body);
    if checkpoint || running_old || scheduled_old || dual || copying || moving || satb {
        let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
        graph::call_owner(g, enter);
    }
    for (i, (actor, control)) in actors.iter().zip(&controls).enumerate() {
        g.asm.mov_imm64(X::Rdi, STACK_BYTES);
        g.call_label(map);
        g.asm.cmp_reg_imm32(X::Rax, -4095);
        g.asm.jcc_label(XC::AboveOrEqual, fail);
        g.mov_data_addr(R::V9, *control);
        g.store_ptr_disp32(R::V9, STACK, R::V0);
        g.mov_data_addr(R::V1, actor.pointer);
        g.load_ptr_disp32(R::V1, R::V1, 0);
        g.store_ptr_disp32(R::V9, 0, R::V1);
        g.store_ptr_disp32(R::V9, TLS + 8, R::V1);
        g.mov_reg_reg(R::V1, R::V9);
        g.add_reg_imm32(R::V1, TLS);
        g.store_ptr_disp32(R::V9, TLS, R::V1);
        g.mov_data_addr(R::V1, results);
        g.add_reg_imm32(R::V1, (i as u64 * iterations * 24) as i32);
        g.store_ptr_disp32(R::V9, RESULTS, R::V1);
        if let Some((_, requests, _)) = mode.native_lease() {
            g.mov_data_addr(R::V1, requests);
            g.load_ptr_disp32(R::V1, R::V1, (i * 8) as i32);
            g.store_ptr_disp32(R::V9, 128, R::V1);
        }
        if let Some(roots) = hub_roots {
            g.mov_data_addr(R::V1, roots[i]);
            g.load_ptr_disp32(R::V1, R::V1, 0);
            g.store_ptr_disp32(R::V9, HUB_ROOT, R::V1);
        }
        g.asm.mov_reg_reg(X::Rdi, X::Rax);
        g.asm.add_reg_imm32(X::Rdi, STACK_BYTES as i32 + 4096);
        g.asm.mov_reg_reg(X::Rsi, X::R9);
        g.asm.add_reg_imm32(X::Rsi, TLS);
        g.asm.lea_text_addr(X::Rdx, entry);
        g.asm.mov_reg_reg(X::Rcx, X::R9);
        g.asm.mov_reg_reg(X::R8, X::R9);
        g.asm.add_reg_imm32(X::R8, TID);
        g.call_label(start);
        g.cmp_reg_imm32(R::V0, 0);
        g.jcc_label(C::LessEqual, fail);
    }
    let all_ready = g.create_text_label();
    let wait = g.create_text_label();
    // Scheduler yields are not units of elapsed time. A fixed spin count can
    // expire before a launched OS peer runs on a loaded host. Keep the real
    // startup barrier, bounded by the actual monotonic clock instead.
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1536, R::V0);
    g.bind_text_label(wait);
    g.mov_data_addr(R::V7, ready);
    g.atomic_load_acquire(R::V0, R::V7);
    g.cmp_reg_imm32(R::V0, actors.len() as i32);
    g.jcc_label(C::Equal, all_ready);
    g.call_label(relax);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1536);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.jmp_label(wait);
    g.bind_text_label(all_ready);
    if old_mark {
        load_rt(g, R::V0, RT_DM_WORKER);
        g.store_rbp_slot(1528, R::V0);
    }
    if let GraphMode::NativeCycles(cycles, full, _) = mode {
        completed::collect(g, cycles, full);
        let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
        graph::call_owner(g, enter);
    }
    if let GraphMode::NativeMarks(cycles, full, marks) = mode {
        next_mark::collect_and_mark(g, cycles, full, marks);
        let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
        graph::call_owner(g, enter);
    }
    if let GraphMode::NativeChurn(full) = mode {
        if full {
            store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
        }
        let start = g.gc_gen.as_ref().unwrap().labels.moving_start_actor;
        graph::call_owner(g, start);
        let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
        graph::call_owner(g, enter);
    }
    if let Some((full, _, _)) = mode.native_lease() {
        crate::gc_codegen::production::test_support::lease_open_and_wait(g, full);
        let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
        graph::call_owner(g, enter);
    }
    if let GraphMode::WeakRace(state, young, gate_first) = mode {
        weak_race::coordinator(
            g,
            actors.len() as u64,
            state,
            young,
            gate_first,
            gate,
            closing_gate,
        );
    }
    g.mov_data_addr(R::V7, gate);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V7, R::V0);
    if old_mark {
        let runtime = g.gc_gen.take().unwrap();
        worker::unlock(g, &runtime.labels);
        g.gc_gen = Some(runtime);
        let enter = g.gc_gen.as_ref().unwrap().labels.native_enter_actor;
        graph::call_owner(g, enter);
    }
    if close_old {
        closing::coordinator(g, actors.len() as u64, closing_ready, closing_gate);
    }
    if let GraphMode::CopyRoot(_, state, fault_mode) = mode
        && fault_mode != 0
    {
        copying::parent_fault(g, state, fault_mode);
    }
    for control in &controls {
        g.mov_data_addr(R::V7, *control);
        g.add_reg_imm32(R::V7, TID);
        g.call_label(join);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
        g.mov_data_addr(R::V7, *control);
        g.load_ptr_disp32(R::V7, R::V7, STACK);
        g.mov_imm64(R::V6, STACK_BYTES + 8192);
        g.call_label(unmap);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
    }
    if checkpoint
        || old_mark
        || running_old
        || scheduled_old
        || dual
        || copying
        || native_cycles
        || moving
        || satb
        || matches!(mode, GraphMode::WeakRace(..))
    {
        let exit = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
        graph::call_owner(g, exit);
    }
    if matches!(mode, GraphMode::NativeChurn(..)) || mode.native_lease().is_some() {
        next_mark::finish_move(g);
    }
    ParallelActors {
        controls,
        results,
        ready,
        count,
        iterations,
    }
}

/// Check every real allocation after all threads join. Bitmaps reject duplicate
/// IDs/records; payload and header checks reject cross-owner request corruption.
pub(crate) fn verify_parallel_actors(g: &mut crate::NativeCodeGenerator, p: &ParallelActors) {
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let ids = g.asm.data_label_with_i64s(&vec![0; p.count as usize]);
    let records = g.asm.data_label_with_i64s(&vec![0; p.count as usize]);
    g.mov_imm64(R::V0, 0);
    g.store_rbp_slot(1536, R::V0);
    let again = g.create_text_label();
    g.bind_text_label(again);
    g.load_rbp_slot(R::V0, 1536);
    g.mov_reg_reg(R::V1, R::V0);
    g.shl_reg_imm8(R::V1, 3);
    g.mov_reg_reg(R::V6, R::V1);
    g.shl_reg_imm8(R::V6, 1);
    g.add_reg_reg(R::V1, R::V6);
    g.mov_data_addr(R::V7, p.results);
    g.add_reg_reg(R::V7, R::V1);
    g.load_ptr_disp32(R::V8, R::V7, 0);
    g.load_ptr_disp32(R::V9, R::V7, 8);
    g.load_ptr_disp32(R::V10, R::V7, 16);
    g.load_ptr_disp32(
        R::V1,
        R::V8,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    g.cmp_reg_reg(R::V1, R::V10);
    g.jcc_label(C::NotEqual, fail);
    g.load_ptr_disp32(R::V1, R::V10, offset_of!(MarkRecord, object_id) as i32);
    g.cmp_reg_reg(R::V1, R::V9);
    g.jcc_label(C::NotEqual, fail);
    g.load_ptr_disp32(
        R::V1,
        R::V8,
        offset_of!(ObjectHeader, state) as i32 - size_of::<ObjectHeader>() as i32,
    );
    g.cmp_reg_imm32(R::V1, crate::gc_abi::OBJECT_PUBLISHED as i32);
    g.jcc_label(C::NotEqual, fail);
    g.mov_imm64(R::V6, p.iterations);
    g.div_unsigned(R::V6);
    g.add_reg_imm32(R::V0, 2);
    g.shl_reg_imm8(R::V0, 32);
    g.or_reg_reg(R::V0, R::V2);
    g.load_ptr_disp32(R::V1, R::V8, 0);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    g.test_reg_reg(R::V9, R::V9);
    g.jcc_label(C::Equal, fail);
    g.cmp_reg_imm32(R::V9, p.count as i32);
    g.jcc_label(C::Above, fail);
    g.mov_reg_reg(R::V0, R::V9);
    g.sub_reg_imm8(R::V0, 1);
    g.shl_reg_imm8(R::V0, 3);
    g.mov_data_addr(R::V7, ids);
    g.add_reg_reg(R::V7, R::V0);
    g.atomic_load_acquire(R::V0, R::V7);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V7, R::V0);
    g.mov_reg_reg(R::V0, R::V10);
    load_rt(g, R::V1, RT_MARKS + offset_of!(MarkArena, records));
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::Below, fail);
    g.sub_reg_reg(R::V0, R::V1);
    g.mov_imm64(R::V6, size_of::<MarkRecord>() as u64);
    g.div_unsigned(R::V6);
    g.test_reg_reg(R::V2, R::V2);
    g.jcc_label(C::NotEqual, fail);
    g.cmp_reg_imm32(R::V0, p.count as i32);
    g.jcc_label(C::AboveOrEqual, fail);
    g.shl_reg_imm8(R::V0, 3);
    g.mov_data_addr(R::V7, records);
    g.add_reg_reg(R::V7, R::V0);
    g.atomic_load_acquire(R::V0, R::V7);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, fail);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V7, R::V0);
    g.load_rbp_slot(R::V0, 1536);
    g.add_reg_imm32(R::V0, 1);
    g.store_rbp_slot(1536, R::V0);
    g.cmp_reg_imm32(R::V0, p.count as i32);
    g.jcc_label(C::Below, again);
}
pub(crate) fn parallel_actor_tid(g: &mut crate::NativeCodeGenerator, p: &ParallelActors, i: usize) {
    g.mov_data_addr(R::V0, p.controls[i]);
    g.load_ptr_disp32(R::V0, R::V0, OBSERVED_TID);
}
pub(crate) fn parallel_actor_ready(g: &mut crate::NativeCodeGenerator, p: &ParallelActors) {
    g.mov_data_addr(R::V0, p.ready);
    g.atomic_load_acquire(R::V0, R::V0);
}
pub(crate) fn shared_allocation_value(g: &mut crate::NativeCodeGenerator, which: u64) {
    let field = match which {
        0 => RT_MARKS + offset_of!(MarkArena, used),
        1 => RT_NEXT_OBJECT_ID,
        2 => RT_PUBLISHED,
        3 => old_policy::ALLOCATED,
        4 => RT_NEXT_PAGE,
        _ => RT_SPACE_BASE,
    };
    load_rt(g, R::V0, field);
}

/// Replay an older, previously valid claim after the actual joined allocations.
/// This isolates the mirror's ordering contract without relying on a lucky race.
pub(crate) fn replay_older_page_bound(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, RT_NEXT_PAGE);
    g.sub_reg_imm8(R::V0, 1);
    alloc::publish_next_page(g);
    load_rt(g, R::V0, RT_NEXT_PAGE);
}

#[path = "test_support_actor_graph.rs"]
mod graph;
pub(crate) use graph::*;

#[path = "test_support_mark_close.rs"]
mod closing;
pub(crate) use closing::*;

#[path = "test_support_cooperative_old.rs"]
mod scheduling;
pub(crate) use scheduling::*;

pub(crate) fn run_parallel_copy_actors(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    root: crate::DataLabel,
    state: crate::DataLabel,
    mode: u64,
) -> ParallelActors {
    run_parallel_impl(g, actors, 1, None, GraphMode::CopyRoot(root, state, mode))
}
pub(crate) fn parallel_copy_result(
    g: &mut crate::NativeCodeGenerator,
    p: &ParallelActors,
    i: usize,
    field: usize,
) {
    g.mov_data_addr(R::V1, p.results);
    g.load_ptr_disp32(
        R::V0,
        R::V1,
        (i * 24 + if field == 3 { 0 } else { field * 8 }) as i32,
    );
    if field == 3 {
        g.load_ptr_disp32(R::V0, R::V0, 0);
    }
}

#[path = "test_support_native_completed.rs"]
mod completed;
pub(crate) use completed::*;

#[path = "test_support_moving_round.rs"]
mod moving;
pub(crate) use moving::*;

#[path = "test_support_moving_scheduler.rs"]
mod moving_scheduling;
pub(crate) use moving_scheduling::*;

#[path = "test_support_native_next_mark.rs"]
mod next_mark;
pub(crate) use next_mark::*;

pub(crate) fn run_parallel_native_churn(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    roots: &[crate::DataLabel],
    iterations: u64,
    full: bool,
) -> ParallelActors {
    let enable = g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
    graph::call_owner(g, enable);
    run_parallel_impl(
        g,
        actors,
        iterations,
        Some(roots),
        GraphMode::NativeChurn(full),
    )
}

pub(crate) fn native_tail_observe(g: &mut crate::NativeCodeGenerator, field: usize) {
    match field {
        0 => load_rt(g, R::V0, capacity::FRESH_BUDGET),
        1 => {
            load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, tlab_end));
            load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, tlab_top));
            g.sub_reg_reg(R::V0, R::V1);
        }
        2 => load_rt(g, R::V0, RT_PHASE_OPEN),
        3 => load_rt(g, R::V0, moving_round::NATIVE_TAIL_BYTES),
        4 => load_rt(g, R::V0, moving_round::NATIVE_TAIL_WAITS),
        _ => unreachable!(),
    }
}

pub(crate) fn native_tail_start(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (enable, start) = (l.moving_enable_actor, l.moving_start_actor);
    graph::call_owner(g, enable);
    graph::call_owner(g, start);
}

pub(crate) fn run_parallel_metadata_leases(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    roots: &[crate::DataLabel],
    requests: &[crate::DataLabel],
    full: bool,
) -> ParallelActors {
    parallel_leases(g, actors, roots, requests, full, false)
}
pub(crate) fn run_parallel_metadata_transfers(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    roots: &[crate::DataLabel],
    requests: &[crate::DataLabel],
    full: bool,
) -> ParallelActors {
    parallel_leases(g, actors, roots, requests, full, true)
}
fn parallel_leases(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    roots: &[crate::DataLabel],
    requests: &[crate::DataLabel],
    full: bool,
    transfer: bool,
) -> ParallelActors {
    let enable = g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
    graph::call_owner(g, enable);
    let list = g.asm.data_label_with_i64s(&vec![0; requests.len()]);
    for (i, request) in requests.iter().enumerate() {
        g.mov_data_addr(R::V0, *request);
        g.mov_data_addr(R::V1, list);
        g.store_ptr_disp32(R::V1, (i * 8) as i32, R::V0);
    }
    run_parallel_impl(
        g,
        actors,
        1,
        Some(roots),
        if transfer {
            GraphMode::NativeTransfers(full, list)
        } else {
            GraphMode::NativeLeases(full, list)
        },
    )
}

#[path = "test_support_weak_race.rs"]
mod weak_race;
pub(crate) use weak_race::{prepare_weak_race, run_parallel_weak_race, weak_race_metric};

#[path = "test_support_satb_parallel.rs"]
mod satb_parallel;
pub(crate) use satb_parallel::{
    run_parallel_satb_deletions, satb_parallel_metric, satb_parallel_owner_metric,
};
