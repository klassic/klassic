//! Actual all-Running graph mutators use the public moving rendezvous. Root
//! cells survive collections; historical raw allocation observations are not
//! dereferenced after a move.
use super::*;
pub(crate) fn run_parallel_moving_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
    scenario: (bool, bool, u64, bool, bool),
) -> ParallelActors {
    let (retain_journal, delayed_native, cycles, full, late_allocation) = scenario;
    let state = g
        .asm
        .data_label_with_i64s(&[0, 0, 0, i64::from(late_allocation), 0]);
    if retain_journal {
        selected_journal(g, actors.len() as u64, state);
    }
    graph::run_graph(
        g,
        actors,
        iterations,
        master,
        GraphMode::Moving(state, delayed_native, cycles, full),
    )
}
pub(super) fn iteration(
    g: &mut crate::NativeCodeGenerator,
    iterations: u64,
    state: crate::DataLabel,
    delayed_native: bool,
    cycles: u64,
    full: bool,
) {
    let (start, poll, relax) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (
            l.moving_start_actor,
            l.poll_actor,
            l.worker.as_ref().unwrap().hooks.relax,
        )
    };
    for cycle in 0..cycles {
        let done = g.create_text_label();
        let wait = g.create_text_label();
        g.load_rbp_slot(R::V0, 16);
        g.cmp_reg_imm32(R::V0, (iterations * cycle / cycles) as i32);
        g.jcc_label(C::NotEqual, done);
        load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
        g.cmp_reg_imm32(R::V0, 2);
        g.jcc_label(C::NotEqual, wait);
        if full {
            store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
        }
        graph::call_owner(g, start);
        if delayed_native && cycle + 1 == cycles {
            let waiting = g.create_text_label();
            g.bind_text_label(waiting);
            g.mov_data_addr(R::V1, state);
            g.add_reg_imm32(R::V1, 16);
            g.atomic_load_acquire(R::V0, R::V1);
            g.test_reg_reg(R::V0, R::V0);
            g.jcc_label(C::NotEqual, done);
            g.call_label(relax);
            g.jmp_label(waiting);
        }
        g.jmp_label(done);
        g.bind_text_label(wait);
        publish_selected(g, state);
        graph::call_owner(g, poll);
        lea_rt(g, R::V1, moving_round::STARTS);
        g.atomic_load_acquire(R::V0, R::V1);
        g.cmp_reg_imm32(R::V0, cycle as i32 + 1);
        g.jcc_label(C::AboveOrEqual, done);
        g.call_label(relax);
        g.jmp_label(wait);
        g.bind_text_label(done);
    }
    check_selected_allocation(g, state);
    check_preserved_old(g, state);
    graph::call_owner(g, poll);
}
pub(super) fn finish(g: &mut crate::NativeCodeGenerator) {
    let (end, poll, relax) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (
            l.moving_end_actor,
            l.poll_actor,
            l.worker.as_ref().unwrap().hooks.relax,
        )
    };
    let ordinary = g.create_text_label();
    let done = g.create_text_label();
    let again = g.create_text_label();
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, ordinary);
    g.bind_text_label(again);
    load_rt(g, R::V0, RT_REQUEST + offset_of!(AllocationRequest, page));
    g.store_rbp_slot(120, R::V0);
    load_rt(g, R::V0, moving_round::RETRIES);
    g.store_rbp_slot(128, R::V0);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, tlab_top));
    g.store_rbp_slot(136, R::V0);
    graph::call_owner(g, end);
    check_retried_tlab(g);
    graph::call_owner(g, poll);
    load_rt(g, R::V0, RT_PHASE_OPEN);
    load_rt(g, R::V1, RT_RETIRE_STAGE);
    g.or_reg_reg(R::V0, R::V1);
    load_rt(g, R::V1, phase_round::RESUMING);
    g.or_reg_reg(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(ordinary);
    // Native entry itself services a pending real request before exclusion.
    g.bind_text_label(done);
}
pub(crate) fn moving_metric(g: &mut crate::NativeCodeGenerator, which: usize) {
    let field = [
        moving_round::STARTS,
        moving_round::CLOSES,
        moving_round::CLEANUPS,
        moving_round::RETRIES,
        moving_round::CURRENT_BANK,
        moving_round::CLOSED_BANK,
        RT_PHASE_OPEN,
        RT_RETIRE_STAGE,
        RT_WORKER_COPIES,
        root_work::PENDING,
        young_mark::EPOCH,
        moving_round::START_EXPECTED,
        moving_round::START_ACKNOWLEDGED,
        moving_round::CAPTURED_DEBT,
        moving_round::OPENED_DEBT,
        native_io::ADOPTIONS,
        RT_FULL_COLLECTIONS,
        moving_round::ABORTED_STARTS,
        RT_MARK_CYCLES,
        RT_MARK_ACTIVE,
        RT_MARK_VALID,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    ][which];
    load_rt(g, R::V0, field);
}

// A scheduling gate holds only journal consumption. Actual owner RootStores
// and their release publications remain the production path.
fn publish_selected(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel) {
    let done = g.create_text_label();
    g.mov_data_addr(R::V1, state);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, 1);
    g.jcc_label(C::NotEqual, done);
    // Slot 96 belongs exclusively to this generated mutator stack.
    g.load_rbp_slot(R::V0, 96);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    allocate_selected(g, state);
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (load, store) = (l.root_load_actor, l.root_store_actor);
    g.load_rbp_slot(R::V0, 8);
    g.load_ptr_disp32(R::V0, R::V0, HUB_ROOT);
    graph::call_owner(g, load);
    g.load_rbp_slot(R::V1, 8);
    g.add_reg_imm32(R::V1, HUB_ROOT);
    graph::call_owner(g, store);
    g.mov_imm64(R::V0, 1);
    g.store_rbp_slot(96, R::V0);
    let again = g.create_text_label();
    g.mov_data_addr(R::V7, state);
    g.add_reg_imm32(R::V7, 8);
    g.bind_text_label(again);
    g.atomic_load_acquire(R::V0, R::V7);
    g.mov_reg_reg(R::V8, R::V0);
    g.mov_reg_reg(R::V6, R::V0);
    g.add_reg_imm32(R::V6, 1);
    g.atomic_compare_exchange(R::V7, R::V6);
    g.cmp_reg_reg(R::V0, R::V8);
    g.jcc_label(C::NotEqual, again);
    g.bind_text_label(done);
}
fn selected_journal(g: &mut crate::NativeCodeGenerator, owners: u64, state: crate::DataLabel) {
    let hook = g.create_text_label();
    let gate = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.moving_selected_hook = Some(hook);
    g.gc_gen.as_mut().unwrap().labels.journal_worker_gate = Some(gate);
    g.jmp_label(body);
    g.bind_text_label(gate);
    let open_gate = g.create_text_label();
    g.mov_data_addr(R::V1, state);
    g.atomic_load_acquire(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, open_gate);
    load_rt(g, R::V0, moving_round::RETRIES);
    g.ret();
    g.bind_text_label(open_gate);
    g.mov_imm64(R::V0, 1);
    g.ret();
    g.bind_text_label(hook);
    load_rt(g, R::V0, capacity::SEALED);
    g.mov_data_addr(R::V1, state);
    g.store_ptr_disp32(R::V1, 32, R::V0);
    g.mov_data_addr(R::V1, state);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    let runtime = g.gc_gen.take().unwrap();
    worker::unlock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
    publish_selected(g, state);
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (clock, relax, fail) = (
        l.worker.as_ref().unwrap().hooks.clock,
        l.worker.as_ref().unwrap().hooks.relax,
        l.fail,
    );
    g.call_label(clock);
    g.mov_imm64(R::V1, 10_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1984, R::V0);
    let again = g.create_text_label();
    let ready = g.create_text_label();
    g.bind_text_label(again);
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, 8);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, owners as i32);
    g.jcc_label(C::Equal, ready);
    g.call_label(clock);
    g.load_rbp_slot(R::V1, 1984);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, fail);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(ready);
    let runtime = g.gc_gen.take().unwrap();
    worker::lock(g, &runtime.labels);
    g.gc_gen = Some(runtime);
    g.mov_data_addr(R::V1, state);
    g.load_ptr_disp32(R::V0, R::V1, 32);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, 96, R::V0);
    load_rt(g, R::V0, capacity::SEALED);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, 104, R::V0);
    g.ret();
    g.bind_text_label(body);
}

/// Last thread stays genuinely Native until the other owners finish the real
/// moving-start quorum. Its return must adopt the retained source window.
pub(super) fn native_start(
    g: &mut crate::NativeCodeGenerator,
    owners: u64,
    state: crate::DataLabel,
    ready: crate::DataLabel,
    cycles: u64,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (exit, relax) = (l.native_exit_actor, l.worker.as_ref().unwrap().hooks.relax);
    let late = g.create_text_label();
    let wait = g.create_text_label();
    let done = g.create_text_label();
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, owners as i32 + 1);
    g.jcc_label(C::Equal, late);
    graph::call_owner(g, exit);
    scheduling::scheduled_arrival(g, owners - 1, ready);
    g.jmp_label(done);
    g.bind_text_label(late);
    g.bind_text_label(wait);
    lea_rt(g, R::V1, moving_round::STARTS);
    g.atomic_load_acquire(R::V0, R::V1);
    g.cmp_reg_imm32(R::V0, cycles as i32);
    let released = g.create_text_label();
    g.jcc_label(C::AboveOrEqual, released);
    g.call_label(relax);
    g.jmp_label(wait);
    g.bind_text_label(released);
    graph::call_owner(g, exit);
    load_rt(g, R::V0, native_io::ADOPTIONS);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, OLD_WORKER, R::V0);
    g.mov_data_addr(R::V1, state);
    g.add_reg_imm32(R::V1, 16);
    g.mov_imm64(R::V0, 1);
    g.atomic_store_release(R::V1, R::V0);
    g.bind_text_label(done);
}

/// Probe only after actual physical moves and source cleanup. Incorrect
/// acceptance exits immediately so another invariant cannot mask the fault.
pub(crate) fn moving_journal_fault(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    if mode == 0 {
        load_rt(g, R::V0, moving_round::CURRENT_BANK);
        g.sub_reg_imm8(R::V0, 1);
        store_rt(g, moving_round::CURRENT_BANK, R::V0);
    } else if mode == 1 {
        store_rt_imm(g, moving_round::CLOSED_BANK, 0);
    }
    load_rt(
        g,
        R::V1,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    g.mov_reg_reg(R::V0, R::V1);
    match mode {
        2 => g.add_reg_imm32(R::V0, 0x10000),
        3 => g.sub_reg_imm32(R::V0, 0x20000),
        4 => {
            g.mov_imm64(R::V2, 0x400);
            g.xor_reg_reg(R::V0, R::V2);
        }
        5 => {
            g.mov_imm64(R::V2, 1);
            g.or_reg_reg(R::V0, R::V2);
        }
        6 => {
            g.mov_imm64(R::V2, 0x2000);
            g.or_reg_reg(R::V0, R::V2);
        }
        _ => (),
    }
    moving_round::journal::validate(g, &rt.labels);
    g.gc_gen = Some(rt);
    g.asm.mov_imm64(X::Rax, 231);
    g.asm.mov_imm64(X::Rdi, 0);
    g.asm.syscall();
}

fn allocate_selected(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel) {
    let done = g.create_text_label();
    g.mov_data_addr(R::V1, state);
    g.load_ptr_disp32(R::V0, R::V1, 24);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (alloc, adopt, fail) = (l.alloc_actor, l.root_adopt_actor, l.fail);
    load_rt(g, R::V0, young_mark::VALID);
    g.cmp_reg_imm32(R::V0, 1);
    g.jcc_label(C::NotEqual, fail);
    for field in [young_mark::ACTIVE, RT_PHASE_OPEN, RT_RETIRE_STAGE] {
        load_rt(g, R::V0, field);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::NotEqual, fail);
    }
    g.mov_imm64(R::V7, 16);
    g.mov_imm64(R::V6, 1);
    graph::call_owner(g, alloc);
    load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.shl_reg_imm8(R::V1, 32);
    g.add_reg_imm32(R::V1, 85);
    g.store_ptr_disp32(R::V0, 0, R::V1);
    g.load_ptr_disp32(R::V2, R::V0, copy::MARK_RECORD);
    g.load_ptr_disp32(R::V1, R::V2, offset_of!(MarkRecord, young_epoch) as i32);
    load_rt(g, R::V2, young_mark::EPOCH);
    g.cmp_reg_reg(R::V1, R::V2);
    g.jcc_label(C::NotEqual, fail);
    g.load_ptr_disp32(R::V2, R::V0, copy::MARK_RECORD);
    g.load_ptr_disp32(R::V2, R::V2, offset_of!(MarkRecord, object_id) as i32);
    g.store_rbp_slot(112, R::V2);
    g.store_rbp_slot(104, R::V0);
    g.mov_reg_reg(R::V0, R::V5);
    g.add_reg_imm32(R::V0, -104);
    graph::call_owner(g, adopt);
    g.bind_text_label(done);
}
fn check_selected_allocation(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel) {
    let done = g.create_text_label();
    g.mov_data_addr(R::V1, state);
    g.load_ptr_disp32(R::V0, R::V1, 24);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.load_rbp_slot(R::V0, 96);
    g.cmp_reg_imm32(R::V0, 1);
    g.jcc_label(C::NotEqual, done);
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (load, pop, fail) = (l.root_load_actor, l.root_pop_actor, l.fail);
    g.load_rbp_slot(R::V0, 104);
    graph::call_owner(g, load);
    g.load_ptr_disp32(R::V1, R::V0, copy::MARK_RECORD);
    g.load_ptr_disp32(R::V1, R::V1, offset_of!(MarkRecord, object_id) as i32);
    g.load_rbp_slot(R::V2, 112);
    g.cmp_reg_reg(R::V1, R::V2);
    g.jcc_label(C::NotEqual, fail);
    g.load_rbp_slot(R::V2, 8);
    g.store_ptr_disp32(R::V2, 120, R::V1);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    g.store_ptr_disp32(R::V2, 112, R::V0);
    load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.shl_reg_imm8(R::V1, 32);
    g.add_reg_imm32(R::V1, 85);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    graph::call_owner(g, pop);
    g.mov_imm64(R::V0, 2);
    g.store_rbp_slot(96, R::V0);
    g.bind_text_label(done);
}
pub(crate) fn parallel_moving_selected(
    g: &mut crate::NativeCodeGenerator,
    p: &ParallelActors,
    owner: usize,
    field: i32,
) {
    g.mov_data_addr(R::V1, p.controls[owner]);
    g.load_ptr_disp32(R::V0, R::V1, field);
}

fn check_retried_tlab(g: &mut crate::NativeCodeGenerator) {
    let done = g.create_text_label();
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    load_rt(g, R::V0, RT_PHASE_OPEN);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.load_rbp_slot(R::V0, 120);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    load_rt(g, R::V1, RT_REQUEST + offset_of!(AllocationRequest, page));
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    g.load_rbp_slot(R::V0, 136);
    load_rt(g, R::V1, RT_CTX + offset_of!(MutatorContext, tlab_top));
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    load_rt(g, R::V0, moving_round::RETRIES);
    g.load_rbp_slot(R::V1, 128);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::Equal, done);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V0, R::V1, 72);
    g.add_reg_imm32(R::V0, 1);
    g.store_ptr_disp32(R::V1, 72, R::V0);
    g.bind_text_label(done);
}

pub(crate) fn run_parallel_preserved_old_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
    cycles: u64,
) -> ParallelActors {
    let state = g.asm.data_label_with_i64s(&[0, 0, 0, 2, 0]);
    let gate = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.phase_mark_gate = Some(gate);
    g.jmp_label(body);
    g.bind_text_label(gate);
    load_rt(g, R::V0, moving_round::STARTS);
    g.cmp_reg_imm32(R::V0, cycles as i32);
    let closed = g.create_text_label();
    g.jcc_label(C::Below, closed);
    g.mov_imm64(R::V0, 1);
    g.ret();
    g.bind_text_label(closed);
    g.mov_imm64(R::V0, 0);
    g.ret();
    g.bind_text_label(body);
    graph::run_graph(
        g,
        actors,
        iterations,
        master,
        GraphMode::Moving(state, false, cycles, false),
    )
}
fn preserved_branch(
    g: &mut crate::NativeCodeGenerator,
    state: crate::DataLabel,
    done: crate::TextLabel,
) {
    g.mov_data_addr(R::V1, state);
    g.load_ptr_disp32(R::V0, R::V1, 24);
    let preserved = g.create_text_label();
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::Equal, preserved);
    g.cmp_reg_imm32(R::V0, 3);
    g.jcc_label(C::NotEqual, done);
    g.bind_text_label(preserved);
}
pub(super) fn start_preserved_old(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel) {
    let done = g.create_text_label();
    preserved_branch(g, state, done);
    let entry = g.gc_gen.as_ref().unwrap().labels.old_start_actor;
    graph::call_owner(g, entry);
    load_rt(g, R::V0, RT_MARK_CYCLES);
    g.mov_data_addr(R::V1, state);
    g.store_ptr_disp32(R::V1, 32, R::V0);
    g.bind_text_label(done);
}
fn check_preserved_old(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel) {
    let done = g.create_text_label();
    preserved_branch(g, state, done);
    g.cmp_reg_imm32(R::V0, 3);
    g.jcc_label(C::Equal, done);
    let fail = g.gc_gen.as_ref().unwrap().labels.fail;
    load_rt(g, R::V0, RT_MARK_ACTIVE);
    g.cmp_reg_imm32(R::V0, 1);
    g.jcc_label(C::NotEqual, fail);
    load_rt(g, R::V0, RT_MARK_CYCLES);
    g.mov_data_addr(R::V1, state);
    g.load_ptr_disp32(R::V1, R::V1, 32);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail);
    g.load_rbp_slot(R::V1, 8);
    g.store_ptr_disp32(R::V1, 96, R::V0);
    load_rt(g, R::V0, moving_round::STARTS);
    g.store_ptr_disp32(R::V1, 104, R::V0);
    g.bind_text_label(done);
}
pub(super) fn finish_preserved_old(g: &mut crate::NativeCodeGenerator, state: crate::DataLabel) {
    let done = g.create_text_label();
    preserved_branch(g, state, done);
    g.cmp_reg_imm32(R::V0, 3);
    g.jcc_label(C::Equal, done);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    g.cmp_reg_imm32(R::V0, 2);
    g.jcc_label(C::NotEqual, done);
    store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    let entry = g.gc_gen.as_ref().unwrap().labels.moving_start_actor;
    graph::call_owner(g, entry);
    finish(g);
    g.bind_text_label(done);
}

pub(crate) fn run_parallel_aborted_old_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    iterations: u64,
    master: crate::DataLabel,
) -> ParallelActors {
    let state = g.asm.data_label_with_i64s(&[0, 0, 0, 3, 0]);
    let gate = g.create_text_label();
    let selected = g.create_text_label();
    let body = g.create_text_label();
    g.gc_gen.as_mut().unwrap().labels.phase_mark_gate = Some(gate);
    g.gc_gen.as_mut().unwrap().labels.moving_selected_hook = Some(selected);
    g.jmp_label(body);
    g.bind_text_label(gate);
    load_rt(g, R::V0, old_cycle::REQUEST);
    g.ret();
    g.bind_text_label(selected);
    // The actual preflight already selected Young. This is the public Full
    // capacity request, arriving before the real producer quorum closes.
    store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    g.ret();
    g.bind_text_label(body);
    graph::run_graph(
        g,
        actors,
        iterations,
        master,
        GraphMode::Moving(state, false, 1, false),
    )
}
