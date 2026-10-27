//! Real Native peer threads wait across actual complete collections before
//! returning, reading their own registered roots and allocating in the new bank.
use super::super::super::JournalRegion;
use super::*;
pub(crate) fn run_parallel_native_completed(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    roots: &[crate::DataLabel],
    cycles: u64,
    full: bool,
    graph: bool,
) -> ParallelActors {
    run_parallel_impl(
        g,
        actors,
        1,
        Some(roots),
        GraphMode::NativeCycles(cycles, full, graph),
    )
}
pub(super) fn collect(g: &mut crate::NativeCodeGenerator, cycles: u64, full: bool) {
    for _ in 0..cycles {
        if full {
            // Exercise the actual public collector's capacity-exhaustion request,
            // not a synthetic forwarding header, phase or completion receipt.
            store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
        }
        let legacy = g.create_text_label();
        let done = g.create_text_label();
        load_rt(g, R::V0, moving_round::ENABLED);
        g.test_reg_reg(R::V0, R::V0);
        g.jcc_label(C::Equal, legacy);
        let start = g.gc_gen.as_ref().unwrap().labels.moving_start_actor;
        graph::call_owner(g, start);
        super::next_mark::finish_move(g);
        g.jmp_label(done);
        g.bind_text_label(legacy);
        let rt = g.gc_gen.take().unwrap();
        g.call_label(rt.labels.collect);
        worker::lock(g, &rt.labels);
        g.call_label(rt.labels.finish);
        worker::unlock(g, &rt.labels);
        g.gc_gen = Some(rt);
        g.bind_text_label(done);
    }
}
pub(super) fn iteration(g: &mut crate::NativeCodeGenerator, graph_mode: bool) {
    let (load, alloc) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.root_load_actor, l.alloc_actor)
    };
    g.load_rbp_slot(R::V0, 8);
    g.load_ptr_disp32(R::V0, R::V0, HUB_ROOT);
    graph::call_owner(g, load);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V2, R::V1, RESULTS);
    g.store_ptr_disp32(R::V2, 0, R::V0);
    if graph_mode {
        graph_observe(g);
    } else {
        g.load_ptr_disp32(R::V6, R::V0, 0);
        g.store_ptr_disp32(R::V1, GRAPH_READS, R::V6);
    }
    g.load_ptr_disp32(R::V6, R::V0, copy::MARK_RECORD);
    g.store_ptr_disp32(R::V2, 16, R::V6);
    g.load_ptr_disp32(R::V6, R::V6, d(offset_of!(MarkRecord, object_id)));
    g.store_ptr_disp32(R::V2, 8, R::V6);
    load_rt(g, R::V0, native_io::PHASE);
    g.store_ptr_disp32(R::V1, OLD_WORKER, R::V0);
    load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    g.store_ptr_disp32(R::V1, 96, R::V0);
    load_rt(g, R::V0, allocation_records::REVISION);
    g.store_ptr_disp32(R::V1, 104, R::V0);
    load_rt(g, R::V0, allocation_records::OWNER_REVISION);
    g.store_ptr_disp32(R::V1, DELAYED_COMPLETE, R::V0);
    g.mov_imm64(R::V7, 16);
    g.mov_imm64(R::V6, 1);
    graph::call_owner(g, alloc);
    g.mov_imm64(R::V6, 99);
    g.store_ptr_disp32(R::V0, 0, R::V6);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V6, R::V0, 0);
    g.store_ptr_disp32(R::V1, DELAYED_ACKS, R::V6);
    g.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
    g.store_ptr_disp32(R::V1, 120, R::V0);
}
pub(crate) fn native_completed_metric(
    g: &mut crate::NativeCodeGenerator,
    p: &ParallelActors,
    i: usize,
    field: i32,
) {
    g.mov_data_addr(R::V1, p.controls[i]);
    g.load_ptr_disp32(R::V0, R::V1, field);
}
pub(crate) fn native_receipt_metric(g: &mut crate::NativeCodeGenerator, field: usize) {
    let f = [
        native_completion::BANK,
        native_completion::SCOPE,
        native_completion::PHASE,
        allocation_records::REVISION,
        RT_RETIRE_STAGE,
        RT_PHASE_OPEN,
        RT_MARKS + offset_of!(MarkArena, records),
        RT_MARKS + offset_of!(MarkArena, used),
    ][field];
    load_rt(g, R::V0, f);
}

pub(crate) fn native_completed_root(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    root: crate::DataLabel,
    id: bool,
) {
    select(g, actor);
    let l = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    g.mov_data_addr(R::V0, root);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    graph::call_owner(g, l);
    if id {
        g.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
        g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
    }
    restore(g);
}

fn graph_observe(g: &mut crate::NativeCodeGenerator) {
    let (load, fail) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.field_load_actor, l.fail)
    };
    g.store_rbp_slot(32, R::V0); // no safepoint before the next managed read
    g.mov_reg_reg(R::V10, R::V0);
    g.atomic_load_acquire(R::V0, R::V10);
    graph::call_owner(g, load);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V6, R::V0, 0);
    g.store_ptr_disp32(R::V1, GRAPH_READS, R::V6);
    g.load_ptr_disp32(R::V6, R::V0, copy::MARK_RECORD);
    g.load_ptr_disp32(R::V6, R::V6, d(offset_of!(MarkRecord, object_id)));
    g.store_ptr_disp32(R::V1, 112, R::V6);
    g.load_rbp_slot(R::V10, 32);
    g.add_reg_imm32(R::V10, 8);
    g.atomic_load_acquire(R::V0, R::V10);
    graph::call_owner(g, load);
    g.load_rbp_slot(R::V1, 32);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::NotEqual, fail); // the actual parent.self must still be canonical
    g.load_rbp_slot(R::V0, 32);
    g.load_rbp_slot(R::V1, 8);
    g.load_ptr_disp32(R::V2, R::V1, RESULTS);
}
pub(crate) fn native_completed_graph(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    value: u64,
) -> crate::DataLabel {
    actor_alloc(g, actor, 16);
    g.mov_imm64(R::V1, value);
    g.store_ptr_disp32(R::V0, 0, R::V1);
    let leaf = adopt_actor_value(g, actor);
    select(g, actor);
    let (alloc, load, store, replace, pop) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (
            l.alloc_actor,
            l.root_load_actor,
            l.field_store_actor,
            l.root_store_actor,
            l.root_pop_actor,
        )
    };
    g.mov_imm64(R::V7, 16);
    g.mov_imm64(R::V6, 2);
    graph::call_owner(g, alloc);
    g.asm.data.resize(g.asm.data.len().next_multiple_of(8), 0);
    let parent = g.asm.data_label_with_i64s(&[0]);
    g.mov_data_addr(R::V1, parent);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.mov_data_addr(R::V0, leaf);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    graph::call_owner(g, load);
    g.mov_reg_reg(R::V6, R::V0);
    g.mov_data_addr(R::V1, parent);
    g.load_ptr_disp32(R::V7, R::V1, 0);
    graph::call_owner(g, store);
    g.mov_data_addr(R::V1, parent);
    g.load_ptr_disp32(R::V6, R::V1, 0);
    g.mov_reg_reg(R::V7, R::V6);
    g.add_reg_imm32(R::V7, 8);
    graph::call_owner(g, store);
    g.mov_data_addr(R::V1, parent);
    g.load_ptr_disp32(R::V0, R::V1, 0);
    restore(g);
    let root = adopt_actor_value(g, actor);
    select(g, actor);
    g.mov_data_addr(R::V0, root);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    graph::call_owner(g, load);
    g.mov_reg_reg(R::V6, R::V0);
    g.mov_data_addr(R::V1, leaf);
    graph::call_owner(g, replace);
    graph::call_owner(g, pop); // only leaf's handle now retains the parent/leaf cycle
    restore(g);
    leaf
}
pub(crate) fn native_completed_leaf_id(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    root: crate::DataLabel,
) {
    select(g, actor);
    let (root_load, load) = {
        let l = &g.gc_gen.as_ref().unwrap().labels;
        (l.root_load_actor, l.field_load_actor)
    };
    g.mov_data_addr(R::V0, root);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    graph::call_owner(g, root_load);
    g.mov_reg_reg(R::V10, R::V0);
    g.atomic_load_acquire(R::V0, R::V10);
    graph::call_owner(g, load);
    g.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
    restore(g);
}
pub(crate) fn native_completed_fault(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
    mode: u64,
) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    match mode {
        0 => store_rt_imm(g, native_completion::BANK, 0),
        1 => {
            load_rt(g, R::V0, native_completion::BANK);
            g.sub_reg_imm8(R::V0, 1);
            store_rt(g, native_completion::BANK, R::V0);
        }
        2 => {
            load_rt(g, R::V0, native_completion::PHASE);
            g.add_reg_imm32(R::V0, 0x10000);
            store_rt(g, native_completion::PHASE, R::V0);
        }
        3 => store_rt_imm(g, native_completion::SCOPE, 0),
        _ => (),
    }
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    select(g, actor);
    match mode {
        4 => store_rt_imm(g, native_io::ROOT_SCOPE, 0),
        7 => store_rt_imm(g, native_io::BANK, 0),
        8 => {
            load_rt(g, R::V0, native_completion::PHASE);
            g.add_reg_imm32(R::V0, 0x10000);
            store_rt(g, native_io::PHASE, R::V0);
            store_rt(g, RT_CTX + offset_of!(MutatorContext, adopted_phase), R::V0);
        }
        5 => {
            load_rt(g, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
            g.add_reg_imm32(R::V0, 0x10000);
            store_rt(g, RT_CTX + offset_of!(MutatorContext, adopted_phase), R::V0);
        }
        6 => {
            load_rt(g, R::V0, allocation_records::REVISION);
            g.add_reg_imm32(R::V0, 1);
            store_rt(g, native_io::BANK, R::V0);
        }
        _ => (),
    }
    let exit = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
    graph::call_owner(g, exit);
    // An invalidly accepted receipt must not be hidden by a later guard.
    g.asm.mov_imm64(X::Rax, 231);
    g.asm.mov_imm64(X::Rdi, 0);
    g.asm.syscall();
}
pub(crate) fn native_completed_collect(g: &mut crate::NativeCodeGenerator) {
    collect(g, 1, false);
}

pub(crate) fn native_completed_finish_other(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    g.call_label(rt.labels.finish);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    restore(g);
}
pub(crate) fn native_completed_resume(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    let l = g.gc_gen.as_ref().unwrap().labels.native_exit_actor;
    graph::call_owner(g, l);
    restore(g);
}
pub(crate) fn native_completed_full_other(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    collect(g, 1, true);
    restore(g);
}
pub(crate) fn native_completed_open_other(g: &mut crate::NativeCodeGenerator, actor: TestActor) {
    select(g, actor);
    let rt = g.gc_gen.take().unwrap();
    g.call_label(rt.labels.worker.as_ref().unwrap().young.start);
    young_mark::wait(g, &rt.labels);
    g.call_label(rt.labels.collect);
    g.gc_gen = Some(rt);
    restore(g);
}

pub(crate) fn native_completed_region_id(
    g: &mut crate::NativeCodeGenerator,
    region: JournalRegion,
) {
    let l = g.gc_gen.as_ref().unwrap().labels.field_load_actor;
    g.mov_data_addr(R::V10, region.storage);
    g.add_reg_imm32(R::V10, size_of::<RootStorage>() as i32);
    g.atomic_load_acquire(R::V0, R::V10);
    graph::call_owner(g, l);
    g.load_ptr_disp32(R::V0, R::V0, copy::MARK_RECORD);
    g.load_ptr_disp32(R::V0, R::V0, d(offset_of!(MarkRecord, object_id)));
}
