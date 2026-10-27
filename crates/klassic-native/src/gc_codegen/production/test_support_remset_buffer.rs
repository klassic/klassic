//! Real enrolled-owner remembered transport and forbidden unsealed flips.
use super::super::*;
use super::*;
pub(crate) fn remembered_buffer_metric(g: &mut crate::NativeCodeGenerator, metric: usize) {
    load_rt(
        g,
        R::V0,
        [
            remset_buffer::COUNT,
            remset_buffer::PENDING,
            remset_buffer::ADMITTED,
            remset_buffer::FLUSHED,
            remset_buffer::SYNCHRONOUS,
        ][metric],
    );
}
pub(crate) fn remembered_buffer_enable_moving(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.moving_enable_actor;
    call_owner(g, entry);
}
pub(crate) fn remembered_buffer_cycle(g: &mut crate::NativeCodeGenerator, full: bool) {
    if full {
        store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    }
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (start, end, poll, relax) = (
        l.moving_start_actor,
        l.moving_end_actor,
        l.poll_actor,
        l.worker.as_ref().unwrap().hooks.relax,
    );
    call_owner(g, start);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, end);
    call_owner(g, poll);
    load_rt(g, R::V0, RT_PHASE_OPEN);
    load_rt(g, R::V1, RT_RETIRE_STAGE);
    g.or_reg_reg(R::V0, R::V1);
    load_rt(g, R::V1, phase_round::RESUMING);
    g.or_reg_reg(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}
pub(crate) fn remembered_buffer_unsealed_flip(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    remset::emit_flip(g, &rt.labels);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
/// The real graph stores new parents into an already promoted shared hub.
/// Each participant seals its private entries at the actual moving quorum.
pub(crate) fn run_parallel_buffered_remembered_graph(
    g: &mut crate::NativeCodeGenerator,
    actors: &[TestActor],
    master: crate::DataLabel,
) -> ParallelActors {
    run_parallel_moving_graph(g, actors, 12, master, (false, false, 3, false, false))
}

/// V0=primary's actual holder RootCell. The new owner captures its own roots,
/// publishes one Old->Young field and releases the child's explicit root.
pub(crate) fn remembered_buffer_seed_detaching_actor(
    g: &mut crate::NativeCodeGenerator,
    actor: TestActor,
) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (load, adopt, alloc, store, root_store) = (
        l.root_load_actor,
        l.root_adopt_actor,
        l.alloc_actor,
        l.field_store_actor,
        l.root_store_actor,
    );
    call_owner(g, load);
    let holder = g.asm.data_label_with_i64s(&[0]);
    let child = g.asm.data_label_with_i64s(&[0]);
    g.mov_data_addr(R::V1, holder);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.push_reg(R::OwnerContext);
    g.mov_data_addr(R::V0, actor.pointer);
    g.load_ptr_disp32(R::OwnerContext, R::V0, 0);
    g.mov_data_addr(R::V0, holder);
    call_owner(g, adopt);
    g.mov_imm64(R::V7, 16);
    g.mov_imm64(R::V6, 1);
    call_owner(g, alloc);
    g.mov_imm64(R::V1, 84);
    g.store_ptr_disp32(R::V0, 0, R::V1);
    g.mov_data_addr(R::V1, child);
    g.store_ptr_disp32(R::V1, 0, R::V0);
    g.mov_reg_reg(R::V0, R::V1);
    call_owner(g, adopt);
    g.mov_data_addr(R::V0, holder);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    call_owner(g, load);
    g.mov_reg_reg(R::V7, R::V0);
    g.mov_data_addr(R::V0, child);
    g.load_ptr_disp32(R::V0, R::V0, 0);
    call_owner(g, load);
    g.mov_reg_reg(R::V6, R::V0);
    call_owner(g, store);
    g.mov_data_addr(R::V1, child);
    g.mov_imm64(R::V0, 0);
    call_owner(g, root_store);
    g.pop_reg(R::OwnerContext);
}
