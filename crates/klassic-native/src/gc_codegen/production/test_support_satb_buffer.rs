//! Actual deferred SATB observations and ordinary-poll completion.
use super::super::*;
use super::*;

pub(crate) fn satb_metric(g: &mut crate::NativeCodeGenerator, which: usize) {
    load_rt(
        g,
        R::V0,
        [
            satb_buffer::COUNT,
            satb_buffer::pending_count(true),
            satb_buffer::pending_count(false),
            satb_buffer::ADMITTED,
            satb_buffer::FLUSHED,
            satb_buffer::SYNCHRONOUS,
        ][which],
    );
}
pub(crate) fn satb_gates(g: &mut crate::NativeCodeGenerator) -> crate::DataLabel {
    let state = worker_journal_gate(g);
    let labels = &mut g.gc_gen.as_mut().unwrap().labels;
    labels.young_mark_gate = labels.phase_mark_gate;
    state
}
pub(crate) fn satb_poll(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.poll_actor;
    call_owner(g, entry);
}
pub(crate) fn satb_poll_until_closed(g: &mut crate::NativeCodeGenerator) {
    let l = &g.gc_gen.as_ref().unwrap().labels;
    let (poll, relax) = (l.poll_actor, l.worker.as_ref().unwrap().hooks.relax);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    call_owner(g, poll);
    load_rt(g, R::V0, RT_MARK_ACTIVE);
    load_rt(g, R::V1, young_mark::ACTIVE);
    g.or_reg_reg(R::V0, R::V1);
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::Equal, done);
    g.call_label(relax);
    g.jmp_label(again);
    g.bind_text_label(done);
}

/// Fault injection after a real admission: a new epoch must never consume the
/// prior epoch's private obligation. Acceptance exits before unrelated guards.
pub(crate) fn satb_stale_epoch(g: &mut crate::NativeCodeGenerator, young: bool) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    let epoch = if young {
        young_mark::EPOCH
    } else {
        RT_MARK_CYCLES
    };
    load_rt(g, R::V0, epoch);
    g.add_reg_imm32(R::V0, 1);
    store_rt(g, epoch, R::V0);
    satb_buffer::flush(g, &rt.labels, true);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
    g.mov_imm64(R::V0, 231);
    g.mov_imm64(R::V7, 0);
    g.asm.syscall();
}

/// Deliberately bypass the moving quorum with a genuinely occupied ring.
/// The admission guard must reject before a full mark drain can wait on it.
pub(crate) fn satb_unsealed_move(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    store_rt_imm(g, old_cycle::REQUEST, old_cycle::CAPACITY);
    g.call_label(rt.labels.collect_moving);
    g.gc_gen = Some(rt);
    g.mov_imm64(R::V0, 231);
    g.mov_imm64(R::V7, 0);
    g.asm.syscall();
}
