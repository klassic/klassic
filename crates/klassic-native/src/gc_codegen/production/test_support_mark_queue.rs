use super::super::*;
/// Fixture only: narrow an idle bootstrap to exercise real overflow/growth.
pub(crate) fn mark_queue_capacity(g: &mut crate::NativeCodeGenerator, young: bool, slots: u64) {
    mark_queue::test_capacity(g, young, slots);
}
pub(crate) fn mark_queue_metric(g: &mut crate::NativeCodeGenerator, young: bool, index: usize) {
    mark_queue::test_metric(g, young, index);
}

pub(crate) fn mark_queue_growth_metric(
    g: &mut crate::NativeCodeGenerator,
    young: bool,
    index: usize,
) {
    mark_queue::growth_metric(g, young, index);
}
pub(crate) fn mark_queue_service(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    mark_queue::service(g, &rt.labels);
    g.gc_gen = Some(rt);
}

pub(crate) fn mark_queue_tick(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    mark_queue::work(g, &rt.labels);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
/// V0 is a real registered RootCell; offer its actual canonical object.
pub(crate) fn mark_queue_offer(g: &mut crate::NativeCodeGenerator, young: bool) {
    let load = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    super::call_owner(g, load);
    let rt = g.gc_gen.take().unwrap();
    g.mov_reg_reg(R::V2, R::V0);
    load_rt(g, R::V1, RT_HEAP_BASE);
    g.sub_reg_reg(R::V2, R::V1);
    worker::lock(g, &rt.labels);
    let w = rt.labels.worker.as_ref().unwrap();
    g.call_label(if young { w.young.reference } else { w.mark_ref });
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}

/// Real one-budget mark quanta until an actual record-backed job is active.
pub(crate) fn mark_queue_claim_active(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    let w = rt.labels.worker.as_ref().unwrap();
    g.call_label(w.hooks.clock);
    g.mov_imm64(R::V1, 30_000_000_000);
    g.add_reg_reg(R::V0, R::V1);
    g.store_rbp_slot(1504, R::V0);
    let again = g.create_text_label();
    let done = g.create_text_label();
    g.bind_text_label(again);
    worker::lock(g, &rt.labels);
    store_rt_imm(g, RT_BUDGET, 1);
    g.call_label(w.mark_loop);
    worker::unlock(g, &rt.labels);
    load_rt(g, R::V0, mark_queue::active_page(false));
    g.test_reg_reg(R::V0, R::V0);
    g.jcc_label(C::NotEqual, done);
    g.call_label(w.hooks.clock);
    g.load_rbp_slot(R::V1, 1504);
    g.cmp_reg_reg(R::V0, R::V1);
    g.jcc_label(C::AboveOrEqual, rt.labels.fail);
    g.jmp_label(again);
    g.bind_text_label(done);
    g.gc_gen = Some(rt);
}

/// The real registered cell is captured under the ticket, as root discovery
/// does. This intentionally avoids a mutator read barrier adding Old marking.
pub(crate) fn mark_queue_offer_captured_young(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    load_rt(g, R::V2, RT_SEGMENT);
    roots::frame_handle(g, R::V0, R::V2);
    root_step(g, &rt.labels, RT_FRAME_ROOT, ROOT_READ_RAW);
    load_rt(g, R::V2, RT_FRAME_ROOT + REQUEST_VALUE);
    g.shr_reg_imm8(R::V2, 16);
    g.call_label(rt.labels.worker.as_ref().unwrap().young.reference);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn mark_queue_reference(g: &mut crate::NativeCodeGenerator, young: bool, spill: bool) {
    if spill {
        load_rt(g, R::V0, mark_queue::head(young));
        g.load_ptr_disp32(
            R::V0,
            R::V0,
            d(mark_queue::node_offset(young) + offset_of!(mark_queue::SpillNode, reference)),
        );
    } else {
        load_rt(g, R::V0, mark_queue::entries(young));
        g.load_ptr_disp32(R::V0, R::V0, 0);
    }
}
