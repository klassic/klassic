use super::super::*;
pub(crate) fn pin_adopt_root(g: &mut crate::NativeCodeGenerator) {
    let load = g.gc_gen.as_ref().unwrap().labels.root_load_actor;
    super::call_owner(g, load);
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    g.call_label(rt.labels.pin_adopt_small);
    worker::unlock(g, &rt.labels);
    worker::wake_if_pending(g, &rt.labels);
    g.gc_gen = Some(rt);
}
pub(crate) fn pin_adopt_metric(g: &mut crate::NativeCodeGenerator) {
    load_rt(g, R::V0, pin_adopt::ADOPTED);
}
pub(crate) fn pin_adopt_shrink(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    old_runs::shrink_unused(g, &rt.labels);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}
