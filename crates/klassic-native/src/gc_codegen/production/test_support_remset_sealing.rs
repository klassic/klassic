//! A real owner handoff followed by an intentionally invalid store probe.
use super::super::*;

pub(crate) fn remembered_owner_ack(g: &mut crate::NativeCodeGenerator) {
    let rt = g.gc_gen.take().unwrap();
    worker::lock(g, &rt.labels);
    quiescence::prepare(g, &rt.labels);
    participants::begin(g, &rt.labels);
    worker::unlock(g, &rt.labels);
    g.gc_gen = Some(rt);
}

/// V7 is a captured valid slot; bypass the public actor wrapper deliberately
/// to probe the transport's own state check, including its deduplication path.
pub(crate) fn remembered_record_probe(g: &mut crate::NativeCodeGenerator) {
    let entry = g.gc_gen.as_ref().unwrap().labels.remset_buffer_record;
    g.call_label(entry);
}
