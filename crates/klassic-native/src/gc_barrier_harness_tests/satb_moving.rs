//! Young relocation seals private Old SATB before moving its strong frontier.
use super::satb_fixture::run;
use super::{H, Src};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn move_previous(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 9801);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.alloc(1, 16, 20); // no allocator scratch root to the intended previous value
    publish_primary(h.g);
    // Enrollment seals the sole-consumer construction journals before the
    // snapshot. Enabling before allocation lets the independent journal
    // consumer retain historical children even while the graph tracer is gated.
    remembered_buffer_enable_moving(h.g);
    let identity = h.g.asm.data_label_with_i64s(&[0]);
    h.object_id(Src::Raw(1));
    h.g.mov_data_addr(R::V1, identity);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    h.report("source_id", |h| h.object_id(Src::Raw(1)));
    h.report("source", |h| h.value(Src::Raw(1)));
    let gate = worker_journal_gate(h.g); // Only Old is paused; Young may trace.
    dual_mark_entry(h.g, false, true);
    h.value(Src::Root(0));
    h.keep(4);
    h.report("before_marked", |h| h.marked(Src::Raw(1)));
    h.report_counter("epoch_before", Counter::MarkCycles);
    h.report_counter("collections_before", Counter::Collections);
    h.report_counter("full_before", Counter::FullCollections);
    h.report("leases_baseline", |h| lease_metric(h.g, 0));
    h.report("journal_before_store", |h| worker_journal_counter(h.g, 0));
    h.store(Src::Raw(4), 0, Src::Null, true);
    h.report("fallback_after_store", |h| satb_metric(h.g, 5));
    h.report("journal_after_store", |h| worker_journal_counter(h.g, 0));
    h.report_counter("phase_after_store", Counter::PhaseOpen);
    h.report_counter("retire_after_store", Counter::RetireStage);
    h.report("pending_before", |h| satb_metric(h.g, 2));
    h.report("deferred_marked", |h| h.marked(Src::Raw(1)));
    h.report("leases_before", |h| lease_metric(h.g, 0));
    // No poll, root creation or manual marker transfer intervenes. The actual
    // moving-start quorum must transfer the private previous value first.
    remembered_buffer_cycle(h.g, false);
    h.report_counter("epoch_after", Counter::MarkCycles);
    h.report_counter("collections_after", Counter::Collections);
    h.report_counter("full_after", Counter::FullCollections);
    h.report_counter("old_active", Counter::MarkActive);
    h.report("pending_after", |h| satb_metric(h.g, 2));
    h.report("leases_after", |h| lease_metric(h.g, 0));
    h.report("admitted", |h| satb_metric(h.g, 3));
    h.report("flushed", |h| satb_metric(h.g, 4));
    // The child has had no RootHandle throughout deletion and relocation.
    // Find it in the real Old frontier, whose words must already be healed.
    satb_moving_frontier(h.g, identity);
    h.keep(5);
    h.report("destination", |h| h.value(Src::Raw(5)));
    h.report("destination_id", |h| h.object_id(Src::Raw(5)));
    h.report("destination_value", |h| h.load(Src::Raw(5), 0));
    h.report("destination_old", |h| h.is_old(Src::Raw(5)));
    h.root(5); // Only now expose the recovered canonical reference as a root.
    worker_journal_open_gate(h.g, gate);
    satb_moving_close_old(h.g);
    h.report_counter("old_closed", Counter::MarkActive);
    h.report("root_value", |h| h.load(Src::Root(5), 0));
}

#[test]
fn production_satb_young_move_transfers_private_old_previous_into_relocated_frontier() {
    let (o, _image) = run(move_previous, 0, true);
    assert_eq!(o.get("before_marked"), 0, "{}", o.text());
    assert_eq!(o.get("deferred_marked"), 0);
    assert_eq!(o.get("pending_before"), 1);
    assert_eq!(o.get("fallback_after_store"), 0);
    assert_eq!(o.get("phase_after_store"), 0);
    assert_eq!(o.get("retire_after_store"), 0);
    assert_eq!(o.get("leases_before"), o.get("leases_baseline") + 2);
    assert_eq!(o.get("epoch_after"), o.get("epoch_before"));
    assert_eq!(o.get("collections_after"), o.get("collections_before") + 1);
    assert_eq!(o.get("full_after"), o.get("full_before"));
    assert_eq!(o.get("old_active"), 1);
    assert_eq!(o.get("pending_after"), 0);
    assert_eq!(o.get("leases_after"), o.get("leases_baseline"));
    assert_eq!(o.get("admitted"), 1);
    assert_eq!(o.get("flushed"), 1);
    assert_ne!(o.get("destination"), 0);
    assert_ne!(o.get("destination"), o.get("source"));
    assert_eq!(o.get("destination_id"), o.get("source_id"));
    assert_eq!(o.get("destination_value"), 9801);
    assert_eq!(o.get("destination_old"), 0);
    assert_eq!(o.get("old_closed"), 0);
    assert_eq!(o.get("root_value"), 9801);
}
