//! Actual field-store fallback while real Young relocation is open.
use super::satb_fixture::run;
use super::{H, Src};
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn phase_open_previous(g: &mut crate::NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let park = satb_fallback_prepare(h.g);
    h.alloc(2, 16, 0);
    h.root(0);
    publish_primary(h.g);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.value(Src::Root(0));
    h.keep(4);
    h.report("holder_old", |h| h.is_old(Src::Raw(4)));
    h.report_counter("initial_active", Counter::MarkActive);
    h.report_counter("initial_valid", Counter::MarkValid);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 9817);
    h.store(Src::Raw(4), 0, Src::Raw(1), true);
    h.alloc(1, 16, 20);
    publish_primary(h.g);
    remembered_buffer_enable_moving(h.g);
    let identity = h.g.asm.data_label_with_i64s(&[0]);
    h.object_id(Src::Raw(1));
    h.g.mov_data_addr(R::V1, identity);
    h.g.store_ptr_disp32(R::V1, 0, R::V0);
    h.report("source_id", |h| h.object_id(Src::Raw(1)));
    h.report("source", |h| h.value(Src::Raw(1)));
    h.report("field_before", |h| h.load(Src::Raw(4), 0));
    let gate = worker_journal_gate(h.g);
    dual_mark_entry(h.g, false, true);
    h.report("leases_before", |h| lease_metric(h.g, 0));
    h.report("fallback_before", |h| satb_metric(h.g, 5));
    h.report("admitted_before", |h| satb_metric(h.g, 3));
    satb_fallback_open(h.g, park);
    h.report_counter("phase", Counter::PhaseOpen);
    h.report_counter("retire", Counter::RetireStage);
    // Capture the actual logical previous referent, whether the moving
    // machinery has already healed this field or still names its source.
    // This raw observation does not introduce a GC root or mark obligation.
    h.load(Src::Raw(4), 0);
    h.g.shr_reg_imm8(R::V0, 16);
    h.g.load_ptr_disp32(R::V1, R::V3, heap_base_offset());
    h.g.add_reg_reg(R::V0, R::V1);
    h.keep(6);
    h.report("previous_id", |h| h.object_id(Src::Raw(6)));
    h.report("white_before", |h| h.marked(Src::Raw(6)));
    h.store(Src::Raw(4), 0, Src::Null, true);
    h.report("marked_after", |h| h.marked(Src::Raw(6)));
    h.report("fallback_after", |h| satb_metric(h.g, 5));
    h.report("admitted_after", |h| satb_metric(h.g, 3));
    h.report("pending_after", |h| satb_metric(h.g, 2));
    h.report("leases_after", |h| lease_metric(h.g, 0));
    satb_fallback_finish(h.g, park);
    satb_moving_frontier(h.g, identity);
    h.keep(5);
    h.report("destination", |h| h.value(Src::Raw(5)));
    h.report("destination_id", |h| h.object_id(Src::Raw(5)));
    h.report("destination_value", |h| h.load(Src::Raw(5), 0));
    h.root(5);
    worker_journal_open_gate(h.g, gate);
    satb_moving_close_old(h.g);
    h.report_counter("old_closed", Counter::MarkActive);
    h.report("root_value", |h| h.load(Src::Root(5), 0));
}

#[test]
fn production_satb_phase_open_store_synchronously_retains_logical_previous() {
    let (o, _image) = run(phase_open_previous, 0, true);
    assert_eq!(o.get("holder_old"), 1, "{}", o.text());
    assert_eq!(o.get("initial_active"), 0);
    assert_eq!(o.get("initial_valid"), 0);
    assert_eq!(o.get("phase"), 1);
    assert_eq!(o.get("retire"), 0);
    assert_eq!(o.get("white_before"), 0);
    assert_eq!(o.get("previous_id"), o.get("source_id"));
    assert_eq!(o.get("marked_after"), 1);
    assert_eq!(o.get("fallback_after"), o.get("fallback_before") + 1);
    assert_eq!(o.get("admitted_after"), o.get("admitted_before"));
    assert_eq!(o.get("pending_after"), 0);
    assert_eq!(o.get("leases_after"), o.get("leases_before"));
    assert_ne!(o.get("destination"), 0);
    assert_ne!(o.get("destination"), o.get("source"));
    assert_eq!(o.get("destination_id"), o.get("source_id"));
    assert_eq!(o.get("destination_value"), 9817);
    assert_eq!(o.get("old_closed"), 0);
    assert_eq!(o.get("root_value"), 9817);
}
