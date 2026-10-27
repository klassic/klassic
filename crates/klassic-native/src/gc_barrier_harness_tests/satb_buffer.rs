//! Real previous-value transport, independent Young/Old epochs and poll liveness.
use super::satb_fixture::run;
use super::{H, Src};
use crate::gc_codegen::production::test_support::*;

fn deferred(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let allocation_handoff = mode >= 3;
    let kind = mode % 3;
    let mut h = H::begin(g);
    h.alloc(2, 32, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 8101);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 8102);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.store(Src::Root(0), 1, Src::Raw(2), true);
    // Allocation scratch/journal handoff must not independently retain the
    // last candidate child when the snapshot starts.
    h.alloc(1, 16, 20);
    publish_primary(h.g);
    dual_mark_enable(h.g);
    let gate = satb_gates(h.g);
    if kind != 1 {
        dual_mark_entry(h.g, false, true);
    }
    if kind != 0 {
        dual_mark_entry(h.g, true, true);
    }
    h.value(Src::Root(0));
    h.keep(4);
    h.report("old_before", |h| h.marked(Src::Raw(1)));
    h.report("young_before", |h| {
        h.value(Src::Raw(1));
        emit_young_marked(h.g);
    });
    // No RootHandle points to either child. Null publication must not erase
    // their captured previous-value obligations, in either active generation.
    h.store(Src::Raw(4), 0, Src::Null, true);
    h.store(Src::Raw(4), 1, Src::Null, true);
    h.report("old_deferred", |h| h.marked(Src::Raw(1)));
    h.report("young_deferred", |h| {
        h.value(Src::Raw(1));
        emit_young_marked(h.g);
    });
    h.report("pending_young", |h| satb_metric(h.g, 1));
    h.report("pending_old", |h| satb_metric(h.g, 2));
    h.report("leases_before", |h| lease_metric(h.g, 0));
    // Exercise each ordinary safepoint independently: a partial ring must
    // transfer on either one allocation or one explicit poll.
    if allocation_handoff {
        h.alloc(1, 16, 5);
    } else {
        satb_poll(h.g);
    }
    h.report("old_transferred", |h| h.marked(Src::Raw(1)));
    h.report("young_transferred", |h| {
        h.value(Src::Raw(1));
        emit_young_marked(h.g);
    });
    h.report("pending_young_after", |h| satb_metric(h.g, 1));
    h.report("pending_old_after", |h| satb_metric(h.g, 2));
    h.report("leases_after", |h| lease_metric(h.g, 0));
    // H::begin suppresses automatic Young rounds for explicit fixtures.
    // This test deliberately requires ordinary scheduler-driven completion.
    set_young_auto(h.g, true);
    worker_journal_open_gate(h.g, gate);
    // Stop creating fresh allocation/root-journal work before waiting for
    // both generations to close. An unbounded allocator can start later
    // epochs and exhaust this deliberately nonmoving fixed-bank fixture.
    satb_poll_until_closed(h.g);
    h.report("admitted", |h| satb_metric(h.g, 3));
    h.report("flushed", |h| satb_metric(h.g, 4));
    h.report("fallback", |h| satb_metric(h.g, 5));
    h.report("value1", |h| h.load(Src::Raw(1), 0));
    h.report("value2", |h| h.load(Src::Raw(2), 0));
}
#[test]
fn production_satb_previous_values_defer_without_premature_marks_and_progress_on_polls() {
    for mode in 0..6 {
        let (o, _image) = run(deferred, mode, true);
        let old = i64::from(mode % 3 != 1);
        let young = i64::from(mode % 3 != 0);
        assert_eq!(o.get("old_before"), 0, "{}", o.text());
        assert_eq!(o.get("young_before"), 0, "{}", o.text());
        assert_eq!(o.get("old_deferred"), 0, "{}", o.text());
        assert_eq!(o.get("young_deferred"), 0, "{}", o.text());
        assert_eq!(o.get("pending_old"), 2 * old, "{}", o.text());
        assert_eq!(o.get("pending_young"), 2 * young, "{}", o.text());
        assert_eq!(o.get("leases_before"), 4 * (old + young), "{}", o.text());
        assert_eq!(o.get("old_transferred"), old, "{}", o.text());
        assert_eq!(o.get("young_transferred"), young, "{}", o.text());
        assert_eq!(o.get("pending_old_after"), 0);
        assert_eq!(o.get("pending_young_after"), 0);
        assert_eq!(o.get("leases_after"), 0);
        assert_eq!(o.get("admitted"), 2 * (old + young));
        assert_eq!(o.get("flushed"), o.get("admitted"));
        assert_eq!(o.get("fallback"), 0);
        assert_eq!(o.get("value1"), 8101);
        assert_eq!(o.get("value2"), 8102);
    }
}

fn stale_epoch(g: &mut crate::NativeCodeGenerator, young: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.alloc(1, 16, 20);
    publish_primary(h.g);
    dual_mark_enable(h.g);
    let _gate = satb_gates(h.g);
    dual_mark_entry(h.g, young == 1, true);
    h.store(Src::Root(0), 0, Src::Null, true);
    h.report("count", |h| satb_metric(h.g, 0));
    h.report("leases", |h| lease_metric(h.g, 0));
    if young == 2 {
        satb_unsealed_move(h.g);
    } else {
        satb_stale_epoch(h.g, young != 0);
    }
}
#[test]
fn production_satb_rejects_captured_previous_obligations_from_another_epoch() {
    for young in 0..2 {
        let image = super::satb_fixture::capture(stale_epoch, young, true);
        let o = &image.output;
        assert_eq!(o.status.code(), Some(1));
        let out = String::from_utf8_lossy(&o.stdout);
        assert!(out.contains("count=1\n"), "{out}");
        assert!(out.contains("leases=2\n"), "{out}");
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}

#[test]
fn production_satb_unsealed_full_move_rejects_before_waiting_on_private_mark_work() {
    let image = super::satb_fixture::capture(stale_epoch, 2, true);
    let o = &image.output;
    assert_eq!(o.status.code(), Some(1));
    let out = String::from_utf8_lossy(&o.stdout);
    assert!(out.contains("count=1\n"), "{out}");
    assert!(out.contains("leases=2\n"), "{out}");
    assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
}
