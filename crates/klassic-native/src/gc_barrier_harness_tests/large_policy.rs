//! Dedicated Old pressure must start snapshots and planned cycles before OOM.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::HEAP_PAGE_BYTES as P;
use crate::gc_codegen::production::{
    large_object as large,
    test_support::{
        Counter, completed_mark_hint, emit_unbounded_budget, large_retained_pages, old_used,
        set_worker_alive,
    },
};
use crate::portable_asm::PortableAsm;

fn seed(g: &mut NativeCodeGenerator) -> H<'_> {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    h.alloc(1, 2 << 20, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h
}
fn ready(h: &mut H<'_>) {
    h.mark_start();
    h.drain_marks();
    h.g.call_label(h.l.young_start);
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    completed_mark_hint(h.g);
}
fn poll(h: &mut H<'_>, slot: u32) {
    set_worker_alive(h.g, true); // deterministic completion hint, no OS worker
    h.alloc(1, 16, slot);
    set_worker_alive(h.g, false);
    h.finish_collection();
}
fn planned(g: &mut NativeCodeGenerator, interpose_young: u64) {
    let mut h = seed(g);
    h.report("moving_before", |h| old_used(h.g));
    ready(&mut h);
    poll(&mut h, 1);
    h.report_counter("first", Counter::OldPlannedCycles);
    h.report("retained", |h| large_retained_pages(h.g));
    ready(&mut h);
    poll(&mut h, 2);
    h.report_counter("repeat", Counter::OldPlannedCycles);
    h.alloc(1, 1 << 20, 3); // unrooted growth, below the dedicated page limit
    if interpose_young != 0 {
        h.collect();
        h.finish_collection();
    }
    h.report("young_retained", |h| large_retained_pages(h.g));
    h.drain_marks();
    completed_mark_hint(h.g);
    poll(&mut h, 4);
    h.report_counter("growth", Counter::OldPlannedCycles);
    h.report("after_retained", |h| large_retained_pages(h.g));
    for (name, offset) in [
        ("live", large::report_offsets()[3]),
        ("reclaimed", large::report_offsets()[4]),
        ("retries", large::report_offsets()[2]),
    ] {
        h.report(name, |h| {
            h.g.load_ptr_disp32(
                crate::portable_asm::Reg::V0,
                crate::portable_asm::Reg::V3,
                offset,
            )
        });
    }
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_large_policy_plans_for_growth_and_defers_the_same_retained_heap() {
    for interpose_young in [0, 1] {
        let o = run(planned, interpose_young, false);
        assert_eq!(o.get("moving_before"), 0);
        assert_eq!(o.get("first"), 1);
        assert_eq!(o.get("repeat"), 1);
        assert_eq!(o.get("growth"), 2);
        assert_eq!(o.get("retained"), 17);
        assert_eq!(o.get("young_retained"), 17);
        assert_eq!(o.get("after_retained"), 17);
        assert_eq!(o.get("live"), 17);
        assert_eq!(o.get("reclaimed"), 9);
        assert_eq!(o.get("retries"), 0);
        assert_eq!(o.get("value"), 84);
    }
}

fn stale(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = seed(g);
    h.mark_start();
    // An empty-frontier hint cannot bypass the actual unvisited root bound.
    completed_mark_hint(h.g);
    poll(&mut h, 1);
    h.report_counter("checks", Counter::OldCycleChecks);
    h.report_counter("retries", Counter::OldCycleRetries);
    h.report_counter("planned", Counter::OldPlannedCycles);
    h.report_counter("full", Counter::FullCollections);
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_large_policy_rechecks_the_actual_snapshot_frontier_before_reclaiming() {
    let o = run(stale, 0, false);
    assert_eq!(o.get("checks"), 1);
    assert_eq!(o.get("retries"), 1);
    assert_eq!(o.get("planned"), 0);
    assert_eq!(o.get("full"), 0);
    assert_eq!(o.get("value"), 84);
}
fn automatic(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = seed(g);
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 1);
    h.report_counter("prepared", Counter::MarkPrepareState);
    h.alloc(1, 16, 2);
    set_worker_alive(h.g, false);
    h.report_counter("cycles", Counter::MarkCycles);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("full", Counter::FullCollections);
    h.report("moving", |h| old_used(h.g));
    h.drain_marks();
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_large_policy_starts_old_marking_with_an_almost_empty_young_window() {
    let o = run(automatic, 0, false);
    assert_eq!(o.get("prepared"), 2);
    assert_eq!(o.get("cycles"), 1);
    assert_eq!(o.get("active"), 1);
    assert_eq!(o.get("full"), 0);
    assert_eq!(o.get("moving"), 0);
    assert_eq!(o.get("value"), 84);
}
