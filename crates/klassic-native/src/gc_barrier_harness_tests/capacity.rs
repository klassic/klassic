//! Actual allocation-window changes, protected byte accounting and reuse.
use super::{H, Src, run};
use crate::gc_codegen::production::{
    MAX_PAYLOAD,
    test_support::{
        Counter, capacity_policy_input, capacity_policy_repeat, request_old_cycle,
        request_young_pages, young_trace_complete, young_window_pages,
    },
};
use crate::{NativeCodeGenerator, gc_abi::HEAP_PAGE_BYTES as PAGE};
fn cycle(h: &mut H<'_>) {
    h.collect();
    h.finish_collection();
}
fn traced(h: &mut H<'_>) {
    young_trace_complete(h.g, &h.l);
}
fn windows(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 15);
    h.set_scalar(Src::Raw(15), 0, 84);
    h.root(15);
    cycle(&mut h);
    cycle(&mut h); // An Old keeper is not Young volume.
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 99);
    h.root(0);
    h.value(Src::Root(0));
    h.keep(1);
    h.root(1); // Duplicate snapshot root.
    request_young_pages(h.g, 64);
    traced(&mut h);
    h.collect();
    h.report_counter("small", Counter::CapacityActive);
    h.report_counter("small_start", Counter::StartTarget);
    h.report("window", |h| young_window_pages(h.g));
    h.report_counter("small_bytes", Counter::CapacitySealed);
    h.report_counter("small_reserve", Counter::CapacityReserve);
    h.report_counter("fresh0", Counter::CapacityFresh);
    h.finish_collection();
    h.repeat(20, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.root(9);
    });
    traced(&mut h);
    h.alloc(1, 0, 2);
    h.root(2); // Born black, after root traversal completed.
    h.collect();
    h.report_counter("grown", Counter::CapacityActive);
    h.report_counter("trace", Counter::CapacityTrace);
    h.report_counter("black", Counter::CapacityBlack);
    h.report_counter("protected", Counter::CapacitySealed);
    h.report_counter("reserve", Counter::CapacityReserve);
    h.report_counter("clamps", Counter::CapacityClamps);
    h.report_counter("grows", Counter::CapacityGrows);
    h.report_counter("shrinks", Counter::CapacityShrinks);
    h.report("grown_window", |h| young_window_pages(h.g));
    h.finish_collection();
    request_old_cycle(h.g);
    h.collect();
    h.report_counter("full_window", Counter::CapacityActive);
    h.report_counter("full_reserve", Counter::CapacityReserve);
    h.finish_collection();
    h.report("old", |h| h.load(Src::Root(15), 0));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report_counter("unmarked", Counter::YoungUnmarked);
}
#[test]
fn production_young_capacity_shrinks_grows_for_protected_copy_volume_and_reuses_both_windows() {
    let o = run(windows, 0, false);
    for (n, v) in [
        ("small", 64),
        ("small_start", 48),
        ("window", 64),
        ("small_bytes", 48),
        ("small_reserve", 5),
        ("fresh0", PAGE as i64),
        ("grown", 85),
        ("grown_window", 85),
        ("trace", 20 * PAGE as i64 + 48),
        ("black", 48),
        ("protected", 20 * PAGE as i64 + 96),
        ("reserve", 85),
        ("clamps", 1),
        ("grows", 1),
        ("shrinks", 1),
        ("full_window", 64),
        ("full_reserve", 0),
        ("old", 84),
        ("value", 99),
        ("id", 2),
        ("unmarked", 0),
    ] {
        assert_eq!(o.get(n), v, "{n}: {}", o.text());
    }
}
fn policy(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    capacity_policy_input(h.g, 256, 0, false);
    h.report_counter("first", Counter::CapacityRequested);
    capacity_policy_repeat(h.g);
    h.report_counter("reused", Counter::CapacityRequested);
    capacity_policy_input(h.g, 256, 0, false);
    h.report_counter("second", Counter::CapacityRequested);
    capacity_policy_input(h.g, 256, 20 << 20, false);
    h.report_counter("high1", Counter::CapacityRequested);
    capacity_policy_input(h.g, 256, 20 << 20, false);
    h.report_counter("high2", Counter::CapacityRequested);
    capacity_policy_input(h.g, 256, 0, false);
    capacity_policy_input(h.g, 256, 0, true);
    capacity_policy_input(h.g, 256, 0, false);
    h.report_counter("full_reset", Counter::CapacityRequested);
    request_young_pages(h.g, 64);
    for _ in 0..8 {
        capacity_policy_input(h.g, 64, 0, false);
    }
    h.report_counter("minimum", Counter::CapacityRequested);
    request_young_pages(h.g, 256);
    for _ in 0..8 {
        capacity_policy_input(h.g, 256, 20 << 20, false);
    }
    h.report_counter("maximum", Counter::CapacityRequested);
    h.report_counter("samples", Counter::CapacitySamples);
}
#[test]
fn production_young_capacity_policy_uses_fresh_observations_survival_hysteresis_and_bounds() {
    let o = run(policy, 0, false);
    for (n, v) in [
        ("first", 256),
        ("reused", 256),
        ("second", 224),
        ("high1", 224),
        ("high2", 256),
        ("full_reset", 256),
        ("minimum", 64),
        ("maximum", 256),
        ("samples", 22),
    ] {
        assert_eq!(o.get(n), v, "{n}: {}", o.text());
    }
}

fn budget(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::{capacity_admit, set_worker_alive};
    let mut h = H::begin(g);
    h.alloc(1, 65536, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    request_young_pages(h.g, 64);
    traced(&mut h);
    h.collect();
    // No OS thread: hold off the allocation entry's preliminary fallback
    // quantum, then explicitly service admission with the owner again.
    set_worker_alive(h.g, true);
    h.alloc(1, MAX_PAYLOAD - 64, 2);
    h.root(2);
    h.report_counter("remaining", Counter::CapacityFresh);
    h.report_counter("open_before", Counter::PhaseOpen);
    h.value(Src::Root(0)); // Owner copier binds a fresh TLAB despite quota 64.
    h.report_counter("after_copy", Counter::CapacityFresh);
    set_worker_alive(h.g, false);
    capacity_admit(h.g, 64); // Footprint 96; room in TLAB but not the cycle quota.
    h.report_counter("stalls", Counter::AllocationStalls);
    h.report_counter("open_after", Counter::PhaseOpen);
    h.alloc(1, 64, 3);
    h.set_scalar(Src::Raw(3), 0, 99);
    h.root(3);
    h.finish_collection();
    h.report("source", |h| h.load(Src::Root(0), 0));
    h.report("fresh", |h| h.load(Src::Root(3), 0));
}
#[test]
fn production_young_copy_reserve_bounds_fresh_bytes_even_after_a_load_barrier_binds_another_tlab() {
    let o = run(budget, 0, false);
    for (n, v) in [
        ("remaining", 64),
        ("open_before", 1),
        ("after_copy", 64),
        ("stalls", 1),
        ("open_after", 0),
        ("source", 84),
        ("fresh", 99),
    ] {
        assert_eq!(o.get(n), v, "{n}: {}", o.text());
    }
}

fn dense(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.repeat(70, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.set_scalar(Src::Raw(9), 0, 74);
        h.root(9);
    });
    request_young_pages(h.g, 64);
    traced(&mut h);
    h.collect();
    h.report_counter("active", Counter::CapacityActive);
    h.report_counter("reserve", Counter::CapacityReserve);
    h.report_counter("clamps", Counter::CapacityClamps);
    h.report_counter("protected", Counter::CapacitySealed);
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(9), 0));
    h.report("id", |h| h.object_id(Src::Root(9)));
}
#[test]
fn production_young_capacity_keeps_the_maximum_window_when_the_copy_bound_precludes_shrinking() {
    let o = run(dense, 0, false);
    for (n, v) in [
        ("active", 256),
        ("reserve", 256),
        ("clamps", 1),
        ("protected", 70 * PAGE as i64),
        ("value", 74),
        ("id", 70),
    ] {
        assert_eq!(o.get(n), v, "{n}: {}", o.text());
    }
}
fn invalid(g: &mut NativeCodeGenerator, pages: u64) {
    let mut h = H::begin(g);
    request_young_pages(h.g, pages);
    h.collect();
}
#[test]
fn production_young_capacity_rejects_out_of_range_requests_before_binding_pages() {
    for n in [0, 63, 257] {
        let o = super::run_failure(invalid, n);
        assert_eq!(o.status.code(), Some(1));
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}
