//! Full budgets follow actual source occupancy, including protected Young.
use super::{H, Src, run};
use crate::gc_codegen::production::test_support::*;
use crate::{NativeCodeGenerator, gc_abi::HEAP_PAGE_BYTES as P};
fn capacity(h: &mut H<'_>, name: &str) {
    h.report(name, |h| pool_metric(h.g, PoolMetric::OldCapacity));
}
fn full(h: &mut H<'_>) {
    // Ordinary allocation can open relocation through Old's completion poll,
    // even when this harness disables automatic Young marking. A new Young
    // snapshot requires the preceding moving phase and retirement to finish.
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    // Planned requests are rechecked; drain Old work before requesting Full.
    h.drain_marks();
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
}
fn adaptive(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    h.alloc(1, 3 * P + 16, 0);
    h.set_scalar(Src::Raw(0), 0, 99);
    h.root(0);
    h.report("large_before", |h| h.value(Src::Root(0)));
    for slot in 1..3 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    full(&mut h);
    capacity(&mut h, "small");
    for slot in 3..7 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    full(&mut h);
    capacity(&mut h, "grown");
    for slot in 1..7 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
    }
    for _ in 0..6 {
        h.pop_root();
    }
    full(&mut h);
    full(&mut h); // Occupancy is an upper bound; the next source is empty.
    capacity(&mut h, "empty");
    h.report("large_after", |h| h.value(Src::Root(0)));
    h.report("large_value", |h| h.load(Src::Root(0), 0));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_full_capacity_grows_for_old_and_young_sources_and_shrinks_after_death() {
    for worker in [false, true] {
        let o = run(adaptive, 0, worker);
        assert_eq!(o.get("small"), 12, "{}", o.text());
        assert_eq!(o.get("grown"), 28, "{}", o.text());
        assert_eq!(o.get("empty"), 11, "{}", o.text());
        for slot in 1..7 {
            assert_eq!(o.get(&format!("v{slot}")), 84 + slot);
        }
        assert_eq!(o.get("large_value"), 99);
        assert_eq!(o.get("large_before"), o.get("large_after"));
        assert_eq!(o.get("unmarked"), 0);
    }
}
fn maximum(g: &mut NativeCodeGenerator, pending: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for slot in 0..11 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    if pending != 0 {
        h.collect(); // Exercise entry with an explicitly open moving phase.
    }
    full(&mut h);
    capacity(&mut h, "cap");
    for slot in 0..11 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
    }
}
#[test]
fn production_full_capacity_exceeds_the_bootstrap_partition_for_source_demand() {
    for worker in [false, true] {
        for pending in [0, 1] {
            let o = run(maximum, pending, worker);
            assert_eq!(o.get("cap"), 48, "{}", o.text());
            for slot in 0..11 {
                assert_eq!(o.get(&format!("v{slot}")), 84 + slot);
            }
        }
    }
}

fn unsealed(g: &mut NativeCodeGenerator, soft: u64) {
    g.gc_max_heap = 16 << 20;
    if soft != 0 {
        g.gc_soft_max_heap = Some(P);
    }
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    // Harness automatic Young tracing is disabled; no sealed measurement.
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    capacity(&mut h, "cap");
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_full_capacity_uses_claimed_source_pages_without_a_sealed_measurement() {
    for worker in [false, true] {
        for soft in [0, 1] {
            let o = run(unsealed, soft, worker);
            assert_eq!(o.get("cap"), if soft == 0 { 11 } else { 8 }, "{}", o.text());
            assert_eq!(o.get("value"), 84);
        }
    }
}

// Build the live set in real Young batches, then move more ordinary Old pages
// than the bootstrap partition can hold. A small advisory Young window leaves
// room for both physical source and destination copies on each repeated Full.
fn above_bootstrap(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 32 << 20;
    g.gc_soft_max_heap = Some(8 << 20);
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for batch in 0..9 {
        for slot in batch * 10..batch * 10 + 10 {
            // Slots 40/41 are harness scratch and loop storage.
            let slot = slot + 50;
            h.alloc(1, P - 32, slot);
            h.set_scalar(Src::Raw(slot), 0, 1000 + u64::from(slot));
            h.root(slot);
        }
        h.finish_collection();
        young_trace_complete(h.g, &h.l);
        h.drain_marks();
        request_old_cycle(h.g);
        h.collect();
        h.finish_collection();
    }
    capacity(&mut h, "above");
    h.report("used", |h| old_used(h.g));
    for cycle in 0..3 {
        full(&mut h);
        h.report(&format!("used{cycle}"), |h| old_used(h.g));
        for slot in 50..140 {
            h.report(&format!("v{cycle}_{slot}"), |h| h.load(Src::Root(slot), 0));
            h.report(&format!("old{cycle}_{slot}"), |h| h.is_old(Src::Root(slot)));
            h.report(&format!("id{cycle}_{slot}"), |h| {
                h.object_id(Src::Root(slot))
            });
        }
    }
    for _ in 0..90 {
        h.pop_root();
    }
    full(&mut h);
    full(&mut h);
    capacity(&mut h, "empty");
    h.report("empty_used", |h| old_used(h.g));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_full_capacity_preserves_live_old_above_bootstrap_across_repeated_moves() {
    for worker in [false, true] {
        let o = run(above_bootstrap, 0, worker);
        assert!(o.get("above") > 85, "{}", o.text());
        assert_eq!(o.get("used"), 90 * P as i64, "{}", o.text());
        for cycle in 0..3 {
            assert_eq!(
                o.get(&format!("used{cycle}")),
                90 * P as i64,
                "{}",
                o.text()
            );
            for slot in 50..140 {
                assert_eq!(
                    o.get(&format!("v{cycle}_{slot}")),
                    1000 + slot,
                    "{}",
                    o.text()
                );
                assert_eq!(o.get(&format!("old{cycle}_{slot}")), 1, "{}", o.text());
                assert_eq!(
                    o.get(&format!("id{cycle}_{slot}")),
                    slot - 49,
                    "{}",
                    o.text()
                );
            }
        }
        assert_eq!(o.get("empty"), 6, "{}", o.text());
        assert_eq!(o.get("empty_used"), 0, "{}", o.text());
        assert_eq!(o.get("unmarked"), 0, "{}", o.text());
    }
}

fn fragmented_window(g: &mut NativeCodeGenerator, stride: u64) {
    g.gc_max_heap = 32 << 20;
    g.gc_soft_max_heap = Some(8 << 20);
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 99);
    h.root(0);
    fragment_free_runs(h.g, stride as usize);
    request_young_pages(h.g, 42);
    full(&mut h);
    h.report_counter("active", Counter::CapacityActive);
    h.report_counter("requested", Counter::CapacityRequested);
    capacity(&mut h, "old_capacity");
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.alloc(1, 16, 1);
    h.root(1);
    h.set_scalar(Src::Root(1), 0, 84);
    h.report("fresh", |h| h.load(Src::Root(1), 0));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_full_capacity_fits_fresh_young_window_to_fragmented_free_runs() {
    for worker in [false, true] {
        let o = run(fragmented_window, 21, worker);
        for (name, value) in [
            ("active", 20),
            ("requested", 42),
            ("old_capacity", 6),
            ("value", 99),
            ("id", 1),
            ("fresh", 84),
            ("unmarked", 0),
        ] {
            assert_eq!(o.get(name), value, "{name}: {}", o.text());
        }
    }
}
#[test]
fn production_full_capacity_rejects_fragmentation_below_the_fresh_window_minimum() {
    let o = super::run_failure(fragmented_window, 10);
    assert_eq!(o.status.code(), Some(1));
    assert_eq!(o.stderr, crate::gc_codegen::production::OOM_TEXT);
}

fn old_headroom(g: &mut NativeCodeGenerator, fresh: u64) {
    g.gc_max_heap = 32 << 20;
    g.gc_soft_max_heap = Some(8 << 20);
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    // Forty actual Old pages. Use slots outside the harness loop scratch.
    for batch in 0..4 {
        for slot in (50 + batch * 10)..(60 + batch * 10) {
            h.alloc(1, P - 32, slot);
            h.set_scalar(Src::Raw(slot), 0, 1000 + u64::from(slot));
            h.root(slot);
        }
        full(&mut h);
    }
    for slot in 90..(90 + fresh as u32) {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 1000 + u64::from(slot));
        h.root(slot);
    }
    request_young_pages(h.g, 42);
    full(&mut h);
    h.report_counter("active", Counter::CapacityActive);
    h.report_counter("requested", Counter::CapacityRequested);
    capacity(&mut h, "old_capacity");
    for slot in 50..(90 + fresh as u32) {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
        h.report(&format!("id{slot}"), |h| h.object_id(Src::Root(slot)));
    }
    // No requested Old cycle and no Large objects. A concurrent copier can
    // leave a fitting Free run, so first reserve independent empty separators
    // without shrinking Old. Establish that the requested normal Young run
    // is missing; Full must then reclaim unused Old tails before claiming it.
    h.report("free_before", |h| largest_free_run(h.g));
    fragment_free_runs_retaining_old(h.g, 42);
    h.report("free_shaped", |h| largest_free_run(h.g));
    capacity(&mut h, "old_capacity_shaped");
    h.report_counter("full_before_next", Counter::FullCollections);
    young_trace_complete(h.g, &h.l);
    h.collect();
    h.finish_collection();
    h.report_counter("full_after_next", Counter::FullCollections);
    for slot in 50..(90 + fresh as u32) {
        h.report(&format!("next_v{slot}"), |h| h.load(Src::Root(slot), 0));
        h.report(&format!("next_id{slot}"), |h| h.object_id(Src::Root(slot)));
    }
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_full_capacity_yields_fresh_window_space_to_live_old_demand() {
    for worker in [false, true] {
        for fresh in [0, 8] {
            let o = run(old_headroom, fresh, worker);
            assert_eq!(
                o.get("active"),
                if fresh == 0 { 42 } else { 10 },
                "{}",
                o.text()
            );
            assert_eq!(o.get("requested"), 42);
            assert!(o.get("free_shaped") < 42, "{}", o.text());
            assert_eq!(o.get("old_capacity_shaped"), o.get("old_capacity"));
            assert_eq!(
                o.get("old_capacity"),
                if fresh == 0 { 164 } else { 196 },
                "{}",
                o.text()
            );
            for slot in 50..(90 + fresh) {
                assert_eq!(o.get(&format!("v{slot}")), 1000 + slot as i64);
                assert_eq!(o.get(&format!("id{slot}")), slot as i64 - 49);
            }
            assert!(
                o.get("full_after_next") > o.get("full_before_next"),
                "{}",
                o.text()
            );
            for slot in 50..(90 + fresh) {
                assert_eq!(o.get(&format!("next_v{slot}")), 1000 + slot as i64);
                assert_eq!(o.get(&format!("next_id{slot}")), slot as i64 - 49);
            }
            assert_eq!(o.get("unmarked"), 0);
        }
    }
}
