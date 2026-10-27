//! Old promotion needs its own uncapped bound even without Large objects.
use super::{H, Src, run};
use crate::gc_codegen::production::test_support::*;
use crate::{NativeCodeGenerator, gc_abi::HEAP_PAGE_BYTES as P};

fn boundary(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    for (i, (pages, used, protected)) in [
        (27, 0, 6 * P),
        (28, 0, 6 * P),
        (29, 0, 6 * P),
        (42, 15 * P, 6 * P),
        (42, 14 * P, 6 * P),
        (42, 14 * P + 16, 6 * P),
        (28, 0, 6 * P + 1),
        (29, 0, 6 * P + 1),
        (3, 0, 0),
        (4, 0, 0),
        (42, 43 * P, 0),
    ]
    .into_iter()
    .enumerate()
    {
        h.report(&format!("p{i}"), |h| {
            pool_preflight(h.g, pages, used, Some(protected))
        });
    }
}
#[test]
fn production_promotion_reserve_without_large_uses_uncapped_old_bound_and_exact_fit() {
    let o = run(boundary, 0, false);
    for (i, full) in [1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1].into_iter().enumerate() {
        assert_eq!(o.get(&format!("p{i}")), full, "{}", o.text());
    }
}

fn selection(g: &mut NativeCodeGenerator, old_pages: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    for slot in 0..old_pages as u32 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    // Planned Old requests are rechecked against an empty Old frontier.
    h.drain_marks();
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report_counter("setup_full", Counter::FullCollections);
    h.report("setup_old", |h| h.is_old(Src::Root(0)));
    h.report("old_capacity", |h| {
        pool_metric(h.g, PoolMetric::OldCapacity)
    });
    for slot in old_pages as u32..old_pages as u32 + 6 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    h.report_counter("fresh_full_before", Counter::FullCollections);
    h.report("fresh_mature", |h| young_mature_bytes(h.g, false));
    h.collect();
    h.finish_collection();
    h.report_counter("fresh_full_after", Counter::FullCollections);
    h.report("fresh_promoted", |h| h.is_old(Src::Root(old_pages as u32)));
    young_trace_complete(h.g, &h.l);
    h.report("mature", |h| young_mature_bytes(h.g, false));
    h.report_counter("full_before", Counter::FullCollections);
    h.collect();
    h.finish_collection();
    h.report_counter("full_after", Counter::FullCollections);
    h.report("promoted", |h| h.is_old(Src::Root(old_pages as u32)));
    for slot in 0..old_pages as u32 + 6 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
    }
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_promotion_reserve_without_large_selects_full_only_below_the_required_old_space() {
    for worker in [false, true] {
        for old_pages in [8, 7] {
            let o = run(selection, old_pages, worker);
            assert!(o.get("setup_full") >= 1, "{}", o.text());
            assert_eq!(o.get("setup_old"), 1, "{}", o.text());
            assert_eq!(
                o.get("old_capacity"),
                (4 * old_pages + 4) as i64,
                "{}",
                o.text()
            );
            let full = i64::from(old_pages == 7);
            assert_eq!(
                o.get("full_after") - o.get("full_before"),
                full,
                "{}",
                o.text()
            );
            assert_eq!(o.get("fresh_mature"), 0, "{}", o.text());
            assert_eq!(
                o.get("fresh_full_after"),
                o.get("fresh_full_before"),
                "{}",
                o.text()
            );
            assert_eq!(o.get("fresh_promoted"), 0, "{}", o.text());
            assert_eq!(o.get("mature"), 6 * P as i64, "{}", o.text());
            assert_eq!(o.get("promoted"), 1, "{}", o.text());
            for slot in 0..old_pages + 6 {
                assert_eq!(o.get(&format!("v{slot}")), 84 + slot as i64);
            }
            assert_eq!(o.get("unmarked"), 0);
        }
    }
}

fn accounting(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.store(Src::Raw(0), 0, Src::Raw(2), true);
    h.store(Src::Raw(0), 1, Src::Null, true);
    h.root(0);
    h.value(Src::Root(0));
    h.keep(1);
    h.root(1); // Same graph has two snapshot roots.
    young_trace_complete(h.g, &h.l);
    h.report("age0", |h| young_mature_bytes(h.g, false));
    h.collect();
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    h.report("age1", |h| young_mature_bytes(h.g, false));
    h.alloc(1, 0, 3); // Fresh black allocation contributes only to all-Young.
    h.root(3);
    h.report("with_black", |h| young_mature_bytes(h.g, false));
    h.collect();
    h.report("sealed", |h| young_mature_bytes(h.g, true));
    h.report_counter("all", Counter::CapacitySealed);
    h.report_counter("black", Counter::CapacityBlack);
    h.finish_collection();
    h.report("parent_old", |h| h.is_old(Src::Root(0)));
    h.report("black_young", |h| h.is_old(Src::Root(3)));
    young_trace_complete(h.g, &h.l);
    h.report("reset", |h| young_mature_bytes(h.g, false));
    h.collect();
    h.report("reset_sealed", |h| young_mature_bytes(h.g, true));
    h.finish_collection();
    h.report("black_old", |h| h.is_old(Src::Root(3)));
    young_trace_complete(h.g, &h.l);
    h.report("empty", |h| young_mature_bytes(h.g, false));
    h.collect();
    h.report("empty_sealed", |h| young_mature_bytes(h.g, true));
    h.finish_collection();
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_promotion_reserve_counts_mature_graph_once_excludes_black_and_resets_each_snapshot() {
    for worker in [false, true] {
        let o = run(accounting, 0, worker);
        for (name, expected) in [
            ("age0", 0),
            ("age1", 96),
            ("with_black", 96),
            ("sealed", 96),
            ("all", 144),
            ("black", 48),
            ("parent_old", 1),
            ("black_young", 0),
            ("reset", 48),
            ("reset_sealed", 48),
            ("black_old", 1),
            ("empty", 0),
            ("empty_sealed", 0),
            ("unmarked", 0),
        ] {
            assert_eq!(o.get(name), expected, "{name}: {}", o.text());
        }
    }
}

fn unsealed_bound(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    for (i, (bounds, old_pages, used)) in [
        ([10, 11, 31, 31, 21], 7, 0),
        ([10, 11, 31, 31, 21], 8, 0),
        ([10, 16, 31, 31, 21], 27, 0),
        ([10, 16, 31, 31, 21], 28, 0),
        ([50, 51, 66, 71, 21], 28, 0), // One owner + five worker pages.
        ([50, 51, 66, 71, 21], 42, 14 * P + 16),
        ([50, 51, 53, 58, 8], 28, 0), // Same six pages in a smaller window.
        ([10, 31, 31, 31, 21], 42, 0), // Full window needs 88 pages.
        ([10, 10, 31, 31, 21], 4, 0), // Empty numerical source: boundaries only.
    ]
    .into_iter()
    .enumerate()
    {
        seed_young_source_pages(h.g, bounds);
        h.report(&format!("claimed{i}"), |h| young_allocated_pages(h.g));
        h.report(&format!("full{i}"), |h| {
            pool_preflight(h.g, old_pages, used, None)
        });
    }
}
#[test]
fn production_unsealed_promotion_reserve_counts_both_source_streams_and_accepts_exact_fit() {
    let o = run(unsealed_bound, 0, false);
    for (i, (claimed, full)) in [
        (1, 1),
        (1, 0),
        (6, 1),
        (6, 0),
        (6, 0),
        (6, 1),
        (6, 0),
        (21, 1),
        (0, 0),
    ]
    .into_iter()
    .enumerate()
    {
        assert_eq!(o.get(&format!("claimed{i}")), claimed, "{}", o.text());
        assert_eq!(o.get(&format!("full{i}")), full, "{}", o.text());
    }
}
fn invalid_source(g: &mut NativeCodeGenerator, case: u64) {
    let mut h = H::begin(g);
    let bounds = [
        [12, 11, 30, 31, 19], // Owner below base.
        [10, 20, 19, 31, 21], // Streams overlap.
        [10, 11, 32, 31, 21], // Worker above end.
        [10, 11, 31, 31, 20], // Active span disagrees.
    ][case as usize];
    seed_young_source_pages(h.g, bounds);
    h.report("pages", |h| young_allocated_pages(h.g));
}
#[test]
fn production_unsealed_promotion_reserve_rejects_inconsistent_source_geometry() {
    for case in 0..4 {
        let o = super::run_failure(invalid_source, case);
        assert_eq!(o.status.code(), Some(1));
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}

fn unsealed_collection(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    for slot in 0..7 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 200 + u64::from(slot));
        h.root(slot);
    }
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    h.drain_marks();
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report_counter("setup_full", Counter::FullCollections);
    h.report("setup_old", |h| h.is_old(Src::Root(0)));
    for slot in 7..13 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 200 + u64::from(slot));
        h.root(slot);
    }
    h.finish_collection();
    // Fresh allocation cannot promote; move it once without a census first.
    h.collect();
    h.finish_collection();
    // No new census: the six prior Young winners are now possible candidates.
    h.report("owner_pages", |h| young_source_stream_pages(h.g, false));
    h.report("worker_pages", |h| young_source_stream_pages(h.g, true));
    h.report_counter("owner_lost", Counter::CopyOwnerLost);
    h.report_counter("worker_lost", Counter::CopyWorkerLost);
    h.report_counter("valid", Counter::YoungValid);
    h.report("claimed", |h| young_allocated_pages(h.g));
    h.report_counter("before", Counter::FullCollections);
    h.collect();
    h.finish_collection();
    h.report_counter("after", Counter::FullCollections);
    for slot in 0..13 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
    }
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_unsealed_promotion_reserve_selects_full_for_real_six_page_source() {
    for worker in [false, true] {
        let o = run(unsealed_collection, 0, worker);
        assert!(o.get("setup_full") >= 1, "{}", o.text());
        assert_eq!(o.get("setup_old"), 1, "{}", o.text());
        assert_eq!(o.get("valid"), 0, "{}", o.text());
        assert_eq!(o.get("claimed"), 6, "{}", o.text());
        assert_eq!(o.get("after") - o.get("before"), 1, "{}", o.text());
        for slot in 0..13 {
            assert_eq!(o.get(&format!("v{slot}")), 200 + slot, "{}", o.text());
        }
        assert_eq!(o.get("unmarked"), 0, "{}", o.text());
    }
}

fn worker_source(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    h.alloc(1, P - 32, 0);
    h.root(0);
    h.value(Src::Root(0));
    h.keep(2);
    h.collect();
    h.worker_copy(Src::Raw(2));
    h.finish_collection();
    h.alloc(1, P - 32, 1);
    h.root(1);
    h.finish_collection();
    h.report_counter("valid", Counter::YoungValid);
    h.report_counter("worker_copies", Counter::AgeWorkerYoung);
    h.report("owner", |h| young_source_stream_pages(h.g, false));
    h.report("worker", |h| young_source_stream_pages(h.g, true));
    h.report("claimed", |h| young_allocated_pages(h.g));
    h.report("below", |h| pool_preflight(h.g, 7, 0, None));
    h.report("exact", |h| pool_preflight(h.g, 8, 0, None));
}
#[test]
fn production_unsealed_promotion_reserve_includes_real_worker_copy_pages() {
    let o = run(worker_source, 0, false);
    assert_eq!(o.get("valid"), 0, "{}", o.text());
    assert_eq!(o.get("worker_copies"), P as i64, "{}", o.text());
    assert_eq!(o.get("owner"), 1, "{}", o.text());
    assert_eq!(o.get("worker"), 1, "{}", o.text());
    assert_eq!(o.get("claimed"), 2, "{}", o.text());
    assert_eq!(o.get("below"), 1, "{}", o.text());
    assert_eq!(o.get("exact"), 0, "{}", o.text());
}

fn prior_copy_bound(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    for (i, (bytes, room)) in [(0, 4), (P, 7), (P, 8), (P + 16, 8), (P + 16, 9)]
        .into_iter()
        .enumerate()
    {
        seed_young_source_pages(h.g, [10, 12, 31, 31, 21]);
        seed_previous_young_copies(h.g, bytes);
        h.report(&format!("p{i}"), |h| pool_preflight(h.g, room, 0, None));
    }
}
#[test]
fn production_unsealed_promotion_reserve_bounds_prior_winners_and_rounds_partial_pages_up() {
    let o = run(prior_copy_bound, 0, false);
    for (i, expected) in [0, 1, 0, 1, 0].into_iter().enumerate() {
        assert_eq!(o.get(&format!("p{i}")), expected, "{}", o.text());
    }
}
fn impossible_census(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    seed_young_source_pages(h.g, [10, 11, 31, 31, 21]);
    seed_previous_young_copies(h.g, P + 16);
    h.report("full", |h| pool_preflight(h.g, 42, 0, None));
}
#[test]
fn production_unsealed_promotion_reserve_rejects_census_larger_than_claimed_source() {
    let o = super::run_failure(impossible_census, 0);
    assert_eq!(o.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
}

fn empty_reservation(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.collect();
    h.report("during", |h| current_tlab_bound(h.g));
    h.finish_collection();
    h.report("after", |h| current_tlab_bound(h.g));
    h.report("empty_pages", |h| young_allocated_pages(h.g));
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.report("rebound", |h| current_tlab_bound(h.g));
    h.report("new_pages", |h| young_allocated_pages(h.g));
    h.finish_collection();
    h.report("occupied", |h| current_tlab_bound(h.g));
    h.collect();
    h.finish_collection();
    h.report("live_pages", |h| young_allocated_pages(h.g));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_idle_returns_only_unused_owner_reservation_and_rebinds_for_fresh_allocation() {
    for worker in [false, true] {
        let o = run(empty_reservation, 0, worker);
        assert_ne!(o.get("during"), 0, "{}", o.text());
        assert_eq!(o.get("after"), 0, "{}", o.text());
        assert_eq!(o.get("empty_pages"), 0, "{}", o.text());
        assert_ne!(o.get("rebound"), 0, "{}", o.text());
        assert_ne!(o.get("occupied"), 0, "{}", o.text());
        assert_eq!(o.get("new_pages"), 1, "{}", o.text());
        assert_eq!(o.get("live_pages"), 1, "{}", o.text());
        assert_eq!(o.get("value"), 84, "{}", o.text());
    }
}
