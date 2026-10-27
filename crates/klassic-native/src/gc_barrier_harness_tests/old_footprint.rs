//! Snapshot sizing follows logical objects rather than source reservations.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::HEAP_PAGE_BYTES as P;
use crate::gc_codegen::production::test_support::*;

fn report(h: &mut H<'_>, name: &str) {
    h.report(name, |h| old_protected_bytes(h.g));
}
fn full(h: &mut H<'_>) {
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
}
fn dead_old(g: &mut NativeCodeGenerator, marked: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for slot in 0..6 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    // No sealed Young measurement: first establish ordinary Old occupancy.
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    for _ in 0..5 {
        h.pop_root();
    }
    h.alloc(1, 3 * P + 16, 1);
    h.set_scalar(Src::Raw(1), 0, 99);
    h.root(1);
    h.report("large_before", |h| h.value(Src::Root(1)));
    if marked != 0 {
        h.mark_start();
        h.drain_marks();
        h.end_marks();
    }
    report(&mut h, "protected");
    h.report("occupied", |h| old_used(h.g));
    full(&mut h);
    h.report("capacity", |h| pool_metric(h.g, PoolMetric::OldCapacity));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("large_after", |h| h.value(Src::Root(1)));
    h.report("large_value", |h| h.load(Src::Root(1), 0));
}
#[test]
fn production_old_footprint_discards_dead_old_occupancy_on_the_first_marked_full() {
    for worker in [false, true] {
        for marked in [0, 1] {
            let o = run(dead_old, marked, worker);
            assert_eq!(o.get("occupied"), (6 * P) as i64, "{}", o.text());
            assert_eq!(o.get("protected"), (marked * P) as i64, "{}", o.text());
            assert_eq!(
                o.get("capacity"),
                if marked == 0 { 28 } else { 11 },
                "{}",
                o.text()
            );
            assert_eq!(o.get("value"), 84);
            assert_eq!(o.get("large_value"), 99);
            assert_eq!(o.get("large_before"), o.get("large_after"));
        }
    }
}

fn graph(h: &mut H<'_>) {
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 32, 40);
    h.set_scalar(Src::Raw(40), 0, 85);
    h.store(Src::Root(0), 0, Src::Raw(40), true);
    h.alloc(1, 0, 1);
    h.root(1);
}
fn logical(g: &mut NativeCodeGenerator, copier: u64) {
    let mut h = H::begin(g);
    graph(&mut h);
    h.mark_start();
    // The unread B is retained only by deletion SATB, then counted once even
    // though the root and field discovery paths revisit the other objects.
    h.store(Src::Root(0), 0, Src::Root(1), true);
    h.drain_marks();
    report(&mut h, "snapshot");
    h.alloc(1, 24, 2);
    h.set_scalar(Src::Raw(2), 0, 86);
    h.root(2);
    h.alloc(1, 3 * P + 16, 3);
    h.set_scalar(Src::Raw(3), 0, 99);
    h.root(3);
    report(&mut h, "fresh");
    for name in ["copied", "promoted"] {
        h.collect();
        h.relocate_quantum(128, copier != 0);
        h.finish_collection();
        h.drain_marks();
        report(&mut h, name);
    }
    h.report("old", |h| h.is_old(Src::Root(2)));
    h.report("value", |h| h.load(Src::Root(2), 0));
    h.report("large", |h| h.load(Src::Root(3), 0));
}
#[test]
fn production_old_footprint_counts_satb_and_fresh_once_across_both_young_copiers_and_promotion() {
    for copier in [0, 1] {
        let o = run(logical, copier, false);
        assert_eq!(o.get("snapshot"), 160, "{}", o.text());
        for name in ["fresh", "copied", "promoted"] {
            assert_eq!(o.get(name), 224, "{name}: {}", o.text());
        }
        assert_eq!(o.get("old"), 1);
        assert_eq!(o.get("value"), 86);
        assert_eq!(o.get("large"), 99);
    }
}

fn saturation(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    let mut h = H::begin(g);
    graph(&mut h);
    h.mark_start();
    seed_old_protection(h.g, u64::MAX - 16, 48);
    report(&mut h, "overflow_sum");
    seed_old_protection(h.g, u64::MAX - 16, u64::MAX - 16);
    h.store(Src::Root(0), 0, Src::Root(1), true);
    h.alloc(1, 24, 2);
    h.root(2);
    h.alloc(1, 24, 3); // Already saturated: clamp even without u64 overflow.
    h.root(3);
    h.report("trace", |h| old_protection_counter(h.g, false));
    h.report("black", |h| old_protection_counter(h.g, true));
    report(&mut h, "combined");
    h.drain_marks();
    h.end_marks();
    full(&mut h);
    h.mark_start();
    h.report("reset_trace", |h| old_protection_counter(h.g, false));
    h.report("reset_black", |h| old_protection_counter(h.g, true));
}
#[test]
fn production_old_footprint_saturates_each_actor_and_combination_and_resets_at_new_snapshot() {
    let o = run(saturation, 0, false);
    for name in ["overflow_sum", "trace", "black", "combined"] {
        assert_eq!(o.get(name), ((16 << 20) + P) as i64, "{}", o.text());
    }
    assert_eq!(o.get("reset_trace"), 0);
    assert_eq!(o.get("reset_black"), 0);
}

fn joint_census(g: &mut NativeCodeGenerator, after_close: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for slot in 0..6 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    for _ in 0..5 {
        h.pop_root();
    }
    if after_close != 0 {
        h.mark_start();
        h.drain_marks();
        h.end_marks();
        report(&mut h, "closed_before_fresh");
    }
    for slot in 1..3 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    if after_close == 0 {
        h.mark_start();
        h.drain_marks();
        // A drained but still open snapshot must not tighten the whole set.
        h.report_counter("active", Counter::MarkActive);
        h.report_counter("valid_before_close", Counter::MarkValid);
        h.report("open_demand", |h| full_destination_pages(h.g));
        h.end_marks();
    }
    report(&mut h, "all_moving");
    h.report("old_occupied", |h| old_used(h.g));
    h.report("closed_demand", |h| full_destination_pages(h.g));
    // Dedicated Large allocation after closure must not increase moving demand.
    h.alloc(1, 3 * P + 16, 3);
    h.set_scalar(Src::Raw(3), 0, 99);
    h.root(3);
    h.report("large_before", |h| h.value(Src::Root(3)));
    report(&mut h, "after_large");
    h.report("large_demand", |h| full_destination_pages(h.g));
    full(&mut h);
    h.report("capacity", |h| pool_metric(h.g, PoolMetric::OldCapacity));
    for slot in 0..3 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
        h.report(&format!("id{slot}"), |h| h.object_id(Src::Root(slot)));
        h.report(&format!("old{slot}"), |h| h.is_old(Src::Root(slot)));
    }
    h.report("large_after", |h| h.value(Src::Root(3)));
    h.report("large_value", |h| h.load(Src::Root(3), 0));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_full_capacity_uses_a_closed_whole_census_without_counting_young_twice() {
    for worker in [false, true] {
        for after_close in [0, 1] {
            let o = run(joint_census, after_close, worker);
            if after_close == 0 {
                assert_eq!(o.get("active"), 1);
                assert_eq!(o.get("valid_before_close"), 0);
                assert_eq!(o.get("open_demand"), 36, "{}", o.text());
            } else {
                assert_eq!(o.get("closed_before_fresh"), P as i64);
            }
            assert_eq!(o.get("old_occupied"), (6 * P) as i64);
            for name in ["all_moving", "after_large"] {
                assert_eq!(o.get(name), (3 * P) as i64, "{}", o.text());
            }
            for name in ["closed_demand", "large_demand", "capacity"] {
                assert_eq!(o.get(name), 16, "{name}: {}", o.text());
            }
            for slot in 0..3 {
                assert_eq!(o.get(&format!("v{slot}")), 84 + slot);
                assert_eq!(
                    o.get(&format!("id{slot}")),
                    if slot == 0 { 1 } else { 6 + slot }
                );
                assert_eq!(o.get(&format!("old{slot}")), 1);
            }
            assert_eq!(o.get("large_before"), o.get("large_after"));
            assert_eq!(o.get("large_value"), 99);
            assert_eq!(o.get("unmarked"), 0);
        }
    }
}

fn closed_growth(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for slot in 0..6 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    for _ in 0..5 {
        h.pop_root();
    }
    h.mark_start();
    h.drain_marks();
    h.end_marks();
    report(&mut h, "closed");
    // These nineteen new identities were absent from the closed snapshot.
    // Without post-closure charging the whole-set bound would reserve only
    // eleven pages for twenty live one-page objects, causing a real Old OOM.
    for slot in 1..20 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    report(&mut h, "grown");
    h.report("demand", |h| full_destination_pages(h.g));
    full(&mut h);
    h.report("capacity", |h| pool_metric(h.g, PoolMetric::OldCapacity));
    for slot in 0..20 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
        h.report(&format!("id{slot}"), |h| h.object_id(Src::Root(slot)));
        h.report(&format!("old{slot}"), |h| h.is_old(Src::Root(slot)));
    }
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_closed_full_census_charges_later_fresh_objects_before_real_evacuation() {
    for worker in [false, true] {
        let o = run(closed_growth, 0, worker);
        assert_eq!(o.get("closed"), P as i64);
        assert_eq!(o.get("grown"), (20 * P) as i64, "{}", o.text());
        assert_eq!(o.get("demand"), 84, "{}", o.text());
        assert_eq!(o.get("capacity"), 80, "{}", o.text());
        for slot in 0..20 {
            assert_eq!(o.get(&format!("v{slot}")), 84 + slot);
            assert_eq!(
                o.get(&format!("id{slot}")),
                if slot == 0 { 1 } else { 6 + slot }
            );
            assert_eq!(o.get(&format!("old{slot}")), 1);
        }
        assert_eq!(o.get("unmarked"), 0);
    }
}

fn floating_census(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for slot in 0..6 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    h.mark_start();
    h.drain_marks();
    h.end_marks();
    for _ in 0..5 {
        h.pop_root();
    }
    young_trace_complete(h.g, &h.l);
    seal_young_trace(h.g);
    report(&mut h, "floating");
    h.report("demand", |h| full_destination_pages(h.g));
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("capacity", |h| pool_metric(h.g, PoolMetric::OldCapacity));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_full_capacity_keeps_the_smaller_source_bound_when_the_census_retains_garbage() {
    for worker in [false, true] {
        let o = run(floating_census, 0, worker);
        assert_eq!(o.get("floating"), (6 * P) as i64);
        assert_eq!(o.get("demand"), 11, "{}", o.text());
        assert_eq!(o.get("capacity"), 11, "{}", o.text());
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("id"), 1);
        assert_eq!(o.get("unmarked"), 0);
    }
}
