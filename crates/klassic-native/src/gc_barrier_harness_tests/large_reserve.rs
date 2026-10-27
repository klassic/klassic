//! Shared-pool admission with real Large allocation and directed free runs.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::HEAP_PAGE_BYTES as P;
use crate::gc_codegen::production::{large_object as large, test_support::*};
use crate::portable_asm::{PortableAsm, Reg as R};

fn metric(h: &mut H<'_>, name: &str, index: usize) {
    h.report(name, |h| {
        h.g.load_ptr_disp32(R::V0, R::V3, large::report_offsets()[index])
    });
}
fn big(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 256 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    h.alloc(1, 16, 3);
    h.set_scalar(Src::Raw(3), 0, 84);
    h.root(3);
    // One span, then two independent spans, each live footprint above the
    // former aggregate Old budget (682 pages at this hard capacity).
    for pair in [false, true] {
        let size = if pair { 48 << 20 } else { 96 << 20 };
        h.alloc(1, size, 0);
        h.set_scalar(Src::Raw(0), (size / 8 - 1) as u32, 99);
        h.root(0);
        h.report(&format!("address{pair}"), |h| h.value(Src::Root(0)));
        if pair {
            h.alloc(1, size, 1);
            h.set_scalar(Src::Raw(1), (size / 8 - 1) as u32, 100);
            h.root(1);
        }
        h.report(&format!("old_capacity{pair}"), |h| {
            pool_metric(h.g, PoolMetric::OldCapacity)
        });
        metric(&mut h, &format!("live{pair}"), 3);
        metric(&mut h, &format!("retries{pair}"), 2);
        for cycle in 0..3 {
            request_old_cycle(h.g);
            h.collect();
            h.finish_collection();
            h.report(&format!("value{pair}_{cycle}"), |h| {
                h.load(Src::Root(0), (size / 8 - 1) as u32)
            });
            h.report(&format!("stable{pair}_{cycle}"), |h| h.value(Src::Root(0)));
            h.report(&format!("small{pair}_{cycle}"), |h| h.load(Src::Root(3), 0));
            if pair {
                h.report(&format!("other{cycle}"), |h| {
                    h.load(Src::Root(1), (size / 8 - 1) as u32)
                });
            }
        }
        h.value(Src::Null);
        h.set_root(0);
        if pair {
            h.value(Src::Null);
            h.set_root(1);
        }
        request_old_cycle(h.g);
        h.collect();
        h.finish_collection();
        metric(&mut h, &format!("dead{pair}"), 3);
    }
    metric(&mut h, "reclaimed", 4);
    metric(&mut h, "retries", 2);
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_large_shared_reserve_allows_single_and_aggregate_spans_above_the_old_budget() {
    for worker in [false, true] {
        let o = run(big, 0, worker);
        assert_eq!(o.get("livefalse"), 769);
        assert_eq!(o.get("livetrue"), 770);
        for pair in [false, true] {
            assert_eq!(
                o.get(&format!("old_capacity{pair}")),
                if pair { 171 } else { 682 }
            );
            assert_eq!(o.get(&format!("dead{pair}")), 0);
            for cycle in 0..3 {
                assert_eq!(o.get(&format!("value{pair}_{cycle}")), 99);
                assert_eq!(o.get(&format!("small{pair}_{cycle}")), 84);
                assert_eq!(
                    o.get(&format!("stable{pair}_{cycle}")),
                    o.get(&format!("address{pair}"))
                );
            }
        }
        for c in 0..3 {
            assert_eq!(o.get(&format!("other{c}")), 100);
        }
        assert_eq!(o.get("reclaimed"), 1539);
        assert_eq!(o.get("retriesfalse"), 0);
        // After rotating moving spaces, a fragmented candidate may require
        // the existing one-Full retry before the pair can be admitted.
        assert!((0..=1).contains(&o.get("retries")), "{}", o.text());
        assert_eq!(o.get("unmarked"), 0);
    }
}

fn probe(g: &mut NativeCodeGenerator, case: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    match case {
        3 => {
            for p in [83, 104, 125] {
                pool_claim_at(h.g, p);
            }
        } // no Young-sized run
        4 => {
            for p in [83, 105] {
                pool_claim_at(h.g, p);
            }
        } // second-largest saves Young
        _ => (),
    }
    let pages = match case {
        0 => 3,
        1 => 43,
        2 => 44,
        5 => 66,
        _ => 3,
    };
    h.report("unused", |h| old_unused_pages(h.g));
    h.report("before", |h| pool_metric(h.g, PoolMetric::Claims));
    h.report("admitted", |h| large_reserve_probe(h.g, pages));
    h.report("after", |h| pool_metric(h.g, PoolMetric::Claims));
    h.report("old_capacity", |h| {
        pool_metric(h.g, PoolMetric::OldCapacity)
    });
    h.report("role", |h| pool_role(h.g, if case == 4 { 106 } else { 63 }));
}
#[test]
fn production_large_shared_reserve_protects_aggregate_capacity_and_contiguous_young_destinations() {
    for case in 0..6 {
        let o = run(probe, case, false);
        let admitted = i64::from(matches!(case, 0 | 1 | 4));
        assert_eq!(o.get("unused"), 41, "case {case}: {}", o.text());
        assert_eq!(o.get("admitted"), admitted, "case {case}: {}", o.text());
        assert_eq!(o.get("after") - o.get("before"), admitted);
        assert_eq!(o.get("role"), if admitted == 1 { 3 } else { 0 });
        assert_eq!(o.get("old_capacity"), 42);
    }
}

fn populated(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for slot in 0..2 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("used", |h| old_used(h.g));
    h.report("unused", |h| old_unused_pages(h.g));
    h.report("before", |h| pool_metric(h.g, PoolMetric::Claims));
    h.report("admitted", |h| large_reserve_probe(h.g, 43));
    h.report("after", |h| pool_metric(h.g, PoolMetric::Claims));
    for slot in 0..2 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
    }
}
#[test]
fn production_large_shared_reserve_counts_only_unpopulated_old_tail_pages() {
    let o = run(populated, 0, false);
    assert_eq!(o.get("used"), 2 * P as i64);
    assert_eq!(o.get("unused"), 10);
    assert_eq!(o.get("admitted"), 0);
    assert_eq!(o.get("before"), o.get("after"));
    assert_eq!(o.get("v0"), 84);
    assert_eq!(o.get("v1"), 85);
}

fn headroom(g: &mut NativeCodeGenerator, count: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for slot in 1..=count as u32 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    h.alloc(1, 2 << 20, 0);
    h.set_scalar(Src::Raw(0), 0, 99);
    h.root(0);
    h.mark_start();
    young_trace_complete(h.g, &h.l);
    h.report_counter("fresh_full_before", Counter::FullCollections);
    // First survival ages fresh ordinary objects to one. Only the next
    // snapshot can require normal promotion space.
    h.collect();
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    h.report_counter("fresh_full_after", Counter::FullCollections);
    h.report_counter("active_before", Counter::MarkActive);
    h.report_counter("full_before", Counter::FullCollections);
    h.collect();
    h.finish_collection();
    h.report_counter("full_after", Counter::FullCollections);
    h.report_counter("active_after", Counter::MarkActive);
    h.report("large", |h| h.load(Src::Root(0), 0));
    for slot in 1..=count as u32 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
    }
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_large_shared_headroom_selects_full_before_promotion_consumes_future_destinations() {
    for worker in [false, true] {
        for count in [5, 6] {
            let o = run(headroom, count, worker);
            assert_eq!(
                o.get("fresh_full_after"),
                o.get("fresh_full_before"),
                "{}",
                o.text()
            );
            assert_eq!(o.get("active_before"), 1, "{}", o.text());
            assert_eq!(
                o.get("full_after") - o.get("full_before"),
                i64::from(count == 6),
                "{}",
                o.text()
            );
            if count == 6 {
                assert_eq!(o.get("active_after"), 0);
            }
            assert_eq!(o.get("large"), 99);
            for slot in 1..=count {
                assert_eq!(o.get(&format!("v{slot}")), 84 + slot as i64);
            }
            assert_eq!(o.get("unmarked"), 0);
        }
    }
}

// Predicate-only states: independent empty reservations shape the pool;
// directed Old-capacity inputs are never followed by allocation/collection.
fn headroom_probe(g: &mut NativeCodeGenerator, case: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    h.alloc(1, 3 * P + 16, 0); // Four physical Large pages.
    h.root(0);
    if case == 1 {
        for page in [87, 108] {
            pool_claim_at(h.g, page);
        }
    }
    h.report("before", |h| pool_metric(h.g, PoolMetric::Claims));
    h.report("full", |h| {
        pool_preflight(
            h.g,
            if case >= 2 { 30 - case } else { 42 },
            0,
            Some(if case >= 2 { 6 * P } else { 0 }),
        )
    });
    h.report("after", |h| pool_metric(h.g, PoolMetric::Claims));
}
#[test]
fn production_large_shared_headroom_requires_an_actual_contiguous_young_destination() {
    for case in [0, 1] {
        let o = run(headroom_probe, case, false);
        assert_eq!(o.get("full"), case as i64, "{}", o.text());
        assert_eq!(o.get("before"), o.get("after"));
    }
}
#[test]
fn production_large_shared_headroom_does_not_cap_old_promotion_at_the_young_window() {
    // Six protected pages require 28 Old pages, above Young's 21-page cap.
    // Aggregate capacity and contiguous Young space both suffice here.
    for case in [2, 3] {
        let o = run(headroom_probe, case, false);
        assert_eq!(o.get("full"), i64::from(case == 3), "{}", o.text());
        assert_eq!(o.get("before"), o.get("after"));
    }
}

fn twelve_old(g: &mut NativeCodeGenerator) -> H<'_> {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = P;
    let mut h = H::begin(g);
    for slot in 0..12 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    h.drain_marks();
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("used", |h| old_used(h.g));
    h.report("capacity", |h| pool_metric(h.g, PoolMetric::OldCapacity));
    h
}
fn larger_full_reserve(g: &mut NativeCodeGenerator, pages: u64) {
    let mut h = twelve_old(g);
    h.report("young_claimed", |h| young_allocated_pages(h.g));
    h.report("before", |h| pool_metric(h.g, PoolMetric::Claims));
    h.report("admitted", |h| large_reserve_probe(h.g, pages));
    h.report("after", |h| pool_metric(h.g, PoolMetric::Claims));
    for slot in 0..12 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
    }
}
#[test]
fn production_large_shared_reserve_preserves_full_demand_above_bootstrap_at_exact_fit() {
    for pages in [22, 23] {
        let o = run(larger_full_reserve, pages, false);
        assert_eq!(o.get("used"), 12 * P as i64, "{}", o.text());
        assert_eq!(o.get("capacity"), 52, "{}", o.text());
        // Idle returned the unused owner reservation: there is no Young source.
        assert_eq!(o.get("young_claimed"), 0, "{}", o.text());
        // 48 Old + 4 boundary pages, plus the next 21-page Young run.
        // 95 reclaimable/free pages minus 22 Large pages is the exact fit.
        let admitted = i64::from(pages == 22);
        assert_eq!(o.get("admitted"), admitted, "{}", o.text());
        assert_eq!(o.get("after") - o.get("before"), admitted, "{}", o.text());
        for slot in 0..12 {
            assert_eq!(o.get(&format!("v{slot}")), 84 + slot, "{}", o.text());
        }
    }
}
fn larger_headroom(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = twelve_old(g);
    h.alloc(1, 2 * P + 16, 12); // Three stable Large pages.
    h.set_scalar(Src::Raw(12), 0, 99);
    h.root(12);
    h.report("large_before", |h| h.value(Src::Root(12)));
    for slot in 13..17 {
        h.alloc(1, P - 32, slot);
        h.set_scalar(Src::Raw(slot), 0, 84 + u64::from(slot));
        h.root(slot);
    }
    h.finish_collection();
    young_trace_complete(h.g, &h.l);
    h.report_counter("before", Counter::FullCollections);
    h.collect();
    h.finish_collection();
    h.report_counter("after", Counter::FullCollections);
    for slot in 0..17 {
        h.report(&format!("v{slot}"), |h| h.load(Src::Root(slot), 0));
    }
    h.report("large_after", |h| h.value(Src::Root(12)));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_large_shared_headroom_preserves_the_uncapped_next_full_destination() {
    for worker in [false, true] {
        let o = run(larger_headroom, 0, worker);
        assert_eq!(o.get("after") - o.get("before"), 1, "{}", o.text());
        assert_eq!(o.get("large_before"), o.get("large_after"), "{}", o.text());
        for slot in 0..17 {
            assert_eq!(
                o.get(&format!("v{slot}")),
                if slot == 12 { 99 } else { 84 + slot },
                "{}",
                o.text()
            );
        }
        assert_eq!(o.get("unmarked"), 0, "{}", o.text());
    }
}
