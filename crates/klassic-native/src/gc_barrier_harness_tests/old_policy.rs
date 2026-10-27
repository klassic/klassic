//! Feedback from actual copying, plus directed probes of production arithmetic.
use super::{H, Src, run};
use crate::gc_codegen::production::{
    MAX_PAYLOAD,
    test_support::{
        Counter, completed_mark_hint, emit_unbounded_budget, old_used, policy_target,
        request_old_cycle, request_young_pages, set_worker_alive, store_counter,
    },
};
use crate::portable_asm::{PortableAsm, Reg as R};
use crate::{NativeCodeGenerator, gc_abi::MarkRecord};
use std::mem::offset_of;
const M: u64 = 1 << 20;
fn young_complete(h: &mut H<'_>) {
    h.g.call_label(h.l.young_start);
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
}
fn ready(h: &mut H<'_>) {
    h.mark_start();
    h.drain_marks();
    young_complete(h);
    completed_mark_hint(h.g);
}
fn held_n(h: &mut H<'_>, count: u64) {
    // Keep later Young snapshots within the genuine Old promotion reserve.
    // The feedback tests isolate policy from capacity-driven emergency Full.
    request_young_pages(h.g, 64);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.repeat(count, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.root(9);
    });
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    ready(h);
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    ready(h); // A new completed Old snapshot retaining the same live set.
}
fn held(h: &mut H<'_>) {
    held_n(h, 129);
}
fn report(h: &mut H<'_>) {
    for (name, c) in [
        ("allocated", Counter::PolicyAllocated),
        ("retained", Counter::PolicyRetained),
        ("ema", Counter::PolicyEMA),
        ("target", Counter::PolicyTarget),
        ("base", Counter::PolicyBase),
        ("budget", Counter::PolicyBudget),
        ("full_samples", Counter::PolicyFull),
        ("young_samples", Counter::PolicyYoung),
        ("updates", Counter::PolicyUpdates),
        ("deferred", Counter::PolicyDeferred),
        ("periodic", Counter::PolicyPeriodic),
        ("pressure", Counter::PolicyPressure),
        ("plans", Counter::OldPlannedCycles),
        ("active", Counter::MarkActive),
        ("drains", Counter::MarkDrainSlices),
        ("unmarked", Counter::Unmarked),
        ("young_unmarked", Counter::YoungUnmarked),
    ] {
        h.report_counter(name, c);
    }
    h.report("used", |h| old_used(h.g));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
fn control(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    held(&mut h);
    h.finish_collection();
    h.finish_collection();
    h.report_counter("target_before", Counter::PolicyTarget);
    h.report_counter("updates_before", Counter::PolicyUpdates);
    h.report_counter("ema_before", Counter::PolicyEMA);
    h.report_counter("base_before", Counter::PolicyBase);
    h.report_counter("allocation_before", Counter::PolicyAllocated);
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 5);
    h.set_scalar(Src::Raw(5), 0, 99);
    h.root(5);
    h.report_counter("allocation_after_fresh", Counter::PolicyAllocated);
    set_worker_alive(h.g, false);
    h.collect();
    h.finish_collection(); // No Old growth: a real EMA/deadband sample.
    report(&mut h);
    h.report("fresh", |h| h.load(Src::Root(5), 0));
}
#[test]
fn production_old_feedback_defers_repeat_full_for_retained_heap_and_smooths_empty_young() {
    let o = run(control, 0, false);
    let live = 129 * 131072 + 48;
    assert_eq!(o.get("retained"), live);
    assert_eq!(o.get("used"), live);
    assert_eq!(o.get("budget"), live * 2);
    assert_eq!(
        o.get("ema_before"),
        (live / 4).min((32 * M as i64 - live) / 4)
    );
    assert_eq!(o.get("ema"), o.get("ema_before") * 3 / 4);
    assert!(o.get("target") > live && o.get("target") <= 32 * M as i64);
    assert_eq!(o.get("target"), o.get("target_before")); // <1 MiB candidate change.
    assert_eq!(o.get("updates"), o.get("updates_before"));
    assert_eq!(o.get("base"), o.get("base_before"));
    assert_eq!(o.get("allocation_before"), live);
    assert_eq!(o.get("base_before"), o.get("allocation_before"));
    assert_eq!(o.get("allocation_after_fresh"), live + 48);
    assert_eq!(o.get("allocated"), o.get("allocation_after_fresh"));
    assert!(o.get("deferred") > 0, "{}", o.text());
    assert_eq!(o.get("full_samples"), 1);
    assert_eq!(o.get("young_samples"), 3);
    assert_eq!(o.get("plans"), 1);
    assert_eq!(o.get("active"), 1);
    for k in [
        "periodic",
        "pressure",
        "drains",
        "unmarked",
        "young_unmarked",
    ] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("fresh"), 99);
    assert_eq!(o.get("id"), 1);
}
fn periodic(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    held(&mut h);
    // Allocate real bytes, with the deterministic worker disabled until all
    // frontiers are manually complete. No counter or object header is forged.
    h.repeat(300, |h| h.alloc(1, 120000, 9));
    h.finish_collection();
    h.repeat(129, |h| h.pop_root());
    young_complete(&mut h);
    completed_mark_hint(h.g);
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 5);
    h.set_scalar(Src::Raw(5), 0, 99);
    set_worker_alive(h.g, false);
    h.finish_collection();
    report(&mut h);
    h.report("fresh", |h| h.load(Src::Raw(5), 0));
    h.report("stamp", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch))
    });
}
#[test]
fn production_post_full_allocation_credit_revisits_unchanged_old_occupancy_and_reclaims_dead_roots()
{
    let o = run(periodic, 0, false);
    assert_eq!(o.get("periodic"), 1, "{}", o.text());
    assert_eq!(o.get("pressure"), 0);
    assert_eq!(o.get("full_samples"), 2);
    assert_eq!(o.get("plans"), 2);
    assert_eq!(o.get("retained"), 48);
    assert_eq!(o.get("used"), 48);
    assert_eq!(o.get("budget"), 32 * M as i64);
    assert!((16 * M as i64..=32 * M as i64).contains(&o.get("target")));
    for k in ["active", "drains", "unmarked", "young_unmarked"] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("fresh"), 99);
    assert_eq!(o.get("id"), 1);
    assert_eq!(o.get("stamp"), 2);
}
fn pressure(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    held(&mut h);
    h.report_counter("before", Counter::PolicyTarget);
    h.report_counter("ema_before", Counter::PolicyEMA);
    // Two reservable promotion batches keep this a policy-growth sample.
    for count in [32, 88] {
        h.repeat(count, |h| {
            h.alloc(1, MAX_PAYLOAD, 9);
            h.root(9);
        });
        for _ in 0..2 {
            h.collect();
            h.finish_collection();
        }
    }
    report(&mut h);
}
#[test]
fn production_observed_promotion_growth_advances_the_old_headroom_target() {
    let o = run(pressure, 0, false);
    assert!(o.get("target") < o.get("before"), "{}", o.text());
    assert!(o.get("ema") > o.get("ema_before"));
    assert!((16 * M as i64..=32 * M as i64).contains(&o.get("target")));
    assert_eq!(o.get("full_samples"), 1);
    assert_eq!(o.get("young_samples"), 6);
    assert_eq!(o.get("used"), o.get("retained") + 15 * M as i64);
    for k in ["drains", "unmarked", "young_unmarked"] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
}
fn arithmetic(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    for (name, live, ema, previous) in [
        ("mid", 8 * M, 0, 0),
        ("max", 64 * M, 0, 0),
        ("min", 24 * M, 16 * M, 0),
        ("quiet", 9 * M, 0, 20 * M),
        ("rise", 12 * M, 0, 20 * M),
        ("fall", 8 * M, 8 * M, 24 * M),
    ] {
        policy_target(h.g, live, ema, previous);
        h.report_counter(name, Counter::PolicyTarget);
    }
}
#[test]
fn production_old_feedback_bounds_and_deadband_use_the_production_emitter() {
    let o = run(arithmetic, 0, false);
    for (name, n) in [
        ("mid", 20),
        ("max", 32),
        ("min", 16),
        ("quiet", 20),
        ("rise", 22),
        ("fall", 16),
    ] {
        assert_eq!(o.get(name), n * M as i64, "{name}: {}", o.text());
    }
}

fn wrapped(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    held(&mut h);
    h.repeat(150, |h| h.alloc(1, 120000, 9));
    // Smaller Young windows may leave ordinary relocation/frontiers open.
    // Complete that real work before presenting a completed worker hint.
    h.finish_collection();
    h.drain_marks();
    completed_mark_hint(h.g);
    // A controller window from before rollover; actual heap and allocation
    // measurements remain real. It must rebase, then resume periodic work.
    h.g.mov_imm64(R::V0, u64::MAX);
    store_counter(h.g, Counter::PolicyBase, R::V0);
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 5);
    h.report_counter("rebased", Counter::PolicyBase);
    h.report_counter("plans_before", Counter::OldPlannedCycles);
    set_worker_alive(h.g, false);
    h.repeat(300, |h| h.alloc(1, 120000, 9));
    h.finish_collection();
    h.drain_marks();
    young_complete(&mut h);
    completed_mark_hint(h.g);
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 5);
    h.set_scalar(Src::Raw(5), 0, 99);
    set_worker_alive(h.g, false);
    h.finish_collection();
    report(&mut h);
    h.report("fresh", |h| h.load(Src::Raw(5), 0));
}
#[test]
fn production_old_allocation_counter_wrap_rebases_then_periodic_collection_resumes() {
    let o = run(wrapped, 0, false);
    assert!(
        o.get("rebased") > 0 && o.get("rebased") < 100 * M as i64,
        "{}",
        o.text()
    );
    assert_eq!(o.get("plans_before"), 1);
    assert_eq!(o.get("periodic"), 1);
    assert_eq!(o.get("plans"), 2);
    assert_eq!(o.get("full_samples"), 2);
    assert!(o.get("deferred") > 0);
    for k in ["drains", "unmarked", "young_unmarked"] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("fresh"), 99);
}

fn bootstrap(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    for (n, c) in [
        ("target", Counter::PolicyTarget),
        ("budget", Counter::PolicyBudget),
        ("full", Counter::PolicyFull),
        ("young", Counter::PolicyYoung),
        ("updates", Counter::PolicyUpdates),
        ("retained", Counter::PolicyRetained),
        ("ema", Counter::PolicyEMA),
    ] {
        h.report_counter(n, c);
    }
    h.alloc(1, 0, 0);
    h.report_counter("empty_footprint", Counter::PolicyAllocated);
    h.alloc(1, 17, 1);
    h.report_counter("aligned_footprints", Counter::PolicyAllocated);
    h.collect();
    h.finish_collection();
    h.report_counter("after_collection", Counter::PolicyAllocated);
}
#[test]
fn production_old_policy_bootstraps_effective_limits_without_claiming_observations() {
    let o = run(bootstrap, 0, false);
    assert_eq!(o.get("target"), 16 * M as i64);
    assert_eq!(o.get("budget"), 32 * M as i64);
    assert_eq!(o.get("empty_footprint"), 48);
    assert_eq!(o.get("aligned_footprints"), 112);
    assert_eq!(o.get("after_collection"), 112);
    for k in ["full", "young", "updates", "retained", "ema"] {
        assert_eq!(o.get(k), 0);
    }
}

fn high_retention(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    held_n(&mut h, 200);
    h.report_counter("target_before", Counter::PolicyTarget);
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 5);
    h.set_scalar(Src::Raw(5), 0, 99);
    set_worker_alive(h.g, false);
    h.finish_collection();
    report(&mut h);
    h.report("fresh", |h| h.load(Src::Raw(5), 0));
}
#[test]
fn production_full_retention_rebounds_prediction_and_avoids_repeat_full_loop() {
    let o = run(high_retention, 0, false);
    assert!(o.get("target_before") > o.get("retained"), "{}", o.text());
    assert_eq!(o.get("retained"), 200 * 131072 + 48);
    assert_eq!(o.get("full_samples"), 1);
    assert_eq!(o.get("plans"), 1);
    assert_eq!(o.get("active"), 1);
    assert!(o.get("deferred") > 0);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("fresh"), 99);
}
