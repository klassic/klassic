//! Normal planned Old cycles, with actual occupancy and stale-frontier checks.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::{Generation, MarkRecord, Phase, PhaseSnapshot};
use crate::gc_codegen::production::{
    MAX_PAYLOAD,
    test_support::{
        Counter, completed_mark_hint, emit_unbounded_budget, old_used, request_old_cycle,
        set_worker_alive, set_young_auto, store_counter, wake_worker,
    },
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;
const SLOTS: u32 = 8192;

fn graph(h: &mut H<'_>) {
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.root(0);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.store(Src::Root(0), SLOTS - 1, Src::Raw(2), true);
}
fn seed(h: &mut H<'_>) {
    graph(h);
    // Authentic large raw allocations and RootCells, not fabricated Old top.
    h.repeat(129, |h| {
        h.alloc(1, MAX_PAYLOAD, 9);
        h.root(9);
    });
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.repeat(129, |h| h.pop_root());
}
fn young_complete(h: &mut H<'_>) {
    h.g.call_label(h.l.young_start);
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
}
fn hint(h: &mut H<'_>) {
    completed_mark_hint(h.g);
    set_worker_alive(h.g, true);
}
fn observe(h: &mut H<'_>) {
    for (name, counter) in [
        ("request", Counter::OldRequest),
        ("checks", Counter::OldCycleChecks),
        ("retries", Counter::OldCycleRetries),
        ("planned", Counter::OldPlannedCycles),
        ("active", Counter::MarkActive),
        ("epoch", Counter::MarkCycles),
        ("full", Counter::FullCollections),
        ("collections", Counter::Collections),
        ("drains", Counter::MarkDrains),
        ("slices", Counter::MarkDrainSlices),
        ("unmarked", Counter::Unmarked),
        ("young_unmarked", Counter::YoungUnmarked),
    ] {
        h.report_counter(name, counter);
    }
}
fn completed(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    seed(&mut h);
    h.report("before_bytes", |h| old_used(h.g));
    h.mark_start();
    h.drain_marks();
    young_complete(&mut h); // Deterministic fake worker: complete its lane first.
    hint(&mut h);
    h.alloc(1, 16, 5); // Normal allocation poll chooses real full relocation.
    h.set_scalar(Src::Raw(5), 0, 99);
    set_worker_alive(h.g, false);
    h.finish_collection();
    observe(&mut h);
    h.report("after_bytes", |h| old_used(h.g));
    h.report("phase", |h| h.counter(Counter::AdoptedPhase));
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report("old_word", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch))
    });
    h.report("young_word", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch))
    });
    h.value(Src::Root(0));
    h.keep(3);
    h.mark_start();
    h.report_counter("next_epoch", Counter::MarkCycles);
    h.report("stale", |h| h.marked(Src::Raw(3)));
    h.report("fresh", |h| h.load(Src::Raw(5), 0));
}
#[test]
fn production_worker_completed_old_snapshot_selects_full_move_before_capacity_exhaustion() {
    let o = run(completed, 0, false);
    assert!(o.get("before_bytes") >= 16 << 20);
    assert!(o.get("before_bytes") < 32 << 20);
    assert!(o.get("after_bytes") < 128 << 10);
    for k in [
        "request",
        "retries",
        "active",
        "drains",
        "slices",
        "unmarked",
        "young_unmarked",
        "young_word",
        "stale",
    ] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
    for k in ["checks", "planned", "epoch", "full", "id", "old_word"] {
        assert_eq!(o.get(k), 1, "{k}: {}", o.text());
    }
    assert_eq!(o.get("collections"), 3);
    assert_eq!(o.get("next_epoch"), 2);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("fresh"), 99);
    let p = PhaseSnapshot::from_bits(o.get("phase") as u64).unwrap();
    for g in [Generation::Young, Generation::Old] {
        assert_eq!(p.phase(g), Phase::Idle);
    }
}

fn stale(g: &mut NativeCodeGenerator, kind: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    seed(&mut h);
    if kind == 1 {
        h.alloc(2, u64::from(SLOTS) * 8, 7);
    } else {
        h.alloc(1, 16, 7);
        h.set_scalar(Src::Raw(7), 0, 99);
    }
    h.mark_start();
    if kind != 0 {
        h.drain_marks();
        h.root(7); // Post-bound root: all captured roots are already finished.
        if kind == 1 {
            h.mark_quantum(1); // Queue consumed, retained receiver fields pending.
        }
    }
    for (name, c) in [
        ("root", Counter::MarkRootIndex),
        ("field", Counter::MarkField),
        ("object", Counter::MarkObject),
        ("queue", Counter::MarkStackLen),
    ] {
        h.report_counter(&format!("before_{name}"), c);
    }
    hint(&mut h);
    h.alloc(1, 16, 5);
    h.set_scalar(Src::Raw(5), 0, 101);
    observe(&mut h);
    for (name, c) in [
        ("root", Counter::MarkRootIndex),
        ("field", Counter::MarkField),
        ("object", Counter::MarkObject),
        ("queue", Counter::MarkStackLen),
    ] {
        h.report_counter(name, c);
    }
    set_worker_alive(h.g, false);
    h.drain_marks();
    h.report("fresh", |h| h.load(Src::Raw(5), 0));
}
#[test]
fn production_planned_old_cycle_rearms_root_receiver_and_queue_hints_without_owner_tracing() {
    for kind in 0..3 {
        let o = run(stale, kind, false);
        for k in [
            "request",
            "planned",
            "full",
            "drains",
            "slices",
            "unmarked",
            "young_unmarked",
        ] {
            assert_eq!(o.get(k), 0, "{kind}/{k}: {}", o.text());
        }
        for k in ["checks", "retries", "active", "epoch"] {
            assert_eq!(o.get(k), 1, "{kind}/{k}: {}", o.text());
        }
        assert_eq!(o.get("collections"), 2);
        for k in ["root", "field", "object", "queue"] {
            assert_eq!(
                o.get(k),
                o.get(&format!("before_{k}")),
                "{kind}/{k}: {}",
                o.text()
            );
        }
        assert_eq!(o.get("fresh"), 101);
    }
}
fn late(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    seed(&mut h);
    h.alloc(1, 16, 7);
    h.set_scalar(Src::Raw(7), 0, 99);
    h.mark_start();
    h.drain_marks();
    young_complete(&mut h);
    request_old_cycle(h.g);
    h.root(7); // Adds Old work after a once-valid planned request.
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop); // Explicit fake worker completes Young only.
    h.g.call_label(h.l.unlock);
    h.collect();
    h.finish_collection();
    observe(&mut h);
    h.report_counter("queue", Counter::MarkStackLen);
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
    h.report("late", |h| h.load(Src::Root(7), 0));
}
#[test]
fn production_old_request_rechecks_late_work_and_preserves_old_snapshot_on_young_move() {
    let o = run(late, 0, false);
    for k in [
        "request",
        "checks",
        "planned",
        "full",
        "drains",
        "slices",
        "unmarked",
        "young_unmarked",
    ] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
    for k in ["retries", "active", "epoch", "queue"] {
        assert_eq!(o.get(k), 1, "{k}: {}", o.text());
    }
    assert_eq!(o.get("collections"), 3);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("late"), 99);
}

fn ordinary(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    seed(&mut h);
    // Earlier ordinary Young scheduling is allowed during seeding; compare
    // against the actual completed prefix, not an assumed OS interleaving.
    for (counter, slot) in [
        (Counter::Collections, 20),
        (Counter::FullCollections, 21),
        (Counter::OldPlannedCycles, 22),
    ] {
        h.counter(counter);
        h.keep(slot);
    }
    h.report("before_bytes", |h| old_used(h.g));
    let active = h.g.create_text_label();
    h.counter(Counter::MarkActive);
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::NotEqual, active);
    h.mark_start();
    h.g.bind_text_label(active);
    wake_worker(h.g, h.l.unpark);
    let ready = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::MarkDone);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, ready);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(ready);
    h.report_counter("ready", Counter::MarkDone);
    h.counter(Counter::MarkCycles);
    h.keep(23);
    set_young_auto(h.g, true); // Missing Young snapshot must be worker-created.
    let collected = h.g.create_text_label();
    h.repeat(1024, |h| {
        h.alloc(1, 120000, 5);
        h.set_scalar(Src::Raw(5), 0, 99);
        h.counter(Counter::FullCollections);
        h.g.load_rbp_slot(R::V1, super::slot_offset(21));
        h.g.cmp_reg_reg(R::V0, R::V1);
        h.g.jcc_label(C::Above, collected);
    });
    h.g.bind_text_label(collected);
    h.finish_collection();
    for (name, counter, slot) in [
        ("collections_delta", Counter::Collections, 20),
        ("full_delta", Counter::FullCollections, 21),
        ("planned_delta", Counter::OldPlannedCycles, 22),
    ] {
        h.report(name, |h| {
            h.counter(counter);
            h.g.load_rbp_slot(R::V1, super::slot_offset(slot));
            h.g.sub_reg_reg(R::V0, R::V1);
        });
    }
    h.report("after_bytes", |h| old_used(h.g));
    h.report_counter("closed", Counter::MarkActive);
    h.report_counter("drains", Counter::MarkDrainSlices);
    h.report_counter("young_units", Counter::YoungWorkerUnits);
    h.report_counter("young_epoch", Counter::YoungEpoch);
    h.report_counter("young_unmarked", Counter::YoungUnmarked);
    h.report_counter("unmarked", Counter::Unmarked);
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
    h.root(5); // Retain the fresh value across allocations in the next cycle.
    let restarted = h.g.create_text_label();
    h.repeat(1024, |h| {
        h.alloc(1, 120000, 9);
        h.counter(Counter::MarkCycles);
        h.g.load_rbp_slot(R::V1, super::slot_offset(23));
        h.g.cmp_reg_reg(R::V0, R::V1);
        h.g.jcc_label(C::Above, restarted);
    });
    h.g.bind_text_label(restarted);
    h.finish_collection();
    h.report("epoch_delta", |h| {
        h.counter(Counter::MarkCycles);
        h.g.load_rbp_slot(R::V1, super::slot_offset(23));
        h.g.sub_reg_reg(R::V0, R::V1);
    });
    h.report_counter("active", Counter::MarkActive);
    h.report("fresh", |h| h.load(Src::Root(5), 0));
}
#[test]
fn production_ordinary_old_cycle_reclaims_before_exhaustion_and_starts_a_new_epoch() {
    let o = run(ordinary, 0, true);
    assert!(
        o.get("before_bytes") >= 16 << 20 && o.get("before_bytes") < 32 << 20,
        "{}",
        o.text()
    );
    assert!(o.get("after_bytes") < 128 << 10, "{}", o.text());
    for k in [
        "ready",
        "full_delta",
        "planned_delta",
        "id",
        "epoch_delta",
        "active",
    ] {
        assert_eq!(o.get(k), 1, "{k}: {}", o.text());
    }
    assert!(o.get("collections_delta") >= 1, "{}", o.text());
    for k in ["closed", "drains", "young_unmarked", "unmarked"] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
    assert!(
        o.get("young_units") > 0 && o.get("young_epoch") > 0,
        "{}",
        o.text()
    );
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("fresh"), 99);
}

fn young_ready_old_busy(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    seed(&mut h);
    h.mark_start(); // All captured Old roots are still pending.
    young_complete(&mut h); // Young skips the Old graph and finishes independently.
    h.repeat(64, |h| h.alloc(1, 120000, 9));
    h.g.mov_imm64(R::V0, 1);
    store_counter(h.g, Counter::YoungDone, R::V0);
    hint(&mut h);
    h.alloc(1, 16, 5);
    set_worker_alive(h.g, false);
    h.finish_collection();
    observe(&mut h);
    h.report_counter("old_root", Counter::MarkRootIndex);
    h.report_counter("young_active", Counter::YoungActive);
    h.report_counter("young_epoch", Counter::YoungEpoch);
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
}
#[test]
fn production_rejected_old_plan_still_allows_ready_young_collection() {
    let o = run(young_ready_old_busy, 0, false);
    for k in [
        "request",
        "planned",
        "full",
        "drains",
        "slices",
        "unmarked",
        "young_unmarked",
        "young_active",
    ] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
    for k in [
        "checks",
        "retries",
        "active",
        "epoch",
        "old_root",
        "young_epoch",
    ] {
        assert_eq!(o.get(k), 1, "{k}: {}", o.text());
    }
    assert_eq!(o.get("collections"), 3);
    assert_eq!(o.get("value"), 84);
}

fn below_threshold(g: &mut NativeCodeGenerator, _arg: u64) {
    // Keep the original 192 MiB policy with headroom for conservative promotion.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    graph(&mut h);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.mark_start();
    h.drain_marks();
    young_complete(&mut h);
    hint(&mut h);
    h.alloc(1, 16, 5);
    h.set_scalar(Src::Raw(5), 0, 99);
    observe(&mut h);
    h.report("bytes", |h| old_used(h.g));
    h.report("fresh", |h| h.load(Src::Raw(5), 0));
}
#[test]
fn production_old_completion_below_occupancy_threshold_preserves_independent_young_mark() {
    let o = run(below_threshold, 0, false);
    assert!(o.get("bytes") > 0 && o.get("bytes") < 16 << 20);
    for k in [
        "request",
        "checks",
        "retries",
        "planned",
        "full",
        "drains",
        "slices",
        "unmarked",
        "young_unmarked",
    ] {
        assert_eq!(o.get(k), 0, "{k}: {}", o.text());
    }
    assert_eq!(o.get("active"), 1);
    assert_eq!(o.get("epoch"), 1);
    assert_eq!(o.get("collections"), 2);
    assert_eq!(o.get("fresh"), 99);
}
