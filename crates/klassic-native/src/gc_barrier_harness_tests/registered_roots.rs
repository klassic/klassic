//! Real registered storage retained through independent budgeted frontiers.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
use RootRegistryMetric as M;

fn metric(h: &mut H<'_>, name: &str, metric: M) {
    h.report(name, |h| root_registry_metric(h.g, metric));
}
fn lifecycle(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    for (name, m) in [
        ("active", M::Active),
        ("registered", M::Registered),
        ("scope", M::Scope),
        ("initial", M::Leases),
    ] {
        metric(&mut h, name, m);
    }
    h.report("linked", |h| root_registry_links(h.g));
    for i in 0..8 {
        h.alloc(1, 24, i);
        h.set_scalar(Src::Raw(i), 0, 84 + u64::from(i));
        h.root(i);
    }
    h.mark_start();
    metric(&mut h, "old", M::Old);
    metric(&mut h, "old_leases", M::Leases);
    h.g.call_label(h.l.young_start);
    metric(&mut h, "young", M::Young);
    metric(&mut h, "both", M::Leases);
    metric(&mut h, "phase", M::Phase);
    h.report_counter("adopted", Counter::AdoptedPhase);
    h.mark_quantum(0);
    h.g.call_label(h.l.lock);
    emit_budget(h.g, 0);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    metric(&mut h, "yielded", M::Leases);
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    metric(&mut h, "young_done", M::Young);
    metric(&mut h, "old_retained", M::Old);
    metric(&mut h, "one_left", M::Leases);
    h.drain_marks();
    metric(&mut h, "mark_done", M::Leases);
    h.collect();
    metric(&mut h, "reloc", M::Relocation);
    metric(&mut h, "reloc_leases", M::Leases);
    h.relocate_quantum(0, false);
    metric(&mut h, "reloc_yielded", M::Leases);
    h.relocate_quantum(u64::MAX, true);
    metric(&mut h, "reloc_done", M::Relocation);
    metric(&mut h, "all_done", M::Leases);
    h.finish_collection();
    h.report("first", |h| h.load(Src::Root(0), 0));
    h.report("last", |h| h.load(Src::Root(7), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
    metric(&mut h, "final_phase", M::Phase);
    h.report_counter("final_adopted", Counter::AdoptedPhase);
    metric(&mut h, "final_active", M::Active);
}
#[test]
fn production_registered_roots_keep_independent_frontier_leases_until_budgeted_completion() {
    // OS scheduling may legally finish discovery before an intermediate
    // observation. Exact budget states use the emitted failed-spawn path.
    {
        let o = run(lifecycle, 0, false);
        for name in [
            "active",
            "registered",
            "scope",
            "linked",
            "old",
            "old_leases",
            "young",
            "old_retained",
            "one_left",
            "reloc",
            "reloc_leases",
            "reloc_yielded",
            "final_active",
        ] {
            assert_eq!(o.get(name), 1, "{}", o.text());
        }
        for name in ["both", "yielded"] {
            assert_eq!(o.get(name), 2, "{}", o.text());
        }
        for name in [
            "initial",
            "young_done",
            "mark_done",
            "reloc_done",
            "all_done",
        ] {
            assert_eq!(o.get(name), 0, "{}", o.text());
        }
        assert_eq!(o.get("phase"), o.get("adopted"));
        assert_eq!(o.get("final_phase"), o.get("final_adopted"));
        assert_eq!(o.get("first"), 84);
        assert_eq!(o.get("last"), 91);
        assert_eq!(o.get("id"), 1);
    }
}
fn invalid(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    match mode {
        0 => root_registry_set(h.g, M::Registered, 0),
        1 => root_registry_set(h.g, M::Scope, 2),
        2 => root_registry_set(h.g, M::LastLease, u64::MAX),
        3 => {
            h.mark_start();
            root_registry_set(h.g, M::Scope, 2);
            h.drain_marks();
            return;
        }
        _ => unreachable!(),
    }
    h.mark_start();
}
#[test]
fn production_registered_roots_reject_unregistered_stale_and_exhausted_storage() {
    for mode in 0..4 {
        let output = run_failure(invalid, mode);
        assert!(String::from_utf8_lossy(&output.stderr).contains("runtime invariant"));
    }
}

fn repeated(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    for _ in 0..12 {
        h.mark_start();
        h.drain_marks();
        h.end_marks();
        h.collect();
        h.relocate_quantum(u64::MAX, false);
        h.finish_collection();
    }
    metric(&mut h, "leases", M::Leases);
    metric(&mut h, "serial", M::LastLease);
    h.report("journal_entries", |h| root_journal_claimed_entries(h.g));
    metric(&mut h, "active", M::Active);
    h.report("linked", |h| root_registry_links(h.g));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
}
#[test]
fn production_registered_roots_reuse_lease_controls_without_leaking_or_reusing_serials() {
    for worker in [false, true] {
        let o = run(repeated, 0, worker);
        assert_eq!(o.get("leases"), 0, "{}", o.text());
        assert_eq!(
            o.get("serial"),
            24 + o.get("journal_entries"),
            "{}",
            o.text()
        );
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("linked"), 1);
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("id"), 1);
    }
}

fn empty(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    for i in 0..12 {
        h.mark_start();
        // No graph, frame roots or mark_loop call: end's metadata-only path.
        h.end_marks();
        metric(&mut h, &format!("empty{i}"), M::Leases);
        h.collect();
        h.finish_collection();
    }
    metric(&mut h, "leases", M::Leases);
    metric(&mut h, "serial", M::LastLease);
    h.report("journal_entries", |h| root_journal_claimed_entries(h.g));
    metric(&mut h, "active", M::Active);
    metric(&mut h, "old", M::Old);
}
#[test]
fn production_registered_roots_release_empty_snapshots_that_skip_discovery() {
    for worker in [false, true] {
        let o = run(empty, 0, worker);
        assert_eq!(o.get("leases"), 0, "{}", o.text());
        assert_eq!(o.get("old"), 0);
        assert_eq!(o.get("serial"), 24 + o.get("journal_entries"));
        for i in 0..12 {
            assert_eq!(o.get(&format!("empty{i}")), 0, "{}", o.text());
        }
        assert_eq!(o.get("active"), 1);
    }
}

fn inventory(g: &mut NativeCodeGenerator, _: u64) {
    use crate::portable_asm::Reg as R;
    let mut h = H::begin(g);
    let mut cells = Vec::new();
    for i in 0..8 {
        h.alloc(1, 24, 0);
        h.set_scalar(Src::Raw(0), 0, 101 + i);
        h.root(0);
        // The raw harness slot is now the real frame RootCell pointer.
        h.g.load_rbp_slot(R::V0, super::slot_offset(0));
        cells.push(root_registry_add_region(h.g, false));
        h.pop_root();
    }
    // These empty registrations precede every live region in inventory order.
    for _ in 0..8 {
        root_registry_add_region(h.g, true);
    }
    metric(&mut h, "regions", M::Active);
    h.mark_start();
    h.mark_quantum(0);
    metric(&mut h, "zero_serial", M::LastLease);
    h.report("initial_journal_entries", |h| {
        root_journal_claimed_entries(h.g)
    });
    h.report("initial_journal_regions", |h| journal_regions_visited(h.g));
    h.mark_quantum(1);
    metric(&mut h, "one_serial", M::LastLease);
    metric(&mut h, "old_still_live", M::Old);
    // Its first region is exhausted, but seven empty regions and eight live
    // ones remain. A local index/bound check must not finish either lane.
    h.g.call_label(h.l.young_start);
    h.g.call_label(h.l.lock);
    emit_budget(h.g, 1);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    metric(&mut h, "young_still_live", M::Young);
    metric(&mut h, "two_walkers", M::Leases);
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    // Old remains on an exhausted empty region. mark_end must drain the
    // remaining inventory rather than trusting that local index/bound.
    h.end_marks();
    metric(&mut h, "marks_released", M::Leases);
    h.report_counter("marked", Counter::Marked);
    h.report_counter("young_marked", Counter::YoungMarked);
    h.collect();
    h.relocate_quantum(1, false);
    metric(&mut h, "relocation_still_live", M::Relocation);
    h.finish_collection();
    metric(&mut h, "released", M::Leases);
    for (i, cell) in cells.into_iter().enumerate() {
        h.report(&format!("value{i}"), |h| {
            h.g.mov_data_addr(R::V10, cell);
            h.g.load_ptr_disp32(R::V0, R::V10, 0);
            h.g.call_label(h.l.field_load);
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
    }
}
#[test]
fn production_registered_roots_discover_all_regions_with_independent_bounded_walkers() {
    let o = run(inventory, 0, false);
    assert_eq!(o.get("regions"), 17, "{}", o.text());
    assert_eq!(
        o.get("zero_serial"),
        o.get("initial_journal_entries") + o.get("initial_journal_regions") + 1,
        "{}",
        o.text()
    );
    assert_eq!(
        o.get("one_serial"),
        o.get("zero_serial") + 1,
        "{}",
        o.text()
    );
    for name in [
        "old_still_live",
        "young_still_live",
        "relocation_still_live",
    ] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    assert_eq!(o.get("two_walkers"), 2, "{}", o.text());
    assert_eq!(o.get("marks_released"), 0, "{}", o.text());
    assert_eq!(o.get("released"), 0, "{}", o.text());
    assert_eq!(o.get("marked"), 8, "{}", o.text());
    assert_eq!(o.get("young_marked"), 8, "{}", o.text());
    for i in 0..8 {
        assert_eq!(o.get(&format!("value{i}")), 101 + i, "{}", o.text());
    }
}

#[test]
fn production_registered_roots_real_worker_preserves_other_region_values() {
    let original = run(lifecycle, 0, true);
    assert_eq!(original.get("first"), 84, "{}", original.text());
    assert_eq!(original.get("last"), 91, "{}", original.text());
    assert_eq!(original.get("id"), 1, "{}", original.text());
    assert_eq!(original.get("all_done"), 0, "{}", original.text());
    let o = run(inventory, 0, true);
    assert_eq!(o.get("marked"), 8, "{}", o.text());
    assert_eq!(o.get("young_marked"), 8, "{}", o.text());
    assert_eq!(o.get("released"), 0, "{}", o.text());
    for i in 0..8 {
        assert_eq!(o.get(&format!("value{i}")), 101 + i, "{}", o.text());
    }
}
fn capacity_with_pending_inventory(g: &mut NativeCodeGenerator, _: u64) {
    g.gc_max_heap = 16 << 20;
    g.gc_min_heap = crate::gc_abi::HEAP_PAGE_BYTES;
    let mut h = H::begin(g);
    for _ in 0..4 {
        h.alloc(1, 1 << 20, 0);
    }
    // Leave an actual empty root iterator unvisited. The next physical
    // reservation exhausts large-span capacity and must force Full, even
    // though this snapshot is not a worker-ready policy candidate.
    h.mark_start();
    metric(&mut h, "pending_old", M::Old);
    h.alloc(1, 1 << 20, 0);
    h.set_scalar(Src::Raw(0), 65536, 84);
    h.root(0);
    h.report("value", |h| h.load(Src::Root(0), 65536));
    h.report_counter("full", Counter::FullCollections);
    h.report_counter("planned", Counter::OldPlannedCycles);
    h.report_counter("old_active", Counter::MarkActive);
    h.report("retries", |h| {
        h.g.load_ptr_disp32(
            crate::portable_asm::Reg::V0,
            crate::portable_asm::Reg::V3,
            crate::gc_codegen::production::large_object::RETRIES as i32,
        )
    });
    metric(&mut h, "released", M::Leases);
}
#[test]
fn production_registered_roots_capacity_full_is_not_cancelled_by_pending_inventory() {
    let o = run(capacity_with_pending_inventory, 0, false);
    assert_eq!(o.get("pending_old"), 1, "{}", o.text());
    assert_eq!(o.get("full"), 1, "{}", o.text());
    assert_eq!(o.get("planned"), 0, "{}", o.text());
    assert_eq!(o.get("retries"), 1, "{}", o.text());
    assert_eq!(o.get("old_active"), 0, "{}", o.text());
    assert_eq!(o.get("released"), 0, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
}

fn journal_drain(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    root_journal_drain_probe(h.g, mode);
    h.report("value", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_root_journal_drain_preserves_claimed_ring_slots() {
    for mode in [0, 1, 4] {
        let output = run_failure(journal_drain, mode);
        assert!(
            !output.status.success(),
            "mode {mode} discarded a live or malformed claim"
        );
        assert!(String::from_utf8_lossy(&output.stderr).contains("runtime invariant"));
    }
    for mode in [2, 3] {
        assert_eq!(run(journal_drain, mode, false).get("value"), 84);
    }
}

pub(super) fn journal_handoff(g: &mut NativeCodeGenerator, count: u64) {
    let worker_only = count & (1 << 32) != 0;
    let native = count & (1 << 33) != 0;
    let count = count & u64::from(u32::MAX);
    let mut h = H::begin(g);
    h.alloc(1, 24, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.alloc(2, 8, 0);
    h.store(Src::Raw(0), 0, Src::Raw(2), true);
    h.alloc(1, 24, 1);
    h.set_scalar(Src::Raw(1), 0, 91);
    h.mark_start();
    h.g.call_label(h.l.young_start);
    root_journal_entries(h.g);
    h.keep(20);
    root_journal_yields(h.g);
    h.keep(21);
    h.value(Src::Raw(1));
    h.g.mov_reg_reg(R::V1, R::V0);
    h.value(Src::Raw(0));
    root_journal_publish_pair(h.g, count as usize);
    h.keep(3);
    // Select an explicit transport boundary before observing the handoff.
    if native {
        root_journal_native_boundary(h.g);
    } else {
        root_journal_advance_young(h.g);
    }
    h.report("entries", |h| root_journal_entries(h.g));
    h.report("before_entries", |h| {
        h.g.load_rbp_slot(R::V0, super::slot_offset(20))
    });
    h.report("yields", |h| root_journal_yields(h.g));
    h.report("before_yields", |h| {
        h.g.load_rbp_slot(R::V0, super::slot_offset(21))
    });
    metric(&mut h, "leases", M::Leases);
    if worker_only {
        root_journal_wait_for_worker(h.g, super::slot_offset(22));
        h.report_counter("old_worker_units", Counter::MarkWorkerUnits);
        h.report_counter("young_worker_units", Counter::YoungWorkerUnits);
    } else {
        h.drain_marks();
        h.g.call_label(h.l.lock);
        emit_unbounded_budget(h.g);
        h.g.call_label(h.l.young_loop);
        h.g.call_label(h.l.unlock);
    }
    h.report_counter("old_marked", Counter::Marked);
    h.report_counter("young_marked", Counter::YoungMarked);
    h.end_marks();
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(3), 0));
    metric(&mut h, "released", M::Leases);
}

#[test]
fn production_phase_journal_handoff_marks_both_sides_and_the_overwritten_graph() {
    for count in [2, 34] {
        let o = run(journal_handoff, count, false);
        assert_eq!(
            o.get("entries") - o.get("before_entries"),
            count as i64,
            "{}",
            o.text()
        );
        assert_eq!(
            o.get("yields") - o.get("before_yields"),
            (count as i64 - 1) / 16,
            "{}",
            o.text()
        );
        assert_eq!(o.get("leases"), 2, "{}", o.text());
        assert_eq!(o.get("old_marked"), 3, "{}", o.text());
        assert_eq!(o.get("young_marked"), 3, "{}", o.text());
        assert_eq!(o.get("value"), 91, "{}", o.text());
        assert_eq!(o.get("released"), 0, "{}", o.text());
    }
}

#[test]
fn production_phase_journal_handoff_keeps_real_worker_graph_work_and_releases_leases() {
    let arg = (1 << 32) | 34;
    let failed_spawn = run_failure(journal_handoff, arg);
    assert!(!failed_spawn.status.success());
    assert!(String::from_utf8_lossy(&failed_spawn.stderr).contains("runtime invariant"));
    let o = run(journal_handoff, arg, true);
    assert!(o.get("old_worker_units") > 0, "{}", o.text());
    assert!(o.get("young_worker_units") > 0, "{}", o.text());
    assert_eq!(
        o.get("entries") - o.get("before_entries"),
        34,
        "{}",
        o.text()
    );
    assert_eq!(o.get("yields") - o.get("before_yields"), 2, "{}", o.text());
    assert_eq!(o.get("old_marked"), 3, "{}", o.text());
    assert_eq!(o.get("young_marked"), 3, "{}", o.text());
    assert_eq!(o.get("value"), 91, "{}", o.text());
    assert_eq!(o.get("released"), 0, "{}", o.text());
}

fn invalid_journal_handoff(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.mark_start();
    if mode == 0 {
        // An actual allocation remains Initializing until its publication
        // boundary. Phase adoption may not trace this pending scratch value.
        h.alloc(1, 24, 1);
    } else {
        h.value(Src::Raw(0));
        h.g.mov_reg_reg(R::V1, R::V0);
        root_journal_publish_pair(h.g, 2);
        root_journal_corrupt_entry(h.g, mode == 1);
    }
    root_journal_advance_young(h.g);
}
#[test]
fn production_phase_journal_handoff_rejects_initializing_stale_phase_and_scope() {
    for mode in 0..3 {
        let output = run_failure(invalid_journal_handoff, mode);
        assert!(
            !output.status.success(),
            "mode {mode} accepted an unsafe handoff"
        );
        assert!(String::from_utf8_lossy(&output.stderr).contains("runtime invariant"));
    }
}

fn known_journal_handoff(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.mark_start();
    root_journal_complete_old(h.g);
    h.report_counter("before", Counter::MarkDone);
    h.value(Src::Root(0));
    h.g.mov_reg_reg(R::V1, R::V0);
    root_journal_publish_pair(h.g, 2);
    h.keep(3);
    root_journal_advance_old(h.g);
    h.report_counter("after", Counter::MarkDone);
    h.report_counter("queue", Counter::MarkStackLen);
    h.report_counter("marked", Counter::Marked);
    h.report("value", |h| h.load(Src::Root(3), 0));
}
#[test]
fn production_phase_journal_known_values_preserve_real_mark_completion() {
    let o = run(known_journal_handoff, 0, false);
    assert_eq!(o.get("before"), 1, "{}", o.text());
    assert_eq!(o.get("after"), 1, "{}", o.text());
    assert_eq!(o.get("queue"), 0, "{}", o.text());
    assert_eq!(o.get("marked"), 1, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
}
