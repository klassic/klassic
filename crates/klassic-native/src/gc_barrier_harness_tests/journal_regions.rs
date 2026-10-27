//! Per-region actual root journal admission and bounded inventory discovery.
use super::{H, Src, run, run_failure, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};
fn saved(h: &mut H<'_>, name: &str, slot: u32) {
    h.report(name, |h| h.g.load_rbp_slot(R::V0, slot_offset(slot)));
}
fn done(h: &mut H<'_>) {
    h.drain_marks();
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
}
fn objects(h: &mut H<'_>) {
    h.alloc(1, 24, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.alloc(2, 8, 0);
    h.store(Src::Raw(0), 0, Src::Raw(2), true);
    h.alloc(1, 24, 1);
    h.set_scalar(Src::Raw(1), 0, 91);
}
fn inventory(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    let first = journal_region_new(h.g, 8);
    for _ in 0..20 {
        root_registry_add_region(h.g, true);
    }
    let second = journal_region_new(h.g, 8);
    objects(&mut h);
    h.alloc(1, 24, 4);
    h.set_scalar(Src::Raw(4), 0, 101);
    h.mark_start();
    h.g.call_label(h.l.young_start);
    root_journal_entries(h.g);
    h.keep(20);
    root_journal_yields(h.g);
    h.keep(21);
    journal_regions_visited(h.g);
    h.keep(22);
    h.value(Src::Raw(0));
    journal_region_store(h.g, first, false);
    h.value(Src::Raw(1));
    journal_region_store(h.g, first, false);
    h.value(Src::Raw(4));
    journal_region_store(h.g, second, false);
    h.g.mov_imm64(R::V0, 0);
    journal_region_store(h.g, second, false);
    if mode == 0 {
        root_journal_native_boundary(h.g);
    } else {
        root_journal_advance_young(h.g);
    }
    h.report("entries", |h| root_journal_entries(h.g));
    saved(&mut h, "before_entries", 20);
    h.report("yields", |h| root_journal_yields(h.g));
    saved(&mut h, "before_yields", 21);
    h.report("regions", |h| journal_regions_visited(h.g));
    saved(&mut h, "before_regions", 22);
    for (name, r) in [("first", first), ("second", second)] {
        h.report(&format!("{name}_published"), |h| {
            journal_region_cursor(h.g, r, false)
        });
        h.report(&format!("{name}_consumed"), |h| {
            journal_region_cursor(h.g, r, true)
        });
    }
    if mode == 2 {
        root_journal_wait_for_worker(h.g, slot_offset(23));
        h.report_counter("old_worker", Counter::MarkWorkerUnits);
        h.report_counter("young_worker", Counter::YoungWorkerUnits);
    } else {
        done(&mut h);
    }
    h.report_counter("old", Counter::Marked);
    h.report_counter("young", Counter::YoungMarked);
    h.end_marks();
    h.collect();
    h.finish_collection();
    h.report("value", |h| journal_region_load(h.g, first));
    h.report("leases", |h| {
        root_registry_metric(h.g, RootRegistryMetric::Leases)
    });
    h.report("walkers", |h| journal_registry_walkers(h.g));
}
#[test]
fn production_all_registered_journals_handoff_overwritten_graphs_at_native_and_phase_boundaries() {
    for mode in 0..3 {
        let o = run(inventory, mode, mode == 2);
        assert_eq!(
            o.get("entries") - o.get("before_entries"),
            4,
            "{}",
            o.text()
        );
        assert_eq!(
            o.get("regions") - o.get("before_regions"),
            23,
            "{}",
            o.text()
        );
        assert_eq!(o.get("yields") - o.get("before_yields"), 1, "{}", o.text());
        for name in ["first", "second"] {
            assert_eq!(
                o.get(&format!("{name}_published")),
                o.get(&format!("{name}_consumed")),
                "{}",
                o.text()
            );
        }
        for name in ["old", "young"] {
            assert_eq!(o.get(name), 4, "{}", o.text());
        }
        if mode == 2 {
            assert!(o.get("old_worker") > 0);
            assert!(o.get("young_worker") > 0);
        }
        assert_eq!(o.get("value"), 91, "{}", o.text());
        assert_eq!(o.get("leases"), 0, "{}", o.text());
        assert_eq!(o.get("walkers"), 0, "{}", o.text());
    }
}
fn pressure(g: &mut NativeCodeGenerator, locked: u64) {
    let mut h = H::begin(g);
    let region = journal_region_new(h.g, 2);
    objects(&mut h);
    h.mark_start();
    h.g.call_label(h.l.young_start);
    h.value(Src::Raw(0));
    journal_region_store(h.g, region, false);
    h.value(Src::Raw(1));
    journal_region_store(h.g, region, false);
    root_journal_entries(h.g);
    h.keep(20);
    root_journal_yields(h.g);
    h.keep(21);
    journal_region_cursor(h.g, region, true);
    h.keep(22);
    h.g.mov_imm64(R::V0, 0);
    journal_region_store(h.g, region, locked != 0);
    h.report("entries", |h| root_journal_entries(h.g));
    saved(&mut h, "before_entries", 20);
    h.report("yields", |h| root_journal_yields(h.g));
    saved(&mut h, "before_yields", 21);
    h.report("consumed", |h| journal_region_cursor(h.g, region, true));
    saved(&mut h, "before_consumed", 22);
    h.report("a", |h| h.marked(Src::Raw(0)));
    h.report("b", |h| h.marked(Src::Raw(1)));
    done(&mut h);
    h.report_counter("old_graph", Counter::Marked);
    h.report_counter("young_graph", Counter::YoungMarked);
    root_journal_native_boundary(h.g);
    done(&mut h);
    h.report_counter("final_old", Counter::Marked);
    h.report_counter("final_young", Counter::YoungMarked);
    h.report("leases", |h| {
        root_registry_metric(h.g, RootRegistryMetric::Leases)
    });
}
#[test]
fn production_other_region_full_ring_retries_its_own_entry_without_yield_inside_or_outside_lock() {
    for locked in [0, 1] {
        let o = run(pressure, locked, false);
        assert_eq!(
            o.get("entries") - o.get("before_entries"),
            1,
            "{}",
            o.text()
        );
        assert_eq!(
            o.get("consumed") - o.get("before_consumed"),
            1,
            "{}",
            o.text()
        );
        assert_eq!(o.get("yields") - o.get("before_yields"), 0, "{}", o.text());
        assert_eq!(o.get("a"), 1, "{}", o.text());
        assert_eq!(o.get("b"), 0, "{}", o.text());
        for name in ["old_graph", "young_graph"] {
            assert_eq!(o.get(name), 2, "{}", o.text());
        }
        for name in ["final_old", "final_young"] {
            assert_eq!(o.get(name), 3, "{}", o.text());
        }
        assert_eq!(o.get("leases"), 0, "{}", o.text());
    }
}
fn corrupt(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    let region = journal_region_new(h.g, 2);
    h.mark_start();
    h.g.mov_imm64(R::V0, 0);
    journal_region_store(h.g, region, false);
    journal_region_corrupt(h.g, region, mode);
    root_journal_native_boundary(h.g);
}
#[test]
fn production_other_region_journals_reject_stale_phase_scope_and_foreign_context() {
    for mode in 0..3 {
        let o = run_failure(corrupt, mode);
        assert!(!o.status.success());
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant"));
    }
}
