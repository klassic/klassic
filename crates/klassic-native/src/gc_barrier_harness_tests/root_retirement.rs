//! Retirement is real deletion work, not a counter substituted for scanning.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::PortableAsm;

fn budgeted(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let region = journal_region_sized(h.g, 2048, 8);
    for _ in 1..2048 {
        journal_region_reserve(h.g, region);
    }
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.value(Src::Raw(0));
    journal_region_store(h.g, region, false);
    h.mark_start();
    h.report("before_mark", |h| h.marked(Src::Raw(0)));
    let retired = retire_region(h.g, region);
    h.report("debt0", |h| retirement_metric(h.g, retired, 2));
    h.report("pending0", |h| retirement_metric(h.g, retired, 3));
    h.report("done0", |h| retirement_metric(h.g, retired, 0));
    retirement_step(h.g); // real deletion lease, no cell traversal
    h.report("debt1", |h| retirement_metric(h.g, retired, 2));
    retirement_step(h.g); // exactly the first actual cell, which is null
    h.report("debt2", |h| retirement_metric(h.g, retired, 2));
    h.report("not_marked", |h| h.marked(Src::Raw(0)));
    // The actual retained reference is in the last reserved cell.
    h.end_marks(); // existing phase handoff must process the rest before Select
    h.report("marked", |h| h.marked(Src::Raw(0)));
    retirement_wait(h.g, retired, true);
    for (name, field) in [
        ("done", 0),
        ("registered", 1),
        ("debt", 6),
        ("transport", 7),
        ("pending", 3),
        ("leases", 4),
    ] {
        h.report(name, |h| retirement_metric(h.g, retired, field));
    }
    h.report("value", |h| h.load(Src::Raw(0), 0));
}
#[test]
fn production_retired_region_deletion_is_budgeted_and_actual_old_mark_precedes_completion() {
    let o = run(budgeted, 0, false);
    assert_eq!(o.get("before_mark"), 0, "{}", o.text());
    assert_eq!(o.get("debt0"), 2048, "{}", o.text());
    assert_eq!(o.get("debt1"), 2048, "{}", o.text());
    assert_eq!(o.get("debt2"), 2047, "{}", o.text());
    assert_eq!(o.get("not_marked"), 0, "{}", o.text());
    assert_eq!(o.get("marked"), 1, "{}", o.text());
    assert_eq!(o.get("pending0"), 1, "{}", o.text());
    assert_eq!(o.get("done0"), 0, "{}", o.text());
    assert_eq!(o.get("done"), 1, "{}", o.text());
    assert_eq!(o.get("value"), 84, "{}", o.text());
    for name in ["registered", "debt", "transport", "pending", "leases"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
}

fn independent(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let region = journal_region_new(h.g, 8);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.value(Src::Raw(0));
    journal_region_store(h.g, region, false);
    let retired = retire_region(h.g, region);
    retirement_wait(h.g, retired, false); // observer does not invoke consumer
    for (name, field) in [
        ("done", 0),
        ("registered", 1),
        ("debt", 6),
        ("transport", 7),
        ("pending", 3),
        ("leases", 4),
    ] {
        h.report(name, |h| retirement_metric(h.g, retired, field));
    }
}
#[test]
fn production_worker_completes_retired_root_storage_without_owner_scan_or_cleanup() {
    let o = run(independent, 0, true);
    assert_eq!(o.get("done"), 1, "{}", o.text());
    for name in ["registered", "debt", "transport", "pending", "leases"] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
}

fn readers(g: &mut NativeCodeGenerator, _: u64) {
    let mut h = H::begin(g);
    let first = journal_region_new(h.g, 8);
    let reader = retirement_reader(h.g, first);
    let a = retire_region(h.g, first);
    let second = journal_region_new(h.g, 8);
    let b = retire_region(h.g, second);
    retirement_wait(h.g, b, false);
    h.report("first_done", |h| retirement_metric(h.g, a, 0));
    h.report("first_registered", |h| retirement_metric(h.g, a, 1));
    h.report("first_leases", |h| retirement_metric(h.g, a, 4));
    h.report("first_disposable", |h| retirement_disposable(h.g, a));
    h.report("second_disposable", |h| retirement_disposable(h.g, b));
    retirement_reader_release(h.g, reader);
    retirement_wait(h.g, a, false);
    h.report("first_final_disposable", |h| retirement_disposable(h.g, a));
    h.report("retiring", |h| retirement_metric(h.g, a, 3));
    h.report("cleared", |h| retirement_metric(h.g, a, 8));
}
#[test]
fn production_root_retirement_waits_for_real_readers_and_does_not_starve_other_regions() {
    let o = run(readers, 0, true);
    for name in [
        "first_done",
        "second_disposable",
        "first_final_disposable",
        "retiring",
    ] {
        assert_eq!(o.get(name), 0, "{}", o.text());
    }
    for name in ["first_registered", "first_leases", "first_disposable"] {
        assert_eq!(o.get(name), 1, "{}", o.text());
    }
    assert_eq!(o.get("cleared"), 2, "{}", o.text());
}

fn dual(g: &mut NativeCodeGenerator, old_first: u64) {
    let mut h = H::begin(g);
    let region = journal_region_new(h.g, 8);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.value(Src::Raw(0));
    journal_region_store(h.g, region, false);
    h.alloc(1, 24, 1); // allocation scratch no longer retains the target
    if old_first != 0 {
        h.mark_start();
    }
    h.g.call_label(h.l.young_start);
    if old_first == 0 {
        h.mark_start();
    }
    h.report("old_before", |h| h.marked(Src::Raw(0)));
    h.report("young_before", |h| {
        h.value(Src::Raw(0));
        emit_young_marked(h.g);
    });
    let retired = retire_region(h.g, region);
    retirement_step(h.g); // claim
    retirement_step(h.g); // actual reference admissions for both engines
    h.report("old_after", |h| h.marked(Src::Raw(0)));
    h.report("young_after", |h| {
        h.value(Src::Raw(0));
        emit_young_marked(h.g);
    });
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    h.end_marks();
    retirement_wait(h.g, retired, true);
    h.report("disposable", |h| retirement_disposable(h.g, retired));
}
#[test]
fn production_root_deletion_admits_both_actual_young_and_old_generations_in_either_start_order() {
    for old_first in 0..2 {
        let o = run(dual, old_first, false);
        for name in ["old_before", "young_before", "disposable"] {
            assert_eq!(o.get(name), 0, "order {old_first}: {}", o.text());
        }
        for name in ["old_after", "young_after"] {
            assert_eq!(o.get(name), 1, "order {old_first}: {}", o.text());
        }
    }
}
