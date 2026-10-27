//! Actual full-ring pressure, native handoff and private initialization.
use super::{H, Src, run, run_failure, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::*;
use crate::portable_asm::{PortableAsm, Reg as R};

fn saved(h: &mut H<'_>, name: &str, slot: u32) {
    h.report(name, |h| h.g.load_rbp_slot(R::V0, slot_offset(slot)));
}
fn both_done(h: &mut H<'_>) {
    h.drain_marks();
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
}
#[test]
fn production_native_entry_hands_both_journal_values_to_real_graph_work() {
    for worker in [false, true] {
        let arg = (1 << 33) | 34 | if worker { 1 << 32 } else { 0 };
        let o = run(super::registered_roots::journal_handoff, arg, worker);
        assert_eq!(
            o.get("entries") - o.get("before_entries"),
            34,
            "{}",
            o.text()
        );
        assert_eq!(o.get("yields") - o.get("before_yields"), 2);
        assert_eq!(o.get("old_marked"), 3);
        assert_eq!(o.get("young_marked"), 3);
        assert_eq!(o.get("value"), 91);
        assert_eq!(o.get("released"), 0);
        if worker {
            assert!(o.get("old_worker_units") > 0);
            assert!(o.get("young_worker_units") > 0);
        }
    }
}

fn pressure(g: &mut NativeCodeGenerator, locked: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 24, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.alloc(2, 8, 0);
    h.store(Src::Raw(0), 0, Src::Raw(2), true);
    h.alloc(1, 24, 1);
    h.set_scalar(Src::Raw(1), 0, 91);
    h.mark_start();
    h.g.call_label(h.l.young_start);
    h.value(Src::Raw(1));
    h.g.mov_reg_reg(R::V1, R::V0);
    h.value(Src::Raw(0));
    root_journal_publish_pair(h.g, 2);
    h.keep(3);
    // Fill through the real producer's FLUSH, without eager pre-barriers.
    root_journal_fill_frame(h.g);
    root_journal_outstanding(h.g);
    h.keep(30);
    root_journal_entries(h.g);
    h.keep(20);
    root_journal_yields(h.g);
    h.keep(21);
    root_journal_pressure_store(h.g, locked != 0);
    h.keep(22);
    root_journal_entries(h.g);
    h.keep(23);
    root_journal_yields(h.g);
    h.keep(24);
    h.marked(Src::Raw(0));
    h.keep(25);
    h.marked(Src::Raw(1));
    h.keep(26);
    root_journal_owner(h.g);
    h.keep(27);
    root_journal_outstanding(h.g);
    h.keep(31);
    // Only the first entry (0 -> A) was handed off. Trace A's sole child C
    // before any native/phase seal can replay the remaining A/B records.
    both_done(&mut h);
    h.counter(Counter::Marked);
    h.keep(28);
    h.counter(Counter::YoungMarked);
    h.keep(29);
    for (name, slot) in [
        ("before_entries", 20),
        ("before_yields", 21),
        ("inside_owner", 22),
        ("entries", 23),
        ("yields", 24),
        ("a", 25),
        ("b", 26),
        ("owner", 27),
        ("old_graph", 28),
        ("young_graph", 29),
        ("before_outstanding", 30),
        ("after_outstanding", 31),
    ] {
        saved(&mut h, name, slot);
    }
    root_journal_native_boundary(h.g);
    // The remaining records now legitimately queue B. This failed-spawn
    // fixture must service that new Young work before requesting collection.
    both_done(&mut h);
    h.report_counter("final_old_graph", Counter::Marked);
    h.report_counter("final_young_graph", Counter::YoungMarked);
    h.end_marks();
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(3), 0));
}
#[test]
fn production_full_journal_pressure_retries_one_record_inside_and_outside_owner_lock() {
    for locked in [0, 1] {
        let o = run(pressure, locked, false);
        assert_eq!(
            o.get("entries") - o.get("before_entries"),
            1,
            "{}",
            o.text()
        );
        assert_eq!(o.get("yields") - o.get("before_yields"), 0);
        assert_eq!(o.get("before_outstanding"), 4096);
        assert_eq!(o.get("after_outstanding"), 4096);
        assert_eq!(o.get("inside_owner"), locked as i64);
        assert_eq!(o.get("owner"), 0);
        assert_eq!(o.get("a"), 1);
        assert_eq!(o.get("b"), 0);
        assert_eq!(o.get("old_graph"), 2);
        assert_eq!(o.get("young_graph"), 2);
        assert_eq!(o.get("final_old_graph"), 3);
        assert_eq!(o.get("final_young_graph"), 3);
        assert_eq!(o.get("value"), 91);
    }
}
fn initializing(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    let large = mode >= 3;
    h.mark_start();
    if !large {
        h.g.call_label(h.l.young_start);
    }
    h.alloc(2, if large { 131072 } else { 8 }, 0);
    h.value(Src::Raw(0));
    h.g.mov_reg_reg(R::V1, R::V0);
    root_journal_publish_pair(h.g, 2);
    h.keep(3);
    root_journal_fill_frame(h.g);
    if matches!(mode, 1 | 2 | 4) {
        root_journal_pressure_once(h.g); // 0 -> 0 scratch clear
        root_journal_forget_pending(h.g, mode == 2);
        root_journal_pressure_once(h.g); // reject unknown Initializing P
        return;
    }
    root_journal_published(h.g);
    h.keep(20);
    h.value(Src::Raw(0));
    h.set_root(3);
    h.value(Src::Raw(0));
    h.set_root(3);
    root_journal_published(h.g);
    h.keep(21);
    root_journal_pending(h.g);
    h.keep(22);
    root_journal_pending_state(h.g);
    h.keep(23);
    root_journal_pending_offers(h.g);
    h.keep(24);
    h.counter(Counter::MarkStackLen);
    h.keep(25);
    h.counter(Counter::YoungStackLen);
    h.keep(26);
    // Finish initialization only now, then publish at the real native boundary.
    h.store(Src::Raw(0), 0, Src::Null, false);
    root_journal_native_boundary(h.g);
    root_journal_pending(h.g);
    h.keep(27);
    root_journal_published(h.g);
    h.keep(28);
    for (name, slot) in [
        ("before_pub", 20),
        ("after_pressure_pub", 21),
        ("pending", 22),
        ("state", 23),
        ("pending_offers", 24),
        ("old_queue", 25),
        ("young_queue", 26),
        ("final_pending", 27),
        ("final_pub", 28),
    ] {
        saved(&mut h, name, slot);
    }
    if large {
        h.drain_marks();
    } else {
        both_done(&mut h);
    }
    h.end_marks();
    h.collect();
    h.finish_collection();
}
#[test]
fn production_full_journal_pressure_keeps_initializing_allocation_private_until_publication() {
    for mode in [0, 3] {
        let o = run(initializing, mode, false);
        assert_eq!(
            o.get("before_pub"),
            o.get("after_pressure_pub"),
            "{}",
            o.text()
        );
        assert_ne!(o.get("pending"), 0);
        assert_eq!(o.get("state"), crate::gc_abi::OBJECT_INITIALIZING as i64);
        assert!(o.get("pending_offers") > 0);
        assert_eq!(o.get("old_queue"), 0);
        assert_eq!(o.get("young_queue"), 0);
        assert_eq!(o.get("final_pending"), 0);
        assert_eq!(o.get("final_pub"), o.get("before_pub") + 1);
    }
}
#[test]
fn production_journal_pressure_rejects_unprotected_initializing_values_in_either_generation() {
    for mode in [1, 2, 4] {
        let o = run_failure(initializing, mode);
        assert!(!o.status.success());
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant"));
    }
}

fn invalid_owner_lock(g: &mut NativeCodeGenerator, nested: u64) {
    let h = H::begin(g);
    root_journal_bad_owner_lock(h.g, nested != 0);
}
#[test]
fn production_owner_lock_rejects_recursion_and_unmatched_release() {
    for nested in [0, 1] {
        let o = run_failure(invalid_owner_lock, nested);
        assert!(!o.status.success());
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant"));
    }
}

fn invalid_protected_record(g: &mut NativeCodeGenerator, scope: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 24, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    h.mark_start();
    root_journal_complete_old(h.g);
    h.value(Src::Root(0));
    h.g.mov_reg_reg(R::V1, R::V0);
    root_journal_publish_pair(h.g, 2);
    root_journal_corrupt_entry(h.g, scope == 0);
    root_journal_pressure_once(h.g);
}
#[test]
fn production_protected_journal_pressure_still_rejects_stale_phase_and_scope() {
    for scope in [0, 1] {
        let o = run_failure(invalid_protected_record, scope);
        assert!(!o.status.success());
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant"));
    }
}
