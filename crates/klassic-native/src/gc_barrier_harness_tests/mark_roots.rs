//! Mark-start root bounds and SATB protection before asynchronous discovery.
use super::{H, Src, run, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::Counter;
use crate::portable_asm::{PortableAsm, Reg};

fn drop_before_scan(g: &mut NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.keep(2); // Observe its mark bit without recreating a strong root.
    h.root(1);
    h.mark_start();
    h.report_counter("marked_at_start", Counter::Marked);
    h.report_counter("cursor_at_start", Counter::MarkRootIndex);
    h.report_counter("bound", Counter::MarkRootBound);
    if arg == 2 {
        // Negative control: clear the cell directly, omitting root SATB.
        h.g.load_rbp_slot(Reg::V1, slot_offset(1));
        h.g.mov_imm64(Reg::V0, 0);
        h.g.store_ptr_disp32(Reg::V1, 0, Reg::V0);
    } else if arg == 1 {
        h.g.call_label(h.l.root_pop);
    } else {
        h.value(Src::Null);
        h.set_root(1);
    }
    h.report("old_marked_after_drop", |h| h.marked(Src::Raw(2)));
    h.drain_marks();
    h.report("old_marked_after_drain", |h| h.marked(Src::Raw(2)));
    h.report("retained_marked", |h| h.marked(Src::Root(0)));
    h.report_counter("cursor_after_drain", Counter::MarkRootIndex);
}

#[test]
fn production_mark_start_defers_root_scanning_and_root_mutations_preserve_snapshot() {
    for op in [0, 1] {
        let o = run(drop_before_scan, op, false);
        assert_eq!(o.get("marked_at_start"), 0, "{}", o.text());
        assert!(o.get("cursor_at_start") < o.get("bound"), "{}", o.text());
        assert_eq!(o.get("old_marked_after_drop"), 1, "{}", o.text());
        assert_eq!(o.get("old_marked_after_drain"), 1, "{}", o.text());
        assert_eq!(o.get("retained_marked"), 1);
        assert_eq!(o.get("cursor_after_drain"), o.get("bound"));
    }
}

#[test]
fn production_root_drop_without_satb_loses_the_snapshot_referent() {
    let o = run(drop_before_scan, 2, false);
    assert_eq!(o.get("marked_at_start"), 0, "{}", o.text());
    assert_eq!(o.get("old_marked_after_drop"), 0);
    assert_eq!(o.get("old_marked_after_drain"), 0);
    assert_eq!(o.get("retained_marked"), 1);
    assert_eq!(o.get("cursor_after_drain"), o.get("bound"));
}

fn many_roots(g: &mut NativeCodeGenerator, worker: u64) {
    let mut h = H::begin(g);
    h.repeat(1000, |h| {
        h.alloc(1, 16, 0);
        h.root(0);
    });
    h.mark_start();
    h.report_counter("initial_marked", Counter::Marked);
    h.report_counter("initial_cursor", Counter::MarkRootIndex);
    h.report_counter("bound", Counter::MarkRootBound);
    if worker == 0 {
        h.mark_quantum(1); // One root is discovered; its graph work is queued.
        h.keep(3);
        h.report("done1", |h| h.value(Src::Raw(3)));
        h.report_counter("cursor1", Counter::MarkRootIndex);
        h.mark_quantum(1); // The raw object is drained, but roots still remain.
        h.keep(3);
        h.report("done2", |h| h.value(Src::Raw(3)));
        h.report_counter("cursor2", Counter::MarkRootIndex);
        h.drain_marks();
    } else {
        h.alloc(1, 16, 1); // Wakes the real worker; the object is born black.
        let done = h.g.create_text_label();
        h.repeat(1_000_000, |h| {
            h.counter(Counter::MarkRootIndex);
            h.g.mov_reg_reg(Reg::V1, Reg::V0);
            h.counter(Counter::MarkRootBound);
            h.g.cmp_reg_reg(Reg::V1, Reg::V0);
            h.g.jcc_label(crate::portable_asm::Condition::AboveOrEqual, done);
            h.g.mov_imm64(Reg::V0, 24); // sched_yield; no mutator mark assist.
            h.g.asm.syscall();
        });
        h.g.bind_text_label(done);
    }
    h.report_counter("cursor_done", Counter::MarkRootIndex);
    h.report_counter("marked_done", Counter::Marked);
    h.report("last_root_marked", |h| h.marked(Src::Root(0)));
}

#[test]
fn production_mark_root_discovery_spends_budget_and_blocks_premature_termination() {
    let o = run(many_roots, 0, false);
    assert_eq!(o.get("initial_marked"), 0);
    assert_eq!(o.get("bound"), 1001);
    assert_eq!(o.get("cursor1"), o.get("initial_cursor") + 1);
    assert_eq!(o.get("cursor2"), o.get("cursor1"));
    assert_eq!(o.get("done1"), 0);
    assert_eq!(o.get("done2"), 0);
    assert_eq!(o.get("cursor_done"), o.get("bound"));
    assert_eq!(o.get("marked_done"), 1000);
    assert_eq!(o.get("last_root_marked"), 1);
}

#[test]
fn production_worker_discovers_pending_roots_without_mutator_mark_assistance() {
    let o = run(many_roots, 1, true);
    // The worker may start as soon as mark_start releases its lock.
    assert!(o.get("initial_marked") <= 1000);
    assert!((1..=1001).contains(&o.get("initial_cursor")));
    assert_eq!(o.get("bound"), 1001);
    assert_eq!(o.get("cursor_done"), o.get("bound"), "{}", o.text());
    assert!(o.get("marked_done") >= 1000);
    assert_eq!(o.get("last_root_marked"), 1);
}
