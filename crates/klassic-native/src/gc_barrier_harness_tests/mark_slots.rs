//! Slot budgets and an unfinished object as a retained mark frontier.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::Counter;
use crate::portable_asm::{Condition, PortableAsm, Reg};

const SLOTS: u32 = 8192;

fn begin(g: &mut NativeCodeGenerator) -> H<'_> {
    let mut h = H::begin(g);
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.root(0);
    for (slot, value) in [(1, 42), (2, 84), (3, 99)] {
        h.alloc(1, 16, slot);
        h.set_scalar(Src::Raw(slot), 0, value);
    }
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.store(Src::Root(0), SLOTS - 1, Src::Raw(2), true);
    h.mark_start();
    h.mark_quantum(1); // Discover the sole frame root.
    h.mark_quantum(1); // Pop the object, but no budget remains for fields.
    h.report_counter("active", Counter::MarkObject);
    h.report_counter("initial_field", Counter::MarkField);
    h.report_counter("initial_queue", Counter::MarkStackLen);
    h.report_counter("root_cursor", Counter::MarkRootIndex);
    h.report_counter("root_bound", Counter::MarkRootBound);
    h
}

fn bounded_scan(g: &mut NativeCodeGenerator, overwrite: u64) {
    let mut h = begin(g);
    h.mark_quantum(1);
    h.keep(4);
    h.report("done1", |h| h.value(Src::Raw(4)));
    h.report_counter("field1", Counter::MarkField);
    h.report("first", |h| h.marked(Src::Raw(1)));
    h.report("last", |h| h.marked(Src::Raw(2)));
    h.mark_quantum(2); // Null slots still cost inspection work.
    h.report_counter("field3", Counter::MarkField);
    if overwrite != 0 {
        h.store(Src::Root(0), SLOTS - 1, Src::Raw(3), true);
    }
    h.report_counter("field_after_store", Counter::MarkField);
    h.report("old_after_store", |h| h.marked(Src::Raw(2)));
    h.report("new_after_store", |h| h.marked(Src::Raw(3)));
    h.repeat(130, |h| {
        h.mark_quantum(64);
        h.keep(4);
    });
    h.report("done", |h| h.value(Src::Raw(4)));
    h.report_counter("active_done", Counter::MarkObject);
    h.report_counter("queue_done", Counter::MarkStackLen);
    h.report("old_done", |h| h.marked(Src::Raw(2)));
    h.collect();
    h.finish_collection();
    h.report_counter("unmarked", Counter::Unmarked);
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
}

#[test]
fn production_mark_slots_obey_budget_resume_and_preserve_intervening_stores() {
    for overwrite in [0, 1] {
        let o = run(bounded_scan, overwrite, false);
        assert!(o.get("active") > 0);
        assert_eq!(o.get("initial_field"), 0);
        assert_eq!(o.get("initial_queue"), 0);
        assert_eq!(o.get("root_cursor"), o.get("root_bound"));
        assert_eq!(o.get("done1"), 0);
        assert_eq!(o.get("field1"), 1);
        assert_eq!(o.get("first"), 1);
        assert_eq!(o.get("last"), 0);
        assert_eq!(o.get("field3"), 3);
        assert_eq!(o.get("field_after_store"), 3);
        assert_eq!(o.get("old_after_store"), overwrite as i64);
        assert_eq!(o.get("new_after_store"), overwrite as i64);
        assert_eq!(o.get("done"), 1);
        assert_eq!(o.get("active_done"), 0);
        assert_eq!(o.get("queue_done"), 0);
        assert_eq!(o.get("old_done"), 1);
        assert_eq!(o.get("unmarked"), 0);
        assert_eq!(o.get("value"), if overwrite == 0 { 84 } else { 99 });
    }
}

fn cursor_only_drain(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = begin(g);
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.mark_end); // The active receiver prevents termination.
    h.g.call_label(h.l.unlock);
    h.collect();
    h.finish_collection();
    h.report_counter("drains", Counter::MarkDrains);
    h.report_counter("unmarked", Counter::Unmarked);
    h.report_counter("active_done", Counter::MarkObject);
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
}

#[test]
fn production_mark_end_drains_a_cursor_even_when_roots_and_queue_are_empty() {
    let o = run(cursor_only_drain, 0, false);
    assert!(o.get("active") > 0);
    assert_eq!(o.get("initial_queue"), 0);
    assert_eq!(o.get("root_cursor"), o.get("root_bound"));
    assert_eq!(o.get("drains"), 1);
    assert_eq!(o.get("unmarked"), 0, "{}", o.text());
    assert_eq!(o.get("active_done"), 0);
    assert_eq!(o.get("value"), 84);
}

fn worker_scan(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), SLOTS - 1, Src::Raw(1), true);
    h.mark_start();
    h.alloc(1, 16, 2); // Wake the real worker; no mutator mark_loop calls.
    let done = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::MarkDone);
        h.g.test_reg_reg(Reg::V0, Reg::V0);
        h.g.jcc_label(Condition::NotEqual, done);
        h.g.mov_imm64(Reg::V0, 24); // Linux sched_yield.
        h.g.asm.syscall();
    });
    h.g.bind_text_label(done);
    h.report_counter("done", Counter::MarkDone);
    h.report_counter("active_done", Counter::MarkObject);
    h.report_counter("queue_done", Counter::MarkStackLen);
    h.report_counter("field_done", Counter::MarkField);
    h.report("last_marked", |h| h.marked(Src::Raw(1)));
}

#[test]
fn production_worker_finishes_a_large_mark_cursor_without_mutator_assistance() {
    let o = run(worker_scan, 0, true);
    assert_eq!(o.get("done"), 1, "{}", o.text());
    assert_eq!(o.get("active_done"), 0);
    assert_eq!(o.get("queue_done"), 0);
    assert_eq!(o.get("field_done"), i64::from(SLOTS));
    assert_eq!(o.get("last_marked"), 1);
}
