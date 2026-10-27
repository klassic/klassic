//! Deferred remembered roots, independent membership and concurrent slot repair.
use super::{H, Src, run, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{
    Counter, emit_encode_reference, heal_remembered_slot,
};
use crate::portable_asm::{Condition, PortableAsm, Reg};

const SLOTS: u32 = 1000;

fn holder(h: &mut H<'_>, slots: u32) {
    h.alloc(2, u64::from(slots) * 8, 0);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
}

fn child(h: &mut H<'_>, slot: u32, value: u64) {
    h.alloc(1, 16, slot);
    h.set_scalar(Src::Raw(slot), 0, value);
}

fn fill(h: &mut H<'_>) {
    h.value(Src::Root(0));
    h.keep(3); // Stable Old slot address; Old does not move in this cycle.
    h.repeat(SLOTS.into(), |h| {
        h.value(Src::Raw(3));
        h.g.mov_reg_reg(Reg::V7, Reg::V0);
        h.value(Src::Raw(1));
        h.g.mov_reg_reg(Reg::V6, Reg::V0);
        h.g.call_label(h.l.field_store);
        h.value(Src::Raw(3));
        h.g.add_reg_imm32(Reg::V0, 8);
        h.keep(3);
    });
}

fn budgeted(g: &mut NativeCodeGenerator, actor: u64) {
    let mut h = H::begin(g);
    holder(&mut h, SLOTS);
    child(&mut h, 1, 42);
    fill(&mut h);
    child(&mut h, 2, 99);
    h.root(2);
    h.report_counter("mutator_before", Counter::MutatorCopies);
    h.report_counter("worker_before", Counter::WorkerCopies);
    h.collect();
    h.report_counter("initial_index", Counter::RemsetIndex);
    h.report_counter("initial_previous", Counter::RemsetPreviousLen);
    h.report_counter("initial_current", Counter::RemsetLen);
    h.report_counter("mutator_start", Counter::MutatorCopies);
    h.report_counter("worker_start", Counter::WorkerCopies);
    h.relocate_quantum(7, actor != 0);
    h.keep(4);
    h.report("done7", |h| h.value(Src::Raw(4)));
    h.report_counter("index7", Counter::RemsetIndex);
    h.report_counter("previous7", Counter::RemsetPreviousLen);
    h.report_counter("current7", Counter::RemsetLen);
    h.report_counter("mutator7", Counter::MutatorCopies);
    h.report_counter("worker7", Counter::WorkerCopies);
    h.report_counter("root7", Counter::RelocateRootIndex);
    h.relocate_quantum(0, actor == 0);
    h.keep(4);
    h.report("done0", |h| h.value(Src::Raw(4)));
    h.report_counter("index0", Counter::RemsetIndex);
    h.store(Src::Root(0), SLOTS - 2, Src::Null, true);
    h.store(Src::Root(0), SLOTS - 3, Src::Imm(0x1235), true);
    h.store(Src::Root(0), SLOTS - 1, Src::Root(2), true);
    h.relocate_quantum(u64::from(SLOTS - 10), actor == 0);
    h.report_counter("before_opaque", Counter::RemsetIndex);
    h.relocate_quantum(1, actor != 0);
    h.report_counter("after_opaque", Counter::RemsetIndex);
    h.relocate_quantum(1, actor == 0);
    h.report_counter("after_null", Counter::RemsetIndex);
    h.repeat(20, |h| h.relocate_quantum(64, actor == 0));
    h.report_counter("done_previous", Counter::RemsetPreviousLen);
    h.report_counter("done_current", Counter::RemsetLen);
    h.finish_collection();
    h.report("first", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report("null", |h| h.load(Src::Root(0), SLOTS - 2));
    h.report("opaque", |h| h.load(Src::Root(0), SLOTS - 3));
    h.report("last", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
    h.collect();
    h.finish_collection();
    h.report_counter("promoted_current", Counter::RemsetLen);
    h.report("promoted", |h| h.field_is_old(Src::Root(0), 0));
    h.report("last_again", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
}

#[test]
fn production_remembered_roots_are_deferred_budgeted_and_resume_across_copiers() {
    for actor in [0, 1] {
        let o = run(budgeted, actor, false);
        assert_eq!(o.get("initial_index"), 0);
        assert_eq!(o.get("initial_previous"), SLOTS.into());
        assert_eq!(o.get("initial_current"), 0);
        assert_eq!(o.get("mutator_start"), o.get("mutator_before"));
        assert_eq!(o.get("worker_start"), o.get("worker_before"));
        assert_eq!(o.get("done7"), 0);
        assert_eq!(o.get("index7"), 7);
        assert_eq!(o.get("previous7"), SLOTS.into());
        assert_eq!(o.get("current7"), 7);
        assert_eq!(
            o.get("mutator7") - o.get("mutator_start"),
            if actor == 0 { 1 } else { 0 }
        );
        assert_eq!(
            o.get("worker7") - o.get("worker_start"),
            if actor == 0 { 0 } else { 1 }
        );
        assert_eq!(o.get("root7"), 1);
        assert_eq!(o.get("done0"), 0);
        assert_eq!(o.get("index0"), 7);
        assert_eq!(o.get("done_previous"), 0);
        assert_eq!(o.get("before_opaque"), i64::from(SLOTS - 3));
        assert_eq!(o.get("after_opaque"), i64::from(SLOTS - 2));
        assert_eq!(o.get("after_null"), i64::from(SLOTS - 1));
        assert_eq!(o.get("done_current"), i64::from(SLOTS - 2));
        assert_eq!(o.get("first"), 42);
        assert_eq!(o.get("null"), 0);
        assert_eq!(o.get("opaque"), 0x1235);
        assert_eq!(o.get("last"), 99);
        assert_eq!(o.get("promoted_current"), 0);
        assert_eq!(o.get("promoted"), 1);
        assert_eq!(o.get("last_again"), 99);
    }
}

fn memberships(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    holder(&mut h, 2);
    for cycle in 0..4 {
        child(&mut h, 1, 42);
        for field in 0..2 {
            h.store(Src::Root(0), field, Src::Raw(1), true);
        }
        child(&mut h, 2, 99);
        h.root(2);
        h.mark_start();
        h.drain_marks(); // Complete mark before the stores resolve into to-space.
        h.end_marks();
        h.collect();
        for field in 0..2 {
            h.store(Src::Root(0), field, Src::Root(2), true);
        }
        h.report_counter(&format!("before{cycle}"), Counter::RemsetLen);
        h.report_counter(&format!("kept_before{cycle}"), Counter::RemsetKept);
        h.report_counter(&format!("dropped_before{cycle}"), Counter::RemsetDropped);
        h.relocate_quantum(1, true);
        h.store(Src::Root(0), 0, Src::Root(2), true);
        h.report_counter(&format!("middle{cycle}"), Counter::RemsetLen);
        h.relocate_quantum(1, false);
        h.repeat(100, |h| {
            h.store(Src::Root(0), 0, Src::Root(2), true);
            h.store(Src::Root(0), 1, Src::Root(2), true);
        });
        h.report_counter(&format!("after{cycle}"), Counter::RemsetLen);
        h.report_counter(&format!("kept_after{cycle}"), Counter::RemsetKept);
        h.report_counter(&format!("dropped_after{cycle}"), Counter::RemsetDropped);
        h.finish_collection();
        h.report(&format!("value{cycle}"), |h| {
            h.deref_field(Src::Root(0), 1, 0)
        });
        h.store(Src::Root(0), 0, Src::Null, true);
        h.store(Src::Root(0), 1, Src::Null, true);
        h.pop_root();
        h.collect();
        h.finish_collection();
        h.report_counter(&format!("empty{cycle}"), Counter::RemsetLen);
    }
}

#[test]
fn production_current_and_previous_remembered_memberships_are_independent() {
    let o = run(memberships, 0, false);
    for cycle in 0..4 {
        for stage in ["before", "middle", "after"] {
            assert_eq!(o.get(&format!("{stage}{cycle}")), 2, "{}", o.text());
        }
        assert_eq!(o.get(&format!("value{cycle}")), 99);
        assert_eq!(
            o.get(&format!("kept_after{cycle}")),
            o.get(&format!("kept_before{cycle}")) + 2
        );
        assert_eq!(
            o.get(&format!("dropped_after{cycle}")),
            o.get(&format!("dropped_before{cycle}"))
        );
        assert_eq!(o.get(&format!("empty{cycle}")), 0);
    }
}

fn stale_heal(g: &mut NativeCodeGenerator, arg: u64) {
    let mut h = H::begin(g);
    holder(&mut h, 2);
    child(&mut h, 1, 42);
    child(&mut h, 2, 99);
    h.root(2);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.load(Src::Root(0), 0);
    h.keep(4); // Observed old word, retained across a new owner publication.
    h.collect();
    h.worker_copy(Src::Raw(1));
    h.keep(5);
    h.store(
        Src::Root(0),
        0,
        if arg & 1 == 0 {
            Src::Null
        } else {
            Src::Root(2)
        },
        true,
    );
    h.report("replacement", |h| h.load(Src::Root(0), 0));
    h.value(Src::Root(0));
    h.g.mov_reg_reg(Reg::V7, Reg::V0);
    h.value(Src::Raw(5));
    emit_encode_reference(h.g, Reg::V0, Reg::V1);
    h.g.mov_reg_reg(Reg::V6, Reg::V0);
    h.g.load_rbp_slot(Reg::V8, slot_offset(4));
    heal_remembered_slot(h.g, arg & 2 == 0);
    h.report("after", |h| h.load(Src::Root(0), 0));
    h.finish_collection();
    if arg & 1 != 0 {
        h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    }
}

#[test]
fn production_remembered_heal_preserves_a_newer_store_or_null() {
    for arg in 0..4 {
        let o = run(stale_heal, arg, false);
        assert_eq!(o.get("after"), o.get("replacement"));
        if arg & 1 != 0 {
            assert_eq!(o.get("value"), 99);
        }
    }
}

fn real_worker(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    holder(&mut h, SLOTS);
    child(&mut h, 1, 42);
    fill(&mut h);
    h.report_counter("copies_before", Counter::MutatorCopies);
    h.collect();
    h.alloc(1, 16, 2); // Wake the OS worker without running a mutator scan.
    let done = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::PhaseOpen);
        h.g.test_reg_reg(Reg::V0, Reg::V0);
        h.g.jcc_label(Condition::Equal, done);
        h.counter(Counter::WorkerDone);
        h.g.test_reg_reg(Reg::V0, Reg::V0);
        h.g.jcc_label(Condition::NotEqual, done);
        h.g.mov_imm64(Reg::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(done);
    h.report_counter("previous", Counter::RemsetPreviousLen);
    h.report_counter("copies_after", Counter::MutatorCopies);
    h.report_counter("worker_copies", Counter::WorkerCopies);
    h.report_counter("worker_scanned", Counter::WorkerScanned);
    h.finish_collection();
    h.report("last", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
}

#[test]
fn production_worker_consumes_remembered_roots_without_mutator_scanning() {
    let o = run(real_worker, 0, true);
    assert_eq!(o.get("previous"), 0, "{}", o.text());
    assert_eq!(o.get("copies_after"), o.get("copies_before"));
    assert!(o.get("worker_copies") > 0);
    assert!(o.get("worker_scanned") < SLOTS.into());
    assert_eq!(o.get("last"), 42);
}
