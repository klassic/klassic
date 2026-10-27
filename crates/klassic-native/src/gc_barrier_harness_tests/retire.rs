//! Quiescent owner handoff, resumable dead-space cleanup and safe reuse.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_abi::{AllocationPage, MarkRecord, ObjectHeader, PAGE_READY};
use crate::gc_codegen::production::test_support::{
    Counter, RETIRE_DONE, emit_budget, retiring_page,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

fn close(h: &mut H<'_>) {
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    h.g.call_label(h.l.unlock);
}

fn quantum(h: &mut H<'_>, units: u64) {
    h.g.call_label(h.l.lock);
    emit_budget(h.g, units);
    h.g.call_label(h.l.retire_step);
    h.g.call_label(h.l.unlock);
}

fn setup(h: &mut H<'_>) {
    h.alloc(1, 65536, 0); // Dead payload: its bytes must survive until Retire.
    h.set_scalar(Src::Raw(0), 0, 77);
    h.set_scalar(Src::Raw(0), 8191, 100);
    h.value(Src::Raw(0));
    h.g.load_ptr_disp32(
        R::V0,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    h.keep(8);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 42);
    h.root(1);
    h.collect();
}

fn budgeted(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    let invalid = h.g.create_text_label();
    setup(&mut h);
    quantum(&mut h, 100); // A worker cannot clean before the owner acknowledges.
    h.report_counter("before_stage", Counter::RetireStage);
    h.report("before", |h| h.load(Src::Raw(0), 0));
    close(&mut h);
    h.report_counter("stage", Counter::RetireStage);
    h.report_counter("open", Counter::PhaseOpen);
    h.report_counter("retire_phase", Counter::AdoptedPhase);
    h.report("after_ack", |h| h.load(Src::Raw(0), 0));
    retiring_page(h.g);
    h.keep(7);
    h.report("incarnation_before", |h| {
        h.load(
            Src::Raw(7),
            (offset_of!(AllocationPage, incarnation) / 8) as u32,
        )
    });
    quantum(&mut h, 0);
    h.keep(4);
    h.report("zero_done", |h| h.value(Src::Raw(4)));
    h.report_counter("units0", Counter::RetireUnits);
    quantum(&mut h, 1); // Admit the first source page, without zeroing it.
    h.report_counter("payload_stage", Counter::RetireStage);
    h.report_counter("cursor1", Counter::RetireCursor);
    h.counter(Counter::RetireCursor);
    h.keep(6);
    h.report_counter("end1", Counter::RetireEnd);
    h.report_counter("page1", Counter::RetirePage);
    h.report_counter("units1", Counter::RetireUnits);
    h.report("not_ready", |h| {
        h.load(Src::Raw(7), (offset_of!(AllocationPage, state) / 8) as u32)
    });
    quantum(&mut h, 7);
    h.report_counter("cursor8", Counter::RetireCursor);
    h.report_counter("units8", Counter::RetireUnits);
    // A broken cursor must fail an observation, not enter finish's resource
    // wait forever in a mutation test. Guard before attempting later cleanup.
    h.counter(Counter::RetireCursor);
    h.g.load_rbp_slot(R::V1, super::slot_offset(6));
    h.g.add_reg_imm32(R::V1, 56);
    h.g.cmp_reg_reg(R::V0, R::V1);
    h.g.jcc_label(C::NotEqual, invalid);
    h.report("first_zero", |h| h.load(Src::Raw(0), 0));
    h.report("last_unvisited", |h| h.load(Src::Raw(0), 8191));
    h.alloc(1, 16, 2); // One cleanup quantum, then return ordinary fresh work.
    h.set_scalar(Src::Raw(2), 0, 99);
    h.root(2);
    h.report_counter("units_alloc", Counter::RetireUnits);
    h.report_counter("stage_alloc", Counter::RetireStage);
    h.report("mutator", |h| h.load(Src::Root(2), 0));
    h.repeat(150, |h| quantum(h, 64));
    h.report_counter("done_stage", Counter::RetireStage);
    h.report_counter("worker_phase", Counter::AdoptedPhase);
    h.report("last_zero", |h| h.load(Src::Raw(0), 8191));
    h.report("ready", |h| {
        h.load(Src::Raw(7), (offset_of!(AllocationPage, state) / 8) as u32)
    });
    h.report("incarnation_after", |h| {
        h.load(
            Src::Raw(7),
            (offset_of!(AllocationPage, incarnation) / 8) as u32,
        )
    });
    h.report("record_zero", |h| {
        h.load(Src::Raw(8), (offset_of!(MarkRecord, object_id) / 8) as u32)
    });
    h.alloc(1, 16, 3); // Owner adopts Idle, publishing its pending allocation.
    h.report_counter("idle_stage", Counter::RetireStage);
    h.report_counter("idle_phase", Counter::AdoptedPhase);
    h.collect(); // Only now may the cleaned half be rebound.
    h.report_counter("next_stage", Counter::RetireStage);
    h.finish_collection();
    h.report("live", |h| h.load(Src::Root(1), 0));
    h.report("fresh", |h| h.load(Src::Root(2), 0));
    h.g.bind_text_label(invalid);
}

#[test]
fn production_retirement_preserves_pre_ack_bytes_and_budgets_cleanup_before_reuse() {
    let o = run(budgeted, 0, false);
    assert_eq!(o.get("before_stage"), 0);
    assert_eq!(o.get("before"), 77);
    assert_eq!(o.get("stage"), 1);
    assert_eq!(o.get("open"), 0);
    assert_eq!(o.get("retire_phase") & 255, 7);
    assert_eq!(o.get("after_ack"), 77);
    assert_eq!(o.get("zero_done"), 0);
    assert_eq!(o.get("units0"), 0);
    assert_eq!(o.get("payload_stage"), 3);
    assert_eq!(o.get("units1"), 1);
    assert!(o.get("end1") - o.get("cursor1") > 65536);
    assert_eq!(o.get("page1"), 0);
    assert_ne!(o.get("not_ready"), PAGE_READY as i64);
    assert_eq!(o.get("cursor8") - o.get("cursor1"), 56);
    assert_eq!(o.get("units8"), 8);
    assert_eq!(o.get("first_zero"), 0);
    assert_eq!(o.get("last_unvisited"), 100);
    assert_eq!(o.get("units_alloc"), 72);
    assert_eq!(o.get("stage_alloc"), 3);
    assert_eq!(o.get("mutator"), 99);
    assert_eq!(o.get("done_stage"), RETIRE_DONE as i64);
    assert_eq!(o.get("worker_phase") & 255, 7);
    assert_eq!(o.get("last_zero"), 0);
    assert_eq!(o.get("ready"), PAGE_READY as i64);
    assert_eq!(
        o.get("incarnation_after") - o.get("incarnation_before"),
        crate::gc_codegen::production::test_support::pool_pages() as i64
    );
    assert_eq!(o.get("record_zero"), 0);
    assert_eq!(o.get("idle_stage"), 0);
    assert_eq!(o.get("idle_phase") & 255, 0);
    assert_eq!(o.get("next_stage"), 0);
    assert_eq!(o.get("live"), 42);
    assert_eq!(o.get("fresh"), 99);
}

fn worker_pages(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    h.collect();
    h.relocate_quantum(1, true); // Put its first survivor in the worker chain.
    h.finish_collection();
    h.value(Src::Root(0));
    h.keep(2); // The next cycle's actual chain-B source payload.
    h.collect();
    close(&mut h);
    let found = h.g.create_text_label();
    h.repeat(500, |h| {
        quantum(h, 1);
        h.counter(Counter::RetireStage);
        h.g.cmp_reg_imm8(R::V0, 4);
        h.g.jcc_label(C::Equal, found);
    });
    h.g.bind_text_label(found);
    h.report_counter("chain_b", Counter::RetireStage);
    h.report("before", |h| h.load(Src::Raw(2), 0));
    quantum(&mut h, 6); // Four header words plus the two payload words.
    h.report("zeroed", |h| h.load(Src::Raw(2), 0));
    h.finish_collection();
    h.report("live", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_retirement_cleans_worker_private_pages_after_owner_handoff() {
    let o = run(worker_pages, 0, false);
    assert_eq!(o.get("chain_b"), 4, "{}", o.text());
    assert_eq!(o.get("before"), 42);
    assert_eq!(o.get("zeroed"), 0);
    assert_eq!(o.get("live"), 42);
}

fn old_span(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 42);
    h.root(2);
    h.gc_until(2);
    h.settle();
    let top = h.g.create_text_label();
    h.g.bind_text_label(top);
    h.alloc(2, 30000, 1);
    h.store(Src::Raw(1), 0, Src::Root(0), true);
    h.value(Src::Raw(1));
    h.set_root(0);
    h.alloc(1, 60000, 1);
    h.counter(Counter::FullCollections);
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::Equal, top);
    close(&mut h);
    let found = h.g.create_text_label();
    h.repeat(200_000, |h| {
        quantum(h, 64);
        h.counter(Counter::RetireStage);
        h.g.cmp_reg_imm8(R::V0, 10);
        h.g.jcc_label(C::Equal, found);
    });
    h.g.bind_text_label(found);
    h.report_counter("old_stage", Counter::RetireStage);
    let invalid = h.g.create_text_label();
    h.counter(Counter::RetireStage);
    h.g.cmp_reg_imm8(R::V0, 10);
    h.g.jcc_label(C::NotEqual, invalid);
    h.counter(Counter::RetireCursor);
    h.keep(3);
    // Poison only dead Old bytes at the retained cursor. No live object or
    // publication is edited; the next seven words must actually be cleared.
    h.g.mov_imm64(R::V1, 777);
    for index in 0..8 {
        h.g.store_ptr_disp32(R::V0, index * 8, R::V1);
    }
    h.report_counter("before", Counter::RetireUnits);
    quantum(&mut h, 7);
    h.report_counter("after", Counter::RetireUnits);
    h.report_counter("cursor", Counter::RetireCursor);
    h.report("first", |h| h.load(Src::Raw(3), 0));
    h.report("seventh", |h| h.load(Src::Raw(3), 6));
    h.report("eighth", |h| h.load(Src::Raw(3), 7));
    h.finish_collection();
    h.report_counter("idle", Counter::RetireStage);
    h.report("age", |h| {
        h.object_metadata(Src::Root(2), offset_of!(MarkRecord, age))
    });
    h.report("anchor", |h| h.load(Src::Root(2), 0));
    h.g.bind_text_label(invalid);
}

#[test]
fn production_retirement_splits_the_real_full_collection_old_span_by_words() {
    let o = run(old_span, 0, false);
    assert_eq!(o.get("old_stage"), 10);
    assert_eq!(o.get("after") - o.get("before"), 7);
    assert_eq!(o.get("first"), 0);
    assert_eq!(o.get("seventh"), 0);
    assert_eq!(o.get("eighth"), 777);
    assert_eq!(o.get("idle"), 0);
    assert!(o.get("age") >= 2);
    assert_eq!(o.get("anchor"), 42);
}

fn premature_mark(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    setup(&mut h);
    close(&mut h);
    h.mark_start();
}

#[test]
fn production_mark_start_rejects_pending_retirement() {
    let o = run_failure(premature_mark, 0);
    assert_eq!(o.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
}

fn real_worker(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    setup(&mut h);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.root(2);
    close(&mut h);
    // Wake without an owner allocation after handoff: even a preempted owner
    // must still see Retire after the worker reaches cleanup-complete.
    crate::gc_codegen::production::test_support::wake_worker(h.g, h.l.unpark);
    let done = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::RetireStage);
        h.g.cmp_reg_imm8(R::V0, RETIRE_DONE as i8);
        h.g.jcc_label(C::Equal, done);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(done);
    h.report_counter("done", Counter::RetireStage);
    h.report_counter("phase", Counter::AdoptedPhase);
    h.report("dead_zero", |h| h.load(Src::Raw(0), 8191));
    h.report("fresh", |h| h.load(Src::Root(2), 0));
    h.alloc(1, 16, 3); // Only the owner may adopt Idle.
    h.report_counter("idle", Counter::RetireStage);
    h.report_counter("idle_phase", Counter::AdoptedPhase);
    h.report("live", |h| h.load(Src::Root(1), 0));
}

#[test]
fn production_worker_cleans_retired_space_without_adopting_the_owner_phase() {
    let o = run(real_worker, 0, true);
    assert_eq!(o.get("done"), RETIRE_DONE as i64, "{}", o.text());
    assert_eq!(o.get("phase") & 255, 7);
    assert_eq!(o.get("dead_zero"), 0);
    assert_eq!(o.get("fresh"), 99);
    assert_eq!(o.get("idle"), 0);
    assert_eq!(o.get("idle_phase") & 255, 0);
    assert_eq!(o.get("live"), 42);
}

fn next_flip(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    setup(&mut h);
    close(&mut h);
    quantum(&mut h, 7);
    h.report_counter("pending", Counter::RetireStage);
    h.collect(); // Must finish old cleanup before replacing its saved ranges.
    h.report_counter("stage", Counter::RetireStage);
    h.report_counter("cycles", Counter::Collections);
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(1), 0));
    h.report("id", |h| h.object_id(Src::Root(1)));
}

#[test]
fn production_next_flip_waits_for_pending_cleanup_before_reusing_its_ranges() {
    let o = run(next_flip, 0, false);
    assert_eq!(o.get("pending"), 3);
    assert_eq!(o.get("stage"), 0);
    assert_eq!(o.get("cycles"), 2);
    assert_eq!(o.get("value"), 42);
    assert_eq!(o.get("id"), 2);
}
