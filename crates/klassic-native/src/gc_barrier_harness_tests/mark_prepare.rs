//! Epoch readiness has no heap-wide reset and only the owner starts a snapshot.
use super::{H, Src, run, run_failure};
use crate::gc_codegen::production::test_support::{Counter, set_worker_alive, wake_worker};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use crate::{NativeCodeGenerator, Reg as X};

fn begin(h: &mut H<'_>) {
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.mark_prepare_begin);
    h.g.call_label(h.l.unlock);
}
fn root(h: &mut H<'_>) {
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.value(Src::Raw(0));
    h.keep(1);
    h.root(0);
}
fn metadata_ready(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    begin(&mut h);
    begin(&mut h); // Idempotent before activation.
    h.report_counter("ready", Counter::MarkPrepareState);
    h.report_counter("requests", Counter::MarkReadinessRequests);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("cycles", Counter::MarkCycles);
    h.report_counter("phase", Counter::AdoptedPhase);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.value(Src::Raw(2));
    h.keep(3);
    h.root(2);
    h.value(Src::Null);
    h.set_root(0); // Dropped before the snapshot, so retention is unnecessary.
    h.mark_start();
    h.report_counter("consumed", Counter::MarkPrepareState);
    h.report_counter("started", Counter::MarkCycles);
    h.report_counter("marked_start", Counter::Marked);
    h.drain_marks();
    h.report("old_unrooted", |h| h.marked(Src::Raw(1)));
    h.report("new_root", |h| h.marked(Src::Raw(3)));
    h.report("value", |h| h.load(Src::Root(2), 0));
}
#[test]
fn production_epoch_readiness_is_constant_metadata_without_starting_a_snapshot() {
    let o = run(metadata_ready, 0, false);
    assert_eq!(o.get("ready"), 2);
    assert_eq!(o.get("requests"), 1);
    assert_eq!(o.get("active"), 0);
    assert_eq!(o.get("cycles"), 0);
    assert_eq!(o.get("phase") & 255, 0);
    assert_eq!(o.get("consumed"), 0);
    assert_eq!(o.get("started"), 1);
    assert_eq!(o.get("marked_start"), 0);
    assert_eq!(o.get("old_unrooted"), 0);
    assert_eq!(o.get("new_root"), 1);
    assert_eq!(o.get("value"), 99);
}
fn protect(h: &mut H<'_>, prot: u64) {
    h.value(Src::Raw(1));
    h.g.and_reg_imm32(R::V0, -4096);
    h.g.asm.mov_reg_reg(X::Rdi, X::Rax);
    h.g.asm.mov_imm64(X::Rsi, 4096);
    h.g.asm.mov_imm64(X::Rdx, prot);
    h.g.asm.mov_imm64(X::Rax, 10);
    h.g.asm.syscall();
}
fn protected(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    h.g.call_label(h.l.publish_pending); // Seal private initialization first.
    protect(&mut h, 0); // Neither root traversal nor bulk reset may touch payload.
    h.keep(3);
    h.report("protected", |h| h.value(Src::Raw(3)));
    begin(&mut h);
    h.mark_start();
    h.report_counter("active", Counter::MarkActive);
    protect(&mut h, 3);
    h.keep(3);
    h.report("restored", |h| h.value(Src::Raw(3)));
    h.drain_marks();
    h.report("marked", |h| h.marked(Src::Root(0)));
}
#[test]
fn production_epoch_readiness_and_snapshot_start_do_not_touch_protected_root_payload() {
    let o = run(protected, 0, false);
    assert_eq!(o.get("protected"), 0);
    assert_eq!(o.get("active"), 1);
    assert_eq!(o.get("restored"), 0);
    assert_eq!(o.get("marked"), 1);
}
fn cancel(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    begin(&mut h);
    h.collect();
    h.report_counter("cancelled", Counter::MarkPrepareState);
    h.report_counter("cancels", Counter::MarkPrepareCancels);
    h.report_counter("cycle", Counter::MarkCycles);
    h.report_counter("valid", Counter::MarkValid);
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.mark_start();
    h.drain_marks();
    h.report_counter("requests", Counter::MarkReadinessRequests);
    h.report("marked", |h| h.marked(Src::Root(0)));
}
#[test]
fn production_young_flip_cancels_unstarted_epoch_readiness_and_can_request_again() {
    let o = run(cancel, 0, false);
    assert_eq!(o.get("cancelled"), 0);
    assert_eq!(o.get("cancels"), 1);
    assert_eq!(o.get("cycle"), 0);
    assert_eq!(o.get("valid"), 0);
    assert_eq!(o.get("value"), 42);
    assert_eq!(o.get("requests"), 2);
    assert_eq!(o.get("marked"), 1);
}
fn forbidden(g: &mut NativeCodeGenerator, retire: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    if retire == 0 {
        h.mark_start();
    } else {
        h.collect();
        h.g.call_label(h.l.lock);
        h.g.call_label(h.l.close_relocation);
        h.g.call_label(h.l.unlock);
    }
    begin(&mut h);
}
#[test]
fn production_epoch_readiness_rejects_an_active_mark_or_pending_retirement() {
    for retire in [0, 1] {
        let o = run_failure(forbidden, retire);
        assert_eq!(o.status.code(), Some(1));
        assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
    }
}
fn worker(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    h.report_counter("before_phase", Counter::AdoptedPhase);
    begin(&mut h);
    h.report_counter("ready", Counter::MarkPrepareState);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("cycle", Counter::MarkCycles);
    h.report_counter("phase", Counter::AdoptedPhase);
    h.mark_start();
    wake_worker(h.g, h.l.unpark);
    let done = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::MarkDone);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, done);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(done);
    h.report_counter("traced", Counter::MarkDone);
    h.report_counter("worker_units", Counter::MarkWorkerUnits);
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("marked", |h| h.marked(Src::Root(0)));
}
#[test]
fn production_epoch_readiness_keeps_owner_activation_and_real_worker_tracing_separate() {
    let o = run(worker, 0, true);
    assert_eq!(o.get("ready"), 2);
    assert_eq!(o.get("active"), 0);
    assert_eq!(o.get("cycle"), 0);
    assert_eq!(o.get("phase"), o.get("before_phase"));
    assert_eq!(o.get("traced"), 1);
    assert!(o.get("worker_units") > 0);
    assert_eq!(o.get("value"), 42);
    assert_eq!(o.get("marked"), 1);
}
fn automatic(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    h.repeat(70, |h| h.alloc(1, 120000, 2));
    h.report_counter("cycle", Counter::MarkCycles);
    h.report_counter("requests", Counter::MarkReadinessRequests);
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_allocation_requests_epoch_readiness_then_starts_a_real_worker_cycle() {
    let o = run(automatic, 0, true);
    assert!(o.get("cycle") > 0);
    assert!(o.get("requests") > 0);
    assert_eq!(o.get("value"), 42);
    assert_eq!(o.get("unmarked"), 0);
}
fn descheduled(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    h.repeat(70, |h| h.alloc(1, 120000, 2));
    set_worker_alive(h.g, true);
    h.alloc(1, 16, 2);
    set_worker_alive(h.g, false);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.report_counter("ready", Counter::MarkPrepareState);
    h.report_counter("requests", Counter::MarkReadinessRequests);
    h.report_counter("cycle", Counter::MarkCycles);
    h.report_counter("active", Counter::MarkActive);
    h.report("value", |h| h.load(Src::Raw(2), 0));
    h.mark_start();
    h.drain_marks();
    h.report("marked", |h| h.marked(Src::Root(0)));
}
#[test]
fn production_epoch_readiness_returns_without_waiting_for_a_descheduled_worker() {
    let o = run(descheduled, 0, false);
    assert_eq!(o.get("ready"), 2);
    assert_eq!(o.get("requests"), 1);
    assert_eq!(o.get("cycle"), 0);
    assert_eq!(o.get("active"), 0);
    assert_eq!(o.get("value"), 99);
    assert_eq!(o.get("marked"), 1);
}
