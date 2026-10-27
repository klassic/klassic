//! Normal allocation end checks return pending work to concurrent relocation.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{
    Counter, completed_relocation_hint, set_worker_alive, wake_worker,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

const SLOTS: u32 = 8192;
fn graph(h: &mut H<'_>, slot: u32) {
    h.alloc(2, u64::from(SLOTS) * 8, slot);
    h.alloc(1, 16, 3);
    h.set_scalar(Src::Raw(3), 0, 84);
    h.store(Src::Raw(slot), SLOTS - 1, Src::Raw(3), true);
}
fn observe(h: &mut H<'_>) {
    h.report_counter("sequence", Counter::WorkSequence);
    h.report_counter("wake", Counter::WakePending);
    h.report_counter("checks", Counter::RelocateEndChecks);
    h.report_counter("retries", Counter::RelocateEndRetries);
    h.report_counter("drains", Counter::RelocateDrainSlices);
    h.report_counter("open", Counter::PhaseOpen);
    h.report_counter("retire", Counter::RetireStage);
    h.report_counter("done", Counter::WorkerDone);
    h.report_counter("field", Counter::RelocateField);
    h.report_counter("index", Counter::RelocateRootIndex);
    h.report_counter("kind", Counter::RelocateKind);
}
fn try_close(h: &mut H<'_>) {
    h.g.call_label(h.l.lock);
    completed_relocation_hint(h.g);
    h.g.call_label(h.l.try_close_relocation);
    h.g.call_label(h.l.unlock);
}
fn frontiers(g: &mut NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    graph(&mut h, 0);
    h.root(0);
    if kind == 3 {
        h.collect();
        h.finish_collection();
    }
    h.counter(Counter::RelocateDrainSlices);
    h.keep(30);
    h.collect();
    if kind != 0 {
        h.relocate_quantum(if kind == 4 { 2 } else { 1 }, kind == 2 || kind == 4);
    }
    h.report_counter("before_field", Counter::RelocateField);
    h.report_counter("before_index", Counter::RelocateRootIndex);
    h.report_counter("before_kind", Counter::RelocateKind);
    h.report_counter("before_sequence", Counter::WorkSequence);
    try_close(&mut h);
    observe(&mut h);
    h.report("drain_delta", |h| {
        h.counter(Counter::RelocateDrainSlices);
        h.g.load_rbp_slot(R::V1, super::slot_offset(30));
        h.g.sub_reg_reg(R::V0, R::V1);
    });
    h.finish_collection();
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
}
#[test]
fn production_end_check_rearms_root_young_old_and_partial_field_frontiers_without_tracing() {
    for kind in 0..5 {
        let o = run(frontiers, kind, false);
        assert_eq!(o.get("checks"), 1);
        assert_eq!(o.get("retries"), 1);
        assert_eq!(o.get("drain_delta"), 0);
        assert_eq!(o.get("sequence"), o.get("before_sequence") + 1);
        assert_eq!(o.get("wake"), 1);
        assert_eq!(o.get("open"), 1);
        assert_eq!(o.get("retire"), 0);
        assert_eq!(o.get("done"), 0);
        for field in ["field", "index", "kind"] {
            assert_eq!(o.get(field), o.get(&format!("before_{field}")));
        }
        assert_eq!(o.get("value"), 84);
    }
}

fn late_page(g: &mut NativeCodeGenerator, worker: u64) {
    let mut h = H::begin(g);
    // Two large raw objects fill the first copy page far enough that the
    // late 64-KiB record must bind another page. The original cursor is at
    // the old page's end, so its offset alone cannot detect the new frontier.
    for slot in 0..2 {
        h.alloc(1, 60000, slot);
        h.root(slot);
    }
    graph(&mut h, 2); // Hidden until a late production barrier copy.
    h.collect();
    h.relocate_quantum(64, worker != 0);
    h.keep(6);
    h.report("traced", |h| h.value(Src::Raw(6)));
    if worker != 0 {
        h.g.call_label(h.l.lock);
        h.worker_copy(Src::Raw(2));
        h.keep(2);
        h.g.call_label(h.l.unlock);
    } else {
        h.root(2); // Late root adoption resolves to a new mutator page.
    }
    h.g.call_label(h.l.lock);
    completed_relocation_hint(h.g);
    set_worker_alive(h.g, true); // Deterministically descheduled worker.
    h.g.call_label(h.l.unlock);
    h.alloc(1, 16, 7); // The real normal allocation must return, with work pending.
    h.set_scalar(Src::Raw(7), 0, 99);
    observe(&mut h);
    h.report("fresh", |h| h.load(Src::Raw(7), 0));
    set_worker_alive(h.g, false);
    h.finish_collection();
    h.report("value", |h| {
        h.deref_field(
            if worker != 0 {
                Src::Raw(2)
            } else {
                Src::Root(2)
            },
            SLOTS - 1,
            0,
        );
    });
}
#[test]
fn production_normal_allocation_rejects_stale_worker_done_after_either_copy_chain_grows() {
    for worker in [0, 1] {
        let o = run(late_page, worker, false);
        assert_eq!(o.get("traced"), 1);
        assert_eq!(o.get("checks"), 1);
        assert_eq!(o.get("retries"), 1);
        assert_eq!(o.get("drains"), 0);
        assert_eq!(o.get("open"), 1);
        assert_eq!(o.get("retire"), 0);
        assert_eq!(o.get("done"), 0);
        assert_eq!(o.get("fresh"), 99);
        assert_eq!(o.get("value"), 84);
    }
}

fn real_worker(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    graph(&mut h, 0);
    h.collect();
    h.g.call_label(h.l.lock);
    h.worker_copy(Src::Raw(0));
    h.keep(2);
    completed_relocation_hint(h.g);
    h.g.call_label(h.l.try_close_relocation);
    observe(&mut h);
    h.g.call_label(h.l.unlock);
    wake_worker(h.g, h.l.unpark);
    let done = h.g.create_text_label();
    h.g.mov_imm64(R::V0, 0);
    h.keep(7);
    h.repeat(1_000_000, |h| {
        h.value(Src::Raw(7));
        h.g.add_reg_imm32(R::V0, 1);
        h.keep(7);
        h.counter(Counter::WorkerDone);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, done);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(done);
    h.report("progress", |h| h.value(Src::Raw(7)));
    h.report_counter("worker_done", Counter::WorkerDone);
    h.report_counter("worker_units", Counter::RelocateWorkerUnits);
    h.g.call_label(h.l.quantum);
    h.report_counter("closed", Counter::PhaseOpen);
    h.report_counter("drains_after", Counter::RelocateDrainSlices);
    h.finish_collection(); // Prove repair survives actual source cleanup too.
    h.report("value", |h| h.deref_field(Src::Raw(2), SLOTS - 1, 0));
}
#[test]
fn production_end_check_returns_to_mutator_then_real_worker_finishes_before_metadata_only_close() {
    let o = run(real_worker, 0, true);
    assert_eq!(o.get("checks"), 1);
    assert_eq!(o.get("retries"), 1);
    assert_eq!(o.get("drains"), 0);
    assert_eq!(o.get("open"), 1);
    assert_eq!(o.get("retire"), 0);
    assert!(o.get("progress") > 0);
    assert_eq!(o.get("worker_done"), 1, "{}", o.text());
    assert!(o.get("worker_units") > 0);
    assert_eq!(o.get("closed"), 0);
    assert_eq!(o.get("drains_after"), 0);
    assert_eq!(o.get("value"), 84);
}

fn wake_before_return(g: &mut NativeCodeGenerator, stale_hint: u64) {
    let mut h = H::begin(g);
    graph(&mut h, 0);
    h.root(0);
    h.collect();
    h.relocate_quantum(1, false);
    h.g.call_label(h.l.lock);
    set_worker_alive(h.g, true); // Descheduled worker, no scheduling-dependent wait.
    if stale_hint != 0 {
        completed_relocation_hint(h.g);
    }
    h.g.call_label(h.l.unlock);
    h.g.call_label(h.l.quantum); // Exactly the call used inside refill's wait loop.
    observe(&mut h);
    set_worker_alive(h.g, false);
    h.finish_collection();
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
}
#[test]
fn production_end_check_quantum_delivers_pending_wake_before_refill_can_wait_again() {
    for stale_hint in [0, 1] {
        let o = run(wake_before_return, stale_hint, false);
        assert_eq!(
            o.get("wake"),
            0,
            "wake must not wait for allocation return: {}",
            o.text()
        );
        assert_eq!(o.get("checks"), stale_hint as i64);
        assert_eq!(o.get("retries"), stale_hint as i64);
        assert_eq!(o.get("done"), 0);
        assert_eq!(o.get("open"), 1);
        assert_eq!(o.get("retire"), 0);
        assert_eq!(o.get("drains"), 0);
        assert_eq!(o.get("value"), 84);
    }
}
