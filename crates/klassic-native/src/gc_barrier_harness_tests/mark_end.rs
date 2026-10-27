//! Mark-end frontier completion, finite slices and a deterministic lock handoff.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{
    Counter, enqueue_mark, waiting_ticket, wake_worker,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

const SLOTS: u32 = 8192;

fn graph(h: &mut H<'_>) {
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(0), SLOTS - 1, Src::Raw(1), true);
}

fn frontiers(g: &mut NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    if kind == 0 {
        h.alloc(1, 16, 1);
        h.set_scalar(Src::Raw(1), 0, 42);
        h.repeat(1000, |h| {
            h.value(Src::Raw(1));
            h.keep(0);
            h.root(0);
        });
    } else {
        graph(&mut h);
        if kind == 1 {
            h.root(0);
        }
    }
    h.mark_start();
    if kind == 1 {
        h.mark_quantum(2);
    } // Root and admission; active cursor only.
    if kind == 2 {
        h.root(0);
    } // A new strong root beyond the captured bound.
    h.report_counter("before_object", Counter::MarkObject);
    h.report_counter("before_queue", Counter::MarkStackLen);
    h.report_counter("before_index", Counter::MarkRootIndex);
    h.report_counter("before_bound", Counter::MarkRootBound);
    crate::gc_codegen::production::test_support::request_full_collection(h.g);
    h.collect(); // A full Old move must finish the shared mark before flipping.
    h.report_counter("slices", Counter::MarkDrainSlices);
    h.report_counter("yields", Counter::MarkDrainYields);
    h.report_counter("max", Counter::MarkDrainMax);
    h.report_counter("assisted", Counter::MarkDrainAssisted);
    h.report_counter("drains", Counter::MarkDrains);
    h.report_counter("valid", Counter::MarkValid);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("object", Counter::MarkObject);
    h.report_counter("queue", Counter::MarkStackLen);
    h.report("marked", |h| h.marked(Src::Raw(1)));
    h.finish_collection();
    h.report("value", |h| {
        if kind == 0 {
            h.load(Src::Root(0), 0);
        } else {
            h.deref_field(Src::Root(0), SLOTS - 1, 0);
        }
    });
    h.report_counter("unmarked", Counter::Unmarked);
}

#[test]
fn production_full_move_slices_mark_root_object_and_queue_frontiers_before_flip() {
    for kind in 0..3 {
        let o = run(frontiers, kind, false);
        if kind == 0 {
            assert_eq!(o.get("before_index"), 1);
            assert_eq!(o.get("before_bound"), 1001);
        } else {
            assert_eq!(o.get("before_index"), o.get("before_bound"));
            assert_eq!(o.get("before_object") > 0, kind == 1);
            assert_eq!(o.get("before_queue") > 0, kind == 2);
        }
        assert!(o.get("slices") > 1, "{}", o.text());
        assert_eq!(o.get("yields"), o.get("slices") - 1);
        assert_eq!(o.get("max"), 64);
        assert_eq!(o.get("assisted"), 0);
        assert_eq!(o.get("drains"), 1);
        assert_eq!(o.get("valid"), 1);
        assert_eq!(o.get("active"), 0);
        assert_eq!(o.get("object"), 0);
        assert_eq!(o.get("queue"), 0);
        assert_eq!(o.get("marked"), 1);
        assert_eq!(o.get("value"), if kind == 0 { 42 } else { 84 });
        assert_eq!(o.get("unmarked"), 0);
    }
}

fn already_traced(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    h.mark_start();
    h.drain_marks(); // Empty graph and root frontiers, still an active cycle.
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.mark_end);
    h.g.call_label(h.l.unlock);
    h.report_counter("slices", Counter::MarkDrainSlices);
    h.report_counter("yields", Counter::MarkDrainYields);
    h.report_counter("drains", Counter::MarkDrains);
    h.report_counter("valid", Counter::MarkValid);
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_mark_end_with_empty_frontiers_validates_without_spurious_slices() {
    let o = run(already_traced, 0, false);
    assert_eq!(o.get("slices"), 0);
    assert_eq!(o.get("yields"), 0);
    assert_eq!(o.get("drains"), 0);
    assert_eq!(o.get("valid"), 1);
    assert_eq!(o.get("value"), 42);
}

fn worker_handoff(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    graph(&mut h);
    h.mark_start();
    h.g.call_label(h.l.lock);
    h.value(Src::Raw(0));
    enqueue_mark(h.g, h.l.mark_ref);
    wake_worker(h.g, h.l.unpark);
    let queued = h.g.create_text_label();
    let done = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        waiting_ticket(h.g);
        h.g.jcc_label(C::Above, queued);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.call_label(h.l.unlock);
    h.report("queued", |h| h.value(Src::Imm(0)));
    h.g.jmp_label(done); // A missing worker fails an observation, not an endless wait.
    h.g.bind_text_label(queued);
    h.report("queued", |h| h.value(Src::Imm(1)));
    // The worker has a ticket, but cannot trace the admitted graph until
    // mark_end releases this lock after its first bounded slice. Fair ticket
    // order puts that worker ahead of the owner's reacquisition, on one CPU too.
    h.g.call_label(h.l.mark_end);
    h.g.call_label(h.l.unlock);
    h.report_counter("slices", Counter::MarkDrainSlices);
    h.report_counter("max", Counter::MarkDrainMax);
    h.report_counter("yields", Counter::MarkDrainYields);
    h.report_counter("assisted", Counter::MarkDrainAssisted);
    h.report_counter("worker", Counter::MarkWorkerUnits);
    h.report_counter("valid", Counter::MarkValid);
    h.report("child", |h| h.marked(Src::Raw(1)));
    h.report("value", |h| h.load(Src::Raw(1), 0));
    h.g.bind_text_label(done);
}
#[test]
fn production_mark_end_releases_its_lock_to_an_already_waiting_real_worker() {
    let o = run(worker_handoff, 0, true);
    assert_eq!(o.get("queued"), 1, "{}", o.text());
    assert!(o.get("slices") >= 2);
    assert_eq!(o.get("max"), 64);
    assert!(o.get("yields") > 0);
    assert!(o.get("assisted") > 0, "{}", o.text());
    assert!(o.get("worker") >= o.get("assisted"));
    assert_eq!(o.get("valid"), 1);
    assert_eq!(o.get("child"), 1);
    assert_eq!(o.get("value"), 84);
}
