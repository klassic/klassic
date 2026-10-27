//! Start ordinary Young evacuation from genuinely quiescent Old mark work.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{
    Counter, completed_mark_hint, mark_trigger_pages, set_worker_alive, young_mutator_pages,
    young_semispace_pages,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

const SLOTS: u32 = 8192;
fn graph(h: &mut H<'_>) {
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(0), SLOTS - 1, Src::Raw(1), true);
}
fn fill(h: &mut H<'_>) {
    // Disable automatic starts, not the physical worker. All these objects
    // are real allocations and unrooted garbage; no cursor/header is forged.
    set_worker_alive(h.g, false);
    h.repeat(mark_trigger_pages(), |h| h.alloc(1, 120000, 9));
}
fn hint(h: &mut H<'_>) {
    h.g.call_label(h.l.lock);
    completed_mark_hint(h.g);
    set_worker_alive(h.g, true);
    h.g.call_label(h.l.unlock);
}
fn report(h: &mut H<'_>) {
    h.report("pages_after", |h| young_mutator_pages(h.g));
    h.report_counter("sequence", Counter::WorkSequence);
    h.report_counter("wake", Counter::WakePending);
    h.report_counter("checks", Counter::CycleChecks);
    h.report_counter("retries", Counter::CycleRetries);
    h.report_counter("ready", Counter::ReadyCycles);
    h.report_counter("collections", Counter::Collections);
    h.report_counter("drains", Counter::MarkDrainSlices);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("done", Counter::MarkDone);
    h.report_counter("open", Counter::PhaseOpen);
}
fn fresh(h: &mut H<'_>) {
    h.alloc(1, 16, 5); // Exercise the production allocation-trigger call.
    h.set_scalar(Src::Raw(5), 0, 99);
    h.report("fresh", |h| h.load(Src::Raw(5), 0));
    fresh_metadata(h);
}
fn fresh_metadata(h: &mut H<'_>) {
    for (name, offset) in [
        (
            "fresh_bytes",
            std::mem::offset_of!(crate::gc_abi::ObjectHeader, payload_bytes),
        ),
        (
            "fresh_descriptor",
            std::mem::offset_of!(crate::gc_abi::ObjectHeader, descriptor_id),
        ),
    ] {
        h.report(name, |h| {
            h.value(Src::Raw(5));
            h.g.load_ptr_disp32(
                R::V0,
                R::V0,
                offset as i32 - std::mem::size_of::<crate::gc_abi::ObjectHeader>() as i32,
            );
        });
    }
}
fn fresh_record(h: &mut H<'_>) {
    h.alloc(2, 16, 5); // A new shape after the preceding raw garbage.
    h.store(Src::Raw(5), 0, Src::Root(0), true);
    h.report("fresh", |h| {
        h.load_barriered(Src::Raw(5), 0);
        h.keep(6);
        h.object_id(Src::Raw(6));
    });
    fresh_metadata(h);
}
fn finish(h: &mut H<'_>, collect: bool) {
    set_worker_alive(h.g, false);
    if collect {
        h.collect();
    }
    h.finish_collection();
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
    h.report("identity", |h| h.object_id(Src::Root(0)));
    h.report_counter("unmarked", Counter::Unmarked);
}
fn frontiers(g: &mut NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    graph(&mut h);
    if kind != 2 {
        h.root(0);
    }
    if kind == 0 {
        h.repeat(999, |h| {
            h.value(Src::Root(0));
            h.keep(2);
            h.root(2);
        });
    }
    fill(&mut h);
    h.mark_start();
    if kind == 1 {
        h.mark_quantum(2); // Retained object, no queued children yet.
    } else if kind == 2 {
        h.root(0); // A late root beyond the captured bound queues the graph.
    } else if kind == 3 {
        h.drain_marks();
    }
    hint(&mut h);
    h.report_counter("before_index", Counter::MarkRootIndex);
    h.report_counter("before_field", Counter::MarkField);
    h.report_counter("before_object", Counter::MarkObject);
    h.report_counter("before_queue", Counter::MarkStackLen);
    h.report("pages_before", |h| young_mutator_pages(h.g));
    h.report_counter("before_sequence", Counter::WorkSequence);
    if kind == 3 {
        fresh_record(&mut h);
    } else {
        fresh(&mut h);
    }
    report(&mut h);
    h.report_counter("index", Counter::MarkRootIndex);
    h.report_counter("field", Counter::MarkField);
    h.report_counter("object", Counter::MarkObject);
    h.report_counter("queue", Counter::MarkStackLen);
    finish(&mut h, kind != 3);
}
#[test]
fn production_ready_cycle_rearms_each_stale_mark_frontier_without_tracing() {
    for kind in 0..3 {
        let o = run(frontiers, kind, false);
        assert_eq!(o.get("checks"), 1);
        assert_eq!(o.get("retries"), 1);
        assert_eq!(o.get("ready"), 0);
        assert_eq!(o.get("collections"), 0);
        assert_eq!(o.get("drains"), 0);
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("done"), 0);
        assert_eq!(o.get("open"), 0);
        assert_eq!(o.get("pages_after"), o.get("pages_before"));
        assert_eq!(o.get("sequence"), o.get("before_sequence") + 1);
        assert_eq!(o.get("wake"), 0);
        for key in ["index", "field", "object", "queue"] {
            assert_eq!(o.get(key), o.get(&format!("before_{key}")));
        }
        assert_eq!(o.get("fresh"), 99);
        assert_eq!(o.get("fresh_bytes"), 16);
        assert_eq!(o.get("fresh_descriptor"), 1);
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("identity"), 1);
        assert_eq!(o.get("unmarked"), 0);
    }
}
#[test]
fn production_ready_cycle_collects_a_completed_snapshot_from_normal_allocation_without_mark_drain()
{
    let o = run(frontiers, 3, false);
    assert_eq!(o.get("checks"), 1);
    assert_eq!(o.get("retries"), 0);
    assert_eq!(o.get("ready"), 1);
    assert_eq!(o.get("collections"), 1);
    assert_eq!(o.get("drains"), 0);
    assert_eq!(o.get("active"), 1);
    assert_eq!(o.get("done"), 0);
    assert_eq!(o.get("open"), 1);
    assert!(o.get("pages_before") >= mark_trigger_pages() as i64);
    assert!(o.get("pages_before") < young_semispace_pages() as i64);
    assert!(o.get("pages_after") < o.get("pages_before"));
    assert_eq!(o.get("fresh"), 1);
    assert_eq!(o.get("fresh_bytes"), 16);
    assert_eq!(o.get("fresh_descriptor"), 2);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("identity"), 1);
    assert_eq!(o.get("unmarked"), 0);
}
fn guards(g: &mut NativeCodeGenerator, mode: u64) {
    let mut h = H::begin(g);
    graph(&mut h);
    h.root(0);
    if mode != 0 {
        fill(&mut h);
    }
    if mode != 3 {
        h.mark_start();
    }
    if mode != 1 {
        h.g.call_label(h.l.lock);
        completed_mark_hint(h.g);
        h.g.call_label(h.l.unlock);
    }
    set_worker_alive(h.g, mode != 2);
    h.g.call_label(h.l.mark_cycle_poll);
    fresh(&mut h);
    report(&mut h);
    finish(&mut h, true);
}
#[test]
fn production_ready_cycle_requires_the_threshold_live_worker_and_completion_hint() {
    for mode in 0..4 {
        let o = run(guards, mode, false);
        assert_eq!(o.get("checks"), 0);
        assert_eq!(o.get("ready"), 0);
        assert_eq!(o.get("collections"), 0);
        assert_eq!(o.get("drains"), 0);
        assert_eq!(o.get("fresh"), 99);
        assert_eq!(o.get("fresh_bytes"), 16);
        assert_eq!(o.get("fresh_descriptor"), 1);
        assert_eq!(o.get("value"), 84);
    }
}
fn real_worker(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    graph(&mut h);
    h.root(0);
    // Ordinary allocations alone cross the trigger and request preparation.
    // No explicit mark_start, forged hint or worker-alive flag in this fixture.
    h.repeat(mark_trigger_pages(), |h| h.alloc(1, 120000, 9));
    let prepared = h.g.create_text_label();
    let done = h.g.create_text_label();
    let missing = h.g.create_text_label();
    let exit = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::MarkPrepareState);
        h.g.cmp_reg_imm8(R::V0, 2);
        h.g.jcc_label(C::Equal, prepared);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.jmp_label(missing);
    h.g.bind_text_label(prepared);
    h.report_counter("prepared", Counter::MarkPrepareState);
    h.report_counter("cycles_before", Counter::MarkCycles);
    h.alloc(1, 16, 4); // Consume Ready and activate the snapshot normally.
    h.report_counter("cycles_started", Counter::MarkCycles);
    h.repeat(1_000_000, |h| {
        h.counter(Counter::MarkDone);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, done);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.jmp_label(missing);
    h.g.bind_text_label(done);
    h.report("completed", |h| h.value(Src::Imm(1)));
    h.report_counter("mark_worker", Counter::MarkWorkerUnits);
    h.report("pages_before", |h| young_mutator_pages(h.g));
    fresh_record(&mut h);
    report(&mut h);
    // Keep the physical worker enabled through actual relocation and cleanup.
    h.finish_collection();
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
    h.report("identity", |h| h.object_id(Src::Root(0)));
    h.report_counter("unmarked", Counter::Unmarked);
    h.report("member", |h| h.marked(Src::Root(0)));
    h.g.jmp_label(exit);
    h.g.bind_text_label(missing);
    h.report("completed", |h| h.value(Src::Imm(0)));
    h.g.bind_text_label(exit);
}
#[test]
fn production_ready_cycle_uses_real_worker_mark_completion_before_ordinary_allocation_exhausts() {
    let o = run(real_worker, 0, true);
    assert_eq!(o.get("completed"), 1, "{}", o.text());
    assert_eq!(o.get("member"), 1);
    assert_eq!(o.get("prepared"), 2);
    assert_eq!(o.get("cycles_before"), 0);
    assert_eq!(o.get("cycles_started"), 1);
    assert!(o.get("mark_worker") > 0);
    assert_eq!(o.get("checks"), 1);
    assert_eq!(o.get("ready"), 1);
    assert_eq!(o.get("retries"), 0);
    assert_eq!(o.get("collections"), 1);
    assert_eq!(o.get("drains"), 0);
    assert_eq!(o.get("active"), 1);
    assert_eq!(o.get("open"), 1);
    assert!(o.get("pages_before") >= mark_trigger_pages() as i64);
    assert!(o.get("pages_before") < young_semispace_pages() as i64);
    assert!(o.get("pages_after") < o.get("pages_before"));
    assert_eq!(o.get("fresh"), 1);
    assert_eq!(o.get("fresh_bytes"), 16);
    assert_eq!(o.get("fresh_descriptor"), 2);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("identity"), 1);
    assert_eq!(o.get("unmarked"), 0);
}
