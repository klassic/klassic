//! Explicit relocation completion keeps the shared frontier across lock turns.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{
    Counter, reopen_relocation_work, waiting_ticket, wake_worker,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

const SLOTS: u32 = 8192;

fn graph(h: &mut H<'_>) {
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(0), SLOTS - 1, Src::Raw(1), true);
}
fn baseline(h: &mut H<'_>) {
    h.counter(Counter::RelocateDrainSlices);
    h.keep(30);
    h.counter(Counter::RelocateDrainYields);
    h.keep(31);
}
fn delta(h: &mut H<'_>, name: &str, counter: Counter, slot: u32) {
    h.report(name, |h| {
        h.counter(counter);
        h.g.load_rbp_slot(R::V1, super::slot_offset(slot));
        h.g.sub_reg_reg(R::V0, R::V1);
    });
}
fn frontiers(g: &mut NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    if kind == 0 {
        h.alloc(1, 16, 0);
        h.set_scalar(Src::Raw(0), 0, 84);
        h.repeat(1000, |h| {
            h.value(Src::Raw(0));
            h.keep(1);
            h.root(1);
        });
    } else {
        graph(&mut h);
        h.root(0);
        if kind >= 3 {
            h.collect();
            h.finish_collection();
        }
        if kind == 4 {
            h.collect();
            h.finish_collection();
            h.alloc(1, 16, 1);
            h.set_scalar(Src::Raw(1), 0, 84);
            for field in 0..1000 {
                h.store(Src::Root(0), field, Src::Raw(1), true);
            }
        }
    }
    baseline(&mut h);
    h.collect();
    if kind != 0 && kind != 4 {
        super::relocate_slots::admit(&mut h, kind, kind == 2);
    }
    h.report_counter("kind", Counter::RelocateKind);
    h.report_counter("previous", Counter::RemsetPreviousLen);
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    h.g.call_label(h.l.unlock);
    delta(&mut h, "slices", Counter::RelocateDrainSlices, 30);
    delta(&mut h, "yields", Counter::RelocateDrainYields, 31);
    h.report_counter("max", Counter::RelocateDrainMax);
    h.report_counter("assisted", Counter::RelocateDrainAssisted);
    h.report_counter("open", Counter::PhaseOpen);
    h.report_counter("retire", Counter::RetireStage);
    h.report_counter("completed_kind", Counter::RelocateKind);
    h.report_counter("index", Counter::RelocateRootIndex);
    h.report_counter("bound", Counter::RelocateRootBound);
    h.report_counter("remset_index", Counter::RemsetIndex);
    h.finish_collection();
    delta(&mut h, "slices_after", Counter::RelocateDrainSlices, 30);
    h.report("value", |h| {
        if kind == 0 {
            h.load(Src::Root(1), 0);
        } else {
            h.deref_field(Src::Root(0), if kind == 4 { 999 } else { SLOTS - 1 }, 0);
        }
    });
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_relocation_end_slices_pending_roots_and_all_three_object_chains() {
    for kind in 0..5 {
        let o = run(frontiers, kind, false);
        assert_eq!(
            o.get("kind"),
            if kind == 4 { 0 } else { kind as i64 },
            "{}",
            o.text()
        );
        if kind == 4 {
            assert_eq!(o.get("previous"), 1000);
            assert_eq!(o.get("remset_index"), 1000);
        }
        assert!(o.get("slices") > 1, "{}", o.text());
        assert_eq!(o.get("yields"), o.get("slices") - 1);
        assert_eq!(o.get("max"), 64);
        assert_eq!(o.get("assisted"), 0);
        assert_eq!(o.get("open"), 0);
        assert!(o.get("retire") > 0);
        assert_eq!(o.get("completed_kind"), 0);
        assert_eq!(o.get("index"), o.get("bound"));
        assert_eq!(o.get("slices_after"), o.get("slices"));
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("unmarked"), 0);
    }
}

fn handoff(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    // No root initially: admit an actual allocated graph after acquiring the
    // phase lock, so the worker cannot consume this fixture's work too early.
    graph(&mut h);
    h.collect();
    h.g.call_label(h.l.lock);
    h.worker_copy(Src::Raw(0));
    h.keep(2);
    reopen_relocation_work(h.g);
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
    h.g.jmp_label(done);
    h.g.bind_text_label(queued);
    h.report("queued", |h| h.value(Src::Imm(1)));
    h.g.call_label(h.l.close_relocation);
    h.g.call_label(h.l.unlock);
    h.report_counter("slices", Counter::RelocateDrainSlices);
    h.report_counter("yields", Counter::RelocateDrainYields);
    h.report_counter("max", Counter::RelocateDrainMax);
    h.report_counter("assisted", Counter::RelocateDrainAssisted);
    h.report_counter("worker", Counter::RelocateWorkerUnits);
    h.report_counter("open", Counter::PhaseOpen);
    h.report_counter("kind", Counter::RelocateKind);
    h.report("value", |h| h.deref_field(Src::Raw(2), SLOTS - 1, 0));
    h.finish_collection();
    h.report("value_after_cleanup", |h| {
        h.deref_field(Src::Raw(2), SLOTS - 1, 0)
    });
    h.g.bind_text_label(done);
}
#[test]
fn production_relocation_end_hands_pending_fields_to_an_already_queued_real_worker() {
    let o = run(handoff, 0, true);
    assert_eq!(o.get("queued"), 1, "{}", o.text());
    assert!(o.get("slices") >= 2);
    assert!(o.get("yields") > 0);
    assert_eq!(o.get("max"), 64);
    assert!(o.get("assisted") > 0, "{}", o.text());
    assert!(o.get("worker") >= o.get("assisted"));
    assert_eq!(o.get("open"), 0);
    assert_eq!(o.get("kind"), 0);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("value_after_cleanup"), 84);
}
