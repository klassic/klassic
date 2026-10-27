//! Detached Old bitmap cleanup must preserve resumed stores and budget work.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::{
    Counter, emit_budget, old_bitmap_word, request_old_cycle,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

fn quantum(h: &mut H<'_>, units: u64) {
    h.g.call_label(h.l.lock);
    emit_budget(h.g, units);
    h.g.call_label(h.l.retire_step);
    h.g.call_label(h.l.unlock);
}
fn word(h: &mut H<'_>, source: bool, set: u64, last: bool) {
    old_bitmap_word(h.g, source, set, last);
    h.g.load_ptr_disp32(R::V0, R::V0, 0);
}
fn probe(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 65536, 0);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    // Exercise both physical halves and reuse the original Old slot address.
    for pass in 0..2 {
        let report = |name: &str| format!("p{pass}_{name}");
        h.report(&report("holder"), |h| h.value(Src::Root(0)));
        h.alloc(1, 16, 1);
        h.set_scalar(Src::Raw(1), 0, 84 + pass);
        h.store(Src::Root(0), 0, Src::Raw(1), true);
        h.report_counter(&report("registered"), Counter::RemsetLen);
        // Complete a real snapshot, then directly request its full relocation.
        // Policy eligibility is tested elsewhere; no synthetic Old top is used.
        h.mark_start();
        h.drain_marks();
        for set in 0..2 {
            // Poison only bitmap metadata, after the last source store. These
            // boundary sentinels test cleanup coverage, not fabricated entries.
            old_bitmap_word(h.g, false, set, false);
            h.g.mov_imm64(R::V1, u64::MAX);
            for i in 0..8 {
                h.g.store_ptr_disp32(R::V0, i * 8, R::V1);
            }
            old_bitmap_word(h.g, false, set, true);
            h.g.mov_imm64(R::V1, 777);
            h.g.store_ptr_disp32(R::V0, 0, R::V1);
        }
        request_old_cycle(h.g);
        h.collect();
        h.report_counter(&report("full"), Counter::FullCollections);
        h.report_counter(&report("discarded"), Counter::RemsetLen);
        h.report_counter(&report("previous"), Counter::RemsetPreviousLen);
        h.report(&report("deferred"), |h| word(h, true, 0, false));
        h.g.call_label(h.l.lock);
        h.g.call_label(h.l.close_relocation);
        h.g.call_label(h.l.unlock);
        let found = h.g.create_text_label();
        h.repeat(200_000, |h| {
            quantum(h, 1);
            h.counter(Counter::RetireStage);
            h.g.cmp_reg_imm8(R::V0, 11);
            h.g.jcc_label(C::Equal, found);
        });
        h.g.bind_text_label(found);
        h.report_counter(&report("bitmap_stage"), Counter::RetireStage);
        h.report(&report("last_before"), |h| word(h, true, 0, true));
        h.report_counter(&report("units_before"), Counter::RetireUnits);
        h.report_counter(&report("cursor_before"), Counter::RetireCursor);
        quantum(&mut h, 0);
        h.report_counter(&report("units_zero"), Counter::RetireUnits);
        quantum(&mut h, 7);
        h.report_counter(&report("units_after"), Counter::RetireUnits);
        h.report_counter(&report("cursor_after"), Counter::RetireCursor);
        h.report(&report("first"), |h| word(h, true, 0, false));
        h.report(&report("eighth"), |h| {
            old_bitmap_word(h.g, true, 0, false);
            h.g.load_ptr_disp32(R::V0, R::V0, 56);
        });
        // Ordinary allocation during cleanup returns a fresh Young referent.
        // Store it through a moved Old holder before either bitmap is finished.
        h.alloc(1, 16, 2);
        h.set_scalar(Src::Raw(2), 0, 99 + pass);
        h.store(Src::Root(0), 0, Src::Raw(2), true);
        h.report_counter(&report("during"), Counter::RemsetLen);
        h.report_counter(&report("during_stage"), Counter::RetireStage);
        h.finish_collection();
        for set in 0..2 {
            h.report(&report(&format!("cleared{set}")), |h| {
                word(h, true, set, false)
            });
            h.report(&report(&format!("last{set}")), |h| word(h, true, set, true));
        }
        h.report(&report("destination_bitmap0"), |h| word(h, false, 0, false));
        h.report(&report("destination_bitmap1"), |h| word(h, false, 1, false));
        h.report_counter(&report("kept"), Counter::RemsetLen);
        h.report(&report("entry"), |h| h.entry_is_slot(0, Src::Root(0), 0));
        // Sole strong edge to the new child is the remembered Old field.
        h.collect();
        h.finish_collection();
        h.report(&report("child"), |h| h.deref_field(Src::Root(0), 0, 0));
        h.report(&report("id"), |h| h.object_id(Src::Root(0)));
    }
}
#[test]
fn production_full_bitmap_retirement_is_budgeted_and_preserves_live_registrations_across_reuse() {
    let o = run(probe, 0, false);
    for pass in 0..2 {
        let get = |name: &str| o.get(&format!("p{pass}_{name}"));
        for (name, value) in [
            ("registered", 1),
            ("full", pass + 1),
            ("discarded", 0),
            ("previous", 0),
            ("deferred", -1),
            ("bitmap_stage", 11),
            ("last_before", 777),
            ("first", 0),
            ("eighth", -1),
            ("during", 1),
            ("during_stage", 11),
            ("kept", 1),
            ("entry", 1),
            ("cleared0", 0),
            ("cleared1", 0),
            ("last0", 0),
            ("last1", 0),
            ("child", 99 + pass),
            ("id", 1),
        ] {
            assert_eq!(get(name), value, "p{pass}_{name}: {}", o.text());
        }
        assert_ne!(
            get("destination_bitmap0") | get("destination_bitmap1"),
            0,
            "{}",
            o.text()
        );
        assert_eq!(get("units_zero"), get("units_before"));
        assert_eq!(get("units_after") - get("units_before"), 7);
        assert_eq!(get("cursor_after") - get("cursor_before"), 56);
    }
    assert_ne!(o.get("p0_holder"), o.get("p1_holder"));
}

fn concurrent(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    // Real retained objects give retirement a sizable Old source span.
    h.repeat(100, |h| {
        h.alloc(1, crate::gc_codegen::production::MAX_PAYLOAD, 9);
        h.root(9);
    });
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.repeat(100, |h| h.pop_root());
    let active = h.g.create_text_label();
    h.counter(Counter::MarkActive);
    h.g.test_reg_reg(R::V0, R::V0);
    h.g.jcc_label(C::NotEqual, active);
    h.mark_start();
    h.g.bind_text_label(active);
    h.drain_marks();
    request_old_cycle(h.g);
    h.collect();
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.close_relocation);
    h.g.call_label(h.l.unlock);
    crate::gc_codegen::production::test_support::wake_worker(h.g, h.l.unpark);
    h.repeat(3000, |h| {
        h.alloc(1, 16, 1);
        h.set_scalar(Src::Raw(1), 0, 123);
        h.store(Src::Root(0), 0, Src::Raw(1), true);
    });
    h.finish_collection();
    h.report_counter("full", Counter::FullCollections);
    h.report_counter("entries", Counter::RemsetLen);
    h.report("entry", |h| h.entry_is_slot(0, Src::Root(0), 0));
    h.report("bitmap0", |h| word(h, true, 0, false));
    h.report("bitmap1", |h| word(h, true, 1, false));
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report_counter("worker", Counter::WorkerScanned);
}
#[test]
fn production_worker_full_retirement_preserves_resumed_old_to_young_stores() {
    let o = run(concurrent, 0, true);
    for (name, value) in [
        ("full", 1),
        ("entries", 1),
        ("entry", 1),
        ("bitmap0", 0),
        ("bitmap1", 0),
        ("value", 123),
        ("id", 1),
    ] {
        assert_eq!(o.get(name), value, "{name}: {}", o.text());
    }
    assert!(o.get("worker") > 0, "{}", o.text());
}
