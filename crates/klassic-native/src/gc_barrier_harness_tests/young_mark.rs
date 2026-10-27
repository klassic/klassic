//! Independent Young roots, remembered slots and barriers in emitted code.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_abi::{Generation, MarkRecord, ObjectHeader, Phase, PhaseSnapshot};
use crate::gc_codegen::production::test_support::{
    Counter, emit_budget, emit_unbounded_budget, emit_young_marked, mark_receiver, set_young_auto,
    set_young_epoch,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

fn start(h: &mut H<'_>) {
    h.g.call_label(h.l.young_start);
}
fn quantum(h: &mut H<'_>, units: u64) {
    h.g.call_label(h.l.lock);
    emit_budget(h.g, units);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
}
fn drain(h: &mut H<'_>) {
    h.g.call_label(h.l.lock);
    emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
}
fn marked(h: &mut H<'_>, src: Src) {
    h.value(src);
    emit_young_marked(h.g);
}
fn basic(g: &mut NativeCodeGenerator, old_first: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    if old_first != 0 {
        h.mark_start();
    }
    start(&mut h);
    if old_first == 0 {
        h.mark_start();
    }
    h.report_counter("phase", Counter::AdoptedPhase);
    h.report_counter("initial_young", Counter::YoungMarked);
    h.report_counter("root_bound", Counter::YoungRootBound);
    quantum(&mut h, 1);
    h.report_counter("one_root", Counter::YoungRoot);
    h.report_counter("one_marked", Counter::YoungMarked);
    h.report("old_before", |h| h.marked(Src::Raw(1)));
    drain(&mut h);
    h.keep(5);
    h.report("done", |h| h.value(Src::Raw(5)));
    h.report("young", |h| marked(h, Src::Raw(1)));
    h.report("old_still", |h| h.marked(Src::Raw(1)));
    h.report_counter("object_done", Counter::YoungObject);
    h.report_counter("queue_done", Counter::YoungStackLen);
    h.report_counter("old_cycles", Counter::MarkCycles);
    h.report_counter("young_cycles", Counter::YoungEpoch);
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report("young_word", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch))
    });
    h.report_counter("young_active", Counter::YoungActive);
    h.report_counter("old_active", Counter::MarkActive);
    h.report_counter("unmarked", Counter::YoungUnmarked);
}
#[test]
fn production_young_and_old_markers_own_independent_epochs_and_phases() {
    for order in [0, 1] {
        let o = run(basic, order, false);
        let p = PhaseSnapshot::from_bits(o.get("phase") as u64).unwrap();
        for g in [Generation::Young, Generation::Old] {
            assert_eq!(p.phase(g), Phase::ConcurrentMark);
        }
        assert_eq!((p.bits() >> 8) & 3, 3);
        assert_eq!(o.get("initial_young"), 0);
        assert_eq!(o.get("root_bound"), 2);
        assert_eq!(o.get("object_done"), 0);
        assert_eq!(o.get("queue_done"), 0);
        assert_eq!(o.get("one_root"), 2);
        assert_eq!(o.get("one_marked"), 1);
        assert_eq!(o.get("old_before"), 0);
        assert_eq!(o.get("done"), 1);
        assert_eq!(o.get("young"), 1);
        assert_eq!(o.get("old_still"), 0);
        assert_eq!(o.get("old_cycles"), 1);
        assert_eq!(o.get("young_cycles"), 1);
        assert_eq!(o.get("young_active"), 0);
        assert_eq!(o.get("old_active"), 1);
        assert_eq!(o.get("young_word"), 1);
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("unmarked"), 0, "{}", o.text());
    }
}
fn protected_old(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 8192 * 8, 0);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.value(Src::Root(0));
    h.keep(3);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(3), 0, Src::Raw(1), true);
    start(&mut h);
    h.value(Src::Raw(3));
    h.g.add_reg_imm32(R::V0, 8192);
    h.g.and_reg_imm32(R::V0, -4096);
    h.keep(4);
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V6, 4096);
    h.g.mov_imm64(R::V2, 0);
    h.g.mov_imm64(R::V0, 10);
    h.g.asm.syscall();
    h.keep(5);
    h.report("protected", |h| h.value(Src::Raw(5)));
    quantum(&mut h, 1);
    h.report_counter("after_root", Counter::YoungMarked);
    quantum(&mut h, 1); // Empty Old receiver is a charged root.
    quantum(&mut h, 1); // Only the registered Old slot is inspected.
    h.report_counter("slot_cursor", Counter::YoungRemset);
    h.report_counter("slot_bound", Counter::YoungRemsetBound);
    drain(&mut h);
    h.keep(5);
    h.report("done", |h| h.value(Src::Raw(5)));
    h.report("child", |h| marked(h, Src::Raw(1)));
    h.report("old_owner", |h| marked(h, Src::Raw(3)));
    h.value(Src::Raw(4));
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V6, 4096);
    h.g.mov_imm64(R::V2, 3);
    h.g.mov_imm64(R::V0, 10);
    h.g.asm.syscall();
    h.collect();
    h.finish_collection();
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report_counter("old_cycles", Counter::MarkCycles);
    h.report_counter("unmarked", Counter::YoungUnmarked);
}
#[test]
fn production_young_marker_uses_remembered_slots_without_reading_old_payloads() {
    let o = run(protected_old, 0, false);
    assert_eq!(o.get("protected"), 0);
    assert_eq!(o.get("after_root"), 0);
    assert_eq!(o.get("slot_cursor"), 1);
    assert_eq!(o.get("slot_bound"), 1);
    assert_eq!(o.get("done"), 1);
    assert_eq!(o.get("child"), 1);
    assert_eq!(o.get("old_owner"), 0);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("old_cycles"), 0);
    assert_eq!(o.get("unmarked"), 0, "{}", o.text());
}
fn barriers(g: &mut NativeCodeGenerator, root: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 8192 * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 42);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.store(Src::Root(0), 8191, Src::Raw(1), true);
    if root != 0 {
        h.value(Src::Raw(1));
        h.keep(4);
        h.root(4); // Captured before start; still unread when overwritten.
    }
    start(&mut h);
    quantum(&mut h, 2);
    quantum(&mut h, 3);
    h.report_counter("field_before", Counter::YoungField);
    if root == 0 {
        h.store(Src::Root(0), 8191, Src::Raw(2), true);
    } else {
        h.value(Src::Raw(2));
        h.set_root(4);
    }
    h.report("previous", |h| marked(h, Src::Raw(1)));
    h.report("next", |h| marked(h, Src::Raw(2)));
    h.report_counter("field_after", Counter::YoungField);
    h.alloc(1, 16, 6);
    h.set_scalar(Src::Raw(6), 0, 99);
    h.report("black_young", |h| marked(h, Src::Raw(6)));
    h.report("black_old", |h| h.marked(Src::Raw(6)));
    drain(&mut h);
    h.collect();
    h.finish_collection();
    h.report_counter("unmarked", Counter::YoungUnmarked);
}
#[test]
fn production_young_satb_and_new_publications_protect_without_old_marking() {
    for root in [0, 1] {
        let o = run(barriers, root, false);
        assert_eq!(o.get("field_before"), 3);
        assert_eq!(o.get("field_after"), 3);
        for k in ["previous", "next", "black_young"] {
            assert_eq!(o.get(k), 1, "{k}: {}", o.text());
        }
        assert_eq!(o.get("black_old"), 0);
        assert_eq!(o.get("unmarked"), 0);
    }
}
fn ordinary(g: &mut NativeCodeGenerator, _arg: u64) {
    // This test requires two Young cycles in one Old snapshot. Reserve enough
    // Old space for even a whole black Young window's uncapped copy bound.
    g.gc_max_heap = 512 << 20;
    g.gc_soft_max_heap = Some(192 << 20);
    let mut h = H::begin(g);
    set_young_auto(h.g, true);
    h.alloc(2, 8192 * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), 8191, Src::Raw(1), true);
    let done = h.g.create_text_label();
    h.repeat(1024, |h| {
        h.alloc(1, 120000, 2);
        h.counter(Counter::Collections);
        h.g.cmp_reg_imm8(R::V0, 2);
        h.g.jcc_label(C::AboveOrEqual, done);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(done);
    h.finish_collection();
    h.report_counter("collections", Counter::Collections);
    h.report_counter("old_cycles", Counter::MarkCycles);
    h.report_counter("young_cycles", Counter::YoungEpoch);
    h.report_counter("young_units", Counter::YoungWorkerUnits);
    h.report_counter("old_units", Counter::MarkWorkerUnits);
    h.report_counter("old_active", Counter::MarkActive);
    h.report_counter("young_active", Counter::YoungActive);
    h.report_counter("unmarked", Counter::YoungUnmarked);
    h.report("value", |h| h.deref_field(Src::Root(0), 8191, 0));
    h.report("old", |h| h.is_old(Src::Root(0)));
    h.report("young_word", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch))
    });
    h.report("old_word", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch))
    });
}
#[test]
fn production_ordinary_cycles_run_a_new_young_snapshot_while_old_snapshot_survives() {
    let o = run(ordinary, 0, true);
    assert_eq!(o.get("collections"), 2, "{}", o.text());
    assert_eq!(o.get("old_cycles"), 1);
    assert_eq!(o.get("young_cycles"), 2);
    assert!(o.get("young_units") > 0);
    assert!(o.get("old_units") > 0);
    assert_eq!(o.get("old_active"), 1);
    assert_eq!(o.get("young_active"), 0);
    assert_eq!(o.get("unmarked"), 0, "{}", o.text());
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("old"), 1);
    assert_eq!(o.get("young_word"), 2);
    assert_eq!(o.get("old_word"), 1);
}

fn growing_old_queue(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    for (slot, value) in [(1, 42), (2, 84)] {
        h.alloc(1, 16, slot);
        h.set_scalar(Src::Raw(slot), 0, value);
        h.store(Src::Root(0), slot - 1, Src::Raw(slot), true);
    }
    h.mark_start();
    h.value(Src::Root(0)); // Old queue owns the only remaining strong root.
    h.pop_root();
    start(&mut h);
    h.report_counter("old_bound", Counter::YoungOldBound);
    h.repeat(4, |h| {
        h.counter(Counter::YoungOldIndex);
        let skip = h.g.create_text_label();
        h.g.cmp_reg_imm8(R::V0, 1);
        h.g.jcc_label(C::AboveOrEqual, skip);
        quantum(h, 1);
        h.g.bind_text_label(skip);
    });
    h.report_counter("old_index_before", Counter::YoungOldIndex);
    // Pop A and visit its two fields. B reuses the captured queue slot;
    // C is appended beyond that bound, after the captured active receiver.
    h.mark_quantum(3);
    h.report_counter("old_queue", Counter::MarkStackLen);
    h.report_counter("old_object", Counter::MarkObject);
    drain(&mut h);
    h.report("b", |h| marked(h, Src::Raw(1)));
    h.report("c", |h| marked(h, Src::Raw(2)));
    h.report_counter("old_index_after", Counter::YoungOldIndex);
    h.collect();
    h.finish_collection();
    for index in 0..2 {
        h.report(&format!("value{index}"), |h| {
            mark_receiver(h.g, Some(index));
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
    }
    h.report_counter("unmarked", Counter::YoungUnmarked);
}
#[test]
fn production_young_marker_covers_old_jobs_appended_beyond_captured_queue_bound() {
    let o = run(growing_old_queue, 0, false);
    assert_eq!(o.get("old_bound"), 2);
    assert_eq!(o.get("old_index_before"), 1);
    assert_eq!(o.get("old_queue"), 2);
    assert_eq!(o.get("old_object"), 0);
    assert_eq!(o.get("old_index_after"), 2);
    assert_eq!(o.get("b"), 1);
    assert_eq!(o.get("c"), 1);
    assert_eq!(o.get("value0"), 42);
    assert_eq!(o.get("value1"), 84);
    assert_eq!(o.get("unmarked"), 0, "{}", o.text());
}

fn resource_wait(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 8192 * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), 8191, Src::Raw(1), true);
    start(&mut h);
    h.collect(); // No owner call to the Young tracing entry.
    h.finish_collection();
    h.report_counter("waits", Counter::YoungWaits);
    h.report_counter("units", Counter::YoungWorkerUnits);
    h.report_counter("done", Counter::YoungDone);
    h.report_counter("valid", Counter::YoungValid);
    h.report_counter("active", Counter::YoungActive);
    h.report_counter("old_drains", Counter::MarkDrains);
    h.report_counter("old_cycles", Counter::MarkCycles);
    h.report_counter("unmarked", Counter::YoungUnmarked);
    h.report("value", |h| h.deref_field(Src::Root(0), 8191, 0));
}
#[test]
fn production_young_allocation_resource_wait_leaves_all_tracing_to_worker() {
    let o = run(resource_wait, 0, true);
    assert!(o.get("waits") > 0, "{}", o.text());
    assert!(o.get("units") >= 8192, "{}", o.text());
    for key in [
        "done",
        "valid",
        "active",
        "old_drains",
        "old_cycles",
        "unmarked",
    ] {
        assert_eq!(o.get(key), 0, "{key}: {}", o.text());
    }
    assert_eq!(o.get("value"), 84);
}

fn serial(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    start(&mut h);
    drain(&mut h);
    h.collect();
    h.finish_collection();
    h.value(Src::Root(0));
    h.keep(1);
    // A matching Old serial and matching one-bit Young parity cannot stand
    // in for full Young epoch membership.
    h.g.load_ptr_disp32(
        R::V1,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    h.g.add_reg_imm32(R::V1, offset_of!(MarkRecord, old_epoch) as i32);
    h.g.mov_imm64(R::V0, 3);
    h.g.atomic_store_release(R::V1, R::V0);
    set_young_epoch(h.g, 2);
    start(&mut h);
    h.report("stale", |h| marked(h, Src::Raw(1)));
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.report("black", |h| marked(h, Src::Raw(2)));
    h.root(2);
    drain(&mut h);
    h.report("reached", |h| marked(h, Src::Raw(1)));
    h.collect();
    h.finish_collection();
    h.report_counter("epoch", Counter::YoungEpoch);
    h.report("young_word", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch))
    });
    h.report("old_word", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch))
    });
    h.report("promoted", |h| h.is_old(Src::Root(0)));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("black_value", |h| h.load(Src::Root(2), 0));
    h.report_counter("unmarked", Counter::YoungUnmarked);
}
#[test]
fn production_young_membership_uses_full_serial_and_preserves_both_stamps_on_promotion() {
    let o = run(serial, 0, false);
    assert_eq!(o.get("stale"), 0);
    for k in ["black", "reached", "promoted"] {
        assert_eq!(o.get(k), 1, "{k}: {}", o.text());
    }
    for k in ["epoch", "young_word", "old_word"] {
        assert_eq!(o.get(k), 3, "{k}: {}", o.text());
    }
    assert_eq!(o.get("value"), 42);
    assert_eq!(o.get("black_value"), 99);
    assert_eq!(o.get("unmarked"), 0);
}
fn overflow(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    set_young_epoch(h.g, u64::MAX);
    h.report("before", |h| h.value(Src::Imm(1)));
    start(&mut h);
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_young_epoch_exhaustion_rejects_wraparound() {
    let o = run_failure(overflow, 0);
    assert_eq!(o.status.code(), Some(1));
    assert_eq!(o.stdout, b"before=1\n");
    assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
}

fn load_other_lane(g: &mut NativeCodeGenerator, known_old: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.mark_start();
    if known_old != 0 {
        h.drain_marks();
    }
    start(&mut h);
    if known_old == 0 {
        drain(&mut h);
    }
    h.report("old_before", |h| h.marked(Src::Raw(1)));
    h.report("young_before", |h| marked(h, Src::Raw(1)));
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report("old_after", |h| h.marked(Src::Raw(1)));
    h.report("young_after", |h| marked(h, Src::Raw(1)));
    h.report_counter("young_queue", Counter::YoungStackLen);
}
#[test]
fn production_load_protects_each_real_mark_lane_independently() {
    for known_old in [0, 1] {
        let o = run(load_other_lane, known_old, false);
        assert_eq!(o.get("old_before"), known_old as i64);
        assert_eq!(o.get("young_before"), (1 - known_old) as i64);
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("old_after"), 1);
        assert_eq!(o.get("young_after"), 1);
        assert_eq!(o.get("young_queue"), 2 * known_old as i64);
    }
}

fn late_store(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.value(Src::Raw(0));
    h.keep(3);
    h.root(0);
    h.alloc(2, 16, 1);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.store(Src::Raw(1), 0, Src::Raw(2), true);
    start(&mut h);
    crate::gc_codegen::production::test_support::wake_worker(h.g, h.l.unpark);
    let idle = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::YoungDone);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, idle);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(idle);
    h.report_counter("idle", Counter::YoungDone);
    h.repeat(1000, |h| {
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.report("before", |h| marked(h, Src::Raw(2)));
    h.store(Src::Raw(3), 0, Src::Raw(1), true);
    let traced = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        marked(h, Src::Raw(2));
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, traced);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(traced);
    h.report("child", |h| marked(h, Src::Raw(2)));
    h.report_counter("worker", Counter::YoungWorkerUnits);
    h.report_counter("old_cycles", Counter::MarkCycles);
    h.report("value", |h| h.load(Src::Raw(2), 0));
}
#[test]
fn production_late_young_store_rearms_and_wakes_completed_worker_without_allocation() {
    let o = run(late_store, 0, true);
    assert_eq!(o.get("idle"), 1, "{}", o.text());
    assert_eq!(o.get("before"), 0);
    assert_eq!(o.get("child"), 1);
    assert!(o.get("worker") > 0);
    assert_eq!(o.get("old_cycles"), 0);
    assert_eq!(o.get("value"), 84);
}
