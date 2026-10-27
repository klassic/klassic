//! Epoch membership follows object identity through physical copies.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_abi::{MarkRecord, ObjectHeader};
use crate::gc_codegen::production::test_support::{Counter, set_mark_cycles};
use crate::portable_asm::{PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

fn epoch(h: &mut H<'_>, object: Src, field: usize, value: u64) {
    h.value(object);
    h.g.load_ptr_disp32(
        R::V1,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    h.g.add_reg_imm32(R::V1, field as i32);
    h.g.mov_imm64(R::V0, value);
    h.g.atomic_store_release(R::V1, R::V0);
}
fn root(h: &mut H<'_>) {
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.value(Src::Raw(0));
    h.keep(1);
    h.root(0);
}
fn membership(h: &mut H<'_>, name: &str) {
    h.report(name, |h| h.marked(Src::Root(0)));
}
fn copied(g: &mut NativeCodeGenerator, worker: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    // Reserve the Young lane's contents without marking the Old snapshot.
    epoch(&mut h, Src::Raw(1), offset_of!(MarkRecord, young_epoch), 77);
    h.mark_start();
    h.drain_marks();
    membership(&mut h, "before");
    h.collect();
    h.relocate_quantum(64, worker != 0);
    membership(&mut h, "copied");
    h.finish_collection();
    membership(&mut h, "after_cleanup");
    h.report("first_epoch", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch))
    });
    h.report("first_old", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch))
    });
    h.collect();
    h.relocate_quantum(64, worker == 0);
    membership(&mut h, "promoted");
    h.finish_collection();
    h.report("old", |h| h.is_old(Src::Root(0)));
    h.report("epoch", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch))
    });
    h.report("young_epoch", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch))
    });
    h.report("identity", |h| h.object_id(Src::Root(0)));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_mark_epochs_survive_young_copy_promotion_both_copiers_and_source_cleanup() {
    for worker in [0, 1] {
        let o = run(copied, worker, false);
        for key in ["before", "copied", "after_cleanup", "promoted", "old"] {
            assert_eq!(o.get(key), 1, "{key}: {}", o.text());
        }
        assert_eq!(o.get("first_epoch"), 1);
        assert_eq!(o.get("epoch"), 1);
        assert_eq!(o.get("first_old"), 77);
        assert_eq!(o.get("young_epoch"), 77);
        assert_eq!(o.get("identity"), 1);
        assert_eq!(o.get("value"), 42);
        assert_eq!(o.get("unmarked"), 0);
    }
}
fn independent(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    h.report("initial", |h| h.marked(Src::Raw(1))); // Zero must never mean marked.
    epoch(&mut h, Src::Raw(1), offset_of!(MarkRecord, young_epoch), 1);
    h.mark_start();
    h.report("young_only", |h| h.marked(Src::Raw(1)));
    h.drain_marks();
    h.report("old", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch))
    });
    h.report("young", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch))
    });
    h.report("marked", |h| h.marked(Src::Raw(1)));
}
#[test]
fn production_old_mark_membership_requires_its_current_nonzero_epoch_not_the_young_lane() {
    let o = run(independent, 0, false);
    assert_eq!(o.get("initial"), 0);
    assert_eq!(o.get("young_only"), 0);
    assert_eq!(o.get("young"), 1);
    assert_eq!(o.get("old"), 1);
    assert_eq!(o.get("marked"), 1);
}
fn next_epoch(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    h.mark_start();
    h.drain_marks();
    h.end_marks();
    h.collect();
    h.finish_collection();
    h.value(Src::Root(0));
    h.keep(1);
    // A monotone serial may skip values; membership must compare the whole
    // serial, independently of the one-bit color's parity.
    set_mark_cycles(h.g, 2);
    h.mark_start();
    h.report_counter("cycle", Counter::MarkCycles);
    h.report("stale", |h| h.marked(Src::Raw(1)));
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 99);
    h.report("black", |h| h.marked(Src::Raw(2)));
    h.root(2);
    h.drain_marks();
    h.report("reached", |h| h.marked(Src::Raw(1)));
    h.report("epoch", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch))
    });
    h.report_counter("marked", Counter::Marked);
    h.collect();
    h.finish_collection();
    membership(&mut h, "after_move");
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("black_value", |h| h.load(Src::Root(2), 0));
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_new_mark_serial_rejects_stale_membership_and_new_allocations_are_black() {
    let o = run(next_epoch, 0, false);
    assert_eq!(o.get("cycle"), 3);
    assert_eq!(o.get("stale"), 0);
    assert_eq!(o.get("black"), 1);
    assert_eq!(o.get("reached"), 1);
    assert_eq!(o.get("epoch"), 3);
    assert_eq!(o.get("marked"), 2); // One admission in each actual snapshot.
    assert_eq!(o.get("after_move"), 1);
    assert_eq!(o.get("value"), 42);
    assert_eq!(o.get("black_value"), 99);
    assert_eq!(o.get("unmarked"), 0);
}
fn overflow(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    root(&mut h);
    set_mark_cycles(h.g, u64::MAX);
    h.report("before", |h| h.value(Src::Imm(1)));
    h.mark_start();
    h.report("after", |h| h.value(Src::Imm(1)));
}
#[test]
fn production_mark_epoch_exhaustion_rejects_wraparound_before_another_snapshot() {
    let o = run_failure(overflow, 0);
    assert_eq!(o.status.code(), Some(1));
    assert_eq!(o.stdout, b"before=1\n");
    assert!(String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"));
}
