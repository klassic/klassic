//! Bounded relocation field work on both Young chains and promoted Old.
use super::{H, Src, run, run_failure};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::test_support::Counter;
use crate::portable_asm::{Condition, PortableAsm, Reg};

const SLOTS: u32 = 8192;

/// Page metadata is budgeted too. Reach the actual object-admission boundary
/// with one-unit slices, then measure field work from its real initial cursor.
pub(super) fn admit(h: &mut H<'_>, kind: u64, worker: bool) {
    let again = h.g.create_text_label();
    let done = h.g.create_text_label();
    let fail = crate::gc_codegen::production::test_support::relocate_page_test_fail(h.g);
    h.g.mov_imm64(Reg::V0, 16);
    h.g.store_rbp_slot(1600, Reg::V0);
    h.g.bind_text_label(again);
    h.relocate_quantum(1, worker);
    h.counter(Counter::RelocateKind);
    h.g.cmp_reg_imm32(Reg::V0, kind as i32);
    h.g.jcc_label(Condition::Equal, done);
    h.g.load_rbp_slot(Reg::V0, 1600);
    h.g.sub_reg_imm8(Reg::V0, 1);
    h.g.jcc_label(Condition::Equal, fail);
    h.g.store_rbp_slot(1600, Reg::V0);
    h.g.jmp_label(again);
    h.g.bind_text_label(done);
}

fn bounded(g: &mut NativeCodeGenerator, mode: u64) {
    let kind = mode & 3;
    let list = mode & 4 != 0;
    let overwrite = mode & 8 != 0;
    let first = u32::from(list);
    let worker = kind != 1;
    let mut h = H::begin(g);
    h.alloc(if list { 4 } else { 2 }, u64::from(SLOTS) * 8, 0);
    h.root(0);
    if list {
        h.set_scalar(Src::Root(0), 0, u64::from(SLOTS - 1));
    }
    for (slot, value) in [(1, 42), (2, 84)] {
        h.alloc(1, 16, slot);
        h.set_scalar(Src::Raw(slot), 0, value);
    }
    h.store(Src::Root(0), first, Src::Raw(1), true);
    h.store(Src::Root(0), SLOTS - 1, Src::Raw(2), true);
    if kind == 3 {
        h.collect();
        h.finish_collection();
    }
    h.alloc(1, 16, 3);
    h.set_scalar(Src::Raw(3), 0, 99);
    h.collect();
    admit(&mut h, kind, worker);
    h.report_counter("kind", Counter::RelocateKind);
    h.report_counter("initial_field", Counter::RelocateField);
    h.report_counter("initial_completed", Counter::RelocateCompleted);
    h.relocate_quantum(1, !worker); // Resume through the other copier variant.
    h.keep(4);
    h.report("done1", |h| h.value(Src::Raw(4)));
    h.report_counter("field1", Counter::RelocateField);
    h.report_counter("completed1", Counter::RelocateCompleted);
    h.relocate_quantum(2, worker); // Null slots still cost work.
    h.report_counter("field3", Counter::RelocateField);
    if overwrite {
        h.store(Src::Root(0), SLOTS - 1, Src::Raw(3), true);
    }
    h.report_counter("field_after_store", Counter::RelocateField);
    h.g.mov_imm64(Reg::V0, 0);
    h.keep(5);
    h.repeat(130, |h| {
        h.relocate_quantum(64, worker);
        h.keep(4);
        h.counter(Counter::RelocateCompleted);
        h.g.mov_reg_reg(Reg::V1, Reg::V0);
        h.value(Src::Raw(5));
        h.g.add_reg_reg(Reg::V0, Reg::V1);
        h.keep(5);
    });
    h.report("done", |h| h.value(Src::Raw(4)));
    h.report("completed", |h| h.value(Src::Raw(5)));
    h.report_counter("kind_done", Counter::RelocateKind);
    h.finish_collection();
    h.report("first", |h| h.deref_field(Src::Root(0), first, 0));
    h.report("last", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
    if list {
        h.report("length", |h| h.load(Src::Root(0), 0));
    }
}

#[test]
fn production_relocation_fields_resume_across_copiers_and_obey_slot_budgets() {
    for kind in 1..=3 {
        for list in [false, true] {
            for overwrite in [false, true] {
                let mode = kind | (u64::from(list) << 2) | (u64::from(overwrite) << 3);
                let o = run(bounded, mode, false);
                let first = i64::from(list);
                assert_eq!(o.get("kind"), kind as i64, "{mode}: {}", o.text());
                assert_eq!(o.get("initial_field"), first);
                assert_eq!(o.get("initial_completed"), 0);
                assert_eq!(o.get("done1"), 0);
                assert_eq!(o.get("field1"), first + 1);
                assert_eq!(o.get("completed1"), 0);
                assert_eq!(o.get("field3"), first + 3);
                assert_eq!(o.get("field_after_store"), first + 3);
                assert_eq!(o.get("done"), 1);
                assert_eq!(o.get("kind_done"), 0);
                assert_eq!(o.get("completed"), 3, "{mode}: {}", o.text());
                assert_eq!(o.get("first"), 42);
                assert_eq!(o.get("last"), if overwrite { 99 } else { 84 });
                if list {
                    assert_eq!(o.get("length"), i64::from(SLOTS - 1));
                }
            }
        }
    }
}

fn real_worker(g: &mut NativeCodeGenerator, promote: u64) {
    let mut h = H::begin(g);
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), SLOTS - 1, Src::Raw(1), true);
    if promote != 0 {
        h.collect();
        h.finish_collection();
    }
    h.counter(Counter::WorkerScanned);
    h.keep(3);
    h.collect();
    // Wake without mutator scan assistance before worker completion. If the
    // worker already finished, allocation may run the normal final check.
    h.alloc(1, 16, 2);
    crate::gc_codegen::production::test_support::publish_primary(h.g);
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
    h.counter(Counter::WorkerScanned);
    h.g.mov_reg_reg(Reg::V1, Reg::V0);
    h.value(Src::Raw(3));
    h.g.sub_reg_reg(Reg::V1, Reg::V0);
    h.g.mov_reg_reg(Reg::V0, Reg::V1);
    h.keep(4);
    h.report("objects", |h| h.value(Src::Raw(4)));
    h.report_counter("kind_done", Counter::RelocateKind);
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
}

#[test]
fn production_worker_chunks_large_young_and_promoted_old_field_scans() {
    for promote in [0, 1] {
        let o = run(real_worker, promote, true);
        assert!((2..=3).contains(&o.get("objects")), "{}", o.text());
        assert_eq!(o.get("kind_done"), 0);
        assert_eq!(o.get("value"), 84);
    }
}

fn unready_promotion(g: &mut NativeCodeGenerator, finish: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    h.collect();
    h.finish_collection();
    h.collect();
    h.relocate_quantum(1, true); // Promote the root; no scan budget remains.
    h.value(Src::Root(0));
    h.keep(2);
    let descriptor = (std::mem::offset_of!(crate::gc_abi::ObjectHeader, descriptor_id) as i32)
        - std::mem::size_of::<crate::gc_abi::ObjectHeader>() as i32;
    // Model the reserved Old slot's pre-publication gap. Keep the real
    // object's remaining metadata so completing the publication is valid.
    h.g.mov_imm64(Reg::V1, 0);
    h.g.store_ptr_disp32(Reg::V0, descriptor, Reg::V1);
    if finish != 0 {
        h.report("before", |h| h.value(Src::Imm(1)));
        h.finish_collection();
        h.report("after", |h| h.value(Src::Imm(1)));
    } else {
        h.relocate_quantum(64, true);
        h.keep(3);
        h.report("unready_done", |h| h.value(Src::Raw(3)));
        h.report_counter("unready_completed", Counter::RelocateCompleted);
        h.value(Src::Raw(2));
        h.g.mov_imm64(Reg::V1, 1);
        h.g.store_ptr_disp32(Reg::V0, descriptor, Reg::V1);
        h.relocate_quantum(64, false);
        h.keep(3);
        h.report("ready_done", |h| h.value(Src::Raw(3)));
        h.report_counter("ready_completed", Counter::RelocateCompleted);
        h.finish_collection();
        h.report("value", |h| h.load(Src::Root(0), 0));
    }
}

#[test]
fn production_relocation_yields_an_unready_promotion_instead_of_spinning_locked() {
    let o = run(unready_promotion, 0, false);
    assert_eq!(o.get("unready_done"), 0);
    assert_eq!(o.get("unready_completed"), 0);
    assert_eq!(o.get("ready_done"), 1);
    assert_eq!(o.get("ready_completed"), 1);
    assert_eq!(o.get("value"), 42);
}

#[test]
fn production_finish_rejects_an_incomplete_scan_before_recycling() {
    let o = run_failure(unready_promotion, 1);
    assert_eq!(o.status.code(), Some(1));
    assert_eq!(o.stdout, b"before=1\n");
}
