//! Old snapshots own their epoch/phase; Young evacuation does not close them.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::{Generation, MarkRecord, ObjectHeader, Phase, PhaseSnapshot};
use crate::gc_codegen::production::test_support::{Counter, request_full_collection};
use crate::portable_asm::{PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

const SLOTS: u32 = 8192;

fn seed_young_epoch(h: &mut H<'_>) {
    h.value(Src::Raw(3));
    h.g.load_ptr_disp32(
        R::V1,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - size_of::<ObjectHeader>() as i32,
    );
    h.g.add_reg_imm32(R::V1, offset_of!(MarkRecord, young_epoch) as i32);
    h.g.mov_imm64(R::V0, 77);
    h.g.atomic_store_release(R::V1, R::V0);
}
fn old_graph(h: &mut H<'_>) {
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.value(Src::Root(0));
    h.keep(3);
    seed_young_epoch(h);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(3), 0, Src::Raw(1), true);
}
fn permission(h: &mut H<'_>, mode: u64) {
    h.value(Src::Raw(3));
    h.g.add_reg_imm32(R::V0, 8192);
    h.g.and_reg_imm32(R::V0, -4096);
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V6, 4096);
    h.g.mov_imm64(R::V2, mode);
    h.g.mov_imm64(R::V0, 10); // Linux mprotect; exclude header and remset field 0.
    h.g.asm.syscall();
}
fn protected_old(g: &mut NativeCodeGenerator, actor: u64) {
    let mut h = H::begin(g);
    old_graph(&mut h);
    h.mark_start();
    h.mark_quantum(2); // Discover the Old root and retain its unscanned receiver.
    h.report_counter("before_field", Counter::MarkField);
    permission(&mut h, 0);
    h.keep(4);
    h.report("protected", |h| h.value(Src::Raw(4)));
    h.collect();
    h.relocate_quantum(64, actor != 0);
    h.finish_collection();
    h.report_counter("after_field", Counter::MarkField);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("cycles", Counter::MarkCycles);
    h.report_counter("drains", Counter::MarkDrains);
    h.report_counter("phase", Counter::AdoptedPhase);
    permission(&mut h, 3);
    h.keep(4);
    h.report("restored", |h| h.value(Src::Raw(4)));
    h.report("value", |h| h.deref_field(Src::Raw(3), 0, 0));
    h.drain_marks();
    h.report_counter("object_done", Counter::MarkObject);
    h.report("young_epoch", |h| {
        h.object_metadata(Src::Raw(3), offset_of!(MarkRecord, young_epoch));
    });
    h.report("old_epoch", |h| {
        h.object_metadata(Src::Raw(3), offset_of!(MarkRecord, old_epoch));
    });
}
#[test]
fn production_young_collection_never_scans_the_protected_pending_old_payload() {
    for actor in [0, 1] {
        let o = run(protected_old, actor, false);
        for key in [
            "protected",
            "restored",
            "before_field",
            "after_field",
            "drains",
            "object_done",
        ] {
            assert_eq!(o.get(key), 0, "{key}: {}", o.text());
        }
        assert_eq!(o.get("active"), 1);
        assert_eq!(o.get("cycles"), 1);
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("young_epoch"), 77);
        assert_eq!(o.get("old_epoch"), 1);
        let p = PhaseSnapshot::from_bits(o.get("phase") as u64).unwrap();
        assert_eq!(p.phase(Generation::Young), Phase::Idle);
        assert_eq!(p.phase(Generation::Old), Phase::ConcurrentMark);
    }
}
fn completed(g: &mut NativeCodeGenerator, young_parity: u64) {
    let mut h = H::begin(g);
    h.g.mov_imm64(
        R::V0,
        PhaseSnapshot::new(Phase::Idle, Phase::Idle, young_parity as u8, 1)
            .unwrap()
            .bits(),
    );
    crate::gc_codegen::production::test_support::adopt_snapshot(h.g);
    old_graph(&mut h);
    h.mark_start();
    h.drain_marks();
    h.keep(4);
    h.report("traced", |h| h.value(Src::Raw(4)));
    for cycle in 0..3 {
        h.collect();
        h.finish_collection();
        h.report_counter(&format!("active{cycle}"), Counter::MarkActive);
        h.report_counter(&format!("epoch{cycle}"), Counter::MarkCycles);
        h.report_counter(&format!("phase{cycle}"), Counter::AdoptedPhase);
        h.report_counter(&format!("queue{cycle}"), Counter::MarkStackLen);
    }
    h.alloc(1, 16, 2);
    h.report("born_old", |h| {
        h.object_metadata(Src::Raw(2), offset_of!(MarkRecord, old_epoch));
    });
    h.report("born_young", |h| {
        h.object_metadata(Src::Raw(2), offset_of!(MarkRecord, young_epoch));
    });
    h.report("reserved", |h| {
        h.object_metadata(Src::Raw(3), offset_of!(MarkRecord, young_epoch));
    });
    h.report_counter("drains", Counter::MarkDrains);
    h.end_marks();
    h.collect();
    h.finish_collection();
    h.value(Src::Root(0));
    h.keep(3);
    h.mark_start();
    h.report_counter("next_phase", Counter::AdoptedPhase);
    h.report_counter("next_epoch", Counter::MarkCycles);
    h.report("stale", |h| h.marked(Src::Raw(3)));
    h.value(Src::Root(0));
    h.report("admitted", |h| h.marked(Src::Raw(3)));
    h.report("reserved_again", |h| {
        h.object_metadata(Src::Raw(3), offset_of!(MarkRecord, young_epoch));
    });
}
#[test]
fn production_empty_old_frontiers_stay_active_through_young_cycles_and_black_allocation() {
    for young_parity in [0, 1] {
        let o = run(completed, young_parity, false);
        assert_eq!(o.get("traced"), 1);
        for cycle in 0..3 {
            assert_eq!(o.get(&format!("active{cycle}")), 1, "{}", o.text());
            assert_eq!(o.get(&format!("epoch{cycle}")), 1);
            assert_eq!(o.get(&format!("queue{cycle}")), 0);
            let p = PhaseSnapshot::from_bits(o.get(&format!("phase{cycle}")) as u64).unwrap();
            assert_eq!(p.phase(Generation::Young), Phase::Idle);
            assert_eq!(p.phase(Generation::Old), Phase::ConcurrentMark);
            assert_eq!((p.bits() >> 8) & 3, 2 | young_parity);
        }
        assert_eq!(o.get("born_old"), 1);
        assert_eq!(o.get("born_young"), 0);
        assert_eq!(o.get("reserved"), 77);
        assert_eq!(o.get("reserved_again"), 77);
        assert_eq!(o.get("drains"), 0);
        assert_eq!(o.get("next_epoch"), 2);
        assert_eq!(o.get("stale"), 0);
        assert_eq!(o.get("admitted"), 1);
        let p = PhaseSnapshot::from_bits(o.get("next_phase") as u64).unwrap();
        assert_eq!(p.phase(Generation::Young), Phase::Idle);
        assert_eq!(p.phase(Generation::Old), Phase::ConcurrentMark);
        assert_eq!((p.bits() >> 8) & 3, young_parity);
    }
}
fn full_old(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    old_graph(&mut h);
    h.mark_start();
    h.mark_quantum(2);
    request_full_collection(h.g);
    h.collect();
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("valid", Counter::MarkValid);
    h.report_counter("drains", Counter::MarkDrains);
    h.report_counter("phase", Counter::AdoptedPhase);
    h.finish_collection();
    h.report("young_epoch", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch));
    });
    h.report("old_epoch", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch));
    });
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report_counter("full", Counter::FullCollections);
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_full_old_move_closes_its_mark_without_changing_the_young_epoch() {
    let o = run(full_old, 0, false);
    assert_eq!(o.get("active"), 0);
    assert_eq!(o.get("valid"), 1);
    assert_eq!(o.get("drains"), 1);
    assert_eq!(o.get("full"), 1);
    assert_eq!(o.get("young_epoch"), 77);
    assert_eq!(o.get("old_epoch"), 1);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("unmarked"), 0);
    let p = PhaseSnapshot::from_bits(o.get("phase") as u64).unwrap();
    assert_eq!(p.phase(Generation::Young), Phase::ConcurrentRelocate);
    assert_eq!(p.phase(Generation::Old), Phase::ConcurrentRelocate);
    assert_eq!((p.bits() >> 8) & 3, 2);
}

fn ordinary_worker(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::MAX_PAYLOAD;
    use crate::portable_asm::Condition as C;
    let mut h = H::begin(g);
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), SLOTS - 1, Src::Raw(1), true);
    let collected = h.g.create_text_label();
    h.repeat(1024, |h| {
        h.alloc(1, MAX_PAYLOAD, 2);
        h.counter(Counter::Collections);
        h.g.cmp_reg_imm8(R::V0, 2);
        h.g.jcc_label(C::AboveOrEqual, collected);
        h.g.mov_imm64(R::V0, 24); // Let the real worker trace the pending graph.
        h.g.asm.syscall();
    });
    h.g.bind_text_label(collected);
    h.finish_collection();
    // Cleanup adoption wakes Old marking, but does not wait for its next
    // scheduled quantum. Observe actual worker completion without root loads
    // or mutator tracing before reading its work counters.
    let marked = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::MarkDone);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, marked);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(marked);
    h.report_counter("mark_done", Counter::MarkDone);
    h.report_counter("collections", Counter::Collections);
    h.report_counter("full", Counter::FullCollections);
    h.report_counter("ready", Counter::ReadyCycles);
    h.report_counter("cycles", Counter::MarkCycles);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("drains", Counter::MarkDrains);
    h.report_counter("phase", Counter::AdoptedPhase);
    h.report_counter("worker_mark", Counter::MarkWorkerUnits);
    h.report_counter("worker_copy", Counter::WorkerCopies);
    h.report("old", |h| h.is_old(Src::Root(0)));
    h.report("value", |h| h.deref_field(Src::Root(0), SLOTS - 1, 0));
    h.report("identity", |h| h.object_id(Src::Root(0)));
    h.report("old_epoch", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, old_epoch));
    });
    h.report("young_epoch", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch));
    });
    h.report_counter("unmarked", Counter::Unmarked);
}
#[test]
fn production_ordinary_young_cycles_reuse_one_real_worker_old_snapshot() {
    let o = run(ordinary_worker, 0, true);
    assert_eq!(o.get("collections"), 2, "{}", o.text());
    assert_eq!(o.get("full"), 0);
    // Cleanup can postpone a readiness poll until capacity pressure. Both
    // normal Young entries must preserve the same Old snapshot.
    assert_eq!(o.get("cycles"), 1);
    assert_eq!(o.get("active"), 1);
    assert_eq!(o.get("drains"), 0);
    assert_eq!(o.get("mark_done"), 1);
    assert!(o.get("worker_mark") > 0);
    assert!(o.get("worker_copy") > 0);
    assert_eq!(o.get("old"), 1);
    assert_eq!(o.get("value"), 84);
    assert_eq!(o.get("identity"), 1);
    assert_eq!(o.get("old_epoch"), 1);
    assert_eq!(o.get("young_epoch"), 0);
    assert_eq!(o.get("unmarked"), 0);
    let p = PhaseSnapshot::from_bits(o.get("phase") as u64).unwrap();
    assert_eq!(p.phase(Generation::Young), Phase::Idle);
    assert_eq!(p.phase(Generation::Old), Phase::ConcurrentMark);
    assert_eq!((p.bits() >> 8) & 3, 2);
}
