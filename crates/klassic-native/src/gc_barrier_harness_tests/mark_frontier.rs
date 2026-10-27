//! Real mark receivers survive Young flips and source metadata retirement.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::{Generation, Phase, PhaseSnapshot};
use crate::gc_codegen::production::test_support::{Counter, mark_receiver};
use crate::portable_asm::{PortableAsm, Reg as R};

const SLOTS: u32 = 8192;

fn preserve(g: &mut NativeCodeGenerator, actor: u64) {
    let mut h = H::begin(g);
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), SLOTS - 1, Src::Raw(1), true);
    h.mark_start();
    h.mark_quantum(5); // Root, receiver, three null fields.
    h.store(Src::Root(0), SLOTS - 1, Src::Null, true); // Only SATB retains child.
    h.pop_root(); // Only the active receiver retains the large object.
    h.report_counter("field_before", Counter::MarkField);
    h.report_counter("queue_before", Counter::MarkStackLen);
    h.report_counter("root_before", Counter::MarkRootIndex);
    h.report_counter("bound_before", Counter::MarkRootBound);
    h.report("id_before", |h| {
        mark_receiver(h.g, None);
        h.keep(2);
        h.object_id(Src::Raw(2));
    });
    for cycle in 0..3 {
        h.collect();
        h.report_counter(&format!("moving{cycle}"), Counter::AdoptedPhase);
        h.report_counter(
            &format!("initial_cursor{cycle}"),
            Counter::MarkRelocateIndex,
        );
        h.mark_quantum(64); // A direct request must also respect suspension.
        h.keep(3);
        h.report(&format!("mark_paused{cycle}"), |h| h.value(Src::Raw(3)));
        h.relocate_quantum(1, actor != 0);
        h.report_counter(&format!("cursor_one{cycle}"), Counter::MarkRelocateIndex);
        h.relocate_quantum(1, actor == 0);
        h.report_counter(&format!("cursor_two{cycle}"), Counter::MarkRelocateIndex);
        h.finish_collection(); // Also destroys source MarkRecords.
        h.report_counter(&format!("resumed{cycle}"), Counter::AdoptedPhase);
        h.report_counter(&format!("active{cycle}"), Counter::MarkActive);
        h.report_counter(&format!("field{cycle}"), Counter::MarkField);
        h.report_counter(&format!("queue{cycle}"), Counter::MarkStackLen);
        h.report_counter(&format!("epoch{cycle}"), Counter::MarkCycles);
        h.report(&format!("id{cycle}"), |h| {
            mark_receiver(h.g, None);
            h.keep(2);
            h.object_id(Src::Raw(2));
        });
        h.report(&format!("old{cycle}"), |h| h.is_old(Src::Raw(2)));
        h.report(&format!("receiver_marked{cycle}"), |h| {
            h.marked(Src::Raw(2))
        });
        h.report(&format!("child{cycle}"), |h| {
            mark_receiver(h.g, Some(0));
            h.g.load_ptr_disp32(R::V0, R::V0, 0);
        });
    }
    h.drain_marks();
    h.report_counter("object_done", Counter::MarkObject);
    h.report_counter("queue_done", Counter::MarkStackLen);
    h.report_counter("drains", Counter::MarkDrains);
    h.report_counter("root_after", Counter::MarkRootIndex);
    h.report_counter("bound_after", Counter::MarkRootBound);
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.mark_end);
    h.g.call_label(h.l.unlock);
    h.report_counter("valid", Counter::MarkValid);
    h.report_counter("unmarked", Counter::Unmarked);
}

#[test]
fn production_old_mark_preserves_queue_and_receiver_across_young_moves() {
    for actor in [0, 1] {
        let o = run(preserve, actor, false);
        assert_eq!(o.get("field_before"), 3);
        assert_eq!(o.get("queue_before"), 1);
        for cycle in 0..3 {
            let moving = PhaseSnapshot::from_bits(o.get(&format!("moving{cycle}")) as u64).unwrap();
            let resumed =
                PhaseSnapshot::from_bits(o.get(&format!("resumed{cycle}")) as u64).unwrap();
            assert_eq!(moving.phase(Generation::Young), Phase::ConcurrentRelocate);
            assert_eq!(moving.phase(Generation::Old), Phase::ConcurrentMark);
            for generation in [Generation::Young, Generation::Old] {
                assert_eq!(
                    resumed.phase(generation),
                    if generation == Generation::Old {
                        Phase::ConcurrentMark
                    } else {
                        Phase::Idle
                    }
                );
            }
            assert_eq!((moving.bits() >> 8) & 3, 2); // No mark parity restart.
            assert_eq!(o.get(&format!("initial_cursor{cycle}")), 0);
            assert_eq!(o.get(&format!("cursor_one{cycle}")), 1);
            assert_eq!(o.get(&format!("cursor_two{cycle}")), 2);
            assert_eq!(o.get(&format!("mark_paused{cycle}")), 0);
            assert_eq!(o.get(&format!("active{cycle}")), 1);
            assert_eq!(o.get(&format!("field{cycle}")), 3);
            assert_eq!(o.get(&format!("queue{cycle}")), 1);
            assert_eq!(o.get(&format!("epoch{cycle}")), 1);
            assert_eq!(o.get(&format!("id{cycle}")), o.get("id_before"));
            assert_eq!(o.get(&format!("old{cycle}")), i64::from(cycle > 0));
            assert_eq!(o.get(&format!("receiver_marked{cycle}")), 1);
            assert_eq!(o.get(&format!("child{cycle}")), 84, "{}", o.text());
        }
        assert_eq!(o.get("object_done"), 0);
        assert_eq!(o.get("queue_done"), 0);
        assert_eq!(o.get("drains"), 0);
        assert_eq!(o.get("root_before"), o.get("root_after"));
        assert_eq!(o.get("bound_before"), o.get("bound_after"));
        assert_eq!(o.get("valid"), 1);
        assert_eq!(o.get("unmarked"), 0);
    }
}

fn old_holder(h: &mut H<'_>) {
    h.alloc(2, 16, 0);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.value(Src::Root(0));
    h.keep(3); // Old does not move in these Young-only fixtures.
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(3), 0, Src::Raw(1), true);
    h.mark_start(); // Root discovery remains pending throughout relocation.
    h.collect();
}

fn white_copy(g: &mut NativeCodeGenerator, actor: u64) {
    let mut h = H::begin(g);
    old_holder(&mut h);
    h.relocate_quantum(64, actor != 0);
    // Inspect GC-only healing before the Retire phase hands copy-allocation
    // journal records to real mark work. That later handoff may mark this value.
    h.report("white_word", |h| h.load(Src::Raw(3), 0));
    // Decode the GC-healed word directly so observing it cannot mark it.
    h.load(Src::Raw(3), 0);
    h.g.shr_reg_imm8(R::V0, 16);
    h.g.load_ptr_disp32(
        R::V1,
        R::V3,
        crate::gc_codegen::production::test_support::heap_base_offset(),
    );
    h.g.add_reg_reg(R::V0, R::V1);
    h.keep(2);
    h.report("before", |h| h.marked(Src::Raw(2)));
    h.report("value", |h| {
        h.load_barriered(Src::Raw(3), 0);
        h.g.load_ptr_disp32(R::V0, R::V0, 0);
    });
    h.report("after", |h| h.marked(Src::Raw(2)));
    h.report_counter("queue", Counter::MarkStackLen);
    h.finish_collection();
}

#[test]
fn production_white_relocation_copy_keeps_a_mark_obligation_on_its_healed_word() {
    for actor in [0, 1] {
        let o = run(white_copy, actor, false);
        assert_eq!(o.get("before"), 0, "{}", o.text());
        assert_eq!(
            o.get("white_word") & i64::from(crate::gc_abi::YOUNG_MARK_MASK),
            i64::from(PhaseSnapshot::initial().color() & crate::gc_abi::YOUNG_MARK_MASK)
        );
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("after"), 1);
        assert_eq!(o.get("queue"), 1);
    }
}

fn late_admission(g: &mut NativeCodeGenerator, actor: u64) {
    use crate::gc_codegen::production::test_support::{completed_relocation_hint, enqueue_mark};
    let mut h = H::begin(g);
    old_holder(&mut h);
    h.relocate_quantum(64, actor != 0); // Every relocation frontier is caught up.
    h.report_counter("cursor_before", Counter::MarkRelocateIndex);
    h.report_counter("queue_before", Counter::MarkStackLen);
    // Model a SATB operation which read the old word before waiting for the
    // collector lock. The real mark admission receives the original offset,
    // after its white copy has already been published.
    h.g.call_label(h.l.lock);
    h.value(Src::Raw(1));
    enqueue_mark(h.g, h.l.mark_ref);
    completed_relocation_hint(h.g);
    h.g.call_label(h.l.try_close_relocation);
    h.g.call_label(h.l.unlock);
    h.report_counter("still_open", Counter::PhaseOpen);
    h.report_counter("retries", Counter::RelocateEndRetries);
    h.report_counter("queue_late", Counter::MarkStackLen);
    h.relocate_quantum(1, actor == 0);
    h.report_counter("cursor_after", Counter::MarkRelocateIndex);
    h.finish_collection();
    mark_receiver(h.g, Some(0));
    h.keep(2);
    h.report("value", |h| h.load(Src::Raw(2), 0));
    h.report("marked", |h| h.marked(Src::Raw(2)));
    h.drain_marks();
    h.report_counter("done", Counter::MarkStackLen);
}

#[test]
fn production_late_satb_extends_relocation_and_stamps_the_existing_white_copy() {
    for actor in [0, 1] {
        let o = run(late_admission, actor, false);
        assert_eq!(o.get("cursor_before"), 1);
        assert_eq!(o.get("queue_before"), 0);
        assert_eq!(o.get("still_open"), 1, "{}", o.text());
        assert_eq!(o.get("retries"), 1);
        assert_eq!(o.get("queue_late"), 1);
        assert_eq!(o.get("cursor_after"), 2);
        assert_eq!(o.get("value"), 84);
        assert_eq!(o.get("marked"), 1);
        assert_eq!(o.get("done"), 0);
    }
}

fn real_worker(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::{enqueue_mark, wake_worker};
    use crate::portable_asm::Condition as C;
    let mut h = H::begin(g);
    h.alloc(2, u64::from(SLOTS) * 8, 0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Raw(0), SLOTS - 1, Src::Raw(1), true);
    h.mark_start();
    h.g.call_label(h.l.lock);
    h.value(Src::Raw(0));
    enqueue_mark(h.g, h.l.mark_ref);
    wake_worker(h.g, h.l.unpark);
    h.g.call_label(h.l.unlock);
    // Fair ticket order admits at most one worker quantum before this flip.
    // The 8192-field receiver therefore still has genuine pending work.
    h.collect();
    h.counter(Counter::MarkField);
    h.keep(5);
    h.report_counter("active", Counter::MarkActive);
    h.report_counter("drains", Counter::MarkDrains);
    wake_worker(h.g, h.l.unpark);
    let scanned = h.g.create_text_label();
    h.repeat(1_000_000, |h| {
        h.counter(Counter::WorkerDone);
        h.g.test_reg_reg(R::V0, R::V0);
        h.g.jcc_label(C::NotEqual, scanned);
        h.g.mov_imm64(R::V0, 24);
        h.g.asm.syscall();
    });
    h.g.bind_text_label(scanned);
    h.report_counter("worker_done", Counter::WorkerDone);
    h.report_counter("field_waiting", Counter::MarkField);
    h.report("field_at_flip", |h| h.value(Src::Raw(5)));
    h.report_counter("worker_copies", Counter::WorkerCopies);
    h.report_counter("phase_before_adopt", Counter::AdoptedPhase);
    h.finish_collection();
    // Clearing completed retirement adopts ConcurrentMark and the same worker
    // resumes its retained receiver without another snapshot or epoch reset.
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
    h.report_counter("epoch", Counter::MarkCycles);
    h.report_counter("object_done", Counter::MarkObject);
    h.report_counter("queue_done", Counter::MarkStackLen);
    h.g.call_label(h.l.lock);
    h.g.call_label(h.l.mark_end);
    h.g.call_label(h.l.unlock);
    h.report_counter("valid", Counter::MarkValid);
    h.report_counter("unmarked", Counter::Unmarked);
}

#[test]
fn production_worker_relocates_then_resumes_the_same_pending_old_snapshot() {
    let o = run(real_worker, 0, true);
    assert_eq!(o.get("active"), 1, "{}", o.text());
    assert_eq!(o.get("drains"), 0);
    assert_eq!(o.get("worker_done"), 1);
    assert_eq!(o.get("field_waiting"), o.get("field_at_flip"));
    assert!(o.get("worker_copies") >= 2);
    let phase = PhaseSnapshot::from_bits(o.get("phase_before_adopt") as u64).unwrap();
    assert_eq!(phase.phase(Generation::Old), Phase::ConcurrentMark);
    assert_eq!(phase.phase(Generation::Young), Phase::ConcurrentRelocate);
    assert_eq!(o.get("mark_done"), 1);
    assert_eq!(o.get("epoch"), 1);
    assert_eq!(o.get("object_done"), 0);
    assert_eq!(o.get("queue_done"), 0);
    assert_eq!(o.get("valid"), 1);
    assert_eq!(o.get("unmarked"), 0);
}

fn white_root(g: &mut NativeCodeGenerator, variant: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    if variant & 2 != 0 {
        for _ in 0..2 {
            h.collect();
            h.finish_collection();
        }
    }
    h.mark_start();
    h.collect();
    h.relocate_quantum(64, variant & 1 != 0);
    // Inspect before the Retire boundary can legitimately admit copy scratch
    // references from the journal. This isolates the healer's certificate.
    // Observe the healed RootCell without taking its load barrier.
    h.g.load_rbp_slot(R::V0, super::slot_offset(0));
    h.g.atomic_load_acquire(R::V0, R::V0);
    h.g.shr_reg_imm8(R::V0, 16);
    h.g.load_ptr_disp32(
        R::V1,
        R::V3,
        crate::gc_codegen::production::test_support::heap_base_offset(),
    );
    h.g.add_reg_reg(R::V0, R::V1);
    h.keep(2);
    h.report("old", |h| h.is_old(Src::Raw(2)));
    h.report("before", |h| h.marked(Src::Raw(2)));
    h.report("value", |h| h.load(Src::Root(0), 0));
    h.report("after", |h| h.marked(Src::Raw(2)));
    h.report_counter("queue", Counter::MarkStackLen);
    h.finish_collection();
}

#[test]
fn production_relocated_and_nonmoving_white_roots_keep_their_mark_obligation() {
    for variant in 0..4 {
        let o = run(white_root, variant, false);
        assert_eq!(o.get("old"), i64::from(variant & 2 != 0));
        assert_eq!(o.get("before"), 0, "{}", o.text());
        assert_eq!(o.get("value"), 42);
        assert_eq!(o.get("after"), 1, "{}", o.text());
        assert_eq!(o.get("queue"), 1);
    }
}
