//! Actual relocation snapshots, parity wraps and stopped-owner admission.
use super::{H, Src, run, run_failure, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_abi::{Generation, MAX_OFFSET, Phase, PhaseSnapshot};
use crate::gc_codegen::production::test_support::{
    Counter, adopt_snapshot, block_phase_publication, publish_relocation_for_test,
};
use crate::portable_asm::{PortableAsm, Reg};

fn relocating(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 77);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    for cycle in 0..12 {
        h.collect();
        h.report_counter(&format!("open{cycle}"), Counter::AdoptedPhase);
        h.value(Src::Root(0)); // Resolve a root not visited by relocation yet.
        h.report(&format!("root{cycle}"), |h| {
            h.g.load_rbp_slot(Reg::V0, slot_offset(0));
            h.g.atomic_load_acquire(Reg::V0, Reg::V0);
        });
        h.load_barriered(Src::Root(0), 0);
        h.keep(2);
        h.report(&format!("field{cycle}"), |h| h.load(Src::Root(0), 0));
        h.report(&format!("id{cycle}"), |h| h.object_id(Src::Raw(2)));
        h.report(&format!("value{cycle}"), |h| {
            h.deref_field(Src::Root(0), 0, 0)
        });
        h.finish_collection();
        h.report_counter(&format!("closed{cycle}"), Counter::AdoptedPhase);
    }
    h.report_counter("full", Counter::FullCollections);
}

#[test]
fn production_relocation_publishes_real_remap_parities_and_survives_repeated_wraps() {
    let o = run(relocating, 0, false);
    assert_eq!(o.get("full"), 0);
    for cycle in 0..12 {
        let open = PhaseSnapshot::from_bits(o.get(&format!("open{cycle}")) as u64).unwrap();
        let closed = PhaseSnapshot::from_bits(o.get(&format!("closed{cycle}")) as u64).unwrap();
        assert_eq!(open.phase(Generation::Young), Phase::ConcurrentRelocate);
        assert_eq!(open.phase(Generation::Old), Phase::Idle);
        assert_eq!(closed.phase(Generation::Young), Phase::Idle);
        assert_eq!(closed.phase(Generation::Old), Phase::Idle);
        assert_eq!((open.bits() >> 8) & 31, if cycle % 2 == 0 { 4 } else { 0 });
        assert_eq!(open.bits() >> 16, cycle * 4 + 2);
        assert_eq!(closed.bits() >> 16, cycle * 4 + 4);
        assert_eq!(closed.color(), open.color());
        assert_eq!(
            o.get(&format!("root{cycle}")) & 0xffff,
            i64::from(open.color_unremembered())
        );
        assert_eq!(
            o.get(&format!("field{cycle}")) & 0xffff,
            i64::from(open.color_unremembered())
        );
        assert_eq!(o.get(&format!("id{cycle}")), 2);
        assert_eq!(o.get(&format!("value{cycle}")), 77);
    }
}

fn blocked_publication(g: &mut NativeCodeGenerator, case: u64) {
    let mut h = H::begin(g);
    if case < 3 {
        block_phase_publication(h.g, case);
    } else {
        h.g.mov_imm64(
            Reg::V0,
            PhaseSnapshot::new(Phase::Idle, Phase::Idle, 0, MAX_OFFSET)
                .unwrap()
                .bits(),
        );
        adopt_snapshot(h.g);
    }
    h.report("before", |h| h.value(Src::Imm(1)));
    let runtime = h.g.gc_gen.take().unwrap();
    publish_relocation_for_test(h.g, &runtime.labels);
    h.g.gc_gen = Some(runtime);
    h.report("after", |h| h.value(Src::Imm(1)));
}

#[test]
fn production_phase_publication_rejects_private_operations_and_sequence_wrap() {
    for case in 0..4 {
        let o = run_failure(blocked_publication, case);
        assert_eq!(o.status.code(), Some(1), "{case}: {o:?}");
        assert_eq!(o.stdout, b"before=1\n", "{case}: {o:?}");
    }
}

fn marked_cycle(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 42);
    h.root(0);
    for cycle in 0..8 {
        h.value(Src::Root(0));
        h.keep(2);
        h.mark_start();
        h.report_counter(&format!("mark_phase{cycle}"), Counter::AdoptedPhase);
        h.report(&format!("before_read{cycle}"), |h| h.marked(Src::Raw(2)));
        h.value(Src::Root(0));
        h.report(&format!("after_read{cycle}"), |h| h.marked(Src::Raw(2)));
        h.drain_marks();
        h.end_marks();
        h.collect();
        h.finish_collection();
        h.report_counter(&format!("idle{cycle}"), Counter::AdoptedPhase);
        h.report(&format!("value{cycle}"), |h| h.load(Src::Root(0), 0));
    }
    h.report_counter("cycles", Counter::MarkCycles);
}

#[test]
fn production_snapshot_tracks_only_the_old_marker_before_relocation() {
    let o = run(marked_cycle, 0, false);
    for cycle in 0..8 {
        let marking =
            PhaseSnapshot::from_bits(o.get(&format!("mark_phase{cycle}")) as u64).unwrap();
        let idle = PhaseSnapshot::from_bits(o.get(&format!("idle{cycle}")) as u64).unwrap();
        for generation in [Generation::Young, Generation::Old] {
            assert_eq!(
                marking.phase(generation),
                if generation == Generation::Old {
                    Phase::ConcurrentMark
                } else {
                    Phase::Idle
                }
            );
            assert_eq!(idle.phase(generation), Phase::Idle);
        }
        assert_eq!(marking.bits() >> 16, cycle * 8 + 2);
        assert_eq!(idle.bits() >> 16, cycle * 8 + 8);
        assert_eq!(
            (marking.bits() >> 8) & 3,
            if cycle % 2 == 0 { 2 } else { 0 }
        );
        assert_eq!((idle.bits() >> 8) & 31, if cycle % 2 == 0 { 6 } else { 0 });
        assert_eq!(o.get(&format!("before_read{cycle}")), 0);
        assert_eq!(o.get(&format!("after_read{cycle}")), 1);
        assert_eq!(o.get(&format!("value{cycle}")), 42);
    }
    assert_eq!(o.get("cycles"), 8);
}

fn strong_publication(g: &mut NativeCodeGenerator, root: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.value(Src::Raw(1));
    h.keep(2);
    h.mark_start();
    h.report("before", |h| h.marked(Src::Raw(2)));
    if root != 0 {
        h.root(1); // Beyond the captured root discovery bound.
    } else {
        h.store(Src::Root(0), 0, Src::Raw(1), true);
    }
    h.report("after", |h| h.marked(Src::Raw(2)));
    h.collect();
    h.finish_collection();
    h.report("value", |h| {
        if root != 0 {
            h.load(Src::Root(1), 0);
        } else {
            h.deref_field(Src::Root(0), 0, 0);
        }
    });
}

#[test]
fn production_strong_publication_protects_before_certifying_current_mark_bits() {
    for root in [0, 1] {
        let o = run(strong_publication, root, false);
        assert_eq!(o.get("before"), 0);
        assert_eq!(o.get("after"), 1);
        assert_eq!(o.get("value"), 84);
    }
}
