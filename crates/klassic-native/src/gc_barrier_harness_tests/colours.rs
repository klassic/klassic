//! ABI snapshots drive the real production reference codec/barriers.
//! Snapshot injection configures conformance states; it is not a coordinator.
use super::{H, Src, run, slot_offset};
use crate::NativeCodeGenerator;
use crate::gc_abi::{
    Access, Generation, HEAP_PAGE_BYTES, MarkRecord, OLD_MARK_MASK, Phase, PhaseSnapshot,
};
use crate::gc_codegen::production::test_support::{
    Counter, adopt_snapshot, load_access, reference_offset,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg};
use std::mem::offset_of;

fn adopt(h: &mut H<'_>, phase: PhaseSnapshot) {
    h.g.mov_imm64(Reg::V0, phase.bits());
    adopt_snapshot(h.g);
}

fn roundtrip(g: &mut NativeCodeGenerator, epochs: u64) {
    let mut h = H::begin(g);
    adopt(
        &mut h,
        PhaseSnapshot::new(Phase::Idle, Phase::Idle, epochs as u8, 1).unwrap(),
    );
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 4242);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.report("root_bits", |h| {
        h.g.load_rbp_slot(Reg::V0, slot_offset(0));
        h.g.atomic_load_acquire(Reg::V0, Reg::V0);
    });
    h.report("stored", |h| h.load(Src::Root(0), 0));
    for name in ["young", "old"] {
        h.collect();
        h.finish_collection();
        h.report(name, |h| h.deref_field(Src::Root(0), 0, 0));
    }
    h.report("moved_bits", |h| h.load(Src::Root(0), 0));
    h.load_barriered(Src::Root(0), 0);
    h.keep(2);
    h.report("id", |h| h.object_id(Src::Raw(2)));
    h.report("age", |h| {
        h.object_metadata(Src::Raw(2), offset_of!(MarkRecord, age))
    });
    h.report("null", |h| h.load_barriered(Src::Root(0), 1));
}

#[test]
fn production_reference_roundtrip_uses_all_adopted_epoch_combinations() {
    for epochs in 0..32 {
        let phase = PhaseSnapshot::new(Phase::Idle, Phase::Idle, epochs, 1).unwrap();
        let o = run(roundtrip, u64::from(epochs), false);
        assert_eq!(
            o.get("root_bits") & 0xffff,
            i64::from(phase.color()),
            "{epochs}: {}",
            o.text()
        );
        assert_eq!(
            o.get("stored") & 0xffff,
            i64::from(phase.color()),
            "{epochs}: {}",
            o.text()
        );
        assert_eq!(
            o.get("moved_bits") & 0xffff,
            i64::from(phase.color_unremembered()),
            "{epochs}: {}",
            o.text()
        );
        assert_eq!(o.get("young"), 4242);
        assert_eq!(o.get("old"), 4242);
        assert_eq!(o.get("id"), 2);
        assert_eq!(o.get("age"), 2);
        assert_eq!(o.get("null"), 0);
    }
}

fn access_matrix(g: &mut NativeCodeGenerator, _arg: u64) {
    let phases = [
        Phase::Idle,
        Phase::MarkStart,
        Phase::ConcurrentMark,
        Phase::MarkEndCheck,
        Phase::Select,
        Phase::RelocateStart,
        Phase::ConcurrentRelocate,
        Phase::Retire,
    ];
    let mut cases = Vec::new();
    for young in phases {
        for old in phases {
            for epochs in 0..32 {
                let p = PhaseSnapshot::new(young, old, epochs, 7).unwrap();
                for (generation, offset) in [
                    (Generation::Young, HEAP_PAGE_BYTES + 32),
                    (
                        Generation::Old,
                        crate::gc_codegen::production::test_support::young_end() + 32,
                    ),
                ] {
                    cases.extend([
                        p.bits(),
                        (offset << 16) | u64::from(p.color()),
                        u64::from(p.color()),
                        u64::from(p.required_mask(generation, Access::Load)),
                    ]);
                }
            }
        }
    }
    let count = cases.len() / 4;
    let data = g
        .asm
        .data_label_with_i64s(&cases.iter().map(|v| *v as i64).collect::<Vec<_>>());
    let mut h = H::begin(g);
    h.g.mov_data_addr(Reg::V0, data);
    h.keep(4);
    h.g.mov_imm64(Reg::V0, 0);
    h.keep(5);
    h.repeat(count as u64, |h| {
        let bad = h.g.create_text_label();
        let next = h.g.create_text_label();
        h.value(Src::Raw(4));
        h.g.mov_reg_reg(Reg::V8, Reg::V0);
        h.g.load_ptr_disp32(Reg::V0, Reg::V8, 0);
        adopt_snapshot(h.g);
        h.g.load_ptr_disp32(Reg::V0, Reg::V8, 8);
        load_access(h.g);
        h.g.load_ptr_disp32(Reg::V6, Reg::V8, 16);
        h.g.cmp_reg_reg(Reg::V1, Reg::V6);
        h.g.jcc_label(C::NotEqual, bad);
        h.g.load_ptr_disp32(Reg::V6, Reg::V8, 24);
        h.g.cmp_reg_reg(Reg::V2, Reg::V6);
        h.g.jcc_label(C::Equal, next);
        h.g.bind_text_label(bad);
        h.value(Src::Raw(5));
        h.g.add_reg_imm32(Reg::V0, 1);
        h.keep(5);
        h.g.bind_text_label(next);
        h.g.mov_reg_reg(Reg::V0, Reg::V8);
        h.g.add_reg_imm32(Reg::V0, 32);
        h.keep(4);
    });
    h.report("cases", |h| h.g.mov_imm64(Reg::V0, count as u64));
    h.report("mismatches", |h| h.value(Src::Raw(5)));
}

#[test]
fn production_load_colours_and_masks_match_all_4096_abi_phase_cases() {
    let o = run(access_matrix, 0, false);
    assert_eq!(o.get("cases"), 4096);
    assert_eq!(o.get("mismatches"), 0, "{}", o.text());
}

fn stale_remap(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 42);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.g.call_label(h.l.publish_pending);
    adopt(
        &mut h,
        PhaseSnapshot::new(Phase::Idle, Phase::Idle, 4, 1).unwrap(),
    );
    h.report("same_target", |h| h.field_is(Src::Root(0), 0, Src::Raw(1)));
    h.report("healed", |h| h.load(Src::Root(0), 0));
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
    h.report_counter("collections", Counter::Collections);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.report("stored", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_load_recolours_a_stale_nonmoving_reference_without_copying() {
    let o = run(stale_remap, 0, false);
    let p = PhaseSnapshot::new(Phase::Idle, Phase::Idle, 4, 1).unwrap();
    assert_eq!(o.get("same_target"), 1, "{}", o.text());
    assert_eq!(o.get("healed") & 0xffff, i64::from(p.color_unremembered()));
    assert_eq!(o.get("stored") & 0xffff, i64::from(p.color()));
    assert_eq!(o.get("collections"), 0);
    assert_eq!(o.get("value"), 42);
}

fn marking_load(g: &mut NativeCodeGenerator, case: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 42);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    if case >= 2 {
        for _ in 0..2 {
            h.collect();
            h.finish_collection();
        }
    }
    h.load_barriered(Src::Root(0), 0);
    h.keep(2);
    h.g.call_label(h.l.publish_pending);
    h.mark_start();
    let p = if case.is_multiple_of(2) {
        PhaseSnapshot::new(Phase::ConcurrentMark, Phase::Idle, 1, 1).unwrap()
    } else {
        PhaseSnapshot::new(Phase::Idle, Phase::ConcurrentMark, 2, 1).unwrap()
    };
    adopt(&mut h, p);
    h.report("before", |h| h.marked(Src::Raw(2)));
    h.report("same_target", |h| h.field_is(Src::Root(0), 0, Src::Raw(2)));
    h.report("after_load", |h| h.marked(Src::Raw(2)));
    h.report("field", |h| h.load(Src::Root(0), 0));
    h.drain_marks();
    h.report("after_drain", |h| h.marked(Src::Raw(2)));
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
}

#[test]
fn production_load_mark_protection_obeys_the_target_generation() {
    for case in 0..4 {
        let o = run(marking_load, case, false);
        let needs_mark = case != 2;
        let p = if case.is_multiple_of(2) {
            PhaseSnapshot::new(Phase::ConcurrentMark, Phase::Idle, 1, 1).unwrap()
        } else {
            PhaseSnapshot::new(Phase::Idle, Phase::ConcurrentMark, 2, 1).unwrap()
        };
        assert_eq!(o.get("before"), 0, "{case}: {}", o.text());
        assert_eq!(o.get("same_target"), 1);
        assert_eq!(
            o.get("after_load"),
            i64::from(needs_mark),
            "{case}: {}",
            o.text()
        );
        let color = if needs_mark {
            p.color_unremembered()
        } else {
            PhaseSnapshot::initial().color_unremembered()
        };
        assert_eq!(
            o.get("field") & 0xffff,
            i64::from(color),
            "{case}: {}",
            o.text()
        );
        assert_eq!(o.get("after_drain"), 1);
        assert_eq!(o.get("value"), 42);
    }
}

fn metadata_matrix(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut cases = Vec::new();
    for meta in 0..=u16::MAX {
        let word = ((HEAP_PAGE_BYTES + 32) << 16) | u64::from(meta);
        cases.extend([
            word,
            u64::from(crate::gc_abi::GcRef::from_bits(word).is_ok()),
        ]);
    }
    let data = g
        .asm
        .data_label_with_i64s(&cases.iter().map(|v| *v as i64).collect::<Vec<_>>());
    let mut h = H::begin(g);
    h.g.mov_data_addr(Reg::V0, data);
    h.keep(4);
    h.g.mov_imm64(Reg::V0, 0);
    h.keep(5);
    h.keep(6);
    h.repeat(65536, |h| {
        let bad = h.g.create_text_label();
        let parsed = h.g.create_text_label();
        let next = h.g.create_text_label();
        h.value(Src::Raw(4));
        h.g.mov_reg_reg(Reg::V8, Reg::V0);
        h.g.load_ptr_disp32(Reg::V0, Reg::V8, 0);
        reference_offset(h.g, Reg::V0, Reg::V1, bad);
        h.g.mov_imm64(Reg::V0, 1);
        h.g.jmp_label(parsed);
        h.g.bind_text_label(bad);
        h.g.mov_imm64(Reg::V0, 0);
        h.g.bind_text_label(parsed);
        h.g.mov_reg_reg(Reg::V6, Reg::V0);
        h.g.load_ptr_disp32(Reg::V2, Reg::V8, 8);
        h.g.cmp_reg_reg(Reg::V0, Reg::V2);
        h.g.jcc_label(C::Equal, next);
        h.value(Src::Raw(5));
        h.g.add_reg_imm32(Reg::V0, 1);
        h.keep(5);
        h.g.bind_text_label(next);
        h.value(Src::Raw(6));
        h.g.add_reg_reg(Reg::V0, Reg::V6);
        h.keep(6);
        h.g.mov_reg_reg(Reg::V0, Reg::V8);
        h.g.add_reg_imm32(Reg::V0, 16);
        h.keep(4);
    });
    h.report("accepted", |h| h.value(Src::Raw(6)));
    h.report("mismatches", |h| h.value(Src::Raw(5)));
}

#[test]
fn production_metadata_parser_accepts_exactly_the_48_abi_colours() {
    let o = run(metadata_matrix, 0, false);
    assert_eq!(o.get("accepted"), 48);
    assert_eq!(o.get("mismatches"), 0);
}

fn mark_heals(g: &mut NativeCodeGenerator, forgotten: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 42);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    if forgotten != 0 {
        // A previous GC heal cannot certify remembered bookkeeping.
        h.load_barriered(Src::Root(0), 0);
        h.value(Src::Root(0));
        h.g.mov_reg_reg(Reg::V10, Reg::V0);
        h.g.load_ptr_disp32(Reg::V0, Reg::V10, 0);
        h.g.mov_imm64(Reg::V1, u64::from(crate::gc_abi::REMEMBERED_MASK));
        h.g.or_reg_reg(Reg::V0, Reg::V1);
        h.g.store_ptr_disp32(Reg::V10, 0, Reg::V0);
    }
    h.report("before_field", |h| h.load(Src::Root(0), 0));
    h.mark_start();
    adopt(
        &mut h,
        PhaseSnapshot::new(Phase::Idle, Phase::ConcurrentMark, 7, 1).unwrap(),
    );
    h.drain_marks();
    h.report("after_root", |h| {
        h.g.load_rbp_slot(Reg::V0, slot_offset(0));
        h.g.atomic_load_acquire(Reg::V0, Reg::V0);
    });
    h.report("after_field", |h| h.load(Src::Root(0), 0));
    h.report("child_marked", |h| h.marked(Src::Raw(1)));
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
}

#[test]
fn production_old_marker_recertifies_only_its_own_mark_bits_in_roots_and_fields() {
    let marks = i64::from(OLD_MARK_MASK);
    let current = PhaseSnapshot::new(Phase::Idle, Phase::ConcurrentMark, 7, 1)
        .unwrap()
        .color();
    for forgotten in [0, 1] {
        let o = run(mark_heals, forgotten, false);
        assert_eq!(o.get("child_marked"), 1);
        assert_eq!(o.get("after_field") & marks, i64::from(current) & marks);
        assert_eq!(o.get("after_root") & marks, i64::from(current) & marks);
        assert_eq!(
            o.get("after_field") & !marks,
            o.get("before_field") & !marks
        );
        assert_eq!(o.get("value"), 42);
    }
}

fn intervening_mark_store(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.alloc(1, 16, 2);
    h.set_scalar(Src::Raw(2), 0, 84);
    h.root(2);
    h.mark_start();
    h.drain_marks(); // The old referent is protected before recertification.
    h.load(Src::Root(0), 0);
    h.keep(3);
    adopt(
        &mut h,
        PhaseSnapshot::new(Phase::ConcurrentMark, Phase::ConcurrentMark, 3, 1).unwrap(),
    );
    h.store(Src::Root(0), 0, Src::Root(2), true);
    h.report("replacement", |h| h.load(Src::Root(0), 0));
    h.g.call_label(h.l.lock);
    h.value(Src::Root(0));
    h.g.mov_reg_reg(Reg::V10, Reg::V0);
    h.value(Src::Raw(3));
    crate::gc_codegen::production::test_support::heal_marked_slot(h.g);
    h.g.call_label(h.l.unlock);
    h.report("after_cas", |h| h.load(Src::Root(0), 0));
    h.report("value", |h| h.deref_field(Src::Root(0), 0, 0));
}

#[test]
fn production_mark_recertification_preserves_an_intervening_real_store() {
    let o = run(intervening_mark_store, 0, false);
    assert_eq!(o.get("after_cas"), o.get("replacement"), "{}", o.text());
    assert_eq!(o.get("value"), 84);
}
