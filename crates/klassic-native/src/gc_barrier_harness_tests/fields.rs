//! Heap fields use the GcRef representation; raw boxes remain opaque.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_abi::PhaseSnapshot;
use crate::gc_codegen::production::test_support::Counter;
use crate::portable_asm::{PortableAsm, Reg};

fn encoded_field(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 12345);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.report("expected", |h| {
        h.value(Src::Raw(1));
        h.g.load_ptr_disp32(
            Reg::V1,
            Reg::V3,
            crate::gc_codegen::production::test_support::heap_base_offset(),
        );
        h.g.sub_reg_reg(Reg::V0, Reg::V1);
        h.g.shl_reg_imm8(Reg::V0, 16);
        h.g.mov_imm64(Reg::V1, u64::from(PhaseSnapshot::initial().color()));
        h.g.or_reg_reg(Reg::V0, Reg::V1);
    });
    h.report("stored", |h| h.load(Src::Root(0), 0));
    h.report("decoded", |h| h.field_is(Src::Root(0), 0, Src::Raw(1)));
    for name in ["young_value", "promoted_value"] {
        h.collect();
        h.finish_collection();
        h.report(name, |h| h.deref_field(Src::Root(0), 0, 0));
    }
    h.report("moved_bits", |h| h.load(Src::Root(0), 0));
}

#[test]
fn production_heap_fields_store_gc_ref_bits_and_survive_promotion() {
    let o = run(encoded_field, 0, false);
    assert_eq!(o.get("stored"), o.get("expected"), "{}", o.text());
    assert_eq!(o.get("decoded"), 1);
    assert_eq!(o.get("young_value"), 12345);
    assert_eq!(o.get("promoted_value"), 12345);
    assert_eq!(
        o.get("moved_bits") & 0xffff,
        i64::from(PhaseSnapshot::initial().color_unremembered())
    );
}

fn opaque_boxes(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.alloc(1, 16, 2);
    h.value(Src::Raw(1));
    h.keep(4);
    h.g.mov_reg_reg(Reg::V1, Reg::V0);
    h.value(Src::Raw(2));
    h.g.store_ptr_disp32(Reg::V0, 0, Reg::V1);
    h.load(Src::Root(0), 0); // Actual GcRef bits, used as an opaque integer.
    h.keep(5);
    h.g.mov_reg_reg(Reg::V1, Reg::V0);
    h.value(Src::Raw(2));
    h.g.store_ptr_disp32(Reg::V0, 8, Reg::V1);
    h.root(2);
    h.collect(); // Leave relocation open so the address aliases from-space.
    h.report("raw_expected", |h| h.value(Src::Raw(4)));
    h.report("bits_expected", |h| h.value(Src::Raw(5)));
    h.report("raw_read", |h| h.load_untyped(Src::Root(2), 0));
    h.report("bits_read", |h| h.load_untyped(Src::Root(2), 1));
    h.load_untyped(Src::Root(0), 0);
    h.keep(3);
    h.report("reference_decoded", |h| {
        h.field_is(Src::Root(0), 0, Src::Raw(3))
    });
    h.finish_collection();
}

#[test]
fn production_untyped_reads_use_owner_shape_and_keep_pointer_like_scalars_opaque() {
    let o = run(opaque_boxes, 0, false);
    assert_eq!(o.get("raw_read"), o.get("raw_expected"), "{}", o.text());
    assert_eq!(o.get("bits_read"), o.get("bits_expected"), "{}", o.text());
    assert_eq!(o.get("reference_decoded"), 1, "{}", o.text());
}

fn forgotten_heal(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.collect();
    h.load_barriered(Src::Root(0), 0); // A mutator load relocates the child.
    h.report("healed", |h| h.load(Src::Root(0), 0));
    h.finish_collection();
    h.mark_start(); // The snapshot sees the holder, not its child yet.
    h.load_barriered(Src::Root(0), 0);
    h.keep(1);
    h.report("marked_before", |h| h.marked(Src::Raw(1)));
    h.store(Src::Root(0), 0, Src::Null, true);
    h.report("marked_after", |h| h.marked(Src::Raw(1)));
    h.report("null", |h| h.load(Src::Root(0), 0));
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.report("stored", |h| h.load(Src::Root(0), 0));
    h.report("decoded", |h| h.field_is(Src::Root(0), 0, Src::Raw(1)));
    h.drain_marks();
}

#[test]
fn production_gc_heals_forget_store_certification_and_real_stores_restore_it() {
    let o = run(forgotten_heal, 0, false);
    assert_eq!(
        o.get("healed") & 0xffff,
        i64::from(
            PhaseSnapshot::new(crate::gc_abi::Phase::Idle, crate::gc_abi::Phase::Idle, 4, 0)
                .unwrap()
                .color_unremembered()
        )
    );
    assert_eq!(
        o.get("marked_before"),
        1,
        "the stale-color load protects first: {}",
        o.text()
    );
    assert_eq!(o.get("marked_after"), 1, "{}", o.text());
    assert_eq!(o.get("null"), 0);
    assert_eq!(
        o.get("stored") & 0xffff,
        i64::from(
            PhaseSnapshot::new(crate::gc_abi::Phase::Idle, crate::gc_abi::Phase::Idle, 6, 0)
                .unwrap()
                .color()
        )
    );
    assert_eq!(o.get("decoded"), 1);
}

fn remset_certification(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.alloc(1, 16, 1);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.collect();
    h.relocate_quantum(1, true); // Consume this remembered entry outside the pause.
    h.report("kept", |h| h.load(Src::Root(0), 0));
    h.report_counter("kept_len", Counter::RemsetLen);
    h.finish_collection();
    h.collect();
    h.relocate_quantum(1, false);
    h.report("promoted", |h| h.load(Src::Root(0), 0));
    h.report_counter("promoted_len", Counter::RemsetLen);
    h.finish_collection();
    h.alloc(1, 16, 1);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.report("stored", |h| h.load(Src::Root(0), 0));
    h.report_counter("stored_len", Counter::RemsetLen);
    h.report("decoded", |h| h.field_is(Src::Root(0), 0, Src::Raw(1)));
}

#[test]
fn production_remset_recertifies_only_after_remembering_a_young_target() {
    let o = run(remset_certification, 0, false);
    assert_eq!(
        o.get("kept") & 0xffff,
        i64::from(
            PhaseSnapshot::new(crate::gc_abi::Phase::Idle, crate::gc_abi::Phase::Idle, 4, 0)
                .unwrap()
                .color()
        )
    );
    assert_eq!(o.get("kept_len"), 1, "{}", o.text());
    assert_eq!(
        o.get("promoted") & 0xffff,
        i64::from(PhaseSnapshot::initial().color_unremembered())
    );
    assert_eq!(o.get("promoted_len"), 0, "{}", o.text());
    assert_eq!(
        o.get("stored") & 0xffff,
        i64::from(PhaseSnapshot::initial().color())
    );
    assert_eq!(o.get("stored_len"), 1, "{}", o.text());
    assert_eq!(o.get("decoded"), 1);
}
