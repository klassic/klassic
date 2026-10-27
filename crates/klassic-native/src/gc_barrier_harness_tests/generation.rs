//! Directory consumers under directed layouts, distinct from pool handoff.
use super::{H, Src, run};
use crate::gc_codegen::production::test_support::{
    Counter, GENERATION_LARGE as L, GENERATION_OLD as O, GENERATION_YOUNG as Y,
    generation_destination, generation_entry, generation_flags, generation_heap_bytes,
    generation_load_mask, generation_matches, generation_override, generation_role_flags,
    request_old_cycle, young_end,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use crate::{
    NativeCodeGenerator,
    gc_abi::{HEAP_PAGE_BYTES as PAGE, MarkRecord, REMAP_MASK, YOUNG_MARK_MASK},
};
use std::mem::offset_of;
fn bounds(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    let end = generation_heap_bytes();
    for (i, off) in [
        (0, 0),
        (1, PAGE - 1),
        (2, PAGE),
        (3, young_end() - 1),
        (4, young_end()),
        (5, end - 1),
        (6, end),
        (7, u64::MAX),
    ] {
        for k in [Y, O] {
            h.report(&format!("b{i}_{k}"), |h| generation_matches(h.g, off, k));
        }
    }
    // Role 3 is dedicated large Old; role 4 remains unknown.
    for k in [0, 3, 4, 99, O] {
        generation_entry(h.g, PAGE + 32, k);
        h.report(&format!("low{k}_y"), |h| {
            generation_matches(h.g, PAGE + 32, Y)
        });
        h.report(&format!("low{k}_o"), |h| {
            generation_matches(h.g, PAGE + 32, O)
        });
    }
    generation_entry(h.g, young_end() + 32, Y);
    h.report("high_y", |h| generation_matches(h.g, young_end() + 32, Y));
    h.report("high_o", |h| generation_matches(h.g, young_end() + 32, O));
}
#[test]
fn production_generation_directory_bounds_unknown_tags_and_both_address_sides_are_authoritative() {
    let o = run(bounds, 0, false);
    for (i, k) in [
        (0, 0),
        (1, 0),
        (2, Y),
        (3, O),
        (4, O),
        (5, 0),
        (6, 0),
        (7, 0),
    ] {
        for kind in [Y, O] {
            assert_eq!(
                o.get(&format!("b{i}_{kind}")),
                i64::from(k == kind),
                "{}",
                o.text()
            );
        }
    }
    for k in [0, 3, 4, 99, O] {
        assert_eq!(o.get(&format!("low{k}_y")), 0);
        assert_eq!(o.get(&format!("low{k}_o")), i64::from(k == O || k == 3));
    }
    assert_eq!(o.get("high_y"), 1);
    assert_eq!(o.get("high_o"), 0);
}
fn registers(g: &mut NativeCodeGenerator, exact: u64) {
    let mut h = H::begin(g);
    let bad = h.g.create_text_label();
    let end = h.g.create_text_label();
    h.g.mov_reg_reg(R::V0, R::V3);
    h.keep(15);
    let regs = [
        R::V0,
        R::V1,
        R::V2,
        R::V6,
        R::V7,
        R::V8,
        R::V9,
        R::V10,
        R::V11,
    ];
    for off in regs {
        for (value, role, kind, yes) in [
            (PAGE + 32, Y, Y, true),
            (young_end() + 32, O, Y, false),
            (young_end() + 32, O, O, true),
            (young_end() + 32, L, O, exact == 0),
            (young_end() + 32, L, L, true),
            (young_end() + 32, L, Y, false),
        ] {
            generation_entry(h.g, value, role);
            for (i, reg) in regs.iter().enumerate() {
                h.g.mov_imm64(*reg, 0x51510000 + i as u64);
            }
            h.g.mov_imm64(off, value);
            if exact == 0 {
                generation_flags(h.g, off, kind);
            } else {
                generation_role_flags(h.g, off, kind);
            }
            h.g.jcc_label(if yes { C::NotEqual } else { C::Equal }, bad);
            for (i, reg) in regs.iter().enumerate() {
                h.g.cmp_reg_imm32(
                    *reg,
                    if *reg == off {
                        value as i32
                    } else {
                        0x51510000 + i as i32
                    },
                );
                h.g.jcc_label(C::NotEqual, bad);
            }
            h.g.load_rbp_slot(R::V1, super::slot_offset(15));
            h.g.cmp_reg_reg(R::V3, R::V1);
            h.g.jcc_label(C::NotEqual, bad);
        }
    }
    h.g.mov_imm64(R::V0, 1);
    h.keep(0);
    h.g.jmp_label(end);
    h.g.bind_text_label(bad);
    h.g.mov_imm64(R::V0, 0);
    h.keep(0);
    h.g.bind_text_label(end);
    h.report("preserved", |h| h.value(Src::Raw(0)));
}
#[test]
fn production_generation_lookup_preserves_all_registers_and_flags_with_scratch_aliases() {
    for exact in [0, 1] {
        assert_eq!(run(registers, exact, false).get("preserved"), 1);
    }
}
fn consumers(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
    h.alloc(2, 16, 1);
    h.root(1);
    // Static directed placement for this snapshot, before tracing starts.
    // No allocation or normal collection follows this layout override.
    h.value(Src::Root(0));
    generation_override(h.g, Y);
    h.value(Src::Root(1));
    generation_override(h.g, O);
    h.g.call_label(h.l.young_start);
    h.store(Src::Root(1), 0, Src::Root(0), true);
    h.g.call_label(h.l.lock);
    super::emit_unbounded_budget(h.g);
    h.g.call_label(h.l.young_loop);
    h.g.call_label(h.l.unlock);
    h.report("high_old", |h| h.is_old(Src::Root(0)));
    h.report("low_old", |h| h.is_old(Src::Root(1)));
    h.report("high_stamp", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, young_epoch))
    });
    h.report("low_stamp", |h| {
        h.object_metadata(Src::Root(1), offset_of!(MarkRecord, young_epoch))
    });
    h.report_counter("epoch", Counter::YoungEpoch);
    h.report_counter("marked", Counter::YoungMarked);
    h.report_counter("trace", Counter::CapacityTrace);
    h.report_counter("entries", Counter::RemsetLen);
    h.report("entry", |h| h.entry_is_slot(0, Src::Root(1), 0));
    crate::gc_codegen::production::test_support::generation_remembered_retention(h.g);
    h.report_counter("kept", Counter::RemsetKept);
    h.report_counter("after_entries", Counter::RemsetLen);
    h.report_counter("previous", Counter::RemsetPreviousLen);
    h.report("after_entry", |h| h.entry_is_slot(0, Src::Root(1), 0));
    h.report("high_mask", |h| {
        h.value(Src::Root(0));
        generation_load_mask(h.g);
    });
    h.report("low_mask", |h| {
        h.value(Src::Root(1));
        generation_load_mask(h.g);
    });
    h.report("value", |h| h.deref_field(Src::Root(1), 0, 0));
}
#[test]
fn production_generation_directory_controls_young_marking_load_masks_and_low_address_remembered_holders()
 {
    let o = run(consumers, 0, false);
    for (n, v) in [
        ("high_old", 0),
        ("low_old", 1),
        ("high_stamp", 1),
        ("low_stamp", 0),
        ("epoch", 1),
        ("marked", 1),
        ("trace", 48),
        ("entries", 1),
        ("entry", 1),
        ("kept", 1),
        ("after_entries", 1),
        ("previous", 0),
        ("after_entry", 1),
        ("high_mask", i64::from(REMAP_MASK | YOUNG_MARK_MASK)),
        ("low_mask", i64::from(REMAP_MASK)),
        ("value", 84),
    ] {
        assert_eq!(o.get(n), v, "{n}: {}", o.text());
    }
}
fn moving(g: &mut NativeCodeGenerator, high: u64) {
    let mut h = H::begin(g);
    h.alloc(1, 16, 0);
    h.set_scalar(Src::Raw(0), 0, 84);
    h.root(0);
    if high != 0 {
        for _ in 0..2 {
            h.collect();
            h.finish_collection();
        }
    }
    h.value(Src::Root(0));
    generation_override(h.g, if high != 0 { Y } else { O });
    request_old_cycle(h.g);
    h.collect();
    h.finish_collection();
    h.report("age", |h| {
        h.object_metadata(Src::Root(0), offset_of!(MarkRecord, age))
    });
    h.report_counter("young", Counter::AgeYoung);
    h.report_counter("promoted", Counter::AgePromoted);
    h.report("id", |h| h.object_id(Src::Root(0)));
    h.report("value", |h| h.load(Src::Root(0), 0));
}
#[test]
fn production_generation_directory_controls_copy_age_and_winner_accounting_in_a_directed_full_move()
{
    for high in [0, 1] {
        let o = run(moving, high, false);
        for (n, v) in [
            ("age", if high == 0 { 0 } else { 3 }),
            ("young", 0),
            ("promoted", if high == 0 { 0 } else { 48 }),
            ("id", 1),
            ("value", 84),
        ] {
            assert_eq!(o.get(n), v, "{n}, high {high}: {}", o.text());
        }
    }
}

fn wrong_destination(g: &mut NativeCodeGenerator, actor: u64) {
    let mut h = H::begin(g);
    match actor {
        0 => {
            generation_entry(h.g, PAGE, O);
            h.alloc(1, 16, 0); // TLAB must not write into an Old-owned page.
        }
        1 => {
            h.alloc(1, 16, 0);
            h.root(0);
            h.collect();
            generation_destination(h.g, None, O);
            h.relocate_quantum(1, true);
        }
        2 | 5 => {
            h.alloc(1, 16, 0);
            h.root(0);
            h.collect();
            h.finish_collection();
            generation_destination(h.g, Some(0), if actor == 5 { L } else { Y });
            h.collect();
            h.finish_collection(); // Age-two promotion.
        }
        3 | 6 => {
            // First promotion uses page_bytes-16, so the second header and
            // payload straddle into the next page. Its first page stays Old.
            h.alloc(1, crate::gc_codegen::production::MAX_PAYLOAD - 16, 0);
            h.root(0);
            h.alloc(1, 16, 1);
            h.root(1);
            h.collect();
            h.finish_collection();
            generation_destination(h.g, Some(PAGE), if actor == 6 { L } else { Y });
            h.collect();
            h.finish_collection();
        }
        4 | 7 => {
            h.alloc(1, crate::gc_codegen::production::MAX_PAYLOAD - 16, 0);
            h.root(0);
            h.alloc(1, 16, 1);
            h.root(1);
            h.collect();
            h.finish_collection();
            h.collect();
            h.value(Src::Root(0)); // Actually reserve page_bytes-16 Old bytes.
            generation_destination(h.g, Some(0), if actor == 7 { L } else { Y });
            h.finish_collection(); // Next start is wrong; its final page is Old.
        }
        _ => unreachable!(),
    }
}
#[test]
fn production_generation_directory_rejects_wrong_tlab_worker_copy_and_promotion_destinations() {
    for actor in 0..8 {
        let o = super::run_failure(wrong_destination, actor);
        assert_eq!(o.status.code(), Some(1), "actor {actor}");
        assert!(
            String::from_utf8_lossy(&o.stderr).contains("runtime invariant violated"),
            "actor {actor}"
        );
    }
}
