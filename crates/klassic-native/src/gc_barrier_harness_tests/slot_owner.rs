//! Exercise the production page/card owner lookup with actual promoted objects.
use super::{H, Src, run};
use crate::NativeCodeGenerator;
use crate::gc_codegen::production::{
    MAX_PAYLOAD,
    test_support::{Counter, heap_base_offset},
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

fn promote(h: &mut H<'_>) {
    for _ in 0..2 {
        h.collect();
        h.finish_collection();
    }
}

fn query(h: &mut H<'_>, object: Src, displacement: i32) {
    h.value(object);
    h.g.add_reg_imm32(R::V0, displacement);
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.call_label(h.l.slot_owner);
}

fn matches(h: &mut H<'_>, object: Src, displacement: i32) {
    h.value(object);
    h.keep(30);
    query(h, object, displacement);
    h.g.load_ptr_disp32(R::V1, R::V3, heap_base_offset());
    h.g.add_reg_reg(R::V0, R::V1);
    h.g.load_rbp_slot(R::V1, super::slot_offset(30));
    let bad = h.g.create_text_label();
    let done = h.g.create_text_label();
    h.g.cmp_reg_reg(R::V0, R::V1);
    h.g.jcc_label(C::NotEqual, bad);
    h.g.mov_imm64(R::V0, 1);
    h.g.jmp_label(done);
    h.g.bind_text_label(bad);
    h.g.mov_imm64(R::V0, 0);
    h.g.bind_text_label(done);
}

fn shapes(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    for (slot, tag, bytes) in [(0, 2, 65536), (1, 4, 40), (2, 1, 32), (3, 2, 8)] {
        h.alloc(tag, bytes, slot);
        if tag == 4 {
            h.set_scalar(Src::Raw(slot), 0, 4);
        }
        h.root(slot);
    }
    promote(&mut h);
    for displacement in [0, 8, 976, 1008, 1024, 2048, 65528] {
        h.report(&format!("record_{displacement}"), |h| {
            matches(h, Src::Root(0), displacement)
        });
    }
    for displacement in [8, 16, 32] {
        h.report(&format!("list_{displacement}"), |h| {
            matches(h, Src::Root(1), displacement)
        });
    }
    h.report("small", |h| matches(h, Src::Root(3), 0));
    for (name, slot, displacement) in [
        ("header", 0, -32),
        ("misaligned", 0, 1),
        ("record_end", 0, 65536),
        ("list_length", 1, 0),
        ("list_end", 1, 40),
        ("raw", 2, 0),
        ("padding", 3, 8),
    ] {
        h.report(name, |h| query(h, Src::Root(slot), displacement));
    }
    h.alloc(2, 16, 4);
    h.report("young", |h| query(h, Src::Raw(4), 0));
    for address in [0, 8, u64::MAX - 7] {
        h.report(&format!("outside_{address}"), |h| {
            query(h, Src::Imm(address), 0)
        });
    }
}

#[test]
fn production_slot_owner_accepts_only_complete_reference_fields_of_published_old_objects() {
    let o = run(shapes, 0, false);
    for displacement in [0, 8, 976, 1008, 1024, 2048, 65528] {
        assert_eq!(o.get(&format!("record_{displacement}")), 1, "{}", o.text());
    }
    for displacement in [8, 16, 32] {
        assert_eq!(o.get(&format!("list_{displacement}")), 1);
    }
    assert_eq!(o.get("small"), 1);
    for name in [
        "header",
        "misaligned",
        "record_end",
        "list_length",
        "list_end",
        "raw",
        "padding",
        "young",
    ] {
        assert_eq!(o.get(name), 0, "{name}: {}", o.text());
    }
    for address in [0, 8, u64::MAX - 7] {
        assert_eq!(o.get(&format!("outside_{address}")), 0);
    }
}

fn boundary(g: &mut NativeCodeGenerator, kind: u64) {
    let mut h = H::begin(g);
    let filler = match kind {
        0 => 960,
        1 => 944,
        _ => MAX_PAYLOAD - 16,
    };
    h.alloc(1, filler, 0);
    h.root(0);
    h.alloc(2, 8192, 1);
    h.root(1);
    promote(&mut h);
    for displacement in [0, 8, 1008, 1024, 8184] {
        h.report(&format!("field_{displacement}"), |h| {
            matches(h, Src::Root(1), displacement)
        });
    }
    h.report("header", |h| query(h, Src::Root(1), -8));
    h.report("end", |h| query(h, Src::Root(1), 8192));
}

#[test]
fn production_slot_owner_handles_bit_zero_bit_sixty_three_and_headers_crossing_pages() {
    for kind in 0..3 {
        let o = run(boundary, kind, false);
        for displacement in [0, 8, 1008, 1024, 8184] {
            assert_eq!(
                o.get(&format!("field_{displacement}")),
                1,
                "kind {kind}: {}",
                o.text()
            );
        }
        for name in ["header", "end"] {
            assert_eq!(o.get(name), 0, "kind {kind}: {}", o.text());
        }
    }
}

fn publication(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_abi::{OBJECT_PUBLISHED, ObjectHeader};
    use crate::gc_codegen::production::test_support::clear_owner_start;
    use std::mem::{offset_of, size_of};
    let mut h = H::begin(g);
    h.alloc(2, 8192, 0);
    h.root(0);
    promote(&mut h);
    let state = offset_of!(ObjectHeader, state) as i32 - size_of::<ObjectHeader>() as i32;
    h.value(Src::Root(0));
    h.g.mov_imm64(R::V1, 0);
    h.g.store_ptr_disp32(R::V0, state, R::V1);
    h.report("unpublished", |h| query(h, Src::Root(0), 1024));
    h.value(Src::Root(0));
    h.g.mov_imm64(R::V1, OBJECT_PUBLISHED);
    h.g.store_ptr_disp32(R::V0, state, R::V1);
    h.report("published", |h| matches(h, Src::Root(0), 1024));
    h.value(Src::Root(0));
    clear_owner_start(h.g);
    h.report("unregistered_hint", |h| query(h, Src::Root(0), 1024));
}

#[test]
fn production_slot_owner_requires_publication_and_exact_start_membership_even_for_card_hints() {
    let o = run(publication, 0, false);
    assert_eq!(o.get("unpublished"), 0);
    assert_eq!(o.get("published"), 1);
    assert_eq!(o.get("unregistered_hint"), 0);
}

fn dead_holder(g: &mut NativeCodeGenerator, sealed: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 65536, 0);
    h.root(0);
    promote(&mut h);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.root(1);
    h.store(Src::Root(0), 1024, Src::Root(1), true);
    h.value(Src::Root(0));
    h.keep(29);
    h.value(Src::Null);
    h.set_root(0);
    h.mark_start();
    if sealed != 0 {
        h.drain_marks();
        h.end_marks();
        protect_dead_slot(&mut h, 0);
        // Exercise Young remembered-root discovery as well as relocation's
        // previous-set scanner; automatic Young triggering is off in H.
        h.g.call_label(h.l.young_start);
        h.g.call_label(h.l.lock);
        super::emit_unbounded_budget(h.g);
        h.g.call_label(h.l.young_loop);
        h.g.call_label(h.l.unlock);
    }
    h.report_counter("before", Counter::RemsetLen);
    h.collect();
    h.finish_collection();
    if sealed != 0 {
        protect_dead_slot(&mut h, 3);
    }
    h.report_counter("after", Counter::RemsetLen);
    h.report("child", |h| h.load(Src::Root(1), 0));
}

fn protect_dead_slot(h: &mut H<'_>, permissions: u64) {
    h.value(Src::Raw(29));
    h.g.add_reg_imm32(R::V0, 8192);
    h.g.and_reg_imm32(R::V0, -4096);
    h.g.mov_reg_reg(R::V7, R::V0);
    h.g.mov_imm64(R::V6, 4096);
    h.g.mov_imm64(R::V2, permissions);
    h.g.mov_imm64(R::V0, 10); // Linux mprotect, excluding the owner header.
    h.g.asm.syscall();
    h.keep(28);
    h.report(
        if permissions == 0 {
            "protected"
        } else {
            "restored"
        },
        |h| h.value(Src::Raw(28)),
    );
}

#[test]
fn production_remembered_slots_prune_dead_holders_even_when_the_child_is_live() {
    for sealed in [0, 1] {
        let o = run(dead_holder, sealed, false);
        assert_eq!(o.get("before"), 1);
        assert_eq!(o.get("after"), 1 - sealed as i64, "{}", o.text());
        assert_eq!(o.get("child"), 84);
        if sealed != 0 {
            assert_eq!(o.get("protected"), 0);
            assert_eq!(o.get("restored"), 0);
        }
    }
}

fn registers(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 8192, 0);
    h.root(0);
    promote(&mut h);
    let bad = h.g.create_text_label();
    let done = h.g.create_text_label();
    for displacement in [0, 1024, 8184] {
        h.value(Src::Root(0));
        h.g.add_reg_imm32(R::V0, displacement);
        h.keep(31);
        h.g.mov_reg_reg(R::V7, R::V0);
        let regs = [R::V1, R::V2, R::V6, R::V8, R::V9, R::V10, R::V11];
        for (i, reg) in regs.iter().enumerate() {
            h.g.mov_imm64(*reg, 0x51510000 + i as u64);
        }
        h.g.call_label(h.l.slot_owner);
        for (i, reg) in regs.iter().enumerate() {
            h.g.cmp_reg_imm32(*reg, 0x51510000 + i as i32);
            h.g.jcc_label(C::NotEqual, bad);
        }
        h.g.load_rbp_slot(R::V0, super::slot_offset(31));
        h.g.cmp_reg_reg(R::V7, R::V0);
        h.g.jcc_label(C::NotEqual, bad);
    }
    h.g.mov_imm64(R::V0, 1);
    h.g.jmp_label(done);
    h.g.bind_text_label(bad);
    h.g.mov_imm64(R::V0, 0);
    h.g.bind_text_label(done);
    h.keep(30);
    h.report("preserved", |h| h.value(Src::Raw(30)));
}

#[test]
fn production_slot_owner_preserves_all_nonresult_registers_for_bitmap_and_hint_paths() {
    assert_eq!(run(registers, 0, false).get("preserved"), 1);
}

fn newborn(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::{
        GENERATION_OLD, generation_override, generation_remembered_retention,
    };
    let mut h = H::begin(g);
    h.mark_start();
    h.drain_marks();
    h.end_marks();
    h.alloc(2, MAX_PAYLOAD, 0);
    h.root(0);
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    // Isolate the snapshot birth guard before a moving cycle clears validity.
    // This directed layout is not a production pool ownership transfer.
    h.value(Src::Root(0));
    generation_override(h.g, GENERATION_OLD);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.report_counter("valid", Counter::MarkValid);
    generation_remembered_retention(h.g);
    h.report_counter("after", Counter::RemsetLen);
    h.report("child", |h| h.deref_field(Src::Root(0), 0, 0));
}

#[test]
fn production_remembered_slots_keep_holders_born_after_the_completed_old_snapshot() {
    let o = run(newborn, 0, false);
    assert_eq!(o.get("valid"), 1);
    assert_eq!(o.get("after"), 1);
    assert_eq!(o.get("child"), 84);
}

fn newborn_child(g: &mut NativeCodeGenerator, _arg: u64) {
    let mut h = H::begin(g);
    h.alloc(2, 16, 0);
    h.root(0);
    promote(&mut h);
    h.mark_start();
    h.drain_marks();
    h.end_marks();
    h.alloc(1, 16, 1);
    h.set_scalar(Src::Raw(1), 0, 84);
    h.store(Src::Root(0), 0, Src::Raw(1), true);
    h.report_counter("valid", Counter::MarkValid);
    h.collect();
    h.finish_collection();
    h.report_counter("after", Counter::RemsetLen);
    h.report_counter("unmarked", Counter::Unmarked);
    h.report("child", |h| h.deref_field(Src::Root(0), 0, 0));
}

#[test]
fn production_remembered_slots_keep_new_children_of_existing_snapshot_holders() {
    let o = run(newborn_child, 0, false);
    assert_eq!(o.get("valid"), 1);
    assert_eq!(o.get("after"), 1, "{}", o.text());
    assert_eq!(o.get("child"), 84);
    assert_eq!(o.get("unmarked"), 0, "{}", o.text());
}

fn retirement(g: &mut NativeCodeGenerator, _arg: u64) {
    use crate::gc_codegen::production::test_support::{
        emit_budget, old_owner_word, request_old_cycle,
    };
    let mut h = H::begin(g);
    h.alloc(2, MAX_PAYLOAD, 0);
    h.root(0);
    promote(&mut h);
    for pass in 0..2 {
        let n = |name: &str| format!("p{pass}_{name}");
        request_old_cycle(h.g);
        h.collect();
        h.g.call_label(h.l.lock);
        h.g.call_label(h.l.close_relocation);
        h.g.call_label(h.l.unlock);
        let found = h.g.create_text_label();
        h.repeat(400_000, |h| {
            h.g.call_label(h.l.lock);
            emit_budget(h.g, 1);
            h.g.call_label(h.l.retire_step);
            h.g.call_label(h.l.unlock);
            h.counter(Counter::RetireStage);
            h.g.cmp_reg_imm8(R::V0, 13);
            h.g.jcc_label(C::Equal, found);
        });
        h.g.bind_text_label(found);
        h.report_counter(&n("stage"), Counter::RetireStage);
        for hint in [false, true] {
            h.report(
                &n(if hint { "hint_before" } else { "starts_before" }),
                |h| {
                    old_owner_word(h.g, hint);
                    h.g.load_ptr_disp32(R::V0, R::V0, 0);
                },
            );
        }
        h.report_counter(&n("units_before"), Counter::RetireUnits);
        h.report_counter(&n("cursor_before"), Counter::RetireCursor);
        for units in [0, 7] {
            h.g.call_label(h.l.lock);
            emit_budget(h.g, units);
            h.g.call_label(h.l.retire_step);
            h.g.call_label(h.l.unlock);
            h.report_counter(&n(&format!("units_{units}")), Counter::RetireUnits);
        }
        h.report_counter(&n("cursor_after"), Counter::RetireCursor);
        h.alloc(1, 16, 1);
        h.set_scalar(Src::Raw(1), 0, 84 + pass);
        h.store(
            Src::Root(0),
            (MAX_PAYLOAD / 8 - 1) as u32,
            Src::Raw(1),
            true,
        );
        h.report_counter(&n("during"), Counter::RetireStage);
        h.finish_collection();
        for hint in [false, true] {
            h.report(&n(if hint { "hint_after" } else { "starts_after" }), |h| {
                old_owner_word(h.g, hint);
                h.g.load_ptr_disp32(R::V0, R::V0, 0);
            });
        }
        h.report(&n("destination"), |h| {
            matches(h, Src::Root(0), MAX_PAYLOAD as i32 - 8)
        });
        h.collect();
        h.finish_collection();
        h.report(&n("child"), |h| {
            h.deref_field(Src::Root(0), (MAX_PAYLOAD / 8 - 1) as u32, 0)
        });
    }
}

#[test]
fn production_owner_metadata_retirement_is_budgeted_and_preserves_live_destination_across_reuse() {
    let o = run(retirement, 0, false);
    for pass in 0..2 {
        let get = |name: &str| o.get(&format!("p{pass}_{name}"));
        assert_eq!(get("stage"), 13, "{}", o.text());
        assert!(get("starts_before") != 0);
        assert!(get("hint_before") != 0);
        assert_eq!(get("units_0"), get("units_before"));
        assert_eq!(get("units_7") - get("units_before"), 7);
        assert_eq!(get("cursor_after") - get("cursor_before"), 56);
        assert_eq!(get("during"), 13);
        assert_eq!(get("starts_after"), 0);
        assert_eq!(get("hint_after"), 0);
        assert_eq!(get("destination"), 1);
        assert_eq!(get("child"), 84 + pass);
    }
}
