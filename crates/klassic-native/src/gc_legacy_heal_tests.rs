//! Execute the actual legacy runtime emitters against stale observations.
use super::{generator, run_c_driver};
use crate::portable_asm::{self, PortableAsm, Reg as P};

#[test]
fn gc_legacy_heal_preserves_replacement_and_observed_result() {
    let mut g = generator();
    let phase = g.asm.data_label_with_i64s(&[0]);
    let base = g.asm.data_label_with_i64s(&[0]);
    let from = g.asm.data_label_with_i64s(&[0]);
    let marked = g.asm.data_label_with_i64s(&[0]);
    let top = g.asm.data_label_with_i64s(&[0]);
    let work = g.asm.data_label_with_i64s(&[0]);
    let cursor = g.asm.data_label_with_i64s(&[0]);
    let end = g.asm.data_label_with_i64s(&[0]);
    let scan = portable_asm::MarkScanCells { top, cursor, end };
    let quantum = g.asm.data_label_with_i64s(&[0]);
    let heap_top = g.asm.data_label_with_i64s(&[0]);
    let heap_end = g.asm.data_label_with_i64s(&[0]);
    let end_calls = g.asm.data_label_with_i64s(&[0]);
    let slow = g.create_text_label();
    let trace = g.create_text_label();
    let drain = g.create_text_label();
    let alloc = g.create_text_label();
    let restart = g.create_text_label();
    let store = g.create_text_label();
    let store_race = g.create_text_label();
    let racing_mark = g.create_text_label();
    let race_count = g.asm.data_label_with_i64s(&[0]);
    let race_slot = g.asm.data_label_with_i64s(&[0]);
    let race_value = g.asm.data_label_with_i64s(&[0]);
    let store_mark = g.create_text_label();
    let header_mark = g.asm.data_label_with_i64s(&[2]);
    let fallback = g.asm.data_label_with_i64s(&[0]);
    let end_mark = g.create_text_label();
    let evacuate = g.create_text_label();
    let mark = g.create_text_label();
    let mut offsets = Vec::new();
    for target in [slow, trace, drain, alloc, restart, store, store_race] {
        offsets.push(g.asm.text.len());
        g.push_reg(P::V5);
        g.mov_reg_reg(P::V5, P::V4);
        g.push_reg(P::ColorStrip);
        g.push_reg(P::GoodColor);
        g.mov_imm64(P::ColorStrip, crate::gc_layout::GC_COLOR_STRIP);
        g.mov_imm64(P::GoodColor, crate::gc_layout::GC_COLOR_R);
        if target == slow {
            g.mov_reg_reg(P::V10, P::V7); // slot
            g.mov_reg_reg(P::V0, P::V6); // exact old observation
        }
        g.call_label(target);
        g.pop_reg(P::GoodColor);
        g.pop_reg(P::ColorStrip);
        g.leave();
        g.ret();
    }
    portable_asm::emit_gc_load_barrier_slow(&mut g, slow, phase, base, from, evacuate, mark);
    portable_asm::emit_gc_trace(&mut g, trace, scan, work, mark);
    portable_asm::emit_gc_drain(&mut g, drain, trace, scan);
    portable_asm::emit_gc_alloc(
        &mut g,
        alloc,
        portable_asm::AllocCells {
            phase,
            bytes_since_cycle: base,
            budget_regions: base,
            bytes_since_quantum: quantum,
            mark_scan: scan,
            header_mark: base,
            alloc_count: base,
            bytes_allocated: base,
            heap_top,
            heap_end,
            oom_text: base,
        },
        portable_asm::AllocTargets {
            mark_start: end_mark,
            trace,
            mark_end: end_mark,
            relocate_quantum: end_mark,
            collect: end_mark,
            grow_budget: end_mark,
            acquire_region: end_mark,
            alloc_large: end_mark,
        },
        portable_asm::AllocFlags {
            stress: false,
            log: false,
        },
        0,
        2,
    );
    portable_asm::emit_gc_stw_mark_complete(
        &mut g,
        restart,
        portable_asm::StwMarkTargets {
            clear_all_marks: evacuate,
            mark_roots: evacuate,
            drain,
        },
        from,
        scan,
        base,
        0,
    );
    g.bind_text_label(end_mark);
    g.mov_data_addr(P::V10, end_calls);
    g.load_ptr_disp32(P::V0, P::V10, 0);
    g.add_reg_imm32(P::V0, 1);
    g.store_ptr_disp32(P::V10, 0, P::V0);
    g.ret();
    g.bind_text_label(evacuate);
    g.ret(); // test covers existing forwarding, not relocation allocation
    g.bind_text_label(mark);
    g.mov_data_addr(P::V10, marked);
    g.store_ptr_disp32(P::V10, 0, P::V7);
    g.ret();
    portable_asm::emit_gc_store_pointer(&mut g, store, phase, store_mark);
    portable_asm::emit_gc_mark_visit(
        &mut g,
        store_mark,
        portable_asm::MarkWorklist {
            header_mark,
            worklist: work,
            worklist_top: top,
            stw_fallback_pending: fallback,
        },
    );
    portable_asm::emit_gc_store_pointer(&mut g, store_race, phase, racing_mark);
    g.bind_text_label(racing_mark);
    g.mov_data_addr(P::V10, race_count);
    g.load_ptr_disp32(P::V0, P::V10, 0);
    g.add_reg_imm32(P::V0, 1);
    g.store_ptr_disp32(P::V10, 0, P::V0);
    let unchanged = g.create_text_label();
    g.cmp_reg_imm8(P::V0, 2); // new target, then first observed old target
    g.jcc_label(crate::portable_asm::Condition::NotEqual, unchanged);
    g.mov_data_addr(P::V10, race_slot);
    g.load_ptr_disp32(P::V10, P::V10, 0);
    g.mov_data_addr(P::V11, race_value);
    g.load_ptr_disp32(P::V11, P::V11, 0);
    g.atomic_store_release(P::V10, P::V11); // deterministic competing store
    g.bind_text_label(unchanged);
    g.jmp_label(store_mark); // still execute the real mark visitor
    let mut object = g.asm.finish();
    let data_start = object.text.len().next_multiple_of(4096);
    let mut absolute = Vec::new();
    for relocation in &object.relocs {
        let target = match relocation.target {
            crate::RelocTarget::Text(label) => object.text_labels[label.0],
            crate::RelocTarget::Data(label) => data_start + object.data_labels[label.0],
            _ => panic!("unexpected external relocation"),
        };
        match relocation.kind {
            crate::RelocKind::Rel32 => {
                let delta = i32::try_from(target as i64 - (relocation.pos + 4) as i64).unwrap();
                object.text[relocation.pos..relocation.pos + 4]
                    .copy_from_slice(&delta.to_le_bytes());
            }
            crate::RelocKind::Abs64 => {
                absolute.push(relocation.pos.to_string());
                object.text[relocation.pos..relocation.pos + 8]
                    .copy_from_slice(&(target as u64).to_le_bytes());
            }
        }
    }
    object.text.resize(data_start, 0);
    object.text.extend_from_slice(&object.data);
    let bytes = object
        .text
        .iter()
        .map(u8::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let mut header = format!("static const unsigned char code[] = {{{bytes}}};\n");
    header.push_str(&format!(
        "static const unsigned long fixups[] = {{{}}};\n",
        absolute.join(",")
    ));
    for (name, offset) in [
        ("DATA_START", data_start),
        ("SLOW_ENTRY", offsets[0]),
        ("TRACE_ENTRY", offsets[1]),
        ("DRAIN_ENTRY", offsets[2]),
        ("ALLOC_ENTRY", offsets[3]),
        ("RESTART_ENTRY", offsets[4]),
        ("STORE_ENTRY", offsets[5]),
        ("STORE_RACE_ENTRY", offsets[6]),
        ("RACE_COUNT", data_start + object.data_labels[race_count.0]),
        ("RACE_SLOT", data_start + object.data_labels[race_slot.0]),
        ("RACE_VALUE", data_start + object.data_labels[race_value.0]),
        ("QUANTUM", data_start + object.data_labels[quantum.0]),
        ("HEAP_TOP", data_start + object.data_labels[heap_top.0]),
        ("HEAP_END", data_start + object.data_labels[heap_end.0]),
        ("END_CALLS", data_start + object.data_labels[end_calls.0]),
        ("CURSOR", data_start + object.data_labels[cursor.0]),
        ("END", data_start + object.data_labels[end.0]),
        ("PHASE", data_start + object.data_labels[phase.0]),
        ("MARKED", data_start + object.data_labels[marked.0]),
        ("TOP", data_start + object.data_labels[top.0]),
        ("WORK", data_start + object.data_labels[work.0]),
    ] {
        header.push_str(&format!("#define {name} {offset}\n"));
    }
    let stdout = run_c_driver("gc_legacy_heal_code.h", &header, "legacy_heal_harness.c");
    assert!(
        stdout.contains(
            "legacy emitted trace: bounded record/list scans and partial-only drain PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains("legacy emitted heal: 128 stale observations and trace repair PASS"),
        "{stdout}"
    );
}
