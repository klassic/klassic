//! M-G3: heap objects that hold references (enum records and lists).
//!
//! Heap fields hold payload addresses, as on the legacy path: generic enum
//! payloads are written and read through untyped (scalar) funnels, so a
//! field cannot carry reference bits. Evacuation treats a field word as a
//! reference exactly when it addresses the heap (the legacy range rule),
//! and rewrites it with the copy's address. RootCells keep reference bits.
//! This file composes heap-field barriers from `fields.rs`, remembered-set
//! roots, collection-literal cells and structural equality. Copying is in
//! `copy.rs`, the scan in `scan.rs`, marking in `mark.rs`.
use super::{
    L, Labels, RT_FORWARD_CELL, RT_LIT_MARK,
    copy::{DESCRIPTOR, PAYLOAD, forward_target, unless_forwarded},
    d, decode, encode, load_runtime,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

// Preserve the existing helper paths used by mark, scan and remset.
pub(super) use super::fields::{heap_offset, young_offset};

const _: () = assert!(L.pages > 0);
pub(super) fn emit_enums<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    super::fields::emit_field_load(a, l, runtime);
    super::fields::emit_field_store(a, l, runtime);
    super::remset::emit_remset_roots(a, l);
    emit_forward_cell(a, l, runtime);
    emit_deep_equal(a, l, runtime);
}

/// forward_cell_entry: one collection-literal cell, relocated in a pause
/// or (with `lit_mark`) marked as a snapshot root.
fn emit_forward_cell<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    // forward_cell_entry: V0 = native cell holding a payload address.
    // Called (by the backend's `literals` routine) from inside `collect`;
    // preserves V3 for that caller.
    let empty = a.create_text_label();
    a.bind_text_label(l.forward_cell_entry);
    a.push_reg(R::V3);
    load_runtime(a, runtime);
    if let Some(w) = &l.worker {
        // Mark start: the cell is a root of the snapshot, not relocated.
        let relocate = a.create_text_label();
        a.load_ptr_disp32(R::V1, R::V3, d(RT_LIT_MARK));
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::Equal, relocate);
        a.load_ptr_disp32(R::V0, R::V0, 0);
        heap_offset(a, R::V0, R::V2, empty);
        a.call_label(w.mark_ref);
        a.jmp_label(empty);
        a.bind_text_label(relocate);
    }
    a.store_ptr_disp32(R::V3, d(RT_FORWARD_CELL), R::V0);
    a.load_ptr_disp32(R::V0, R::V0, 0);
    encode(a, R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    a.call_label(l.forward);
    decode(a, R::V0, R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_FORWARD_CELL));
    a.store_ptr_disp32(R::V1, 0, R::V0);
    a.bind_text_label(empty);
    a.pop_reg(R::V3);
    a.ret();
}

/// Structural equality over payload addresses (V7, V6 -> V0). Compares the
/// shared 16-byte-rounded prefix, like the legacy routine: raw bytes word
/// by word, records/lists slot by slot (a list's length word directly),
/// recursing through referenced payloads. No allocation, no poll.
fn emit_deep_equal<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    let yes = a.create_text_label();
    let no = a.create_text_label();
    let raw = a.create_text_label();
    let raw_loop = a.create_text_label();
    let slots = a.create_text_label();
    let slot_loop = a.create_text_label();
    let slot_next = a.create_text_label();
    let min_done = a.create_text_label();
    a.bind_text_label(l.deep_equal);
    a.bind_text_label(l.deep_equal_body);
    // During an open relocation phase an operand may be an evacuated
    // source: follow its forwarding word to the copy. Each descriptor is
    // read once per operand (V9, V10): a source may be claimed by the
    // worker at any moment, but its payload never changes.
    for (operand, desc) in [(R::V7, R::V9), (R::V6, R::V10)] {
        let direct = a.create_text_label();
        a.test_reg_reg(operand, operand);
        a.jcc_label(C::Equal, direct);
        a.load_ptr_disp32(desc, operand, DESCRIPTOR);
        unless_forwarded(a, desc, R::V1, direct);
        a.mov_reg_reg(operand, desc);
        forward_target(a, operand, R::V1);
        a.push_reg(R::V3);
        load_runtime(a, runtime);
        decode(a, operand, R::V0);
        a.pop_reg(R::V3);
        a.load_ptr_disp32(desc, operand, DESCRIPTOR);
        a.bind_text_label(direct);
    }
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::Equal, yes);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, no);
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, no);
    a.mov_reg_reg(R::V0, R::V9);
    a.cmp_reg_reg(R::V9, R::V10);
    a.jcc_label(C::NotEqual, no);
    // V2 = qwords in the shared rounded prefix.
    a.load_ptr_disp32(R::V2, R::V7, PAYLOAD);
    a.load_ptr_disp32(R::V1, R::V6, PAYLOAD);
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, min_done);
    a.mov_reg_reg(R::V2, R::V1);
    a.bind_text_label(min_done);
    a.add_reg_imm32(R::V2, 15);
    a.and_reg_imm32(R::V2, -16);
    a.shr_reg_imm8(R::V2, 3);
    a.mov_imm64(R::V8, 0); // index
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Equal, raw);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, slots);
    // List: the length word compares directly.
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, yes);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    a.load_ptr_disp32(R::V1, R::V6, 0);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, no);
    a.mov_imm64(R::V8, 1);
    a.jmp_label(slots);
    a.bind_text_label(raw);
    a.bind_text_label(raw_loop);
    a.cmp_reg_reg(R::V8, R::V2);
    a.jcc_label(C::AboveOrEqual, yes);
    a.mov_reg_reg(R::V9, R::V8);
    a.shl_reg_imm8(R::V9, 3);
    a.mov_reg_reg(R::V10, R::V7);
    a.add_reg_reg(R::V10, R::V9);
    a.load_ptr_disp32(R::V0, R::V10, 0);
    a.mov_reg_reg(R::V10, R::V6);
    a.add_reg_reg(R::V10, R::V9);
    a.load_ptr_disp32(R::V1, R::V10, 0);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, no);
    a.add_reg_imm32(R::V8, 1);
    a.jmp_label(raw_loop);
    a.bind_text_label(slots);
    a.bind_text_label(slot_loop);
    a.cmp_reg_reg(R::V8, R::V2);
    a.jcc_label(C::AboveOrEqual, yes);
    a.mov_reg_reg(R::V9, R::V8);
    a.shl_reg_imm8(R::V9, 3);
    a.mov_reg_reg(R::V10, R::V7);
    a.add_reg_reg(R::V10, R::V9);
    a.load_ptr_disp32(R::V0, R::V10, 0);
    a.mov_reg_reg(R::V10, R::V6);
    a.add_reg_reg(R::V10, R::V9);
    a.load_ptr_disp32(R::V1, R::V10, 0);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, slot_next);
    // Recurse on the referents (payload addresses); keep a, b, index, count.
    a.push_reg(R::V7);
    a.push_reg(R::V6);
    a.push_reg(R::V8);
    a.push_reg(R::V2);
    a.mov_reg_reg(R::V7, R::V0);
    a.mov_reg_reg(R::V6, R::V1);
    a.call_label(l.deep_equal_body);
    a.pop_reg(R::V2);
    a.pop_reg(R::V8);
    a.pop_reg(R::V6);
    a.pop_reg(R::V7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no);
    a.bind_text_label(slot_next);
    a.add_reg_imm32(R::V8, 1);
    a.jmp_label(slot_loop);
    a.bind_text_label(yes);
    a.mov_imm64(R::V0, 1);
    a.ret();
    a.bind_text_label(no);
    a.mov_imm64(R::V0, 0);
    a.ret();
}
