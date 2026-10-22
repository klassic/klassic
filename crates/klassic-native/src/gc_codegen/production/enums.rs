//! M-G3: heap objects that hold references (enum records and lists).
//!
//! Heap fields hold payload addresses, as on the legacy path: generic enum
//! payloads are written and read through untyped (scalar) funnels, so a
//! field cannot carry reference bits. Evacuation treats a field word as a
//! reference exactly when it addresses the heap (the legacy range rule),
//! and rewrites it with the copy's address. RootCells keep reference bits.
//! This file holds the barriers (`field_load`, `field_store`), the
//! remembered-set roots, collection-literal cells and structural equality;
//! copying is in `copy.rs`, the scan in `scan.rs`, marking in `mark.rs`.
use super::{
    HEAP_BYTES, L, Labels, REMSET_ENTRIES, RT_FORWARD_CELL, RT_HEAP_BASE, RT_LIT_MARK,
    RT_MARK_ACTIVE, RT_MARK_DONE, RT_MARK_VALID, RT_REMSET_DROPPED, RT_REMSET_LEN, RT_RS_INDEX,
    RT_RS_WRITE, YOUNG_END,
    copy::{DESCRIPTOR, HEADER, PAYLOAD, forward_target, unless_forwarded},
    d, decode, encode, load_runtime,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

const _: () = assert!(L.pages > 0);
const HEAP_PAGE: u64 = crate::gc_abi::HEAP_PAGE_BYTES;
/// Lowest heap offset of a Young payload (first page + one header).
const YOUNG_LOW: i32 = HEAP_PAGE as i32 + HEADER;

/// Jump to `outside` unless `value - heap_base` (left in `scratch`) is a
/// Young payload offset.
fn young_offset<E: PortableAsm>(a: &mut E, value: R, scratch: R, outside: E::TextLabel) {
    a.mov_reg_reg(scratch, value);
    a.load_ptr_disp32(value, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(scratch, value);
    a.jcc_label(C::Below, outside);
    a.cmp_reg_imm32(scratch, YOUNG_LOW);
    a.jcc_label(C::Below, outside);
    a.cmp_reg_imm32(scratch, YOUNG_END as i32);
    a.jcc_label(C::AboveOrEqual, outside);
}

/// Like `young_offset`, for any heap payload offset (Young or Old).
pub(super) fn heap_offset<E: PortableAsm>(a: &mut E, value: R, scratch: R, outside: E::TextLabel) {
    a.mov_reg_reg(scratch, value);
    a.load_ptr_disp32(value, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(scratch, value);
    a.jcc_label(C::Below, outside);
    a.cmp_reg_imm32(scratch, YOUNG_LOW);
    a.jcc_label(C::Below, outside);
    a.mov_imm64(value, HEAP_BYTES);
    a.cmp_reg_reg(scratch, value);
    a.jcc_label(C::AboveOrEqual, outside);
}

/// Append the field address in `field` to the remembered set (V3 =
/// runtime). With a worker both threads append, so the slot is reserved by
/// a CAS on the length (clobbers V0 then).
pub(super) fn remember<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    field: R,
    scratch: R,
    index: R,
) {
    if l.worker.is_none() {
        a.load_ptr_disp32(index, R::V3, d(RT_REMSET_LEN));
        a.cmp_reg_imm32(index, REMSET_ENTRIES as i32);
        a.jcc_label(C::AboveOrEqual, l.old_full);
        a.mov_reg_reg(scratch, index);
        a.shl_reg_imm8(scratch, 3);
        a.add_reg_reg(scratch, R::V3);
        a.store_ptr_disp32(scratch, L.remset as i32, field);
        a.add_reg_imm32(index, 1);
        a.store_ptr_disp32(R::V3, d(RT_REMSET_LEN), index);
        return;
    }
    let retry = a.create_text_label();
    a.mov_reg_reg(scratch, R::V3);
    a.add_reg_imm32(scratch, d(RT_REMSET_LEN));
    a.bind_text_label(retry);
    a.load_ptr_disp32(R::V0, scratch, 0);
    a.cmp_reg_imm32(R::V0, REMSET_ENTRIES as i32);
    a.jcc_label(C::AboveOrEqual, l.old_full);
    a.mov_reg_reg(index, R::V0);
    a.add_reg_imm32(index, 1);
    a.atomic_compare_exchange(scratch, index);
    a.sub_reg_imm8(index, 1);
    a.cmp_reg_reg(R::V0, index);
    a.jcc_label(C::NotEqual, retry);
    a.shl_reg_imm8(index, 3);
    a.add_reg_reg(index, R::V3);
    a.store_ptr_disp32(index, L.remset as i32, field);
}

pub(super) fn emit_enums<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    emit_field_load(a, l, runtime);
    emit_field_store(a, l, runtime);
    emit_remset_roots(a, l);
    emit_forward_cell(a, l, runtime);
    emit_deep_equal(a, l, runtime);
}

/// field_load: the load barrier. V0 = loaded word, V10 = field address.
/// While a relocation phase is open, a word that addresses from-space is
/// relocated (copied once, or the winner's copy taken) and the field
/// healed by a CAS against the stale word; preserves the rest.
fn emit_field_load<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    let pass = a.create_text_label();
    let hit = a.create_text_label();
    a.bind_text_label(l.field_load);
    a.push_reg(R::V3);
    a.push_reg(R::V1);
    a.push_reg(R::V2);
    load_runtime(a, runtime);
    a.load_ptr_disp32(R::V1, R::V3, d(super::RT_PHASE_OPEN));
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, pass);
    a.mov_reg_reg(R::V1, R::V0);
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, pass);
    // V1 = heap offset; keep the loaded word on the stack while testing.
    a.push_reg(R::V0);
    for (lo, hi) in [
        (super::RT_RY_LO, super::RT_RY_HI),
        (super::RT_RO_LO, super::RT_RO_HI),
    ] {
        a.mov_reg_reg(R::V2, R::V1);
        a.load_ptr_disp32(R::V0, R::V3, d(lo));
        a.sub_reg_reg(R::V2, R::V0);
        a.load_ptr_disp32(R::V0, R::V3, d(hi));
        a.push_reg(R::V1);
        a.load_ptr_disp32(R::V1, R::V3, d(lo));
        a.sub_reg_reg(R::V0, R::V1);
        a.pop_reg(R::V1);
        a.cmp_reg_reg(R::V2, R::V0);
        a.jcc_label(C::Below, hit);
    }
    a.pop_reg(R::V0);
    a.jmp_label(pass);
    // The stale word stays on the stack for the healing CAS.
    a.bind_text_label(hit);
    for reg in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(reg);
    }
    a.mov_imm64(R::V2, 1);
    a.store_ptr_disp32(R::V3, d(super::RT_COLLECTING), R::V2);
    a.shl_reg_imm8(R::V1, 16);
    a.mov_imm64(R::V2, super::color());
    a.or_reg_reg(R::V1, R::V2);
    a.mov_reg_reg(R::V0, R::V1);
    a.call_label(l.forward);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V1);
    a.mov_imm64(R::V1, 0);
    a.store_ptr_disp32(R::V3, d(super::RT_COLLECTING), R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(super::RT_HEALED));
    a.add_reg_imm32(R::V1, 1);
    a.store_ptr_disp32(R::V3, d(super::RT_HEALED), R::V1);
    for reg in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(reg);
    }
    a.mov_reg_reg(R::V1, R::V0);
    a.pop_reg(R::V0);
    if l.worker.is_some() {
        // Only the worker's own heal of the same field can intervene; it
        // stores the same copy, so a failed CAS changes nothing.
        a.atomic_compare_exchange(R::V10, R::V1);
    } else {
        a.store_ptr_disp32(R::V10, 0, R::V1);
    }
    a.mov_reg_reg(R::V0, R::V1);
    a.bind_text_label(pass);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.pop_reg(R::V3);
    a.ret();
}

/// field_store: the SATB pre-barrier (while marking, the overwritten
/// reference is marked), the store, then the generational write barrier:
/// an Old field that now holds a Young payload address joins the remset.
fn emit_field_store<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    let done = a.create_text_label();
    a.bind_text_label(l.field_store);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    a.push_reg(R::V2);
    a.push_reg(R::V3);
    load_runtime(a, runtime);
    if let Some(w) = &l.worker {
        let satb_done = a.create_text_label();
        a.load_ptr_disp32(R::V0, R::V3, d(RT_MARK_ACTIVE));
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, satb_done);
        a.load_ptr_disp32(R::V0, R::V7, 0);
        heap_offset(a, R::V0, R::V2, satb_done);
        for reg in [R::V6, R::V7, R::V8, R::V9] {
            a.push_reg(reg);
        }
        a.call_label(w.lock);
        a.call_label(w.mark_ref);
        a.mov_imm64(R::V0, 0);
        a.store_ptr_disp32(R::V3, d(RT_MARK_DONE), R::V0);
        super::worker::request_work(a);
        a.call_label(w.unlock);
        for reg in [R::V9, R::V8, R::V7, R::V6] {
            a.pop_reg(reg);
        }
        a.bind_text_label(satb_done);
    }
    a.store_ptr_disp32(R::V7, 0, R::V6);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.mov_reg_reg(R::V0, R::V7);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.cmp_reg_imm32(R::V0, YOUNG_END as i32);
    a.jcc_label(C::Below, done);
    a.mov_imm64(R::V1, HEAP_BYTES);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    young_offset(a, R::V6, R::V0, done);
    remember(a, l, R::V7, R::V1, R::V2);
    a.bind_text_label(done);
    a.load_ptr_disp32(R::V6, R::V7, 0);
    a.pop_reg(R::V3);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.ret();
}

/// Remembered Old fields are roots of a Young collection: forward each
/// referent and keep the entry only while it still points into Young.
fn emit_remset_roots<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let next = a.create_text_label();
    let keep = a.create_text_label();
    let done = a.create_text_label();
    let lp = a.create_text_label();
    a.bind_text_label(l.remset_roots);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_RS_INDEX), R::V0);
    a.store_ptr_disp32(R::V3, d(RT_RS_WRITE), R::V0);
    a.bind_text_label(lp);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_RS_INDEX));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_REMSET_LEN));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.load_ptr_disp32(R::V7, R::V0, L.remset as i32);
    a.store_ptr_disp32(R::V3, d(RT_FORWARD_CELL), R::V7);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    young_offset(a, R::V0, R::V1, next);
    if l.worker.is_some() {
        // After a complete mark, an unmarked referent is garbage and so is
        // every Old holder of it: drop the entry instead of copying it.
        let live = a.create_text_label();
        a.load_ptr_disp32(R::V2, R::V3, d(RT_MARK_VALID));
        a.test_reg_reg(R::V2, R::V2);
        a.jcc_label(C::Equal, live);
        a.mov_reg_reg(R::V2, R::V1);
        super::mark::test_mark(a, R::V2, R::V6, R::V8);
        a.mov_reg_reg(R::V1, R::V2);
        a.jcc_label(C::NotEqual, live);
        super::copy::bump(a, RT_REMSET_DROPPED, R::V2);
        a.jmp_label(next);
        a.bind_text_label(live);
    }
    // V1 = Young offset -> reference bits.
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    a.mov_imm64(R::V1, super::color());
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(l.forward);
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V2);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_FORWARD_CELL));
    a.store_ptr_disp32(R::V7, 0, R::V0);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_imm32(R::V1, YOUNG_END as i32);
    a.jcc_label(C::Below, keep);
    a.jmp_label(next);
    a.bind_text_label(keep);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_RS_WRITE));
    a.mov_reg_reg(R::V1, R::V0);
    a.shl_reg_imm8(R::V1, 3);
    a.add_reg_reg(R::V1, R::V3);
    a.store_ptr_disp32(R::V1, L.remset as i32, R::V7);
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_RS_WRITE), R::V0);
    a.bind_text_label(next);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_RS_INDEX));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_RS_INDEX), R::V0);
    a.jmp_label(lp);
    a.bind_text_label(done);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_RS_WRITE));
    a.store_ptr_disp32(R::V3, d(RT_REMSET_LEN), R::V0);
    a.ret();
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
