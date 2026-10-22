//! M-G3: heap objects that hold references (enum records and lists).
//!
//! Heap fields hold payload addresses, as on the legacy path: generic enum
//! payloads are written and read through untyped (scalar) funnels, so a
//! field cannot carry reference bits. Evacuation treats a field word as a
//! reference exactly when it addresses the heap (the legacy range rule),
//! and rewrites it with the copy's address. RootCells keep reference bits. Evacuation forwards
//! each object once and then scans to-space Cheney-style, so every
//! reference reachable from roots and collection-literal cells moves.
use super::{
    HEAP_BYTES, L, Labels, REMSET_ENTRIES, RT_ALLOC_SIZE, RT_ALLOC_TAG, RT_CS_COUNT, RT_CS_FIELD,
    RT_CS_OFF, RT_CS_PAGE, RT_FORWARD_CELL, RT_HEAP_BASE, RT_NEXT_PAGE, RT_OLD_SCAN, RT_OLD_TOP,
    RT_PAGES, RT_PENDING_REF, RT_PROMOTED, RT_REMSET_LEN, RT_RS_INDEX, RT_RS_WRITE, RT_SCAN_NEW,
    RT_SCAN_OLD, RT_SPACE_BASE, YOUNG_END, d, decode, encode, load_runtime,
};
use crate::gc_abi::{AllocationPage, MarkRecord, OBJECT_PUBLISHED, ObjectHeader};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

/// Descriptor id written over an evacuated source header; its
/// `mark_record` word then holds the copy's reference. Never a real id
/// (builtin ids are 1, 2 and 4) and only ever seen in from-space.
pub(super) const FORWARDED: u64 = 0xf0f0_f0f0;
const HEADER: i32 = size_of::<ObjectHeader>() as i32;
const DESCRIPTOR: i32 = offset_of!(ObjectHeader, descriptor_id) as i32 - HEADER;
const PAYLOAD: i32 = offset_of!(ObjectHeader, payload_bytes) as i32 - HEADER;
const MARK_RECORD: i32 = offset_of!(ObjectHeader, mark_record) as i32 - HEADER;
const _: () = assert!(L.pages > 0);
const HEAP_PAGE: u64 = crate::gc_abi::HEAP_PAGE_BYTES;
const AGE: i32 = offset_of!(MarkRecord, age) as i32;
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
fn heap_offset<E: PortableAsm>(a: &mut E, value: R, scratch: R, outside: E::TextLabel) {
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

/// Append the field address in `field` to the remembered set (V3 = runtime).
fn remember<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, field: R, scratch: R, index: R) {
    a.load_ptr_disp32(index, R::V3, d(RT_REMSET_LEN));
    a.cmp_reg_imm32(index, REMSET_ENTRIES as i32);
    a.jcc_label(C::AboveOrEqual, l.old_full);
    a.mov_reg_reg(scratch, index);
    a.shl_reg_imm8(scratch, 3);
    a.add_reg_reg(scratch, R::V3);
    a.store_ptr_disp32(scratch, L.remset as i32, field);
    a.add_reg_imm32(index, 1);
    a.store_ptr_disp32(R::V3, d(RT_REMSET_LEN), index);
}

pub(super) fn emit_enums<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    emit_field_access(a, l, runtime);
    emit_forward(a, l, runtime);
    emit_deep_equal(a, l, runtime);
    emit_cheney(a, l);
}

fn emit_field_access<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    // Fields hold payload addresses: both funnels are plain moves.
    let _ = runtime;
    // field_load: the load barrier. V0 = loaded word, V10 = field address.
    // While a relocation phase is open, a word that addresses from-space is
    // relocated (copied once) and the field healed; preserves the rest.
    let pass = a.create_text_label();
    let hit = a.create_text_label();
    let restore = a.create_text_label();
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
    let restore_word = a.create_text_label();
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
    a.bind_text_label(restore_word);
    a.pop_reg(R::V0);
    a.jmp_label(pass);
    a.bind_text_label(hit);
    a.pop_reg(R::V0); // the stale word; replaced below
    // Relocate: forward clobbers the caller-saved registers.
    for reg in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(reg);
    }
    super::worker::lock_if(a, l, false);
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
    a.store_ptr_disp32(R::V10, 0, R::V0);
    super::worker::unlock_if(a, l, false);
    a.jmp_label(restore);
    a.bind_text_label(pass);
    a.bind_text_label(restore);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.pop_reg(R::V3);
    a.ret();
    // field_store: the store, then the generational write barrier: an Old
    // field that now holds a Young payload address joins the remset.
    let done = a.create_text_label();
    a.bind_text_label(l.field_store);
    a.store_ptr_disp32(R::V7, 0, R::V6);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    a.push_reg(R::V2);
    a.push_reg(R::V3);
    load_runtime(a, runtime);
    super::worker::lock_if(a, l, true);
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
    // young_offset left heap_base in V6; restore the stored value.
    a.load_ptr_disp32(R::V6, R::V7, 0);
    remember(a, l, R::V7, R::V1, R::V2);
    a.bind_text_label(done);
    super::worker::unlock_if(a, l, true);
    a.load_ptr_disp32(R::V6, R::V7, 0);
    a.pop_reg(R::V3);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.ret();
}

fn emit_forward<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    // forward: V3 = runtime, V0 = non-null bits -> V0 = to-space bits.
    let copy = a.create_text_label();
    a.bind_text_label(l.forward);
    // Only words that are references move: the phase color in the low 16
    // bits and an offset inside the heap. External tagged pointers and
    // scalars stored in reference slots (like the legacy range check)
    // stay as they are.
    let not_ref = a.create_text_label();
    let heap = a.create_text_label();
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, 0xffff);
    a.cmp_reg_imm32(R::V1, super::color() as i32);
    a.jcc_label(C::NotEqual, not_ref);
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_imm32(R::V1, HEAP_PAGE as i32);
    a.jcc_label(C::Below, not_ref);
    a.mov_imm64(R::V2, super::HEAP_BYTES);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, heap);
    a.bind_text_label(not_ref);
    a.ret();
    a.bind_text_label(heap);
    // Only objects in this collection's from-space ranges move; everything
    // else (to-space, Old outside a full collection) stays.
    let young = a.create_text_label();
    for (lo, hi) in [
        (super::RT_RY_LO, super::RT_RY_HI),
        (super::RT_RO_LO, super::RT_RO_HI),
    ] {
        a.mov_reg_reg(R::V2, R::V1);
        a.push_reg(R::V1);
        a.load_ptr_disp32(R::V1, R::V3, d(lo));
        a.sub_reg_reg(R::V2, R::V1);
        a.load_ptr_disp32(R::V1, R::V3, d(hi));
        a.push_reg(R::V0);
        a.load_ptr_disp32(R::V0, R::V3, d(lo));
        a.sub_reg_reg(R::V1, R::V0);
        a.pop_reg(R::V0);
        a.cmp_reg_reg(R::V2, R::V1);
        a.pop_reg(R::V1);
        a.jcc_label(C::Below, young);
    }
    a.jmp_label(not_ref);
    a.bind_text_label(young);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(R::V1, R::V0, DESCRIPTOR);
    a.mov_imm64(R::V2, FORWARDED);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::NotEqual, copy);
    a.load_ptr_disp32(R::V0, R::V0, MARK_RECORD);
    a.ret();
    a.bind_text_label(copy);
    a.store_ptr_disp32(R::V3, d(RT_SCAN_OLD), R::V0);
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_TAG), R::V1);
    // A Young object that already survived one collection is promoted.
    let promote = a.create_text_label();
    a.load_ptr_disp32(R::V2, R::V3, d(super::RT_FULL));
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, promote);
    a.load_ptr_disp32(R::V2, R::V0, MARK_RECORD);
    a.load_ptr_disp32(R::V2, R::V2, AGE);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, promote);
    a.load_ptr_disp32(R::V1, R::V0, PAYLOAD);
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_SIZE), R::V1);
    a.call_label(l.alloc_raw);
    a.store_ptr_disp32(R::V3, d(RT_SCAN_NEW), R::V0);
    a.mov_reg_reg(R::V7, R::V0);
    a.load_ptr_disp32(R::V6, R::V3, d(RT_SCAN_OLD));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_ALLOC_SIZE));
    a.rep_movsb();
    a.load_ptr_disp32(R::V6, R::V3, d(RT_SCAN_OLD));
    a.mov_imm64(R::V1, FORWARDED);
    a.store_ptr_disp32(R::V6, DESCRIPTOR, R::V1);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PENDING_REF));
    a.store_ptr_disp32(R::V6, MARK_RECORD, R::V0);
    // The copy has survived once.
    a.load_ptr_disp32(R::V1, R::V3, d(RT_SCAN_NEW));
    a.load_ptr_disp32(R::V1, R::V1, MARK_RECORD);
    a.mov_imm64(R::V2, 1);
    a.store_ptr_disp32(R::V1, AGE, R::V2);
    a.ret();

    // Promotion: bump-allocate in Old with a published header and copy.
    let sized = a.create_text_label();
    a.bind_text_label(promote);
    a.load_ptr_disp32(R::V1, R::V0, PAYLOAD);
    a.mov_reg_reg(R::V2, R::V1);
    a.add_reg_imm32(R::V2, 15);
    a.and_reg_imm32(R::V2, -16);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V2, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V2, HEADER);
    a.load_ptr_disp32(R::V6, R::V3, d(RT_OLD_TOP));
    a.mov_reg_reg(R::V7, R::V6);
    a.add_reg_reg(R::V7, R::V2);
    a.load_ptr_disp32(R::V8, R::V3, d(super::RT_OLD_BASE));
    a.add_reg_imm32(R::V8, super::OLD_HALF as i32);
    a.cmp_reg_reg(R::V7, R::V8);
    a.jcc_label(C::Above, l.old_full);
    a.store_ptr_disp32(R::V3, d(RT_OLD_TOP), R::V7);
    a.load_ptr_disp32(R::V8, R::V3, d(RT_PROMOTED));
    a.add_reg_reg(R::V8, R::V2);
    a.store_ptr_disp32(R::V3, d(RT_PROMOTED), R::V8);
    // V8 = header address, V6 = header offset.
    a.load_ptr_disp32(R::V8, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V8, R::V6);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, payload_bytes)), R::V1);
    a.load_ptr_disp32(R::V2, R::V0, DESCRIPTOR);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, descriptor_id)), R::V2);
    a.mov_imm64(R::V2, OBJECT_PUBLISHED);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, state)), R::V2);
    // New reference bits for the promoted copy.
    a.add_reg_imm32(R::V6, HEADER);
    a.shl_reg_imm8(R::V6, 16);
    a.mov_imm64(R::V2, super::color());
    a.or_reg_reg(R::V6, R::V2);
    a.store_ptr_disp32(R::V3, d(RT_SCAN_NEW), R::V6);
    a.mov_reg_reg(R::V7, R::V8);
    a.add_reg_imm32(R::V7, HEADER);
    a.mov_reg_reg(R::V6, R::V0);
    a.rep_movsb();
    a.load_ptr_disp32(R::V6, R::V3, d(RT_SCAN_OLD));
    a.mov_imm64(R::V1, FORWARDED);
    a.store_ptr_disp32(R::V6, DESCRIPTOR, R::V1);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SCAN_NEW));
    a.store_ptr_disp32(R::V6, MARK_RECORD, R::V0);
    a.ret();

    emit_remset_roots(a, l);

    // forward_cell_entry: V0 = native cell holding a payload address.
    // Called (by the backend's `literals` routine) from inside `collect`;
    // preserves V3 for that caller.
    let empty = a.create_text_label();
    a.bind_text_label(l.forward_cell_entry);
    a.push_reg(R::V3);
    load_runtime(a, runtime);
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

/// Scan to-space objects in allocation order, forwarding every reference
/// field; copies append behind the cursor until it catches up.
pub(super) fn emit_cheney_init<E: PortableAsm>(a: &mut E) {
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SPACE_BASE));
    a.store_ptr_disp32(R::V3, d(RT_CS_PAGE), R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, start)));
    a.store_ptr_disp32(R::V3, d(RT_CS_OFF), R::V0);
}

/// Resumable scan (`cheney`, V3 = runtime): processes objects until both
/// cursors catch up (V0 = 1) or the object budget runs out (V0 = 0).
fn emit_cheney<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let pages = a.create_text_label();
    let objects = a.create_text_label();
    let fields = a.create_text_label();
    let next_object = a.create_text_label();
    let next_field = a.create_text_label();
    let done = a.create_text_label();
    let paused = a.create_text_label();
    let page = |field: usize| d(field);
    a.bind_text_label(l.cheney);
    a.bind_text_label(pages);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_CS_PAGE));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_NEXT_PAGE));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    a.bind_text_label(objects);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_CS_PAGE));
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_CS_OFF));
    a.load_ptr_disp32(
        R::V2,
        R::V8,
        page(offset_of!(AllocationPage, allocated_top)),
    );
    a.cmp_reg_reg(R::V1, R::V2);
    let in_page = a.create_text_label();
    a.jcc_label(C::Below, in_page);
    // Page scanned: advance to the next bound page, if any.
    a.load_ptr_disp32(R::V0, R::V3, d(RT_CS_PAGE));
    a.add_reg_imm32(R::V0, 1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_NEXT_PAGE));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    a.store_ptr_disp32(R::V3, d(RT_CS_PAGE), R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, page(offset_of!(AllocationPage, start)));
    a.store_ptr_disp32(R::V3, d(RT_CS_OFF), R::V0);
    a.jmp_label(pages);
    a.bind_text_label(in_page);
    spend(a, paused);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_CS_OFF));
    // V1 = header offset. Reference-bearing descriptors: 2 and 4 (skip
    // the list's length word).
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V2, R::V1);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(ObjectHeader, descriptor_id)));
    a.load_ptr_disp32(R::V6, R::V2, d(offset_of!(ObjectHeader, payload_bytes)));
    a.shr_reg_imm8(R::V6, 3);
    a.store_ptr_disp32(R::V3, d(RT_CS_COUNT), R::V6);
    a.mov_imm64(R::V7, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, fields);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, next_object);
    a.mov_imm64(R::V7, 1);
    a.bind_text_label(fields);
    a.store_ptr_disp32(R::V3, d(RT_CS_FIELD), R::V7);
    let field_loop = a.create_text_label();
    a.bind_text_label(field_loop);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    a.load_ptr_disp32(R::V6, R::V3, d(RT_CS_COUNT));
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::AboveOrEqual, next_object);
    field_address(a, R::V2, R::V7);
    a.load_ptr_disp32(R::V0, R::V2, 0);
    super::worker::remember_read(a, l);
    // A field is a reference iff it addresses a heap page's payload.
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, next_field);
    a.cmp_reg_imm32(R::V0, YOUNG_LOW);
    a.jcc_label(C::Below, next_field);
    a.cmp_reg_imm32(R::V0, YOUNG_END as i32);
    a.jcc_label(C::AboveOrEqual, next_field);
    a.shl_reg_imm8(R::V0, 16);
    a.mov_imm64(R::V1, super::color());
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(l.forward);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    field_address(a, R::V2, R::V7);
    super::worker::heal(a, l, next_field);
    a.bind_text_label(next_field);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    a.add_reg_imm32(R::V7, 1);
    a.store_ptr_disp32(R::V3, d(RT_CS_FIELD), R::V7);
    a.jmp_label(field_loop);
    // Advance past header + max(16, round_up(payload, 16)).
    a.bind_text_label(next_object);
    let sized = a.create_text_label();
    a.load_ptr_disp32(R::V1, R::V3, d(RT_CS_OFF));
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V2, R::V1);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(ObjectHeader, payload_bytes)));
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V0, HEADER);
    a.add_reg_reg(R::V1, R::V0);
    a.store_ptr_disp32(R::V3, d(RT_CS_OFF), R::V1);
    a.jmp_label(objects);
    // Young caught up: scan objects promoted during this collection, which
    // may copy more Young objects (then resume the Young scan).
    let finished = a.create_text_label();
    let old_object = a.create_text_label();
    let old_fields = a.create_text_label();
    let old_field = a.create_text_label();
    let old_next_field = a.create_text_label();
    let old_next_object = a.create_text_label();
    a.bind_text_label(done);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_OLD_SCAN));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_OLD_TOP));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, finished);
    a.bind_text_label(old_object);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_OLD_SCAN));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_OLD_TOP));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, pages);
    spend(a, paused);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_OLD_SCAN));
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V2, R::V0);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(ObjectHeader, descriptor_id)));
    a.load_ptr_disp32(R::V6, R::V2, d(offset_of!(ObjectHeader, payload_bytes)));
    a.shr_reg_imm8(R::V6, 3);
    a.store_ptr_disp32(R::V3, d(RT_CS_COUNT), R::V6);
    a.mov_imm64(R::V7, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, old_fields);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, old_next_object);
    a.mov_imm64(R::V7, 1);
    a.bind_text_label(old_fields);
    a.store_ptr_disp32(R::V3, d(RT_CS_FIELD), R::V7);
    a.bind_text_label(old_field);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    a.load_ptr_disp32(R::V6, R::V3, d(RT_CS_COUNT));
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::AboveOrEqual, old_next_object);
    old_field_address(a, R::V2, R::V7);
    a.load_ptr_disp32(R::V0, R::V2, 0);
    super::worker::remember_read(a, l);
    // Any heap payload address (Young, or old-half Old in a full
    // collection) is forwarded; forward leaves non-moving ones alone.
    heap_offset(a, R::V0, R::V1, old_next_field);
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    a.mov_imm64(R::V1, super::color());
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(l.forward);
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V2);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    old_field_address(a, R::V2, R::V7);
    super::worker::heal(a, l, old_next_field);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_imm32(R::V1, YOUNG_END as i32);
    a.jcc_label(C::AboveOrEqual, old_next_field);
    remember(a, l, R::V2, R::V1, R::V6);
    a.bind_text_label(old_next_field);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    a.add_reg_imm32(R::V7, 1);
    a.store_ptr_disp32(R::V3, d(RT_CS_FIELD), R::V7);
    a.jmp_label(old_field);
    a.bind_text_label(old_next_object);
    let old_sized = a.create_text_label();
    a.load_ptr_disp32(R::V1, R::V3, d(RT_OLD_SCAN));
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V2, R::V1);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(ObjectHeader, payload_bytes)));
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, old_sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(old_sized);
    a.add_reg_imm32(R::V0, HEADER);
    a.add_reg_reg(R::V1, R::V0);
    a.store_ptr_disp32(R::V3, d(RT_OLD_SCAN), R::V1);
    a.jmp_label(old_object);
    a.bind_text_label(finished);
    a.mov_imm64(R::V0, 1);
    a.ret();
    a.bind_text_label(paused);
    a.mov_imm64(R::V0, 0);
    a.ret();
}

/// Take one object from the scan budget, or jump to `paused`.
fn spend<E: PortableAsm>(a: &mut E, paused: E::TextLabel) {
    a.load_ptr_disp32(R::V9, R::V3, d(super::RT_BUDGET));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, paused);
    a.sub_reg_imm8(R::V9, 1);
    a.store_ptr_disp32(R::V3, d(super::RT_BUDGET), R::V9);
}

/// `dst = heap_base + old_scan + header + index * 8`.
fn old_field_address<E: PortableAsm>(a: &mut E, dst: R, index: R) {
    a.load_ptr_disp32(dst, R::V3, d(RT_HEAP_BASE));
    a.shl_reg_imm8(index, 3);
    a.add_reg_reg(dst, index);
    a.load_ptr_disp32(index, R::V3, d(RT_OLD_SCAN));
    a.add_reg_reg(dst, index);
    a.add_reg_imm32(dst, HEADER);
}

/// `dst = heap_base + cs_off + header + index * 8`.
fn field_address<E: PortableAsm>(a: &mut E, dst: R, index: R) {
    a.load_ptr_disp32(dst, R::V3, d(RT_HEAP_BASE));
    a.shl_reg_imm8(index, 3);
    a.add_reg_reg(dst, index);
    a.load_ptr_disp32(index, R::V3, d(RT_CS_OFF));
    a.add_reg_reg(dst, index);
    a.add_reg_imm32(dst, HEADER);
}

fn page_ptr<E: PortableAsm>(a: &mut E, dst: R, index: R, scratch: R) {
    a.mov_reg_reg(dst, index);
    a.shl_reg_imm8(dst, 7);
    a.mov_reg_reg(scratch, index);
    a.shl_reg_imm8(scratch, 4);
    a.sub_reg_reg(dst, scratch);
    a.load_ptr_disp32(scratch, R::V3, d(RT_PAGES));
    a.add_reg_reg(dst, scratch);
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
    if l.worker_entry().is_some() {
        // Lock while a phase is open: the worker rewrites from-space
        // headers into forwarding records.
        a.push_reg(R::V3);
        load_runtime(a, runtime);
        super::worker::lock_if(a, l, true);
        a.call_label(l.deep_equal_body);
        super::worker::unlock_if(a, l, true);
        a.pop_reg(R::V3);
        a.ret();
    }
    a.bind_text_label(l.deep_equal_body);
    // During an open relocation phase an operand may be an evacuated
    // source: follow its forwarding record to the copy.
    for operand in [R::V7, R::V6] {
        let direct = a.create_text_label();
        a.test_reg_reg(operand, operand);
        a.jcc_label(C::Equal, direct);
        a.load_ptr_disp32(R::V0, operand, DESCRIPTOR);
        a.mov_imm64(R::V1, FORWARDED);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, direct);
        a.load_ptr_disp32(operand, operand, MARK_RECORD);
        a.push_reg(R::V3);
        load_runtime(a, runtime);
        decode(a, operand, R::V0);
        a.pop_reg(R::V3);
        a.bind_text_label(direct);
    }
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::Equal, yes);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, no);
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, no);
    a.load_ptr_disp32(R::V0, R::V7, DESCRIPTOR);
    a.load_ptr_disp32(R::V1, R::V6, DESCRIPTOR);
    a.cmp_reg_reg(R::V0, R::V1);
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
