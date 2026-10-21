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
    L, Labels, RT_ALLOC_SIZE, RT_ALLOC_TAG, RT_CS_COUNT, RT_CS_FIELD, RT_CS_OFF, RT_CS_PAGE,
    RT_FORWARD_CELL, RT_HEAP_BASE, RT_NEXT_PAGE, RT_PAGES, RT_PENDING_REF, RT_SCAN_OLD,
    RT_SPACE_BASE, d, decode, encode, load_runtime,
};
use crate::gc_abi::{AllocationPage, ObjectHeader};
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

pub(super) fn emit_enums<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
) {
    emit_field_access(a, l, runtime);
    emit_forward(a, l, runtime);
    emit_deep_equal(a, l);
}

fn emit_field_access<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, runtime: E::DataLabel) {
    // Fields hold payload addresses: both funnels are plain moves.
    let _ = runtime;
    a.bind_text_label(l.field_load);
    a.ret();
    a.bind_text_label(l.field_store);
    a.store_ptr_disp32(R::V7, 0, R::V6);
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
    a.load_ptr_disp32(R::V1, R::V0, PAYLOAD);
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_SIZE), R::V1);
    a.call_label(l.alloc_raw);
    a.mov_reg_reg(R::V7, R::V0);
    a.load_ptr_disp32(R::V6, R::V3, d(RT_SCAN_OLD));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_ALLOC_SIZE));
    a.rep_movsb();
    a.load_ptr_disp32(R::V6, R::V3, d(RT_SCAN_OLD));
    a.mov_imm64(R::V1, FORWARDED);
    a.store_ptr_disp32(R::V6, DESCRIPTOR, R::V1);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PENDING_REF));
    a.store_ptr_disp32(R::V6, MARK_RECORD, R::V0);
    a.ret();

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

/// Scan to-space objects in allocation order, forwarding every reference
/// field; copies append behind the cursor until it catches up.
pub(super) fn emit_cheney_scan<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let pages = a.create_text_label();
    let objects = a.create_text_label();
    let fields = a.create_text_label();
    let next_object = a.create_text_label();
    let next_field = a.create_text_label();
    let done = a.create_text_label();
    let page = |field: usize| d(field);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SPACE_BASE));
    a.store_ptr_disp32(R::V3, d(RT_CS_PAGE), R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, page(offset_of!(AllocationPage, start)));
    a.store_ptr_disp32(R::V3, d(RT_CS_OFF), R::V0);
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
    // A field is a reference iff it addresses a heap page's payload.
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, next_field);
    a.cmp_reg_imm32(R::V0, HEAP_PAGE as i32 + HEADER);
    a.jcc_label(C::Below, next_field);
    a.mov_imm64(R::V1, super::HEAP_BYTES);
    a.cmp_reg_reg(R::V0, R::V1);
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
    a.store_ptr_disp32(R::V2, 0, R::V0);
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
    a.bind_text_label(done);
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
fn emit_deep_equal<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let yes = a.create_text_label();
    let no = a.create_text_label();
    let raw = a.create_text_label();
    let raw_loop = a.create_text_label();
    let slots = a.create_text_label();
    let slot_loop = a.create_text_label();
    let slot_next = a.create_text_label();
    let min_done = a.create_text_label();
    a.bind_text_label(l.deep_equal);
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
    a.call_label(l.deep_equal);
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
