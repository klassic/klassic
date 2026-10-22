//! The resumable Cheney scan of an open relocation phase.
//!
//! Three regions hold objects that may still contain from-space words:
//! chain A, the to-space pages the mutator's TLAB binds bottom-up (pause
//! copies, load-barrier copies and fresh objects); chain B, the worker's
//! private copy pages taken top-down; and the Old objects promoted during
//! this collection. A pass scans all three; the scan is complete after a
//! pass that scanned nothing. With a worker the mutator keeps adding to
//! chain A and the promoted range while the worker scans: every header is
//! written before the page's `allocated_top` (TLAB) or the descriptor is
//! written last (promotion; the worker waits for a reserved slot whose
//! descriptor is still zero).
use super::{
    Labels, RT_BUDGET, RT_CB_OFF, RT_CB_PAGE, RT_COPY_LOW, RT_CS_COUNT, RT_CS_FIELD, RT_CS_OFF,
    RT_CS_PAGE, RT_HEAP_BASE, RT_M_COPYING, RT_NEXT_PAGE, RT_OLD_SCAN, RT_OLD_TOP, RT_PAGES,
    RT_PROGRESS, RT_SPACE_BASE, RT_SPACE_END, YOUNG_END, color,
    copy::{DESCRIPTOR, HEADER},
    d,
};
use crate::gc_abi::{AllocationPage, ObjectHeader};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

/// Lowest heap offset of a Young payload (first page + one header).
const YOUNG_LOW: i32 = crate::gc_abi::HEAP_PAGE_BYTES as i32 + HEADER;

/// `dst = pages + index * 112`.
pub(super) fn page_ptr<E: PortableAsm>(a: &mut E, dst: R, index: R, scratch: R) {
    a.mov_reg_reg(dst, index);
    a.shl_reg_imm8(dst, 7);
    a.mov_reg_reg(scratch, index);
    a.shl_reg_imm8(scratch, 4);
    a.sub_reg_reg(dst, scratch);
    a.load_ptr_disp32(scratch, R::V3, d(RT_PAGES));
    a.add_reg_reg(dst, scratch);
}

/// Take one object from the scan budget, or jump to `paused`. Records that
/// this pass made progress. Clobbers V9.
pub(super) fn spend<E: PortableAsm>(a: &mut E, paused: E::TextLabel) {
    a.load_ptr_disp32(R::V9, R::V3, d(RT_BUDGET));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, paused);
    a.sub_reg_imm8(R::V9, 1);
    a.store_ptr_disp32(R::V3, d(RT_BUDGET), R::V9);
    a.mov_imm64(R::V9, 1);
    a.store_ptr_disp32(R::V3, d(RT_PROGRESS), R::V9);
}

/// Point the chain-A cursor at the first to-space page and the chain-B
/// cursor at the last one.
pub(super) fn emit_cheney_init<E: PortableAsm>(a: &mut E) {
    let start = d(offset_of!(AllocationPage, start));
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SPACE_BASE));
    a.store_ptr_disp32(R::V3, d(RT_CS_PAGE), R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, start);
    a.store_ptr_disp32(R::V3, d(RT_CS_OFF), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SPACE_END));
    a.sub_reg_imm8(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_CB_PAGE), R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, start);
    a.store_ptr_disp32(R::V3, d(RT_CB_OFF), R::V0);
}

pub(super) fn emit_cheneys<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_cheney(a, l, l.cheney, l.forward, None);
    if let Some(w) = &l.worker {
        emit_cheney(a, l, w.wcheney, w.wforward, Some(w.hooks.relax));
    }
}

/// `cheney` (V3 = runtime): V0 = 1 when the scan is complete, 0 when the
/// object budget ran out. `relax` is set for the worker's variant, which
/// may meet a promotion still being written by the mutator.
fn emit_cheney<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    entry: E::TextLabel,
    fwd: E::TextLabel,
    relax: Option<E::TextLabel>,
) {
    let start = d(offset_of!(AllocationPage, start));
    let top = d(offset_of!(AllocationPage, allocated_top));
    let pass = a.create_text_label();
    let paused = a.create_text_label();
    a.bind_text_label(entry);
    a.bind_text_label(pass);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_PROGRESS), R::V0);

    // Chain A: pages [cs_page, next_page) bottom-up.
    let a_page = a.create_text_label();
    let a_in = a.create_text_label();
    let a_done = a.create_text_label();
    a.bind_text_label(a_page);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_CS_PAGE));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_NEXT_PAGE));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, a_done);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_CS_OFF));
    a.load_ptr_disp32(R::V2, R::V8, top);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, a_in);
    // Page scanned: a later bound page means this one is closed.
    a.load_ptr_disp32(R::V0, R::V3, d(RT_CS_PAGE));
    a.add_reg_imm32(R::V0, 1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_NEXT_PAGE));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, a_done);
    a.store_ptr_disp32(R::V3, d(RT_CS_PAGE), R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, start);
    a.store_ptr_disp32(R::V3, d(RT_CS_OFF), R::V0);
    a.jmp_label(a_page);
    a.bind_text_label(a_in);
    if relax.is_some() {
        copy_in_flight(a, a_done);
    }
    spend(a, paused);
    scan_young_object(a, l, fwd, RT_CS_OFF, a_page);
    a.bind_text_label(a_done);

    // Chain B: the worker's pages [copy_low, space_end) top-down.
    let b_page = a.create_text_label();
    let b_in = a.create_text_label();
    let b_done = a.create_text_label();
    a.bind_text_label(b_page);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_CB_PAGE));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_COPY_LOW));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, b_done);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_CB_OFF));
    a.load_ptr_disp32(R::V2, R::V8, top);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, b_in);
    // Only the worker fills chain B, one page at a time: a lower reserved
    // page means this one is closed.
    a.load_ptr_disp32(R::V0, R::V3, d(RT_CB_PAGE));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_COPY_LOW));
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::AboveOrEqual, b_done);
    a.sub_reg_imm8(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_CB_PAGE), R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, start);
    a.store_ptr_disp32(R::V3, d(RT_CB_OFF), R::V0);
    a.jmp_label(b_page);
    a.bind_text_label(b_in);
    spend(a, paused);
    scan_young_object(a, l, fwd, RT_CB_OFF, b_page);
    a.bind_text_label(b_done);

    // Objects promoted during this collection.
    let old_object = a.create_text_label();
    let old_ready = a.create_text_label();
    let old_done = a.create_text_label();
    let old_fields = a.create_text_label();
    let old_field = a.create_text_label();
    let old_next_field = a.create_text_label();
    let old_next_object = a.create_text_label();
    a.bind_text_label(old_object);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_OLD_SCAN));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_OLD_TOP));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, old_done);
    if let Some(relax) = relax {
        // A slot reserved by the mutator whose descriptor is not written yet.
        a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
        a.add_reg_reg(R::V2, R::V0);
        a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(ObjectHeader, descriptor_id)));
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, old_ready);
        a.call_label(relax);
        a.jmp_label(old_object);
    }
    a.bind_text_label(old_ready);
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
    field_address(a, R::V2, R::V7, RT_OLD_SCAN);
    a.load_ptr_disp32(R::V0, R::V2, 0);
    super::worker::remember_read(a, l);
    // Any heap payload address (Young, or old-half Old in a full
    // collection) is forwarded; forward leaves non-moving ones alone.
    super::enums::heap_offset(a, R::V0, R::V1, old_next_field);
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    a.mov_imm64(R::V1, color());
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(fwd);
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V2, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V2);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    field_address(a, R::V2, R::V7, RT_OLD_SCAN);
    super::worker::heal(a, l, old_next_field);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_imm32(R::V1, YOUNG_END as i32);
    a.jcc_label(C::AboveOrEqual, old_next_field);
    super::remset::remember(a, l, R::V2, R::V1, R::V6);
    a.bind_text_label(old_next_field);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    a.add_reg_imm32(R::V7, 1);
    a.store_ptr_disp32(R::V3, d(RT_CS_FIELD), R::V7);
    a.jmp_label(old_field);
    a.bind_text_label(old_next_object);
    advance(a, RT_OLD_SCAN);
    a.jmp_label(old_object);

    a.bind_text_label(old_done);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PROGRESS));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pass);
    a.mov_imm64(R::V0, 1);
    a.ret();
    a.bind_text_label(paused);
    a.mov_imm64(R::V0, 0);
    a.ret();
}

/// Worker variant, chain A (V8 = page, V2 = its `allocated_top`, already
/// read): while the mutator is copying into its TLAB, the last object of
/// the last bound page may still be incomplete; treat chain A as caught up
/// there (the cursor does not move past it). `allocated_top` is read before
/// the flag and the mutator sets the flag before allocating, so seeing the
/// object implies seeing the flag; clearing it follows the claiming CAS.
fn copy_in_flight<E: PortableAsm>(a: &mut E, caught_up: E::TextLabel) {
    let scan = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V3, d(RT_M_COPYING));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, scan);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_CS_PAGE));
    a.add_reg_imm32(R::V0, 1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_NEXT_PAGE));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, scan);
    // Is the object at the cursor the page's last one?
    a.push_reg(R::V2);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_CS_OFF));
    a.load_ptr_disp32(R::V0, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(ObjectHeader, payload_bytes)));
    let sized = a.create_text_label();
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V0, HEADER);
    a.add_reg_reg(R::V0, R::V1);
    a.pop_reg(R::V2);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::AboveOrEqual, caught_up);
    a.bind_text_label(scan);
}

/// Scan the Young object at heap offset `[cursor]`: forward every field
/// that addresses Young payload, heal it, and advance the cursor; then jump
/// to `back`.
fn scan_young_object<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    fwd: E::TextLabel,
    cursor: usize,
    back: E::TextLabel,
) {
    let fields = a.create_text_label();
    let field_loop = a.create_text_label();
    let next_field = a.create_text_label();
    let next_object = a.create_text_label();
    a.load_ptr_disp32(R::V1, R::V3, d(cursor));
    // Reference-bearing descriptors: 2 and 4 (skip the list's length word).
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
    a.bind_text_label(field_loop);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    a.load_ptr_disp32(R::V6, R::V3, d(RT_CS_COUNT));
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::AboveOrEqual, next_object);
    field_address(a, R::V2, R::V7, cursor);
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
    a.mov_imm64(R::V1, color());
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(fwd);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    field_address(a, R::V2, R::V7, cursor);
    super::worker::heal(a, l, next_field);
    a.bind_text_label(next_field);
    a.load_ptr_disp32(R::V7, R::V3, d(RT_CS_FIELD));
    a.add_reg_imm32(R::V7, 1);
    a.store_ptr_disp32(R::V3, d(RT_CS_FIELD), R::V7);
    a.jmp_label(field_loop);
    a.bind_text_label(next_object);
    advance(a, cursor);
    a.jmp_label(back);
}

/// `[cursor] += header + max(16, round_up(payload, 16))`.
fn advance<E: PortableAsm>(a: &mut E, cursor: usize) {
    let sized = a.create_text_label();
    a.load_ptr_disp32(R::V1, R::V3, d(cursor));
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
    a.store_ptr_disp32(R::V3, d(cursor), R::V1);
}

/// `dst = heap_base + [cursor] + header + index * 8` (clobbers `index`).
fn field_address<E: PortableAsm>(a: &mut E, dst: R, index: R, cursor: usize) {
    a.load_ptr_disp32(dst, R::V3, d(RT_HEAP_BASE));
    a.shl_reg_imm8(index, 3);
    a.add_reg_reg(dst, index);
    a.load_ptr_disp32(index, R::V3, d(cursor));
    a.add_reg_reg(dst, index);
    a.add_reg_imm32(dst, HEADER);
}

const _: () = assert!(DESCRIPTOR < 0);
