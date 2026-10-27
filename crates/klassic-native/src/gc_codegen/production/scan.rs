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
//! written last (promotion; the worker yields when a reserved slot's
//! descriptor is still zero).
use super::{
    Labels, RT_BUDGET, RT_CB_OFF, RT_CB_PAGE, RT_COPY_LOW, RT_CS_COUNT, RT_CS_FIELD, RT_CS_OFF,
    RT_CS_PAGE, RT_HEAP_BASE, RT_NEXT_PAGE, RT_OLD_SCAN, RT_PAGES, RT_PROGRESS, RT_SPACE_BASE,
    RT_SPACE_END,
    copy::{DESCRIPTOR, HEADER},
    d, load_rt, store_rt,
};
use crate::gc_abi::{AllocationPage, ObjectHeader};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

/// `dst = pages + index * 112`.
pub(super) fn page_ptr<E: PortableAsm>(a: &mut E, dst: R, index: R, scratch: R) {
    a.mov_reg_reg(dst, index);
    a.shl_reg_imm8(dst, 7);
    a.mov_reg_reg(scratch, index);
    a.shl_reg_imm8(scratch, 4);
    a.sub_reg_reg(dst, scratch);
    load_rt(a, scratch, RT_PAGES);
    a.add_reg_reg(dst, scratch);
}

/// Take one work unit from the scan budget, or jump to `paused`. Records that
/// this pass made progress. Clobbers V9.
pub(super) fn spend<E: PortableAsm>(a: &mut E, paused: E::TextLabel) {
    load_rt(a, R::V9, RT_BUDGET);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, paused);
    a.sub_reg_imm8(R::V9, 1);
    store_rt(a, RT_BUDGET, R::V9);
    a.mov_imm64(R::V9, 1);
    store_rt(a, RT_PROGRESS, R::V9);
}

/// Point the chain-A cursor at the first to-space page and the chain-B
/// cursor at the last one.
pub(super) fn emit_cheney_init<E: PortableAsm>(a: &mut E, fail: E::TextLabel) {
    super::relocate_pages::begin(a, fail);
    super::weak::begin_move(a);
    // Young's captured Old spill prefix must be discovered before movement.
    load_rt(a, R::V0, super::mark_queue::discovery_frozen());
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, fail);
    super::store_rt_imm(a, super::mark_queue::relocate_previous(), 0);
    load_rt(a, R::V0, super::mark_queue::head(false));
    store_rt(a, super::mark_queue::relocate_current(), R::V0);
    a.mov_imm64(R::V0, 0);
    for field in [super::RT_CS_KIND, RT_CS_FIELD, RT_CS_COUNT] {
        store_rt(a, field, R::V0);
    }
    let start = d(offset_of!(AllocationPage, start));
    load_rt(a, R::V0, RT_SPACE_BASE);
    store_rt(a, RT_CS_PAGE, R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, start);
    store_rt(a, RT_CS_OFF, R::V0);
    load_rt(a, R::V0, RT_SPACE_END);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, RT_CB_PAGE, R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, start);
    store_rt(a, RT_CB_OFF, R::V0);
}

pub(super) fn emit_cheneys<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_cheney(a, l, l.cheney, l.forward);
    if let Some(w) = &l.worker {
        emit_cheney(a, l, w.wcheney, w.wforward);
    }
}

/// `cheney` (V3 = runtime): V0 = 1 when the scan is complete, 0 when the
/// work budget ran out. The worker also skips incomplete mutator copies.
fn emit_cheney<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    entry: E::TextLabel,
    fwd: E::TextLabel,
) {
    let start = d(offset_of!(AllocationPage, start));
    let top = d(offset_of!(AllocationPage, allocated_top));
    let pass = a.create_text_label();
    let paused = a.create_text_label();
    let a_resume = a.create_text_label();
    let b_resume = a.create_text_label();
    let old_resume = a.create_text_label();
    let a_page = a.create_text_label();
    let a_in = a.create_text_label();
    let a_done = a.create_text_label();
    a.bind_text_label(entry);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_R_ROOT_WORK, R::V0);
    store_rt(a, super::RT_CS_COMPLETED, R::V0);
    store_rt(a, RT_PROGRESS, R::V0);
    load_rt(a, R::V0, super::RT_CS_KIND);
    for (kind, resume) in [(1, a_resume), (2, b_resume), (3, old_resume)] {
        a.cmp_reg_imm8(R::V0, kind);
        a.jcc_label(C::Equal, resume);
    }
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, super::relocate_pages::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, a_page);
    a.bind_text_label(pass);
    super::relocate_pages::start_pass(a);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_PROGRESS, R::V0);

    super::mark_frontier::scan(a, l, fwd, paused);
    super::remset::scan(a, l, fwd, paused);
    // Unvisited roots are part of the relocation frontier. An empty heap
    // scan cannot finish/recycle while these cells still name from-space.
    super::relocate_roots::scan(a, l, fwd, paused);
    super::store_rt_imm(a, super::relocate_pages::ACTIVE, 1);

    // Chain A: every owner may keep an earlier page open. Retain a cursor
    // per page and revisit all pages on each budgeted sweep.
    a.bind_text_label(a_page);
    load_rt(a, R::V0, RT_CS_PAGE);
    load_rt(a, R::V1, RT_NEXT_PAGE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, a_done);
    let opened = a.create_text_label();
    load_rt(a, R::V0, super::relocate_pages::PAGE_OPENED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, opened);
    // Empty pages consume budget too. A charged page remains opened across
    // yields, so even a one-unit quantum eventually reaches its objects.
    load_rt(a, R::V0, RT_BUDGET);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, paused);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, RT_BUDGET, R::V0);
    load_rt(a, R::V0, RT_CS_PAGE);
    page_ptr(a, R::V8, R::V0, R::V1);
    super::relocate_pages::load_cursor(a);
    a.bind_text_label(opened);
    load_rt(a, R::V0, RT_CS_PAGE);
    page_ptr(a, R::V8, R::V0, R::V1);
    load_rt(a, R::V1, RT_CS_OFF);
    a.mov_reg_reg(R::V2, R::V8);
    a.add_reg_imm32(R::V2, top);
    a.atomic_load_acquire(R::V2, R::V2);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, a_in);
    // Persisted frontier remains valid if this owner appends after we leave.
    load_rt(a, R::V0, RT_CS_PAGE);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, RT_CS_PAGE, R::V0);
    super::store_rt_imm(a, super::relocate_pages::PAGE_OPENED, 0);
    a.jmp_label(a_page);
    a.bind_text_label(a_in);
    scan_young_object(a, l, fwd, (RT_CS_OFF, a_page, 1, a_resume), paused);
    a.bind_text_label(a_done);

    // Chain B: the worker's pages [copy_low, space_end) top-down.
    let b_page = a.create_text_label();
    let b_in = a.create_text_label();
    let b_done = a.create_text_label();
    a.bind_text_label(b_page);
    load_rt(a, R::V0, RT_CB_PAGE);
    load_rt(a, R::V1, RT_COPY_LOW);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, b_done);
    page_ptr(a, R::V8, R::V0, R::V1);
    load_rt(a, R::V1, RT_CB_OFF);
    a.load_ptr_disp32(R::V2, R::V8, top);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, b_in);
    // Only the worker fills chain B, one page at a time: a lower reserved
    // page means this one is closed.
    load_rt(a, R::V0, RT_CB_PAGE);
    load_rt(a, R::V1, RT_COPY_LOW);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::AboveOrEqual, b_done);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, RT_CB_PAGE, R::V0);
    page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, start);
    store_rt(a, RT_CB_OFF, R::V0);
    a.jmp_label(b_page);
    a.bind_text_label(b_in);
    scan_young_object(a, l, fwd, (RT_CB_OFF, b_page, 2, b_resume), paused);
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
    super::old_reserve::scan(a, l, old_object, old_done, paused);
    load_rt(a, R::V0, RT_OLD_SCAN);
    {
        // A slot reserved by the mutator whose descriptor is not written yet.
        load_rt(a, R::V2, RT_HEAP_BASE);
        a.add_reg_reg(R::V2, R::V0);
        a.mov_reg_reg(R::V0, R::V2);
        a.add_reg_imm32(R::V0, d(offset_of!(ObjectHeader, descriptor_id)));
        a.atomic_load_acquire(R::V1, R::V0);
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, old_ready);
        // Release the lock instead of waiting for an in-flight promoter.
        a.jmp_label(paused);
    }
    a.bind_text_label(old_ready);
    // Fresh large Old allocations retain an INITIALIZING header until the
    // next owner safepoint. Never scan their partly initialized payload.
    a.mov_reg_reg(R::V0, R::V2);
    a.add_reg_imm32(R::V0, d(offset_of!(ObjectHeader, state)));
    a.atomic_load_acquire(R::V1, R::V0);
    a.cmp_reg_imm8(R::V1, crate::gc_abi::OBJECT_PUBLISHED as i8);
    a.jcc_label(C::NotEqual, paused);
    spend(a, paused);
    load_rt(a, R::V0, RT_OLD_SCAN);
    load_rt(a, R::V2, RT_HEAP_BASE);
    a.add_reg_reg(R::V2, R::V0);
    a.mov_reg_reg(R::V1, R::V2);
    a.add_reg_imm32(R::V1, d(offset_of!(ObjectHeader, descriptor_id)));
    a.atomic_load_acquire(R::V0, R::V1);
    a.load_ptr_disp32(R::V6, R::V2, d(offset_of!(ObjectHeader, payload_bytes)));
    a.shr_reg_imm8(R::V6, 3);
    store_rt(a, RT_CS_COUNT, R::V6);
    a.mov_imm64(R::V7, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, old_fields);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, old_next_object);
    a.mov_imm64(R::V7, 1);
    a.bind_text_label(old_fields);
    a.mov_imm64(R::V0, 3);
    store_rt(a, super::RT_CS_KIND, R::V0);
    store_rt(a, RT_CS_FIELD, R::V7);
    a.bind_text_label(old_resume);
    a.bind_text_label(old_field);
    load_rt(a, R::V7, RT_CS_FIELD);
    load_rt(a, R::V6, RT_CS_COUNT);
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::AboveOrEqual, old_next_object);
    spend(a, paused);
    field_address(a, R::V2, R::V7, RT_OLD_SCAN);
    a.atomic_load_acquire(R::V0, R::V2);
    super::worker::remember_read(a, l);
    // Any managed field reference (Young, or old-half Old in a full
    // collection) is forwarded; forward leaves non-moving ones alone.
    super::fields::offset(a, R::V0, R::V1, old_next_field);
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    super::colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(fwd);
    a.mov_reg_reg(R::V1, R::V0);
    load_rt(a, R::V7, RT_CS_FIELD);
    field_address(a, R::V2, R::V7, RT_OLD_SCAN);
    super::worker::heal(a, l, old_next_field);
    a.shr_reg_imm8(R::V1, 16);
    super::generation::test(a, R::V1, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, old_next_field);
    super::remset::remember(a, l, R::V2, R::V1, R::V6);
    a.bind_text_label(old_next_field);
    load_rt(a, R::V7, RT_CS_FIELD);
    a.add_reg_imm32(R::V7, 1);
    store_rt(a, RT_CS_FIELD, R::V7);
    a.jmp_label(old_field);
    a.bind_text_label(old_next_object);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_CS_KIND, R::V0);
    advance(a, RT_OLD_SCAN);
    super::copy::bump(a, super::RT_CS_COMPLETED, R::V0);
    a.jmp_label(old_object);

    a.bind_text_label(old_done);
    super::large_trace::scan(a, l, fwd, paused);
    super::weak::relocate(a, l, fwd, paused);
    load_rt(a, R::V0, RT_PROGRESS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pass);
    super::relocate_pages::seal_pass(a, pass);
    a.mov_imm64(R::V0, 1);
    a.ret();
    a.bind_text_label(paused);
    a.mov_imm64(R::V0, 0);
    a.ret();
}

/// Scan the Young object at heap offset `[cursor]`: forward every field
/// whose reference points into Young, heal it, and advance the cursor; then jump
/// to `back`.
fn scan_young_object<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    fwd: E::TextLabel,
    point: (usize, E::TextLabel, u64, E::TextLabel),
    paused: E::TextLabel,
) {
    let (cursor, back, kind, resume) = point;
    let fields = a.create_text_label();
    let field_loop = a.create_text_label();
    let next_field = a.create_text_label();
    let next_object = a.create_text_label();
    load_rt(a, R::V1, cursor);
    // Reference-bearing descriptors: 2 and 4 (skip the list's length word).
    load_rt(a, R::V2, RT_HEAP_BASE);
    a.add_reg_reg(R::V2, R::V1);
    a.mov_reg_reg(R::V0, R::V2);
    a.add_reg_imm32(R::V0, d(offset_of!(ObjectHeader, state)));
    a.atomic_load_acquire(R::V0, R::V0);
    a.cmp_reg_imm8(R::V0, crate::gc_abi::OBJECT_PUBLISHED as i8);
    a.jcc_label(C::NotEqual, paused);
    spend(a, paused);
    a.mov_reg_reg(R::V0, R::V2);
    a.add_reg_imm32(R::V0, d(offset_of!(ObjectHeader, descriptor_id)));
    a.atomic_load_acquire(R::V0, R::V0);
    a.load_ptr_disp32(R::V6, R::V2, d(offset_of!(ObjectHeader, payload_bytes)));
    a.shr_reg_imm8(R::V6, 3);
    store_rt(a, RT_CS_COUNT, R::V6);
    a.mov_imm64(R::V7, 0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, fields);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, next_object);
    a.mov_imm64(R::V7, 1);
    a.bind_text_label(fields);
    a.mov_imm64(R::V0, kind);
    store_rt(a, super::RT_CS_KIND, R::V0);
    store_rt(a, RT_CS_FIELD, R::V7);
    a.bind_text_label(resume);
    a.bind_text_label(field_loop);
    load_rt(a, R::V7, RT_CS_FIELD);
    load_rt(a, R::V6, RT_CS_COUNT);
    a.cmp_reg_reg(R::V7, R::V6);
    a.jcc_label(C::AboveOrEqual, next_object);
    spend(a, paused);
    field_address(a, R::V2, R::V7, cursor);
    a.atomic_load_acquire(R::V0, R::V2);
    super::worker::remember_read(a, l);
    super::fields::young_offset(a, R::V0, R::V1, next_field);
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    super::colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(fwd);
    load_rt(a, R::V7, RT_CS_FIELD);
    field_address(a, R::V2, R::V7, cursor);
    super::worker::heal(a, l, next_field);
    a.bind_text_label(next_field);
    load_rt(a, R::V7, RT_CS_FIELD);
    a.add_reg_imm32(R::V7, 1);
    store_rt(a, RT_CS_FIELD, R::V7);
    a.jmp_label(field_loop);
    a.bind_text_label(next_object);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_CS_KIND, R::V0);
    advance(a, cursor);
    if kind == 1 {
        super::relocate_pages::save_cursor(a);
    }
    super::copy::bump(a, super::RT_CS_COMPLETED, R::V0);
    a.jmp_label(back);
}

/// `[cursor] += header + max(16, round_up(payload, 16))`.
fn advance<E: PortableAsm>(a: &mut E, cursor: usize) {
    let sized = a.create_text_label();
    load_rt(a, R::V1, cursor);
    load_rt(a, R::V2, RT_HEAP_BASE);
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
    store_rt(a, cursor, R::V1);
}

/// `dst = heap_base + [cursor] + header + index * 8` (clobbers `index`).
fn field_address<E: PortableAsm>(a: &mut E, dst: R, index: R, cursor: usize) {
    load_rt(a, dst, RT_HEAP_BASE);
    a.shl_reg_imm8(index, 3);
    a.add_reg_reg(dst, index);
    load_rt(a, index, cursor);
    a.add_reg_reg(dst, index);
    a.add_reg_imm32(dst, HEADER);
}

const _: () = assert!(DESCRIPTOR < 0);
