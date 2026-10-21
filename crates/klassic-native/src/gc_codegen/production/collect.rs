//! M-G2 stop-the-world Young evacuation for `--gc=generational`.
//!
//! Every managed reference of the M-G1 subset lives in a RootCell (heap
//! strings carry no references), so a collection is: flip semispaces, copy
//! each object a cell references through the ordinary allocate/publish path,
//! STORE the new reference through the root journal, then recycle the
//! evacuated pages and their metadata for the next flip. Shared referents are
//! copied once: the dead source header is turned into a forwarding record.
use super::{
    CELLS_PER_PAGE, HALF, L, Labels, PAGES, REQUEST_VALUE, ROOT_STORE, RT_ALLOC_ROOT,
    RT_ALLOC_SIZE, RT_COLLECTING, RT_COLLECTIONS, RT_CTX, RT_FRAME_ROOT, RT_FROM_BASE, RT_FROM_END,
    RT_FROM_MARKS, RT_HEAP_BASE, RT_MARKS, RT_NEXT_PAGE, RT_PAGES, RT_PENDING_REF, RT_REQUEST,
    RT_SAVED_ELEMENTS, RT_SAVED_SIZE, RT_SCAN_INDEX, RT_SCAN_OLD, RT_SEGMENT, RT_SPACE_BASE,
    RT_SPACE_END, STARTS_WORDS, d, imm, lea_rt, root_step, roots::frame_handle,
};
use crate::gc_abi::{
    AllocationPage, AllocationRequest, ForwardTable, MarkArena, MarkRecord, ObjectHeader,
    PAGE_READY, RootSegment,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

/// Descriptor id written over an evacuated source header; its
/// `mark_record` word then holds the copy's reference. Never a real id
/// (builtin ids are 1, 2 and 4) and only ever seen in from-space.
const FORWARDED: u64 = 0xf0f0_f0f0;
const HEADER: i32 = size_of::<ObjectHeader>() as i32;
/// Bytes of one semispace's mark-record arena.
const ARENA_BYTES: u64 = HALF * CELLS_PER_PAGE * size_of::<MarkRecord>() as u64;

/// `dst = pages + index * 112`.
fn page_ptr<E: PortableAsm>(a: &mut E, dst: R, index: R, scratch: R) {
    a.mov_reg_reg(dst, index);
    a.shl_reg_imm8(dst, 7);
    a.mov_reg_reg(scratch, index);
    a.shl_reg_imm8(scratch, 4);
    a.sub_reg_reg(dst, scratch);
    a.load_ptr_disp32(scratch, R::V3, d(RT_PAGES));
    a.add_reg_reg(dst, scratch);
}

/// `dst = mark arena of the semispace starting at page `base``.
fn arena<E: PortableAsm>(a: &mut E, dst: R, base: R) {
    let first = a.create_text_label();
    lea_rt(a, dst, L.marks as usize);
    a.test_reg_reg(base, base);
    a.jcc_label(C::Equal, first);
    a.add_reg_imm32(dst, imm(ARENA_BYTES));
    a.bind_text_label(first);
}

/// `dst = cells + index * 32` for the root segment.
fn cell_ptr<E: PortableAsm>(a: &mut E, dst: R, index: R) {
    a.load_ptr_disp32(dst, R::V3, d(RT_SEGMENT));
    a.load_ptr_disp32(dst, dst, d(offset_of!(RootSegment, cells)));
    a.shl_reg_imm8(index, 5);
    a.add_reg_reg(dst, index);
}

pub(super) fn emit_collect<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let elements = d(RT_REQUEST + offset_of!(AllocationRequest, elements));
    let page_field = d(RT_REQUEST + offset_of!(AllocationRequest, page));

    // zero: V7 = address, V1 = qwords. Clobbers V0, V1, V7 only.
    let zero_loop = a.create_text_label();
    let zero_done = a.create_text_label();
    a.bind_text_label(l.zero);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(zero_loop);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, zero_done);
    a.store_ptr_disp32(R::V7, 0, R::V0);
    a.add_reg_imm32(R::V7, 8);
    a.sub_reg_imm8(R::V1, 1);
    a.jmp_label(zero_loop);
    a.bind_text_label(zero_done);
    a.ret();

    // collect: V3 = runtime. Clobbers the caller-saved registers; the
    // interrupted allocation's size and element count are restored.
    a.bind_text_label(l.collect);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_ALLOC_SIZE));
    a.store_ptr_disp32(R::V3, d(RT_SAVED_SIZE), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, elements);
    a.store_ptr_disp32(R::V3, d(RT_SAVED_ELEMENTS), R::V0);
    clear_scratch(a, l);
    // Close the allocating page: it belongs to the space being evacuated.
    let closed = a.create_text_label();
    a.load_ptr_disp32(R::V6, R::V3, page_field);
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, closed);
    lea_rt(a, R::V7, RT_CTX);
    a.mov_imm64(R::V2, 1);
    a.call_label(l.tlab_page);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, page_field, R::V0);
    a.bind_text_label(closed);

    // Flip: remember from-space, allocate from the other half.
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SPACE_BASE));
    a.store_ptr_disp32(R::V3, d(RT_FROM_BASE), R::V0);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_NEXT_PAGE));
    a.store_ptr_disp32(R::V3, d(RT_FROM_END), R::V1);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_MARKS + offset_of!(MarkArena, used)));
    a.store_ptr_disp32(R::V3, d(RT_FROM_MARKS), R::V1);
    a.mov_imm64(R::V1, HALF);
    a.sub_reg_reg(R::V1, R::V0);
    a.store_ptr_disp32(R::V3, d(RT_SPACE_BASE), R::V1);
    a.store_ptr_disp32(R::V3, d(RT_NEXT_PAGE), R::V1);
    arena(a, R::V2, R::V1);
    a.store_ptr_disp32(R::V3, d(RT_MARKS + offset_of!(MarkArena, records)), R::V2);
    a.add_reg_imm32(R::V1, imm(HALF));
    a.store_ptr_disp32(R::V3, d(RT_SPACE_END), R::V1);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_MARKS + offset_of!(MarkArena, used)), R::V0);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V0);

    // Evacuate every referent of cells [1, used); cell 0 is the scratch root.
    let scan = a.create_text_label();
    let next = a.create_text_label();
    let store = a.create_text_label();
    let scanned = a.create_text_label();
    a.store_ptr_disp32(R::V3, d(RT_SCAN_INDEX), R::V0);
    a.bind_text_label(scan);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SCAN_INDEX));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_SEGMENT));
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(RootSegment, used)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, scanned);
    cell_ptr(a, R::V1, R::V0);
    a.load_ptr_disp32(R::V0, R::V1, 0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, next);
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V1);
    // Already evacuated through another cell: reuse its copy.
    a.load_ptr_disp32(
        R::V1,
        R::V0,
        offset_of!(ObjectHeader, descriptor_id) as i32 - HEADER,
    );
    a.mov_imm64(R::V2, FORWARDED);
    a.cmp_reg_reg(R::V1, R::V2);
    let copy = a.create_text_label();
    a.jcc_label(C::NotEqual, copy);
    a.load_ptr_disp32(
        R::V0,
        R::V0,
        offset_of!(ObjectHeader, mark_record) as i32 - HEADER,
    );
    a.jmp_label(store);
    a.bind_text_label(copy);
    a.store_ptr_disp32(R::V3, d(RT_SCAN_OLD), R::V0);
    a.load_ptr_disp32(
        R::V1,
        R::V0,
        offset_of!(ObjectHeader, payload_bytes) as i32 - HEADER,
    );
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_SIZE), R::V1);
    a.call_label(l.alloc_raw);
    a.mov_reg_reg(R::V7, R::V0);
    a.load_ptr_disp32(R::V6, R::V3, d(RT_SCAN_OLD));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_ALLOC_SIZE));
    a.rep_movsb();
    // Turn the source header into a forwarding record.
    a.load_ptr_disp32(R::V6, R::V3, d(RT_SCAN_OLD));
    a.mov_imm64(R::V1, FORWARDED);
    a.store_ptr_disp32(
        R::V6,
        offset_of!(ObjectHeader, descriptor_id) as i32 - HEADER,
        R::V1,
    );
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PENDING_REF));
    a.store_ptr_disp32(
        R::V6,
        offset_of!(ObjectHeader, mark_record) as i32 - HEADER,
        R::V0,
    );
    a.bind_text_label(store);
    a.store_ptr_disp32(R::V3, d(RT_FRAME_ROOT + REQUEST_VALUE), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SCAN_INDEX));
    cell_ptr(a, R::V1, R::V0);
    frame_handle(a, R::V1, R::V2);
    root_step(a, l, RT_FRAME_ROOT, ROOT_STORE);
    a.bind_text_label(next);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SCAN_INDEX));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_SCAN_INDEX), R::V0);
    a.jmp_label(scan);
    a.bind_text_label(scanned);
    clear_scratch(a, l);

    // Recycle from-space pages [from_base, from_end): zero payload, start
    // bits and forward cells, then make each READY under a new incarnation.
    let pages = a.create_text_label();
    let recycled = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V3, d(RT_FROM_BASE));
    a.store_ptr_disp32(R::V3, d(RT_SCAN_INDEX), R::V0);
    a.bind_text_label(pages);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SCAN_INDEX));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_FROM_END));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, recycled);
    page_ptr(a, R::V8, R::V0, R::V1);
    let field = |f: usize| d(f);
    a.load_ptr_disp32(R::V7, R::V8, field(offset_of!(AllocationPage, start)));
    a.load_ptr_disp32(
        R::V1,
        R::V8,
        field(offset_of!(AllocationPage, allocated_top)),
    );
    a.sub_reg_reg(R::V1, R::V7);
    a.shr_reg_imm8(R::V1, 3);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V7, R::V0);
    a.call_label(l.zero);
    a.load_ptr_disp32(R::V7, R::V8, field(offset_of!(AllocationPage, starts)));
    a.mov_imm64(R::V1, STARTS_WORDS);
    a.call_label(l.zero);
    a.load_ptr_disp32(
        R::V9,
        R::V8,
        field(offset_of!(AllocationPage, forward_table)),
    );
    a.load_ptr_disp32(R::V7, R::V9, d(offset_of!(ForwardTable, cells)));
    a.load_ptr_disp32(R::V1, R::V9, d(offset_of!(ForwardTable, used)));
    a.shl_reg_imm8(R::V1, 2);
    a.call_label(l.zero);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V9, d(offset_of!(ForwardTable, used)), R::V0);
    a.store_ptr_disp32(R::V8, field(offset_of!(AllocationPage, owner)), R::V0);
    a.load_ptr_disp32(R::V0, R::V8, field(offset_of!(AllocationPage, start)));
    a.store_ptr_disp32(
        R::V8,
        field(offset_of!(AllocationPage, allocated_top)),
        R::V0,
    );
    a.mov_imm64(R::V0, PAGE_READY);
    a.store_ptr_disp32(R::V8, field(offset_of!(AllocationPage, state)), R::V0);
    a.load_ptr_disp32(R::V0, R::V8, field(offset_of!(AllocationPage, incarnation)));
    a.add_reg_imm32(R::V0, imm(PAGES));
    a.store_ptr_disp32(R::V8, field(offset_of!(AllocationPage, incarnation)), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SCAN_INDEX));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_SCAN_INDEX), R::V0);
    a.jmp_label(pages);
    a.bind_text_label(recycled);
    // Zero the used part of from-space's mark arena (168 B = 21 qwords).
    a.load_ptr_disp32(R::V0, R::V3, d(RT_FROM_BASE));
    arena(a, R::V7, R::V0);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_FROM_MARKS));
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 4);
    a.mov_reg_reg(R::V2, R::V1);
    a.shl_reg_imm8(R::V2, 2);
    a.add_reg_reg(R::V1, R::V0);
    a.add_reg_reg(R::V1, R::V2);
    a.call_label(l.zero);

    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_COLLECTIONS));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTIONS), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SAVED_SIZE));
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_SIZE), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SAVED_ELEMENTS));
    a.store_ptr_disp32(R::V3, elements, R::V0);
    a.ret();
}

/// Publish the pending object, then empty the scratch root (cell 0).
fn clear_scratch<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.call_label(l.publish_pending);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_ROOT + REQUEST_VALUE), R::V0);
    root_step(a, l, RT_ALLOC_ROOT, ROOT_STORE);
}

const _: () = assert!(size_of::<MarkRecord>() == 168);
