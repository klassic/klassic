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
    RT_ALLOC_SIZE, RT_ALLOC_TAG, RT_COLLECTING, RT_COLLECTIONS, RT_CTX, RT_FRAME_ROOT,
    RT_FROM_BASE, RT_FROM_END, RT_FROM_MARKS, RT_HEAP_BASE, RT_MARKS, RT_NEXT_PAGE, RT_PAGES,
    RT_REQUEST, RT_SAVED_DESCRIPTOR, RT_SAVED_ELEMENTS, RT_SAVED_SIZE, RT_SAVED_TAG, RT_SCAN_INDEX,
    RT_SEGMENT, RT_SPACE_BASE, RT_SPACE_END, STARTS_WORDS, d, imm, lea_rt, root_step,
    roots::frame_handle,
};
use crate::gc_abi::{
    AllocationPage, AllocationRequest, ForwardTable, MarkArena, MarkRecord, PAGE_READY, RootSegment,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

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
    emit_phase(a, l);
    let elements = d(RT_REQUEST + offset_of!(AllocationRequest, elements));
    let descriptor = d(RT_REQUEST + offset_of!(AllocationRequest, descriptor));
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
    // `forward` copies through alloc_raw, which overwrites the shape tag and
    // the request descriptor; the interrupted allocation needs its own back.
    a.load_ptr_disp32(R::V0, R::V3, d(RT_ALLOC_TAG));
    a.store_ptr_disp32(R::V3, d(RT_SAVED_TAG), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, descriptor);
    a.store_ptr_disp32(R::V3, d(RT_SAVED_DESCRIPTOR), R::V0);
    // A relocation phase still open from the previous collection must
    // complete before its spaces can flip again.
    a.call_label(l.finish);
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
    // Full collection when Old could not absorb a worst-case promotion:
    // flip Old too and evacuate every live object into the new Old half.
    let young_only = a.create_text_label();
    let decided = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_OLD_TOP));
    a.load_ptr_disp32(R::V1, R::V3, d(super::RT_OLD_BASE));
    a.sub_reg_reg(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, imm(super::OLD_HALF - super::YOUNG_SEMI_BYTES));
    a.jcc_label(C::Below, young_only);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(super::RT_FULL), R::V0);
    a.store_ptr_disp32(R::V3, d(super::RT_OLD_FROM), R::V1);
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_OLD_TOP));
    a.store_ptr_disp32(R::V3, d(super::RT_OLD_FROM_END), R::V0);
    let second = a.create_text_label();
    let flipped = a.create_text_label();
    a.cmp_reg_imm32(R::V1, imm(super::YOUNG_END));
    a.jcc_label(C::Equal, second);
    a.mov_imm64(R::V1, super::YOUNG_END);
    a.jmp_label(flipped);
    a.bind_text_label(second);
    a.mov_imm64(R::V1, super::YOUNG_END + super::OLD_HALF);
    a.bind_text_label(flipped);
    a.store_ptr_disp32(R::V3, d(super::RT_OLD_BASE), R::V1);
    a.store_ptr_disp32(R::V3, d(super::RT_OLD_TOP), R::V1);
    // Everything is promoted, so no Young survivor needs a remembered field.
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(super::RT_REMSET_LEN), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_FULL_COLLECTIONS));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(super::RT_FULL_COLLECTIONS), R::V0);
    a.jmp_label(decided);
    a.bind_text_label(young_only);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(super::RT_FULL), R::V0);
    a.bind_text_label(decided);
    // From-space ranges: exactly the objects this collection moves.
    let full_range = a.create_text_label();
    let ranges = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V3, d(RT_FROM_BASE));
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    a.store_ptr_disp32(R::V3, d(super::RT_RY_LO), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_FROM_END));
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    a.store_ptr_disp32(R::V3, d(super::RT_RY_HI), R::V0);
    a.mov_imm64(R::V0, 0);
    a.mov_imm64(R::V1, 0);
    a.load_ptr_disp32(R::V2, R::V3, d(super::RT_FULL));
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, ranges);
    a.bind_text_label(full_range);
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_OLD_FROM));
    a.load_ptr_disp32(R::V1, R::V3, d(super::RT_OLD_FROM_END));
    a.bind_text_label(ranges);
    a.store_ptr_disp32(R::V3, d(super::RT_RO_LO), R::V0);
    a.store_ptr_disp32(R::V3, d(super::RT_RO_HI), R::V1);
    // Objects promoted from here on are scanned by the Cheney pass.
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_OLD_TOP));
    a.store_ptr_disp32(R::V3, d(super::RT_OLD_SCAN), R::V0);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V0);

    // Evacuate every referent of cells [1, used); cell 0 is the scratch root.
    let scan = a.create_text_label();
    let next = a.create_text_label();
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
    a.call_label(l.forward);
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
    // Old fields that point into Young are roots of a Young collection.
    a.call_label(l.remset_roots);
    // Collection-literal cells hold payload addresses in native data.
    a.call_label(l.literals);
    // The pause ends here: to-space and promoted objects are scanned by the
    // open relocation phase (allocation quanta, load barriers), and
    // from-space is recycled when that phase finishes.
    super::enums::emit_cheney_init(a);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(super::RT_PHASE_OPEN), R::V0);
    clear_scratch(a, l);

    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_COLLECTIONS));
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTIONS), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SAVED_SIZE));
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_SIZE), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SAVED_ELEMENTS));
    a.store_ptr_disp32(R::V3, elements, R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SAVED_TAG));
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_TAG), R::V0);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_SAVED_DESCRIPTOR));
    a.store_ptr_disp32(R::V3, descriptor, R::V0);
    a.ret();
}

/// `finish`: complete an open relocation phase and recycle from-space;
/// `quantum`: advance it by a bounded number of objects. V3 = runtime.
fn emit_phase<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let closed = a.create_text_label();
    a.bind_text_label(l.quantum);
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_PHASE_OPEN));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, closed);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V0);
    a.mov_imm64(R::V0, super::QUANTUM_OBJECTS);
    a.store_ptr_disp32(R::V3, d(super::RT_BUDGET), R::V0);
    a.call_label(l.cheney);
    a.mov_imm64(R::V1, 0);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, closed);
    a.jmp_label(l.finish);

    a.bind_text_label(l.finish);
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_PHASE_OPEN));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, closed);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V3, d(RT_COLLECTING), R::V0);
    a.mov_imm64(R::V0, u64::MAX);
    a.store_ptr_disp32(R::V3, d(super::RT_BUDGET), R::V0);
    a.call_label(l.cheney);
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

    // After a full collection the old Old half is garbage: zero it so the
    // next flip bump-allocates over clean memory (padding compares equal).
    let no_full = a.create_text_label();
    a.load_ptr_disp32(R::V0, R::V3, d(super::RT_FULL));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no_full);
    a.load_ptr_disp32(R::V7, R::V3, d(super::RT_OLD_FROM));
    a.load_ptr_disp32(R::V1, R::V3, d(super::RT_OLD_FROM_END));
    a.sub_reg_reg(R::V1, R::V7);
    a.shr_reg_imm8(R::V1, 3);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V7, R::V0);
    a.call_label(l.zero);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V3, d(super::RT_FULL), R::V0);
    a.bind_text_label(no_full);
    a.mov_imm64(R::V0, 0);
    for field in [
        super::RT_PHASE_OPEN,
        super::RT_RY_LO,
        super::RT_RY_HI,
        super::RT_RO_LO,
        super::RT_RO_HI,
        RT_COLLECTING,
    ] {
        a.store_ptr_disp32(R::V3, d(field), R::V0);
    }
    a.bind_text_label(closed);
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
