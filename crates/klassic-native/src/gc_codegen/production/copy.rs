//! M-G6 step 5: copying one from-space object, claimed by a CAS.
//!
//! Both copiers (the mutator, through the load barrier, the pause and its
//! own scans; the worker, through `wcheney`) copy first and then claim the
//! object by a compare-and-swap of the source header's descriptor word
//! from its original id to a forwarding word (the copy's reference bits
//! with `FWD16` in the color bits). The winner's copy is the object; a loser
//! turns its tentative copy into a raw-bytes filler (never traced again) and
//! answers the winner's reference. Young copies of the mutator go through
//! its TLAB (`alloc_raw`); the worker bump-allocates in private to-space
//! pages taken from the top of the semispace (`bounds` CAS) and publishes
//! each header itself. Promotions reserve Old space with a CAS on
//! `old_top` and write the descriptor last, so a scanner of the promoted
//! range never parses a half-written header.
use super::{
    HEAP_BYTES, L, Labels, OLD_HALF, RT_ALLOC_SIZE, RT_ALLOC_TAG, RT_BOUNDS, RT_COPY_LOW, RT_FULL,
    RT_HEAP_BASE, RT_M_COPYING, RT_MARK_VALID, RT_MUTATOR_COPIES, RT_MUTATOR_LOST, RT_OLD_BASE,
    RT_OLD_TOP, RT_PENDING_REF, RT_PROMOTED, RT_RO_HI, RT_RO_LO, RT_RY_HI, RT_RY_LO, RT_SCAN_NEW,
    RT_SCAN_OLD, RT_SURVIVOR, RT_UNMARKED, RT_W_DESC, RT_W_LIMIT, RT_W_NEW, RT_W_PAGE,
    RT_W_PROMOTED, RT_W_SRC, RT_W_TOP, RT_W_UNMARKED, RT_WORKER_COPIES, RT_WORKER_LOST, color, d,
};
use crate::gc_abi::{AllocationPage, MarkRecord, OBJECT_PUBLISHED, ObjectHeader};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

/// Color bits of a forwarding descriptor word; never a descriptor id.
pub(super) const FWD16: i32 = 0xf0f0;
pub(super) const HEADER: i32 = size_of::<ObjectHeader>() as i32;
pub(super) const DESCRIPTOR: i32 = offset_of!(ObjectHeader, descriptor_id) as i32 - HEADER;
pub(super) const PAYLOAD: i32 = offset_of!(ObjectHeader, payload_bytes) as i32 - HEADER;
pub(super) const MARK_RECORD: i32 = offset_of!(ObjectHeader, mark_record) as i32 - HEADER;
const AGE: i32 = offset_of!(MarkRecord, age) as i32;
const HEAP_PAGE: u64 = crate::gc_abi::HEAP_PAGE_BYTES;

/// Jump to `not` unless the descriptor word in `reg` is a forwarding word.
pub(super) fn unless_forwarded<E: PortableAsm>(a: &mut E, reg: R, scratch: R, not: E::TextLabel) {
    a.mov_reg_reg(scratch, reg);
    a.and_reg_imm32(scratch, 0xffff);
    a.cmp_reg_imm32(scratch, FWD16);
    a.jcc_label(C::NotEqual, not);
}

/// Forwarding word in `reg` -> the copy's reference bits.
pub(super) fn forward_target<E: PortableAsm>(a: &mut E, reg: R, scratch: R) {
    a.and_reg_imm32(reg, -0x10000);
    a.mov_imm64(scratch, color());
    a.or_reg_reg(reg, scratch);
}

/// `[V3 + offset] += 1`.
pub(super) fn bump<E: PortableAsm>(a: &mut E, offset: usize, scratch: R) {
    a.load_ptr_disp32(scratch, R::V3, d(offset));
    a.add_reg_imm32(scratch, 1);
    a.store_ptr_disp32(R::V3, d(offset), scratch);
}

/// `dst = max(16, round_up(payload, 16)) + header` for `payload` bytes.
fn footprint<E: PortableAsm>(a: &mut E, dst: R, payload: R) {
    let sized = a.create_text_label();
    a.mov_reg_reg(dst, payload);
    a.add_reg_imm32(dst, 15);
    a.and_reg_imm32(dst, -16);
    a.test_reg_reg(dst, dst);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(dst, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(dst, HEADER);
}

/// One copier's entry and private scratch/counter slots.
struct Copier<T> {
    entry: T,
    worker: bool,
    src: usize,
    new: usize,
    desc: usize,
    promoted: usize,
    unmarked: usize,
    copies: usize,
    lost: usize,
}

pub(super) fn emit_forwards<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    emit_forward(
        a,
        l,
        &Copier {
            entry: l.forward,
            worker: false,
            src: RT_SCAN_OLD,
            new: RT_SCAN_NEW,
            // alloc_raw reads the shape tag here; descriptor ids equal tags.
            desc: RT_ALLOC_TAG,
            promoted: RT_PROMOTED,
            unmarked: RT_UNMARKED,
            copies: RT_MUTATOR_COPIES,
            lost: RT_MUTATOR_LOST,
        },
    );
    if let Some(w) = &l.worker {
        emit_forward(
            a,
            l,
            &Copier {
                entry: w.wforward,
                worker: true,
                src: RT_W_SRC,
                new: RT_W_NEW,
                desc: RT_W_DESC,
                promoted: RT_W_PROMOTED,
                unmarked: RT_W_UNMARKED,
                copies: RT_WORKER_COPIES,
                lost: RT_WORKER_LOST,
            },
        );
    }
}

/// forward: V3 = runtime, V0 = non-null bits -> V0 = to-space bits.
/// Clobbers every other caller-saved register.
fn emit_forward<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, c: &Copier<E::TextLabel>) {
    let concurrent = l.worker.is_some();
    a.bind_text_label(c.entry);
    // Only words that are references move: the phase color in the low 16
    // bits and an offset inside the heap. External tagged pointers and
    // scalars stored in reference slots stay as they are.
    let not_ref = a.create_text_label();
    let heap = a.create_text_label();
    let moving = a.create_text_label();
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, 0xffff);
    a.cmp_reg_imm32(R::V1, color() as i32);
    a.jcc_label(C::NotEqual, not_ref);
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_imm32(R::V1, HEAP_PAGE as i32);
    a.jcc_label(C::Below, not_ref);
    a.mov_imm64(R::V2, HEAP_BYTES);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, heap);
    a.bind_text_label(not_ref);
    a.ret();
    a.bind_text_label(heap);
    // Only objects in this collection's from-space ranges move.
    for (lo, hi) in [(RT_RY_LO, RT_RY_HI), (RT_RO_LO, RT_RO_HI)] {
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
        a.jcc_label(C::Below, moving);
    }
    a.jmp_label(not_ref);
    a.bind_text_label(moving);
    let copy = a.create_text_label();
    a.shr_reg_imm8(R::V0, 16);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V0, R::V1);
    a.load_ptr_disp32(R::V1, R::V0, DESCRIPTOR);
    unless_forwarded(a, R::V1, R::V2, copy);
    a.mov_reg_reg(R::V0, R::V1);
    forward_target(a, R::V0, R::V1);
    a.ret();
    a.bind_text_label(copy);
    a.store_ptr_disp32(R::V3, d(c.src), R::V0);
    a.store_ptr_disp32(R::V3, d(c.desc), R::V1);
    if concurrent {
        unmarked_check(a, c.unmarked);
    }
    let promote = a.create_text_label();
    let claim = a.create_text_label();
    // A Young object that already survived one collection is promoted.
    a.load_ptr_disp32(R::V2, R::V3, d(RT_FULL));
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, promote);
    a.load_ptr_disp32(R::V2, R::V0, MARK_RECORD);
    a.load_ptr_disp32(R::V2, R::V2, AGE);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, promote);
    if c.worker {
        worker_young_copy(a, l, c);
    } else {
        mutator_young_copy(a, l, c);
    }
    a.jmp_label(claim);
    a.bind_text_label(promote);
    promote_copy(a, l, c, concurrent);
    a.bind_text_label(claim);
    emit_claim(a, c, concurrent);
}

/// With a valid mark: count a copied object the mark did not reach (an
/// invariant violation; it is still copied). V0 = source payload address
/// (preserved).
fn unmarked_check<E: PortableAsm>(a: &mut E, counter: usize) {
    let checked = a.create_text_label();
    a.load_ptr_disp32(R::V2, R::V3, d(RT_MARK_VALID));
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, checked);
    a.mov_reg_reg(R::V2, R::V0);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.sub_reg_reg(R::V2, R::V1);
    a.mov_reg_reg(R::V1, R::V2);
    a.shr_reg_imm8(R::V1, 4);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(R::V6, 1);
    a.shl_reg_count(R::V6);
    a.shr_reg_imm8(R::V2, 10);
    a.shl_reg_imm8(R::V2, 3);
    a.add_reg_reg(R::V2, R::V3);
    a.load_ptr_disp32(R::V7, R::V2, L.mbits as i32);
    a.test_reg_reg(R::V7, R::V6);
    a.jcc_label(C::NotEqual, checked);
    bump(a, counter, R::V2);
    a.bind_text_label(checked);
}

/// Mutator Young copy through its TLAB; leaves the copy's bits in `new`.
fn mutator_young_copy<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    c: &Copier<E::TextLabel>,
) {
    if l.worker.is_some() {
        // Before alloc_raw publishes the slot: see `scan::copy_in_flight`.
        a.mov_imm64(R::V1, 1);
        a.store_ptr_disp32(R::V3, d(RT_M_COPYING), R::V1);
    }
    a.load_ptr_disp32(R::V1, R::V0, PAYLOAD);
    a.store_ptr_disp32(R::V3, d(RT_ALLOC_SIZE), R::V1);
    a.call_label(l.alloc_raw);
    a.mov_reg_reg(R::V7, R::V0);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    a.load_ptr_disp32(R::V6, R::V3, d(c.src));
    a.load_ptr_disp32(R::V1, R::V3, d(RT_ALLOC_SIZE));
    a.rep_movsb();
    // The copy has survived once.
    a.pop_reg(R::V1);
    a.pop_reg(R::V1);
    a.load_ptr_disp32(R::V1, R::V1, MARK_RECORD);
    a.mov_imm64(R::V2, 1);
    a.store_ptr_disp32(R::V1, AGE, R::V2);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_PENDING_REF));
    a.store_ptr_disp32(R::V3, d(c.new), R::V0);
}

/// Worker Young copy: bump-allocate in the worker's private to-space page
/// (taking a new page from the top of the semispace when it is full),
/// write a published header and the payload, descriptor last, then advance
/// the page's `allocated_top`. Leaves the copy's bits in `new`.
fn worker_young_copy<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    c: &Copier<E::TextLabel>,
) {
    let fit = a.create_text_label();
    let reserve = a.create_text_label();
    let retry = a.create_text_label();
    let have = a.create_text_label();
    a.bind_text_label(fit);
    a.load_ptr_disp32(R::V0, R::V3, d(c.src));
    a.load_ptr_disp32(R::V1, R::V0, PAYLOAD);
    footprint(a, R::V2, R::V1);
    a.load_ptr_disp32(R::V6, R::V3, d(RT_W_TOP));
    a.mov_reg_reg(R::V7, R::V6);
    a.add_reg_reg(R::V7, R::V2);
    a.load_ptr_disp32(R::V9, R::V3, d(RT_W_PAGE));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, reserve);
    a.load_ptr_disp32(R::V8, R::V3, d(RT_W_LIMIT));
    a.cmp_reg_reg(R::V8, R::V7);
    a.jcc_label(C::AboveOrEqual, have);
    // Take the highest free page: copy_low - 1, if it is above next_page.
    a.bind_text_label(reserve);
    a.bind_text_label(retry);
    a.load_ptr_disp32(R::V0, R::V3, d(RT_BOUNDS));
    a.mov_reg_reg(R::V1, R::V0);
    a.shl_reg_imm8(R::V1, 32);
    a.shr_reg_imm8(R::V1, 32);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 32);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::AboveOrEqual, l.oom);
    a.mov_reg_reg(R::V8, R::V0);
    a.mov_reg_reg(R::V6, R::V0);
    a.mov_imm64(R::V9, 1 << 32);
    a.sub_reg_reg(R::V6, R::V9);
    a.mov_reg_reg(R::V7, R::V3);
    a.add_reg_imm32(R::V7, d(RT_BOUNDS));
    a.atomic_compare_exchange(R::V7, R::V6);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, retry);
    a.sub_reg_imm8(R::V2, 1);
    a.store_ptr_disp32(R::V3, d(RT_COPY_LOW), R::V2);
    super::scan::page_ptr(a, R::V8, R::V2, R::V1);
    a.store_ptr_disp32(R::V3, d(RT_W_PAGE), R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, start)));
    a.store_ptr_disp32(R::V3, d(RT_W_TOP), R::V0);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, limit)));
    a.store_ptr_disp32(R::V3, d(RT_W_LIMIT), R::V0);
    a.jmp_label(fit);
    // V6 = header offset, V7 = end offset, V1 = payload bytes.
    a.bind_text_label(have);
    a.store_ptr_disp32(R::V3, d(RT_W_TOP), R::V7);
    a.store_ptr_disp32(R::V3, d(c.new), R::V6);
    a.load_ptr_disp32(R::V8, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V8, R::V6);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, payload_bytes)), R::V1);
    a.mov_reg_reg(R::V2, R::V3);
    a.add_reg_imm32(R::V2, d(RT_SURVIVOR));
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, mark_record)), R::V2);
    a.mov_imm64(R::V2, OBJECT_PUBLISHED);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, state)), R::V2);
    a.mov_reg_reg(R::V7, R::V8);
    a.add_reg_imm32(R::V7, HEADER);
    a.load_ptr_disp32(R::V6, R::V3, d(c.src));
    a.rep_movsb();
    a.load_ptr_disp32(R::V6, R::V3, d(c.new));
    a.load_ptr_disp32(R::V8, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V8, R::V6);
    a.load_ptr_disp32(R::V2, R::V3, d(c.desc));
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, descriptor_id)), R::V2);
    a.load_ptr_disp32(R::V9, R::V3, d(RT_W_PAGE));
    a.load_ptr_disp32(R::V0, R::V3, d(RT_W_TOP));
    a.add_reg_imm32(R::V9, d(offset_of!(AllocationPage, allocated_top)));
    a.atomic_store_release(R::V9, R::V0);
    a.add_reg_imm32(R::V6, HEADER);
    a.shl_reg_imm8(R::V6, 16);
    a.mov_imm64(R::V2, color());
    a.or_reg_reg(R::V6, R::V2);
    a.store_ptr_disp32(R::V3, d(c.new), R::V6);
}

/// Promotion: reserve Old space (CAS with a worker), write a published
/// header and the payload, descriptor last. V0 = source payload address.
fn promote_copy<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    c: &Copier<E::TextLabel>,
    concurrent: bool,
) {
    a.load_ptr_disp32(R::V1, R::V0, PAYLOAD);
    footprint(a, R::V2, R::V1);
    let retry = a.create_text_label();
    a.mov_reg_reg(R::V8, R::V3);
    a.add_reg_imm32(R::V8, d(RT_OLD_TOP));
    a.bind_text_label(retry);
    a.load_ptr_disp32(R::V6, R::V3, d(RT_OLD_TOP));
    a.mov_reg_reg(R::V7, R::V6);
    a.add_reg_reg(R::V7, R::V2);
    a.load_ptr_disp32(R::V9, R::V3, d(RT_OLD_BASE));
    a.add_reg_imm32(R::V9, OLD_HALF as i32);
    a.cmp_reg_reg(R::V7, R::V9);
    a.jcc_label(C::Above, l.old_full);
    if concurrent {
        a.mov_reg_reg(R::V0, R::V6);
        a.atomic_compare_exchange(R::V8, R::V7);
        a.cmp_reg_reg(R::V0, R::V6);
        a.jcc_label(C::NotEqual, retry);
    } else {
        a.store_ptr_disp32(R::V3, d(RT_OLD_TOP), R::V7);
    }
    a.load_ptr_disp32(R::V8, R::V3, d(c.promoted));
    a.add_reg_reg(R::V8, R::V2);
    a.store_ptr_disp32(R::V3, d(c.promoted), R::V8);
    // V8 = header address, V6 = header offset.
    a.load_ptr_disp32(R::V8, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V8, R::V6);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, payload_bytes)), R::V1);
    a.mov_imm64(R::V2, OBJECT_PUBLISHED);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, state)), R::V2);
    a.add_reg_imm32(R::V6, HEADER);
    a.shl_reg_imm8(R::V6, 16);
    a.mov_imm64(R::V2, color());
    a.or_reg_reg(R::V6, R::V2);
    a.store_ptr_disp32(R::V3, d(c.new), R::V6);
    a.mov_reg_reg(R::V7, R::V8);
    a.add_reg_imm32(R::V7, HEADER);
    a.load_ptr_disp32(R::V6, R::V3, d(c.src));
    a.rep_movsb();
    a.load_ptr_disp32(R::V7, R::V3, d(c.new));
    a.shr_reg_imm8(R::V7, 16);
    a.load_ptr_disp32(R::V1, R::V3, d(RT_HEAP_BASE));
    a.add_reg_reg(R::V7, R::V1);
    a.load_ptr_disp32(R::V2, R::V3, d(c.desc));
    a.store_ptr_disp32(R::V7, DESCRIPTOR, R::V2);
}

/// Claim the source for the copy in `new` (CAS with a worker, a plain
/// forwarding store without). A lost race turns the copy into a filler and
/// answers the winner's bits.
fn emit_claim<E: PortableAsm>(a: &mut E, c: &Copier<E::TextLabel>, concurrent: bool) {
    let lost = concurrent.then(|| a.create_text_label());
    a.load_ptr_disp32(R::V6, R::V3, d(c.new));
    a.and_reg_imm32(R::V6, -0x10000);
    a.mov_imm64(R::V1, FWD16 as u64);
    a.or_reg_reg(R::V6, R::V1);
    a.load_ptr_disp32(R::V7, R::V3, d(c.src));
    a.add_reg_imm32(R::V7, DESCRIPTOR);
    if concurrent {
        a.load_ptr_disp32(R::V0, R::V3, d(c.desc));
        a.atomic_compare_exchange(R::V7, R::V6);
        a.load_ptr_disp32(R::V1, R::V3, d(c.desc));
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, lost.expect("concurrent claim"));
    } else {
        a.store_ptr_disp32(R::V7, 0, R::V6);
    }
    bump(a, c.copies, R::V1);
    copy_done(a, c, concurrent);
    a.load_ptr_disp32(R::V0, R::V3, d(c.new));
    a.ret();
    if let Some(lost) = lost {
        // V0 = the winner's forwarding word.
        a.bind_text_label(lost);
        a.mov_reg_reg(R::V2, R::V0);
        a.load_ptr_disp32(R::V1, R::V3, d(c.new));
        a.shr_reg_imm8(R::V1, 16);
        a.load_ptr_disp32(R::V6, R::V3, d(RT_HEAP_BASE));
        a.add_reg_reg(R::V1, R::V6);
        a.mov_imm64(R::V6, 1);
        a.store_ptr_disp32(R::V1, DESCRIPTOR, R::V6);
        bump(a, c.lost, R::V1);
        copy_done(a, c, concurrent);
        a.mov_reg_reg(R::V0, R::V2);
        forward_target(a, R::V0, R::V1);
        a.ret();
    }
}

/// The mutator's copy (or filler) is complete: the CAS above ordered it.
fn copy_done<E: PortableAsm>(a: &mut E, c: &Copier<E::TextLabel>, concurrent: bool) {
    if concurrent && !c.worker {
        a.mov_imm64(R::V1, 0);
        a.store_ptr_disp32(R::V3, d(RT_M_COPYING), R::V1);
    }
}
