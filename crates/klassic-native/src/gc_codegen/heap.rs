//! Owned, prezeroed Young TLAB allocation. Matching contract: gc_heap.h.
//! No calls, polling, locking, payload walk or fall-back allocation.
use crate::gc_abi::{
    ABI_VERSION, AllocationPage as P, AllocationRequest as Q, ForwardCell, ForwardTable,
    MAX_OFFSET, MarkArena, MarkRecord, MutatorContext as M, OBJECT_INITIALIZING, ObjectHeader as H,
    PAGE_ALLOCATING, TypeDescriptor as D,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

/// `(V7=ctx, V6=page, V2=bind/close 0/1) -> OK/invalid 0/2`, heap lock held.
#[allow(dead_code)]
pub(crate) fn emit_tlab_page_locked<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let close = a.create_text_label();
    let publish = a.create_text_label();
    let done = a.create_text_label();
    a.cmp_reg_imm32(R::V2, 1);
    a.jcc_label(C::Above, invalid);
    a.atomic_load_acquire_u32(R::V0, R::V7);
    a.cmp_reg_imm32(R::V0, ABI_VERSION as i32);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V8, R::V7, offset_of!(M, thread_id) as i32);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V11, R::V7);
    a.add_reg_imm32(R::V11, offset_of!(M, state) as i32);
    a.atomic_load_acquire_u32(R::V0, R::V11);
    let owner_state_ok = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, owner_state_ok);
    a.cmp_reg_imm32(R::V2, 1);
    a.jcc_label(C::NotEqual, invalid);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(owner_state_ok);
    acquire(a, R::V0, R::V7, offset_of!(M, access_epoch));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(P, generation) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    for field in [
        offset_of!(P, incarnation),
        offset_of!(P, identity_base),
        offset_of!(P, starts),
        offset_of!(P, start),
    ] {
        a.load_ptr_disp32(R::V0, R::V6, field as i32);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, invalid);
    }
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(P, starts) as i32);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V9, R::V6, offset_of!(P, start) as i32);
    a.load_ptr_disp32(R::V10, R::V6, offset_of!(P, limit) as i32);
    a.cmp_reg_reg(R::V9, R::V10);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.mov_imm64(R::V0, MAX_OFFSET);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Above, invalid);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(P, heap_base) as i32);
    a.mov_reg_reg(R::V1, R::V0);
    a.or_reg_reg(R::V1, R::V9);
    a.or_reg_reg(R::V1, R::V10);
    a.and_reg_imm32(R::V1, 15);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_reg_reg(R::V1, R::V10);
    a.add_reg_reg(R::V1, R::V0);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V0, R::V10);
    a.sub_reg_reg(R::V0, R::V9); // span
    a.mov_reg_reg(R::V1, R::V0);
    a.add_reg_imm32(R::V1, 1023);
    a.shr_reg_imm8(R::V1, 10);
    a.load_ptr_disp32(R::V11, R::V6, offset_of!(P, starts_words) as i32);
    a.cmp_reg_reg(R::V11, R::V1);
    a.jcc_label(C::Below, invalid);
    a.shr_reg_imm8(R::V0, 4);
    a.sub_reg_imm8(R::V0, 1);
    a.load_ptr_disp32(R::V1, R::V6, offset_of!(P, identity_base) as i32);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(P, heap_base) as i32);
    a.add_reg_reg(R::V9, R::V0); // absolute start/end
    a.add_reg_reg(R::V10, R::V0);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, close);
    for field in [
        offset_of!(M, tlab_start),
        offset_of!(M, tlab_top),
        offset_of!(M, tlab_end),
    ] {
        a.load_ptr_disp32(R::V0, R::V7, field as i32);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(P, owner) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    acquire(a, R::V0, R::V6, offset_of!(P, state));
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::NotEqual, invalid);
    acquire(a, R::V0, R::V6, offset_of!(P, allocated_top));
    a.load_ptr_disp32(R::V1, R::V6, offset_of!(P, start) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.store_ptr_disp32(R::V6, offset_of!(P, owner) as i32, R::V8);
    a.store_ptr_disp32(R::V7, offset_of!(M, tlab_start) as i32, R::V9);
    a.store_ptr_disp32(R::V7, offset_of!(M, tlab_top) as i32, R::V9);
    a.store_ptr_disp32(R::V7, offset_of!(M, tlab_end) as i32, R::V10);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(publish);
    a.bind_text_label(close);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(P, owner) as i32);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, invalid);
    for (reg, field) in [
        (R::V9, offset_of!(M, tlab_start)),
        (R::V10, offset_of!(M, tlab_end)),
    ] {
        a.load_ptr_disp32(R::V0, R::V7, field as i32);
        a.cmp_reg_reg(R::V0, reg);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.load_ptr_disp32(R::V8, R::V7, offset_of!(M, tlab_top) as i32);
    a.cmp_reg_reg(R::V8, R::V9);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_reg(R::V8, R::V10);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V0, R::V8);
    a.and_reg_imm32(R::V0, 15);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    acquire(a, R::V0, R::V6, offset_of!(P, state));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(P, heap_base) as i32);
    a.sub_reg_reg(R::V8, R::V0);
    acquire(a, R::V0, R::V6, offset_of!(P, allocated_top));
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V6, offset_of!(P, owner) as i32, R::V0);
    for field in [
        offset_of!(M, tlab_start),
        offset_of!(M, tlab_top),
        offset_of!(M, tlab_end),
    ] {
        a.store_ptr_disp32(R::V7, field as i32, R::V0);
    }
    a.mov_imm64(R::V0, 5);
    a.bind_text_label(publish);
    a.mov_reg_reg(R::V11, R::V6);
    a.add_reg_imm32(R::V11, offset_of!(P, state) as i32);
    a.atomic_store_release(R::V11, R::V0);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
}

fn acquire<E: PortableAsm>(a: &mut E, dst: R, base: R, offset: usize) {
    a.mov_reg_reg(R::V11, base);
    a.add_reg_imm32(R::V11, offset as i32);
    a.atomic_load_acquire(dst, R::V11);
}

/// `(V7=ctx, V6=request) -> V0=OK/refill/invalid/overflow/metadata-refill (0..4)`.
/// Same no-safepoint result handoff as C. Preserves frame/nonvolatile registers.
#[allow(dead_code)]
pub(crate) fn emit_try_tlab_allocate<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let overflow = a.create_text_label();
    let metadata_refill = a.create_text_label();
    let refill = a.create_text_label();
    let done = a.create_text_label();
    // Frame slots: payload 16, total 24, color 32, user offset 40, id 48,
    // bitmap address 56, bitmap value 64, new top offset 72, descriptor id 80.
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 112);
    a.sub_reg_imm8(R::V4, 32);
    a.atomic_load_acquire_u32(R::V0, R::V7);
    a.cmp_reg_imm32(R::V0, ABI_VERSION as i32);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V7, offset_of!(M, thread_id) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V11, R::V7);
    a.add_reg_imm32(R::V11, offset_of!(M, state) as i32);
    a.atomic_load_acquire_u32(R::V0, R::V11);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    acquire(a, R::V0, R::V7, offset_of!(M, access_epoch));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V9, R::V6, offset_of!(Q, descriptor) as i32);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V1, R::V9);
    a.and_reg_imm32(R::V1, 7);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(Q, output) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, 7);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V9, offset_of!(D, id) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.mov_imm64(R::V1, u32::MAX as u64);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    a.store_rbp_slot(80, R::V0);
    a.load_ptr_disp32(R::V0, R::V9, offset_of!(D, alignment) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_imm32(R::V0, 16);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V1, R::V0);
    a.sub_reg_imm8(R::V1, 1);
    a.test_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V6, offset_of!(Q, elements) as i32);
    a.load_ptr_disp32(R::V1, R::V9, offset_of!(D, element_bytes) as i32);
    let sized = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, sized);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(sized);
    a.mul_unsigned_wide(R::V1);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, overflow);
    a.mov_reg_reg(R::V2, R::V0);
    a.load_ptr_disp32(R::V1, R::V9, offset_of!(D, fixed_bytes) as i32);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, overflow);
    a.store_rbp_slot(16, R::V0);
    a.mov_reg_reg(R::V2, R::V0);
    a.add_reg_imm32(R::V0, 15);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, overflow);
    a.and_reg_imm32(R::V0, -16);
    let rounded = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, rounded);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(rounded);
    a.mov_reg_reg(R::V2, R::V0);
    a.add_reg_imm32(R::V0, size_of::<H>() as i32);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, overflow);
    a.store_rbp_slot(24, R::V0);
    acquire(a, R::V10, R::V7, offset_of!(M, adopted_phase));
    a.mov_reg_reg(R::V0, R::V10);
    a.and_reg_imm32(R::V0, 0xe088); // reserved and invalid phase nibble high bits
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_imm64(R::V2, 0);
    for (shift, mask, base) in [(12, 1, 4), (8, 1, 8), (9, 1, 10), (10, 3, 12)] {
        a.mov_reg_reg(R::V1, R::V10);
        a.shr_reg_imm8(R::V1, shift);
        a.and_reg_imm32(R::V1, mask);
        a.add_reg_imm32(R::V1, base);
        a.mov_imm64(R::V0, 1);
        a.shl_reg_count(R::V0);
        a.or_reg_reg(R::V2, R::V0);
    }
    a.store_rbp_slot(32, R::V2);
    a.load_ptr_disp32(R::V8, R::V6, offset_of!(Q, page) as i32);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, refill);
    a.mov_reg_reg(R::V1, R::V8);
    a.and_reg_imm32(R::V1, 7);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    acquire(a, R::V0, R::V8, offset_of!(P, state));
    a.cmp_reg_imm32(R::V0, PAGE_ALLOCATING as i32);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, generation) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, owner) as i32);
    a.load_ptr_disp32(R::V1, R::V7, offset_of!(M, thread_id) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    for offset in [
        offset_of!(P, incarnation),
        offset_of!(P, identity_base),
        offset_of!(P, starts),
        offset_of!(P, start),
    ] {
        a.load_ptr_disp32(R::V0, R::V8, offset as i32);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, invalid);
    }
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, starts) as i32);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    // V9 = heap base, V10 = current absolute top.
    a.load_ptr_disp32(R::V9, R::V8, offset_of!(P, heap_base) as i32);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, start) as i32);
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(P, limit) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.mov_imm64(R::V2, MAX_OFFSET);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V2, R::V0);
    a.or_reg_reg(R::V2, R::V1);
    a.or_reg_reg(R::V2, R::V9);
    a.and_reg_imm32(R::V2, 15);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, invalid);
    a.add_reg_reg(R::V1, R::V9);
    a.cmp_reg_reg(R::V1, R::V9);
    a.jcc_label(C::Below, invalid);
    a.add_reg_reg(R::V0, R::V9);
    for (reg, field) in [
        (R::V0, offset_of!(M, tlab_start)),
        (R::V1, offset_of!(M, tlab_end)),
    ] {
        a.load_ptr_disp32(R::V2, R::V7, field as i32);
        a.cmp_reg_reg(reg, R::V2);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.load_ptr_disp32(R::V10, R::V7, offset_of!(M, tlab_top) as i32);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_reg(R::V10, R::V1);
    a.jcc_label(C::Above, invalid);
    a.mov_reg_reg(R::V2, R::V10);
    a.and_reg_imm32(R::V2, 15);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, invalid);
    a.mov_reg_reg(R::V2, R::V10);
    a.sub_reg_reg(R::V2, R::V9); // current offset
    acquire(a, R::V0, R::V8, offset_of!(P, allocated_top));
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::NotEqual, invalid);
    a.sub_reg_reg(R::V1, R::V10); // available
    a.load_rbp_slot(R::V0, 24);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, refill);
    a.add_reg_reg(R::V0, R::V2);
    a.store_rbp_slot(72, R::V0);
    a.add_reg_imm32(R::V2, size_of::<H>() as i32);
    a.store_rbp_slot(40, R::V2);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, start) as i32);
    a.sub_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 4); // payload granule
    a.mov_reg_reg(R::V1, R::V2);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(R::V11, 1);
    a.shl_reg_count(R::V11);
    a.mov_reg_reg(R::V1, R::V2);
    a.shr_reg_imm8(R::V1, 6);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, starts_words) as i32);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, identity_base) as i32);
    a.add_reg_reg(R::V2, R::V0);
    a.cmp_reg_reg(R::V2, R::V0);
    a.jcc_label(C::Below, overflow);
    a.store_rbp_slot(48, R::V2);
    a.shl_reg_imm8(R::V1, 3);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, starts) as i32);
    a.add_reg_reg(R::V1, R::V0);
    a.store_rbp_slot(56, R::V1);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V11);
    a.jcc_label(C::NotEqual, invalid);
    a.or_reg_reg(R::V0, R::V11);
    a.store_rbp_slot(64, R::V0);
    a.load_ptr_disp32(R::V11, R::V6, offset_of!(Q, marks) as i32);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, metadata_refill);
    a.mov_reg_reg(R::V0, R::V11);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.store_rbp_slot(88, R::V11);
    a.load_ptr_disp32(R::V0, R::V11, offset_of!(MarkArena, records) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V11, offset_of!(MarkArena, capacity) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.mov_imm64(R::V1, u64::MAX / size_of::<MarkRecord>() as u64);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    a.mov_imm64(R::V1, size_of::<MarkRecord>() as u64);
    a.mul_unsigned_wide(R::V1);
    a.load_ptr_disp32(R::V1, R::V11, offset_of!(MarkArena, records) as i32);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    a.load_ptr_disp32(R::V0, R::V11, offset_of!(MarkArena, used) as i32);
    a.load_ptr_disp32(R::V1, R::V11, offset_of!(MarkArena, capacity) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    a.jcc_label(C::Equal, metadata_refill);
    a.mov_imm64(R::V1, size_of::<MarkRecord>() as u64);
    a.mul_unsigned_wide(R::V1);
    a.load_ptr_disp32(R::V1, R::V11, offset_of!(MarkArena, records) as i32);
    a.add_reg_reg(R::V0, R::V1);
    a.mov_reg_reg(R::V11, R::V0);
    a.store_rbp_slot(96, R::V11);
    for field in (0..size_of::<MarkRecord>()).step_by(8) {
        a.load_ptr_disp32(R::V0, R::V11, field as i32);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.load_ptr_disp32(R::V11, R::V8, offset_of!(P, forward_table) as i32);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, metadata_refill);
    a.mov_reg_reg(R::V0, R::V11);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.store_rbp_slot(104, R::V11);
    a.load_ptr_disp32(R::V0, R::V11, offset_of!(ForwardTable, cells) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V11, offset_of!(ForwardTable, capacity) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.mov_imm64(R::V1, u32::MAX as u64);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    const {
        assert!(size_of::<ForwardCell>() == 32);
    }
    a.shl_reg_imm8(R::V0, 5);
    a.load_ptr_disp32(R::V1, R::V11, offset_of!(ForwardTable, cells) as i32);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    // acquire() uses V11 as scratch, so reload the table afterwards.
    acquire(a, R::V0, R::V11, offset_of!(ForwardTable, used));
    a.load_rbp_slot(R::V11, 104);
    a.store_rbp_slot(120, R::V0);
    a.load_ptr_disp32(R::V1, R::V11, offset_of!(ForwardTable, capacity) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    a.jcc_label(C::Equal, metadata_refill);
    a.shl_reg_imm8(R::V0, 5);
    a.load_ptr_disp32(R::V1, R::V11, offset_of!(ForwardTable, cells) as i32);
    a.add_reg_reg(R::V0, R::V1);
    a.mov_reg_reg(R::V11, R::V0);
    a.store_rbp_slot(112, R::V11);
    for field in [
        offset_of!(ForwardCell, source_offset),
        offset_of!(ForwardCell, incarnation),
    ] {
        a.load_ptr_disp32(R::V0, R::V11, field as i32);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    for field in [
        offset_of!(ForwardCell, word),
        offset_of!(ForwardCell, access),
    ] {
        a.load_rbp_slot(R::V11, 112);
        acquire(a, R::V0, R::V11, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.load_rbp_slot(R::V11, 112);
    let first_cell = a.create_text_label();
    a.load_rbp_slot(R::V0, 120);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, first_cell);
    a.load_ptr_disp32(R::V0, R::V11, -32);
    a.load_rbp_slot(R::V1, 40);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V11, -24);
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(P, incarnation) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(first_cell);
    // All rejection paths precede these writes.
    a.load_rbp_slot(R::V0, 40);
    a.store_ptr_disp32(R::V11, offset_of!(ForwardCell, source_offset) as i32, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, incarnation) as i32);
    a.store_ptr_disp32(R::V11, offset_of!(ForwardCell, incarnation) as i32, R::V0);
    a.load_rbp_slot(R::V11, 104);
    a.add_reg_imm32(R::V11, offset_of!(ForwardTable, used) as i32);
    a.load_rbp_slot(R::V0, 120);
    a.add_reg_imm32(R::V0, 1);
    a.atomic_store_release(R::V11, R::V0);
    a.load_rbp_slot(R::V11, 96);
    a.load_rbp_slot(R::V0, 48);
    a.store_ptr_disp32(R::V11, offset_of!(MarkRecord, object_id) as i32, R::V0);
    a.load_rbp_slot(R::V0, 88);
    a.load_ptr_disp32(R::V1, R::V0, offset_of!(MarkArena, used) as i32);
    a.add_reg_imm32(R::V1, 1);
    a.store_ptr_disp32(R::V0, offset_of!(MarkArena, used) as i32, R::V1);
    for (slot, field) in [
        (16, offset_of!(H, payload_bytes)),
        (80, offset_of!(H, descriptor_id)),
        (96, offset_of!(H, mark_record)),
    ] {
        a.load_rbp_slot(R::V0, slot);
        a.store_ptr_disp32(R::V10, field as i32, R::V0);
    }
    a.mov_reg_reg(R::V11, R::V10);
    a.add_reg_imm32(R::V11, offset_of!(H, state) as i32);
    a.mov_imm64(R::V0, OBJECT_INITIALIZING);
    a.atomic_store_release(R::V11, R::V0);
    a.load_rbp_slot(R::V1, 56);
    a.load_rbp_slot(R::V0, 64);
    a.atomic_store_release(R::V1, R::V0);
    a.load_rbp_slot(R::V0, 72);
    a.mov_reg_reg(R::V1, R::V0);
    a.add_reg_reg(R::V1, R::V9);
    a.store_ptr_disp32(R::V7, offset_of!(M, tlab_top) as i32, R::V1);
    a.mov_reg_reg(R::V11, R::V8);
    a.add_reg_imm32(R::V11, offset_of!(P, allocated_top) as i32);
    a.atomic_store_release(R::V11, R::V0);
    // Sole-writer statistics publication; saturation never fails allocation.
    let counted = a.create_text_label();
    acquire(a, R::V0, R::V7, offset_of!(M, allocated_bytes));
    a.mov_reg_reg(R::V2, R::V0);
    a.load_rbp_slot(R::V1, 24);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::AboveOrEqual, counted);
    a.mov_imm64(R::V0, u64::MAX);
    a.bind_text_label(counted);
    a.atomic_store_release(R::V11, R::V0);
    a.load_ptr_disp32(R::V11, R::V6, offset_of!(Q, output) as i32);
    a.load_rbp_slot(R::V0, 40);
    a.shl_reg_imm8(R::V0, 16);
    a.load_rbp_slot(R::V1, 32);
    a.or_reg_reg(R::V0, R::V1);
    a.store_ptr_disp32(R::V11, 0, R::V0);
    a.load_rbp_slot(R::V0, 48);
    a.store_ptr_disp32(R::V11, 8, R::V0);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    for (label, status) in [
        (refill, 1),
        (invalid, 2),
        (overflow, 3),
        (metadata_refill, 4),
    ] {
        a.bind_text_label(label);
        a.mov_imm64(R::V0, status);
        a.jmp_label(done);
    }
    a.bind_text_label(done);
    a.leave();
}
