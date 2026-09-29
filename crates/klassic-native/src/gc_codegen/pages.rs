//! Heap-lock protected page reservation transaction; see gc_pages.h.
use crate::gc_abi::{AllocationPage as P, HeapPool as H, MAX_OFFSET, PageLease as L};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

const OP: i32 = 8;
const BYTES: i32 = 16;
const META: i32 = 24;
const USED: i32 = 32;
const FIRST: i32 = 40;
const END: i32 = 48;
const ID: i32 = 56;
const INC: i32 = 64;
const LIMIT: i32 = 72;

fn h<E: PortableAsm>(a: &mut E, dst: R, field: usize) {
    a.load_ptr_disp32(dst, R::V7, field as i32);
}
fn l<E: PortableAsm>(a: &mut E, dst: R, field: usize) {
    a.load_ptr_disp32(dst, R::V6, field as i32);
}
fn page<E: PortableAsm>(a: &mut E, index: R) {
    a.mov_reg_reg(R::V0, index);
    a.mov_imm64(R::V1, size_of::<P>() as u64);
    a.mul_unsigned_wide(R::V1);
    h(a, R::V1, offset_of!(H, pages));
    a.add_reg_reg(R::V0, R::V1);
    a.mov_reg_reg(R::V8, R::V0);
}
fn state_address<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V11, R::V8);
    a.add_reg_imm32(R::V11, offset_of!(P, state) as i32);
}
fn adjust<E: PortableAsm>(a: &mut E, field: usize, slot: i32, subtract: bool) {
    h(a, R::V0, field);
    a.load_rbp_slot(R::V1, slot);
    if subtract {
        a.sub_reg_reg(R::V0, R::V1);
    } else {
        a.add_reg_reg(R::V0, R::V1);
    }
    a.store_ptr_disp32(R::V7, field as i32, R::V0);
}
fn pressure<E: PortableAsm>(a: &mut E) {
    h(a, R::V0, offset_of!(H, committed));
    h(a, R::V1, offset_of!(H, pending));
    a.add_reg_reg(R::V0, R::V1);
    h(a, R::V1, offset_of!(H, soft_max));
    a.cmp_reg_reg(R::V0, R::V1);
    let high = a.create_text_label();
    let store = a.create_text_label();
    a.jcc_label(C::Above, high);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(store);
    a.bind_text_label(high);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(store);
    a.store_ptr_disp32(R::V7, offset_of!(H, pressure) as i32, R::V0);
}

/// `(V7=pool, V6=unique lease, V2=reserve/commit/abort 0..2) -> status 0..5`.
/// All storage/OS preparation preconditions match C; caller holds the heap lock.
#[allow(dead_code)]
pub(crate) fn emit_pool_step_locked<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let heap_limit = a.create_text_label();
    let metadata_limit = a.create_text_label();
    let fragmented = a.create_text_label();
    let exhausted = a.create_text_label();
    let success = a.create_text_label();
    let done = a.create_text_label();
    let settle = a.create_text_label();
    a.push_reg(R::V5);
    a.mov_reg_reg(R::V5, R::V4);
    a.sub_reg_imm8(R::V4, 80);
    a.store_rbp_slot(OP, R::V2);
    a.cmp_reg_imm32(R::V2, 2);
    a.jcc_label(C::Above, invalid);
    for field in [
        offset_of!(H, pages),
        offset_of!(H, page_count),
        offset_of!(H, hard_max),
    ] {
        h(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, invalid);
    }
    h(a, R::V0, offset_of!(H, page_count));
    a.mov_imm64(R::V1, MAX_OFFSET >> 17);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    h(a, R::V1, offset_of!(H, heap_base));
    a.mov_reg_reg(R::V2, R::V1);
    a.and_reg_imm32(R::V2, 15);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, invalid);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    h(a, R::V1, offset_of!(H, page_count));
    a.shl_reg_imm8(R::V1, 17);
    h(a, R::V0, offset_of!(H, hard_max));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    for field in [
        offset_of!(H, soft_max),
        offset_of!(H, relocation_reserve),
        offset_of!(H, committed),
    ] {
        h(a, R::V1, field);
        a.cmp_reg_reg(R::V1, R::V0);
        a.jcc_label(C::Above, invalid);
    }
    // V1 = committed; subtract before adding to avoid counter overflow.
    a.sub_reg_reg(R::V0, R::V1);
    h(a, R::V2, offset_of!(H, pending));
    a.cmp_reg_reg(R::V2, R::V0);
    a.jcc_label(C::Above, invalid);
    a.add_reg_reg(R::V1, R::V2);
    a.store_rbp_slot(USED, R::V1);
    h(a, R::V0, offset_of!(H, page_count));
    a.mov_imm64(R::V1, size_of::<P>() as u64);
    a.mul_unsigned_wide(R::V1);
    h(a, R::V1, offset_of!(H, metadata_committed));
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, invalid);
    h(a, R::V0, offset_of!(H, metadata_max));
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, invalid);
    a.sub_reg_reg(R::V0, R::V1);
    h(a, R::V1, offset_of!(H, metadata_pending));
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, invalid);
    l(a, R::V0, offset_of!(L, page_count));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    h(a, R::V1, offset_of!(H, page_count));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, invalid);
    a.shl_reg_imm8(R::V0, 17);
    a.store_rbp_slot(BYTES, R::V0);
    a.shr_reg_imm8(R::V0, 7);
    a.store_rbp_slot(META, R::V0);
    for field in [
        offset_of!(L, generation),
        offset_of!(L, relocation),
        offset_of!(L, committed_mapping),
    ] {
        l(a, R::V0, field);
        a.cmp_reg_imm32(R::V0, 1);
        a.jcc_label(C::Above, invalid);
    }
    a.load_rbp_slot(R::V0, OP);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, settle);
    for field in [
        offset_of!(L, stage),
        offset_of!(L, committed_mapping),
        offset_of!(L, starts),
        offset_of!(L, head_index),
        offset_of!(L, incarnation),
        offset_of!(L, identity_base),
    ] {
        l(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, invalid);
    }
    h(a, R::V0, offset_of!(H, hard_max));
    l(a, R::V1, offset_of!(L, relocation));
    let worker = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, worker);
    h(a, R::V1, offset_of!(H, relocation_reserve));
    a.sub_reg_reg(R::V0, R::V1);
    a.bind_text_label(worker);
    a.load_rbp_slot(R::V1, USED);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, heap_limit);
    a.sub_reg_reg(R::V0, R::V1);
    a.load_rbp_slot(R::V1, BYTES);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, heap_limit);
    h(a, R::V0, offset_of!(H, metadata_max));
    for field in [
        offset_of!(H, metadata_committed),
        offset_of!(H, metadata_pending),
    ] {
        h(a, R::V1, field);
        a.sub_reg_reg(R::V0, R::V1);
    }
    a.load_rbp_slot(R::V1, META);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, metadata_limit);
    a.load_rbp_slot(R::V1, BYTES);
    a.shr_reg_imm8(R::V1, 4);
    h(a, R::V0, offset_of!(H, last_identity));
    a.add_reg_reg(R::V1, R::V0);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, exhausted);
    a.add_reg_imm32(R::V0, 1);
    a.store_rbp_slot(ID, R::V0);
    h(a, R::V0, offset_of!(H, last_incarnation));
    a.cmp_reg_imm32(R::V0, -1);
    a.jcc_label(C::Equal, exhausted);
    a.add_reg_imm32(R::V0, 1);
    a.store_rbp_slot(INC, R::V0);
    let search = a.create_text_label();
    let occupied = a.create_text_label();
    let counted = a.create_text_label();
    let found = a.create_text_label();
    a.mov_imm64(R::V9, 0); // consecutive free slots
    a.mov_imm64(R::V10, 0); // slot index
    a.bind_text_label(search);
    page(a, R::V10);
    state_address(a);
    a.atomic_load_acquire(R::V0, R::V11);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, metadata_leases) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    a.add_reg_imm32(R::V9, 1);
    a.jmp_label(counted);
    a.bind_text_label(occupied);
    a.mov_imm64(R::V9, 0);
    a.bind_text_label(counted);
    l(a, R::V0, offset_of!(L, page_count));
    a.cmp_reg_reg(R::V9, R::V0);
    a.jcc_label(C::Equal, found);
    a.add_reg_imm32(R::V10, 1);
    h(a, R::V0, offset_of!(H, page_count));
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Below, search);
    a.jmp_label(fragmented);
    a.bind_text_label(found);
    a.add_reg_imm32(R::V10, 1);
    a.store_rbp_slot(END, R::V10);
    a.sub_reg_reg(R::V10, R::V9);
    a.store_rbp_slot(FIRST, R::V10);
    let initialize = a.create_text_label();
    a.bind_text_label(initialize);
    page(a, R::V10);
    h(a, R::V0, offset_of!(H, heap_base));
    a.store_ptr_disp32(R::V8, offset_of!(P, heap_base) as i32, R::V0);
    a.mov_reg_reg(R::V0, R::V10);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    a.store_ptr_disp32(R::V8, offset_of!(P, start) as i32, R::V0);
    a.mov_reg_reg(R::V11, R::V8);
    a.add_reg_imm32(R::V11, offset_of!(P, allocated_top) as i32);
    a.atomic_store_release(R::V11, R::V0);
    a.load_rbp_slot(R::V0, END);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    a.store_ptr_disp32(R::V8, offset_of!(P, limit) as i32, R::V0);
    for (slot, field) in [
        (INC, offset_of!(P, incarnation)),
        (ID, offset_of!(P, identity_base)),
        (FIRST, offset_of!(P, run_head)),
    ] {
        a.load_rbp_slot(R::V0, slot);
        a.store_ptr_disp32(R::V8, field as i32, R::V0);
    }
    l(a, R::V0, offset_of!(L, generation));
    a.store_ptr_disp32(R::V8, offset_of!(P, generation) as i32, R::V0);
    a.mov_imm64(R::V0, 0);
    for field in [
        offset_of!(P, owner),
        offset_of!(P, starts),
        offset_of!(P, starts_words),
        offset_of!(P, forward_table),
    ] {
        a.store_ptr_disp32(R::V8, field as i32, R::V0);
    }
    let tail = a.create_text_label();
    let publish = a.create_text_label();
    a.load_rbp_slot(R::V0, FIRST);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::NotEqual, tail);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(publish);
    a.bind_text_label(tail);
    a.mov_imm64(R::V0, 4);
    a.bind_text_label(publish);
    state_address(a);
    a.atomic_store_release(R::V11, R::V0);
    a.add_reg_imm32(R::V10, 1);
    a.load_rbp_slot(R::V0, END);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Below, initialize);
    adjust(a, offset_of!(H, pending), BYTES, false);
    adjust(a, offset_of!(H, metadata_pending), META, false);
    h(a, R::V0, offset_of!(H, last_identity));
    a.load_rbp_slot(R::V1, BYTES);
    a.shr_reg_imm8(R::V1, 4);
    a.add_reg_reg(R::V0, R::V1);
    a.store_ptr_disp32(R::V7, offset_of!(H, last_identity) as i32, R::V0);
    a.load_rbp_slot(R::V0, INC);
    a.store_ptr_disp32(R::V7, offset_of!(H, last_incarnation) as i32, R::V0);
    for (slot, field) in [
        (FIRST, offset_of!(L, head_index)),
        (INC, offset_of!(L, incarnation)),
        (ID, offset_of!(L, identity_base)),
    ] {
        a.load_rbp_slot(R::V0, slot);
        a.store_ptr_disp32(R::V6, field as i32, R::V0);
    }
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V6, offset_of!(L, stage) as i32, R::V0);
    a.jmp_label(success);

    a.bind_text_label(settle);
    l(a, R::V0, offset_of!(L, stage));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, invalid);
    h(a, R::V0, offset_of!(H, page_count));
    l(a, R::V1, offset_of!(L, page_count));
    a.sub_reg_reg(R::V0, R::V1);
    l(a, R::V10, offset_of!(L, head_index));
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Above, invalid);
    a.store_rbp_slot(FIRST, R::V10);
    a.add_reg_reg(R::V1, R::V10);
    a.store_rbp_slot(END, R::V1);
    a.add_reg_imm32(R::V1, 1);
    a.shl_reg_imm8(R::V1, 17);
    a.store_rbp_slot(LIMIT, R::V1);
    for (field, slot) in [
        (offset_of!(H, pending), BYTES),
        (offset_of!(H, metadata_pending), META),
    ] {
        h(a, R::V0, field);
        a.load_rbp_slot(R::V1, slot);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Below, invalid);
    }
    l(a, R::V0, offset_of!(L, starts));
    a.load_rbp_slot(R::V1, OP);
    a.cmp_reg_imm32(R::V1, 1);
    let abort_check = a.create_text_label();
    let check_range = a.create_text_label();
    a.jcc_label(C::NotEqual, abort_check);
    l(a, R::V1, offset_of!(L, committed_mapping));
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.jmp_label(check_range);
    a.bind_text_label(abort_check);
    l(a, R::V1, offset_of!(L, committed_mapping));
    a.or_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    a.bind_text_label(check_range);
    let verify_page = a.create_text_label();
    a.bind_text_label(verify_page);
    page(a, R::V10);
    h(a, R::V0, offset_of!(H, heap_base));
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(P, heap_base) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    for (field, lease_field) in [
        (offset_of!(P, run_head), offset_of!(L, head_index)),
        (offset_of!(P, incarnation), offset_of!(L, incarnation)),
        (offset_of!(P, identity_base), offset_of!(L, identity_base)),
        (offset_of!(P, generation), offset_of!(L, generation)),
    ] {
        a.load_ptr_disp32(R::V0, R::V8, field as i32);
        l(a, R::V1, lease_field);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.mov_reg_reg(R::V0, R::V10);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(P, start) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_rbp_slot(R::V0, LIMIT);
    a.load_ptr_disp32(R::V1, R::V8, offset_of!(P, limit) as i32);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    let check_tail = a.create_text_label();
    let compare_state = a.create_text_label();
    a.load_rbp_slot(R::V0, FIRST);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::NotEqual, check_tail);
    a.mov_imm64(R::V1, 2);
    a.jmp_label(compare_state);
    a.bind_text_label(check_tail);
    a.mov_imm64(R::V1, 4);
    a.bind_text_label(compare_state);
    a.load_ptr_disp32(R::V0, R::V8, offset_of!(P, metadata_leases) as i32);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    state_address(a);
    a.atomic_load_acquire(R::V0, R::V11);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.add_reg_imm32(R::V10, 1);
    a.load_rbp_slot(R::V0, END);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Below, verify_page);
    // No further errors after this point; the transaction changes ownership.
    adjust(a, offset_of!(H, pending), BYTES, true);
    adjust(a, offset_of!(H, metadata_pending), META, true);
    a.load_rbp_slot(R::V0, OP);
    a.cmp_reg_imm32(R::V0, 1);
    let abort = a.create_text_label();
    a.jcc_label(C::NotEqual, abort);
    a.load_rbp_slot(R::V10, FIRST);
    page(a, R::V10);
    l(a, R::V0, offset_of!(L, starts));
    a.store_ptr_disp32(R::V8, offset_of!(P, starts) as i32, R::V0);
    a.load_rbp_slot(R::V0, META);
    a.shr_reg_imm8(R::V0, 3);
    a.store_ptr_disp32(R::V8, offset_of!(P, starts_words) as i32, R::V0);
    adjust(a, offset_of!(H, committed), BYTES, false);
    adjust(a, offset_of!(H, metadata_committed), META, false);
    state_address(a);
    a.mov_imm64(R::V0, 3);
    a.atomic_store_release(R::V11, R::V0);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V6, offset_of!(L, starts) as i32, R::V0);
    a.store_ptr_disp32(R::V6, offset_of!(L, committed_mapping) as i32, R::V0);
    a.mov_imm64(R::V0, 2);
    a.store_ptr_disp32(R::V6, offset_of!(L, stage) as i32, R::V0);
    a.jmp_label(success);
    a.bind_text_label(abort);
    a.load_rbp_slot(R::V10, FIRST);
    let release_page = a.create_text_label();
    a.bind_text_label(release_page);
    page(a, R::V10);
    state_address(a);
    a.mov_imm64(R::V0, 0);
    a.atomic_store_release(R::V11, R::V0);
    a.add_reg_imm32(R::V10, 1);
    a.load_rbp_slot(R::V0, END);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Below, release_page);
    a.mov_imm64(R::V0, 3);
    a.store_ptr_disp32(R::V6, offset_of!(L, stage) as i32, R::V0);
    a.bind_text_label(success);
    pressure(a);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    for (label, status) in [
        (invalid, 1),
        (heap_limit, 2),
        (metadata_limit, 3),
        (fragmented, 4),
        (exhausted, 5),
    ] {
        a.bind_text_label(label);
        a.mov_imm64(R::V0, status);
        a.jmp_label(done);
    }
    a.bind_text_label(done);
    a.leave();
}
