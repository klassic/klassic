//! Strict THP backing: a 2-MiB physical transition covers sixteen GC pages.
//! Never decommit a group containing an owned, moving, dirty or too-young page.
use super::*;
use crate::gc_codegen::production::{load_rt, store_rt};
const PAGES: u64 = 16;
const BYTES: u64 = PAGES * HEAP_PAGE_BYTES;
fn validate<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.cmp_reg_imm8(R::V10, 1);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm32(R::V10, geometry(a).pages as i32);
    a.jcc_label(C::Above, l.fail);
}
fn align<E: PortableAsm>(a: &mut E) {
    a.sub_reg_imm32(R::V10, 1);
    a.and_reg_imm32(R::V10, -(PAGES as i32));
    a.add_reg_imm32(R::V10, 1);
}
fn group_address<E: PortableAsm>(a: &mut E) {
    address(a);
    a.mov_imm64(R::V6, BYTES);
}
fn bump<E: PortableAsm>(a: &mut E, field: usize) {
    load_rt(a, R::V0, field);
    a.add_reg_imm32(R::V0, PAGES as i32);
    store_rt(a, field, R::V0);
}
fn tags<E: PortableAsm>(a: &mut E, value: u64) {
    page_tag(a);
    a.mov_imm64(R::V6, PAGES);
    a.mov_imm64(R::V0, value);
    let next = a.create_text_label();
    a.bind_text_label(next);
    a.atomic_store_release(R::V9, R::V0);
    a.add_reg_imm32(R::V9, 8);
    a.sub_reg_imm32(R::V6, 1);
    a.jcc_label(C::NotEqual, next);
}
fn reset_age<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V9, R::V10);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, layout(a).free_since as i32);
    a.mov_imm64(R::V6, PAGES);
    a.mov_imm64(R::V0, 0);
    let next = a.create_text_label();
    a.bind_text_label(next);
    a.store_ptr_disp32(R::V9, 0, R::V0);
    a.add_reg_imm32(R::V9, 8);
    a.sub_reg_imm32(R::V6, 1);
    a.jcc_label(C::NotEqual, next);
}
/// Caller holds the commit lock; V10 is one GC page, V6 loop count survives.
fn releasable<E: PortableAsm>(a: &mut E, skip: E::TextLabel) {
    for table in [layout(a).generations, layout(a).old_sources] {
        a.mov_reg_reg(R::V9, R::V10);
        a.shl_reg_imm8(R::V9, 3);
        a.add_reg_reg(R::V9, R::V3);
        a.add_reg_imm32(R::V9, table as i32);
        a.atomic_load_acquire(R::V0, R::V9);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, skip);
    }
    a.mov_reg_reg(R::V0, R::V10);
    a.sub_reg_imm32(R::V0, 1);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, state)));
    a.cmp_reg_imm8(R::V0, PAGE_READY as i8);
    a.jcc_label(C::NotEqual, skip);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, owner)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, skip);
    a.load_ptr_disp32(R::V8, R::V8, d(offset_of!(AllocationPage, forward_table)));
    a.load_ptr_disp32(
        R::V0,
        R::V8,
        d(offset_of!(crate::gc_abi::ForwardTable, used)),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, skip);
    page_tag(a);
    a.atomic_load_acquire(R::V0, R::V9);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, skip);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.ensure_heap_page);
    save(a);
    validate(a, l);
    lock(a);
    // The requested page must have a payload owner; neighboring free pages
    // merely become accessible and remain unavailable for allocation.
    a.mov_reg_reg(R::V9, R::V10);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, layout(a).generations as i32);
    a.atomic_load_acquire(R::V0, R::V9);
    let owned = a.create_text_label();
    a.cmp_reg_imm8(R::V0, generation::YOUNG as i8);
    a.jcc_label(C::Equal, owned);
    a.cmp_reg_imm8(R::V0, generation::OLD as i8);
    a.jcc_label(C::Equal, owned);
    a.cmp_reg_imm8(R::V0, generation::LARGE as i8);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(owned);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(
        R::V9,
        (layout(a).free_since - layout(a).generations) as i32,
        R::V0,
    );
    align(a);
    page_tag(a);
    a.atomic_load_acquire(R::V11, R::V9);
    let ready = a.create_text_label();
    a.cmp_reg_imm8(R::V11, 1);
    a.jcc_label(C::Equal, ready);
    a.cmp_reg_imm8(R::V11, 2);
    a.jcc_label(C::Above, l.fail);
    // A mixed group would allow recommit to overwrite accessible live data.
    a.mov_imm64(R::V6, PAGES);
    let consistent = a.create_text_label();
    a.bind_text_label(consistent);
    a.atomic_load_acquire(R::V0, R::V9);
    a.cmp_reg_reg(R::V0, R::V11);
    a.jcc_label(C::NotEqual, l.fail);
    a.add_reg_imm32(R::V9, 8);
    a.sub_reg_imm32(R::V6, 1);
    a.jcc_label(C::NotEqual, consistent);
    a.push_reg(R::V10);
    a.push_reg(R::V11);
    let normal = a.create_text_label();
    let checked = a.create_text_label();
    load_rt(a, R::V0, FAIL_AFTER);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, normal);
    a.sub_reg_imm32(R::V0, 1);
    store_rt(a, FAIL_AFTER, R::V0);
    a.jcc_label(C::NotEqual, normal);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(checked);
    a.bind_text_label(normal);
    group_address(a);
    a.call_label(l.memory.commit);
    a.bind_text_label(checked);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.large_pages_failed);
    a.pop_reg(R::V11);
    a.pop_reg(R::V10);
    if a.gc_pre_touch() {
        group_address(a);
        pretouch::emit(a);
    }
    reset_age(a);
    tags(a, 1);
    let first = a.create_text_label();
    a.cmp_reg_imm8(R::V11, 2);
    a.jcc_label(C::NotEqual, first);
    bump(a, RECOMMITS);
    a.bind_text_label(first);
    bump(a, COMMITS);
    load_rt(a, R::V0, COMMITTED);
    a.add_reg_imm32(R::V0, BYTES as i32);
    store_rt(a, COMMITTED, R::V0);
    load_rt(a, R::V1, PEAK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, ready);
    store_rt(a, PEAK, R::V0);
    a.bind_text_label(ready);
    unlock(a);
    restore(a);

    a.bind_text_label(l.uncommit_heap_page);
    save(a);
    validate(a, l);
    align(a);
    lock(a);
    let skip = a.create_text_label();
    let next = a.create_text_label();
    a.mov_imm64(R::V6, PAGES);
    a.bind_text_label(next);
    releasable(a, skip);
    a.add_reg_imm32(R::V10, 1);
    a.sub_reg_imm32(R::V6, 1);
    a.jcc_label(C::NotEqual, next);
    a.sub_reg_imm32(R::V10, PAGES as i32);
    load_rt(a, R::V0, COMMITTED);
    a.sub_reg_imm32(R::V0, BYTES as i32);
    load_rt(a, R::V1, MINIMUM);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, skip);
    a.push_reg(R::V10);
    a.push_reg(R::V10);
    group_address(a);
    a.call_label(l.memory.decommit);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.heap_decommit_failed);
    a.pop_reg(R::V10);
    a.pop_reg(R::V10);
    tags(a, 2);
    load_rt(a, R::V0, COMMITTED);
    a.sub_reg_imm32(R::V0, BYTES as i32);
    store_rt(a, COMMITTED, R::V0);
    bump(a, UNCOMMITS);
    unlock(a);
    a.mov_imm64(R::V0, 1);
    restore(a);
    a.bind_text_label(skip);
    unlock(a);
    a.mov_imm64(R::V0, 0);
    restore(a);
}

/// Idle policy must wait for every member's Free age, not just the candidate.
/// V11=now and V10=candidate, as in uncommit_poll; preserve all inputs.
pub(in super::super) fn check_idle_age<E: PortableAsm>(a: &mut E, advance: E::TextLabel) {
    let saved = [R::V0, R::V1, R::V6, R::V7, R::V8, R::V9];
    for r in saved {
        a.push_reg(r);
    }
    let blocked = a.create_text_label();
    let done = a.create_text_label();
    let next = a.create_text_label();
    a.mov_reg_reg(R::V7, R::V10);
    a.sub_reg_imm32(R::V7, 1);
    a.and_reg_imm32(R::V7, -(PAGES as i32));
    a.add_reg_imm32(R::V7, 1);
    a.shl_reg_imm8(R::V7, 3);
    a.add_reg_reg(R::V7, R::V3);
    a.add_reg_imm32(R::V7, layout(a).free_since as i32);
    a.mov_imm64(R::V6, PAGES);
    a.mov_imm64(R::V9, a.gc_uncommit_delay_ns());
    a.bind_text_label(next);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, blocked);
    a.cmp_reg_reg(R::V11, R::V0);
    a.jcc_label(C::Below, blocked);
    a.mov_reg_reg(R::V1, R::V11);
    a.sub_reg_reg(R::V1, R::V0);
    a.cmp_reg_reg(R::V1, R::V9);
    a.jcc_label(C::Below, blocked);
    a.add_reg_imm32(R::V7, 8);
    a.sub_reg_imm32(R::V6, 1);
    a.jcc_label(C::NotEqual, next);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
    a.jmp_label(done);
    a.bind_text_label(blocked);
    for r in saved.into_iter().rev() {
        a.pop_reg(r);
    }
    a.jmp_label(advance);
    a.bind_text_label(done);
}
