//! Contiguous runs from one physical page pool. Selection is owner-only at
//! startup or under the collector lock, after prior retirement has completed.
//! Source roles remain occupied until metadata cleanup and sole-owner remap
//! quiescence. These rules do not implement general N-owner lifetime leases.
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};
#[cfg(test)]
pub(super) const PAGES: u64 = generation::ENTRIES - 1;
#[repr(C)]
pub(super) struct State {
    old_pages: u64,
    reduced_old_runs: u64,
    bank: u64,
    from_bank: u64,
    claims: u64,
    released: u64,
    transfers: u64,
}
pub(super) const OLD_PAGES: usize = offset_of!(Runtime, pool.old_pages);
pub(super) const REDUCED_OLD_RUNS: usize = offset_of!(Runtime, pool.reduced_old_runs);
pub(super) const BANK: usize = offset_of!(Runtime, pool.bank);
pub(super) const FROM_BANK: usize = offset_of!(Runtime, pool.from_bank);
pub(super) const CLAIMS: usize = offset_of!(Runtime, pool.claims);
pub(super) const RELEASED: usize = offset_of!(Runtime, pool.released);
pub(super) const TRANSFERS: usize = offset_of!(Runtime, pool.transfers);

/// V7 = page count, V6 = generation -> V0 = physical zero-based first page.
/// Contiguous free entries coalesce implicitly. No existing role is overwritten.
/// A free directory entry is insufficient if page metadata is not Ready.
pub(super) fn claim<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let exhausted = a.create_text_label();
    let done = a.create_text_label();
    claim_or(a, l, exhausted);
    a.jmp_label(done);
    a.bind_text_label(exhausted);
    a.cmp_reg_imm8(R::V6, generation::OLD as i8);
    a.jcc_label(C::Equal, l.old_full);
    a.jmp_label(l.oom);
    a.bind_text_label(done);
}
/// Owner holds the collector lock; a failed search leaves all roles untouched.
pub(super) fn claim_or<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, failure: E::TextLabel) {
    let exhausted = a.create_text_label();
    let search = a.create_text_label();
    let occupied = a.create_text_label();
    let found = a.create_text_label();
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, l.fail);
    a.cmp_reg_imm32(R::V7, super::geometry(a).pages as i32);
    a.jcc_label(C::Above, l.oom);
    let kind = a.create_text_label();
    a.cmp_reg_imm8(R::V6, generation::YOUNG as i8);
    a.jcc_label(C::Equal, kind);
    a.cmp_reg_imm8(R::V6, generation::OLD as i8);
    a.jcc_label(C::Equal, kind);
    a.cmp_reg_imm8(R::V6, generation::LARGE as i8);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(kind);
    a.mov_imm64(R::V8, 0);
    a.mov_imm64(R::V2, 0);
    lea_rt(a, R::V9, (super::layout(a).generations + 8) as usize);
    a.bind_text_label(search);
    a.cmp_reg_imm32(R::V8, super::geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, exhausted);
    a.atomic_load_acquire(R::V10, R::V9);
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::NotEqual, occupied);
    let started = a.create_text_label();
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, started);
    a.mov_reg_reg(R::V1, R::V8);
    a.bind_text_label(started);
    a.add_reg_imm32(R::V2, 1);
    a.cmp_reg_reg(R::V2, R::V7);
    a.jcc_label(C::Equal, found);
    let advance = a.create_text_label();
    a.jmp_label(advance);
    a.bind_text_label(occupied);
    a.mov_imm64(R::V2, 0);
    a.bind_text_label(advance);
    a.add_reg_imm32(R::V8, 1);
    a.add_reg_imm32(R::V9, 8);
    a.jmp_label(search);
    a.bind_text_label(found);
    publish(a, l);
    let done = a.create_text_label();
    a.jmp_label(done);
    a.bind_text_label(exhausted);
    a.jmp_label(failure);
    a.bind_text_label(done);
}
/// The full cycle takes its Young run first, then uses the largest remaining
/// Old run up to the per-cycle cap. Return its actual length in V7.
#[cfg(test)]
pub(super) fn largest_old<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.mov_imm64(R::V7, super::geometry(a).old_bytes / HEAP_PAGE_BYTES);
    largest_old_limit(a, l, false);
}
pub(super) fn largest_old_limit<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    allow_empty: bool,
) {
    let search = a.create_text_label();
    let occupied = a.create_text_label();
    let advance = a.create_text_label();
    let found = a.create_text_label();
    a.mov_imm64(R::V8, 0);
    a.mov_imm64(R::V2, 0);
    a.mov_imm64(R::V11, 0);
    lea_rt(a, R::V9, (super::layout(a).generations + 8) as usize);
    a.bind_text_label(search);
    a.cmp_reg_imm32(R::V8, super::geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, found);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    let started = a.create_text_label();
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, started);
    a.mov_reg_reg(R::V10, R::V8);
    a.bind_text_label(started);
    a.add_reg_imm32(R::V2, 1);
    a.cmp_reg_reg(R::V2, R::V11);
    a.jcc_label(C::Below, advance);
    a.jcc_label(C::Equal, advance);
    a.mov_reg_reg(R::V11, R::V2);
    a.mov_reg_reg(R::V1, R::V10);
    a.cmp_reg_reg(R::V2, R::V7);
    a.jcc_label(C::Equal, found);
    a.jmp_label(advance);
    a.bind_text_label(occupied);
    a.mov_imm64(R::V2, 0);
    a.bind_text_label(advance);
    a.add_reg_imm32(R::V8, 1);
    a.add_reg_imm32(R::V9, 8);
    a.jmp_label(search);
    a.bind_text_label(found);
    let empty = a.create_text_label();
    let completed = a.create_text_label();
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, if allow_empty { empty } else { l.old_full });
    a.mov_reg_reg(R::V7, R::V11);
    let maximal = a.create_text_label();
    a.cmp_reg_imm32(
        R::V7,
        (super::geometry(a).old_bytes / HEAP_PAGE_BYTES) as i32,
    );
    a.jcc_label(C::AboveOrEqual, maximal);
    copy::bump(a, REDUCED_OLD_RUNS, R::V0);
    a.bind_text_label(maximal);
    a.mov_reg_reg(R::V2, R::V11);
    a.mov_imm64(R::V6, generation::OLD);
    publish(a, l);
    a.jmp_label(completed);
    a.bind_text_label(empty);
    a.mov_imm64(R::V7, 0);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(completed);
}
/// Remaining Full Old runs, in physical order after the first largest run.
/// WALK is a private cursor for this quiescent claim operation. Every directory
/// entry is inspected at most once across these calls; publish still validates
/// Ready metadata before assigning each page. V7=limit -> V0=base, V7=length;
/// zero length means exhausted. Ordinary moving objects fit within one page,
/// so every nonempty page-aligned run can serve them; Large spans do not move.
pub(super) fn next_old_limit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V8, old_runs::WALK);
    a.mov_imm64(R::V11, 0);
    a.mov_reg_reg(R::V9, R::V8);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, (layout(a).generations + 8) as i32);
    let next = a.create_text_label();
    let occupied = a.create_text_label();
    let found = a.create_text_label();
    let empty = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(next);
    a.cmp_reg_imm32(R::V8, geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, found);
    a.atomic_load_acquire(R::V0, R::V9);
    a.add_reg_imm32(R::V8, 1);
    a.add_reg_imm32(R::V9, 8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    let started = a.create_text_label();
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::NotEqual, started);
    a.mov_reg_reg(R::V1, R::V8);
    a.sub_reg_imm8(R::V1, 1);
    a.bind_text_label(started);
    a.add_reg_imm32(R::V11, 1);
    a.cmp_reg_reg(R::V11, R::V7);
    a.jcc_label(C::AboveOrEqual, found);
    a.jmp_label(next);
    a.bind_text_label(occupied);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, next);
    a.bind_text_label(found);
    store_rt(a, old_runs::WALK, R::V8);
    a.mov_reg_reg(R::V7, R::V11);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, empty);
    let maximal = a.create_text_label();
    a.cmp_reg_imm32(R::V7, (geometry(a).old_bytes / HEAP_PAGE_BYTES) as i32);
    a.jcc_label(C::AboveOrEqual, maximal);
    copy::bump(a, REDUCED_OLD_RUNS, R::V0);
    a.bind_text_label(maximal);
    a.mov_reg_reg(R::V2, R::V7);
    a.mov_imm64(R::V6, generation::OLD);
    publish(a, l);
    a.jmp_label(done);
    a.bind_text_label(empty);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
}
pub(super) fn publish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::page_runs::claim(a, l);
    let publish = a.create_text_label();
    a.mov_reg_reg(R::V9, R::V1);
    a.shl_reg_imm8(R::V9, 3);
    a.add_reg_reg(R::V9, R::V3);
    a.add_reg_imm32(R::V9, (super::layout(a).generations + 8) as i32);
    a.mov_reg_reg(R::V8, R::V9);
    a.add_reg_imm32(
        R::V8,
        (super::layout(a).pool_history - super::layout(a).generations) as i32,
    );
    a.bind_text_label(publish);
    // All allocation-page metadata, including previously Old pages, exists.
    a.mov_reg_reg(R::V0, R::V9);
    a.sub_reg_reg(R::V0, R::V3);
    a.sub_reg_imm32(R::V0, (super::layout(a).generations + 8) as i32);
    a.shr_reg_imm8(R::V0, 3);
    scan::page_ptr(a, R::V10, R::V0, R::V11);
    a.load_ptr_disp32(R::V11, R::V10, d(offset_of!(AllocationPage, state)));
    a.cmp_reg_imm8(R::V11, PAGE_READY as i8);
    a.jcc_label(C::NotEqual, l.fail);
    // The public AllocationPage ABI uses Young=0/Old=1. Dedicated Large
    // pages are Old too; the directory retains its distinct physical role.
    a.mov_imm64(R::V11, 0);
    let young = a.create_text_label();
    a.cmp_reg_imm8(R::V6, generation::YOUNG as i8);
    a.jcc_label(C::Equal, young);
    a.mov_imm64(R::V11, 1);
    a.bind_text_label(young);
    a.store_ptr_disp32(R::V10, d(offset_of!(AllocationPage, generation)), R::V11);
    // Shared run tails are metadata aliases, never independently owned heads.
    a.load_ptr_disp32(R::V11, R::V10, d(offset_of!(AllocationPage, run_head)));
    a.cmp_reg_reg(R::V11, R::V0);
    let head = a.create_text_label();
    a.jcc_label(C::Equal, head);
    a.mov_reg_reg(R::V11, R::V10);
    a.add_reg_imm32(R::V11, d(offset_of!(AllocationPage, state)));
    a.mov_imm64(R::V0, 4);
    a.atomic_store_release(R::V11, R::V0);
    a.bind_text_label(head);
    a.load_ptr_disp32(R::V11, R::V10, d(offset_of!(AllocationPage, owner)));
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V10, R::V10, d(offset_of!(AllocationPage, forward_table)));
    a.load_ptr_disp32(
        R::V11,
        R::V10,
        d(offset_of!(crate::gc_abi::ForwardTable, used)),
    );
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V10, R::V8, 0);
    let same = a.create_text_label();
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::Equal, same);
    a.cmp_reg_reg(R::V10, R::V6);
    a.jcc_label(C::Equal, same);
    copy::bump(a, TRANSFERS, R::V11);
    a.bind_text_label(same);
    a.store_ptr_disp32(R::V8, 0, R::V6);
    // A reserved-but-never-touched incarnation also restarts idle age.
    a.mov_reg_reg(R::V10, R::V9);
    a.add_reg_imm32(
        R::V10,
        (layout(a).free_since - layout(a).generations) as i32,
    );
    a.mov_imm64(R::V11, 0);
    a.store_ptr_disp32(R::V10, 0, R::V11);
    a.atomic_store_release(R::V9, R::V6);
    a.add_reg_imm32(R::V8, 8);
    a.add_reg_imm32(R::V9, 8);
    a.sub_reg_imm8(R::V2, 1);
    a.jcc_label(C::NotEqual, publish);
    copy::bump(a, CLAIMS, R::V11);
    a.mov_reg_reg(R::V0, R::V1);
}
/// Normal Old promotion uses the uncapped mature-Young bound. The Young window
/// cap constrains its own destination only, not the two Old copying actors
/// and their run tails. Unsealed cycles bound prior Young copy winners, checked against claimed pages.
pub(super) fn must_full<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, full: E::TextLabel) {
    load_rt(a, R::V0, OLD_PAGES);
    a.shl_reg_imm8(R::V0, 17);
    old_runs::used(a, R::V1);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, full);
    a.mov_reg_reg(R::V2, R::V0);
    let ready = a.create_text_label();
    capacity::mature_promotion_pages(a, l);
    a.shl_reg_imm8(R::V0, 17);
    a.cmp_reg_reg(R::V2, R::V0);
    a.jcc_label(C::AboveOrEqual, ready);
    a.jmp_label(full);
    a.bind_text_label(ready);
    young_run_requires_full(a, l, full);
    super::shared_headroom::must_full(a, l, full);
}
/// A retained Old reservation can split the free pool even without Large
/// objects. Decide Full before a normal Young flip changes roles: Full can
/// return unused Old tails and fit a fresh window, whereas Young must retain
/// its entire protected-copy floor. Owner holds the collector lock.
fn young_run_requires_full<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    full: E::TextLabel,
) {
    capacity::young_destination(a, l);
    // V2 is the required contiguous destination. Stop at the first fitting run.
    a.mov_imm64(R::V8, 0);
    a.mov_imm64(R::V10, 0);
    lea_rt(a, R::V9, (layout(a).generations + 8) as usize);
    let next = a.create_text_label();
    let occupied = a.create_text_label();
    let advance = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(next);
    a.cmp_reg_imm32(R::V8, geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, full);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    a.add_reg_imm32(R::V10, 1);
    a.cmp_reg_reg(R::V10, R::V2);
    a.jcc_label(C::AboveOrEqual, done);
    a.jmp_label(advance);
    a.bind_text_label(occupied);
    a.mov_imm64(R::V10, 0);
    a.bind_text_label(advance);
    a.add_reg_imm32(R::V8, 1);
    a.add_reg_imm32(R::V9, 8);
    a.jmp_label(next);
    a.bind_text_label(done);
}
/// Called by retirement before clearing one role word. Old pages never used
/// the Young TLAB table in this incarnation, but still advance incarnation
/// before they can be assigned to Young. Preserve stage/cursor/end.
pub(super) fn release<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    copy::bump(a, RELEASED, R::V9);
    let done = a.create_text_label();
    a.cmp_reg_imm8(R::V0, 16);
    a.jcc_label(C::NotEqual, done);
    for r in [R::V0, R::V1, R::V2, R::V8] {
        a.push_reg(r);
    }
    a.mov_reg_reg(R::V0, R::V1);
    a.sub_reg_reg(R::V0, R::V3);
    a.sub_reg_imm32(R::V0, (super::layout(a).generations + 8) as i32);
    a.shr_reg_imm8(R::V0, 3);
    a.mov_reg_reg(R::V8, R::V0);
    a.add_reg_imm32(R::V8, 1);
    a.shl_reg_imm8(R::V8, 3);
    a.add_reg_reg(R::V8, R::V3);
    a.add_reg_imm32(R::V8, super::layout(a).old_sources as i32);
    a.mov_imm64(R::V2, 0);
    a.atomic_store_release(R::V8, R::V2);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, metadata_leases)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V1, R::V8);
    a.add_reg_imm32(R::V1, d(offset_of!(AllocationPage, state)));
    a.mov_imm64(R::V0, PAGE_READY);
    a.atomic_store_release(R::V1, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.add_reg_imm32(R::V0, super::geometry(a).pages as i32);
    a.jcc_label(C::Below, l.fail);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, incarnation)), R::V0);
    for r in [R::V8, R::V2, R::V1, R::V0] {
        a.pop_reg(r);
    }
    a.bind_text_label(done);
}
