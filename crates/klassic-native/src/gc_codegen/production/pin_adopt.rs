//! Final small-pin release: transfer its published dedicated page into the
//! actual ordinary Old inventory. The caller has ended all raw uses and keeps
//! a real strong root throughout. No copy, incarnation change or allocation.
use super::*;
#[repr(C)]
pub(super) struct State {
    adopted: u64,
}
#[repr(C)]
pub(super) struct Owner {
    raw: u64,
    page: u64,
    footprint: u64,
    current: u64,
    append: u64,
}
const BASE: usize = owner_state::offset(offset_of!(OwnerState, pin_adopt));
const RAW: usize = BASE + offset_of!(Owner, raw);
const PAGE: usize = BASE + offset_of!(Owner, page);
const FOOTPRINT: usize = BASE + offset_of!(Owner, footprint);
const CURRENT: usize = BASE + offset_of!(Owner, current);
const APPEND: usize = BASE + offset_of!(Owner, append);
pub(super) const ADOPTED: usize = offset_of!(Runtime, pin_adoptions) + offset_of!(State, adopted);

/// V0=canonical raw payload, actual collector ticket held -> 0/2busy/3invalid.
/// The private caller guarantees that no live pin/raw use remains on this page.
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.pin_adopt_small);
    store_rt(a, RAW, R::V0);
    let busy = a.create_text_label();
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    for field in [RT_PHASE_OPEN, RT_RETIRE_STAGE] {
        load_rt(a, R::V1, field);
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, busy);
    }
    load_rt(a, R::V2, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_imm32(R::V0, geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.mov_reg_reg(R::V2, R::V0);
    a.and_reg_imm32(R::V2, (HEAP_PAGE_BYTES - 1) as i32);
    a.cmp_reg_imm32(R::V2, copy::HEADER);
    a.jcc_label(C::NotEqual, invalid);
    generation::test_role(a, R::V0, generation::LARGE);
    a.jcc_label(C::NotEqual, invalid);
    a.shr_reg_imm8(R::V0, 17);
    store_rt(a, PAGE, R::V0);
    large_object::record(a, R::V0);
    a.load_ptr_disp32(R::V1, R::V8, 0);
    a.cmp_reg_imm8(R::V1, 1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V8, pin_destination::RESERVATION);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, busy);
    load_rt(a, R::V0, RAW);
    a.load_ptr_disp32(
        R::V1,
        R::V0,
        d(offset_of!(ObjectHeader, state)) - copy::HEADER,
    );
    a.cmp_reg_imm32(R::V1, crate::gc_abi::OBJECT_PUBLISHED as i32);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V0, copy::PAYLOAD);
    a.cmp_reg_imm32(R::V1, MAX_PAYLOAD as i32);
    a.jcc_label(C::Above, invalid);
    a.add_reg_imm32(R::V1, 15);
    a.and_reg_imm32(R::V1, -16);
    let sized = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V1, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V1, copy::HEADER);
    store_rt(a, FOOTPRINT, R::V1);
    load_rt(a, R::V0, old_runs::COUNT);
    a.cmp_reg_imm32(R::V0, geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, l.fail);
    old_runs::lock(a);
    // Preserve the allocation cursor and every future empty reservation. A
    // plain append of an occupied run would be dropped by shrink_unused.
    load_rt(a, R::V0, old_runs::ALLOC);
    old_runs::ptr(a, old_runs::BANK);
    store_rt(a, CURRENT, R::V8);
    load_rt(a, R::V1, RT_OLD_TOP);
    a.store_ptr_disp32(R::V8, old_runs::TOP, R::V1);
    load_rt(a, R::V0, old_runs::COUNT);
    old_runs::ptr(a, old_runs::BANK);
    store_rt(a, APPEND, R::V8);
    load_rt(a, R::V0, old_runs::ALLOC);
    a.add_reg_imm32(R::V0, 1);
    load_rt(a, R::V1, old_runs::COUNT);
    a.cmp_reg_reg(R::V0, R::V1);
    let no_future = a.create_text_label();
    a.jcc_label(C::Equal, no_future);
    a.jcc_label(C::Above, l.fail);
    old_runs::ptr(a, old_runs::BANK);
    a.load_ptr_disp32(R::V0, R::V8, old_runs::BASE);
    a.load_ptr_disp32(R::V1, R::V8, old_runs::TOP);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V9, APPEND);
    for field in [old_runs::BASE, old_runs::END, old_runs::TOP] {
        a.load_ptr_disp32(R::V1, R::V8, field);
        a.store_ptr_disp32(R::V9, field, R::V1);
    }
    a.bind_text_label(no_future);
    load_rt(a, R::V0, old_runs::ALLOC);
    a.add_reg_imm32(R::V0, 1);
    old_runs::ptr(a, old_runs::BANK);
    load_rt(a, R::V9, CURRENT);
    for field in [old_runs::BASE, old_runs::END, old_runs::TOP] {
        a.load_ptr_disp32(R::V1, R::V9, field);
        a.store_ptr_disp32(R::V8, field, R::V1);
    }
    load_rt(a, R::V0, PAGE);
    a.shl_reg_imm8(R::V0, 17);
    a.store_ptr_disp32(R::V9, old_runs::BASE, R::V0);
    a.mov_reg_reg(R::V1, R::V0);
    a.add_reg_imm32(R::V1, HEAP_PAGE_BYTES as i32);
    a.store_ptr_disp32(R::V9, old_runs::END, R::V1);
    load_rt(a, R::V1, FOOTPRINT);
    a.add_reg_reg(R::V1, R::V0);
    a.store_ptr_disp32(R::V9, old_runs::TOP, R::V1);
    let same_first = a.create_text_label();
    load_rt(a, R::V0, old_runs::ALLOC);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, same_first);
    load_rt(a, R::V1, PAGE);
    a.shl_reg_imm8(R::V1, 17);
    store_rt(a, RT_OLD_BASE, R::V1);
    a.bind_text_label(same_first);
    for field in [old_runs::ALLOC, old_runs::COUNT, pool::OLD_PAGES] {
        copy::bump(a, field, R::V0);
    }
    load_rt(a, R::V0, old_runs::PREFIX);
    a.add_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    store_rt(a, old_runs::PREFIX, R::V0);
    load_rt(a, R::V0, PAGE);
    large_object::record(a, R::V0);
    a.mov_imm64(R::V1, 0);
    for field in [0, 8, 16, pin_destination::RESERVATION] {
        a.store_ptr_disp32(R::V8, field, R::V1);
    }
    load_rt(a, R::V0, large_object::LIVE_PAGES);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, large_object::LIVE_PAGES, R::V0);
    load_rt(a, R::V1, PAGE);
    a.shl_reg_imm8(R::V1, 3);
    a.add_reg_reg(R::V1, R::V3);
    a.add_reg_imm32(R::V1, layout(a).generations as i32);
    a.mov_imm64(R::V0, generation::OLD);
    a.atomic_store_release(R::V1, R::V0);
    copy::bump(a, pool::TRANSFERS, R::V0);
    old_runs::unlock(a);
    // Large membership was omitted from the moving-byte bound. If it has
    // already been marked this epoch, admit this real new moving footprint
    // once. White values use the ordinary Old admission after this transfer.
    load_rt(a, R::V0, RT_MARK_ACTIVE);
    load_rt(a, R::V1, RT_MARK_VALID);
    a.or_reg_reg(R::V0, R::V1);
    let untracked = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, untracked);
    load_rt(a, R::V2, RAW);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V2, R::V1);
    mark::test_mark(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::Equal, untracked);
    load_rt(a, R::V0, FOOTPRINT);
    counters::add_capped(a, old_footprint::BLACK, R::V0, geometry(a).heap_bytes);
    a.bind_text_label(untracked);
    copy::bump(a, ADOPTED, R::V0);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(busy);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    a.mov_imm64(R::V1, 0);
    for field in (0..size_of::<Owner>()).step_by(8) {
        store_rt(a, BASE + field, R::V1);
    }
    a.ret();
}
