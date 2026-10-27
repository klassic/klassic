//! Bounded retirement of unreachable dedicated Old spans, after remap
//! quiescence. A page becomes Free only after payload/slot metadata cleanup
//! and checked OS release of its private identity backing.
use super::large_object as o;
use super::*;
use crate::gc_codegen::production::{load_rt, store_rt};

fn address<E: PortableAsm>(a: &mut E, base: u64, shift: u8, subtract_page: bool) {
    load_rt(a, R::V1, o::SWEEP_PAGE);
    if subtract_page {
        a.sub_reg_imm8(R::V1, 1);
    }
    a.shl_reg_imm8(R::V1, shift);
    a.add_reg_reg(R::V1, R::V3);
    a.add_reg_imm32(R::V1, base as i32);
}
/// Clear at most one word. State stores an index, never a raw metadata slot.
fn zero_range<E: PortableAsm>(a: &mut E, words: u64, next: u64, loop_to: E::TextLabel) {
    load_rt(a, R::V2, o::SWEEP_CURSOR);
    let complete = a.create_text_label();
    a.cmp_reg_imm32(R::V2, words as i32);
    a.jcc_label(C::AboveOrEqual, complete);
    a.mov_reg_reg(R::V7, R::V2);
    a.shl_reg_imm8(R::V7, 3);
    a.add_reg_reg(R::V1, R::V7);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V1, 0, R::V0);
    copy::bump(a, o::SWEEP_CURSOR, R::V0);
    a.jmp_label(loop_to);
    a.bind_text_label(complete);
    store_rt_imm(a, o::SWEEP_CURSOR, 0);
    store_rt_imm(a, o::SWEEP_STAGE, next);
    a.jmp_label(loop_to);
}

/// Collector lock held. Each descriptor, clear word and Free publication
/// consumes a work unit. Metadata OS release runs after dropping that lock.
pub(super) fn step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, paused: E::TextLabel) {
    let top = a.create_text_label();
    let done = a.create_text_label();
    let observed = a.create_text_label();
    let choose = a.create_text_label();
    let stages: [_; 6] = std::array::from_fn(|_| a.create_text_label());
    a.bind_text_label(top);
    load_rt(a, R::V0, o::SWEEP_PAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm32(R::V0, geometry(a).pages as i32);
    a.jcc_label(C::Above, observed);
    super::scan::spend(a, paused);
    load_rt(a, R::V1, o::SWEEP_STAGE);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, choose);
    for (i, label) in stages.iter().enumerate() {
        a.cmp_reg_imm8(R::V1, (i + 1) as i8);
        a.jcc_label(C::Equal, *label);
    }
    a.jmp_label(l.fail);
    a.bind_text_label(choose);
    o::record(a, R::V0);
    a.load_ptr_disp32(R::V1, R::V8, 0);
    let skip = a.create_text_label();
    let have = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, have);
    a.mov_imm64(R::V1, 1);
    a.jmp_label(skip);
    a.bind_text_label(have);
    // A pin destination is a real unpublished allocation reservation. Its
    // native owner, not an invented live epoch, excludes it from sweeping.
    a.load_ptr_disp32(R::V2, R::V8, pin_destination::RESERVATION);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, skip);
    a.load_ptr_disp32(R::V2, R::V8, 8);
    load_rt(a, R::V7, o::EPOCH);
    a.cmp_reg_reg(R::V2, R::V7);
    a.jcc_label(C::Equal, skip);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, o::SWEEP_END, R::V0);
    a.mov_imm64(R::V1, 0);
    for off in [0, 8, 16] {
        a.store_ptr_disp32(R::V8, off, R::V1);
    }
    store_rt_imm(a, o::SWEEP_STAGE, 1);
    store_rt_imm(a, o::SWEEP_CURSOR, 0);
    a.jmp_label(top);
    a.bind_text_label(skip);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, o::SWEEP_PAGE, R::V0);
    a.jmp_label(top);

    a.bind_text_label(stages[0]);
    load_rt(a, R::V1, o::SWEEP_PAGE);
    a.shl_reg_imm8(R::V1, 17);
    load_rt(a, R::V2, RT_HEAP_BASE);
    a.add_reg_reg(R::V1, R::V2);
    zero_range(a, HEAP_PAGE_BYTES / 8, 2, top);
    a.bind_text_label(stages[1]);
    address(a, layout(a).starts, 10, true); // 128 start words / physical page
    zero_range(a, STARTS_WORDS, 3, top);
    for (i, label) in [stages[2], stages[3]].into_iter().enumerate() {
        a.bind_text_label(label);
        address(
            a,
            layout(a).rbits + i as u64 * geometry(a).rbits_words * 8,
            11,
            false,
        );
        zero_range(a, HEAP_PAGE_BYTES / 512, 4 + i as u64, top);
    }
    a.bind_text_label(stages[4]);
    address(a, layout(a).owner_cards, 10, false);
    zero_range(a, HEAP_PAGE_BYTES / super::slot_owner::CARD_BYTES, 6, top);
    a.bind_text_label(stages[5]);
    address(a, layout(a).generations, 3, false);
    super::metadata::retire_page(a, l, paused);
    // Reuse the ordinary Old-page incarnation/ownership handoff. These pages
    // never joined an Old source bitmap or a Young forwarding table.
    a.mov_imm64(R::V0, 16);
    super::pool::release(a, l);
    a.mov_imm64(R::V2, 0);
    a.atomic_store_release(R::V1, R::V2);
    load_rt(a, R::V0, o::LIVE_PAGES);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, o::LIVE_PAGES, R::V0);
    copy::bump(a, o::RECLAIMED_PAGES, R::V0);
    copy::bump(a, o::SWEEP_PAGE, R::V0);
    load_rt(a, R::V1, o::SWEEP_END);
    a.cmp_reg_reg(R::V0, R::V1);
    let more = a.create_text_label();
    a.jcc_label(C::Below, more);
    store_rt_imm(a, o::SWEEP_STAGE, 0);
    a.jmp_label(top);
    a.bind_text_label(more);
    store_rt_imm(a, o::SWEEP_STAGE, 1);
    a.jmp_label(top);
    a.bind_text_label(observed);
    // Only a completed Full sweep establishes the retained Large baseline.
    // Young retirement has SWEEP_PAGE=0 and must not absorb later allocations.
    load_rt(a, R::V0, o::LIVE_PAGES);
    store_rt(a, o::RETAINED_PAGES, R::V0);
    a.bind_text_label(done);
    store_rt_imm(a, o::SWEEP_PAGE, 0);
}
