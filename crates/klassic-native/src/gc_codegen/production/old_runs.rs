//! Old allocation/source run lists. Immutable bounds are prepared before
//! relocation; a short reservation lock publishes tops and run transitions.
//! Payload copies and forwarding claims remain outside that lock.
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};
pub(super) const WORDS: u64 = 3;
pub(super) const BASE: i32 = 0;
pub(super) const END: i32 = 8;
pub(super) const TOP: i32 = 16;
#[repr(C)]
pub(super) struct State {
    switches: u64,
    bank: u64,
    from_bank: u64,
    count: u64,
    from_count: u64,
    allocation: u64,
    scan: u64,
    prefix: u64,
    lock: u64,
    retire: u64,
    young_returned: u64,
    walk: u64,
    temp_base: u64,
    temp_pages: u64,
    remaining: u64,
}
macro_rules! f { ($($n:ident:$v:ident),* $(,)?) => { $(
    pub(super) const $n: usize = offset_of!(Runtime, old_runs.$v);
)* }; }
f! { SWITCHES:switches, BANK:bank, FROM_BANK:from_bank, COUNT:count, FROM_COUNT:from_count,
ALLOC:allocation, SCAN:scan, PREFIX:prefix, LOCK:lock, RETIRE:retire,
YOUNG_RETURNED:young_returned, WALK:walk, TEMP_BASE:temp_base,
TEMP_PAGES:temp_pages, REMAINING:remaining }
/// V0 = run index -> V8 = descriptor. Clobbers V1; preserves other registers.
pub(super) fn ptr<E: PortableAsm>(a: &mut E, bank: usize) {
    a.mov_reg_reg(R::V8, R::V0);
    a.shl_reg_imm8(R::V8, 1);
    a.add_reg_reg(R::V8, R::V0);
    a.shl_reg_imm8(R::V8, 3);
    a.add_reg_reg(R::V8, R::V3);
    a.add_reg_imm32(R::V8, super::layout(a).old_runs as i32);
    load_rt(a, R::V1, bank);
    let first = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, first);
    a.add_reg_imm32(R::V8, super::geometry(a).bank_bytes as i32);
    a.bind_text_label(first);
}
pub(super) fn lock<E: PortableAsm>(a: &mut E) {
    let retry = a.create_text_label();
    a.bind_text_label(retry);
    lea_rt(a, R::V8, LOCK);
    a.mov_imm64(R::V0, 0);
    a.mov_imm64(R::V7, 1);
    a.atomic_compare_exchange(R::V8, R::V7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, retry);
}
pub(super) fn unlock<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V8, LOCK);
    a.mov_imm64(R::V0, 0);
    a.atomic_store_release(R::V8, R::V0);
}
/// Charged Old bytes, including unusable tails of completed runs. Every other
/// register survives, so policy callers do not depend on pointer arithmetic.
pub(super) fn used<E: PortableAsm>(a: &mut E, dst: R) {
    let saved = [
        R::V0,
        R::V1,
        R::V2,
        R::V6,
        R::V7,
        R::V8,
        R::V9,
        R::V10,
        R::V11,
    ];
    // Portable pushes occupy one backend-defined stack slot (16 bytes on
    // AArch64). Exclude the result register instead of discarding eight bytes.
    for r in saved.into_iter().filter(|r| *r != dst) {
        a.push_reg(r);
    }
    lock(a);
    load_rt(a, R::V0, ALLOC);
    ptr(a, BANK);
    a.load_ptr_disp32(R::V6, R::V8, BASE);
    load_rt(a, R::V11, RT_OLD_TOP);
    a.sub_reg_reg(R::V11, R::V6);
    load_rt(a, R::V1, PREFIX);
    a.add_reg_reg(R::V11, R::V1);
    unlock(a);
    a.mov_reg_reg(dst, R::V11);
    for r in saved.into_iter().rev().filter(|r| *r != dst) {
        a.pop_reg(r);
    }
}
pub(super) fn startup<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, COUNT, 1);
    a.mov_imm64(R::V0, 0);
    ptr(a, BANK);
    load_rt(a, R::V6, RT_OLD_BASE);
    a.store_ptr_disp32(R::V8, BASE, R::V6);
    a.store_ptr_disp32(R::V8, TOP, R::V6);
    a.add_reg_imm32(R::V6, super::geometry(a).old_bytes as i32);
    a.store_ptr_disp32(R::V8, END, R::V6);
}
/// Quiescent owner, collector lock held: pages a Full detach can return
/// without touching any populated page. Preserve all registers except dst.
/// The current empty allocation run keeps one page, as shrink_unused does.
pub(super) fn unused_pages<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, dst: R) {
    let saved = [R::V0, R::V1, R::V6, R::V7, R::V8, R::V9];
    for r in saved.into_iter().filter(|r| *r != dst) {
        a.push_reg(r);
    }
    a.mov_imm64(R::V0, 0);
    a.mov_imm64(R::V9, 0);
    let next = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(next);
    load_rt(a, R::V1, COUNT);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    ptr(a, BANK);
    a.load_ptr_disp32(R::V6, R::V8, TOP);
    load_rt(a, R::V1, ALLOC);
    a.cmp_reg_reg(R::V0, R::V1);
    let rounded = a.create_text_label();
    let end = a.create_text_label();
    a.jcc_label(C::NotEqual, rounded);
    load_rt(a, R::V6, RT_OLD_TOP);
    a.bind_text_label(rounded);
    a.add_reg_imm32(R::V6, (HEAP_PAGE_BYTES - 1) as i32);
    a.shr_reg_imm8(R::V6, 17);
    a.shl_reg_imm8(R::V6, 17);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, end);
    a.load_ptr_disp32(R::V7, R::V8, BASE);
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::Above, end);
    a.add_reg_imm32(R::V6, HEAP_PAGE_BYTES as i32);
    a.bind_text_label(end);
    a.load_ptr_disp32(R::V7, R::V8, END);
    a.sub_reg_reg(R::V7, R::V6);
    a.jcc_label(C::Below, l.fail);
    a.shr_reg_imm8(R::V7, 17);
    a.add_reg_reg(R::V9, R::V7);
    a.add_reg_imm32(R::V0, 1);
    a.jmp_label(next);
    a.bind_text_label(done);
    a.mov_reg_reg(dst, R::V9);
    for r in saved.into_iter().rev().filter(|r| *r != dst) {
        a.pop_reg(r);
    }
}
/// Quiescent owner, collector lock held, previous relocation/retirement done.
/// Return the Full destination demand in V0; only V1/V2 are scratch. A closed
/// Old census can tighten charged occupancy. Young uses sealed protection or
/// both physical source streams, since Full promotes even fresh age-zero data.
/// Two actors and next-fit tails require four times the logical footprint plus
/// boundary pages. A closed whole-moving census, including later fresh
/// identities, also caps the sum to avoid counting its Young portion twice.
/// Demand is not capped by the bootstrap Old partition; claim
/// consumes actual shared free runs after the Young destination is reserved.
pub(super) fn destination_pages<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    used(a, R::V2);
    let occupancy = a.create_text_label();
    load_rt(a, R::V0, RT_MARK_VALID);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, occupancy);
    super::old_footprint::protected(a);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::AboveOrEqual, occupancy);
    a.mov_reg_reg(R::V2, R::V0);
    a.bind_text_label(occupancy);
    a.shl_reg_imm8(R::V2, 2);
    a.add_reg_imm32(R::V2, (HEAP_PAGE_BYTES - 1) as i32);
    a.shr_reg_imm8(R::V2, 17);
    let sealed = a.create_text_label();
    let summed = a.create_text_label();
    load_rt(a, R::V0, young_mark::VALID);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sealed);
    capacity::allocated_source_pages(a, l);
    a.shl_reg_imm8(R::V0, 2);
    a.add_reg_imm32(R::V0, 4);
    a.jmp_label(summed);
    a.bind_text_label(sealed);
    capacity::promotion_pages(a, l);
    a.bind_text_label(summed);
    a.add_reg_reg(R::V0, R::V2);
    // The Old census includes Young objects as well. After closure, TRACE +
    // BLACK bounds the entire moving set (fresh keeps charging while valid),
    // so do not require its Young contribution twice. An active/incomplete
    // snapshot is not a whole-heap bound. Retain the independent source bound
    // if smaller, including when the census has saturated or retains garbage.
    let bounded = a.create_text_label();
    load_rt(a, R::V1, RT_MARK_VALID);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, bounded);
    a.mov_reg_reg(R::V2, R::V0);
    super::old_footprint::protected(a);
    a.shl_reg_imm8(R::V0, 2);
    a.add_reg_imm32(R::V0, (HEAP_PAGE_BYTES - 1) as i32);
    a.shr_reg_imm8(R::V0, 17);
    a.add_reg_imm32(R::V0, 4);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, bounded);
    a.mov_reg_reg(R::V0, R::V2);
    a.bind_text_label(bounded);
    let minimum = super::geometry::policy_geometry(a)
        .old_min
        .div_ceil(HEAP_PAGE_BYTES)
        .max(1);
    let done = a.create_text_label();
    a.cmp_reg_imm32(R::V0, minimum as i32);
    a.jcc_label(C::AboveOrEqual, done);
    a.mov_imm64(R::V0, minimum);
    a.bind_text_label(done);
}
/// Freeze before detaching source roles; claim subsequently consumes REMAINING.
pub(super) fn prepare_capacity<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    destination_pages(a, l);
    store_rt(a, REMAINING, R::V0);
}
/// The Young destination is already occupied. Combine disjoint free runs up
/// to the frozen Full budget, rather than discarding smaller free runs. Keep
/// the first largest-run choice, then consume the remainder in one forward
/// directory pass instead of a complete largest-run search per fragment.
pub(super) fn claim<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for field in [COUNT, ALLOC, SCAN, PREFIX, WALK, pool::OLD_PAGES] {
        store_rt_imm(a, field, 0);
    }
    let next = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(next);
    load_rt(a, R::V7, REMAINING);
    let subsequent = a.create_text_label();
    let claimed = a.create_text_label();
    load_rt(a, R::V0, COUNT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, subsequent);
    pool::largest_old_limit(a, l, true);
    a.jmp_label(claimed);
    a.bind_text_label(subsequent);
    pool::next_old_limit(a, l);
    a.bind_text_label(claimed);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, done);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    store_rt(a, TEMP_BASE, R::V0);
    store_rt(a, TEMP_PAGES, R::V7);
    load_rt(a, R::V0, COUNT);
    ptr(a, BANK);
    load_rt(a, R::V6, TEMP_BASE);
    a.store_ptr_disp32(R::V8, BASE, R::V6);
    a.store_ptr_disp32(R::V8, TOP, R::V6);
    load_rt(a, R::V7, TEMP_PAGES);
    a.shl_reg_imm8(R::V7, 17);
    a.add_reg_reg(R::V7, R::V6);
    a.store_ptr_disp32(R::V8, END, R::V7);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, COUNT, R::V0);
    load_rt(a, R::V6, TEMP_PAGES);
    load_rt(a, R::V7, pool::OLD_PAGES);
    a.add_reg_reg(R::V7, R::V6);
    store_rt(a, pool::OLD_PAGES, R::V7);
    load_rt(a, R::V7, REMAINING);
    a.sub_reg_reg(R::V7, R::V6);
    store_rt(a, REMAINING, R::V7);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::NotEqual, next);
    a.bind_text_label(done);
    load_rt(a, R::V0, COUNT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.old_full);
    a.mov_imm64(R::V0, 0);
    ptr(a, BANK);
    a.load_ptr_disp32(R::V0, R::V8, BASE);
    store_rt(a, RT_OLD_BASE, R::V0);
    store_rt(a, RT_OLD_TOP, R::V0);
}
/// Return unallocated Old reservation pages at a quiescent owner safepoint.
/// The active run keeps its last partial page; unused later descriptors are
/// removed. Used headers, pending roots and completed-run tails are untouched.
/// Called by full detach and exposed to deterministic idle-pressure probes.
pub(super) fn shrink_unused<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, ALLOC);
    ptr(a, BANK);
    load_rt(a, R::V6, RT_OLD_TOP);
    a.store_ptr_disp32(R::V8, TOP, R::V6);
    store_rt_imm(a, WALK, 0);
    let next = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(next);
    load_rt(a, R::V0, WALK);
    load_rt(a, R::V1, COUNT);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    ptr(a, BANK);
    a.load_ptr_disp32(R::V6, R::V8, TOP);
    a.add_reg_imm32(R::V6, (HEAP_PAGE_BYTES - 1) as i32);
    a.shr_reg_imm8(R::V6, 17);
    a.shl_reg_imm8(R::V6, 17);
    let later = a.create_text_label();
    let ranged = a.create_text_label();
    load_rt(a, R::V1, ALLOC);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, later);
    a.load_ptr_disp32(R::V7, R::V8, BASE);
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::Above, later);
    a.add_reg_imm32(R::V6, HEAP_PAGE_BYTES as i32);
    a.bind_text_label(later);
    a.load_ptr_disp32(R::V7, R::V8, END);
    a.store_ptr_disp32(R::V8, END, R::V6);
    super::page_runs::resize(a);
    a.shr_reg_imm8(R::V6, 17);
    a.shr_reg_imm8(R::V7, 17);
    a.bind_text_label(ranged);
    a.cmp_reg_reg(R::V6, R::V7);
    let advanced = a.create_text_label();
    a.jcc_label(C::AboveOrEqual, advanced);
    super::page_runs::free_unused(a, l);
    a.mov_reg_reg(R::V0, R::V6);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::layout(a).generations as i32);
    a.mov_imm64(R::V1, 0);
    a.atomic_store_release(R::V0, R::V1);
    copy::bump(a, pool::RELEASED, R::V9);
    a.add_reg_imm32(R::V6, 1);
    a.jmp_label(ranged);
    a.bind_text_label(advanced);
    copy::bump(a, WALK, R::V0);
    a.jmp_label(next);
    a.bind_text_label(done);
    load_rt(a, R::V0, ALLOC);
    a.mov_reg_reg(R::V6, R::V0);
    a.add_reg_imm32(R::V6, 1);
    store_rt(a, COUNT, R::V6);
    ptr(a, BANK);
    a.load_ptr_disp32(R::V6, R::V8, END);
    a.load_ptr_disp32(R::V7, R::V8, BASE);
    a.sub_reg_reg(R::V6, R::V7);
    load_rt(a, R::V7, PREFIX);
    a.add_reg_reg(R::V6, R::V7);
    a.shr_reg_imm8(R::V6, 17);
    store_rt(a, pool::OLD_PAGES, R::V6);
}
/// Stop-the-owner/full pause, collector lock held. Save the immutable source
/// list, mark its populated pages, and return only unused reservation tails.
pub(super) fn detach<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    shrink_unused(a, l);
    load_rt(a, R::V0, ALLOC);
    ptr(a, BANK);
    load_rt(a, R::V6, RT_OLD_TOP);
    a.store_ptr_disp32(R::V8, TOP, R::V6);
    load_rt(a, R::V0, BANK);
    store_rt(a, FROM_BANK, R::V0);
    load_rt(a, R::V0, COUNT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, FROM_COUNT, R::V0);
    for field in [WALK, RETIRE, YOUNG_RETURNED] {
        store_rt_imm(a, field, 0);
    }
    let next = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(next);
    load_rt(a, R::V0, WALK);
    load_rt(a, R::V1, FROM_COUNT);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    ptr(a, FROM_BANK);
    a.load_ptr_disp32(R::V6, R::V8, BASE);
    a.shr_reg_imm8(R::V6, 17);
    a.load_ptr_disp32(R::V7, R::V8, TOP);
    a.add_reg_imm32(R::V7, (HEAP_PAGE_BYTES - 1) as i32);
    a.shr_reg_imm8(R::V7, 17);
    let flags = a.create_text_label();
    let trim = a.create_text_label();
    a.bind_text_label(flags);
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::AboveOrEqual, trim);
    a.mov_reg_reg(R::V0, R::V6);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::layout(a).old_sources as i32);
    a.mov_imm64(R::V1, 1);
    a.atomic_store_release(R::V0, R::V1);
    a.add_reg_imm32(R::V6, 1);
    a.jmp_label(flags);
    a.bind_text_label(trim);
    a.load_ptr_disp32(R::V2, R::V8, END);
    a.shr_reg_imm8(R::V2, 17);
    let tail = a.create_text_label();
    let advanced = a.create_text_label();
    a.bind_text_label(tail);
    a.cmp_reg_reg(R::V6, R::V2);
    a.jcc_label(C::AboveOrEqual, advanced);
    super::page_runs::free_unused(a, l);
    a.mov_reg_reg(R::V0, R::V6);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::layout(a).generations as i32);
    a.mov_imm64(R::V1, 0);
    a.atomic_store_release(R::V0, R::V1);
    copy::bump(a, pool::RELEASED, R::V9);
    a.add_reg_imm32(R::V6, 1);
    a.jmp_label(tail);
    a.bind_text_label(advanced);
    copy::bump(a, WALK, R::V0);
    a.jmp_label(next);
    a.bind_text_label(done);
    load_rt(a, R::V0, BANK);
    a.mov_imm64(R::V1, 1);
    a.xor_reg_reg(R::V0, R::V1);
    store_rt(a, BANK, R::V0);
    source_span(a);
}
pub(super) fn source_span<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, RETIRE);
    ptr(a, FROM_BANK);
    a.load_ptr_disp32(R::V0, R::V8, BASE);
    store_rt(a, RT_OLD_FROM, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, TOP);
    store_rt(a, RT_OLD_FROM_END, R::V0);
}
/// Equal iff offset belongs to a populated Old source page during a full
/// relocation, including synchronous raw-root rewriting before the concurrent
/// phase opens. Preserve all registers and the original encoded reference.
pub(super) fn source<E: PortableAsm>(a: &mut E, off: R) {
    let scratch = if off == R::V11 { R::V10 } else { R::V11 };
    let no = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(scratch);
    let active = a.create_text_label();
    load_rt(a, scratch, RT_PHASE_OPEN);
    a.test_reg_reg(scratch, scratch);
    a.jcc_label(C::NotEqual, active);
    load_rt(a, scratch, super::RT_COLLECTING);
    a.test_reg_reg(scratch, scratch);
    a.jcc_label(C::Equal, no);
    a.bind_text_label(active);
    load_rt(a, scratch, RT_FULL);
    a.test_reg_reg(scratch, scratch);
    a.jcc_label(C::Equal, no);
    a.mov_reg_reg(scratch, off);
    a.cmp_reg_imm32(scratch, HEAP_PAGE_BYTES as i32);
    a.jcc_label(C::Below, no);
    a.cmp_reg_imm32(scratch, super::geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, no);
    a.shr_reg_imm8(scratch, 17);
    a.shl_reg_imm8(scratch, 3);
    a.add_reg_reg(scratch, R::V3);
    a.add_reg_imm32(scratch, super::layout(a).old_sources as i32);
    a.atomic_load_acquire(scratch, scratch);
    a.jmp_label(done);
    a.bind_text_label(no);
    a.mov_imm64(scratch, 0);
    a.bind_text_label(done);
    a.cmp_reg_imm8(scratch, 1);
    a.pop_reg(scratch);
}
