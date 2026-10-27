//! Young windows and sealed copy bounds in the shared physical page pool.
use super::{Labels, Runtime, load_rt, store_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;
#[repr(C)]
pub(super) struct State {
    fresh_budget: u64,
    requested: u64,
    active: u64,
    source_end: u64,
    trace_bytes: u64,
    black_bytes: u64,
    sealed_bytes: u64,
    mature_bytes: u64,
    sealed_mature_bytes: u64,
    reserve: u64,
    clamps: u64,
    grows: u64,
    shrinks: u64,
    samples: u64,
    seen: u64,
    up: u64,
    down: u64,
}
macro_rules! field { ($($name:ident:$member:ident),* $(,)?)=> { $(
    pub(super) const $name:usize=offset_of!(Runtime,capacity.$member);
)* }; }
field! { FRESH_BUDGET:fresh_budget,REQUESTED:requested,ACTIVE:active,SOURCE_END:source_end,TRACE:trace_bytes,
BLACK:black_bytes,SEALED:sealed_bytes,MATURE:mature_bytes,MATURE_SEALED:sealed_mature_bytes,
RESERVE:reserve,CLAMPS:clamps,GROWS:grows,
SHRINKS:shrinks,SAMPLES:samples,SEEN:seen,UP:up,DOWN:down }
fn store<E: PortableAsm>(a: &mut E, f: usize, n: u64) {
    a.mov_imm64(R::V0, n);
    store_rt(a, f, R::V0);
}
/// V2 = Young source offset. Preserve V2 and the caller's V0. TRACE is
/// collector-lock protected; distinct fresh allocators update BLACK atomically.
pub(super) fn protect<E: PortableAsm>(a: &mut E, black: bool) {
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    load_rt(a, R::V0, super::RT_HEAP_BASE);
    a.add_reg_reg(R::V0, R::V2);
    a.load_ptr_disp32(R::V0, R::V0, super::copy::PAYLOAD);
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    let sized = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V0, super::copy::HEADER);
    if !black {
        // Retain the object footprint across the all-Young accumulation.
        a.push_reg(R::V0);
        a.push_reg(R::V0);
    }
    let f = if black { BLACK } else { TRACE };
    if black {
        super::copy::add_bytes(a, f, R::V0);
    } else {
        load_rt(a, R::V1, f);
        a.add_reg_reg(R::V0, R::V1);
        store_rt(a, f, R::V0);
    }
    if !black {
        a.pop_reg(R::V0);
        a.pop_reg(R::V0);
        let immature = a.create_text_label();
        load_rt(a, R::V1, super::RT_HEAP_BASE);
        a.add_reg_reg(R::V1, R::V2);
        a.load_ptr_disp32(R::V1, R::V1, super::copy::MARK_RECORD);
        a.load_ptr_disp32(
            R::V1,
            R::V1,
            offset_of!(crate::gc_abi::MarkRecord, age) as i32,
        );
        // Every legal frozen threshold is at least MIN. Fresh age-zero black
        // allocations cannot promote; older objects remain a safe upper bound
        // even when adaptive policy chooses a threshold above MIN.
        a.cmp_reg_imm8(R::V1, (super::tenuring::MIN - 1) as i8);
        a.jcc_label(C::Below, immature);
        load_rt(a, R::V1, MATURE);
        a.add_reg_reg(R::V0, R::V1);
        store_rt(a, MATURE, R::V0);
        a.bind_text_label(immature);
    }
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
}
pub(super) fn begin<E: PortableAsm>(a: &mut E) {
    for f in [TRACE, BLACK, SEALED, MATURE, MATURE_SEALED] {
        store(a, f, 0);
    }
}
pub(super) fn seal<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, TRACE);
    load_rt(a, R::V1, BLACK);
    a.add_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    load_rt(a, R::V1, ACTIVE);
    a.shl_reg_imm8(R::V1, 17);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    store_rt(a, SEALED, R::V0);
    load_rt(a, R::V1, MATURE);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, l.fail);
    store_rt(a, MATURE_SEALED, R::V1);
}
/// Exact current Young byte capacity, as distinct from reserved maximum.
pub(super) fn bytes<E: PortableAsm>(a: &mut E, reg: R) {
    load_rt(a, reg, ACTIVE);
    a.shl_reg_imm8(reg, 17);
}
/// Conservative Old promotion capacity in pages, without the Young-window
/// cap. Two actors can reserve twice an object's footprint; next-fit run
/// tails can double that charge. Four pages cover rounding/stream boundaries.
/// Return V0; V1 is scratch. Unsealed input uses the entire active source.
pub(super) fn promotion_pages<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    protected_pages(a, l, SEALED);
}
/// Normal Young promotion excludes age-zero survivors and fresh black
/// allocations. Full evacuation and Young sizing still use all protection.
/// Without a sealed census, LAST_YOUNG bounds every possible mature object:
/// only previous winning Young copies have age >= 1; all later fresh allocation
/// starts at age zero. Finish publishes this total before either caller enters,
/// and a Full move leaves zero Young copies. It can include now-dead survivors.
/// Validate that total against both claimed source streams; preserve V2.
pub(super) fn mature_promotion_pages<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    let sealed = a.create_text_label();
    let no_mature = a.create_text_label();
    load_rt(a, R::V0, super::moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no_mature);
    load_rt(a, R::V0, super::young_mark::VALID);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no_mature);
    load_rt(a, R::V0, MATURE_SEALED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.bind_text_label(no_mature);
    load_rt(a, R::V0, super::young_mark::VALID);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sealed);
    allocated_source_pages(a, l);
    a.shl_reg_imm8(R::V0, 17);
    load_rt(a, R::V1, super::tenuring::LAST_YOUNG);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Above, l.fail);
    a.mov_reg_reg(R::V0, R::V1);
    copy_bound(a, l);
    a.jmp_label(done);
    a.bind_text_label(sealed);
    protected_pages(a, l, MATURE_SEALED);
    a.bind_text_label(done);
}
/// Bound unsealed Young volume by all claimed pages, including the worker's
/// upper stream and both partially used pages. Sole owner, collector lock held,
/// previous relocation/retirement complete: published mirrors cannot lag an
/// in-flight page claim. Return V0; only V1 is scratch (V2 may hold Old room).
/// No page, object or root walk; the middle unclaimed gap contains no objects.
pub(super) fn allocated_source_pages<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for (low, high) in [
        (super::RT_SPACE_BASE, super::RT_NEXT_PAGE),
        (super::RT_NEXT_PAGE, super::RT_COPY_LOW),
        (super::RT_COPY_LOW, super::RT_SPACE_END),
    ] {
        load_rt(a, R::V0, low);
        load_rt(a, R::V1, high);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Above, l.fail);
    }
    load_rt(a, R::V0, super::RT_SPACE_END);
    load_rt(a, R::V1, super::RT_SPACE_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, ACTIVE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, super::RT_COPY_LOW);
    load_rt(a, R::V1, super::RT_NEXT_PAGE);
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, ACTIVE);
    a.sub_reg_reg(R::V1, R::V0);
    a.mov_reg_reg(R::V0, R::V1);
}
fn protected_pages<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, field: usize) {
    let unsealed = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, super::young_mark::VALID);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, unsealed);
    load_rt(a, R::V0, field);
    a.cmp_reg_imm32(R::V0, super::geometry(a).young_bytes as i32);
    a.jcc_label(C::Above, l.fail);
    copy_bound(a, l);
    a.jmp_label(done);
    a.bind_text_label(unsealed);
    load_rt(a, R::V0, ACTIVE);
    a.shl_reg_imm8(R::V0, 17);
    copy_bound(a, l);
    a.bind_text_label(done);
}
fn requested<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V2, REQUESTED);
    a.cmp_reg_imm32(R::V2, super::geometry::policy_geometry(a).young_min as i32);
    a.jcc_label(C::Below, l.fail);
    a.cmp_reg_imm32(R::V2, super::geometry(a).young_pages as i32);
    a.jcc_label(C::Above, l.fail);
    super::moving_round::admission::request_window(a, l);
}
fn young_floor<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let maximum = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, super::young_mark::VALID);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, maximum);
    promotion_pages(a, l);
    super::moving_round::admission::add_fresh_window(a, l);
    a.cmp_reg_imm32(R::V0, super::geometry(a).young_pages as i32);
    a.jcc_label(C::Below, done);
    a.bind_text_label(maximum);
    a.mov_imm64(R::V0, super::geometry(a).young_pages);
    a.bind_text_label(done);
}
/// Hypothetical normal Young destination, independent of the preceding
/// cycle's RT_FULL bit. Pure sizing, no role publication or policy counters.
/// Returns V2=max(request, protected floor), using V0/V1 as scratch.
pub(super) fn young_destination<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    requested(a, l);
    young_floor(a, l);
    let done = a.create_text_label();
    a.cmp_reg_reg(R::V2, R::V0);
    a.jcc_label(C::AboveOrEqual, done);
    a.mov_reg_reg(R::V2, R::V0);
    a.bind_text_label(done);
}
/// Full has no Young survivors. Fit its next fresh window to an actual free
/// run, retaining the frozen Old demand when it can be funded in full. If that
/// conservative upper bound cannot fit even with the minimum fresh window,
/// retain the policy request (subject to shape) and attempt actual copies.
/// An unfundable upper bound is not an exact live size or a reason to force
/// the minimum window and alter the following Young collection's capacity.
/// Called after Old detach returns unused tails, before any destination claim.
/// V2=requested pages -> selected pages; other caller-saved registers scratch.
fn full_destination<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for r in [R::V6, R::V7, R::V8, R::V10] {
        a.mov_imm64(r, 0);
    }
    super::lea_rt(a, R::V9, (super::layout(a).generations + 8) as usize);
    let next = a.create_text_label();
    let occupied = a.create_text_label();
    let advance = a.create_text_label();
    let counted = a.create_text_label();
    a.bind_text_label(next);
    a.cmp_reg_imm32(R::V8, super::geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, counted);
    a.atomic_load_acquire(R::V0, R::V9);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, occupied);
    a.add_reg_imm32(R::V6, 1); // total free
    a.add_reg_imm32(R::V10, 1); // current contiguous run
    a.cmp_reg_reg(R::V10, R::V7);
    a.jcc_label(C::Below, advance);
    a.mov_reg_reg(R::V7, R::V10); // largest run
    a.jmp_label(advance);
    a.bind_text_label(occupied);
    a.mov_imm64(R::V10, 0);
    a.bind_text_label(advance);
    a.add_reg_imm32(R::V8, 1);
    a.add_reg_imm32(R::V9, 8);
    a.jmp_label(next);
    a.bind_text_label(counted);
    let minimum = super::geometry::policy_geometry(a).young_min;
    a.cmp_reg_imm32(R::V7, minimum as i32);
    a.jcc_label(C::Below, l.oom);
    let shape_only = a.create_text_label();
    let available = a.create_text_label();
    load_rt(a, R::V0, super::old_runs::REMAINING);
    a.sub_reg_reg(R::V6, R::V0);
    a.jcc_label(C::Below, shape_only);
    a.cmp_reg_imm32(R::V6, minimum as i32);
    a.jcc_label(C::AboveOrEqual, available);
    a.bind_text_label(shape_only);
    a.mov_reg_reg(R::V6, R::V2);
    a.bind_text_label(available);
    let shaped = a.create_text_label();
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::Below, shaped);
    a.mov_reg_reg(R::V6, R::V7);
    a.bind_text_label(shaped);
    let done = a.create_text_label();
    a.cmp_reg_reg(R::V2, R::V6);
    a.jcc_label(C::Below, done);
    a.mov_reg_reg(R::V2, R::V6);
    a.bind_text_label(done);
}

/// Prior retirement completed, full/Young decision made, lock held. Freeze the
/// destination size and claim its physical run before binding any page.
pub(super) fn select<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let chosen = a.create_text_label();
    let floor = a.create_text_label();
    let full = a.create_text_label();
    requested(a, l);
    load_rt(a, R::V0, super::RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, full); // Full copies all sources into Old.
    young_floor(a, l);
    a.jmp_label(floor);
    a.bind_text_label(full);
    full_destination(a, l);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(floor);
    store_rt(a, RESERVE, R::V0);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, chosen);
    a.jcc_label(C::Equal, chosen);
    a.mov_reg_reg(R::V2, R::V0);
    super::copy::bump(a, CLAMPS, R::V0);
    a.bind_text_label(chosen);
    let smaller = a.create_text_label();
    let same = a.create_text_label();
    load_rt(a, R::V0, ACTIVE);
    a.cmp_reg_reg(R::V2, R::V0);
    a.jcc_label(C::Equal, same);
    a.jcc_label(C::Below, smaller);
    super::copy::bump(a, GROWS, R::V0);
    a.jmp_label(same);
    a.bind_text_label(smaller);
    super::copy::bump(a, SHRINKS, R::V0);
    a.bind_text_label(same);
    store_rt(a, ACTIVE, R::V2);
    a.mov_reg_reg(R::V7, R::V2);
    a.mov_imm64(R::V6, super::generation::YOUNG);
    super::pool::claim(a, l);
    store_rt(a, super::RT_SPACE_BASE, R::V0);
    store_rt(a, super::RT_NEXT_PAGE, R::V0);
    load_rt(a, R::V2, ACTIVE);
    a.add_reg_reg(R::V2, R::V0);
    store_rt(a, super::RT_SPACE_END, R::V2);
    store_rt(a, super::RT_COPY_LOW, R::V2);
    if l.worker.is_some() {
        a.shl_reg_imm8(R::V2, 32);
        a.or_reg_reg(R::V2, R::V0);
        store_rt(a, super::RT_BOUNDS, R::V2);
        store(a, super::RT_W_PAGE, 0);
    }
    store(a, FRESH_BUDGET, super::HEAP_PAGE_BYTES);
    super::moving_round::admission::fund(a, l);
    super::start_policy::cap_to_space(a);
}
/// Only a new measured normal Young cycle votes; full's forced promotion
/// cannot provide a low-survival observation. Requested size moves by 32 pages.
pub(super) fn observe<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    let reset = a.create_text_label();
    let full_reset = a.create_text_label();
    load_rt(a, R::V0, super::start_policy::MARK_SAMPLES);
    load_rt(a, R::V1, super::RT_FULL);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, full_reset);
    load_rt(a, R::V1, SEEN);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, done);
    store_rt(a, SEEN, R::V0);
    super::copy::bump(a, SAMPLES, R::V0);
    load_rt(a, R::V2, REQUESTED);
    load_rt(a, R::V0, super::tenuring::LAST_YOUNG);
    bytes(a, R::V1);
    a.shr_reg_imm8(R::V1, 3);
    a.cmp_reg_reg(R::V0, R::V1);
    let high = a.create_text_label();
    a.jcc_label(C::Above, high);
    store(a, UP, 0);
    super::copy::bump(a, DOWN, R::V0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Below, done);
    a.cmp_reg_imm32(R::V2, super::geometry::policy_geometry(a).young_min as i32);
    a.jcc_label(C::Equal, reset);
    a.sub_reg_imm8(R::V2, 32);
    a.cmp_reg_imm32(R::V2, super::geometry::policy_geometry(a).young_min as i32);
    let save = a.create_text_label();
    a.jcc_label(C::AboveOrEqual, save);
    a.mov_imm64(R::V2, super::geometry::policy_geometry(a).young_min);
    a.jmp_label(save);
    a.bind_text_label(high);
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V1, 1);
    a.add_reg_reg(R::V1, R::V0);
    load_rt(a, R::V0, super::tenuring::LAST_YOUNG);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, reset);
    store(a, DOWN, 0);
    super::copy::bump(a, UP, R::V0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Below, done);
    a.add_reg_imm32(R::V2, 32);
    a.cmp_reg_imm32(
        R::V2,
        super::geometry::policy_geometry(a).young_pages as i32,
    );
    a.jcc_label(C::Below, save);
    a.mov_imm64(R::V2, super::geometry::policy_geometry(a).young_pages);
    a.bind_text_label(save);
    store_rt(a, REQUESTED, R::V2);
    a.jmp_label(reset);
    a.bind_text_label(full_reset);
    store_rt(a, SEEN, R::V0);
    a.bind_text_label(reset);
    store(a, UP, 0);
    store(a, DOWN, 0);
    a.bind_text_label(done);
}

/// Successful fresh allocation consumes its budget exactly once. Moving owners
/// consume their bank-stamped reservation; copies never charge a fresh budget.
pub(super) fn fresh<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let legacy = a.create_text_label();
    let moving_done = a.create_text_label();
    a.push_reg(R::V0);
    load_rt(a, R::V0, super::moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.pop_reg(R::V0);
    a.jcc_label(C::Equal, legacy);
    super::moving_round::admission::consume(a, l);
    a.jmp_label(moving_done);
    a.bind_text_label(legacy);
    let done = a.create_text_label();
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    load_rt(a, R::V0, super::RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, super::RT_FRESH_SIZE);
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    let sized = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V0, super::copy::HEADER);
    load_rt(a, R::V1, FRESH_BUDGET);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V1, R::V0);
    store_rt(a, FRESH_BUDGET, R::V1);
    a.bind_text_label(done);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.bind_text_label(moving_done);
}

/// Worst next-fit fragmentation and one partial page per actor. Independent
/// mutators plus the worker can each initialize a losing tentative copy.
pub(super) fn copy_bound<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V2);
    let legacy = a.create_text_label();
    let scale = a.create_text_label();
    load_rt(a, R::V1, super::moving_round::ENABLED);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, legacy);
    load_rt(
        a,
        R::V1,
        super::participants::HANDSHAKE + offset_of!(crate::gc_abi::HandshakeState, members),
    );
    a.add_reg_imm32(R::V1, 1);
    a.jcc_label(C::Equal, l.oom);
    a.add_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Below, l.oom);
    a.jmp_label(scale);
    a.bind_text_label(legacy);
    a.mov_imm64(R::V1, 4);
    a.bind_text_label(scale);
    a.mul_unsigned_wide(R::V1);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.oom);
    a.add_reg_imm32(R::V0, (super::HEAP_PAGE_BYTES - 1) as i32);
    a.jcc_label(C::Below, l.oom);
    a.shr_reg_imm8(R::V0, 17);
    a.add_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.oom);
    a.pop_reg(R::V2);
}
