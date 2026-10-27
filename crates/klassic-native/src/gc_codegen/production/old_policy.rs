//! Old-cycle feedback, sampled only by the owner at relocation completion.
//! Reserved destination bytes conservatively estimate retention/promotion;
//! duplicate copy reservations are included. No clock or graph walk is used.
use super::{Labels, Runtime, load_rt, store_rt};
use crate::gc_abi::ObjectHeader;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

const DEADBAND: u64 = 1 << 20;
#[repr(C)]
pub(super) struct State {
    allocated: u64,
    retained: u64,
    last_old: u64,
    promotion_ema: u64,
    target: u64,
    allocation_base: u64,
    allocation_budget: u64,
    full_samples: u64,
    young_samples: u64,
    updates: u64,
    deferred: u64,
    periodic: u64,
    pressure: u64,
    reason: u64,
    prediction_clamps: u64,
}
macro_rules! field { ($($name:ident: $member:ident),* $(,)?) => { $(
    pub(super) const $name: usize = offset_of!(Runtime, old_policy.$member);
)* }; }
field! { ALLOCATED:allocated, RETAINED:retained, LAST_OLD:last_old, EMA:promotion_ema, TARGET:target,
BASE:allocation_base, BUDGET:allocation_budget, FULL:full_samples,
YOUNG:young_samples, UPDATES:updates, DEFERRED:deferred, PERIODIC:periodic,
PRESSURE:pressure, REASON:reason, CLAMPS:prediction_clamps }
fn allocated<E: PortableAsm>(a: &mut E, dst: R) {
    load_rt(a, dst, ALLOCATED);
}

/// Dedicated Old growth can require a snapshot even with an almost empty
/// Young allocation window. This is a readiness hint; phase/frontier checks
/// still authorize the actual snapshot and subsequent Full collection.
pub(super) fn large_mark_needed<E: PortableAsm>(a: &mut E, needed: E::TextLabel) {
    let done = a.create_text_label();
    load_rt(a, R::V0, super::large_object::LIVE_PAGES);
    a.shl_reg_imm8(R::V0, 17);
    a.cmp_reg_imm32(R::V0, super::geometry::policy_geometry(a).old_min as i32);
    a.jcc_label(C::Below, done);
    load_rt(a, R::V1, super::large_object::RETAINED_PAGES);
    a.shl_reg_imm8(R::V1, 17);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.cmp_reg_imm32(R::V0, DEADBAND as i32);
    a.jcc_label(C::AboveOrEqual, needed);
    a.bind_text_label(done);
}

/// Only the successful fresh-allocation entry calls this; alloc_raw also
/// serves owner GC copies and must not charge application allocation credit.
/// Preserve V0/V3 and all registers except V1/V2.
pub(super) fn record_fresh<E: PortableAsm>(a: &mut E) {
    let sized = a.create_text_label();
    load_rt(a, R::V1, super::RT_FRESH_SIZE);
    a.add_reg_imm32(R::V1, 15);
    a.and_reg_imm32(R::V1, -16);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V1, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V1, std::mem::size_of::<ObjectHeader>() as i32);
    super::counters::add(a, ALLOCATED, R::V1);
}

/// Called after the Old worker/phase eligibility guards. At least the configured minimum Old
/// bytes are required. Matching target or enough post-full allocation allows
/// a request; neither statistic bypasses the actual mark-frontier recheck.
pub(super) fn eligible<E: PortableAsm>(a: &mut E, skip: E::TextLabel) {
    let pressure = a.create_text_label();
    let periodic = a.create_text_label();
    let target = a.create_text_label();
    let defer = a.create_text_label();
    let done = a.create_text_label();
    super::old_runs::used(a, R::V0);
    // Dedicated pages occupy the shared pool but never enter a moving run.
    load_rt(a, R::V1, super::large_object::LIVE_PAGES);
    a.shl_reg_imm8(R::V1, 17);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, super::geometry::policy_geometry(a).old_min as i32);
    a.jcc_label(C::Below, skip);
    load_rt(a, R::V1, TARGET);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, target);
    a.mov_imm64(R::V1, super::geometry::policy_geometry(a).old_min);
    a.bind_text_label(target);
    {
        // A retained live set may exceed an advisory target. Repeating a full
        // move on every allocation cannot shrink it; require fresh growth or
        // the existing post-full allocation credit before trying again.
        let enough_headroom = a.create_text_label();
        load_rt(a, R::V2, super::large_object::RETAINED_PAGES);
        if a.gc_heap_soft_maximum() == a.gc_heap_capacity() {
            a.test_reg_reg(R::V2, R::V2);
            a.jcc_label(C::Equal, enough_headroom);
        }
        a.shl_reg_imm8(R::V2, 17);
        load_rt(a, R::V7, RETAINED);
        a.add_reg_reg(R::V2, R::V7);
        a.cmp_reg_reg(R::V2, R::V1);
        a.jcc_label(C::Below, enough_headroom);
        a.add_reg_imm32(R::V2, DEADBAND as i32);
        a.mov_reg_reg(R::V1, R::V2);
        a.bind_text_label(enough_headroom);
    }
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, pressure);
    load_rt(a, R::V0, FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, defer);
    allocated(a, R::V0);
    load_rt(a, R::V1, BASE);
    a.cmp_reg_reg(R::V0, R::V1);
    let monotone = a.create_text_label();
    a.jcc_label(C::AboveOrEqual, monotone);
    // Rebase a wrapped counter without credit, then allow a fresh window.
    store_rt(a, BASE, R::V0);
    a.jmp_label(defer);
    a.bind_text_label(monotone);
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, BUDGET);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, periodic);
    a.bind_text_label(defer);
    super::copy::bump(a, DEFERRED, R::V0);
    a.jmp_label(skip);
    a.bind_text_label(pressure);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(periodic);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    store_rt(a, REASON, R::V0);
}
/// Count only an accepted request (eligibility itself is checked twice).
pub(super) fn accepted<E: PortableAsm>(a: &mut E) {
    let periodic = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, REASON);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, periodic);
    super::copy::bump(a, PRESSURE, R::V0);
    a.jmp_label(done);
    a.bind_text_label(periodic);
    super::copy::bump(a, PERIODIC, R::V0);
    a.bind_text_label(done);
}

/// One fixed-count observation at the quiescent moving-range close. Old top
/// and both copier frontiers are stable; there is no worker-private statistic
/// to read unsynchronized. Clobbers caller-saved registers, preserving V3.
pub(super) fn observe<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let young = a.create_text_label();
    let sampled = a.create_text_label();
    let budget = a.create_text_label();
    let average = a.create_text_label();
    let average_done = a.create_text_label();
    super::old_runs::used(a, R::V0);
    load_rt(a, R::V1, super::pool::OLD_PAGES);
    a.shl_reg_imm8(R::V1, 17);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    a.mov_reg_reg(R::V6, R::V0);
    load_rt(a, R::V1, super::RT_FULL);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, young);
    store_rt(a, RETAINED, R::V6);
    allocated(a, R::V0);
    store_rt(a, BASE, R::V0);
    a.mov_reg_reg(R::V0, R::V6);
    a.shl_reg_imm8(R::V0, 1);
    a.cmp_reg_imm32(R::V0, super::geometry(a).young_bytes as i32);
    a.jcc_label(C::AboveOrEqual, budget);
    a.mov_imm64(R::V0, super::geometry(a).young_bytes);
    a.bind_text_label(budget);
    store_rt(a, BUDGET, R::V0);
    cap_prediction(a);
    super::copy::bump(a, FULL, R::V0);
    a.jmp_label(sampled);
    a.bind_text_label(young);
    load_rt(a, R::V1, LAST_OLD);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, YOUNG);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, average);
    a.jmp_label(average_done);
    a.bind_text_label(average);
    load_rt(a, R::V1, EMA);
    a.mov_reg_reg(R::V2, R::V1);
    a.shl_reg_imm8(R::V1, 1);
    a.add_reg_reg(R::V1, R::V2);
    a.add_reg_reg(R::V0, R::V1);
    a.shr_reg_imm8(R::V0, 2); // 3/4 history + 1/4 this Young's Old growth.
    a.bind_text_label(average_done);
    store_rt(a, EMA, R::V0);
    super::copy::bump(a, YOUNG, R::V0);
    a.bind_text_label(sampled);
    store_rt(a, LAST_OLD, R::V6);
    retarget(a);
}

/// Candidate is retained + half remaining promotion-safe headroom, reduced
/// by two Young cycles of smoothed Old growth. Bound it to the configured bounds and
/// retain the previous target when it differs by less than 1 MiB.
pub(super) fn retarget<E: PortableAsm>(a: &mut E) {
    let base = a.create_text_label();
    let min = a.create_text_label();
    let prediction = a.create_text_label();
    let compare = a.create_text_label();
    let choose = a.create_text_label();
    let publish = a.create_text_label();
    let lower = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V6, RETAINED);
    a.cmp_reg_imm32(R::V6, super::geometry::policy_geometry(a).old_max as i32);
    a.jcc_label(C::Below, base);
    a.mov_imm64(R::V6, super::geometry::policy_geometry(a).old_max);
    a.jmp_label(prediction);
    a.bind_text_label(base);
    a.mov_imm64(R::V7, super::geometry::policy_geometry(a).old_max);
    a.sub_reg_reg(R::V7, R::V6);
    a.shr_reg_imm8(R::V7, 1);
    a.add_reg_reg(R::V6, R::V7);
    a.cmp_reg_imm32(R::V6, super::geometry::policy_geometry(a).old_min as i32);
    a.jcc_label(C::AboveOrEqual, prediction);
    a.mov_imm64(R::V6, super::geometry::policy_geometry(a).old_min);
    a.bind_text_label(prediction);
    load_rt(a, R::V7, EMA);
    a.shl_reg_imm8(R::V7, 1);
    a.cmp_reg_imm32(
        R::V7,
        (super::geometry::policy_geometry(a).old_max - super::geometry::policy_geometry(a).old_min)
            as i32,
    );
    a.jcc_label(C::AboveOrEqual, min);
    a.mov_imm64(R::V0, super::geometry::policy_geometry(a).old_max);
    a.sub_reg_reg(R::V0, R::V7);
    a.mov_reg_reg(R::V7, R::V0);
    a.jmp_label(compare);
    a.bind_text_label(min);
    a.mov_imm64(R::V7, super::geometry::policy_geometry(a).old_min);
    a.bind_text_label(compare);
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::Below, choose);
    a.jcc_label(C::Equal, choose);
    a.mov_reg_reg(R::V6, R::V7);
    a.bind_text_label(choose);
    load_rt(a, R::V0, TARGET);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, publish);
    a.cmp_reg_reg(R::V6, R::V0);
    a.jcc_label(C::Below, lower);
    a.mov_reg_reg(R::V1, R::V6);
    a.sub_reg_reg(R::V1, R::V0);
    a.mov_reg_reg(R::V0, R::V1);
    let delta = a.create_text_label();
    a.jmp_label(delta);
    a.bind_text_label(lower);
    a.sub_reg_reg(R::V0, R::V6);
    a.bind_text_label(delta);
    a.cmp_reg_imm32(R::V0, DEADBAND as i32);
    a.jcc_label(C::Below, done);
    a.bind_text_label(publish);
    store_rt(a, TARGET, R::V6);
    super::copy::bump(a, UPDATES, R::V0);
    a.bind_text_label(done);
}

/// A full move changes the Young population. Bound its inherited growth
/// predictor to the new retained headroom, avoiding a self-sustaining loop of
/// full moves with no new promotion observations. V6 (used) is preserved.
fn cap_prediction<E: PortableAsm>(a: &mut E) {
    let empty = a.create_text_label();
    let bounded = a.create_text_label();
    let done = a.create_text_label();
    a.cmp_reg_imm32(R::V6, super::geometry::policy_geometry(a).old_max as i32);
    a.jcc_label(C::AboveOrEqual, empty);
    a.mov_imm64(R::V1, super::geometry::policy_geometry(a).old_max);
    a.sub_reg_reg(R::V1, R::V6);
    a.shr_reg_imm8(R::V1, 2);
    a.jmp_label(bounded);
    a.bind_text_label(empty);
    a.mov_imm64(R::V1, 0);
    a.bind_text_label(bounded);
    load_rt(a, R::V0, EMA);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.jcc_label(C::Equal, done);
    store_rt(a, EMA, R::V1);
    super::copy::bump(a, CLAMPS, R::V0);
    a.bind_text_label(done);
}
