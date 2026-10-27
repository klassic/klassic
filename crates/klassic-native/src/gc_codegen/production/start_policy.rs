//! Collector-lock protected mark-start feedback. Rates include scheduling and allocation
//! stalls: they are wall-time rates, not worker CPU throughput measurements.
use super::{Labels, Runtime, load_rt, store_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;
pub(super) const MIN: u64 = 8;
pub(super) const MAX: u64 = super::HALF * 3 / 4;
const WINDOW: u64 = 1 << 20;
const MAX_RATE: u64 = 1 << 30; // bytes or work units per millisecond
#[repr(C)]
pub(super) struct State {
    target: u64,
    rate_ns: u64,
    rate_bytes: u64,
    alloc_rate: u64,
    rate_samples: u64,
    mark_ns: u64,
    mark_base: u64,
    mark_rate: u64,
    mark_units: u64,
    duration_ns: u64,
    mark_samples: u64,
    observed: u64,
    invalid: u64,
    updates: u64,
    up: u64,
    down: u64,
}
macro_rules! field { ($($name:ident:$member:ident),* $(,)?)=> { $(
    pub(super) const $name:usize=offset_of!(Runtime,start_policy.$member);
)* }; }
field! { TARGET:target,RATE_NS:rate_ns,RATE_BYTES:rate_bytes,ALLOC_RATE:alloc_rate,
RATE_SAMPLES:rate_samples,MARK_NS:mark_ns,MARK_BASE:mark_base,MARK_RATE:mark_rate,
MARK_UNITS:mark_units,DURATION_NS:duration_ns,MARK_SAMPLES:mark_samples,OBSERVED:observed,INVALID:invalid,UPDATES:updates,UP:up,DOWN:down }
fn store<E: PortableAsm>(a: &mut E, f: usize, n: u64) {
    a.mov_imm64(R::V0, n);
    store_rt(a, f, R::V0);
}
/// Bound pages in both allocation chains, sampled from one packed CAS word.
/// Only called in worker builds; the owner alone changes the space base/end.
pub(super) fn below<E: PortableAsm>(a: &mut E, skip: E::TextLabel) {
    super::lea_rt(a, R::V2, super::RT_BOUNDS);
    a.atomic_load_acquire(R::V0, R::V2);
    a.mov_reg_reg(R::V1, R::V0);
    a.shl_reg_imm8(R::V0, 32);
    a.shr_reg_imm8(R::V0, 32);
    a.shr_reg_imm8(R::V1, 32);
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, super::RT_SPACE_END);
    a.add_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, super::RT_SPACE_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, TARGET);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, skip);
}
/// V0 = nonnegative work/bytes, V1 = positive elapsed ns -> bounded rate V0.
/// A wide product never reaches div_unsigned until its high half is zero.
fn rate<E: PortableAsm>(a: &mut E, overflow_rate: u64) {
    let overflow = a.create_text_label();
    let cap = a.create_text_label();
    let done = a.create_text_label();
    a.mov_imm64(R::V8, 1_000_000);
    a.mul_unsigned_wide(R::V8);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, overflow);
    a.div_unsigned(R::V1);
    a.mov_imm64(R::V8, MAX_RATE);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::Above, cap);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    a.mov_imm64(R::V0, 1); // Positive work cannot disappear by rounding.
    a.jmp_label(done);
    a.bind_text_label(overflow);
    a.mov_imm64(R::V0, overflow_rate);
    a.jmp_label(done);
    a.bind_text_label(cap);
    a.mov_imm64(R::V0, MAX_RATE);
    a.bind_text_label(done);
}
fn smooth<E: PortableAsm>(a: &mut E, f: usize) {
    let first = a.create_text_label();
    load_rt(a, R::V1, f);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, first);
    a.mov_reg_reg(R::V2, R::V1);
    a.shl_reg_imm8(R::V1, 1);
    a.add_reg_reg(R::V1, R::V2);
    a.add_reg_reg(R::V0, R::V1);
    a.shr_reg_imm8(R::V0, 2);
    a.bind_text_label(first);
    store_rt(a, f, R::V0);
}
/// After a fresh allocation, at most one clock read per MiB. Copies are not
/// application allocation. Preserve the canonical allocation result V0.
pub(super) fn poll<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else { return };
    let done = a.create_text_label();
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    load_rt(a, R::V0, super::RT_WORKER_ALIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, super::old_policy::ALLOCATED);
    load_rt(a, R::V1, RATE_BYTES);
    a.cmp_reg_reg(R::V0, R::V1);
    let sample = a.create_text_label();
    a.jcc_label(C::Below, sample); // Counter wrap rebases the next window.
    a.sub_reg_reg(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, WINDOW as i32);
    a.jcc_label(C::Below, done);
    a.bind_text_label(sample);
    // Many allocating owners may reach one sampling window together. Only
    // serialize the infrequent feedback update, not their allocation path.
    super::worker::lock(a, l);
    let unlock = a.create_text_label();
    let due = a.create_text_label();
    load_rt(a, R::V0, super::old_policy::ALLOCATED);
    load_rt(a, R::V1, RATE_BYTES);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, due);
    a.sub_reg_reg(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, WINDOW as i32);
    a.jcc_label(C::Below, unlock);
    a.bind_text_label(due);
    a.call_label(w.hooks.clock);
    allocation_sample(a);
    a.bind_text_label(unlock);
    super::worker::unlock(a, l);
    a.bind_text_label(done);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
}
/// V0 = observed timestamp. The numerical harness uses the same emitter.
pub(super) fn allocation_sample<E: PortableAsm>(a: &mut E) {
    let rebase = a.create_text_label();
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    a.mov_reg_reg(R::V6, R::V0);
    load_rt(a, R::V1, RATE_NS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, rebase);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    a.jcc_label(C::Equal, invalid);
    a.sub_reg_reg(R::V0, R::V1);
    a.mov_reg_reg(R::V1, R::V0);
    load_rt(a, R::V0, super::old_policy::ALLOCATED);
    load_rt(a, R::V2, RATE_BYTES);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, invalid);
    a.jcc_label(C::Equal, invalid);
    a.sub_reg_reg(R::V0, R::V2);
    rate(a, MAX_RATE);
    smooth(a, ALLOC_RATE);
    super::copy::bump(a, RATE_SAMPLES, R::V0);
    a.jmp_label(rebase);
    a.bind_text_label(invalid);
    super::copy::bump(a, INVALID, R::V0);
    a.bind_text_label(rebase);
    store_rt(a, RATE_NS, R::V6);
    load_rt(a, R::V0, super::old_policy::ALLOCATED);
    store_rt(a, RATE_BYTES, R::V0);
    a.bind_text_label(done);
}
/// Snapshot start, lock held. Shared samples/feedback use the collector lock.
pub(super) fn begin_mark<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else { return };
    a.call_label(w.hooks.clock);
    store_rt(a, MARK_NS, R::V0);
    load_rt(a, R::V0, super::young_mark::WORKER_UNITS);
    store_rt(a, MARK_BASE, R::V0);
}
pub(super) fn end_mark<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else { return };
    a.call_label(w.hooks.clock);
    mark_sample(a);
}
/// Wall time through sealing includes Old interleaving, scheduler delay and
/// late SATB reopens. Direct owner tracing is not a worker-throughput sample.
pub(super) fn mark_sample<E: PortableAsm>(a: &mut E) {
    let invalid = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V1, MARK_NS);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    a.jcc_label(C::Equal, invalid);
    a.sub_reg_reg(R::V0, R::V1);
    a.mov_reg_reg(R::V7, R::V0);
    a.mov_reg_reg(R::V1, R::V0);
    load_rt(a, R::V0, super::young_mark::WORKER_UNITS);
    load_rt(a, R::V2, MARK_BASE);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, invalid);
    a.jcc_label(C::Equal, invalid);
    a.sub_reg_reg(R::V0, R::V2);
    store_rt(a, MARK_UNITS, R::V0);
    store_rt(a, DURATION_NS, R::V7);
    rate(a, 1);
    smooth(a, MARK_RATE);
    super::copy::bump(a, MARK_SAMPLES, R::V0);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    super::copy::bump(a, INVALID, R::V0);
    a.bind_text_label(done);
    store(a, MARK_NS, 0); // One sealed snapshot cannot be observed twice.
}
/// Called after winning copy bytes were folded, while the owner holds the
/// closing lock. Target is stable until the next relocation closes.
pub(super) fn observe<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    load_rt(a, R::V0, MARK_SAMPLES);
    load_rt(a, R::V1, OBSERVED);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, done);
    store_rt(a, OBSERVED, R::V0);
    for f in [ALLOC_RATE, MARK_RATE, MARK_UNITS] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, done);
    }
    super::capacity::bytes(a, R::V7);
    let minimum = a.create_text_label();
    let candidate = a.create_text_label();
    // Two previous workloads cover rate/model variation. Round time upward.
    load_rt(a, R::V0, MARK_UNITS);
    a.mov_imm64(R::V1, u64::MAX / 2);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, minimum);
    a.shl_reg_imm8(R::V0, 1);
    load_rt(a, R::V1, MARK_RATE);
    a.div_unsigned(R::V1);
    a.test_reg_reg(R::V2, R::V2);
    let rounded = a.create_text_label();
    a.jcc_label(C::Equal, rounded);
    a.add_reg_imm32(R::V0, 1);
    a.bind_text_label(rounded);
    a.mov_reg_reg(R::V6, R::V0);
    load_rt(a, R::V0, DURATION_NS);
    a.mov_imm64(R::V1, u64::MAX / 2);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, minimum);
    a.shl_reg_imm8(R::V0, 1);
    a.mov_imm64(R::V1, 1_000_000);
    a.div_unsigned(R::V1);
    a.test_reg_reg(R::V2, R::V2);
    let duration_rounded = a.create_text_label();
    a.jcc_label(C::Equal, duration_rounded);
    a.add_reg_imm32(R::V0, 1);
    a.bind_text_label(duration_rounded);
    a.cmp_reg_reg(R::V0, R::V6);
    let predicted = a.create_text_label();
    a.jcc_label(C::AboveOrEqual, predicted);
    a.mov_reg_reg(R::V0, R::V6);
    a.bind_text_label(predicted);
    load_rt(a, R::V1, ALLOC_RATE);
    a.mul_unsigned_wide(R::V1);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, minimum);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::AboveOrEqual, minimum);
    load_rt(a, R::V1, super::tenuring::LAST_YOUNG);
    a.cmp_reg_imm32(R::V1, 2 << 20);
    let reserved = a.create_text_label();
    a.jcc_label(C::AboveOrEqual, reserved);
    a.mov_imm64(R::V1, 2 << 20);
    a.bind_text_label(reserved);
    a.cmp_reg_reg(R::V1, R::V7);
    a.jcc_label(C::AboveOrEqual, minimum);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::AboveOrEqual, minimum);
    a.mov_reg_reg(R::V2, R::V7);
    a.sub_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 17);
    a.cmp_reg_imm32(R::V2, MIN as i32);
    a.jcc_label(C::Below, minimum);
    a.cmp_reg_imm32(R::V2, MAX as i32);
    a.jcc_label(C::Below, candidate);
    a.jcc_label(C::Equal, candidate);
    a.mov_imm64(R::V2, MAX);
    a.jmp_label(candidate);
    a.bind_text_label(minimum);
    a.mov_imm64(R::V2, MIN);
    a.bind_text_label(candidate);
    limit(a, R::V1);
    a.cmp_reg_reg(R::V2, R::V1);
    let within = a.create_text_label();
    a.jcc_label(C::Below, within);
    a.jcc_label(C::Equal, within);
    a.mov_reg_reg(R::V2, R::V1);
    a.bind_text_label(within);
    adjust(a);
    a.bind_text_label(done);
}
fn adjust<E: PortableAsm>(a: &mut E) {
    let lower = a.create_text_label();
    let neutral = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V1, TARGET);
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, lower);
    a.mov_reg_reg(R::V0, R::V2);
    a.sub_reg_reg(R::V0, R::V1);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::Below, neutral);
    a.jcc_label(C::Equal, neutral);
    store(a, DOWN, 0);
    super::copy::bump(a, UP, R::V0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Below, done);
    a.add_reg_imm32(R::V1, 16);
    let up = a.create_text_label();
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, up);
    a.mov_reg_reg(R::V2, R::V1);
    a.bind_text_label(up);
    let changed = a.create_text_label();
    a.jmp_label(changed);
    a.bind_text_label(lower);
    a.mov_reg_reg(R::V0, R::V1);
    a.sub_reg_reg(R::V0, R::V2);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::Below, neutral);
    a.jcc_label(C::Equal, neutral);
    store(a, UP, 0);
    super::copy::bump(a, DOWN, R::V0);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Below, done);
    let down = a.create_text_label();
    a.cmp_reg_imm8(R::V1, (MIN + 16) as i8);
    a.jcc_label(C::Below, down);
    a.sub_reg_imm8(R::V1, 16);
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Above, down);
    a.mov_reg_reg(R::V2, R::V1);
    a.bind_text_label(down);
    a.bind_text_label(changed);
    store_rt(a, TARGET, R::V2);
    super::copy::bump(a, UPDATES, R::V0);
    a.bind_text_label(neutral);
    store(a, UP, 0);
    store(a, DOWN, 0);
    a.bind_text_label(done);
}

fn limit<E: PortableAsm>(a: &mut E, reg: R) {
    load_rt(a, reg, super::capacity::ACTIVE);
    a.mov_reg_reg(R::V0, reg);
    a.shl_reg_imm8(reg, 1);
    a.add_reg_reg(reg, R::V0);
    a.shr_reg_imm8(reg, 2);
}
pub(super) fn cap_to_space<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    limit(a, R::V1);
    load_rt(a, R::V0, TARGET);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.jcc_label(C::Equal, done);
    store_rt(a, TARGET, R::V1);
    super::copy::bump(a, UPDATES, R::V0);
    a.bind_text_label(done);
}
