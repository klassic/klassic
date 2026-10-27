//! Bounded sole-owner phase/collection history. No formatting, I/O or allocation at capture.
//! Flushed after the exit verifier, outside the collector lock. Not global-pause timing.
use super::{Labels, Runtime, d, layout, load_rt, store_rt};
use crate::gc_abi::Phase;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;
pub(crate) const CAPACITY: u64 = 131072;
pub(crate) const RECORD_BYTES: u64 = 64;
pub(crate) const HEADER: &[u8] =
    b"{\"schema_version\":1,\"collector\":\"generational\",\"event\":\"phase_transition\"";
pub(crate) const FIELDS: [(&[u8], i32); 8] = [
    (b",\"sequence\":", 0),
    (b",\"timestamp_ns\":", 8),
    (b",\"cycle_id\":", 16),
    (b",\"generation\":", 24),
    (b",\"phase\":", 32),
    (b",\"previous_phase\":", 40),
    (b",\"elapsed_phase_ns\":", 48),
    (b",\"clock_status\":", 56),
];
#[repr(C)]
pub(super) struct State {
    count: u64,
    dropped: u64,
    invalid: u64,
    capacity: u64,
    next_cycle: u64,
    young_cycle: u64,
    old_cycle: u64,
    young_phase: u64,
    old_phase: u64,
    last_ns: u64,
    young_ns: u64,
    old_ns: u64,
}
macro_rules! field { ($($name:ident:$member:ident),* $(,)?)=> { $(
    pub(crate) const $name:usize=offset_of!(Runtime,trace.$member);
)* }; }
field! { COUNT:count,DROPPED:dropped,INVALID:invalid,CAP:capacity,NEXT:next_cycle,
YOUNG:young_cycle,OLD:old_cycle,YPHASE:young_phase,OPHASE:old_phase,
LAST:last_ns,YNS:young_ns,ONS:old_ns }
pub(crate) fn buffer_offset<E: PortableAsm>(a: &E) -> i32 {
    d(layout(a).events as usize)
}
pub(crate) fn report_offsets() -> [i32; 4] {
    [d(COUNT), d(DROPPED), d(INVALID), d(CAP)]
}
pub(super) fn startup<E: PortableAsm>(a: &mut E) {
    if a.gc_json_events() {
        super::store_rt_imm(a, CAP, a.gc_event_capacity());
        // Prefault only the optional history, before application execution.
        // Event publication itself never maps or allocates collector storage.
        super::lea_rt(a, R::V1, layout(a).events as usize);
        a.mov_reg_reg(R::V2, R::V1);
        a.add_reg_imm32(R::V2, (a.gc_event_capacity() * RECORD_BYTES) as i32);
        a.mov_imm64(R::V0, 0);
        let again = a.create_text_label();
        a.bind_text_label(again);
        a.store_ptr_disp32(R::V1, 0, R::V0);
        a.add_reg_imm32(R::V1, a.gc_metadata_alignment() as i32);
        a.cmp_reg_reg(R::V1, R::V2);
        a.jcc_label(C::Below, again);
    }
}
pub(super) fn checked_total(base: u64, capacity: u64) -> Result<u64, &'static str> {
    capacity
        .checked_mul(RECORD_BYTES)
        .and_then(|n| base.checked_add(n))
        .filter(|&n| n <= i32::MAX as u64)
        .ok_or("JSON GC event history requires metadata beyond direct-emitter address limits")
}
pub(crate) fn validate_layout(payload: u64, alignment: u64) -> Result<(), &'static str> {
    checked_total(
        super::round_up(super::geometry::build_layout(payload).total, alignment),
        CAPACITY,
    )
    .map(|_| ())
}
/// Raw V0 timestamp -> validated positive signed-range monotonic value.
/// Updates the common last-clock sample; V1 and flags clobbered.
pub(super) fn check_clock<E: PortableAsm>(a: &mut E, invalid: E::TextLabel) {
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::LessEqual, invalid);
    load_rt(a, R::V1, LAST);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, invalid);
    store_rt(a, LAST, R::V0);
}
pub(super) enum Value {
    Literal(u64),
    Reg(R),
}
/// V0 timestamp, V10 cycle, V8 clock status; payload32/40/48.
/// Shares the same bounded reservation for phase and mutator interval records.
pub(super) fn append<E: PortableAsm>(a: &mut E, tag: u64, values: [Value; 3]) {
    let overflow = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V2, COUNT);
    a.cmp_reg_imm32(R::V2, a.gc_event_capacity() as i32);
    a.jcc_label(C::AboveOrEqual, overflow);
    a.mov_reg_reg(R::V6, R::V2);
    a.shl_reg_imm8(R::V6, 6);
    a.add_reg_reg(R::V6, R::V3);
    a.add_reg_imm32(R::V6, buffer_offset(a));
    a.add_reg_imm32(R::V2, 1);
    a.store_ptr_disp32(R::V6, 0, R::V2);
    a.store_ptr_disp32(R::V6, 8, R::V0);
    a.store_ptr_disp32(R::V6, 16, R::V10);
    a.mov_imm64(R::V1, tag);
    a.store_ptr_disp32(R::V6, 24, R::V1);
    for (i, value) in values.into_iter().enumerate() {
        let reg = match value {
            Value::Literal(n) => {
                a.mov_imm64(R::V1, n);
                R::V1
            }
            Value::Reg(r) => r,
        };
        a.store_ptr_disp32(R::V6, 32 + i as i32 * 8, reg);
    }
    a.store_ptr_disp32(R::V6, 56, R::V8);
    store_rt(a, COUNT, R::V2);
    a.jmp_label(done);
    a.bind_text_label(overflow);
    super::copy::bump(a, DROPPED, R::V1);
    a.bind_text_label(done);
}
/// Phase publication already serialized the sole owner with the worker.
/// V3=runtime; only V0..V2 and flags clobbered. Repeated phases are not events.
pub(super) fn record<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    generation: u64,
    stage: Phase,
) {
    capture(a, l, generation, stage, |a| {
        if let Some(w) = &l.worker {
            a.call_label(w.hooks.clock);
        } else {
            a.mov_imm64(R::V0, 0);
        }
    });
}
fn capture<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    generation: u64,
    stage: Phase,
    clock: impl FnOnce(&mut E),
) {
    if !a.gc_json_events() {
        return;
    }
    assert!(generation == 1 || generation == 2);
    let (cycle, phase, stamp) = if generation == 1 {
        (YOUNG, YPHASE, YNS)
    } else {
        (OLD, OPHASE, ONS)
    };
    let done = a.create_text_label();
    let have_cycle = a.create_text_label();
    let new_cycle = a.create_text_label();
    let invalid = a.create_text_label();
    let sampled = a.create_text_label();
    let elapsed = a.create_text_label();
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    load_rt(a, R::V9, phase);
    a.cmp_reg_imm8(R::V9, stage as i8);
    a.jcc_label(C::Equal, done);
    super::store_rt_imm(a, phase, stage as u64);
    load_rt(a, R::V10, cycle);
    if stage == Phase::MarkStart {
        a.jmp_label(new_cycle);
    }
    a.test_reg_reg(R::V10, R::V10);
    a.jcc_label(C::NotEqual, have_cycle);
    a.bind_text_label(new_cycle);
    load_rt(a, R::V10, NEXT);
    a.mov_imm64(R::V1, i64::MAX as u64);
    a.cmp_reg_reg(R::V10, R::V1);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.add_reg_imm32(R::V10, 1);
    store_rt(a, NEXT, R::V10);
    store_rt(a, cycle, R::V10);
    a.bind_text_label(have_cycle);
    clock(a);
    a.mov_imm64(R::V7, 0);
    a.mov_imm64(R::V8, 0);
    check_clock(a, invalid);
    a.mov_imm64(R::V8, 1); // timestamp valid
    load_rt(a, R::V11, stamp);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, elapsed);
    a.mov_reg_reg(R::V7, R::V0);
    a.sub_reg_reg(R::V7, R::V11);
    a.mov_imm64(R::V8, 3); // timestamp and preceding-phase interval valid
    a.bind_text_label(elapsed);
    a.jmp_label(sampled);
    a.bind_text_label(invalid);
    super::copy::bump(a, INVALID, R::V1);
    a.mov_imm64(R::V0, 0); // explicitly invalid, never counted as a measured zero
    a.bind_text_label(sampled);
    store_rt(a, stamp, R::V0);
    append(
        a,
        generation,
        [
            Value::Literal(stage as u64),
            Value::Reg(R::V9),
            Value::Reg(R::V7),
        ],
    );
    if stage == Phase::Idle {
        super::store_rt_imm(a, cycle, 0);
    }
    a.bind_text_label(done);
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
}
#[cfg(test)]
pub(crate) fn record_at<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    generation: u64,
    stage: Phase,
    ns: u64,
) {
    capture(a, l, generation, stage, |a| a.mov_imm64(R::V0, ns));
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn json_phase_history_checks_metadata_total_without_wrapping() {
        let bytes = CAPACITY * RECORD_BYTES;
        let edge = i32::MAX as u64 - bytes;
        assert_eq!(checked_total(edge, CAPACITY), Ok(i32::MAX as u64));
        assert!(checked_total(edge + 1, CAPACITY).is_err());
        assert!(checked_total(u64::MAX, CAPACITY).is_err());
        assert!(checked_total(0, u64::MAX).is_err());
        for p in [16u64, 64, 256, 512].map(|m| m << 20) {
            assert!(validate_layout(p, 16384).is_ok());
        }
    }
}
