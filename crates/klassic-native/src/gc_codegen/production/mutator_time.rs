//! Sole-mutator collection-call wall time, including lock/resource waits.
//! This is not the N-mutator rendezvous/global-pause measurement contract.
use super::{Labels, load_rt, store_rt, trace};
use crate::gc_codegen::production::{OwnerState, owner_state};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;
pub(crate) const TAG: u64 = 16;
pub(crate) const HEADER: &[u8] = b"{\"schema_version\":1,\"collector\":\"generational\",\"event\":\"mutator_collection\",\"mutator_id\":1";
pub(crate) const FIELDS: [(&[u8], i32); 7] = [
    (b",\"sequence\":", 0),
    (b",\"timestamp_ns\":", 8),
    (b",\"cycle_id\":", 16),
    (b",\"started_ns\":", 32),
    (b",\"duration_ns\":", 40),
    (b",\"start_clock_valid\":", 48),
    (b",\"clock_status\":", 56),
];
#[repr(C)]
pub(super) struct State {
    active: u64,
    start: u64,
    calls: u64,
    valid: u64,
    invalid: u64,
    total: u64,
    max: u64,
    last: u64,
    clock_invalid: u64,
    last_status: u64,
}
macro_rules! field { ($($name:ident:$member:ident),* $(,)?)=> { $(
    pub(crate) const $name:usize=owner_state::offset(offset_of!(OwnerState, mutator_time.$member));
)* }; }
field! {ACTIVE:active,START:start,CALLS:calls,VALID:valid,INVALID:invalid,TOTAL:total,MAX:max,LAST:last,CLOCK_INVALID:clock_invalid,LAST_STATUS:last_status}
pub(crate) fn report_offsets() -> [usize; 8] {
    [
        CALLS,
        VALID,
        INVALID,
        TOTAL,
        MAX,
        LAST,
        CLOCK_INVALID,
        LAST_STATUS,
    ]
}
const SAVED: [R; 10] = [
    R::V0,
    R::V1,
    R::V2,
    R::V6,
    R::V7,
    R::V8,
    R::V9,
    R::V10,
    R::V11,
    R::V3,
];
fn save<E: PortableAsm>(a: &mut E) {
    for r in SAVED {
        a.push_reg(r);
    }
}
fn restore<E: PortableAsm>(a: &mut E) {
    for r in SAVED.into_iter().rev() {
        a.pop_reg(r);
    }
}
fn clock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.clock);
    } else {
        a.mov_imm64(R::V0, 0);
    }
}
fn failed_clock<E: PortableAsm>(a: &mut E) {
    super::copy::bump(a, trace::INVALID, R::V1);
    super::copy::bump(a, CLOCK_INVALID, R::V1);
    a.mov_imm64(R::V0, 0);
}
pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin_with(a, l, |a| clock(a, l));
}
pub(super) fn end<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    end_with(a, l, |a| clock(a, l));
}
fn begin_with<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, sample: impl FnOnce(&mut E)) {
    if !a.gc_timing() {
        return;
    }
    save(a);
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    let invalid = a.create_text_label();
    let sampled = a.create_text_label();
    sample(a);
    trace::check_clock(a, invalid);
    a.jmp_label(sampled);
    a.bind_text_label(invalid);
    failed_clock(a);
    a.bind_text_label(sampled);
    store_rt(a, START, R::V0);
    super::store_rt_imm(a, ACTIVE, 1);
    restore(a);
}
fn end_with<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, sample: impl FnOnce(&mut E)) {
    if !a.gc_timing() {
        return;
    }
    save(a);
    load_rt(a, R::V0, ACTIVE);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    super::store_rt_imm(a, ACTIVE, 0);
    load_rt(a, R::V9, START);
    a.mov_imm64(R::V7, 0);
    a.mov_imm64(R::V8, 0);
    a.mov_imm64(R::V11, 0);
    let no_start = a.create_text_label();
    let invalid_clock = a.create_text_label();
    let invalid_interval = a.create_text_label();
    let sampled = a.create_text_label();
    let maximum = a.create_text_label();
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, no_start);
    a.mov_imm64(R::V11, 1);
    a.bind_text_label(no_start);
    sample(a);
    trace::check_clock(a, invalid_clock);
    a.mov_imm64(R::V8, 1);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, invalid_interval);
    a.mov_reg_reg(R::V7, R::V0);
    a.sub_reg_reg(R::V7, R::V9);
    a.mov_imm64(R::V8, 3);
    super::copy::bump(a, VALID, R::V1);
    load_rt(a, R::V1, TOTAL);
    a.mov_reg_reg(R::V2, R::V1);
    a.add_reg_reg(R::V1, R::V7);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, l.fail);
    a.mov_imm64(R::V2, i64::MAX as u64);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Above, l.fail);
    store_rt(a, TOTAL, R::V1);
    load_rt(a, R::V1, MAX);
    a.cmp_reg_reg(R::V7, R::V1);
    a.jcc_label(C::Below, maximum);
    store_rt(a, MAX, R::V7);
    a.bind_text_label(maximum);
    a.jmp_label(sampled);
    a.bind_text_label(invalid_clock);
    failed_clock(a);
    a.bind_text_label(invalid_interval);
    super::copy::bump(a, INVALID, R::V1);
    a.bind_text_label(sampled);
    super::copy::bump(a, CALLS, R::V1);
    store_rt(a, LAST, R::V7);
    store_rt(a, LAST_STATUS, R::V8);
    if a.gc_json_events() {
        load_rt(a, R::V10, trace::YOUNG);
        trace::append(
            a,
            TAG,
            [
                trace::Value::Reg(R::V9),
                trace::Value::Reg(R::V7),
                trace::Value::Reg(R::V11),
            ],
        );
    }
    restore(a);
}
#[cfg(test)]
pub(crate) fn begin_at<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, ns: u64) {
    begin_with(a, l, |a| a.mov_imm64(R::V0, ns));
}
#[cfg(test)]
pub(crate) fn end_at<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, ns: u64) {
    end_with(a, l, |a| a.mov_imm64(R::V0, ns));
}
