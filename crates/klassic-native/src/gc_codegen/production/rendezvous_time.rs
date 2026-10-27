//! Ticket-serialized handshake timing. The hold ends at END, before resume;
//! neither interval claims to measure the entire global stop or release.
use super::{Labels, OwnerState, Runtime, load_rt, owner_state, participants, store_rt, trace};
use crate::gc_abi::{HandshakeState, ThreadRegistration};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

pub(crate) const TAG: u64 = 17;
pub(crate) const PARK_TAG: u64 = 18;
pub(crate) const PARK_HEADER: &[u8] =
    b"{\"schema_version\":1,\"collector\":\"generational\",\"event\":\"mutator_park\"";
pub(crate) const PARK_FIELDS: [(&[u8], i32); 7] = [
    (b",\"sequence\":", 0),
    (b",\"timestamp_ns\":", 8),
    (b",\"round_id\":", 16),
    (b",\"started_ns\":", 32),
    (b",\"duration_ns\":", 40),
    (b",\"mutator_id\":", 48),
    (b",\"clock_status\":", 56),
];
pub(crate) const HEADER: &[u8] =
    b"{\"schema_version\":1,\"collector\":\"generational\",\"event\":\"rendezvous\"";
pub(crate) const FIELDS: [(&[u8], i32); 7] = [
    (b",\"sequence\":", 0),
    (b",\"timestamp_ns\":", 8),
    (b",\"round_id\":", 16),
    (b",\"time_to_safepoint_ns\":", 32),
    (b",\"hold_until_end_ns\":", 40),
    (b",\"participants\":", 48),
    (b",\"clock_status\":", 56),
];
#[repr(C)]
pub(super) struct State {
    request: u64,
    ready: u64,
    round: u64,
    participants: u64,
}
#[repr(C)]
pub(super) struct Owner {
    start: u64,
    round: u64,
}
const PARK_START: usize = owner_state::offset(offset_of!(OwnerState, rendezvous_time.start));
const PARK_ROUND: usize = owner_state::offset(offset_of!(OwnerState, rendezvous_time.round));
const REQUEST: usize = offset_of!(Runtime, rendezvous_time.request);
const READY: usize = offset_of!(Runtime, rendezvous_time.ready);
const ROUND: usize = offset_of!(Runtime, rendezvous_time.round);
const PARTICIPANTS: usize = offset_of!(Runtime, rendezvous_time.participants);

fn clock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if let Some(worker) = &l.worker {
        a.call_label(worker.hooks.clock);
    } else {
        a.mov_imm64(R::V0, 0);
    }
    validate_clock(a);
}

fn validate_clock<E: PortableAsm>(a: &mut E) {
    let valid = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Greater, valid);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(valid);
}

pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    clock(a, l);
    store_rt(a, REQUEST, R::V0);
    super::store_rt_imm(a, READY, 0);
    load_rt(
        a,
        R::V1,
        participants::HANDSHAKE + offset_of!(HandshakeState, generation),
    );
    store_rt(a, ROUND, R::V1);
    load_rt(
        a,
        R::V1,
        participants::HANDSHAKE + offset_of!(HandshakeState, expected),
    );
    store_rt(a, PARTICIPANTS, R::V1);
    let done = a.create_text_label();
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, done);
    store_rt(a, READY, R::V0);
    a.bind_text_label(done);
}

pub(super) fn ack<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    clock(a, l);
    store_rt(a, PARK_START, R::V0);
    a.mov_reg_reg(R::V9, R::V0);
    load_rt(
        a,
        R::V1,
        participants::HANDSHAKE + offset_of!(HandshakeState, generation),
    );
    store_rt(a, PARK_ROUND, R::V1);
    load_rt(
        a,
        R::V0,
        participants::HANDSHAKE + offset_of!(HandshakeState, acknowledged),
    );
    load_rt(
        a,
        R::V1,
        participants::HANDSHAKE + offset_of!(HandshakeState, expected),
    );
    let done = a.create_text_label();
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, done);
    store_rt(a, READY, R::V9);
    a.bind_text_label(done);
}

pub(super) fn resume<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    resume_with(a, |a| clock(a, l));
}

fn resume_with<E: PortableAsm>(a: &mut E, sample: impl FnOnce(&mut E)) {
    sample(a);
    load_rt(a, R::V7, PARK_START);
    load_rt(a, R::V10, PARK_ROUND);
    load_rt(
        a,
        R::V11,
        participants::OWNER + offset_of!(ThreadRegistration, identity),
    );
    a.mov_imm64(R::V9, 0);
    a.mov_imm64(R::V8, 0);
    let invalid = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V9, R::V0);
    a.sub_reg_reg(R::V9, R::V7);
    a.mov_imm64(R::V8, 3);
    a.bind_text_label(invalid);
    trace::append(
        a,
        PARK_TAG,
        [
            trace::Value::Reg(R::V7),
            trace::Value::Reg(R::V9),
            trace::Value::Reg(R::V11),
        ],
    );
    super::store_rt_imm(a, PARK_START, 0);
    super::store_rt_imm(a, PARK_ROUND, 0);
}

pub(super) fn finish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    finish_with(a, |a| clock(a, l));
}

fn finish_with<E: PortableAsm>(a: &mut E, sample: impl FnOnce(&mut E)) {
    sample(a);
    load_rt(a, R::V1, REQUEST);
    load_rt(a, R::V2, READY);
    a.mov_imm64(R::V7, 0);
    a.mov_imm64(R::V9, 0);
    a.mov_imm64(R::V8, 0);
    let invalid = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, invalid);
    a.mov_reg_reg(R::V7, R::V2);
    a.sub_reg_reg(R::V7, R::V1);
    a.mov_reg_reg(R::V9, R::V0);
    a.sub_reg_reg(R::V9, R::V2);
    a.mov_imm64(R::V8, 7);
    a.bind_text_label(invalid);
    load_rt(a, R::V10, ROUND);
    load_rt(a, R::V11, PARTICIPANTS);
    trace::append(
        a,
        TAG,
        [
            trace::Value::Reg(R::V7),
            trace::Value::Reg(R::V9),
            trace::Value::Reg(R::V11),
        ],
    );
}

#[cfg(test)]
pub(crate) fn test_interval<E: PortableAsm>(a: &mut E, request: u64, ready: u64, end: u64) {
    super::store_rt_imm(a, REQUEST, request);
    super::store_rt_imm(a, READY, ready);
    super::store_rt_imm(a, ROUND, 1);
    super::store_rt_imm(a, PARTICIPANTS, 2);
    finish_with(a, |a| {
        a.mov_imm64(R::V0, end);
        validate_clock(a);
    });
}

#[cfg(test)]
pub(crate) fn test_park_interval<E: PortableAsm>(a: &mut E, start: u64, end: u64) {
    super::store_rt_imm(a, PARK_START, start);
    super::store_rt_imm(a, PARK_ROUND, 1);
    resume_with(a, |a| {
        a.mov_imm64(R::V0, end);
        validate_clock(a);
    });
}
