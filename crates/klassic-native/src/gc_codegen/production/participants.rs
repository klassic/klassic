//! Production participant registration and finite phase handshakes.
//! The collector ticket owns this registry. Real Running owners acknowledge
//! their phase boundaries; native scopes are excluded from expected ACKs.
//! Fresh attach waits outside open rounds/physical relocation and joins the
//! current stable snapshot. General detach/storage retirement remains separate.
use super::*;
use crate::gc_abi::{HandshakeState, ThreadRegistration};
use crate::gc_codegen::production::{OwnerState, lea_rt, load_rt, owner_state};

#[repr(C)]
pub(super) struct State {
    handshake: HandshakeState,
}
pub(super) const HANDSHAKE: usize =
    offset_of!(Runtime, participants) + offset_of!(State, handshake);
pub(super) const OWNER: usize = owner_state::offset(offset_of!(OwnerState, participant));

/// Registry lock held, except fresh unpublished startup. Preserve every
/// register other than V0/V1/V2; reject wait/invalid rather than adopting a
/// phase without the registered participants' acknowledgement.
pub(super) fn step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, operation: u64) {
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    lea_rt(a, R::V7, HANDSHAKE);
    lea_rt(a, R::V6, OWNER);
    a.mov_imm64(R::V2, operation);
    a.call_label(l.participant_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    if a.gc_json_events() {
        match operation {
            6 => rendezvous_time::begin(a, l),
            1 => rendezvous_time::ack(a, l),
            7 => rendezvous_time::finish(a, l),
            2 => rendezvous_time::resume(a, l),
            _ => {}
        }
    }
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
}

pub(super) fn startup<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    store_rt_addr(a, OWNER + offset_of!(ThreadRegistration, context), RT_CTX);
    step(a, l, 0);
}

pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    step(a, l, 6);
    step(a, l, 1);
}

/// V0 = proposed snapshot, V1 = adopted-phase address. Both registers are
/// preserved, including on rejection. The collector lock excludes registry
/// mutations throughout readiness validation, publication and round closure.
pub(super) fn adopt<E: PortableAsm>(a: &mut E, failed: E::TextLabel) {
    let rejected = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    // Test readiness BEFORE the caller publishes a new adopted snapshot.
    // End validates again after publication, but cannot be the first gate:
    // another registered owner may still owe this round its acknowledgement.
    load_rt(a, R::V0, HANDSHAKE + offset_of!(HandshakeState, expected));
    load_rt(
        a,
        R::V1,
        HANDSHAKE + offset_of!(HandshakeState, acknowledged),
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, rejected);
    // An empty inactive registry has equal counters too. Require a live
    // rendezvous; registration/ack operations already validated its members.
    load_rt(a, R::V0, HANDSHAKE + offset_of!(HandshakeState, active));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, rejected);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.atomic_store_release(R::V1, R::V0);
    a.jmp_label(done);
    a.bind_text_label(rejected);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.jmp_label(failed);
    a.bind_text_label(done);
}

pub(super) fn finish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    step(a, l, 7);
    step(a, l, 2);
}
