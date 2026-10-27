//! Constant-time readiness for epoch-based marking, one owner.
//!
//! No heap-wide clearing is needed. Only the owner consumes Ready to publish
//! a new monotone epoch/snapshot; an unstarted request can still be cancelled.
use super::{Labels, load_rt, store_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};

fn eligible<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for flag in [
        super::RT_PHASE_OPEN,
        super::RT_MARK_ACTIVE,
        super::RT_MARK_VALID,
        super::RT_RETIRE_STAGE,
    ] {
        load_rt(a, R::V0, flag);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
}

/// Called by collect with the lock held, before mark drain / moving ranges.
/// An active snapshot is never cancelled by this operation.
pub(super) fn cancel<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    load_rt(a, R::V0, super::RT_MP_STATE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, super::RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_MP_STATE, R::V0);
    super::copy::bump(a, super::RT_MP_CANCELS, R::V0);
    a.bind_text_label(done);
}

/// Normal allocation: request preparation, or consume completed preparation.
/// No heap traversal or initialization wait runs in this trigger.
pub(super) fn trigger<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let skip = a.create_text_label();
    let ready = a.create_text_label();
    load_rt(a, R::V0, super::RT_WORKER_ALIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, skip);
    for flag in [
        super::RT_PHASE_OPEN,
        super::RT_MARK_ACTIVE,
        super::RT_MARK_VALID,
        super::RT_RETIRE_STAGE,
    ] {
        load_rt(a, R::V0, flag);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, skip);
    }
    let pressure = a.create_text_label();
    super::old_policy::large_mark_needed(a, pressure);
    super::start_policy::below(a, skip);
    a.bind_text_label(pressure);
    super::worker::lock(a, l);
    load_rt(a, R::V0, super::RT_MP_STATE);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, ready);
    a.call_label(l.mark_prepare_begin);
    super::worker::unlock(a, l);
    a.jmp_label(skip);
    a.bind_text_label(ready);
    super::worker::unlock(a, l);
    a.call_label(w.mark_start);
    a.bind_text_label(skip);
}

/// Explicit mark-start callers establish metadata readiness before the snapshot.
/// Normal production trigger reaches this with Ready already established.
pub(super) fn ensure<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::worker::lock(a, l);
    a.call_label(l.mark_prepare_begin);
    super::worker::unlock(a, l);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let ready = a.create_text_label();
    a.bind_text_label(l.mark_prepare_begin);
    eligible(a, l);
    load_rt(a, R::V0, super::RT_MP_STATE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, ready);
    a.mov_imm64(R::V0, 2);
    store_rt(a, super::RT_MP_STATE, R::V0);
    super::copy::bump(a, super::RT_MP_REQUESTS, R::V0);
    a.bind_text_label(ready);
    a.ret();
}
