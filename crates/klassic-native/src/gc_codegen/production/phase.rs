//! Independent Young/Old marking and relocation snapshots, one mutator.
//!
//! The owner is at an allocation safepoint and holds the collector lock;
//! neither actor has a private operation in flight. Prior relocation has
//! completed before a new remap parity is published. This is not the general
//! MarkPhaseControl/N-owner adoption or an adaptive dual-mark scheduler.
//! The registered owner's finite handshake brackets every phase adoption.
use super::{Labels, RT_CTX, RT_FULL, load_rt};
use crate::gc_abi::{MAX_OFFSET, MutatorContext, Phase};
use crate::gc_codegen::production::lea_rt;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

/// Clobbers V0..V2; V3 = runtime. All other registers are preserved.
fn publish_masked<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    young: Option<Phase>,
    old: Option<Phase>,
    epoch_flip: u64,
) {
    super::worker_view::require_quiescent(a, l);
    super::copy_claims::require_empty(a, l);
    load_rt(a, R::V0, super::RT_PENDING_REF);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    for field in [
        offset_of!(MutatorContext, private_producers),
        offset_of!(MutatorContext, access_epoch),
    ] {
        load_rt(a, R::V0, RT_CTX + field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    super::phase_round::require_coordinator(a, l);
    super::phase_round::allow_stage(a, l, young, old);
    super::native_io::require_phase_clear(a, l);
    super::journal_snapshot::boundary(a, l, young, old);
    super::phase_round::publish_begin(a, l);
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.atomic_load_acquire(R::V0, R::V1);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    a.push_reg(R::V1);
    a.mov_imm64(R::V1, MAX_OFFSET);
    a.cmp_reg_reg(R::V2, R::V1);
    a.pop_reg(R::V1);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.add_reg_imm32(R::V0, 0x10000);
    let mask = if young.is_some() { 15 } else { 0 } | if old.is_some() { 0xf0 } else { 0 };
    a.and_reg_imm32(R::V0, !mask);
    a.mov_imm64(R::V2, epoch_flip << 8);
    a.xor_reg_reg(R::V0, R::V2);
    let value = young.map_or(0, |p| p as u64) | old.map_or(0, |p| (p as u64) << 4);
    a.mov_imm64(R::V2, value);
    a.or_reg_reg(R::V0, R::V2);
    super::participants::adopt(a, l.fail);
    super::registered_roots::adopted(a);
    super::phase_round::publish_finish(a, l);
    if let Some(stage) = young {
        super::trace::record(a, l, 1, stage);
    }
    if let Some(stage) = old {
        super::trace::record(a, l, 2, stage);
    }
}

fn publish<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    young: Phase,
    old: Phase,
    flip: u64,
) {
    publish_masked(a, l, Some(young), Some(old), flip);
}

pub(super) fn young_marking<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, stage: Phase) {
    publish_masked(
        a,
        l,
        Some(stage),
        None,
        if stage == Phase::MarkStart { 1 } else { 0 },
    );
}

/// The Young semispace always moves; Old moves only in a full collection.
/// A parity flip occurs only at the start, before forwarding roots/fields.
pub(super) fn relocating<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, stage: Phase) {
    let young = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, young);
    publish(
        a,
        l,
        stage,
        stage,
        if stage == Phase::RelocateStart { 12 } else { 0 },
    );
    a.jmp_label(done);
    a.bind_text_label(young);
    let idle = a.create_text_label();
    load_rt(a, R::V0, super::RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, idle);
    publish(
        a,
        l,
        stage,
        Phase::ConcurrentMark,
        if stage == Phase::RelocateStart { 4 } else { 0 },
    );
    a.jmp_label(done);
    a.bind_text_label(idle);
    publish(
        a,
        l,
        stage,
        Phase::Idle,
        if stage == Phase::RelocateStart { 4 } else { 0 },
    );
    a.bind_text_label(done);
}

pub(super) fn idle<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let idle = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, super::RT_MARK_ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, idle);
    publish(a, l, Phase::Idle, Phase::ConcurrentMark, 0);
    a.jmp_label(done);
    a.bind_text_label(idle);
    publish(a, l, Phase::Idle, Phase::Idle, 0);
    a.bind_text_label(done);
}

/// Only the Old phase/mark parity changes at an Old boundary. Young collection
/// owns its phase and remap parity; it does not start or end this snapshot.
pub(super) fn marking<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, stage: Phase) {
    publish_masked(
        a,
        l,
        None,
        Some(stage),
        if stage == Phase::MarkStart { 2 } else { 0 },
    );
}
