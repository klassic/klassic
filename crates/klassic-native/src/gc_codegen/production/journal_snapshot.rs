//! Captured fixed-bank journal lifetime, not permission to reuse a bank.
//! Old-only enrollment keeps Young fixed. Explicit dual enrollment validates
//! both actual engines while retaining physical bank/remap/remset anchors.
//! Captured stages are stable and their sequence cannot name a future snapshot.
use super::*;
use crate::gc_abi::{Phase, RootRegistry};
const FIXED: i32 = 0x1d0f; // Young stage/epoch, remaps and remembered parity.

/// Ticket held. Preserve V0/V1 and all persistent scratch.
pub(super) fn lifetime<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let fixed_path = a.create_text_label();
    a.push_reg(R::V0);
    load_rt(a, R::V0, moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.pop_reg(R::V0);
    a.jcc_label(C::Equal, fixed_path);
    moving_round::journal::lifetime(a, l);
    let moving_done = a.create_text_label();
    a.jmp_label(moving_done);
    a.bind_text_label(fixed_path);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    load_rt(a, R::V0, allocation_records::REVISION);
    load_rt(a, R::V1, cooperative_old::BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    load_rt(a, R::V1, cooperative_old::SNAPSHOT);
    a.xor_reg_reg(R::V0, R::V1);
    fixed(a, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.bind_text_label(moving_done);
}

/// V0=captured, V1=current snapshot. Preserve both; clobber V2.
/// A retained record may conservatively retain floating garbage in either
/// current mark epoch. No source page/mark bank can be recycled in this mode.
pub(super) fn validate<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let fixed_moving = a.create_text_label();
    let moving_done = a.create_text_label();
    load_rt(a, R::V2, moving_round::ENABLED);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, fixed_moving);
    moving_round::journal::validate(a, l);
    a.jmp_label(moving_done);
    a.bind_text_label(fixed_moving);
    let exact = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V2, cooperative_old::ENABLED);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, exact);
    lifetime(a, l);
    let old_engine = a.create_text_label();
    load_rt(a, R::V2, cooperative_old::YOUNG_ENABLED);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, old_engine);
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    a.mov_reg_reg(R::V2, R::V0);
    a.mov_reg_reg(R::V0, R::V1);
    a.mov_reg_reg(R::V1, R::V2);
    native_io::adoption::dual_stable(a, l);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
    a.bind_text_label(old_engine);
    a.mov_reg_reg(R::V2, R::V0);
    a.and_reg_imm32(R::V2, 0xe000);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V2, R::V0);
    a.xor_reg_reg(R::V2, R::V1);
    fixed(a, R::V2);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    let old_only = a.create_text_label();
    let stable = a.create_text_label();
    load_rt(a, R::V2, cooperative_old::YOUNG_ENABLED);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, old_only);
    a.mov_reg_reg(R::V2, R::V0);
    a.and_reg_imm32(R::V2, 15);
    stable_stage(a, l);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 4);
    a.and_reg_imm32(R::V2, 15);
    stable_stage(a, l);
    a.jmp_label(stable);
    a.bind_text_label(old_only);
    a.mov_reg_reg(R::V2, R::V0);
    a.and_reg_imm32(R::V2, 255);
    for stage in [Phase::Idle, Phase::ConcurrentMark, Phase::Select] {
        a.cmp_reg_imm32(R::V2, (stage as i32) << 4);
        a.jcc_label(C::Equal, stable);
    }
    a.jmp_label(l.fail);
    a.bind_text_label(stable);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, done);
    a.push_reg(R::V1);
    a.shr_reg_imm8(R::V1, 16);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    a.cmp_reg_reg(R::V2, R::V1);
    a.pop_reg(R::V1);
    a.jcc_label(C::AboveOrEqual, l.fail);
    a.jmp_label(done);
    a.bind_text_label(exact);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(done);
    a.bind_text_label(moving_done);
}

/// Called after actual coordinator/stage authentication, with the ticket.
/// Retain work through authenticated start/end-check/reopen. Select requires
/// zero actual obligations. Young needs dual enrollment; movement stays forbidden.
pub(super) fn boundary<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    young: Option<Phase>,
    old: Option<Phase>,
) {
    let ordinary = a.create_text_label();
    let moving_done = a.create_text_label();
    load_rt(a, R::V0, phase_round::KIND);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::Below, ordinary);
    a.jcc_label(C::Equal, ordinary);
    moving_round::require_coordinator(a, l);
    a.jmp_label(moving_done);
    a.bind_text_label(ordinary);
    let legacy = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, cooperative_old::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, legacy);
    lifetime(a, l);
    load_rt(a, R::V0, phase_round::KIND);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    if let Some(stage) = young {
        if !matches!(
            stage,
            Phase::MarkStart | Phase::ConcurrentMark | Phase::MarkEndCheck | Phase::Select
        ) {
            a.jmp_label(l.fail);
        }
        load_rt(a, R::V0, cooperative_old::YOUNG_ENABLED);
        a.cmp_reg_imm8(R::V0, 1);
        a.jcc_label(C::NotEqual, l.fail);
    } else if !matches!(
        old,
        Some(Phase::MarkStart | Phase::ConcurrentMark | Phase::MarkEndCheck | Phase::Select)
    ) {
        a.jmp_label(l.fail);
    }
    if old == Some(Phase::Select) || young == Some(Phase::Select) {
        root_work::require_empty(a, l);
    }
    a.jmp_label(done);
    a.bind_text_label(legacy);
    root_work::seal(a, l);
    a.bind_text_label(done);
    a.bind_text_label(moving_done);
}

/// Only physical bank/remap/remset anchors may remain fixed in dual marking.
/// Disabled heaps keep the stricter original Young-stage/epoch fence.
fn fixed<E: PortableAsm>(a: &mut E, value: R) {
    let old = a.create_text_label();
    let done = a.create_text_label();
    let temp = if value == R::V2 { R::V0 } else { R::V2 };
    a.push_reg(temp);
    load_rt(a, temp, cooperative_old::YOUNG_ENABLED);
    a.test_reg_reg(temp, temp);
    a.jcc_label(C::Equal, old);
    a.and_reg_imm32(value, 0x1c00);
    a.jmp_label(done);
    a.bind_text_label(old);
    a.and_reg_imm32(value, FIXED);
    a.bind_text_label(done);
    a.pop_reg(temp);
}
pub(super) fn stable_stage<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let ok = a.create_text_label();
    for stage in [Phase::Idle, Phase::ConcurrentMark, Phase::Select] {
        a.cmp_reg_imm32(R::V2, stage as i32);
        a.jcc_label(C::Equal, ok);
    }
    a.jmp_label(l.fail);
    a.bind_text_label(ok);
}
