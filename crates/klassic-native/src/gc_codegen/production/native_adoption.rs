//! Fixed-bank mark adoption by the excluded owner itself, under the ticket.
//! Dual enrollment validates both engines and their actual epoch parities.
//! Native entry sealed every owned journal and closed the TLAB. Old rounds
//! may instead retain captured journals and suspend the actual TLAB span.
//! Explicit root-only return also admits one actual, still-open Young change,
//! or completed Young/Old movement through native_completion. Root scopes and
//! actual completed census/retirement authorize the latter, including repeated
//! bank/color reuse. This does not authorize Running-peer moving admission.
use super::*;
#[path = "native_young_return.rs"]
mod young_return;

pub(in super::super) fn private_closed<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    copy_claims::private_closed(a, l);
    for f in [
        RT_COLLECTING,
        RT_PENDING_REF,
        RT_CTX + offset_of!(MutatorContext, private_producers),
        RT_CTX + offset_of!(MutatorContext, access_epoch),
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
}
pub(super) fn return_snapshot<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let fixed_mode = a.create_text_label();
    let moving_done = a.create_text_label();
    load_rt(a, R::V0, moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, fixed_mode);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    load_rt(a, R::V1, RT_RETIRE_STAGE);
    a.or_reg_reg(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    let moving_return = a.create_text_label();
    a.jcc_label(C::NotEqual, moving_return);
    load_rt(a, R::V0, BANK);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, moving_return);
    load_rt(a, R::V0, PHASE);
    a.and_reg_imm32(R::V0, 15);
    a.cmp_reg_imm32(R::V0, 6);
    a.jcc_label(C::Below, fixed_mode);
    a.bind_text_label(moving_return);
    moving_round::native::adopt(a, l);
    a.jmp_label(moving_done);
    a.bind_text_label(fixed_mode);
    let fixed = a.create_text_label();
    let changed = a.create_text_label();
    let completed = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, allocation_records::REVISION);
    load_rt(a, R::V1, BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, changed);
    // An explicit scope may also enter during relocation and return after its
    // completion without another bank change. Such a return needs the census
    // receipt, rather than the fixed-bank mark-only adoption rules.
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, fixed);
    load_rt(a, R::V0, PHASE);
    a.and_reg_imm32(R::V0, 255);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, fixed);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    a.and_reg_imm32(R::V0, 255);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, completed);
    a.jmp_label(fixed);
    a.bind_text_label(changed);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, completed);
    young_return::adopt(a, l);
    a.jmp_label(done);
    a.bind_text_label(completed);
    native_completion::adopt(a, l);
    a.jmp_label(done);
    a.bind_text_label(fixed);
    adopt_snapshot(a, l, PHASE, BANK, ADOPTIONS);
    a.bind_text_label(done);
    a.bind_text_label(moving_done);
}
pub(in super::super) fn adopt_snapshot<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    phase: usize,
    bank: usize,
    adoptions: usize,
) {
    private_closed(a, l);
    for f in [
        offset_of!(MutatorContext, tlab_start),
        offset_of!(MutatorContext, tlab_top),
        offset_of!(MutatorContext, tlab_end),
    ] {
        load_rt(a, R::V0, RT_CTX + f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    // The cached old snapshot must be exactly the one this scope entered with.
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    load_rt(a, R::V1, phase);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, allocation_records::REVISION);
    load_rt(a, R::V1, bank);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    a.atomic_load_acquire(R::V0, R::V1);
    load_rt(a, R::V1, phase);
    let legacy = a.create_text_label();
    load_rt(a, R::V2, cooperative_old::YOUNG_ENABLED);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, legacy);
    journal_snapshot::lifetime(a, l);
    dual_stable(a, l);
    a.bind_text_label(legacy);
    let done = a.create_text_label();
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, done);
    let old_only = a.create_text_label();
    let sequence = a.create_text_label();
    load_rt(a, R::V2, cooperative_old::YOUNG_ENABLED);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, old_only);
    a.jmp_label(sequence);
    a.bind_text_label(old_only);
    // Only stable nonmoving Old stages may bridge this excluded scope.
    a.mov_reg_reg(R::V2, R::V1);
    a.and_reg_imm32(R::V2, 255);
    let entered = a.create_text_label();
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, entered);
    a.cmp_reg_imm32(R::V2, (crate::gc_abi::Phase::ConcurrentMark as i32) << 4);
    a.jcc_label(C::Equal, entered);
    a.cmp_reg_imm32(R::V2, (crate::gc_abi::Phase::Select as i32) << 4);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(entered);
    a.mov_reg_reg(R::V2, R::V0);
    a.and_reg_imm32(R::V2, 255);
    let concurrent = a.create_text_label();
    let ready = a.create_text_label();
    a.cmp_reg_imm32(R::V2, (crate::gc_abi::Phase::ConcurrentMark as i32) << 4);
    a.jcc_label(C::Equal, concurrent);
    a.cmp_reg_imm32(R::V2, (crate::gc_abi::Phase::Select as i32) << 4);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V2, RT_MARK_ACTIVE);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V2, RT_MARK_VALID);
    a.cmp_reg_imm32(R::V2, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.jmp_label(ready);
    a.bind_text_label(concurrent);
    load_rt(a, R::V2, RT_MARK_ACTIVE);
    a.cmp_reg_imm32(R::V2, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(ready);
    load_rt(a, R::V2, RT_MARK_CYCLES);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, l.fail);
    // The Young epoch and both remaps remain unchanged.
    a.mov_reg_reg(R::V2, R::V0);
    a.xor_reg_reg(R::V2, R::V1);
    a.and_reg_imm32(R::V2, 0xd00);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(sequence);
    a.push_reg(R::V0);
    a.shr_reg_imm8(R::V0, 16);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_reg(R::V0, R::V1);
    a.pop_reg(R::V0);
    a.jcc_label(C::Below, l.fail);
    a.jcc_label(C::Equal, l.fail);
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.atomic_store_release(R::V1, R::V0);
    // Revalidate the bank reservation before this actual owner resumes access.
    allocation_records::adopt(a, l);
    copy::bump(a, adoptions, R::V0);
    a.bind_text_label(done);
}

/// Current/entered stable stage pairs, actual generation engine state and fixed
/// physical anchors must all agree before a real excluded owner adopts.
pub(in super::super) fn dual_stable<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.mov_reg_reg(R::V2, R::V0);
    a.or_reg_reg(R::V2, R::V1);
    a.and_reg_imm32(R::V2, 0xe000);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    for shift in [0, 4] {
        a.mov_reg_reg(R::V2, R::V1);
        a.shr_reg_imm8(R::V2, shift);
        a.and_reg_imm32(R::V2, 15);
        journal_snapshot::stable_stage(a, l);
        a.mov_reg_reg(R::V2, R::V0);
        a.shr_reg_imm8(R::V2, shift);
        a.and_reg_imm32(R::V2, 15);
        journal_snapshot::stable_stage(a, l);
        let idle = a.create_text_label();
        let concurrent = a.create_text_label();
        let checked = a.create_text_label();
        a.cmp_reg_imm8(R::V2, 0);
        a.jcc_label(C::Equal, idle);
        a.cmp_reg_imm8(R::V2, 2);
        a.jcc_label(C::Equal, concurrent);
        let (active, valid, epoch) = if shift == 0 {
            (young_mark::ACTIVE, young_mark::VALID, young_mark::EPOCH)
        } else {
            (RT_MARK_ACTIVE, RT_MARK_VALID, RT_MARK_CYCLES)
        };
        load_rt(a, R::V2, active);
        a.test_reg_reg(R::V2, R::V2);
        a.jcc_label(C::NotEqual, l.fail);
        load_rt(a, R::V2, valid);
        a.cmp_reg_imm8(R::V2, 1);
        a.jcc_label(C::NotEqual, l.fail);
        a.jmp_label(checked);
        a.bind_text_label(concurrent);
        load_rt(a, R::V2, active);
        a.cmp_reg_imm8(R::V2, 1);
        a.jcc_label(C::NotEqual, l.fail);
        a.bind_text_label(checked);
        load_rt(a, R::V2, epoch);
        a.test_reg_reg(R::V2, R::V2);
        a.jcc_label(C::Equal, l.fail);
        let done = a.create_text_label();
        a.jmp_label(done);
        a.bind_text_label(idle);
        for field in [active, valid] {
            load_rt(a, R::V2, field);
            a.test_reg_reg(R::V2, R::V2);
            a.jcc_label(C::NotEqual, l.fail);
        }
        a.bind_text_label(done);
        load_rt(a, R::V2, epoch);
        a.and_reg_imm32(R::V2, 1);
        a.push_reg(R::V0);
        a.shr_reg_imm8(R::V0, 8 + shift / 4);
        a.and_reg_imm32(R::V0, 1);
        a.cmp_reg_reg(R::V0, R::V2);
        a.pop_reg(R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.mov_reg_reg(R::V2, R::V0);
    a.xor_reg_reg(R::V2, R::V1);
    a.and_reg_imm32(R::V2, 0x1c00);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
}
