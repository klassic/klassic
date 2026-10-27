//! One owner owns one actual copying attempt; a checked atomic count protects
//! source retirement while independent mutators copy. This is not a phase
//! quorum: moving-bank admission still requires every actor's exclusion.
use super::*;
#[repr(C)]
pub(super) struct Owner {
    source: u64,
    phase: u64,
    bank: u64,
    identity: u64,
}
const OWN: usize = owner_state::offset(offset_of!(OwnerState, copy_claim));
pub(super) const SOURCE: usize = OWN + offset_of!(Owner, source);
const PHASE: usize = OWN + offset_of!(Owner, phase);
const BANK: usize = OWN + offset_of!(Owner, bank);
const IDENTITY: usize = OWN + offset_of!(Owner, identity);

/// Constant-count source exclusion. The count is not a completion hint and
/// cannot be reset by a different copier or by the phase coordinator.
pub(super) fn require_empty<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    lea_rt(a, R::V1, RT_M_COPYING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
pub(super) fn pending<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    lea_rt(a, R::V1, RT_M_COPYING);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
}
pub(super) fn private_closed<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, SOURCE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
/// Runs only after the real forwarder chose an unforwarded source, before
/// allocation, promotion reservation or payload copying. Preserve every reg.
pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, source: usize) {
    quiescence::save(a);
    private_closed(a, l);
    load_rt(a, R::V8, RT_CTX + offset_of!(MutatorContext, thread_id));
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V6, source);
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, l.fail);
    lea_rt(a, R::V7, RT_M_COPYING);
    let retry = a.create_text_label();
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V7);
    a.mov_imm64(R::V1, u64::MAX);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::V9, R::V0);
    a.mov_reg_reg(R::V2, R::V0);
    a.add_reg_imm32(R::V2, 1);
    a.atomic_compare_exchange(R::V7, R::V2);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, retry);
    lea_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    a.atomic_load_acquire(R::V0, R::V1);
    store_rt(a, PHASE, R::V0);
    lea_rt(a, R::V1, allocation_records::REVISION);
    a.atomic_load_acquire(R::V0, R::V1);
    store_rt(a, BANK, R::V0);
    store_rt(a, IDENTITY, R::V8);
    store_rt(a, SOURCE, R::V6);
    #[cfg(test)]
    if let Some(hook) = l.copy_claim_begin_hook {
        a.call_label(hook);
    }
    quiescence::restore(a);
}
/// Both the actual winner and the initialized loser/filler retire exactly
/// their own attempt, after publication is complete. Preserve winner bits.
pub(super) fn complete<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, source: usize) {
    quiescence::save(a);
    load_rt(a, R::V0, SOURCE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, source);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    for (captured, current) in [
        (IDENTITY, RT_CTX + offset_of!(MutatorContext, thread_id)),
        (
            PHASE,
            registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
        ),
        (BANK, allocation_records::REVISION),
    ] {
        load_rt(a, R::V0, captured);
        load_rt(a, R::V1, current);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, l.fail);
    }
    lea_rt(a, R::V7, RT_M_COPYING);
    let retry = a.create_text_label();
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::V9, R::V0);
    a.mov_reg_reg(R::V2, R::V0);
    a.sub_reg_imm8(R::V2, 1);
    a.atomic_compare_exchange(R::V7, R::V2);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, retry);
    for field in [SOURCE, PHASE, BANK, IDENTITY] {
        store_rt_imm(a, field, 0);
    }
    #[cfg(test)]
    if let Some(hook) = l.copy_claim_end_hook {
        a.call_label(hook);
    }
    quiescence::restore(a);
}

#[cfg(test)]
pub(super) fn corrupt_actual_claim(g: &mut crate::NativeCodeGenerator, mode: u64) {
    let field = match mode {
        1 => RT_M_COPYING,
        2 => SOURCE,
        3 => IDENTITY,
        4 => PHASE,
        5 => BANK,
        7 => SOURCE,
        _ => unreachable!(),
    };
    if mode == 1 || mode == 7 {
        store_rt_imm(g, field, 0);
    } else {
        load_rt(g, R::V0, field);
        g.add_reg_imm32(
            R::V0,
            if mode == 2 {
                16
            } else if mode == 4 {
                0x10000
            } else {
                1
            },
        );
        store_rt(g, field, R::V0);
    }
}
