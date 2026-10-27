//! The worker never borrows a mutator's private storage or adopted snapshot.
//! This zero-initialized view is not registered and has no producer identity,
//! root segment or TLAB. Common tracing/copying leaves use only its phase cache.
//! Each quantum refreshes that cache under the same ticket as phase publication.
use super::*;
use crate::gc_abi::RootRegistry;

pub(super) fn bind<E: PortableAsm>(a: &mut E) {
    // OS callbacks and park leaves retain the actual worker-slot pointer.
    // Loading the ordinary runtime entry would bind the primary mutator.
    load_rt(
        a,
        R::V3,
        owner_state::offset(offset_of!(OwnerState, shared_runtime)),
    );
}

/// Only exact stable slot addresses identify workers. Registered mutators and
/// native callers use the same raw ticket without acquiring a worker epoch.
pub(super) fn only_worker<E: PortableAsm>(a: &mut E, other: E::TextLabel) {
    let found = a.create_text_label();
    for index in 0..worker_pool::CAPACITY {
        lea_rt(a, R::V0, worker_pool::slot(index));
        a.cmp_reg_reg(R::OwnerContext, R::V0);
        a.jcc_label(C::Equal, found);
    }
    a.jmp_label(other);
    a.bind_text_label(found);
}

/// Branch for either real pool slot, leaving ordinary owners on the fallthrough.
pub(super) fn branch_worker<E: PortableAsm>(a: &mut E, target: E::TextLabel) {
    for index in 0..worker_pool::CAPACITY {
        lea_rt(a, R::V0, worker_pool::slot(index));
        a.cmp_reg_reg(R::OwnerContext, R::V0);
        a.jcc_label(C::Equal, target);
    }
}

/// Raw locks also serve mutators and reclaim callers. Only the actual worker
/// view may refresh here; never change a caller's adopted mutator snapshot.
/// The lock entry saves V0/V1 around this operation.
pub(super) fn refresh<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    only_worker(a, done);
    begin_epoch(a, l);
    lea_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    a.atomic_load_acquire(R::V1, R::V1);
    store_rt(a, RT_CTX + offset_of!(MutatorContext, adopted_phase), R::V1);
    #[cfg(test)]
    if let Some(hook) = l.worker_epoch_hook {
        a.call_label(hook);
    }
    a.bind_text_label(done);
}

/// One real worker ticket hold owns one raw-address epoch. The worker may
/// retain managed references and registry leases between quanta, but no raw
/// heap address may be used after the matching release. This protects phase
/// and bank changes; it does not grant page-retirement or N-owner permission.
#[repr(C)]
pub(super) struct Epoch {
    active: u64,
    phase: u64,
    bank: u64,
    entered: u64,
    completed: u64,
}
const EPOCH: usize = owner_state::offset(offset_of!(worker_pool::Slot, epoch));
pub(super) const ACTIVE: usize = EPOCH + offset_of!(Epoch, active);
pub(super) const PHASE: usize = EPOCH + offset_of!(Epoch, phase);
pub(super) const BANK: usize = EPOCH + offset_of!(Epoch, bank);
pub(super) const ENTERED: usize = EPOCH + offset_of!(Epoch, entered);
pub(super) const COMPLETED: usize = EPOCH + offset_of!(Epoch, completed);

/// Preserves all registers except V0/V1. Caller holds the actual ticket.
pub(super) fn require_quiescent<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, worker_pool::ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, worker_pool::ENTERED);
    load_rt(a, R::V1, worker_pool::COMPLETED);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
}
fn private_closed<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    copy_claims::private_closed(a, l);
    for field in [
        RT_PENDING_REF,
        RT_COLLECTING,
        RT_CTX + offset_of!(MutatorContext, private_producers),
        RT_CTX + offset_of!(MutatorContext, access_epoch),
    ] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
}
fn begin_epoch<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    require_quiescent(a, l);
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, ENTERED);
    load_rt(a, R::V1, COMPLETED);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    private_closed(a, l);
    load_rt(a, R::V0, ENTERED);
    a.mov_imm64(R::V1, u64::MAX);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, l.fail);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, ENTERED, R::V0);
    lea_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    a.atomic_load_acquire(R::V0, R::V1);
    store_rt(a, PHASE, R::V0);
    lea_rt(a, R::V1, allocation_records::REVISION);
    a.atomic_load_acquire(R::V0, R::V1);
    store_rt(a, BANK, R::V0);
    store_rt_imm(a, ACTIVE, 1);
    worker_pool::increment(a, l, worker_pool::ENTERED);
    store_rt_imm(a, worker_pool::ACTIVE, 1);
}

/// Every real raw-unlock route passes here, including park and shutdown.
/// Mutator/reclaimer callers preserve their own state and never own this lease.
/// V0/V1 are saved by the lock wrapper; all other registers are preserved.
pub(super) fn release<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    only_worker(a, done);
    load_rt(a, R::V0, ACTIVE);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    private_closed(a, l);
    for (captured, current) in [
        (
            PHASE,
            registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
        ),
        (BANK, allocation_records::REVISION),
        (PHASE, RT_CTX + offset_of!(MutatorContext, adopted_phase)),
    ] {
        load_rt(a, R::V0, captured);
        load_rt(a, R::V1, current);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(a, R::V0, ENTERED);
    load_rt(a, R::V1, COMPLETED);
    a.add_reg_imm32(R::V1, 1);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, COMPLETED, R::V0);
    store_rt_imm(a, ACTIVE, 0);
    load_rt(a, R::V0, worker_pool::ACTIVE);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    worker_pool::increment(a, l, worker_pool::COMPLETED);
    store_rt_imm(a, worker_pool::ACTIVE, 0);
    #[cfg(test)]
    if let Some(hook) = l.worker_epoch_hook {
        a.call_label(hook);
    }
    a.bind_text_label(done);
}
