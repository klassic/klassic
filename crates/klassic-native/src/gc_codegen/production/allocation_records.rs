//! Reserve an exclusive record from the shared, prezeroed fresh bank.
//! The generic C/direct leaf keeps its owner-exclusive MarkArena contract.
//! Only the shared reservation cursor is contested; object initialization
//! and TLAB bumps run independently after its successful CAS. Bank changes
//! still require the production phase rendezvous, not this cursor alone.
use super::*;

pub(super) const OWNER: usize = owner_state::offset(offset_of!(OwnerState, marks));
pub(super) const OWNER_REVISION: usize = owner_state::offset(offset_of!(OwnerState, mark_revision));
pub(super) const REVISION: usize = offset_of!(Runtime, mark_revision);

/// Acquire the bank identity before its backing pointer. An exhausted local
/// arena has valid storage/capacity but used == capacity, so a physically full
/// TLAB still returns REFILL before the leaf considers metadata exhaustion.
pub(super) fn adopt<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    lea_rt(a, R::V1, REVISION);
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, OWNER_REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, done);
    store_rt(a, OWNER_REVISION, R::V0);
    load_rt(a, R::V0, RT_MARKS + offset_of!(MarkArena, records));
    store_rt(a, OWNER + offset_of!(MarkArena, records), R::V0);
    store_rt_imm(a, OWNER + offset_of!(MarkArena, capacity), 1);
    store_rt_imm(a, OWNER + offset_of!(MarkArena, used), 1);
    a.bind_text_label(done);
}

/// Called after resetting the bank pointer/cursor, with all producers excluded
/// by the phase coordinator. Revision exhaustion is fatal, never an ABA wrap.
pub(super) fn advance<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    worker_view::require_quiescent(a, l);
    copy_claims::require_empty(a, l);
    load_rt(a, R::V0, REVISION);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    lea_rt(a, R::V1, REVISION);
    a.atomic_store_release(R::V1, R::V0);
    let ordinary = a.create_text_label();
    load_rt(a, R::V1, moving_round::ENABLED);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, ordinary);
    store_rt(a, moving_round::CURRENT_BANK, R::V0);
    a.bind_text_label(ordinary);
}

/// Clobbers caller-saved registers. Reserve only when the owned physical TLAB
/// can fit this payload; otherwise let the ordinary leaf/refill path decide.
/// A reserved free record is never read before winning the shared cursor CAS.
pub(super) fn prepare<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    adopt(a, l);
    let done = a.create_text_label();
    let sized = a.create_text_label();
    let retry = a.create_text_label();
    load_rt(a, R::V0, OWNER + offset_of!(MarkArena, used));
    load_rt(a, R::V1, OWNER + offset_of!(MarkArena, capacity));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.jcc_label(C::Above, l.fail);
    load_rt(a, R::V0, RT_REQUEST + offset_of!(AllocationRequest, page));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, RT_ALLOC_SIZE);
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V0, size_of::<ObjectHeader>() as i32);
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, tlab_end));
    load_rt(a, R::V2, RT_CTX + offset_of!(MutatorContext, tlab_top));
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V1, R::V2);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, done);
    lea_rt(a, R::V7, RT_MARKS + offset_of!(MarkArena, used));
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V7);
    load_rt(a, R::V1, RT_MARKS + offset_of!(MarkArena, capacity));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    a.jcc_label(C::Equal, done);
    a.mov_reg_reg(R::V8, R::V0);
    a.mov_reg_reg(R::V6, R::V0);
    a.add_reg_imm32(R::V6, 1);
    a.atomic_compare_exchange(R::V7, R::V6);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, retry);
    a.mov_reg_reg(R::V0, R::V8);
    a.mov_imm64(R::V1, size_of::<MarkRecord>() as u64);
    a.mul_unsigned_wide(R::V1);
    load_rt(a, R::V1, RT_MARKS + offset_of!(MarkArena, records));
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, OWNER + offset_of!(MarkArena, records), R::V0);
    store_rt_imm(a, OWNER + offset_of!(MarkArena, used), 0);
    a.bind_text_label(done);
}
