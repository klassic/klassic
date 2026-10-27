//! Completed relocation receipt for an explicitly excluded, root-only owner.
//! The collector captures the actual inventory scope at relocation start and
//! publishes only after root/graph completion and actual source retirement.
//! Returning owners use healed RootCells, never their old heap addresses.
use super::*;
use crate::gc_abi::RootRegistry;
#[repr(C)]
pub(super) struct State {
    pending_bank: u64,
    pending_scope: u64,
    bank: u64,
    scope: u64,
    phase: u64,
}
const BASE: usize = offset_of!(Runtime, native_completion);
pub(super) const PENDING_BANK: usize = BASE + offset_of!(State, pending_bank);
pub(super) const PENDING_SCOPE: usize = BASE + offset_of!(State, pending_scope);
pub(super) const BANK: usize = BASE + offset_of!(State, bank);
pub(super) const SCOPE: usize = BASE + offset_of!(State, scope);
pub(super) const PHASE: usize = BASE + offset_of!(State, phase);
/// Ticket held, immediately before the actual registered-root walk begins.
pub(super) fn capture<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, allocation_records::REVISION);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, PENDING_BANK, R::V0);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, last_scope),
    );
    store_rt(a, PENDING_SCOPE, R::V0);
}
/// This is called only by actual retirement completion, after Idle publication.
/// A completion hint or an exhausted single region cannot mint this receipt.
pub(super) fn publish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    load_rt(a, R::V0, PENDING_BANK);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    worker_view::require_quiescent(a, l);
    copy_claims::require_empty(a, l);
    registered_roots::finish(a, l, registered_roots::RELOCATION);
    let ordinary = a.create_text_label();
    let ledger_valid = a.create_text_label();
    load_rt(a, R::V0, moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, ordinary);
    moving_round::journal::require_closed(a, l);
    a.jmp_label(ledger_valid);
    a.bind_text_label(ordinary);
    root_work::require_empty(a, l);
    a.bind_text_label(ledger_valid);
    load_rt(a, R::V0, PENDING_BANK);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V0, PENDING_SCOPE);
    store_rt(a, SCOPE, R::V0);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    store_rt(a, PHASE, R::V0);
    load_rt(a, R::V0, PENDING_BANK);
    lea_rt(a, R::V1, BANK);
    a.atomic_store_release(R::V1, R::V0);
    store_rt_imm(a, PENDING_BANK, 0);
    store_rt_imm(a, PENDING_SCOPE, 0);
    a.bind_text_label(done);
}
/// Ticket held by the actual Native owner. Root-only entry predates the walk;
/// all its registered scopes were included, and no owner publication ran while
/// Native. The canonical cells survived even if both banks/colors cycled.
pub(super) fn adopt<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    native_io::adoption::private_closed(a, l);
    for f in [
        native_io::SCOPE_BORROWED,
        RT_REQUEST + offset_of!(AllocationRequest, page),
        RT_CTX + offset_of!(MutatorContext, tlab_start),
        RT_CTX + offset_of!(MutatorContext, tlab_top),
        RT_CTX + offset_of!(MutatorContext, tlab_end),
        RT_PHASE_OPEN,
        RT_RY_LO,
        RT_RY_HI,
        RT_RO_LO,
        RT_RO_HI,
        RT_RETIRE_STAGE,
        RT_MARK_ACTIVE,
        young_mark::ACTIVE,
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    let ordinary_heap = a.create_text_label();
    moving_round::branch_enabled(a, ordinary_heap);
    load_rt(a, R::V0, cooperative_old::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(ordinary_heap);
    load_rt(a, R::V8, native_io::PHASE);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    a.atomic_load_acquire(R::V9, R::V1);
    // The entered phase can be marking or moving: the actual root-only
    // exclusion and completed census protect cells independently of old colors.
    a.mov_reg_reg(R::V0, R::V8);
    a.or_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 0xe000);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    for shift in [0, 4] {
        a.mov_reg_reg(R::V0, R::V8);
        a.shr_reg_imm8(R::V0, shift);
        a.and_reg_imm32(R::V0, 15);
        a.cmp_reg_imm32(R::V0, 7);
        a.jcc_label(C::Above, l.fail);
    }
    a.mov_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 255); // the completed destination is genuinely Idle
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V0, R::V8);
    a.mov_reg_reg(R::V1, R::V9);
    a.shr_reg_imm8(R::V0, 16);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, l.fail);
    a.jcc_label(C::Equal, l.fail);
    lea_rt(a, R::V1, BANK);
    a.atomic_load_acquire(R::V0, R::V1);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V1, native_io::BANK);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, l.fail);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    load_rt(a, R::V0, PHASE);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, native_io::ROOT_SCOPE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, SCOPE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.atomic_store_release(R::V1, R::V9);
    allocation_records::adopt(a, l);
    copy::bump(a, native_io::ADOPTIONS, R::V0);
}
