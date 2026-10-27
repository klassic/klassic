//! Explicit, root-only Native return through exactly one still-open Young
//! relocation. The actual source remains mapped and barriers resolve stale
//! RootCells. Completed multi-cycle/Old return uses native_completion instead.
use super::super::*;
use crate::gc_abi::{Phase, RootRegistry};
pub(super) fn adopt<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::private_closed(a, l);
    for field in [
        native_io::SCOPE_BORROWED,
        RT_REQUEST + offset_of!(AllocationRequest, page),
        RT_CTX + offset_of!(MutatorContext, tlab_start),
        RT_CTX + offset_of!(MutatorContext, tlab_top),
        RT_CTX + offset_of!(MutatorContext, tlab_end),
        cooperative_old::ENABLED,
        RT_FULL,
        RT_RETIRE_STAGE,
        RT_MARK_ACTIVE,
        young_mark::ACTIVE,
    ] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, young_mark::VALID);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, relocate_pages::EPOCH);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V0, RT_RY_LO);
    load_rt(a, R::V1, RT_RY_HI);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, l.fail);
    load_rt(a, R::V8, native_io::PHASE);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, l.fail);
    // The excluded scope entered Idle, with no heap cursor. This branch does
    // not infer safety for an arbitrary Native phase or a borrowed callback.
    a.mov_reg_reg(R::V0, R::V8);
    a.and_reg_imm32(R::V0, 255);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(
        a,
        R::V1,
        registered_roots::REGISTRY + offset_of!(RootRegistry, phase),
    );
    a.atomic_load_acquire(R::V9, R::V1);
    a.mov_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 255);
    a.cmp_reg_imm32(R::V0, Phase::ConcurrentRelocate as i32);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V0, R::V8);
    a.or_reg_reg(R::V0, R::V9);
    a.and_reg_imm32(R::V0, 0xe000);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V0, R::V8);
    a.xor_reg_reg(R::V0, R::V9);
    a.mov_reg_reg(R::V1, R::V0);
    a.and_reg_imm32(R::V1, 0xc00);
    a.cmp_reg_imm32(R::V1, 0x400); // exactly the actual Young remap flip
    a.jcc_label(C::NotEqual, l.fail);
    a.and_reg_imm32(R::V0, 0x1200); // Old mark and remembered parity stay fixed
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V0, R::V8);
    a.mov_reg_reg(R::V1, R::V9);
    a.shr_reg_imm8(R::V0, 16);
    a.shr_reg_imm8(R::V1, 16);
    a.cmp_reg_reg(R::V1, R::V0);
    a.jcc_label(C::Below, l.fail);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V10, native_io::BANK);
    a.mov_imm64(R::V0, u64::MAX);
    a.cmp_reg_reg(R::V10, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::V0, R::V10);
    a.add_reg_imm32(R::V0, 1);
    load_rt(a, R::V1, allocation_records::REVISION);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    // Revisions start at one; the two real fresh-record banks alternate.
    a.sub_reg_imm8(R::V10, 1);
    a.and_reg_imm32(R::V10, 1);
    load_rt(a, R::V0, pool::FROM_BANK);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V1, 1);
    a.xor_reg_reg(R::V0, R::V1);
    load_rt(a, R::V1, pool::BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, young_mark::EPOCH);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.and_reg_imm32(R::V0, 1);
    a.mov_reg_reg(R::V1, R::V9);
    a.shr_reg_imm8(R::V1, 8);
    a.and_reg_imm32(R::V1, 1);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.atomic_store_release(R::V1, R::V9);
    allocation_records::adopt(a, l);
    copy::bump(a, native_io::ADOPTIONS, R::V0);
}
