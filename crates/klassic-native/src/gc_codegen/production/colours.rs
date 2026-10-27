//! The adopted ABI snapshot is the source of reference colors and load masks.
//! Publication/adoption must keep that snapshot stable through an operation.
//! This module does not authorize epoch or virtual-address reuse.
use super::{RT_CTX, load_rt};
use crate::gc_abi::{
    HEAP_PAGE_BYTES, METADATA_MASK, MutatorContext, OLD_MARK_MASK, REMAP_MASK, REMEMBERED_MASK,
    YOUNG_MARK_MASK,
};
use crate::gc_codegen::production::lea_rt;
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

fn snapshot<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V2, RT_CTX + offset_of!(MutatorContext, adopted_phase));
    a.atomic_load_acquire(R::V2, R::V2);
}

/// V2 = validated PhaseSnapshot -> V0 = color; preserve other registers.
/// The immutable table is generated from PhaseSnapshot::color at startup.
fn colour<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V0, R::V2);
    a.shr_reg_imm8(R::V0, 8);
    a.and_reg_imm32(R::V0, 31);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.load_ptr_disp32(R::V0, R::V0, super::layout(a).colours as i32);
}

/// Runtime in V3; color in `dst`, preserving every other register.
pub(super) fn load_current<E: PortableAsm>(a: &mut E, dst: R) {
    assert_ne!(dst, R::V3);
    let temps = [R::V0, R::V2];
    for reg in temps {
        if reg != dst {
            a.push_reg(reg);
        }
    }
    snapshot(a);
    colour(a);
    a.mov_reg_reg(dst, R::V0);
    for reg in temps.into_iter().rev() {
        if reg != dst {
            a.pop_reg(reg);
        }
    }
}

/// V0 = nonnull managed reference (preserved), V3 = runtime.
/// V1 = color, V2 = ABI load mask from the *same* acquired snapshot.
/// Preserve all other registers. Old marking requires its bit for either
/// generation; Young marking additionally requires its bit for Young targets.
pub(super) fn load_access<E: PortableAsm>(a: &mut E) {
    for reg in [R::V0, R::V6, R::V7] {
        a.push_reg(reg);
    }
    snapshot(a);
    a.mov_imm64(R::V7, u64::from(REMAP_MASK));
    let old_done = a.create_text_label();
    a.mov_reg_reg(R::V1, R::V2);
    a.shr_reg_imm8(R::V1, 4);
    a.and_reg_imm32(R::V1, 15);
    marking_mask(a, OLD_MARK_MASK, old_done);
    a.bind_text_label(old_done);
    let young_done = a.create_text_label();
    a.mov_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V1, 16);
    super::generation::test(a, R::V1, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, young_done);
    a.mov_reg_reg(R::V1, R::V2);
    a.and_reg_imm32(R::V1, 15);
    marking_mask(a, YOUNG_MARK_MASK, young_done);
    a.bind_text_label(young_done);
    colour(a);
    a.mov_reg_reg(R::V1, R::V0);
    a.mov_reg_reg(R::V2, R::V7);
    for reg in [R::V7, R::V6, R::V0] {
        a.pop_reg(reg);
    }
}

fn marking_mask<E: PortableAsm>(a: &mut E, mask: u16, done: E::TextLabel) {
    a.cmp_reg_imm8(R::V1, 1);
    a.jcc_label(C::Below, done);
    a.cmp_reg_imm8(R::V1, 3);
    a.jcc_label(C::Above, done);
    a.mov_imm64(R::V6, u64::from(mask));
    a.or_reg_reg(R::V7, R::V6);
}

/// V0 = observed reference, V10 = slot. Caller has already protected the
/// referent in the Old mark engine. Change only Old mark bits, preserving
/// remembered/remap obligations; a competing real store wins the CAS.
/// Clobbers V0..V2. The adopted snapshot is stable under the collector lock.
pub(super) fn heal_marked_slot<E: PortableAsm>(a: &mut E) {
    load_current(a, R::V1);
    a.and_reg_imm32(R::V1, i32::from(OLD_MARK_MASK));
    a.mov_reg_reg(R::V2, R::V0);
    a.and_reg_imm32(R::V2, !i32::from(OLD_MARK_MASK));
    a.or_reg_reg(R::V1, R::V2);
    a.atomic_compare_exchange(R::V10, R::V1);
}

/// Validate all 48 ABI metadata words, alignment and the production heap
/// range; return offset in `scratch` without changing `value`. Committed
/// object-start/metadata-lease validation is a separate, unfinished contract.
pub(super) fn offset<E: PortableAsm>(a: &mut E, value: R, scratch: R, outside: E::TextLabel) {
    a.mov_reg_reg(scratch, value);
    a.and_reg_imm32(scratch, i32::from(!METADATA_MASK));
    a.test_reg_reg(scratch, scratch);
    a.jcc_label(C::NotEqual, outside);
    a.mov_reg_reg(scratch, value);
    a.and_reg_imm32(scratch, i32::from(REMEMBERED_MASK));
    a.test_reg_reg(scratch, scratch);
    a.jcc_label(C::Equal, outside);
    for mask in [YOUNG_MARK_MASK, OLD_MARK_MASK] {
        a.mov_reg_reg(scratch, value);
        a.and_reg_imm32(scratch, i32::from(mask));
        a.test_reg_reg(scratch, scratch);
        a.jcc_label(C::Equal, outside);
        a.cmp_reg_imm32(scratch, i32::from(mask));
        a.jcc_label(C::Equal, outside);
    }
    let remap_ok = a.create_text_label();
    a.mov_reg_reg(scratch, value);
    a.and_reg_imm32(scratch, i32::from(REMAP_MASK));
    for bit in [0x1000, 0x2000, 0x4000] {
        a.cmp_reg_imm32(scratch, bit);
        a.jcc_label(C::Equal, remap_ok);
    }
    a.cmp_reg_imm32(scratch, 0x8000);
    a.jcc_label(C::NotEqual, outside);
    a.bind_text_label(remap_ok);
    a.mov_reg_reg(scratch, value);
    a.and_reg_imm32(scratch, 0xf0000);
    a.test_reg_reg(scratch, scratch);
    a.jcc_label(C::NotEqual, outside);
    a.mov_reg_reg(scratch, value);
    a.shr_reg_imm8(scratch, 16);
    a.cmp_reg_imm32(scratch, HEAP_PAGE_BYTES as i32 + super::copy::HEADER);
    a.jcc_label(C::Below, outside);
    a.cmp_reg_imm32(scratch, super::geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, outside);
}

/// V0 is a managed forwarding result. A copied white object has no mark
/// admission yet: returning current mark bits would let a load skip that duty.
/// Preserve V0's offset and all nonmark bits; clobber V1, V2, V6, V7.
pub(super) fn uncertify_white<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    load_rt(a, R::V2, super::RT_MARK_ACTIVE);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, done);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    super::mark_epoch::test(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::NotEqual, done);
    load_current(a, R::V1);
    a.mov_imm64(R::V2, u64::from(OLD_MARK_MASK));
    a.xor_reg_reg(R::V1, R::V2);
    a.and_reg_imm32(R::V1, i32::from(OLD_MARK_MASK));
    a.and_reg_imm32(R::V0, !i32::from(OLD_MARK_MASK));
    a.or_reg_reg(R::V0, R::V1);
    a.bind_text_label(done);
    let finished = a.create_text_label();
    load_rt(a, R::V2, super::young_mark::ACTIVE);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::Equal, finished);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, finished);
    super::young_mark::test(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::NotEqual, finished);
    load_current(a, R::V1);
    a.mov_imm64(R::V2, u64::from(YOUNG_MARK_MASK));
    a.xor_reg_reg(R::V1, R::V2);
    a.and_reg_imm32(R::V1, i32::from(YOUNG_MARK_MASK));
    a.and_reg_imm32(R::V0, !i32::from(YOUNG_MARK_MASK));
    a.or_reg_reg(R::V0, R::V1);
    a.bind_text_label(finished);
}
