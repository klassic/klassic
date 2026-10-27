//! Snapshot protection for moving objects, independent of their physical age.
//! Epoch admission counts a logical object once through Young moves/promotion.
//! Dedicated Large spans never consume a moving destination. Floating garbage
//! and fresh objects remain charged until the next snapshot; saturating at the
//! hard heap size preserves a conservative bound through long Young churn.
use super::{Runtime, load_rt, store_rt};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::offset_of;

pub(super) const TRACE: usize = offset_of!(Runtime, old_mark.trace_bytes);
pub(super) const BLACK: usize = offset_of!(Runtime, old_mark.black_bytes);

/// Add V0 bytes to an actor-owned counter, clamping before arithmetic can wrap.
/// Changes V0/V1 only. No shared read/modify/write between the two actors.
fn charge<E: PortableAsm>(a: &mut E, field: usize) {
    let saturated = a.create_text_label();
    let done = a.create_text_label();
    let maximum = super::geometry(a).heap_bytes;
    load_rt(a, R::V1, field);
    a.add_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, saturated);
    a.mov_imm64(R::V1, maximum);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.bind_text_label(saturated);
    a.mov_imm64(R::V0, maximum);
    a.bind_text_label(done);
    store_rt(a, field, R::V0);
}

/// First Old-epoch admission, under the collector lock. V2 is the payload
/// offset. Preserve it and V0; change V1. Mark membership gates this call.
pub(super) fn admit<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    super::generation::test_role(a, R::V2, super::generation::LARGE);
    a.jcc_label(C::Equal, done);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    load_rt(a, R::V0, super::RT_HEAP_BASE);
    a.add_reg_reg(R::V0, R::V2);
    a.load_ptr_disp32(R::V0, R::V0, super::copy::PAYLOAD);
    footprint(a);
    charge(a, TRACE);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.bind_text_label(done);
}

fn footprint<E: PortableAsm>(a: &mut E) {
    let sized = a.create_text_label();
    a.add_reg_imm32(R::V0, 15);
    a.and_reg_imm32(R::V0, -16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V0, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V0, super::copy::HEADER);
}

/// Successful application allocation only, never alloc_raw's copy callers.
/// A closed valid snapshot still bounds the whole moving live set: charge new
/// identities until that census is invalidated, including after mark closure.
/// Preserve the returned address V0 and all registers except V1/V2.
pub(super) fn fresh<E: PortableAsm>(a: &mut E) {
    let done = a.create_text_label();
    let tracked = a.create_text_label();
    load_rt(a, R::V1, super::RT_MARK_ACTIVE);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, tracked);
    load_rt(a, R::V1, super::RT_MARK_VALID);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, done);
    a.bind_text_label(tracked);
    load_rt(a, R::V1, super::RT_FRESH_SIZE);
    a.cmp_reg_imm32(R::V1, super::MAX_PAYLOAD as i32);
    a.jcc_label(C::Above, done);
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    a.mov_reg_reg(R::V0, R::V1);
    footprint(a);
    super::counters::add_capped(a, BLACK, R::V0, super::geometry(a).heap_bytes);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.bind_text_label(done);
}

/// Closed snapshot, collector lock held: V0 = saturated protection bytes.
/// Includes Young snapshot objects and all fresh ordinary allocations through
/// the valid census lifetime. Copies/promotion retain identities without a new
/// charge. Dead/floating objects may remain charged; Large spans are excluded.
pub(super) fn protected<E: PortableAsm>(a: &mut E) {
    let saturated = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, TRACE);
    load_rt(a, R::V1, BLACK);
    a.add_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, saturated);
    a.mov_imm64(R::V1, super::geometry(a).heap_bytes);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, done);
    a.bind_text_label(saturated);
    a.mov_imm64(R::V0, super::geometry(a).heap_bytes);
    a.bind_text_label(done);
}
