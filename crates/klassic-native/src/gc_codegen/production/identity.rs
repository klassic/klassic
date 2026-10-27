//! Stable identity and per-object copy metadata for the production collector.
//!
//! Copy metadata has a nonmoving backing page per destination heap page,
//! installed lazily with a CAS. Records within it are indexed by
//! floor(header_offset_in_page / 48). Object starts are at least 48 bytes
//! apart, so independent copies never share a record. Source backing retires
//! after sole-owner remap quiescence and before heap-page reuse; a copy
//! initializes only its own private record.
//! Old bump allocations can straddle heap pages, including a header in the
//! final partial 48-byte interval. Young TLAB cell counts exclude that interval.
use super::{Labels, RT_HEAP_BASE, RT_NEXT_OBJECT_ID, RT_RESULT, d, lea_rt, load_rt, store_rt};
use crate::gc_abi::{AllocationResult, MAX_AGE, MarkRecord, ObjectHeader};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};

const HEADER: i32 = size_of::<ObjectHeader>() as i32;
const RECORD: i32 = offset_of!(ObjectHeader, mark_record) as i32 - HEADER;
const ID: i32 = offset_of!(MarkRecord, object_id) as i32;
const AGE: i32 = offset_of!(MarkRecord, age) as i32;

/// Assign a fresh identity only for a mutator allocation, never a relocation.
/// V0 = payload address (preserved); clobbers V1/V2. Concurrent allocating
/// owners reserve unique checked IDs. Never derive identity from an address.
pub(super) fn assign_new<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V6);
    a.push_reg(R::V7);
    a.push_reg(R::V8);
    a.push_reg(R::V0);
    lea_rt(a, R::V7, RT_NEXT_OBJECT_ID);
    let retry = a.create_text_label();
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V7);
    a.mov_reg_reg(R::V8, R::V0);
    a.mov_reg_reg(R::V6, R::V0);
    a.add_reg_imm32(R::V6, 1);
    a.jcc_label(C::Equal, l.fail); // exhausted u64 identity space
    a.atomic_compare_exchange(R::V7, R::V6);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, retry);
    a.mov_reg_reg(R::V1, R::V6);
    a.pop_reg(R::V0);
    a.pop_reg(R::V8);
    a.pop_reg(R::V7);
    a.pop_reg(R::V6);
    a.load_ptr_disp32(R::V2, R::V0, RECORD);
    a.store_ptr_disp32(R::V2, ID, R::V1);
    store_rt(
        a,
        RT_RESULT + offset_of!(AllocationResult, object_id),
        R::V1,
    );
}

/// Initialize one private header's stable metadata; V8 header survives,
/// V7 returns its zeroed record. The caller publishes the header afterward.
pub(super) fn prepare_record<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    // Find the metadata backing entry for this destination heap page.
    a.mov_reg_reg(R::V0, R::V8);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.shr_reg_imm8(R::V0, 17);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::layout(a).copy_marks as i32);
    a.mov_reg_reg(R::V9, R::V0);
    let retry = a.create_text_label();
    let ready = a.create_text_label();
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V9);
    a.cmp_reg_imm8(R::V0, 1); // the installing copier owns this entry
    a.jcc_label(C::Equal, retry);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, ready);
    a.mov_imm64(R::V6, 1);
    a.atomic_compare_exchange(R::V9, R::V6);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, retry);
    // Map outside any source-access guard. Relocation retains source storage
    // until every copier finishes. There is one map per destination page;
    // competitors wait for release-publication rather than leaking a loser map.
    for reg in [R::V8, R::V8, R::V9, R::V9] {
        a.push_reg(reg);
    }
    a.mov_imm64(R::V7, super::metadata::copy_bytes(a));
    a.call_label(l.metadata_map);
    for reg in [R::V9, R::V9, R::V8, R::V8] {
        a.pop_reg(reg);
    }
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.native_oom);
    a.atomic_store_release(R::V9, R::V0);
    a.bind_text_label(ready);
    a.mov_reg_reg(R::V7, R::V0);
    a.mov_reg_reg(R::V0, R::V8);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.and_reg_imm32(R::V0, (crate::gc_abi::HEAP_PAGE_BYTES - 1) as i32);
    a.mov_imm64(R::V6, 48);
    a.div_unsigned(R::V6);
    a.mov_imm64(R::V6, size_of::<MarkRecord>() as u64);
    a.mul_unsigned_wide(R::V6);
    a.add_reg_reg(R::V7, R::V0);
    a.mov_imm64(R::V1, 0);
    for offset in (0..size_of::<MarkRecord>()).step_by(8) {
        a.store_ptr_disp32(R::V7, offset as i32, R::V1);
    }
    a.store_ptr_disp32(R::V8, offset_of!(ObjectHeader, mark_record) as i32, R::V7);
}

/// Initialize a private copy's metadata before its publication. V8 is the
/// destination header address (preserved), and `source` is the runtime slot
/// holding the source payload address. Other caller-saved registers change.
pub(super) fn copy_record<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, source: usize) {
    prepare_record(a, l);
    copy_fields(a, l, source);
}

/// An exclusively owned precommitted pin candidate already has its actual
/// private record. This leaf cannot map or allocate while owning a copy claim.
pub(super) fn copy_prepared_record<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    source: usize,
) {
    a.load_ptr_disp32(R::V7, R::V8, offset_of!(ObjectHeader, mark_record) as i32);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, l.fail);
    a.mov_imm64(R::V1, 0);
    for field in (0..size_of::<MarkRecord>()).step_by(8) {
        a.store_ptr_disp32(R::V7, field as i32, R::V1);
    }
    copy_fields(a, l, source);
}
fn copy_fields<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, source: usize) {
    load_rt(a, R::V0, source);
    a.load_ptr_disp32(R::V6, R::V0, RECORD);
    a.load_ptr_disp32(R::V2, R::V6, ID);
    a.store_ptr_disp32(R::V7, ID, R::V2);
    // Epochs describe logical membership; spill links/claims remain private
    // and are not copied. Source epochs are stable during this relocation.
    for field in [
        offset_of!(MarkRecord, young_epoch),
        offset_of!(MarkRecord, old_epoch),
    ] {
        a.mov_reg_reg(R::V1, R::V6);
        a.add_reg_imm32(R::V1, d(field));
        a.atomic_load_acquire(R::V2, R::V1);
        a.mov_reg_reg(R::V1, R::V7);
        a.add_reg_imm32(R::V1, d(field));
        a.atomic_store_release(R::V1, R::V2);
    }
    a.load_ptr_disp32(R::V2, R::V6, AGE);
    a.cmp_reg_imm32(R::V2, MAX_AGE as i32);
    a.jcc_label(C::Above, l.fail);
    let aged = a.create_text_label();
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    super::generation::test(a, R::V0, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, aged); // moving Old does not age it again
    a.cmp_reg_imm32(R::V2, MAX_AGE as i32);
    a.jcc_label(C::AboveOrEqual, aged);
    a.add_reg_imm32(R::V2, 1);
    a.bind_text_label(aged);
    a.store_ptr_disp32(R::V7, AGE, R::V2);
    a.store_ptr_disp32(R::V8, offset_of!(ObjectHeader, mark_record) as i32, R::V7);
}
