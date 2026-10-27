//! Page-local Old field ownership. Starts share the Young page bitmap layout;
//! a card hint covers objects that began before the slot's 1-KiB card.
//! This is not a page/object metadata lease or a shared-pool transfer protocol.
use super::{HEAP_PAGE_BYTES, Labels, RT_HEAP_BASE, d, load_rt};
use crate::gc_abi::{MarkRecord, OBJECT_PUBLISHED, ObjectHeader};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};
pub(super) const CARD_BYTES: u64 = 1024;
const HEADER: i32 = size_of::<ObjectHeader>() as i32;
const RECORD: i32 = offset_of!(ObjectHeader, mark_record) as i32 - HEADER;
const SAVED: [R; 8] = [R::V1, R::V2, R::V6, R::V7, R::V8, R::V9, R::V10, R::V11];

/// V8 = Old header, preserved. Publish hints/start membership after zeroing
/// the payload and installing metadata/descriptor. Owner lookup independently
/// requires PUBLISHED: a fresh large object can register its interior cards
/// before returning to its initializer. A copy claims forwarding afterward.
pub(super) fn publish<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V10, RT_HEAP_BASE);
    a.mov_reg_reg(R::V6, R::V8);
    a.sub_reg_reg(R::V6, R::V10); // Header offset.
    a.load_ptr_disp32(R::V7, R::V8, d(offset_of!(ObjectHeader, payload_bytes)));
    a.add_reg_imm32(R::V7, 15);
    a.and_reg_imm32(R::V7, -16);
    let sized = a.create_text_label();
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V7, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V7, HEADER);
    a.add_reg_reg(R::V7, R::V6); // End of the complete footprint.
    a.mov_reg_reg(R::V9, R::V6);
    a.and_reg_imm32(R::V9, -1024);
    a.add_reg_imm32(R::V9, 1024);
    let next = a.create_text_label();
    let bits = a.create_text_label();
    a.bind_text_label(next);
    a.cmp_reg_reg(R::V9, R::V7);
    a.jcc_label(C::AboveOrEqual, bits);
    a.mov_reg_reg(R::V2, R::V9);
    a.shr_reg_imm8(R::V2, 10);
    a.shl_reg_imm8(R::V2, 3);
    a.add_reg_reg(R::V2, R::V3);
    a.add_reg_imm32(R::V2, super::layout(a).owner_cards as i32);
    a.atomic_store_release(R::V2, R::V6);
    a.add_reg_imm32(R::V9, 1024);
    a.jmp_label(next);
    a.bind_text_label(bits);
    a.add_reg_imm32(R::V6, HEADER); // Shared bitmap records payload starts.
    a.mov_reg_reg(R::V2, R::V6);
    a.sub_reg_imm32(R::V2, HEAP_PAGE_BYTES as i32);
    a.shr_reg_imm8(R::V2, 10);
    a.shl_reg_imm8(R::V2, 3);
    a.add_reg_reg(R::V2, R::V3);
    a.add_reg_imm32(R::V2, super::layout(a).starts as i32);
    a.mov_reg_reg(R::V1, R::V6);
    a.shr_reg_imm8(R::V1, 4);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(R::V7, 1);
    a.shl_reg_count(R::V7);
    a.push_reg(R::V8);
    a.push_reg(R::V8);
    a.atomic_fetch_or(R::V2, R::V7);
    a.pop_reg(R::V8);
    a.pop_reg(R::V8);
    super::page_runs::publish(a);
}

/// Six fixed comparisons find the highest nonzero bit in V9, result V10.
fn highest<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V10, 0);
    for shift in [32, 16, 8, 4, 2, 1] {
        let lower = a.create_text_label();
        a.mov_reg_reg(R::V0, R::V9);
        a.shr_reg_imm8(R::V0, shift);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, lower);
        a.mov_reg_reg(R::V9, R::V0);
        a.add_reg_imm32(R::V10, i32::from(shift));
        a.bind_text_label(lower);
    }
}
/// `(V7=slot address)->V0=owner payload offset, or zero`. All other registers
/// survive. No heap-prefix walk: two bitmap words, at most one hint, one
/// candidate header. Validate before dereferencing the slot's value.
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let none = a.create_text_label();
    let done = a.create_text_label();
    let hinted = a.create_text_label();
    let candidate = a.create_text_label();
    a.bind_text_label(l.slot_owner);
    for r in SAVED {
        a.push_reg(r);
    }
    load_rt(a, R::V6, RT_HEAP_BASE);
    a.mov_reg_reg(R::V0, R::V7);
    a.sub_reg_reg(R::V0, R::V6);
    a.jcc_label(C::Below, none);
    a.mov_reg_reg(R::V6, R::V0);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, none);
    super::generation::test(a, R::V6, super::generation::OLD);
    a.jcc_label(C::NotEqual, none);
    a.mov_reg_reg(R::V0, R::V6);
    a.sub_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    a.shr_reg_imm8(R::V0, 10);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::layout(a).starts as i32);
    a.atomic_load_acquire(R::V9, R::V0);
    a.mov_reg_reg(R::V1, R::V6);
    a.shr_reg_imm8(R::V1, 4);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(R::V2, 63);
    a.sub_reg_reg(R::V2, R::V1);
    a.mov_reg_reg(R::V1, R::V2);
    a.mov_imm64(R::V0, u64::MAX);
    a.shr_reg_count(R::V0);
    a.and_reg_reg(R::V9, R::V0);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, hinted);
    highest(a);
    a.shl_reg_imm8(R::V10, 4);
    a.mov_reg_reg(R::V0, R::V6);
    a.and_reg_imm32(R::V0, -1024);
    a.add_reg_reg(R::V10, R::V0);
    a.sub_reg_imm32(R::V10, HEADER);
    a.jmp_label(candidate);
    a.bind_text_label(hinted);
    a.mov_reg_reg(R::V0, R::V6);
    a.shr_reg_imm8(R::V0, 10);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::layout(a).owner_cards as i32);
    a.atomic_load_acquire(R::V10, R::V0);
    a.bind_text_label(candidate);
    a.cmp_reg_imm32(R::V10, HEAP_PAGE_BYTES as i32);
    a.jcc_label(C::Below, none);
    a.cmp_reg_imm32(
        R::V10,
        (super::geometry(a).heap_bytes - size_of::<ObjectHeader>() as u64 - 8) as i32,
    );
    a.jcc_label(C::Above, none);
    a.mov_reg_reg(R::V0, R::V10);
    a.and_reg_imm32(R::V0, 15);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, none);
    a.cmp_reg_reg(R::V10, R::V6);
    a.jcc_label(C::Above, none);
    super::generation::test(a, R::V10, super::generation::OLD);
    a.jcc_label(C::NotEqual, none);
    // A hint never bypasses exact object-start membership.
    a.mov_reg_reg(R::V0, R::V10);
    a.add_reg_imm32(R::V0, HEADER);
    a.sub_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    a.shr_reg_imm8(R::V0, 10);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::layout(a).starts as i32);
    a.atomic_load_acquire(R::V2, R::V0);
    a.mov_reg_reg(R::V1, R::V10);
    a.add_reg_imm32(R::V1, HEADER);
    a.shr_reg_imm8(R::V1, 4);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(R::V9, 1);
    a.shl_reg_count(R::V9);
    a.test_reg_reg(R::V2, R::V9);
    a.jcc_label(C::Equal, none);
    load_rt(a, R::V2, RT_HEAP_BASE);
    a.add_reg_reg(R::V2, R::V10);
    a.mov_reg_reg(R::V0, R::V2);
    a.add_reg_imm32(R::V0, d(offset_of!(ObjectHeader, descriptor_id)));
    a.atomic_load_acquire(R::V11, R::V0);
    let record = a.create_text_label();
    a.cmp_reg_imm8(R::V11, 2);
    a.jcc_label(C::Equal, record);
    a.cmp_reg_imm8(R::V11, 4);
    a.jcc_label(C::NotEqual, none);
    a.bind_text_label(record);
    a.load_ptr_disp32(R::V0, R::V2, d(offset_of!(ObjectHeader, state)));
    a.cmp_reg_imm8(R::V0, OBJECT_PUBLISHED as i8);
    a.jcc_label(C::NotEqual, none);
    a.load_ptr_disp32(R::V9, R::V2, d(offset_of!(ObjectHeader, payload_bytes)));
    a.cmp_reg_imm32(R::V9, super::large_object::max_payload(a) as i32);
    a.jcc_label(C::Above, none);
    a.load_ptr_disp32(R::V1, R::V2, d(offset_of!(ObjectHeader, mark_record)));
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, none);
    a.mov_reg_reg(R::V0, R::V10);
    a.add_reg_imm32(R::V0, HEADER);
    a.mov_reg_reg(R::V1, R::V0);
    a.cmp_reg_imm8(R::V11, 4);
    let lower = a.create_text_label();
    a.jcc_label(C::NotEqual, lower);
    a.add_reg_imm32(R::V1, 8);
    a.bind_text_label(lower);
    a.cmp_reg_reg(R::V6, R::V1);
    a.jcc_label(C::Below, none);
    a.mov_reg_reg(R::V2, R::V0);
    a.add_reg_reg(R::V2, R::V9);
    a.cmp_reg_imm32(R::V2, super::geometry(a).heap_bytes as i32);
    a.jcc_label(C::Above, none);
    a.mov_reg_reg(R::V1, R::V6);
    a.add_reg_imm32(R::V1, 8);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Above, none);
    a.jmp_label(done);
    a.bind_text_label(none);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
    for r in SAVED.into_iter().rev() {
        a.pop_reg(r);
    }
    a.ret();
    emit_remembered(a, l);
}
/// Completed Old snapshots prune only pre-snapshot holders. New identities
/// cannot be inferred dead from a snapshot that predates their allocation.
fn emit_remembered<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    a.bind_text_label(l.remembered_owner);
    a.call_label(l.slot_owner);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    for r in [R::V1, R::V2, R::V6, R::V8] {
        a.push_reg(r);
    }
    let keep = a.create_text_label();
    load_rt(a, R::V1, super::RT_MARK_VALID);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, keep);
    a.mov_reg_reg(R::V2, R::V0);
    newer(a, R::V2);
    a.jcc_label(C::Above, keep);
    super::mark::test_mark(a, R::V2, R::V6, R::V8);
    a.jcc_label(C::NotEqual, keep);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(keep);
    for r in [R::V8, R::V6, R::V2, R::V1] {
        a.pop_reg(r);
    }
    a.bind_text_label(done);
    a.ret();
}
/// Compare object ID at payload offset `off` with the Old start bound.
/// Preserve off/V0/V1/V7/V10; clobber V6/V8 and flags.
pub(super) fn newer<E: PortableAsm>(a: &mut E, off: R) {
    assert!(!matches!(off, R::V6 | R::V8));
    load_rt(a, R::V6, RT_HEAP_BASE);
    a.add_reg_reg(R::V6, off);
    a.load_ptr_disp32(R::V6, R::V6, RECORD);
    a.load_ptr_disp32(R::V6, R::V6, d(offset_of!(MarkRecord, object_id)));
    load_rt(a, R::V8, super::old_mark::OBJECT_BOUND);
    a.cmp_reg_reg(R::V6, R::V8);
}
