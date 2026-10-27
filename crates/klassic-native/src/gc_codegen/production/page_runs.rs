//! Shared run geometry for reference leases and subsequent pin admission.
//! Physical start/bitmap addresses stay page-local. Nonmoving/Old runs share
//! a head, incarnation, identity base and limit; only the head owns occupancy.
use super::*;
/// Pool claim ABI V1=zero-based base,V2=count,V6=physical role. Preserve all.
/// Called under the collector ticket before publishing directory ownership.
pub(super) fn claim<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    let young = a.create_text_label();
    let shared = a.create_text_label();
    let choose = a.create_text_label();
    let done = a.create_text_label();
    a.mov_reg_reg(R::V7, R::V1); // immutable head index
    a.mov_reg_reg(R::V10, R::V2); // count
    a.cmp_reg_imm8(R::V6, generation::YOUNG as i8);
    a.jcc_label(C::Equal, young);
    // A new run incarnation must exceed every constituent incarnation.
    // Copy/source cells can retain a physical page's previous serial.
    a.mov_reg_reg(R::V6, R::V1);
    a.mov_imm64(R::V11, 0);
    let maximum = a.create_text_label();
    let smaller = a.create_text_label();
    a.bind_text_label(maximum);
    scan::page_ptr(a, R::V8, R::V6, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.cmp_reg_reg(R::V0, R::V11);
    a.jcc_label(C::Below, smaller);
    a.mov_reg_reg(R::V11, R::V0);
    a.bind_text_label(smaller);
    a.add_reg_imm32(R::V6, 1);
    a.sub_reg_imm8(R::V2, 1);
    a.jcc_label(C::NotEqual, maximum);
    a.add_reg_imm32(R::V11, 1);
    a.jcc_label(C::Equal, l.fail);
    scan::page_ptr(a, R::V8, R::V7, R::V0);
    a.load_ptr_disp32(R::V9, R::V8, d(offset_of!(AllocationPage, identity_base)));
    a.jmp_label(shared);
    a.bind_text_label(young);
    a.mov_imm64(R::V11, 0); // individual TLAB page, never a run-tail alias
    a.bind_text_label(shared);
    a.mov_reg_reg(R::V6, R::V7);
    a.mov_reg_reg(R::V2, R::V10);
    a.bind_text_label(choose);
    scan::page_ptr(a, R::V8, R::V6, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, metadata_leases)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V0, R::V6);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, start)), R::V0);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, allocated_top)), R::V0);
    a.add_reg_imm32(R::V0, HEAP_PAGE_BYTES as i32);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, limit)), R::V0);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, run_head)), R::V6);
    // Restore the physical bitmap and fixed identity base on Young reuse.
    a.mov_reg_reg(R::V0, R::V6);
    a.shl_reg_imm8(R::V0, 10);
    lea_rt(a, R::V1, layout(a).starts as usize);
    a.add_reg_reg(R::V0, R::V1);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, starts)), R::V0);
    a.mov_imm64(R::V0, STARTS_WORDS);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, starts_words)), R::V0);
    a.test_reg_reg(R::V11, R::V11);
    let single = a.create_text_label();
    let next = a.create_text_label();
    a.jcc_label(C::Equal, single);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, run_head)), R::V7);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, incarnation)), R::V11);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, identity_base)), R::V9);
    a.mov_reg_reg(R::V0, R::V7);
    a.add_reg_reg(R::V0, R::V10);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, limit)), R::V0);
    a.jmp_label(next);
    a.bind_text_label(single);
    a.mov_reg_reg(R::V0, R::V6);
    a.shl_reg_imm8(R::V0, IDENTITY_STRIDE.trailing_zeros() as u8);
    a.add_reg_imm32(R::V0, 1);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, identity_base)), R::V0);
    a.bind_text_label(next);
    a.add_reg_imm32(R::V6, 1);
    a.sub_reg_imm8(R::V2, 1);
    a.jcc_label(C::NotEqual, choose);
    a.test_reg_reg(R::V11, R::V11);
    a.jcc_label(C::Equal, done);
    scan::page_ptr(a, R::V8, R::V7, R::V0);
    a.mov_reg_reg(R::V0, R::V10);
    a.shl_reg_imm8(R::V0, 7);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, starts_words)), R::V0);
    a.bind_text_label(done);
    quiescence::restore(a);
}
/// Preserve every register. V8 is a fully initialized Old/Large header. Starts
/// have been published; monotonic head bounds cannot hide another owner's top.
pub(super) fn publish<E: PortableAsm>(a: &mut E) {
    quiescence::save(a);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.mov_reg_reg(R::V6, R::V8);
    a.sub_reg_reg(R::V6, R::V1);
    a.load_ptr_disp32(R::V7, R::V8, d(offset_of!(ObjectHeader, payload_bytes)));
    a.add_reg_imm32(R::V7, 15);
    a.and_reg_imm32(R::V7, -16);
    let sized = a.create_text_label();
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V7, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V7, size_of::<ObjectHeader>() as i32);
    a.add_reg_reg(R::V7, R::V6);
    a.shr_reg_imm8(R::V6, 17);
    a.sub_reg_imm8(R::V6, 1);
    scan::page_ptr(a, R::V8, R::V6, R::V0);
    a.load_ptr_disp32(R::V6, R::V8, d(offset_of!(AllocationPage, run_head)));
    scan::page_ptr(a, R::V8, R::V6, R::V0);
    a.add_reg_imm32(R::V8, d(offset_of!(AllocationPage, allocated_top)));
    let again = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(again);
    a.atomic_load_acquire(R::V0, R::V8);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::AboveOrEqual, done);
    a.mov_reg_reg(R::V6, R::V0);
    a.atomic_compare_exchange(R::V8, R::V7);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::NotEqual, again);
    a.bind_text_label(done);
    quiescence::restore(a);
}

/// V8=Old run descriptor,V6=page-aligned end after unused-tail shrink.
/// Active source readers prevent the phase handshake; metadata-only leases
/// retain the head but do not authorize payload access in returned tail pages.
pub(super) fn resize<E: PortableAsm>(a: &mut E) {
    quiescence::save(a);
    a.load_ptr_disp32(R::V7, R::V8, old_runs::BASE);
    a.mov_reg_reg(R::V10, R::V6);
    a.shr_reg_imm8(R::V7, 17);
    a.sub_reg_imm8(R::V7, 1);
    a.mov_reg_reg(R::V6, R::V7);
    let top = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(top);
    a.mov_reg_reg(R::V0, R::V6);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 17);
    a.cmp_reg_reg(R::V0, R::V10);
    a.jcc_label(C::AboveOrEqual, done);
    scan::page_ptr(a, R::V8, R::V6, R::V0);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, limit)), R::V10);
    a.add_reg_imm32(R::V6, 1);
    a.jmp_label(top);
    a.bind_text_label(done);
    scan::page_ptr(a, R::V8, R::V7, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, start)));
    a.mov_reg_reg(R::V1, R::V10);
    a.sub_reg_reg(R::V1, R::V0);
    a.shr_reg_imm8(R::V1, 10);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, starts_words)), R::V1);
    quiescence::restore(a);
}

/// The Old reservation tail contains no object, but its shared ABI tail state
/// must retire before directory reuse. V6 is the one-based physical page.
pub(super) fn free_unused<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    a.mov_reg_reg(R::V0, R::V6);
    a.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, metadata_leases)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.add_reg_imm32(R::V0, geometry(a).pages as i32);
    a.jcc_label(C::Below, l.fail);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, incarnation)), R::V0);
    a.add_reg_imm32(R::V8, d(offset_of!(AllocationPage, state)));
    a.mov_imm64(R::V0, PAGE_READY);
    a.atomic_store_release(R::V8, R::V0);
    quiescence::restore(a);
}
