//! Return the sole owner's unused tail reservation after relocation and source
//! cleanup complete. Keep occupied pages and every worker page counted.
use super::*;
use crate::gc_abi::{AllocationPage, PAGE_ALLOCATING, PAGE_READY};
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};

pub(super) fn release<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    let done = a.create_text_label();
    load_rt(a, R::V6, RT_REQUEST + offset_of!(AllocationRequest, page));
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, done);
    // Only a current, still-owned, truly empty page at the owner stream's
    // tail can return to the unclaimed middle gap. The collector lock excludes
    // worker reservations; only this stopped owner can append to its TLAB.
    a.load_ptr_disp32(R::V0, R::V6, d(offset_of!(AllocationPage, state)));
    a.cmp_reg_imm8(R::V0, PAGE_ALLOCATING as i8);
    a.jcc_label(C::NotEqual, done);
    a.load_ptr_disp32(R::V0, R::V6, d(offset_of!(AllocationPage, owner)));
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, done);
    a.load_ptr_disp32(R::V9, R::V6, d(offset_of!(AllocationPage, start)));
    a.load_ptr_disp32(R::V0, R::V6, d(offset_of!(AllocationPage, allocated_top)));
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, done);
    a.load_ptr_disp32(R::V0, R::V6, d(offset_of!(AllocationPage, metadata_leases)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    load_rt(a, R::V0, RT_HEAP_BASE);
    a.add_reg_reg(R::V0, R::V9);
    for field in [
        offset_of!(MutatorContext, tlab_start),
        offset_of!(MutatorContext, tlab_top),
    ] {
        load_rt(a, R::V1, RT_CTX + field);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, done);
    }
    a.shr_reg_imm8(R::V9, 17);
    a.sub_reg_imm8(R::V9, 1); // page-table index excludes the leading guard
    load_rt(a, R::V0, RT_NEXT_PAGE);
    a.sub_reg_imm8(R::V0, 1);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, done);
    load_rt(a, R::V1, RT_SPACE_BASE);
    a.cmp_reg_reg(R::V9, R::V1);
    a.jcc_label(C::Below, l.fail);
    // Use the real checked close. Empty pages have no header/bitmap/identity
    // publication, so the same prepared metadata may be rebound later.
    lea_rt(a, R::V7, RT_CTX);
    a.mov_imm64(R::V2, 1);
    a.call_label(l.tlab_page);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V6, RT_REQUEST + offset_of!(AllocationRequest, page));
    a.mov_imm64(R::V0, PAGE_READY);
    a.mov_reg_reg(R::V1, R::V6);
    a.add_reg_imm32(R::V1, d(offset_of!(AllocationPage, state)));
    a.atomic_store_release(R::V1, R::V0);
    load_rt(a, R::V0, RT_NEXT_PAGE);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, RT_NEXT_PAGE, R::V0);
    if l.worker.is_some() {
        load_rt(a, R::V0, RT_BOUNDS);
        a.sub_reg_imm8(R::V0, 1);
        lea_rt(a, R::V1, RT_BOUNDS);
        a.atomic_store_release(R::V1, R::V0);
    }
    store_rt_imm(a, RT_REQUEST + offset_of!(AllocationRequest, page), 0);
    a.bind_text_label(done);
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
}
