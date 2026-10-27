//! Short, serialized Old reservations; copy/initialization/claim remain outside.
use super::old_runs as o;
use super::*;
use crate::gc_codegen::production::{load_rt, store_rt};
/// V1=payload bytes/V2=footprint -> V6=header offset,V7=end; preserve V1/V2.
pub(super) fn reserve<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    reserve_or(a, l, l.old_full);
}
/// Failed fresh reservations release the run lock before requesting collection.
/// Sealed run tops remain valid even when an unsuccessful search advances runs.
pub(super) fn reserve_or<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    failure: E::TextLabel,
) {
    let full = a.create_text_label();
    let done = a.create_text_label();
    a.push_reg(R::V1);
    a.push_reg(R::V2);
    o::lock(a);
    // CAS scratch is not the payload/footprint ABI. Recover footprint while
    // retaining the stack pair for the final restoration.
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.push_reg(R::V1);
    a.push_reg(R::V2);
    let retry = a.create_text_label();
    let fit = a.create_text_label();
    a.bind_text_label(retry);
    load_rt(a, R::V0, o::ALLOC);
    o::ptr(a, o::BANK);
    load_rt(a, R::V6, RT_OLD_TOP);
    a.mov_reg_reg(R::V7, R::V6);
    a.add_reg_reg(R::V7, R::V2);
    a.load_ptr_disp32(R::V9, R::V8, o::BASE);
    a.mov_reg_reg(R::V10, R::V7);
    a.sub_reg_reg(R::V10, R::V9);
    load_rt(a, R::V9, o::PREFIX);
    a.add_reg_reg(R::V10, R::V9);
    load_rt(a, R::V9, pool::OLD_PAGES);
    a.shl_reg_imm8(R::V9, 17);
    a.cmp_reg_reg(R::V10, R::V9);
    a.jcc_label(C::Above, full);
    a.load_ptr_disp32(R::V9, R::V8, o::END);
    a.cmp_reg_reg(R::V7, R::V9);
    a.jcc_label(C::Below, fit);
    a.jcc_label(C::Equal, fit);
    // Seal this run's actual top and charge its unused tail once. No padding
    // object is fabricated; scanners use the recorded top, not the run end.
    a.store_ptr_disp32(R::V8, o::TOP, R::V6);
    a.load_ptr_disp32(R::V6, R::V8, o::BASE);
    a.sub_reg_reg(R::V9, R::V6);
    load_rt(a, R::V6, o::PREFIX);
    a.add_reg_reg(R::V9, R::V6);
    store_rt(a, o::PREFIX, R::V9);
    a.add_reg_imm32(R::V0, 1);
    load_rt(a, R::V9, o::COUNT);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::AboveOrEqual, full);
    store_rt(a, o::ALLOC, R::V0);
    copy::bump(a, o::SWITCHES, R::V9);
    o::ptr(a, o::BANK);
    a.load_ptr_disp32(R::V6, R::V8, o::BASE);
    store_rt(a, RT_OLD_TOP, R::V6);
    a.jmp_label(retry);
    a.bind_text_label(fit);
    generation::test_role(a, R::V6, generation::OLD);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::V9, R::V7);
    a.sub_reg_imm32(R::V9, 1);
    generation::test_role(a, R::V9, generation::OLD);
    a.jcc_label(C::NotEqual, l.fail);
    // A large footprint may cover more than the two endpoint pages.
    // Preserve the exact reservation ABI across each checked physical commit.
    for r in [R::V0, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    a.mov_reg_reg(R::V10, R::V6);
    a.and_reg_imm32(R::V10, -(HEAP_PAGE_BYTES as i32));
    let commit_next = a.create_text_label();
    a.bind_text_label(commit_next);
    super::commit::ensure(a, l, R::V10);
    // These pages now contain an actual Old reservation, rather than an
    // untouched Ready pool page. Shared metadata leases admit Closed pages.
    a.mov_reg_reg(R::V0, R::V10);
    a.shr_reg_imm8(R::V0, 17);
    a.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(a, R::V9, R::V0, R::V11);
    a.load_ptr_disp32(R::V11, R::V9, d(offset_of!(AllocationPage, run_head)));
    a.cmp_reg_reg(R::V11, R::V0);
    let tail = a.create_text_label();
    a.jcc_label(C::NotEqual, tail);
    a.add_reg_imm32(R::V9, d(offset_of!(AllocationPage, state)));
    a.mov_imm64(R::V11, crate::gc_abi::PAGE_CLOSED);
    a.atomic_store_release(R::V9, R::V11);
    a.bind_text_label(tail);
    a.add_reg_imm32(R::V10, HEAP_PAGE_BYTES as i32);
    a.cmp_reg_reg(R::V10, R::V7);
    a.jcc_label(C::Below, commit_next);
    for r in [R::V11, R::V10, R::V9, R::V0] {
        a.pop_reg(r);
    }
    a.store_ptr_disp32(R::V8, o::TOP, R::V7);
    store_rt(a, RT_OLD_TOP, R::V7);
    o::unlock(a);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.jmp_label(done);
    a.bind_text_label(full);
    o::unlock(a);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    a.jmp_label(failure);
    a.bind_text_label(done);
}
/// Collector lock held. Read a coherent top; never advance beyond the current
/// allocation run. Run transitions consume budget independently of objects.
pub(super) fn scan<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    again: E::TextLabel,
    done: E::TextLabel,
    paused: E::TextLabel,
) {
    o::lock(a);
    load_rt(a, R::V0, o::SCAN);
    o::ptr(a, o::BANK);
    load_rt(a, R::V6, RT_OLD_SCAN);
    a.load_ptr_disp32(R::V7, R::V8, o::TOP);
    let ready = a.create_text_label();
    let advance = a.create_text_label();
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::Below, ready);
    load_rt(a, R::V1, o::ALLOC);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, advance);
    o::unlock(a);
    a.jmp_label(done);
    a.bind_text_label(advance);
    a.mov_reg_reg(R::V10, R::V0);
    a.add_reg_imm32(R::V10, 1);
    o::unlock(a);
    scan::spend(a, paused);
    store_rt(a, o::SCAN, R::V10);
    a.mov_reg_reg(R::V0, R::V10);
    o::ptr(a, o::BANK);
    a.load_ptr_disp32(R::V0, R::V8, o::BASE);
    store_rt(a, RT_OLD_SCAN, R::V0);
    a.jmp_label(again);
    a.bind_text_label(ready);
    o::unlock(a);
    // Existing object scan reloads the cursor and reads the descriptor. A
    // reserved-but-unpublished header still pauses before any field access.
    let _ = l;
}
pub(super) fn pending<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    load_rt(a, R::V0, o::SCAN);
    load_rt(a, R::V1, o::ALLOC);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, pending);
    load_rt(a, R::V0, RT_OLD_SCAN);
    load_rt(a, R::V1, RT_OLD_TOP);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, pending);
}
