//! Reserve initially free gap pages after a real full Old space is populated.
//! These are independent empty pool reservations, not fabricated Old objects.
use super::super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};
pub(crate) fn fragment_old_destinations(a: &mut crate::NativeCodeGenerator) {
    // Full capacity fitting may place the populated source in either end of
    // the pool. Split actual free pages and preserve a real Young candidate,
    // instead of assuming that two fixed numeric intervals are empty.
    idle_fragment(a);
}
pub(crate) fn old_run_metric<E: PortableAsm>(a: &mut E, switches: bool) {
    load_rt(
        a,
        R::V0,
        if switches {
            old_runs::SWITCHES
        } else {
            old_runs::COUNT
        },
    );
}
/// Canonical address in an actual independently reserved gap (fixture slot 44).
pub(crate) fn gap_address<E: PortableAsm>(a: &mut E) {
    a.load_rbp_slot(R::V0, 8 * 44);
}

/// Quiescent idle shrink, then leave one empty 64-page destination candidate
/// and reserve individual gap pages everywhere else that is currently free.
pub(crate) fn idle_fragment(a: &mut crate::NativeCodeGenerator) {
    let runtime = a.gc_gen.take().expect("generational runtime");
    worker::lock(a, &runtime.labels);
    old_runs::shrink_unused(a, &runtime.labels);
    a.mov_imm64(R::V7, 64);
    a.mov_imm64(R::V6, generation::YOUNG);
    pool::claim(a, &runtime.labels);
    a.store_rbp_slot(8 * 43, R::V0); // Independent empty run to return below.
    a.mov_imm64(R::V0, 0);
    a.store_rbp_slot(8 * 44, R::V0);
    a.store_rbp_slot(8 * 47, R::V0);
    for page in (8..super::super::geometry(a).pages).step_by(9) {
        let occupied = a.create_text_label();
        lea_rt(
            a,
            R::V0,
            (super::super::layout(a).generations + (page + 1) * 8) as usize,
        );
        a.atomic_load_acquire(R::V0, R::V0);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, occupied);
        a.load_rbp_slot(R::V0, 8 * 47);
        a.add_reg_imm32(R::V0, 1);
        a.store_rbp_slot(8 * 47, R::V0);
        a.cmp_reg_imm8(R::V0, 2);
        let saved = a.create_text_label();
        a.jcc_label(C::NotEqual, saved);
        load_rt(a, R::V0, RT_HEAP_BASE);
        a.add_reg_imm32(R::V0, ((page + 1) * HEAP_PAGE_BYTES) as i32);
        a.store_rbp_slot(8 * 44, R::V0);
        a.bind_text_label(saved);
        a.mov_imm64(R::V1, page);
        a.mov_imm64(R::V2, 1);
        a.mov_imm64(R::V6, generation::YOUNG);
        pool::publish(a, &runtime.labels);
        a.bind_text_label(occupied);
    }
    // This reservation has no allocation/forwarding cells or handles. Return
    // its empty pages, preserving a genuine free Young destination candidate.
    a.load_rbp_slot(R::V6, 8 * 43);
    a.add_reg_imm32(R::V6, 1);
    a.shl_reg_imm8(R::V6, 3);
    a.add_reg_reg(R::V6, R::V3);
    a.add_reg_imm32(R::V6, super::super::layout(a).generations as i32);
    a.mov_imm64(R::V7, 64);
    let next = a.create_text_label();
    a.bind_text_label(next);
    a.mov_imm64(R::V0, 0);
    a.atomic_store_release(R::V6, R::V0);
    copy::bump(a, pool::RELEASED, R::V9);
    a.add_reg_imm32(R::V6, 8);
    a.sub_reg_imm8(R::V7, 1);
    a.jcc_label(C::NotEqual, next);
    worker::unlock(a, &runtime.labels);
    a.gc_gen = Some(runtime);
}

/// Inspect source membership directly, even after the relocation phase closes.
/// V0 is a canonical source payload address; result is the page's raw tag.
pub(crate) fn old_source_tag<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.shr_reg_imm8(R::V0, 17);
    a.shl_reg_imm8(R::V0, 3);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, super::super::layout(a).old_sources as i32);
    a.atomic_load_acquire(R::V0, R::V0);
}

/// Reserve a genuine private Old copy, optionally delaying descriptor
/// publication. No source forwarding claim is installed by this test actor.
/// V0=canonical source payload; returns destination header offset.
pub(crate) fn private_old_copy(a: &mut crate::NativeCodeGenerator, publish: bool) {
    let runtime = a.gc_gen.take().expect("generational runtime");
    store_rt(a, RT_W_SRC, R::V0);
    worker::lock(a, &runtime.labels);
    a.mov_imm64(R::V1, MAX_PAYLOAD);
    a.mov_imm64(R::V2, HEAP_PAGE_BYTES);
    old_reserve::reserve(a, &runtime.labels);
    a.push_reg(R::V6);
    a.push_reg(R::V6);
    load_rt(a, R::V8, RT_HEAP_BASE);
    a.add_reg_reg(R::V8, R::V6);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, payload_bytes)), R::V1);
    a.mov_imm64(R::V2, crate::gc_abi::OBJECT_PUBLISHED);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, state)), R::V2);
    a.mov_reg_reg(R::V7, R::V8);
    a.add_reg_imm32(R::V7, size_of::<ObjectHeader>() as i32);
    load_rt(a, R::V6, RT_W_SRC);
    a.rep_movsb();
    a.pop_reg(R::V8);
    a.pop_reg(R::V8);
    a.push_reg(R::V8);
    a.push_reg(R::V8);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V8, R::V1);
    identity::copy_record(a, &runtime.labels, RT_W_SRC);
    if publish {
        publish_private_header(a);
    }
    worker::unlock(a, &runtime.labels);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
    a.gc_gen = Some(runtime);
}
fn publish_private_header<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V9, R::V8);
    a.add_reg_imm32(R::V9, d(offset_of!(ObjectHeader, descriptor_id)));
    a.mov_imm64(R::V2, 1);
    a.atomic_store_release(R::V9, R::V2);
    slot_owner::publish(a);
}
/// V0=previously reserved private header offset.
pub(crate) fn publish_private_old<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V8, RT_HEAP_BASE);
    a.add_reg_reg(R::V8, R::V0);
    publish_private_header(a);
}
pub(crate) fn old_scan_cursor<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, RT_OLD_SCAN);
}
