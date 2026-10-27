//! Full-cycle reachability for dedicated nonmoving Old objects. A retain CAS
//! only publishes metadata; payload reference slots are scanned in GC quanta.
use super::large_object as o;
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};

pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, o::EPOCH);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, o::EPOCH, R::V0);
    for f in [o::RESCAN, o::SCAN_OBJECT, o::SWEEP_STAGE, o::SWEEP_RUNNING] {
        store_rt_imm(a, f, 0);
    }
    load_rt(a, R::V0, o::LIVE_PAGES);
    a.test_reg_reg(R::V0, R::V0);
    let empty = a.create_text_label();
    let done = a.create_text_label();
    a.jcc_label(C::Equal, empty);
    store_rt_imm(a, o::SCAN_PAGE, 1);
    store_rt_imm(a, o::SWEEP_PAGE, 1);
    a.jmp_label(done);
    a.bind_text_label(empty);
    store_rt_imm(a, o::SCAN_PAGE, 0);
    store_rt_imm(a, o::SWEEP_PAGE, 0);
    a.bind_text_label(done);
}

/// V0=encoded reference, V1=payload offset. Retain once per full epoch, never
/// copy any payload byte. Preserve V0; caller-saved registers may change.
pub(super) fn retain<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V0);
    a.push_reg(R::V0);
    let done = a.create_text_label();
    let active = a.create_text_label();
    load_rt(a, R::V0, RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, RT_PHASE_OPEN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, active);
    load_rt(a, R::V0, RT_COLLECTING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.bind_text_label(active);
    // Dedicated payloads have exactly one permitted start in their first page.
    a.mov_reg_reg(R::V0, R::V1);
    a.and_reg_imm32(R::V0, (HEAP_PAGE_BYTES - 1) as i32);
    a.cmp_reg_imm8(R::V0, size_of::<ObjectHeader>() as i8);
    a.jcc_label(C::NotEqual, l.fail);
    a.shr_reg_imm8(R::V1, 17);
    o::record(a, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, 0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.add_reg_imm32(R::V8, 8);
    load_rt(a, R::V7, o::EPOCH);
    let retry = a.create_text_label();
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V8);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::Equal, done);
    a.mov_reg_reg(R::V6, R::V0);
    a.atomic_compare_exchange(R::V8, R::V7);
    a.cmp_reg_reg(R::V0, R::V6);
    a.jcc_label(C::NotEqual, retry);
    lea_rt(a, R::V8, o::RESCAN);
    a.mov_imm64(R::V7, 1);
    a.atomic_store_release(R::V8, R::V7);
    if l.worker.is_some() {
        store_rt_imm(a, RT_WORKER_DONE, 0);
        super::worker::request_work(a);
    }
    a.bind_text_label(done);
    a.pop_reg(R::V0);
    a.pop_reg(R::V0);
}

/// Quiescent owner end check, under the collector lock. A late retain is
/// rechecked here even when the worker previously reported an empty frontier.
pub(super) fn pending<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    for f in [o::RESCAN, o::SCAN_PAGE, o::SCAN_OBJECT] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, pending);
    }
}
fn field<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V2, o::SCAN_OBJECT);
    load_rt(a, R::V6, RT_HEAP_BASE);
    a.add_reg_reg(R::V2, R::V6);
    load_rt(a, R::V7, o::SCAN_FIELD);
    a.shl_reg_imm8(R::V7, 3);
    a.add_reg_reg(R::V2, R::V7);
}

/// One unit per descriptor or reference slot. Stable page descriptors are
/// independent of both Young mark banks and moving Old run lists.
pub(super) fn scan<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    fwd: E::TextLabel,
    paused: E::TextLabel,
) {
    let done = a.create_text_label();
    let page = a.create_text_label();
    let fields = a.create_text_label();
    let next_field = a.create_text_label();
    let completed = a.create_text_label();
    let next = a.create_text_label();
    let drained = a.create_text_label();
    let restart = a.create_text_label();
    load_rt(a, R::V0, o::SCAN_OBJECT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, fields);
    a.bind_text_label(page);
    load_rt(a, R::V0, o::SCAN_PAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, drained);
    a.cmp_reg_imm32(R::V0, geometry(a).pages as i32);
    a.jcc_label(C::Above, drained);
    super::scan::spend(a, paused);
    o::record(a, R::V0);
    a.load_ptr_disp32(R::V1, R::V8, 0);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, next);
    a.load_ptr_disp32(R::V1, R::V8, 8);
    load_rt(a, R::V2, o::EPOCH);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::NotEqual, next);
    a.load_ptr_disp32(R::V1, R::V8, 16);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Equal, next);
    a.shl_reg_imm8(R::V0, 17);
    a.add_reg_imm32(R::V0, size_of::<ObjectHeader>() as i32);
    store_rt(a, o::SCAN_OBJECT, R::V0);
    load_rt(a, R::V2, RT_HEAP_BASE);
    a.add_reg_reg(R::V2, R::V0);
    a.load_ptr_disp32(
        R::V1,
        R::V2,
        offset_of!(ObjectHeader, state) as i32 - size_of::<ObjectHeader>() as i32,
    );
    a.cmp_reg_imm8(R::V1, crate::gc_abi::OBJECT_PUBLISHED as i8);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V1, R::V2, copy::PAYLOAD);
    a.shr_reg_imm8(R::V1, 3);
    store_rt(a, o::SCAN_COUNT, R::V1);
    a.load_ptr_disp32(R::V1, R::V2, copy::DESCRIPTOR);
    store_rt_imm(a, o::SCAN_FIELD, 0);
    a.cmp_reg_imm8(R::V1, 2);
    a.jcc_label(C::Equal, fields);
    a.cmp_reg_imm8(R::V1, 4);
    a.jcc_label(C::NotEqual, completed);
    store_rt_imm(a, o::SCAN_FIELD, 1); // length is scalar
    a.bind_text_label(fields);
    load_rt(a, R::V0, o::SCAN_FIELD);
    load_rt(a, R::V1, o::SCAN_COUNT);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, completed);
    super::scan::spend(a, paused);
    field(a);
    a.load_ptr_disp32(R::V0, R::V2, 0);
    super::worker::remember_read(a, l);
    super::fields::offset(a, R::V0, R::V1, next_field);
    a.mov_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    super::colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    a.call_label(fwd);
    a.mov_reg_reg(R::V1, R::V0);
    field(a);
    super::worker::heal(a, l, next_field);
    a.bind_text_label(next_field);
    copy::bump(a, o::SCAN_FIELD, R::V0);
    copy::bump(a, o::SCANNED_FIELDS, R::V0);
    a.jmp_label(fields);
    a.bind_text_label(completed);
    load_rt(a, R::V0, o::SCAN_PAGE);
    o::record(a, R::V0);
    load_rt(a, R::V1, o::EPOCH);
    a.store_ptr_disp32(R::V8, 16, R::V1);
    store_rt_imm(a, o::SCAN_OBJECT, 0);
    copy::bump(a, RT_CS_COMPLETED, R::V0);
    a.bind_text_label(next);
    copy::bump(a, o::SCAN_PAGE, R::V0);
    a.jmp_label(page);
    a.bind_text_label(drained);
    store_rt_imm(a, o::SCAN_PAGE, 0);
    lea_rt(a, R::V8, o::RESCAN);
    a.mov_imm64(R::V0, 1);
    a.mov_imm64(R::V7, 0);
    a.atomic_compare_exchange(R::V8, R::V7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, restart);
    a.jmp_label(done);
    a.bind_text_label(restart);
    store_rt_imm(a, o::SCAN_PAGE, 1);
    a.jmp_label(page);
    a.bind_text_label(done);
}
