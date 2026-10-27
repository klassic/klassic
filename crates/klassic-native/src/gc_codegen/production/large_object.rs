//! Large fresh objects occupy dedicated nonmoving Old pages in the shared pool.
//! Initialization runs outside the collector lock. A later owner safepoint
//! release-publishes readiness before a marker/relocator may inspect the payload.
use super::*;
use crate::gc_abi::{OBJECT_INITIALIZING, OBJECT_PUBLISHED};
use crate::gc_codegen::production::{OwnerState, load_rt, owner_state, store_rt};
#[repr(C)]
pub(super) struct AllocationState {
    retry: u64,
    header: u64,
    reserve_second_run: u64,
    allocation_pages: u64,
}
#[repr(C)]
pub(super) struct State {
    allocations: u64,
    bytes: u64,
    retries: u64,
    epoch: u64,
    rescan: u64,
    scan_page: u64,
    scan_object: u64,
    scan_field: u64,
    scan_count: u64,
    sweep_page: u64,
    sweep_end: u64,
    sweep_stage: u64,
    sweep_cursor: u64,
    sweep_running: u64,
    live_pages: u64,
    reclaimed_pages: u64,
    scanned_fields: u64,
    retained_pages: u64,
}
pub(super) const ALLOC_PAGES: usize =
    owner_state::offset(offset_of!(OwnerState, large_allocation.allocation_pages));
const RETRY: usize = owner_state::offset(offset_of!(OwnerState, large_allocation.retry));
const HEADER_ADDR: usize = owner_state::offset(offset_of!(OwnerState, large_allocation.header));
pub(crate) const ALLOCATIONS: usize = offset_of!(Runtime, large_object.allocations);
pub(crate) const BYTES: usize = offset_of!(Runtime, large_object.bytes);
pub(crate) const RETRIES: usize = offset_of!(Runtime, large_object.retries);
macro_rules! f { ($($n:ident:$v:ident),* $(,)?) => { $(
    pub(super) const $n: usize = offset_of!(Runtime, large_object.$v);
)* }; }
f! { EPOCH:epoch, RESCAN:rescan,
SCAN_PAGE:scan_page, SCAN_OBJECT:scan_object, SCAN_FIELD:scan_field, SCAN_COUNT:scan_count,
SWEEP_PAGE:sweep_page, SWEEP_END:sweep_end, SWEEP_STAGE:sweep_stage,
SWEEP_CURSOR:sweep_cursor, SWEEP_RUNNING:sweep_running,
LIVE_PAGES:live_pages, RECLAIMED_PAGES:reclaimed_pages, SCANNED_FIELDS:scanned_fields }
pub(super) const RETAINED_PAGES: usize = offset_of!(Runtime, large_object.retained_pages);
pub(super) const RESERVE_SECOND_RUN: usize =
    owner_state::offset(offset_of!(OwnerState, large_allocation.reserve_second_run));
/// Page number (guard-inclusive) -> dedicated descriptor V8; other registers survive.
pub(super) fn record<E: PortableAsm>(a: &mut E, page: R) {
    a.mov_reg_reg(R::V8, page);
    a.shl_reg_imm8(R::V8, 5);
    a.add_reg_reg(R::V8, R::V3);
    a.add_reg_imm32(R::V8, layout(a).large_records as i32);
}

pub(crate) fn max_payload<E: PortableAsm>(a: &E) -> u64 {
    // Even the smallest source needs a Young and an Old page. Actual current
    // occupancy and a contiguous Young destination are checked at admission.
    let g = geometry(a);
    (g.pages - g.young_pages - g.old_bytes / HEAP_PAGE_BYTES - 2) * HEAP_PAGE_BYTES
        - size_of::<ObjectHeader>() as u64
}
pub(crate) fn report_offsets() -> [i32; 7] {
    [
        d(ALLOCATIONS),
        d(BYTES),
        d(RETRIES),
        d(LIVE_PAGES),
        d(RECLAIMED_PAGES),
        d(SCANNED_FIELDS),
        d(EPOCH),
    ]
}
fn request<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V1, RT_FRESH_SIZE);
    a.cmp_reg_imm32(R::V1, max_payload(a) as i32);
    a.jcc_label(C::Above, l.too_large);
    load_rt(a, R::V0, RT_FRESH_TAG);
    let raw = a.create_text_label();
    let sized = a.create_text_label();
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Equal, raw);
    a.cmp_reg_imm8(R::V0, 2);
    a.jcc_label(C::Equal, sized);
    a.cmp_reg_imm8(R::V0, 4);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(sized);
    a.mov_reg_reg(R::V2, R::V1);
    a.and_reg_imm32(R::V2, 7);
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(raw);
    a.mov_reg_reg(R::V2, R::V1);
    a.add_reg_imm32(R::V2, 15);
    a.and_reg_imm32(R::V2, -16);
    a.add_reg_imm32(R::V2, size_of::<ObjectHeader>() as i32);
}
/// Allocation entry has retained fresh size/tag. Returns raw payload V0;
/// common allocation code assigns identity and charges application credit.
pub(super) fn allocate<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    request(a, l); // Reject unrepresentable sizes before any collection or map.
    a.call_label(l.publish_pending);
    // Complete an existing Young snapshot before exposing a new Old holder;
    // initialized fields then enter remembered sets before the next snapshot.
    super::young_mark::wait(a, l);
    super::worker::lock(a, l);
    a.call_label(l.finish);
    super::worker::unlock(a, l);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_ALLOC_ROOT + REQUEST_VALUE, R::V0);
    root_step(a, l, RT_ALLOC_ROOT, ROOT_STORE);
    store_rt_imm(a, RETRY, 0);
    let retry = a.create_text_label();
    let full = a.create_text_label();
    let allocated = a.create_text_label();
    a.bind_text_label(retry);
    request(a, l);
    // Dedicated pages share capacity with both future moving destinations.
    a.add_reg_imm32(R::V2, (HEAP_PAGE_BYTES - 1) as i32);
    a.shr_reg_imm8(R::V2, 17);
    store_rt(a, ALLOC_PAGES, R::V2);
    super::worker::lock(a, l);
    let unavailable = a.create_text_label();
    load_rt(a, R::V2, ALLOC_PAGES);
    a.mov_reg_reg(R::V7, R::V2);
    super::large_reserve::claim_or(a, l, unavailable);
    a.add_reg_imm32(R::V0, 1);
    record(a, R::V0);
    load_rt(a, R::V1, ALLOC_PAGES);
    a.store_ptr_disp32(R::V8, 0, R::V1);
    a.mov_imm64(R::V1, 0);
    a.store_ptr_disp32(R::V8, 8, R::V1);
    a.store_ptr_disp32(R::V8, 16, R::V1);
    a.shl_reg_imm8(R::V0, 17);
    store_rt(a, HEADER_ADDR, R::V0);
    load_rt(a, R::V1, LIVE_PAGES);
    load_rt(a, R::V2, ALLOC_PAGES);
    a.add_reg_reg(R::V1, R::V2);
    store_rt(a, LIVE_PAGES, R::V1);
    super::worker::unlock(a, l);
    // OS commitment/zeroing are outside the collector and reservation locks.
    load_rt(a, R::V10, HEADER_ADDR);
    load_rt(a, R::V11, ALLOC_PAGES);
    a.shl_reg_imm8(R::V11, 17);
    a.add_reg_reg(R::V11, R::V10);
    let page = a.create_text_label();
    a.bind_text_label(page);
    super::commit::ensure(a, l, R::V10);
    a.mov_reg_reg(R::V0, R::V10);
    a.shr_reg_imm8(R::V0, 17);
    a.sub_reg_imm8(R::V0, 1);
    super::scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(AllocationPage, run_head)));
    a.cmp_reg_reg(R::V1, R::V0);
    let tail = a.create_text_label();
    a.jcc_label(C::NotEqual, tail);
    a.add_reg_imm32(R::V8, d(offset_of!(AllocationPage, state)));
    a.mov_imm64(R::V1, crate::gc_abi::PAGE_CLOSED);
    a.atomic_store_release(R::V8, R::V1);
    a.bind_text_label(tail);
    a.add_reg_imm32(R::V10, HEAP_PAGE_BYTES as i32);
    a.cmp_reg_reg(R::V10, R::V11);
    a.jcc_label(C::Below, page);
    load_rt(a, R::V8, HEADER_ADDR);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V8, R::V1);
    store_rt(a, HEADER_ADDR, R::V8);
    super::identity::prepare_record(a, l);
    load_rt(a, R::V1, RT_FRESH_SIZE);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, payload_bytes)), R::V1);
    load_rt(a, R::V1, RT_FRESH_TAG);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, descriptor_id)), R::V1);
    a.mov_imm64(R::V1, OBJECT_INITIALIZING);
    a.store_ptr_disp32(R::V8, d(offset_of!(ObjectHeader, state)), R::V1);
    // All reference words (and padding) start null, including on reused runs.
    a.mov_reg_reg(R::V6, R::V8);
    a.add_reg_imm32(R::V6, size_of::<ObjectHeader>() as i32);
    load_rt(a, R::V7, RT_FRESH_SIZE);
    a.add_reg_imm32(R::V7, 15);
    a.and_reg_imm32(R::V7, -16);
    a.add_reg_reg(R::V7, R::V6);
    a.mov_imm64(R::V0, 0);
    let clear = a.create_text_label();
    a.bind_text_label(clear);
    a.store_ptr_disp32(R::V6, 0, R::V0);
    a.add_reg_imm32(R::V6, 8);
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::Below, clear);
    // Hints/start membership are ready before returning; owner lookup still
    // rejects INITIALIZING, so later readiness publication is constant-count.
    super::slot_owner::publish(a);
    load_rt(a, R::V0, HEADER_ADDR);
    a.add_reg_imm32(R::V0, size_of::<ObjectHeader>() as i32);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.shl_reg_imm8(R::V0, 16);
    super::colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    store_rt(a, RT_PENDING_REF, R::V0);
    store_rt_imm(a, RT_PENDING_PAGE, 0);
    store_rt_imm(a, RT_PENDING_CELL, 0);
    // New Old objects are black in an active Old snapshot; initialization
    // stores retain both old/new referents through the existing store barrier.
    load_rt(a, R::V0, RT_PENDING_REF);
    super::mark::alloc_black(a, l);
    super::copy::bump(a, ALLOCATIONS, R::V0);
    load_rt(a, R::V1, RT_FRESH_SIZE);
    a.add_reg_imm32(R::V1, 15);
    a.and_reg_imm32(R::V1, -16);
    a.add_reg_imm32(R::V1, size_of::<ObjectHeader>() as i32);
    load_rt(a, R::V0, BYTES);
    a.add_reg_reg(R::V0, R::V1);
    store_rt(a, BYTES, R::V0);
    load_rt(
        a,
        R::V0,
        RT_CTX + offset_of!(MutatorContext, allocated_bytes),
    );
    a.mov_reg_reg(R::V2, R::V0);
    a.add_reg_reg(R::V0, R::V1);
    let counted = a.create_text_label();
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::AboveOrEqual, counted);
    a.mov_imm64(R::V0, u64::MAX);
    a.bind_text_label(counted);
    store_rt(
        a,
        RT_CTX + offset_of!(MutatorContext, allocated_bytes),
        R::V0,
    );
    load_rt(a, R::V0, HEADER_ADDR);
    a.add_reg_imm32(R::V0, size_of::<ObjectHeader>() as i32);
    a.jmp_label(allocated);
    a.bind_text_label(unavailable);
    super::worker::unlock(a, l);
    a.jmp_label(full);
    a.bind_text_label(full);
    load_rt(a, R::V0, RETRY);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.old_full);
    store_rt_imm(a, RETRY, 1);
    super::copy::bump(a, RETRIES, R::V0);
    store_rt_imm(a, super::old_cycle::REQUEST, super::old_cycle::CAPACITY);
    a.call_label(l.collect);
    super::worker::lock(a, l);
    a.call_label(l.finish);
    super::worker::unlock(a, l);
    a.jmp_label(retry);
    a.bind_text_label(allocated);
}
/// Pending large Old header has no Young AllocationPage/ForwardCell lease.
/// Initialization preceded this owner safepoint; only readiness is published.
pub(super) fn publish<E: PortableAsm>(a: &mut E) {
    a.shr_reg_imm8(R::V0, 16);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V0, R::V1);
    a.add_reg_imm32(
        R::V0,
        d(offset_of!(ObjectHeader, state)) - size_of::<ObjectHeader>() as i32,
    );
    a.mov_imm64(R::V1, OBJECT_PUBLISHED);
    a.atomic_store_release(R::V0, R::V1);
}
