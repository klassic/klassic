//! Common growable token/incarnation leases over actual production pages. A bounded
//! source census precedes every cleanup, including shared mark arenas. It
//! runs after peers resume; retained Old/destination leases do not block it.
//! A metadata lease grants no strong reachability or raw payload access.
use super::*;
use crate::gc_abi::{HeapPool, MetadataLeasePool, MetadataLeaseSlot};
#[path = "lease_growth.rs"]
mod growth;
#[path = "metadata_transfer.rs"]
mod transfer;
pub(super) use transfer::Owner;
pub(super) const CAPACITY: u64 = 4096;
#[repr(C)]
pub(super) struct State {
    heap: HeapPool,
    pool: MetadataLeasePool,
    census: u64,
    cursor: u64,
    waits: u64,
    units: u64,
    growth: growth::State,
}
const BASE: usize = offset_of!(Runtime, metadata_leases);
pub(super) const POOL: usize = BASE + offset_of!(State, pool);
pub(super) const SIZE: usize = POOL + offset_of!(MetadataLeasePool, capacity);
const ENTRIES: usize = POOL + offset_of!(MetadataLeasePool, slots);
const HEAD: usize = POOL + offset_of!(MetadataLeasePool, free_head);
const HEAP: usize = BASE + offset_of!(State, heap);
const CENSUS: usize = BASE + offset_of!(State, census);
pub(super) const CURSOR: usize = BASE + offset_of!(State, cursor);
pub(super) const WAITS: usize = BASE + offset_of!(State, waits);
pub(super) const UNITS: usize = BASE + offset_of!(State, units);

pub(super) fn startup<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, RT_HEAP_BASE);
    store_rt(a, HEAP + offset_of!(HeapPool, heap_base), R::V0);
    load_rt(a, R::V0, RT_PAGES);
    store_rt(a, HEAP + offset_of!(HeapPool, pages), R::V0);
    store_rt_imm(
        a,
        HEAP + offset_of!(HeapPool, page_count),
        geometry(a).pages,
    );
    store_rt_addr(a, POOL + offset_of!(MetadataLeasePool, heap), HEAP);
    store_rt_addr(
        a,
        POOL + offset_of!(MetadataLeasePool, slots),
        layout(a).metadata_lease_slots as usize,
    );
    store_rt_imm(a, POOL + offset_of!(MetadataLeasePool, capacity), CAPACITY);
    store_rt_imm(a, POOL + offset_of!(MetadataLeasePool, free_head), 1);
    lea_rt(a, R::V8, layout(a).metadata_lease_slots as usize);
    a.mov_imm64(R::V0, 2);
    let again = a.create_text_label();
    a.bind_text_label(again);
    a.store_ptr_disp32(R::V8, d(offset_of!(MetadataLeaseSlot, next_free)), R::V0);
    a.add_reg_imm32(R::V8, 32);
    a.add_reg_imm32(R::V0, 1);
    a.cmp_reg_imm32(R::V0, CAPACITY as i32 + 1);
    a.jcc_label(C::Below, again);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V8, d(offset_of!(MetadataLeaseSlot, next_free)), R::V0);
}

/// V0=registered source RootHandle address, V7=empty MetadataLeaseRequest.
/// Shared metadata status, or 4 while source retirement forbids new leases.
/// Release/resolve of existing leases remain available during retirement.
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    growth::emit(a, l);
    transfer::emit(a, l);
    transfer::emit_acquire(a, l);
    for (entry, op) in [
        (l.metadata_lease_release_actor, 1),
        (l.metadata_lease_resolve_actor, 2),
    ] {
        a.bind_text_label(entry);
        a.push_reg(R::V3);
        a.push_reg(R::OwnerContext);
        owner_state::bind_running(a, l);
        a.mov_reg_reg(R::V6, R::V7);
        worker::lock(a, l);
        lea_rt(a, R::V7, POOL);
        a.mov_imm64(R::V2, op);
        a.call_label(l.metadata_lease_step);
        if op == 1 {
            let failed = a.create_text_label();
            a.test_reg_reg(R::V0, R::V0);
            a.jcc_label(C::NotEqual, failed);
            a.push_reg(R::V0);
            worker::request_work(a);
            a.pop_reg(R::V0);
            a.bind_text_label(failed);
        }
        worker::unlock(a, l);
        worker::wake_if_pending(a, l);
        a.call_label(l.lease_grow_service);
        a.pop_reg(R::OwnerContext);
        a.pop_reg(R::V3);
        a.ret();
    }
}

pub(super) fn begin_retire<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, POOL + offset_of!(MetadataLeasePool, active));
    a.push_reg(R::V1);
    a.push_reg(R::V2);
    a.mov_reg_reg(R::V2, R::V0);
    load_rt(a, R::V1, super::mark_queue::HOLDERS);
    a.add_reg_reg(R::V0, R::V1);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::Below, l.fail);
    // Both are real uniquely owned page holders; neither count may wrap.
    store_rt(a, CENSUS, R::V0);
    a.pop_reg(R::V2);
    a.pop_reg(R::V1);
    store_rt_imm(a, CURSOR, 0);
}

/// One page per budget unit before any payload, bitmap, record or OS cleanup.
/// Admission stays closed through Retire, so passed pages cannot gain a lease.
pub(super) fn before_retire<E: PortableAsm>(a: &mut E, paused: E::TextLabel) {
    let top = a.create_text_label();
    let done = a.create_text_label();
    let check = a.create_text_label();
    let next = a.create_text_label();
    let complete = a.create_text_label();
    a.bind_text_label(top);
    load_rt(a, R::V0, CENSUS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V8, CURSOR);
    a.cmp_reg_imm32(R::V8, geometry(a).pages as i32);
    a.jcc_label(C::AboveOrEqual, complete);
    scan::spend(a, paused);
    copy::bump(a, UNITS, R::V0);
    source(a, check, next);
    a.bind_text_label(check);
    scan::page_ptr(a, R::V1, R::V8, R::V0);
    a.load_ptr_disp32(R::V0, R::V1, d(offset_of!(AllocationPage, metadata_leases)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, next);
    copy::bump(a, WAITS, R::V0);
    a.jmp_label(paused);
    a.bind_text_label(next);
    copy::bump(a, CURSOR, R::V0);
    a.jmp_label(top);
    a.bind_text_label(complete);
    store_rt_imm(a, CENSUS, 0);
    a.bind_text_label(done);
}

/// V8=physical zero-based index; V0..V2 scratch. Exact retirement ownership,
/// shared by the census and admission of a transferred destination lease.
pub(super) fn source<E: PortableAsm>(a: &mut E, check: E::TextLabel, next: E::TextLabel) {
    let old = a.create_text_label();
    load_rt(a, R::V0, RT_FROM_BASE);
    a.cmp_reg_reg(R::V8, R::V0);
    a.jcc_label(C::Below, old);
    load_rt(a, R::V0, capacity::SOURCE_END);
    a.cmp_reg_reg(R::V8, R::V0);
    a.jcc_label(C::Below, check);
    a.bind_text_label(old);
    load_rt(a, R::V0, RT_FULL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, next);
    a.mov_reg_reg(R::V1, R::V8);
    a.add_reg_imm32(R::V1, 1);
    a.shl_reg_imm8(R::V1, 3);
    a.add_reg_reg(R::V1, R::V3);
    a.add_reg_imm32(R::V1, layout(a).old_sources as i32);
    a.load_ptr_disp32(R::V0, R::V1, 0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, check);
    a.mov_reg_reg(R::V0, R::V8);
    a.add_reg_imm32(R::V0, 1);
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V0, R::V3);
    a.add_reg_imm32(R::V0, layout(a).large_records as i32);
    a.load_ptr_disp32(R::V1, R::V0, 0);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, next);
    a.load_ptr_disp32(R::V1, R::V0, 8);
    load_rt(a, R::V2, large_object::EPOCH);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::Equal, next);
    a.jmp_label(check);
}

pub(super) fn full<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    growth::request(a, l);
    quiescence::restore(a);
}
pub(super) fn mirror<E: PortableAsm>(a: &mut E) {
    growth::mirror(a);
}
pub(super) fn work<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    growth::step(a, l);
}
pub(super) fn retry<E: PortableAsm>(a: &mut E, release: E::TextLabel) {
    growth::retry(a, release);
}
pub(super) fn after_unlock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    growth::after_unlock(a, l);
}
/// Metadata FULL remains status2 at its ABI. Weak APIs expose pending growth
/// as retry2 and a real failed allocation as resource6.
pub(super) fn weak_pending<E: PortableAsm>(a: &mut E, retry: E::TextLabel) {
    // The full hook may just have published the candidate. Its original
    // failed acquire remains unchanged, but the next attempt can now succeed.
    a.push_reg(R::V0);
    load_rt(a, R::V0, HEAD);
    a.test_reg_reg(R::V0, R::V0);
    a.pop_reg(R::V0);
    a.jcc_label(C::NotEqual, retry);
    growth::retry(a, retry);
}
#[cfg(test)]
pub(super) fn growth_metric<E: PortableAsm>(a: &mut E, index: usize) {
    load_rt(a, R::V0, growth::metric_offsets()[index]);
}
#[cfg(test)]
pub(super) fn growth_pause<E: PortableAsm>(a: &mut E, paused: bool) {
    growth::pause(a, paused);
}
