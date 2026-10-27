//! The M-G1/M-G2 allocator: the legacy `gc_alloc` ABI over `alloc_into`,
//! with close/bind refill, journal drains, evacuation on exhaustion and an
//! honest OOM.
use super::*;
use crate::gc_codegen::production::{lea_rt, load_rt, store_rt};

pub(super) fn emit_alloc<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    runtime: E::DataLabel,
    stress: bool,
) {
    let actor_body = a.create_text_label();
    a.bind_text_label(l.alloc);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    load_runtime(a, runtime);
    a.jmp_label(actor_body);
    a.bind_text_label(l.alloc_actor);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    a.bind_text_label(actor_body);
    store_rt(a, RT_FRESH_SIZE, R::V7);
    store_rt(a, RT_FRESH_TAG, R::V6);
    // This safepoint ends the preceding constructor before relocation progress
    // or refill admission can wait for its actual readiness publication.
    a.call_label(l.publish_pending);
    quiescence::poll(a, l);
    mark_queue::service(a, l);
    super::uncommit::owner_poll(a, l);
    // Advance an open relocation phase by one bounded quantum (or, with a
    // worker, finish one it scanned); maybe start a mark cycle.
    a.call_label(l.quantum);
    // Stress's forced collection below owns this allocation safepoint.
    if !stress {
        a.call_label(l.mark_cycle_poll);
    }
    mark::trigger(a, l);
    restore_fresh_request(a);
    if stress {
        // --gc-stress: evacuate before every allocation, so any reference
        // left outside a RootCell across an allocation is caught at once.
        a.call_label(l.collect);
    }
    super::admission::wait_for_refill(a, l);
    // Copies use alloc_raw and overwrite its request, but never these retained
    // fresh-allocation arguments. Restore them after any relocation progress.
    restore_fresh_request(a);
    let large = a.create_text_label();
    let allocated = a.create_text_label();
    load_rt(a, R::V0, RT_FRESH_SIZE);
    a.cmp_reg_imm32(R::V0, MAX_PAYLOAD as i32);
    a.jcc_label(C::Above, large);
    a.call_label(l.alloc_raw);
    a.jmp_label(allocated);
    a.bind_text_label(large);
    super::large_object::allocate(a, l);
    a.bind_text_label(allocated);
    identity::assign_new(a, l);
    super::capacity::fresh(a, l);
    super::old_policy::record_fresh(a);
    super::old_footprint::fresh(a);
    super::start_policy::poll(a, l);
    worker::wake_if_pending(a, l);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();

    // alloc_raw: V3 = runtime, size in alloc_size -> V0 = payload address.
    a.bind_text_label(l.alloc_raw);
    // Publish the previous object before its page can close, then empty the
    // scratch root that alloc_into requires.
    a.call_label(l.publish_pending);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_ALLOC_ROOT + REQUEST_VALUE, R::V0);
    root_step(a, l, RT_ALLOC_ROOT, ROOT_STORE);
    load_rt(a, R::V0, RT_ALLOC_SIZE);
    a.mov_imm64(R::V1, MAX_PAYLOAD);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.too_large);
    // Descriptor and element count from the tag: raw bytes count bytes,
    // records count reference words, lists add one fixed length word.
    let record = a.create_text_label();
    let list = a.create_text_label();
    let chosen = a.create_text_label();
    load_rt(a, R::V1, RT_ALLOC_TAG);
    a.cmp_reg_imm8(R::V1, 2);
    a.jcc_label(C::Equal, record);
    a.cmp_reg_imm8(R::V1, 4);
    a.jcc_label(C::Equal, list);
    a.cmp_reg_imm8(R::V1, 1);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(a, R::V2, RT_BUILTINS + 8);
    a.jmp_label(chosen);
    a.bind_text_label(record);
    lea_rt(a, R::V2, RT_BUILTINS + 72);
    a.shr_reg_imm8(R::V0, 3);
    a.jmp_label(chosen);
    a.bind_text_label(list);
    lea_rt(a, R::V2, RT_BUILTINS + 136);
    a.sub_reg_imm8(R::V0, 8);
    a.shr_reg_imm8(R::V0, 3);
    a.bind_text_label(chosen);
    store_rt(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, descriptor),
        R::V2,
    );
    store_rt(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, elements),
        R::V0,
    );
    let attempt = a.create_text_label();
    let done = a.create_text_label();
    let refill = a.create_text_label();
    let not_refill = a.create_text_label();
    let bind = a.create_text_label();
    a.bind_text_label(attempt);
    let copying = a.create_text_label();
    let reserved = a.create_text_label();
    load_rt(a, R::V0, super::moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, reserved);
    load_rt(a, R::V0, RT_COLLECTING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, copying);
    super::moving_round::admission::reserve(a, l);
    a.bind_text_label(copying);
    a.bind_text_label(reserved);
    allocation_records::prepare(a, l);
    lea_rt(a, R::V7, RT_CTX);
    lea_rt(a, R::V6, RT_INTO);
    a.call_label(l.alloc_into);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm8(R::V0, ALLOC_REFILL as i8);
    a.jcc_label(C::Equal, refill);
    a.bind_text_label(not_refill);
    a.cmp_reg_imm8(R::V0, ALLOC_METADATA_REFILL as i8);
    a.jcc_label(C::Equal, l.metadata_oom);
    a.cmp_reg_imm8(R::V0, ALLOC_ROOT_FLUSH as i8);
    a.jcc_label(C::NotEqual, l.fail);
    a.call_label(l.drain);
    a.jmp_label(attempt);
    // Refill: close the current page (if any) and bind the next prepared one.
    a.bind_text_label(refill);
    load_rt(a, R::V6, RT_REQUEST + offset_of!(AllocationRequest, page));
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, bind);
    lea_rt(a, R::V7, RT_CTX);
    a.mov_imm64(R::V2, 1);
    a.call_label(l.tlab_page);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_REQUEST + offset_of!(AllocationRequest, page), R::V0);
    a.bind_text_label(bind);
    let have_page = a.create_text_label();
    if l.worker.is_some() {
        // The worker takes pages from the top: claim next_page by a CAS on
        // the packed bounds (next_page | copy_low << 32).
        let retry = a.create_text_label();
        let full = a.create_text_label();
        a.bind_text_label(retry);
        load_rt(a, R::V0, RT_BOUNDS);
        a.mov_reg_reg(R::V1, R::V0);
        a.shl_reg_imm8(R::V1, 32);
        a.shr_reg_imm8(R::V1, 32);
        a.mov_reg_reg(R::V2, R::V0);
        a.shr_reg_imm8(R::V2, 32);
        a.cmp_reg_reg(R::V1, R::V2);
        a.jcc_label(C::AboveOrEqual, full);
        a.mov_reg_reg(R::V8, R::V0);
        a.mov_reg_reg(R::V6, R::V0);
        a.add_reg_imm32(R::V6, 1);
        lea_rt(a, R::V7, RT_BOUNDS);
        a.atomic_compare_exchange(R::V7, R::V6);
        a.cmp_reg_reg(R::V0, R::V8);
        a.jcc_label(C::NotEqual, retry);
        a.mov_reg_reg(R::V0, R::V1);
        a.jmp_label(have_page);
        a.bind_text_label(full);
    } else {
        load_rt(a, R::V0, RT_NEXT_PAGE);
        load_rt(a, R::V1, RT_SPACE_END);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::Below, have_page);
    }
    // Copy exhaustion is an actual destination failure. A fresh owner instead
    // reaches a safepoint: another owner may already be requesting a new bank.
    load_rt(a, R::V0, RT_COLLECTING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.oom);
    a.call_label(l.collect);
    #[cfg(test)]
    if let Some(hook) = l.alloc_after_collect_hook {
        a.call_label(hook);
    }
    let legacy_capacity = a.create_text_label();
    load_rt(a, R::V0, super::moving_round::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, legacy_capacity);
    // Moving-start can return a stale-request receipt, and peers can consume
    // the returned window before this owner runs again. A full cursor here is
    // not a live-size certificate. Service the real pending round and retry
    // admission/the leaf against this owner's current TLAB and actual bank.
    a.call_label(l.owner_checkpoint_poll);
    a.jmp_label(attempt);
    a.bind_text_label(legacy_capacity);
    // Evacuation may have bound a to-space page into the TLAB already, so
    // retry the allocation instead of binding another page here.
    load_rt(a, R::V0, RT_NEXT_PAGE);
    load_rt(a, R::V1, RT_SPACE_END);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::AboveOrEqual, l.oom);
    a.jmp_label(attempt);
    a.bind_text_label(have_page);
    bind_page(a, l);
    a.jmp_label(attempt);
    // Success: remember the object for publication and answer its address.
    a.bind_text_label(done);
    #[cfg(test)]
    if let Some(hook) = l.alloc_before_black_hook {
        a.call_label(hook);
    }
    load_rt(
        a,
        R::V0,
        RT_RESULT + offset_of!(AllocationResult, reference),
    );
    store_rt(a, RT_PENDING_REF, R::V0);
    load_rt(a, R::V1, RT_REQUEST + offset_of!(AllocationRequest, page));
    store_rt(a, RT_PENDING_PAGE, R::V1);
    // The object's forward cell is the table's last used cell.
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(AllocationPage, forward_table)));
    a.load_ptr_disp32(R::V2, R::V1, d(offset_of!(ForwardTable, used)));
    a.sub_reg_imm8(R::V2, 1);
    a.shl_reg_imm8(R::V2, 5);
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(ForwardTable, cells)));
    a.add_reg_reg(R::V1, R::V2);
    store_rt(a, RT_PENDING_CELL, R::V1);
    mark::alloc_black(a, l);
    decode(a, R::V0, R::V1);
    a.ret();
}

/// Bind the already-claimed page index in V0 to this owner.
fn bind_page<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.mov_reg_reg(R::V1, R::V0);
    a.add_reg_imm32(R::V1, 1);
    a.shl_reg_imm8(R::V1, 17);
    super::generation::test(a, R::V1, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, l.fail);
    super::commit::ensure(a, l, R::V1);
    // page = pages + index * 112 (= index*128 - index*16).
    a.mov_reg_reg(R::V6, R::V0);
    a.shl_reg_imm8(R::V6, 7);
    a.mov_reg_reg(R::V1, R::V0);
    a.shl_reg_imm8(R::V1, 4);
    a.sub_reg_reg(R::V6, R::V1);
    load_rt(a, R::V1, RT_PAGES);
    a.add_reg_reg(R::V6, R::V1);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, RT_BIND_PAGE, R::V6);
    publish_next_page(a);
    load_rt(a, R::V6, RT_BIND_PAGE);
    lea_rt(a, R::V7, RT_CTX);
    a.mov_imm64(R::V2, 0);
    a.call_label(l.tlab_page);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, RT_BIND_PAGE);
    store_rt(a, RT_REQUEST + offset_of!(AllocationRequest, page), R::V0);
}

/// Publish V0 as a claimed page end. Clobbers V0/V6..V9; the mirror is
/// monotone even when independently claimed pages finish binding out of order.
pub(super) fn publish_next_page<E: PortableAsm>(a: &mut E) {
    // Independent winning bounds claims can arrive here out of order. The
    // separately reported high-water mirror must never move backwards.
    let retry = a.create_text_label();
    let published = a.create_text_label();
    a.mov_reg_reg(R::V9, R::V0);
    lea_rt(a, R::V7, RT_NEXT_PAGE);
    a.bind_text_label(retry);
    a.atomic_load_acquire(R::V0, R::V7);
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::AboveOrEqual, published);
    a.mov_reg_reg(R::V8, R::V0);
    a.mov_reg_reg(R::V6, R::V9);
    a.atomic_compare_exchange(R::V7, R::V6);
    a.cmp_reg_reg(R::V0, R::V8);
    a.jcc_label(C::NotEqual, retry);
    a.bind_text_label(published);
}

/// The new space is empty and the worker is stopped. Reserve one current
/// TLAB before opening relocation, so its first fresh allocation can return
/// without waiting for all deferred roots. Later fresh refills still stall.
pub(super) fn reserve_initial_page<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, RT_SPACE_BASE);
    if l.worker.is_some() {
        load_rt(a, R::V1, RT_BOUNDS);
        a.add_reg_imm32(R::V1, 1);
        store_rt(a, RT_BOUNDS, R::V1);
    }
    bind_page(a, l);
}

fn restore_fresh_request<E: PortableAsm>(a: &mut E) {
    for (source, target) in [(RT_FRESH_SIZE, RT_ALLOC_SIZE), (RT_FRESH_TAG, RT_ALLOC_TAG)] {
        load_rt(a, R::V0, source);
        store_rt(a, target, R::V0);
    }
}

fn source_access<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    load_rt(a, R::V7, RT_PENDING_CELL);
    lea_rt(a, R::V6, RT_ACCESS);
    a.mov_imm64(R::V2, op);
    a.call_label(l.source_access);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}

/// ENTER the pending object's source guard, publish readiness, EXIT.
pub(super) fn emit_publish_pending<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    a.bind_text_label(l.publish_pending);
    load_rt(a, R::V0, RT_PENDING_REF);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    let young = a.create_text_label();
    let published = a.create_text_label();
    load_rt(a, R::V1, RT_PENDING_PAGE);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, young);
    super::large_object::publish(a);
    a.jmp_label(published);
    a.bind_text_label(young);
    let access = |field: usize| RT_ACCESS + field;
    let publish = |field: usize| RT_PUBLISH + field;
    let lease = |field: usize| RT_PUBLISH + offset_of!(GuardedMarkPrepare, lease) + field;
    lea_rt(a, R::V1, RT_CTX);
    store_rt(a, access(offset_of!(SourceAccessRequest, context)), R::V1);
    // Readiness publication must authenticate the same adopted snapshot as
    // the source guard; leaving this at startup's zero breaks later epochs.
    a.add_reg_imm32(R::V1, d(offset_of!(MutatorContext, adopted_phase)));
    a.atomic_load_acquire(R::V2, R::V1);
    store_rt(a, publish(offset_of!(GuardedMarkPrepare, phase)), R::V2);
    store_rt(a, publish(offset_of!(GuardedMarkPrepare, reference)), R::V0);
    a.shr_reg_imm8(R::V0, 16);
    store_rt(
        a,
        access(offset_of!(SourceAccessRequest, source_offset)),
        R::V0,
    );
    load_rt(a, R::V1, RT_PENDING_PAGE);
    store_rt(a, publish(offset_of!(GuardedMarkPrepare, page)), R::V1);
    store_rt(a, lease(offset_of!(MetadataLeaseRequest, page)), R::V1);
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(AllocationPage, incarnation)));
    store_rt(
        a,
        access(offset_of!(SourceAccessRequest, incarnation)),
        R::V1,
    );
    store_rt(
        a,
        lease(offset_of!(MetadataLeaseRequest, incarnation)),
        R::V1,
    );
    a.mov_imm64(R::V1, PUBLISH_LEASE_TOKEN);
    store_rt(a, lease(offset_of!(MetadataLeaseRequest, token)), R::V1);
    lea_rt(a, R::V1, RT_ACCESS);
    store_rt(a, publish(offset_of!(GuardedMarkPrepare, scope)), R::V1);
    source_access(a, l, SOURCE_ENTER);
    lea_rt(a, R::V7, RT_PUBLISH);
    a.call_label(l.publish);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    source_access(a, l, SOURCE_EXIT);
    a.bind_text_label(published);
    a.mov_imm64(R::V0, 0);
    store_rt(a, RT_PENDING_REF, R::V0);
    counters::increment(a, RT_PUBLISHED);
    a.bind_text_label(done);
    a.ret();
}

#[path = "alloc_empty.rs"]
mod empty;
pub(super) fn release_empty_tail<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    empty::release(a, l);
}

/// Close this owner's actual TLAB before publishing quiescence/native state.
/// Caller holds its collector ticket and has completed pending initialization.
pub(super) fn close_owner_tlab<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    load_rt(a, R::V6, RT_REQUEST + offset_of!(AllocationRequest, page));
    a.test_reg_reg(R::V6, R::V6);
    a.jcc_label(C::Equal, done);
    lea_rt(a, R::V7, RT_CTX);
    a.mov_imm64(R::V2, 1);
    a.call_label(l.tlab_page);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt_imm(a, RT_REQUEST + offset_of!(AllocationRequest, page), 0);
    a.bind_text_label(done);
}
