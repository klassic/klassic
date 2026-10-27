//! Retained source lease -> same object's canonical strong-root destination.
//! The new lease is funded before the old one is released. Both ownership
//! changes and the published ReferenceLease occur under one collector ticket.
use super::super::*;
#[repr(C)]
pub(in super::super) struct Owner {
    caller: u64,
    canonical: u64,
    held: MetadataLeaseRequest,
    candidate: MetadataLeaseRequest,
    reference: u64,
    root_candidate: crate::gc_abi::ReferenceLease,
}
const BASE: usize = owner_state::offset(offset_of!(OwnerState, lease_transfer));
const CALLER: usize = BASE + offset_of!(Owner, caller);
const CANONICAL: usize = BASE + offset_of!(Owner, canonical);
const HELD: usize = BASE + offset_of!(Owner, held);
const CANDIDATE: usize = BASE + offset_of!(Owner, candidate);
const REFERENCE: usize = BASE + offset_of!(Owner, reference);

/// V0=registered RootCell, V7=owned ReferenceLease. Shared metadata status;
/// invalid source/reference/identity leaves the caller's ownership unchanged.
/// Root resolution precedes the ticket; no safepoint occurs after its raw
/// result is retained. A destination in Retire must be outside actual sources.
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.metadata_lease_retarget_actor);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    load_rt(a, R::V1, CALLER);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, CALLER, R::V7);
    a.call_label(l.root_load_actor);
    store_rt(a, CANONICAL, R::V0);
    worker::lock(a, l);
    let invalid = a.create_text_label();
    let finish = a.create_text_label();
    let safe = a.create_text_label();
    let same = a.create_text_label();
    let commit = a.create_text_label();
    let success = a.create_text_label();
    load_rt(a, R::V8, CALLER);
    for off in [0, 8, 16] {
        a.load_ptr_disp32(R::V1, R::V8, off + 8);
        store_rt(a, HELD + off as usize, R::V1);
    }
    lea_rt(a, R::V7, super::POOL);
    lea_rt(a, R::V6, HELD);
    a.mov_imm64(R::V2, 2);
    a.call_label(l.metadata_lease_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, invalid);
    load_rt(a, R::V8, CALLER);
    for off in [0, 8, 16] {
        a.load_ptr_disp32(R::V1, R::V8, off + 8);
        load_rt(a, R::V2, HELD + off as usize);
        a.cmp_reg_reg(R::V1, R::V2);
        a.jcc_label(C::NotEqual, invalid);
    }
    a.load_ptr_disp32(R::V0, R::V8, 0);
    fields::offset(a, R::V0, R::V2, invalid);
    load_rt(a, R::V8, HELD);
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(AllocationPage, start)));
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, invalid);
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(AllocationPage, limit)));
    a.cmp_reg_reg(R::V2, R::V1);
    a.jcc_label(C::AboveOrEqual, invalid);
    // The shared physical bitmap validates payload starts, including an Old
    // header whose payload starts exactly on the following page boundary.
    a.mov_reg_reg(R::V8, R::V2);
    a.sub_reg_imm32(R::V8, HEAP_PAGE_BYTES as i32);
    a.shr_reg_imm8(R::V8, 10);
    a.shl_reg_imm8(R::V8, 3);
    a.add_reg_reg(R::V8, R::V3);
    a.add_reg_imm32(R::V8, layout(a).starts as i32);
    a.mov_reg_reg(R::V1, R::V2);
    a.shr_reg_imm8(R::V1, 4);
    a.and_reg_imm32(R::V1, 63);
    a.mov_imm64(R::V7, 1);
    a.shl_reg_count(R::V7);
    a.atomic_load_acquire(R::V8, R::V8);
    a.and_reg_reg(R::V8, R::V7);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    load_rt(a, R::V8, RT_HEAP_BASE);
    a.add_reg_reg(R::V8, R::V2);
    a.load_ptr_disp32(R::V8, R::V8, copy::MARK_RECORD);
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V8, R::V8, d(offset_of!(MarkRecord, object_id)));
    load_rt(a, R::V0, CANONICAL);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.mov_reg_reg(R::V2, R::V0);
    a.sub_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_imm32(R::V2, geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V0, copy::MARK_RECORD);
    a.load_ptr_disp32(R::V1, R::V1, d(offset_of!(MarkRecord, object_id)));
    a.cmp_reg_reg(R::V8, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    encode(a, R::V0, R::V1);
    store_rt(a, REFERENCE, R::V0);
    a.shr_reg_imm8(R::V0, 16);
    a.shr_reg_imm8(R::V0, 17);
    a.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, run_head)));
    scan::page_ptr(a, R::V1, R::V0, R::V2);
    store_rt(a, CANDIDATE, R::V1);
    a.load_ptr_disp32(R::V2, R::V1, d(offset_of!(AllocationPage, incarnation)));
    store_rt(a, CANDIDATE + 8, R::V2);
    a.mov_reg_reg(R::V8, R::V0);
    load_rt(a, R::V0, RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, safe);
    super::source(a, invalid, safe);
    a.bind_text_label(safe);
    load_rt(a, R::V0, CANDIDATE);
    load_rt(a, R::V1, HELD);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, same);
    lea_rt(a, R::V7, super::POOL);
    lea_rt(a, R::V6, CANDIDATE);
    a.mov_imm64(R::V2, 0);
    a.call_label(l.metadata_lease_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, finish);
    a.jmp_label(commit);
    a.bind_text_label(same);
    load_rt(a, R::V8, CALLER);
    load_rt(a, R::V0, REFERENCE);
    a.store_ptr_disp32(R::V8, 0, R::V0);
    a.jmp_label(success);
    a.bind_text_label(commit);
    load_rt(a, R::V8, CALLER);
    for off in [0, 8, 16] {
        load_rt(a, R::V0, CANDIDATE + off as usize);
        a.store_ptr_disp32(R::V8, off + 8, R::V0);
    }
    load_rt(a, R::V0, REFERENCE);
    a.store_ptr_disp32(R::V8, 0, R::V0);
    lea_rt(a, R::V7, super::POOL);
    lea_rt(a, R::V6, HELD);
    a.mov_imm64(R::V2, 1);
    a.call_label(l.metadata_lease_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(success);
    worker::request_work(a);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(finish);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(finish);
    a.mov_imm64(R::V1, 0);
    for off in (0..size_of::<Owner>()).step_by(8) {
        store_rt(a, BASE + off, R::V1);
    }
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.call_label(l.lease_grow_service);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}

/// Actual strong root -> shared source-validation and metadata lease leaf.
/// The caller owns a zeroed MetadataLeaseRequest; no raw value escapes.
pub(super) fn emit_acquire<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    const ROOT: usize = BASE + offset_of!(Owner, root_candidate);
    a.bind_text_label(l.metadata_lease_root_actor);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    load_rt(a, R::V1, CALLER);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, CALLER, R::V7);
    a.call_label(l.root_load_actor);
    encode(a, R::V0, R::V1);
    store_rt(a, ROOT, R::V0);
    worker::lock(a, l);
    let invalid = a.create_text_label();
    let retry = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V8, CALLER);
    for off in [0, 8, 16] {
        a.load_ptr_disp32(R::V1, R::V8, off);
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, invalid);
    }
    load_rt(a, R::V1, RT_RETIRE_STAGE);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, retry);
    lea_rt(a, R::V7, super::POOL);
    lea_rt(a, R::V6, ROOT);
    a.call_label(l.metadata_reference_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, done);
    load_rt(a, R::V8, CALLER);
    for off in [0, 8, 16] {
        load_rt(a, R::V1, ROOT + 8 + off as usize);
        a.store_ptr_disp32(R::V8, off, R::V1);
    }
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(retry);
    a.mov_imm64(R::V0, 4);
    a.bind_text_label(done);
    a.mov_imm64(R::V1, 0);
    for off in (0..size_of::<Owner>()).step_by(8) {
        store_rt(a, BASE + off, R::V1);
    }
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.call_label(l.lease_grow_service);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}
