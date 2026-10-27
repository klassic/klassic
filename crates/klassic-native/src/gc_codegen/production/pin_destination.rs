//! Precommitted, unpublished destinations for scoped pin evacuation. A real
//! dedicated-span reservation owns capacity without publishing an INITIALIZING
//! object into a mark frontier. No address/reference escapes through this API.
//! Large sweep skips the uniquely owned reservation, not a fabricated live epoch.
use super::*;
#[repr(C)]
pub(super) struct State {
    active: u64,
    pages: u64,
    cancelled: u64,
}
#[repr(C)]
pub(super) struct Owner {
    caller: u64,
    pages: u64,
    page: u64,
    source_id: u64,
    copied: u64,
    job: u64,
}
const BASE: usize = offset_of!(Runtime, pin_destinations);
pub(super) const ACTIVE: usize = BASE + offset_of!(State, active);
pub(super) const PAGES: usize = BASE + offset_of!(State, pages);
pub(super) const CANCELLED: usize = BASE + offset_of!(State, cancelled);
const OWN: usize = owner_state::offset(offset_of!(OwnerState, pin_destination));
pub(super) const CALLER: usize = OWN + offset_of!(Owner, caller);
pub(super) const PAGE_COUNT: usize = OWN + offset_of!(Owner, pages);
pub(super) const PAGE: usize = OWN + offset_of!(Owner, page);
/// The fourth word of a dedicated descriptor: unique native token address,
/// zero for ordinary published/dead objects. No heap reference is stored here.
pub(super) const SOURCE_ID: usize = OWN + offset_of!(Owner, source_id);
pub(super) const COPIED: usize = OWN + offset_of!(Owner, copied);
pub(super) const JOB: usize = OWN + offset_of!(Owner, job);
pub(super) const RESERVATION: i32 = 24;

pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, entry: E::TextLabel) {
    begin_common(a, l, entry, true);
}
pub(super) fn begin_resolved<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    entry: E::TextLabel,
) {
    begin_common(a, l, entry, false);
}
fn begin_common<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    entry: E::TextLabel,
    poll: bool,
) {
    a.bind_text_label(entry);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    load_rt(a, R::V1, CALLER);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, CALLER, R::V7);
    if poll {
        a.push_reg(R::V0);
        a.call_label(l.publish_pending);
        quiescence::poll(a, l);
        a.pop_reg(R::V0);
    }
}
pub(super) fn finish<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V1, 0);
    for off in (0..size_of::<Owner>()).step_by(8) {
        store_rt(a, OWN + off, R::V1);
    }
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}
pub(super) fn head<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, PAGE);
    a.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
}
fn reserve<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.pin_destination_reserve_actor);
    let invalid = a.create_text_label();
    let retry = a.create_text_label();
    let unavailable = a.create_text_label();
    let locked_done = a.create_text_label();
    let done = a.create_text_label();
    a.call_label(l.root_load_actor);
    let resolved = a.create_text_label();
    a.jmp_label(resolved);
    begin_resolved(a, l, l.pin_destination_reserve_resolved);
    a.bind_text_label(resolved);
    // Source remains protected in the caller's real root through all OS work.
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.mov_reg_reg(R::V2, R::V0);
    a.sub_reg_reg(R::V2, R::V1);
    a.jcc_label(C::Below, invalid);
    a.cmp_reg_imm32(R::V2, geometry(a).heap_bytes as i32);
    a.jcc_label(C::AboveOrEqual, invalid);
    a.load_ptr_disp32(R::V2, R::V0, copy::PAYLOAD);
    // Whole source footprint including a zero-byte object's payload granule.
    a.add_reg_imm32(R::V2, 15);
    a.jcc_label(C::Below, invalid);
    a.and_reg_imm32(R::V2, -16);
    let sized = a.create_text_label();
    a.test_reg_reg(R::V2, R::V2);
    a.jcc_label(C::NotEqual, sized);
    a.mov_imm64(R::V2, 16);
    a.bind_text_label(sized);
    a.add_reg_imm32(R::V2, copy::HEADER + HEAP_PAGE_BYTES as i32 - 1);
    a.jcc_label(C::Below, invalid);
    a.shr_reg_imm8(R::V2, 17);
    store_rt(a, PAGE_COUNT, R::V2);
    load_rt(a, R::V7, CALLER);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.mov_reg_reg(R::V1, R::V7);
    a.and_reg_imm32(R::V1, 7);
    a.jcc_label(C::NotEqual, invalid);
    for off in [0, 8, 16] {
        a.load_ptr_disp32(R::V1, R::V7, off);
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::NotEqual, invalid);
    }
    worker::lock(a, l);
    // Do not change pool geometry in a moving/source-retirement window.
    for field in [RT_PHASE_OPEN, RT_RETIRE_STAGE] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, retry);
    }
    load_rt(a, R::V7, PAGE_COUNT);
    large_reserve::claim_or(a, l, unavailable);
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, PAGE, R::V0);
    large_object::record(a, R::V0);
    load_rt(a, R::V1, PAGE_COUNT);
    a.store_ptr_disp32(R::V8, 0, R::V1);
    a.mov_imm64(R::V1, 0);
    for off in [8, 16] {
        a.store_ptr_disp32(R::V8, off, R::V1);
    }
    load_rt(a, R::V1, CALLER);
    a.store_ptr_disp32(R::V8, RESERVATION, R::V1);
    head(a);
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, owner)), R::V1);
    a.mov_reg_reg(R::V9, R::V8);
    a.add_reg_imm32(R::V9, d(offset_of!(AllocationPage, state)));
    a.mov_imm64(R::V1, crate::gc_abi::PAGE_CLOSED);
    a.atomic_store_release(R::V9, R::V1);
    load_rt(a, R::V7, CALLER);
    a.store_ptr_disp32(R::V7, 0, R::V3);
    load_rt(a, R::V1, PAGE);
    a.store_ptr_disp32(R::V7, 8, R::V1);
    a.load_ptr_disp32(R::V1, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(R::V7, 16, R::V1);
    copy::bump(a, ACTIVE, R::V0);
    load_rt(a, R::V1, PAGE_COUNT);
    for field in [PAGES, large_object::LIVE_PAGES] {
        load_rt(a, R::V0, field);
        a.add_reg_reg(R::V0, R::V1);
        store_rt(a, field, R::V0);
    }
    worker::unlock(a, l);
    // Capacity/ownership is published before any OS operation, with no raw
    // managed result or source guard retained. Each real page is committed.
    load_rt(a, R::V10, PAGE);
    a.shl_reg_imm8(R::V10, 17);
    load_rt(a, R::V11, PAGE_COUNT);
    a.shl_reg_imm8(R::V11, 17);
    a.add_reg_reg(R::V11, R::V10);
    let page = a.create_text_label();
    a.bind_text_label(page);
    commit::ensure(a, l, R::V10);
    a.add_reg_imm32(R::V10, HEAP_PAGE_BYTES as i32);
    a.cmp_reg_reg(R::V10, R::V11);
    a.jcc_label(C::Below, page);
    load_rt(a, R::V8, PAGE);
    a.shl_reg_imm8(R::V8, 17);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.add_reg_reg(R::V8, R::V1);
    // Fund the actual private identity record now, outside a copy claim.
    // No start bit/top/PUBLISHED header or logical identity is exposed yet.
    identity::prepare_record(a, l);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(retry);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(locked_done);
    a.bind_text_label(unavailable);
    a.mov_imm64(R::V0, 6);
    a.bind_text_label(locked_done);
    worker::unlock(a, l);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    finish(a);
}
/// Collector ticket held. Returns the actual dedicated record in V8,
/// page count in V1 and unique caller token in V7.
pub(super) fn authenticate<E: PortableAsm>(
    a: &mut E,
    _l: &Labels<E::TextLabel>,
    invalid: E::TextLabel,
) {
    load_rt(a, R::V7, CALLER);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, invalid);
    a.load_ptr_disp32(R::V0, R::V7, 0);
    a.cmp_reg_reg(R::V0, R::V3);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V7, 8);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, invalid);
    a.cmp_reg_imm32(R::V0, geometry(a).pages as i32);
    a.jcc_label(C::Above, invalid);
    store_rt(a, PAGE, R::V0);
    head(a);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.load_ptr_disp32(R::V1, R::V7, 16);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, owner)));
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, invalid);
    load_rt(a, R::V0, PAGE);
    large_object::record(a, R::V0);
    a.load_ptr_disp32(R::V0, R::V8, RESERVATION);
    a.cmp_reg_reg(R::V0, R::V7);
    a.jcc_label(C::NotEqual, invalid);
    a.load_ptr_disp32(R::V1, R::V8, 0);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, invalid);
}
fn cancel<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l, l.pin_destination_cancel_actor);
    let invalid = a.create_text_label();
    let pending = a.create_text_label();
    let done = a.create_text_label();
    worker::lock(a, l);
    authenticate(a, l, invalid);
    pin_copy::before_cancel(a, l, pending);
    load_rt(a, R::V0, PAGES);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V0, R::V1);
    store_rt(a, PAGES, R::V0);
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, ACTIVE, R::V0);
    a.mov_imm64(R::V0, 0);
    let keep_live = a.create_text_label();
    load_rt(a, R::V1, COPIED);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::NotEqual, keep_live);
    for off in [8, 16] {
        a.store_ptr_disp32(R::V8, off, R::V0);
    }
    a.bind_text_label(keep_live);
    a.store_ptr_disp32(R::V8, RESERVATION, R::V0);
    for off in [0, 8, 16] {
        a.store_ptr_disp32(R::V7, off, R::V0);
    }
    head(a);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V8, d(offset_of!(AllocationPage, owner)), R::V0);
    // Transfer the dead unpublished span to the existing budgeted Large
    // retirement. Metadata/payload/pool cleanup is not performed in this call.
    copy::bump(a, CANCELLED, R::V0);
    worker::request_work(a);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(pending);
    a.mov_imm64(R::V0, 2);
    a.jmp_label(done);
    a.bind_text_label(invalid);
    a.mov_imm64(R::V0, 3);
    a.bind_text_label(done);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    finish(a);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    reserve(a, l);
    cancel(a, l);
    pin_copy::emit_watch(a, l);
}
