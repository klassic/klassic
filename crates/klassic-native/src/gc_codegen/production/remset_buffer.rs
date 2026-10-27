//! Bounded remembered-slot transport owned by each enrolled mutator. Entries
//! capture values, never RootHandles. Real pooled leases protect metadata only;
//! each handoff seals before phase ACK/Native, so no entry crosses a bank move.
//! Admission and sealing share the collector ticket. Only Running owners may
//! append: an acknowledged owner stays parked until phase adoption/resume.
//! A moving coordinator requires global emptiness before mutating source banks.
use super::*;
use crate::gc_abi::{BufferedSlot, MetadataLeaseRequest};
pub(super) const CAPACITY: usize = 16;
#[repr(C)]
struct Entry {
    slot: BufferedSlot,
    page: u64,
}
#[repr(C)]
pub(super) struct Owner {
    count: u64,
    entries: [Entry; CAPACITY],
    request: MetadataLeaseRequest,
    field: u64,
    admitted: u64,
    flushed: u64,
    synchronous: u64,
}
#[repr(C)]
pub(super) struct State {
    pending: u64,
    admitted: u64,
    flushed: u64,
    synchronous: u64,
}
const BASE: usize = offset_of!(Runtime, remset_buffer);
pub(super) const PENDING: usize = BASE + offset_of!(State, pending);
pub(super) const ADMITTED: usize = BASE + offset_of!(State, admitted);
pub(super) const FLUSHED: usize = BASE + offset_of!(State, flushed);
pub(super) const SYNCHRONOUS: usize = BASE + offset_of!(State, synchronous);
const OWN: usize = owner_state::offset(offset_of!(OwnerState, remset_buffer));
pub(super) const COUNT: usize = OWN + offset_of!(Owner, count);
const ENTRIES: usize = OWN + offset_of!(Owner, entries);
const REQUEST: usize = OWN + offset_of!(Owner, request);
const FIELD: usize = OWN + offset_of!(Owner, field);
const PAGE: i32 = offset_of!(Entry, page) as i32;
const INCARNATION: i32 = offset_of!(BufferedSlot, page_incarnation) as i32;
const EPOCH: i32 = offset_of!(BufferedSlot, record_epoch) as i32;
const TOKEN: i32 = offset_of!(BufferedSlot, metadata_lease) as i32;
const BYTE_OFFSET: i32 = offset_of!(BufferedSlot, byte_offset) as i32;
const _: () = assert!(size_of::<Entry>() == 48);

/// R9=index -> R10=entry, R11 scratch. No raw payload cursor is retained.
fn entry<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V10, ENTRIES);
    a.mov_reg_reg(R::V11, R::V9);
    a.shl_reg_imm8(R::V11, 5);
    a.add_reg_reg(R::V10, R::V11);
    a.mov_reg_reg(R::V11, R::V9);
    a.shl_reg_imm8(R::V11, 4);
    a.add_reg_reg(R::V10, R::V11);
}
fn metadata<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, op: u64) {
    lea_rt(a, R::V7, metadata_leases::POOL);
    lea_rt(a, R::V6, REQUEST);
    a.mov_imm64(R::V2, op);
    a.call_label(l.metadata_lease_step);
}
fn increment<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, field: usize) {
    load_rt(a, R::V0, field);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, field, R::V0);
}
/// The caller owns the collector ticket, the same exclusion as store admission.
pub(super) fn require_empty<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, PENDING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
pub(super) fn flush<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, locked: bool) {
    a.call_label(if locked {
        l.remset_buffer_flush_locked
    } else {
        l.remset_buffer_flush
    });
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.remset_buffer_flush);
    quiescence::save(a);
    // Empty polls/Native calls need no additional ticket.
    let empty = a.create_text_label();
    load_rt(a, R::V0, COUNT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    worker::lock(a, l);
    a.call_label(l.remset_buffer_flush_locked);
    worker::unlock(a, l);
    a.bind_text_label(empty);
    quiescence::restore(a);
    a.ret();

    a.bind_text_label(l.remset_buffer_flush_locked);
    quiescence::save(a);
    let again = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(again);
    load_rt(a, R::V9, COUNT);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm32(R::V9, CAPACITY as i32);
    a.jcc_label(C::Above, l.fail);
    a.sub_reg_imm8(R::V9, 1);
    entry(a);
    a.load_ptr_disp32(R::V0, R::V10, EPOCH);
    load_rt(a, R::V1, RT_REMSET_FLIPS);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    for (src, dst) in [(PAGE, 0), (INCARNATION, 8), (TOKEN, 16)] {
        a.load_ptr_disp32(R::V0, R::V10, src);
        store_rt(a, REQUEST + dst, R::V0);
    }
    metadata(a, l, 2);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V9, COUNT);
    a.sub_reg_imm8(R::V9, 1);
    entry(a);
    a.load_ptr_disp32(R::V2, R::V10, 0);
    a.shr_reg_imm8(R::V2, 16);
    load_rt(a, R::V7, RT_HEAP_BASE);
    a.add_reg_reg(R::V7, R::V2);
    a.load_ptr_disp32(R::V0, R::V10, BYTE_OFFSET);
    a.add_reg_reg(R::V7, R::V0);
    a.call_label(l.slot_owner);
    a.cmp_reg_reg(R::V0, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    remset::remember(a, l, R::V7, R::V1, R::V2);
    metadata(a, l, 1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V9, COUNT);
    a.sub_reg_imm8(R::V9, 1);
    entry(a);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V10, TOKEN, R::V0);
    store_rt(a, COUNT, R::V9);
    load_rt(a, R::V0, PENDING);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, PENDING, R::V0);
    copy::bump(a, FLUSHED, R::V0);
    copy::bump(a, OWN + offset_of!(Owner, flushed), R::V0);
    a.jmp_label(again);
    a.bind_text_label(done);
    quiescence::restore(a);
    a.ret();

    // V7=current Old slot, every register preserved. SATB/Young protection
    // already completed; this entry records only precise remembered membership.
    a.bind_text_label(l.remset_buffer_record);
    quiescence::save(a);
    worker::lock(a, l);
    // Check even for an already remembered slot: a sealed owner must not
    // publish a logical store between its final flush and phase resumption.
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.atomic_load_acquire_u32(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0); // GC_RUNNING
    a.jcc_label(C::NotEqual, l.fail);
    store_rt(a, FIELD, R::V7);
    let skip = a.create_text_label();
    let fallback = a.create_text_label();
    let room = a.create_text_label();
    remset::registered(a, R::V7, skip);
    load_rt(a, R::V0, COUNT);
    a.cmp_reg_imm32(R::V0, CAPACITY as i32);
    a.jcc_label(C::Below, room);
    a.call_label(l.remset_buffer_flush_locked);
    a.bind_text_label(room);
    a.call_label(l.slot_owner);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, fallback); // A not-yet-published Old initializer.
    a.mov_reg_reg(R::V2, R::V0);
    load_rt(a, R::V9, COUNT);
    entry(a);
    a.mov_reg_reg(R::V11, R::V7);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V11, R::V1);
    a.sub_reg_reg(R::V11, R::V2);
    a.store_ptr_disp32(R::V10, BYTE_OFFSET, R::V11);
    a.mov_reg_reg(R::V0, R::V2);
    a.shl_reg_imm8(R::V0, 16);
    colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    a.store_ptr_disp32(R::V10, 0, R::V0);
    load_rt(a, R::V0, RT_REMSET_FLIPS);
    a.store_ptr_disp32(R::V10, EPOCH, R::V0);
    mark_queue::page_for_offset(a);
    a.store_ptr_disp32(R::V10, PAGE, R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(R::V10, INCARNATION, R::V0);
    store_rt(a, REQUEST + 8, R::V0);
    store_rt(a, REQUEST, R::V8);
    store_rt_imm(a, REQUEST + 16, 0);
    // Source-retirement admission follows the same actual run-head contract
    // as other metadata users; a current canonical Old slot is not a source.
    let admitted = a.create_text_label();
    load_rt(a, R::V0, RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, admitted);
    a.load_ptr_disp32(R::V8, R::V8, d(offset_of!(AllocationPage, run_head)));
    metadata_leases::source(a, l.fail, admitted);
    a.bind_text_label(admitted);
    metadata(a, l, 0);
    let acquired = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, acquired);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::Equal, fallback);
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Equal, fallback);
    a.jmp_label(l.fail);
    a.bind_text_label(acquired);
    load_rt(a, R::V9, COUNT);
    entry(a);
    load_rt(a, R::V0, REQUEST + 16);
    a.store_ptr_disp32(R::V10, TOKEN, R::V0);
    store_rt_imm(a, REQUEST + 16, 0); // The entry now owns this exact token.
    increment(a, l, COUNT);
    increment(a, l, PENDING);
    copy::bump(a, ADMITTED, R::V0);
    copy::bump(a, OWN + offset_of!(Owner, admitted), R::V0);
    a.jmp_label(skip);
    a.bind_text_label(fallback);
    // Restore the exact caller slot, not an uninitialized/failed request.
    load_rt(a, R::V7, FIELD);
    remset::remember(a, l, R::V7, R::V1, R::V2);
    copy::bump(a, SYNCHRONOUS, R::V0);
    copy::bump(a, OWN + offset_of!(Owner, synchronous), R::V0);
    a.bind_text_label(skip);
    worker::unlock(a, l);
    quiescence::restore(a);
    a.ret();
}
