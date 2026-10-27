//! Bounded previous-referent transport. A captured generation/epoch obligation
//! remains strong until a real marker queue owns it; metadata leases alone do
//! not supply that reachability. Entries seal before any physical bank move.
use super::*;
use crate::gc_abi::{BufferedSlot, GcRef, MetadataLeaseRequest};

pub(super) const CAPACITY: usize = 16;
#[repr(C)]
struct Entry {
    slot: BufferedSlot,
    previous: GcRef,
    previous_page: u64,
    previous_incarnation: u64,
    previous_token: u64,
    generation: u64,
    owner_page: u64,
    reserved: u64,
}
#[repr(C)]
pub(super) struct Owner {
    count: u64,
    entries: [Entry; CAPACITY],
    owner_request: MetadataLeaseRequest,
    previous_request: MetadataLeaseRequest,
    previous_offset: u64,
    field: u64,
}
#[repr(C)]
pub(super) struct State {
    pending: [u64; 2], // Young, Old; a Young reset never discards Old work.
    admitted: u64,
    flushed: u64,
    synchronous: u64,
}
const BASE: usize = offset_of!(Runtime, satb_buffer);
pub(super) const ADMITTED: usize = BASE + offset_of!(State, admitted);
pub(super) const FLUSHED: usize = BASE + offset_of!(State, flushed);
pub(super) const SYNCHRONOUS: usize = BASE + offset_of!(State, synchronous);
pub(super) const fn pending_count(young: bool) -> usize {
    BASE + offset_of!(State, pending) + (!young as usize) * 8
}
const OWN: usize = owner_state::offset(offset_of!(OwnerState, satb_buffer));
pub(super) const COUNT: usize = OWN + offset_of!(Owner, count);
const ENTRIES: usize = OWN + offset_of!(Owner, entries);
const OWNER_REQUEST: usize = OWN + offset_of!(Owner, owner_request);
const PREVIOUS_REQUEST: usize = OWN + offset_of!(Owner, previous_request);
const PREVIOUS: usize = OWN + offset_of!(Owner, previous_offset);
const FIELD: usize = OWN + offset_of!(Owner, field);
const _: () = assert!(size_of::<Entry>() == 96);
const EPOCH: i32 = offset_of!(BufferedSlot, record_epoch) as i32;
const OWNER_TOKEN: i32 = offset_of!(BufferedSlot, metadata_lease) as i32;
const GENERATION_ENTRY: i32 = offset_of!(Entry, generation) as i32;
const fn epoch(young: bool) -> usize {
    if young {
        young_mark::EPOCH
    } else {
        RT_MARK_CYCLES
    }
}
const fn active(young: bool) -> usize {
    if young {
        young_mark::ACTIVE
    } else {
        RT_MARK_ACTIVE
    }
}
fn entry<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V10, ENTRIES);
    a.mov_reg_reg(R::V11, R::V9);
    a.shl_reg_imm8(R::V11, 6);
    a.add_reg_reg(R::V10, R::V11);
    a.mov_reg_reg(R::V11, R::V9);
    a.shl_reg_imm8(R::V11, 5);
    a.add_reg_reg(R::V10, R::V11);
}
fn metadata<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, request: usize, op: u64) {
    lea_rt(a, R::V7, metadata_leases::POOL);
    lea_rt(a, R::V6, request);
    a.mov_imm64(R::V2, op);
    a.call_label(l.metadata_lease_step);
}
fn check<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}
fn increment<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, field: usize) {
    load_rt(a, R::V0, field);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, field, R::V0);
}
pub(super) fn pending<E: PortableAsm>(a: &mut E, young: bool, target: E::TextLabel) {
    load_rt(a, R::V0, pending_count(young));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, target);
}
pub(super) fn require_empty<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    pending(a, true, l.fail);
    pending(a, false, l.fail);
}
pub(super) fn flush<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, locked: bool) {
    a.call_label(if locked {
        l.satb_flush_locked
    } else {
        l.satb_flush
    });
}
/// V2 = previous payload offset, V7 = current slot. Preserve all registers.
pub(super) fn offer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, young: bool) {
    a.call_label(if young {
        l.satb_record_young
    } else {
        l.satb_record_old
    });
}
fn restore_request<E: PortableAsm>(a: &mut E, owner: bool) {
    load_rt(a, R::V9, COUNT);
    a.sub_reg_imm8(R::V9, 1);
    entry(a);
    let (request, fields) = if owner {
        (
            OWNER_REQUEST,
            [
                offset_of!(Entry, owner_page),
                offset_of!(BufferedSlot, page_incarnation),
                offset_of!(BufferedSlot, metadata_lease),
            ],
        )
    } else {
        (
            PREVIOUS_REQUEST,
            [
                offset_of!(Entry, previous_page),
                offset_of!(Entry, previous_incarnation),
                offset_of!(Entry, previous_token),
            ],
        )
    };
    for (i, src) in fields.into_iter().enumerate() {
        a.load_ptr_disp32(R::V0, R::V10, src as i32);
        store_rt(a, request + i * 8, R::V0);
    }
}
fn transfer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, young: bool) {
    load_rt(a, R::V0, active(young));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, epoch(young));
    a.load_ptr_disp32(R::V0, R::V10, EPOCH);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V2, R::V10, offset_of!(Entry, previous) as i32);
    a.shr_reg_imm8(R::V2, 16);
    let w = l.worker.as_ref().unwrap();
    a.call_label(if young { w.young.reference } else { w.mark_ref });
    // Both the in-array and spill paths now own traversal before private
    // protection is released. Already-marked means an earlier admission owns it.
    for owner in [false, true] {
        restore_request(a, owner);
        metadata(
            a,
            l,
            if owner {
                OWNER_REQUEST
            } else {
                PREVIOUS_REQUEST
            },
            1,
        );
        check(a, l);
    }
    load_rt(a, R::V0, pending_count(young));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, pending_count(young), R::V0);
}

pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if l.worker.is_none() {
        for label in [
            l.satb_record_old,
            l.satb_record_young,
            l.satb_flush,
            l.satb_flush_locked,
        ] {
            a.bind_text_label(label);
            a.ret();
        }
        return;
    }
    a.bind_text_label(l.satb_flush);
    quiescence::save(a);
    let empty = a.create_text_label();
    load_rt(a, R::V0, COUNT);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    worker::lock(a, l);
    a.call_label(l.satb_flush_locked);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.bind_text_label(empty);
    quiescence::restore(a);
    a.ret();

    a.bind_text_label(l.satb_flush_locked);
    quiescence::save(a);
    let again = a.create_text_label();
    let done = a.create_text_label();
    let young = a.create_text_label();
    let transferred = a.create_text_label();
    a.bind_text_label(again);
    load_rt(a, R::V9, COUNT);
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm32(R::V9, CAPACITY as i32);
    a.jcc_label(C::Above, l.fail);
    for owner in [true, false] {
        restore_request(a, owner);
        metadata(
            a,
            l,
            if owner {
                OWNER_REQUEST
            } else {
                PREVIOUS_REQUEST
            },
            2,
        );
        check(a, l);
    }
    load_rt(a, R::V9, COUNT);
    a.sub_reg_imm8(R::V9, 1);
    entry(a);
    a.load_ptr_disp32(R::V0, R::V10, GENERATION_ENTRY);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, young);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    transfer(a, l, false);
    a.jmp_label(transferred);
    a.bind_text_label(young);
    transfer(a, l, true);
    a.bind_text_label(transferred);
    load_rt(a, R::V9, COUNT);
    a.sub_reg_imm8(R::V9, 1);
    entry(a);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V10, OWNER_TOKEN, R::V0);
    a.store_ptr_disp32(R::V10, offset_of!(Entry, previous_token) as i32, R::V0);
    store_rt(a, COUNT, R::V9);
    copy::bump(a, FLUSHED, R::V0);
    worker::request_work(a);
    a.jmp_label(again);
    a.bind_text_label(done);
    quiescence::restore(a);
    a.ret();
    for young in [false, true] {
        emit_offer(a, l, young);
    }
}

/// An acquired matching stamp already owns traversal. This owner cannot adopt
/// a later epoch until it reaches its own rendezvous, so this hint is safe on
/// the ticket-free path; admissions recheck it after taking the ticket.
fn already_protected<E: PortableAsm>(a: &mut E, young: bool, done: E::TextLabel) {
    load_rt(a, R::V0, active(young));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    if young {
        generation::test(a, R::V2, generation::YOUNG);
        a.jcc_label(C::NotEqual, done);
        young_mark::test(a, R::V2, R::V6, R::V7);
    } else {
        mark::test_mark(a, R::V2, R::V6, R::V7);
    }
    a.jcc_label(C::NotEqual, done);
}

fn emit_offer<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, young: bool) {
    a.bind_text_label(if young {
        l.satb_record_young
    } else {
        l.satb_record_old
    });
    quiescence::save(a);
    store_rt(a, PREVIOUS, R::V2);
    store_rt(a, FIELD, R::V7);
    let done = a.create_text_label();
    let protected = a.create_text_label();
    let fallback = a.create_text_label();
    let release_owner = a.create_text_label();
    let room = a.create_text_label();
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.atomic_load_acquire_u32(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    // Keep the existing protected-store contract: no ticket, work request or
    // completion-hint reset when the current epoch already owns the value.
    already_protected(a, young, protected);
    worker::lock(a, l);
    already_protected(a, young, done);
    load_rt(a, R::V0, cooperative_old::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, fallback);
    // Source retirement forbids acquiring fresh captured-source obligations.
    // Preserve the existing immediate barrier while any physical move is open.
    for f in [RT_PHASE_OPEN, RT_RETIRE_STAGE] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, fallback);
    }
    load_rt(a, R::V0, COUNT);
    a.cmp_reg_imm32(R::V0, CAPACITY as i32);
    a.jcc_label(C::Below, room);
    a.call_label(l.satb_flush_locked);
    a.bind_text_label(room);
    load_rt(a, R::V7, FIELD);
    a.call_label(l.satb_slot_owner);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, fallback);
    a.mov_reg_reg(R::V2, R::V0);
    load_rt(a, R::V9, COUNT);
    entry(a);
    a.mov_reg_reg(R::V11, R::V7);
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V11, R::V1);
    a.sub_reg_reg(R::V11, R::V2);
    a.store_ptr_disp32(R::V10, offset_of!(BufferedSlot, byte_offset) as i32, R::V11);
    a.mov_reg_reg(R::V0, R::V2);
    a.shl_reg_imm8(R::V0, 16);
    colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    a.store_ptr_disp32(R::V10, 0, R::V0);
    load_rt(a, R::V0, epoch(young));
    a.store_ptr_disp32(R::V10, EPOCH, R::V0);
    a.mov_imm64(R::V0, !young as u64);
    a.store_ptr_disp32(R::V10, GENERATION_ENTRY, R::V0);
    mark_queue::page_for_offset(a);
    a.store_ptr_disp32(R::V10, offset_of!(Entry, owner_page) as i32, R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(
        R::V10,
        offset_of!(BufferedSlot, page_incarnation) as i32,
        R::V0,
    );
    store_rt(a, OWNER_REQUEST, R::V8);
    store_rt(a, OWNER_REQUEST + 8, R::V0);
    store_rt_imm(a, OWNER_REQUEST + 16, 0);
    metadata(a, l, OWNER_REQUEST, 0);
    lease_admitted(a, l, fallback);
    load_rt(a, R::V9, COUNT);
    entry(a);
    load_rt(a, R::V0, OWNER_REQUEST + 16);
    a.store_ptr_disp32(R::V10, OWNER_TOKEN, R::V0);
    load_rt(a, R::V2, PREVIOUS);
    a.mov_reg_reg(R::V0, R::V2);
    a.shl_reg_imm8(R::V0, 16);
    colours::load_current(a, R::V1);
    a.or_reg_reg(R::V0, R::V1);
    a.store_ptr_disp32(R::V10, offset_of!(Entry, previous) as i32, R::V0);
    mark_queue::page_for_offset(a);
    a.store_ptr_disp32(R::V10, offset_of!(Entry, previous_page) as i32, R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(
        R::V10,
        offset_of!(Entry, previous_incarnation) as i32,
        R::V0,
    );
    store_rt(a, PREVIOUS_REQUEST, R::V8);
    store_rt(a, PREVIOUS_REQUEST + 8, R::V0);
    store_rt_imm(a, PREVIOUS_REQUEST + 16, 0);
    metadata(a, l, PREVIOUS_REQUEST, 0);
    lease_admitted(a, l, release_owner);
    load_rt(a, R::V9, COUNT);
    entry(a);
    load_rt(a, R::V0, PREVIOUS_REQUEST + 16);
    a.store_ptr_disp32(R::V10, offset_of!(Entry, previous_token) as i32, R::V0);
    store_rt_imm(a, OWNER_REQUEST + 16, 0);
    store_rt_imm(a, PREVIOUS_REQUEST + 16, 0);
    increment(a, l, COUNT);
    increment(a, l, pending_count(young));
    copy::bump(a, ADMITTED, R::V0);
    store_rt_imm(
        a,
        if young {
            young_mark::DONE
        } else {
            RT_MARK_DONE
        },
        0,
    );
    worker::request_work(a);
    a.jmp_label(done);
    a.bind_text_label(release_owner);
    metadata(a, l, OWNER_REQUEST, 1);
    check(a, l);
    store_rt_imm(a, OWNER_REQUEST + 16, 0);
    a.bind_text_label(fallback);
    load_rt(a, R::V2, PREVIOUS);
    let w = l.worker.as_ref().unwrap();
    a.call_label(if young { w.young.reference } else { w.mark_ref });
    copy::bump(a, SYNCHRONOUS, R::V0);
    store_rt_imm(
        a,
        if young {
            young_mark::DONE
        } else {
            RT_MARK_DONE
        },
        0,
    );
    worker::request_work(a);
    a.bind_text_label(done);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.bind_text_label(protected);
    quiescence::restore(a);
    a.ret();
}
fn lease_admitted<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, fallback: E::TextLabel) {
    let acquired = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, acquired);
    for status in [2, 3] {
        a.cmp_reg_imm32(R::V0, status);
        a.jcc_label(C::Equal, fallback);
    }
    a.jmp_label(l.fail);
    a.bind_text_label(acquired);
}
