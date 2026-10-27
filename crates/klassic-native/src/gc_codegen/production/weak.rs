//! Production weak ownership. Handles never enter the strong-root inventory.
//! Strong closure closes admission before a budgeted metadata-only weak pass.
use super::*;
use crate::gc_abi::{MetadataLeaseSlot, WeakEntry, WeakLoadGate, WeakTable};
#[path = "weak_api.rs"]
mod api;
#[path = "weak_growth.rs"]
mod growth;
pub(super) use api::Owner;
pub(super) const CAPACITY: u64 = 128;
#[repr(C)]
pub(super) struct State {
    gate: WeakLoadGate,
    table: WeakTable,
    pending: [u64; 2],
    cursor: [u64; 2],
    epoch: [u64; 2],
    moving: u64,
    cleared: u64,
    rebound: u64,
    units: u64,
    growth: growth::State,
}
const BASE: usize = offset_of!(Runtime, weak);
const GATE: usize = BASE + offset_of!(State, gate);
pub(super) const CLOSED: usize = GATE + offset_of!(WeakLoadGate, closed);
const TABLE: usize = BASE + offset_of!(State, table);
pub(super) const SIZE: usize = TABLE + offset_of!(WeakTable, capacity);
const ENTRIES: usize = TABLE + offset_of!(WeakTable, entries);
pub(super) const ACTIVE: usize = TABLE + offset_of!(WeakTable, active);
const HEAD: usize = TABLE + offset_of!(WeakTable, free_head);
const SERIAL: usize = TABLE + offset_of!(WeakTable, last_serial);
const PENDING: usize = BASE + offset_of!(State, pending);
const CURSOR: usize = BASE + offset_of!(State, cursor);
const EPOCH: usize = BASE + offset_of!(State, epoch);
const MOVING: usize = BASE + offset_of!(State, moving);
pub(super) const CLEARED: usize = BASE + offset_of!(State, cleared);
pub(super) const REBOUND: usize = BASE + offset_of!(State, rebound);
pub(super) const UNITS: usize = BASE + offset_of!(State, units);
const STRIDE: i32 = size_of::<WeakEntry>() as i32;
const _: () = assert!(size_of::<WeakEntry>() == 56);

pub(super) fn startup<E: PortableAsm>(a: &mut E) {
    store_rt_addr(
        a,
        GATE + offset_of!(WeakLoadGate, pool),
        metadata_leases::POOL,
    );
    store_rt_addr(
        a,
        GATE + offset_of!(WeakLoadGate, registry),
        registered_roots::REGISTRY,
    );
    store_rt_addr(
        a,
        GATE + offset_of!(WeakLoadGate, handshake),
        participants::HANDSHAKE,
    );
    store_rt_addr(a, TABLE, GATE);
    store_rt_addr(a, TABLE + 8, layout(a).weak_entries as usize);
    store_rt_imm(a, TABLE + 16, CAPACITY);
    store_rt_imm(a, HEAD, 1);
    lea_rt(a, R::V8, layout(a).weak_entries as usize);
    a.mov_imm64(R::V0, 2);
    let again = a.create_text_label();
    a.bind_text_label(again);
    a.store_ptr_disp32(R::V8, 48, R::V0);
    a.add_reg_imm32(R::V8, STRIDE);
    a.add_reg_imm32(R::V0, 1);
    a.cmp_reg_imm32(R::V0, CAPACITY as i32 + 1);
    a.jcc_label(C::Below, again);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V8, 48, R::V0);
}
// V8=index (zero based) -> entry, V1 scratch.
fn entry<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V1, R::V8);
    a.shl_reg_imm8(R::V8, 3);
    a.shl_reg_imm8(R::V1, 6);
    a.sub_reg_reg(R::V1, R::V8);
    load_rt(a, R::V8, ENTRIES);
    a.add_reg_reg(R::V8, R::V1);
}
fn gate<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, PENDING);
    load_rt(a, R::V1, PENDING + 8);
    a.or_reg_reg(R::V0, R::V1);
    store_rt(a, CLOSED, R::V0);
}
/// Ticket held, actual strong queues and counted root obligations are empty.
/// No table traversal is performed in the phase pause.
pub(super) fn seal<E: PortableAsm>(a: &mut E, young: bool, busy: E::TextLabel) {
    let lane = usize::from(!young) * 8;
    let epoch = if young {
        young_mark::EPOCH
    } else {
        RT_MARK_CYCLES
    };
    let started = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, PENDING + lane);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, started);
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    load_rt(a, R::V0, epoch);
    store_rt(a, EPOCH + lane, R::V0);
    store_rt_imm(a, CURSOR + lane, 0);
    store_rt_imm(a, PENDING + lane, 1);
    store_rt_imm(a, CLOSED, 1);
    worker::request_work(a);
    a.jmp_label(busy);
    a.bind_text_label(started);
    load_rt(a, R::V0, epoch);
    load_rt(a, R::V1, EPOCH + lane);
    a.cmp_reg_reg(R::V0, R::V1);
    // An epoch cannot change until this exact weak pass is committed.
    // An incomplete cursor forces another concurrent quantum.
    a.jcc_label(C::NotEqual, busy);
    load_rt(a, R::V0, CURSOR + lane);
    load_rt(a, R::V1, SIZE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, busy);
    store_rt_imm(a, PENDING + lane, 0);
    gate(a);
    a.bind_text_label(done);
}
/// Ticket held, each table slot consumes one existing collector budget unit.
pub(super) fn work<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let done = a.create_text_label();
    growth::step(a, l);
    #[cfg(test)]
    if let Some(hook) = l.weak_work_hook {
        // A deterministic fixture may defer a quantum, never certify it.
        a.call_label(hook);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    for young in [true, false] {
        let lane = usize::from(!young) * 8;
        let next_lane = a.create_text_label();
        let top = a.create_text_label();
        let next = a.create_text_label();
        load_rt(a, R::V0, PENDING + lane);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, next_lane);
        a.bind_text_label(top);
        load_rt(a, R::V8, CURSOR + lane);
        load_rt(a, R::V1, SIZE);
        a.cmp_reg_reg(R::V8, R::V1);
        a.jcc_label(C::AboveOrEqual, next_lane);
        scan::spend(a, done);
        copy::bump(a, UNITS, R::V0);
        entry(a);
        a.load_ptr_disp32(R::V0, R::V8, 16);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, next);
        a.mov_reg_reg(R::V2, R::V0);
        a.shr_reg_imm8(R::V2, 16);
        if young {
            generation::test(a, R::V2, generation::YOUNG);
            a.jcc_label(C::NotEqual, next);
        }
        // Old follows both generations; its closure also decides Young weak
        // targets. Otherwise a Young weak could revive an Old-snapshot white.

        if young {
            young_mark::test(a, R::V2, R::V6, R::V7);
        } else {
            mark::test_mark(a, R::V2, R::V6, R::V7);
        }
        a.jcc_label(C::NotEqual, next);
        a.call_label(l.weak_clear);
        a.bind_text_label(next);
        copy::bump(a, CURSOR + lane, R::V0);
        a.jmp_label(top);
        a.bind_text_label(next_lane);
    }
    a.bind_text_label(done);
}
/// Selected snapshots cannot resurrect a white referent. V2=offset, preserves V8.
fn require_live<E: PortableAsm>(a: &mut E, dead: E::TextLabel) {
    for young in [true, false] {
        let skip = a.create_text_label();
        if young {
            generation::test(a, R::V2, generation::YOUNG);
            a.jcc_label(C::NotEqual, skip);
        }
        load_rt(
            a,
            R::V1,
            if young {
                young_mark::VALID
            } else {
                RT_MARK_VALID
            },
        );
        a.test_reg_reg(R::V1, R::V1);
        a.jcc_label(C::Equal, skip);
        if young {
            young_mark::test(a, R::V2, R::V6, R::V7);
        } else {
            mark::test_mark(a, R::V2, R::V6, R::V7);
        }
        a.jcc_label(C::Equal, dead);
        a.bind_text_label(skip);
    }
}
/// GC owns the token exclusively. Rebind the existing funded token before
/// releasing the source count; this also works with every pool slot occupied.
fn emit_rebind<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.weak_rebind); // V8=entry, V0=canonical GcRef
    quiescence::save(a);
    a.push_reg(R::V0);
    a.push_reg(R::V8);
    a.shr_reg_imm8(R::V0, 16);
    a.shr_reg_imm8(R::V0, 17);
    a.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, run_head)));
    scan::page_ptr(a, R::V7, R::V0, R::V1);
    a.pop_reg(R::V8);
    a.pop_reg(R::V0);
    let same = a.create_text_label();
    a.load_ptr_disp32(R::V6, R::V8, 24);
    a.cmp_reg_reg(R::V6, R::V7);
    a.jcc_label(C::Equal, same);
    a.load_ptr_disp32(R::V2, R::V8, 40);
    // Locate and authenticate the actual slot before touching either count.
    a.mov_reg_reg(R::V1, R::V2);
    a.shl_reg_imm8(R::V1, 32);
    a.shr_reg_imm8(R::V1, 32);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V9, metadata_leases::SIZE);
    a.cmp_reg_reg(R::V1, R::V9);
    a.jcc_label(C::Above, l.fail);
    a.sub_reg_imm8(R::V1, 1);
    a.shl_reg_imm8(R::V1, 5);
    load_rt(
        a,
        R::V9,
        metadata_leases::POOL + offset_of!(crate::gc_abi::MetadataLeasePool, slots),
    );
    a.add_reg_reg(R::V9, R::V1);
    a.load_ptr_disp32(R::V1, R::V9, d(offset_of!(MetadataLeaseSlot, page)));
    a.cmp_reg_reg(R::V1, R::V6);
    a.jcc_label(C::NotEqual, l.fail);
    a.shr_reg_imm8(R::V2, 32);
    a.load_ptr_disp32(R::V1, R::V9, d(offset_of!(MetadataLeaseSlot, generation)));
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V1, R::V6, d(offset_of!(AllocationPage, incarnation)));
    a.load_ptr_disp32(R::V2, R::V8, 32);
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V1, R::V7, d(offset_of!(AllocationPage, metadata_leases)));
    a.add_reg_imm32(R::V1, 1);
    a.store_ptr_disp32(R::V7, d(offset_of!(AllocationPage, metadata_leases)), R::V1);
    a.load_ptr_disp32(R::V2, R::V7, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(R::V9, d(offset_of!(MetadataLeaseSlot, page)), R::V7);
    a.store_ptr_disp32(R::V9, d(offset_of!(MetadataLeaseSlot, incarnation)), R::V2);
    a.push_reg(R::V8);
    a.mov_reg_reg(R::V8, R::V9);
    metadata_leases::mirror(a);
    a.pop_reg(R::V8);
    a.store_ptr_disp32(R::V8, 24, R::V7);
    a.store_ptr_disp32(R::V8, 32, R::V2);
    a.load_ptr_disp32(R::V1, R::V6, d(offset_of!(AllocationPage, metadata_leases)));
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V1, 1);
    a.store_ptr_disp32(R::V6, d(offset_of!(AllocationPage, metadata_leases)), R::V1);
    copy::bump(a, REBOUND, R::V1);
    a.bind_text_label(same);
    a.store_ptr_disp32(R::V8, 16, R::V0);
    growth::mirror(a);
    quiescence::restore(a);
    a.ret();
}
pub(super) fn begin_move<E: PortableAsm>(a: &mut E) {
    store_rt_imm(a, MOVING, 0);
}
pub(super) fn relocate<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    fwd: E::TextLabel,
    paused: E::TextLabel,
) {
    let top = a.create_text_label();
    let done = a.create_text_label();
    let next = a.create_text_label();
    let dead = a.create_text_label();
    a.bind_text_label(top);
    load_rt(a, R::V8, MOVING);
    load_rt(a, R::V1, SIZE);
    a.cmp_reg_reg(R::V8, R::V1);
    a.jcc_label(C::AboveOrEqual, done);
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    scan::spend(a, paused);
    entry(a);
    a.load_ptr_disp32(R::V0, R::V8, 16);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, next);
    a.mov_reg_reg(R::V2, R::V0);
    a.shr_reg_imm8(R::V2, 16);
    // Young is always selected. Old is tested only when physically Full;
    // an unfinished overlapping Old mark cannot decide Old weak liveness.
    let live = a.create_text_label();
    generation::test(a, R::V2, generation::YOUNG);
    let old = a.create_text_label();
    a.jcc_label(C::NotEqual, old);
    young_mark::test(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::Equal, dead);
    a.bind_text_label(old);
    load_rt(a, R::V1, RT_FULL);
    a.test_reg_reg(R::V1, R::V1);
    a.jcc_label(C::Equal, live);
    mark::test_mark(a, R::V2, R::V6, R::V7);
    a.jcc_label(C::Equal, dead);
    a.bind_text_label(live);
    a.push_reg(R::V8);
    a.call_label(fwd);
    a.pop_reg(R::V8);
    a.call_label(l.weak_rebind);
    a.jmp_label(next);
    a.bind_text_label(dead);
    a.call_label(l.weak_clear);
    a.bind_text_label(next);
    copy::bump(a, MOVING, R::V0);
    a.jmp_label(top);
    a.bind_text_label(done);
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    growth::emit(a, l);
    api::emit(a, l);
    emit_rebind(a, l);
    a.bind_text_label(l.weak_clear); // V8=owned entry, ticket held, preserve registers.
    quiescence::save(a);
    a.push_reg(R::V8);
    a.load_ptr_disp32(R::V0, R::V8, 40);
    let empty = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    a.mov_reg_reg(R::V6, R::V8);
    a.add_reg_imm32(R::V6, 24);
    lea_rt(a, R::V7, metadata_leases::POOL);
    a.mov_imm64(R::V2, 1);
    a.call_label(l.metadata_lease_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    copy::bump(a, CLEARED, R::V0);
    a.bind_text_label(empty);
    a.pop_reg(R::V8);
    a.mov_imm64(R::V0, 0);
    for off in [16, 24, 32, 40] {
        a.store_ptr_disp32(R::V8, off, R::V0);
    }
    growth::mirror(a);
    quiescence::restore(a);
    a.ret();
}

/// Legacy one-owner resource wait, entered with the collector ticket held.
pub(super) fn ensure_old<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let Some(w) = &l.worker else {
        return;
    };
    let done = a.create_text_label();
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    for f in [RT_MARK_ACTIVE, RT_MARK_VALID] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
    }
    worker::unlock(a, l);
    a.call_label(w.mark_start);
    worker::lock(a, l);
    a.bind_text_label(done);
}

/// Keep pending native growth/retirement running between bounded quanta.
pub(super) fn retry<E: PortableAsm>(a: &mut E, release: E::TextLabel) {
    growth::retry(a, release);
}
/// Only an unregistered collector view may perform implicit native OS work.
pub(super) fn after_unlock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    growth::after_unlock(a, l);
}

#[cfg(test)]
pub(super) fn growth_metric<E: PortableAsm>(a: &mut E, index: usize) {
    load_rt(a, R::V0, growth::metric_offsets()[index]);
}
#[cfg(test)]
pub(super) fn growth_pause<E: PortableAsm>(a: &mut E, paused: bool) {
    growth::pause(a, paused);
}
#[cfg(test)]
pub(super) fn growth_tick<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    growth::step(a, l);
}
