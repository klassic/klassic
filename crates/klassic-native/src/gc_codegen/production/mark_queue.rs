//! Direct marker queues and prepared record-backed overflow ownership.
//! SpillNode is a PRIVATE native page-holder receipt. Its `page` is never a
//! shared MarkTask.metadata_lease token. MarkRecord already funds the storage.
use super::*;
#[path = "mark_queue_growth.rs"]
mod backing;
#[repr(C)]
pub(super) struct SpillNode {
    pub next: u64, // next MarkRecord address, not a managed payload address
    pub serial: u64,
    pub state: u64, // 0 free, 1 queued; claim is a ticket-local transfer
    pub record: u64,
    pub reference: u64,
    pub incarnation: u64,
    pub page: u64,
    pub epoch: u64,
}
const _: () = assert!(size_of::<SpillNode>() == size_of::<crate::gc_abi::MarkTask>());
#[repr(C)]
pub(super) struct Lane {
    entries: u64,
    capacity: u64,
    head: u64,
    tail: u64,
    count: u64,
    last_serial: u64,
    active_page: u64,
    active_incarnation: u64,
    active_serial: u64,
    active_epoch: u64,
    pub relocate_previous: u64,
    pub relocate_current: u64,
    pub discovery_current: u64,
    pub discovery_tail: u64,
    pub discovery_frozen: u64,
    growth: Growth,
}
#[repr(C)]
struct Growth {
    stage: u64,
    buffer: u64,
    capacity: u64,
    cursor: u64,
    tail: u64,
    old: u64,
    old_bytes: u64,
    error: u64,
    failures: u64,
    grown: u64,
    copied: u64,
    released: u64,
}
#[repr(C)]
pub(super) struct State {
    lanes: [Lane; 2], // Young0, Old1
    holders: u64,
}
const BASE: usize = offset_of!(Runtime, mark_queue);
pub(super) const HOLDERS: usize = BASE + offset_of!(State, holders);
pub(super) const fn lane(young: bool) -> usize {
    BASE + offset_of!(State, lanes) + (!young as usize) * size_of::<Lane>()
}
pub(super) const fn entries(young: bool) -> usize {
    lane(young) + offset_of!(Lane, entries)
}
pub(super) const fn capacity(young: bool) -> usize {
    lane(young) + offset_of!(Lane, capacity)
}
pub(super) const fn head(young: bool) -> usize {
    lane(young) + offset_of!(Lane, head)
}
pub(super) const fn tail(young: bool) -> usize {
    lane(young) + offset_of!(Lane, tail)
}
pub(super) const fn count(young: bool) -> usize {
    lane(young) + offset_of!(Lane, count)
}
pub(super) const fn active_page(young: bool) -> usize {
    lane(young) + offset_of!(Lane, active_page)
}
pub(super) const fn active_incarnation(young: bool) -> usize {
    lane(young) + offset_of!(Lane, active_incarnation)
}
pub(super) const fn relocate_previous() -> usize {
    lane(false) + offset_of!(Lane, relocate_previous)
}
pub(super) const fn relocate_current() -> usize {
    lane(false) + offset_of!(Lane, relocate_current)
}
pub(super) const fn discovery_current() -> usize {
    lane(false) + offset_of!(Lane, discovery_current)
}
pub(super) const fn discovery_tail() -> usize {
    lane(false) + offset_of!(Lane, discovery_tail)
}
pub(super) const fn discovery_frozen() -> usize {
    lane(false) + offset_of!(Lane, discovery_frozen)
}
pub(super) const fn node_offset(young: bool) -> usize {
    if young {
        offset_of!(MarkRecord, young_spill)
    } else {
        offset_of!(MarkRecord, old_spill)
    }
}
/// Materialize a native node; no managed address or lifetime change.
pub(super) fn node<E: PortableAsm>(a: &mut E, young: bool, record: R, dst: R) {
    a.mov_reg_reg(dst, record);
    a.add_reg_imm32(dst, d(node_offset(young)));
}

/// Exact native source-page borrow, funded by the containing prepared node.
/// Distinct from pooled tokens; source census counts both holder ledgers.
pub(super) fn retain_page<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, page: R) {
    // The retirement census admits no new holder behind its passed source
    // cursor. Canonical destination jobs remain admissible throughout Retire.
    quiescence::save(a);
    a.mov_reg_reg(R::V8, page);
    let admitted = a.create_text_label();
    load_rt(a, R::V0, RT_RETIRE_STAGE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, admitted);
    a.load_ptr_disp32(R::V8, R::V8, d(offset_of!(AllocationPage, run_head)));
    metadata_leases::source(a, l.fail, admitted);
    a.bind_text_label(admitted);
    quiescence::restore(a);
    a.load_ptr_disp32(R::V0, page, d(offset_of!(AllocationPage, metadata_leases)));
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    a.store_ptr_disp32(page, d(offset_of!(AllocationPage, metadata_leases)), R::V0);
    load_rt(a, R::V0, HOLDERS);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, HOLDERS, R::V0);
}
pub(super) fn release_page<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, page: R) {
    a.load_ptr_disp32(R::V0, page, d(offset_of!(AllocationPage, metadata_leases)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    a.store_ptr_disp32(page, d(offset_of!(AllocationPage, metadata_leases)), R::V0);
    load_rt(a, R::V0, HOLDERS);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, HOLDERS, R::V0);
}
/// V2=heap offset -> V8 physical run head, V0/V1 scratch. V2 is preserved.
pub(super) fn page_for_offset<E: PortableAsm>(a: &mut E) {
    a.mov_reg_reg(R::V0, R::V2);
    a.sub_reg_imm8(R::V0, size_of::<ObjectHeader>() as i8);
    a.shr_reg_imm8(R::V0, 17);
    a.sub_reg_imm8(R::V0, 1);
    scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, run_head)));
    scan::page_ptr(a, R::V8, R::V0, R::V1);
}
/// Same-epoch unique overflow work. Called after the real mark stamp, under
/// the queue ticket; storage already belongs to this actual object's record.
pub(super) fn spill<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, young: bool) {
    quiescence::save(a);
    // Young's caller already removed uncertified Old mark bits. Preserve that
    // exact admitted reference, rather than inventing a good-color receipt.
    if young {
        a.mov_reg_reg(R::V11, R::V8);
    }
    load_rt(a, R::V9, RT_HEAP_BASE);
    a.add_reg_reg(R::V9, R::V2);
    a.load_ptr_disp32(R::V9, R::V9, copy::MARK_RECORD);
    node(a, young, R::V9, R::V10);
    a.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, state)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    page_for_offset(a);
    retain_page(a, l, R::V8);
    let serial = lane(young) + offset_of!(Lane, last_serial);
    load_rt(a, R::V0, serial);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    store_rt(a, serial, R::V0);
    a.store_ptr_disp32(R::V10, d(offset_of!(SpillNode, serial)), R::V0);
    a.mov_imm64(R::V0, 1);
    a.store_ptr_disp32(R::V10, d(offset_of!(SpillNode, state)), R::V0);
    a.store_ptr_disp32(R::V10, d(offset_of!(SpillNode, record)), R::V9);
    a.store_ptr_disp32(R::V10, d(offset_of!(SpillNode, page)), R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.store_ptr_disp32(R::V10, d(offset_of!(SpillNode, incarnation)), R::V0);
    load_rt(
        a,
        R::V0,
        if young {
            young_mark::EPOCH
        } else {
            RT_MARK_CYCLES
        },
    );
    a.store_ptr_disp32(R::V10, d(offset_of!(SpillNode, epoch)), R::V0);
    if young {
        a.mov_reg_reg(R::V0, R::V11);
    } else {
        a.mov_reg_reg(R::V0, R::V2);
        a.shl_reg_imm8(R::V0, 16);
        colours::load_current(a, R::V1);
        a.or_reg_reg(R::V0, R::V1);
    }
    a.store_ptr_disp32(R::V10, d(offset_of!(SpillNode, reference)), R::V0);
    a.mov_imm64(R::V0, 0);
    a.store_ptr_disp32(R::V10, d(offset_of!(SpillNode, next)), R::V0);
    load_rt(a, R::V8, tail(young));
    a.mov_reg_reg(R::V11, R::V8); // exact predecessor before this append
    let empty = a.create_text_label();
    let linked = a.create_text_label();
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, empty);
    node(a, young, R::V8, R::V8);
    a.store_ptr_disp32(R::V8, d(offset_of!(SpillNode, next)), R::V9);
    a.jmp_label(linked);
    a.bind_text_label(empty);
    store_rt(a, head(young), R::V9);
    a.bind_text_label(linked);
    store_rt(a, tail(young), R::V9);
    copy::bump(a, count(young), R::V0);
    if !young {
        let done = a.create_text_label();
        load_rt(a, R::V0, RT_PHASE_OPEN);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::Equal, done);
        load_rt(a, R::V0, RT_RETIRE_STAGE);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
        load_rt(a, R::V0, relocate_current());
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, done);
        a.load_ptr_disp32(R::V8, R::V10, d(offset_of!(SpillNode, page)));
        a.load_ptr_disp32(R::V8, R::V8, d(offset_of!(AllocationPage, run_head)));
        let source = a.create_text_label();
        metadata_leases::source(a, source, done);
        a.bind_text_label(source);
        store_rt(a, relocate_previous(), R::V11);
        store_rt(a, relocate_current(), R::V9);
        a.bind_text_label(done);
    }
    backing::request(a, l, young);
    worker::request_work(a);
    quiescence::restore(a);
}
/// Spill take transfers its exact borrow into runtime-stable active storage.
/// V2 returns the real queued GcRef; branch `empty` leaves no claimed work.
pub(super) fn take<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    young: bool,
    empty: E::TextLabel,
) {
    if !young {
        load_rt(a, R::V0, discovery_frozen());
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, empty);
    }
    load_rt(a, R::V9, head(young));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, empty);
    node(a, young, R::V9, R::V10);
    a.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, state)));
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, record)));
    a.cmp_reg_reg(R::V0, R::V9);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, epoch)));
    load_rt(
        a,
        R::V1,
        if young {
            young_mark::EPOCH
        } else {
            RT_MARK_CYCLES
        },
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    a.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, serial)));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, lane(young) + offset_of!(Lane, last_serial));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    a.load_ptr_disp32(R::V8, R::V10, d(offset_of!(SpillNode, page)));
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    a.load_ptr_disp32(R::V1, R::V10, d(offset_of!(SpillNode, incarnation)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, active_page(young));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    for (from, to) in [
        (offset_of!(SpillNode, page), active_page(young)),
        (
            offset_of!(SpillNode, incarnation),
            active_incarnation(young),
        ),
        (
            offset_of!(SpillNode, serial),
            lane(young) + offset_of!(Lane, active_serial),
        ),
        (
            offset_of!(SpillNode, epoch),
            lane(young) + offset_of!(Lane, active_epoch),
        ),
    ] {
        a.load_ptr_disp32(R::V0, R::V10, d(from));
        store_rt(a, to, R::V0);
    }
    a.load_ptr_disp32(R::V2, R::V10, d(offset_of!(SpillNode, reference)));
    a.load_ptr_disp32(R::V0, R::V10, d(offset_of!(SpillNode, next)));
    store_rt(a, head(young), R::V0);
    let rest = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, rest);
    store_rt_imm(a, tail(young), 0);
    a.bind_text_label(rest);
    load_rt(a, R::V0, count(young));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    a.sub_reg_imm8(R::V0, 1);
    store_rt(a, count(young), R::V0);
    a.mov_imm64(R::V0, 0);
    for off in (0..size_of::<SpillNode>()).step_by(8) {
        a.store_ptr_disp32(R::V10, d(off), R::V0);
    }
}
pub(super) fn finish_active<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, young: bool) {
    quiescence::save(a);
    let done = a.create_text_label();
    load_rt(a, R::V8, active_page(young));
    a.test_reg_reg(R::V8, R::V8);
    a.jcc_label(C::Equal, done);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    load_rt(a, R::V1, active_incarnation(young));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, lane(young) + offset_of!(Lane, active_epoch));
    load_rt(
        a,
        R::V1,
        if young {
            young_mark::EPOCH
        } else {
            RT_MARK_CYCLES
        },
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, lane(young) + offset_of!(Lane, active_serial));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, lane(young) + offset_of!(Lane, last_serial));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Above, l.fail);
    release_page(a, l, R::V8);
    for f in [
        active_page(young),
        active_incarnation(young),
        lane(young) + offset_of!(Lane, active_serial),
        lane(young) + offset_of!(Lane, active_epoch),
    ] {
        store_rt_imm(a, f, 0);
    }
    a.bind_text_label(done);
    quiescence::restore(a);
}
/// Active receiver already underwent real forwarding. Native ledger ownership
/// moves to its actual canonical page before the old page can retire.
pub(super) fn rebind_active_old<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    quiescence::save(a);
    let done = a.create_text_label();
    load_rt(a, R::V9, active_page(false));
    a.test_reg_reg(R::V9, R::V9);
    a.jcc_label(C::Equal, done);
    a.shr_reg_imm8(R::V2, 16);
    page_for_offset(a);
    a.cmp_reg_reg(R::V8, R::V9);
    a.jcc_label(C::Equal, done);
    retain_page(a, l, R::V8);
    store_rt(a, active_page(false), R::V8);
    a.load_ptr_disp32(R::V0, R::V8, d(offset_of!(AllocationPage, incarnation)));
    store_rt(a, active_incarnation(false), R::V0);
    release_page(a, l, R::V9);
    a.bind_text_label(done);
    quiescence::restore(a);
}
pub(super) fn freeze_old<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, head(false));
    store_rt(a, discovery_current(), R::V0);
    load_rt(a, R::V0, tail(false));
    store_rt(a, discovery_tail(), R::V0);
    let empty = a.create_text_label();
    let done = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    store_rt_imm(a, discovery_frozen(), 1);
    a.jmp_label(done);
    a.bind_text_label(empty);
    store_rt_imm(a, discovery_frozen(), 0);
    a.bind_text_label(done);
}

pub(super) fn startup<E: PortableAsm>(a: &mut E) {
    for young in [true, false] {
        store_rt_addr(
            a,
            entries(young),
            if young {
                layout(a).ystack
            } else {
                layout(a).mstack
            } as usize,
        );
        store_rt_imm(a, capacity(young), MSTACK_ENTRIES);
    }
}
/// Zero-based index -> current queue slot; preserve index unless it is dst.
pub(super) fn slot<E: PortableAsm>(a: &mut E, young: bool, index: R, dst: R) {
    a.mov_reg_reg(dst, index);
    a.shl_reg_imm8(dst, 3);
    load_rt(a, R::V1, entries(young));
    a.add_reg_reg(dst, R::V1);
}
#[cfg(test)]
pub(super) fn test_capacity<E: PortableAsm>(a: &mut E, young: bool, slots: u64) {
    assert!(slots > 0 && slots <= MSTACK_ENTRIES);
    store_rt_imm(a, capacity(young), slots);
}
#[cfg(test)]
pub(super) fn test_metric<E: PortableAsm>(a: &mut E, young: bool, index: usize) {
    let f = [
        capacity(young),
        count(young),
        HOLDERS,
        head(young),
        tail(young),
        active_page(young),
        discovery_frozen(),
        relocate_current(),
    ][index];
    load_rt(a, R::V0, f);
}

pub(super) fn work<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for young in [true, false] {
        backing::step(a, l, young);
    }
}
pub(super) fn retry<E: PortableAsm>(a: &mut E, release: E::TextLabel) {
    for young in [true, false] {
        backing::retry(a, release, young);
    }
}
pub(super) fn after_unlock<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for young in [true, false] {
        backing::after_unlock(a, l, young);
    }
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for young in [true, false] {
        backing::emit(a, l, young);
    }
}
pub(super) fn service<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let pending = a.create_text_label();
    let done = a.create_text_label();
    backing::hint(a, pending, true);
    backing::hint(a, pending, false);
    a.jmp_label(done);
    a.bind_text_label(pending);
    quiescence::save(a);
    worker::lock(a, l);
    work(a, l);
    worker::unlock(a, l);
    for label in l.queue_grow_services {
        a.call_label(label);
    }
    quiescence::restore(a);
    a.bind_text_label(done);
}
/// Mirror one published queue word, preserving every register. Entry pointer
/// is ticket-local and never retained after candidate publication.
pub(super) fn mirror_address<E: PortableAsm>(a: &mut E, young: bool, entry: R) {
    quiescence::save(a);
    a.mov_reg_reg(R::V8, entry);
    backing::mirror(a, young);
    quiescence::restore(a);
}
#[cfg(test)]
pub(super) fn growth_metric<E: PortableAsm>(a: &mut E, young: bool, index: usize) {
    load_rt(a, R::V0, backing::metric_offsets(young)[index]);
}
