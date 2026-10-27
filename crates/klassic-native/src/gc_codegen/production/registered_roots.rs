//! Budgeted registered-region inventory for independent Old, Young and
//! relocation frontiers. Operations hold the collector lock (or unpublished
//! startup). Fresh attach publishes an empty region with the current snapshot
//! and actual bank under that ticket. Retirement still needs generation-specific
//! revision auditing and deletion/journal handoff before an owner can detach.
use super::*;
use crate::gc_abi::{
    RootLease, RootLifeRequest, RootRegistry, RootSegment, RootWalk, RootWalkItem,
};
use crate::gc_codegen::production::{OwnerState, lea_rt, load_rt, owner_state, store_rt};

#[repr(C)]
pub(super) struct State {
    registry: RootRegistry,
    request: RootLifeRequest,
    old: RootWalk,
    young: RootWalk,
    relocation: RootWalk,
    item: RootWalkItem,
}
const BASE: usize = offset_of!(Runtime, registered_roots);
pub(super) const REGISTRY: usize = BASE + offset_of!(State, registry);
pub(super) fn registration<E: PortableAsm>(a: &E) -> usize {
    {
        let _ = a;
        owner_state::offset(offset_of!(OwnerState, roots) + offset_of!(RootStorage, registration))
    }
}
const REQUEST: usize = BASE + offset_of!(State, request);
pub(super) const OLD: usize = BASE + offset_of!(State, old) + offset_of!(RootWalk, lease);
pub(super) const YOUNG: usize = BASE + offset_of!(State, young) + offset_of!(RootWalk, lease);
pub(super) const RELOCATION: usize =
    BASE + offset_of!(State, relocation) + offset_of!(RootWalk, lease);

/// V0..V2 clobbered; preserve frontier, budget and caller scratch registers.
fn step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, operation: u64) {
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
    lea_rt(a, R::V7, REGISTRY);
    lea_rt(a, R::V6, REQUEST);
    a.mov_imm64(R::V2, operation);
    a.call_label(l.root_life_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
}

pub(super) fn startup<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    store_rt_addr(
        a,
        REQUEST + offset_of!(RootLifeRequest, registration),
        registration(a),
    );
    store_rt_addr(a, REQUEST + offset_of!(RootLifeRequest, context), RT_CTX);
    step(a, l, 0);
}

/// Called after checked adoption and before the owner resumes. Existing
/// reader leases remain valid across unrelated generation phase changes.
pub(super) fn adopted<E: PortableAsm>(a: &mut E) {
    lea_rt(a, R::V1, REGISTRY + offset_of!(RootRegistry, phase));
    a.atomic_store_release(R::V1, R::V0);
}

fn walk(lease: usize) -> usize {
    lease - offset_of!(RootWalk, lease)
}
fn frontier(lease: usize) -> (usize, usize) {
    match lease {
        OLD => (RT_M_ROOT_INDEX, RT_M_ROOT_BOUND),
        YOUNG => (young_mark::ROOT, young_mark::ROOT_BOUND),
        RELOCATION => (RT_R_ROOT_INDEX, RT_R_ROOT_BOUND),
        _ => unreachable!("production root lane"),
    }
}
const ITEM: usize = BASE + offset_of!(State, item);
fn save<E: PortableAsm>(a: &mut E) {
    for r in [R::V6, R::V7, R::V8, R::V9, R::V10, R::V11] {
        a.push_reg(r);
    }
}
fn restore<E: PortableAsm>(a: &mut E) {
    for r in [R::V11, R::V10, R::V9, R::V8, R::V7, R::V6] {
        a.pop_reg(r);
    }
}
/// O(1) list step: never traverses cells or skips an unbounded run of empty regions.
fn advance<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, lease: usize, op: u64) {
    save(a);
    lea_rt(a, R::V7, REGISTRY);
    lea_rt(a, R::V6, walk(lease));
    a.mov_imm64(R::V2, op);
    a.call_label(l.root_walk_step);
    let ok = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, ok);
    a.cmp_reg_imm32(R::V0, 2);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(ok);
    restore(a);
    let exhausted = a.create_text_label();
    load_rt(a, R::V0, walk(lease) + offset_of!(RootWalk, active));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, exhausted);
    // Only this lane's work is acknowledged by this iterator. No MarkQueue
    // audit certificate is synthesized from these custom production queues.
    let mask = if lease == OLD {
        2
    } else if lease == YOUNG {
        1
    } else {
        0
    };
    store_rt_imm(a, walk(lease) + offset_of!(RootWalk, mask), mask);
    let (index, bound) = frontier(lease);
    load_rt(a, R::V0, walk(lease) + offset_of!(RootWalk, limit));
    store_rt(a, bound, R::V0);
    a.mov_imm64(R::V0, 0);
    let ready = a.create_text_label();
    load_rt(a, R::V1, lease + offset_of!(RootLease, registration));
    a.load_ptr_disp32(
        R::V2,
        R::V1,
        d(offset_of!(crate::gc_abi::RootRegistration, context)),
    );
    // Production contexts lead their actual OwnerState. A worker must skip
    // the visited owner's scratch, independently of its own OwnerContext.
    const _: () = assert!(offset_of!(OwnerState, ctx) == 0);
    a.load_ptr_disp32(R::V2, R::V2, d(offset_of!(OwnerState, segment)));
    a.load_ptr_disp32(
        R::V1,
        R::V1,
        d(offset_of!(crate::gc_abi::RootRegistration, segment)),
    );
    a.cmp_reg_reg(R::V1, R::V2);
    a.jcc_label(C::NotEqual, ready);
    // Cell zero of the owner frame storage is allocation scratch, published
    // and emptied before each start. Other registered regions include cell 0.
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(ready);
    store_rt(a, index, R::V0);
    store_rt(a, walk(lease) + offset_of!(RootWalk, cursor), R::V0);
    a.bind_text_label(exhausted);
}
pub(super) fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, lease: usize) {
    advance(a, l, lease, 0);
}
/// O(1) completion checks include the inventory cursor, not just this region's bound.
pub(super) fn pending<E: PortableAsm>(a: &mut E, lease: usize, busy: E::TextLabel) {
    load_rt(a, R::V0, walk(lease) + offset_of!(RootWalk, active));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, busy);
}
pub(super) fn finish<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, lease: usize) {
    pending(a, lease, l.fail);
}
/// Branch to `empty` only after NEXT releases the final cursor. Every list
/// transition to another region costs a unit, including empty regions. Final
/// release is constant metadata completion; budget yields retain live cursors.
pub(super) fn prepare<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    lease: usize,
    top: E::TextLabel,
    empty: E::TextLabel,
    paused: E::TextLabel,
) {
    let active = a.create_text_label();
    pending(a, lease, active);
    a.jmp_label(empty);
    a.bind_text_label(active);
    let (index, bound) = frontier(lease);
    load_rt(a, R::V0, index);
    load_rt(a, R::V1, bound);
    a.cmp_reg_reg(R::V0, R::V1);
    let cell = a.create_text_label();
    a.jcc_label(C::Below, cell);
    // Releasing the final cursor is constant metadata completion, as in the
    // former single-region frontier. Moving to another region costs a unit.
    let last = a.create_text_label();
    load_rt(a, R::V0, lease + offset_of!(RootLease, registration));
    a.load_ptr_disp32(
        R::V0,
        R::V0,
        d(offset_of!(crate::gc_abi::RootRegistration, inventory_next)),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, last);
    super::scan::spend(a, paused);
    a.bind_text_label(last);
    load_rt(a, R::V0, walk(lease) + offset_of!(RootWalk, mask));
    store_rt(a, walk(lease) + offset_of!(RootWalk, completed_mask), R::V0);
    advance(a, l, lease, 1);
    a.jmp_label(top);
    a.bind_text_label(cell);
    super::scan::spend(a, paused);
}
fn item<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, lease: usize, op: u64) {
    store_rt_addr(a, ITEM + offset_of!(RootWalkItem, walk), walk(lease));
    load_rt(a, R::V0, lease + offset_of!(RootLease, serial));
    store_rt(a, ITEM + offset_of!(RootWalkItem, serial), R::V0);
    save(a);
    lea_rt(a, R::V7, ITEM);
    a.mov_imm64(R::V6, op);
    a.call_label(l.root_walk_item);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    restore(a);
}
/// CAPTURE retains the observed reference and cursor until the actual mark or
/// forwarding and exact-word repair finish, then ACK permits list advance.
/// Result: V0 observed reference, V10 cell address.
pub(super) fn capture<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, lease: usize) {
    item(a, l, lease, 0);
    load_rt(a, R::V10, lease + offset_of!(RootLease, registration));
    a.load_ptr_disp32(
        R::V10,
        R::V10,
        d(offset_of!(crate::gc_abi::RootRegistration, segment)),
    );
    a.load_ptr_disp32(R::V10, R::V10, d(offset_of!(RootSegment, cells)));
    load_rt(a, R::V0, ITEM + offset_of!(RootWalkItem, index));
    a.shl_reg_imm8(R::V0, 5);
    a.add_reg_reg(R::V10, R::V0);
    load_rt(
        a,
        R::V0,
        walk(lease) + offset_of!(RootWalk, pending_reference),
    );
}
pub(super) fn acknowledge<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, lease: usize) {
    item(a, l, lease, 1);
    let (index, _) = frontier(lease);
    load_rt(a, R::V0, walk(lease) + offset_of!(RootWalk, cursor));
    store_rt(a, index, R::V0);
}
