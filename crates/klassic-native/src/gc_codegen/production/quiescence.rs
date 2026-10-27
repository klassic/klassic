//! Actual peer checkpoints over the registered finite handshake. No roots are
//! traversed and no phase/color/bank is changed here. Integrating a moving GC
//! still requires captured-phase transport/adoption and all generation audits.
use super::*;
use crate::gc_abi::{HandshakeState, ThreadRegistration};
#[repr(C)]
pub(super) struct State {
    request: u64,
    completed: u64,
    phase: u64,
    bank_revision: u64,
    expected: u64,
    acknowledged: u64,
    retries: u64,
}
const BASE: usize = offset_of!(Runtime, quiescence);
pub(super) const REQUEST: usize = BASE + offset_of!(State, request);
pub(super) const COMPLETED: usize = BASE + offset_of!(State, completed);
pub(super) const EXPECTED: usize = BASE + offset_of!(State, expected);
pub(super) const ACKNOWLEDGED: usize = BASE + offset_of!(State, acknowledged);
pub(super) const RETRIES: usize = BASE + offset_of!(State, retries);
const PHASE: usize = BASE + offset_of!(State, phase);
const BANK: usize = BASE + offset_of!(State, bank_revision);
pub(super) const OWN_ACKS: usize = owner_state::offset(offset_of!(OwnerState, checkpoint_acks));
const SAVED: [R; 11] = [
    R::OwnerContext,
    R::V0,
    R::V1,
    R::V2,
    R::V3,
    R::V6,
    R::V7,
    R::V8,
    R::V9,
    R::V10,
    R::V11,
];
pub(super) fn save<E: PortableAsm>(a: &mut E) {
    for r in SAVED {
        a.push_reg(r);
    }
}
pub(super) fn restore<E: PortableAsm>(a: &mut E) {
    for r in SAVED.into_iter().rev() {
        a.pop_reg(r);
    }
}
fn acquire<E: PortableAsm>(a: &mut E, field: usize) {
    lea_rt(a, R::V1, field);
    a.atomic_load_acquire(R::V0, R::V1);
}
fn release<E: PortableAsm>(a: &mut E, field: usize) {
    lea_rt(a, R::V1, field);
    a.atomic_store_release(R::V1, R::V0);
}
fn hs(field: usize) -> usize {
    participants::HANDSHAKE + field
}
/// Called only at an allocation/explicit safepoint whose caller has no live
/// unrooted address. Root/field primitives with raw inputs do not poll.
pub(super) fn poll<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    acquire(a, REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    let done = a.create_text_label();
    a.jcc_label(C::Equal, done);
    a.call_label(l.owner_checkpoint_poll);
    a.bind_text_label(done);
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
}
/// Under the collector ticket, at the executing owner's real safepoint.
/// Root steps already release-publish their journals; retain that backlog.
/// This checkpoint does not drain/walk every region or certify GC termination.
pub(super) fn prepare<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    prepare_private(a, l);
    alloc::close_owner_tlab(a, l);
    empty_tlab(a, l);
}
fn prepare_private<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    copy_claims::private_closed(a, l);
    moving_round::admission::refund(a, l);
    a.call_label(l.publish_pending);
    for f in [
        RT_COLLECTING,
        RT_CTX + offset_of!(MutatorContext, private_producers),
        RT_CTX + offset_of!(MutatorContext, access_epoch),
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
}
fn empty_tlab<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    for f in [
        offset_of!(MutatorContext, tlab_start),
        offset_of!(MutatorContext, tlab_top),
        offset_of!(MutatorContext, tlab_end),
    ] {
        load_rt(a, R::V0, RT_CTX + f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
}
/// Same-bank mark/end/cleanup rounds retain the affine page reservation while
/// exposing no allocating TLAB in the parked context. Native entry and plain
/// Idle checkpoints still close; physical start must close before bank reuse.
pub(super) fn prepare_old<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    let legacy = a.create_text_label();
    let done = a.create_text_label();
    load_rt(a, R::V0, cooperative_old::ENABLED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, legacy);
    prepare_private(a, l);
    old_tlab::suspend(a, l);
    empty_tlab(a, l);
    a.jmp_label(done);
    a.bind_text_label(legacy);
    prepare(a, l);
    a.bind_text_label(done);
}
fn journal_backlog<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, after: bool) {
    use crate::gc_abi::BufferCursor;
    load_rt(a, R::V2, RT_JOURNAL);
    a.mov_reg_reg(R::V1, R::V2);
    a.add_reg_imm32(R::V1, offset_of!(BufferCursor, published) as i32);
    a.atomic_load_acquire(R::V0, R::V1);
    a.add_reg_imm32(R::V2, offset_of!(BufferCursor, consumed) as i32);
    a.atomic_load_acquire(R::V1, R::V2);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, l.fail);
    a.sub_reg_reg(R::V0, R::V1);
    store_rt(
        a,
        owner_state::offset(if after {
            offset_of!(OwnerState, checkpoint_journal_after)
        } else {
            offset_of!(OwnerState, checkpoint_journal_before)
        }),
        R::V0,
    );
}
fn ack<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    journal_backlog(a, l, false);
    let ordinary = a.create_text_label();
    let prepared = a.create_text_label();
    load_rt(a, R::V0, phase_round::KIND);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, ordinary);
    // Only the physical start changes banks. End/retry/Idle rounds keep
    // the current destination page affine to its actual owner.
    a.cmp_reg_imm32(R::V0, 5);
    a.jcc_label(C::Equal, ordinary);
    prepare_old(a, l);
    a.jmp_label(prepared);
    a.bind_text_label(ordinary);
    prepare(a, l);
    a.bind_text_label(prepared);
    phase_round::prepare_peer(a, l);
    participants::step(a, l, 1);
    journal_backlog(a, l, true);
    copy::bump(a, OWN_ACKS, R::V0);
}
fn relax<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    if let Some(w) = &l.worker {
        a.call_label(w.hooks.relax);
    }
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.poll_actor);
    save(a);
    owner_state::bind_running(a, l);
    poll(a, l);
    a.call_label(l.cooperative_old_poll);
    restore(a);
    a.ret();
    a.bind_text_label(l.owner_checkpoint_poll);
    save(a);
    let again = a.create_text_label();
    let no_request = a.create_text_label();
    let waiting = a.create_text_label();
    let resumed = a.create_text_label();
    a.bind_text_label(again);
    worker::lock(a, l);
    acquire(a, REQUEST);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no_request);
    load_rt(a, R::V1, hs(offset_of!(HandshakeState, generation)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V1,
        participants::OWNER + offset_of!(ThreadRegistration, acknowledged_generation),
    );
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, waiting);
    ack(a, l);
    a.bind_text_label(waiting);
    worker::unlock(a, l);
    relax(a, l);
    a.jmp_label(again);
    a.bind_text_label(no_request);
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.atomic_load_acquire_u32(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 0);
    a.jcc_label(C::Equal, resumed);
    let stopping = a.create_text_label();
    let resume = a.create_text_label();
    a.cmp_reg_imm32(R::V0, 3);
    a.jcc_label(C::Equal, stopping);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.jmp_label(resume);
    a.bind_text_label(stopping);
    // Detaching remains state 3 both at rest and after ACK. No owned phase
    // means there is nothing to resume. A coordinator may have begun a new
    // handshake but not published REQUEST yet; RESUME there is invalid.
    load_rt(a, R::V0, phase_round::OWN_KIND);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, resumed);
    a.bind_text_label(resume);
    phase_round::resume_peer(a, l);
    participants::step(a, l, 2);
    a.bind_text_label(resumed);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    restore(a);
    a.ret();

    a.bind_text_label(l.checkpoint_actor);
    save(a);
    owner_state::bind_running(a, l);
    worker::lock(a, l);
    acquire(a, REQUEST);
    let begin = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, begin);
    worker::unlock(a, l);
    a.call_label(l.owner_checkpoint_poll);
    restore(a);
    a.ret();
    a.bind_text_label(begin);
    acquire(
        a,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    store_rt(a, PHASE, R::V0);
    a.and_reg_imm32(R::V0, 255);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    for f in [
        RT_PHASE_OPEN,
        RT_RETIRE_STAGE,
        RT_MARK_ACTIVE,
        young_mark::ACTIVE,
    ] {
        load_rt(a, R::V0, f);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    acquire(a, allocation_records::REVISION);
    store_rt(a, BANK, R::V0);
    journal_backlog(a, l, false);
    prepare(a, l);
    participants::step(a, l, 6);
    participants::step(a, l, 1);
    journal_backlog(a, l, true);
    copy::bump(a, OWN_ACKS, R::V0);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, expected)));
    store_rt(a, EXPECTED, R::V0);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, generation)));
    release(a, REQUEST);
    worker::unlock(a, l);
    let wait = a.create_text_label();
    let complete = a.create_text_label();
    a.bind_text_label(wait);
    relax(a, l);
    worker::lock(a, l);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, acknowledged)));
    load_rt(a, R::V1, hs(offset_of!(HandshakeState, expected)));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, complete);
    a.jcc_label(C::Above, l.fail);
    copy::bump(a, RETRIES, R::V0);
    worker::unlock(a, l);
    a.jmp_label(wait);
    a.bind_text_label(complete);
    store_rt(a, ACKNOWLEDGED, R::V0);
    acquire(
        a,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    load_rt(a, R::V1, PHASE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    acquire(a, allocation_records::REVISION);
    load_rt(a, R::V1, BANK);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    participants::step(a, l, 7);
    participants::step(a, l, 2);
    load_rt(a, R::V0, hs(offset_of!(HandshakeState, generation)));
    release(a, COMPLETED);
    a.mov_imm64(R::V0, 0);
    release(a, REQUEST);
    worker::unlock(a, l);
    restore(a);
    a.ret();
    phase_round::emit(a, l);
}
