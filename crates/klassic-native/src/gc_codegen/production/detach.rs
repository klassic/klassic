//! Actual owner detach. Begin closes producers/TLAB before publishing Detaching;
//! bounded steps submit owned regions and wait for their real lifetime finish.
//! Caller retains native context/backing and services steps until DONE, then
//! removes its TLS/register binding before disposing that backing outside GC.
use super::*;
use crate::gc_abi::{RootLifeRequest, RootRegistration, ThreadRegistration};
use crate::gc_codegen::root_storage;

#[repr(C)]
pub(super) struct Owner {
    pub(super) node: root_retirement::Node,
    pub(super) active: u64,
    pub(super) begun: u64,
    pub(super) submitted: u64,
    pub(super) finished: u64,
    pub(super) waits: u64,
}
const OWN: usize = owner_state::offset(offset_of!(OwnerState, detach));
const NODE: usize = OWN + offset_of!(Owner, node);
const ACTIVE: usize = OWN + offset_of!(Owner, active);
const BEGUN: usize = OWN + offset_of!(Owner, begun);
pub(super) const HANDLES: usize = owner_state::offset(offset_of!(OwnerState, native_handles));

fn open_round<E: PortableAsm>(a: &mut E, pending: E::TextLabel) {
    for field in [quiescence::REQUEST, phase_round::RESUMING] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, pending);
    }
}
fn leave<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}
pub(super) fn emit<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    begin(a, l);
    step(a, l);
    dispose(a, l);
}

/// V7 owns detached native OwnerState backing. The caller has already removed
/// that owner's TLS/register binding and holds exclusive backing ownership.
/// Returns 0 on actual OS release, 2 while a real TLS traversal retains native
/// backing, or 6 on OS failure. Retain the address and retry on either 2 or 6.
fn dispose<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.detach_dispose_actor);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    a.cmp_reg_reg(R::V7, R::OwnerContext);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::OwnerContext, R::V7);
    load_rt(
        a,
        R::V3,
        owner_state::offset(offset_of!(OwnerState, shared_runtime)),
    );
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.atomic_load_acquire_u32(R::V0, R::V1);
    a.cmp_reg_imm32(R::V0, 4);
    a.jcc_label(C::NotEqual, l.fail);
    for field in [
        participants::OWNER + offset_of!(ThreadRegistration, registered),
        RT_CTX + offset_of!(MutatorContext, root_segments),
        RT_CTX + offset_of!(MutatorContext, retiring_roots),
        ACTIVE,
        HANDLES,
        thread_tls::OWNER,
        thread_tls::THREAD,
    ] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, l.fail);
    }
    load_rt(a, R::V0, OWN + offset_of!(Owner, submitted));
    load_rt(a, R::V1, OWN + offset_of!(Owner, finished));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    lea_rt(a, R::V7, owner_state::offset(offset_of!(OwnerState, roots)));
    root_storage::emit_can_dispose(a);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V6, RT_SPEC + offset_of!(RootStorageSpec, bytes));
    a.add_reg_imm32(R::V6, d(offset_of!(OwnerState, roots)));
    a.jcc_label(C::Below, l.fail);
    a.mov_reg_reg(R::V7, R::OwnerContext);
    // A current-context traversal may still hold this removed node as its
    // cursor. Reserve disposal under the same ticket as reader admission.
    // Once unlinked, a new reader can never acquire this node.
    worker::lock_raw(a, l);
    load_rt(a, R::V0, thread_tls::READERS);
    let retained = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, retained);
    worker::unlock_raw(a, l);
    // No subsequent instruction dereferences the disposed context, including
    // on OS failure. The surviving caller retains its address for retry.
    a.pop_reg(R::OwnerContext);
    a.call_label(l.metadata_native_release);
    a.pop_reg(R::V3);
    a.ret();
    a.bind_text_label(retained);
    worker::unlock_raw(a, l);
    a.mov_imm64(R::V0, 2);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}

/// Executing Running owner; no live unrooted inputs. 0 entered Detaching,
/// 2 still Running while caller-owned native handles/pins need their cleanup.
fn begin<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.detach_begin_actor);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_running(a, l);
    a.call_label(l.publish_pending);
    super::remset_buffer::flush(a, l, false);
    let retry = a.create_text_label();
    let round = a.create_text_label();
    let busy = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(retry);
    a.call_label(l.owner_checkpoint_poll);
    worker::lock(a, l);
    open_round(a, round);
    load_rt(a, R::V0, BEGUN);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    // Native handles include scoped-pin strong roots. A live external token
    // retains this context; return before changing state so Release can run.
    load_rt(a, R::V0, HANDLES);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, busy);
    native_io::adoption::private_closed(a, l);
    // ROOT_SCOPE is a retained receipt of the last native boundary, not a
    // live scope token. Running plus no borrowed cursor authorizes detach.
    load_rt(a, R::V0, native_io::SCOPE_BORROWED);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    let empty = a.create_text_label();
    load_rt(a, R::V0, RT_ALLOC_ROOT + HANDLE_CELL);
    a.atomic_load_acquire(R::V0, R::V0);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, empty);
    collect::clear_scratch(a, l);
    a.bind_text_label(empty);
    // A pressure record can release the ticket; recheck before publication.
    open_round(a, round);
    let tail = a.create_text_label();
    let funded = a.create_text_label();
    moving_round::admission::native_tail(a, l, tail);
    a.jmp_label(funded);
    a.bind_text_label(tail);
    worker::request_work(a);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.call_label(l.moving_end_actor);
    a.jmp_label(retry);
    a.bind_text_label(funded);
    quiescence::prepare(a, l);
    store_rt_imm(a, BEGUN, 1);
    lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, state));
    a.mov_imm64(R::V0, 3);
    a.atomic_store_release_u32(R::V1, R::V0);
    a.mov_imm64(R::V0, 0);
    a.jmp_label(done);
    a.bind_text_label(round);
    worker::unlock(a, l);
    a.jmp_label(retry);
    a.bind_text_label(busy);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    leave(a, l);
}

/// Executing Detaching owner. No allocation/root producer operation is legal.
/// 2 means keep this context and service another step; 0 means real Detached.
fn step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.detach_step_actor);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    owner_state::bind_detaching(a, l);
    a.call_label(l.owner_checkpoint_poll);
    worker::lock(a, l);
    let pending = a.create_text_label();
    let no_node = a.create_text_label();
    let finish = a.create_text_label();
    let done = a.create_text_label();
    open_round(a, pending);
    load_rt(a, R::V0, ACTIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, no_node);
    lea_rt(
        a,
        R::V1,
        NODE + offset_of!(root_retirement::Node, completed),
    );
    a.atomic_load_acquire(R::V0, R::V1);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, pending);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    // The worker's completion store was its LAST Node access. Reuse only
    // after this acquire; every prior region/reader/journal already finished.
    for off in (0..size_of::<root_retirement::Node>()).step_by(8) {
        store_rt_imm(a, NODE + off, 0);
    }
    store_rt_imm(a, ACTIVE, 0);
    copy::bump(a, OWN + offset_of!(Owner, finished), R::V0);
    a.bind_text_label(no_node);
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, root_segments));
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, finish);
    store_rt(
        a,
        NODE + offset_of!(root_retirement::Node, life) + offset_of!(RootLifeRequest, registration),
        R::V0,
    );
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootRegistration, scope)));
    store_rt(
        a,
        NODE + offset_of!(root_retirement::Node, life) + offset_of!(RootLifeRequest, scope),
        R::V0,
    );
    lea_rt(a, R::V7, NODE);
    root_retirement::submit_locked(a, l);
    store_rt_imm(a, ACTIVE, 1);
    copy::bump(a, OWN + offset_of!(Owner, submitted), R::V0);
    a.jmp_label(pending);
    a.bind_text_label(finish);
    load_rt(
        a,
        R::V0,
        RT_CTX + offset_of!(MutatorContext, retiring_roots),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, pending);
    // The common detach operation validates actual TLAB/buffer consumption,
    // borrowed consumers and registry links before removing this participant.
    lea_rt(a, R::V7, participants::HANDSHAKE);
    lea_rt(a, R::V6, participants::OWNER);
    a.mov_imm64(R::V2, 5);
    a.call_label(l.participant_step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    a.bind_text_label(pending);
    copy::bump(a, OWN + offset_of!(Owner, waits), R::V0);
    // Real OS-worker startup failure retains bounded owner assistance.
    let worker = a.create_text_label();
    load_rt(a, R::V0, RT_WORKER_ALIVE);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, worker);
    a.call_label(l.root_retirement_step);
    a.bind_text_label(worker);
    worker::request_work(a);
    a.mov_imm64(R::V0, 2);
    a.bind_text_label(done);
    // No subsequent registry/poll operation may touch the removed participant.
    leave(a, l);
}
