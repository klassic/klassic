//! Owner-private state. Shared heap/collector storage remains addressed by V3;
//! OwnerContext selects private state. Tagged offsets exist only during Rust
//! emission, and can enter machine code only through runtime address helpers.
use super::*;
use crate::gc_abi::ThreadRegistration;
pub(super) const TAG: usize = 1usize << (usize::BITS - 1);
pub(super) const fn offset(value: usize) -> usize {
    TAG | value
}
pub(super) const fn private(value: usize) -> bool {
    value & TAG != 0
}
pub(super) const fn raw(value: usize) -> usize {
    value & !TAG
}
#[repr(C)]
pub(super) struct OwnerState {
    pub(super) ctx: MutatorContext,
    /// Immutable link to the one heap/collector this owner registered with.
    pub(super) shared_runtime: u64,
    /// Fresh-owner admission retries, release-published for diagnostics.
    pub(super) attach_waits: u64,
    /// This owner's copy/allocation reentrancy guard; not shared phase-open.
    pub(super) collecting: u64,
    pub(super) checkpoint_acks: u64,
    pub(super) phase_round: phase_round::Owner,
    pub(super) checkpoint_journal_before: u64,
    pub(super) checkpoint_journal_after: u64,
    pub(super) mutator_time: mutator_time::State,
    pub(super) rendezvous_time: rendezvous_time::Owner,
    pub(super) native_io: native_io::State,
    pub(super) native_api: crate::gc_abi::NativeApiBinding,
    pub(super) native_api_request: crate::gc_abi::NativeApiCall,
    #[cfg(test)]
    pub(super) rooted_write_hook: u64,
    pub(super) segment: u64,
    pub(super) journal: u64,
    pub(super) cell_sp: u64,
    pub(super) alloc_size: u64,
    /// Arguments retained over collector quanta and resource waits.
    pub(super) fresh_size: u64,
    pub(super) fresh_tag: u64,
    pub(super) allocation_stalls: u64,
    pub(super) maintenance_tick: u64,
    pub(super) adopt_slot: u64,
    pub(super) bind_page: u64,
    pub(super) pending_ref: u64,
    pub(super) pending_page: u64,
    pub(super) pending_cell: u64,
    pub(super) saved_size: u64,
    pub(super) saved_elements: u64,
    pub(super) saved_tag: u64,
    pub(super) saved_descriptor: u64,
    pub(super) alloc_tag: u64,
    pub(super) forward_cell: u64,
    /// Source/destination scratch for this owner's load-barrier copier.
    pub(super) scan_old: u64,
    pub(super) scan_new: u64,
    pub(super) pin_copy_job: u64,
    pub(super) mutator_copies: u64,
    pub(super) mutator_lost: u64,
    /// The leaf allocator alone bumps this owner's one reserved record.
    pub(super) marks: MarkArena,
    pub(super) mark_revision: u64,
    pub(super) request: AllocationRequest,
    pub(super) into: AllocationInto,
    pub(super) result: AllocationResult,
    pub(super) alloc_root: RootRequest,
    pub(super) frame_root: RootRequest,
    pub(super) spec: RootStorageSpec,
    pub(super) access: SourceAccessRequest,
    pub(super) publish: GuardedMarkPrepare,
    /// Native bounce storage, private through blocking I/O.
    pub(super) copy_out: [u8; COPY_OUT_BYTES as usize],
    pub(super) root_journal: root_journal::State,
    pub(super) participant: ThreadRegistration,
    pub(super) large_allocation: large_object::AllocationState,
    pub(super) copy_claim: copy_claims::Owner,
    pub(super) moving_round: moving_round::Owner,
    pub(super) lease_transfer: super::metadata_leases::Owner,
    pub(super) weak: super::weak::Owner,
    pub(super) native_roots: super::native_roots::Owner,
    pub(super) pin_destination: super::pin_destination::Owner,
    pub(super) pin_adopt: super::pin_adopt::Owner,
    pub(super) scoped_pin: super::scoped_pin::Owner,
    pub(super) native_handles: u64,
    pub(super) native_tls: u64,
    pub(super) native_tls_thread: u64,
    pub(super) native_thread_token: u64,
    pub(super) native_tls_prev: u64,
    pub(super) native_tls_next: u64,
    pub(super) detach: detach::Owner,
    // RootStorage must be last: its cells and ring entries follow this prefix.
    pub(super) roots: RootStorage,
}

const _: () =
    assert!(offset_of!(OwnerState, roots) + size_of::<RootStorage>() == size_of::<OwnerState>());

/// Participant already registered; storage is fresh, disjoint and zeroed.
/// Spec describes the trailing RootStorage range, not the owner prefix.
pub(super) fn initialize<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    // Prepared storage belongs to the actual registered participant.
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    store_rt(a, RT_SPEC + offset_of!(RootStorageSpec, owner), R::V0);
    prepare_storage(a, l);
    initialize_prepared(a, l);
}

/// Unpublished native storage only. Clearing a large cell array must not hold
/// the shared collector ticket while existing owners wait at a rendezvous.
fn prepare_storage<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    lea_rt(a, R::V7, owner_state::offset(offset_of!(OwnerState, roots)));
    lea_rt(a, R::V6, RT_SPEC);
    a.call_label(l.root_storage_prepare);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
}

/// The participant is registered and its private storage is already prepared.
/// Publish the actual identity before linking its empty region into inventory.
fn initialize_prepared<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    load_rt(a, R::V0, RT_CTX + offset_of!(MutatorContext, thread_id));
    store_rt(a, RT_SPEC + offset_of!(RootStorageSpec, owner), R::V0);
    store_rt(
        a,
        offset(
            offset_of!(OwnerState, roots)
                + offset_of!(RootStorage, segment)
                + offset_of!(RootSegment, owner),
        ),
        R::V0,
    );
    lea_rt(
        a,
        R::V1,
        owner_state::offset(offset_of!(OwnerState, roots) + offset_of!(RootStorage, segment)),
    );
    store_rt(a, RT_SEGMENT, R::V1);
    registered_roots::startup(a, l);
    load_rt(a, R::V1, RT_SEGMENT);
    store_rt(a, RT_ALLOC_ROOT + offset_of!(RootRequest, segment), R::V1);
    store_rt(a, RT_FRAME_ROOT + offset_of!(RootRequest, segment), R::V1);
    store_rt_addr(
        a,
        RT_JOURNAL,
        owner_state::offset(offset_of!(OwnerState, roots) + offset_of!(RootStorage, journal)),
    );
    // Allocation requests: raw-bytes descriptor, result, mark arena.
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, descriptor),
        RT_BUILTINS + 8,
    );
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, output),
        RT_RESULT,
    );
    store_rt_addr(
        a,
        RT_REQUEST + offset_of!(AllocationRequest, marks),
        allocation_records::OWNER,
    );
    allocation_records::adopt(a, l);
    store_rt_addr(
        a,
        RT_INTO + offset_of!(AllocationInto, allocation),
        RT_REQUEST,
    );
    store_rt_addr(a, RT_INTO + offset_of!(AllocationInto, root), RT_ALLOC_ROOT);
    // Cell 0 is the allocation scratch root; frame roots use cells 1...
    root_step(a, l, RT_ALLOC_ROOT, ROOT_RESERVE);
    native_api::initialize(a, l);
}

/// V3 is the shared runtime; V7 owns fresh zeroed native storage plus its
/// configured trailing cells/ring. No managed reference is an input. Prepare
/// privately, then join the current stable mark/Idle snapshot under the ticket.
/// A fresh owner cannot acknowledge an already-open rendezvous. It waits
/// unregistered, without the ticket, until that round and its resumes finish.
/// Physical relocation/retirement also finishes before admission: its existing
/// destination reserve was funded for the previous set of possible copiers.
/// Returns the owner pointer in V0, preserving the caller's OwnerContext/V3.
pub(super) fn emit_attach<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    a.bind_text_label(l.attach_actor);
    a.push_reg(R::V3);
    a.push_reg(R::OwnerContext);
    a.test_reg_reg(R::V7, R::V7);
    a.jcc_label(C::Equal, l.fail);
    a.mov_reg_reg(R::V0, R::V7);
    a.and_reg_imm32(R::V0, 7);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    a.mov_reg_reg(R::OwnerContext, R::V7);
    load_rt(a, R::V0, RT_CTX);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    // Private preparation needs a nonzero owner word, not a public identity.
    // initialize_prepared replaces it before publishing this empty region.
    store_rt_imm(a, RT_SPEC + offset_of!(RootStorageSpec, owner), 1);
    prepare_storage(a, l);
    let retry = a.create_text_label();
    let busy = a.create_text_label();
    let ready = a.create_text_label();
    a.bind_text_label(retry);
    worker::lock(a, l);
    for field in [
        participants::HANDSHAKE + offset_of!(crate::gc_abi::HandshakeState, active),
        quiescence::REQUEST,
        phase_round::RESUMING,
        RT_PHASE_OPEN,
        RT_RETIRE_STAGE,
    ] {
        load_rt(a, R::V0, field);
        a.test_reg_reg(R::V0, R::V0);
        a.jcc_label(C::NotEqual, busy);
    }
    a.jmp_label(ready);
    a.bind_text_label(busy);
    let waits = offset(offset_of!(OwnerState, attach_waits));
    load_rt(a, R::V0, waits);
    a.add_reg_imm32(R::V0, 1);
    a.jcc_label(C::Equal, l.fail);
    lea_rt(a, R::V1, waits);
    a.atomic_store_release(R::V1, R::V0);
    worker::unlock(a, l);
    if let Some(worker) = &l.worker {
        a.call_label(worker.hooks.relax);
    }
    a.jmp_label(retry);
    a.bind_text_label(ready);
    load_rt(
        a,
        R::V0,
        registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    store_rt(a, RT_CTX + offset_of!(MutatorContext, adopted_phase), R::V0);
    store_rt(a, offset(offset_of!(OwnerState, shared_runtime)), R::V3);
    store_rt_imm(a, RT_CTX, u64::from(ABI_VERSION));
    participants::startup(a, l);
    initialize_prepared(a, l);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.mov_reg_reg(R::V0, R::OwnerContext);
    a.pop_reg(R::OwnerContext);
    a.pop_reg(R::V3);
    a.ret();
}

const _: () = assert!(offset_of!(OwnerState, ctx) == 0);

/// Select an explicit owner's immutable heap without a safepoint. Preserve
/// every register except V3 and flags: raw input values must survive binding.
/// A cached-phase check is not the future concurrent admission protocol.
pub(super) fn bind_running<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    bind_state(a, l, 0, true);
}
pub(super) fn bind_detaching<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    bind_state(a, l, 3, false);
}

/// Native return validates/adopts its snapshot under the collector ticket.
pub(super) fn bind_native_return<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    bind_state(a, l, 2, false);
}
fn bind_state<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, state: u64, phase: bool) {
    a.push_reg(R::V0);
    a.push_reg(R::V1);
    load_rt(a, R::V0, RT_CTX);
    a.mov_imm64(R::V1, u64::from(ABI_VERSION) | (state << 32));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V3, offset(offset_of!(OwnerState, shared_runtime)));
    a.test_reg_reg(R::V3, R::V3);
    a.jcc_label(C::Equal, l.fail);
    // A copied/stale context is not the registered participant merely because
    // its ABI word and heap link were copied too. Check the real binding.
    load_rt(
        a,
        R::V0,
        participants::OWNER + offset_of!(ThreadRegistration, registered),
    );
    a.cmp_reg_imm32(R::V0, 1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V0,
        participants::OWNER + offset_of!(ThreadRegistration, context),
    );
    a.cmp_reg_reg(R::V0, R::OwnerContext);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V0,
        participants::OWNER + offset_of!(ThreadRegistration, registry),
    );
    lea_rt(a, R::V1, participants::HANDSHAKE);
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(
        a,
        R::V0,
        participants::OWNER + offset_of!(ThreadRegistration, identity),
    );
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, l.fail);
    load_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, thread_id));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::NotEqual, l.fail);
    if phase {
        lea_rt(a, R::V1, RT_CTX + offset_of!(MutatorContext, adopted_phase));
        a.atomic_load_acquire(R::V0, R::V1);
        lea_rt(
            a,
            R::V1,
            registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
        );
        a.atomic_load_acquire(R::V1, R::V1);
        a.cmp_reg_reg(R::V0, R::V1);
        a.jcc_label(C::NotEqual, l.fail);
    }
    a.pop_reg(R::V1);
    a.pop_reg(R::V0);
}
