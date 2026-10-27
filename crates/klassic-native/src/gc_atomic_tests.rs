//! Native execution tests for emitted atomics. This uses C only as an external
//! conformance driver; the direct compiler's build path never invokes it.
use crate::portable_asm::{PortableAsm, Reg as P};
use crate::{NativeCodeGenerator, SourceFile, TargetPlatform};

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[path = "gc_native_contract_tests.rs"]
mod native_contract;

#[path = "gc_native_frame_tests.rs"]
mod native_frames;

pub(super) fn generator() -> NativeCodeGenerator {
    let mut generator = NativeCodeGenerator::new(
        SourceFile::new("<gc-atomics>", ""),
        None,
        TargetPlatform::for_target(crate::NativeTarget::LinuxX86_64),
    );
    // No startup/runtime code is needed for these leaf functions.
    generator.asm = crate::Assembler::new();
    generator
}

#[test]
fn gc_atomic_x86_cas_encodes_lock_and_extended_registers() {
    let mut g = generator();
    g.atomic_compare_exchange(P::V10, P::V9);
    assert_eq!(g.asm.text, [0xf0, 0x4d, 0x0f, 0xb1, 0x8a, 0, 0, 0, 0]);
}

#[test]
fn gc_atomic_x86_acquire_release_and_fence_encodings() {
    let mut g = generator();
    g.atomic_load_acquire(P::V0, P::V7);
    g.atomic_store_release(P::V7, P::V2);
    g.atomic_fence();
    assert_eq!(
        g.asm.text,
        [
            0x48, 0x8b, 0x87, 0, 0, 0, 0, 0x48, 0x89, 0x97, 0, 0, 0, 0, 0x0f, 0xae, 0xf0,
        ]
    );
}

#[test]
fn gc_atomic_u32_x86_encodings_preserve_width() {
    let mut g = generator();
    g.atomic_load_acquire_u32(P::V0, P::V7);
    g.atomic_store_release_u32(P::V7, P::V2);
    g.atomic_compare_exchange_u32(P::V10, P::V9);
    assert_eq!(
        g.asm.text,
        [
            0x8b, 0x87, 0, 0, 0, 0, 0x89, 0x97, 0, 0, 0, 0, 0xf0, 0x45, 0x0f, 0xb1, 0x8a, 0, 0, 0,
            0, 0x89, 0xc0,
        ]
    );
    let mut g = generator();
    g.asm
        .store_u32_ptr_disp32(crate::Reg::R12, 4, crate::Reg::R9);
    assert_eq!(g.asm.text, [0x45, 0x89, 0x8c, 0x24, 4, 0, 0, 0]);
}

#[derive(Clone, Copy)]
pub(super) enum Operation {
    Load,
    Store,
    Cas,
    FetchOr,
    Fence,
    Load32,
    Store32,
    Cas32,
    ContextTransition,
    HandshakeStep,
    TlabAllocate,
    PoolStep,
    TlabPage,
    DescriptorRegister,
    DescriptorScan,
    RootStep,
    RootScan,
    RootDrain,
    RootLife,
    RootClear,
    RootScanLeased,
    RootWalk,
    RootWalkScan,
    RootWalkHeal,
    RootWalkItem,
    RelocationGate,
    PinPage,
    PinRegistryBind,
    PinRegistryUnbind,
    PinRegistryPin,
    PinRegistryUnpin,
    PinRegistrySelect,
    PinRegistryReopen,
    PinSelection,
    PinRelocationClaim,
    PinSourceDrain,
    PinSourceUncommit,
    RootPinPrepare,
    RootPinEnter,
    RootPinExit,
    RootPinCleanup,
    RootPinCancel,
    RootPinFinish,
    PinDispatch,
    NativePinPublish,
    NativePinPrepare,
    NativePinRetire,
    NativePinDispose,
    NativePinRelease,
    NativePinClose,
    NativePinProgress,
    NativePinWait,
    NativeSourceUncommit,
    RootFinishNotify,
    RelocationJob,
    RelocationComplete,
    RelocationClaim,
    MarkedRelocation,
    RelocateDiscard,
    RelocationCancel,
    AllocInto,
    DetachStep,
    BufferStep,
    BufferDrain,
    MetadataStep,
    BudgetStep,
    ReferenceLease,
    ForwardStep,
    SourceAccess,
    ForwardLookup,
    ResolveAccess,
    RelocateCopy,
    RelocatePublish,
    RelocateTarget,
    PageRelease,
    GuardedStore,
    GuardedObserve,
    GuardedIdentity,
    GuardedBlockSize,
    GuardedLogicalCas,
    GuardedLoad,
    GuardedScalar,
    GuardedHeal,
    RootFollow,
    PendingFollow,
    PendingAccess,
    PendingMark,
    WeakLoad,
    WeakRelease,
    WeakFollow,
    WeakIncoming,
    WeakTableAccess,
    WeakCreate,
    WeakMark,
    WeakCapture,
    WeakAccess,
    WeakFinish,
    WeakScan,
    WeakRelocation,
    WeakLiveness,
    SurvivorAge,
    MarkedSurvivorAge,
    PromotionRemember,
    RelocationTurn,
    ForwardCompress,
    HeapIncoming,
    HeapWalk,
    ObjectWalk,
    HeapAuditSlice,
    HeapAudit,
    RootedObservation,
    RootedScalar,
    RootedMark,
    RootedMarkSpill,
    RootedMarkSet,
    RootedMarkPublish,
    RootedLoad,
    RootedValue,
    RootedStoreCommit,
    RootedStoreDispatch,
    RootedLoadDispatch,
    BarrierPoll,
    BarrierAssist,
    NativeRootPublish,
    RootedStore,
    RootedStoreAttempt,
    RootedLoadAdmit,
    RootedLoadHeal,
    RootAccessCleanup,
    RootAccess,
    MarkPrepare,
    MarkRetarget,
    WorkerStep,
    RootJournal,
    BufferJob,
    RootAudit,
    RootRetireAudit,
    MarkEnd,
    RetireInventory,
    MarkPhase,
    CycleStep,
    RememberedStep,
    RememberedInventory,
    RememberedLoad,
    RememberedTransfer,
    RememberedOwner,
    RememberedWalk,
    RememberedLease,
    MarkRoute,
    Initialization,
    InitializationStore,
    InitializationCopy,
    MetadataGrow,
    BuiltinPrepare,
    RootStoragePrepare,
    RootStorageCanDispose,
    AllocationShape,
    AllocationStart,
    NativeStartup,
    NativeStartupRelease,
    NativeStartupCommit,
    AllocationSample,
    AllocationPressure,
    HeapAllocationPressure,
    CyclePressure,
    CycleMarkStart,
    RelocationPhase,
    RelocationRetire,
    RootCancelEmpty,
    RootCopy,
    MarkStep,
    MarkSpill,
    MarkScan,
    MarkScanCommit,
    SourceDrain,
    SourceUncommit,
    SourceRetire,
    ForwardIncoming,
}

pub(super) fn emit_body<E: PortableAsm>(out: &mut E, operation: Operation) {
    match operation {
        Operation::Load => out.atomic_load_acquire(P::V0, P::V7),
        Operation::Store => {
            out.atomic_store_release(P::V7, P::V6);
            out.mov_imm64(P::V0, 0);
        }
        Operation::Cas => {
            out.mov_reg_reg(P::V0, P::V6);
            out.atomic_compare_exchange(P::V7, P::V2);
        }
        Operation::FetchOr => out.atomic_fetch_or(P::V7, P::V6),
        Operation::Fence => {
            out.atomic_fence();
            out.mov_imm64(P::V0, 0);
        }
        Operation::Load32 => out.atomic_load_acquire_u32(P::V0, P::V7),
        Operation::Store32 => {
            out.atomic_store_release_u32(P::V7, P::V6);
            out.mov_imm64(P::V0, 0);
        }
        Operation::Cas32 => {
            out.mov_reg_reg(P::V0, P::V6);
            out.atomic_compare_exchange_u32(P::V7, P::V2);
        }
        Operation::ContextTransition => crate::gc_codegen::context::emit_transition_locked(out),
        Operation::HandshakeStep => crate::gc_codegen::handshake::emit_step_locked(out),
        Operation::TlabAllocate => crate::gc_codegen::heap::emit_try_tlab_allocate(out),
        Operation::PoolStep => crate::gc_codegen::pages::emit_pool_step_locked(out),
        Operation::TlabPage => crate::gc_codegen::heap::emit_tlab_page_locked(out),
        Operation::DescriptorRegister => crate::gc_codegen::descriptors::emit_register(out),
        Operation::DescriptorScan => crate::gc_codegen::descriptors::emit_scan(out),
        Operation::RootStep => crate::gc_codegen::roots::emit_step(out),
        Operation::RootScan => crate::gc_codegen::roots::emit_transfer(out, false),
        Operation::RootDrain => crate::gc_codegen::roots::emit_transfer(out, true),
        Operation::RootLife => crate::gc_codegen::root_lifetime::emit_life_locked(out),
        Operation::RootClear => crate::gc_codegen::root_lifetime::emit_clear_chunk(out),
        Operation::RootScanLeased => crate::gc_codegen::root_lifetime::emit_scan_leased(out),
        Operation::RootWalk => crate::gc_codegen::root_lifetime::walk::emit_walk_locked(out),
        Operation::ForwardIncoming => {
            crate::gc_codegen::root_lifetime::forward_incoming::emit_forward_incoming(out)
        }
        Operation::SourceRetire => {
            crate::gc_codegen::root_lifetime::source_retire::emit_source_retire(out)
        }
        Operation::SourceUncommit => {
            crate::gc_codegen::root_lifetime::source_uncommit::emit_source_uncommit(out)
        }
        Operation::SourceDrain => {
            crate::gc_codegen::root_lifetime::source_drain::emit_source_drain(out)
        }
        Operation::MarkScanCommit => crate::gc_codegen::root_lifetime::mark_scan_commit::emit(out),
        Operation::MarkScan => crate::gc_codegen::root_lifetime::mark_scan::emit_mark_scan(out),
        Operation::MarkSpill => crate::gc_codegen::root_lifetime::mark_spill::emit_mark_spill(out),
        Operation::MarkStep => crate::gc_codegen::root_lifetime::mark::emit_mark_step(out),
        Operation::RootJournal => {
            crate::gc_codegen::root_lifetime::root_journal::emit_root_journal(out)
        }
        Operation::BufferJob => crate::gc_codegen::root_lifetime::buffer_job::emit_buffer_job(out),
        Operation::RememberedInventory => {
            crate::gc_codegen::root_lifetime::remembered_inventory::emit_inventory(out)
        }
        Operation::RememberedStep => {
            crate::gc_codegen::root_lifetime::remembered::emit_remembered(out)
        }
        Operation::CycleStep => crate::gc_codegen::root_lifetime::cycle::emit_cycle_step(out),
        Operation::MarkPhase => crate::gc_codegen::root_lifetime::mark_phase::emit_mark_phase(out),
        Operation::RetireInventory => {
            crate::gc_codegen::root_lifetime::mark_end::emit_retire_inventory(out)
        }
        Operation::MarkEnd => crate::gc_codegen::root_lifetime::mark_end::emit_mark_end(out),
        Operation::RootRetireAudit => {
            crate::gc_codegen::root_lifetime::root_audit::emit_root_retire_audit(out)
        }
        Operation::RootAudit => crate::gc_codegen::root_lifetime::root_audit::emit_root_audit(out),
        Operation::WorkerStep => crate::gc_codegen::root_lifetime::worker::emit_worker_step(out),
        Operation::MarkRetarget => {
            crate::gc_codegen::root_lifetime::mark_retarget::emit_mark_retarget(out)
        }
        Operation::MarkPrepare => {
            crate::gc_codegen::root_lifetime::mark_prepare::emit_mark_prepare(out)
        }
        Operation::RootedMark => crate::gc_codegen::root_lifetime::rooted_mark::emit(out, false),
        Operation::RootedMarkPublish => {
            crate::gc_codegen::root_lifetime::rooted_mark_publish::emit(out)
        }
        Operation::RootedLoadHeal => crate::gc_codegen::root_lifetime::rooted_load_heal::emit(out),
        Operation::RootedLoadAdmit => {
            crate::gc_codegen::root_lifetime::rooted_load_admit::emit(out)
        }
        Operation::NativeRootPublish => {
            crate::gc_codegen::root_lifetime::native_root_publish::emit(out)
        }
        Operation::BarrierAssist => crate::gc_codegen::root_lifetime::barrier_assist::emit(out),
        Operation::BarrierPoll => crate::gc_codegen::root_lifetime::barrier_poll::emit(out),
        Operation::RootedLoadDispatch => {
            crate::gc_codegen::root_lifetime::rooted_store_dispatch::emit_load(out)
        }
        Operation::RootedStoreDispatch => {
            crate::gc_codegen::root_lifetime::rooted_store_dispatch::emit(out)
        }
        Operation::RootedStore => crate::gc_codegen::root_lifetime::rooted_store::emit(out),
        Operation::RootedStoreAttempt => {
            crate::gc_codegen::root_lifetime::rooted_store_attempt::emit(out)
        }
        Operation::RootedStoreCommit => {
            crate::gc_codegen::root_lifetime::rooted_store_commit::emit(out)
        }
        Operation::RootedValue => crate::gc_codegen::root_lifetime::rooted_load::emit_value(out),
        Operation::RootedLoad => crate::gc_codegen::root_lifetime::rooted_load::emit(out),
        Operation::RootedMarkSet => crate::gc_codegen::root_lifetime::rooted_mark_set::emit(out),
        Operation::RootedMarkSpill => {
            crate::gc_codegen::root_lifetime::rooted_mark::emit(out, true)
        }
        Operation::RootedScalar => crate::gc_codegen::root_lifetime::rooted_scalar::emit(out),
        Operation::RootedObservation => {
            crate::gc_codegen::root_lifetime::rooted_observation::emit(out)
        }
        Operation::HeapAudit => crate::gc_codegen::root_lifetime::heap_audit::emit(out),
        Operation::HeapAuditSlice => crate::gc_codegen::root_lifetime::heap_audit_slice::emit(out),
        Operation::ObjectWalk => crate::gc_codegen::root_lifetime::object_walk::emit(out),
        Operation::HeapWalk => crate::gc_codegen::root_lifetime::heap_walk::emit(out),
        Operation::HeapIncoming => crate::gc_codegen::root_lifetime::heap_incoming::emit(out),
        Operation::ForwardCompress => crate::gc_codegen::root_lifetime::forward_compress::emit(out),
        Operation::RelocationTurn => crate::gc_codegen::root_lifetime::relocation_turn::emit(out),
        Operation::PromotionRemember => {
            crate::gc_codegen::root_lifetime::promotion_remember::emit(out)
        }
        Operation::MarkedSurvivorAge => {
            crate::gc_codegen::root_lifetime::survivor_age::emit_marked_age(out)
        }
        Operation::SurvivorAge => crate::gc_codegen::root_lifetime::survivor_age::emit_age(out),
        Operation::RootAccessCleanup => {
            crate::gc_codegen::root_lifetime::root_access::emit_cleanup(out)
        }
        Operation::RootAccess => {
            crate::gc_codegen::root_lifetime::root_access::emit_root_access(out)
        }
        Operation::WeakLiveness => crate::gc_codegen::root_lifetime::weak_liveness::emit(out),
        Operation::WeakRelocation => crate::gc_codegen::root_lifetime::weak_relocation::emit(out),
        Operation::WeakScan => crate::gc_codegen::root_lifetime::weak_scan::emit(out),
        Operation::WeakFinish => crate::gc_codegen::root_lifetime::weak_finish::emit(out),
        Operation::WeakAccess => crate::gc_codegen::root_lifetime::weak_access::emit(out),
        Operation::WeakCapture => crate::gc_codegen::root_lifetime::weak_capture::emit(out),
        Operation::WeakMark => crate::gc_codegen::root_lifetime::weak_mark::emit(out),
        Operation::WeakCreate => crate::gc_codegen::root_lifetime::weak_create::emit(out),
        Operation::WeakTableAccess => {
            crate::gc_codegen::root_lifetime::weak_table_access::emit(out)
        }
        Operation::WeakIncoming => crate::gc_codegen::root_lifetime::weak_incoming::emit(out),
        Operation::WeakFollow => crate::gc_codegen::root_lifetime::weak_follow::emit(out),
        Operation::WeakRelease => crate::gc_codegen::root_lifetime::weak_release::emit(out),
        Operation::WeakLoad => crate::gc_codegen::root_lifetime::weak_load::emit(out),
        Operation::PendingMark => crate::gc_codegen::root_lifetime::pending_mark::emit(out),
        Operation::PendingAccess => crate::gc_codegen::root_lifetime::pending_access::emit(out),
        Operation::PendingFollow => crate::gc_codegen::root_lifetime::pending_follow::emit(out),
        Operation::RootFollow => {
            crate::gc_codegen::root_lifetime::root_follow::emit_root_follow(out)
        }
        Operation::RememberedLoad => {
            crate::gc_codegen::root_lifetime::remembered_load::emit_load(out)
        }
        Operation::RememberedTransfer => {
            crate::gc_codegen::root_lifetime::remembered_transfer::emit_transfer(out)
        }
        Operation::RememberedLease => {
            crate::gc_codegen::root_lifetime::remembered_lease::emit_lease(out)
        }
        Operation::RememberedWalk => {
            crate::gc_codegen::root_lifetime::remembered_walk::emit_walk(out)
        }
        Operation::RememberedOwner => {
            crate::gc_codegen::root_lifetime::remembered_owner::emit_owner(out)
        }
        Operation::RootCopy => crate::gc_codegen::root_lifetime::root_copy::emit(out),
        Operation::RootCancelEmpty => crate::gc_codegen::roots::emit_cancel_empty(out),
        Operation::AllocationShape => crate::gc_codegen::descriptors::emit_allocation_shape(out),
        Operation::RootStorageCanDispose => crate::gc_codegen::root_storage::emit_can_dispose(out),
        Operation::RootStoragePrepare => crate::gc_codegen::root_storage::emit_prepare(out),
        Operation::BuiltinPrepare => crate::gc_codegen::descriptors::emit_builtin_prepare(out),
        Operation::MetadataGrow => crate::gc_codegen::root_lifetime::metadata_grow::emit_grow(out),
        Operation::InitializationCopy => {
            crate::gc_codegen::root_lifetime::initialization::emit_copy(out)
        }
        Operation::InitializationStore => {
            crate::gc_codegen::root_lifetime::initialization::emit_store(out)
        }
        Operation::Initialization => {
            crate::gc_codegen::root_lifetime::initialization::emit_publish(out)
        }
        Operation::MarkRoute => crate::gc_codegen::root_lifetime::mark_route::emit_route(out),
        Operation::GuardedHeal => crate::gc_codegen::root_lifetime::barrier::emit_guarded_heal(out),
        Operation::GuardedScalar => {
            crate::gc_codegen::root_lifetime::barrier::emit_guarded_scalar(out)
        }
        Operation::GuardedLoad => crate::gc_codegen::root_lifetime::barrier::emit_guarded_load(out),
        Operation::GuardedObserve => {
            crate::gc_codegen::root_lifetime::barrier::emit_guarded_observe(out)
        }
        Operation::GuardedBlockSize => {
            crate::gc_codegen::root_lifetime::barrier::emit_guarded_block_size(out)
        }
        Operation::GuardedIdentity => {
            crate::gc_codegen::root_lifetime::barrier::emit_guarded_identity(out)
        }
        Operation::GuardedLogicalCas => {
            crate::gc_codegen::root_lifetime::barrier::emit_logical_cas(out)
        }
        Operation::GuardedStore => {
            crate::gc_codegen::root_lifetime::barrier::emit_guarded_store(out)
        }
        Operation::PageRelease => {
            crate::gc_codegen::root_lifetime::page_release::emit_page_release(out)
        }
        Operation::RelocateTarget => {
            crate::gc_codegen::root_lifetime::relocate::emit_relocate_target(out)
        }
        Operation::RelocatePublish => {
            crate::gc_codegen::root_lifetime::relocate::emit_relocate_publish(out)
        }
        Operation::RelocateCopy => {
            crate::gc_codegen::root_lifetime::relocate::emit_relocate_copy(out)
        }
        Operation::ResolveAccess => {
            crate::gc_codegen::root_lifetime::resolve::emit_resolve_access(out)
        }
        Operation::ForwardLookup => {
            crate::gc_codegen::root_lifetime::forward_table::emit_forward_lookup(out)
        }
        Operation::SourceAccess => {
            crate::gc_codegen::root_lifetime::source_access::emit_source_access(out)
        }
        Operation::ForwardStep => crate::gc_codegen::root_lifetime::forward::emit_forward_step(out),
        Operation::ReferenceLease => {
            crate::gc_codegen::root_lifetime::reference_lease::emit_reference_lease(out)
        }
        Operation::BudgetStep => crate::gc_codegen::root_lifetime::budget::emit_budget_step(out),
        Operation::MetadataStep => {
            crate::gc_codegen::root_lifetime::metadata::emit_metadata_step(out)
        }
        Operation::BufferStep => crate::gc_codegen::root_lifetime::buffers::emit_buffer_step(out),
        Operation::BufferDrain => crate::gc_codegen::root_lifetime::buffers::emit_buffer_drain(out),
        Operation::DetachStep => crate::gc_codegen::root_lifetime::detach::emit_detach_step(out),
        Operation::RelocationRetire => {
            crate::gc_codegen::root_lifetime::relocation_phase::emit_retire(out)
        }
        Operation::RelocationPhase => crate::gc_codegen::root_lifetime::relocation_phase::emit(out),
        Operation::CycleMarkStart => crate::gc_codegen::root_lifetime::cycle_mark_start::emit(out),
        Operation::CyclePressure => crate::gc_codegen::root_lifetime::cycle_pressure::emit(out),
        Operation::HeapAllocationPressure => {
            crate::gc_codegen::root_lifetime::heap_pressure::emit(out)
        }
        Operation::AllocationPressure => {
            crate::gc_codegen::root_lifetime::allocation_sample::emit_pressure(out)
        }
        Operation::AllocationSample => {
            crate::gc_codegen::root_lifetime::allocation_sample::emit_sample(out)
        }
        Operation::NativeStartupRelease => {
            crate::gc_codegen::root_lifetime::native_startup::emit_release(out)
        }
        Operation::NativeStartupCommit => {
            crate::gc_codegen::root_lifetime::native_startup::emit_commit(out)
        }
        Operation::NativeStartup => {
            crate::gc_codegen::root_lifetime::native_startup::emit_reserve(out)
        }
        Operation::AllocationStart => crate::gc_codegen::alloc_start::emit_start(out),
        Operation::AllocInto => crate::gc_codegen::root_lifetime::alloc_into::emit_alloc_into(out),
        Operation::RelocationCancel => {
            crate::gc_codegen::root_lifetime::relocation_cancel::emit_relocation_cancel(out)
        }
        Operation::RelocateDiscard => {
            crate::gc_codegen::root_lifetime::relocate_discard::emit_relocate_discard(out)
        }
        Operation::MarkedRelocation => {
            crate::gc_codegen::root_lifetime::relocation_claim::emit_marked_relocation_claim(out)
        }
        Operation::RelocationClaim => {
            crate::gc_codegen::root_lifetime::relocation_claim::emit_relocation_claim(out)
        }
        Operation::RelocationComplete => {
            crate::gc_codegen::root_lifetime::relocation_complete::emit_relocation_complete(out)
        }
        Operation::RelocationJob => {
            crate::gc_codegen::root_lifetime::relocation_job::emit_relocation_job(out)
        }
        Operation::PinRegistryBind => {
            crate::gc_codegen::root_lifetime::pin_registry::emit_bind(out)
        }
        Operation::PinRegistryUnbind => {
            crate::gc_codegen::root_lifetime::pin_registry::emit_unbind(out)
        }
        Operation::PinRegistryPin => {
            crate::gc_codegen::root_lifetime::pin_registry::emit_step(out, 0)
        }
        Operation::PinRegistryUnpin => {
            crate::gc_codegen::root_lifetime::pin_registry::emit_step(out, 1)
        }
        Operation::PinRegistrySelect => {
            crate::gc_codegen::root_lifetime::pin_registry::emit_step(out, 2)
        }
        Operation::PinRegistryReopen => {
            crate::gc_codegen::root_lifetime::pin_registry::emit_step(out, 3)
        }
        Operation::PinSelection => {
            crate::gc_codegen::root_lifetime::pin_registry::emit_selection(out)
        }
        Operation::PinRelocationClaim => {
            crate::gc_codegen::root_lifetime::pin_relocation::emit_claim(out)
        }
        Operation::PinSourceDrain => {
            crate::gc_codegen::root_lifetime::pin_source_release::emit_drain(out)
        }
        Operation::PinSourceUncommit => {
            crate::gc_codegen::root_lifetime::pin_source_release::emit_uncommit(out)
        }
        Operation::RootPinPrepare => crate::gc_codegen::root_lifetime::root_pin::emit_prepare(out),
        Operation::RootPinEnter => crate::gc_codegen::root_lifetime::root_pin::emit_step(out, 0),
        Operation::RootPinExit => crate::gc_codegen::root_lifetime::root_pin::emit_step(out, 1),
        Operation::RootPinCleanup => crate::gc_codegen::root_lifetime::root_pin::emit_step(out, 2),
        Operation::RootPinCancel => crate::gc_codegen::root_lifetime::root_pin::emit_step(out, 3),
        Operation::RootPinFinish => crate::gc_codegen::root_lifetime::root_pin::emit_step(out, 4),
        Operation::PinDispatch => crate::gc_codegen::root_lifetime::pin_dispatch::emit(out),
        Operation::NativePinPublish => {
            crate::gc_codegen::root_lifetime::native_pin::emit_publish(out)
        }
        Operation::NativePinPrepare => {
            crate::gc_codegen::root_lifetime::native_pin::emit_prepare(out)
        }
        Operation::NativePinRetire => {
            crate::gc_codegen::root_lifetime::native_pin::emit_retire(out)
        }
        Operation::RootFinishNotify => {
            crate::gc_codegen::root_lifetime::root_finish_notify::emit(out)
        }
        Operation::NativeSourceUncommit => {
            crate::gc_codegen::root_lifetime::native_source_uncommit::emit(out)
        }
        Operation::NativePinWait => crate::gc_codegen::root_lifetime::native_pin_wait::emit(out),
        Operation::NativePinProgress => {
            crate::gc_codegen::root_lifetime::native_pin_progress::emit(out)
        }
        Operation::NativePinClose => crate::gc_codegen::root_lifetime::native_pin_close::emit(out),
        Operation::NativePinRelease => {
            crate::gc_codegen::root_lifetime::native_pin::emit_release(out)
        }
        Operation::NativePinDispose => {
            crate::gc_codegen::root_lifetime::native_pin::emit_dispose(out)
        }
        Operation::PinPage => crate::gc_codegen::root_lifetime::relocation_gate::emit_pin_page(out),
        Operation::RelocationGate => {
            crate::gc_codegen::root_lifetime::relocation_gate::emit_relocation_gate(out)
        }
        Operation::RootWalkItem => crate::gc_codegen::root_lifetime::walk::emit_walk_item(out),
        Operation::RootWalkHeal => crate::gc_codegen::root_lifetime::walk::emit_walk_heal(out),
        Operation::RootWalkScan => crate::gc_codegen::root_lifetime::walk::emit_walk_scan(out),
    }
    // The wrapper emits its own ABI epilogue on Windows.
}

fn x86_code(operation: Operation, windows: bool) -> Vec<u8> {
    let mut g = generator();
    // Callback calls inside emitted routines need the host ABI too, not only
    // the entry wrapper's argument-register conversion.
    g.is_windows = windows;
    if windows {
        let body = g.create_text_label();
        g.push_reg(P::V7); // Win64 rdi/rsi are nonvolatile
        g.push_reg(P::V6);
        g.sub_reg_imm8(P::V4, 8); // align the internal call
        g.mov_reg_reg(P::V7, P::V1); // rcx -> rdi (address)
        g.mov_reg_reg(P::V6, P::V2); // rdx -> rsi (expected/value)
        g.mov_reg_reg(P::V2, P::V8); // r8 -> rdx (desired)
        // Some emitters contain their own RETs and nested routines. Call the
        // body so every return reaches the register-restoring outer wrapper.
        g.call_label(body);
        g.add_reg_imm32(P::V4, 8);
        g.pop_reg(P::V6);
        g.pop_reg(P::V7);
        g.ret();
        g.bind_text_label(body);
    }
    emit_body(&mut g, operation);
    g.ret();
    finish_code(g)
}

#[cfg(target_arch = "x86_64")]
#[test]
fn gc_atomic_windows_nested_returns_restore_entry_registers() {
    let bytes = x86_code(Operation::RootCopy, true)
        .into_iter()
        .map(|byte| byte.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let header = format!("static const unsigned char code[] = {{{bytes}}};\n");
    let output = run_c_driver("gc_win_return_code.h", &header, "win_return_harness.c");
    assert!(output.contains("Win64 nested returns restore entry registers PASS"));
}

fn finish_code(g: NativeCodeGenerator) -> Vec<u8> {
    let mut object = g.asm.finish();
    for relocation in &object.relocs {
        assert_eq!(relocation.kind, crate::RelocKind::Rel32);
        let crate::RelocTarget::Text(target) = relocation.target else {
            panic!("leaf primitive has external relocation")
        };
        let delta =
            i32::try_from(object.text_labels[target.0] as i64 - (relocation.pos + 4) as i64)
                .unwrap();
        object.text[relocation.pos..relocation.pos + 4].copy_from_slice(&delta.to_le_bytes());
    }
    object.text
}

#[path = "gc_linux_handshake_tests.rs"]
mod linux_handshake;

// Package the production Win64 entries with a local IAT. The C owner binds
// kernel32 addresses before changing the mapping from writable to executable.
pub(crate) fn append_windows_metadata_header(header: &mut String) {
    use crate::{ImportSymbol as I, RelocKind, RelocTarget};
    use std::fmt::Write;
    let mut g = generator();
    let map = g.asm.create_text_label();
    let unmap = g.asm.create_text_label();
    let roots_prepare = g.asm.create_text_label();
    let roots_dispose = g.asm.create_text_label();
    let builtin_registry_prepare = g.asm.create_text_label();
    let startup_prepare = g.asm.create_text_label();
    g.asm.bind_text_label(map);
    crate::gc_codegen::native_metadata::emit_windows_map(&mut g);
    g.asm.bind_text_label(unmap);
    crate::gc_codegen::native_metadata::emit_windows_unmap(&mut g);
    g.asm.bind_text_label(roots_prepare);
    crate::gc_codegen::native_metadata::emit_windows_roots_prepare(&mut g);
    g.asm.bind_text_label(roots_dispose);
    crate::gc_codegen::native_metadata::emit_windows_roots_dispose(&mut g);
    g.asm.bind_text_label(builtin_registry_prepare);
    crate::gc_codegen::native_metadata::emit_windows_builtin_registry_prepare(&mut g);
    g.asm.bind_text_label(startup_prepare);
    crate::gc_codegen::native_metadata::emit_windows_startup_prepare(&mut g);
    let mut object = g.asm.finish();
    let iat = object.text.len().next_multiple_of(8);
    object.text.resize(iat + 16, 0);
    let mut imports = [false; 2];
    for relocation in &object.relocs {
        assert_eq!(relocation.kind, RelocKind::Rel32);
        let target = match relocation.target {
            RelocTarget::Text(label) => object.text_labels[label.0],
            RelocTarget::Import(symbol) => {
                let index = match symbol {
                    I::VirtualAlloc => 0,
                    I::VirtualFree => 1,
                    _ => panic!("unexpected metadata import"),
                };
                imports[index] = true;
                iat + 8 * index
            }
            _ => panic!("unexpected metadata relocation"),
        };
        let delta = i32::try_from(target as i64 - (relocation.pos + 4) as i64).unwrap();
        object.text[relocation.pos..relocation.pos + 4].copy_from_slice(&delta.to_le_bytes());
    }
    assert_eq!(imports, [true, true]);
    header.push_str("#if defined(_WIN32) || defined(GC_MODEL_WIN_METADATA)\nstatic const unsigned char gc_win_metadata_code[] = {");
    for byte in object.text {
        write!(header, "{byte},").unwrap();
    }
    header.push_str("};\n");
    for (name, offset) in [
        ("MAP", object.text_labels[map.0]),
        ("UNMAP", object.text_labels[unmap.0]),
        ("ROOTS_PREPARE", object.text_labels[roots_prepare.0]),
        ("ROOTS_DISPOSE", object.text_labels[roots_dispose.0]),
        (
            "BUILTIN_REGISTRY_PREPARE",
            object.text_labels[builtin_registry_prepare.0],
        ),
        ("STARTUP_PREPARE", object.text_labels[startup_prepare.0]),
        ("ALLOC_IMPORT", iat),
        ("FREE_IMPORT", iat + 8),
    ] {
        writeln!(header, "#define GC_WIN_METADATA_{name} {offset}").unwrap();
    }
    header.push_str("#endif\n");
}

fn append_darwin_metadata_header(header: &mut String) {
    use std::fmt::Write;
    let (bytes, offsets) = crate::aarch64::gc_metadata_test_payload();
    header.push_str("#if defined(__APPLE__) && defined(__aarch64__)\nstatic const unsigned char gc_darwin_metadata_code[] = {");
    for byte in bytes {
        write!(header, "{byte},").unwrap();
    }
    header.push_str("};\n");
    for (name, offset) in [
        "MAP",
        "UNMAP",
        "MAP_IMPORT",
        "UNMAP_IMPORT",
        "ROOTS_PREPARE",
        "ROOTS_DISPOSE",
        "BUILTIN_REGISTRY_PREPARE",
        "STARTUP_PREPARE",
    ]
    .into_iter()
    .zip(offsets)
    {
        writeln!(header, "#define GC_DARWIN_METADATA_{name} {offset}").unwrap();
    }
    header.push_str("#endif\n");
}

fn linux_worker_code() -> Vec<(&'static str, Vec<u8>)> {
    use crate::gc_codegen::threads;
    use crate::{Condition, Reg};
    let mut result = Vec::new();
    for (name, emit) in [
        (
            "worker_start",
            threads::emit_linux_worker_start as fn(&mut NativeCodeGenerator),
        ),
        (
            "metadata_map",
            crate::gc_codegen::native_metadata::emit_linux_map,
        ),
        (
            "metadata_unmap",
            crate::gc_codegen::native_metadata::emit_linux_unmap,
        ),
        (
            "heap_decommit",
            crate::gc_codegen::native_metadata::emit_linux_decommit,
        ),
        (
            "native_roots_prepare",
            crate::gc_codegen::native_metadata::emit_linux_roots_prepare,
        ),
        (
            "native_roots_dispose",
            crate::gc_codegen::native_metadata::emit_linux_roots_dispose,
        ),
        (
            "native_builtin_registry_prepare",
            crate::gc_codegen::native_metadata::emit_linux_builtin_registry_prepare,
        ),
        (
            "native_startup_prepare",
            crate::gc_codegen::native_metadata::emit_linux_startup_prepare,
        ),
        ("worker_join", threads::emit_linux_worker_join),
        ("worker_fail", threads::emit_linux_worker_start_failure),
        ("stack_alloc", threads::emit_linux_stack_alloc),
        ("stack_free", threads::emit_linux_stack_free),
        ("stack_map_fail", threads::emit_linux_stack_map_failure),
        (
            "stack_protect_fail",
            threads::emit_linux_stack_protect_failure,
        ),
        ("worker_park", threads::emit_linux_worker_park),
        ("worker_wake", threads::emit_linux_worker_wake),
        ("registry_lock", threads::emit_linux_registry_lock),
        ("registry_unlock", threads::emit_linux_registry_unlock),
        ("registry_notify", threads::emit_linux_registry_notify),
    ] {
        let mut g = generator();
        emit(&mut g);
        result.push((name, finish_code(g)));
    }
    result.push(("handshake_worker", linux_handshake::worker_code()));
    let mut g = generator();
    g.asm.store_ptr_disp32(Reg::Rdi, 48, Reg::Rsp);
    g.asm.push_reg(Reg::Rbx);
    g.asm.mov_reg_reg(Reg::Rbx, Reg::Rdi);
    g.mov_reg_reg(P::V10, P::V7); // context survives the fetch-or CAS loop
    g.asm.load_fs_u64(Reg::Rax, 0);
    g.asm.store_ptr_disp32(Reg::R10, 8, Reg::Rax);
    threads::emit_linux_thread_context(&mut g, Reg::Rax);
    g.asm.store_ptr_disp32(Reg::R10, 16, Reg::Rax);
    let ready = g.asm.create_text_label();
    let released = g.asm.create_text_label();
    g.asm.bind_text_label(ready);
    g.asm.load_ptr_disp32(Reg::Rdi, Reg::Rbx, 40);
    g.asm.load_u32_ptr_disp32(Reg::Rax, Reg::Rdi, 0);
    g.asm.test_reg_reg(Reg::Rax, Reg::Rax);
    g.asm.jcc_label(Condition::NotEqual, released);
    g.asm.mov_imm64(Reg::Rsi, 0);
    threads::emit_linux_park_body(&mut g);
    g.asm.jmp_label(ready); // recheck the predicate after wake/EAGAIN/EINTR
    g.asm.bind_text_label(released);
    g.asm.mov_reg_reg(Reg::R10, Reg::Rbx);
    g.asm.load_ptr_disp32(Reg::Rdi, Reg::R10, 0);
    g.asm.load_ptr_disp32(Reg::Rsi, Reg::R10, 24);
    g.atomic_fetch_or(P::V7, P::V6);
    g.asm.store_ptr_disp32(Reg::R10, 32, Reg::Rax);
    // Neither worker exits until both have modified the shared word.
    let both = g.asm.create_text_label();
    g.asm.bind_text_label(both);
    g.atomic_load_acquire(P::V0, P::V7);
    g.asm.cmp_reg_imm8(Reg::Rax, 3);
    g.asm.jcc_label(Condition::NotEqual, both);
    g.asm.pop_reg(Reg::Rbx);
    g.ret();
    result.push(("worker_entry", finish_code(g)));
    // SysV wrapper: (function, five arguments) -> { return value, ABI error }.
    // Seed every nonvolatile GPR before calling, then restore the C caller's
    // original values even on failure. This tests the emitted callees, rather
    // than depending on the C compiler to happen to keep values in registers.
    let mut g = generator();
    let preserved = [Reg::Rbx, Reg::Rbp, Reg::R12, Reg::R13, Reg::R14, Reg::R15];
    for reg in preserved {
        g.asm.push_reg(reg);
    }
    g.asm.sub_reg_imm32(Reg::Rsp, 8); // align before CALL
    g.asm.mov_reg_reg(Reg::R11, Reg::Rdi);
    for (dst, src) in [
        (Reg::Rdi, Reg::Rsi),
        (Reg::Rsi, Reg::Rdx),
        (Reg::Rdx, Reg::Rcx),
        (Reg::Rcx, Reg::R8),
        (Reg::R8, Reg::R9),
    ] {
        g.asm.mov_reg_reg(dst, src);
    }
    for (index, reg) in preserved.into_iter().enumerate() {
        g.asm.mov_imm64(reg, 0x1234_5670 + index as u64);
    }
    g.asm.call_reg(Reg::R11);
    let bad = g.asm.create_text_label();
    let restore = g.asm.create_text_label();
    g.asm.mov_imm64(Reg::Rdx, 0);
    for (index, reg) in preserved.into_iter().enumerate() {
        g.asm.cmp_reg_imm32(reg, 0x1234_5670 + index as i32);
        g.asm.jcc_label(Condition::NotEqual, bad);
    }
    g.asm.jmp_label(restore);
    g.asm.bind_text_label(bad);
    g.asm.mov_imm64(Reg::Rdx, 1);
    g.asm.bind_text_label(restore);
    g.asm.add_reg_imm32(Reg::Rsp, 8);
    for reg in preserved.into_iter().rev() {
        g.asm.pop_reg(reg);
    }
    g.ret();
    result.push(("abi_call", finish_code(g)));
    result
}

#[test]
fn gc_atomic_all_operations_lower_for_both_isas() {
    for operation in [
        Operation::Load,
        Operation::Store,
        Operation::Cas,
        Operation::FetchOr,
        Operation::Fence,
        Operation::Load32,
        Operation::Store32,
        Operation::Cas32,
        Operation::ContextTransition,
        Operation::HandshakeStep,
        Operation::TlabAllocate,
        Operation::PoolStep,
        Operation::TlabPage,
        Operation::DescriptorRegister,
        Operation::DescriptorScan,
        Operation::RootStep,
        Operation::RootScan,
        Operation::RootDrain,
        Operation::RootLife,
        Operation::RootClear,
        Operation::RootScanLeased,
        Operation::RootWalk,
        Operation::RootWalkScan,
        Operation::RootWalkHeal,
        Operation::RootWalkItem,
        Operation::RelocationGate,
        Operation::PinPage,
        Operation::PinRegistryBind,
        Operation::PinRegistryUnbind,
        Operation::PinRegistryPin,
        Operation::PinRegistryUnpin,
        Operation::PinRegistrySelect,
        Operation::PinRegistryReopen,
        Operation::PinSelection,
        Operation::PinRelocationClaim,
        Operation::PinSourceDrain,
        Operation::PinSourceUncommit,
        Operation::RootPinPrepare,
        Operation::RootPinEnter,
        Operation::RootPinExit,
        Operation::RootPinCleanup,
        Operation::RootPinCancel,
        Operation::RootPinFinish,
        Operation::PinDispatch,
        Operation::NativePinPublish,
        Operation::NativePinPrepare,
        Operation::NativePinRetire,
        Operation::NativePinDispose,
        Operation::NativePinRelease,
        Operation::NativePinClose,
        Operation::NativePinProgress,
        Operation::NativePinWait,
        Operation::NativeSourceUncommit,
        Operation::RootFinishNotify,
        Operation::RelocationJob,
        Operation::RelocationComplete,
        Operation::RelocationClaim,
        Operation::MarkedRelocation,
        Operation::RelocateDiscard,
        Operation::RelocationCancel,
        Operation::AllocInto,
        Operation::DetachStep,
        Operation::BufferStep,
        Operation::BufferDrain,
        Operation::MetadataStep,
        Operation::BudgetStep,
        Operation::ReferenceLease,
        Operation::ForwardStep,
        Operation::SourceAccess,
        Operation::ForwardLookup,
        Operation::ResolveAccess,
        Operation::RelocateCopy,
        Operation::RelocatePublish,
        Operation::RelocateTarget,
        Operation::PageRelease,
        Operation::GuardedStore,
        Operation::GuardedObserve,
        Operation::GuardedIdentity,
        Operation::GuardedBlockSize,
        Operation::GuardedLogicalCas,
        Operation::GuardedLoad,
        Operation::GuardedScalar,
        Operation::GuardedHeal,
        Operation::RootFollow,
        Operation::PendingFollow,
        Operation::PendingAccess,
        Operation::PendingMark,
        Operation::WeakLoad,
        Operation::WeakRelease,
        Operation::WeakFollow,
        Operation::WeakIncoming,
        Operation::WeakTableAccess,
        Operation::WeakCreate,
        Operation::WeakMark,
        Operation::WeakCapture,
        Operation::WeakAccess,
        Operation::WeakFinish,
        Operation::WeakScan,
        Operation::WeakRelocation,
        Operation::WeakLiveness,
        Operation::SurvivorAge,
        Operation::MarkedSurvivorAge,
        Operation::PromotionRemember,
        Operation::RelocationTurn,
        Operation::ForwardCompress,
        Operation::HeapIncoming,
        Operation::HeapWalk,
        Operation::ObjectWalk,
        Operation::HeapAuditSlice,
        Operation::HeapAudit,
        Operation::RootedObservation,
        Operation::RootedScalar,
        Operation::RootedMark,
        Operation::RootedMarkSpill,
        Operation::RootedMarkSet,
        Operation::RootedMarkPublish,
        Operation::RootedLoad,
        Operation::RootedValue,
        Operation::RootedStoreCommit,
        Operation::RootedStoreDispatch,
        Operation::RootedLoadDispatch,
        Operation::BarrierPoll,
        Operation::BarrierAssist,
        Operation::NativeRootPublish,
        Operation::RootedStore,
        Operation::RootedStoreAttempt,
        Operation::RootedLoadAdmit,
        Operation::RootedLoadHeal,
        Operation::RootAccessCleanup,
        Operation::RootAccess,
        Operation::MarkPrepare,
        Operation::MarkRetarget,
        Operation::WorkerStep,
        Operation::RootJournal,
        Operation::BufferJob,
        Operation::RootAudit,
        Operation::RootRetireAudit,
        Operation::MarkEnd,
        Operation::RetireInventory,
        Operation::MarkPhase,
        Operation::CycleStep,
        Operation::RememberedStep,
        Operation::RememberedInventory,
        Operation::RememberedLoad,
        Operation::RememberedTransfer,
        Operation::RememberedOwner,
        Operation::RememberedWalk,
        Operation::RememberedLease,
        Operation::MarkRoute,
        Operation::Initialization,
        Operation::InitializationStore,
        Operation::InitializationCopy,
        Operation::MetadataGrow,
        Operation::BuiltinPrepare,
        Operation::RootStoragePrepare,
        Operation::RootStorageCanDispose,
        Operation::AllocationShape,
        Operation::AllocationStart,
        Operation::NativeStartup,
        Operation::NativeStartupRelease,
        Operation::NativeStartupCommit,
        Operation::AllocationSample,
        Operation::AllocationPressure,
        Operation::HeapAllocationPressure,
        Operation::CyclePressure,
        Operation::CycleMarkStart,
        Operation::RelocationPhase,
        Operation::RelocationRetire,
        Operation::RootCancelEmpty,
        Operation::RootCopy,
        Operation::MarkStep,
        Operation::MarkSpill,
        Operation::MarkScan,
        Operation::MarkScanCommit,
        Operation::SourceDrain,
        Operation::SourceUncommit,
        Operation::SourceRetire,
        Operation::ForwardIncoming,
    ] {
        assert!(!x86_code(operation, false).is_empty());
        assert!(!x86_code(operation, true).is_empty());
        let arm = crate::aarch64::gc_atomic_test_code(operation);
        assert_eq!(arm.len() % 4, 0);
    }
}

fn atomic_header(windows: bool, aarch64: bool, linux_workers: bool) -> String {
    use std::fmt::Write;
    let mut header = String::new();
    for (name, operation) in [
        ("load", Operation::Load),
        ("store", Operation::Store),
        ("cas", Operation::Cas),
        ("fetch_or", Operation::FetchOr),
        ("fence", Operation::Fence),
        ("load32", Operation::Load32),
        ("store32", Operation::Store32),
        ("cas32", Operation::Cas32),
        ("context_transition", Operation::ContextTransition),
        ("handshake_step", Operation::HandshakeStep),
        ("tlab_allocate", Operation::TlabAllocate),
        ("pool_step", Operation::PoolStep),
        ("tlab_page", Operation::TlabPage),
        ("descriptor_register", Operation::DescriptorRegister),
        ("descriptor_scan", Operation::DescriptorScan),
        ("root_step", Operation::RootStep),
        ("root_scan", Operation::RootScan),
        ("root_drain", Operation::RootDrain),
        ("root_life", Operation::RootLife),
        ("root_clear", Operation::RootClear),
        ("root_scan_leased", Operation::RootScanLeased),
        ("root_walk", Operation::RootWalk),
        ("root_walk_scan", Operation::RootWalkScan),
        ("root_walk_heal", Operation::RootWalkHeal),
        ("root_walk_item", Operation::RootWalkItem),
        ("relocation_gate", Operation::RelocationGate),
        ("pin_page", Operation::PinPage),
        ("pin_registry_bind", Operation::PinRegistryBind),
        ("pin_registry_unbind", Operation::PinRegistryUnbind),
        ("pin_registry_pin", Operation::PinRegistryPin),
        ("pin_registry_unpin", Operation::PinRegistryUnpin),
        ("pin_registry_select", Operation::PinRegistrySelect),
        ("pin_registry_reopen", Operation::PinRegistryReopen),
        ("pin_selection", Operation::PinSelection),
        ("pin_relocation_claim", Operation::PinRelocationClaim),
        ("pin_source_drain", Operation::PinSourceDrain),
        ("pin_source_uncommit", Operation::PinSourceUncommit),
        ("root_pin_prepare", Operation::RootPinPrepare),
        ("root_pin_enter", Operation::RootPinEnter),
        ("root_pin_exit", Operation::RootPinExit),
        ("root_pin_cleanup", Operation::RootPinCleanup),
        ("root_pin_cancel", Operation::RootPinCancel),
        ("root_pin_finish", Operation::RootPinFinish),
        ("pin_dispatch", Operation::PinDispatch),
        ("native_pin_publish", Operation::NativePinPublish),
        ("native_pin_prepare", Operation::NativePinPrepare),
        ("native_pin_retire", Operation::NativePinRetire),
        ("native_pin_dispose", Operation::NativePinDispose),
        ("native_pin_release", Operation::NativePinRelease),
        ("native_pin_close", Operation::NativePinClose),
        ("native_pin_progress", Operation::NativePinProgress),
        ("native_pin_wait", Operation::NativePinWait),
        ("native_source_uncommit", Operation::NativeSourceUncommit),
        ("root_finish_notify", Operation::RootFinishNotify),
        ("relocation_job", Operation::RelocationJob),
        ("relocation_complete", Operation::RelocationComplete),
        ("relocation_claim", Operation::RelocationClaim),
        ("marked_relocation", Operation::MarkedRelocation),
        ("relocate_discard", Operation::RelocateDiscard),
        ("relocation_cancel", Operation::RelocationCancel),
        ("alloc_into", Operation::AllocInto),
        ("detach_step", Operation::DetachStep),
        ("buffer_step", Operation::BufferStep),
        ("buffer_drain", Operation::BufferDrain),
        ("metadata_step", Operation::MetadataStep),
        ("budget_step", Operation::BudgetStep),
        ("reference_lease", Operation::ReferenceLease),
        ("forward_step", Operation::ForwardStep),
        ("source_access", Operation::SourceAccess),
        ("forward_lookup", Operation::ForwardLookup),
        ("resolve_access", Operation::ResolveAccess),
        ("relocate_copy", Operation::RelocateCopy),
        ("relocate_publish", Operation::RelocatePublish),
        ("relocate_target", Operation::RelocateTarget),
        ("page_release", Operation::PageRelease),
        ("guarded_store", Operation::GuardedStore),
        ("guarded_observe", Operation::GuardedObserve),
        ("guarded_identity", Operation::GuardedIdentity),
        ("guarded_block_size", Operation::GuardedBlockSize),
        ("guarded_logical_cas", Operation::GuardedLogicalCas),
        ("guarded_load", Operation::GuardedLoad),
        ("guarded_scalar", Operation::GuardedScalar),
        ("guarded_heal", Operation::GuardedHeal),
        ("root_follow", Operation::RootFollow),
        ("pending_follow", Operation::PendingFollow),
        ("pending_access", Operation::PendingAccess),
        ("pending_mark", Operation::PendingMark),
        ("weak_load", Operation::WeakLoad),
        ("weak_release", Operation::WeakRelease),
        ("weak_follow", Operation::WeakFollow),
        ("weak_incoming", Operation::WeakIncoming),
        ("weak_table_access", Operation::WeakTableAccess),
        ("weak_create", Operation::WeakCreate),
        ("weak_mark", Operation::WeakMark),
        ("weak_capture", Operation::WeakCapture),
        ("weak_access", Operation::WeakAccess),
        ("weak_finish", Operation::WeakFinish),
        ("weak_scan", Operation::WeakScan),
        ("weak_relocation", Operation::WeakRelocation),
        ("weak_liveness", Operation::WeakLiveness),
        ("survivor_age", Operation::SurvivorAge),
        ("marked_survivor_age", Operation::MarkedSurvivorAge),
        ("promotion_remember", Operation::PromotionRemember),
        ("relocation_turn", Operation::RelocationTurn),
        ("forward_compress", Operation::ForwardCompress),
        ("heap_incoming", Operation::HeapIncoming),
        ("heap_walk", Operation::HeapWalk),
        ("object_walk", Operation::ObjectWalk),
        ("heap_audit_slice", Operation::HeapAuditSlice),
        ("heap_audit", Operation::HeapAudit),
        ("rooted_observation", Operation::RootedObservation),
        ("rooted_scalar", Operation::RootedScalar),
        ("rooted_mark", Operation::RootedMark),
        ("rooted_mark_spill", Operation::RootedMarkSpill),
        ("rooted_mark_set", Operation::RootedMarkSet),
        ("rooted_mark_publish", Operation::RootedMarkPublish),
        ("rooted_load", Operation::RootedLoad),
        ("rooted_value", Operation::RootedValue),
        ("rooted_store_commit", Operation::RootedStoreCommit),
        ("rooted_store_dispatch", Operation::RootedStoreDispatch),
        ("rooted_load_dispatch", Operation::RootedLoadDispatch),
        ("barrier_poll", Operation::BarrierPoll),
        ("barrier_assist", Operation::BarrierAssist),
        ("native_root_publish", Operation::NativeRootPublish),
        ("rooted_store", Operation::RootedStore),
        ("rooted_store_attempt", Operation::RootedStoreAttempt),
        ("rooted_load_admit", Operation::RootedLoadAdmit),
        ("rooted_load_heal", Operation::RootedLoadHeal),
        ("root_access_cleanup", Operation::RootAccessCleanup),
        ("root_access", Operation::RootAccess),
        ("mark_prepare", Operation::MarkPrepare),
        ("mark_retarget", Operation::MarkRetarget),
        ("worker_step", Operation::WorkerStep),
        ("root_journal", Operation::RootJournal),
        ("buffer_job", Operation::BufferJob),
        ("root_audit", Operation::RootAudit),
        ("root_retire_audit", Operation::RootRetireAudit),
        ("mark_end", Operation::MarkEnd),
        ("retire_inventory", Operation::RetireInventory),
        ("mark_phase", Operation::MarkPhase),
        ("cycle_step", Operation::CycleStep),
        ("remembered_step", Operation::RememberedStep),
        ("remembered_inventory", Operation::RememberedInventory),
        ("remembered_load", Operation::RememberedLoad),
        ("remembered_transfer", Operation::RememberedTransfer),
        ("remembered_owner", Operation::RememberedOwner),
        ("remembered_walk", Operation::RememberedWalk),
        ("remembered_lease", Operation::RememberedLease),
        ("mark_route", Operation::MarkRoute),
        ("initialization", Operation::Initialization),
        ("initialization_store", Operation::InitializationStore),
        ("initialization_copy", Operation::InitializationCopy),
        ("metadata_grow", Operation::MetadataGrow),
        ("builtin_prepare", Operation::BuiltinPrepare),
        ("root_storage_prepare", Operation::RootStoragePrepare),
        ("root_storage_can_dispose", Operation::RootStorageCanDispose),
        ("allocation_shape", Operation::AllocationShape),
        ("allocation_start", Operation::AllocationStart),
        ("native_startup", Operation::NativeStartup),
        ("native_startup_release", Operation::NativeStartupRelease),
        ("native_startup_commit", Operation::NativeStartupCommit),
        ("cycle_pressure", Operation::CyclePressure),
        ("cycle_mark_start", Operation::CycleMarkStart),
        ("relocation_phase", Operation::RelocationPhase),
        ("relocation_retire", Operation::RelocationRetire),
        ("allocation_sample", Operation::AllocationSample),
        ("allocation_pressure", Operation::AllocationPressure),
        (
            "heap_allocation_pressure",
            Operation::HeapAllocationPressure,
        ),
        ("root_cancel_empty", Operation::RootCancelEmpty),
        ("root_copy", Operation::RootCopy),
        ("mark_step", Operation::MarkStep),
        ("mark_spill", Operation::MarkSpill),
        ("mark_scan", Operation::MarkScan),
        ("mark_scan_commit", Operation::MarkScanCommit),
        ("source_drain", Operation::SourceDrain),
        ("source_uncommit", Operation::SourceUncommit),
        ("source_retire", Operation::SourceRetire),
        ("forward_incoming", Operation::ForwardIncoming),
    ] {
        let bytes = if aarch64 {
            crate::aarch64::gc_atomic_test_code(operation)
        } else {
            x86_code(operation, windows)
        };
        write!(header, "static const unsigned char gc_code_{name}[] = {{").unwrap();
        for byte in bytes {
            write!(header, "{byte},").unwrap();
        }
        header.push_str("};\n");
    }
    if linux_workers {
        header.push_str("#define GC_LINUX_WORKERS 1\n");
        for (name, bytes) in linux_worker_code() {
            write!(header, "static const unsigned char gc_code_{name}[] = {{").unwrap();
            for byte in bytes {
                write!(header, "{byte},").unwrap();
            }
            header.push_str("};\n");
        }
    }
    append_windows_metadata_header(&mut header);
    append_darwin_metadata_header(&mut header);
    header
}

#[test]
fn gc_atomic_emitted_code_two_threads() {
    const {
        assert!(
            cfg!(any(target_arch = "x86_64", target_arch = "aarch64")),
            "unsupported conformance host; no skip-success"
        );
    }
    let header = atomic_header(
        cfg!(target_os = "windows"),
        cfg!(target_arch = "aarch64"),
        cfg!(all(target_os = "linux", target_arch = "x86_64")),
    );
    // Export real Win64 bytes for independent cross-build / execution probes.
    if let Some(path) = std::env::var_os("KLASSIC_GC_WINDOWS_ATOMIC_HEADER") {
        std::fs::write(path, atomic_header(true, false, false)).unwrap();
    }
    let stdout = run_c_driver("gc_atomic_code.h", &header, "atomic_harness.c");
    assert!(stdout.contains("root disposal race: 64 reader-release/FINISH races acquire completion before metadata and unmap PASS"), "{stdout}");
    assert!(stdout.contains("root finish notification: 96 owner-bound completions, 48 failed-wake retries and terminal replay PASS"), "{stdout}");
    assert!(stdout.contains("native heap bootstrap: checked guard-offset reservation, rounded table budget and retryable private disposal PASS"), "{stdout}");
    assert!(stdout.contains("native runtime bootstrap: owned heap/context/packages, delayed activation and retryable quiescent disposal PASS"), "{stdout}");
    assert!(stdout.contains("native work bootstrap: charged SATB/remset rings, lease pool and two mark queues retain pending work through disposal PASS"), "{stdout}");
    assert!(stdout.contains("native cycle controls: overlapping mark lanes, deferred tickets and retirement re-audit through final adoption PASS"), "{stdout}");
    assert!(stdout.contains("native graph cycle: rooted cyclic graph, bounded scans, counted spill and journal handoff through retirement PASS"), "{stdout}");
    assert!(stdout.contains("native mark scan owner: lease exhaustion, guarded restart and child cancellation preserve claims through bounded commit PASS"), "{stdout}");
    assert!(stdout.contains("native scan inventory: captured children prevent incoming absence, terminal unlink invalidates audit cursors PASS"), "{stdout}");
    assert!(stdout.contains("native source retirement gate: bound scan receipt required, stale or mismatched audits preserve page and budget ownership PASS"), "{stdout}");
    assert!(stdout.contains("native queue incoming: leased spill cursors survive pressure and invalidate on serial-preserving refill PASS"), "{stdout}");
    assert!(stdout.contains("native root incoming: live cells, deletion journals and reader snapshots block absence until handoff, retiring scopes stay pinned PASS"), "{stdout}");
    assert!(stdout.contains("native allocation runs: bootstrap-owned backing, rooted allocation, large payload and retryable cancellation PASS"), "{stdout}");
    assert!(stdout.contains("native owned retirement: source backing returns credits while copied logical records survive range reuse PASS"), "{stdout}");
    assert!(stdout.contains("native logical records: imported copies and queued leases delay final unmap, budgets and runtime shutdown complete PASS"), "{stdout}");
    assert!(stdout.contains("native bitmap backing: rounded admission, page publication and retryable detached disposal PASS"), "{stdout}");
    assert!(stdout.contains("native page payload release: authenticated detachment, partial OS failure and deferred budget credit PASS"), "{stdout}");
    assert!(stdout.contains("native pin wait: 96 completion-before-arm, notify-before-park and concurrent-park lifecycles PASS"), "{stdout}");
    assert!(stdout.contains("native pin progress: 40 worker-bound shutdowns, native retry, phase park and resume PASS"), "{stdout}");
    assert!(stdout.contains("native pin close: 40 package shutdowns, boundary requests, balanced locks and retry ownership PASS"), "{stdout}");
    assert!(stdout.contains("native pin budget: disposal-gated release, failed credit retry and terminal replay rejection PASS"), "{stdout}");
    assert!(stdout.contains("native pin disposal: blocked-only release, live-reader exclusion and OS retry preserve ownership PASS"), "{stdout}");
    assert!(stdout.contains("native pin package: registered publication, root reservation retries and exclusive retirement PASS"), "{stdout}");
    assert!(stdout.contains("pin dispatch: native mutex ownership, guard-only exit, sticky cancellation and resume/flush retries PASS"), "{stdout}");
    assert!(
        stdout
            .contains("native pin driver: 24 acquired-package lifecycles use mutex dispatch PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("native pin mark handoff: 40 young/old/overlapping phase lifecycles retain journals through guarded marking PASS"), "{stdout}");
    assert!(stdout.contains("root pin: canonical forwarding, independent root, guard exit, native interval and resumable cleanup PASS"), "{stdout}");
    assert!(stdout.contains("pin source release: pin exclusion, selection-bound cursors, OS decommit and exact budget credit PASS"), "{stdout}");
    assert!(stdout.contains("pin relocation bridge: pinned exclusion, page bounds, mark epoch and actual source claim PASS"), "{stdout}");
    assert!(stdout.contains("pin registry admission: bound lease authentication, selected epoch and post-move cleanup PASS"), "{stdout}");
    assert!(stdout.contains("page pin registry: authenticated pages, independent lifetime, pressure, replay and marked relocation PASS"), "{stdout}");
    assert!(stdout.contains("native startup rollback: second mapping and root disposal failures retain private ownership until retry PASS"), "{stdout}");
    assert!(stdout.contains("rooted load capture: 64 registered snapshot admissions and post-handshake readmissions PASS"), "{stdout}");
    assert!(stdout.contains("rooted load admission: 64 counted owners block ACK/native entry until both release, without overflow PASS"), "{stdout}");
    assert!(stdout.contains("rooted load rearm: 64 worker phase handoffs retain observations and reject parked resume PASS"), "{stdout}");
    assert!(stdout.contains("rooted load owner: 64 marked/null results, snapshot restarts and cancellation boundaries PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "cycle pressure: 108 C/candidate quiet, coalesced, next-cycle and rejection cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "cycle mark start: 40 C/candidate atomic ledger/rendezvous admission cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("rooted load completion: 64 two-OS-owner self-heal/null-store races preserve results and SATB PASS"), "{stdout}");
    assert!(stdout.contains("rooted mark publication: 64 guarded repairs preserve identity, remembered bits, journals and logical revisions PASS"), "{stdout}");
    assert!(stdout.contains("rooted mark set: 64 dual-generation observations retain Old work across snapshot restart and Young completion PASS"), "{stdout}");
    assert!(stdout.contains("rooted mark owner: 64 observed referents survive queue pressure, duplicate cleanup and stale-epoch cancellation PASS"), "{stdout}");
    assert!(stdout.contains("rooted observation owner: 64 moved receivers, clean journal-pressure retries and exactly-once rooted reads PASS"), "{stdout}");
    assert!(stdout.contains("scalar root access: 64 protected-source moves, journal-pressure retries and canonical field updates PASS"), "{stdout}");
    assert!(stdout.contains("scalar relocation: 32 two-user updates survive closed admission, copy drain and retained-source paths PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "scalar access 575 C/candidate typed-slot, exact-bit and guard preservation cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("initialization root access: chunk boundaries release metadata and reacquire from roots after lease pressure PASS"), "{stdout}");
    assert!(stdout.contains("initialization copy 796 C/candidate bounded-byte, scalar descriptor and failure preservation cases PASS"), "{stdout}");
    assert!(stdout.contains("initialization store 1310 C/candidate typed-slot, phase and overwrite rejection cases PASS"), "{stdout}");
    #[cfg(any(
        all(target_os = "linux", target_arch = "x86_64"),
        all(target_os = "macos", target_arch = "aarch64"),
        all(target_os = "windows", target_arch = "x86_64")
    ))]
    assert!(stdout.contains("native root preparation: native mapping plus shared initialization, 18 rejected inputs and real mapping failure PASS"), "{stdout}");
    #[cfg(any(
        all(target_os = "linux", target_arch = "x86_64"),
        all(target_os = "macos", target_arch = "aarch64"),
        all(target_os = "windows", target_arch = "x86_64")
    ))]
    assert!(stdout.contains("native builtin registry: three mappings, immutable shapes, 14 rejected inputs and map failure PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "allocation start 120 C/candidate shape, reservation and failure cleanup cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("root cancel: 20 empty-root pressure and stale/nonempty rejection cases preserve journal and scan bounds PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "root backing: budgeted native cells and journal retire after all scanner leases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("root cancel pipeline: two abandoned refill roots remain null through leased retirement without journal entries PASS"), "{stdout}");

    assert!(stdout.contains("allocation shape: 62 registry-ID and logical-payload cases preserve unrelated request fields PASS"), "{stdout}");
    assert!(stdout.contains("descriptor bootstrap: budgeted private registry survives marking and retires after all heap users PASS"), "{stdout}");
    assert!(stdout.contains("descriptor bootstrap failures: map and every registration failure clean private storage before budget rollback PASS"), "{stdout}");

    assert!(stdout.contains("builtin descriptors: raw, pointer records and scalar-prefixed lists scan exact logical fields PASS"), "{stdout}");
    if cfg!(all(target_os = "macos", target_arch = "aarch64")) {
        assert!(stdout.contains("emitted Darwin metadata: libSystem imports back live lease storage through worker retirement PASS"), "{stdout}");
    }
    if cfg!(target_os = "windows") {
        assert!(stdout.contains("emitted Windows metadata: kernel32 imports back live lease storage through worker retirement PASS"), "{stdout}");
    }
    assert!(stdout.contains("remembered offer pressure: 32 rooted children retain slot claims across full-pool native waits PASS"), "{stdout}");
    assert!(stdout.contains("rooted offer steps: 64 two-edge child moves permit phase rendezvous between adoptions before queue routing PASS"), "{stdout}");
    assert!(stdout.contains("journal offer steps: 64 two-edge copies retain claims and metadata through phase rendezvous and full-pool waits PASS"), "{stdout}");
    assert!(stdout.contains("scan cursor steps: 64 claimed arrays poll before retarget, 48 move twice with original receipts retained PASS"), "{stdout}");
    assert!(
        stdout.contains("source drain 112 C/candidate bounded metadata readiness cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "source uncommit 154 C/candidate cleanup ownership, retry and accounting cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "root walk item 2749 C/candidate durable snapshot, pending exclusion and ACK cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("root audit deletion: 32 owner null stores survive delayed repair before explicit republish PASS"), "{stdout}");
    assert!(stdout.contains("root audit pressure: 64 registered scanners retain snapshots across 128 native metadata waits and phase retries PASS"), "{stdout}");
    assert!(stdout.contains("root walk item graph: 64 leased snapshots survive forwarding-edge polls before root repair PASS"), "{stdout}");
    assert!(stdout.contains("root walk heal graph: 64 moving-array rounds repair registered roots before mark end PASS"), "{stdout}");
    assert!(stdout.contains("root walk heal 804 C/candidate exact-CAS, lease authentication and color preservation cases PASS"), "{stdout}");
    assert!(stdout.contains("root walk heal: 64 scanner/owner store-release rounds preserve journals and logical revisions PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "relocation gate 593 C/candidate admission, drain, stale permit and epoch cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "page pin admission: selection exclusion, replay rejection and unpin/reselect PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains("page pin admission: 128 two-actor selection races PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("relocation job 1134 C/candidate ownership, pending publication and stale completion cases PASS"), "{stdout}");
    assert!(stdout.contains("relocation job graph: 64 moving rounds reject premature permit release and finish every publication PASS"), "{stdout}");
    assert!(stdout.contains("relocation claim 431 C/candidate admission, winner, loser cleanup and ticket exhaustion cases PASS"), "{stdout}");
    assert!(stdout.contains("relocation claim concurrency: 64 two-actor rounds preserve one winner and release every losing permit PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "initialization publish 536 C/candidate readiness and failure preservation cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("native metadata: map failure and pending/committed cleanup retries retain budgets and storage PASS"), "{stdout}");
    assert!(stdout.contains("metadata backing: OS lease storage is budgeted before admission and released after worker retirement PASS"), "{stdout}");
    assert!(
        stdout.contains("metadata grow: 180 bounded admission and unchanged rejection cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("mark initialization graph: 32 registered roots and copied journals retain unfinished objects across mark-end retry PASS"), "{stdout}");
    assert!(stdout.contains("allocation graph: 512 rooted allocations, 448 pre-allocation journal waits and 1088 leased observer handoffs PASS"), "{stdout}");
    assert!(stdout.contains("mark initialization: 64 two-actor waits preserve input ownership until release/acquire ready publication PASS"), "{stdout}");
    assert!(stdout.contains("allocation into 618 C/candidate empty-root publication and pre-allocation failure cases PASS"), "{stdout}");
    assert!(stdout.contains("relocation cancel 769 C/candidate bounded cleanup, source reopening and permit release cases PASS"), "{stdout}");
    assert!(stdout.contains("relocation cancel pipeline: 32 resumable cancellations reopen guarded readers before release and reject 32 installed jobs PASS"), "{stdout}");
    assert!(stdout.contains("relocation discard 386 C/candidate bounded private cleanup and installed-target rejection cases PASS"), "{stdout}");
    assert!(stdout.contains("relocation discard pipeline: 48 bounded cleanups retain admission and 32 installed targets reject discard PASS"), "{stdout}");
    assert!(stdout.contains("relocation completion 1047 C/candidate resumable publication, finish and release cases PASS"), "{stdout}");
    assert!(stdout.contains("relocation completion pipeline: 448 installed jobs and 64 moving graph rounds use the shared completion funnel PASS"), "{stdout}");
    assert!(stdout.contains("relocation job pipeline: 448 installed jobs authenticate publication and reject completion after permit release PASS"), "{stdout}");
    assert!(stdout.contains("relocation installed recovery: 32 interrupted jobs preserve installed targets and complete forwarding before permit release PASS"), "{stdout}");
    assert!(stdout.contains("relocation cancellation: 32 failures at four private-copy stages retain source access and drain admission PASS"), "{stdout}");
    assert!(stdout.contains("relocation gate graph: 64 pending publications block source sealing and 32 incoming audits freeze admission PASS"), "{stdout}");
    assert!(stdout.contains("forward incoming 134 C/candidate bounded range, pending publication and preservation cases PASS"), "{stdout}");
    assert!(stdout.contains("forward incoming graph: 32 lease-free source chains retain incoming forwarding dependencies PASS"), "{stdout}");
    assert!(stdout.contains("source uncommit graph: 64 OS decommits credit payload once while retaining leased metadata and virtual ranges PASS"), "{stdout}");
    assert!(stdout.contains("source drain graph: 64 sealed source runs reject target append after payload protection with live metadata leases PASS"), "{stdout}");
    assert!(stdout.contains("guarded heal graph: 32 worker repairs and 32 intervening deletions survive stale CAS and array copy PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "guarded heal 1563 C/candidate exact-observation, phase and preservation cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("mark busy offers: 64 rooted child moves admit phase rendezvous before publication and queue handoff PASS"), "{stdout}");
    assert!(stdout.contains("mark rooted offers: 64 full pools retain pending child roots across native waits and queue handoff PASS"), "{stdout}");
    assert!(stdout.contains("mark pending roots: 64 scanned child batches survive phase rendezvous before queue handoff and root retirement PASS"), "{stdout}");
    assert!(stdout.contains("mark busy forwarding: 64 pending publications admit phase rendezvous with unchanged scan claims PASS"), "{stdout}");
    assert!(stdout.contains("mark destination pressure: 22 full pools discard private forwarding results and resume original claims PASS"), "{stdout}");
    assert!(stdout.contains("mark worker pressure: 64 full pools retain queued claims across native waits before moved-array scans PASS"), "{stdout}");
    assert!(stdout.contains("mark worker retarget: 64 moved partial-array claims retain cursors and reject stale receipts, 16 protected source pages PASS"), "{stdout}");
    assert!(stdout.contains("rooted pressure: 32 intermediate owner roots survive metadata exhaustion, native waits and second moves PASS"), "{stdout}");
    assert!(stdout.contains("rooted access: 32 real-worker two-edge owners survive journal pressure, phase rendezvous and source decommits PASS"), "{stdout}");
    assert!(stdout.contains("root access: 32 two-edge chains retain roots across busy/full/native retries before canonical guards PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "root access 1298 C/candidate bounded forwarding, guard and backpressure cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "remembered detach: 32 buffered and copied source leases block page-bound unlink PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("remembered retirement: 32 buffered source replays after transfer retain metadata and reject mark end PASS"), "{stdout}");
    assert!(stdout.contains("remembered transfer: 32 real-worker moved claims retain late records and rescan destination slots PASS"), "{stdout}");
    assert!(stdout.contains("remembered movement: 32 blocked claims reload canonical Old fields after source decommit PASS"), "{stdout}");
    assert!(stdout.contains("promotion Old work admission: 16 full-queue parents retain source leases through spill, promotion and post-protection retarget PASS"), "{stdout}");
    assert!(stdout.contains("promotion during Old mark: 16 partial-array claims retain cursors, overflow children and all 513 remembered slots across protected-source promotion PASS"), "{stdout}");
    assert!(stdout.contains("relocation turn pipeline: 96 real-copy turns retain ownership through cancellation, installed recovery and source teardown before Old admission PASS"), "{stdout}");
    assert!(stdout.contains("forward compression graph: 32 joined-worker audits shorten two retained edges, preserve canonical identity and clear intermediate incoming references PASS"), "{stdout}");
    assert!(stdout.contains("heap audit owner graph: 64 cooperative whole-pool walks agree on findings and release every lease PASS"), "{stdout}");
    assert!(stdout.contains("heap audit owner: C/candidate clean and dirty completion, restart, pressure and retained cancellation cases PASS"), "{stdout}");
    assert!(stdout.contains("heap audit slice: C/candidate bounded scans, discarded forwarding and retained exit ownership PASS"), "{stdout}");
    assert!(stdout.contains("heap payload audit: 64 whole-pool scans visit eight canonical identities, skip protected sources and release every field guard PASS"), "{stdout}");
    assert!(stdout.contains("object walk graph: 64 leased whole-pool key walks cover every allocation and installed copy without touching protected payloads PASS"), "{stdout}");
    assert!(stdout.contains("object walk: 32 C/candidate leased key walks, append restarts and 60 unchanged rejection cases PASS"), "{stdout}");
    assert!(stdout.contains("heap walk graph: 64 real pools enumerate moved and protected source heads with balanced metadata leases PASS"), "{stdout}");
    assert!(stdout.contains("heap walk: 64 C/candidate mixed-page walks preserve lease ownership and 48 unchanged rejection cases PASS"), "{stdout}");
    assert!(stdout.contains("heap incoming graph: 32 protected-source moved owners retain child findings and exclude two old source ranges PASS"), "{stdout}");
    assert!(stdout.contains("heap incoming C/candidate: bounded descriptor slots, range boundaries, retained findings and unchanged rejection PASS"), "{stdout}");
    assert!(stdout.contains("forward compression: 156 C/candidate exact-edge, gate and unchanged rejection cases PASS"), "{stdout}");
    assert!(stdout.contains("forward compression chain: 64 protected two-hop sources shorten to rooted canonical identities and clear intermediate incoming edges PASS"), "{stdout}");
    assert!(stdout.contains("relocation turns: 128 C/candidate alternating admissions, active-job drainage and stale receipts PASS"), "{stdout}");
    assert!(stdout.contains("relocation turn rejection: 46 C/candidate unchanged malformed, exhausted and pending-priority cases PASS"), "{stdout}");
    assert!(stdout.contains("promotion remember: 82 C/candidate bounded shapes, unchanged rejection and phase restart cases PASS"), "{stdout}");
    assert!(stdout.contains("promotion handoff: 192 age-selected Old moves register reference and null slots before forwarding publication PASS"), "{stdout}");
    assert!(stdout.contains("mark routing: 32 registered Old roots and journal deletions skip Young marking before remembered handoff PASS"), "{stdout}");
    assert!(stdout.contains("remembered pressure: 32 full metadata pools retain claims across blocked phase adoption and retry PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "remembered lease 842 C/candidate claim, run binding and preserved failure cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "remembered lease: 96 protected-source acquisitions and consumed-claim rejections PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("remembered dispatch: 32 late previous-epoch replays survive ACK and force a second worker pass PASS"), "{stdout}");
    assert!(stdout.contains("remembered walk 731 C/candidate bounded traversal, membership retention and claim handoff cases PASS"), "{stdout}");
    assert!(stdout.contains("remembered dispatch: 32 real-worker page walks skip empty Old runs and transfer membership to slot claims PASS"), "{stdout}");
    assert!(
        stdout.contains("4096 CAS winner, 4096 fetch-or, 4096 publication trials PASS"),
        "{stdout}"
    );
    assert!(
        stdout
            .contains("4096 u32 CAS, 4096 u32 publication, exact-width context/guard checks PASS"),
        "{stdout}"
    );
    if cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        assert!(
            stdout.contains("emitted Linux metadata: direct mmap/munmap preserve the native backing lifecycle PASS") &&
            stdout.contains("emitted Linux clone/TLS/futex join and startup failure PASS"),
            "{stdout}"
        );
        assert!(stdout.contains("emitted Linux lifecycle: 1/2/8 raw-clone owners, 384 rounds, TLS and futex registry PASS"), "{stdout}");
    }
    assert!(
        stdout.contains(
            "remembered slots 6638 C/candidate flip, late-entry, claim and retention cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "remembered slots: 64 buffered moved-slot replays, late entries and repeated flips PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "remembered inventory 1137 C/candidate registration, aggregate work and flip cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "remembered load 2333 C/candidate claim, phase and rooted-handoff cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "remembered handoff: 64 Old slots, rooted Young moves and gap-free queue transfers PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "mark route 4987 C/candidate null, canonical generation and lease ownership cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("mark route: 64 null and 64 promoted Old claims skip Young marking without losing current records PASS"), "{stdout}");
    assert!(stdout.contains("remembered transfer 1769 C/candidate moved-slot, late-bit and claim accounting cases PASS"), "{stdout}");
    assert!(stdout.contains("remembered transfer: 96 protected owner moves rehome claims into canonical page inventories PASS"), "{stdout}");
    assert!(stdout.contains("remembered transfer: 64 later flips reread retained canonical slots and mark moved Young references PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "remembered owner 5938 C/candidate predecessor, prefix and claim validation cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("remembered owner: 32 append publication rounds reject unpublished slots without reading protected heap PASS"), "{stdout}");
    assert!(stdout.contains("remembered owner: 192 claimed interior slots and 96 protected source lookups preserve relative offsets PASS"), "{stdout}");
    assert!(stdout.contains("remembered inventory: 64 two-page Young end checks retain unflipped, pending and claimed work PASS"), "{stdout}");
    assert!(stdout.contains("remembered inventory: 64 moved-buffer handoffs preserve aggregate pending and claim counts PASS"), "{stdout}");
    assert!(
        stdout.contains("1536 C/emitted shared-context transition and preservation cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "mark end 1101 C/candidate full-inventory, rendezvous and pending-work cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains("mark end: 64 shared C/emitted complete-inventory graph end checks PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("mark start: 64 prepared-epoch adoption and activation boundaries before graph traversal PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "mark phase 6528 C/candidate decision, retry, adoption and preservation cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "mark phase: 64 real-worker retry and Select transitions with phase adoption PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "cycle tickets 480 C/candidate request, start, completion and preservation cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("thread inventory: 64 complete registered-owner end checks and pending-work rejections PASS"), "{stdout}");
    assert!(
        stdout.contains("41615 C/emitted handshake sequence, failure and preservation cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("emitted handshake: 1/2/8 owners, 384 rounds, duplicate suppression and outside-lock progress PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "TLAB allocation: sizes, colors, object starts, rollover and failure preservation PASS"
        ),
        "{stdout}"
    );
    eprint!("{stdout}");
    assert!(
        stdout
            .contains("mark spill 959 C/candidate saturation, refill and preservation cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("mark graph overflow: 64 bounded-capacity rounds (1/2/16 slots), deferred jobs drained PASS"), "{stdout}");

    assert!(
        stdout.contains("mark queue 6117 C/candidate stamp, ownership and preservation cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("mark slices 1578 C/candidate bounded atomic heap-scan cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("mark graph: 64 two-worker root/SATB deletion, cyclic graph and bounded-array rounds PASS"), "{stdout}");

    assert!(
        stdout.contains("metadata leases 2166 C/candidate token, retention and reuse cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "metadata budget 214 C/candidate admission, cleanup and shared-page limit cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("mark graph metadata: 64 pre-publication limit, allocation rollback and retirement rounds PASS"), "{stdout}");
    assert!(stdout.contains("mark graph records: 64 allocator-owned record, full-arena and header-lookup rounds PASS"), "{stdout}");
    assert!(
        stdout.contains("2806 C/candidate TLAB cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "reference leases 1705 C/candidate run-head, start-bit and failure cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "mark graph reference leases: 64 varying run-head and multi-page array rounds PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains("forwarding 1673 C/candidate claim, publication and retention cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("forwarding: 64 two-worker partial-copy, acquire-reader, loser cleanup and retention rounds PASS"), "{stdout}");
    assert!(
        stdout.contains("source access 154 C/candidate admission, drain and reopen cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("forward lookup 2329 C/candidate metadata-only cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("mark graph: 64 metadata lookup and guarded header/scan rounds PASS"),
        "{stdout}"
    );
    assert!(
        stdout
            .contains("forward table: 32 concurrent append, lookup and guarded header rounds PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("resolve access 1544 C/candidate bounded forwarding and guard cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "root follow 1338 C/candidate rooted forwarding, retry and lease balance cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "root follow: 64 protected-source two-hop roots and 16 journal-full retries PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("root follow: 64 guarded-field output roots, busy retries and protected referent moves PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "mark prepare 628 C/candidate guarded header and unique lease transfer cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "mark prepare: 64 two-worker graph, spill and duplicate lease handoff rounds PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("mark prepare: 64 moved output roots, queue duplicates and guarded completion rounds PASS"), "{stdout}");
    assert!(
        stdout.contains("mark retarget 587 C/candidate cursor, claim and lease handoff cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("mark retarget: 64 protected-source partial-array moves, overflow children and stale-claim rounds PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "root journal 930 C/candidate claim, copy, handoff and preservation cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "root journal: 64 counted pending handoffs across rendezvous and retirement rounds PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("root journal: 64 concurrent producer/copy, full-ring retention and competing-consumer rounds PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "buffer jobs 1263 C/candidate SATB/remset claim, copy and handoff cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("buffer jobs: 64 registered SATB/remset pending tokens, detach waits and ACK rounds PASS"), "{stdout}");
    assert!(stdout.contains("native buffer quiescence: private prefixes and concurrent consumer copies require owner handoff and ACK PASS"), "{stdout}");
    assert!(stdout.contains("native buffer inventory: bounded owner handoffs reject detached cursors and same-address registration reuse PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "buffer jobs: 64 pending SATB copies across registered graph end checks PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "root audit 621 C/candidate inventory, cycle and owner certificate cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("cycle graph admission: 64 two-worker coalesced starts, next requests and unfinished Select tickets PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "marked survivor age: 76 C/candidate epoch, generation, retry and rejection cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("relocation phase: 1228 C/candidate adoption, remap, exclusion and exact rejection cases PASS"), "{stdout}");
    assert!(stdout.contains("relocation phase graph: 64 selected heaps adopt two boundaries before survivor copies and keep cycle tickets unfinished PASS"), "{stdout}");
    assert!(stdout.contains("relocation retire: 840 C/candidate drainage, adoption, closed-gate and exact rejection cases PASS"), "{stdout}");
    assert!(stdout.contains("relocation retire graph: 64 drained heaps adopt Retire before audits and keep admission closed and tickets unfinished PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "retire inventory 5435 C/candidate rendezvous, all-owner and registered-work cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("retire inventory graph: 8 changed inventories and 16 registered jobs prevent READY before 8 completed rendezvous pass PASS"), "{stdout}");
    assert!(stdout.contains("native startup publication: committed roots, growth and caller return scope use registered native packages PASS"), "{stdout}");
    assert!(stdout.contains("native root publication: committed mapping to registered scope, rejection preservation and leased retirement PASS"), "{stdout}");
    assert!(stdout.contains("barrier assist: TLAB close and complete buffer publication precede ACK without consuming leases PASS"), "{stdout}");
    assert!(stdout.contains("barrier poll: guarded cancellation, registry rendezvous and load/store rearm through native dispatch PASS"), "{stdout}");
    assert!(stdout.contains("pending mark chain: 64 protected two-hop graphs, metadata pressure, both generation queues and duplicate cleanup before journal ACK PASS"), "{stdout}");
    assert!(stdout.contains("weak release: 32 C/candidate ownership and replay cases, 64 two-actor load/dispose schedules PASS"), "{stdout}");
    assert!(stdout.contains("weak table: 48 C/emitted creation and pressure cases, 128 serial reuse lifecycles and rooted promotion PASS"), "{stdout}");
    assert!(stdout.contains("weak backing: six budget, map, live-handle, free-list and disposal-retry scenarios PASS"), "{stdout}");
    assert!(stdout.contains("weak mark admission: 64 two-actor load/end schedules and 20 failure-atomic C/emitted owner cases PASS"), "{stdout}");
    assert!(stdout.contains("weak liveness: 136 C/candidate canonical epoch, guard and completed-owner observations preserve all state PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "weak finish: 104 C/emitted state comparisons and 4 partial-cleanup retries PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("weak access: 96 C/emitted rejected or retryable ENTER cases and 8 stale-owner guard exits PASS"), "{stdout}");
    assert!(
        stdout.contains("weak capture: 120 C/emitted failure-preservation cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("weak processing: 40 C/candidate guarded dead-clear, live-lease, pressure, disposal and cleanup-retry lifecycles PASS"), "{stdout}");
    assert!(stdout.contains("weak scan owner: 96 C/emitted admission and geometry rejections, 4 abort-cleanup retries PASS"), "{stdout}");
    assert!(stdout.contains("weak scan: 36 C/emitted bounded table, moved referent, retry, abort and covered-reopen lifecycles PASS"), "{stdout}");
    assert!(stdout.contains("weak scan race: 192 two-actor C/candidate clear, closed-load, reopened-root and release schedules PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "weak relocation: 384 C/emitted rejected or deferred handoffs preserve state PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("weak mark-to-relocate: 24 C/emitted-owner journal, mark, weak coverage and atomic relocation-adoption lifecycles PASS"), "{stdout}");
    assert!(stdout.contains("weak load: 58 C/candidate rooted publication, closed admission, stale handle, journal pressure and metadata rejection cases PASS"), "{stdout}");
    assert!(stdout.contains("pending journal move race: 32 OS movers publish and protect after follow DONE, then consumers retry canonical access with retained claims PASS"), "{stdout}");
    assert!(stdout.contains("pending journal chain: 64 protected-source graphs retain copied items through root clear, metadata pressure and two-hop guarded handoff PASS"), "{stdout}");
    assert!(stdout.contains("pending access: forwarding after follow, scratch discard, guard ownership and unchanged rejection PASS"), "{stdout}");
    assert!(stdout.contains("pending reference follow: bounded two-hop metadata handoff, retained pressure and balanced cleanup PASS"), "{stdout}");
    assert!(stdout.contains("root journal pressure: 1/2/8 live mutators, wrapped eight-entry rings and counted concurrent mark handoff PASS"), "{stdout}");
    assert!(stdout.contains("rooted store registry: 1/2/8 owners register roots, drain leased journals, retire scopes and detach PASS"), "{stdout}");
    assert!(stdout.contains("rooted load dispatch: native locks, automatic admission, concurrent loads and captured-result rearm PASS"), "{stdout}");
    assert!(stdout.contains("rooted store cross-page SATB: independent owner/referent leases and Young/Old queue handoff PASS"), "{stdout}");
    assert!(stdout.contains("rooted store pressure: 1/2/8 mutators, one-entry rings, concurrent SATB/remset worker and retry progress PASS"), "{stdout}");
    assert!(stdout.contains("rooted store handshake: 1/2/8 OS owners cancel guarded attempts before ACK, resume and readmit PASS"), "{stdout}");
    assert!(stdout.contains("rooted store concurrency: 1/2/8 OS mutators, shared-slot CAS retry and transferred lease conservation PASS"), "{stdout}");
    assert!(stdout.contains("rooted store dispatch: native heap/registry/both-queue locks, guard-free acquisition and automatic admission PASS"), "{stdout}");
    assert!(stdout.contains("rooted store: 300 C/candidate admitted preparation, observation, retry and cancellation schedules PASS"), "{stdout}");
    assert!(stdout.contains("rooted store attempt: 416 C/candidate OS-backed transfer, partial admission, abort and cleanup retry schedules PASS"), "{stdout}");
    assert!(stdout.contains("rooted store pipeline: 64 admitted value preparations drive 128 physical commits with real buffer lease handoff PASS"), "{stdout}");
    assert!(stdout.contains("rooted store commit: 2102 C/candidate rooted colors, null stores, mark receipts and lease-transfer effects PASS"), "{stdout}");
    assert!(stdout.contains("rooted value: 64 dual-mark and null preparations plus 1536 cancellation/rearm boundaries preserve owned roots without field reads PASS"), "{stdout}");
    assert!(stdout.contains("rooted scalar: 768 C/candidate OS-backed canonical accesses, backpressure, abort and exactly-once cleanup cases PASS"), "{stdout}");
    assert!(stdout.contains("source replacement graph: 64 rooted allocations reuse retired object addresses with new identities and balanced publication/deletion journals PASS"), "{stdout}");
    assert!(stdout.contains("source reuse graph: 64 audited ranges recommit at the same addresses with new identities, reject stale incarnation and retirement receipts, and retain live destinations PASS"), "{stdout}");
    assert!(stdout.contains("source retire graph: 64 audited source tables and bitmaps freed before teardown with balanced charges and surviving canonical identities PASS"), "{stdout}");
    assert!(stdout.contains("source range audits: 64 whole-pool forwarding findings vanish after compression; 64 heap and 32 root/inventory re-audits pass PASS"), "{stdout}");
    assert!(stdout.contains("source retire 211 C/candidate bounded cleanup, lease, replay and prefix reuse cases PASS"), "{stdout}");
    assert!(stdout.contains("weak follow: 64 C/emitted slot-transfer, pressure, cleanup and failure-preservation cases PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "weak table access: 64 C/emitted rooted-load, release and serial-reuse cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("weak incoming: 48 C/emitted bounded scan, remap, pressure and stale-capture lifecycles PASS"), "{stdout}");
    assert!(stdout.contains("source retire OS: 16 copied objects and weak handles survive payload uncommit, metadata retirement and same-range recommit PASS"), "{stdout}");
    assert!(stdout.contains("native source payload: request-bound OS retry preserves weak metadata and defers budget credit PASS"), "{stdout}");
    assert!(
        stdout.contains("native payload decommit: OS callback drives source retirement PASS"),
        "{stdout}"
    );
    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    assert!(
        stdout.contains(
            "Linux payload decommit: direct syscall callback drives source retirement PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("native source bitmap: actual mapped retirement, retained failed-unmap ownership and rounded budget return PASS"), "{stdout}");
    assert!(stdout.contains("retire context holders: 8 unpublished buffers delay adoption and 16 published or copied jobs delay owner certification PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "retire root audit 20257 C/candidate snapshot, revision and journal cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("retire root repair: 16 pre-move roots healed, 8 changed-owner certificates rejected and 8 fresh range audits pass PASS"), "{stdout}");
    assert!(stdout.contains("retire heap repair: 24 rooted field repairs survive journal pressure and 16 fresh range audits find no stale edges PASS"), "{stdout}");
    assert!(stdout.contains("selected promotion: 8 reference-bearing Old copies retain Young child slots before publication PASS"), "{stdout}");
    assert!(stdout.contains("graph survivor selection: 64 canonical Select walks age only marked Young survivors once PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "guarded block size: 104 C/candidate empty, rounded, guarded and rejection cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "marked relocation: 68 C/candidate survivor, stale, claim and nonmutation cases PASS"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("selected relocation: 40 closed-gate candidates, 8 retained sources and 16 promotions preserve identity and payload PASS"), "{stdout}");
    assert!(stdout.contains("root audit: 64 registered inventory scans, stale owner rejection and renewed end checks PASS"), "{stdout}");
    assert!(stdout.contains("worker rendezvous 2101 C/candidate registration, phase adoption and preservation cases PASS"), "{stdout}");
    assert!(stdout.contains("worker rendezvous: 64 registered two-worker graphs, live-claim ACK, native adoption and root retirement rounds PASS"), "{stdout}");
    assert!(stdout.contains("root publication: 64 stopped-owner stale audit rejections and resumed journal handoffs PASS"), "{stdout}");
    assert!(stdout.contains("resolve chains: 64 protected-source, two-hop, rooted-handoff and guarded-destination rounds PASS"), "{stdout}");
    assert!(
        stdout.contains(
            "resolve access: 64 competing-copy, busy retry and retained-source rounds PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "resolve handoff: 16 full-journal, discard-before-poll and rooted-retry rounds PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains(
            "relocation copy 4335 C/candidate bounded payload and preservation cases PASS"
        ),
        "{stdout}"
    );
    assert!(
        stdout
            .contains("relocation publish 217 C/candidate complete-copy and ownership cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("relocation copy: 64 emitted/C bounded-copy, partial-abandonment and publication rounds PASS"), "{stdout}");
    assert!(stdout.contains("source access: 64 delayed-source-user, closed-admission, drain and retained-reopen rounds PASS"), "{stdout}");
    assert!(stdout.contains("metadata leases: 64 buffer-full rollback, delayed-job retention and stale-token rounds PASS"), "{stdout}");
    assert!(
        stdout.contains("10496 C/candidate page-pool cases PASS"),
        "{stdout}"
    );

    assert!(
        stdout.contains("barrier buffers 2340 producer / 1244 drain cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("barrier buffers: 64 private-prefix, concurrent ring-wrap and complete-entry rounds PASS"), "{stdout}");
    assert!(
        stdout.contains("detach progress 1120 C/candidate bounded-step cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout.contains("62 C/candidate TLAB bind/close cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("root detach: 64 retained-context, delayed-reader/journal and buffer-ownership rounds PASS"), "{stdout}");
    assert!(
        stdout.contains("root inventory walk 1891 / captured-prefix scan 18335 cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("root inventory: 64 delayed-walker, unlink/reuse, publication-journal and two-walker rounds PASS"), "{stdout}");
    assert!(
        stdout.contains("root lifetime 1261 / clear 150 / leased scan 384 cases PASS"),
        "{stdout}"
    );
    assert!(stdout.contains("root retirement: 64 delayed-reader, dual-mark ACK, protected-page and same-address reuse rounds PASS"), "{stdout}");
    assert!(stdout.contains("page pipeline: real mappings, preparation failures, TLAB rollover, large payload and reserved output roots PASS"), "{stdout}");
    assert!(
        stdout.contains("root storage 969 / scan 16 / journal drain 737 cases PASS"),
        "{stdout}"
    );
    assert!(
        stdout
            .contains("root overwrite/clear, journal wrap and deferred frame scan: 64 rounds PASS"),
        "{stdout}"
    );
    assert!(
        stdout
            .contains("page pool: budgets, reserve, fragmentation, rollback and stale leases PASS"),
        "{stdout}"
    );
}

#[test]
fn c_driver_supervision_drains_stdout_and_stderr_before_exit() {
    let stdout = run_c_driver("pipe_probe.h", "", "pipe_drain_harness.c");
    let (payload, marker) = stdout.split_at(4 * 1024 * 1024);
    assert!(payload.bytes().all(|byte| byte == b'x'));
    // C text stdout uses CRLF on a native Windows host.
    assert_eq!(
        marker.replace("\r\n", "\n"),
        "\nconcurrent pipe drain PASS\n"
    );
}

/// External C driver shared by native instruction and OS-adapter tests.
pub(super) fn run_c_driver(header_name: &str, header: &str, harness: &str) -> String {
    use std::process::Command;
    struct Scratch(std::path::PathBuf);
    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }
    let scratch = Scratch(std::env::temp_dir().join(format!(
            "klassic-emitted-atomics-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        )));
    std::fs::create_dir(&scratch.0).unwrap();
    std::fs::write(scratch.0.join(header_name), header).unwrap();
    let cc = std::env::var("KLASSIC_CLANG").unwrap_or_else(|_| {
        ["clang-15", "clang", "cc"]
            .into_iter()
            .find(|cc| {
                Command::new(cc)
                    .arg("--version")
                    .output()
                    .is_ok_and(|o| o.status.success())
            })
            .expect("emitted-code conformance requires a C11 compiler")
            .to_owned()
    });
    let executable = scratch.0.join(if cfg!(windows) {
        "atomics.exe"
    } else {
        "atomics"
    });
    let mut command = Command::new(cc);
    command.args(["-std=c11", "-O2", "-Wall", "-Wextra", "-Werror"]);
    if cfg!(windows) {
        command.args(["-D_CRT_SECURE_NO_WARNINGS", "-D_CRT_USE_BUILTIN_OFFSETOF"]);
    } else {
        command.arg("-pthread");
    }
    let output = command
        .arg("-I")
        .arg(&scratch.0)
        .arg("-I")
        .arg(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../runtime/gc"))
        .arg(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests")
                .join(harness),
        )
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "compile conformance driver: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let output = run_with_deadline(&executable);
    assert!(
        output.status.success(),
        "emitted-code conformance {:?}: stdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).unwrap()
}

pub(super) fn run_with_deadline(executable: &std::path::Path) -> std::process::Output {
    use std::io::Read;
    use std::process::Command;
    let mut child = Command::new(executable)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    // The C fixtures intentionally publish progress without stdio buffering.
    // Drain both pipes while supervising: waiting for exit first can fill a
    // small host pipe and strand the child in write until its real alarm fires.
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let stdout_reader = std::thread::spawn(move || {
        let mut bytes = Vec::new();
        stdout.read_to_end(&mut bytes).unwrap();
        bytes
    });
    let stderr_reader = std::thread::spawn(move || {
        let mut bytes = Vec::new();
        stderr.read_to_end(&mut bytes).unwrap();
        bytes
    });
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(45);
    let mut timed_out = false;
    while child.try_wait().unwrap().is_none() {
        if std::time::Instant::now() >= deadline {
            let _ = child.kill();
            timed_out = true;
            break;
        }
        // Timeout supervision only; fixture interleavings use latches/events.
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    let output = std::process::Output {
        status: child.wait().unwrap(),
        stdout: stdout_reader.join().unwrap(),
        stderr: stderr_reader.join().unwrap(),
    };
    assert!(
        !timed_out,
        "conformance driver timed out: stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[path = "gc_legacy_heal_tests.rs"]
mod legacy_heal;

// This fixture executes the ELF/PE x86-64 emitter, not AArch64 code.
#[cfg(target_arch = "x86_64")]
#[path = "gc_scope_lifetime_tests.rs"]
mod scope_lifetime;
