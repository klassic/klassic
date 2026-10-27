//! Test-only emission helpers for the machine-code harness in
//! `gc_barrier_harness_tests.rs`. They read runtime state that the harness
//! program reports (mark bits, remembered-set contents, counters), so the
//! tests observe the real emitted collector rather than a model of it.
use super::{
    Labels, RT_COLLECTIONS, RT_FULL_COLLECTIONS, RT_HEAP_BASE, RT_MARK_ACTIVE, RT_MARK_CYCLES,
    RT_MARKED, RT_PHASE_OPEN, RT_REMSET_BASE, RT_REMSET_DROPPED, RT_REMSET_FLIPPED,
    RT_REMSET_FLIPS, RT_REMSET_KEPT, RT_REMSET_LEN, RT_UNMARKED, RT_WORKER_COPIES,
    RT_WORKER_SCANNED, d, lea_rt, load_rt, mark, store_rt,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
#[path = "test_support_oom.rs"]
mod oom;
pub(crate) use oom::*;
#[path = "test_support_refill.rs"]
mod refill;
pub(crate) use refill::*;
#[path = "test_support_root_retirement.rs"]
mod root_retirement_support;
pub(crate) use root_retirement_support::*;
#[path = "test_support_detach.rs"]
mod detach_support;
pub(crate) use detach_support::*;
#[path = "test_support_thread_tls.rs"]
mod thread_tls_support;
pub(crate) use thread_tls_support::*;
#[path = "test_support_participants.rs"]
mod participants;
pub(crate) use participants::*;
#[path = "test_support_registered_roots.rs"]
mod registered_roots;
pub(crate) use registered_roots::*;
#[path = "test_support_mark_queue.rs"]
mod mark_queue_support;
#[path = "test_support_metadata_leases.rs"]
mod metadata_leases;
pub(crate) use mark_queue_support::*;
pub(crate) use metadata_leases::*;
#[path = "test_support_native_roots.rs"]
mod native_roots;
pub(crate) use native_roots::*;
#[path = "test_support_pin_destination.rs"]
mod pin_destination;
pub(crate) use pin_destination::*;

/// A runtime counter or flag the harness can report.
#[derive(Clone, Copy)]
pub(crate) enum Counter {
    CapacityActive,
    CapacityRequested,
    CapacitySealed,
    CapacityReserve,
    CapacityClamps,
    CapacityGrows,
    CapacityShrinks,
    CapacitySamples,
    CapacityTrace,
    CapacityBlack,
    CapacityFresh,

    StartTarget,
    StartAllocationRate,
    StartMarkRate,
    StartRateSamples,
    StartMarkSamples,
    StartUpdates,
    StartInvalid,
    StartUnits,

    AdoptedPhase,
    YoungActive,
    YoungDone,
    YoungEpoch,
    YoungMarked,
    YoungRoot,
    YoungRootBound,
    YoungRemset,
    YoungRemsetBound,
    YoungOldIndex,
    YoungOldBound,
    YoungObject,
    YoungField,
    YoungStackLen,
    YoungWorkerUnits,
    YoungWaits,
    YoungValid,
    YoungUnmarked,
    CopyWorkerLost,
    CopyOwnerLost,
    AgeThreshold,
    AgeFrozen,
    AgeUp,
    AgeDown,
    AgeYoungSamples,
    AgeFullSamples,
    AgeUpdates,
    AgeYoung,
    AgePromoted,
    AgeOwnerYoung,
    AgeWorkerYoung,
    AgeOwnerPromoted,
    AgeWorkerPromoted,
    PolicyAllocated,
    PolicyRetained,
    PolicyEMA,
    PolicyTarget,
    PolicyBase,
    PolicyBudget,
    PolicyFull,
    PolicyYoung,
    PolicyUpdates,
    PolicyDeferred,
    PolicyPeriodic,
    PolicyPressure,
    OldRequest,
    OldCycleChecks,
    OldCycleRetries,
    OldPlannedCycles,
    MarkPrepareState,
    MarkReadinessRequests,
    MarkPrepareCancels,
    MarkValid,
    RetireStage,
    RetireCursor,
    RetireEnd,
    RetireUnits,
    RetirePage,
    Collections,
    FullCollections,
    Marked,
    MarkCycles,
    MarkActive,
    MarkRootIndex,
    MarkRootBound,
    MarkObject,
    MarkRelocateIndex,
    MarkField,
    MarkStackLen,
    MarkDrains,
    CycleChecks,
    CycleRetries,
    ReadyCycles,
    WorkSequence,
    LockTickets,
    WakePending,
    RelocateEndChecks,
    RelocateEndRetries,
    RelocateDrainSlices,
    RelocateDrainYields,
    RelocateDrainMax,
    RelocateWorkerUnits,
    RelocateDrainAssisted,
    MarkDrainSlices,
    MarkDrainYields,
    MarkDrainMax,
    MarkWorkerUnits,
    MarkDrainAssisted,
    MarkDone,
    RelocateRootIndex,
    RelocateRootBound,
    WorkerDone,
    TlabPage,
    RelocateKind,
    RelocateField,
    RelocateCompleted,
    Unmarked,
    PhaseOpen,
    RemsetLen,
    RemsetPreviousLen,
    RemsetIndex,
    RemsetDropped,
    RemsetFlips,
    RemsetFlipped,
    RemsetKept,
    WorkerScanned,
    WorkerCopies,
    MutatorCopies,
    AllocationStalls,
    Healed,
    TlabTop,
    ObjectIds,
    MetadataMaps,
}

fn counter_offset(counter: Counter) -> usize {
    match counter {
        Counter::CapacityActive => super::capacity::ACTIVE,
        Counter::CapacityRequested => super::capacity::REQUESTED,
        Counter::CapacitySealed => super::capacity::SEALED,
        Counter::CapacityReserve => super::capacity::RESERVE,
        Counter::CapacityClamps => super::capacity::CLAMPS,
        Counter::CapacityGrows => super::capacity::GROWS,
        Counter::CapacityShrinks => super::capacity::SHRINKS,
        Counter::CapacitySamples => super::capacity::SAMPLES,
        Counter::CapacityTrace => super::capacity::TRACE,
        Counter::CapacityBlack => super::capacity::BLACK,
        Counter::CapacityFresh => super::capacity::FRESH_BUDGET,

        Counter::StartTarget => super::start_policy::TARGET,
        Counter::StartAllocationRate => super::start_policy::ALLOC_RATE,
        Counter::StartMarkRate => super::start_policy::MARK_RATE,
        Counter::StartRateSamples => super::start_policy::RATE_SAMPLES,
        Counter::StartMarkSamples => super::start_policy::MARK_SAMPLES,
        Counter::StartUpdates => super::start_policy::UPDATES,
        Counter::StartInvalid => super::start_policy::INVALID,
        Counter::StartUnits => super::start_policy::MARK_UNITS,

        Counter::CopyWorkerLost => super::RT_WORKER_LOST,
        Counter::CopyOwnerLost => super::RT_MUTATOR_LOST,
        Counter::AgeThreshold => super::tenuring::THRESHOLD,
        Counter::AgeFrozen => super::tenuring::FROZEN,
        Counter::AgeUp => super::tenuring::UP,
        Counter::AgeDown => super::tenuring::DOWN,
        Counter::AgeYoungSamples => super::tenuring::YOUNG_SAMPLES,
        Counter::AgeFullSamples => super::tenuring::FULL_SAMPLES,
        Counter::AgeUpdates => super::tenuring::UPDATES,
        Counter::AgeYoung => super::tenuring::LAST_YOUNG,
        Counter::AgePromoted => super::tenuring::LAST_PROMOTED,
        Counter::AgeOwnerYoung => super::tenuring::OWNER_YOUNG,
        Counter::AgeWorkerYoung => super::tenuring::WORKER_YOUNG,
        Counter::AgeOwnerPromoted => super::tenuring::OWNER_PROMOTED,
        Counter::AgeWorkerPromoted => super::tenuring::WORKER_PROMOTED,
        Counter::PolicyAllocated => super::old_policy::ALLOCATED,
        Counter::PolicyRetained => super::old_policy::RETAINED,
        Counter::PolicyEMA => super::old_policy::EMA,
        Counter::PolicyTarget => super::old_policy::TARGET,
        Counter::PolicyBase => super::old_policy::BASE,
        Counter::PolicyBudget => super::old_policy::BUDGET,
        Counter::PolicyFull => super::old_policy::FULL,
        Counter::PolicyYoung => super::old_policy::YOUNG,
        Counter::PolicyUpdates => super::old_policy::UPDATES,
        Counter::PolicyDeferred => super::old_policy::DEFERRED,
        Counter::PolicyPeriodic => super::old_policy::PERIODIC,
        Counter::PolicyPressure => super::old_policy::PRESSURE,
        Counter::OldRequest => super::old_cycle::REQUEST,
        Counter::OldCycleChecks => super::old_cycle::CHECKS,
        Counter::OldCycleRetries => super::old_cycle::RETRIES,
        Counter::OldPlannedCycles => super::old_cycle::PLANNED,
        Counter::YoungActive => super::young_mark::ACTIVE,
        Counter::YoungDone => super::young_mark::DONE,
        Counter::YoungEpoch => super::young_mark::EPOCH,
        Counter::YoungMarked => super::young_mark::MARKED,
        Counter::YoungRoot => super::young_mark::ROOT,
        Counter::YoungRootBound => super::young_mark::ROOT_BOUND,
        Counter::YoungRemset => super::young_mark::REMSET,
        Counter::YoungRemsetBound => super::young_mark::REMSET_BOUND,
        Counter::YoungOldIndex => super::young_mark::OLD_INDEX,
        Counter::YoungOldBound => super::young_mark::OLD_BOUND,
        Counter::YoungObject => super::young_mark::OBJECT,
        Counter::YoungField => super::young_mark::FIELD,
        Counter::YoungStackLen => super::young_mark::LEN,
        Counter::YoungWorkerUnits => super::young_mark::WORKER_UNITS,
        Counter::YoungWaits => super::young_mark::WAITS,
        Counter::YoungValid => super::young_mark::VALID,
        Counter::YoungUnmarked => super::young_mark::UNMARKED,
        Counter::AdoptedPhase => {
            super::RT_CTX + std::mem::offset_of!(crate::gc_abi::MutatorContext, adopted_phase)
        }
        Counter::RetireStage => super::RT_RETIRE_STAGE,
        Counter::RetireCursor => super::RT_RETIRE_CURSOR,
        Counter::RetireEnd => super::RT_RETIRE_END,
        Counter::RetireUnits => super::RT_RETIRE_UNITS,
        Counter::RetirePage => super::RT_SCAN_INDEX,
        Counter::MarkPrepareState => super::RT_MP_STATE,
        Counter::MarkReadinessRequests => super::RT_MP_REQUESTS,
        Counter::MarkPrepareCancels => super::RT_MP_CANCELS,
        Counter::MarkValid => super::RT_MARK_VALID,
        Counter::Collections => RT_COLLECTIONS,
        Counter::FullCollections => RT_FULL_COLLECTIONS,
        Counter::Marked => RT_MARKED,
        Counter::MarkCycles => RT_MARK_CYCLES,
        Counter::MarkActive => RT_MARK_ACTIVE,
        Counter::MarkRootIndex => super::RT_M_ROOT_INDEX,
        Counter::MarkRootBound => super::RT_M_ROOT_BOUND,
        Counter::MarkObject => super::RT_M_OBJ,
        Counter::MarkRelocateIndex => super::RT_M_RELOCATE_INDEX,
        Counter::MarkField => super::RT_M_FIELD,
        Counter::MarkStackLen => super::RT_MSTACK_LEN,
        Counter::MarkDrains => super::RT_MARK_DRAINS,
        Counter::CycleChecks => super::RT_CYCLE_CHECKS,
        Counter::CycleRetries => super::RT_CYCLE_RETRIES,
        Counter::ReadyCycles => super::RT_CYCLE_READY,
        Counter::WorkSequence => super::RT_WORK_SEQ,
        Counter::LockTickets => super::RT_LOCK_NEXT,
        Counter::WakePending => super::RT_WAKE_PENDING,
        Counter::RelocateEndChecks => super::RT_END_CHECKS,
        Counter::RelocateEndRetries => super::RT_END_RETRIES,
        Counter::RelocateDrainSlices => super::RT_DR_SLICES,
        Counter::RelocateDrainYields => super::RT_DR_YIELDS,
        Counter::RelocateDrainMax => super::RT_DR_MAX,
        Counter::RelocateWorkerUnits => super::RT_DR_WORKER,
        Counter::RelocateDrainAssisted => super::RT_DR_ASSISTED,
        Counter::MarkDrainSlices => super::RT_DM_SLICES,
        Counter::MarkDrainYields => super::RT_DM_YIELDS,
        Counter::MarkDrainMax => super::RT_DM_MAX,
        Counter::MarkWorkerUnits => super::RT_DM_WORKER,
        Counter::MarkDrainAssisted => super::RT_DM_ASSISTED,
        Counter::MarkDone => super::RT_MARK_DONE,
        Counter::RelocateRootIndex => super::RT_R_ROOT_INDEX,
        Counter::RelocateRootBound => super::RT_R_ROOT_BOUND,
        Counter::WorkerDone => super::RT_WORKER_DONE,
        Counter::TlabPage => {
            super::RT_REQUEST + std::mem::offset_of!(crate::gc_abi::AllocationRequest, page)
        }
        Counter::RelocateKind => super::RT_CS_KIND,
        Counter::RelocateField => super::RT_CS_FIELD,
        Counter::RelocateCompleted => super::RT_CS_COMPLETED,
        Counter::Unmarked => RT_UNMARKED,
        Counter::PhaseOpen => RT_PHASE_OPEN,
        Counter::RemsetLen => RT_REMSET_LEN,
        Counter::RemsetPreviousLen => super::RT_REMSET_PREV_LEN,
        Counter::RemsetIndex => super::RT_RS_INDEX,
        Counter::RemsetDropped => RT_REMSET_DROPPED,
        Counter::RemsetFlips => RT_REMSET_FLIPS,
        Counter::RemsetFlipped => RT_REMSET_FLIPPED,
        Counter::RemsetKept => RT_REMSET_KEPT,
        Counter::WorkerScanned => RT_WORKER_SCANNED,
        Counter::WorkerCopies => RT_WORKER_COPIES,
        Counter::MutatorCopies => super::RT_MUTATOR_COPIES,
        Counter::AllocationStalls => super::RT_ALLOCATION_STALLS,
        Counter::Healed => super::RT_HEALED,
        Counter::TlabTop => {
            super::RT_CTX + std::mem::offset_of!(crate::gc_abi::MutatorContext, tlab_top)
        }
        Counter::ObjectIds => super::RT_NEXT_OBJECT_ID,
        Counter::MetadataMaps => super::metadata::live_offsets()[4] as usize,
    }
}

pub(crate) fn emit_counter<E: PortableAsm>(a: &mut E, counter: Counter) {
    load_rt(a, R::V0, counter_offset(counter));
}
pub(crate) fn store_counter<E: PortableAsm>(a: &mut E, counter: Counter, value: R) {
    store_rt(a, counter_offset(counter), value);
}
/// Bind the explicit owner selected by the legacy startup cell for a harness.
pub(crate) fn bind_primary_owner<E: PortableAsm>(a: &mut E) {
    a.load_ptr_disp32(
        R::OwnerContext,
        R::V3,
        d(std::mem::offset_of!(super::Runtime, primary_owner)),
    );
}

/// The labels a harness program calls.
pub(crate) struct HarnessLabels<T> {
    pub cheney: T,
    pub worker_cheney: T,
    pub publish_pending: T,
    pub alloc: T,
    pub field_store: T,
    pub field_load: T,
    pub root_adopt: T,
    pub root_load: T,
    pub root_pop: T,
    pub root_store: T,
    pub slot_owner: T,
    pub clock: T,
    pub mark_start: T,
    pub young_start: T,
    pub young_loop: T,
    pub mark_loop: T,
    pub mark_end: T,
    pub mark_cycle_poll: T,
    pub mark_ref: T,
    pub mark_prepare_begin: T,
    pub collect: T,
    pub finish: T,
    pub close_relocation: T,
    pub try_close_relocation: T,
    pub quantum: T,
    pub native_enter: T,
    pub native_exit: T,
    pub write_heap_string: T,
    pub sleep_millis: T,
    pub retire_step: T,
    pub worker_forward: T,
    pub lock: T,
    pub unlock: T,
    pub unpark: T,
}

pub(crate) fn harness_labels<T: Copy>(l: &Labels<T>) -> HarnessLabels<T> {
    let w = l.worker.as_ref().expect("the harness needs a worker build");
    HarnessLabels {
        cheney: l.cheney,
        worker_cheney: w.wcheney,
        publish_pending: l.publish_pending,
        alloc: l.alloc,
        field_store: l.field_store,
        field_load: l.field_load,
        root_adopt: l.root_adopt,
        root_load: l.root_load,
        root_pop: l.root_pop,
        root_store: l.root_store,
        slot_owner: l.slot_owner,
        clock: w.hooks.clock,
        mark_start: w.mark_start,
        young_start: w.young.start,
        young_loop: w.young.trace,
        mark_loop: w.mark_loop,
        mark_end: l.mark_end,
        mark_cycle_poll: l.mark_cycle_poll,
        mark_ref: w.mark_ref,
        mark_prepare_begin: l.mark_prepare_begin,
        collect: l.collect,
        finish: l.finish,
        close_relocation: l.finish_relocation,
        try_close_relocation: l.try_finish_relocation,
        quantum: l.quantum,
        native_enter: l.native_enter,
        native_exit: l.native_exit,
        write_heap_string: l.write_heap_string,
        sleep_millis: l.sleep_millis,
        retire_step: l.retire_step,
        worker_forward: w.wforward,
        lock: l.owner_lock,
        unlock: l.owner_unlock,
        unpark: w.hooks.unpark,
    }
}

/// V0 = payload address -> V0 = 1 if its mark bit is set, else 0
/// (V3 = runtime; clobbers V1, V2, V6, V8).
pub(crate) fn emit_marked<E: PortableAsm>(a: &mut E) {
    let unset = a.create_text_label();
    let end = a.create_text_label();
    a.mov_reg_reg(R::V2, R::V0);
    load_rt(a, R::V0, RT_HEAP_BASE);
    a.sub_reg_reg(R::V2, R::V0);
    mark::test_mark(a, R::V2, R::V6, R::V8);
    a.jcc_label(C::Equal, unset);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(end);
    a.bind_text_label(unset);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(end);
}

/// V0 = payload address -> V0 = 1 if it lies in the Old generation.
pub(crate) fn emit_is_old<E: PortableAsm>(a: &mut E) {
    let no = a.create_text_label();
    let end = a.create_text_label();
    load_rt(a, R::V1, RT_HEAP_BASE);
    a.sub_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Below, no);
    super::generation::test(a, R::V0, super::generation::OLD);
    a.jcc_label(C::NotEqual, no);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(end);
    a.bind_text_label(no);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(end);
}

/// V0 = entry `index` of the current remembered set (V3 = runtime;
/// clobbers V1).
pub(crate) fn emit_current_entry<E: PortableAsm>(a: &mut E, index: u32) {
    load_rt(a, R::V1, RT_REMSET_BASE);
    a.add_reg_reg(R::V1, R::V3);
    a.load_ptr_disp32(R::V0, R::V1, (index * 8) as i32);
}

/// Let `mark_loop` run until its stack is empty (V3 = runtime; clobbers V0).
pub(crate) fn emit_unbounded_budget<E: PortableAsm>(a: &mut E) {
    emit_budget(a, u64::MAX);
}

pub(crate) fn emit_budget<E: PortableAsm>(a: &mut E, units: u64) {
    a.mov_imm64(R::V0, units);
    store_rt(a, super::RT_BUDGET, R::V0);
}

pub(crate) fn heap_base_offset() -> i32 {
    d(RT_HEAP_BASE)
}

/// Encode an address for a test's barrier-bypassing store. A negative control
/// bypasses bookkeeping, while preserving the same field representation.
pub(crate) fn emit_encode_reference<E: PortableAsm>(a: &mut E, value: R, scratch: R) {
    super::encode(a, value, scratch);
}

/// Fault injection: leave one free record in an otherwise valid arena. Two
/// real allocations then distinguish metadata exhaustion from heap exhaustion.
pub(crate) fn leave_one_mark_record<E: PortableAsm>(a: &mut E) {
    use crate::gc_abi::MarkArena;
    use std::mem::offset_of;
    load_rt(a, R::V0, super::RT_MARKS + offset_of!(MarkArena, used));
    a.add_reg_imm32(R::V0, 1);
    store_rt(a, super::RT_MARKS + offset_of!(MarkArena, capacity), R::V0);
}

/// Conformance setup only: V0 is a valid ABI snapshot and all actors are
/// quiescent. Keep the registry and sole owner's cache consistent, including
/// calls that enter through an explicitly validated owner boundary. This is
/// not the production phase-publication/retirement protocol.
pub(crate) fn adopt_snapshot<E: PortableAsm>(a: &mut E) {
    use crate::gc_abi::MutatorContext;
    use std::mem::offset_of;
    lea_rt(
        a,
        R::V1,
        super::RT_CTX + offset_of!(MutatorContext, adopted_phase),
    );
    a.atomic_store_release(R::V1, R::V0);
    lea_rt(
        a,
        R::V1,
        super::registered_roots::REGISTRY + offset_of!(crate::gc_abi::RootRegistry, phase),
    );
    a.atomic_store_release(R::V1, R::V0);
}

pub(crate) fn load_access<E: PortableAsm>(a: &mut E) {
    super::colours::load_access(a);
}

pub(crate) fn set_collecting<E: PortableAsm>(a: &mut E, collecting: bool) {
    a.mov_imm64(R::V2, u64::from(collecting));
    store_rt(a, super::RT_COLLECTING, R::V2);
}

/// Model a descheduled live worker for a bounded, deterministic handoff test.
pub(crate) fn set_worker_alive<E: PortableAsm>(a: &mut E, alive: bool) {
    a.mov_imm64(R::V0, u64::from(alive));
    store_rt(a, super::RT_WORKER_ALIVE, R::V0);
}

pub(crate) fn heal_relocated_root<E: PortableAsm>(a: &mut E) {
    super::relocate_roots::heal(a);
}

/// Invalid-context setup for publication rejection tests only.
pub(crate) fn block_phase_publication<E: PortableAsm>(a: &mut E, kind: u64) {
    use crate::gc_abi::MutatorContext;
    let field = if kind == 2 {
        super::RT_PENDING_REF
    } else if kind == 1 {
        super::RT_CTX + std::mem::offset_of!(MutatorContext, access_epoch)
    } else {
        super::RT_CTX + std::mem::offset_of!(MutatorContext, private_producers)
    };
    a.mov_imm64(R::V0, 2);
    store_rt(a, field, R::V0);
}

pub(crate) fn publish_relocation_for_test<E: PortableAsm>(
    a: &mut E,
    labels: &super::Labels<E::TextLabel>,
) {
    super::phase::relocating(a, labels, crate::gc_abi::Phase::RelocateStart);
}

pub(crate) fn heal_marked_slot<E: PortableAsm>(a: &mut E) {
    super::colours::heal_marked_slot(a);
}

pub(crate) fn young_end() -> u64 {
    super::YOUNG_END
}

pub(crate) fn reference_offset<E: PortableAsm>(a: &mut E, value: R, scratch: R, bad: E::TextLabel) {
    super::colours::offset(a, value, scratch, bad);
}

/// Same exact-word repair used by the previous remembered-set consumer.
pub(crate) fn heal_remembered_slot<E: PortableAsm>(a: &mut E, kept: bool) {
    super::remset::heal(a, kept);
}

pub(crate) fn retiring_page<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, super::RT_SCAN_INDEX);
    super::scan::page_ptr(a, R::V8, R::V0, R::V1);
    a.mov_reg_reg(R::V0, R::V8);
}

/// Wake after an explicit test-only owner handoff, without another allocation
/// (which could already adopt completed cleanup on a preempted owner).
pub(crate) fn wake_worker<E: PortableAsm>(a: &mut E, unpark: E::TextLabel) {
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_WAKE_PENDING, R::V0);
    super::lea_rt(a, R::V7, super::RT_WORK_SEQ);
    a.call_label(unpark);
}

/// The fixture holds the collector lock; V0 is a valid heap payload address.
pub(crate) fn enqueue_mark<E: PortableAsm>(a: &mut E, mark_ref: E::TextLabel) {
    a.mov_reg_reg(R::V2, R::V0);
    load_rt(a, R::V1, super::RT_HEAP_BASE);
    a.sub_reg_reg(R::V2, R::V1);
    a.call_label(mark_ref);
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_MARK_DONE, R::V0);
    super::worker::request_work(a);
}
/// Flags indicate whether a worker has already reserved the next lock ticket.
pub(crate) fn waiting_ticket<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, super::RT_LOCK_NEXT);
    load_rt(a, R::V1, super::RT_LOCK_SERVING);
    a.add_reg_imm32(R::V1, 1);
    a.cmp_reg_reg(R::V0, R::V1);
}

/// Admit a late production worker-copy frontier after an earlier empty pass.
/// The fixture holds the collector lock; it does not fabricate object headers.
pub(crate) fn reopen_relocation_work<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V0, 0);
    store_rt(a, super::RT_WORKER_DONE, R::V0);
    super::worker::request_work(a);
}

/// A deliberately stale worker hint, modelling a completed pass followed by a
/// late real barrier copy. Callers hold the lock; no payload is fabricated.
pub(crate) fn completed_relocation_hint<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V0, 1);
    store_rt(a, super::RT_WORKER_DONE, R::V0);
}

/// Simulate a stale mark-complete hint while retaining real pending frontiers.
/// Callers hold the collector lock; the coordinator must validate the hint.
pub(crate) fn completed_mark_hint<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V0, 1);
    store_rt(a, super::RT_MARK_DONE, R::V0);
}
pub(crate) fn mark_trigger_pages() -> u64 {
    super::MARK_TRIGGER_PAGES
}

pub(crate) fn young_mutator_pages<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, super::RT_NEXT_PAGE);
    load_rt(a, R::V1, super::RT_SPACE_BASE);
    a.sub_reg_reg(R::V0, R::V1);
}
pub(crate) fn young_semispace_pages() -> u64 {
    super::HALF
}

pub(crate) fn set_mark_cycles<E: PortableAsm>(a: &mut E, cycles: u64) {
    a.mov_imm64(R::V0, cycles);
    store_rt(a, super::RT_MARK_CYCLES, R::V0);
}

/// Read an actual retained reference as a payload address, without invoking a
/// barrier that could hide a stale queue word. None selects the active receiver.
pub(crate) fn mark_receiver<E: PortableAsm>(a: &mut E, queue: Option<u32>) {
    if let Some(index) = queue {
        load_rt(a, R::V1, super::mark_queue::entries(false));
        a.load_ptr_disp32(R::V0, R::V1, (index * 8) as i32);
    } else {
        load_rt(a, R::V0, super::RT_M_OBJ);
    }
    a.shr_reg_imm8(R::V0, 16);
    load_rt(a, R::V1, super::RT_HEAP_BASE);
    a.add_reg_reg(R::V0, R::V1);
}

/// Inject a temporary allocation ceiling at the actually charged Old bytes.
/// This forces the capacity branch without fabricating used heap positions,
/// headers or accessible pages. Full detach restores actual reservation bounds.
pub(crate) fn request_full_collection<E: PortableAsm>(a: &mut E) {
    super::old_runs::used(a, R::V0);
    a.add_reg_imm32(R::V0, (crate::gc_abi::HEAP_PAGE_BYTES - 1) as i32);
    a.shr_reg_imm8(R::V0, 17);
    let nonempty = a.create_text_label();
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, nonempty);
    a.mov_imm64(R::V0, 1);
    a.bind_text_label(nonempty);
    store_rt(a, super::pool::OLD_PAGES, R::V0);
}

/// A prepared real RootRequest whose new reference was observed white before
/// another actor protected it. Exercise production's fast recertification.
/// V0 = canonical new payload; preserve the existing authentic frame handle.
pub(crate) fn protect_stale_root_reference<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    super::encode(a, R::V0, R::V1);
    a.mov_imm64(R::V1, u64::from(crate::gc_abi::OLD_MARK_MASK));
    a.xor_reg_reg(R::V0, R::V1);
    store_rt(a, super::RT_FRAME_ROOT + super::REQUEST_VALUE, R::V0);
    super::mark_roots::before_store(a, l, super::RT_FRAME_ROOT);
    load_rt(a, R::V0, super::RT_FRAME_ROOT + super::REQUEST_VALUE);
}

pub(crate) fn set_young_auto<E: PortableAsm>(a: &mut E, enabled: bool) {
    a.mov_imm64(R::V0, u64::from(!enabled));
    store_rt(a, super::young_mark::AUTO_DISABLED, R::V0);
}

pub(crate) fn emit_young_marked<E: PortableAsm>(a: &mut E) {
    let no = a.create_text_label();
    let done = a.create_text_label();
    a.mov_reg_reg(R::V2, R::V0);
    load_rt(a, R::V1, super::RT_HEAP_BASE);
    a.sub_reg_reg(R::V2, R::V1);
    super::generation::test(a, R::V2, super::generation::YOUNG);
    a.jcc_label(C::NotEqual, no);
    super::young_mark::test(a, R::V2, R::V6, R::V8);
    a.jcc_label(C::Equal, no);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(no);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
}
pub(crate) fn set_young_epoch<E: PortableAsm>(a: &mut E, epoch: u64) {
    a.mov_imm64(R::V0, epoch);
    store_rt(a, super::young_mark::EPOCH, R::V0);
}

/// Runtime occupancy, not a synthetic capacity/request. V0 = Old bytes in use.
pub(crate) fn old_used<E: PortableAsm>(a: &mut E) {
    super::old_runs::used(a, R::V0);
}
pub(crate) fn request_old_cycle<E: PortableAsm>(a: &mut E) {
    a.mov_imm64(R::V0, 1);
    store_rt(a, super::old_cycle::REQUEST, R::V0);
}

/// Directed controller input test; uses the exact production retarget emitter.
/// No heap header/occupancy is fabricated or claimed by this numerical probe.
pub(crate) fn policy_target<E: PortableAsm>(a: &mut E, retained: u64, ema: u64, previous: u64) {
    for (field, value) in [
        (super::old_policy::RETAINED, retained),
        (super::old_policy::EMA, ema),
        (super::old_policy::TARGET, previous),
    ] {
        a.mov_imm64(R::V0, value);
        store_rt(a, field, R::V0);
    }
    super::old_policy::retarget(a);
}

pub(crate) fn large_retained_pages<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, super::large_object::RETAINED_PAGES);
}

pub(crate) fn set_tenuring<E: PortableAsm>(a: &mut E, threshold: u64) {
    a.mov_imm64(R::V0, threshold);
    store_rt(a, super::tenuring::THRESHOLD, R::V0);
}
/// Numeric controller probe, invoking the same production choice emitter.
pub(crate) fn tenuring_probe<E: PortableAsm>(
    a: &mut E,
    threshold: u64,
    young: u64,
    promoted: u64,
    old: u64,
    up: u64,
    down: u64,
) {
    for (f, v) in [
        (super::tenuring::THRESHOLD, threshold),
        (super::tenuring::LAST_YOUNG, young),
        (super::tenuring::LAST_PROMOTED, promoted),
        (super::tenuring::UP, up),
        (super::tenuring::DOWN, down),
    ] {
        a.mov_imm64(R::V0, v);
        store_rt(a, f, R::V0);
    }
    a.mov_imm64(R::V2, old);
    super::tenuring::choose(a);
}

/// Numerical controller inputs; these probes neither allocate nor claim an
/// observed live heap, clock duration or worker progress.
pub(crate) fn start_policy_input<E: PortableAsm>(a: &mut E, values: [u64; 5]) {
    let fields = [
        super::start_policy::TARGET,
        super::start_policy::ALLOC_RATE,
        super::start_policy::MARK_RATE,
        super::start_policy::MARK_UNITS,
        super::tenuring::LAST_YOUNG,
    ];
    for (f, n) in fields.into_iter().zip(values) {
        a.mov_imm64(R::V0, n);
        store_rt(a, f, R::V0);
    }
    observe_start_policy(a, true, 0);
}
pub(crate) fn start_allocation_sample<E: PortableAsm>(a: &mut E, values: [u64; 4]) {
    for (f, n) in [
        super::start_policy::RATE_NS,
        super::start_policy::RATE_BYTES,
        super::old_policy::ALLOCATED,
    ]
    .into_iter()
    .zip(values)
    {
        a.mov_imm64(R::V0, n);
        store_rt(a, f, R::V0);
    }
    a.mov_imm64(R::V0, values[3]);
    super::start_policy::allocation_sample(a);
}
pub(crate) fn start_mark_sample<E: PortableAsm>(a: &mut E, values: [u64; 4]) {
    for (f, n) in [
        super::start_policy::MARK_NS,
        super::start_policy::MARK_BASE,
        super::young_mark::WORKER_UNITS,
    ]
    .into_iter()
    .zip(values)
    {
        a.mov_imm64(R::V0, n);
        store_rt(a, f, R::V0);
    }
    a.mov_imm64(R::V0, values[3]);
    super::start_policy::mark_sample(a);
}

/// One synthetic observation, or a repeated close with no new measured mark.
pub(crate) fn observe_start_policy<E: PortableAsm>(a: &mut E, new_sample: bool, ns: u64) {
    a.mov_imm64(R::V0, ns);
    store_rt(a, super::start_policy::DURATION_NS, R::V0);
    if new_sample {
        super::copy::bump(a, super::start_policy::MARK_SAMPLES, R::V0);
    }
    super::start_policy::observe(a);
}
pub(crate) fn at_start_trigger<E: PortableAsm>(a: &mut E) {
    let below = a.create_text_label();
    let done = a.create_text_label();
    super::start_policy::below(a, below);
    a.mov_imm64(R::V0, 1);
    a.jmp_label(done);
    a.bind_text_label(below);
    a.mov_imm64(R::V0, 0);
    a.bind_text_label(done);
}

#[path = "test_support_capacity.rs"]
mod capacity_support;
pub(crate) use capacity_support::*;

#[path = "test_support_generation.rs"]
mod generation_support;
pub(crate) use generation_support::*;
#[path = "test_support_owner.rs"]
mod owner_support;
pub(crate) use owner_support::*;

pub(crate) const RETIRE_DONE: u64 = super::retire::DONE;

#[path = "test_support_pool.rs"]
mod pool_support;
pub(crate) use pool_support::*;

#[path = "test_support_old_runs.rs"]
mod old_runs_support;
pub(crate) use old_runs_support::*;

#[path = "test_support_heap.rs"]
mod heap_support;
pub(crate) use heap_support::*;

#[path = "test_support_commit.rs"]
mod commit_support;
pub(crate) use commit_support::*;

/// Deterministic no-native-worker fixtures check the actual OS leaf runs with
/// no outstanding collector ticket. The live-worker path permits other actors
/// to own tickets while a release caller is outside the lock.
pub(crate) fn metadata_release_ticket_check<E: PortableAsm>(a: &mut E) {
    load_rt(a, R::V0, super::RT_LOCK_NEXT);
    load_rt(a, R::V1, super::RT_LOCK_SERVING);
    a.cmp_reg_reg(R::V0, R::V1);
}

/// Current physical arena-bank observations for allocation boundary fixtures.
pub(crate) fn mark_arena_offsets() -> [i32; 3] {
    use crate::gc_abi::MarkArena;
    [
        std::mem::offset_of!(MarkArena, records),
        std::mem::offset_of!(MarkArena, capacity),
        std::mem::offset_of!(MarkArena, used),
    ]
    .map(|field| d(super::RT_MARKS + field))
}

/// Real native boundary metrics; no mock lifecycle state is substituted.
pub(crate) fn native_io_metric<E: PortableAsm>(a: &mut E, metric: u8) {
    let field = match metric {
        0 => super::native_io::ENTRIES,
        1 => super::native_io::EXITS,
        2 => super::native_io::CHUNKS,
        3 => super::native_io::BUFFER_WRITES,
        _ => unreachable!(),
    };
    load_rt(a, R::V0, field);
}
pub(crate) fn native_context_state<E: PortableAsm>(a: &mut E) {
    lea_rt(
        a,
        R::V1,
        super::RT_CTX + std::mem::offset_of!(crate::gc_abi::MutatorContext, state),
    );
    a.atomic_load_acquire_u32(R::V0, R::V1);
}
/// Invalid caller probe. Only a boundary rejection may follow a nonzero input.
pub(crate) fn native_private_probe<E: PortableAsm>(a: &mut E, access: bool) {
    super::store_rt_imm(
        a,
        super::RT_CTX
            + if access {
                std::mem::offset_of!(crate::gc_abi::MutatorContext, access_epoch)
            } else {
                std::mem::offset_of!(crate::gc_abi::MutatorContext, private_producers)
            },
        1,
    );
}

#[path = "test_support_root_transport.rs"]
mod root_transport;
pub(crate) use root_transport::*;

#[path = "test_support_journal_regions.rs"]
mod journal_regions;
pub(crate) use journal_regions::*;

#[path = "test_support_actors.rs"]
mod actors;
pub(crate) use actors::*;

/// Observe a current shared/private field without truncating its address domain.
pub(crate) fn runtime_value<E: PortableAsm>(a: &mut E, offset: usize) {
    load_rt(a, R::V0, offset);
}
pub(crate) fn runtime_store<E: PortableAsm>(a: &mut E, offset: usize, value: R) {
    store_rt(a, offset, value);
}

#[path = "test_support_root_work.rs"]
mod root_obligations;
pub(crate) use root_obligations::*;

#[path = "test_support_worker_journal.rs"]
mod independent_journals;
pub(crate) use independent_journals::*;

#[path = "test_support_captured_journal.rs"]
mod captured_journals;
pub(crate) use captured_journals::*;

#[path = "test_support_pin_adopt.rs"]
mod pin_adopt;
pub(crate) use pin_adopt::*;

#[path = "test_support_scoped_pin.rs"]
mod scoped_pin;
pub(crate) use scoped_pin::*;

#[path = "test_support_pin_abi.rs"]
mod pin_abi;
pub(crate) use pin_abi::*;

#[path = "test_support_native_output.rs"]
mod native_output;
pub(crate) use native_output::*;

#[path = "test_support_thread_api.rs"]
mod thread_api_support;
pub(crate) use thread_api_support::*;

#[path = "test_support_thread_api_threads.rs"]
mod thread_api_threads;
pub(crate) use thread_api_threads::*;

#[path = "test_support_thread_api_probe.rs"]
mod thread_api_probe;
pub(crate) use thread_api_probe::*;

#[path = "test_support_spill_audit.rs"]
mod spill_audit;
pub(crate) use spill_audit::*;
