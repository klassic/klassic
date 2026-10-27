//! Production wiring for `--gc=generational`; see `docs/gc/production-integration-plan.md`.
//!
//! Thin emitted routines that compose the existing generational components
//! (root steps, TLAB allocation, source access, initialization publish,
//! builtin descriptors, root storage) into the entry points the direct
//! backend calls. The legacy path has one autonomous mutator; explicit owners
//! can allocate concurrently while Idle. Peer phase adoption remains unfinished.
//! The collector has Young/Old evacuation,
//! private copy identity metadata and (on Linux) a GC worker. It implements
//! only part of the full generational contract; exhaustion is an honest OOM.
//!
//! Runtime state and initial metadata live in one mapping whose address is kept in a
//! single data cell: the `Runtime` block at offset 0, then the page table,
//! object-start bitmaps, forward tables and cells, the mark-record arena and
//! the root storage. The heap payload is a second mapping; copy metadata is
//! backed by additional mappings installed on demand. Internal routines
//! expect the runtime pointer in `V3` (rbx), which no component touches.
use crate::gc_abi::{
    ABI_VERSION, AllocationInto, AllocationPage, AllocationRequest, AllocationResult, ForwardCell,
    ForwardTable, GuardedMarkPrepare, HEAP_PAGE_BYTES, MarkArena, MarkRecord, MetadataLeaseRequest,
    MutatorContext, ObjectHeader, PAGE_READY, RootCell, RootChange, RootHandle, RootRequest,
    RootSegment, RootStorage, RootStorageSpec, SourceAccessRequest,
};
use crate::portable_asm::{Condition as C, PortableAsm, Reg as R};
use std::mem::{offset_of, size_of};
mod admission;
mod alloc;
mod allocation_records;
mod capacity;
mod collect;
mod colours;
mod commit;
mod copy;
mod copy_claims;
mod counters;
mod detach;
mod drain;
mod enums;
mod fields;
mod generation;
pub(crate) mod geometry;
pub(crate) mod large_pages;
mod owner_state;
mod page_runs;
mod quiescence;
pub(crate) use commit::MemoryLabels;
use geometry::{Layout, build_layout, geometry, layout};
mod cooperative_old;
mod cooperative_young;
mod identity;
mod journal_snapshot;
pub(crate) mod large_object;
mod large_reserve;
mod large_sweep;
mod large_trace;
mod mark;
mod mark_cycle;
mod mark_epoch;
mod mark_frontier;
mod mark_prepare;
mod mark_queue;
mod mark_roots;
pub(crate) mod metadata;
mod metadata_leases;
mod moving_round;
pub(crate) mod mutator_time;
mod native_api;
mod native_buffer;
mod native_completion;
mod native_io;
mod offsets;
mod old_cycle;
mod old_footprint;
mod old_mark;
mod old_policy;
mod old_reserve;
mod old_runs;
mod old_tlab;
mod participants;
mod phase;
mod phase_round;
mod pool;
mod registered_roots;
mod relocate_pages;
mod relocate_roots;
mod remset;
mod remset_buffer;
pub(crate) mod rendezvous_time;
mod retire;
mod root_journal;
mod root_retirement;
mod root_work;
mod roots;
mod satb_buffer;
mod scan;
mod shared_headroom;
mod slot_owner;
mod start_policy;
mod startup;
mod stats;
mod tenuring;
mod thread_api;
mod thread_tls;
pub(crate) mod trace;
mod uncommit;
mod worker_journal;
pub(crate) use stats::stat_offsets;
mod native_roots;
mod pin_abi;
mod pin_adopt;
mod pin_copy;
mod pin_destination;
mod scoped_pin;
#[cfg(test)]
pub(crate) mod test_support;
mod weak;
mod worker;
mod worker_pool;
mod worker_view;
mod young_barrier;
mod young_mark;
mod young_trace;
pub(crate) use fields::emit_untyped_load;
use offsets::*;
pub(crate) use worker::{WorkerHooks, shutdown as shutdown_worker};
/// Native unary request is { key, value, OS thread token }; result is 0 or 6. Create/get write
/// the corresponding request word only on success; no collector lock crosses OS.
pub(crate) struct ThreadTlsOsLabels<T> {
    pub(crate) create: T,
    pub(crate) get: T,
    pub(crate) set: T,
    pub(crate) delete: T,
    pub(crate) identity: T,
}
/// Two banks of fresh-allocation mark metadata, each for the maximum Young
/// window. Physical pages are selected independently from the shared pool.
pub(crate) const PAGES: u64 = 512;
const HALF: u64 = PAGES / 2;
/// Forward cells per page: one per smallest object (32-byte header plus
/// 16-byte minimum payload), so a page can never exhaust its table first.
const CELLS_PER_PAGE: u64 = HEAP_PAGE_BYTES / 48;
/// Object-start bitmap words per page (one bit per 16-byte granule).
const STARTS_WORDS: u64 = HEAP_PAGE_BYTES / 16 / 64;
/// Distinct identity range per page (granules per page).
const IDENTITY_STRIDE: u64 = HEAP_PAGE_BYTES / 16;
/// Frame root cells (LIFO) plus the allocation scratch root.
const ROOT_CELLS: u64 = 1 << 20;
/// Root journal entries; the eager single-owner adapter drains on FLUSH.
const JOURNAL_ENTRIES: u64 = 4096;
/// Largest payload one page can hold.
pub(crate) const MAX_PAYLOAD: u64 = HEAP_PAGE_BYTES - size_of::<ObjectHeader>() as u64;
/// Bytes the print path copies out of the heap per `write`.
pub(crate) const COPY_OUT_BYTES: u64 = 4096;
/// Synthetic owner metadata-lease token for readiness publication; it is
/// only format-checked and never transferred.
const PUBLISH_LEASE_TOKEN: u64 = (1 << 32) | 1;
const ROOT_RESERVE: u64 = 0;
const ROOT_STORE: u64 = 1;
const ROOT_READ_RAW: u64 = 3;
const ROOT_FLUSH: i32 = 3;
const ALLOC_REFILL: i32 = 1;
const ALLOC_METADATA_REFILL: i32 = 4;
const ALLOC_ROOT_FLUSH: i32 = 5;
const SOURCE_ENTER: u64 = 0;
const SOURCE_EXIT: u64 = 1;
/// Color bits of every reference while the adopted phase stays initial.
#[cfg(test)]
pub(super) fn color() -> u64 {
    u64::from(crate::gc_abi::PhaseSnapshot::initial().color())
}

/// Shared heap, collector and controller state at the metadata mapping base.
/// Owner-private storage lives in OwnerState; primary_owner serves legacy entry points.
#[repr(C)]
struct Runtime {
    primary_owner: u64,
    builtins: [u64; 25],
    heap_base: u64,
    commit: commit::State,
    large_pages: large_pages::State,
    trace: trace::State,
    rendezvous_time: rendezvous_time::State,
    large_object: large_object::State,
    pin_destinations: pin_destination::State,
    pin_copies: pin_copy::State,
    pin_adoptions: pin_adopt::State,
    scoped_pins: scoped_pin::State,
    remset_buffer: remset_buffer::State,
    satb_buffer: satb_buffer::State,
    pin_api: crate::gc_abi::ScopedPinApi,
    heap_capacity_bytes: u64,
    old_limit_bytes: u64,
    old_charged_bytes: u64,
    young_limit_pages: u64,
    pages: u64,
    next_page: u64,
    published: u64,
    objects: u64,
    verified: u64,
    pages_used: u64,
    space_base: u64,
    space_end: u64,
    collections: u64,
    scan_index: u64,
    from_base: u64,
    from_end: u64,
    from_marks: u64,
    cs_page: u64,
    cs_off: u64,
    cs_field: u64,
    cs_count: u64,
    /// Retained heap-field cursor: 0 idle, 1 chain A, 2 chain B, 3 Old.
    cs_kind: u64,
    /// Objects completed in this Cheney invocation, excluding root/field work.
    cs_completed: u64,
    old_top: u64,
    old_base: u64,
    old_from: u64,
    old_from_end: u64,
    full: u64,
    full_collections: u64,
    phase_open: u64,
    /// Permanent frame-root relocation frontier, captured before resuming.
    r_root_index: u64,
    r_root_bound: u64,
    /// Root-cell units used by the current Cheney invocation (not objects).
    r_root_work: u64,
    budget: u64,
    ry_lo: u64,
    ry_hi: u64,
    ro_lo: u64,
    ro_hi: u64,
    healed: u64,
    old_scan: u64,
    promoted: u64,
    remset_len: u64,
    /// Next previous-set entry to consume in a relocation quantum.
    rs_index: u64,
    /// Double-buffered remembered sets (G07): byte offset (from the
    /// runtime block) of the *current* set, sealed *previous* set bound
    /// (cleared after all its entries are consumed), and counters.
    remset_base: u64,
    remset_prev_len: u64,
    remset_flips: u64,
    remset_flipped: u64,
    remset_kept: u64,
    /// M-G6 worker: ticket lock, wake sequence (futex word), liveness,
    /// phase-scanned flag, objects scanned by the worker, pending wake,
    /// and the field word a Cheney heal expects (CAS).
    lock_next: u64,
    lock_serving: u64,
    work_seq: u64,
    worker_alive: u64,
    worker_done: u64,
    worker_scanned: u64,
    wake_pending: u64,
    cs_old: u64,
    /// Packed to-space page bounds and collector copy-page chain. Pool workers
    /// share these copy cursors/scratch only while holding the collector ticket;
    /// mutator copying retains its separate owner-local scratch and CAS claims.
    bounds: u64,
    copy_low: u64,
    from_copy_low: u64,
    /// Detached cleanup: 0 idle, 1..16 work, 17 awaits owner Idle adoption.
    retire_stage: u64,
    retire_cursor: u64,
    retire_end: u64,
    retire_units: u64,
    cb_page: u64,
    cb_off: u64,
    progress: u64,
    w_page: u64,
    w_top: u64,
    w_limit: u64,
    w_src: u64,
    w_new: u64,
    w_pin_job: u64,
    w_desc: u64,
    w_promoted: u64,
    w_unmarked: u64,
    worker_copies: u64,
    worker_lost: u64,
    /// Set while the mutator copies into its TLAB: the newest chain-A
    /// object may still be incomplete, so the worker must not scan past it.
    /// Checked aggregate of actual owner copying attempts, not a boolean.
    m_copying: u64,
    old_mark: old_mark::State,
    pool: pool::State,
    old_runs: old_runs::State,
    old_policy: old_policy::State,
    tenuring: tenuring::State,
    start_policy: start_policy::State,
    capacity: capacity::State,
    young_mark: young_mark::State,
    drain: drain::Stats,
    unmarked: u64,
    remset_dropped: u64,
    lit_mark: u64,
    /// Monotonic across page reuse; relocation copies preserve their source ID.
    next_object_id: u64,
    marks: MarkArena,
    /// Checked monotonic identity of the active fresh-record bank.
    mark_revision: u64,
    metadata: metadata::State,
    heap_soft_maximum: u64,
    participants: participants::State,
    quiescence: quiescence::State,
    root_work: root_work::State,
    native_borrows: u64,
    native_completion: native_completion::State,
    moving_round: moving_round::State,
    phase_round: phase_round::State,
    cooperative_old: cooperative_old::State,
    cooperative_young: cooperative_young::State,
    relocate_pages: relocate_pages::State,
    worker_journal: worker_journal::State,
    registered_roots: registered_roots::State,
    /// Stable unregistered worker identities and their independent OS lifetimes.
    worker_pool: worker_pool::State,
    metadata_leases: metadata_leases::State,
    weak: weak::State,
    mark_queue: mark_queue::State,
    native_roots: native_roots::State,
    root_retirement: root_retirement::State,
    thread_tls: thread_tls::State,
    thread_api: crate::gc_abi::NativeThreadApi,
}

use owner_state::OwnerState;

const _: () = assert!(offset_of!(Runtime, primary_owner) == 0);
const _: () = assert!(size_of::<u64>() * 25 == 200); // BuiltinDescriptors
const _: () = assert!(offset_of!(MutatorContext, abi_version) == 0);
const _: () = assert!(offset_of!(MutatorContext, state) == 4);
const _: () = assert!(size_of::<RootCell>() == 32);
const _: () = assert!(size_of::<AllocationPage>() == 112);

const fn round_up(value: u64, align: u64) -> u64 {
    value.div_ceil(align) * align
}

const L: Layout = build_layout(192 << 20);
/// Former Young boundary, retained only to define the unchanged total heap
/// reservation and directed cutoff probes. Pool placement does not use it.
pub(super) const YOUNG_END: u64 = (PAGES + 1) * HEAP_PAGE_BYTES;
/// Historical 192-MiB standalone-fixture geometry. Production limits come
/// from the configured geometry, including every physical metadata array.
pub(super) const OLD_HALF: u64 = 64 << 20;
pub(super) const OLD_BYTES: u64 = 2 * OLD_HALF;
pub(super) const HEAP_BYTES: u64 = YOUNG_END + OLD_BYTES;
/// Capacity of each of the two remembered sets (current/previous): Old
/// fields that hold Young payload addresses.
pub(super) const REMSET_ENTRIES: u64 = 1 << 20;
/// Mark bitmap: one bit per 16-byte heap granule.
/// Mark stack capacity (each object is pushed at most once per cycle).
pub(super) const MSTACK_ENTRIES: u64 = 128;
/// Bootstrap threshold before measured mark-start feedback is available.
#[cfg(test)]
pub(super) const MARK_TRIGGER_PAGES: u64 = HALF / 4;
const _: () = assert!(L.total < i32::MAX as u64);
const _: () = assert!(L.pages >= size_of::<Runtime>() as u64);
const _: () = assert!(L.pages < L.starts && L.starts < L.tables && L.tables < L.cells);
const _: () = assert!(L.cells < L.marks && L.marks < L.roots && L.roots < L.total);
const _: () =
    assert!(L.starts.is_multiple_of(8) && L.tables.is_multiple_of(8) && L.cells.is_multiple_of(8));
const _: () =
    assert!(L.marks.is_multiple_of(8) && L.roots.is_multiple_of(8) && L.total.is_multiple_of(4096));
const _: () = assert!(HEAP_BYTES.is_multiple_of(4096));
// One forward cell per smallest possible object on a page.
const _: () = assert!(CELLS_PER_PAGE * 48 <= HEAP_PAGE_BYTES);

fn d(offset: usize) -> i32 {
    assert!(
        !owner_state::private(offset),
        "owner offset requires runtime address helpers"
    );
    i32::try_from(offset).expect("runtime offset fits disp32")
}
fn imm(value: u64) -> i32 {
    i32::try_from(value).expect("layout constant fits imm32")
}

/// Root cells and objects per allocation while relocation is open.
const QUANTUM_OBJECTS: u64 = 64;

const HANDLE_CELL: usize = offset_of!(RootRequest, handle) + offset_of!(RootHandle, cell);
const HANDLE_GENERATION: usize =
    offset_of!(RootRequest, handle) + offset_of!(RootHandle, reuse_generation);
const HANDLE_SCOPE: usize = offset_of!(RootRequest, handle) + offset_of!(RootHandle, scope);
const REQUEST_VALUE: usize = offset_of!(RootRequest, value);

/// Offset of the copy-out buffer inside the runtime block.
pub(crate) fn copy_out_offset() -> usize {
    owner_state::offset(offset_of!(OwnerState, copy_out))
}

/// Entry points and leaf subroutines, all created by the backend.
pub(crate) struct Labels<T> {
    /// `() -> ()`: map and prepare the runtime; preserves rbx.
    pub startup: T,
    /// Legacy `gc_alloc` ABI: V7 = payload bytes, V6 = legacy tag ->
    /// V0 = payload address. Clobbers the caller-saved registers.
    pub alloc: T,
    /// Explicit registered OwnerContext entry; derives its shared heap.
    pub(crate) alloc_actor: T,
    satb_slot_owner: T,
    satb_record_old: T,
    satb_record_young: T,
    satb_flush: T,
    satb_flush_locked: T,
    remset_buffer_record: T,
    remset_buffer_flush: T,
    remset_buffer_flush_locked: T,
    pub(crate) attach_actor: T,
    pub(crate) metadata_lease_root_actor: T,
    pub(crate) metadata_lease_release_actor: T,
    pub(crate) metadata_lease_resolve_actor: T,
    pub(crate) metadata_lease_retarget_actor: T,
    pin_destination_reserve_resolved: T,
    pin_destination_watch_resolved: T,
    pin_adopt_small: T,
    pub(crate) scoped_pin_new_actor: T,
    pub(crate) scoped_pin_end_running_actor: T,
    pub(crate) scoped_pin_abi_call: T,
    pub(crate) scoped_pin_abi_scope: T,
    pub(crate) scoped_pin_step_actor: T,
    pub(crate) scoped_pin_release_actor: T,
    pub(crate) scoped_pin_callback_return_actor: T,
    pub(crate) scoped_pin_callback_enter_actor: T,
    pub(crate) scoped_pin_native_exit_actor: T,
    pub(crate) scoped_pin_native_enter_actor: T,
    pub(crate) pin_destination_watch_actor: T,
    pub(crate) pin_destination_reserve_actor: T,
    pub(crate) pin_destination_cancel_actor: T,
    native_root_validate: T,
    native_root_resolve: T,
    pub(crate) native_root_new_actor: T,
    pub(crate) native_root_load_actor: T,
    pub(crate) native_root_release_actor: T,
    metadata_native_release: T,
    root_clear_chunk: T,
    root_retired_clear_one: T,
    root_scan_leased: T,
    pub(crate) root_retire_actor: T,
    pub(crate) detach_begin_actor: T,
    pub(crate) detach_step_actor: T,
    pub(crate) detach_dispose_actor: T,
    pub(crate) tls_initialize: T,
    pub(crate) tls_query: T,
    pub(crate) tls_query_thread: T,
    pub(crate) thread_api_call: T,
    pub(crate) tls_current: T,
    pub(crate) tls_prepare_attach: T,
    pub(crate) tls_lookup_begin: T,
    pub(crate) tls_lookup_step: T,
    pub(crate) tls_install_actor: T,
    pub(crate) tls_clear_actor: T,
    pub(crate) tls_destroy: T,
    pub(crate) tls_os: ThreadTlsOsLabels<T>,
    root_retirement_step: T,
    pub(crate) weak_new_actor: T,
    pub(crate) weak_load_actor: T,
    pub(crate) weak_release_actor: T,
    pub(crate) weak_grow_service: T,
    pub(crate) lease_grow_service: T,
    pub(crate) queue_grow_services: [T; 2],
    weak_clear: T,
    weak_rebind: T,
    metadata_lease_step: T,
    metadata_reference_step: T,
    pub(crate) poll_actor: T,
    pub(crate) checkpoint_actor: T,
    pub(crate) old_start_actor: T,
    pub(crate) old_end_actor: T,
    pub(crate) dual_mark_enable: T,
    pub(crate) moving_enable_actor: T,
    pub(crate) moving_auto_enable_actor: T,
    pub(crate) moving_poll_actor: T,
    pub(crate) moving_start_observed_actor: T,
    pub(crate) moving_end_observed_actor: T,
    #[cfg(test)]
    pub(crate) moving_scheduler_request_hook: Option<T>,
    #[cfg(test)]
    pub(crate) moving_scheduler_return_hook: Option<T>,
    #[cfg(test)]
    pub(crate) moving_scheduler_finish_hook: Option<T>,
    pub(crate) moving_start_actor: T,
    pub(crate) moving_end_actor: T,
    #[cfg(test)]
    pub(crate) moving_selected_hook: Option<T>,
    pub(crate) young_start_actor: T,
    pub(crate) young_end_actor: T,
    pub(crate) young_start_observed_actor: T,
    pub(crate) young_end_observed_actor: T,
    pub(crate) old_start_observed_actor: T,
    pub(crate) old_end_observed_actor: T,
    pub(crate) cooperative_old_enable: T,
    pub(crate) cooperative_old_poll: T,
    pub(crate) cooperative_old_only_poll: T,
    pub(crate) cooperative_young_poll: T,
    #[cfg(test)]
    pub(crate) scheduler_request_hook: Option<T>,
    #[cfg(test)]
    pub(crate) phase_resume_hook: Option<T>,
    #[cfg(test)]
    pub(crate) phase_mark_gate: Option<T>,
    #[cfg(test)]
    pub(crate) young_mark_gate: Option<T>,
    #[cfg(test)]
    pub(crate) young_scheduler_request_hook: Option<T>,
    #[cfg(test)]
    pub(crate) young_observed_entry_hook: Option<T>,
    #[cfg(test)]
    pub(crate) worker_epoch_hook: Option<T>,
    #[cfg(test)]
    pub(crate) copy_claim_begin_hook: Option<T>,
    #[cfg(test)]
    pub(crate) copy_claim_end_hook: Option<T>,
    #[cfg(test)]
    pub(crate) copy_ready_hook: Option<T>,
    #[cfg(test)]
    pub(crate) worker_before_lock_hook: Option<T>,
    #[cfg(test)]
    pub(crate) journal_worker_gate: Option<T>,
    #[cfg(test)]
    invariant_hook: Option<T>,
    #[cfg(test)]
    oom_hook: Option<T>,
    #[cfg(test)]
    alloc_after_collect_hook: Option<T>,
    owner_checkpoint_poll: T,
    /// V0 = frame slot address. Roots the raw address (or null) the slot
    /// holds in a fresh LIFO cell and leaves the cell pointer in the slot.
    /// Preserves every register.
    pub root_adopt: T,
    pub(crate) root_adopt_actor: T,
    /// Clear and pop the top LIFO cell. Preserves every register.
    pub root_pop: T,
    pub(crate) root_pop_actor: T,
    /// V0 = payload address (or null), V1 = frame slot address. Stores
    /// through the slot's cell. Preserves every register.
    pub root_store: T,
    pub(crate) root_store_actor: T,
    /// V0 = cell pointer -> V0 = payload address (or null), via READ_RAW.
    /// Preserves every other register.
    pub root_load: T,
    pub(crate) root_load_actor: T,
    /// Sole-owner native boundary; preserves all portable registers.
    pub native_enter: T,
    pub native_exit: T,
    pub(crate) native_enter_actor: T,
    pub(crate) native_api_bootstrap: T,
    pub(crate) native_api_call: T,
    pub(crate) native_exit_actor: T,
    /// V0=nonnegative milliseconds; root-only Native with poll/adoption.
    /// Preserves all portable registers, including V0.
    pub sleep_millis: T,
    /// V0=canonical string payload, V7=fd; preserves all portable registers.
    /// Copy chunks to native storage before publishing BlockedNative.
    pub write_heap_string: T,
    /// V0=string payload, V7=fd; a safepoint. Own a temporary RootCell, keep
    /// offset rather than a raw cursor, and reload between native copy chunks.
    pub write_rooted_string: T,
    /// V7=fd, V6=unmanaged buffer, V2=len -> V0=write result. Safepoint;
    /// caller roots every live managed value. Other portable regs survive.
    pub write_native_buffer: T,
    /// Publish the pending object, then walk every used page and count
    /// objects / published headers into the runtime stats. Clobbers the
    /// caller-saved registers.
    pub verify: T,
    /// `(V7 = bytes) -> V0 = zeroed mapping or 0` (backend supplied).
    pub map: T,
    pub metadata_map: T,
    pub metadata_reclaim: T,
    pub memory: MemoryLabels<T>,
    ensure_heap_page: T,
    pub(crate) uncommit_heap_page: T,
    uncommit_poll: T,
    heap_commit_failed: T,
    large_pages_failed: T,
    heap_decommit_failed: T,
    step: T,
    tlab_page: T,
    alloc_into: T,
    source_access: T,
    publish: T,
    builtin_prepare: T,
    root_storage_prepare: T,
    root_life_step: T,
    root_walk_step: T,
    root_walk_item: T,
    root_journal_step: T,
    root_journal_seal: T,
    root_journal_seal_owner: T,
    root_journal_pressure: T,
    root_journal_pressure_region: T,
    root_journal_drain_region: T,
    root_journal_protected: T,
    worker_journal_step: T,
    worker_journal_consume_region: T,
    #[cfg(test)]
    alloc_before_black_hook: Option<T>,
    #[cfg(test)]
    weak_work_hook: Option<T>,
    #[cfg(test)]
    weak_publish_hook: Option<T>,
    #[cfg(test)]
    weak_end_enter_hook: Option<T>,
    owner_lock: T,
    owner_unlock: T,
    owner_unlock_raw: T,
    participant_step: T,
    publish_pending: T,
    alloc_raw: T,
    collect: T,
    collect_moving: T,
    forward: T,
    cheney: T,
    finish: T,
    finish_relocation: T,
    try_finish_relocation: T,
    retire_step: T,
    slot_owner: T,
    remembered_owner: T,
    mark_prepare_begin: T,
    mark_end: T,
    mark_cycle_poll: T,
    quantum: T,
    /// Heap-field load: V0 = reference bits -> V0 = payload address.
    /// Preserves every other register.
    pub field_load: T,
    pub(crate) field_load_actor: T,
    /// Heap-field store (legacy `gc_store_pointer` ABI): V7 = field
    /// address, V6 = payload address or null.
    pub field_store: T,
    pub(crate) field_store_actor: T,
    /// Legacy `gc_deep_equal` ABI: V7, V6 = payload addresses -> V0 bool.
    pub deep_equal: T,
    /// V0 = address of a native cell holding a payload address (or null):
    /// evacuate its referent and rewrite the cell. Called from `literals`.
    pub forward_cell_entry: T,
    /// Synchronous external-root hook (AArch64 raw shadow roots). Direct x86
    /// adopts literals into permanent RootCells at startup, so its hook is empty.
    pub literals: T,
    /// External hook enumerates raw roots rather than adopted RootCells.
    pub raw_roots: bool,
    drain: T,
    fail: T,
    oom: T,
    native_oom: T,
    metadata_oom: T,
    metadata_budget_oom: T,
    metadata_release_failed: T,
    relocation_oom: T,
    too_large: T,
    old_full: T,
    /// Recursive body of `deep_equal`.
    deep_equal_body: T,
    /// M-G6 GC worker thread (None: the single-threaded incremental path).
    worker: Option<worker::WorkerLabels<T>>,
}

/// Labels the backend already calls, bound to M-G routines under the flag.
pub(crate) struct Backend<T> {
    pub field_store: T,
    pub deep_equal: T,
    pub literals: T,
    /// External hook enumerates raw roots rather than adopted RootCells.
    pub raw_roots: bool,
}

impl<T: Copy> Labels<T> {
    /// `map` and `alloc` are backend labels: the mapping leaf and the
    /// existing allocation entry every site already calls.
    pub(crate) fn new(
        mut create: impl FnMut() -> T,
        map: T,
        alloc: T,
        backend: Backend<T>,
    ) -> Self {
        Self {
            startup: create(),
            alloc,
            alloc_actor: create(),
            satb_slot_owner: create(),
            satb_record_old: create(),
            satb_record_young: create(),
            satb_flush: create(),
            satb_flush_locked: create(),
            remset_buffer_record: create(),
            remset_buffer_flush: create(),
            remset_buffer_flush_locked: create(),
            attach_actor: create(),
            metadata_lease_root_actor: create(),
            metadata_lease_release_actor: create(),
            metadata_lease_resolve_actor: create(),
            metadata_lease_retarget_actor: create(),
            pin_destination_reserve_resolved: create(),
            pin_destination_watch_resolved: create(),
            pin_adopt_small: create(),
            scoped_pin_new_actor: create(),
            scoped_pin_end_running_actor: create(),
            scoped_pin_abi_call: create(),
            scoped_pin_abi_scope: create(),
            scoped_pin_step_actor: create(),
            scoped_pin_release_actor: create(),
            scoped_pin_callback_return_actor: create(),
            scoped_pin_callback_enter_actor: create(),
            scoped_pin_native_exit_actor: create(),
            scoped_pin_native_enter_actor: create(),
            pin_destination_watch_actor: create(),
            pin_destination_reserve_actor: create(),
            pin_destination_cancel_actor: create(),
            native_root_validate: create(),
            native_root_resolve: create(),
            native_root_new_actor: create(),
            native_root_load_actor: create(),
            native_root_release_actor: create(),
            metadata_native_release: create(),
            root_clear_chunk: create(),
            root_retired_clear_one: create(),
            root_scan_leased: create(),
            root_retire_actor: create(),
            detach_begin_actor: create(),
            detach_step_actor: create(),
            detach_dispose_actor: create(),
            tls_initialize: create(),
            tls_query: create(),
            tls_query_thread: create(),
            thread_api_call: create(),
            tls_current: create(),
            tls_prepare_attach: create(),
            tls_lookup_begin: create(),
            tls_lookup_step: create(),
            tls_install_actor: create(),
            tls_clear_actor: create(),
            tls_destroy: create(),
            tls_os: ThreadTlsOsLabels {
                create: create(),
                get: create(),
                set: create(),
                delete: create(),
                identity: create(),
            },
            root_retirement_step: create(),
            weak_new_actor: create(),
            weak_load_actor: create(),
            weak_release_actor: create(),
            weak_grow_service: create(),
            lease_grow_service: create(),
            queue_grow_services: [create(), create()],
            weak_clear: create(),
            weak_rebind: create(),
            metadata_lease_step: create(),
            metadata_reference_step: create(),
            poll_actor: create(),
            checkpoint_actor: create(),
            old_start_actor: create(),
            old_end_actor: create(),
            dual_mark_enable: create(),
            moving_enable_actor: create(),
            moving_auto_enable_actor: create(),
            moving_poll_actor: create(),
            moving_start_observed_actor: create(),
            moving_end_observed_actor: create(),
            #[cfg(test)]
            moving_scheduler_request_hook: None,
            #[cfg(test)]
            moving_scheduler_return_hook: None,
            #[cfg(test)]
            moving_scheduler_finish_hook: None,
            moving_start_actor: create(),
            moving_end_actor: create(),
            #[cfg(test)]
            moving_selected_hook: None,
            young_start_actor: create(),
            young_end_actor: create(),
            young_start_observed_actor: create(),
            young_end_observed_actor: create(),
            old_start_observed_actor: create(),
            old_end_observed_actor: create(),
            cooperative_old_enable: create(),
            cooperative_old_poll: create(),
            cooperative_old_only_poll: create(),
            cooperative_young_poll: create(),
            #[cfg(test)]
            scheduler_request_hook: None,
            #[cfg(test)]
            phase_resume_hook: None,
            #[cfg(test)]
            phase_mark_gate: None,
            #[cfg(test)]
            young_mark_gate: None,
            #[cfg(test)]
            young_scheduler_request_hook: None,
            #[cfg(test)]
            young_observed_entry_hook: None,
            #[cfg(test)]
            worker_epoch_hook: None,
            #[cfg(test)]
            copy_claim_begin_hook: None,
            #[cfg(test)]
            copy_claim_end_hook: None,
            #[cfg(test)]
            copy_ready_hook: None,
            #[cfg(test)]
            worker_before_lock_hook: None,
            #[cfg(test)]
            journal_worker_gate: None,
            #[cfg(test)]
            invariant_hook: None,
            #[cfg(test)]
            oom_hook: None,
            #[cfg(test)]
            alloc_after_collect_hook: None,
            owner_checkpoint_poll: create(),
            root_adopt: create(),
            root_adopt_actor: create(),
            root_pop: create(),
            root_pop_actor: create(),
            root_store: create(),
            root_store_actor: create(),
            root_load: create(),
            root_load_actor: create(),
            native_enter: create(),
            native_exit: create(),
            native_enter_actor: create(),
            native_api_bootstrap: create(),
            native_api_call: create(),
            native_exit_actor: create(),
            sleep_millis: create(),
            write_heap_string: create(),
            write_rooted_string: create(),
            write_native_buffer: create(),
            verify: create(),
            map,
            metadata_map: create(),
            metadata_reclaim: create(),
            memory: MemoryLabels {
                reserve: create(),
                commit: create(),
                decommit: create(),
                release: create(),
            },
            ensure_heap_page: create(),
            uncommit_heap_page: create(),
            uncommit_poll: create(),
            heap_commit_failed: create(),
            large_pages_failed: create(),
            heap_decommit_failed: create(),
            step: create(),
            tlab_page: create(),
            alloc_into: create(),
            source_access: create(),
            publish: create(),
            builtin_prepare: create(),
            root_storage_prepare: create(),
            root_life_step: create(),
            root_walk_step: create(),
            root_walk_item: create(),
            root_journal_step: create(),
            root_journal_seal: create(),
            root_journal_seal_owner: create(),
            root_journal_pressure: create(),
            root_journal_pressure_region: create(),
            root_journal_drain_region: create(),
            root_journal_protected: create(),
            worker_journal_step: create(),
            worker_journal_consume_region: create(),
            #[cfg(test)]
            alloc_before_black_hook: None,
            #[cfg(test)]
            weak_work_hook: None,
            #[cfg(test)]
            weak_publish_hook: None,
            #[cfg(test)]
            weak_end_enter_hook: None,
            owner_lock: create(),
            owner_unlock: create(),
            owner_unlock_raw: create(),
            participant_step: create(),
            publish_pending: create(),
            alloc_raw: create(),
            collect: create(),
            collect_moving: create(),
            forward: create(),
            cheney: create(),
            finish: create(),
            finish_relocation: create(),
            try_finish_relocation: create(),
            retire_step: create(),
            slot_owner: create(),
            remembered_owner: create(),
            mark_prepare_begin: create(),
            mark_end: create(),
            mark_cycle_poll: create(),
            quantum: create(),
            field_load: create(),
            field_load_actor: create(),
            field_store: backend.field_store,
            field_store_actor: create(),
            deep_equal: backend.deep_equal,
            forward_cell_entry: create(),
            literals: backend.literals,
            raw_roots: backend.raw_roots,
            drain: create(),
            fail: create(),
            oom: create(),
            native_oom: create(),
            metadata_oom: create(),
            metadata_budget_oom: create(),
            metadata_release_failed: create(),
            relocation_oom: create(),
            too_large: create(),
            old_full: create(),
            deep_equal_body: create(),
            worker: None,
        }
    }

    /// Run relocation on a GC worker thread spawned through `hooks`.
    pub(crate) fn with_worker(mut self, hooks: WorkerHooks<T>, create: impl FnMut() -> T) -> Self {
        self.worker = Some(worker::WorkerLabels::new(hooks, create));
        self
    }

    /// Entry of the worker thread (the backend's `spawn` jumps here).
    pub(crate) fn worker_entry(&self) -> Option<T> {
        self.worker.as_ref().map(|w| w.entry)
    }

    /// The backend hooks the worker was configured with.
    pub(crate) fn worker_hooks(&self) -> Option<&WorkerHooks<T>> {
        self.worker.as_ref().map(|w| &w.hooks)
    }
}

pub(crate) const FAIL_TEXT: &[u8] = b"klassic gc: --gc=generational runtime invariant violated\n";
pub(crate) const OOM_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational Young run capacity exhausted)\n";
pub(crate) const LARGE_PAGES_FAILED_TEXT: &[u8] =
    b"klassic gc: required large-page commit failed (--gc-large-pages=required)\n";
pub(crate) const HEAP_COMMIT_FAILED_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational heap commit failed)\n";
pub(crate) const HEAP_DECOMMIT_FAILED_TEXT: &[u8] =
    b"klassic gc: --gc=generational heap uncommit failed\n";
pub(crate) const NATIVE_OOM_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational native mapping failed)\n";
pub(crate) const METADATA_BUDGET_OOM_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational metadata budget exhausted)\n";
pub(crate) const METADATA_RELEASE_FAILED_TEXT: &[u8] =
    b"klassic gc: native metadata release failed (--gc=generational)\n";
pub(crate) const METADATA_OOM_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational allocation metadata exhausted)\n";
pub(crate) const RELOCATION_OOM_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational worker relocation to-space exhausted)\n";
pub(crate) const OLD_FULL_TEXT: &[u8] =
    b"klassic gc: out of memory (--gc=generational Old run capacity exhausted)\n";
pub(crate) const TOO_LARGE_TEXT: &[u8] =
    b"klassic gc: object exceeds --gc=generational Old span capacity\n";

/// Data the routines read: the cell holding the runtime pointer and the
/// three diagnostics above.
pub(crate) struct Data<D> {
    pub runtime: D,
    pub fail_text: D,
    pub oom_text: D,
    pub native_oom_text: D,
    pub heap_commit_failed_text: D,
    pub large_pages_failed_text: D,
    pub heap_decommit_failed_text: D,
    pub metadata_oom_text: D,
    pub metadata_budget_oom_text: D,
    pub metadata_release_failed_text: D,
    pub relocation_oom_text: D,
    pub too_large_text: D,
    pub old_full_text: D,
}

fn load_runtime<E: PortableAsm>(a: &mut E, runtime: E::DataLabel) {
    a.mov_data_addr(R::V3, runtime);
    a.load_ptr_disp32(R::V3, R::V3, 0);
    a.load_ptr_disp32(
        R::OwnerContext,
        R::V3,
        d(offset_of!(Runtime, primary_owner)),
    );
}
/// Diagnostic address domain; never a signed displacement into the wrong base.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct RuntimeField(usize);
pub(crate) fn load_diagnostic<E: PortableAsm>(a: &mut E, dst: R, shared: R, field: RuntimeField) {
    if owner_state::private(field.0) {
        a.load_ptr_disp32(dst, shared, d(offset_of!(Runtime, primary_owner)));
        a.load_ptr_disp32(dst, dst, rt_disp(field.0));
    } else {
        a.load_ptr_disp32(dst, shared, d(field.0));
    }
}
fn rt_base(offset: usize) -> R {
    if owner_state::private(offset) {
        R::OwnerContext
    } else {
        R::V3
    }
}
fn rt_disp(offset: usize) -> i32 {
    d(owner_state::raw(offset))
}
fn load_rt<E: PortableAsm>(a: &mut E, dst: R, offset: usize) {
    a.load_ptr_disp32(dst, rt_base(offset), rt_disp(offset));
}
fn store_rt<E: PortableAsm>(a: &mut E, offset: usize, src: R) {
    a.store_ptr_disp32(rt_base(offset), rt_disp(offset), src);
}
fn lea_rt<E: PortableAsm>(a: &mut E, dst: R, offset: usize) {
    a.mov_reg_reg(dst, rt_base(offset));
    a.add_reg_imm32(dst, rt_disp(offset));
}
fn store_rt_imm<E: PortableAsm>(a: &mut E, offset: usize, value: u64) {
    a.mov_imm64(R::V0, value);
    store_rt(a, offset, R::V0);
}
/// Store the actual shared/private field address, never an encoded offset.
fn store_rt_addr<E: PortableAsm>(a: &mut E, offset: usize, field: usize) {
    lea_rt(a, R::V0, field);
    store_rt(a, offset, R::V0);
}

/// `status = step(ctx, request, op)`, draining the journal and retrying on
/// FLUSH; any other non-OK status is a fatal invariant failure.
fn root_step<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>, request: usize, op: u64) {
    if op == ROOT_STORE && request != RT_ALLOC_ROOT {
        young_barrier::root_store(a, l, request);
        mark_roots::before_store(a, l, request);
    }
    let retry = a.create_text_label();
    let done = a.create_text_label();
    a.bind_text_label(retry);
    lea_rt(a, R::V7, RT_CTX);
    lea_rt(a, R::V6, request);
    a.mov_imm64(R::V2, op);
    a.call_label(l.step);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, done);
    a.cmp_reg_imm8(R::V0, ROOT_FLUSH as i8);
    a.jcc_label(C::NotEqual, l.fail);
    load_rt(a, R::V0, request + offset_of!(RootRequest, segment));
    a.load_ptr_disp32(R::V0, R::V0, d(offset_of!(RootSegment, registration)));
    a.call_label(l.root_journal_drain_region);
    a.jmp_label(retry);
    a.bind_text_label(done);
}

/// Tag bit marking an immortal pointer outside the managed heap (static
/// enum constants in native data). Heap references always have bit 0
/// clear (their metadata is a phase color), so the tag is unambiguous; such
/// values are carried verbatim and never traced or moved.
pub(super) const EXTERNAL: i32 = 1;

/// `value` (a payload address or null) -> reference bits, in place.
fn encode<E: PortableAsm>(a: &mut E, value: R, scratch: R) {
    let null = a.create_text_label();
    let external = a.create_text_label();
    a.test_reg_reg(value, value);
    a.jcc_label(C::Equal, null);
    load_rt(a, scratch, RT_HEAP_BASE);
    a.sub_reg_reg(value, scratch);
    a.jcc_label(C::Below, external);
    a.mov_imm64(scratch, geometry(a).heap_bytes);
    a.cmp_reg_reg(value, scratch);
    a.jcc_label(C::AboveOrEqual, external);
    a.shl_reg_imm8(value, 16);
    colours::load_current(a, scratch);
    a.or_reg_reg(value, scratch);
    a.jmp_label(null);
    a.bind_text_label(external);
    load_rt(a, scratch, RT_HEAP_BASE);
    a.add_reg_reg(value, scratch);
    a.mov_imm64(scratch, EXTERNAL as u64);
    a.or_reg_reg(value, scratch);
    a.bind_text_label(null);
}

/// Reference bits (or null, or an external tagged pointer) -> address.
fn decode<E: PortableAsm>(a: &mut E, value: R, scratch: R) {
    let null = a.create_text_label();
    let external = a.create_text_label();
    a.test_reg_reg(value, value);
    a.jcc_label(C::Equal, null);
    a.bt_reg_imm8(value, 0);
    a.jcc_label(C::Below, external);
    a.shr_reg_imm8(value, 16);
    load_rt(a, scratch, RT_HEAP_BASE);
    a.add_reg_reg(value, scratch);
    a.jmp_label(null);
    a.bind_text_label(external);
    a.mov_imm64(scratch, !1);
    a.and_reg_reg(value, scratch);
    a.bind_text_label(null);
}

/// Emit every M-G1 routine except `map`.
pub(crate) fn emit<E: PortableAsm>(
    a: &mut E,
    l: &Labels<E::TextLabel>,
    data: &Data<E::DataLabel>,
    stderr_fd: u64,
    stress: bool,
) {
    emit_leaves(a, l);
    root_journal::emit(a, l);
    worker_journal::emit(a, l);
    root_retirement::emit(a, l);
    detach::emit(a, l);
    thread_tls::emit(a, l);
    owner_state::emit_attach(a, l);
    commit::emit(a, l);
    metadata::emit(a, l);
    mark_queue::emit(a, l);
    uncommit::emit(a, l);
    worker::emit_worker(a, l, data.runtime);
    startup::emit_startup(a, l, data.runtime);
    alloc::emit_alloc(a, l, data.runtime, stress);
    collect::emit_collect(a, l);
    enums::emit_enums(a, l, data.runtime);
    copy::emit_forwards(a, l);
    scan::emit_cheneys(a, l);
    mark::emit_mark(a, l);
    slot_owner::emit(a, l);
    remset_buffer::emit(a, l);
    satb_buffer::emit(a, l);
    alloc::emit_publish_pending(a, l);
    roots::emit_roots(a, l, data.runtime);
    native_io::emit(a, l, data.runtime);
    native_api::emit(a, l);
    thread_api::emit(a, l);
    native_buffer::emit(a, l, data.runtime);
    metadata_leases::emit(a, l);
    weak::emit(a, l);
    native_roots::emit(a, l);
    pin_destination::emit(a, l);
    pin_adopt::emit(a, l);
    scoped_pin::emit(a, l);
    pin_abi::emit(a, l);
    moving_round::emit(a, l);
    cooperative_old::emit(a, l, stress);
    roots::emit_verify(a, l, data.runtime);
    // Pressure callers can be in an allocation or inside a locked scan/copy.
    // Consume one record without yielding, then retry the actual producer.
    a.bind_text_label(l.root_journal_drain_region);
    lea_rt(a, R::V1, registered_roots::registration(a));
    a.cmp_reg_reg(R::V0, R::V1);
    a.jcc_label(C::Equal, l.drain);
    // Other regions use their own real journal. Preserve the target through
    // ownership checks; no SPSC fast assumption is made for this storage.
    a.push_reg(R::V8);
    a.mov_reg_reg(R::V8, R::V0);
    let regional_owned = a.create_text_label();
    load_rt(a, R::V0, root_journal::OWNER_LOCKED);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Equal, regional_owned);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    worker::lock(a, l);
    a.mov_reg_reg(R::V0, R::V8);
    a.call_label(l.root_journal_pressure_region);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.pop_reg(R::V8);
    a.ret();
    a.bind_text_label(regional_owned);
    a.mov_reg_reg(R::V0, R::V8);
    a.call_label(l.root_journal_pressure_region);
    a.pop_reg(R::V8);
    a.ret();
    a.bind_text_label(l.drain);
    let needs_work = a.create_text_label();
    a.call_label(l.root_journal_protected);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::Equal, needs_work);
    a.ret();
    a.bind_text_label(needs_work);
    let owned = a.create_text_label();
    load_rt(a, R::V0, root_journal::OWNER_LOCKED);
    a.cmp_reg_imm8(R::V0, 1);
    a.jcc_label(C::Equal, owned);
    a.test_reg_reg(R::V0, R::V0);
    a.jcc_label(C::NotEqual, l.fail);
    worker::lock(a, l);
    a.call_label(l.root_journal_pressure);
    worker::unlock(a, l);
    worker::wake_if_pending(a, l);
    a.ret();
    a.bind_text_label(owned);
    a.call_label(l.root_journal_pressure);
    a.ret();
    for (index, (label, text, len)) in [
        (l.fail, data.fail_text, FAIL_TEXT.len()),
        (l.oom, data.oom_text, OOM_TEXT.len()),
        (l.native_oom, data.native_oom_text, NATIVE_OOM_TEXT.len()),
        (
            l.large_pages_failed,
            data.large_pages_failed_text,
            LARGE_PAGES_FAILED_TEXT.len(),
        ),
        (
            l.metadata_release_failed,
            data.metadata_release_failed_text,
            METADATA_RELEASE_FAILED_TEXT.len(),
        ),
        (
            l.metadata_budget_oom,
            data.metadata_budget_oom_text,
            METADATA_BUDGET_OOM_TEXT.len(),
        ),
        (
            l.heap_commit_failed,
            data.heap_commit_failed_text,
            HEAP_COMMIT_FAILED_TEXT.len(),
        ),
        (
            l.heap_decommit_failed,
            data.heap_decommit_failed_text,
            HEAP_DECOMMIT_FAILED_TEXT.len(),
        ),
        (
            l.metadata_oom,
            data.metadata_oom_text,
            METADATA_OOM_TEXT.len(),
        ),
        (
            l.relocation_oom,
            data.relocation_oom_text,
            RELOCATION_OOM_TEXT.len(),
        ),
        (l.too_large, data.too_large_text, TOO_LARGE_TEXT.len()),
        (l.old_full, data.old_full_text, OLD_FULL_TEXT.len()),
    ]
    .into_iter()
    .enumerate()
    {
        a.bind_text_label(label);
        #[cfg(test)]
        if index == 0
            && let Some(hook) = l.invariant_hook
        {
            a.call_label(hook);
        }
        #[cfg(test)]
        if index == 1
            && let Some(hook) = l.oom_hook
        {
            a.call_label(hook);
        }
        #[cfg(not(test))]
        let _ = index;
        a.plat_write_data(stderr_fd, text, len);
        a.plat_exit(1);
    }
}

fn emit_leaves<E: PortableAsm>(a: &mut E, l: &Labels<E::TextLabel>) {
    use crate::gc_codegen::{descriptors, heap, root_lifetime, root_storage, roots};
    a.bind_text_label(l.metadata_reference_step);
    root_lifetime::reference_lease::emit_reference_lease_tracked(
        a,
        &mut |a| metadata_leases::mirror(a),
        &mut |a| metadata_leases::full(a, l),
    );
    a.ret();
    a.bind_text_label(l.metadata_lease_step);
    root_lifetime::metadata::emit_metadata_step_tracked(
        a,
        &mut |a| metadata_leases::mirror(a),
        &mut |a| metadata_leases::full(a, l),
    );
    a.ret();
    a.bind_text_label(l.step);
    roots::emit_step_tracked(a, &mut |a| root_work::publish(a, l));
    a.ret();
    a.bind_text_label(l.tlab_page);
    heap::emit_tlab_page_locked(a);
    a.ret();
    a.bind_text_label(l.alloc_into);
    root_lifetime::alloc_into::emit_alloc_into_tracked(a, &mut |a| root_work::publish(a, l));
    a.ret();
    a.bind_text_label(l.source_access);
    root_lifetime::source_access::emit_source_access(a);
    a.ret();
    a.bind_text_label(l.publish);
    root_lifetime::initialization::emit_publish(a);
    a.ret();
    a.bind_text_label(l.builtin_prepare);
    descriptors::emit_builtin_prepare(a);
    a.ret();
    a.bind_text_label(l.root_storage_prepare);
    root_storage::emit_prepare(a);
    a.ret();
    a.bind_text_label(l.root_clear_chunk);
    root_lifetime::emit_clear_chunk(a);
    a.ret();
    a.bind_text_label(l.root_retired_clear_one);
    root_lifetime::emit_clear_chunk_bounded(a, 1);
    a.ret();
    a.bind_text_label(l.root_scan_leased);
    root_lifetime::emit_scan_leased(a);
    a.ret();
    a.bind_text_label(l.root_life_step);
    root_lifetime::emit_life_locked(a);
    a.ret();
    a.bind_text_label(l.root_walk_step);
    root_lifetime::walk::emit_walk_locked(a);
    a.ret();
    a.bind_text_label(l.root_walk_item);
    root_lifetime::walk::emit_walk_item(a);
    a.ret();
    a.bind_text_label(l.root_journal_step);
    root_lifetime::root_journal::emit_root_journal_tracked(a, &mut |a| {
        root_work::ack(a, l, Some(R::V0))
    });
    a.ret();
    a.bind_text_label(l.participant_step);
    super::handshake::emit_step_locked(a);
    a.ret();
}

/// Absolute first-worker offset retained for existing diagnostic fixtures.
#[cfg(test)]
pub(crate) fn linux_worker_offset() -> i32 {
    d(worker_pool::slot(0)) + worker_slot_linux_offset()
}

/// OS callbacks receive a stable slot pointer in OwnerContext. These offsets
/// are relative to that pointer, not to shared Runtime or the primary owner.
pub(crate) fn worker_slot_linux_offset() -> i32 {
    d(offset_of!(worker_pool::Slot, linux))
}
pub(crate) fn worker_slot_windows_offsets() -> (i32, i32) {
    (
        d(offset_of!(worker_pool::Slot, windows_event)),
        d(offset_of!(worker_pool::Slot, windows_thread)),
    )
}
pub(crate) fn worker_slot_macos_offset() -> i32 {
    d(offset_of!(worker_pool::Slot, macos))
}
pub(crate) fn worker_slot_runtime_offset() -> i32 {
    d(offset_of!(OwnerState, shared_runtime))
}
#[cfg(test)]
pub(crate) fn worker_slot_identity_offset() -> i32 {
    d(offset_of!(worker_pool::Slot, identity))
}
#[cfg(test)]
pub(crate) fn worker_slot_size() -> usize {
    size_of::<worker_pool::Slot>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_color_encodes_valid_references() {
        let reference = crate::gc_abi::encode_ref(HEAP_PAGE_BYTES + 32, color() as u16)
            .expect("initial-phase color is valid reference metadata");
        assert_eq!(reference.bits(), ((HEAP_PAGE_BYTES + 32) << 16) | color());
    }
}
