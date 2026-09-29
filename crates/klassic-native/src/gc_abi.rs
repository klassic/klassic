//! Generational collector ABI v4. Independent of the legacy pointer encoding.
//!
//! Addresses returned by decoding are integers, not permission to dereference.
//! Heap access must also validate committed object-start metadata and hold the
//! runtime's finite no-safepoint access guard. Phase transitions here construct
//! proposals; the coordinator must establish the publication/retirement gates.
//!
//! Implemented finite allocation/root transfer contracts (C/shared emitter):
//!
//! | Operation | May safepoint | Result ownership |
//! | --- | --- | --- |
//! | `weak_release_locked` | No | Weak handle invalidated; metadata returned; existing strong roots unchanged |
//! | `weak_create_step_locked` / C `weak_table_new_locked` | No (retry outside locks) | Registered source root retained; indexed weak handle owns metadata only |
//! | `weak_table_step_locked` LOAD / C `weak_table_load_locked` | No (retry outside locks) | Registered output root + root journal |
//! | `weak_table_step_locked` RELEASE / C `weak_table_release_locked` | No | Serial invalidated and index returned to free list |
//! | `weak_mark_step_locked` / C `weak_mark_end_locked` | No (wait/assist outside lock) | Gate closed through rendezvous; adopted RETRY reopens, SELECT awaits weak processing |
//! | `guarded_weak_liveness` / C `weak_mark_observe` | No (guard held) | Read-only decision; caller retains guard and independent metadata lifetime |
//! | C `weak_process` BEGIN/ENTER/COMMIT/CANCEL | No (retry outside lock) | Private metadata lease; serial-bound slot clear/retarget; cleanup retained on failure |
//! | C `weak_process` OBSERVE/EXIT | No (guard held, outside locks) | Liveness decision then guard release; no strong publication |
//! | C `weak_scan` BEGIN/NEXT/FINISH/ABORT | No (outside guards) | Exclusive scan claim; one-slot progress; covered FINISH reopens, ABORT retains closure |
//! | C `weak_scan_relocate_begin_locked` | No (retry outside common lock) | Covered scan transfers only after successful relocation BEGIN; WAIT/failure retains weak closure |
//! | `weak_follow_step_locked` / C `weak_table_follow_locked` | No (common weak/heap lock, outside guards) | One forwarding edge transfers slot metadata; failed source release retains separate cleanup token |
//! | `weak_incoming_step_locked` BEGIN/NEXT | No (common weak/heap lock, outside guards) | One-slot incoming audit; findings cannot be skipped; DONE covers slots, not cleanup leases |
//! | weak_load_into_locked | No (retry outside locks) | Registered output root + root journal; weak slot retains metadata |
//! | `gc_try_tlab_allocate` | No | Private initializing reference; adopt before polling |
//! | `gc_try_tlab_allocate_into` | No | Pre-reserved output root plus publication journal |
//! | `gc_guarded_initialization_publish` | No | Release readiness; retain guard, lease and producer obligations |
//! | `gc_root_step(PREPARE_EMPTY)` | No | No mutation; valid only through the same owner interval |
//! | `gc_root_step(READ_RAW)` | No | Unresolved bits, not a managed address |
//!
//! Refill/flush are returned to the caller, never executed inside these leaves.
//! Rooted allocation does not finish payload initialization or authorize payload
//! scanning. The ordinary compiler/runtime still requires migration to this ABI.
//!
//! C weak-table backing adapters run outside locks/guards in native state:
//!
//! | Operation | May block | Result ownership |
//! | --- | --- | --- |
//! | `gc_native_weak_prepare` | Yes (OS map) | Private reserved charge; commit before publication |
//! | `gc_native_weak_committed` | No | Borrowed table, valid only while backing owner remains live |
//! | `gc_native_weak_dispose` | Yes (OS unmap) | Failure retains mapping and charge; requires shutdown quiescence |

use std::sync::atomic::{AtomicU32, AtomicU64};

pub const ABI_VERSION: u32 = 4;
pub const MAX_AGE: u64 = 15;
pub const OBJECT_ALIGNMENT: u64 = 16;
pub const MAX_OFFSET: u64 = (1 << 48) - 1;
pub const REMEMBERED_MASK: u16 = 0x0030;
pub const YOUNG_MARK_MASK: u16 = 0x0300;
pub const OLD_MARK_MASK: u16 = 0x0c00;
pub const REMAP_MASK: u16 = 0xf000;
pub const METADATA_MASK: u16 = REMEMBERED_MASK | YOUNG_MARK_MASK | OLD_MARK_MASK | REMAP_MASK;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum GcError {
    InvalidSize,
    InvalidReference,
    OutOfMemory,
    UnsupportedConfig,
    InvalidPhase,
    EpochExhausted,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct GcRef(u64);

impl GcRef {
    pub const NULL: Self = Self(0);

    pub const fn bits(self) -> u64 {
        self.0
    }

    pub fn from_bits(bits: u64) -> Result<Self, GcError> {
        if bits == 0 {
            Ok(Self::NULL)
        } else {
            encode_ref(bits >> 16, bits as u16)
        }
    }
}

fn valid_metadata(metadata: u16) -> bool {
    metadata & !METADATA_MASK == 0
        && metadata & REMEMBERED_MASK != 0
        && [YOUNG_MARK_MASK, OLD_MARK_MASK, REMAP_MASK]
            .into_iter()
            .all(|mask| (metadata & mask).count_ones() == 1)
}

pub fn encode_ref(offset: u64, metadata: u16) -> Result<GcRef, GcError> {
    if offset == 0
        || offset > MAX_OFFSET
        || !offset.is_multiple_of(OBJECT_ALIGNMENT)
        || !valid_metadata(metadata)
    {
        return Err(GcError::InvalidReference);
    }
    Ok(GcRef((offset << 16) | u64::from(metadata)))
}

/// Arithmetic decoding only. Never use its result as a managed address before
/// validating heap metadata (`decode_ref_checked`) and acquiring access rights.
pub fn decode_ref(value: GcRef, heap_base: usize) -> Result<usize, GcError> {
    if value == GcRef::NULL {
        return Ok(0);
    }
    GcRef::from_bits(value.bits())?;
    if !heap_base.is_multiple_of(OBJECT_ALIGNMENT as usize) {
        return Err(GcError::InvalidReference);
    }
    let offset = usize::try_from(value.bits() >> 16).map_err(|_| GcError::InvalidReference)?;
    heap_base
        .checked_add(offset)
        .ok_or(GcError::InvalidReference)
}

/// The validator must prove that this offset is a committed object start (or
/// retained forwarding source), never padding/interior/free space. It must run
/// under the same metadata lifetime lease as subsequent address use.
pub fn decode_ref_checked(
    value: GcRef,
    heap_base: usize,
    is_committed_object_start: impl FnOnce(u64) -> bool,
) -> Result<usize, GcError> {
    let address = decode_ref(value, heap_base)?;
    if value != GcRef::NULL && !is_committed_object_start(value.bits() >> 16) {
        return Err(GcError::InvalidReference);
    }
    Ok(address)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Generation {
    Young = 0,
    Old = 1,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Phase {
    Idle = 0,
    MarkStart = 1,
    ConcurrentMark = 2,
    MarkEndCheck = 3,
    Select = 4,
    RelocateStart = 5,
    ConcurrentRelocate = 6,
    Retire = 7,
}

impl Phase {
    fn from_bits(bits: u8) -> Result<Self, GcError> {
        match bits {
            0 => Ok(Self::Idle),
            1 => Ok(Self::MarkStart),
            2 => Ok(Self::ConcurrentMark),
            3 => Ok(Self::MarkEndCheck),
            4 => Ok(Self::Select),
            5 => Ok(Self::RelocateStart),
            6 => Ok(Self::ConcurrentRelocate),
            7 => Ok(Self::Retire),
            _ => Err(GcError::InvalidPhase),
        }
    }

    pub fn marking(self) -> bool {
        matches!(
            self,
            Self::MarkStart | Self::ConcurrentMark | Self::MarkEndCheck
        )
    }

    pub fn permits(self, next: Self) -> bool {
        matches!(
            (self, next),
            (Self::Idle, Self::MarkStart)
                | (Self::MarkStart, Self::ConcurrentMark)
                | (Self::ConcurrentMark, Self::MarkEndCheck)
                | (Self::MarkEndCheck, Self::ConcurrentMark | Self::Select)
                | (Self::Select, Self::RelocateStart)
                | (Self::RelocateStart, Self::ConcurrentRelocate)
                | (Self::ConcurrentRelocate, Self::Retire)
                | (Self::Retire, Self::Idle)
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Access {
    Load,
    Mark,
    /// Tests the *previous slot value*. Null never certifies a remembered slot.
    Store,
}

/// One atomic release/acquire word: young phase [0:3], old phase [4:7],
/// epochs [8:12] = young mark, old mark, young remap, old remap, remset;
/// [13:15] reserved, publication sequence [16:63]. Sequence must never wrap.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PhaseSnapshot(u64);

impl PhaseSnapshot {
    pub const fn initial() -> Self {
        Self(0)
    }

    pub fn new(young: Phase, old: Phase, epochs: u8, sequence: u64) -> Result<Self, GcError> {
        if epochs >= 32 || sequence > MAX_OFFSET {
            return Err(GcError::InvalidPhase);
        }
        Ok(Self(
            (sequence << 16) | (u64::from(epochs) << 8) | ((old as u64) << 4) | young as u64,
        ))
    }

    pub fn from_bits(bits: u64) -> Result<Self, GcError> {
        if bits & 0xe000 != 0 {
            return Err(GcError::InvalidPhase);
        }
        Self::new(
            Phase::from_bits((bits & 0xf) as u8)?,
            Phase::from_bits(((bits >> 4) & 0xf) as u8)?,
            ((bits >> 8) & 31) as u8,
            bits >> 16,
        )
    }

    pub const fn bits(self) -> u64 {
        self.0
    }

    pub fn phase(self, generation: Generation) -> Phase {
        Phase::from_bits(((self.0 >> (generation as u8 * 4)) & 0xf) as u8)
            .expect("validated phase snapshot")
    }

    pub fn color(self) -> u16 {
        let epochs = (self.0 >> 8) as u16;
        (1 << (4 + ((epochs >> 4) & 1)))
            | (1 << (8 + (epochs & 1)))
            | (1 << (10 + ((epochs >> 1) & 1)))
            | (1 << (12 + ((epochs >> 2) & 3)))
    }

    /// GC-forgotten slot: neither parity may certify a future store fast path.
    /// Both bits stay bad through arbitrary parity wraps until a real store
    /// completes the remembered-set bookkeeping for the current epoch.
    pub fn color_unremembered(self) -> u16 {
        self.color() | REMEMBERED_MASK
    }

    /// Color comparison only; reaching a good color requires completion of the
    /// barrier's marking/remapping/remembering obligations before publication.
    pub fn required_mask(self, generation: Generation, access: Access) -> u16 {
        if access == Access::Store {
            return METADATA_MASK;
        }
        let mut mask = REMAP_MASK;
        if self.phase(Generation::Old).marking()
            || (access == Access::Mark && generation == Generation::Old)
        {
            mask |= OLD_MARK_MASK;
        }
        if generation == Generation::Young
            && (self.phase(Generation::Young).marking() || access == Access::Mark)
        {
            mask |= YOUNG_MARK_MASK;
        }
        mask
    }

    pub fn is_good(self, reference: GcRef, generation: Generation, access: Access) -> bool {
        if reference == GcRef::NULL {
            return access != Access::Store;
        }
        let mask = self.required_mask(generation, access);
        reference.bits() as u16 & mask == self.color() & mask
    }

    /// Construct a transition proposal. Runtime publication additionally needs
    /// the remset handoff, mark termination or retirement certificate appropriate
    /// to the edge. This method alone does not authorize epoch/address reuse.
    pub fn transition(self, generation: Generation, next: Phase) -> Result<Self, GcError> {
        if !self.phase(generation).permits(next) {
            return Err(GcError::InvalidPhase);
        }
        let sequence = (self.0 >> 16) + 1;
        if sequence > MAX_OFFSET {
            return Err(GcError::EpochExhausted);
        }
        let mut epochs = ((self.0 >> 8) & 31) as u8;
        if next == Phase::MarkStart {
            epochs ^= 1 << generation as u8;
            if generation == Generation::Young {
                epochs ^= 16;
            }
        }
        if next == Phase::RelocateStart {
            epochs ^= 1 << (2 + generation as u8);
        }
        let (young, old) = match generation {
            Generation::Young => (next, self.phase(Generation::Old)),
            Generation::Old => (self.phase(Generation::Young), next),
        };
        Self::new(young, old, epochs, sequence)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ObjectId(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct TypeDescId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum ThreadState {
    Running = 0,
    AtSafepoint = 1,
    BlockedNative = 2,
    Detaching = 3,
    Detached = 4,
}

/// A nonmoving cell address plus reuse generation and owning scope ID.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct RootHandle {
    pub cell: u64,
    pub reuse_generation: u64,
    pub scope: u64,
}

/// Nonmoving cell. Only value is shared with scanners. Generation/active are
/// private to the owner until retirement; storage cannot be reused while leased.
#[repr(C)]
pub struct RootCell {
    pub value: AtomicU64,
    pub reuse_generation: u64,
    pub active: u64,
    pub reserved: u64,
}

pub const ROOT_SEGMENT_PREPARED: u64 = 0;
pub const ROOT_SEGMENT_CLEANING: u64 = 3;
pub const ROOT_SEGMENT_ACTIVE: u64 = 1;
pub const ROOT_SEGMENT_RETIRING: u64 = 2;

/// Prepared native storage. Owner and globally unique scope remain immutable
/// through retirement. The owner alone appends/reserves cells. Scanners acquire
/// used before reading cells and hold a storage lease until their jobs finish.
#[repr(C)]
pub struct RootSegment {
    pub owner: u64,
    pub scope: u64,
    pub cells: u64,
    pub capacity: u64,
    pub used: AtomicU64,
    pub state: AtomicU64,
    pub journal: u64,
    pub registration: u64,
}

/// Registry-lock protected scope/lease IDs, membership and retirement queue.
/// Phase changes require the coordinator's root/job handoff protocol.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootRegistry {
    pub last_scope: u64,
    pub last_lease: u64,
    pub active: u64,
    pub retiring: u64,
    pub leases: u64,
    pub head: u64,
    pub phase: u64,
    pub inventory: u64,
    pub walkers: u64,
}

/// One stable registration per segment. Metadata is registry-lock protected;
/// cleanup_cursor alone is written outside the lock by the exclusive cleaner.
#[repr(C)]
pub struct RootRegistration {
    pub segment: u64,
    pub scope: u64,
    pub registered: u64,
    pub leases: u64,
    pub limit: u64,
    pub cursor: u64,
    pub phase: u64,
    pub required_mask: u64,
    pub claim_serial: u64,
    pub claim_end: u64,
    pub previous: u64,
    pub next: u64,
    pub cleanup_cursor: AtomicU64,
    pub inventory_previous: u64,
    pub inventory_next: u64,
    pub context: u64,
    pub owner_previous: u64,
    pub owner_next: u64,
    pub journal_serial: u64,
    pub journal_end: u64,
}

/// Unique, noncopied control storage, with a serial to reject stale job tokens
/// after this control slot is reused. Kind: inactive/reader/deletion/cleaner/journal.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootLease {
    pub registration: u64,
    pub scope: u64,
    pub serial: u64,
    pub kind: u64,
    pub cursor: u64,
}

#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootLifeRequest {
    pub registration: u64,
    pub lease: u64,
    pub serial: u64,
    pub scope: u64,
    pub context: u64,
    pub begin: u64,
    pub end: u64,
    pub mask: u64,
    pub phase: u64,
}

/// Stable native addresses for a bounded detach step under the shared registry lock.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct DetachRequest {
    pub roots: u64,
    pub handshake: u64,
    pub participant: u64,
}

/// One owner drives this noncopied iterator. The current lease pins its list
/// cursor, so adjacent removal can repair links without freeing that cursor.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootWalk {
    pub registry: u64,
    pub phase: u64,
    pub active: u64,
    pub limit: u64,
    pub cursor: u64,
    pub mask: u64,
    pub completed_mask: u64,
    pub lease: RootLease,
    /// Counted in-flight snapshot. NEXT cannot release the walk while set.
    pub pending: u64,
    pub pending_reference: u64,
    pub pending_observed: u64,
    pub pending_index: u64,
}

#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootWalkScan {
    pub walk: u64,
    pub serial: u64,
    pub output: u64,
    pub capacity: u64,
    pub written: u64,
}
/// Same-object root repair under a live walk lease; preserves observed colors.
/// Caller retains both referents and excludes source/address reuse.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootWalkHeal {
    pub walk: u64,
    pub serial: u64,
    pub index: u64,
    pub expected: u64,
    pub desired: u64,
}
/// Unique walk owner captures/acknowledges one durable root snapshot.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootWalkItem {
    pub walk: u64,
    pub serial: u64,
    pub index: u64,
}

/// SPSC journal entries retain values, never a borrowed/reusable RootHandle.
/// Both references remain phase-lifetime holders until the consumer processes
/// them. Scope is diagnostic identity, not an address to dereference.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootChange {
    pub previous: u64,
    pub next: u64,
    pub scope: u64,
    pub phase: u64,
}

/// Finite storage operation, no polling. Reserve returns handle; raw read
/// returns an unresolved GcRef in value. Store input must already be protected.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct RootRequest {
    pub segment: u64,
    pub handle: RootHandle,
    pub value: u64,
}

/// Caller-owned in-flight scan job. The output references and segment borrow
/// remain protected until mark/remap processing finishes, including after DONE.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootScan {
    pub segment: u64,
    pub cursor: u64,
    pub output: u64,
    pub capacity: u64,
    pub written: u64,
}

/// Checked scanner entry for registered storage. The lease belongs to this job
/// until all copied references are processed or transferred to protected work.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootScanJob {
    pub scan: RootScan,
    pub lease: u64,
    pub serial: u64,
}

/// A sole journal consumer copies into its protected in-flight job before
/// publishing consumption. Empty ring is not by itself mark termination.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootDrain {
    pub buffer: u64,
    pub output: u64,
    pub capacity: u64,
    pub written: u64,
}

/// Exclusive journal consumer: CLAIM/ACK under registry lock, COPY outside it.
/// The lease remains counted and ring slots stay occupied until ACK certifies
/// that both sides of every copied change have been handed to counted work.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RootJournalRequest {
    pub registration: u64,
    pub lease: u64,
    pub serial: u64,
    pub scope: u64,
    pub begin: u64,
    pub end: u64,
    pub output: u64,
    pub capacity: u64,
    pub written: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct FieldLoc {
    pub owner: RootHandle,
    pub byte_offset: u64,
}

/// Enqueued owner value, never a borrowed/reusable RootHandle. The lease keeps
/// metadata alive, not the owner strongly reachable. Consumers release it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct BufferedSlot {
    pub owner: GcRef,
    pub byte_offset: u64,
    pub page_incarnation: u64,
    pub record_epoch: u64,
    pub metadata_lease: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct BarrierEntry {
    pub previous: GcRef,
    pub slot: BufferedSlot,
}

/// Sole consumer transfers complete entries (including lease obligations) into
/// protected native job storage before publishing consumed. No graph traversal.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct BarrierDrain {
    pub buffer: u64,
    pub output: u64,
    pub capacity: u64,
    pub written: u64,
}

/// Mutator owns [published, top); worker may consume only entries made visible
/// by a release store to published. Storage is native, bounded and nonmoving.
#[repr(C)]
pub struct BufferCursor {
    pub entries: u64,
    pub capacity: u64,
    pub top: u64,
    pub published: AtomicU64,
    pub consumed: AtomicU64,
}

/// Prefix of a fresh, zeroed native mapping; trailing cells and journal entries
/// are sized by RootStorageSpec. Publication remains a separate operation.
#[repr(C)]
pub struct RootStorage {
    pub registration: RootRegistration,
    pub segment: RootSegment,
    pub journal: BufferCursor,
}
#[repr(C)]
pub struct RootStorageSpec {
    pub owner: u64,
    pub cells: u64,
    pub entries: u64,
    pub bytes: u64,
}

/// Fixed-width addresses keep Rust, C and the emitted 64-bit ABIs identical.
/// Private fields (TLAB/cursor top) are accessed only by the owning mutator;
/// transfer to workers requires the phase adoption protocol, not plain reads.
#[repr(C)]
pub struct MutatorContext {
    pub abi_version: u32,
    pub state: AtomicU32,
    pub thread_id: u64,
    pub adopted_phase: AtomicU64,
    pub tlab_start: u64,
    pub tlab_top: u64,
    pub tlab_end: u64,
    pub satb: BufferCursor,
    pub remembered: BufferCursor,
    /// Registry-lock protected head of this owner's active RootRegistrations.
    pub root_segments: u64,
    /// Registry-lock protected count, retaining this context through cleanup.
    pub retiring_roots: u64,
    pub access_epoch: AtomicU64,
    /// Completed root publications. Compare an audit's pre-traversal sample
    /// only with the owner stopped by the handshake; never reset or wrap.
    pub root_revision: AtomicU64,
    pub satb_consumer: BufferConsumer,
    pub remembered_consumer: BufferConsumer,
    pub root_audit_serial: [u64; 2],
    pub root_audit_identity: [u64; 2],
    pub root_audit_revision: [u64; 2],
    /// Cumulative successful allocation bytes including header/padding.
    /// Sole owner publishes; acquire readers treat u64::MAX as saturated.
    pub allocated_bytes: AtomicU64,
    pub private_producers: AtomicU64,
}

/// Unique observer state; a retained context and its identity bind the baseline.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct AllocationSample {
    pub context: u64,
    pub identity: u64,
    pub timestamp_ns: u64,
    pub total_bytes: u64,
    pub elapsed_ns: u64,
    pub delta_bytes: u64,
}

/// A serial-bound inventory audit. The embedded walk owns its cursor lease;
/// context is a borrowed input used only for a stopped-owner certificate check.
#[repr(C)]
pub struct RootAudit {
    pub serial: u64,
    pub last_scope: u64,
    pub phase: u64,
    pub state: u64,
    pub context: u64,
    pub queue: u64,
    pub epoch: u64,
    pub generation: u64,
    pub walk: RootWalk,
}

/// Read-only end-check inputs, valid only in this active rendezvous generation.
#[derive(Debug, Default)]
#[repr(C)]
pub struct MarkEndRequest {
    pub handshake: u64,
    pub audit: u64,
    pub generation: u64,
    pub remembered: u64,
}

/// Exclusive coordinator state for prepared mark-start/end phase adoption.
#[derive(Debug, Default)]
#[repr(C)]
pub struct MarkPhaseControl {
    pub handshake: u64,
    pub audit: u64,
    pub generation: u64,
    pub state: u64,
    pub rendezvous: u64,
    pub phase: u64,
    pub epoch: u64,
    pub outcome: u64,
    pub remembered: u64,
}

/// Nonmoving precise slot maps, serialized by the registry lock.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RememberedSet {
    pub start: u64,
    pub limit: u64,
    pub incarnation: u64,
    pub bits: [u64; 2],
    pub words: u64,
    pub current: u64,
    pub epoch: u64,
    pub previous_epoch: u64,
    pub last_phase: u64,
    pub current_count: u64,
    pub pending: u64,
    pub cursor: u64,
    pub last_serial: u64,
    pub active_serial: u64,
    pub active_offset: u64,
}

/// One retained slot claim; offset denotes a slot, never its current referent.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RememberedJob {
    pub offset: u64,
    pub incarnation: u64,
    pub phase: u64,
    pub serial: u64,
    pub keep: u64,
    pub budget: u64,
}

#[derive(Debug, Default)]
#[repr(C)]
pub struct RememberedInventory {
    pub head: u64,
    pub members: u64,
    pub last_identity: u64,
    pub epoch: u64,
    pub previous_epoch: u64,
    pub unflipped: u64,
    pub pending: u64,
    pub claims: u64,
    pub last_phase: u64,
    pub walkers: u64,
}

#[derive(Debug, Default)]
#[repr(C)]
pub struct RememberedRegistration {
    pub registry: u64,
    pub set: u64,
    pub identity: u64,
    pub previous: u64,
    pub next: u64,
    pub epoch: u64,
}

#[derive(Debug, Default)]
#[repr(C)]
pub struct RememberedInventoryRequest {
    pub registration: u64,
    pub job: u64,
    /// BEGIN phase snapshot; for DETACH_PAGE (op 9), a live AllocationPage pointer.
    pub phase: u64,
}

/// Unique bounded page-inventory cursor; membership is retained until release.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RememberedWalk {
    pub registry: u64,
    pub registration: u64,
    pub identity: u64,
    pub limit: u64,
    pub epoch: u64,
}

/// Registry-locked transfer of a claimed moved slot to destination pending work.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RememberedTransfer {
    pub source: u64,
    pub claim: u64,
    pub destination: u64,
    pub offset: u64,
    pub incarnation: u64,
}

/// Protected, same-object forwarding shortening at a frozen relocation epoch.
#[derive(Debug, Default)]
#[repr(C)]
pub struct ForwardCompress {
    pub cell: u64,
    pub source_offset: u64,
    pub incarnation: u64,
    pub observed: u64,
    pub destination: u64,
    pub epoch: u64,
}

/// Fair, exclusive Young/Old relocation admission under the scheduler lock.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RelocationCoordinator {
    pub pending: [u64; 2],
    pub active: u64,
    pub turn: u64,
    pub serial: u64,
    pub gate: u64,
    pub gate_epoch: u64,
}

/// Registry/heap-locked phase adoption; start and Retire use separate controls.
/// Adoption does not certify source retirement or cycle completion.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RelocationPhaseControl {
    pub handshake: u64,
    pub gate: u64,
    pub generation: u64,
    pub state: u64,
    pub rendezvous: u64,
    pub phase: u64,
    pub gate_epoch: u64,
}

/// Unique owner of an admitted generation's relocation gate epoch.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RelocationTurn {
    pub coordinator: u64,
    pub generation: u64,
    pub serial: u64,
    pub gate: u64,
}

/// Private Old destination descriptor cursor; phase changes require restart.
#[derive(Debug, Default)]
#[repr(C)]
pub struct PromotionRemember {
    pub registry: u64,
    pub registration: u64,
    pub owner: u64,
    pub descriptor_id: u64,
    pub payload_bytes: u64,
    pub phase: u64,
    pub cursor: u64,
}

/// Request-lock protected Young/Major cycle progress, with nonwrapping IDs.
#[derive(Debug, Default)]
#[repr(C)]
pub struct CycleLedger {
    pub requested: [u64; 2],
    pub started: [u64; 2],
    pub completed: [u64; 2],
}

/// Coalescible request for a cycle starting after the request is issued.
#[derive(Debug, Default)]
#[repr(C)]
pub struct CycleTicket {
    pub ledger: u64,
    pub kind: u64,
    pub cycle: u64,
}

/// Coherent heap/sample forecast borrowed while issuing a cycle request.
#[derive(Debug, Default)]
#[repr(C)]
pub struct CyclePressure {
    pub heap: u64,
    pub sample: u64,
    pub horizon_ns: u64,
}

/// Prepared mark-start admission paired with a cycle ledger start.
#[derive(Debug, Default)]
#[repr(C)]
pub struct CycleMarkStart {
    pub registry: u64,
    pub control: u64,
}

/// Stable per-buffer control. Serial/end use the registry lock; cursor belongs
/// to the exclusive copy job. Never copy or reset a live control record.
#[derive(Debug, Default)]
#[repr(C)]
pub struct BufferConsumer {
    pub last_serial: u64,
    pub serial: u64,
    pub end: u64,
    pub cursor: u64,
}

/// A bounded barrier-buffer job. ACK certifies phase-specific processing and
/// ownership transfer/release of every copied metadata lease before consuming.
#[derive(Debug, Default)]
#[repr(C)]
pub struct BarrierJob {
    pub identity: u64,
    pub selector: u64,
    pub serial: u64,
    pub begin: u64,
    pub end: u64,
    pub output: u64,
    pub capacity: u64,
    pub written: u64,
}

/// Registry-lock protected bookkeeping. This generation is not a phase/color
/// epoch. The coordinator separately publishes its OS poll/notification word.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct HandshakeState {
    pub generation: u64,
    pub members: u64,
    pub native: u64,
    pub expected: u64,
    pub acknowledged: u64,
    pub active: u64,
    pub last_identity: u64,
    pub head: u64,
}

/// Nonmoving registry entry. The context has the sole shared MutatorContext
/// layout; this entry tracks ownership and exactly-once round acknowledgement.
/// A live entry belongs to exactly one coordinator and must never be copied.
/// Registry-owned newest-first links are read/mutated only under its lock.
/// Storage reuse requires detach and completion of every external scanner borrow.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct ThreadRegistration {
    pub context: u64,
    pub identity: u64,
    pub acknowledged_generation: u64,
    pub registered: u64,
    pub registry: u64,
    pub previous: u64,
    pub next: u64,
}

/// Immutable native descriptor; id zero is reserved. Bitmaps have one bit per
/// aligned eight-byte slot in the fixed prefix / each repeated array element.
/// Registration validates bitmap length, trailing bits and pointer alignment.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct TypeDescriptor {
    pub id: u64,
    pub alignment: u64,
    pub fixed_bytes: u64,
    pub element_bytes: u64,
    pub fixed_bitmap: u64,
    pub element_bitmap: u64,
    pub fixed_bitmap_words: u64,
    pub element_bitmap_words: u64,
}

/// Fixed-capacity native table. Slot zero is unused; every other slot is an
/// acquire/release atomic descriptor address. Published definitions and bitmaps
/// remain immutable and alive until the registry and all its heap users retire.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct DescriptorRegistry {
    pub slots: u64,
    pub capacity: u64,
}

pub const DESCRIPTOR_SCAN_MAX_SLOTS: u64 = 256;

/// Resumable shape walk. Only byte offsets are emitted, never raw heap pointers.
/// The caller keeps the logical owner rooted and re-resolves it when consuming
/// offsets; next_offset is the only position retained across a yield.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct DescriptorScan {
    pub descriptor_id: u64,
    pub payload_bytes: u64,
    pub next_offset: u64,
    pub slot_budget: u64,
    pub output: u64,
    pub output_capacity: u64,
    pub written: u64,
    pub examined: u64,
}

/// Nonmoving metadata for an exclusively owned, committed and prezeroed TLAB.
/// Offsets are heap-relative; object-start bits identify payloads, not headers.
/// identity_base reserves a unique id for each granule for this incarnation.
/// Reusing the virtual range requires the separate retirement/color protocol.
#[repr(C)]
pub struct AllocationPage {
    pub heap_base: u64,
    pub start: u64,
    pub limit: u64,
    pub incarnation: u64,
    pub identity_base: u64,
    /// Mutator identity while allocating; stable SourceUncommit address during
    /// evacuated payload cleanup or SourceRetire during metadata retirement;
    /// otherwise zero. Protected by the heap lock.
    pub owner: u64,
    pub generation: u64,
    pub state: AtomicU64,
    pub starts: u64,
    pub starts_words: u64,
    pub allocated_top: AtomicU64,
    /// Index of the run head in HeapPool.pages (also stored in tail slots).
    pub run_head: u64,
    /// Heap-lock protected metadata holders; zero alone does not permit reclaim.
    pub metadata_leases: u64,
    /// Stable table installed before the first allocation; retained by metadata leases.
    pub forward_table: u64,
}

/// Heap-lock protected bounded lease table. Slots are stable for the heap life.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MetadataLeasePool {
    pub heap: u64,
    pub slots: u64,
    pub capacity: u64,
    pub free_head: u64,
    pub active: u64,
}

/// Token = generation << 32 | (slot index + 1). Generations never wrap.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MetadataLeaseSlot {
    pub generation: u64,
    pub page: u64,
    pub incarnation: u64,
    pub next_free: u64,
}

#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MetadataLeaseRequest {
    pub page: u64,
    pub incarnation: u64,
    pub token: u64,
}

/// Canonical reference to metadata lease; no forwarding or heap dereference.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct ReferenceLease {
    pub reference: u64,
    pub lease: MetadataLeaseRequest,
}

/// A retained remembered claim to its run-head metadata, not strong liveness.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RememberedLease {
    pub registration: u64,
    pub claim: u64,
    pub lease: MetadataLeaseRequest,
}

/// Stable source key and one atomic claim/destination publication word.
/// Low bits: empty=0, claimed=1, published=2, retained=3. See gc_forward.h.
#[derive(Debug, Default)]
#[repr(C)]
pub struct ForwardCell {
    pub source_offset: u64,
    pub incarnation: u64,
    pub word: AtomicU64,
    /// High bit closes admission; low bits count every live raw-access guard.
    pub access: AtomicU64,
}
/// Immutable backing, owner-appended ascending keys, release-published prefix.
#[derive(Debug, Default)]
#[repr(C)]
pub struct ForwardTable {
    pub cells: u64,
    pub capacity: u64,
    pub used: AtomicU64,
}
#[derive(Debug, Default)]
#[repr(C)]
pub struct ForwardLookup {
    pub source_offset: u64,
    pub incarnation: u64,
    pub cell: u64,
}
pub const SOURCE_CLOSED: u64 = 1 << 63;

/// Unique actor scope, or a collector request with context/held zero.
#[derive(Debug, Default)]
#[repr(C)]
pub struct SourceAccessRequest {
    pub context: u64,
    pub source_offset: u64,
    pub incarnation: u64,
    pub held: u64,
    pub ticket: u64,
}

/// One bounded forwarding edge or a guarded source access; gc_resolve.h.
#[derive(Debug, Default)]
#[repr(C)]
pub struct ResolveRequest {
    pub reference: u64,
    pub destination: u64,
    pub scope: SourceAccessRequest,
}

/// Unique private copy cursor. Limits delimit retained committed reservations.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RelocationCopy {
    pub heap_base: u64,
    pub source_incarnation: u64,
    pub ticket: u64,
    pub source_limit: u64,
    pub destination: u64,
    pub destination_limit: u64,
    pub budget: u64,
    pub cursor: u64,
    pub stage: u64,
}
pub const COPY_MAX_BYTES: u64 = 256;

#[derive(Debug, Default)]
#[repr(C)]
pub struct ForwardRequest {
    pub source_offset: u64,
    pub incarnation: u64,
    pub ticket: u64,
    pub destination: u64,
}

/// Unique allocation charge; bytes and owner stay fixed until retirement.
/// Stage: empty=0, reserved=1, committed=2, closed=3. See gc_budget.h.
#[derive(Debug, Default)]
#[repr(C)]
pub struct MetadataCharge {
    pub bytes: u64,
    pub owner: u64,
    pub allocation: u64,
    pub stage: u64,
}

/// Private startup metadata admission; pointers denote disjoint empty charges.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct NativeStartupRequest {
    pub roots: u64,
    pub descriptors: u64,
    pub cells: u64,
    pub entries: u64,
    pub capacity: u64,
    pub quantum: u64,
}

/// Stable logical mark identity, shared by all physical copies. Each stamp is
/// accessed under its generation's queue lock; record lifetime outlives jobs.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MarkRecord {
    pub object_id: u64,
    pub young_epoch: u64,
    pub old_epoch: u64,
    /// Prepared per-object overflow storage; links are record addresses.
    pub young_spill: MarkTask,
    pub old_spill: MarkTask,
    /// Young-cycle lock protected survival accounting, shared by all copies.
    pub age_epoch: u64,
    pub age: u64,
}
/// Owner-private cursor; admitted zeroed native storage outlives all objects/jobs.
#[derive(Debug, Default)]
#[repr(C)]
pub struct MarkArena {
    pub records: u64,
    pub capacity: u64,
    pub used: u64,
}
/// Prepared native task slot: Free=0, Queued=1, Claimed=2.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MarkTask {
    pub next: u64,
    pub serial: u64,
    pub state: u64,
    pub record: u64,
    pub reference: u64,
    pub incarnation: u64,
    pub metadata_lease: u64,
    pub cursor: u64,
}
/// One queue/lock per marking generation. Epochs are nonzero monotone cycle
/// identities, independent of the one-bit reference color. No reset with users.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MarkQueue {
    pub tasks: u64,
    pub capacity: u64,
    pub free_head: u64,
    pub head: u64,
    pub tail: u64,
    pub queued: u64,
    pub in_flight: u64,
    pub last_serial: u64,
    pub epoch: u64,
    pub generation: u64,
    pub overflow_head: u64,
    pub overflow_tail: u64,
    pub overflow_count: u64,
}
/// Protected reference/lease obligation; claims are validated by slot + serial.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MarkRequest {
    pub record: u64,
    pub reference: u64,
    pub incarnation: u64,
    pub metadata_lease: u64,
    pub index: u64,
    pub serial: u64,
    pub cursor: u64,
}

pub const PAGE_ALLOCATING: u64 = 1;
pub const OBJECT_INITIALIZING: u64 = 1;
pub const OBJECT_PUBLISHED: u64 = 2;
/// Bounded incoming-forwarding scan under frozen publication/table membership.
/// Necessary for source retirement, but not a root/heap/buffer/job certificate.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct ForwardIncoming {
    pub incarnation: u64,
    pub used: u64,
    pub target_start: u64,
    pub target_limit: u64,
    pub cursor: u64,
    pub budget: u64,
    pub examined: u64,
    pub source_offset: u64,
}

/// Metadata-only physical source payload readiness cursor. A unique scheduler
/// excludes appends/table replacement through cleanup and holds the heap lock
/// for each slice. DONE seals the page as EVACUATED, preventing future append.
/// DONE never permits
/// virtual range, color or forwarding metadata reuse.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct SourceDrain {
    pub incarnation: u64,
    pub top: u64,
    pub used: u64,
    pub cursor: u64,
    pub budget: u64,
    pub examined: u64,
}

/// Stable bounded drain bound to one registry, page incarnation and selection.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct PinSourceDrain {
    pub registry: u64,
    pub page: u64,
    pub incarnation: u64,
    pub epoch: u64,
    pub scan: SourceDrain,
}
/// Stable physical-uncommit transaction retaining exclusive page selection.
#[derive(Debug, Default)]
#[repr(C)]
pub struct PinSourceUncommit {
    pub registry: u64,
    pub epoch: u64,
    pub work: SourceUncommit,
}
/// Borrowed lease/selection and stable source-release owner address.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct PinSourceRequest {
    pub lease: u64,
    pub selection: u64,
    pub work: u64,
}

/// Bounded reference slice from an already canonical, leased worker claim.
/// Results remain protected work; job cursor advances only after child offers.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MarkScan {
    pub job: u64,
    pub heap_base: u64,
    pub registry: u64,
    pub output: u64,
    pub capacity: u64,
    pub next_cursor: u64,
    pub written: u64,
    pub examined: u64,
    /// Optional native buffer of byte offsets parallel to output; zero disables it.
    pub locations: u64,
}

/// In-heap immutable size/type/stable record followed by an atomic lifecycle word.
/// Forwarding lives outside this header. Minimum payload reservation is 16
/// bytes, including for zero-sized objects, so the user address stays in-page.
#[repr(C)]
pub struct ObjectHeader {
    pub payload_bytes: u64,
    pub descriptor_id: u64,
    pub mark_record: u64,
    pub state: AtomicU64,
}

/// Borrowed for a no-safepoint TLAB attempt. All storage is valid and disjoint.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct AllocationRequest {
    pub page: u64,
    pub descriptor: u64,
    pub elements: u64,
    pub output: u64,
    pub marks: u64,
}

/// Resolve logical payload bytes against a registered immutable descriptor.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct AllocationShape {
    pub descriptor_id: u64,
    pub payload_bytes: u64,
    pub allocation: u64,
}

/// Finite allocation and publication into a pre-reserved empty output root.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct AllocationInto {
    pub allocation: u64,
    pub root: u64,
}

/// Atomic scalar slot access under an existing canonical source guard.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct GuardedScalar {
    pub scope: u64,
    pub page: u64,
    pub registry: u64,
    pub byte_offset: u64,
    pub phase: u64,
    pub value: u64,
}

/// Bounded native-byte initialization of a descriptor with no reference slots.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct InitializationBytes {
    pub registry: u64,
    pub byte_offset: u64,
    pub source: u64,
    pub bytes: u64,
}

/// One descriptor-checked word written while the receiver is INITIALIZING.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct InitializationField {
    pub registry: u64,
    pub byte_offset: u64,
    pub value: u64,
    pub is_reference: u64,
}

/// Resolve a logical shape and allocate into a newly reserved output root.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct AllocationStart {
    pub registry: u64,
    pub shape: u64,
    pub root: u64,
}

/// Private no-safepoint handoff, NOT a root. Caller must adopt the initializing
/// reference into a root before any poll, callback or slow-path allocation.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct AllocationResult {
    pub reference: u64,
    pub object_id: u64,
}

pub const HEAP_PAGE_SHIFT: u8 = 17;
pub const HEAP_PAGE_BYTES: u64 = 1 << HEAP_PAGE_SHIFT;
pub const PAGE_FREE: u64 = 0;
pub const PAGE_PREPARING: u64 = 2;
pub const PAGE_READY: u64 = 3;
pub const PAGE_RUN_TAIL: u64 = 4;
pub const PAGE_CLOSED: u64 = 5;
pub const PAGE_RELEASING: u64 = 6;
/// Payload sealed against append/access; native forwarding metadata remains live.
pub const PAGE_EVACUATED: u64 = 7;
/// Payload decommitted; metadata and virtual range remain retained.
pub const PAGE_UNCOMMITTED: u64 = 8;

/// Heap-lock protected pool. Slots describe fixed 128 KiB virtual ranges;
/// slot zero starts after one guard page. Native slot-table bytes are charged
/// to metadata_committed at initialization. Pending reservations count against
/// both limits before any OS commitment/bitmap allocation is attempted.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct HeapPool {
    pub heap_base: u64,
    pub pages: u64,
    pub page_count: u64,
    pub hard_max: u64,
    pub soft_max: u64,
    pub relocation_reserve: u64,
    pub committed: u64,
    pub pending: u64,
    pub metadata_max: u64,
    pub metadata_committed: u64,
    pub metadata_pending: u64,
    pub last_identity: u64,
    pub last_incarnation: u64,
    pub pressure: u64,
}

/// Unique reservation token. Inputs: page_count, generation, relocation (0/1).
/// Stage 0 is empty, 1 reserved, 2 committed, 3 aborted. OS preparation occurs
/// outside the pool lock. Track commitment in committed_mapping and keep starts
/// until commit transfers both resources to the pool and clears these fields.
/// Abort requires successful cleanup and both ownership fields zero. Committed
/// pages cannot be returned through abort; retirement has a separate protocol.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct PageLease {
    pub page_count: u64,
    pub generation: u64,
    pub relocation: u64,
    pub head_index: u64,
    pub incarnation: u64,
    pub identity_base: u64,
    pub stage: u64,
    pub starts: u64,
    pub committed_mapping: u64,
}

/// Unique evacuated-payload cleanup transaction, bound to its stable address.
/// The scheduler clears committed_mapping only after successful OS decommit.
#[derive(Debug, Default)]
#[repr(C)]
pub struct SourceUncommit {
    pub head_index: u64,
    pub page_count: u64,
    pub incarnation: u64,
    pub stage: u64,
    pub owner: u64,
    pub committed_mapping: u64,
}

/// Unique bounded source cleanup transaction; gc_source_retire.h.
/// The cursor starts at tail one. Released prefixes may be reserved again.
#[derive(Debug, Default)]
#[repr(C)]
pub struct SourceRetire {
    pub head_index: u64,
    pub page_count: u64,
    pub incarnation: u64,
    pub stage: u64,
    pub owner: u64,
    pub starts: u64,
    pub forward_table: u64,
    pub cursor: u64,
}

/// Unique cleanup transaction; never copied/replayed. See gc_page_release.h.
#[derive(Debug, Default)]
#[repr(C)]
pub struct PageRelease {
    pub head_index: u64,
    pub page_count: u64,
    pub incarnation: u64,
    pub stage: u64,
    pub owner: u64,
    pub starts: u64,
    pub forward_table: u64,
    pub committed_mapping: u64,
}

/// One guarded physical reference-store attempt; gc_barrier.h ownership contract.
#[derive(Debug)]
#[repr(C)]
pub struct GuardedStore {
    pub scope: u64,
    pub page: u64,
    pub registry: u64,
    pub byte_offset: u64,
    pub expected: u64,
    pub desired: u64,
    pub phase: u64,
    pub transferred: u64,
    pub satb: BarrierEntry,
    pub remembered: BarrierEntry,
}

/// Guarded unresolved reference acquisition into a prepared output root.
#[derive(Debug)]
#[repr(C)]
pub struct GuardedLoad {
    pub scope: u64,
    pub page: u64,
    pub registry: u64,
    pub byte_offset: u64,
    pub phase: u64,
    pub output: RootRequest,
}

/// Physical repair of a guarded slot; caller proves same logical referent.
#[derive(Debug)]
#[repr(C)]
pub struct GuardedHeal {
    pub scope: u64,
    pub page: u64,
    pub registry: u64,
    pub byte_offset: u64,
    pub expected: u64,
    pub desired: u64,
    pub phase: u64,
}

/// A retained remembered claim materializes its guarded slot into a root.
#[derive(Debug)]
#[repr(C)]
pub struct RememberedLoad {
    pub registration: u64,
    pub claim: u64,
    pub source_owner: u64,
    pub load: GuardedLoad,
}

/// Metadata-only owner candidate for an authenticated remembered slot claim.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RememberedOwner {
    pub registration: u64,
    pub claim: u64,
    pub page: u64,
    pub owner: u64,
    pub byte_offset: u64,
    pub cell: u64,
}

/// Bounded rooted forwarding step, with explicit exceptional cleanup ownership.
#[derive(Debug)]
#[repr(C)]
pub struct RootFollow {
    pub context: u64,
    pub root: RootRequest,
    pub cleanup: MetadataLeaseRequest,
}

/// Rooted forwarding progress or a transferred canonical guard/metadata lease.
#[derive(Debug)]
#[repr(C)]
pub struct RootAccess {
    pub follow: RootFollow,
    pub access: ResolveRequest,
}

/// Unique self-bound owner of an independent root, page permit and metadata lease.
#[derive(Debug)]
#[repr(C)]
pub struct RootPin {
    pub self_: u64,
    pub registry: u64,
    pub stage: u64,
    pub address: u64,
    pub reference: u64,
    pub pending: u64,
    pub cancel: u64,
    pub root: RootAccess,
    pub permit: RelocationPermit,
    pub token: RelocationGateRequest,
}
/// Borrowed controls for transfer of a reserved independent root to a pin owner.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RootPinStart {
    pub registry: u64,
    pub context: u64,
    pub source: u64,
    pub reserved: u64,
}

/// The pin inventory's native heap mutex and native-ABI lock callbacks.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct PinRuntime {
    pub pages: u64,
    pub heap_lock: u64,
    pub lock: u64,
    pub unlock: u64,
}

/// Unique registered native package retaining its RootPin through retirement.
#[derive(Debug)]
#[repr(C)]
pub struct NativePin {
    pub self_: u64,
    pub registry: u64,
    pub context: u64,
    pub charge: u64,
    pub storage: u64,
    pub stage: u64,
    pub reserved: RootRequest,
    pub pin: RootPin,
}
/// Borrowed publication arguments; metadata backing is already committed.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct NativePinStart {
    pub registry: u64,
    pub context: u64,
    pub charge: u64,
}

/// Native ABI metadata mapping callbacks; false unmap retains owned storage.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct NativeMetadataOps {
    pub quantum: u64,
    pub map: u64,
    pub unmap: u64,
}

/// Guarded header-to-mark-job handoff; unique input lease transfers to output.
#[derive(Debug)]
#[repr(C)]
pub struct GuardedMarkPrepare {
    pub scope: u64,
    pub page: u64,
    pub phase: u64,
    pub reference: u64,
    pub lease: MetadataLeaseRequest,
    pub output: MarkRequest,
}

/// Generation-filtered guarded mark offer; SKIP preserves its input lease.
#[derive(Debug)]
#[repr(C)]
pub struct GuardedMarkRoute {
    pub generation: u64,
    pub prepare: GuardedMarkPrepare,
}

/// Authenticated claim retargeting with explicit old-protection cleanup output.
#[derive(Debug)]
#[repr(C)]
pub struct MarkRetarget {
    pub job: u64,
    pub destination: MarkRequest,
    pub retired: MarkRequest,
}

/// Owner-side registry-serialized worker rendezvous request.
#[derive(Debug)]
#[repr(C)]
pub struct WorkerRequest {
    pub handshake: u64,
    pub participant: u64,
}

/// Admission remains closed through a relocation-publication audit.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RelocationGate {
    pub head: u64,
    pub active: u64,
    pub last_serial: u64,
    pub epoch: u64,
    pub closed: u64,
}
/// Stable intrusive control retained from before source claim through completion.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RelocationPermit {
    pub gate: u64,
    pub serial: u64,
    pub epoch: u64,
    pub previous: u64,
    pub next: u64,
    pub job: u64,
}
/// Non-repeating serial/epoch authenticate a possibly reused permit address.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RelocationGateRequest {
    pub permit: u64,
    pub serial: u64,
    pub epoch: u64,
}

/// Per-page pin admission with an independently owned metadata lease.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct PinPageRecord {
    pub lifetime: MetadataLeaseRequest,
    pub admission: RelocationGate,
}
/// Stable heap-owned inventory; pointers remain valid through all users.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct PinPageRegistry {
    pub pool: u64,
    pub records: u64,
    pub capacity: u64,
}

/// Stable installed-job ownership and exact forwarding receipt.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RelocationJob {
    pub owner: u64,
    pub gate: u64,
    pub permit: u64,
    pub serial: u64,
    pub epoch: u64,
    pub source: u64,
    pub source_offset: u64,
    pub incarnation: u64,
    pub ticket: u64,
    pub destination: u64,
    pub state: u64,
}

/// Resumable installed-relocation completion; all controls stay live until DONE.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RelocationCompletion {
    pub owner: u64,
    pub job: u64,
    pub copy: u64,
    pub stage: u64,
}

/// Gate-enrolled source claim; ticket and epoch are success-only outputs.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RelocationClaim {
    pub permit: u64,
    pub source: u64,
    pub source_offset: u64,
    pub incarnation: u64,
    pub ticket: u64,
    pub epoch: u64,
}

/// Cycle-owned canonical source and mark-record receipt for relocation admission.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct MarkedRelocation {
    pub claim: RelocationClaim,
    pub queue: u64,
    pub record: u64,
    pub epoch: u64,
}

/// Borrowed native inputs, retained by the scheduler through mover completion.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct PinRelocationClaim {
    pub lease: u64,
    pub selection: u64,
    pub movers: u64,
    pub work: u64,
}

/// Resumable private-copy cancellation; retain all controls until terminal DONE.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RelocationCancellation {
    pub owner: u64,
    pub permit: u64,
    pub serial: u64,
    pub epoch: u64,
    pub target: u64,
    pub discard: u64,
    pub stage: u64,
}

/// Unique bounded cleanup cursor for an exclusively owned uninstalled copy.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct RelocationDiscard {
    pub owner: u64,
    pub source: u64,
    pub copy: u64,
    pub cursor: u64,
    pub bytes: u64,
}

/// Bounded guarded heap audit; observed bits are diagnostic, not rooted.
#[derive(Debug, Default)]
#[repr(C)]
pub struct HeapIncoming {
    pub scope: u64,
    pub page: u64,
    pub registry: u64,
    pub phase: u64,
    pub target_start: u64,
    pub target_limit: u64,
    pub cursor: u64,
    pub observed: u64,
}

/// One-page heap enumeration with explicit metadata lease ownership.
#[derive(Debug, Default)]
#[repr(C)]
pub struct HeapWalk {
    pub pool: u64,
    pub heap: u64,
    pub pages: u64,
    pub page_count: u64,
    pub cursor: u64,
    pub lease: MetadataLeaseRequest,
}

/// Metadata-only object-key enumeration borrowing an authenticated page lease.
#[derive(Debug, Default)]
#[repr(C)]
pub struct ObjectWalk {
    pub lease: u64,
    pub page: u64,
    pub incarnation: u64,
    pub table: u64,
    pub used: u64,
    pub cursor: u64,
    pub source_offset: u64,
}

/// Bounded resolve/scan/exit owner; stage 1 retains a guard until cleanup.
#[derive(Debug, Default)]
#[repr(C)]
pub struct HeapAuditSlice {
    pub page: u64,
    pub context: u64,
    pub reference: u64,
    pub registry: u64,
    pub phase: u64,
    pub target_start: u64,
    pub target_limit: u64,
    pub cursor: u64,
    pub observed: u64,
    pub access: ResolveRequest,
    pub stage: u64,
    pub pending_cursor: u64,
    pub pending_observed: u64,
    pub pending_result: u64,
}

/// Cooperative whole-heap audit ownership; metadata and payload steps are split.
#[derive(Debug, Default)]
#[repr(C)]
pub struct HeapAudit {
    pub context: u64,
    pub registry: u64,
    pub gate: u64,
    pub epoch: u64,
    pub phase: u64,
    pub target_start: u64,
    pub target_limit: u64,
    pub stage: u64,
    pub findings: u64,
    pub walk: HeapWalk,
    pub object: ObjectWalk,
    pub slice: HeapAuditSlice,
}

/// Owns a rooted observation through source exit and metadata release.
#[derive(Debug)]
#[repr(C)]
pub struct RootedObservation {
    pub receiver: RootAccess,
    pub registry: u64,
    pub byte_offset: u64,
    pub output: RootRequest,
    pub stage: u64,
    pub result: u64,
}

/// Owns one scalar field operation through canonical access and cleanup.
#[derive(Debug)]
#[repr(C)]
pub struct RootedScalar {
    pub receiver: RootAccess,
    pub registry: u64,
    pub byte_offset: u64,
    pub value: u64,
    pub operation: u64,
    pub stage: u64,
    pub result: u64,
}

/// Owns one store attempt through guard and untransferred entry-lease cleanup.
#[derive(Debug)]
#[repr(C)]
pub struct RootedStoreAttempt {
    pub receiver: RootAccess,
    pub marks: u64,
    pub commit: GuardedStore,
    pub entries: [MetadataLeaseRequest; 2],
    pub stage: u64,
    pub result: u64,
    pub release_index: u64,
}

/// Retains rooted mark work until enqueue or exact cancellation cleanup.
#[derive(Debug)]
#[repr(C)]
pub struct RootedMark {
    pub access: RootAccess,
    pub queue: u64,
    pub generation: u64,
    pub epoch: u64,
    pub phase: u64,
    pub stage: u64,
    pub route_result: u64,
    pub offer_result: u64,
    pub route: GuardedMarkRoute,
}

/// Mark owner for an externally counted journal/root-walk/worker reference.
#[derive(Debug)]
#[repr(C)]
pub struct PendingMark {
    pub context: u64,
    pub current: ReferenceLease,
    pub retired: MetadataLeaseRequest,
    pub access: ResolveRequest,
    pub queue: u64,
    pub generation: u64,
    pub epoch: u64,
    pub phase: u64,
    pub stage: u64,
    pub route_result: u64,
    pub offer_result: u64,
    pub route: GuardedMarkRoute,
}

/// Required-generation mark admissions for a retained root and phase snapshot.
#[derive(Debug)]
#[repr(C)]
pub struct RootedMarkSet {
    pub context: u64,
    pub root: RootRequest,
    pub queues: [u64; 2],
    pub epochs: [u64; 2],
    pub phase: u64,
    pub state: u64,
    pub required: u64,
    pub completed: u64,
    pub current: u64,
    pub jobs: [RootedMark; 2],
}

/// Owns field observation or existing-root preparation through marks and repair.
#[derive(Debug)]
#[repr(C)]
pub struct RootedLoad {
    pub observation: RootedObservation,
    pub marks: RootedMarkSet,
    pub publication: RootAccess,
    pub stage: u64,
    pub last: u64,
    pub cancel: u64,
    pub fault: u64,
    pub retry_event: u64,
    pub admitted: u64,
}

/// Owns desired preparation, observation and retrying physical stores.
#[derive(Debug)]
#[repr(C)]
pub struct RootedStore {
    pub value: RootedLoad,
    pub observation: RootedObservation,
    pub attempt: RootedStoreAttempt,
    pub stage: u64,
    pub last: u64,
    pub cancel: u64,
    pub admitted: u64,
    pub result: u64,
}

/// Native lock and admission bindings for slow rooted-store dispatch.
#[derive(Debug)]
#[repr(C)]
pub struct RootedStoreRuntime {
    pub heap_pool: u64,
    pub admission: u64,
    pub heap_lock: u64,
    pub registry_lock: u64,
    pub queue_locks: [u64; 2],
    pub lock: u64,
    pub unlock: u64,
}

/// Registry-lock inputs for counted load/value-preparation snapshot admission.
#[derive(Debug)]
#[repr(C)]
pub struct RootedLoadAdmission {
    pub registry: u64,
    pub handshake: u64,
    pub participant: u64,
    pub queues: [u64; 2],
}

/// Weak admission is closed before strong termination and remains closed until
/// the owning weak-table pass has cleared dead entries. Metadata is not strong.
#[derive(Debug)]
#[repr(C)]
pub struct WeakLoadGate {
    pub pool: u64,
    pub closed: u64,
    pub registry: u64,
    pub handshake: u64,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakMarkControl {
    pub gate: u64,
    pub mark: u64,
    pub stage: u64,
    pub processor: u64,
    pub scan: u64,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakSlot {
    pub gate: u64,
    pub serial: u64,
    pub reference: u64,
    pub lease: MetadataLeaseRequest,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakEntry {
    pub slot: WeakSlot,
    pub next_free: u64,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakTable {
    pub gate: u64,
    pub entries: u64,
    pub capacity: u64,
    pub free_head: u64,
    pub active: u64,
    pub last_serial: u64,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakHandle {
    pub table: u64,
    pub index: u64,
    pub serial: u64,
}
/// Worker-private weak processing state shared with the C lifecycle controller.
/// The source snapshot authenticates commit after guard exit. Independent
/// current/retired leases protect metadata, not strong reachability.
#[derive(Debug)]
#[repr(C)]
pub struct WeakProcess {
    pub table: u64,
    pub owner: u64,
    pub pool: u64,
    pub context: u64,
    pub handle: WeakHandle,
    pub source: WeakSlot,
    pub phase: u64,
    pub epoch: u64,
    pub generation: u64,
    pub rendezvous: u64,
    pub stage: u64,
    pub result: u64,
    pub current: ReferenceLease,
    pub retired: MetadataLeaseRequest,
    pub access: ResolveRequest,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakProcessStart {
    pub table: u64,
    pub owner: u64,
    pub context: u64,
    pub handle: WeakHandle,
}
/// Exclusive bounded pass over the single published weak table.
#[derive(Debug)]
#[repr(C)]
pub struct WeakScan {
    pub table: u64,
    pub owner: u64,
    pub context: u64,
    pub entries: u64,
    pub capacity: u64,
    pub last_serial: u64,
    pub phase: u64,
    pub epoch: u64,
    pub generation: u64,
    pub rendezvous: u64,
    pub cursor: u64,
    pub stage: u64,
    pub work: WeakProcess,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakScanStart {
    pub table: u64,
    pub owner: u64,
    pub context: u64,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakCreateRequest {
    pub admission: u64,
    pub context: u64,
    pub source_root: u64,
    pub output_handle: u64,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakTableRequest {
    pub handle: WeakHandle,
    pub admission: u64,
    pub context: u64,
    pub output_root: u64,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakIncoming {
    pub table: u64,
    pub entries: u64,
    pub gate: u64,
    pub pool: u64,
    pub capacity: u64,
    pub last_serial: u64,
    pub target_start: u64,
    pub target_limit: u64,
    pub cursor: u64,
    pub stage: u64,
    pub handle: WeakHandle,
    pub observed: u64,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakFollowRequest {
    pub index: u64,
    pub serial: u64,
    pub retired: MetadataLeaseRequest,
}
#[derive(Debug)]
#[repr(C)]
pub struct WeakLoadRequest {
    pub admission: u64,
    pub context: u64,
    pub slot: u64,
    pub serial: u64,
    pub output_root: u64,
}

/// Borrowed package shutdown bindings; all resources outlive terminal close.
#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct NativePinRuntime {
    pub pins: u64,
    pub registry: u64,
    pub registry_lock: u64,
    pub lock: u64,
    pub unlock: u64,
    pub heap: u64,
    pub native: u64,
}

/// Unique FINISH/notification obligation; backing may disappear after stage 1.
#[derive(Debug, Default)]
#[repr(C)]
pub struct RootFinishNotification {
    pub context: u64,
    pub event: u64,
    pub notify: u64,
    pub stage: u64,
    pub last_error: u64,
}
