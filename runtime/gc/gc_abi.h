/* Generational ABI v4. Checked against Rust by tests/gc_abi.rs. The C thread
 * lifecycle and TLAB use MutatorContext; generational references, roots and
 * buffers are not activated merely by including this header. */
#ifndef KLASSIC_GENERATIONAL_GC_ABI_H
#define KLASSIC_GENERATIONAL_GC_ABI_H
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdatomic.h>

#define GC_ABI_VERSION 4u
#define GC_MAX_AGE UINT64_C(15)
#define GC_OBJECT_ALIGNMENT UINT64_C(16)
#define GC_MAX_OFFSET UINT64_C(0xffffffffffff)
#define GC_REMEMBERED_MASK UINT16_C(0x0030)
#define GC_YOUNG_MARK_MASK UINT16_C(0x0300)
#define GC_OLD_MARK_MASK UINT16_C(0x0c00)
#define GC_REMAP_MASK UINT16_C(0xf000)
#define GC_METADATA_MASK UINT16_C(0xff30)

_Static_assert(sizeof(uintptr_t) == 8, "Generational GC requires a 64-bit target");
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#error "Generational GC ABI v4 requires little endian"
#endif

typedef struct { uint64_t bits; } GcRef;
typedef struct { uint64_t bits; } PhaseSnapshot;
typedef struct { uint64_t value; } ObjectId;
typedef struct { uint32_t value; } TypeDescId;
typedef struct { uint64_t cell, reuse_generation, scope; } RootHandle;
typedef struct {
    _Atomic uint64_t value;
    uint64_t reuse_generation, active, reserved;
} RootCell;
#define GC_ROOT_SEGMENT_PREPARED UINT64_C(0)
#define GC_ROOT_SEGMENT_CLEANING UINT64_C(3)
#define GC_ROOT_SEGMENT_ACTIVE UINT64_C(1)
#define GC_ROOT_SEGMENT_RETIRING UINT64_C(2)
typedef struct {
    uint64_t owner, scope, cells, capacity;
    _Atomic uint64_t used, state;
    uint64_t journal, registration;
} RootSegment;
typedef struct { uint64_t last_scope, last_lease, active, retiring, leases, head, phase, inventory, walkers; } RootRegistry;
typedef struct {
    uint64_t segment, scope, registered, leases, limit, cursor, phase, required_mask;
    uint64_t claim_serial, claim_end, previous, next;
    _Atomic uint64_t cleanup_cursor;
    uint64_t inventory_previous, inventory_next;
    uint64_t context, owner_previous, owner_next;
    uint64_t journal_serial, journal_end;
} RootRegistration;
typedef struct { uint64_t registration, scope, serial, kind, cursor; } RootLease;
typedef struct { uint64_t registration, lease, serial, scope, context, begin, end, mask, phase; } RootLifeRequest;
typedef struct { uint64_t roots, handshake, participant; } DetachRequest;
typedef struct {
    uint64_t registry, phase, active, limit, cursor, mask, completed_mask;
    RootLease lease;
    uint64_t pending, pending_reference, pending_observed, pending_index;
} RootWalk;
typedef struct { uint64_t walk, serial, output, capacity, written; } RootWalkScan;
typedef struct { uint64_t walk, serial, index, expected, desired; } RootWalkHeal;
typedef struct { uint64_t walk, serial, index; } RootWalkItem;
typedef struct { uint64_t serial, last_scope, phase, state, context, queue, epoch, generation; RootWalk walk; } RootAudit;
typedef struct { uint64_t handshake, audit, generation, remembered; } MarkEndRequest;
typedef struct { uint64_t handshake, audit, generation, state, rendezvous, phase, epoch, outcome, remembered; } MarkPhaseControl;
typedef struct { uint64_t handshake, gate, generation, state, rendezvous, phase, gate_epoch; } RelocationPhaseControl;
typedef struct { uint64_t head, active, last_serial, epoch, closed; } RelocationGate;
typedef struct { uint64_t gate, serial, epoch, previous, next, job; } RelocationPermit;
typedef struct { uint64_t permit, serial, epoch; } RelocationGateRequest;
typedef struct { uint64_t pending[2], active, turn, serial, gate, gate_epoch; } RelocationCoordinator;
typedef struct { uint64_t coordinator, generation, serial, gate; } RelocationTurn;
typedef struct { uint64_t owner, gate, permit, serial, epoch, source, source_offset, incarnation, ticket, destination, state; } RelocationJob;
typedef struct { uint64_t owner, job, copy, stage; } RelocationCompletion;
typedef struct { uint64_t permit, source, source_offset, incarnation, ticket, epoch; } RelocationClaim;
typedef struct { RelocationClaim claim; uint64_t queue, record, epoch; } MarkedRelocation;
/* Borrowed native inputs; the owner retains lease/selection through mover completion. */
typedef struct { uint64_t lease, selection, movers, work; } PinRelocationClaim;
typedef struct { uint64_t owner, source, copy, cursor, bytes; } RelocationDiscard;
typedef struct { uint64_t owner, permit, serial, epoch, target, discard, stage; } RelocationCancellation;
typedef struct { uint64_t requested[2], started[2], completed[2]; } CycleLedger;
typedef struct { uint64_t ledger, kind, cycle; } CycleTicket;
typedef struct { uint64_t heap, sample, horizon_ns; } CyclePressure;
typedef struct { uint64_t registry, control; } CycleMarkStart;
typedef struct {
    uint64_t start, limit, incarnation, bits[2], words, current, epoch, previous_epoch, last_phase;
    uint64_t current_count, pending, cursor, last_serial, active_serial, active_offset;
} RememberedSet;
typedef struct { uint64_t offset, incarnation, phase, serial, keep, budget; } RememberedJob;
typedef struct { uint64_t head, members, last_identity, epoch, previous_epoch, unflipped, pending, claims, last_phase, walkers; } RememberedInventory;
typedef struct { uint64_t registry, set, identity, previous, next, epoch; } RememberedRegistration;
/* phase is a PhaseSnapshot for BEGIN, AllocationPage* for DETACH_PAGE. */
typedef struct { uint64_t registration, job, phase; } RememberedInventoryRequest;
typedef struct { uint64_t registry, registration, identity, limit, epoch; } RememberedWalk;
typedef struct { uint64_t source, claim, destination, offset, incarnation; } RememberedTransfer;
typedef struct { uint64_t registry, registration, owner, descriptor_id, payload_bytes, phase, cursor; } PromotionRemember;
typedef struct { uint64_t previous, next, scope, phase; } RootChange;
typedef struct { uint64_t segment; RootHandle handle; uint64_t value; } RootRequest;
typedef struct { uint64_t segment, cursor, output, capacity, written; } RootScan;
typedef struct { RootScan scan; uint64_t lease, serial; } RootScanJob;
typedef struct { uint64_t buffer, output, capacity, written; } RootDrain;
typedef struct {
    uint64_t registration, lease, serial, scope, begin, end, output, capacity, written;
} RootJournalRequest;
typedef struct { RootHandle owner; uint64_t byte_offset; } FieldLoc;
typedef struct {
    GcRef owner;
    uint64_t byte_offset, page_incarnation, record_epoch, metadata_lease;
} BufferedSlot;
typedef struct { GcRef previous; BufferedSlot slot; } BarrierEntry;
typedef struct { uint64_t buffer, output, capacity, written; } BarrierDrain;
/* Guarded physical commit; references and entry leases are prepared outside. */
typedef struct {
    uint64_t scope, page, registry, byte_offset, expected, desired, phase, transferred;
    BarrierEntry satb, remembered;
} GuardedStore;
typedef struct {
    uint64_t scope, page, registry, byte_offset, phase;
    RootRequest output;
} GuardedLoad;
typedef struct { uint64_t scope, page, registry, byte_offset, phase, value; } GuardedScalar;
typedef struct {
    uint64_t scope, page, registry, byte_offset, expected, desired, phase;
} GuardedHeal;
typedef struct { uint64_t registration, claim, source_owner; GuardedLoad load; } RememberedLoad;
typedef struct { uint64_t registration, claim, page, owner, byte_offset, cell; } RememberedOwner;
typedef struct {
    uint64_t entries, capacity, top;
    _Atomic uint64_t published, consumed;
} BufferCursor;
typedef struct { RootRegistration registration; RootSegment segment; BufferCursor journal; } RootStorage;
typedef struct { uint64_t owner, cells, entries, bytes; } RootStorageSpec;
/* Serial/end are registry-lock protected; the unique job owner advances cursor
 * outside the lock during COPY. Storage is embedded in its retained context. */
typedef struct { uint64_t last_serial, serial, end, cursor; } BufferConsumer;
typedef struct { uint64_t identity, selector, serial, begin, end, output, capacity, written; } BarrierJob;
typedef struct {
    uint32_t abi_version;
    _Atomic uint32_t state;
    uint64_t thread_id;
    _Atomic uint64_t adopted_phase;
    uint64_t tlab_start, tlab_top, tlab_end;
    BufferCursor satb, remembered;
    /* Registry-lock protected: active owner-list head and retirement count. */
    uint64_t root_segments, retiring_roots;
    _Atomic uint64_t access_epoch;
    /* Sole owner increments after each successful root STORE/RELEASE. Never
     * wraps or resets during registration. An audit samples BEFORE traversal;
     * equality is authoritative only after this owner has acknowledged a
     * handshake (or is native-blocked), with producers held stopped. */
    _Atomic uint64_t root_revision;
    BufferConsumer satb_consumer, remembered_consumer;
    /* Registry-lock protected certificate for one completed inventory audit. */
    uint64_t root_audit_serial[2], root_audit_identity[2], root_audit_revision[2];
    /* Sole-mutator cumulative allocation bytes, including header/padding.
     * Acquire readers may sample concurrently. UINT64_MAX means saturated;
     * it must not be interpreted as an ordinary rate sample. Never reset while
     * registered; TLAB close/refill preserves this thread-lifetime counter. */
    _Atomic uint64_t allocated_bytes;
    /* Sole context owner updates; readers acquire after state publication.
     * Nonzero forbids ACK/native entry/detach until private owners clean up. */
    _Atomic uint64_t private_producers;
} MutatorContext;
typedef struct { uint64_t context, identity, timestamp_ns, total_bytes, elapsed_ns, delta_bytes; } AllocationSample;

/* Registry-lock protected handshake bookkeeping, not a PhaseSnapshot. The
 * OS poll/notification word is separate and published by the coordinator.
 * head links every live registration newest first; all links use this lock. */
typedef struct {
    uint64_t generation, members, native, expected, acknowledged, active, last_identity, head;
} HandshakeState;
typedef struct {
    uint64_t context, identity, acknowledged_generation, registered, registry, previous, next;
} ThreadRegistration;
typedef struct { uint64_t handshake, participant; } WorkerRequest;

typedef struct {
    uint64_t id, alignment, fixed_bytes, element_bytes, fixed_bitmap, element_bitmap;
    uint64_t fixed_bitmap_words, element_bitmap_words;
} TypeDescriptor;
typedef struct { uint64_t slots, capacity; } DescriptorRegistry;
#define GC_DESCRIPTOR_SCAN_MAX_SLOTS UINT64_C(256)
typedef struct {
    uint64_t descriptor_id, payload_bytes, next_offset, slot_budget;
    uint64_t output, output_capacity, written, examined;
} DescriptorScan;
typedef struct {
    uint64_t heap_base, start, limit, incarnation, identity_base, owner, generation;
    _Atomic uint64_t state;
    uint64_t starts, starts_words;
    _Atomic uint64_t allocated_top;
    uint64_t run_head, metadata_leases, forward_table;
} AllocationPage;
typedef struct { uint64_t heap, slots, capacity, free_head, active; } MetadataLeasePool;
typedef struct { uint64_t generation, page, incarnation, next_free; } MetadataLeaseSlot;
typedef struct { uint64_t page, incarnation, token; } MetadataLeaseRequest;
/* One stable record per page-table index; independent metadata lifetime. */
typedef struct { MetadataLeaseRequest lifetime;RelocationGate admission; } PinPageRecord;
typedef struct { MetadataLeasePool *pool;PinPageRecord *records;uint64_t capacity; } PinPageRegistry;
typedef struct { uint64_t pool,heap,pages,page_count,cursor; MetadataLeaseRequest lease; } HeapWalk;
typedef struct { uint64_t lease,page,incarnation,table,used,cursor,source_offset; } ObjectWalk;
typedef struct { uint64_t reference; MetadataLeaseRequest lease; } ReferenceLease;
typedef struct { uint64_t registration, claim; MetadataLeaseRequest lease; } RememberedLease;
typedef struct { uint64_t context; RootRequest root; MetadataLeaseRequest cleanup; } RootFollow;
/* Immutable source key, mutable tagged publication word. Never reset live. */
typedef struct { uint64_t source_offset, incarnation; _Atomic uint64_t word, access; } ForwardCell;
/* Stable ascending source keys; one owner appends, readers acquire used.
 * Backing and geometry are immutable from installation through retirement. */
typedef struct { uint64_t cells, capacity; _Atomic uint64_t used; } ForwardTable;
typedef struct { uint64_t source_offset, incarnation, cell; } ForwardLookup;
typedef struct { uint64_t source_offset, incarnation, ticket, destination; } ForwardRequest;
typedef struct { uint64_t cell, source_offset, incarnation, observed, destination, epoch; } ForwardCompress;
/* Bounded guarded heap audit; observed is diagnostic, not rooted. */
typedef struct { uint64_t scope,page,registry,phase,target_start,target_limit,cursor,observed; } HeapIncoming;
#define GC_SOURCE_CLOSED (UINT64_C(1)<<63)
typedef struct { uint64_t context, source_offset, incarnation, held, ticket; } SourceAccessRequest;
typedef struct { uint64_t reference, destination; SourceAccessRequest scope; } ResolveRequest;
typedef struct { RootFollow follow; ResolveRequest access; } RootAccess;
/* Unique rooted pin control: root, metadata lease and page permit ownership. */
typedef struct {
    uint64_t self,registry,stage,address,reference,pending,cancel;
    RootAccess root;RelocationPermit permit;RelocationGateRequest token;
} RootPin;
typedef struct { uint64_t registry,context,source,reserved; } RootPinStart;
typedef void (*PinNativeLock)(uint64_t);
typedef struct { PinPageRegistry *pages;uint64_t heap_lock;PinNativeLock lock,unlock; } PinRuntime;
typedef struct {
    uint64_t self,registry,context,charge,storage,stage;
    RootRequest reserved;RootPin pin;
} NativePin;
typedef struct { uint64_t registry,context,charge; } NativePinStart;
typedef struct { uint64_t context,event,notify,stage,last_error; } RootFinishNotification;
/* Native ABI callbacks. Failed unmap must retain usable owned storage. */
typedef struct {
    uint64_t quantum;
    void *(*map)(size_t);
    bool (*unmap)(void *,size_t);
} NativeMetadataOps;

typedef struct {
    RootAccess receiver;
    uint64_t registry, byte_offset;
    RootRequest output;
    uint64_t stage, result;
} RootedObservation;
typedef struct {
    RootAccess receiver;
    uint64_t registry, byte_offset, value, operation, stage, result;
} RootedScalar;
typedef struct {
    RootAccess receiver;
    uint64_t marks;
    GuardedStore commit;
    MetadataLeaseRequest entries[2];
    uint64_t stage, result, release_index;
} RootedStoreAttempt;

typedef struct {
    uint64_t page,context,reference,registry,phase,target_start,target_limit,cursor,observed;
    ResolveRequest access;
    uint64_t stage,pending_cursor,pending_observed,pending_result;
} HeapAuditSlice;
typedef struct {
    uint64_t context,registry,gate,epoch,phase,target_start,target_limit,stage,findings;
    HeapWalk walk;
    ObjectWalk object;
    HeapAuditSlice slice;
} HeapAudit;
typedef struct {
    uint64_t heap_base, source_incarnation, ticket, source_limit, destination, destination_limit;
    uint64_t budget, cursor, stage;
} RelocationCopy;
/* All mark work metadata is serialized by its queue lock. Record stamps are
 * shared between physical copies and protected by that generation's lock. */
typedef struct { uint64_t next, serial, state, record, reference, incarnation, metadata_lease, cursor; } MarkTask;
/* Reserved once per logical object, not once per cycle. Overflow links use
 * MarkRecord addresses; ordinary task links remain one-based slot indices.
 * age_epoch/age use the Young-cycle lock, independently of Old mark state. */
typedef struct { uint64_t object_id, young_epoch, old_epoch; MarkTask young_spill, old_spill; uint64_t age_epoch, age; } MarkRecord;
/* Owner-private bump cursor over admitted, zeroed stable native records. */
typedef struct { uint64_t records, capacity, used; } MarkArena;
typedef struct { uint64_t tasks, capacity, free_head, head, tail, queued, in_flight, last_serial, epoch, generation, overflow_head, overflow_tail, overflow_count; } MarkQueue;
typedef struct { uint64_t record, reference, incarnation, metadata_lease, index, serial, cursor; } MarkRequest;
/* Weak entries retain metadata, not strong marking obligations. The owning
 * weak table serializes loads, gate closure, clearing and slot reuse. */
typedef struct { uint64_t pool, closed, registry, handshake; } WeakLoadGate;
typedef struct { WeakLoadGate *gate; MarkPhaseControl *mark; uint64_t stage, processor, scan; } WeakMarkControl;
typedef struct { uint64_t gate, serial, reference; MetadataLeaseRequest lease; } WeakSlot;
typedef struct { uint64_t admission, context, slot, serial, output_root; } WeakLoadRequest;
typedef struct { WeakSlot slot; uint64_t next_free; } WeakEntry;
typedef struct {
    WeakLoadGate *gate; WeakEntry *entries;
    uint64_t capacity,free_head,active,last_serial;
} WeakTable;
typedef struct { uint64_t table,index,serial; } WeakHandle;
/* Worker-private weak processing state. Source identity and independent leases
 * survive guard exit and explicit handle disposal; stage controls ownership. */
typedef struct {
    WeakTable *table;WeakMarkControl *owner;MetadataLeasePool *pool;MutatorContext *context;
    WeakHandle handle;WeakSlot source;uint64_t phase,epoch,generation,rendezvous,stage,result;
    ReferenceLease current;MetadataLeaseRequest retired;ResolveRequest access;
} WeakProcess;
typedef struct {
    WeakTable *table;WeakMarkControl *owner;MutatorContext *context;WeakHandle handle;
} WeakProcessStart;
typedef struct {
    WeakTable *table;WeakMarkControl *owner;MutatorContext *context;WeakEntry *entries;
    uint64_t capacity,last_serial,phase,epoch,generation,rendezvous,cursor,stage;
    WeakProcess work;
} WeakScan;
typedef struct { WeakTable *table;WeakMarkControl *owner;MutatorContext *context; } WeakScanStart;
typedef struct { WeakHandle handle; uint64_t admission,context,output_root; } WeakTableRequest;
typedef struct { uint64_t admission,context,source_root,output_handle; } WeakCreateRequest;
typedef struct { uint64_t index,serial; MetadataLeaseRequest retired; } WeakFollowRequest;
typedef struct {
    WeakTable *table;WeakEntry *entries;WeakLoadGate *gate;MetadataLeasePool *pool;
    uint64_t capacity,last_serial,target_start,target_limit,cursor,stage;
    WeakHandle handle;uint64_t observed;
} WeakIncoming;
typedef struct { uint64_t job; MarkRequest destination, retired; } MarkRetarget;
typedef struct { uint64_t scope, page, phase, reference; MetadataLeaseRequest lease; MarkRequest output; } GuardedMarkPrepare;
typedef struct { uint64_t generation; GuardedMarkPrepare prepare; } GuardedMarkRoute;
typedef struct {
    RootAccess access;
    uint64_t queue,generation,epoch,phase,stage,route_result,offer_result;
    GuardedMarkRoute route;
} RootedMark;
/* Owner of one externally counted strong pending reference through marking. */
typedef struct {
    uint64_t context;
    ReferenceLease current;MetadataLeaseRequest retired;ResolveRequest access;
    uint64_t queue,generation,epoch,phase,stage,route_result,offer_result;
    GuardedMarkRoute route;
} PendingMark;
typedef struct {
    uint64_t context;RootRequest root;
    uint64_t queues[2],epochs[2],phase,state,required,completed,current;
    RootedMark jobs[2];
} RootedMarkSet;
typedef struct {
    RootedObservation observation;
    RootedMarkSet marks;
    RootAccess publication;
    uint64_t stage,last,cancel,fault,retry_event,admitted;
} RootedLoad;
/* Owns desired preparation, rooted observation and retrying physical stores. */
typedef struct {
    RootedLoad value;
    RootedObservation observation;
    RootedStoreAttempt attempt;
    uint64_t stage,last,cancel,admitted,result;
} RootedStore;
/* OS-owner bindings for the slow store controller; callbacks return only on
 * successful native lock/unlock and must never poll or enter managed code. */
typedef struct {
    uint64_t heap_pool,admission,heap_lock,registry_lock,queue_locks[2],lock,unlock;
} RootedStoreRuntime;
typedef struct { uint64_t registry,handshake,participant,queues[2]; } RootedLoadAdmission;



typedef struct { uint64_t registry, byte_offset, value, is_reference; } InitializationField;
typedef struct { uint64_t registry, byte_offset, source, bytes; } InitializationBytes;
#define GC_PAGE_ALLOCATING UINT64_C(1)
#define GC_OBJECT_INITIALIZING UINT64_C(1)
#define GC_OBJECT_PUBLISHED UINT64_C(2)
typedef struct { uint64_t head_index, page_count, incarnation, stage, owner, committed_mapping; } SourceUncommit;
typedef struct { uint64_t incarnation, used, target_start, target_limit, cursor, budget, examined, source_offset; } ForwardIncoming;
typedef struct { uint64_t incarnation, top, used, cursor, budget, examined; } SourceDrain;
typedef struct { uint64_t registry,page,incarnation,epoch;SourceDrain scan; } PinSourceDrain;
typedef struct { uint64_t registry,epoch;SourceUncommit work; } PinSourceUncommit;
/* Borrowed lease/selection plus the stable drain or uncommit owner address. */
typedef struct { uint64_t lease,selection,work; } PinSourceRequest;
typedef struct { uint64_t job, heap_base, registry, output, capacity, next_cursor, written, examined, locations; } MarkScan;
typedef struct {
    uint64_t payload_bytes, descriptor_id, mark_record;
    _Atomic uint64_t state;
} ObjectHeader;
typedef struct { uint64_t page, descriptor, elements, output, marks; } AllocationRequest;
typedef struct { uint64_t descriptor_id, payload_bytes, allocation; } AllocationShape;
typedef struct { uint64_t allocation, root; } AllocationInto;
typedef struct { uint64_t registry, shape, root; } AllocationStart;
typedef struct { uint64_t reference, object_id; } AllocationResult;
#define GC_HEAP_PAGE_SHIFT 17u
#define GC_HEAP_PAGE_BYTES (UINT64_C(1) << GC_HEAP_PAGE_SHIFT)
#define GC_PAGE_FREE UINT64_C(0)
#define GC_PAGE_PREPARING UINT64_C(2)
#define GC_PAGE_READY UINT64_C(3)
#define GC_PAGE_RUN_TAIL UINT64_C(4)
#define GC_PAGE_CLOSED UINT64_C(5)
#define GC_PAGE_RELEASING UINT64_C(6)
#define GC_PAGE_EVACUATED UINT64_C(7)
#define GC_PAGE_UNCOMMITTED UINT64_C(8)
typedef struct {
    uint64_t heap_base, pages, page_count, hard_max, soft_max, relocation_reserve;
    uint64_t committed, pending, metadata_max, metadata_committed, metadata_pending;
    uint64_t last_identity, last_incarnation, pressure;
} HeapPool;
typedef struct {
    PinRuntime *pins;RootRegistry *registry;uint64_t registry_lock;
    PinNativeLock lock,unlock;HeapPool *heap;const NativeMetadataOps *native;
} NativePinRuntime;
/* Unique native allocation charge, serialized by the owning heap lock. */
typedef struct { uint64_t bytes, owner, allocation, stage; } MetadataCharge;
typedef struct { uint64_t roots, descriptors, cells, entries, capacity, quantum; } NativeStartupRequest;
typedef struct {
    uint64_t page_count, generation, relocation, head_index, incarnation, identity_base, stage, starts, committed_mapping;
} PageLease;
/* Unique, resumable retirement of an uncommitted source run. */
typedef struct {
    uint64_t head_index, page_count, incarnation, stage, owner, starts, forward_table, cursor;
} SourceRetire;
/* Unique cleanup transaction for a never-installed committed run. */
typedef struct {
    uint64_t head_index, page_count, incarnation, stage, owner, starts, forward_table, committed_mapping;
} PageRelease;

typedef uint8_t Generation;
#define GC_YOUNG 0u
#define GC_OLD 1u
typedef uint8_t Phase;
#define GC_IDLE 0u
#define GC_MARK_START 1u
#define GC_CONCURRENT_MARK 2u
#define GC_MARK_END_CHECK 3u
#define GC_SELECT 4u
#define GC_RELOCATE_START 5u
#define GC_CONCURRENT_RELOCATE 6u
#define GC_RETIRE 7u

typedef uint32_t ThreadState;
#define GC_RUNNING 0u
#define GC_AT_SAFEPOINT 1u
#define GC_BLOCKED_NATIVE 2u
#define GC_DETACHING 3u
#define GC_DETACHED 4u

typedef uint32_t GcError;
#define GC_INVALID_SIZE 0u
#define GC_INVALID_REFERENCE 1u
#define GC_OUT_OF_MEMORY 2u
#define GC_UNSUPPORTED_CONFIG 3u
#define GC_INVALID_PHASE 4u
#define GC_EPOCH_EXHAUSTED 5u

static inline bool gc_abi_one_hot(uint16_t bits) {
    return bits != 0 && (bits & (uint16_t)(bits - 1)) == 0;
}
static inline bool gc_abi_valid_metadata(uint16_t metadata) {
    return (metadata & (uint16_t)~GC_METADATA_MASK) == 0 &&
        (metadata & GC_REMEMBERED_MASK) != 0 &&
        gc_abi_one_hot(metadata & GC_YOUNG_MARK_MASK) &&
        gc_abi_one_hot(metadata & GC_OLD_MARK_MASK) &&
        gc_abi_one_hot(metadata & GC_REMAP_MASK);
}
static inline bool gc_abi_encode_ref(uint64_t offset, uint16_t metadata, GcRef *out) {
    if (!offset || offset > GC_MAX_OFFSET || offset % GC_OBJECT_ALIGNMENT ||
        !gc_abi_valid_metadata(metadata)) return false;
    out->bits = (offset << 16) | metadata;
    return true;
}
/* Arithmetic only. Before dereferencing, the runtime must validate committed
 * object-start/forwarding metadata under a lifetime lease/access guard. */
static inline bool gc_abi_decode_ref(GcRef value, uintptr_t heap_base, uintptr_t *out) {
    if (!value.bits) { *out = 0; return true; }
    uint64_t offset = value.bits >> 16;
    GcRef checked;
    if (heap_base % GC_OBJECT_ALIGNMENT ||
        !gc_abi_encode_ref(offset, (uint16_t)value.bits, &checked) ||
        offset > UINTPTR_MAX - heap_base) return false;
    *out = heap_base + offset;
    return true;
}
static inline bool gc_abi_valid_phase(PhaseSnapshot phase) {
    return (phase.bits & UINT64_C(0xe000)) == 0 &&
        (phase.bits & 15) <= GC_RETIRE && ((phase.bits >> 4) & 15) <= GC_RETIRE;
}
static inline uint16_t gc_abi_color(PhaseSnapshot phase) {
    uint16_t epochs = (uint16_t)(phase.bits >> 8);
    return (uint16_t)((1u << (4 + ((epochs >> 4) & 1))) |
        (1u << (8 + (epochs & 1))) | (1u << (10 + ((epochs >> 1) & 1))) |
        (1u << (12 + ((epochs >> 2) & 3))));
}
static inline uint16_t gc_abi_color_unremembered(PhaseSnapshot phase) {
    return (uint16_t)(gc_abi_color(phase) | GC_REMEMBERED_MASK);
}
#endif
