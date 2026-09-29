# Generational ABI and model work in progress

The full requirements are in `klassic-generational-zgc-instructions.md`.
The ABI supports the C lifecycle and independently exercised emitted runtime
helpers. Normal program startup still uses the legacy collector. Neither ABI tests nor the access model
constitute completion of G01-G15.

## Reference format

`gc_abi.rs` and `runtime/gc/gc_abi.h` define ABI version 4, fixed-width 64-bit
addresses, little-endian storage and 16-byte object alignment. The first heap
granule is reserved. Null is the all-zero word and is recognized before base
addition or metadata lookup. A nonnull word is `(offset << 16) | metadata`.
Version 2 appended an atomic allocation-byte counter to MutatorContext, expanding
it to 280 bytes. Version 3 retains that context layout and expands MarkRecord
from 152 to 168 bytes: age_epoch is at offset 152 and age at offset 160.
Context consumers require the current ABI version before accessing runtime state.
The offset is nonzero, aligned, and at most 48 bits; this is a representation
limit, not a promise that the OS can reserve that much virtual address space.

Mark and remap fields have exactly one bit set. Remembered metadata also
permits the double-bit forgotten state:

| Metadata | Meaning |
| --- | --- |
| 4-5 | remembered parity (one bit), or forgotten (both bits) |
| 8-9 | Young mark parity |
| 10-11 | Old mark parity |
| 12-15 | combined Young/Old remap state |
| 0-3, 6-7 | reserved; must be zero |

The four remap bits encode index `young_remap + 2 * old_remap`.
There are 32 store colors and 16 additional forgotten colors, 48 valid metadata
words in total. Initial metadata is `0x1510`. `PhaseSnapshot::color` generates
store colors from five epoch parities. `color_unremembered` sets both remembered
bits; this state fails store comparison for every parity, including after wrap.

A regression first exposed that rejecting double-bit remembered metadata left
no persistent forgotten state. A second graph regression demonstrated a slot
returning to a false store-good parity after two Young cycles. Collector heals
must not claim store bookkeeping they did not perform: remset scanning may use
single-bit color only after remembering the slot and protecting its target;
forgotten slots and other GC-only heals use double bits. Null remains zero.
This design was checked against the fixed OpenJDK baseline's explanation of
[remembered-bit healing](https://github.com/openjdk/jdk/blob/6c48f4ed707bf0b15f9b6098de30db8aae6fa40f/src/hotspot/share/gc/z/zBarrier.inline.hpp#L206-L247).
The Klassic code is independently written; no OpenJDK source was copied.

`decode_ref(value, heap_base)` checks format, base alignment and addition
but deliberately returns an integer, not a dereferenceable Rust reference.
`decode_ref_checked` additionally invokes a metadata validator. At runtime the
validator must prove committed object-start or retained forwarding-source
membership under a metadata lease. Neither interior addresses nor padding may
pass. Runtime metadata leases, source guards and bounded forwarding resolution
are implemented below, while production ownership and compiler-wide guard
coverage remain open. Arithmetic decoding alone is not a complete load barrier.

## Phase word and transition proposals

One release/acquire `u64` publishes both generations coherently:

| Bits | Contents |
| --- | --- |
| 0-3 / 4-7 | Young / Old phase |
| 8 / 9 | Young / Old mark parity |
| 10 / 11 | Young / Old remap parity |
| 12 | remembered parity |
| 13-15 | reserved zero |
| 16-63 | monotonically increasing publication sequence; cannot wrap |

The eight phases are Idle, MarkStart, ConcurrentMark, MarkEndCheck, Select,
RelocateStart, ConcurrentRelocate, Retire. The legal path follows this order;
MarkEndCheck may return to ConcurrentMark without flipping epochs. Each proposed
edge increments the publication sequence. An exhausted sequence is an error.

| Edge | Color change | Required runtime gate (not implemented by the pure ABI) |
| --- | --- | --- |
| Idle -> MarkStart | generation mark parity; Young also flips remset parity | producer adoption, old buffer handoff, root-segment boundaries |
| MarkStart -> ConcurrentMark | none | publish adopted boundaries |
| ConcurrentMark -> MarkEndCheck | none | root/heap/SATB/remset producers accounted for |
| MarkEndCheck -> ConcurrentMark | none | resume unfinished work |
| MarkEndCheck -> Select | none | completed scanners, empty queues and no in-flight publication |
| Select -> RelocateStart | generation remap parity | previous parity reusable; selected pages and access/pin protocol |
| RelocateStart -> ConcurrentRelocate | none | forwarding metadata published |
| ConcurrentRelocate -> Retire | none | copy jobs complete or retained |
| Retire -> Idle | none | metadata/page/epoch retirement obligations |

`transition` constructs a proposal; it does not authorize publication. In
particular, incrementing the sequence cannot make an old colored reference
unambiguous after parity wraps. The coordinator must carry certificates for
these gates, including all live slots/roots/native handles/buffers/jobs. That
coordinator and its full model are outstanding Task 1/9 work.

## Barrier comparison table

All nonnull comparisons require the exact current combined remap bit. During
Old marking, load/mark comparison includes Old mark parity for either object
generation. For Young objects during Young marking it also includes Young mark
parity. An explicit mark access adds its target generation's mark parity.
Store comparison includes every metadata field. These masks are generated by
`PhaseSnapshot::required_mask` from phase/generation/access, not constants in
individual backends.

The input to store comparison is the **previous slot value**. Null always fails
that comparison: null proves neither remembered-set membership nor epoch
adoption. Null load/mark succeeds without accessing heap memory. Coloring a new
value is allowed only after the barrier has completed the required SATB,
remembering and marking work; a good color alone cannot manufacture that work.

## Shared context and buffers

The Rust and C layouts include `RootHandle` (cell address, reuse generation,
scope), `FieldLoc`, `BufferedSlot`, `BarrierEntry`, `BufferCursor`, and
`MutatorContext`. C uses `_Atomic uint32_t/uint64_t` where Rust uses matching
atomics. `tests/gc_abi.rs` compiles C static assertions for every size, alignment,
field offset, and enum/constant value, then runs the C encode/decode/color cases.
The test fails if a C11 compiler is unavailable; it does not report a skip as a
pass. Production direct builds still do not invoke a C compiler.

The context contains the shared phase/state words, private TLAB bounds,
bounded SATB/remset cursors, root-segment/retirement addresses and access epoch.
A cursor has native entry storage, capacity, private producer top, release
publication and acquire-consumed counters. The consumer releases its completion
counter before the producer reuses storage. Counter wrap is not permitted.
A `BarrierEntry` retains the overwritten reference and an immutable buffered
owner/offset/incarnation/record-epoch/metadata-lease tuple. It never retains the
caller's mutable `RootHandle` as a substitute for ownership at enqueue time.

These are interface layouts for subsequent implementation tasks, not currently
running queues. The C collector now embeds this exact `MutatorContext` and uses
its state, identity and TLAB fields. Its former private TLAB triple is removed.
Normal runtime roots and buffers have not been migrated: the legacy shadow
stack remains an explicit adapter extension, and `adopted_phase` stays at the
initial snapshot until the generational phase coordinator is integrated. The
root storage/journal components below run in separate conformance fixtures.

`gc_threads.h` exports attach/detach and enter/leave-native for the owning OS
thread. Context pointers are borrowed until detach; no external scanner may
retain them across slot reuse. Identities never repeat, and exhaustion fails.
The existing handshake mutex serializes registration, retirement, native
transitions and the coordinator's participant snapshot. Native entry publishes
stable legacy roots/TLABs before becoming BlockedNative; the coordinator excludes
those owners from its park count. Native return waits out the request before
becoming Running. Other managed APIs reject BlockedNative access. This is the
legacy quiescent-root path, not concurrent root-segment scanning or buffer
handoff; those require the remaining implementation tasks.

The finite state-publication step is shared between `gc_context.h` and
`gc_codegen/context.rs`. It validates ABI version, expected state, zero access
epoch and the permitted edge, then release-stores exactly the 32-bit state.
Failure leaves the context untouched. The C coordinator uses this step under
its existing registry mutex. Direct lifecycle callers still need coordinator
and ownership integration; this primitive does not establish a handshake,
adopt a phase, transfer buffers or grant raw heap access by itself.

| Current state | Permitted next state |
| --- | --- |
| Running | Running, AtSafepoint, BlockedNative, Detaching |
| AtSafepoint | AtSafepoint, Running |
| BlockedNative | Running |
| Detaching | Detached |
| Detached | none; initialize fresh unpublished storage before attach |

Running/AtSafepoint self-edges allow an empty poll and consecutive rounds.
The coordinator's request/generation protocol distinguishes those rounds;
observing the same state value does not certify new epoch adoption.

`HandshakeState` and `ThreadRegistration` now share Rust/C layouts and finite
operations in `gc_handshake.h` / `gc_codegen/handshake.rs`. Every operation runs
under the registry lock. Begin snapshots `members - native` and advances a
nonwrapping request generation; each registration acknowledges it at most once.
End requires all expected acknowledgements. A new request may start before an
owner resumes, in which case that AtSafepoint owner acknowledges the new round.
Registration, native transitions and detach wait while a request is active.
Detaching owners remain counted while root retirement or buffer work is pending.
They can acknowledge successive requests and resume their detach wait without
changing back to Running; duplicate acknowledgements still change no memory.
Final detach additionally requires an empty owner-root list, zero retiring-root
count, a closed TLAB, and both barrier cursors fully published and consumed.
Private or queued work returns WAIT, while reversed cursor ordering is INVALID.
The final check does not scan or flush any buffer, nor does it acknowledge the
collector jobs that own values already copied out of those buffers.
Failures, including duplicate acknowledgement, leave all supplied storage intact.

One live, nonmoving entry belongs to one context and one coordinator. Copying a
registered entry is forbidden; detached storage is reusable only after external
scanner borrows end. The C runtime enforces ownership with its existing static
registry and mutex. It publishes the separate atomic `gc_handshake_requested`
fast-poll word after Begin and clears it after End, while holding that same lock.
Poll readers then enter the lock before using the plain bookkeeping fields.
The portable emitted steps implement the same bookkeeping, but their production
OS lock/TLS/notification and startup integration remain open. No step scans
roots, adopts a generational phase, hands off buffers or grants raw heap access.

The Linux adapter also supplies a futex-backed registry mutex and single-waiter
sticky event words. Each owner and the coordinator have their own event; clearing
another waiter's event is forbidden. With the lock held, the waiter checks its
predicate and clears its event, unlocks, then waits on zero. Notification stores
one under the same lock and wakes one waiter; a publication before park produces
EAGAIN. All wake/error paths recheck the protected predicate, and synchronization
storage survives join. The raw-clone fixture connects these helpers to the
shared context and emitted bookkeeping; normal runtime startup is still open.

## Owned Young TLAB allocation

`gc_heap.h` and `gc_codegen/heap.rs` implement the same no-safepoint TLAB attempt.
`AllocationRequest` borrows an immutable `TypeDescriptor`, the caller's owned
`AllocationPage`, an element count and private output storage. Descriptors carry
an id, alignment, fixed-prefix size, repeated-element size and precise bitmap
addresses and word counts. Valid alignments are powers of two up to the heap's
16-byte alignment. Registration/bitmap validation is implemented below; the
compiler's descriptor table and production allocation lookup remain open. This
attempt requires an already registered, immutable, live descriptor.

Payload bytes are checked `fixed_bytes + elements * element_bytes`. The allocator
rounds to 16 bytes, reserves at least one payload granule even for zero bytes,
and prepends a 32-byte `ObjectHeader`. Size, descriptor id and a native pointer
to the stable `MarkRecord` are immutable; forwarding must not overwrite them.
The record holds the immutable logical identity and both generations' mark
state. The atomic header state starts as `OBJECT_INITIALIZING`. Identity is
the page's reserved identity base plus payload
granule index; reserving nonoverlapping id ranges across incarnations belongs to
the page slow path. All arithmetic, ownership, page bounds, bitmap bounds and
preexisting object-start checks precede mutation. Failure preserves all storage.

`AllocationRequest.marks` names an owner-private `MarkArena` over admitted,
zeroed, nonmoving native records. The allocator validates the arena extent and
its next free record, then consumes exactly one record and stores its identity
before publishing the header/start bit/allocated top. Absent or full arenas
return `GC_ALLOC_METADATA_REFILL` without consuming heap space or changing
output, bitmap or records. Malformed or dirty free records return Invalid.
Every failure path precedes both heap and arena mutation. The native emitter
performs the same constant-size record check without a payload walk.
`AllocationPage.forward_table` supplies a separately retained physical source
cell. Both metadata reservations are validated before either is consumed; see
[Source lookup before managed access](#source-lookup-before-managed-access).

Refill/growth admission belongs outside this leaf. Already used records must
never be moved by realloc or reset while a physical object copy, root, buffer
or mark job can still use them. Owner cursor fields are not collector inputs;
readers acquire the header/start publication and follow its immutable record
pointer. Copying an object retains that pointer and therefore its shared mark
state. Scanner conformance rejects a different record even when it contains
the same debug identity, and accepts two physical headers sharing one record;
this does not yet implement concurrent copy/forwarding publication.

Page storage must already be committed and prezeroed, belong to this Running
owner and be excluded from relocation. The owner is the sole start-bitmap writer.
After header initialization, release stores publish its object-start bit and
allocated prefix. Readers acquire those publications and the source access guard before reading
immutable header fields; they must not scan INITIALIZING payloads as completed
objects.
The attempt does not walk/zero a payload, allocate metadata, lock or poll. Refill
is a distinct status, never a hidden legacy-collector fallback.

`AllocationResult` is a private no-safepoint handoff, not a root or initialized
object publication. The caller must adopt it into an output root before polling.
The public `gc_alloc_into` wrapper, production root adoption, initialization completion,
mark/in-flight accounting and normal runtime refill/OS preparation integration
are still unimplemented. The page transaction below now supplies the metadata
side of refill and contiguous runs. Testing colors for all phases does not establish black
allocation or concurrent marking semantics. This leaf is not enabled in normal
programs until those obligations are connected.

## Immutable descriptors and bounded shape scanning

`gc_descriptors.h` and `gc_codegen/descriptors.rs` implement matching registry
publication and shape walks. The fixed native `DescriptorRegistry` holds atomic
addresses indexed by nonzero 32-bit IDs. A descriptor and its bitmaps are fully
initialized before a strong acquire-release CAS publishes the address. Duplicate
registration cannot replace a definition. Slots, descriptors and bitmaps remain
alive and immutable until all registry/heap/job users retire; no unregister or
concurrent table resize is provided. Compiler table construction, metadata
accounting and production lifetime management remain open.

Registration checks size limits, alignment, exact bitmap word counts and trailing
bits. Null bitmaps denote scalar-only regions. Nonnull bitmaps contain one bit
per complete eight-byte slot; partial final bytes cannot contain a reference.
An element containing reference slots must have an eight-byte-aligned size and
start after an aligned fixed prefix. Validation trusts the caller to provide
live native buffers of the declared sizes, rather than probing arbitrary native
addresses. Checking the last bitmap word suffices for the trailing-bit rule.

`DescriptorScan` acquires the published definition, validates the payload shape
and resume offset, and returns reference-field byte offsets into a native output
buffer. Each invocation visits at most `slot_budget` bitmap candidates, capped
at 256, and writes at most 256 offsets. Scalar-only regions are skipped with
arithmetic even for huge payloads. Partial chunks retain only a byte offset, not
a managed address. Invalid input preserves request and output storage. The
scanner never loads object fields, treats scalar bit patterns as references, or
walks a graph. A future collector job must retain a rooted logical owner and
phase lifetime, re-resolve the owner and use bounded guards when consuming these
offsets. Consulting a shape does not publish or authorize scanning an
INITIALIZING object's payload.

## Page preparation and budget ownership

`HeapPool` and `PageLease` share layouts between `gc_pages.h` and
`gc_codegen/pages.rs`. A pool owns a fixed table of 128 KiB virtual page slots;
slot zero starts after one reserved guard page. Free slots may form a contiguous
run. Each slot records its run-head index, and the head describes the entire
allocation extent. Only the head owns its start bitmap and can become a TLAB.

Reserve, commit and abort run under the heap lock, without OS calls or waits.
Reserve finds an unused contiguous run and charges pending heap and bitmap
bytes before releasing the lock for preparation. Committed plus pending bytes
cannot exceed hard_max; ordinary allocations also leave relocation_reserve
available, while relocation requests may use it. The metadata limit includes
the native page table, pending/committed start bitmaps and admitted native
allocations through `gc_budget.h`. Production metadata consumers still need
connection to this accounting. Crossing soft_max sets pressure; it
does not implement the adaptive scheduler or block otherwise affordable work.

Each reservation receives a unique nonwrapping incarnation and a disjoint
granule-sized identity range. Aborting does not roll those counters back. A
stale or shortened lease cannot release a later reservation or a subset of its
run. Failed operations preserve every input/output byte.

The preparing actor commits and zeroes memory and allocates the bitmap outside
the lock, recording ownership in `committed_mapping` and `starts`. Commit makes
the head Ready and transfers both resources to the pool, clearing those lease
fields. Abort requires them both zero after successful cleanup; a failed
cleanup retains the reservation and quota for retry. Adjacent aborted runs are
reusable together. This is reuse of never-published preparations, not retirement
of live or formerly published objects.

`gc_tlab_page_locked` / `emit_tlab_page_locked` bind a Ready Young head to an
empty owning context, or close that context's current TLAB. Bind checks size,
bitmap capacity, identity range and ownership. Close preserves its objects and
allocated prefix, clears the context's private bounds and marks the page Closed.
Closed pages cannot go through reservation abort. Reusing their virtual ranges
requires the still-outstanding source/forwarding/color retirement protocol.

The C/emitted conformance pipeline uses real reserved OS mappings, injected
commit/bitmap/cleanup failures, commitment, binding, large payload allocation,
close and refill. Its OS owner is test code. Production direct OS adapters,
heap initialization/configuration and rooted initialization publication still
need to call these operations; the legacy allocator remains the default.

## Machine API transfer rules for subsequent tasks

| Logical operation | Machine/C shape | May safepoint |
| --- | --- | --- |
| allocation | `gc_alloc_into(ctx, desc, bytes, out_root)` | yes |
| reference load | `gc_load_ref_into(ctx, field_loc, out_root)` | yes |
| weak load | `gc_weak_load_into(ctx, weak, out_root) -> bool` | yes |
| root reservation | reserve empty output cell before loading an unrooted value | yes |
| store/CAS | rooted owner, expected/new value and output, with field re-resolution | yes |
| poll/native transition | context plus previously published roots/buffers | yes |
| raw slot access | attached thread, access guard and metadata lease | no |

Encoding/layout and the C owner/native lifecycle are implemented so far. The
new allocation/root/barrier API rows above remain contracts; legacy allocation
and shadow APIs have not acquired their generational semantics. Any new
implementation that can safepoint must publish results into a
pre-reserved output root. Plain temporary return values cannot cross adoption.

## Nonmoving root storage and bounded transfer components

`gc_roots.h` and `gc_codegen/roots.rs` implement matching storage operations.
A prepared `RootSegment` has one mutator owner, a globally unique scope ID,
nonmoving cell storage, an acquire/release published used prefix and a dedicated
SPSC `BufferCursor` journal. Each 32-byte `RootCell` has an atomic value and
owner-private reuse generation/active fields. Scanners only read the atomic
value; they never inspect concurrently changing owner-private fields. Reserve
appends an empty cell, increments its nonwrapping generation and returns a
`RootHandle`. Existing cells are not recycled during the segment's active life.

Store/release validates owner, Running state, absence of an access guard, segment
membership, scope, generation and active state before touching a cell. It copies
the previous value, next value, scope and adopted phase into a `RootChange`, then
release-publishes the journal entry before release-storing the root value. This
also applies to null, same-value stores and clear. A full journal returns FLUSH
without any mutation; a caller must retain its input root while arranging bounded
drain/poll work. Entries retain values, never reusable root handles. The journal
records both old and new referents unconditionally; actual generation-specific
marking, phase admission and in-flight termination are still future integration.

`gc_root_scan` acquires a segment's used prefix and copies at most 256 values
into caller-owned job storage. `gc_root_drain` is the journal's sole consumer;
it acquires the published counter, copies at most 256 complete entries and
release-publishes consumption only after the copy. The producer acquires that
counter before overwriting a physical ring slot. The consumer does not read the
private producer top. Logical counters never wrap. Output references remain
phase-protected in-flight work after transfer; an empty journal alone is not a
termination condition. These components do not create that job/phase protection.

The owner can freeze a segment by release-publishing RETIRING, leaving its used
prefix and root values intact. This is constant work even for a large frame.
Subsequent owner operations reject the segment, while leased scanners may finish
bounded scans of its retained values. RETIRING does not mean reusable. The
registered lifetime protocol below now provides scope IDs, leases, a retirement
queue and bounded cleanup/reuse. Raw freeze remains an unregistered-storage
operation and rejects registered segments; registered owners must enqueue and
freeze in one lifecycle transaction. Production root registry discovery,
allocation, phase/mark handoff and compiler integration remain open.

All storage steps are no-safepoint leaves. READ_RAW returns unresolved reference
bits only, so it is not the relocation-aware public root-load barrier. Store
inputs must already be protected or in a proven no-safepoint transfer. The real
page fixture reserves an empty cell and enough journal capacity before TLAB
allocation, then stores the result before any poll. Production allocation/root
wrappers, compiler locals/args/return lowering, global/weak handle tables,
retirement on detach and native transitions remain open. No operation here makes
INITIALIZING payloads available to object scanners.

## Registered root segment lifetime

`gc_root_lifetime.h` and `gc_codegen/root_lifetime.rs` implement the same finite
registry-lock transactions. Each registry and registration has stable native
storage; a registration belongs to exactly one registry and is never copied.
Attach consumes a prepared segment and assigns a nonwrapping scope ID. The
segment points back to its registration, so raw freeze cannot bypass its queue.
Attach/retire check owner, exact-width thread state, zero access guard and the
owner's adoption of the registry phase. Retirement also accepts Detaching;
BlockedNative still cannot mutate the managed/root state.

A scanner acquires a unique lease control record under the registry lock.
Requests carry the lease's nonwrapping serial and segment scope. One job owns
each live control record; scan/clear/release cannot run concurrently through the
same record. `gc_root_scan_leased` rejects stale serials/scopes before accessing
cell storage. Lease counts retain the registration and segment until the job
finishes using both native storage and copied references. A serial check is not
a substitute for ownership of a live control record or its native allocation.

Retire freezes the used prefix and captures the current phase and both active
mark obligations. It inserts the registration into an intrusive doubly linked
retirement queue in constant work. A deletion worker claims at most 256 cells,
receives a separate lease and processes the retained references outside the
registry lock. The lease records actual scan progress; skipped ranges, a cursor
beyond the claim and ACK before the range was read are rejected. ACK additionally
requires the worker to certify completion for every captured generation. A
partial mark result retains the claim and storage, permitting retry. Reading
values is mechanically checked; the caller's mark visitor must actually perform
the acknowledged graph work. The current fixture visitor validates symbolic
references; production marking/termination is still unimplemented.

Only after every deletion range is acknowledged and all reader leases are gone
can an exclusive cleaner claim the segment. CLEAN unlinks the registration
from the active inventory in constant work; CLEANING prohibits new readers.
The cleaner runs outside the registry lock and clears at most 256 cells per call,
preserving each cell's reuse generation. The published cleanup cursor gates
FINISH. Finish removes the queue entry in constant work, resets membership and
the used prefix, and returns PREPARED storage to its owner for reuse or freeing.
Reattachment assigns a fresh scope; the next cell reservation increments its
generation. Scope/lease exhaustion never wraps or mutates storage. This reuses
native root cells, not managed heap virtual addresses or forwarding metadata.

The bounded inventory walker below supplies discovery transactions. The phase
coordinator must still schedule discovery and hand off root/journal/job references
before advancing epochs or reclaiming managed pages. These components do not
implement that coordinator. Root segment allocation, metadata budgets,
compiler frame linking, scheduling detach drains, native handles, lowering and
the runtime mark visitor remain open.

Each attached registration also links into its owner's doubly linked active
list, headed by `MutatorContext.root_segments`. The registration stores its
immutable context address until FINISH. RETIRE removes that owner-list entry
and increments `retiring_roots`, which is a count rather than an address. Both
updates are constant work, including retirement from the middle of an owner's
list. The root and thread registries must share the same lock. The context cannot
be deregistered or reused while either list membership or retirement count
remains; a worker's cleanup therefore has a live context to update.

FINISH additionally waits for the segment's journal to be consumed before
decrementing the owner's retirement count. It loads consumed before published,
without reading the producer's private top: other segments may share that
journal and still publish concurrently. Root journals publish before each cell
mutation, and a retired segment cannot produce further changes. The journal's
consumer transfers references into protected work before publishing consumption;
empty native storage does not certify completion of graph marking. Native
journal allocation and sharing still require caller-owned stable storage.
Compiler frame allocation/pop and a production detach driver must invoke these
steps, drain/assist outside the lock and service handshakes while waiting.

`gc_detach_step_locked` now orchestrates those finite steps from an already
Detaching owner. Its readonly `DetachRequest` contains the root registry,
handshake registry and participant addresses. Both registries use the same lock.
An active request is acknowledged first; the first acknowledgement returns MORE
so the caller can notify the coordinator, while a duplicate returns WAIT.
Otherwise it validates membership through the Detaching-preserving resume step.
It reports CLOSE_TLAB, FLUSH_SATB or FLUSH_REMEMBERED when owner work remains.
Before retiring the head root, it reports ADOPT_PHASE if the owner's cached
phase differs from the root registry; this return does not perform adoption.
With the phase adopted it retires exactly one segment per call. Once the owner
list is empty it attempts final resource-gated detach, which waits for retirement
and published buffer ownership to clear.

MORE, WAIT and action returns never grant context reuse. The caller drops the
registry lock before notifying, servicing an action or waiting. TLAB close now
accepts Detaching under the heap lock; binding a fresh page still requires
Running. The emitted detach routine calls the shared handshake and lifecycle
emitters with a private stack request, keeping their validation and mutation
logic in one implementation per backend. The current concurrent fixture drives
this orchestration, including reader/journal delays and repeated handshakes.
Production notification/wait integration, flush consumers, phase adoption,
metadata allocation and compiler frame lowering remain outstanding.

## Bounded root inventory discovery

`gc_root_walk.h` and `gc_codegen/root_walk.rs` add an inventory list separate
from the retirement queue. ATTACH inserts at the head using its increasing scope
ID. RETIRE keeps membership until deletion work is acknowledged and all readers
have released their leases. CLEAN then unlinks the node before cleanup/reuse.
All membership changes and walker transitions use the same registry lock.

BEGIN captures the registry phase and active generation mask, takes one reader
lease on the head, and snapshots that segment's used prefix. It performs no
root traversal. One owner holds the noncopied `RootWalk` control; outside the
lock, `gc_root_walk_scan` copies at most 256 atomic root values per call. Appends
after the captured prefix are publication-journal work. NEXT requires the cursor
to reach the limit and the caller to certify the captured mark obligations. It
then transfers the reader lease to the next linked node, assigning a fresh serial,
or completes the walk. A copied scan request's earlier serial is rejected.

The current reader lease prevents its node from being unlinked or reused. An
unvisited neighbor may finish deletion and be removed while the walker is
delayed; the current node's next link is repaired under the registry lock. Its
roots have already been processed by deletion jobs. Reattachment creates a newer
scope at the head, before the existing cursor, so it cannot extend an older
walk indefinitely. Forward traversal checks strictly decreasing scope IDs.
New roots in those newer segments are covered by publication journals. This
coverage argument requires both deletion and publication work to remain phase
protected, including during overlapping cycles; the production coordinator is
still outstanding.

Each live walk contributes one registry walker and one reader lease. Independent
walk controls can pin different nodes concurrently. Empty BEGIN and final NEXT
leave no live lease. INVALID, WAIT and counter-exhaustion returns preserve all
storage so the owner can retry; there is no cancellation operation. The caller
must retain the unique control and its registry until completion. The fixture
visitors validate symbolic identities, not managed-graph marking. Counts alone
do not implement termination, phase admission, root allocation or compiler roots.

## Barrier buffer transport

`gc_buffers.h` and `gc_codegen/buffers.rs` transport complete 48-byte
`BarrierEntry` records through the context's SATB and remembered buffers. The
record retains the previous reference and the enqueue-time owner, field offset,
page incarnation, record epoch and metadata-lease token. This component moves
already protected entries; it does not acquire a lease, perform the store
barrier, mark a referent or resolve forwarding. The eventual barrier must acquire
all required protection before append. A failed/full append retains that input
obligation, including across any separately permitted retry or safepoint.

The sole owning mutator appends to the private suffix and advances plain `top`.
Only a Running owner may append. An explicit publish release-stores `top` to
`published`; Running, AtSafepoint and Detaching owners may publish. These are
no-safepoint leaves and may execute within a heap access guard, but FULL never
permits waiting with that guard. Counters cannot wrap; geometry, overflow and
counter-order validation precede mutations. A buffer is prepared bounded native
storage, and producer/consumer records and output storage must be disjoint.

One worker per buffer acquires `published` and copies at most 256 records into
its protected job storage before release-advancing `consumed`. The consumer
never reads producer-private `top` or keeps a source-entry address. The emitter
calculates the initial physical ring position once, then advances sequentially
and wraps at its end; it copies all six words before allowing producer reuse.
Copied reference and lease obligations remain live after the buffer empties.
Collector termination must count that work, and scheduling/registry borrows
must prevent another drain attempt through a freed context. Those protocols are
not established by the ring counters alone.

The detach fixture now fills both buffers with real entries before entering
Detaching, services its flush actions using publish, and waits for separate
worker drains before deregistration. It validates every transferred field while
delaying SATB and remembered consumption independently. Entry references and
lease tokens in that detach fixture are still symbolic. The separate lease
fixture below connects real tokens to this transport. Production mark visitors,
remembered bitmap updates and compiler barrier integration remain outstanding.

## Native metadata budget transactions

`gc_budget.h` and `gc_codegen/budget.rs` reserve, commit, abort and release
native storage charges against the page pool's existing metadata counters.
All operations share the heap lock with page preparation. Admission includes
both pending and committed bytes, so simultaneous page and mark-storage
reservations cannot each spend the same remaining capacity. The operations
perform constant work without allocation, traversal or waits. Checked byte
counts include requested capacity and padding, not hidden allocator overhead.

A unique `MetadataCharge` binds immutable bytes to its heap. Reserve increases
pending bytes before OS/native allocation. Commit requires a prepared, aligned,
nonwrapping allocation and transfers pending to committed. Abort requires
successful cleanup first; release additionally requires independent retirement
of every consumer before deallocation. Clearing the allocation field asserts
successful external cleanup; this API does not prove lifetime or perform free.
Failed cleanup keeps the charge. Invalid transitions and exhausted admission
preserve all state, and release cannot consume the page table's base charge.
Closed charges cannot be replayed; the caller must not copy or mutate held
charges, just as for the unique page reservation transaction.

Conformance covers 214 C/emitted state transitions, exact limits and arithmetic
boundaries, plus two reservation orders competing with actual page-pool steps.
The real heap graph fixture uses a dynamically allocated, budgeted block for
all 16 task slots, six 152-byte mark records and 32 metadata lease slots. Each
of 64 rounds rejects a limit one byte below the block size before any object
allocation, checks unchanged TLAB/bitmap/results, aborts an injected preparation
failure, retries at the exact limit, and releases the charge only after both
workers join and every job/lease drains. The fixed fixture block is not the
production growable record/lease allocator. Default-program admission, other
metadata classes, allocation stalls and recovery/OOM policy remain open.

## Page metadata leases

`gc_metadata.h` and `gc_codegen/metadata.rs` provide matching acquire, resolve
and release operations under the same heap lock as page reservation. Stable
native storage holds a `MetadataLeasePool` and a bounded slot array; a prepared
one-based free list permits constant work without allocation or scanning under
the lock. The graph conformance fixture admits its slot array through the
shared metadata budget; production allocation and exhaustion recovery are not
yet wired. Lock acquisition belongs to the eventual slow path, outside a raw heap
access guard; the existing append/drain leaves do not acquire that lock.

An acquire validates page-table membership, run head, incarnation and
Allocating/Closed state. It increments the page's `metadata_leases` count and
returns `(slot_generation << 32) | (slot_index + 1)`. Each token has one release
obligation within its owning pool. Resolving validates the token and returns
the retained page/incarnation; releasing also checks that identity, decrements
both counts, clears the request and returns the slot to the free list. Slot
generation and holder-count overflow are rejected before mutation. A stale
token cannot resolve or release a new lease in the same slot. Table storage
must remain stable for the heap lifetime; this is not an arbitrary-pointer API.

`AllocationPage` is now 112 bytes, including its stable forwarding-table pointer. Page reservation excludes held slots, and
commit/abort reject a pending reservation whose metadata unexpectedly has a
holder. Lease release is independent of page state so a future quarantine can
retain metadata until its last job finishes. A zero count does not authorize
unmapping or reusing a published page: source/color retirement and forwarding
epochs still need their own protocol. A metadata lease neither marks nor pins
an object, and reference liveness must be protected separately.

The concurrent fixture prepares actual OS mappings and binds/closes a page,
then appends a real lease token to alternating SATB/remembered rings. A full
append leaves its lease with the caller, which rolls it back. A delayed worker
copies an entry, empties the ring and retains its token until processing ends.
Both before and after drain, fault injection temporarily marks the held page
Free and confirms reservation skips it without changing its metadata. This
is a reuse defense test, not an implementation of published-page retirement.
After release, a new lease in the same slot rejects the old token. Payload
references are symbolic, so this test establishes metadata ownership rather
than moving-graph correctness.

## Acquiring metadata from a strong source reference

`gc_reference_lease.h` and `gc_codegen/reference_lease.rs` compose page selection
with the existing heap-lock lease allocator. `ReferenceLease` starts with a
nonnull strong current or forwarded-source reference and an empty output. It
checks color/alignment,
heap/table arithmetic, guard/range bounds, selected page/run-head geometry,
incarnation/generation consistency, live head/tail states, acquired allocation
prefix and the acquired object-start bit. A tail page routes to the run head;
no heap header or payload is dereferenced during selection. The resulting
unique token belongs to the head's metadata, including its bitmap. Pool failure
preserves the entire output via a private temporary request.

The caller already owns a strong reference and must observe source/color
retirement. This is not forwarding resolution, a keep-alive barrier or a raw
access guard, and cannot distinguish stale reference bits after premature
address reuse. Holding the heap lock stabilizes geometry and bitmap lifetime;
mutator allocation publishes start bits before allocated_top with release,
so an acquired prefix does not expose an uninstalled start. Metadata retention
alone does not prevent physical relocation or establish strong reachability.

Conformance exercises 1,701 C/emitted cases across both generations, all 48
legal color combinations, Allocating/Closed heads, varying run positions,
tail-page starts and last granules, malformed geometry, nonstarts, unfinished
prefixes and exhausted lease pools. The real two-worker graph alternates
1,024-slot arrays and 16,384-slot arrays spanning two pages, and places its live
run at varying page indices. Graph offers and the SATB owner acquire from their
references; completed jobs resolve their tokens before release. The run head
is no longer supplied as an assumed first page. Production barriers, moving
source resolution and global phase termination still require integration.

## Mark ownership and bounded heap slices

`gc_mark.h` and `gc_codegen/mark.rs` connect logical mark stamps to queued and
in-flight work. A stable `MarkRecord` holds an immutable object identity and
independent Young/Old full-cycle epoch stamps; all physical copies must share
that record. One queue lock per generation serializes stamp and task access.
The reference's one-bit color is not a substitute for these monotone epochs.
Record allocation, mapping from real objects and record retirement are still
production integration work.

A prepared native task array supplies a free list and FIFO. OFFER validates
protected reference/incarnation/lease inputs, rejects future stamps and commits
the stamp and queue insertion together. DUPLICATE and FULL leave the input and
all metadata unchanged, with protection still owned by the caller. The producer
must release duplicate protection or retain unfinished work for retry. A full
queue does not certify a marked-but-unqueued object. The overflow admission
below avoids waiting for a task slot while a parent claim is held. Native
metadata admission, private queues and work stealing remain open.

TAKE transfers a queued task to a serial-checked worker claim and increments
`in_flight`. YIELD returns its cursor to the FIFO without allocating a new slot;
it refreshes the claim serial, so a copied older claim cannot finish a later
scan. FINISH frees the slot and returns the reference/metadata obligation to the
request for release. Serial exhaustion is explicit and never wraps. The caller
must offer/protect all children before advancing the cursor or finishing. CHECK
examines only the bounded queue counters; EMPTY is not collector termination.
The coordinator must also account for private buffers, root scans, publication
and outstanding producers before the phase may end.

`gc_mark_scan.h` and `gc_codegen/mark_scan.rs` read at most 256 descriptor
candidates and acquire-load reference slots into protected native output. The
caller supplies an exclusive claim, a canonical receiver and a finite raw
access guard; this helper does not establish or resolve those prerequisites.
It acquire-checks the Published header state before reading immutable metadata,
rejects Initializing objects, and leaves the job cursor unchanged. The returned
next cursor is committed only after the caller transfers all returned child
obligations. Scalar regions are skipped by the existing descriptor scanner. Incomplete
slices resume at aligned slots; FINISH also accepts a terminal cursor at an
unaligned payload end (for example, a seven-byte scalar object).
The generated helper calls that same emitted scanner and then converts offsets
to atomic loads, without allocating, locking, polling or retaining raw slots.

The integration fixture uses real OS mappings, TLAB allocations, descriptors,
root cells/journal, metadata leases, two workers and the emitted queue/scanner.
It traverses 1,024- or 16,384-element arrays in eight-slot slices, a shared cyclic graph,
and an object whose only edge is deleted before its owner is scanned. The
producer records that old value in a private SATB entry before the null store.
After ordinary graph work finishes, the queue is empty but the unflushed entry
still owns work. Publishing/draining that entry marks the otherwise missed
object. The unreachable object remains unmarked and all job/entry leases are
released. Across 64 rounds the test alternates Young/Old queue stamps; it does
not yet overlap moving generations. Heap residency is fixed during each round,
object publication and receiver canonicalization remain fixture operations.
The allocator now installs the record in the header, and graph admission uses
that pointer directly instead of searching a fixture object list. The final
mapping teardown happens after both workers join. This demonstrates actual
mark traversal, not reclamation, production barrier coverage or termination.

## Progress with saturated mark task slots

`gc_mark_spill.h` and `gc_codegen/mark_spill.rs` compose the queue operations
under the same generation lock. Each nonmoving logical `MarkRecord` reserves
one zeroed overflow cell per generation before it can be published. When an
ordinary OFFER returns FULL, OFFER_OR_SPILL links the appropriate cell into a
FIFO and commits the mark stamp with its protected reference and metadata-lease
obligation. The input clears only after that transfer. Duplicates, malformed
cells and serial/counter exhaustion leave all ownership unchanged.

The queue's CHECK includes overflow work. REFILL validates and transfers at most
one overflow entry to a free ordinary task slot, preserving its serial, record,
reference and lease; it clears the embedded cell afterward. A worker calls
REFILL before TAKE. If all ordinary slots are claimed, the holders can still
record children, advance a bounded scan and yield/finish. They need not wait
for a task slot or recursively scan the child. An empty ordinary queue with
pending overflow is still busy. New offers defer behind existing overflow even
when a slot is free, so new producers cannot take those slots ahead of the
oldest deferred work.

For a finite graph and available reference/metadata protection, a claimed task
can finish its finite scan, which releases a slot for deferred work. Each new
logical object gets one job per generation/epoch; duplicate edges do not grow
the queue. All list changes and transfers have constant work. No allocation,
heap/root traversal, array resizing or worker wait occurs in this helper.
This closes task-slot saturation in the prepared-storage path, not arbitrary
native metadata OOM. The metadata-lease pool can still exhaust; its admission
and failure/progress policy, worker scheduling and global phase termination
remain separate work.

The current representation expands `MarkRecord` from 24 to **152 bytes**
(two 64-byte overflow cells) and `MarkQueue` from 80 to **104 bytes**. That
per-object storage must be charged to production metadata budgets before
publication. The graph fixture now admits and allocates these records with
`MetadataCharge` before object allocation; production allocator integration
remains open. Cells
are reused after draining, not accumulated per cycle, and the owning record
must outlive both generations' jobs and physical object copies.

The real graph fixture now alternates **1, 2 and 16 ordinary task slots** with
two workers. One-slot rounds force child admission while the only slot is held
by its parent. Deferred work drains, both generation cells clear, spill/refill
counts match, and no page metadata lease remains. The same cyclic graph, seven-byte leaf and delayed SATB deletion run with
1,024- and 16,384-element arrays; each round examines array length + 3 slots
in array length / 8 + 4 slices.

## Source lookup before managed access

`gc_forward_table.h` and the shared `forward_table.rs` emitter binary-search a
nonmoving `ForwardTable` by source offset, then verify incarnation. The table
is installed on the run head before its first allocation, retains immutable
backing/geometry through retirement, and is kept alive by the run metadata
lease. Lookup never forms or dereferences a managed address. Its acquired
`used` prefix contains immutable, strictly increasing source keys; an already
initialized but unpublished suffix is invisible. Capacity is at most UINT32_MAX,
so lookup needs at most 32 probes and no lock, allocation or retry loop.

Young allocation now reserves one zero ForwardCell in addition to its stable
MarkRecord. It validates table geometry, free-cell state and predecessor order
before changing any heap or metadata state. Missing/full tables return metadata
refill with all inputs and output unchanged. A full table cannot be resized or
replaced while live; the slow path must close the TLAB and prepare another run.
The owner initializes offset/incarnation and release-publishes the new prefix
before the object-start bit and allocated_top. A valid strong reference plus
reference-directed metadata acquisition therefore sees the cell before any
header read. Lookup alone establishes neither object publication nor liveness.
The source guard must still be entered separately, and source/color retirement
must precede virtual range reuse.

Conformance covers 2,329 lookup and 2,776 allocation cases. The shared concurrent
allocation fixture now performs lookup and guard entry before checking each
header, with C or emitted allocator/lookup/guard callbacks. The real two-worker
mark graph obtains a cell from its leased run head before offer-time header
reads and before each bounded scan, then releases its guard before offering
children. Its admitted GraphStorage budget includes the table and six cells.
These fixtures use explicit Running context stubs and stationary source pages;
production worker registration, moving-reference resolution, metadata refill
and retirement remain open. Table lookup is a prerequisite for complete barrier
integration, not default collector enablement.

## Source-local access admission and drain

`gc_source_access.h` and `gc_codegen/source_access.rs` pair every raw source
access with a unique owner-private `SourceAccessRequest`. ENTER validates the
Running context and source key, CAS-increments the low 63-bit count, and marks
`context.access_epoch` with the source incarnation. EXIT CAS-decrements before
clearing that marker and the held scope. The marker prevents safepoint/native
transitions, allocation or nested guard entry; it is not PhaseSnapshot adoption.
No address may escape the scope. A metadata lease and a strong root protect
cell lifetime and reachability separately.

The forwarding claim winner SELECTs the high closed bit with a CAS on the same
word that entrants update. Entrants already racing claim/selection either fail
or remain counted; later entrants are rejected. CHECK permits copy only at the
exact closed/zero-count word. EXIT release RMWs and the collector's acquire
check order the participating actors' writes before copy, including a release
sequence through multiple exiting actors. Both mutators and GC actors must use
the protocol; actor classification cannot exempt a healer or scanner.

Each operation attempts at most one CAS. Contention returns WAIT without
changing output. Failed ENTER can be handled outside a guard. Failed EXIT must
retry immediately, keeping the guard and forbidding poll/yield/worker waits.
Once admission closes, failures witness other departures (or the close), so
fresh entry cannot starve draining. Collectors waiting on CHECK hold no guard
or data lock. No stack/root traversal or global stop is introduced by this leaf.
This local protocol still needs production barrier/worker coverage and phase,
selection and source/color-retirement integration; it does not measure latency.

After the winner abandons and cleans an unpublished copy, RETAIN publishes the
retained decision. REOPEN then CASes a drained retained gate to open, allowing
source accesses again. A published source cannot reopen. Fresh cycle entries,
reset, metadata allocation and reclamation remain outside this API.

Conformance covers 154 C/emitted cases, including count/identity limits,
duplicate/nested scope use, collector tickets, closed drain and retained-only
reopen. The copy fixture admits two source users before two relocators race,
checks failed native transition with a guard, pauses the users through selection,
rejects late entry, then checks their completed writes in the copy. The fixture
uses a controlled spin point to simulate a delayed guard, without a GC poll or
OS yield inside it. Real generated access sequences must remain bounded.

## Registered worker rendezvous and snapshot adoption

`gc_worker.h` / `gc_codegen/worker.rs` connect owner-side phase adoption to the
existing handshake and root registry. All operations use their shared registry
lock. The coordinator selects one valid `RootRegistry.phase` before BEGIN and
holds it fixed through END. ATTACH inherits that snapshot while registering a
fresh owner; POLL acknowledges an active rendezvous and adopts its coherent
snapshot before the registry lock is released. A parked owner only resumes
through a successful POLL after END. Native-blocked owners remain excluded
from the expected ACK count and adopt on LEAVE before executing managed work.
Changed snapshot bits require a strictly newer sequence; no wrap is accepted.
The handshake generation remains distinct from this phase sequence.

The helper rejects active raw guards. An open TLAB or private SATB/remset prefix
returns a close/flush action to perform outside the lock before retrying. Root
writes already publish their journal before returning. Unconsumed published
entries are allowed to remain holders; ACK never means they have been drained
or marked. No root/heap traversal, allocation, blocking or metadata release is
introduced into this bounded registry operation. The OK status completes the
requested operation: after ENTER the owner is still native-blocked. A Running
owner whose ENTER returns RETRY must POLL before parking; a blocked LEAVE retry
may keep waiting outside the lock.

The actual two-worker mark graph now uses registered OS owners rather than
invented context identities. Each worker polls between bounded slices after
all copied children are offered, while any retained parent remains counted
in-flight. A forced rendezvous holds one parent's claim and another genuinely
native-blocked idle worker. The former ACKs without a raw address, the latter
adopts on reentry, and graph work completes under the new snapshot. The owner
root is registered, scanned through a storage lease, journal-drained and retired
through deletion/cleanup before detach; every participant detaches before native
storage teardown. Snapshot selection and OS notification/wait policy are still
fixture orchestration, not a production cycle coordinator. Producer accounting,
global mark termination, color retirement and compiler/default wiring remain
outstanding.

## Mark claim retargeting after relocation

`gc_mark_retarget.h` / `gc_codegen/mark_retarget.rs` transfer an exclusive
in-flight mark claim to a prepared destination. The 120-byte MarkRetarget names
the existing claim and owns a prepared destination MarkRequest; its retired
output starts empty. Under the generation queue lock the helper validates the
queue, authenticates every claim field against its task, requires matching
logical MarkRecords/current generation stamp, and verifies distinct source and
destination metadata tokens. The destination is obtained through rooted
forwarding, metadata acquisition and guarded mark preparation. No heap access,
source guard, descriptor scan, allocation or wait occurs inside retargeting.

Success preserves the processed aligned cursor, logical record, queue links,
queued/in-flight/overflow counts and generation stamps. Task and claim adopt the
new reference/incarnation/token together, and a fresh serial invalidates copied
old claims even for a same-address refresh. The destination input clears; the
retired output receives the old protected reference/incarnation/token for
cleanup outside the queue lock. Old metadata remains held until that obligation
is resolved and released. Rejection/serial exhaustion preserves both inputs and
all queue state. Completed nonaligned cursors use FINISH rather than retarget.

The OS-backed fixture allocates a 513-slot array and three children using the
actual pool/TLAB/descriptor/record path. A one-slot mark queue scans three slots
and records children before advancing the cursor. It then moves the array in
bounded copy slices, protects its old page, follows a worker root, prepares the
destination, retargets the claimed job and releases only its old lease. Further
1/7/64-slot scans verify all 513 positions exactly once; children spill while
the parent owns the only task slot, then refill/scan/finish. Both generation
queues preserve their stamps and allocator-owned identities. This verifies
Old queue survival across a Young object's move as a component; production
Old/Young cycle overlap, worker registration, relocation assistance, phase
eligibility and global termination are still outstanding.

## Guarded preparation of mark work

`gc_mark_prepare.h` / `gc_codegen/mark_prepare.rs` obtain the logical MarkRecord
from a canonical receiver under its existing finite source guard. The 112-byte
GuardedMarkPrepare contains scope/page/phase/reference, a uniquely owned and
already validated metadata lease, and an empty MarkRequest output. Shared object
validation with field loads/stores checks context/adopted phase, the held source
cell and count, page bounds and the published header before touching metadata
inside the header. A pending relocation claim is allowed while this earlier
entrant still holds its guard. A Published forwarding cell rejects the access.

Preparation validates the reference/key/incarnation, descriptor ID and stable
record, then transfers the input lease obligation into the pending MarkRequest
and clears the input lease. It preserves the guard and never changes mark stamps
or queue state. Rejection preserves all state. No managed pointer escapes. The
caller exits the guard before acquiring the generation queue lock; the pending
request remains protected producer work until OFFER/overflow admission transfers
it. DUPLICATE retains its own token, which the caller resolves and releases;
another task's token must remain held. Phase eligibility, worker registration,
pending-producer accounting and termination are not established by this leaf.

The two-worker graph now calls this helper instead of manually reading headers
and constructing mark offers. Actual field-load integration follows moved output
roots, prepares offers under a new guard, queues them for either generation,
checks a duplicate's independent lease, scans the pointer-free payload under a
fresh guard, and completes/releases the claimed job. Receiver/referent old pages
are protected throughout this handoff. This verifies composition and ownership;
queued work moving again still needs production forwarding/phase scheduling.

## Bounded rooted access

`gc_root_access.h` and `gc_codegen/root_access.rs` combine a rooted forwarding
handoff with canonical source-guard acquisition. The 128-byte `RootAccess`
contains `RootFollow` and `ResolveRequest`. Under the heap/lease lock, one call
reads the live root and either journals one forwarding destination into it or
returns READY (8) with a canonical guard and metadata lease. Existing root-follow
statuses cover null, invalid inputs, busy forwarding, journal pressure, lease
pressure, exhaustion and exceptional cleanup. Non-READY ordinary returns clear
the access output and release the acquired lease; the root protects the next
retry. An already owned access request rejects reuse without changing ownership.

READY requires unlocking before payload access, exiting the guard before any
poll/native transition, and releasing the metadata lease under the lock. The
operation never reads managed memory, polls or allocates. It does not establish
marking, recoloring, source retirement or address reuse. The remembered worker
now uses this operation with a registered owner root and polls between hops.
Journal offers and mark scans still use a fixture-only no-poll resolver with
retained strong producers and sufficient metadata capacity; production barrier
scheduling for those paths remains outstanding.

The conformance fixture compares 1298 full-state C/emitted cases and exercises
32 two-edge chains with busy forwarding, a full journal, a modeled blocked-native
interval and final canonical guard acquisition. The chain's native interval is
a state-machine test, not an OS handshake or concurrent retirement proof.

## Rooted forwarding handoff

`gc_root_follow.h` / `gc_codegen/root_follow.rs` compose the existing root,
reference-lease, forwarding-table and forwarding-cell primitives. A 72-byte
`RootFollow` contains the owning context, an existing RootRequest with zero
input value, and an initially empty exceptional-cleanup obligation. The caller
holds the heap lock and stable native root/lease storage. Each STEP rereads the
root, acquires its run-head metadata, reads one forwarding edge, journals and
stores a published destination into the same root, and releases the source
metadata before returning ADVANCED. No unrooted destination or raw guard is
returned, and no managed address is formed. A chain requires separate calls;
the caller unlocks before every wait, flush, allocation or poll.

Null returns DONE without metadata access. Empty/Retained forwarding also
returns DONE, but this observation grants neither immobility nor markedness:
subsequent payload access must still resolve and enter its source guard. A
claimed cell returns WAIT. A full root journal returns FLUSH, leaves the old
root unchanged and discards the private destination. Retry starts at the root.
Successful acquisition consumes a token generation even if lookup or root
publication subsequently rejects; holder counts are balanced on every ordinary
return. If an internal release invariant fails, CLEANUP_REQUIRED preserves the
exact token/page/incarnation in the request and blocks further STEP calls until
explicit CLEANUP succeeds. Under valid heap-lock/disjoint ownership release
cannot fail. This does not roll back lease generations or retire source pages.

The C/emitted conformance oracle checks complete fixture state, including the
expected token-generation consumption, journal entry, root and cleanup ownership.
Protected-page integration follows A -> B -> C with native transitions between
rooted hops and forced journal-full retries. The actual field-load fixture now
moves both the receiver and the loaded referents, waits on a claimed referent,
protects both old pages, follows the output root, and enters a new guard before
checking the destination payload and allocator-owned logical identity.
Phase/mark integration, production retry/refill scheduling, source/color
retirement and compiler barrier funnels are still separate outstanding work.

## Bounded forwarding resolution and guarded access

`gc_resolve.h` / `gc_codegen/resolve.rs` compose table lookup, forwarding READ
and source admission without touching managed memory. A `ResolveRequest` carries
one nonnull source reference, an initially empty destination, and the unique
source scope. The caller already holds strong reachability and the run-head
metadata lease. ENTER validates the source reference/page/context, then returns:

- OK: a live guard permits bounded source header/payload access under the
  existing initialization/publication rules. The context cannot poll or block.
- FORWARDED: an acquire-observed destination for exactly one forwarding edge;
  no source guard or raw address is held.
- WAIT: a claimed/closed source or guard CAS contention; nothing is held and
  the owner waits/retries outside guards and data locks.
- INVALID or EXHAUSTED: preserved request, context and metadata state.

The metadata lease may refer to a physically decommitted source: run geometry,
start bits, table and cell remain retained, and neither reference lease
acquisition nor resolution reads that source's header. Premature virtual range
reuse is still forbidden. EXIT matches the held scope to the leased page/cell,
releases the count and clears the scope key. A contended EXIT must retry
immediately without polling and keeps its guard alive until it succeeds.

FORWARDED is a private no-safepoint handoff, not an unrooted value that may cross
a pause. Before polling, the caller must store it in an already prepared root;
if root journal publication returns FLUSH, discard the private value before
polling and restart from the original strong root. The next bounded resolution
step reacquires metadata for the rooted destination. There is no internal chain
walk or wait loop that could grow an access interval with chain length.

The shared fixture moves A to B to C, protects both old source mappings, and
resolves from A to guarded C while preserving payload and logical MarkRecord.
B moves either before or after its root adoption. Synthetic native transitions
occur only after successful root publication, and are rejected with a live
final guard. Forced full root journals exercise discard-before-poll and retry.
The competing-copy fixture now enters/exits its source users through the
composed resolver; late access returns WAIT and publication returns FORWARDED.
The actual mark graph also uses the composed resolver for header and slice
access. Graph offers release the heap/queue lock after acquiring their metadata
lease, acquire/release the source guard outside that lock, and only then lock
again for queue admission. Thus even CAS retries cannot wait with that lock
held. The graph retains its stationary-source/Running-context fixture scope.

This implements the bounded mechanism, not the complete safepoint-capable
public `gc_resolve`/load barrier. Production root/phase adoption, canonical
slot healing, worker registration, metadata refill and source/color retirement
remain necessary before default collector integration.

## Atomic forwarding ownership and publication

`gc_forward.h` and `gc_codegen/forward.rs` implement a stable `ForwardCell`
keyed by immutable source offset and page incarnation. Its forwarding state
is one atomic 64-bit word: zero is Empty, `(ticket << 2) | 1` is Claimed,
`destination_ref | 2` is Published, and exactly 3 is Retained. Legal GcRef bits
0 and 1 are reserved zero, so a published word carries the full destination
and state without a second, potentially inconsistent load.

CLAIM uses one strong CAS. Only its winner owns the nonzero 62-bit ticket and
may publish or retain that entry. Contenders receive Busy/Published/Retained
without modifying their output. PUBLISH validates a legal distinct destination
and CASes the owning ticket to a release-published reference. READ acquires the
word, validates the tagged reference and returns it only when Published. RETAIN
CASes the same owner ticket to Retained after abandoning its private candidate.
An additional access word counts source guards and closes new admission.
PUBLISH and RETAIN require it to be closed with zero guards.
All malformed keys, states, colors and stale-owner transitions preserve state;
there is no reset of a live entry or rollback of published forwarding.

This leaf does not freeze a source, copy its body, allocate/reclaim destinations,
resolve nested forwarding, or prove that copying completed. Before copying,
the caller must finish the source-selection epoch protocol for mutators AND
GC actors, including healers. Before publication it must fully initialize and
protect the destination, preserve its logical MarkRecord, and establish any
promotion/remset handoff. Table and source lifetimes require their separate
metadata/color retirement proof. Retention is a cycle decision, not permanent
pinning or a replacement collector.

Conformance includes 1,673 C/emitted transitions (full-width keys/tickets, all
48 destination color combinations, stale owners and irreversible terminal
states). In 64 two-worker rounds, both workers prepare real OS-backed candidate
pages before racing; the loser decommits its candidate. The winner stops halfway
through bounded C/emitted copy slices (at most 256 bytes), while the reader
checks Busy.
After completion the acquire reader verifies header, bytes and the shared mark
record. A mark arriving during copying remains visible through that record.
Injected abandonment decommits the winner's partial copy and retains the source.
Two source users now enter the actual guard protocol, remain active through
selection, then write before leaving. Copying waits for both and preserves
those writes. A late entrant is refused, and retained sources reopen after
cleanup while published sources remain closed. This validates the participating
actors' exclusion, not compiler-wide guard coverage, phase adoption or default
concurrent relocation. Copy and complete-copy publication now use the shared
runtime leaves below; candidate OS preparation/cleanup remains fixture C code.

## Bounded physical copy and complete-copy publication

`gc_relocate.h` and `gc_codegen/relocate.rs` implement the same owner-private
`RelocationCopy` cursor. The caller retains source metadata and liveness plus
an exclusive committed destination reservation; immutable limits delimit those
reservations. Every call checks the source incarnation and claim ticket, then
acquire-checks the exact closed/zero-count gate before reading any heap byte.
An undrained gate returns WAIT without mutation. Source selection must cover
all actors; the claim alone is not an exclusion proof.

The source must have a Published header. Checked base/offset/extent arithmetic,
minimum 16-byte payload reservation, descriptor/record fields, and disjoint
whole object ranges precede mutation. Stage 0 requires an empty destination
header and cursor zero. Initialization copies size, descriptor and the SAME
logical MarkRecord pointer, sets Initializing, and copies no more than the
requested budget (1..256 bytes). Stage 1 checks the matching private header
and a nonterminal cursor before copying another bounded slice. Padding remains
untouched. Completing the exact payload release-publishes the header and moves
the cursor to stage 2; a completed request is idempotent while its claim remains
owned. Rejection preserves both heap and request. No raw pointer is returned;
addresses are recomputed on each invocation and both reservations remain
protected across between-slice scheduling.

`gc_relocate_publish` requires stage 2, validates the destination color and
revalidates complete-copy header/cursor/ownership with no further payload copy,
then publishes the forwarding CAS. Neither a fresh nor a partial cursor can
publish. Destination start/table metadata and promotion/remset obligations must
already be installed by the caller. The lower-level forwarding leaf remains
available for its separate claim/retain operations and conformance tests.
Abandoned copies stay private; cleaning their destination precedes retaining
and reopening the unchanged source. Neither leaf allocates, polls or performs
OS resource cleanup.

Conformance checks 4,335 copy cases (both address orders, zero/odd payloads,
several budgets, boundaries, overlap, malformed headers/cursors and byte-for-byte
failure preservation) and 217 complete-copy publication cases across all 48
legal colors. The delayed-source/two-relocator fixture uses these C or generated
callbacks for every copy slice and publication, including partial abandonment
and a concurrent Old mark update visible via the shared record. The protected
A-to-B-to-C/root-handoff fixture uses the same callbacks. Production candidate
allocation/metadata admission, promotion/remsets, phase scheduling, retirement
and compiler-wide guard coverage still remain open; this is not default
Generational enablement or a latency measurement.

## Pool-backed relocation target metadata

`gc_relocate_target_locked(page, copy, op)` uses the same C/PortableAsm leaf
contract for PREPARE and INSTALL. The heap lock serializes metadata changes;
the collector must additionally own the run exclusively across copy slices,
installation or cleanup. Existing installed objects can retain reader leases,
but no other allocator, copier or selector may claim that run. This ownership
and its eventual transfer are scheduler obligations, not properties proved by
an arbitrary RelocationCopy pointer.

PREPARE accepts a pool-committed Ready/Closed Young or Old run with an admitted,
fixed forward table. It validates page geometry, bitmap capacity, top and the
next empty forwarding cell, and derives destination/header and committed end
from that top. It changes only the private request. Copy checks the actual
payload extent against this end. Neither the top nor the start bitmap advertises
an unfinished candidate. Lack of at least 48 bytes returns REFILL; a missing or
full forwarding table returns METADATA_REFILL without mutation.

INSTALL requires a completed cursor and Published destination header with
matching payload size, descriptor and aligned shared-record pointer. All checks
precede mutation. It appends the destination's physical forwarding cell, then
release-publishes table usage, its start bit, the new top and Closed page state.
It allocates no new logical identity and never touches the source MarkRecord.
Subsequent copies append to the same run; an interior run-tail object resolves
through the head's installed bitmap/table. A failed or partial copy can leave
top unchanged, but its exclusive owner must clear the dirty private range
before a fresh copy job reuses that space. Installed candidates cannot roll
back through this API. Source/promotion/remset handoff still precedes forwarding
publication, and neither target operation performs OS work or polls.

The 240 C/emitted state cases include complete failure preservation, repeated
installation rejection, table/bitmap corruption, both generations and multiple
objects. ARM64 replays the same vectors. The protected two-hop/root-handoff
fixture now obtains destination geometry/incarnations/bitmaps through pool
reserve/commit, with table/start publication performed by this new helper.
A separate 32-round OS fixture uses normal TLAB allocations for five source
objects, including zero and page-sized payloads. It packs them into a Young or
Old two-page destination run, injects partial-copy cleanup, protects the entire
source run, and reads every destination via reference leases and access guards.
Payloads and allocator-owned logical records remain identical; the final object
starts in the run tail. This is physical placement in Old, not completed
promotion accounting. Production OS/scheduler ownership, dynamic native table
admission/refill, committed-candidate release and source/color retirement remain
open, along with the compiler/barrier integration needed for default enablement.

## Unpublished committed-run release

`gc_page_release_locked(heap, release, op)` and the matching PortableAsm emitter
release a committed run which has never had an object installed. The unique
64-byte PageRelease request carries head index, count, incarnation, stage,
heap owner and cleanup resources. BEGIN requires Ready, unchanged top, no owner
or metadata lease on any run page, valid matching run geometry and an optional
empty forwarding table. Closed, Allocating and already Releasing runs reject.
The collector must stop/discard all private copy jobs before calling BEGIN;
these metadata checks do not prove the absence of arbitrary private raw users.
A completed physical copy is still private if INSTALL has never occurred.

BEGIN transfers the bitmap and forwarding table into the request, clears their
page pointers and release-publishes Releasing (state 6) on the head. Tail slots
stay RunTail. The run remains committed and charged throughout cleanup and
cannot be reserved, bound as a TLAB, used as a relocation target or leased.
The caller unlocks, successfully decommits the OS range, frees the bitmap and
frees the table/backing allocation, clearing each request resource only after
success. The forwarding table's separate MetadataCharge remains charged until
its own free/budget RELEASE. This leaf owns only the heap and bitmap accounting.
Any failed cleanup therefore retains the unfinished transaction and resources
for a later retry, without pretending that the budget has become available.

FINISH revalidates every page and accepts only this heap owner's stage-1 request
with all cleanup outputs cleared. Only then does it subtract heap/bitmap bytes,
release-publish Free on every run slot, recompute pressure, and finish the
request. Incarnation and identity counters never rewind. Subsequent pool
reservation assigns fresh keys; a stale release request cannot free that new
run. The helper performs no managed-heap access or OS allocation/deallocation.
An installed candidate or published source requires the separate liveness,
forwarding/lease and color retirement protocol, which is still unfinished.

422 C/emitted full-state conformance cases and matching ARM64 vectors cover
resource retention, all head/count placements in a four-page fixture, both
generations, malformed pool/run geometry, a bad final tail, occupied forwarding
tables, accounting underflow and repeated/stale transaction states. In 32 real
OS rounds, normal pool/TLAB allocation supplies a source while target table
budget exhaustion, native allocation failure, partial-copy abandonment and
complete-but-uninstalled abandonment exercise cleanup and source retention.
After each resource failure the run remains charged. Successful cleanup enables
reuse of the same range with fresh incarnation/identity bounds and completely
zeroed recommitted bytes. Source data and its logical record survive. These
fixtures provide OS/scheduler ownership; production adapter/scheduler integration,
installed-candidate reclamation and published-page retirement remain open.

## Guarded physical reference-store commit

`gc_guarded_store(GuardedStore*)` and `gc_codegen/barrier.rs` implement the
physical commit portion of a store barrier. The 160-byte native request names
an existing canonical SourceAccessRequest, page and descriptor registry, a byte
offset, exact expected bits, desired bits, adopted phase and two prepared
BarrierEntries. The caller already protects receiver/expected/desired roots,
page/descriptor lifetime and distinct entry leases. Desired must have completed
canonicalization and required marking before its store-good color is supplied;
testing those bits alone does not perform that work. Native controls and both
SPSC ring backings are live/disjoint, with one context producer and one consumer
per buffer. Page geometry/generation remains stable while this scope is held.

The leaf validates the live Running scope, context epoch and phase, matching
source cell/page incarnation, nonzero guard count, committed bounds, Published
header and field extent. A one-candidate descriptor scan verifies that the byte
offset denotes a reference slot, including array fields. A source claimant may
have closed admission after this actor entered; that actor may finish its store
while the collector waits for its counted guard. Published/drained sources do
not permit this operation.

SATB records the expected previous nonnull referent when either generation is
in MarkStart/ConcurrentMark/MarkEndCheck. It conservatively logs without using
mark-color fast-path elision. Every Old receiver records its source slot in the
remembered buffer, including null-to-null stores; new-target-only logging is
not a substitute. Each required entry contains the immutable receiver value,
field offset, source incarnation, full phase snapshot and a separate metadata
lease token. Both entries/buffers are validated and checked for capacity before
any ring or heap mutation. FULL/EXHAUSTED/INVALID preserve all state. The caller
must EXIT its guard before flushing, polling or waiting for capacity.

If the slot no longer equals the exact expected bits, RETRY returns without
logging. Otherwise the helper appends/releases each required record before a
strong acq_rel CAS of expected to desired. A concurrent writer/healer may still
win that CAS; conservative records remain valid and the request's transferred
mask identifies obligations now owned by SATB(1)/remembered(2). Only entries
whose bits remain clear are caller-owned. No observed unrooted value or raw
address escapes on failure. The outer barrier releases its guard, re-resolves
from protected roots and reprepares a fresh attempt. This physical CAS is not
the public logical-reference CAS, whose expected value must be normalized so
color-only differences do not change language semantics.

448 C/emitted state cases and ARM64 vector replay cover all phase pairs, both
receiver generations, all 32 epoch-color combinations, null/same-value writes,
ring wraps, descriptor/guard rejection, full/overflow and failure preservation.
128 two-writer rounds check one winner, retry and retained ownership of any
conservative logs. A controlled 64-round interleaving enters writer/healer
scopes before source selection, writes through the helper, rejects a stale
healer CAS, and proves copying waits for both exits and then sees the new value.
Destination metadata is installed before forwarding publication.

The real OS-backed mark-graph fixture now deletes its sole child edge through
this C/emitted helper, using separately acquired guard and SATB-entry leases.
The SATB entry is published before deletion and later drained/offered to the
mark queue. Both Young and Old queue rounds preserve the child and retire the
entry lease exactly once. A queue-empty check alone remains insufficient while
that published buffer entry is pending. Rooted outer load/store resolution,
new-value marking, native lease/refill admission, remset consumption/flip,
logical CAS/bulk/scalar funnels, global termination and default compiler/runtime
integration remain open. These leaves do not enable Generational by default.

## Guarded reference acquisition into an output root

`gc_guarded_load_into(GuardedLoad*)` acquires an unresolved field reference and
publishes it into a prepared nonmoving output RootHandle while the receiver's
source guard remains held. The 80-byte request carries scope/page/registry,
field byte offset, the adopted phase and an output RootRequest with value zero.
The request stays unchanged on success and failure: the result exists in the
root cell, never in an unrooted native output field. The caller protects the
receiver, page/descriptor metadata and output root storage for this invocation.

Loads and stores share the C/PortableAsm scope/context/phase, page/header/extent
and descriptor reference-slot validation. Only after that validation may the
leaf form its internal slot address and acquire-load its bits. Null and every
valid reference color are accepted; malformed encodings reject. Acceptance does
not imply remapping/marking. The result is rooted first so the outer barrier
can resolve it, acquire its metadata and perform marking/healing with safe
polls between bounded steps. Source/color retirement obligations still apply.

The internal `gc_root_store_guarded` mode requires a nonzero active access
marker and performs only the existing finite root journal/store transaction.
The composed load has already validated its unique source scope. It never
suspends or clears the marker/guard, allocates, polls or waits. Ordinary public
root operations still require no active guard. Journal publication precedes the
output cell store, protecting both the overwritten output reference and the
new reference. All root storage ownership, generation/scope checks and journal
capacity checks remain in the shared root implementation.

On invalid input, journal exhaustion or FLUSH, roots/journal/heap/request remain
unchanged, and the caller still owns its guard. The loaded private value is
discarded inside the leaf. The caller must EXIT before flushing/polling and must
re-read the receiver/field afterward; a failed load cannot supply a cached value
to adopt after a safepoint. Successful handoff also keeps the guard until an
explicit EXIT; only the rooted result may cross a subsequent native transition.

826 C/emitted full-state cases cover null and unresolved colors, all epoch
combinations, root overwrite/journaling, ring wraps, malformed source/root/field
metadata, a full journal, exhausted counters and selection with a live entrant.
ARM64 replays these alongside the existing 448 guarded-store and 963 ordinary
root-operation cases after the shared validation/root refactor.

In 64 OS-backed pool/TLAB rounds, the receiver and children are real allocated
objects with registered descriptors, allocator-owned records and metadata
leases. The receiver, output and replacement child use separate actual roots.
32 rounds fill the journal, fail without exposing the loaded value, EXIT/drain,
cross a native transition, reacquire the receiver and perform a logical slot
update before rereading. Successful loads root the current child before source
selection can drain. The receiver is then copied/published and its old page
protected; after another native transition the output root still resolves to
the correct child's payload and identity through its own metadata lease/guard.
These are explicit fixture transitions, not the production scheduler. Public
rooted forwarding/marking/self-heal orchestration, retry/refill scheduling,
compiler-wide funnels, remset/promotion and retirement remain unfinished.

## Root publication revision and end-check audits (partial)

`MutatorContext.root_revision` counts successful root STORE/RELEASE operations,
including guarded load handoffs, forwarding rewrites, null stores and same-value
stores. Only the owner writes it. Journal and cell publication complete before
its release store; no poll or blocking action may intervene. Overflow returns
EXHAUSTED before any mutation. Reserve/read/retire and failed mutations preserve
the revision. The C and emitted implementations share the 280-byte context ABI, including allocation accounting.

A per-owner audit must capture `(registration identity, cycle snapshot,
root_revision)` BEFORE traversing its complete active/retiring journal inventory.
It may certify the traversal only after committing all copied references to
counted work. At MarkEndCheck, all producers must have acknowledged the handshake
or remain native-blocked, and the registry lock must exclude reentry/registration.
The controller compares the revision BEFORE ending that handshake. A mismatch
requires resuming and repeating concurrent work, not draining during the pause.
The identity and cycle bind the certificate to the correct owner and discovery
baseline; revision equality alone is never global termination.

No odd/even seqlock is needed for this stopped-owner comparison: a mutation in
progress during the audit must finish and increment before its owner can ACK.
A revision read while the owner is running is NOT a quiescence certificate.
Versions never reset or wrap during registration, so a completed post-sample
mutation cannot disappear. Retirement, root discovery, pending scanner outputs,
SATB/remset cursors, queued/spilled/in-flight jobs and other producers still need
independent accounting. The inventory audit below supplies this traversal/certificate protocol; the
production controller is not yet wired.

The actual registered two-worker graph fixture samples before its leased root
scan/journal handoff, then injects a late root store once the mark queue is empty.
Even rounds install the referent still held by SATB; odd rounds store null. With
all owners stopped, it rejects the stale revision without scanning. It resumes,
drains and offers both journal sides, consumes SATB, then rendezvouses again and
checks the updated revision and empty queue before worker shutdown. This covers
64 actual OS schedules with C/sanitizers and emitted native helpers, but is still
a single-root fixture, not a complete global termination implementation.

## Registered root journal jobs (partial)

`RootJournalRequest` composes a registered segment with a unique kind-4
`RootLease`. CLAIM and ACK use the root registry lock; COPY uses the claimed
native storage outside that lock. CLAIM captures at most 256 published entries,
assigns a never-repeated lease serial and increments both segment/registry lease
counts. A second consumer receives WAIT. COPY preserves the journal's consumed
cursor, fills the job's native output and advances only its private lease cursor.
The owner can append into free slots concurrently, but cannot reuse the claimed
prefix. COPY never reads registry counters or the producer-private `top`.

ACK certifies that both old and new references in each entry were offered for
its recorded phase, or transferred to counted pending work. It alone advances
the consumed cursor and releases the claim/lease. The claim serial rejects stale
requests after control-record reuse; ACK before COPY waits. A producer's later
published suffix stays pending. Buffer geometry, capacities and cursor ranges
are validated before mutation, including high unsigned cursors and exhaustion.
Output storage is private, live and disjoint, and one job owns the control record
through COPY/ACK. These are the same caller ownership obligations as root scan
and mark jobs; ACK does not inspect the mark queue to infer processing.

The registration's `journal_serial/journal_end` exclude competing consumers and
pin root storage through retirement. Existing cleanup waits for this lease;
only after ACK can a finished deletion job clean/reuse the segment. Raw
`gc_root_drain` remains for exclusively owned adapters/fixtures and must never
run concurrently with this registered consumer. The actual registered graph
fixture now uses CLAIM/COPY/ACK for every journal handoff.

The graph pauses a copied-but-unoffered journal job through a real worker
rendezvous. Even when root_revision matches and the mark queue is empty, the
lease and unconsumed entries remain visible. It resumes before offering and
ACK. A final claimed deletion journal also blocks segment cleanup until its
handoff. A separate OS producer/consumer fixture fills all three journal slots
while a copied job is stalled: overwrite returns FLUSH, a competing consumer
waits, then 65 publications traverse repeated ring wrap exactly once.

This supplies pending-journal ownership, not a global controller. Whole-inventory
root audits, SATB/remset pending-output accounting, generation-specific policy,
production scheduling and compiler/default migration remain incomplete.

## SATB and remembered-buffer jobs (partial)

`BarrierJob` extends deferred consumption to the two barrier buffers. Each
context embeds a distinct `BufferConsumer`; CLAIM and ACK use the same registry
lock as thread attach/detach, while COPY reads at most 256 BarrierEntries outside
that lock. The source owner may be running, stopped, native-blocked or detaching.
COPY never reads its private producer top or suspends its access guard.

CLAIM reserves a published prefix without advancing consumed. The control and
unconsumed prefix retain the context, buffer storage, references and metadata
lease obligations while the copied output is processed. A competing claimant
waits. The serial encodes a nonwrapping 63-bit claim counter plus the buffer
selector, so simultaneous SATB/remset claims cannot accept each other's stale
requests even when their local counters match. The request also carries the
registered context identity; reuse with another owner rejects. Cursor checks
reject repeated COPY, early ACK, stale/double ACK and oversized transfers.

ACK certifies phase-specific processing of every entry and transfer or release
of its metadata token, then advances consumed and clears the control. An appended
suffix remains pending. Raw `gc_barrier_buffer_drain` must not be mixed with an
active job; replacing buffer storage/geometry or copying/resetting its control
while claimed violates the ownership contract. After ACK the worker has dropped
its context borrow and must not keep accessing the source context without a new
registered borrow. The handshake rejects registration with outstanding controls
and defers detach while either control is active, independently of ring cursors.
With the generation-specific audit fields below, the shared context occupies
280 bytes, including the allocation counter appended in ABI version 2.

The registered graph fixture uses the new SATB consumer. After COPY it performs
another actual end-check rendezvous: the queue is empty, the root audit matches
and no root lease remains, but the SATB claim and unconsumed prefix still block
completion. Only after resuming, offering the old reference and releasing its
metadata holder does it ACK. The metadata fixture registers a real owner and
alternates SATB/remset: a copied token stays live while a detach attempt waits;
ACK permits detach, and stale token reuse remains rejected.

These are work-retention and lifetime mechanisms. The full generation-specific
SATB/remset visitors, remembered-set epochs/flip, whole-root inventory audits,
cycle controller, notifications and compiler/default wiring remain incomplete.

## Generation-specific root inventory audits (partial)

`RootAudit` composes the existing leased inventory walk with journal completion
and owner certificates. BEGIN allocates a never-repeated audit serial and pins
the first segment; it records the inventory's last scope and the selected mark
queue's generation/full cycle epoch. The queue stays live throughout the audit,
and its generation/epoch change under the same registry lock. RootWalk's mask
selects only that generation. Root cells are scanned outside the lock in bounded
slices; copied values must be offered/retained as counted mark work before the
caller certifies the walk's completed mask. NEXT refuses to leave a segment with
an unconsumed or claimed journal, then transfers its lease to the next segment.
All these operations run during concurrent marking, not inside MarkEndCheck.

Before the first scan of each owner's segments, the audit records that owner's
identity and root_revision under the audit serial. Reaching another segment of
the same owner does not refresh this sample; intervening writes remain detectable.
Certificates are separate for Young and Old. One current audit per generation
avoids conservatively superseding an earlier same-generation pass. Native storage
leases already exclude cleanup/reuse and retain the context through retirement.
Removed inventory nodes are covered by their completed deletion work; new
attachments change the monotonic last-scope boundary and invalidate the audit.

CHECK and OWNER perform finite metadata checks after the complete walk, while
all producers are held stopped by the same handshake. CHECK compares the current
inventory boundary, selected generation/full mark epoch and selected color epoch.
MarkStart/ConcurrentMark/MarkEndCheck transitions and sequence-only rendezvous
are allowed within that cycle. A later cycle with the same color parity fails
on the full epoch. OWNER requires a stopped/native/detaching owner with no raw
guard and no unfinished root retirement; nonempty owners also require the matching
identity/audit serial/revision certificate. An owner whose roots have all finished
retirement needs no old certificate. Neither check scans roots or journals.

RootAudit is 160 bytes; each context has three two-entry certificate arrays,
occupying the first 272 bytes of MutatorContext. ABI version 2 adds the
allocation counter after these arrays, for a total of 280 bytes. An empty inventory still receives its own
serial and completed audit. Overflow is rejected without wrapping; failure of
the underlying BEGIN walk restores the reserved serial and preserves all state.
The currently selected audit, queue, registry and native storage are unique,
trusted runtime control records, not externally writable requests.

The registered two-worker graph now performs real inventory scans and journal
handoffs, rejects a stale owner after a late root write and renews the audit
before another end check. A genuinely attached empty segment also invalidates
the boundary despite unchanged root_revision; it is discovered concurrently and
retired before the successful final check. Pending SATB work remains independently
visible even when the root audit is valid. These tests do not implement the global
thread-inventory/producer predicate, cycle controller or default runtime wiring.

## Current executable model scope

`cargo test -p klassic-native gc_model -- --nocapture` explores canonical access
and relocation with two mutators and two workers. Load/heal, write acquisition,
store/release, forwarding claim, copy and publication are separate steps. The
forwarding loser cannot publish a second destination. The access guard prevents
copying a writable source. The oracle tracks the last completed logical store
independently of the modeled physical slot. Debug ObjectId checks identity but
is never used to resolve addresses.

The access slice explores 222 distinct states and 8 terminal states for each
generation. Fault models find lost-store counterexamples for unconditional
healing and copying with active access.

`gc_model_generational.rs` adds a four-live-object/eight-field graph with two
mutators, two workers, two roots per mutator, private barrier buffers, root SATB,
current/previous remembered sets, queued and in-flight mark work, split
claim/copy/publication, promotion, metadata leases and source retirement. An
independent logical graph checks reachability, identity and completed stores.
The model resolves real encoded offsets through forwarding, never through the
debug identity. Retained sources block reuse while any registered holder still
contains their reference. Six Young cycles exercise repeated parity wrap;
subsequent allocation reuses a retired offset with a different identity.

The combined store/flip/Old-scanner/Young-promotion schedule explores 6,157 states,
19 terminal states and 5,266 states with overlapping active marking. Reaching its
250,000-state exploration limit is an error, not a passing truncation. Fixed
regressions additionally test caller-root overwrite before buffer flush, old
queue remapping, null clear/rewrite, private SATB and in-flight producer
termination, dead-owner metadata reclamation and premature address reuse.

A failing copy/mark test exposed another design condition: Old mark can start
after a Young copy is made but before its forwarding is published. If the copy
contains a snapshot of the source mark bits, the destination can remain white.
The model now gives both copies a shared nonmoving mark record. That metadata
address is independent of ObjectId and page incarnation, stays live through
forwarding, and is reclaimed with the logical object. The regression originally
failed with a reference to a retired page, then passed with the shared record.

Model bounds/limitations remain explicit. One object occupies one modeled page;
allocation/root publication and store bookkeeping are atomic steps in the graph
slice. The access slice separately splits load/heal and write guards. Convenience
drivers drain small frontiers/remap storage outside the scheduled critical
steps; they must not be used as runtime global-pause implementations. Move
fixtures may exercise an isolated move without a complete phase cycle; they do
not prove phase gating in a runtime coordinator. Allocation after mark termination,
concurrent root-segment retirement, weak/pin/exhaustion, generated memory ordering,
fair scheduling and performance still require later model/runtime work. This
is a bounded exploration of the listed scenarios, not a proof of all schedules
or completion of Task 1/G01-G15.


## Intrusive registered-thread inventory (partial)

HandshakeState is now 64 bytes and ThreadRegistration 56 bytes. The registry's
head and each entry's registry/previous/next fields form a newest-first list.
REGISTER inserts exactly once and DETACH unlinks only after the existing root,
TLAB and buffer lifetime gates pass. Fresh identities strictly order the links;
finite neighbor validation precedes every mutation. Single-entry removal clears
the head, and reattachment uses a fresh identity. Failures preserve the entire
fixture, including both neighboring registrations.

The same registry lock protects membership and every inventory traversal. No
list pointer is a lease to retain a context after unlocking. A completed active
rendezvous prevents producer resume/native return/attach/detach during the end
check. The graph integration fixture now follows the actual list, checks every
owner's root certificate and both buffer prefixes/consumer controls, and checks
queue/in-flight/overflow plus root retirement/leases before accepting completion.
This examines per-thread metadata only; root and journal scans remain outside
the pause. Production cycle/controller emission of this combined predicate and
its complete work-source accounting remain to be implemented.

KLHST002 stores full before/after fixtures and pointer relocation masks. It
replays neighbor mutations as well as the selected participant, including all
six three-owner removal orders, reattachment, and 28 invalid-link cases.

## Shared mark-end predicate (partial)

`gc_mark_end_check_locked` / `gc_codegen/mark_end.rs` replace the graph driver's
local termination logic. MarkEndRequest is 24 bytes: handshake pointer, completed
root-audit pointer and the exact active rendezvous generation. The function is
read-only, including READY; it uses a fixed private audit copy and invokes the
shared RootAudit CHECK/OWNER and MarkQueue CHECK implementations. No externally
visible state is changed on any return. Statuses are READY/INVALID/WAIT/STALE.

The common thread/root/queue lock must remain held from this check through the
caller's phase transition, before handshake END. Inactive or incomplete rounds
cannot return READY; a mismatched requested generation is STALE. Per-node
identity, registry, reciprocal link, context identity and acknowledgement checks
precede access to frozen owner metadata. The traversal counts every member and
native owner against the coordinator, rejecting shortened lists, cycles and
inconsistent counts. Safepoint/Detaching owners must have adopted the selected
snapshot; native owners may retain their old snapshot until controlled reentry.
All owners require a valid root certificate (or no remaining roots), no raw
guard, no pending retirement, and empty/unclaimed SATB and remembered prefixes.
Root registry leases/walkers/retirement and queued/in-flight/overflow marking
remain independently counted work.

This is O(registered threads) fixed metadata work, with no heap/root/journal
scan, copying of payload, allocation or wait. All root owners must belong to
this registry. A producer may acknowledge only after publishing private work or
retaining it in those counted sources. Any newly introduced external work source
must join the accounting before its controller can use READY. The predicate
does not itself emit notifications, end the rendezvous, select a new phase or
complete Young/Old cycle orchestration. The default collector is not migrated.

The same helper is now called by all 64 real two-worker graph rounds, including
stale certificates and pending copied SATB rejection. 1,073 full-state conformance
cases cover both generations, three color combinations, mark phases, all native
owner combinations, zero/multiple roots, malformed inventory, stale rendezvous,
full-width identities, queue work and each owner's independent pending work.
KLMEND01 exports before/after fixtures; every status preserves all bytes.


## Mark-end decision and phase adoption (partial)

`gc_mark_phase_step_locked` / `gc_codegen/mark_phase.rs` implement the marking
boundary around the shared end predicate. MarkPhaseControl is 64 bytes, owned by
one coordinator, and holds handshake/audit pointers, selected generation, control
state, exact rendezvous, phase snapshot, full queue epoch and outcome.

BEGIN requires ConcurrentMark, publishes MarkEndCheck and starts a rendezvous
under the common registry lock. It reserves room for two snapshot sequence and
handshake generation increments before changing anything. DECIDE waits for all
managed acknowledgements, checks the captured rendezvous/snapshot/cycle epoch,
and calls the shared end predicate. READY chooses Select; pending work or stale
root certificates choose ConcurrentMark. Invalid input leaves all state intact.
It ends the old handshake and starts the next one without releasing the lock,
publishing the chosen phase in the same transaction. No owner resumes between
those operations. Each managed owner must ACK the newer generation via the
worker helper; RELEASE returns RETRY or SELECT only after that adoption round
completes. Native owners stay excluded and adopt on controlled reentry.

The snapshot changes only the selected generation's phase nibble and increments
the sequence. Other generation phases and every color/epoch bit are preserved.
The control has idle/checking/adopting/done states; a done ConcurrentMark retry
can reuse it. The caller publishes notifications after successful steps and
waits or assists outside the lock. The helper performs no root/heap traversal,
draining or allocation. Counter exhaustion is explicit, including the last
reservable pair completing without wrap.

Real graph integration now runs repeated retry boundaries and the final Select
transition with actual registered OS workers. It exposed the native-entry race
between poll and the coordinator's new snapshot: ENTER now returns RETRY while
that newer rendezvous is active, so the owner remains out of native code and
polls to ACK. A mismatch without an active rendezvous remains invalid. Journal
handoff in the graph respects its captured selected-generation marking phase;
post-mark cleanup acknowledgements do not offer new marking work.

5,068 full-state conformance cases cover both selected generations, all other
phase values, all color combinations and native-owner sets, pending buffers,
stale roots, repeat retries, incomplete adoption, invalid/stale controls and
counter exhaustion. An independent effect oracle verifies only the expected
registry/control changes; failures preserve every byte. KLMPHS01 exports these
cases for ARM64 replay. This implements a marking boundary, not whole-cycle
scheduling, CycleTicket request/await semantics, relocation/retirement phase
transitions or the default generational runtime.

## Request-time cycle tickets (partial)

CycleLedger (48 bytes) and CycleTicket (24 bytes) share C/Rust layouts and a
C/PortableAsm `gc_cycle_step_locked` implementation. The request lock protects
requested/started/completed counters for independent Young and Major lanes.
There is at most one active and one pending cycle per lane. REQUEST targets
started+1, so callers before START coalesce and callers after START necessarily
wait for a later cycle. START consumes the pending request and returns its job
ticket; FINISH accepts exactly that active identity after the entire cycle is
over. POLL is read-only and succeeds only when the ticket's cycle is complete.
Young completion never advances the Major lane. Starting/finishing a major job
must encompass the scheduler's entire major collection, not just Young work.

The ledger remains nonmoving and alive until every ticket is discarded; it must
not be reset/reused underneath tickets. Copies of a ticket are valid wait inputs,
not exclusive ownership claims. Only the owning scheduler may START/FINISH.
Validation rejects foreign ledgers, future tickets, duplicate completion and
inconsistent counters without mutation. REQUEST rejects identity exhaustion;
coalescing the last pending identity and finishing it remain valid without wrap.

The actual C collector now uses this ledger for its background worker and the
`gc_request_cycle(kind)` / `gc_await_cycle(ctx,ticket)` internal helpers. Its
legacy full collection uses the Major lane only; a Young request is explicitly
unsupported until the generational scheduler is activated. FINISH follows the
whole real `gc_run_concurrent_cycle`, including relocation returning to Idle.
Explicit collect and allocation fallback request tickets, then enter a native
blocking boundary and await the completion predicate outside handshake locks. Pressure hints retain their existing
policy of requesting only when idle. Requests arriving during an active cycle
already leave the next obligation queued, without a post-completion re-request
or reliance on a transient wakeup.

A deterministic real-collector fixture pauses after ledger START, requests a
later/coalesced ticket, allows the first collection to finish and pauses at the
second START. It proves the first ticket is complete while both later tickets
still wait, then releases and awaits the second full collection (16 pairs).
480 C/candidate full-state cases separately cover both lanes, coalescing,
late requests, wrong completion, polling, malformed controls and overflow.
KLCYCL01 exports the exact before/after states for ARM64 replay. The direct
helper does not yet have a production scheduling/wait loop, and complete
Young/Major generational cycle orchestration remains unfinished.


## Blocking cycle waits and managed reentry (C integration, partial)

The C `gc_await_cycle` implementation now checks the ticket once, returns
immediately if complete, otherwise enters native blocking before waiting on a
completion condition variable. This publishes the existing legacy roots/TLAB
boundary and excludes the waiter from the collector's expected acknowledgements.
It never holds the request mutex while entering or leaving native state. After
entry it reacquires the request mutex and rechecks the ticket before every
condition wait; completion before wait and spurious wakes therefore cannot lose
progress or satisfy an unfinished ticket. FINISH broadcasts to all completion
waiters while holding that same mutex.

Completion uses a separate condition variable from the collector's request
notification. A request signal cannot accidentally wake a completion waiter
while leaving the collector asleep. After observing completion, controlled
`gc_leave_native` still waits for any active/new handshake before returning in
Running state. A completed-ticket fast return needs no new native transition.
The wait owns no raw managed pointer or access guard across that boundary.

48 deterministic real-collector rounds retain a rooted value while forcing
completion between native entry and wait, an extra completion notification while
the cycle is still held, or a handshake between completion and managed reentry.
Another 24 cohorts of 1/2/8 waiting owners verify broadcast completion and rooted
value preservation for all waiters. A further 24 cohorts put 1/2/8 owners on each
of two consecutive tickets. Holding the successor at START proves that the first
cohort returns while the second rechecks and sleeps until its own completion.
Per-owner wait counters establish that every owner has reached the wait; one
owner's spurious wake cannot stand in for another. All suites require completion and
join; no timing threshold counts as success. The test-only scheduling seams are
compiled out of the production path. Direct production wait/notification wiring,
full generational scheduling and platform hardware/latency gates remain open.

## Prepared mark-start adoption (partial integration)

The shared C/PortableAsm mark-phase coordinator now accepts START and ACTIVATE
for a prepared MarkStart snapshot. START binds the queue epoch and opens a
rendezvous without traversing roots or heap. After all managed producers publish
and acknowledge their boundary, ACTIVATE ends that rendezvous and opens the
ConcurrentMark adoption rendezvous under the same registry lock. RELEASE returns
MARKING only after this second adoption; native owners adopt on reentry. The
other generation and all color bits are preserved. Two handshake increments and
one snapshot increment are reserved at START, with exhaustion rejected.

The caller owns epoch preparation, safe mark-color reuse, Young remembered-set
flip and publication of MarkStart. Publication and START share the registry
critical section; a failed START must not expose an unprotected prepared
snapshot when unlocking. This helper does not establish those preparation or
retirement obligations, and does not finish a CycleTicket. Its new states are
4 (adopting MarkStart) and 5 (adopting ConcurrentMark); released state 3 can
immediately begin the existing mark-end protocol.

The actual graph integration registers its two OS workers before START, has all
three owners acknowledge both phases, then seeds roots and enables bounded graph
work. It uses the shared coordinator instead of directly jumping to ConcurrentMark.
The 64 graphs cover both generations. Full-state cases additionally cover all
32 color combinations, both generations, all managed/native combinations of two
owners, stale controls, invalid transitions and the final nonwrapping counters.
Production epoch preparation, whole cycles and default migration remain open.

## Precise remembered slot banks (partial integration)

`RememberedSet` owns two nonmoving bitmaps, one bit per aligned eight-byte slot
within a fixed page/run range and incarnation. C and PortableAsm implement the
same registry-locked RECORD, FLIP, TAKE, ACK and CHECK operations. RECORD receives
a canonical slot offset, incarnation and the buffer entry's original full phase;
the caller must first validate the Old owner/descriptor and resolve forwarding
with retained metadata and reference protection. It does not record a target
object in place of the source slot.

FLIP requires a new Young MarkStart with changed remembered parity and a newer
publication sequence. It exchanges the bank indices/counts in constant time;
there is no bitmap clearing/traversal in this transition. The reusable bank must
have no bits or active claim. The scheduler must also retire all older buffer
and worker obligations before the next flip: local bitmap emptiness cannot
certify that global fact. A record predating both bank intervals returns STALE
without consuming it, even if parity has wrapped back to its original value.

Current-interval records set current only. A late previous-interval record sets
both banks and increments pending only for a newly inserted previous bit. This
remains visible even when the scan cursor has already passed the slot. TAKE
examines at most the requested budget (1..256 words), wraps its cursor, removes
one previous bit and creates a unique nonwrapping claim. CHECK stays WAIT while
either bits or the claim remain. Only ACK after guarded/rooted slot processing
retires that claim; `keep=1` preserves a still-Young edge in current. `keep=0`
never clears a concurrent current record. A conservative late/current record
may require another scan before disappearing. One consumer claim per page
allows parallel processing of different pages without a per-slot job table.

Full-state tests cover both bank choices, all 32 phase colors, boundary bits,
late insertion after TAKE, duplicate records/ACKs, repeated flips, retention,
stale generations, bounded scan progress, corrupt maps and serial exhaustion.
The relocation fixture now claims/copies a real remembered buffer entry before
moving its owner. After forwarding publication it resolves the captured owner
through the shared forwarding helper, records the destination slot after FLIP,
then scans that slot under a destination access guard across repeated flips.
Fixture-native ownership retains source/destination metadata; this does not
prove production dead-owner recognition or lease retirement.

Remaining integration includes enumerating/accounting all live page inventories
in global Young termination, rooted referent handoff to Young marking, promotion
enumeration, source-to-destination inventory transfer beyond buffered entries,
page metadata accounting and reclamation, and activation in the default runtime.
No complete Young-only collection or G07 completion is claimed by these leaves.

## Remembered page inventory in Young termination (partial integration)

Every participating page set now has a nonmoving `RememberedRegistration`
linked into a `RememberedInventory` under the shared registry/phase lock. Its
wrapper performs ATTACH/DETACH and all local bitmap operations, maintaining
counts of registered sets that are unflipped, contain pending bits, or have an
active claim. Registered sets must not bypass this wrapper. Counts are per set,
so they cannot overflow by adding all slot counts across a large heap. Fresh
identities and reciprocal links support constant-time attach/detach validation.
Detach waits for a flipped, empty set with no claim and no active inventory
walks; the caller must also prove
that no future buffered/native borrower can use that page's inventory.

BEGIN records a prepared Young MarkStart and the number of sets to flip in
constant time. Each page is flipped separately with bounded work, outside the
global stopped interval. A newly registered empty set starts at the new epoch;
it does not inherit the older sets' flip debt. New-epoch RECORD and TAKE on an
unflipped set return WAIT without consuming work. Older records can still be
recorded into that page's old current bank before its flip. The inventory also
tracks the latest accepted record phase; BEGIN rejects a snapshot older than
that high-water mark before changing any page's state.

`MarkEndRequest` now borrows this complete inventory, and `MarkPhaseControl`
passes it to the shared end predicate. After the existing root/queue checks,
Young termination checks inventory phase freshness/parity and requires all
three per-set aggregate counts and the active walker count to be zero. This
adds no page/bitmap traversal to the
pause. Old termination does not wait on these Young-only inventory counts.
The existing producer/buffer/root/queue predicates remain required: counters
alone do not account for an entry still held in a producer's buffer.

The moved-owner buffer fixture now registers its page, performs global BEGIN
and per-page FLIP, transfers the buffered slot into aggregate pending work, and
retains aggregate claim accounting through guarded slot processing and ACK.
Separate two-page fixtures connect the real wrapper to completed owner
handshakes/root certificates/empty mark queues and require Young WAIT through
unflipped pages, pending slots and the final held claim. Phase cases require a
Young retry rather than Select for each kind of remaining work. C and emitted
paths use the same interfaces; native fixture ownership still supplies page and
metadata lifetimes. Production registration of every Old page, rooted Young
referent handoff, promotion, retirement and default activation remain open.

## Remembered claim to rooted Young mark work (partial integration)

`RememberedLoad` borrows the unique active page claim and a canonical Old
receiver guard. The shared C/PortableAsm leaf validates registration, claim
serial/incarnation/phase, original owner plus field offset, Young marking phase
and remembered parity. It then materializes the slot into a reserved output
root through the guarded load contract. The claim and source access guard remain
held; no raw field address or private referent is returned. The caller proves
that the original owner and canonical receiver represent the same object, using
retained object-start/descriptor metadata and rooted forwarding resolution.

This leaf runs outside the registry lock. It reads claim-owned fields whose
lifetime and stability are protected until ACK; it does not read inventory
counters or mutable registration list links. No other actor may concurrently
ACK or reuse that claim. Registered operations cannot flip or detach a set with
an active claim. Registration, set, job and source metadata remain borrowed
through the later handoff.

A full root journal returns FLUSH without consuming the claim or field value.
The caller exits the source guard, flushes outside the access interval, follows
the owner root again and rereads the canonical slot. After success it exits the
guard, follows the output root across any Young move, acquires destination
metadata, prepares a guarded mark offer, and hands it to the Young queue. Only
then may it ACK the remembered claim. Thus an empty mark queue before OFFER
still has an active remembered claim; after ACK the queue owns the work and
metadata lease. Null/Old targets require their own validated no-Young-work
classification before ACK; the routing extension below covers those targets.

The OS-backed fixture uses real pool/TLAB allocation, forwarding tables,
metadata leases, roots and bounded copy/mark scan operations. It first moves a
parent to an Old page and explicitly registers its sole slot. That slot is the
only heap edge to an initially unrooted Young child. Half the rounds exhaust the
root journal and move the Old parent before retry; every round moves the rooted
Young child while its forwarding is busy, protects old physical mappings,
then marks and scans the canonical child. The test checks both sides of each
claim/queue handoff, shared identity, payload and balanced metadata leases.

These are controlled adversarial barrier schedules with a fixture-owned Running
context and native storage, not a complete legal generational phase scheduler.
The current remembered bit and original metadata remain owned until whole
fixture teardown; no nonempty DETACH or production dead-owner/page retirement
is inferred. Production consumer dispatch, promotion enumeration, full moved
inventory transfer, production routing adoption, all-Old-page registration and
default generational activation remain required.

## Canonical generation routing for held mark work (partial integration)

`GuardedMarkRoute` selects a marking generation and borrows the same protected
input as guarded mark preparation. Null needs no page, source guard or metadata
lease and returns SKIP without touching the heap. A nonnull value must already
be followed into a canonical root and have its destination metadata lease and
source-access guard. Validation of the reference, phase adoption, guarded
header and lease precedes the generation decision; stale source metadata is
not a valid basis for skipping a promoted object.

Young routing skips canonical Old objects. Old routing prepares both Young and
Old objects for the Old queue, allowing traversal through Young intermediaries
to their Old children. The selected generation must be in MarkStart,
ConcurrentMark or MarkEndCheck; all other phases reject without mutation.
READY moves the input lease into a pending mark offer. SKIP leaves the request
and any input lease unchanged, so the caller exits the guard and releases that
lease before completing its producer obligation. A mark offer still requires
the correct generation/epoch queue and producer accounting through completion.

The C and PortableAsm implementations share the existing guarded-preparation
validation and transfer logic; the unfiltered preparation API keeps its
previous contract. The emitter's reference validation uses the KIND temporary,
so the selected generation is held separately in OP across validation. Full
state tests distinguish both kinds, null/Young/Old targets, all phase colors,
selected/other generation phases and exact lease ownership on skip/rejection.

Remembered integration now routes all three target classes. Its 64 Young
handoff rounds are joined by 64 null and 64 Young-to-Old target moves. Nonnull
classification occurs only after rooted forwarding and guard acquisition on
the destination. SKIP causes no mark stamp or queue change and retires the
claim only after lease cleanup. Interposed current-bank records survive a
`keep=0` ACK. These are controlled fixture schedules, not a promotion policy or
proof of Old-mark/Young-cycle overlap. Production consumer dispatch, queue epoch
selection, promotion barriers, inventory transfer/retirement and activation in
normal generated programs remain required.

## Claimed moved-slot inventory transfer (partial integration)

`RememberedTransfer` moves one unique source claim to pending work on a distinct
canonical Old-page registration under the shared registry/phase lock. The
caller has already resolved and validated the same logical owner and field,
retaining source and destination metadata. No heap access or source guard is
held during this transaction. The inventory's read-only VALIDATE operation
checks registered membership, local map geometry and aggregate consistency
without changing state; both pages must have adopted the current epoch before
transfer can proceed. Deferred destination adoption returns WAIT intact.

The destination previous bank gains a pending slot. A source current bit is
also carried into destination current; a late reinserted source previous bit
coalesces with the transferred pending obligation. Existing destination bits,
a destination worker's active claim and all other source slots survive. Only
then are the source slot's bits and active claim removed, with aggregate counts
updated in the same locked transaction. The old claim receipt no longer
permits ACK or replay. The consumer must TAKE a new destination claim and
perform rooted slot processing before completion. No empty-work observation
can occur between the source claim and destination pending work.

New buffer resolution targets the canonical destination. A buffer consumer
that staged the old location before publication can still reinsert source
work after transfer while holding its metadata protection. This remains counted
source work and must be claimed/remapped again. The conformance suite exercises
that ordering and coalescing with an already-pending destination. An emptied
source set alone never proves that the source can be detached or its metadata
reclaimed; all staged buffers, roots/jobs and address/color retirement duties
remain required.

Full-state C/emitted cases cover all 32 epoch colors, both bitmap word
boundaries, current and late bits, destination claims, unrelated neighboring
bits, stale receipts, invalid membership/counters and deferred flip. An
independent oracle recomputes map population counts and all aggregate work.
The OS-backed remembered pipeline now transfers the inventory when its Old
receiver moves during journal-full retry, acquires destination work and reads
under a destination guard. Null, Young and promoted Old targets retain their
respective routing/cleanup behavior. A later prepared fixture epoch flips both
sets and re-reads the retained canonical slot; its still-old Young reference
is followed through protected source metadata and marked again.

These are controlled fixture epochs, not complete Young collections or a
source retirement proof. Production slot-to-owner lookup, dispatch of all page
inventories, staged-buffer retirement, promotion enumeration, normal compiler
activation and the full cycle/performance gates remain unfinished.

## Remembered slot to source owner candidate (partial integration)

`RememberedOwner` authenticates the unique retained claim, matching Old run
metadata and page incarnation, then finds the predecessor object start in the
append-only forwarding table. It returns the original owner's byte offset, the
slot's relative byte offset and a borrowed native forwarding cell. It does not
return a GcRef or managed address, inspect heap bytes, ACK the claim, or transfer
a metadata lease. Registration, set, claim and run metadata remain retained;
mutable inventory counters and registration links are not read outside their
lock. The source physical mapping can already be inaccessible.

The leaf first acquire-loads the run's allocated prefix, then captures the
forwarding table's acquire-published used prefix. New table entries may be
published before their object-start bit and allocated-top update; those entries
cannot authorize a slot beyond the captured allocation boundary. Upper-bound
binary search uses at most 32 probes with the ABI's UINT32_MAX table capacity.
The selected key must match the incarnation and start bitmap. A neighboring
key excludes its known header range. The helper reads only that key, neighbor
and one bitmap word after the bounded search, independent of payload size.

This is still a candidate: padding, zero-length payloads or scalar fields
cannot be classified from forwarding/start metadata alone. The consumer must
establish the owner's liveness, materialize/follow that same logical owner in
protected storage, and validate the field under canonical descriptor access.
It must not manufacture a mark-good reference, treat a metadata lease as strong
reachability, or permanently root dead Old owners. INVALID/MISSING preserve the
claim and output; neither authorizes dropping remembered work.

Full-state C/emitted tests use an independent linear predecessor oracle and
cover all prefix lengths through 33 objects, object/header boundaries,
unpublished suffixes, malformed claims/pages/tables/start bits and forwarding
states. A two-thread fixture forces table publication before starts/top and
rejects the new slot until allocation publication, while retaining the prior
object. Its managed mapping stays inaccessible throughout. The real-page
remembered pipeline now has a scalar/reference/scalar parent and derives the
reference field's nonzero relative offset from the new helper. It repeats the
lookup on protected original sources, rehomes the slot at that same offset,
and uses canonical guarded loads and subsequent Young marking/revisits.

Production candidate-to-live-owner binding, all-page worker dispatch,
dead-owner filtering, metadata retirement, promotion enumeration and default
runtime integration remain required. Controlled fixture epochs do not prove
whole Young cycles or the G01-G15 completion and performance gates.

The registered two-worker mark-graph fixture now exercises the remembered
consumer within the same registry, phase controller, root inventory and mark
queue as ordinary graph/SATB work. A fixture-held Old owner points to a Young
child absent from all other producer inputs. Pending and claimed remembered
work independently force real mark-end retries. The claimant polls while it
retains its job, then loads through a registered output root and transfers to
mark work before ACK. Its root journal is drained and its temporary registered
segment fully retired before native stack storage expires. A fresh inventory
audit is required after this segment activity. This establishes the composition
of existing leaves with actual registered workers, not production liveness
binding or complete generation scheduling.

The same registered-worker consumer now handles journal backpressure instead
of assuming two free entries. A failed guarded load exits its source scope and
releases the metadata lease before draining one journal entry, polling, and
reacquiring/reloading the owner. A failed root release keeps the output root
active while the same bounded drain makes room. COPY references stay protected
by their journal claim until both mark offers and ACK finish; no copied
unrooted reference crosses the subsequent poll.

A second real rendezvous is forced after the slot value reaches the worker
root but before its mark offer. The remembered claim, root and published journal
remain visible while the worker adopts a newer phase. The next offer reads the
adopted snapshot, rather than reusing the guarded load's snapshot. Thirty-two
rounds exercise this resumption; sixteen also force load and release FLUSH with
a one-entry journal. Receiver motion and production dispatch remain separate
open obligations; the controlled graph is stationary during this test.

## Bounded remembered-page dispatch and membership lifetime

`RememberedWalk` is a 40-byte, unique-owner native cursor. The shared C and
PortableAsm BEGIN/NEXT/CANCEL operations run under the same registry lock as
registration, bitmap work and mark end. A call validates at most the current
and next registrations and returns one page; it does not traverse a bitmap or
managed heap. The 80-byte `RememberedInventory` now also counts active walkers.
An active cursor prevents all DETACH operations and next-epoch BEGIN. This
keeps native links and set storage live across polls without retaining a raw
heap address or asserting that the page's objects are strongly reachable.

ATTACH can prepend newer registrations. A cursor records the initial identity
limit and follows strictly older links, so a pass remains finite under ongoing
allocation. Newly attached pages and late bits require further passes whenever
aggregate work remains. DONE is not a mark-completion certificate. NEXT at the
tail and CANCEL clear the cursor and decrement the borrow count. A separately
acquired slot claim continues to protect its registration after the cursor is
released; the claim must still be handed off and ACKed normally. Requests must
not be copied, modified or replayed.

The real-worker graph integration uses this traversal to flip an initially
unflipped empty Old run, poll while retaining membership, skip it and discover
a different run's pending slot. A completed rendezvous tries to detach the
otherwise-empty first page and receives WAIT solely because walkers retain it.
The worker cancels its cursor after TAKE, retaining the independent slot claim
through the existing rooted-load/mark/journal pipeline. All cursors finish
before final Young mark end; the empty registration can then be detached.

731 full-state C/emitted cases cover registration order, phase parities, empty
inventories, multiple cursors, concurrent-style prepend/detach/epoch attempts,
flip/claim ownership, cancellation and malformed controls. This is bounded
registry dispatch, not production candidate-to-live-owner binding, dead-page
retirement, complete cycles or default generational activation.

The registered graph consumer no longer disables remembered dispatch after its
first successful ACK. Its completion counter records processed jobs only; work
availability comes from inventory pending/unflipped counts, and mark end still
requires all producer, claim, walker and mark-queue predicates. A late
previous-epoch RECORD is injected after TAKE while the first worker retains its
claim. It reinstalls the previous-bank bit and preserves the current-bank bit.
The original ACK cannot consume that separate pending work. A second pass must
claim, root, mark/duplicate, drain journals and ACK it before end can succeed.

The 32 Young real-worker rounds now complete exactly two remembered jobs each.
The child is scanned once, duplicate mark offers release their metadata leases,
root segments are retired for both jobs, and the carried current bit remains
one bit. One-entry-journal rounds force load/release backpressure for both jobs.
This tests repeated dispatch of a captured old-epoch record; whole production
cycles, owner liveness and default runtime migration remain unfinished.

## Binding a remembered claim to page metadata

`RememberedLease` is a 40-byte request containing the retained registration,
claim and an initially empty metadata-lease output. The matching C/PortableAsm
leaf validates inventory membership, the unique active serial/offset/epoch,
and captured incarnation under the common registry/heap lock. It derives the
run head from the set's page-aligned range and validates that range against
the actual heap table, Old generation and incarnation before acquiring a
metadata token. It does not require a preexisting owner GcRef. Success retains
one metadata lease without consuming the slot claim; all errors preserve
inputs, pool state, counters and bitmaps.

The operation reads only native metadata. It deliberately does not inspect
object starts or payloads and may bind a header/padding/scalar candidate.
`RememberedOwner` still finds the candidate object; canonical descriptor and
strong-liveness checks remain separate. In particular, neither a metadata
lease nor a successful binding authorizes inventing a mark-good reference or
keeping a dead Old owner alive. Physical source pages may be protected while
their native page/forwarding metadata remains retained.

The registered-worker consumer now obtains this lease directly from its claim
before owner lookup. Its explicit fixture-held reference remains the liveness
source, not an input to page binding. The moving remembered pipeline also
acquires from 96 already-protected original source mappings, performs lookup,
releases the token, and rejects the old receipt after inventory transfer.
842 C/emitted full-state cases cover multi-page run ranges, both live page
states, unique outputs, stale claims/incarnations, malformed geometry, pool
full and token/counter exhaustion. Production owner-liveness binding, dispatch
error recovery and published-page retirement remain open.

The registered remembered-worker integration now handles `GC_ML_FULL` from
claim-derived acquisition. It retains the claim and the registered empty
output root, confirms that no partial token or access guard exists, enters the
shared native-blocked state, and retries acquisition after leaving that state
and adopting the newer phase. The controlled fixture gate is not a production
OS wait/wake or OOM policy.

In all 32 Young rounds the coordinator acquires every one of the pool's 32
slots through the actual metadata API while the worker holds its first claim.
The worker encounters FULL; a real phase rendezvous proceeds with that owner
native-blocked while its claim and the late pending record still prevent mark
completion. The coordinator releases all 32 independently owned tokens, then
permits reentry. The consumer completes both remembered jobs, including the
existing one-entry-journal backpressure cases, with no metadata/guard/root
leaks. Capacity/accounting is not overwritten to simulate exhaustion. General
reservation, production stall wakeup and terminal OOM handling remain open.

The remembered metadata-pressure worker fixture now blocks on an OS condition
variable (pthread condition on POSIX, condition variable with critical section
on Windows). Its resume predicate is protected by the same registry mutex as
token release and notification, so release before wait cannot lose a wakeup;
spurious wakeups recheck the predicate. The worker enters BlockedNative before
waiting, retains its claim and empty registered output root, and adopts the
current phase after leaving native state before retrying acquisition. This is
still a controlled integration fixture, not the production allocator's pressure
scheduler or OOM policy. Windows execution remains a separate required gate.

Validation for OS metadata-pressure wait/wake: emitted x86 conformance test
`gc_atomic_emitted_code_two_threads` passed (1 test, 0 failures); `llvm_gc`
passed (1 test invoking all six C suites under ASan+UBSan and TSan, including
32 remembered metadata-pressure rounds per build). `cargo fmt --check`,
`git diff --check`, and access inventory check (9200 sites / 94 files) passed.
These are Linux host results, not Windows/macOS execution or latency evidence.

### Generation routing at the real-worker graph handoff

The shared graph dispatcher now calls `GuardedMarkRoute` before offering work.
Its selected queue generation comes from the active cycle, and the phase comes
from the owner's adopted snapshot. READY moves the metadata lease into the
pending offer. SKIP exits the source guard first, then releases the retained
input lease under the heap lock; it creates no queue job or mark stamp.

In each of 32 Young rounds, a registered root initially holds the Old owner of
the remembered-only Young edge. A leased root scan, its publication journal,
and the old half of a subsequent root overwrite all route to SKIP. The queue
stays empty and neither owner nor child receives a Young stamp until ordinary
roots and remembered work are dispatched. The retained remset claim still
blocks MarkEnd, and only its rooted child handoff discovers the Young object.
The same graph also runs 32 Old rounds through the router, which must accept
Young intermediaries. This composes existing C and emitted routing with actual
registered OS workers; it is not production default activation or concurrent
owner relocation. That source-remapping/liveness integration remains open.

### Blocked remembered claims across an Old-owner move

The real-worker graph fixture now publishes one additional Old-to-Old forwarding
edge while its remembered consumer is blocked on the exhausted metadata pool.
The unique claim, native source metadata and fixture-held strong owner survive;
no source access guard survives the blocked interval. Shared relocation leaves
select/close the source, copy in bounded slices, install the target, and publish
forwarding. The source OS mapping is then decommitted before the worker wakes.

Journal offers retain their original strong producer during a fixture-only
no-poll chain resolution: destination metadata acquisition precedes source-token
release, and only the canonical receiver gets a guard. Mark preparation uses
that canonical reference and page. The remembered consumer instead publishes
the owner into a second registered root and uses RootAccess for each hop.
After its first hop, a real phase rendezvous precedes a second Old-owner move.
The intermediate mapping is decommitted before the consumer resumes, follows
the next rooted hop and enters its final guard. Journal backpressure is serviced
without a guard, and both owner/output roots are released and retired before
the remembered claim is consumed.

The coordinator also exhausts all metadata tokens after the first rooted hop.
RootAccess returns METADATA_FULL without a guard or partial token; the consumer
enters BlockedNative and waits on the same mutex/condition protocol used by
claim acquisition. Its nonnull owner root and remembered claim remain published.
A second phase rendezvous rejects mark end before the coordinator moves and
decommits the intermediate object, releases its tokens and broadcasts. The
consumer adopts the new phase, verifies the retained root and retries from it.
Source native controls and strong liveness remain fixture-retained; this does not prove production retirement or reuse.

Two actual publication/deletion journal entries carrying the original Old
reference are consumed after source decommit. Both resolve to the new Old
header and skip Young marking. The remembered consumer subsequently resolves
the original owner candidate, validates/reads the identical field offset, and
roots the Young child. Journal-full retries reread the canonical field while
keeping the claim; the late previous-epoch record forces a second pass.

All 32 Young rounds check balanced canonical leases and four rooted advances
(two hops in each source pass). Journal offers perform 13 or 14 no-poll hops,
depending on whether the first publication was consumed before the second move.
Destination claims start at canonical metadata. Native source metadata remains
retained through fixture teardown even after its empty remembered membership
can detach. Production retirement, arbitrary concurrent repeated moves and
default dispatch remain open. No source-address reuse or general concurrent
mutation proof is claimed by this schedule.


### Remembered transfer at the real-worker completion boundary

The worker now finishes its source claim by calling the shared remembered
transfer operation after its canonical guarded load, rooted Young handoff and
root-journal retirement. It holds the same inventory lock as registration,
recording and TAKE/ACK. The destination gains previous pending work and the
source current bit before the source claim disappears. The late previous-epoch
source record is coalesced into that destination obligation; source counts and
active claim become empty, but the inventory still reports WAIT. The consumed
source receipt cannot be ACKed again.

A fresh worker claim then traverses destination metadata, reads the canonical
field, hands the child off and ACKs keep=1. The retained current bit belongs only
to the destination. The first pass pair requires one claim on each registration,
two completed scans, balanced leases and one transfer; delayed-buffer replay
below repeats that pair before final completion. Once all workers
are joined and captured buffers drained, the empty source registration detaches;
the destination registration rejects DETACH while its current edge remains.
Native metadata and the live destination survive until controlled fixture
teardown. This does not establish production source/color retirement or address
reuse, and arbitrary chain/promotion scheduling remains to be integrated.


### Source-buffer replay after transfer completion

The graph captures an old-epoch source-slot description during fixture setup.
Before relocation, one token from the exhausted metadata pool moves into an
actual registered mutator's remembered ring via shared APPEND/PUBLISH. The
other 31 tokens are released to wake the consumer. The ring's independent token
retains source metadata across decommit, both first-pass scans and transfer.
Fixture preparation supplies protection before buffer publication; this is
not a claim that a production store barrier created the captured description.

After source/destination scans and transfer finish, their inventory and mark
queue can both be empty while the source ring still has published work. A real
registered-worker end rendezvous rejects completion. A bounded buffer CLAIM /
COPY executes after resume; a second rendezvous rejects the outstanding copied
job. Only after its old source slot is RECORDed again does the consumer release
the metadata token and ACK the buffer job. Native source metadata is never
reconstructed from inaccessible heap bytes.

The two workers perform another source claim/transfer and destination claim,
for four scans and two transfers in every Young round. Final checks require an
empty producer ring, no active consumer serial, balanced leases, no source
bits, and the retained destination bit. Source DETACH remains an outer-driver
responsibility: its leaf alone cannot see undispatched producer buffers. This
scenario establishes the required producer-aware completion ordering, not a
complete production source-retirement/color-reuse policy.

### Page-bound remembered membership removal

`GC_RI_DETACH_PAGE` (inventory op 9) uses the existing 24-byte request ABI; its
operation-specific `phase` word is a live native `AllocationPage*`, rather than
BEGIN's phase snapshot. It validates exact set/page start, limit, incarnation
and Old generation. The page must be Closed and unowned, and its metadata lease
count must be zero. Allocating/owned/leased pages return WAIT; wrong bindings or
invalid page states return INVALID. Both preserve the entire request, inventory,
page and set. Existing empty-set/claim/epoch/walker requirements still apply.
C and PortableAsm implement the same checks under the shared heap/registry lock.

The worker scenario now calls this operation while the delayed source record
is in the producer ring and while its consumer job owns the copied token: both
unlink attempts wait. After token release, replayed bitmap work still rejects
unlink. Only after both scan/transfer pairs finish and workers join can the
same page-bound operation detach the empty source membership.

This is a necessary retirement guard, not an authorization to reclaim page
metadata or reuse addresses/colors. The caller must still redirect new source
producers and establish the outer retirement boundary; zero leases alone is
insufficient. The original metadata-only DETACH remains for exclusive setup or
callers separately proving no remaining page/entry/native borrow. No managed
header or payload is read by either operation.


## Registered mark-worker claim retargeting

The real-worker graph now moves its array after the first eight-slot scan,
while an exclusive claim retains the nonzero cursor and original metadata
obligation. A registered phase rendezvous rejects termination while that claim
is in flight. Shared relocation copies and publishes the array to another
Young run; in 16 large-array rounds the first source page is decommitted before
worker resumption. Other objects in the source run's tail page remain mapped.

Scan dispatch acquires a separate temporary metadata lease before resolving
forwarding. It must not pass the queue-owned token to a resolver that releases
source leases: that would invalidate the still-authenticated queue claim.
When the canonical reference/incarnation differs, guarded preparation produces
a destination obligation with the same MarkRecord. The worker exits its guard,
retargets the claim under the queue lock, verifies cursor/record preservation
and stale-receipt rejection, and only then releases the retired source token.
It resolves again before scanning the updated physical receiver. The temporary
access lease is released after every scan, separately from the job's lifetime.

All 64 Young/Old rounds require exactly one claim retarget, unchanged total
slot/slice counts, one visit per array slot, retained logical record identity,
rejection of old YIELD/FINISH receipts, and balanced destination leases. The
source and destination retain native metadata and strong producer protection
through fixture teardown. Initial scan-lease pressure uses the native wait
protocol described below. Forwarding-destination pressure also retries from
the unchanged claim. Offer pressure and arbitrary repeated concurrent
relocation remain production integration work; this is not a
source-address reuse or default-collector completion claim.


### Scan access-lease exhaustion

Before resolving a claimed job, scan dispatch can now return from failed lease
acquisition into the registered native condition-wait protocol. The original
queue claim and its own metadata token remain intact throughout the wait; no
access guard or partly acquired access lease survives. The worker checks the
entire claim is unchanged after phase adoption, then retries reference lookup.
Successful native return checks exact registry/context phase agreement under
the transition lock. A late waiter may already have adopted that snapshot, so
the interval does not require a strictly greater phase number.

The coordinator fills every free pool slot while the array is held after its
first slice. Existing queue/SATB tokens remain independently owned. Relocation
and optional source decommit occur before the next scan. One or both workers
then encounter the full pool, enter BlockedNative with live claims, and keep
termination from succeeding at a real phase rendezvous. Releasing only the
coordinator's tokens and broadcasting permits normal claim retargeting and
scan completion. All 64 Young/Old rounds require at least one such wait and
balanced access/queue obligations. Additional forwarding-destination pressure
is handled as described below; offers still assume capacity in this fixture.
Reservation, OOM policy and production default integration remain separate work.


### Forwarding-destination lease exhaustion

The no-poll access attempt now reports destination metadata exhaustion instead
of requiring acquisition to succeed. It releases its temporary source token,
returns no guard/address/private forwarding result, and leaves the original
exclusive queue claim untouched. Scan dispatch enters the registered native
wait, checks that claim is unchanged on return, and retries from its original
reference. Successful retargeting still precedes release of the queue token.

The fixture forces this failure with real independently owned lease tokens in
22 one-task-queue rounds (11 Young, 11 Old). Filling the remaining pool slots
inside the heap lock immediately before destination acquisition prevents an
unrelated marker from starting an offer with a payload slice still in hand.
The rejected attempt drops active leases from 32 to 31, retaining queue/SATB/
spill obligations. A native-blocked rendezvous rejects mark end, then release
of the test-owned tokens permits canonical claim replacement and scan finish.
This covers one forwarding edge under controlled pressure. Scan BUSY handling
is described below; arbitrary-chain hop budgeting, offer scheduling, production
reserve/OOM and retirement policy remain open. Native metadata and strong producer reachability are fixture-retained.


### Cooperative scan retries during forwarding publication

Scan access attempts now distinguish READY, BUSY and destination FULL. On BUSY
(no admitted source guard), the attempt releases its temporary metadata lease
and returns no private destination. The worker retains its exclusive queue
claim, polls the registered phase protocol, yields, and retries from the
original claim. It checks the complete claim is unchanged across this interval.
The fixture's offer/control wrapper retains its earlier no-poll Busy handling;
this scan change does not establish a complete production barrier dispatcher.

In every one of the 64 Young/Old array-move rounds, copying and destination
installation finish but forwarding publication is withheld. After the initial
metadata-pressure wait is released, the scan observes the actual claimed
forwarding cell. A completed phase rendezvous while it is still Busy proves
that scan retry can acknowledge without a raw guard or temporary access lease.
Mark end still rejects the in-flight claim. Only then does the coordinator
publish forwarding (and decommit the first source page in 16 large-array cases),
allowing the existing destination-pressure, retarget and scan checks to finish.
Native metadata/strong producers are retained by the fixture; arbitrary-chain
hop budgeting, offer retries, production retirement and default migration
remain open.


## Registered pending children and worker offer pressure

Each graph worker now attaches a persistent eight-cell root segment and reserves
its handles before marking begins. A bounded scan returns at most eight values.
After guard exit and before any poll/flush/allocation, each nonnull child is
published into its reserved root and removed from the private scan buffer.
Only registered roots and a scalar occupancy mask survive the next poll. Null
results leave already-empty cells alone; this is not a null-store barrier
shortcut, because there is no reference to publish or root value to change.
The empty eight-entry journal has room for the entire bounded publication.

Rooted offers reread their root after a metadata-pressure native wait. Worker
journal offers retain their copied, unacknowledged RootJournal claim throughout
both old/new offers; the immutable source entries and native producer remain
counted until ACK. Initial and forwarding-destination lease failures return
without a raw guard or temporary obligation. Publication journals are committed,
then child roots are cleared and deletion journals committed, before advancing
the parent claim's cursor. At shutdown, all handles are released, both segments
are scanned/cleared through the shared retirement protocol, and only then do
workers detach. Worker offers also poll on Busy as described below.
Coordinator/control scheduling and bounded multihop progression remain separate
production integration.

All 64 Young/Old rounds stop after the first child publication. A fixed test
inspection verifies the live child, seven null cells and its one journal entry;
mark termination rejects the still-owned parent. The coordinator then fills the
actual lease pool, lets the rooted offer block natively, checks the child is
still retained, and releases its tokens. The worker rereads its root and completes
queue handoff. Existing scan/relocation/pressure tests remain active, including
worker journals encountering pressure after their bounded per-entry polls.

The first attempt exposed an unhandled full pool in journal offers. Once retry
was connected, unnecessarily journaling every null scan result exceeded the
existing 40-second emitted-harness alarm; a trace reached round 28 rather than
stalling. Publishing only nonnull results brought the focused test to 12.64
seconds without relaxing that alarm. This is a conformance-suite duration, not
a production pause/throughput measurement. Optional KLASSIC_GC_GRAPH_TRACE logs
round starts for diagnosing future integration stalls.


## Cooperative worker offers during child relocation

Rooted and leased-journal worker offers now request cooperative access attempts.
Busy returns no guard and releases temporary metadata ownership; the retry
polls/yields while retaining the registered root or unacknowledged journal job.
Rooted retries clear their private read result and reread the root. Leased
journal retries retain the immutable copied-entry producer through ACK. The
coordinator/control wrapper keeps its earlier no-poll behavior. Scan-only
pressure injection is distinct from this shared cooperative behavior.

All 64 Young/Old graph rounds now move the first pending child before its first
queue offer. Copy and target installation finish while forwarding stays claimed.
After metadata capacity returns, the offer must observe Busy and complete a
phase rendezvous while the pending root still holds the original reference and
both mark epochs of the child remain unset. Publication then permits canonical
queue handoff and subsequent scanning. The target run later receives the moved
array as a second object; its forward table has two entries. Target PREPARE and
INSTALL use the same heap lock as reference leasing, with exclusive coordinator
append ownership and already-installed objects still readable.

An initial untraced focused run hit the existing 40-second alarm. After enforcing
the target-lock contract and adding optional stage traces, the traced run passed
in 12.70 seconds. The timeout was not localized, so a causal claim about that
failure is not established. Native metadata and source mappings retained by the
fixture are not a production retirement/address-reuse proof.

Scan-pressure fault injection now occurs at the target claimant's lease
acquisition under the same mutex, rather than at the preceding rendezvous.
Concurrent workers can release temporary leases between those events. The old
schedule could miss FULL entirely and leave the coordinator waiting forever
for its notification. A TSan debugger reproduction captured that state. The
claimant still enters the real native condition wait; the coordinator retains
ownership of the injected tokens and wakes it after a phase rendezvous.

Remembered-set workers hand their loaded child to the queue using the same
cooperative rooted offer as ordinary scan outputs. They discard private read
results before retry, preserve the registered output root and remembered slot
claim during native waits, and acknowledge the slot only after queue handoff.
The integration fixture forces full metadata capacity at this boundary in
every Young round and requires a phase rendezvous before returning capacity.

## Rooted queue offers use bounded root access

A rooted worker offer now follows forwarding with one RootAccess invocation
per iteration. Each ADVANCED result has journaled the destination into its
registered root and released access ownership before a phase poll. READY's
guard and lease go straight to guarded routing, followed by guard exit and
queue handoff under the metadata mutex. Root journal pressure can drain a
claimed entry; full metadata capacity uses the existing native wait.

The shared C/emitted graph schedule moves the pending child twice before its
queue offer, rendezvousing between the two root adoptions and before queue
publication. The intermediate and final copy share the array target run,
whose three forward entries are installed under the heap lock. The producer's
root journals retain both old values until their handoff; no source metadata
retirement or address reuse is inferred. Immutable journal offers and scan
claims still require a separate bounded-chain integration.

## Legacy C timing migration

C logging separately measures request-to-quiescence, stopped time through
release, and each mutator's park-to-resume interval; the historical pause
fields remain action-duration counters. An early parked owner includes the
wait for later owners, and consecutive requests may share one continuous
park interval. BlockedNative owners create no artificial park samples.
Measurement boundaries and the limited Linux smoke result are documented in
[benchmarks.md](benchmarks.md). Unbounded stopped legacy traversals, direct
backend parity, allocation-stall/assist measurements and per-cycle JSON
events remain independent requirements.

## Bounded journal COPY cursors

A worker retains the root journal's counted claim and immutable ring entry
while offering each side of its private COPY. The COPY can advance to the
next forwarding reference only with acquired destination metadata ownership.
After replacing its reference/token and releasing the prior token, it may
poll. Destination-capacity failure discards the private next value and waits
natively with the current COPY cursor and token intact. Canonical guarded
routing consumes that token before journal ACK becomes permissible.

The C/emitted graph schedule validates two successive cursor adoptions and
full destination capacity against real phase rendezvous. This closes the
worker journal's unbounded synchronous chain traversal in the fixture; its
original strong producer and all forwarding controls remain retained until
cleanup. A general retirement proof and independently interleaved Young/Old
cycles remain separate requirements. Claimed scans still use their existing
chain resolver. Array-pressure notifications now identify that exact claim,
so earlier unrelated pressure cannot satisfy its required waiting event.

## Bounded access cursor behind a scan claim

The exclusive marker keeps its authenticated queue receipt (reference, token,
record, serial and payload cursor) intact while a separate access cursor
advances through forwarding. Every step acquires destination metadata before
releasing the preceding cursor token and polls only with no source guard.
Busy or destination FULL preserve this cursor and the original queue token;
FULL discards the private next reference before native waiting.

Canonical guarded preparation transfers the access token into MarkRetarget,
which authenticates and replaces the queue reference and rotates the receipt
serial. The retired original token is released and a fresh guard is acquired
before scanning. The fixture now checks phase rendezvous before retarget and
two successive array moves in 48 rounds, with unchanged byte cursor and one
final retarget. Control-plane resolution and production retirement remain
separate from these bounded worker paths.

## Guarded physical slot repair

`GuardedHeal` names the receiver scope/page, descriptor registry, byte offset,
exact observed reference, desired reference and adopted phase. It is a bounded
CAS leaf with no polling, allocation, waiting or barrier logging. It rejects
null repair inputs and non-reference slots. The receiver guard remains held
on every return, including CHANGED; the outer barrier owns EXIT.

The caller must establish that observed and desired denote the same logical
object, retain the observed referent and forwarding metadata, and perform any
required marking. The leaf validates desired mark/remap colors and preserves the observed
remembered bits, so repair cannot forge a remset-recording stamp. It cannot infer
identity or liveness from color. Exact-bit CAS preserves intervening stores;
CHANGED does not return the replacement or authorize changing the logical
load result. A prior guard may repair after selection closes admission because
copy still waits for every admitted guard to exit. This primitive is exercised
by the shared moving-graph and store/copy interleavings, not yet by the default
generational compiler/runtime path or an all-slot retirement traversal.

### Bounded scan locations

`MarkScan.locations` optionally receives payload byte offsets alongside loaded
references, including null slots. C and PortableAsm validate alignment, range
overflow and overlap with the value buffer before writing either output. The
bounded slice preserves unwritten entries and does not advance its mark claim.
Mixed scalar/reference layouts return actual offsets rather than reference
indices. Locations are scalar data suitable for retaining across a poll; they
do not authorize access to a stale receiver or extend a referent lifetime.
Worker-driven repair must re-resolve the receiver and protect both the observed
referent and desired value before calling `GuardedHeal`. The moving-graph worker now performs this repair after child offers and
publication-journal consumption, before clearing its pending roots. It
reacquires the receiver with `graph_scan_enter`, then atomically snapshots its
uniquely owned, still-reserved native root cells inside the finite guard. This
snapshot does not invoke the public root-load API or resolve forwarding; the
preceding rooted offers establish the desired child and the fixture retains
all resolution metadata through teardown. Only byte offsets and exact CAS
comparison bits survive polls, never payload addresses. Comparison bits are
not used as roots or dereferenced. General retirement/address reuse still
requires a separate proof and production integration remains open.

In 32 graph rounds the worker repairs the twice-moved child before its array
claim yields. In the other 32 a real guarded null store occurs after the scan
and before child forwarding: the worker must return CHANGED and preserve null.
The deletion retains a second SATB entry and owner lease through mark-end
checks and bounded buffer copying; both entries must reach the queue before
ACK and lease release finish. Young and Old mark rounds cover both outcomes.
A coordinator CAS with the original observation must also return CHANGED.
The initial array copy and the 48 second copies preserve the exact winning
slot value; 16 rounds (eight with deletion) protect the original source mapping
before resumed scans. The schedule uses the existing pending-root gate, not
timing sleeps.

Validation of the locations extension: 1,578 C/x86 full-state scan cases and
AArch64 emulated replays, eight C/Rust ABI tests, and the full workspace (794
passed, zero failed/ignored across 25 test invocations). The workspace includes
the required ASan+UBSan and TSan runner. Clippy with warnings denied, formatting
and inventory checks passed. Native Darwin execution and hardware ordering are
not established by emulation. The default collector remains legacy.

### Physical source payload readiness

`SourceDrain` / `gc_source_drain` and the shared emitter scan at most 256
forwarding entries per call without reading payload memory. The unique
retirement scheduler excludes allocation, destination append and table
replacement throughout the scan and subsequent OS cleanup. The allocator's
complete, immutable-key allocation table contains every object in the run;
this is a required metadata invariant, not reconstructed by reading headers.
The request captures incarnation, allocated top and used table length. Each
slice holds the heap lock and revalidates those boundaries, validates ordered source keys and published
reference encoding, and requires closed, zero-reader access gates. Empty,
claimed or retained objects return WAIT at the first unfinished entry. INVALID
preserves the request; MORE advances only a bounded native cursor. Published
forwarding is immutable, so entries passed by earlier slices cannot reopen.

DONE release-publishes `PAGE_EVACUATED` before returning, making future target
append and TLAB binding invalid. The completed cursor may be retried unchanged;
a partial cursor on an evacuated page is rejected. DONE permits the exclusive
scheduler to decommit/protect the source payload; cleanup failure leaves the
page sealed for retry. It neither frees the virtual range nor discards forwarding/start metadata,
changes page state to FREE, or credits the heap budget. Metadata leases may
remain live. References still resolve through those retained native controls.
Final source/color retirement, virtual-address reuse and OS retry scheduling
remain separate pending work. Whole-run payload accounting is provided by the
SourceUncommit transaction below; partial-range accounting is still pending. The helper does not
certify dead unforwarded objects or partially evacuated subranges.

The graph fixture uses the candidate before both Old-owner source decommits
in each of 32 Young rounds: copy completion without forwarding publication
returns WAIT, publication changes that to DONE, then real OS protection occurs
while metadata leases and remembered claims remain live. The existing partial
large-array source protection is a different fixture proof and is not silently
covered by this whole-run helper. C/x86 compare 107 complete request/metadata
states, including retained/claimed entries, guard drain, bounded continuation,
invalid snapshots/geometry/reference encodings and unchanged failure outputs.
That initial 107-case validation preceded sealing; the sealed-page extension
has 112 cases and additionally rejects stale/partial completed cursors.

Metadata acquisition, encoded-reference leasing (including run tails),
remembered-owner lookup and page-bound remembered detachment accept evacuated
runs while retaining their ordinary lifetime checks. Resolve returns forwarded
references only: it cannot grant raw access on an evacuated run even if a
malformed entry is empty, retained or claimed, and rejects inherited EXIT
requests. Guarded payload operations still reject this state. The graph checks
that actual target PREPARE fails after each of its 64 full-source decommits.
The scheduler still excludes pre-existing private target candidates throughout
retirement; the state transition does not establish that ownership by itself.

### Evacuated payload uncommit accounting

`SourceUncommit` is a stable-address, 48-byte cleanup request. BEGIN and FINISH
run under the heap lock; the OS operation runs outside it. BEGIN validates the
sealed run's head/incarnation/extent and binds `AllocationPage.owner` to the
exact request address while retaining its heap payload charge. In allocating
pages that field still holds the mutator identity; the page state distinguishes
the two uses. An outstanding request cannot be copied or moved. A copied FINISH
fails even when all request fields match, because the page retains the original
request address.

Only after successful OS decommit does the scheduler clear committed_mapping.
FINISH validates that cleared flag and ownership, decrements HeapPool.committed
by the run extent once, updates pressure, clears the owner and release-publishes
`PAGE_UNCOMMITTED`. Failed/unattempted cleanup retains the sealed run, owner and
charges for retry. Repeated FINISH or BEGIN on an uncommitted run is rejected.
No bitmap/table charge, start bit, forwarding key, identity or virtual range is
freed. Run geometry and tail metadata remain the allocator's immutable invariant;
the transaction checks the head in constant time rather than traversing a run.

Reference/metadata leasing, remembered-owner lookup and page-bound detachment
accept UNCOMMITTED controls with the same lifetime checks as EVACUATED. Resolve
only follows forwarding, never grants raw access. Allocation and copy-target
admission reject both states. This is physical-memory accounting, not a source
retirement or virtual-address reuse certificate. Partial large-array protection
in the graph fixture is not credited by this whole-run transaction.

The graph uses candidate BEGIN/FINISH around 64 real full-run OS decommits,
requires premature FINISH to fail without returning budget, checks payload
credit and metadata-charge preservation, and rejects repeated completion.
154 C/candidate full-state cases cover both generations, run heads/lengths,
retained leases, pressure changes, copied requests, malformed extents and
unfinished cleanup. An OS retry scheduler, final metadata retirement and default
collector integration remain incomplete.

### Incoming forwarding dependencies before metadata retirement

`ForwardIncoming` scans one retained forwarding table against a half-open
target offset range, examining at most 256 entries per call. C and the shared
emitter return MORE for a bounded continuation, WAIT for a claimed publication,
and FOUND with the scalar source offset for a published destination inside the
range. FOUND and WAIT retain the cursor at that entry. Invalid input preserves
the complete request and table. Empty and retained entries have no published
destination to inspect.

The caller must freeze relocation publication across the entire audit, exclude
table append/replacement, and retain both the scanned table and target controls.
WAIT does not implement that freeze: an earlier empty entry could otherwise
be claimed after the scanner passes it. Roots, heap slots, buffers, jobs and
native handles still require their own remap/lifetime audit. Neither a DONE
result nor zero metadata leases permits metadata or address reuse by itself.

After joining workers, the moving graph checks 32 source chains whose old and
intermediate payloads are already uncommitted. Even with zero metadata leases
and the source remembered inventory detached, the earlier immutable forwarding
entries still name those ranges. The deterministic model likewise rejects
retiring the middle source of A -> B -> C until A is retired, even after all
ordinary reference storage is remapped to C. This check does not yet connect a
production publication freeze or enable final source retirement.

### Physical repair of concurrently scanned roots

`RootWalkHeal` authenticates a cell in the already-scanned prefix of a live
walk, using its current lease serial, registration/scope and segment geometry.
The single walk owner keeps the native storage leased until the operation
finishes. C and PortableAsm perform one exact-observation CAS, returning CHANGED
when an owner store or release has replaced the observed bits. They never read
the owner's plain active/reuse fields, poll, allocate or dereference a managed
address. Invalid requests leave all state unchanged; success changes only the
atomic root value.

Both references must be nonnull and valid, and every low-16 color bit must
match. The operation repairs physical location without granting a newer mark,
remap or remembered epoch. The caller proves that the two references denote the
same logical object and protects their liveness and metadata through CAS. The
native walk lease alone does not supply that proof or prohibit managed-address
reuse. A same-object repair does not append a logical root journal entry or
increment root_revision; competing logical mutations continue doing both.

The moving-graph root audit repairs the scanned registered cell after handing
its snapshot to marking. The durable snapshot protocol below now permits polls
between forwarding edges. This fixture retains all forwarding controls and
logical objects through the operation. In all 64
rounds it audits after array relocation and before the owner changes the root,
checks the exact final destination and unchanged colors, and checks that journal
publication and logical revision did not advance. Sixteen rounds use protected
large-array source pages. The scan and repair remain outside global stops.

804 complete-state cases cover epochs, active/retiring segments, scan bounds,
lease serials, malformed references and intervening replacements/releases.
64 controlled scanner/owner rounds additionally use real root STORE/RELEASE,
retain their journal entries/revisions, and reject healing after the walk ends.
These tests do not make RootAudit a retirement certificate: full physical root
remap completion, late publications, all other reference holders, forwarding
publication freeze and final metadata/address reuse still need coordination.

### Durable root snapshots through forwarding and polling

`RootWalkItem` CAPTURE reads exactly one cell of the leased captured prefix into
the walk's pending_reference and pending_observed, records its index, and advances
the scan cursor. Both values are explicit pending-work/retirement holders; the
existing registry lease and walker counts keep mark termination from completing.
Native storage retention alone would not give a copied reference this meaning.
The walk's unique owner retains the record through every poll/native wait and
must hand off its strong work before ACK. The observed value/index remain fixed
for physical repair; the current reference can advance along verified
same-object forwarding only after acquiring destination metadata protection.

CAPTURE returns WAIT while an item is outstanding. Bulk scan rejects outstanding
holders, and NEXT cannot release the segment even when its scan cursor and mask
otherwise indicate completion. BEGIN also rejects leftover holders. ACK matches
both the walk serial and captured index: the segment serial does not change
between items, so matching that serial alone would accept an obsolete ACK for
the next item. ACK clears all pending holder fields after mark handoff and any
repair. It does not alter the source root, publication journal or owner revision.
These leaves never poll, wait, allocate or access managed payload themselves.

The graph audit now uses CAPTURE, the same bounded forwarding/metadata transfer
loop as journal COPY work, exact root repair, and ACK. It polls between edges
and items with its registered participant. Its journal drain also uses counted
COPY cursors through edge polls. All 64 moving-array rounds require the durable
root snapshot to traverse the expected one or two forwarding edges before repair.
The original comparison reference remains held until ACK, preventing a future
retirement implementation from treating it as disposable merely because the
current cursor has advanced.

2,749 full-state cases cover capture/ACK, owner replacement after capture,
nulls, retiring segments, stale item ACKs, malformed controls and preserved
requests. Additional walk/scan/audit vectors reject bypass through their existing
APIs. This still does not implement a complete runtime retirement audit or prove
independent Young/Old coordination. Source/address reuse must account for both
pending fields, in addition to every other reference holder. Production pressure
recovery and late-publication coordination remain separate work.

### Registered root scanner under metadata pressure

The root audit accepts the executing context explicitly, allowing a registered
GC worker to scan another owner's roots without operating through that owner's
context. The graph now attaches an additional OS worker for the post-relocation
audit. This worker has no logical root segment of its own: its leased pending
walk item is the counted strong-work holder. This avoids manufacturing root
mutations/revisions merely to stage a scanner snapshot.

Each of 64 rounds exhausts the metadata pool before the scanner acquires its
source token, then again at its exact first destination-token acquisition. The
second injection occurs under the heap lock after source acquisition, avoiding
a race with unrelated workers returning tokens. The scanner discards the private
forwarding result before native wait, retains the original snapshot and (at the
second wait) its current source token, and retries that edge after wakeup.
The coordinator releases only the injected tokens under the same mutex as the
wait predicate, then notifies. Native reentry adopts the current phase before
managed work resumes.

Both waits cross actual MarkEndCheck/ConcurrentMark adoption rendezvous. While
the scanner is native-blocked, the graph checks its unchanged pending snapshot,
lease serial/index and source-token identity; counted walk work prevents mark
completion. The scanner then traverses forwarding, offers to marking, repairs,
ACKs and detaches. Its native context remains alive until OS join completes.
The fixture checks four registered owners during this interval and returns to
the original three after scanner teardown.

In 32 rounds, evenly split between Young and Old marking, the real root owner
stores null during the destination wait. The delayed physical repair must return
CHANGED and leave null intact. The audit consumes the logical deletion journal
without modifying its publication count or owner revision. Only a subsequent
explicit owner store republishes the root; a fresh audit then repairs it.
These use real moved objects; the pressure schedules also include protected
large-array sources. Runtime-wide source retirement,
independent generation scheduling and default compiler adoption remain open.

### Closing relocation admission before source sealing

`RelocationGate` counts registered move jobs in a native intrusive permit list.
Every participating mover acquires a permit before source claim or private
target preparation, and releases it only after forwarding publication or after
private cleanup and source retention are complete. A permit is an ownership
obligation; the gate does not inspect or certify the physical copy or cleanup.
Gate, permit and request controls are retained and all operations hold the same
heap lock. Each operation examines only fixed metadata and adjacent list links.

CLOSE denies new admission immediately but returns WAIT while existing jobs
remain. Those jobs may still publish and RELEASE, so closing does not prevent
their progress. CHECK succeeds only for a closed, empty gate. The coordinator
keeps it closed through the entire incoming-reference audit or source seal.
REOPEN advances the epoch without wrapping. A saved audit must check that epoch
and closed/empty state again under the lock before acting on its result.
Reaching an empty gate, or merely reading its epoch, is not retirement authority.

Requests authenticate a stable permit with serial and epoch; copied controls,
duplicate releases and stale requests after permit reuse are rejected. Serial
and epoch exhaustion preserve state and report EXHAUSTED. Gate closure can
change closed even on WAIT; invalid requests leave the complete state intact.
No leaf allocates, traverses the job list, polls or accesses managed payload.

All moves in the graph fixture now enter before source claim and leave after
successful complete-copy publication. Its source-drain wrapper closes admission
first: 64 still-unpublished copies return WAIT without advancing the drain
cursor. After publication, the closed/empty interval covers source sealing;
reopening cannot admit a new target into the now-EVACUATED run. The existing
payload-uncommit transaction then proceeds. The final 32 incoming-reference
audits also close admission, reject a new permit, and verify unchanged epoch
and closed/empty state before reopening. Existing incoming forwarding still
prevents final retirement; this gate does not erase that dependency.

533 full-state C/emitter cases cover all three-job release orders and closure
boundaries, completion while closed, malformed links, copied controls, stale
permit serials and counter exhaustion. This is integrated relocation admission
in the conformance graph, not global production adoption. Every production move
and cancellation must enroll through the same gate before its quiescence can
support reclamation. Root/heap/buffer/job/native holders and address/color reuse
still require their separate audits.

#### Cancellation before target installation

The relocation pipeline keeps admission ownership through each private-copy
failure boundary. A mover that has claimed and closed its source can cancel
before prepare, after prepare, during copying, or after copying but before
INSTALL. Its exclusively owned, uninstalled destination range is cleared before
publishing RETAIN. Source access is then reopened, and only then is its admission
permit released. No allocation top, object-start bit or forwarding-table entry
was installed, so later moves may reuse that private range without removing a
published object. This rule cannot be applied to an installed destination.

A closed, empty admission gate only certifies that jobs have finished. It does
not make a retained source evacuable: SourceDrain stops on that entry and the
source mapping and payload charge remain live. The OS-backed fixture checks
this distinction at four failure stages in both target generations, then checks
that the retained object's payload/identity survive alongside four successfully
moved objects. These are caller-side integration obligations; the gate leaf
cannot prove private cleanup or source retention from a RELEASE request.

#### Recovery after target installation

INSTALL is the cancellation boundary. After it publishes the start/table/top,
private-range cleanup is no longer legal: readers can acquire destination
metadata. The relocation job keeps its source claim, destination ownership and
admission permit until forwarding completes. An interruption before forwarding
resumes publication; an interruption after forwarding reads the immutable
forwarding cell and checks the exact destination receipt, then completes the
job. It does not replay copying or INSTALL. A receipt read does not transfer
claim ownership to another actor or authenticate an unrelated request.

The OS-backed pipeline exercises both interruption points with Young and Old
targets in 32 additional rounds. Repeated INSTALL, never-installed-run RELEASE,
and invalid-color publication reject without changing installed metadata,
copy state or accounting. Before forwarding, source access waits without a raw
guard. A destination metadata lease spans recovery and guarded payload/identity
verification. Admission stays closed and nonempty until the original job releases
its permit; duplicate release rejects. All successful source runs now pass
SourceDrain with closed, empty admission before their mappings are decommitted.
Retained-source rounds still stop at the retained entry.

This is the recovery protocol in the conformance scheduler. Production durable
job storage, rescheduling on resource failure, and generation-specific handoff
failure recovery remain separate integration work. No new claim, physical copy,
object identity or target rollback is introduced on resumption.

#### Installed-job publication receipts

`RelocationJob` is a stable 88-byte native control for the interval after target
installation/handoff and before admission release. BIND records its own address,
gate, permit serial/epoch, source key/incarnation, unique claim ticket and exact
colored destination. The scheduler provides the initial permit/source/ticket/
destination fields; all bound inputs remain immutable. The gate and native
controls stay live under the heap lock, and the scheduler retains exclusive
ownership of the complete copy and both mappings.

CHECK authenticates the current permit, including neighboring intrusive links,
and the bound source key. It returns WAIT for that same pending claim, or records
state 2 only for its exact immutable forwarding destination. Repeated successful
CHECK is harmless while the permit remains valid. Copied controls, stale permit
serials, replaced source incarnations, retained/different forwarding and CHECK
after permit release reject without mutation. A successful CHECK permits FINISH,
which rechecks the exact publication, clears the permit binding and records
terminal state 3. Gate RELEASE rejects any still-bound job; the job leaf does not
release the permit by itself.

C and the shared PortableAsm emitter implement the same constant-time, metadata-
only operation. The OS-backed pipeline now uses it for all 448 installed objects
across its 96 rounds, including the two interrupted-publication schedules.
The caller still proves INSTALL and generation handoff, retains the copy job,
and uses the canonical heap gate for claim admission. BIND rejects a second
live job on the same permit and requires the claim ticket to equal its serial. This receipt
does not replace those scheduler obligations, allocate durable job storage or
supply a production job queue. In particular, it is not an arbitrary caller's
permission to take over another actor's claim.


#### Permit-bound installed-job completion

RelocationPermit now includes a job pointer and occupies 48 bytes. BIND requires
that pointer to be zero and assigns it to the stable native job control while
recording state 1. CHECK and FINISH require the exact back-pointer as well as
serial/epoch/source authentication. Thus even a freshly initialized second job
cannot bind the same permit. CHECK can record publication (state 2), but does not
clear the binding. FINISH requires state 2 and the same exact publication, then
clears the binding and records state 3. A pending, copied, stale or already
finished job cannot finish. Terminal jobs cannot CHECK again.

Gate RELEASE now rejects nonzero job pointers without mutation. Both the
OS-backed 448-object pipeline and every move in the 64-round concurrent graph
attempt premature RELEASE before and after CHECK and require rejection. They
FINISH before the actual RELEASE; graph teardown checks every job reached state
3 and every permit was cleared. The graph stores receipts beside its stable
permit records, so they survive copy/publication scheduling boundaries.

The binding starts only after target installation/handoff. Before binding, the
scheduler still owns private cleanup and source retention; the gate cannot prove
those actions. The source claim must come from the shared admission/claim funnel using the
canonical heap gate. That funnel derives its ticket from the winning permit
serial, which installed jobs must match. This change prevents early release of
an enrolled installed job and duplicate binding on its permit; it does not
replace the complete production relocation scheduler.

#### Shared installed-relocation completion funnel

`gc_relocation_complete_locked(gate, completion)` now implements the installed
job's progression in both C and PortableAsm. The 32-byte RelocationCompletion
control keeps its native owner address, job/copy pointers and stage. Each call
performs one bounded stage under the heap lock:

1. Bind the installed job and record the completion control's identity.
2. Check the exact publication receipt; if still pending, publish the complete
   copy and check again. An already published job does not re-read heap headers.
3. FINISH the checked job, unbinding its permit.
4. Release the permit and enter terminal stage 4.

MORE retains all job/copy/metadata/mapping ownership for rescheduling. Invalid
requests retain resources rather than treating an installed object as private
space. Terminal replay and copied completion controls reject. Stage 1 propagates
copy WAIT without discarding state. No stage allocates, calls the OS, waits or
copies payload bytes; the existing publication leaf only validates a completed
copy's headers before its forwarding CAS. INSTALL and generation handoff remain
preconditions, and the controls must remain live/immutable until DONE.

The shared emitter calls the existing job, publication and gate emitters as
internal routines. This replaces hand-written fixture completion ordering in
all 448 OS-backed pipeline objects and every move in the 64-round concurrent
graph. Deliberate external publication remains only at the fixture's interruption
seam, to verify receipt-based recovery. The graph executes at most four stages
per publication call; durable queue allocation/rescheduling remains future work.

The full-state suite includes every reference color, open/closed admission,
publication already completed before resumption, invalid controls at each stage,
and failed header validation followed by repair/retry. In the already-published
cases the old header is deliberately invalidated, proving that completion uses
native forwarding metadata instead of replaying copy/header validation.

Compiler inspection still finds ordinary direct output on the legacy 16-byte
header/high-bit reference ABI, while LLVM's current subset calls the legacy C
collector. Switching either requires allocation, root and barrier lowering to
move together. This funnel supplies reusable generated runtime code for that
migration; it does not activate the new collector in ordinary programs.


#### Shared relocation admission and claim

`gc_relocation_claim_locked(gate, claim)` is the matching entry funnel. The
48-byte RelocationClaim supplies a live source metadata pointer/key and a fresh
stable permit, with zero ticket/epoch outputs. Under the heap lock it acquires
admission before attempting the forwarding CAS. The acquired gate serial is the
unique claim ticket; a successful claim returns that ticket/epoch and retains
the permit. It does not close the source access gate: SELECT/CHECK still drain
all payload actors before any copying.

Busy, retained, already-forwarded or malformed forwarding states release the
newly admitted permit. Closed admission returns without claiming. Ticket-width
exhaustion also releases the permit: serials wider than 62 bits cannot be encoded
in a forwarding word. Admitted failures consume serials rather than rolling
back/reusing them. At u64 exhaustion ACQUIRE itself rejects without mutation.
All non-success results leave zero output ticket/epoch; no target preparation
has begun, so a losing attempt has no private destination to clean.

Installed-job BIND/CHECK/FINISH now require ticket == permit.serial as well as
the existing native ownership/epoch/key checks. With one canonical gate per heap,
a second permit cannot adopt the same source claim: it has a different serial,
and claiming with that serial loses the source CAS. Callers must not manufacture
raw claims through a second gate. This replaces the earlier conformance-only
constant tickets in every OS-backed pipeline and concurrent-graph move.

The 431 complete-state cases cover admission list topology, source states,
all legal destination colors, repeated losers and both 62/64-bit serial limits.
64 real two-thread metadata-actor rounds verify one winner, one released loser,
closure rejecting retry, and winner retention before final permit release.
These actors use no managed payload and do not certify production worker
registration. Source metadata and strong liveness still require caller protection.

#### Bounded private-copy discard

`gc_relocate_discard_locked(target, discard)` implements the cleanup step before
source retention or a fresh copy attempt. Its 40-byte RelocationDiscard control
holds the source/copy pointers, its native owner address and cleanup cursor/size.
The caller keeps its winning permit, source liveness/metadata, both mappings and
exclusive target append rights. Source SELECT/CHECK must already have drained
all actors; every call rechecks the exact claim and closed zero-count source gate.

A prepared destination must still be at target.allocated_top + 32. An installed
object advances that top and cannot be discarded. Before touching bytes, the
helper validates target state/bounds, source header and completed/partial copy
cursor, and excludes source/destination overlap. For an untouched stage-0 copy,
there are no dirty bytes to erase, including failure before PREPARE. Otherwise
the cleanup size is header plus rounded payload, with a minimum 16-byte payload
reservation even for zero-length objects. The source header stays immutable and
is rechecked while the source remains frozen.

Each C/shared-emitter call clears at most 256 bytes. MORE preserves ownership for
rescheduling. DONE is idempotent while the original claim and target reservation
remain valid; copied controls, cursor corruption, retained/published sources,
live source readers and changed target tops reject without mutation. The helper
never changes allocation top, starts/table metadata, logical identity or budgets.
It does not retain/reopen the source or return the permit: those actions follow
DONE, under the existing caller protocol. This is the bounded cleanup step,
not a complete production cancellation scheduler.

The OS-backed pipeline replaces its private-range memsets in 48 cleanup jobs:
32 cancellations and 16 partial-copy retries. Every slice checks the retained
permit and source freeze, and copied cursors reject. Retries create a fresh copy
request and run PREPARE again after cleanup; they do not clone/replay an old copy
receipt. In the 32 installed-recovery rounds, attempted discard rejects and the
installed object remains readable through its metadata lease/access guard.

### Private relocation admission inspection

RelocationGate INSPECT (5) validates an unbound permit with the same nonce,
epoch and adjacent-list checks as RELEASE, under the heap lock, without changing
the gate, request or permit. It succeeds with open or closed admission. Installed
job bindings reject this operation; their completion uses the bound job receipt.
The private-copy pipeline inspects admission before every bounded discard slice
and matches its serial to the source-copy ticket. The caller still owns strong
liveness, metadata, mappings and exclusive target append/cleanup. INSPECT alone
is neither a cleanup receipt nor permission to release admission early.

### Resumable private relocation cancellation

RelocationCancellation is a 56-byte stable native control containing its unique
owner address, permit serial/epoch, target page, discard control and stage.
`gc_relocation_cancel_locked` and its shared emitted counterpart perform one
bounded operation under the heap lock per invocation:

1. INSPECT the live unbound permit and clear at most 256 private target bytes.
2. After the discard receipt reaches DONE, change the owned source claim to
   retained with the existing closed-zero-actor forwarding CAS.
3. Reopen retained-source admission using the shared source-access CAS.
4. Release the relocation permit, then enter terminal stage 4.

MORE retains all ownership and permits rescheduling outside the helper. Each
step authenticates the permit nonce and requires its serial to match the copy
claim. Copied controls, terminal replay, installed targets and bound installed
jobs reject without mutation. Source readers may enter after stage 3; their
nonzero access count does not prevent the final relocation permit release.
No source actor may enter before reopening, and cleanup never credits committed
payload or native metadata budgets. The source remains a retained live object.

The OS-backed pipeline uses this funnel for all 32 cancellation rounds, across
failure before PREPARE, after PREPARE, during copy and after copy before INSTALL.
Each round enters a real source guard after reopening and holds it through the
final release. The 16 private-copy retry jobs continue using bounded discard
without source retention so a fresh PREPARE can retry the same owned claim.

Stable/disjoint controls, immutable inputs, source strong liveness, metadata,
mappings and exclusive target append/cleanup remain caller obligations until
DONE. This supplies the cancellation transition sequence, not the production
scheduler, native-control allocator or default compiler/runtime activation.

### Finite allocation into an output root

AllocationInto holds native pointers to an AllocationRequest and a RootRequest.
The root must already be reserved and empty. The shared C/x86/ARM entry first
runs ROOT_PREPARE_EMPTY (operation 5): all STORE ownership, handle, journal-space
and revision checks, plus null-cell validation, with no mutation. A full journal
returns ALLOC_ROOT_FLUSH (5), exhausted revision/cursor returns
ALLOC_ROOT_EXHAUSTED (6), and neither result allocates an object.

After preflight, the existing finite TLAB allocation either fails unchanged or
returns its initializing object. Success publishes a RootChange from null to the
new reference, then the output RootCell, then root_revision. No fallible step,
poll, lock or OS operation intervenes. The sole owner controls the context,
segment and journal producer throughout this interval; concurrent consumption
can only free journal capacity, and a scanner cannot change an empty root's
logical value. Live/disjoint native storage and committed prezeroed TLAB storage
remain caller obligations. PREPARE_EMPTY is not a durable reservation across
polling or another producer operation.

The OS-backed page pipeline now uses this entry for all four allocations: large,
zero-sized and mixed-layout objects. Payloads remain INITIALIZING on return;
initialization, ready publication, slow-path refill/flush and the production
compiler root lowering still require their own integration.

### Registered observation during graph construction

All eight object allocations in each moving-graph round now use AllocationInto
and nine reserved construction roots (the ninth checks metadata exhaustion).
A separate OS thread attaches through the real worker/handshake registry. Before
each producer publication or deletion it acquires a segment lease, scans at least
once, and signals readiness. It keeps scanning while the producer runs, accepting
null or the acquired published reference; it never reads initializing payloads.
The completed-producer signal permits an exact post-publication snapshot check.

The observer then claims and copies the single-entry root journal and withholds
ACK. The next allocation must return ROOT_FLUSH without advancing TLAB, mark
arena, object result, root value or root revision. Only after the observer ACKs
may the owner retry successfully. Metadata exhaustion is checked with the ninth
empty root after journal capacity is available, so the two failure paths cannot
mask each other.

This is initial idle-phase construction: copied changes must have phase zero,
and ACK asserts no phase rendezvous or active mark. It does not exercise newly
allocated objects during active marking. After payload initialization and ready
publication, all nine roots are released through the observed journal, the
observer detaches/joins, and leased retirement scanning, clearing and FINISH
remove the construction segment. Existing Young/Old marking, moving, root audit
and remembered-set tests then run with their original graph/liveness expectations.
Each round now consumes five distinct thread identities, including this observer;
identities are never reset to hide the extra registered actor.

### Routing initializing allocation roots

GC_ROUTE_WAIT (3) is a non-mutating result for a fully authenticated guarded
INITIALIZING header. Routing validates the phase, source guard, page bounds,
reference/lease key, descriptor and stable MarkRecord before returning WAIT.
It reads no payload, transfers no metadata lease and stamps no mark record.
Young routing does not skip an initializing Old target before this readiness
check. Invalid header states or malformed metadata still return INVALID.

The shared guarded-header validator has an internal initializing-header option
used only by mark routing. Ordinary guarded field access and direct mark prepare
remain restricted to PUBLISHED headers. Routing captures header readiness with
one acquire load; if it observes initializing, a later release publication is
handled by retry rather than reinterpreting a stale snapshot as ready.

Root/journal offer adapters release the source guard and temporary metadata token
before polling on WAIT. The original counted root-walk item, journal job or
registered output root retains the logical reference; no journal ACK or pending
item completion occurs. Root access restarts from its protected handle on retry.
The coordinator fixture's immediate offer still requires ready input.

A separate 64-round two-actor leaf test uses prevalidated fixture guards/metadata.
Its reader observes WAIT, exits the guard before yielding, and reenters on retry.
The initializer writes two reference slots and a scalar, then release-publishes
the ready header. The reader verifies the complete payload only after routing's
acquire sees READY. This tests header publication and ownership preservation,
not registered active-mark allocation scheduling or mark-end termination.

The moving graph now also exercises INITIALIZING routing with real registered
roots and copied root journals during Old mark. WAIT leaves the journal consumer
position and receipt unchanged across a mark-end retry, even with an empty mark
queue and no temporary metadata lease. Readiness is release-published before
retrying and acknowledging the journal. This covers termination integration for
an object allocated in idle setup; active-mark allocation and the initialization
publication/cancellation API remain separate obligations.

`gc_guarded_initialization_publish` now supplies the finite C/shared-emitter
readiness operation. It borrows the existing `GuardedMarkPrepare` envelope with
an empty output: no layout change, mark stamp or lease transfer occurs. The
canonical source guard, matching lease, phase and header are validated; only an
INITIALIZING object on the calling owner's ALLOCATING Young page is accepted.
Selected, retained, closed-admission and already published objects are rejected.
Success release-stores PUBLISHED, ordering the initializer's completed field
writes before scanner acquire. All errors preserve the request, heap and guard.

This leaf requires exclusive initialization rights and registered protection for
parent/children, with publication work retained until normal mark handoff. It
neither proves field completeness nor performs child barriers or journal ACKs.
The caller exits its source guard before waiting or polling. Publication after
TLAB closure needs an ownership protocol; initialization cancellation and the
outer may-safepoint construction API remain unfinished.

## Persistent Young survivor age (ABI v3)

`gc_survivor_age_locked` and its shared emitter keep age in the stable MarkRecord
shared by relocated copies. The Young-cycle owner must hold its cycle/queue
lock, protect the record lifetime and prove that the still-Young object survived
this cycle. A new nonzero, monotonic cycle increments age, saturating at 15;
a retry in the same cycle does not. Old marking and physical copies alone do
not count as Young survival. Invalid inputs leave the complete record unchanged.
The threshold is fixed for each cycle and the result is a placement decision,
not an executed promotion.

The C/generated-x86 suite checks 984 cases; independent AArch64 emulation checks
492 cases. The moved-job pipeline checks that relocation retains the shared age,
that retrying the Young job cannot age twice, and that Old marking does not age
Young objects. The expanded record also passes 2806 TLAB cases, including dirty
age fields, with the C vectors replayed against AArch64 code. Linux full-workspace
validation passes 797 tests with no failures or ignored tests; the log is
`/tmp/klassic-survivor-age-workspace-final.log`. AArch64 emulation does not prove
execution on Darwin hardware.

The production Young-cycle owner must still account for every survivor,
including pinned/retained objects, execute promotion with its Old-mark and
remembered-slot obligations, and supply adaptive tenuring. These fixtures do
not complete G01, G09 or G12 or activate the new ABI in ordinary programs.

## Promotion publication ordering in the remembered owner

The C/generated-x86 remembered pipeline now selects an Old destination using
`gc_survivor_age_locked`, installs the complete copy, registers its reference
slot in the destination current remembered set, and only then publishes source
forwarding. The fixture's mixed descriptor has one reference slot; it registers
that slot even when null. Before publication it asserts that the source is still
claimed and that destination bookkeeping exists. After source decommit and root
following it checks the shared MarkRecord and its retained age. Subsequent
Young marking and remembered flips continue through the same existing pipeline.

This corrects the earlier fixture setup that published before registering the
slot. The mandatory marker covers 192 schedules per implementation, including
Young-child, subsequently promoted-child and null cases. The fixed threshold
and fixture-proven survival are not an adaptive policy or a completed Young
cycle. No Old mark is active at promotion; production Old-mark handoff,
bounded enumeration of arbitrary descriptors and retained/pinned promotion
remain outstanding. Ordinary compiler startup is unchanged.

Validation: generated-x86 integration passed in 21.54 seconds
(`/tmp/klassic-promotion-publication-linux.log`). The mandatory C ASan/UBSan
and TSan integration passed in 109.02 seconds
(`/tmp/klassic-promotion-publication-sanitizer.log`). Strict C syntax, formatting,
access-inventory freshness and whitespace checks also passed. No foreign-OS
execution or full default-runtime cycle is established by these results.

## Bounded promotion remembered-slot handoff

`gc_promotion_remember_locked` and the shared `promotion_remember` emitter now
perform the descriptor-to-remembered-inventory handoff used by the installed
Old destination above. `PromotionRemember` is a new 56-byte request with a
registry, registration, destination byte offset, descriptor/payload shape,
phase and cursor. Existing ABI v3 layouts are unchanged. It examines at most
one descriptor candidate per call, skips scalar-only spans arithmetically,
records reference locations even when null, and advances its cursor only after
successful registration. It never reads payload values or retains raw addresses.

The owning promotion controller must retain the exact installed private object,
its descriptor/page lifetimes and phase producer obligation. WAIT on a phase
change does not advance the cursor: adopting a new phase requires restarting at
zero before publication, re-recording all reference slots in the new bank.
Previous conservative records remain. DONE certifies only this remset scan;
it does not satisfy active Old marking or authorize forwarding on its own.

The C/generated-x86 suite exercises 82 cases covering fixed mixed fields,
scalar-only and zero-byte payloads, a 600-candidate array, unchanged rejections,
completed retries and phase restart. The remembered owner calls this production
helper instead of hardcoding its reference slot, and completes the subsequent
protected-source relocation/root/Young-mark path. AArch64 has independent
whole-state expectations for 14 mixed-field/rejection calls. The shared emitter
uses a 256-byte frame so all direct AArch64 stack accesses remain encodable.
Full cycle ownership, active Old-mark promotion and compiler lowering remain
incomplete.

Validation of the bounded helper: generated-x86 integration passes in 23.04
seconds (`/tmp/klassic-promotion-cursor-linux-final.log`); mandatory ASan/UBSan
and TSan integration passes in 113.73 seconds
(`/tmp/klassic-promotion-cursor-sanitizer.log`). All eight ABI tests, both-ISA
lowering, the 113-helper Mach-O export, ARM emulation (3503 helper calls),
strict C syntax, formatting, Clippy and inventory/whitespace checks pass.
The ARM log is `/tmp/klassic-promotion-cursor-arm.log`; these are emulated
instructions and OS API models, not actual Darwin execution.

## Promotion while an Old scan remains in flight

Sixteen schedules in `gc_mark_move_pipeline.h` now move a partially scanned
Young array into an actual Old destination page. A separate fixture-proven
Young survival ID (1000 + round) selects promotion while the Old queue keeps
its own epoch (round + 1), shared MarkRecord, in-flight claim at byte cursor 24
and overflow child work. The bounded promotion helper registers all 513
reference locations, including nulls, before forwarding publication. Every
registration slice checks that the entire Old queue/task/claim remains unchanged
and that its termination check still reports busy.

After publication the old source mapping is decommitted. The rooted receiver
follows forwarding, the Old claim retargets to the same logical record and
retains its cursor, and stale pre-retarget claims are rejected. The worker
finishes the remaining array and drains all three overflow children; every
reachable record has the original Old mark epoch, all metadata leases balance,
and the promotion age survives independently of the Old stamp. This exercises
real C/generated-x86 copy, page protection, remset, forwarding and mark helpers.

The schedules do not run a complete Young cycle or a global phase coordinator:
fixture roots establish survival, and an existing Old claim supplies the
promotion's outstanding Old work. New promotion with no prior Old work, active
SATB/phase adoption, coordinator progress and starvation remain unproved. This
is additional runtime evidence for G09, not G09 completion or default activation.

Validation: `cargo test --workspace` passes 797 tests across 25 invocations,
with zero failed or ignored tests, including the mandatory ASan/UBSan and TSan
marker and the generated-x86 promotion schedules. The final log is
`/tmp/klassic-old-mark-promotion-workspace.log`. Strict C syntax, formatting,
access-inventory freshness and whitespace checks also pass. Actual Windows/
Darwin execution and the full production coordinator remain outstanding.

## Admitting previously unmarked promotion work under Old queue saturation

An additional 16 schedules begin with a different scalar job occupying the
only ordinary Old task slot. The promotion parent initially has no Old stamp.
Ordinary OFFER returns FULL without changing the request, queue or parent
record. OFFER_OR_SPILL then transfers the source metadata lease and reference
into the parent's stable Old overflow cell before source selection or forwarding
publication. The context explicitly adopts Old ConcurrentMark (`0x10020`), and
mark preparation validates that adopted phase under its canonical guard.

After age selection, bounded registration of all 513 slots and actual promotion,
the source payload mapping is decommitted while overflow work still retains
its metadata lease. Completing the blocking scalar cannot make Old termination
report empty: the parent remains counted. REFILL/TAKE recovers its stale physical
reference, rooted forwarding resolves the new Old copy, and retarget transfers
the claim to the same stable record before scanning from cursor zero. The whole
array and the other two unmarked children finish; leases balance and all four
objects keep the same Old epoch. The scalar already scanned is deduplicated.

These schedules extend the earlier 64 partial-scan schedules rather than
replacing them. They exercise admission through C and generated-x86 helpers,
not a production promotion controller: lifetime/lock ownership, survival and
phase eligibility are fixture-established. Concurrent phase adoption, full
Young cycles and starvation remain outside this evidence.

Validation: the final phase-aware generated-x86 integration passes in 23.85
seconds (`/tmp/klassic-promotion-old-admission-linux-final.log`). Mandatory
ASan/UBSan and TSan integration passes in 108.31 seconds
(`/tmp/klassic-promotion-old-admission-sanitizer.log`). Strict C syntax,
formatting, inventory freshness and whitespace checks also pass. These focused
checks follow the prior 797-test workspace checkpoint; that full suite was not
rerun for this fixture extension.

## Fair inter-generation relocation admission

`gc_relocation_turn_locked` and the shared `relocation_turn` emitter provide
scheduler-lock-protected admission for Young/Old relocation. The new 56-byte
`RelocationCoordinator` has two coalescing pending bits, one active generation,
next priority, nonwrapping receipt serial, active gate and reserved gate epoch.
The 32-byte `RelocationTurn` binds a generation/gate to that coordinator and
serial. These additions leave existing ABI v3 layouts unchanged.

REQUEST can retain a subsequent request while a turn is active. ACQUIRE chooses
the sole requester immediately; if both are pending it follows the priority
and gives the next priority to the other generation. No second owner is admitted
until RELEASE. An acquire requires a closed, drained generation gate and reserves
its next epoch. The owner initially reopens that gate for jobs; later reopenings use RESUME. Release checks
the exact reserved epoch and requires the gate closed and drained again; live
permits, stale receipts, premature release and exhausted counters cannot hand
the turn to another generation. Rejections preserve input and controller state.
No root, payload or marking state is accessed and no lock spans actual GC work.

This is a runtime admission component, not the complete CycleCoordinator. The
owning runtime must request turns, route every mover through the selected gate,
perform phase adoption and source/incoming-reference/retirement audits, and
only then release the turn. Gate drainage alone is not retirement evidence.
It permits Old marking to continue independently during Young relocation, but
has not yet been integrated with a production cycle owner or ordinary compiler.
Alternation proves service order when turns complete; it does not prove a
wall-clock starvation bound or progress of an owner that stops running.

C/generated-x86 tests exercise 128 alternating admissions with fresh requests
during active turns, balanced live permit drainage and stale receipts; 44
additional rejection cases check whole-state preservation, priority and counter
exhaustion. Sole-requester cases cover both generations despite opposite
priority. Independent ARM expectations cover 70 calls and complete native-state
snapshots. These checks are not OS scheduling/performance measurements.

Validation: generated-x86 integration, mandatory ASan/UBSan and TSan, all
eight ABI tests, both-ISA lowering, the 114-helper Mach-O export, independent
ARM execution, Clippy, strict C syntax, formatting and inventory/whitespace
checks pass. Logs: `/tmp/klassic-relocation-turn-linux.log`,
`/tmp/klassic-relocation-turn-sanitizer.log`,
`/tmp/klassic-relocation-turn-arm.log`. ARM runs 3589 helper calls with modeled
OS APIs; real Darwin/Windows execution and scheduler performance remain open.

## Relocation-turn integration and recovery

The real-copy pipeline now requests a Young relocation turn while an Old
request is pending, and uses the admitted generation's gate for all five
relocation claims. All 96 schedules retain the Young receipt during copying,
forwarding, cancellation and installed-target recovery. Closing a gate with a
live job leaves both RELEASE and the waiting Old ACQUIRE unable to proceed.

The production `GC_RT_RESUME` operation was added for recovery: it authenticates
the current receipt and exact gate epoch, requires a closed/drained gate, then
reopens that gate and advances the bound epoch without changing owner, serial,
priority or pending requests. It rejects counter exhaustion before mutation.
An owner may therefore recover within its existing relocation turn rather than
handing another generation a partially completed source/target lifecycle.
The shared emitter performs the same bounded transaction.

After source-drain and protected-source read checks, the fixture releases every
metadata/raw lease and destroys its entire private heap before releasing the
turn. The pending Old request then acquires. This is quiescent fixture teardown,
not a production incoming-reference audit or a completed Old collection. A live
runtime must supply those audits before its turn release. The pipeline replaces
its recovery-time direct gate reopenings with authenticated RESUME calls.

The existing 128 C/candidate alternation schedules also resume within each held
turn, and now 46 unchanged-rejection cases include exhausted RESUME. Independent
ARM whole-state expectations cover 86 turn calls, including recovery reopening.

Validation: all 73 native tests pass, with no failed or ignored tests
(`/tmp/klassic-relocation-resume-native.log`). Mandatory ASan/UBSan and TSan
passes in 111.50 seconds (`/tmp/klassic-relocation-resume-sanitizer.log`).
Independent ARM emulation passes 86 turn calls and 3613 total helper calls
(`/tmp/klassic-relocation-resume-arm.log`). Clippy, strict C syntax, formatting
and access-inventory/whitespace checks also pass. This does not verify actual
Windows/Darwin execution, default cycle integration or wall-clock fairness.

## Forwarding-chain shortening before retirement audits

`gc_forward_compress_locked` and its shared emitter shorten one exact published
forwarding edge after the caller has followed its observed value to the same
rooted canonical object. The 48-byte `ForwardCompress` request carries the
retained source cell/key, observed reference, canonical destination and captured
relocation gate epoch. It validates the closed/drained gate, exact epoch,
closed source access state and reference encodings, then CASes only the observed
published word. Claims, retained states and concurrently changed destinations
cannot be overwritten by a stale request. It does not read source payloads.
Existing ABI v3 structures retain their layouts.

Logical identity is a caller proof obtained by following the observed edge and
validating the rooted destination, not something this metadata-only leaf can
infer from arbitrary addresses or debug ObjectIds. The caller retains all source,
intermediate and destination metadata lifetimes. Publication and compression
must both remain frozen throughout an incoming-reference audit; after shortening
an edge the audit restarts. Removing an incoming forwarding edge does not remove
references in root journals, native handles, buffers or other heap slots.

The real two-hop pipeline follows a root through both moves, checks the final
payload and shared MarkRecord, then holds source/destination leases and exclusive
intermediate metadata while shortening the first edge. Both old payload pages
are inaccessible. Incoming scanning first finds the intermediate edge, then
reports DONE after compression and restart; an old source resolves directly to
the final destination and a stale repeated update returns RETRY. Intermediate
metadata is deliberately retained because journal references can remain.

C/generated-x86 tests cover 156 exact-edge/gate/unchanged-rejection cases and
64 protected two-hop schedules. Independent ARM expectations cover 78 calls.
This removes a concrete prerequisite for reclaiming intermediate forwarding
metadata, but it is not a complete retirement certificate, virtual-range reuse
protocol, or default-runtime integration.

Validation passes: generated-x86 integration
(`/tmp/klassic-forward-compress-linux.log`), required ASan/UBSan and TSan
(`/tmp/klassic-forward-compress-sanitizer.log`), eight ABI tests, both-ISA
lowering, 115-helper Mach-O export, ARM emulation (3691 helper calls), Clippy,
strict C syntax, formatting and inventory/whitespace checks. ARM evidence is
`/tmp/klassic-forward-compress-arm.log`; actual Darwin/Windows execution and
complete runtime retirement remain unverified.

## Forwarding compression in the joined-worker graph audit

The existing two-worker graph pipeline now applies compression in its 32 Young
schedules after workers have joined and remembered source records are consumed.
The original graph source points to an Old source, which points to an intermediate
source, which points to the final Old copy. Both intermediate payload runs have
been uncommitted, while their forwarding tables and the final copy remain live.

Under the closed/drained publication gate, four counted metadata leases retain
the full chain. The owner resolves each observed edge and checks the canonical
header against the original stable MarkRecord under its finite access guard.
After EXIT, it holds the common metadata lock while shortening the two earlier
edges to the canonical copy. It then restarts both incoming-table audits with
publication and compression frozen. Each reaches DONE; both earlier sources
resolve directly to the final copy, and the counted leases are released.

Intermediate metadata and virtual ranges remain retained. These two table
results do not prove the absence of direct stale references in heap slots,
roots, journals or native owners. The extension validates integration with real
worker/phase/remset/source-uncommit history, not compression racing active
workers or a complete virtual-range retirement certificate.

Validation: the focused generated-x86 graph integration passes in 22.00
seconds (`/tmp/klassic-forward-graph-linux.log`). Full `cargo test --workspace`
passes 797 tests across 25 invocations with zero failures or ignored tests,
including required ASan/UBSan and TSan
(`/tmp/klassic-forward-graph-workspace.log`). Strict C syntax, formatting,
inventory freshness and whitespace checks also pass. This checkpoint includes
the new relocation admission/recovery and forwarding-compression helpers.


## Counted private load work (ABI v4)

MutatorContext now has an atomic `private_producers` tail field (offset 280,
size 288); RootedLoad has its own `admitted` tail flag (offset 1096, size 1104).
The context OS owner is the sole writer. A load increments before beginning
private work, retains the count over clean retries, and decrements only after
terminal cleanup. Multiple controls count separately; saturation never wraps.
Handshake owner transitions and direct context publication cannot pass a live
count. Mark-end checks it independently after rendezvous. Cancellation followed
by REARM permits a phase wait without rereading an already rooted observation.
Other producers and complete cycle admission still need integration; this is
not default collector activation. See compiler-migration.md for current tests
and remaining work. All consumers of the changed layouts require ABI v4.
