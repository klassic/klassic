# Standard compiler migration boundary

Status: incomplete. The ordinary compiler still uses the legacy collector.
This note records source-level prerequisites, not activation evidence.

## Verified allocation boundary

`NativeCodeGenerator::emit_gc_alloc_runtime` in `src/lib.rs` of klassic-native
still constructs `portable_asm::AllocCells` / `AllocTargets` and calls
`portable_asm::emit_gc_alloc`. Those targets include the legacy mark, trace,
collection and region-allocation routines. Replacing only this entry would
leave roots, field accesses and initialization on a different object ABI.

The legacy format documented in `gc_layout.rs` uses a 16-byte header and colored
absolute addresses. `gc_abi.rs` uses a 32-byte header and offset/metadata
references. These values must never share a heap or root stack during migration.

`compile_gc_alloc` and `compile_gc_record` return `NativeValue::HeapPointer`
after a call taking byte count and legacy type tag. The latter relies on zeroed
payload being immediately available for tracing. In contrast,
`gc_try_tlab_allocate_into` requires an already reserved empty RootHandle and
journal capacity; success leaves the object INITIALIZING. Its output is not a
published object and must not enter ordinary tracing as though initialization
had completed.

## Descriptor mapping to implement at compiler startup

These shapes are implemented in `runtime/gc/gc_builtin_descriptors.h` and
prepared by C or the shared direct emitter and registered in conformance tests.
Ordinary compiler startup does not
yet register them. IDs 1, 2 and 4 are reserved with stable, immutable package
storage; the pointer bitmap must remain alive with the registry.

| Legacy allocation shape | New fixed bytes | New element bytes | Reference bitmap |
| --- | ---: | ---: | --- |
| Raw bytes / scalar payload (tag 1) | 0 | 1 | None |
| Packed pointer record (tag 2) | 0 | 8 | Element bit 0 |
| Pointer list `[length, pointers...]` (tag 4) | 8 | 8 | No fixed bitmap; element bit 0 |

The pointer-list length is a scalar even when its bits resemble a reference.
Descriptor scan operates on logical payload bytes, not total block size or
alignment padding. Zero-length payloads and zero-element lists must preserve
that distinction. Mixed records and closure captures require their actual
compiler layouts; these three shapes are not coverage of every allocation.

## Next implementation boundary

The ordinary x86 entry still calls `emit_initialize_gc_heap`, which reserves
the legacy region heap and shadow-stack/worklist tables; its allocation leaf
remains `emit_gc_alloc_runtime`. The new metadata path now has paired admission,
private native preparation and paired commit, but these entries are invoked by
conformance owners rather than ordinary compiler startup. Its next connection
must provide the nonmoving heap/context/registry ownership that these entries
consume, along with cleanup and a running collector. Preparing metadata alone
does not make a new heap usable by generated language code.

Replace allocation lowering together with root ownership, publication and
matching access barriers, then test actual compiler-produced payload sizes.
Preserve a single consistent ABI per execution: do not route a legacy
HeapPointer into the new offset-based helpers. Builtin shapes alone do not
cover every compiler allocation.

The native metadata map/unmap helpers now have three target implementations and
conformance-owner wiring. That work supplies backing operations; it does not
supply the heap/context/descriptor/root startup needed by ordinary programs.
A startup path must also own attached mutators, page metadata, mark queues,
phase coordination and cleanup. Standard output cannot claim Generational ZGC
until those paths and the remaining G01–G15 requirements are connected and run.

Scheduler ownership remains a separate missing integration requirement.
`gc_cycle_step_locked` only records requested/started/completed tickets; it
does not run a cycle. `gc_mark_phase_step_locked` coordinates mark rendezvous
but requires the caller to prepare epochs, ensure color reuse safety and flip
Young remembered sets. Neither supplies adaptive initiation, worker/tenuring
adjustment or the full-cycle completion proof required before finishing a
ticket. Those Task 11/G12 requirements remain open in the current runtime.

## Typed compiler shape boundary

`gc_shape::BuiltinShape` is now shared by allocation lowering and generational
builtin preparation. The shared legacy allocation helper accepts this shape
instead of an arbitrary numeric tag. AArch64 record construction, cons cells
and list-reversal cells pass PointerRecord explicitly; scalar boxing passes
RawBytes. Descriptor preparation derives its fields from the same shape. The
legacy-tag conversion remains explicit and does not convert references.

The AArch64 record/cons lowering boxes scalar fields before constructing its
all-reference payload. The x86 `compile_record_literal` path instead produces
StaticRecord/RuntimeRecord labels via its record interners. Thus an ARM heap
record migration cannot be copied onto x86 by assuming identical allocation
sites. Closure and other compound representations still require their own
source-level inventory and root/publication migration.

`AllocationShape` now resolves `(descriptor_id, logical_payload_bytes)` against
the registered shape into an AllocationRequest, preserving unrelated request
fields and rejecting incompatible payload sizes. The OS owner uses it before
root reservation and AllocationInto. Compiler lowering must still supply the
new context/root/page ownership and publish initialized objects explicitly.

Failed allocation may abandon a reserved, still-null output with
`gc_root_cancel_empty` (shared emitted `root_cancel_empty`). This authenticates
owner, scope and generation and rejects nonempty or stale handles. Cancellation
only clears owner-private active metadata: it does not shrink the published
scan range, reuse the slot, advance root_revision or require journal capacity.
Scope retirement still reclaims the slot. Retaining the empty reservation
across refill is also valid; lowering must not cancel a published non-null root.
The C and x86 emitted cases check 20 pressure/rejection states, including full
journal and exhausted revision. The OS allocation pipeline abandons two refill
outputs and later scans all six reserved slots (two null), draining only the
four actual publication journal entries. ARM instruction emulation independently constructs the ABI fixture and checks
all bytes for the same 20 cancellation outcomes, including unchanged query,
journal, revision and scan bound. This verifies emitted ARM instructions;
native Darwin scheduling and hardware ordering remain unverified.

The eight x86 allocation calls for raw objects, pointer records and GC string
operations now use `emit_gc_alloc_dynamic` with BuiltinShape. Checked dynamic
byte counts remain in the allocation argument register; constant-size shared
allocation delegates to the same entry. The compiler no longer supplies raw
numeric tags at these call sites. The common entry still explicitly converts
the shape to a legacy tag, so this change does not mix object ABIs or activate
the generational collector. Migrating this entry requires the root, context,
initialization and field-access changes described above.

AArch64's register-preserving allocation trampoline callers (heap strings and
lowered `__gc_alloc` / `__gc_record`) also supply BuiltinShape through the shared
dynamic entry. Raw `__gc_alloc` now passes its logical byte count unchanged;
block alignment remains the allocator's responsibility. The legacy trace still
recovers rounded payload capacity from its 16-byte header. Consequently it
cannot substitute the new descriptor scanner, which requires logical payload
length, without the coordinated header/allocation/access migration.

The AArch64 internal allocation lowering now rejects negative counts and counts
above the shared payload/pointer-slot limits before multiplication or block
alignment. This closes a wraparound path where a huge pointer count could
become a small allocation. Rejection emits a diagnostic and exits with status
1; accepted counts remain unchanged until shape-specific conversion.
`aarch64::tests::gc_allocation_size_lowering_fixtures` builds 20 actual lowered
Mach-O programs (both shapes, negative extremes, zero, small unaligned sizes,
and overflow boundaries), and executes them on a macOS AArch64 test host.
Set `KLASSIC_ARM_ALLOCATION_FIXTURES` to export the binaries and `cases.tsv` for
instruction emulation with `tools/run-macho-aarch64.py`. All 20 exported cases
passed that syscall model on Linux; this is not native Darwin execution or a
completed generational OOM policy.

`gc_native_roots.h` now prepares a nonmoving native root-segment package with
cells and its publication journal in one budgeted mapping. Checked sizing and
mapping happen before registration, outside registry locks and source guards;
an attached owner enters native state for preparation. The charge is committed
before the segment is attached. The OS owner pipeline uses this storage instead
of stack arrays, including empty-root cancellation, concurrent marking, leased
root scanning and journal retirement. It verifies invalid sizing/owner and a
map-failure retry preserve the pending reservation. Storage is unmapped and its
charge released only after the worker joins and root/journal leases retire.
This supplies root backing/lifetime integration for the conformance owner;
standard compiler startup and dynamic root-segment refill remain unconnected.

The OS owner now exercises root refill during allocation: each native segment
has three cells and two journal entries, so the second allocation round first
observes ROOT_FULL. It prepares and commits a successor mapping in native
state, registers a fresh scope, and allocates through that scope while retaining
the old segment. An existing non-null handle is read before and after refill
and must retain its value. Both segments then complete independent leased scans,
journal drains, retirement and mapping release. This is conformance-owner
integration, not a production compiler root-stack refill implementation.

Each native root package also owns its RootRegistration node. Registry and
owner-list links therefore point into budgeted nonmoving storage, not the
conformance caller's stack. Preparation initializes the private node and its
cleanup cursor before registration. Both initial attachment and refill use
these embedded nodes, retaining them through root/journal leases and unlinking
them before releasing the mapping. Temporary lease/request objects still belong
to the operation owner and must outlive their respective jobs.

Root package initialization is now a shared C/direct entry,
`gc_root_storage_prepare` / emitted `root_storage_prepare`, using the checked
RootStorage and RootStorageSpec ABI. OS preparation supplies a fresh zeroed
mapping; the helper validates owner, counts, extent and address arithmetic
before writing the private registration/segment/journal layout. It initializes
cell atomics outside registry locks and stopped intervals. Registration and
publication are separate, and the mapping cannot be reused while readers exist.
The C OS adapter invokes the selected initializer; the x86 conformance owner
uses actual emitted instructions for both initial backing and refill. Shared
C/x86 tests cover 21 complete-memory outcomes and ARM emulation independently
checks the same layouts, canaries and unchanged rejection states. ABI layout
checks pass; Darwin hardware execution remains outstanding.

Native root bootstrap failure coverage includes six map/initializer/disposal
scenarios using both C and emitted initialization/budget/native mapping calls.
The initializer failure is injected after private header writes to check that
cleanup does not assume untouched backing. Failed disposal preserves the mapping,
registration bytes and pending or committed budget; ABORT/RELEASE is rejected
until disposal succeeds. Successful private preparation may also be abandoned
before registration. These checks cover resource ownership, not a complete
production allocation-stall/OOM policy.

Native root disposal now uses `gc_native_roots_dispose`, which rejects a package
with a registered scope, registry/owner links, outstanding claims/leases,
unfinished cleanup or undrained journal cursors before attempting an OS unmap.
The caller remains the exclusive backing owner and must exclude concurrent
attachment; validation alone is not a synchronization protocol. The OS pipeline
checks rejection in ACTIVE, RETIRING and CLEANING, then succeeds only after
FINISH unlinks the registration. Existing partial-initialization and failed
unmap retry cases use this entry as well, retaining charges on cleanup failure.
The fixed-prefix predicate is shared between C and direct code; the complete
direct owner orchestration remains to be emitted.

`gc_root_storage_can_dispose` / emitted `root_storage_can_dispose` now supplies
that read-only predicate to the native disposal adapter. The x86 OS owner and
failure/retry cases call the generated predicate before generated native unmap.
C/x86 and independent ARM fixtures cover 32 private, drained, malformed and
outstanding-obligation states, preserving all storage. No whole-root scan or
OS call occurs inside the predicate; caller exclusivity remains required.

Linux x86 now has a composed direct `native_roots_prepare` entry: it validates a
reserved MetadataCharge and matching RootStorageSpec, invokes its embedded
mmap helper, records mapping ownership, and calls shared root initialization.
This entry needs no C callback or external linker. It leaves budget commit and
root registration to the owner, and preserves the charge/spec on validation or
OS-map failure. The Linux OS pipeline selects it for normal initial/refill
preparation. Three real mapped sizes, duplicate preparation, 18 invalid inputs
and an actual mmap ENOMEM are checked; release still follows the existing
retirement and budget protocol. Windows and Darwin composed preparation and
ordinary compiler startup are not yet connected to this entry.

The composed root-preparation control flow is now shared PortableAsm emission
with a target-provided mapping entry. Linux retains its 4 KiB syscall adapter;
Darwin AArch64 uses 16 KiB libSystem mapping with AAPCS64 argument normalization.
The Darwin conformance owner binds this entry from the same relocatable payload
as its map/unmap helpers, so normal initial/refill preparation stays inside
emitted code. ARM emulation executes both the Mach-O entry and relocated payload:
each covers three mapped sizes, duplicate preparation, 18 rejected inputs and
map failure, including volatile-register clobber and callee-saved preservation.
Native Darwin execution and Windows composed preparation remain outstanding.

Win64 composed root preparation now wraps the same portable core, preserving
rdi/rsi and aligning private calls before the VirtualAlloc bridge supplies the
required Win64 shadow space. The locally relocated metadata payload exports this
entry, and the Windows OS owner selects it for initial/refill preparation.
The existing Win64 worker/API model executes the exact payload, including three
mapped sizes, duplicate preparation, 18 preflight rejections and native mapping
failure; model calls also probe rdi/rsi preservation. Linux execution of this
API model does not establish Windows OS behavior. All three target adapters are
wired in conformance owners; ordinary compiler activation remains outstanding.

Composed `native_roots_dispose` now validates the charge and shared fixed-prefix
retirement predicate, calls the target OS unmap entry, and clears allocation
ownership only on success. Linux, Win64 and Darwin conformance owners select
this emitted entry after root retirement; budget RELEASE/ABORT remains a
separate operation. Invalid charges, live registrations and duplicate disposal
preserve ownership. Win64 and ARM API models additionally inject unmap failure,
check unchanged mapping/charge contents, and retry successfully. Both reserved
and committed charge stages are exercised across these fixtures. The caller
must still exclude concurrent attachment and disposal.

Validation: Linux emitted-code suite (1 test), Win64 POSIX API-model suite
(4 tests), and ARM emulation (640 helper calls, 1536 shared-context transitions)
passed. ARM checks both Mach-O and relocated metadata payload entries. These
results do not establish Windows/Darwin OS execution or activate the ordinary
compiler's generational collector.
The required C ASan+UBSan/TSan integration test also passed after this change
(`cargo test --test llvm_gc`: 1 test); workspace Clippy, formatting and access
inventory checks passed. This sanitizer evidence covers C code, not emitted
machine-code memory ordering.

Descriptor bootstrap is now also composed direct code. Shared PortableAsm
`native_builtin_registry::emit_prepare` checks the reserved charge and registry
capacity, obtains zeroed native backing, records ownership, initializes the
builtin package and registers IDs 1/2/4 through embedded shared helpers. It
returns before budget commit or publication. A failure after mapping leaves the
private allocation owned for cleanup; OS-map failure leaves the charge intact.
The target adapters use Linux mmap, Win64 VirtualAlloc or Darwin mmap, and all
three conformance owners select this entry for normal descriptor startup.
The old callback-based C path remains the sanitizer/reference implementation
and retains its injected partial-registration failure tests.

Actual Linux execution and the Win64 API model verify three capacities
(5/64/1024), exact descriptor/bitmap/slot contents, untouched padding, duplicate
preparation, 14 invalid inputs and mapping failure. Independent ARM expectations
check the complete mapped bytes for both Mach-O and relocated payload entries;
the expanded emulator run passed 688 helper calls and 1536 context transitions.
These are startup building blocks: ordinary compiler startup, allocation/root
lowering and the complete collector cycle still need integration. Windows and
Darwin execution evidence is still limited to models on this host.
After descriptor bootstrap integration, `cargo test --workspace` passed 796
tests across 25 invocations (0 failures, 0 ignored), including required C
sanitizers. Workspace Clippy, formatting and access-inventory checks also passed.

`AllocationStart` now provides a single finite allocation entry shared by C and
PortableAsm. It resolves the logical descriptor shape, reserves a fresh empty
output root and calls AllocationInto. A failed attempt cancels only the newly
reserved cell's active flag; its generation and published scan bound remain
advanced. This returns explicit ROOT_FULL for segment growth and preserves
refill, metadata-pressure, journal-pressure and exhaustion statuses. A retry
needs a fresh slot. Descriptor/elements can be resolved even if a later step
fails; other allocation state remains unchanged on failure. Existing handles
must be retained separately before reusing the output RootRequest.

The OS allocation owner now uses this entry for its two failed refill attempts
and four successful allocations, including zero-byte and mixed-field objects.
Success still returns a rooted INITIALIZING object; payload initialization,
ready publication and slow-path refill remain separate compiler obligations.
There is no poll, lock, native allocation or whole-root walk in this entry.

Validation covers 120 C/candidate whole-state cases (all valid phases, successful
reservation, malformed inputs and each pressure/cleanup outcome), plus four
null/misaligned entry arguments. Linux executes the emitted helper and real OS
pipeline. ARM replays all 120 C state vectors against the actual emitted helper;
this is instruction-level parity evidence, not native Darwin execution. The
Rust/C ABI layout suite includes AllocationStart (8 tests passed). The ordinary
compiler remains on the legacy ABI pending complete startup/root/access wiring.
The required ASan+UBSan/TSan integration test passed with the new allocation
entry included in both sanitizer runs (`cargo test --test llvm_gc`: 1 test).
Workspace Clippy, formatting and access-inventory checks also passed.

Guarded initialization now has a bounded word-store entry in C and shared
PortableAsm (`gc_guarded_initialization_store`, `initialization::emit_store`).
It reuses the readiness publisher's ownership checks, requires a guarded Young
INITIALIZING object on the allocating owner's page, and validates scalar vs
reference kind against a one-slot descriptor scan. Reference values must be
canonical/store-good and independently strongly rooted through publication and
mark handoff; those lifetime obligations remain with the caller. Only null
reference slots may be initialized. Rewriting a non-null reference requires the
normal deletion barrier. Private scalar slots can be rewritten, including with
bits that resemble managed references. No lease/guard is transferred and no
ready state is published by this entry. Byte/bulk initialization remains open.

The OS pipeline's mixed-record construction now calls this generated/C entry
for all four words, then separately publishes readiness. Its repeated reference
writes are rejected. The shared suite checks 1,308 full-state transitions over
all tested epoch/phase combinations, including null/non-null references,
reference-shaped scalars, duplicate reference writes, wrong descriptor kinds,
invalid ownership and published receivers; four null/misaligned arguments are
also rejected. Linux generated-code execution and ARM replay of all 1,308 state
vectors passed, along with the ABI suite (8 tests), workspace Clippy, formatting
and inventory checks. This does not complete public initialization orchestration
or ordinary compiler activation; root resolution, slow paths and complete
publication/mark handoff remain required.
The required C ASan+UBSan/TSan integration test also passed with the typed
initialization cases and mixed-record pipeline in both sanitizer builds
(`cargo test --test llvm_gc`: 1 test).

Scalar byte initialization now has a bounded C/shared-emitter entry
(`gc_guarded_initialization_copy`, `initialization::emit_copy`). Each call copies
0..256 bytes from immutable live native storage, validates the entire target
range against logical payload length, and rejects descriptors with any fixed or
element reference bitmap. The source must be disjoint from the destination and
runtime controls; this API does not authorize retaining managed raw addresses
across safepoints. Zero-length chunks may have a null source. Failure changes
nothing; readiness publication and guard/lease release remain explicit.

The OS allocation pipeline initializes its 131,072-byte scalar allocation in
256-byte chunks, releases the access guard and yields after every chunk, then
reacquires before continuing. Its zero-payload allocation follows the same
entry. Only rooted references, immutable native source bytes and numeric
offsets/lengths survive those boundaries. Mixed records continue using the
separate descriptor-checked word initializer; reference copies/overwrites must
use their own rooted barrier protocol.

Validation: 796 C/candidate full-state cases exercise every length 0..256 at
three offsets, zero length at the payload end, invalid ownership/descriptors,
source arithmetic overflow and range/size rejection. Four null/misaligned entry
arguments are also rejected. Linux emitted execution and ARM replay of all 796
state vectors passed; ABI layout checks (8 tests), workspace Clippy, formatting
and inventory checks passed. Ordinary compiler activation and native
Windows/Darwin execution remain outstanding.
After chunked byte initialization was connected, `cargo test --workspace`
passed 796 tests across 25 invocations (0 failed, 0 ignored), including the
required ASan+UBSan/TSan C integration and the emitted OS allocation pipeline.

The OS initialization owner now acquires its initial receiver guard through
`RootAccess`, transferring the returned canonical scope and metadata lease to
its private initialization controls. At every scalar-copy chunk boundary it
exits the guard, releases the lease, clears the old scope/prepare envelope and
yields with the registered root still live. The next chunk obtains a new scope
and lease from that root, rebuilding its phase snapshot from the guarded owner
context. It no longer re-enters a cached cell with metadata ownership retained
across the chunk boundary. Live lease-capacity growth remains outside guards.

At the first boundary of each scalar allocation the conformance owner fills all
three metadata lease slots, checks RootAccess returns METADATA_FULL without a
guard or request mutation, releases that pressure and retries from the same
root. This exercises cleanup/retry with both zero-byte and multi-chunk objects.
The owner serializes heap/lease metadata with its existing worker relay; this
is still conformance orchestration, not emitted ordinary compiler startup.
Forwarding/phase adoption across real compiler safepoints remains a separate
integration requirement; these allocating Young receivers stay on their owned
pages during initialization.
Validation after root-based chunk reacquisition: Linux emitted conformance
(1 test) and required C ASan+UBSan/TSan integration (1 test) passed. The new
lease-pressure pipeline marker is mandatory in the emitted harness and both
sanitizer builds. Workspace Clippy, strict C syntax, formatting and inventory
checks passed. No new native Windows/Darwin execution evidence was obtained.

Published scalar fields now have a finite acquire-load/release-store entry
(`gc_guarded_scalar`, shared `barrier::emit_guarded_scalar`). It requires the
existing canonical source guard, validates ownership/phase and logical bounds,
and rejects a descriptor-selected reference slot. Loads change only the native
query value; stores change only the payload word. Scalar bit patterns never
enter marking/root/barrier buffers. The OS mixed-record pipeline reads and
updates its scalar words through this entry, preserving its reference fields.
No new language surface or ordinary compiler activation is implied.

Descriptor scans may skip a scalar prefix and report a later reference. Slot
classification now compares that reported position with the requested position,
rather than treating any reported reference as belonging to the requested slot.
The same correction applies to guarded initialization. The prior ARM initializer
fixture reproduces rejection at new case 1308 (a scalar list length before its
pointer array); the corrected emitter passes all 1310 initialization vectors.

Validation: 575 C/candidate scalar full-state cases cover Young/Old, valid
phases, exact bit patterns, scalar list prefixes, source claims with a prior
entrant, and malformed/non-scalar access rejection. Linux emitted execution
(including the OS mixed-record pipeline), ARM replay of 575 scalar and 1310
initialization cases, the ABI suite (8 tests), workspace Clippy, formatting and
inventory checks passed. Scalar operations retain the caller's guard and
metadata-lifetime obligations; native OS/hardware-ordering and full compiler
integration gates remain open.
Required C ASan+UBSan/TSan integration passed after scalar access and prefix
classification changes (`cargo test --test llvm_gc`: 1 test); both sanitizer
builds must report the new scalar and expanded initialization suite markers.

The existing four-actor forwarding race now routes source-user word accesses
through the scalar emitter/C entry. In 32 of its 64 rounds, two users enter
before source selection, wait at a controlled preemption point, then acquire-
load/release-store separate scalar words after admission is closed. Their
pointer-shaped integer values remain scalar: no SATB/remset/root revision
changes occur. Two competing copiers must wait for both source guards to exit;
a direct copy probe also returns WAIT with an unchanged request while they are
held. Complete destination bytes are compared against the updated source after
forwarding publication, and retained-source rounds preserve the same updates.
Short/empty payload rounds retain the previous byte-boundary coverage.

Linux emitted-code conformance passed with the new mandatory 32-round scalar
relocation marker. This supplies actual OS-thread integration evidence for the
scalar store/source-drain/copy ordering path; it does not establish complete
compiler barrier coverage or native Windows/Darwin hardware ordering.
The required C ASan+UBSan/TSan integration also passed with this scalar race
mandatory in both builds (`cargo test --test llvm_gc`: 1 test). Workspace Clippy,
strict C syntax, formatting and access-inventory checks passed.

The pool-backed field/root pipeline now combines RootAccess and scalar access
on a moved mixed record. Its original receiver page is protected before the
operation starts. In 64 rounds the receiver root advances to the forwarding
destination, crosses a native-state interval without a guard/lease, and then
acquires the destination guard to read and update its scalar field. The test
checks the original ObjectId/MarkRecord and reference field remain intact, and
rejects using the scalar entry to overwrite that reference field.

Half the rounds first fill the root journal. RootAccess must return FLUSH with
its request and original root unchanged, no guard and no metadata lease; after
journal consumption it retries from that same root. Root and metadata cleanup
finish before the existing referent relocation/mark pipeline continues. This
is real protected-page execution of the emitted leaves on Linux, still owned
by a conformance scheduler rather than the ordinary compiler's runtime.

Validation of this rooted scalar path: `cargo test --workspace` completed with
796 passed, zero failed and zero ignored across 25 test invocations, including
the required C ASan+UBSan/TSan integration. The focused Linux emitted-code test
and workspace Clippy also passed. These results cover the conformance-owned
path; they do not close ordinary compiler startup/access integration, actual
Windows/Darwin execution or the G01-G15 performance gates.

Native startup metadata C adapters now reject a misaligned MetadataCharge
before reading its fields, matching the direct preparation entries. The check
also covers descriptor/root preparation and metadata/root disposal. A minimal
reproducer using the previous validator fails UBSan on the first charge member
access. Regression cases require INVALID with the original charge unchanged;
root preparation additionally verifies no mapping or initializer call occurs.
Validation: focused Linux emitted-code execution and required C ASan+UBSan/TSan
integration passed, along with strict C syntax, formatting and inventory checks.

Startup metadata now has a C admission owner entry,
`gc_native_startup_reserve_locked`: it sizes the initial root package and
builtin descriptor registry, reserves their combined rounded charge under the
heap lock, and transfers that private reservation into two independent charges.
Invalid input, overflow or insufficient combined budget leaves both outputs
and the heap unchanged. No mapping, callback, traversal or publication occurs
under this lock. Each package still requires preparation outside locks/guards
and budget commit before publication; failure retains its charge until disposal
and abort succeed.

The OS allocation pipeline uses this entry before preparing its initial root
package, so descriptor budget exhaustion cannot occur after that root package
has been mapped and attached. Root growth remains separately charged. Sixteen
invalid/pressure cases include a one-byte budget deficit and rounded-size sum
overflow; four independent commit/abort combinations preserve unrelated pending
charges. The Linux pipeline then maps, uses and retires both real packages.
This owner entry is C code used by conformance owners, not an emitted direct
startup path or activation of the ordinary compiler's generational runtime.
Validation: the focused Linux emitted-code integration and C ASan+UBSan/TSan
integration passed; the new admission marker is required in both sanitizer
builds. Workspace Clippy, strict C syntax, formatting and inventory checks pass.

Paired admission now also has the `NativeStartupRequest` ABI and shared
`native_startup::emit_reserve` entry. The Linux OS pipeline invokes generated
instructions for this admission step before its native root/descriptor mapping
entries. The emitter validates empty charges and count/rounding arithmetic,
uses the shared budget leaf for the combined reservation, then transfers the
reservation to the two caller-owned charges. The request stays immutable.
It does not call native allocators or publish either package.

Linux generated execution and the eight ABI tests pass. ARM emulation passes
99 independently specified full-memory cases covering 8/4096/16384-byte
rounding, maximum counts, sum overflow, invalid pointers and one-byte budget
deficits; the fixture exports 99 helpers. These checks do not supply Darwin
hardware execution or complete ordinary compiler startup ownership.
The C ASan+UBSan/TSan integration, workspace Clippy, formatting and refreshed
access inventory checks also pass after the ABI and owner wiring changes.
The subsequent full `cargo test --workspace` run completed with 796 passed,
zero failed and zero ignored across 25 invocations, including sanitizer
integration and both-ISA lowering. Native Windows/Darwin execution and the
ordinary compiler migration remain separate, unfulfilled gates.

The OS startup owner now prepares both root and descriptor mappings privately,
then commits both charges before attaching the first root registration. A new
failure path prepares a real root mapping through the selected native entry,
injects descriptor-map failure, and then injects failure of root disposal.
Neither failure publishes a root registration. The failed unmap preserves the
entire root prefix, charge and heap budget; aborting that still-mapped charge
is rejected. The unmapped descriptor reservation is independently aborted,
then root disposal is retried through the selected native entry and its charge
aborted. The complete HeapPool equals its pre-admission state before startup
is attempted successfully again.

The failures are injected through C NativeMetadataOps; on Linux the successful
root prepare/dispose, admission and budget leaves execute generated code.
This is conformance-owner startup ordering and rollback evidence, not a new
ordinary compiler startup implementation.
Validation: Linux emitted integration and required C ASan+UBSan/TSan integration
pass with the rollback marker mandatory in both sanitizer builds. Strict C
syntax, formatting, whitespace and access inventory checks also pass.

Private mapping preparation is now a runtime owner entry,
`gc_native_startup_prepare`, consumed by the OS startup pipeline. It preflights
both reserved charges, common heap ownership, target quantum, exact rounded
sizes and required callbacks before allocating anything. It prepares roots
then descriptors through native generated leaves where supplied, otherwise
through the C adapters. No commit or registration occurs in this function.
Any partial allocation stays recorded in its charge on failure; the caller
must dispose and abort before a fresh preparation attempt.

Twenty-one rejected configurations preserve charges and request bytes. First-
mapping failure preserves both reservations, while second-mapping failure
retains the private root mapping for the existing disposal-retry path. The
orchestration entry itself is C code; its use by conformance owners and calls
to generated leaves do not activate ordinary compiler startup.
Validation: Linux emitted integration and C ASan+UBSan/TSan integration pass,
including the required 21-case preparation marker in both sanitizer builds.
Strict C syntax, formatting, whitespace and inventory checks pass.

Private preparation now has a shared direct emitter and Linux SysV, Win64 and
Darwin AAPCS64 native entries. It preflights both charges and exact rounded
sizes, then calls embedded root and builtin-registry preparation leaves with
target-native mapping. No C callback is needed for the composed operation.
The Linux conformance startup owner now invokes this emitted entry; commit and
registration remain separate. Failed mapping retains the same partial charge
ownership as the C entry. This still does not wire ordinary program startup.

The Win64 relocated metadata payload includes the entry and passes actual
emitted instructions against the explicit POSIX API model, including first-
and second-map failure with independent budget aborts (4 Windows helper tests
passed). ARM verifies both Mach-O and relocated metadata entries against an
OS model supporting two simultaneous mappings: complete root/descriptor bytes,
19 preflight rejections per entry, both mapping failures and retained ownership
through failed disposal and retry. The fixture exports 100 helpers; the complete
ARM run executes 885 helper calls and 1536 context transitions. These remain
instruction/API-model results, not native Windows/Darwin scheduling evidence.

Focused Linux emitted execution, required C ASan+UBSan/TSan integration,
workspace Clippy, strict C syntax, formatting and refreshed inventory checks
pass. Ordinary compiler activation and the remaining G01-G15 gates stay open.

Startup budget commit now has a paired C/shared-emitter entry. Under the heap
lock it validates the heap accounting and both prepared charges, including
their common owner, reserved stage, mapping ranges and combined pending bytes,
before changing any state. Failure preserves the heap and both charges;
success moves their sum from pending to committed and advances both stages.
The OS startup pipeline uses this entry before root registration. It performs
no mapping, traversal or publication and does not itself prove initialization
or lifetime safety; those remain the unique owner's obligations.

Twenty-nine C/candidate cases compare complete heap, charge and request bytes,
including a budget that fits either charge alone but not both, duplicate
commit, malformed pointers and a valid commit reaching UINT64_MAX. Independent
ARM full-memory expectations cover the same accounting boundary; the fixture
now exports 101 helpers. Linux generated execution and ARM emulation pass.
Required C ASan+UBSan/TSan integration also passes with the paired-commit marker
mandatory in both builds. Workspace Clippy, strict C syntax, formatting and
inventory checks pass. Ordinary compiler integration is still incomplete.
After the composed preparation and paired commit changes, a full
`cargo test --workspace` run passes: 796 tests, zero failures and zero ignored
across 25 invocations. This includes the native helper execution/model tests,
both-ISA lowering and required sanitizer integration, without changing the
remaining native-OS, scheduler, compiler-integration or performance gates.

Generational TLAB success now release-publishes cumulative `allocated_bytes`
in the owning MutatorContext. The count includes header and alignment padding
(including the reserved minimum payload granule), survives TLAB close/refill,
and excludes failed allocations. It measures successful mutator block demand,
not live bytes, promotion or relocation work. Addition saturates at UINT64_MAX
without failing allocation; readers must treat that sentinel as an unavailable
rate sample, not subtract it to report a zero rate. The sole writer never
resets the counter while registered; retained-context observers use acquire
loads. This is an input for future scheduling, not an adaptive controller.

The added word makes MutatorContext 280 bytes. C and Rust ABI versions advance
to 2, and context checks reject version 1 before accessing the larger layout.
Allocation accounting is implemented in C and the shared TLAB emitter. Existing
TLAB full-state cases now check the counter and include below/exact/above-limit
and already-saturated values. The OS pipeline's worker observes monotonic
samples during allocation, then sees exactly 131280 allocated bytes after four
successful blocks and two failed refill attempts; closing both TLABs preserves
that total. Normal compiler programs still use the legacy allocator.

Focused Linux emitted execution, ABI layout tests (8), ARM replay of 2804 TLAB
and 120 allocation-entry vectors, required C ASan+UBSan/TSan integration and
workspace Clippy pass. The ARM context checks exercise both the rejected old
version and current version across 1536 transitions.
The subsequent full `cargo test --workspace` run passes 796 tests across 25
invocations, with zero failures or ignored tests. Formatting, whitespace and
the refreshed access inventory also pass after the context ABI extension.

`AllocationSample` now binds an observer's baseline to a retained context and
its immutable identity. C and the shared `allocation_sample` emitter accept a
nearby monotonic timestamp supplied by the caller. The first observation returns
BASELINE; later successful observations return positive elapsed nanoseconds and
the exact cumulative-byte difference. Equal timestamps return WAIT; reversed
time, changed identity/context or decreasing counters return INVALID. Saturated
counters return SATURATED, not a zero-rate sample. These three unsuccessful
statuses preserve the previous query; the context is always read-only.

The finite sampler performs no clock call, division, allocation or managed
access. Its native context borrow and unique query ownership remain caller
obligations. The OS allocation worker uses actual CLOCK_MONOTONIC or QPC-derived
timestamps before and after its retained-context observation window and checks
the complete 131280-byte difference. This supplies rate-estimation inputs;
timestamp/counter observations are not an exact event timeline or pause metric,
and adaptive scheduling remains unimplemented.

Twenty-five C/candidate cases and independently specified ARM full-memory cases
cover baseline, genuine zero allocation, equal/reversed time, identity mismatch,
old ABI, saturation and full-width differences. Linux emitted integration,
the eight ABI tests and ARM emulation pass; the fixture exports 102 helpers.
Required C ASan+UBSan/TSan integration passes with both sampling markers
mandatory in both builds. Workspace Clippy, strict C syntax, formatting,
whitespace and inventory checks also pass.

Allocation pressure now has a C/shared-emitter forecast entry. For a successful,
fresh sample it compares `delta_bytes * horizon_ns` with
`headroom_bytes * elapsed_ns` using exact 128-bit products. The C implementation
uses 32-bit limbs and the emitters use wide unsigned multiplication; neither
rounds rates, divides or silently wraps a product. Invalid/baseline/saturated
samples are rejected. At equality, or with zero usable headroom, the forecast
requests attention. The function never dereferences the recorded context or
changes the sample.

The OS pipeline supplies its measured allocation window and heap headroom after
pending commitments and evacuation reserve. One-window and four-window horizons
exercise quiet/request outcomes; they are test inputs, not estimates of actual
collection duration. The request outcome is connected to the cycle ledger and
remains pending: the test does not START/FINISH a fictitious collection. This is
a single-mutator linear forecast, not an upper bound on future demand, aggregate
multi-mutator scheduling, or adaptive worker/capacity/tenuring control.

The 225 arithmetic cases use unbounded-integer expected outcomes, including
cross-limb boundaries and full-width operands; 11 malformed-sample cases verify
unchanged rejection. Linux emitted execution and 236 independently checked ARM
cases pass. The ARM fixture now exports 103 helpers.
Required C ASan+UBSan/TSan integration passes with both pressure markers
mandatory in both builds. Workspace Clippy, strict C syntax, formatting,
whitespace and inventory checks pass. Full adaptive scheduling, ordinary
compiler activation and the native-OS/performance gates remain open.

The OS mark worker conformance path now resumes mixed-layout jobs across two
scan slices, instead of requiring every scan to return DONE. It offers and
protects the first slice's child before advancing the cursor, releases the raw
access guard, yields its queue claim, polls, and takes a fresh claim. The old
claim cannot finish the reclaimed job. Old-generation overflow and a subsequent
metadata-full retry retain the parent cursor and child protection across that
boundary. The worker reacquires its access guard for the next scan.

Worker-owned counters, read after join, distinguish examined bitmap candidates
from reference slots loaded (including duplicate referents). Each generation
performs six slices, two queue yields, eight candidate examinations and four
reference-slot loads in this fixture. Scalar-only payload skipping contributes
zero examined/loaded slots, even for a page-sized object. Accounting happens
only after all children of the slice have transferred protection; metadata
retry does not count the same slice twice. These are exact conformance workload
counts, not production telemetry, throughput or a collection-duration estimate.
This closes a composed worker test gap, not the ordinary compiler or adaptive
scheduler integration. The sanitizer harness requires the new marker.
Linux emitted integration passes, and required C ASan+UBSan and TSan integration
passes with this composed path exercised. Strict C syntax, formatting,
whitespace and access-inventory checks also pass. No ARM-emulation or native
Windows/macOS execution is claimed for this new composed test.

The partial-scan metadata-pressure handoff now performs an actual rendezvous
while the scanner is native-blocked. The mutator closes its TLAB before ACK;
the coordinator advances the snapshot sequence, parks the remaining Running
owner, and ends the handshake without traversing heap or roots. The queue,
in-flight parent, overflow child and active metadata leases are unchanged by
that rendezvous. LEAVE_NATIVE adopts the published snapshot before the scanner
routes its protected second-slice result. A subsequent generation pass refreshes
its preparation snapshot instead of reusing the pre-rendezvous one.

Both rounds exercise this path (two additional handshake generations). Colors
and mark epochs remain unchanged: this is phase-sequence adoption with retained
work, not proof of remap/color reuse or a complete overlapping collection.
The C sanitizer suite requires the new phase-handoff marker. Ordinary compiler
activation, collector orchestration and the G01-G15 completion gates remain open.
Linux generated-code integration and required C ASan+UBSan/TSan integration pass
for this phase handoff. Strict C syntax, formatting, whitespace and inventory
checks pass; native Windows/macOS and performance verification remain pending.

`gc_mark_scan_commit_locked` and the shared `mark_scan_commit` emitter now own
the receipt-to-queue transition. After the caller protects/offers every returned
child and drops its raw guard, the entry validates receipt bounds and progress,
then commits YIELD or FINISH using the authenticated queue claim. Any queue
rejection (including serial exhaustion) restores the original request cursor;
success consumes the receipt's job pointer. DONE returns metadata protection in
the completed request; MORE transfers it back to the queued task. Reusing a
consumed receipt is rejected without changes. The entry neither dereferences
heap payload nor proves receipt provenance or child protection: the exclusive
worker must supply its latest successful scan and satisfy those obligations.

The OS worker now uses this common entry for both split parent scans and child
completion, including the metadata-pressure phase-adoption path. There are 128
C/candidate checks covering both generations, DONE/MORE, malformed receipts,
stale claims, serial exhaustion, unchanged rejection, and replay rejection.
ARM has 64 independently specified full-state cases; the fixture exports 104
helpers. Linux generated execution and ARM emulation pass, as do workspace
Clippy, strict C syntax, formatting and the refreshed access inventory. This
adds a collector building block; ordinary compiler activation and a complete
collector owner/scheduler remain unimplemented.
Required C ASan+UBSan/TSan integration also passes with the scan-commit marker
mandatory. Native Windows/macOS and performance gates remain unverified.

### Ordinary compiler root-lowering boundary (current source audit)

The next compiler integration must replace slot-address roots as one protocol,
not merely call `gc_root_new` beside the legacy shadow push:

| Backend | Current lowering anchors | Required connected change |
| --- | --- | --- |
| x86 ELF/PE | `register_heap_pointer_slot`, `root_initialized_heap_slot`, `emit_gc_shadow_push`, `pop_scope`, `emit_literal_root_seed` in `lib.rs` | Bind each managed local/argument/static cell to owned nonmoving roots, publish subsequent writes, load the current rooted value after polls, and retire roots before reclaiming their storage. Preserve argument staging and static-cell lifetime. |
| AArch64 | `emit_root_frame_slot`, `open_temp_roots`, `close_temp_roots`, `emit_shadow_pop` in `aarch64.rs` | Preserve expression-local lifetime boundaries and return values while replacing slot-address registration with the same root ownership/publication protocol. |
| LLVM | `push_root`, `close_scope`, record-construction staging in `llvm.rs` | Replace `klassic_gc_shadow_push(ptr alloca)` and direct slot mutation coherently with the new root API and allocation/publication path. |

Today x86 `allocate_slot` zeroes and registers a frame slot before its eventual
value store; incoming arguments use deferred registration after spilling. A
nonmoving RootCell would not observe those later ordinary frame stores by
itself. The current compiler also relies on the legacy collector rewriting the
stack slot in place. Therefore retaining direct frame loads after introducing
independent root cells would read stale references following a move. This audit
identifies a migration dependency, not a completed root-lowering change or a
new failure reproduced in the legacy collector.

After the scan-commit entry and composed worker changes, `cargo test --workspace`
passes all 796 tests across 25 invocations, with zero failures and zero ignored
tests (`/tmp/klassic-scan-commit-workspace.log` in the validation environment).
This includes the required C sanitizer integration and both-ISA lowering; host
API models and ARM emulation still do not substitute for native Windows/macOS
execution. The result validates the accumulated changes, not default activation.

The ordinary x86 ELF/PE `pop_scope` lowering now unregisters its roots before
raising RSP past their frame slots. Previously it freed those slots first and
then called the shadow-pop routine; CALL itself overwrote a still-registered
slot with its return address. An executable regression uses the actual
`pop_scope` lowering and observes every slot immediately before retirement:
the old order fails with one overwritten live slot, and the corrected order
passes scopes containing 1, 2 and 8 roots while outer-scope storage remains.
This reproduces the storage-lifetime window, not a collection-time crash in
the legacy single-threaded direct collector. The change prepares the ordinary
compiler boundary for concurrent root retirement; it does not introduce
RootHandles or certify scanner quiescence, and the default remains legacy.

After the scope-order fix, `cargo test --workspace` passes 797 tests across
25 invocations, with zero failed or ignored tests. Workspace Clippy, formatting,
whitespace and access-inventory checks pass. These results do not establish
concurrent root retirement or native Windows/macOS execution.

`gc_root_copy(ctx, source, destination)` / emitted `root_copy` supplies the
same-owner root-assignment boundary needed by compiler lowering. Both request
objects are immutable borrows (and may alias); their scratch `value` members
are ignored. Each invocation reads the current source cell and passes it to
the existing pre-mutation journal STORE. Null and self-assignment retain that
journal contract, including the destination's overwritten value and phase.
No managed address, allocation, poll or forwarding resolution is involved.

On FLUSH or rejection, requests, roots and journal remain unchanged. Callers
retain both handles while servicing pressure and retry the entire operation:
they must not preserve a sampled GcRef instead. Tests fill the journal, update
the source through a separate protected STORE after servicing pressure, then
check that the retry publishes the new source value and retains the old
destination in the journal. This entry is not a public canonical root-load
barrier, and cross-owner handles remain invalid.

Fifty C/candidate checks compare complete fixture/request state, including
aliasing requests, distinct requests for the same cell, null values, stale
handles, invalid owner/context/guard, malformed journals and exhausted counters.
ARM independently specifies 25 complete-memory outcomes. Linux emitted execution,
ARM emulation and both-ISA lowering pass; the fixture now exports 105 helpers.
The ordinary compiler still needs its root allocation, value access and scope
retirement lowered onto this protocol, along with the new collector owner.
Required C ASan+UBSan/TSan integration passes with the root-copy marker mandatory.
Workspace Clippy, strict C syntax, formatting, whitespace and the refreshed
9563-site access inventory pass. Native Windows/macOS and full collector
integration/performance gates remain open.

Root assignment is now composed with the OS-backed relocation pipeline in 64
rounds. The fixture fills the root journal using self-assignment, rejects a
source-to-destination copy with FLUSH, then moves both child objects and makes
the old payload page inaccessible. Another copy and a root-follow attempt
still return FLUSH without changing cells, journal or root revision. After
servicing the journal and advancing the source root, retry copies its new
canonical reference despite deliberately stale request scratch values.

The resulting journal entry retains the destination's previous B reference.
Using that protected journal value and forwarding metadata, the test reaches
B's destination under an access guard and verifies its unchanged ObjectId and
payload. Half the rounds replace B with A; the other half exercise the same
logical referent across different physical/color representations. Input request
objects stay unchanged throughout. This is a deterministic composition with
real protected mappings, not a concurrent scheduling proof, complete journal
consumer, or ordinary compiler activation. The sanitizer suite requires the
new relocation marker.
Linux emitted integration and required C ASan+UBSan/TSan integration pass with
this relocation composition exercised. Strict C syntax, formatting, whitespace
and the 9563-site inventory check pass. Native Windows/macOS execution and the
remaining full-collector and performance gates are still pending.

`gc_heap_allocation_pressure` / emitted `heap_allocation_pressure` now derives
forecast headroom from a coherent heap accounting snapshot. Its target is
`min(soft_max, hard_max - relocation_reserve)`, matching the mutator admission
reserve while allowing soft-limit pressure to initiate work earlier. Committed
plus pending bytes are checked before addition; reaching or exceeding the target
clamps headroom to zero. A collector using reserved capacity is valid pressure,
not an unsigned-underflow windfall. The cached pressure flag is ignored.

The entry delegates to the existing exact-product allocation forecast, retaining
its invalid/saturated-sample rejection. It never changes accounting, forbids an
allocation or turns the soft limit into OOM. Callers hold the heap lock or pass
an immutable snapshot. Metadata has its own budget, and the supplied sample and
horizon still do not constitute an aggregate adaptive controller.

The OS allocation pipeline now requests a pending Young ticket at the soft
target using its measured one-window horizon, while the hard-limit-only forecast
for that same horizon remains quiet. The ticket is not started or completed in
this fixture. Fifty-six C/candidate checks cover target/reserve ordering,
pending bytes, full-width arithmetic, malformed snapshots and input preservation.
ARM independently checks 1469 combinations and rejections. Linux generated
execution, ARM emulation, both-ISA lowering and workspace Clippy pass; the
fixture exports 106 helpers. Ordinary compiler activation, adaptive worker and
tenuring policy, full cycle ownership and the remaining G01-G15 gates stay open.
Required C ASan+UBSan/TSan integration also passes with both heap-pressure
markers mandatory. Strict C syntax, formatting, whitespace and the refreshed
inventory pass. Native Windows/macOS execution and performance remain unverified.

The OS owner now exercises a return-value lifetime boundary using independently
budgeted native root packages. It allocates and attaches a one-cell caller
segment, reserves its output root, and copies a live mixed object from an older
segment. Both original segments then complete their scanner and journal
obligations, clean and unlink, and are actually unmapped in native state while
the caller remains attached. Their metadata charges are released immediately.

After those source packages are gone, the caller root still yields the returned
reference. RootAccess reacquires its metadata lease and raw guard, a guarded
scalar load verifies the payload, and the object retains the allocator's same
MarkRecord with both generation marks. The caller subsequently retires its own
root, drains its separate publication journal and scanner lease, and unmaps its
package independently. This models the callee-to-caller protocol with actual
native storage lifetimes; ordinary generated-language function return lowering
has not yet been switched to it. No freed source request is dereferenced.

Linux generated execution passes with the new root-return marker. The sanitizer
harness requires that marker, ensuring the lifetime path runs in both builds.
Required C ASan+UBSan/TSan integration passes with the native-unmap return path.
Strict C syntax, formatting, whitespace and inventory checks also pass. This
validation does not establish complete G08/G13, ordinary compiler activation,
native Windows/macOS execution or the final performance gates.

`gc_guarded_compare_exchange` / emitted `guarded_logical_cas` now provides a
prepared logical-CAS commit step. Its caller must first root the observation
and logical expected value, resolve both to their unique nonmoving MarkRecords,
and retain those references and record lifetimes. The observed record must
belong to the exact observed bits in `GuardedStore.expected`; null uses no
record. This provenance is a caller obligation, not something the commit leaf
infers from pointer-shaped integers or debug ObjectIds.

Under the canonical receiver guard, the leaf validates the reference slot and
checks that its bits still equal the prepared observation. A changed slot
returns RETRY without logging. Different record identities return MISMATCH,
linearized at that acquire read, without requiring buffer capacity or emitting
SATB/remset records. Equal identities enter the existing pre-barrier and exact
physical CAS. Physical CAS failure still reports transferred buffer leases and
requires a fresh rooted observation; conservative logging is retained. A
record shared by relocated copies identifies the same object independently of
color or physical address, but records must not be reused while protected.

The 120 C/candidate cases cover null, differing observation colors, identical
and distinct records (including equal debug ObjectId numbers), pre-barrier
publication, full buffers, stale observations and invalid claims. Complete
fixture/record state is checked. ARM independently specifies 60 memory outcomes.
Linux generated execution, both-ISA lowering, ARM emulation and workspace
Clippy pass; the fixture now exports 107 helpers. The full public rooted CAS
API, its forwarding/mark preparation and retry loop, selectable language-level
memory ordering and ordinary compiler integration remain open; this entry alone
does not complete G05.
Required C ASan+UBSan/TSan integration passes with the logical-CAS commit marker
mandatory. Strict C syntax, formatting, whitespace and the refreshed 9580-site
inventory pass. Native Windows/macOS and performance verification remain open.

The protected-source load/relocation pipeline now composes the logical-CAS
commit with actual canonical identity resolution in 64 rounds. The observation
is rooted before relocation; the expected root is copied to the moved referent.
Both identities are read under canonical guards, and strong roots plus a retained
metadata lease protect the shared record while the receiver guard is acquired.
The receiver field still holds the original offset/color after both source pages
have been decommitted. Comparing the records permits replacing that field with
null despite the expected root's different offset/color. Reusing the stale
observation returns RETRY and preserves the request, roots and field.

Linux generated-code execution passes this composition. This is a controlled
relocation schedule exercising the prepared commit contract, not a public rooted
CAS retry API, a concurrent scheduling proof, or ordinary compiler activation.
Required C ASan+UBSan and TSan integration also passes, with the relocation-CAS
marker required from both builds. Strict C syntax, formatting, whitespace and
the 9580-site inventory checks pass.

Canonical identity preparation now uses `gc_guarded_identity` / emitted
`guarded_identity` rather than reading header metadata directly in the relocated
CAS composition. The finite leaf validates the object guard, page incarnation,
context state and adopted phase, published header, aligned nonnull MarkRecord
and nonzero diagnostic ID; it returns the borrowed record pointer or zero.
It does not compare diagnostic IDs. Null references bypass object access in the
outer barrier. The caller must retain the strong root and metadata lease across
guard exit and subsequent identity comparison; this operation does not acquire
or transfer either protection and must not outlive them.

The 64 C/candidate cases check both generations, selection/closed admission with
a live prior guard, malformed record metadata, stale context/page state and
invalid input pointers, with whole-fixture immutability checks. ARM specifies
32 independent full-memory outcomes. Linux generated execution, both-ISA
lowering and ARM emulation pass; fixtures export 108 helpers. The protected-source
relocation pipeline now obtains both CAS identities through this leaf. Full
rooted CAS retry/mark preparation, ordinary compiler integration and actual
Windows/macOS execution remain outstanding.
Required ASan+UBSan/TSan integration passes with the new identity marker required
from both builds. Workspace Clippy, strict C, formatting, whitespace and the
refreshed 9593-site access inventory checks pass.

The relocated logical-CAS composition now exercises fresh observation after a
physical RETRY with a deliberately full root journal. A failed guarded reload
leaves the previous output root and root revision intact. The owner exits the
receiver guard, releases its metadata lease, drains the journal, passes a native
boundary, reacquires the receiver from its root and reloads the now-null field
into the output root. The reload journals the overwritten canonical child.
The expected child remains strongly rooted separately and its record lease is
retained throughout. Comparing that child identity with the fresh null returns
MISMATCH; comparing null with null succeeds. Both paths leave lease ownership
balanced, and copying the retained expected root restores the subsequent mark
pipeline's input.

Linux generated execution passes all 64 protected-source rounds. This closes a
composition coverage gap for pressure during CAS retry, including explicit
rejection of reusing the previous observation's identity. It remains a controlled
single-owner schedule, not the public CAS controller, concurrent race proof or
compiler activation. A public retry controller must invalidate its observation
and all derived identity preparation whenever it retries, retain expected and
desired roots across pressure, and account for transferred barrier leases before
restarting. The identity leaf alone does not implement that controller.
Required ASan+UBSan/TSan integration also passes; both builds must emit the new
CAS retry marker. Strict C, formatting, whitespace and the refreshed 9593-site
inventory checks pass. No new claim is made about Windows/macOS hardware or
whole-collector completion.

`gc_guarded_observe_into` / emitted `guarded_observe` adds the exact observation
handoff needed by a rooted CAS controller. It shares guarded reference loading
and root-journal publication with `gc_guarded_load_into`; only after successful
publication does it return the sampled bits in `GuardedLoad.output.value`.
That scratch field must initially be zero. Failure preserves the entire request,
root and journal. The original load operation retains its unchanged-request API.
The sample is an exact physical comparison token, not a canonical reference or
permission to dereference managed memory. Its output root must remain live while
resolving identity; forwarding updates the root without changing the observation
token. Retry clears the scratch value and obtains a fresh rooted observation.
The output root must not alias receiver/expected/desired roots the controller
needs to preserve.

The protected-source relocation/CAS composition now obtains its physical
comparison token from this production helper instead of constructing it from
the fixture's allocation result. C/candidate testing covers 52 exact-bit/null,
color, full-journal, exhausted-revision and invalid-input cases with complete
state checks. ARM independently specifies 26 outcomes. Linux generated execution,
both-ISA lowering, ARM emulation and workspace Clippy pass; fixtures export 109
helpers. This supplies the observation handoff, not the public CAS controller,
mark preparation, language-level memory-order selection or compiler activation.
Required ASan+UBSan/TSan integration passes, with both builds required to emit
the rooted-observation marker. Strict C, formatting, whitespace and the refreshed
9598-site access inventory checks pass. Actual Windows/macOS execution and
whole-collector/performance gates remain outstanding.

Prepared logical CAS now provides SeqCst ordering, independently of GC logging.
In C, both exact-observation reads and the final successful/failed physical CAS
use `memory_order_seq_cst` when executing logical CAS; the ordinary physical
store path retains Acquire/Release ordering. In generated code, full fences
surround the logical operation, including its acquire-only MISMATCH path.
The existing primitive emits x86 MFENCE / AArch64 DMB ISH. This conservative
choice also orders the pre-barrier and preserves all existing status/lease
transfer semantics. No weaker selectable language-order interface is introduced.

A fixed-lifetime memory-order harness executes 1024 C/candidate rounds using two
OS threads. Each publishes an auxiliary SeqCst competing field value, then calls
logical CAS on the other field. Odd rounds exercise identity mismatch, even
rounds successful null/null comparison. The forbidden outcome is both actors
reporting that they observed the initial null: each store precedes its own
comparison, so that result would require a cycle in the SeqCst order. The fixture
uses direct auxiliary atomic writes and no collector; it tests ordering, not a
full mutator/store-barrier/relocation scenario. Linux execution, both-ISA lowering
and workspace Clippy pass. These observations are not a proof of all weak-memory
interleavings or evidence of native ARM execution. Public rooted CAS control,
compiler activation and the full G01-G15 contract remain outstanding.
ARM emulation retains all independent functional outcomes after adding fences.
Required ASan+UBSan/TSan integration passes, with the ordering marker required
from both builds. Strict C, formatting, whitespace and the refreshed 9600-site
inventory checks pass. Hardware ordering on ARM and Windows execution remain
unverified.

Paired startup commit now rejects overlapping root/descriptor backing ranges
before changing either budget or charge. Each range end is first validated for
overflow; half-open ranges may be adjacent in either address order. Distinct
charge controls are not proof of distinct backing ownership. The new regression
failed before this check because identical mapping addresses were accepted.
The C and shared emitted commit use the same rejection rule.

The startup commit matrix now has 35 C/candidate cases (70 invocations), adding
identical addresses, overlaps in both orders, an eight-byte overlap and both
adjacent arrangements. Every invalid case checks complete heap, charge and
request preservation. ARM independently specifies all 35 outcomes. Linux emitted
execution, both-ISA lowering and workspace Clippy pass. This validates private
startup accounting against malformed ownership inputs; it does not demonstrate
that the OS returned overlapping mappings or activate ordinary compiler startup.
Required ASan+UBSan/TSan integration passes with the updated 35-case marker
required from both builds. Strict C, formatting, whitespace and the refreshed
9600-site inventory checks pass. The last complete workspace checkpoint remains
797 tests before this focused ownership fix; this change has the focused checks
above rather than a new whole-workspace claim.

`gc_native_startup_release_locked` / emitted `native_startup_release` now returns
both private startup budgets as one validated transaction after successful
mapping disposal. Reserved and committed charges may be mixed; each subtracts
from its own accounting pool, and permanent page metadata plus unrelated budget
remain untouched. Both charges must belong to the heap, have nonzero sizes and
no remaining allocation, and be in stage 1 or 2. Aggregate capacity is checked
before either charge or counter changes. Success consumes both at stage 3;
replay is invalid. This operation neither unmaps memory nor proves retirement:
exclusive ownership and absence of published consumers remain caller duties.

The OS startup-failure pipeline now retains both reservations while root unmap
fails. Calling paired release with that mapping still owned rejects the whole
pair. After successful unmap outside locks/source guards and native reentry,
paired release restores the original accounting. It replaces the fixture's
separate descriptor abort followed by root abort; unrelated owners continue to
use the individual budget operations.

The C/candidate matrix covers 160 mixed-stage, live-mapping, stale-charge,
aggregate-underflow and malformed-input cases, checking complete state and
replay rejection. ARM independently specifies 80 memory outcomes. Linux emitted
execution, both-ISA lowering, ARM emulation and workspace Clippy pass; fixtures
export 110 helpers. This supplies startup cleanup accounting and its owner
connection, not a complete runtime owner, collector cycle or compiler activation.
Required ASan+UBSan/TSan integration passes with the new paired-release marker
required from both builds. Strict C, formatting, whitespace and the refreshed
9600-site access inventory checks pass. Ordinary compiler activation, native
Windows/macOS execution and the full-collector/performance gates remain open.

The native startup owner now also tests failure after both packages have been
prepared and committed, but before root attachment or descriptor publication.
It uses real OS mappings and the selected C/generated preparation, commit and
release entries. In each disposal order, unmap fails once for each package;
the mapping and both charges remain owned. Release rejects both a failed unmap
and the intermediate state with only one package unmapped, preserving the entire
heap accounting and both charges. Once both disposals succeed, paired release
returns exactly the committed bytes and rejects replay. All OS operations occur
outside guards in the registered owner's native state.

Normal live-runtime teardown still releases root segments independently after
their scanner/journal retirement and descriptors after all heap users; their
different lifetimes are not artificially coupled to private-startup rollback.
Linux generated execution passes both committed-cleanup orders. This is failure
matrix coverage of the existing ownership primitives, not new public runtime
startup, a collector cycle or compiler activation.
Required ASan+UBSan/TSan integration passes, requiring the committed-cleanup
marker from both builds. Strict C, formatting, whitespace and the 9600-site
inventory check pass. Native foreign-OS execution and full-collector gates
remain unverified.

`gc_root_access_cleanup` / emitted `root_access_cleanup` now supplies ordered
cleanup of a READY root access. EXIT runs outside the heap lock, checks the
access context/incarnation against its retained lease and delegates guarded exit.
WAIT preserves the live guard and requires immediate nonblocking retry; successful
exit clears the entire access scratch while retaining metadata ownership.
RELEASE runs under the heap lock, rejects nonempty access scratch or any active
context guard, then releases the retained token. Metadata rejection preserves
that obligation and returns CLEANUP_REQUIRED; successful release permits reuse
of the RootAccess control. The strong root stays owned throughout. Neither step
polls, acquires a lock, unmaps storage or accesses managed payload.

The moved-CAS retry pipeline now uses both steps when leaving/reacquiring its
receiver around journal pressure. C/candidate tests check 56 cases, including
release-before-exit rejection, stale cleanup claims, failed metadata release,
whole-state preservation and successful control reuse. ARM independently checks
28 full-memory outcomes. Linux generated execution, both-ISA lowering, ARM
emulation and workspace Clippy pass; fixtures export 111 helpers. This makes
cleanup reusable without pretending that the public rooted barrier/controller,
ordinary compiler ABI migration or whole collector is complete.
Required ASan+UBSan/TSan integration passes with the cleanup marker mandatory
from both builds. Strict C, formatting, whitespace and the refreshed 9639-site
inventory checks pass. Native Windows/macOS execution remains unverified.

The caller-return-root pipeline now uses ordered root-access cleanup too. After
both callee root packages have been retired and unmapped, it reads the surviving
caller value under a canonical guard. RELEASE before EXIT is rejected unchanged.
After EXIT it injects a rejected metadata-pool snapshot, verifies retention of
the exact cleanup obligation and checks that reacquisition reports
CLEANUP_REQUIRED. It crosses a registered native boundary with the strong caller
root and metadata lease still owned, successfully retries RELEASE using the real
pool, then reuses the same RootAccess to resolve and read the scalar again.
Only after the second balanced cleanup does it retire the caller's own root
segment and journal. No additional return-root publication is fabricated.

The cleanup C/candidate cases now restore the deliberately invalid pool state
and actually retry the failed RELEASE, check full resulting state and reacquire
using the same control. The independent ARM model does the same restoration,
release and replay-rejection sequence. Linux emitted execution passes. The
invalid-pool snapshot is fault injection, not a demonstrated production pressure
condition; this checks ownership recovery and caller lifetime, not the complete
public rooted barrier or ordinary compiler return lowering.
ARM emulation and required ASan+UBSan/TSan integration pass, with the caller
cleanup marker required in both sanitizer builds. Strict C, formatting,
whitespace and the 9639-site inventory check pass. Native foreign-OS execution
and whole-collector completion remain unverified.

Ordered root-access cleanup now has an OS-thread concurrency fixture. Two
independent contexts and strong root cells acquire the same source through the
shared metadata pool, then exit concurrently using the selected C/generated
cleanup. Half the rounds close source admission before releasing the actors;
the rest race selection with their exits. A closed source with both live guards
must fail the drain check. After joining both actors, the drain check succeeds,
but both root values and both metadata leases must still be retained. Exclusive
owner cleanup then releases each lease exactly once. A fresh access to the still
claimed source returns WAIT without leaking a new guard or lease.

The 256 C/candidate rounds pass in Linux generated execution. EXIT WAIT is retried
immediately without yielding with a live guard; the harness reports observed
retry counts without requiring a probabilistic minimum. All payload addresses
remain unmapped; this isolates native guard/lease ownership and does not claim
concurrent payload copying, a complete collector or foreign-OS execution.
Required ASan+UBSan/TSan integration passes with the two-reader cleanup marker
mandatory in both builds. The focused Linux run observed 89 contended EXIT
retries; this is run-specific evidence, not a minimum required by the test.
Strict C, formatting, whitespace and the 9639-site inventory check pass.

## Promotion handoff prerequisite

The remembered conformance owner now installs an age-selected Old copy and
registers its sole descriptor reference slot (including null) before publishing
forwarding. The installed copy is private until this handoff completes. Default
runtime integration must preserve that order for every reference slot and must
also satisfy an active Old mark before publication; neither requirement can be
replaced by changing a page generation alone. The current fixture has no active
Old mark and supplies survival evidence itself, so it is not a full cycle owner.

The promotion owner now has `gc_promotion_remember_locked` / shared emitted
`promotion_remember` for bounded descriptor-derived slot registration. Use its
persistent cursor only within the captured phase; phase adoption restarts the
private destination scan at zero. This replaces the fixture's hardcoded slot
registration but still requires production lifetime and Old-mark coordination.

A promotion parent without an existing Old stamp can hand its protected source
reference to `gc_mark_spill_locked` before publication when ordinary Old OFFER
is full. The new admission schedules retain that counted overflow obligation
through source decommit, then refill and retarget it before scanning. Production
promotion must likewise complete Old work admission before removing its producer
obligation; a FULL return alone never permits publication or lease release.

## Relocation scheduling owner

`gc_relocation_turn_locked` / shared emitted `relocation_turn` supplies exclusive
Young/Old relocation turns, coalescing requests and alternation under dual
pressure. The owning runtime must bind those receipts to its prepared generation
phase and gate, reopen the reserved gate epoch, and retain the turn through
job drainage and retirement audits. The component does not run a cycle or wake
workers. Default runtime integration and measured cross-generation progress
remain necessary.

A held relocation turn now supports authenticated `GC_RT_RESUME` after a closed,
drained recovery gate. Use it instead of directly reopening that gate after the
initial admission: it advances the reserved epoch while retaining exclusive
generation ownership. The real-copy/cancellation/installed-recovery pipeline
uses this operation and withholds the next generation until its full private
heap teardown. Production retirement audits remain required before RELEASE.

## Forwarding retirement prerequisite

A frozen incoming audit that finds an older forwarding table pointing into an
intermediate source can now use `gc_forward_compress_locked` / emitted
`forward_compress` after resolving that edge to the same rooted canonical object.
Restart the audit after the exact-edge CAS; keep both publication and compression
frozen for the completed audit. The source and intermediate metadata still need
all root/heap/buffer/job/native-reference and color-retirement proofs before reuse.
A single incoming-table DONE result does not authorize release of the relocation
turn or reclamation by itself.

### Bounded C heap incoming audit (2026-09-28)

`gc_heap_incoming.h` adds a C owner-side building block for the live-heap part
of source-range retirement. Under an existing canonical source guard it checks
one descriptor candidate per call, atomically reads reference slots only, and
retains the matching cursor on FOUND. Scalar-only regions are skipped without
heap reads. Malformed reference bits or invalid scope/phase/shape leave the
request unchanged. Diagnostic observed bits are not a root or a raw address;
repair must acquire the ordinary rooted barrier lifetimes before proceeding.

DONE is not a retirement certificate: the owner must enumerate every live
object, exclude introduction of old-range references across the complete audit,
and finish roots, buffers, jobs, handles and forwarding checks. Concurrent
atomic field loads alone do not establish a snapshot. The operation now also has a shared-emitter counterpart (see below), but
is not connected to the default collector or a complete heap owner. Tests cover mixed scalar/reference fields,
null and half-open range boundaries, repeated findings and logical clears,
600 individually sliced array slots, scalar-only/empty payloads, retained guards
after selection closes admission, and unchanged rejection state.

Validation: the dedicated final-source cases pass ASan/UBSan and TSan;
`cargo test --test llvm_gc` exits 0 with the required new audit marker and
all existing sanitizer markers. Log: `/tmp/klassic-heap-incoming-sanitizer.log`.
Strict C warnings, formatting, inventory freshness and diff whitespace checks
also pass. This is C-only evidence, not emitted-code or default-runtime parity.

### Shared heap incoming audit and moved-owner connection (2026-09-28)

`HeapIncoming` is now a checked 64-byte C/Rust request. Existing ABI layouts
are unchanged. `gc_codegen/heap_incoming.rs` emits the same guard, descriptor,
reference-validation and bounded-cursor contract for both ISAs; the Mach-O
fixture exports 116 helpers. The C and generated x86 cases share independent
expected statuses/cursors and whole-fixture nonmutation checks. ARM emulation
adds 40 independently constructed full-state cases, rather than deriving its
expected outputs from C.

The actual two-worker graph also invokes the selected C/emitted scanner after
joining workers. In 32 Young rounds, a canonical moved Old owner is guarded with
all four forwarding-chain metadata leases retained. Its single child reference
is found twice without advancing the cursor, and two decommitted old source
ranges are excluded. The guard exits before compression acquires the metadata
lock. This inspects actual copied payloads with protected old sources, but does
not enumerate the full live heap or certify retirement during concurrent stores.
The runtime still needs an owning enumeration/phase protocol and default
compiler integration.

Validation so far: Linux emitted graph test passes; ABI tests pass (8); ARM
emulation passes (40 new cases, 3731 helper calls in total). ARM execution is
instruction emulation, not execution on Darwin hardware. Logs are
`/tmp/klassic-heap-incoming-graph-native.log`,
`/tmp/klassic-heap-incoming-abi.log`, and
`/tmp/klassic-heap-incoming-arm.log`.

Final graph-connected validation: `cargo test --workspace` exits 0 with
797 passed, 0 failed and 0 ignored across 25 invocations, including mandatory
C ASan/UBSan and TSan marker checks. Log:
`/tmp/klassic-heap-incoming-workspace.log`. Strict C, native Clippy, formatting,
inventory freshness and whitespace checks also pass.

### Bounded heap page ownership (2026-09-28)

`gc_heap_walk_locked` and the shared `heap_walk` emitter connect page-table
traversal to actual metadata lease acquisition. A 64-byte `HeapWalk` binds its
cursor to one pool, heap, immutable page table and count. Each call examines at
most one table entry. Published run heads (including evacuated and uncommitted
sources) return PAGE with a counted lease; the consumer must release that lease
before advancing. The walk skips free pages, empty READY pages and authenticated
large-run tails. Preparation, release and nonempty private READY pages return
WAIT without changing the cursor; metadata FULL/EXHAUSTED and invalid inputs
also preserve state. No managed payload is touched or made strongly reachable.

This supplies the page-enumeration part of an owning cycle, not a snapshot.
The owner still has to exclude topology/range reuse and newly introduced old
references for the complete audit, enumerate each page's objects, guard their
payloads and coordinate root/buffer/job/handle/forwarding completion. Default
compiler startup and full-cycle ownership remain unconnected.

C/generated-x86 tests cover 64 mixed-page walks, real lease-pool exhaustion and
recovery, retained-output rejection, and 48 unchanged malformed/exhausted/wait
cases. ARM emulation constructs 39 independent expected full-memory outcomes.
The real two-worker graph enumerates 64 complete pools after joining workers,
checks every cursor step and releases every returned lease. In the 32 Young
rounds it finds both decommitted Old source heads; both generations retain
an initial installed Old head, while Old-mark rounds leave the two subsequent
relocation destinations empty READY and correctly skip them. The initial graph
assertion incorrectly required decommitted sources in both generations; the
fixture's actual per-generation transitions now define the expected counts.
No concurrent whole-heap snapshot or retirement is claimed.

Validation: native suite 73 passed (including generated x86 and both-ISA
lowering), ABI suite 8 passed, ARM instruction emulation 39 independent new
cases / 3770 total helper calls, and native Clippy with warnings denied.
The Mach-O fixture now exports 117 helpers. Logs:
`/tmp/klassic-heap-walk-native-final.log`, `/tmp/klassic-heap-walk-abi.log`,
`/tmp/klassic-heap-walk-arm.log`, `/tmp/klassic-heap-walk-clippy.log`.
These remain helper/owner integration results, not native Darwin execution
or default generational activation.

Final-source C validation: `cargo test --test llvm_gc` exits 0, 1 passed,
0 failed/ignored, including both required ASan/UBSan and TSan marker sets.
Log: `/tmp/klassic-heap-walk-sanitizer-final.log` (121.66 seconds). Strict C,
formatting, inventory freshness and whitespace checks also pass.

### Authenticated object-key enumeration (2026-09-28)

`gc_object_walk_locked` and the shared `object_walk` emitter take a 56-byte
`ObjectWalk` with a borrowed page lease and captured page/incarnation/table/used
count. Each call authenticates the live token against the metadata pool before
reading the table. ITEM returns one immutable source offset and advances one
entry; the caller consumes and clears the output before continuing. No managed
payload address is formed, no reference is rooted, and no additional lease is
acquired. Forwarded, claimed and retained entries remain in the enumeration.
Released/stale leases are rejected even at end of the walk.

Append changes return WAIT with unchanged request, including after the captured
last entry. The owner must restart the captured sequence or exclude append for
the complete audit. This is still a metadata enumeration, not a liveness test,
heap snapshot or retirement certificate. The next owner connection must resolve
or classify each key, guard and scan appropriate canonical payloads, and close
the full root/buffer/job/native/forwarding phase obligations before reuse.

The two-worker graph now composes page and object enumeration across the whole
actual pool in 64 schedules after workers join. It releases the metadata lock
between object steps while retaining the page lease, checks original source
keys against the eight allocation results, and checks total keys equal eight
plus the number of completed installed relocation jobs. The two decommitted
Old source payloads in Young rounds remain protected throughout. After releasing
each page lease, another object step must reject the stale request. These are
exclusive post-join enumeration checks, not concurrent snapshot certification.

C/generated-x86 cases cover 32 full key walks over four page states and four
forwarding states, append/restart, lease release and 60 unchanged rejections.
ARM emulation independently constructs 100 full-state outcomes. The Mach-O
fixture exports 118 helpers; existing ABI layouts are unchanged. Native Linux
integration passes in `/tmp/klassic-object-walk-native.log`.

Final validation: `cargo test --workspace` exits 0 with 797 passed, no
failures or ignored tests across 25 invocations, including both mandatory
C sanitizer sets and ABI checks. Log: `/tmp/klassic-object-walk-workspace.log`.
ARM emulation passes 100 new independent cases / 3870 total helper calls in
`/tmp/klassic-object-walk-arm.log`. Native Clippy with warnings denied, strict
C warnings, formatting, inventory freshness and whitespace checks also pass.

### Whole-pool guarded payload audit connection (2026-09-28)

The real graph owner now composes page enumeration, authenticated object-key
walks, resolution and `heap_incoming` over every canonical payload in 64
post-worker schedules. It closes the actual relocation gate before enumeration,
captures the adopted phase once, and retains that gate epoch through the heap
and forwarding audits. The fixture exclusively retains all eight logical
objects and physical copies until teardown; metadata leases alone would not
supply that liveness guarantee in production.

A forwarded key is handled without reading its protected source payload. The
private destination bits are discarded immediately; the destination's own key
is visited by the full pool walk. Each canonical object is checked against its
stable MarkRecord and eight-bit identity coverage, preventing duplicate scans
or omission of an object. Each descriptor candidate acquires a fresh guard,
performs one bounded incoming slice, exits the guard and verifies zero access
epoch before yielding. No raw heap address or guard survives the yield.

Old-mark rounds target the original source range and must report at least two
remaining references. Each finding is recorded and consumed for enumeration;
advancing past a finding does not repair it or authorize retirement. Young
rounds target the decommitted first Old source and require no remaining payload
references into that range. All rounds inspect eight unique canonical objects,
skip exactly one forwarded physical source per completed relocation job, and
perform at least the original array's candidate count (up to 16384 entries).
The page lease remains held between slices and is released only after its key
walk completes. Forwarding compression and incoming-table checks then run
under the same frozen relocation epoch; the gate reopens afterward.

This verifies complete fixture payload traversal and balanced guards, not a
production cycle owner or a concurrent heap snapshot. All workers are already
joined. Roots, journals, buffers, native handles, allocation/phase admission and
color retirement still require the production owner before range reuse or a
default compiler switch can be claimed.

Validation: final generated Linux x86 integration passes in 23.82 seconds
(`/tmp/klassic-heap-payload-native-final.log`); C sanitizer integration passes
in 112.76 seconds with the required ASan/UBSan and TSan markers
(`/tmp/klassic-heap-payload-sanitizer.log`). Strict C warnings, formatting,
inventory freshness and whitespace checks pass. No new emitted operation or
ABI layout was added by this connection; whole-pool scheduling remains a host
conformance owner, not a Darwin/Windows production-execution result.

### Reusable guarded audit slice (2026-09-28)

`gc_heap_audit_slice` and the shared `heap_audit_slice` emitter now own one
bounded resolve/descriptor-scan/exit attempt. The 160-byte `HeapAuditSlice`
retains the exact source scope and pending result if EXIT cannot complete.
EXIT_REQUIRED requires immediate cleanup retry without poll, blocking or
metadata release. Cursor and observed bits commit only after successful EXIT;
ordinary completed calls clear access and pending scratch. Idle validation
rejects nonempty scratch unchanged. Forwarded destinations are discarded before
return, so no unrooted private destination crosses the caller's next yield.
FORWARDED is not object completion: the owner still has to visit the canonical
key or restart an audit whose topology changed.

The caller must retain the strong receiver (or the cycle's equivalent exclusive
object lifetime), source metadata and immutable descriptors. This helper does
not authenticate or acquire that parent lease, root an observed value, provide
phase admission, or certify whole-heap retirement. It is the reusable finite
payload operation, not the complete generational runtime owner.

The whole-pool graph uses the C/generated slice for every candidate and for
protected forwarded sources. Its separate identity check uses a temporary
guard only once per canonical object, then exits before continuing. All existing
eight-identity, full-array, positive/negative range, frozen-gate and balanced
page-lease checks remain mandatory. C/generated cases additionally exercise
invalid scans after successful guard entry, forwarding/wait/exhaustion, and
pending DONE/MORE/FOUND/INVALID results across failed cleanup and retry without
losing the held scope or committing early. Injected cleanup errors restore the
faulted control before retry; they are not claims that such invalid controls
are normal runtime states.

Validation: native suite 73 passed in 22.32 seconds, ABI suite 8 passed, native
Clippy with warnings denied passes, and ARM emulation independently checks 58
new scan/cleanup calls (3928 total helper calls). The Mach-O fixture exports
119 helpers; existing ABI layouts are unchanged. Logs:
`/tmp/klassic-heap-audit-slice-native-final.log`,
`/tmp/klassic-heap-audit-slice-abi.log`,
`/tmp/klassic-heap-audit-slice-arm.log`, and
`/tmp/klassic-heap-audit-slice-clippy.log`. ARM results remain instruction
emulation rather than native Darwin execution.

Final C validation: `cargo test --test llvm_gc` exits 0 in 108.98 seconds,
including both mandatory ASan/UBSan and TSan marker sets. Log:
`/tmp/klassic-heap-audit-slice-sanitizer.log`. Strict C, formatting, inventory
freshness and whitespace checks also pass.

### Cooperative whole-heap audit owner (2026-09-28)

`gc_heap_audit_step` and the shared `heap_audit` emitter compose page leases,
object-key enumeration and guarded payload slices in a 352-byte `HeapAudit`.
META/CANCEL run under the metadata lock; SCAN/CONSUME run outside it. Each
operation advances a bounded step. Findings remain diagnostic bits, and
CONSUME counts them without healing. A completed traversal with any findings
returns DIRTY rather than DONE. CANCEL releases the retained page lease before
resetting the request, including after a stale gate or adopted phase.

Before acquiring a new source guard, SCAN checks its prepared page, object,
context, registry, phase and target range against the retained owner bindings.
An existing guard bypasses these checks so its exact EXIT can still complete.
EXIT_REQUIRED retains the guard and requires immediate SCAN retry without a
poll or metadata operation. Pressure and failures preserve cleanup ownership;
changed epochs or appended object tables require cancellation and recapture.

The moved-graph fixture runs this owner over every page and compares its finding
count with the independent leased traversal in 64 rounds. It checks terminal
state and zero outstanding leases, then cancels the completed request. This
runs after worker joins with fixture-owned object lifetimes. Strong lifetimes,
allocation/topology admission, roots, buffers and handle audits remain caller
obligations: a clean traversal is not a source-retirement certificate or a
concurrent snapshot. Ordinary compiler startup still uses the legacy ABI.

Validation: the final workspace run passes 797 tests across 25 invocations with
zero failures or ignored tests, including required C ASan/UBSan and TSan
checks (114.70 seconds for the C suite) and emitted x86 integration. ARM
emulation passes 682 independent audit-owner calls, including nine prepared
binding mismatches, and 4610 helper calls overall. The Mach-O fixture exports
120 helpers. ABI checks, native Clippy with warnings denied, strict C warnings,
formatting and inventory freshness (19427 candidates / 303 files) pass.
Logs use `/tmp/klassic-heap-audit-owner-final-` (`workspace.log`, `arm.log`,
`export.log`, `clippy.log`). ARM emulation does not establish Darwin execution.

### Rooted field-observation ownership (2026-09-28)

`gc_rooted_observation_step` centralizes receiver root acquisition, guarded
field publication into a distinct output root, source EXIT and metadata release.
ENTER/RELEASE use the heap lock; OBSERVE/EXIT run outside it. A pending read is
never repeated during cleanup retries. COMPLETE exposes the root-operation
result only after both guard and lease are released. RESET discards observation
scratch and permits a new read through the receiver root. A null receiver is
reported separately from a completed field read. Output bits remain a rooted
comparison token, not a canonical address.

The 64-round moved-receiver pipeline runs this protocol with C or generated
x86 leaf callbacks. It fills the root journal, obtains FLUSH, exits/releases,
changes the canonical field and retries with a fresh observation. Assertions
cover exactly one root revision per successful read, receiver/output alias
rejection, pending-read stability, cleanup failures retaining the exact guard
and lease, and stable completion until RESET. Injected invalid cleanup controls
are restored before retry; no polling occurs while a guard is held.

This is a host owner with injected backend operations, not a newly emitted
controller or a complete public load barrier. Referent forwarding/marking,
self-healing, phase admission and ordinary compiler lowering remain required.
The shared direct emitter still needs this orchestration before default native
programs can call it without C linkage.

Validation: emitted x86 integration passes in 23.04 seconds
(`/tmp/klassic-rooted-observation-native-final2.log`). The final mandatory C
ASan/UBSan and TSan integration exits 0
(`/tmp/klassic-rooted-observation-sanitizer-final.log`). Strict C warnings,
formatting, whitespace and inventory checks pass (19474 candidates / 304 files).
The initial changed-field fixture incorrectly supplied a stale desired color;
correcting it to the current idle-phase color satisfies the store precondition.
The earlier full-workspace checkpoint predates this orchestration change;
these focused native/C suites validate the affected paths.

### Shared emitted rooted-observation owner (2026-09-28)

The ownership protocol above now has a shared PortableAsm implementation in
`gc_codegen/rooted_observation.rs`. It calls embedded root-acquisition,
observation and cleanup helpers directly, with no C orchestration dependency.
The 200-byte `RootedObservation` control is part of the matching C/Rust ABI;
existing layouts and ABI version are unchanged. The C entry retains a callback
form for composition and provides the same three-argument production signature.

The 64-round protected-receiver integration now invokes the generated owner,
including its pending-read, cleanup-failure and completed-state transitions,
rather than a C owner calling generated leaves. Both ISA lowering and the
native x86 suite exercise the new operation; the Mach-O fixture exports 121
helpers. ARM tests independently construct expected complete memory states
for rooted reads, null and stale-colored values, journal/exhaustion failures,
invalid payloads, retained EXIT/release obligations and repeated completion.

This removes the earlier host-only orchestration limitation. Ordinary compiler
lowering, referent mark/forward/heal control and phase admission remain open;
this operation still returns an unresolved rooted observation, not a complete
public load barrier or a source-retirement proof.

Validation: native suite 73 passed in 22.73 seconds, ABI suite 8 passed, native
Clippy with warnings denied passed, and mandatory C ASan/UBSan and TSan
integration exited 0. Independent ARM validation passes 189 new owner calls
and 4799 helper calls overall, including busy/forwarded receiver acquisition,
forwarding journal pressure and revision exhaustion. Strict C warnings,
formatting, whitespace and inventory freshness pass (19517 sites / 305 files).
Logs: `/tmp/klassic-rooted-observation-emitter-native-final2.log`,
`/tmp/klassic-rooted-observation-emitter-abi.log`,
`/tmp/klassic-rooted-observation-emitter-arm-final.log`,
`/tmp/klassic-rooted-observation-emitter-clippy.log`, and
`/tmp/klassic-rooted-observation-emitter-sanitizer.log`.
ARM remains instruction emulation, not native Darwin execution or hardware
memory-order evidence; no G01-G15 capability is certified complete by this work.

### Rooted mark-offer ownership (2026-09-28)

`gc_rooted_mark_step` composes rooted canonical access, generation-filtered mark
routing, source EXIT and queue admission. The caller retains a counted producer
obligation and captures the queue/generation/epoch/phase under cycle admission.
The root stays protected across every bounded step. Routing and EXIT run outside
locks; heap metadata and mark queue operations use their respective locks only
after the raw guard has exited.

Pending route fields and the access cleanup token are aliases within one owner,
not independently releasable leases. Successful OFFER transfers that obligation
to the queue; duplicates require exact release. FULL and EXHAUSTED retain the
pending offer for retry or counted overflow admission. A changed queue epoch
returns STALE unchanged, with explicit cancellation before recapture. Cleaned
WAIT/INVALID/SKIP results remain distinguishable from successful enqueue.

The protected-receiver pipeline passes the actual rooted-observation output to
this owner for both generation choices. A one-slot queue is filled by its parent;
the child first encounters FULL, then enters counted overflow before the parent
scan advances. Parent and child tasks are scanned, finished and their metadata
leases released. The fixture also covers serial exhaustion, duplicate cleanup,
stale-epoch cancellation, failed EXIT/release retries, null, canonical Old skip,
INITIALIZING wait and a mismatched captured phase. Header/generation mutations
in the latter eligibility cases are exclusive fixture setup/fault injection,
not a production promotion or publication protocol.

This owner currently executes in C with C or emitted leaf callbacks. Emission
of the owner, public load-barrier phase admission/mark/heal orchestration and
default compiler lowering remain open. No producer termination or full cycle
completion is inferred from these queue tests.

Final generated-leaf integration passes in 32.39 seconds with all 64 observed
referents and both generation choices, including skip/wait/invalid cleanup
(`/tmp/klassic-rooted-mark-native-final2.log`). Inventory contains 19613 candidate
sites across 306 files; it remains an access index, not compiler coverage proof.

Final mandatory C ASan/UBSan and TSan integration exits 0
(`/tmp/klassic-rooted-mark-sanitizer-final.log`). Strict C warnings, formatting,
whitespace and inventory freshness checks pass. No new ARM owner operation was
added in this step; previous ARM results do not validate this host orchestration.

### Shared emitted rooted mark owner (2026-09-28)

The mark-offer ownership protocol now runs through shared PortableAsm in
`gc_codegen/rooted_mark.rs`. Ordinary OFFER and OFFER_OR_SPILL entry points
embed the same acquisition, routing and cleanup operations and differ only in
queue admission. Both use the 304-byte C/Rust `RootedMark` control; existing ABI
layouts and version are unchanged. No C callback is needed by generated owners.

The moved-receiver pipeline now invokes these generated owners for all 64
observation-to-mark handoffs, including overflow admission before parent scan
progress, duplicate release, cancellation and route failures. C uses equivalent
three-argument wrappers. This removes the previous host-orchestration limitation
for this protocol; the public barrier still needs cycle admission, completion
of all required generation marks, recoloring/healing and compiler lowering.

Validation: native suite 73 passed in 24.58 seconds; C/Rust ABI suite 8 passed;
mandatory C ASan/UBSan and TSan integration passed in 109.08 seconds. ARM
independently verifies 136 new full-memory state transitions and 4935 helper
calls overall. These include both generations, deferred overflow ownership,
queue serial exhaustion, duplicate cleanup, stale cancellation and route
skip/wait/invalid outcomes. Its initial page fixture set the generation at the
wrong offset; the actual ABI's owner/generation fields were used to correct it.
Native Clippy with warnings denied, strict C warnings, formatting, whitespace
and inventory freshness pass (19679 sites / 307 files). The Mach-O fixture now
exports 123 helpers. Logs use `/tmp/klassic-rooted-mark-emitter-` (`native.log`,
`abi.log`, `arm-final.log`, `clippy.log`, `sanitizer.log`). ARM remains emulation;
ordinary native programs and actual Darwin/Windows execution remain unverified
for the new runtime. G01-G15 completion is not established by these results.

### Required-generation mark completion (2026-09-28)

`gc_rooted_mark_set_step` derives a two-bit requirement from the captured phase
and tracks completion for one retained output root. Old is processed first when
required. Each generation uses its own captured queue and epoch through the
existing rooted mark owner. A mark already admitted remains owned by its queue
across cancellation; resetting the outer request never revokes that work.
The output field is not reread during snapshot restart.

Adopting a different snapshot invalidates both partial and completed receipts.
Pending clean work requires cancellation before recapture, while an existing
guard can still finish EXIT. Queue-epoch changes also request restart. A cleaned
INITIALIZING wait resets only that child request for retry and does not set its
completion bit. DONE reports required mark admissions only; phase admission,
recolor/heal, producer accounting and whole load-barrier completion remain
caller responsibilities.

The 64 moved-receiver rounds now mark the actual observation output for both
concurrent generations. They take Old work in flight, adopt a new snapshot,
cancel/recapture, finish the duplicate Old admission, and complete Young marking
and scanning while the original Old claim remains live. Both leases are then
released after their respective scans. Additional cases exercise all four phase
requirement masks, stable/expired completion receipts and an INITIALIZING fault
restored before retry. Snapshot adoption and header fault injection are exclusive
fixture actions, not a production handshake or publication implementation.

The outer set owner is currently C orchestration calling C or emitted rooted
mark owners; its direct emitter and ordinary compiler connection are still open.
The generated-leaf integration passes in 24.25 seconds
(`/tmp/klassic-rooted-mark-set-native-final.log`). Inventory contains 19758
candidate sites across 308 files and is not a compiler-coverage certificate.

Final mandatory C ASan/UBSan and TSan integration exits 0
(`/tmp/klassic-rooted-mark-set-sanitizer.log`). Strict C warnings, formatting,
whitespace and inventory freshness checks pass. No new ARM owner operation is
claimed in this step; G01-G15 and the default collector migration remain open.

### Shared emitted generation-completion owner (2026-09-28)

`gc_codegen/rooted_mark_set.rs` now emits the required-generation protocol with
an embedded counted-overflow mark owner. The C three-argument entry uses the
same admission policy. `RootedMarkSet` is a 728-byte matching C/Rust ABI control;
existing layouts and ABI version are unchanged. The generated owner selects
and resets its embedded jobs directly, without C orchestration or callbacks.

The 64-round observation/mark pipeline now calls this emitted owner, including
partial/completed snapshot invalidation, retention of Old in-flight work while
Young finishes, all four requirement masks and INITIALIZING wait/reset/retry.
ARM independently compares complete control/heap/queue memory for all 64 phase
combinations, nonnull and null admissions, live pending cancellation, expired
receipts and queue-epoch changes. Its WAIT case also verifies retry generation
consumption and that cancelling after Old admission retains the queue's lease.

This removes the host-only orchestration limitation for generation completion.
It still provides mark admission, not the full load barrier: production cycle
admission, recoloring/healing, producer accounting and ordinary compiler
integration remain required before enabling the generational runtime by default.

Validation: native suite 73 passed in 26.89 seconds; ABI suite 8 passed;
mandatory C ASan/UBSan and TSan integration exited 0. ARM independently passes
265 new owner calls and 5200 helper calls overall. Native Clippy with warnings
denied, strict C warnings, formatting, whitespace and inventory freshness pass
(19821 sites / 309 files). Mach-O exports 124 helpers. Logs use
`/tmp/klassic-rooted-mark-set-emitter-` (`native.log`, `abi.log`,
`arm-final.log`, `clippy.log`, `sanitizer.log`). ARM instruction emulation is not
Darwin execution or hardware memory-order evidence; no G01-G15 completion is
claimed by these helper and owner results.

### Guarded publication of marked root colors (2026-09-28)

`gc_rooted_mark_publish` and its shared emitter validate a completed required-
generation receipt against a canonical RootAccess for the same owner root.
They check the captured phase, required/completed mask, context/handle binding,
guard and metadata token shape, then authenticate the root segment/cell. The
current root must still point at that guard's source offset. An exact CAS updates
only mark/remap colors; remembered bits, the logical referent, root journal and
logical revision are preserved. All returns retain the caller's guard and lease.
CHANGED returns no unrooted value and requires a fresh guarded attempt.

The owner must preserve the root's logical referent throughout mark admission
and publication. Matching handle fields alone do not prove that an intervening
logical replacement was marked. Forwarding, physical recoloring and copying
another proven reference to the same logical object are allowed; replacement
requires fresh admission. This obligation is now explicit in the mark-set
contract. Null is handled by the outer load without entering a source guard.

The 64 moved-object rounds publish a stale-colored reference to the same marked
child, reject incomplete/stale/mismatched bindings, verify the current colors
and unchanged remembered bits, and compare the complete request, journal and
revision before releasing the guard/lease. ARM independently checks all 32 epoch
combinations with three observed color patterns, invalid controls, and injected
same-object recoloring between load and CAS. The intervening update survives,
CHANGED is returned, and a subsequent attempt repairs from the new observed bits.

This connects mark admission to root color repair. Receiver-field self-healing,
production cycle admission/producer accounting and the full public load-barrier
controller are still required, as is default compiler/runtime integration.

Validation: native suite 73 passed in 25.65 seconds; mandatory C ASan/UBSan and
TSan integration exited 0. ARM independently passes 130 new publication cases
and 5330 helper calls overall. Native Clippy with warnings denied, strict C
warnings, formatting, whitespace and inventory freshness pass (19927 sites /
311 files). Mach-O exports 125 helpers; no ABI layouts changed. Logs use
`/tmp/klassic-rooted-mark-publish-` (`native-final.log`, `arm-final.log`,
`clippy.log`, `sanitizer.log`). ARM remains instruction emulation, not Darwin
execution or hardware-order proof; the full G01-G15 contract remains incomplete.


### Marked result to receiver self-heal integration

The OS-backed load pipeline now passes the actual generated guarded-heal helper
through the rooted observation / dual-generation mark-set / root publication
sequence. In each of 64 moved-receiver rounds it releases the result guard,
reacquires the receiver through its retained root, and attempts an exact field
CAS using the original observation token and the protected marked result.
Remembered bits come from that observation, not from the result root.

A guarded logical null store then publishes its overwritten reference to SATB
with a real retained metadata lease. A delayed repeat heal returns CHANGED,
leaves the field null, and preserves the original load result in its root.
Neither repair changes the logical root revision. The exclusive fixture consumes
buffer entries and releases their leases before teardown; this is not concurrent
termination or source-retirement evidence. This successful CAS can be idempotent;
it does not add a new forwarding or recoloring algorithm.

The Linux generated-code integration test passes (1 focused test, 64 rounds).
The mandatory C ASan/UBSan and TSan integration test also passes. Strict C
compilation, formatting, whitespace and inventory freshness checks pass.
The public load controller, cycle admission, compiler lowering and default ABI
activation remain incomplete. Inventory: 19940 candidates across 311 files;
this remains a search index, not coverage evidence.


### Rooted load result owner (C orchestration)

`gc_rooted_load.h` joins rooted observation, generation mark admission, canonical
result access and guarded root-color publication in one bounded C controller.
Its `next` action selects heap-lock, queue-lock or no-lock guarded access work.
Wrong-domain steps are nonmutating. The result is an owned root, never a raw
address or a receipt that remains color-good across a later phase change.
Payload consumers still enter through RootAccess. Input root/metadata lifetimes
and cycle admission/countable-producer responsibility remain caller obligations.
The new control is private C storage, not an addition to the shared ABI layout.

A full observation journal is reported only after receiver EXIT and exact lease
release. Phase restart before mark admission or with an unpublished mark offer
cancels retained work, resets the mark set, and requests a fresh admitted snapshot;
it retains the original observation root and does not reread the receiver field.
Published queue work stays owned by the queues. Cancellation records intent and
continues required EXIT/release steps before returning CANCELLED; an in-flight
observation may finish, and caller roots remain owned. Mark failures
likewise clean pending ownership before returning ERROR. Exhausted/full pending
offers are cancelled before returning a clean retry event. A null receiver is a
distinct error outcome; a null field is a successful rooted result.

The 64-round protected-source pipeline runs this C owner over both C and actual
Linux emitted leaves. It covers full journals, failed EXIT and lease-release
retries, both-generation real offer transfers followed by scan/finish/release,
duplicate cleanup, phase changes before admission and before queue offer,
wrong-domain rejection, 22 cancellation positions, stable completion, null
results and null receivers. Queue epochs are advanced only in the exclusive
fixture after queues drain; this is not a cycle or color-reuse proof.
The focused generated-code integration passes (1 test, 24.14 seconds).

The owner itself is not emitted yet. Receiver-field self-heal orchestration,
cycle/handshake integration and ordinary compiler/runtime activation remain
open. The existing self-heal integration fixture is separate from this owner.
No additional G01-G15 completion claim follows from these checks. Inventory is
20114 candidate sites / 312 files and remains an index, not coverage evidence.

Final validation for this controller: mandatory C ASan/UBSan and TSan integration
passes (1 test); strict C compilation, Rust formatting, whitespace and inventory
freshness checks pass. No shared ABI layout or emitter body changed in this step,
so this is not a new ARM/native Darwin or shared-owner-emission claim.


### Shared rooted-load owner emission

The load owner now has a shared x86-64/AArch64 emitter in `rooted_load.rs`.
It embeds the existing observation, mark-set, root-access, publication and
cleanup helpers, with a bounded action dispatcher and no host callback or C
controller dependency. The OS-backed pipeline now calls the generated owner
itself for its full-journal, real queue transfer/scan, duplicate, restart,
wrong-domain, cancellation and null cases. Scheduling in that fixture still
uses the C `next` query; each emitted step independently checks its lock domain.

`RootedLoad` moved from private C header storage into the C/Rust shared ABI:
1096 bytes, observation at 0, mark set at 200, publication access at 928,
stage/last/cancel/fault/retry event at 1056/1064/1072/1080/1088. Existing ABI
layouts and version 3 are unchanged; C/Rust layout tests cover the addition.
The Mach-O conformance fixture now exports 126 helpers.

Validation: all 73 native crate tests pass (24.64 seconds), ABI 8 tests pass,
mandatory C ASan/UBSan and TSan integration passes (113.57 seconds), and native
Clippy with all targets/features and denied warnings passes. Strict C,
formatting, whitespace and inventory checks pass. ARM emulation performs 2914
whole-fixture dispatcher comparisons against an explicit Python protocol model
composed from separately verified emitted leaves. These cover four generation
masks, null/non-null results, cancellation positions, wrong-domain rejection,
cleanup failures, full journal, pending-offer snapshot restart and invalid
controls. Every terminal success/cancellation also checks that only queued work
retains leases. Including model leaf calls, the run totals 9582 helper calls,
27 API-model scenarios and 1536 context transitions. This is not native Darwin
execution, hardware ordering evidence or an independent reproof of each leaf.

Rooted-load cycle admission/countable producer integration, receiver-field
self-heal orchestration, default compiler/runtime activation, actual cross-OS
execution and performance gates remain open. Inventory: 20234 candidate sites
across 313 files; an index, not whole-compiler coverage evidence.


### Rearming a rooted load after a phase wait

`GC_RLOAD_REARM` (47) is now implemented in C and the shared load-owner emitter.
After cancellation finishes, the caller may yield to the worker rendezvous while
retaining the same strong root handles and logical referents. Rearm runs under
the heap lock only after the context is Running again. It rejects parked/native
contexts, active access epochs, failed owners, unfinished observations/mark sets,
and any retained guard/lease scratch in the receiver, publication or either mark
job. All rejection checks precede reset. The shared ABI layout is unchanged.

A successful observation survives rearm, including a null value: rearm requests
RESTART, so the caller recaptures cycle admission/queues/epochs/phase and resumes
marking the same output root. It never rereads the receiver in that case. A
cancelled operation that had not successfully observed a field starts a fresh
observation instead. This does not release roots, ACK a handshake or discharge
producer accounting automatically.

The 64-round protected-source fixture now runs the actual C worker/handshake
steps at every tested cancellation position, using local registered fixture
controls: BEGIN, POLL/ACK and phase adoption, END, then POLL/resume. Both C and
emitted load owners reject REARM while parked without mutation. After resume,
the same load finishes; its root revision stays unchanged for a captured value,
and increases exactly once when a fresh observation was required. Worker
emission and thread-registration lifetimes have separate tests; these fixture
controls do not prove a production thread lifecycle or mark termination.

All 73 native crate tests pass (22.27 seconds). ARM owner comparisons increase
to 3850, including rearm at cancellation positions and 50 explicit dirty-control
rejections; the full emulation run reports 11204 helper calls, 27 API-model
scenarios and 1536 context transitions. Clippy (all targets/features, warnings
denied), strict C, formatting and whitespace pass. Inventory: 20289 candidates
across 313 files. Native Darwin/Windows, default lowering, receiver self-heal
orchestration and global private-producer/admission integration remain open.
In particular, callers still must clean/cancel private work before ACK; the
worker API does not globally discover active load owners.

The final mandatory C ASan/UBSan and TSan integration also passes (116.97
seconds); inventory freshness passes. No ABI fields changed in this rearm step.


### Context-counted private load work (ABI v4)

Generational ABI v4 appends `private_producers` at MutatorContext offset 280
(size 288) and `admitted` at RootedLoad offset 1096 (size 1104). Earlier field
offsets remain unchanged, but v3 controls must not be mixed with v4 helpers.
C/Rust layout tests cover both additions. Ordinary compiler/runtime activation
still uses the legacy collector and has not switched to this ABI.

A load owner counts itself on its first valid HEAP observation/mark-init step,
with Running/no-access validation and checked saturation. The same context's
OS owner is the sole writer; release stores publish the count, and readers use
acquire loads. Multiple live controls count separately. Wrong-domain calls do
not admit work. Clean WAIT/FLUSH/RESTART events retain admission: a caller must
cancel and drive cleanup before a phase/native wait. Only DONE, CANCELLED,
clean ERROR or null-receiver completion releases that owner's count. Queued
mark leases remain independently owned and counted by the queues. REARM rejects
an owner that still holds its admission, then the resumed operation readmits.

Both C and shared handshake emitters reject registration with private work and
return WAIT for owner transitions while a private count remains. Worker calls
therefore return RETRY instead of acknowledging or entering native. Direct
context publication returns PRIVATE_WORK (5). Mark-end validation independently
returns WAIT even if a premature ACK/state publication was injected. This closes
the load-owner private-work hole; other private protocols still require their
own counted admission or cleanup before rendezvous. Epoch/queue capture, cycle
scheduling, complete runtime startup and default compiler lowering remain open.

The 64-round C/emitted load pipeline admits two controls, checks that native
entry and ACK stay blocked after only one cancels, and observes successful ACK
only after both release. Saturation leaves all input state unchanged. The worker
itself is now selected as C or emitted in this pipeline. Sixteen additional
C/emitted mark-end cases inject private work for either owner, both generations
and all native/managed membership combinations; every case returns WAIT.

Validation: C/Rust ABI 8 tests, native crate 73 tests (22.52 seconds), and full
workspace 797 tests across 25 invocations pass, with no failures or ignored
cases. The added mark-end cases also pass the focused emitted-code integration
(26.97 seconds). ARM emulation reports 3857 whole-fixture owner comparisons,
8 explicit admission/worker/handshake protocol checks, 11223 total helper calls,
27 API-model scenarios and 1536 context transitions. Native Clippy with denied
warnings passes. Actual Darwin/Windows execution, hardware memory ordering and
performance gates are not established by these results. Inventory is 20341
candidate sites across 313 files, not compiler-access coverage evidence.

Final mandatory C ASan/UBSan and TSan integration, including the additional
private-producer mark-end cases, passes in 117.43 seconds. Strict C, formatting,
whitespace and inventory freshness pass as well.


### Registered load snapshot capture

`gc_rooted_load_admit_locked` and the shared `rooted_load_admit` emitter capture
one RootedLoad's phase and active generation queue epochs under the common
thread/root/queue registry lock. The new 40-byte RootedLoadAdmission input holds
registry, handshake, participant and two queue pointers. It is an additive ABI
v4 type; existing layouts/version are unchanged. Its C/Rust layout is checked.

The operation requires a registered, Running participant of the supplied
handshake, no access guard or retained private access scratch, and either a
clean observation start/retry or a RESTART request with a captured observation.
An active rendezvous, unacknowledged generation or non-adopted registry phase
returns WAIT. Active queues must have the matching generation and nonzero epoch;
inactive queues are not dereferenced and their captured bindings are zeroed.
All checks precede writes. Success replaces phase/queue/epoch inputs and admits
a counted private producer in the same lock interval; an already admitted owner
is not counted twice. Saturation is nonmutating. Admission never traverses roots,
reads managed payload, polls or finishes a cycle. BEGIN can still publish a new
snapshot, but cannot retire the admitted owner's epoch by passing its ACK.

The protected-source pipeline now invokes the C/generated capture operation at
initial admission and after cancellation/worker phase adoption/rearm. It checks
WAIT during a real handshake, nonmutating rejection of a bad second queue and
saturation, replacement of deliberately stale inputs, idempotent admission and
post-handshake recapture of the same retained load value. The fixture's worker
is also C/generated. Ordinary compiler/runtime use remains to be connected.

All 73 native tests pass (24.34 seconds), ABI 8 tests pass, and native Clippy
(all targets/features with denied warnings), strict C, formatting and whitespace
checks pass. Mach-O exports 127 helpers. ARM admission adds 1108 independent
whole-memory cases: 64 phase combinations, two epoch masks, fresh/restart and
new/existing admissions, inactive invalid pointers, idempotence and rejection
faults. The full emulation reports 12331 helper calls, 27 API-model scenarios and
1536 context transitions; it does not establish Darwin execution or hardware
ordering. Inventory is 20454 candidate sites across 315 files, not compiler
coverage. Receiver self-heal orchestration, other private-producer admission,
complete cycle execution, default lowering, cross-OS execution and performance
gates remain open.

The final mandatory C ASan/UBSan and TSan integration passes in 127.28 seconds;
inventory freshness also passes.


### Rooted load receiver self-heal owner

`gc_rooted_load_heal.h` and the shared direct emitter now connect receiver
self-healing to `RootedLoad`. After result-root publication and cleanup, the
owner reacquires the receiver, reads the retained result root, and attempts an
exact CAS against the original observation token. A competing field update
wins; the owner still returns the original rooted load result. Stale result
colors request another result-root repair, after releasing the receiver guard.
Cancellation includes all four new receiver access states. Private producer
admission remains held through the final guard and metadata lease cleanup.

The heal never dereferences the target. Strong roots, logical-referent stability
and source/forwarding metadata retention remain caller/cycle obligations; later
target payload access requires RootAccess. This does not finish production
compiler lowering, other producer admission, cycle execution or retirement.

Native tests pass (73, 27.19 seconds), and Clippy with denied warnings passes.
The 128-helper Mach-O export passes ARM emulation: 15557 helper calls, 27 API
model scenarios and 1536 context transitions. The owner model includes all new
states, 26 cancellation boundaries, explicit competing-update outcomes and
stale-color retry. The dispatch model composes emitted child helpers; the
explicit CAS outcome assertions independently check field/result preservation.
This remains emulation, not native Darwin execution or hardware-order testing.
Inventory records 20600 candidate sites across 317 files, not lowering coverage.
Ordinary compilation/runtime remains on the legacy GC; cross-OS execution and
performance gates remain open.

Mandatory C ASan/UBSan and TSan integration also passes for this connection;
formatting, whitespace and inventory freshness checks pass.


### Rooted load versus a second OS store owner

The OS-backed load pipeline now runs the actual C/generated RootedLoad through
its receiver-heal stage while a separate OS thread performs GuardedStore to
null under its own source guard and MutatorContext. All 64 rounds retain the
original result root and real source metadata leases. In 32 rounds the store
is forced to finish before the load attempts repair, requiring CHANGED; the
other 32 release both operations together and accept either heal linearization.
The slot must end null, the original result root must remain unchanged, and the
store's overwritten-value SATB entry remains owned until explicit consumption
and lease release. The load must release its private producer count and guard
before thread join. Rendezvous spins inside guards deliberately simulate test
preemption; production barriers must not poll, block or wait there.

This uses two OS threads with fixture-owned contexts and retained roots and
metadata. It does not test concurrent registration, phase adoption or complete
cycle termination, and does not switch ordinary compiler output. The emitted
x86 integration test passes (1 selected test, 72 filtered, 29.41 seconds).
Inventory is 20621 candidate sites across 317 files, not compiler coverage.

The mandatory C ASan/UBSan and TSan integration also passes with this OS-thread
contention fixture. Strict C warnings, formatting, whitespace and inventory
freshness checks pass.


### Pressure-to-request connection

`gc_cycle_pressure_locked` now connects a coherent heap allocation forecast to
the cycle ledger in C and the shared direct emitter. Its additive 24-byte
`CyclePressure` input borrows heap/sample snapshots and a forecast horizon;
ABI v4 existing layouts are unchanged. A fresh ticket selects Young or Major.
QUIET leaves the ledger and ticket unchanged. Pressure issues a coalescible
request using the ledger's existing nonwrapping IDs; an active cycle cannot
satisfy a request made after its start. Invalid forecasts and exhausted IDs
leave the ledger/ticket unchanged. The caller owns the request lock and either
a coherent private heap snapshot or the heap lock, and wakes the scheduler
outside locks. This helper does not choose a generation, start a collection,
finish a ticket or prove epoch/color reuse safety.

The OS allocation pipeline now uses this operation for its measured-demand
request, replacing caller-side REQUEST dispatch. C/generated conformance checks
108 valid/rejected cases including both lanes, coalescing, active cycles,
quiet, saturation and malformed inputs. ARM independently checks 192 complete
memory states. Native 73 tests pass (30.97 seconds), ABI 8 pass, and Clippy with
denied warnings passes. The Mach-O fixture exports 129 helpers; ARM emulation
reports 15749 helper calls, 27 API-model scenarios and 1536 context transitions.
Inventory is 20650 candidate sites across 320 files, not compiler coverage.
Formatting, strict C and whitespace checks pass. Native Darwin/Windows runs,
cycle execution, adaptive policies and default generational activation remain
unverified or incomplete; the ordinary runtime remains legacy.

Mandatory C ASan/UBSan and TSan integration also passes with the connected
pressure/request operation; inventory freshness passes.

Workspace regression after pressure/request integration: 797 passed across 25 test invocations, zero failures and zero ignored tests. This includes mandatory C sanitizer integration and the current CLI/LLVM/native suites. Workspace Clippy (`--all-targets --all-features -- -D warnings`) also passes. Host-gated/cross-OS behavior is not established by this Linux run. Logs: `/tmp/klassic-cycle-pressure-workspace.log` and `/tmp/klassic-cycle-pressure-workspace-clippy.log`.


### Cycle request to prepared MarkStart admission

`gc_cycle_mark_start_locked` and its shared direct emitter now admit a pending
cycle ticket and the prepared mark-start rendezvous together. The caller holds
both request and registry locks, or owns startup exclusively. A private ledger
preview validates START without changing shared progress. MarkStart is the last
fallible operation; only its success commits the cycle ID and output ticket.
No request, an active cycle, busy handshake, invalid prepared state or exhausted
counters leave shared state unchanged. The additive 16-byte `CycleMarkStart`
input borrows the root registry and phase control; existing ABI v4 layouts are
unchanged.

The OS-backed allocation owner uses this entry for both fresh-heap mark starts.
Its measured pressure later requests Young cycle 2 while cycle 1 remains active,
instead of creating an unrelated fresh ledger. The Major lane begins Old mark
but still owes the full major cycle: neither this entry nor mark-end is a
completion certificate. The fixture deliberately never FINISHes these tickets.
Epoch/queue preparation, Young remset flip and safe color reuse remain caller
obligations; a request is not proof of those conditions. Full cycle execution,
adaptive scheduling and ordinary generational compiler startup remain open.

C/generated cases independently check 40 admission/nonmutation outcomes. Native
73 tests pass (27.92 seconds), ABI 8 pass (0.47 seconds), and native Clippy with
all targets/features and denied warnings passes. The 130-helper Mach-O fixture
passes ARM emulation, including 120 independent full-memory admission cases:
15869 helper calls, 27 API-model scenarios and 1536 context transitions overall.
These are not native Darwin execution or hardware-ordering evidence. Inventory
is 20697 candidate sites across 323 files, not compiler coverage. Strict C,
formatting and whitespace checks pass. The previous 797-test workspace result
precedes this admission change and is not its full-workspace validation.

Mandatory C ASan/UBSan and TSan integration passes in 127.56 seconds with
this admission path; inventory freshness also passes.


### Requested-cycle admission in the concurrent graph owner

The existing OS-backed graph owner now uses `gc_cycle_mark_start_locked`, not
a separate raw MarkStart call. Its two registered marker threads each submit a
request under the common metadata/request lock before startup. Both requests
coalesce into cycle 1. After mark activation, both submit new requests that
coalesce into cycle 2 while cycle 1 remains active. Their tickets remain live
until worker teardown and POLL continues to return WAIT.

This runs in the actual graph fixture with bounded root inventory/journal scans,
SATB work, Young remembered-set claims, source relocation, mark queues/spills,
phase retries and termination checking. Across 64 rounds (alternating Young and
Old mark), Select must leave started=1, requested=2, completed=0 in the selected
lane and preserve the unused lane. Thus the request/start path is connected to
nonempty concurrent marking, while mark termination is explicitly prevented
from being mistaken for full-cycle completion. The fixture still owns phase
preparation and controlled relocation/lifetimes; it is not a production cycle
executor, Major completion proof or ordinary compiler activation.

The emitted x86 integration test passes (1 selected test, 72 filtered, 25.98
seconds). Shared emitter code and ABI are unchanged by this integration; the
prior ARM validation belongs to the unchanged helper bodies, not native ARM
execution of these threads. Strict C warnings, formatting and whitespace checks
pass. Inventory is 20719 candidate sites across 323 files, not coverage.

Mandatory C ASan/UBSan and TSan integration also passes with both worker
request paths; inventory freshness passes.


### Select-time Young survivor accounting

`gc_marked_survivor_age_locked` and its shared emitter now require a Young
queue and an exactly matching Young mark epoch before applying the existing
idempotent survival-age operation. Older stamps return NOT_SURVIVED unchanged;
future stamps and Old queues are rejected. The caller still proves termination,
canonical Young generation membership and native record lifetime under the
queue/cycle lock. NOT_SURVIVED is not a reclamation certificate. No shared ABI
layout changes.

The actual graph owner's post-Select heap/key traversal now applies this entry
only after guarded canonical header identification and guard exit. It retains
the page/cycle lifetime and acquires the queue lock before reading stamps or
updating age. Forwarded physical copies are skipped; each logical canonical
record is accounted once, including unmoved objects. Repeating a survivor entry
in the same epoch leaves its record unchanged. Across 64 rounds, Young rounds
age exactly records 0..4 and 7; dead Young record 5, Old record 6 and every Old
mark round retain zero age. Canonical Young live block bytes equal the actual
array payload plus its header and five aligned scalar/record blocks. Thresholds
one and three exercise promote/retain decisions, not physical promotion here.

C/generated cases cover 76 stamp/generation/retry/rejection outcomes. Native
73 tests pass (27.82 seconds); native Clippy with denied warnings, strict C,
formatting and whitespace pass. Mach-O exports 131 helpers. ARM checks 725 new
independent full-memory cases, with 16594 helper calls, 27 API-model scenarios
and 1536 context transitions overall. This is emulation, not native Darwin
execution. Inventory is 20757 candidate sites across 323 files, not coverage.

The selection traversal still runs in the fixture's exclusive post-join
lifetime, rather than a production concurrent cycle. Full survivor accounting
across new allocations/pinning/promotion, adaptive thresholds, page selection,
cycle completion and ordinary compiler/runtime activation remain incomplete.

Mandatory C ASan/UBSan and TSan integration also passes with marked survivor
accounting and the graph selection checks; inventory freshness passes.


### Guarded block size in selection accounting

`gc_guarded_block_size` now returns allocator-compatible physical bytes through
C and the shared direct emitter. The common canonical/published guard validator
bounds the header before reading size. Result bytes include the 32-byte header
and rounded payload, with a minimum 16-byte payload allocation even when logical
payload is zero. Zero signals invalid access. No managed pointer escapes and no
state changes; the existing guard and lease remain caller-owned.

The graph Select traversal now obtains identity with `gc_guarded_identity` and
block bytes with this new entry, removing its direct managed-header inspection.
Only the borrowed nonmoving record and scalar size cross guard cleanup before
queue-locked survivor accounting. A call after EXIT must return zero. The same
real graph still checks exact logical identity coverage, once-only Young age
updates and expected live block bytes, including skipped forwarded copies.

C/generated tests cover 104 empty/rounded/guard/rejection cases. Native 73 tests
pass (25.65 seconds); native Clippy, strict C, formatting and whitespace pass.
Mach-O exports 132 helpers, and ARM independently checks 432 additional complete
memory states: 17026 helper calls, 27 API-model scenarios and 1536 context
transitions overall. Existing shared ABI layouts are unchanged. Inventory is
20784 candidate sites across 323 files, not compiler coverage.

This supplies a reusable guarded size accessor and connects it to the selection
fixture. Concurrent production page selection, relocation policy, full cycle
completion, ordinary compiler activation, native Darwin/Windows execution and
performance gates remain incomplete or unverified.

Mandatory C ASan/UBSan and TSan integration passes in 122.13 seconds for the
guarded size/selection connection; inventory freshness also passes.


### Marked survivor to relocation admission

`gc_marked_relocation_claim_locked` now joins a cycle-owned selection receipt to
the existing relocation gate/claim operation in C and the shared direct emitter.
The additive 72-byte `MarkedRelocation` holds a fresh embedded claim plus queue,
native record and captured epoch. It rejects changed queue epochs and future
stamps; an older stamp returns NOT_SURVIVED without consuming gate serials or
permits. Only a matching generation mark reaches relocation admission, whose
winner/loser/closed/retained/forwarded behavior remains unchanged. Existing ABI
v4 layouts are unchanged. The cycle must still prove termination, canonical
source-to-record correspondence, source generation membership, pin eligibility,
phase/color safety and lifetime protection; mark bits alone prove none of those.

The actual graph Select walk now captures a canonical Young survivor candidate
with its record while guarded. All 32 Young rounds check that a closed gate
rejects it without mutation. In 16 threshold-one rounds, the owner retains a
real metadata lease, reopens admission, claims that survivor and closes/drains
its source. The real Old destination has a full forwarding table, so target
preparation returns METADATA_REFILL without preparing any private range. The
owner retains/reopens the source, releases the gate permit and metadata lease,
then closes admission and recaptures its epoch. The subsequent full heap audit
runs over that retained object successfully. This tests failed promotion
placement; it does not claim that the object was promoted or a cycle completed.

C/generated selection/admission tests cover 68 outcomes. Native 73 tests pass
(26.82 seconds), ABI 8 pass (0.45 seconds), and native Clippy, strict C, formatting
and whitespace checks pass. ARM independently checks 114 complete memory
states; after correcting the test fixture's gate field order, all 17140 helper
calls, 27 API-model scenarios and 1536 context transitions pass. Mach-O exports
133 helpers. Mandatory C ASan/UBSan and TSan integration passes in 123.74 seconds.
Inventory is 20838 candidate sites across 323 files, not compiler coverage.

Production candidate scheduling and successful Select-driven placement, complete
cycle/retirement ownership, ordinary runtime activation, native cross-OS
execution and performance gates remain incomplete or unverified.

### Select-driven successful Young survivor placement

The joined-worker graph now follows its marked, aged Select candidate through
successful Old placement in eight threshold-one rounds. Eight other rounds
retain the metadata-full rejection and source retention path; all 32 Young
rounds still reject a closed gate without mutation. The target table has two
charged native cells in success rounds and one in failure rounds.

After source admission closes and readers drain, copying runs outside the heap
lock in seven one-byte slices. Every slice checks that forwarding remains
claimed. Target installation and descriptor-based promotion bookkeeping precede
the receipt-checked publication/finish/permit-release funnel. The chosen object
has seven scalar bytes and no reference slots, so promotion does not add remset
bits. Old marking is explicitly idle in these rounds; this does not establish
overlapping Old-mark promotion. Both metadata leases precede reference
resolution, and the stale source reference reaches the installed Old copy.
Under its canonical guard the test checks the original MarkRecord/ObjectId,
48-byte physical size and a seven-byte payload pattern (including zero).
The following reusable whole-heap audit sees the new copy and skips its forwarded
source with balanced leases. No source payload reclamation is inferred.

This connects existing C/shared emitted operations in the Select fixture; no
new helper or ABI layout was added. Production selection/scheduling and complete
cycle ownership, reference-bearing survivor promotion from Select, default
compiler/runtime activation, native cross-OS execution and performance gates
remain incomplete or unverified.

Validation: all 73 native tests pass (25.09 seconds), including the shared
emitted graph; native Clippy and strict C compilation pass. The final one-byte
copy version also passes mandatory C ASan/UBSan and TSan integration. Formatting,
whitespace and inventory freshness pass. The inventory contains 20852 candidate
sites across 323 files and remains an audit index, not compiler coverage.

### Reference-bearing survivor placement from Select

The graph Select walk now captures a second canonical, marked Young candidate:
logical object 3, whose reference field names live Young object 4. Its native
record, forwarding cell and source page are captured under the same canonical
identity/lifetime rules as the scalar candidate. In eight successful placement
rounds both candidates are promoted; the charged Old target table has three
cells (the existing moved owner and two promoted survivors). Eight scalar
metadata-full retention rounds remain, and 40 candidate attempts check closed
gate rejection without mutation.

Each candidate gets its own stable permit/claim. The reference payload is copied
in eight one-byte slices while source access remains closed and forwarding
unpublished. After target installation, descriptor-driven promotion records the
new Old slot in the current remembered bitmap before publication. The test checks
the exact current bit, absence of its previous-bank bit, and a one-slot count
increase; repeating the completed scan leaves that count unchanged. The existing
scalar path still records no slots and checks its seven-byte payload pattern.
Both paths preserve native identity and physical size through stale-reference
resolution and canonical guards.

After publication a generated guarded heap-audit slice reads the promoted
reference slot and verifies the unchanged reference to object 4, its Young page
and completed Young mark. The diagnostic observation is consumed without being
used as a rooted value or dereferenced. The subsequent whole-pool audit includes
both promoted destinations and skips both forwarded sources, with balanced
metadata leases. There are 16 successful promotions (eight scalar, eight
reference-bearing) and eight retained-source cases.

These are current-epoch retention and copied-edge checks in the Select owner
fixture, not a subsequent Young-cycle liveness proof. Null-slot selection,
Old-mark overlap at this connection, cycle completion/reclamation, ordinary
runtime activation, cross-OS execution and performance gates remain open. No
new leaf helper or ABI layout was introduced.

Validation: native 73 tests pass (26.48 seconds), including the mandatory new
reference-promotion marker. C ASan/UBSan and TSan integration passes (122.65
seconds). Native Clippy, strict C compilation, formatting, whitespace and
inventory freshness pass. Inventory: 20868 candidate sites across 323 files,
not barrier-coverage certification. AArch64 helper code was unchanged; this
turn does not add native Darwin or Windows execution evidence.

### Select to concurrent relocation adoption

`gc_relocation_phase_step_locked` adds the missing Select-to-relocation phase
boundary in C and the shared direct emitter. The additive 56-byte ABI-v4
`RelocationPhaseControl` captures handshake, gate, selected generation, state,
rendezvous, snapshot and gate epoch. BEGIN requires Select and a closed/drained
gate, advances the selected remap bit and publishes RelocateStart with a
rendezvous. ACTIVATE waits for managed-owner acknowledgements, then publishes
ConcurrentRelocate and immediately starts the second rendezvous under the same
lock. RELEASE waits for adoption before reopening admission and releasing the
handshake. Native owners adopt on reentry. The other generation's phase and
epoch bits remain intact; its relocation/retirement excludes admission.

All steps are bounded metadata operations. Worker acknowledgements continue to
reject live address guards and counted private producers. Changed snapshot,
rendezvous or gate epoch rejects stale continuation without mutation. BEGIN
reserves two phase/handshake sequence increments and a gate epoch increment;
exhaustion never wraps. The owning scheduler still proves selection/pin safety,
remap-color reuse and exclusive generation ownership before BEGIN. This helper
starts relocation, not source retirement or cycle completion.

The joined-worker graph now runs both adoption boundaries after canonical
Select enumeration and before selected-survivor copies in all 64 rounds. Its
remaining registered owner must ACK each boundary. It captures the new snapshot
and reopened/closed gate epoch for the following placement and whole-pool audit.
Sixteen successful promotions and eight retained-source cases still run, and
all request ledger tickets remain unfinished. First-use fixture colors and
cycle-held native/object lifetimes are explicit; no address reuse is inferred.

The C/candidate suite checks 1228 whole-fixture outcomes: both generations, all
32 epoch combinations, managed/native adoption, gate exclusion, stale controls,
invalid state, exact nonmutation, empty/native-only cohorts and final reservable
counters. The first AArch64 lowering run exposed a zero-bit shift rejected by its
assembler; the emitter now omits that shift. Native execution passes after the
fix. The Mach-O helper export list was also extended to include this entry.

Retire/Idle publication with complete root/heap/buffer/job retirement proofs,
full cycle completion, production selection and default compiler/runtime
activation remain incomplete. Actual cross-OS execution and performance gates
remain unverified.

Validation: 73 native tests pass (24.90 seconds), ABI 8 tests pass (0.44 seconds),
and mandatory C ASan/UBSan plus TSan integration passes (129.35 seconds). Native
Clippy, strict C, formatting, whitespace and inventory freshness pass. The updated
Mach-O export fixture passes with 134 helpers. ARM independently checks 4220 new
whole-memory relocation-phase cases; all 21360 helper calls, 27 API-model
scenarios and 1536 context transitions pass. This is emulation, not Darwin or
hardware-ordering evidence. Inventory: 21011 candidate sites across 326 files.

### Concurrent relocation to Retire adoption

`gc_relocation_retire_step_locked` now publishes the ConcurrentRelocate-to-Retire
boundary in C and the shared direct emitter. It uses a fresh, distinct control
with the existing `RelocationPhaseControl` layout; no ABI fields changed. BEGIN
requires the selected generation in ConcurrentRelocate and a closed, drained
relocation gate. The scheduler still owns the proof that every selected move
was published or retained with cleanup complete. BEGIN advances the snapshot
sequence, preserves all color bits and the other generation's marking state,
and starts owner adoption. RELEASE waits for all managed acknowledgements and
ends the handshake without reopening admission. Native owners adopt on reentry.

A changed phase, handshake generation or gate epoch rejects continuation without
mutation. The final gate epoch is allowed because this boundary never reopens
or increments it. Snapshot/handshake exhaustion rejects BEGIN, while an already
acknowledged boundary at the final counter can RELEASE. Common pointer/phase/
handshake validation is shared with the relocation-start coordinator.

All 64 graph rounds now adopt Retire after selected placements finish and before
the cooperative whole-heap audit. The following incoming-forwarding audit and
compression retain the same closed gate epoch. The previous fixture-only reopen
at the end of these audits was removed; root retirement and teardown run with
admission still closed. The cycle ledger remains unfinished, including rounds
whose heap audit finds stale source-range references. Retire adoption alone is
not a reference-retirement certificate, virtual reuse permission or cycle FINISH.

The C/candidate suite adds 840 exact-state cases, including real pending-permit
release, guard/private-producer ACK rejection, managed/native cohorts, retained
closed admission, unchanged colors, stale continuations and counter endpoints.
The graph adds a mandatory 64-round Retire/admission/unfinished-ticket marker.
Whole-source root/buffer/job/handle proofs, source metadata retirement, Idle and
cycle completion, ordinary runtime activation, native cross-OS execution and
performance gates remain incomplete or unverified.

Validation: 73 native tests pass (24.74 seconds), native Clippy and strict C
compilation pass, and mandatory C ASan/UBSan plus TSan integration passes
(125.66 seconds). ARM independently checks 2624 new exact-memory Retire cases;
all 23984 helper calls, 27 API-model scenarios and 1536 context transitions pass.
Mach-O exports 135 helpers. Formatting, whitespace and regenerated inventory
freshness pass (21086 candidate sites across 326 files). ARM emulation does not
establish native Darwin execution or hardware ordering.

### Rooted repair of retired-source heap findings

The graph Retire owner now connects a heap-audit finding to registered receiver
and output roots, registered load admission and the shared rooted-load/self-heal
controller. The audited page lease and cycle-held object lifetime protect receiver
root publication. The audit's observed reference remains diagnostic: repair
rereads the field under a canonical guard and retains that exact observation in
its output root before resolving and CAS-healing. Rooted load steps respect their
HEAP/ACCESS lock domains and finish private-producer accounting before root
cleanup. A two-entry journal forces clean flush/retry during resolution; the
registered journal is copied and acknowledged while both roots remain owned.
The returned canonical object's MarkRecord is checked under a fresh guard.

Eight promotion rounds repair three actual incoming heap edges each: two to
promoted object 3 and one to promoted object 7. Each range is the old source's
aligned user granule, not its whole mixed-liveness page. The first cooperative
whole-pool pass remains DIRTY with the original findings even after repair.
Only a fresh second pass reports zero remaining references to that range.
Twenty-four fields are repaired and sixteen independent range re-audits are
clean; scratch roots and their registered segment retire with journals consumed,
no private producers and balanced leases. Move admission remains closed.

This establishes the connection between audit, rooted repair and fresh audit in
a joined-worker Retire fixture. It does not establish concurrent snapshot
exclusion, root/buffer/job/handle/forwarding retirement, reclaimability of a page
containing other objects, full cycle completion or default runtime activation.
Both marks are inactive during these repairs; an overlapping Old mark needs its
queue admission/handoff. Existing rooted-load race tests remain separate from
this frozen heap pass. No ABI layout or generated helper changed.

Validation: native 73 tests pass (28.52 seconds), including the required repair
marker. Mandatory C ASan/UBSan plus TSan integration passes (131.86 seconds).
Native Clippy, strict C, formatting, whitespace and inventory freshness pass.
Inventory: 21139 candidate sites across 326 files. Generated helper bodies were
unchanged; this turn adds no native foreign-OS or hardware-ordering evidence.

### Retire root inventory, repair and revision re-audit

`gc_root_retire_audit_step_locked` and its shared direct emitter reuse the leased
root inventory walker with a separate Retire entry. BEGIN requires the selected
queue's generation to be in Retire. CHECK/OWNER require the exact captured phase
snapshot as well as unchanged scope inventory, queue generation and cycle epoch.
The marking entry retains its existing phase/parity rules. Both entries preserve
one pre-scan identity/revision sample per owner and generation, reject unfinished
root items and journals on NEXT, and wait for retiring owner segments. The ABI
layout is unchanged; a pass must use one entry consistently. CHECK and every
OWNER check still require producers stopped by the same handshake. They certify
inventory/revision stability, not range absence or complete source retirement.

The graph retains two registered roots to already-marked objects 3 and 7 before
selected relocation. In eight promotion rounds, a Retire walk snapshots each
root into its counted pending item, follows forwarding with destination metadata
acquired before source release, and heals the root by exact CAS preserving its
observed color. Resolution in this pass does not offer to an inactive mark queue.
The cycle owns all logical object and metadata lifetimes in this joined-worker
fixture; that ownership is not inferred from a metadata lease.

Sixteen roots are repaired. A same-value logical store after each initial scan
increments root_revision and journals the update: final owner checks under a
real handshake reject all eight old certificates. After consuming the journal,
a fresh pass finds neither promoted object's old source user granule in any
registered root. Final CHECK and the sole remaining owner's certificate pass
under one handshake, and both temporary root segments retire with balanced
leases. Admission remains closed and the cycle ticket remains unfinished.

C/candidate tests cover 20087 exact-state inventory, phase/color, revision,
journal and pending-item cases; the graph repair marker is mandatory in both
native and sanitizer harnesses. This supplies root repair and revalidation for
the frozen post-move fixture. General concurrent old-reference introduction,
other-generation marking handoff, buffers/jobs/handles/forwarding retirement,
virtual reuse, full cycle completion, default compiler activation and actual
foreign-OS/performance gates remain unfinished or unverified.

Validation: 73 native tests pass (28.67 seconds), including the required Retire
root repair and audit markers. Mandatory C ASan/UBSan plus TSan integration
passes (135.12 seconds). Native Clippy and strict C compilation pass. ARM checks
26126 new independent exact-memory cases; all 50110 helper calls, 27 API-model
scenarios and 1536 shared-context transitions pass. Mach-O exports 136 helpers.
Formatting, whitespace and inventory freshness pass: 21213 candidate sites
across 326 files. Logs: `/tmp/klassic-root-retire-{native,sanitizer,arm,clippy}.log`.
These checks do not establish native Darwin/Windows execution or hardware ordering.

### Retire owner certification includes context-held work

The Retire OWNER check now requires zero private producers, no active SATB or
remembered consumer serial/end/cursor, and `consumed == published == top` for
both buffers. Reversed cursor ordering is INVALID; unpublished, published or
copied work is WAIT. Historical consumer last_serial and equal exhausted buffer
counters remain valid. These checks run before the no-root-segments shortcut,
so a rootless native owner cannot hide buffered references. The caller must
still hold every producer stopped throughout the owner checks and retirement
decision. The marking root-audit entry keeps its previous contract; mark end
already separately checks its context-held work.

The joined-worker graph now adds a conservative SATB transport entry holding an
actual old source reference and metadata token after root repair. Both marks are
inactive and the cycle already owns that live object's lifetime. Eight real
handshakes request publication before ACK. After publication and again after
registered CLAIM/COPY, the Retire owner check returns WAIT (sixteen checks).
The consumer follows the old reference to its promoted canonical object, checks
its stable MarkRecord, releases the transferred metadata token and ACKs the
registered job. The unchanged root certificate then passes in a new handshake.
This is a conservative transport-holder test, not a claim that normal Retire
stores generate SATB work or that overlapping Old marking has been discharged.

The C/candidate audit suite now has 20257 cases, including rootless owners,
private producers, both consumer controls, both buffers, invalid counter order
and exhausted equal counters. No ABI layout or new helper export is needed.
Global queue/external-job/weak/native-handle/forwarding proofs, safe reuse,
cycle FINISH and default runtime/compiler activation still remain open.

Validation: 73 native tests pass (28.04 seconds); mandatory C ASan/UBSan plus
TSan integration passes (132.27 seconds). Native Clippy, strict C compilation,
formatting, whitespace and inventory freshness pass. ARM independently checks
26330 Retire audit cases; all 50314 helper calls, 27 API-model scenarios and 1536
context transitions pass. Inventory: 21257 candidate sites across 326 files.
Logs: `/tmp/klassic-retire-holders-{native,sanitizer,arm,clippy}.log`. The native
foreign-OS and performance gates are not established by these checks.

### Rendezvous-bound Retire inventory predicate

`gc_retire_inventory_check_locked` and its shared direct emitter use the existing
MarkEndRequest layout for a read-only Retire predicate. A common implementation
preserves mark-end behavior while selecting the strict Retire audit entry. The
predicate requires the exact requested active/completed handshake, zero root
retirement/readers/walkers, a valid completed Retire audit, an empty selected
mark queue and drained remembered flip/pending/claim/walker controls. Remembered
phase/parity and drainage are checked for either retiring generation.

Every thread registration is visited under the common registry lock. Membership,
back links, descending identities, context identity, managed ACK/adopted phase,
native membership counts and the owner's Retire certificate must agree. The
owner certificate includes private producers and both buffer/consumer controls.
Empty membership is supported as a real completed rendezvous. All outcomes leave
every byte of the supplied owners unchanged, including the original audit.
The caller holds producers stopped and the lock through its combined decision.

The graph now uses this predicate instead of certifying only its remaining
coordinator context: eight changed root inventories return STALE, sixteen
published or copied jobs return WAIT, and eight completed inventories return
READY after canonical processing/metadata release/ACK. Existing heap/root range
checks and the closed relocation gate remain separate. C/candidate coverage adds
5435 cases covering two registered owners, managed/native combinations, both
generations, all colors, rootless owners, malformed membership, queued/in-flight/
spill work, remembered flip/take/ACK and stale handshakes. Mach-O exports 137
helpers without an ABI layout change.

READY is an inventory predicate, not a retirement or cycle-completion token.
The other generation's queue, heap-range stability, relocation admission,
external jobs, weak/native handles, incoming forwarding and source leases/reuse
still require the owning runtime's proofs. No phase change, gate reopening,
source reclamation or cycle FINISH is added here. Ordinary compiler/runtime
activation and G01-G15 completion remain open.

Validation: native 73 tests pass (26.43 seconds), mandatory C ASan/UBSan plus
TSan integration passes (133.73 seconds), and native Clippy/strict C compilation
pass. ARM checks 3230 new independent read-only inventory cases; all 53544 helper
calls, 27 API-model scenarios and 1536 context transitions pass. Formatting,
whitespace and inventory freshness pass: 21291 candidate sites across 326 files.
Logs: `/tmp/klassic-retire-inventory-{native,sanitizer,arm,clippy}.log`. This is not
native foreign-OS execution, hardware-ordering or latency evidence.

### Whole-pool source-range audit composition

The joined-worker graph previously checked incoming forwarding in two named
source tables. It now additionally enumerates every published heap head with
`HeapWalk`, retaining one metadata lease while `ForwardIncoming` scans its table
outside the lock with a one-entry budget. Each pass checks all five heads and
all captured keys, including both OS-decommitted source runs; no protected
payload is read. The coordinator excludes allocation, table replacement and
compression across each complete pass as well as keeping relocation admission
closed at the captured epoch/phase. Gate drainage alone is not this exclusion.

For each of 32 Young rounds, full-pool scans find exactly one edge into each of
two obsolete source runs before compression. Fresh scans of every table find
zero edges afterward. Fresh cooperative whole-heap payload audits then find no
references into either complete run (64 heap audits). The root walk now accepts
explicit bounded ranges rather than hard-coding the two promoted object offsets;
a fresh walk excludes both source runs, followed by the all-owner Retire
inventory predicate under an actual completed handshake (32 checks). Leases are
balanced, admission stays closed, and the selected cycle remains unfinished.
The previous promotion-granule repair tests still exercise their original ranges.

The required C/native marker therefore covers 64 positive forwarding findings,
64 clean whole-pool forwarding re-audits, 64 clean heap re-audits and 32 combined
root/inventory checks. This closes a coverage gap in the frozen graph fixture;
it does not establish concurrent old-reference exclusion in ordinary programs.
Native diagnostic references and the independently charged table backing remain
fixture-owned until teardown. No source metadata release, virtual range reuse,
color reuse or cycle FINISH is claimed. No shared helper body or ABI changes.

Validation: native 73 tests pass (28.77 seconds), mandatory C ASan/UBSan plus
TSan integration passes (134.55 seconds), and strict C compilation/native Clippy
pass. Formatting, whitespace and inventory freshness pass: 21339 candidate sites
across 326 files. Logs: `/tmp/klassic-source-ranges-{native,sanitizer,clippy}.log`.
Shared emitted helper bodies are unchanged; this adds no new ARM model or
foreign-OS execution evidence. The remaining release gap is concrete:
`gc_page_release_locked` only accepts never-installed READY runs, whereas
`gc_source_uncommit_locked` retains source metadata and virtual ranges.

### Bounded uncommitted-source metadata retirement

`SourceRetire` adds a 64-byte native transaction to ABI v4 without changing
existing layouts. `gc_source_retire_locked` and the shared emitter accept an
already-UNCOMMITTED run only after the owning cycle has proved absence of every
current/future old-range reference, including both queues, roots/heap, external
jobs/handles/weak work, incoming forwarding and color/admission obligations.
The leaf does not produce that proof from a zero lease count.

BEGIN waits for metadata leases, binds the head owner to the exact stable request
address, transfers start-bitmap/forward-table cleanup ownership and publishes
RELEASING. New metadata acquisition is then rejected. Cleanup runs outside the
lock; STEP waits until both output pointers are cleared after successful cleanup.
Forward-table backing keeps its independent MetadataCharge. Each STEP frees at
most one tail plus the head on the final slice; bitmap budget is credited once
at that final slice, and payload commitment is unchanged because uncommit has
already credited it. Copies/replays and malformed next-tail metadata are rejected
without mutation. The request retains progress and ownership across retry.

A freed tail can be reserved under a new incarnation before the older head's
transaction completes. The full-range proof must therefore precede BEGIN, not
be assembled per released tail. Later STEP calls never inspect or overwrite an
already-freed prefix. C/candidate tests verify a real pool reservation interleaved
between slices, with unchanged new owner metadata and correct pending budgets.
This avoids an O(run-pages) critical section when publishing Free states.

The 211 C/candidate cases check exact memory effects, lease and cleanup waits,
unique request binding, geometry/budget rejection and prefix reuse. A separate
exclusive OS-backed fixture moves one scalar object, updates its only native
logical root to the canonical destination, audits both forwarding tables and
uncommits the source while retaining a metadata lease. After releasing that
lease, it frees the real bitmap/table, releases the table charge, retires the
source and reserves/commits the same virtual range for a different ObjectId.
Across sixteen rounds the original canonical copy retains its MarkRecord and
payload. Pool/budget/uncommit/retire use candidate callbacks; relocation setup
uses the C reference leaves. This fixture has no other actors, pointer payloads,
queues or weak/native handles; it supplies an exclusive lifetime proof rather
than the production concurrent-cycle protocol.

The general joined-worker graph still retains its aggregate table allocation and
native diagnostic references until teardown. It has not been switched to this
cleanup transaction. Full concurrent source/color reuse, Retire-to-Idle/FINISH,
ordinary compiler/runtime activation and G01-G15 completion remain open.

Validation: native 73 tests pass (29.87 seconds), ABI 8 tests pass (0.45 seconds),
and mandatory C ASan/UBSan plus TSan integration passes (133.96 seconds). Native
Clippy and strict C compilation pass. ARM independently checks 470 new exact-state
cases including 128-page sliced release and malformed tails; all 54014 helper
calls, 27 API-model scenarios and 1536 context transitions pass. Its initial
fixture transposed AllocationPage identity_base/owner; correcting the fixture to
the mechanically checked ABI resolved that test failure. Mach-O exports 138
helpers. Formatting, whitespace and inventory freshness pass: 21542 candidate
sites across 330 files. Logs: `/tmp/klassic-source-retire-{native,abi,sanitizer,arm,clippy}.log`.
These checks add no native Windows/Darwin or performance certification.


### Joined-worker graph: audited source metadata cleanup

The two intermediate Old-owner forwarding tables now have separate native
allocations and MetadataCharges instead of sharing GraphStorage with live
records and tables. The existing exact-fit GraphStorage admission/rollback
checks remain; the two additional charges are reserved before allocation and
committed before table publication.

In the 32 Young rounds, both whole-source-range forwarding and heap audits,
the root audit and the all-owner Retire inventory predicate precede source
retirement. Under the same completed rendezvous, after checking closed/drained
relocation admission, the coordinator abandons diagnostic chain references and
terminal relocation receipts and invokes SourceRetire BEGIN for both runs.
Workers are already joined, only the coordinator remains, and this fixture has
no other-generation work queue, weak handles or native handles. This exclusive
fixture ownership is part of the proof; the predicate alone does not exclude
those holders in a production runtime.

After ending the rendezvous, native bitmap/table backing is freed outside the
heap lock. The table charge is released and SourceRetire STEP publishes Free
under the lock. Across 64 source runs, bitmap/table metadata credits balance
exactly, payload committed bytes receive no second credit, and the surviving
canonical owner retains its MarkRecord, ObjectId and reference payload. The
live destination's remembered membership still refuses premature unlink.
Old-cycle rounds retain their live table allocations until fixture teardown.
Both the C and generated-code harness require the new cleanup completion marker.

This connects the earlier full-range audits to real metadata cleanup before
graph teardown. It does not prove general concurrent source/color reuse or
Retire-to-Idle/FINISH. The cycle ticket remains unfinished; default compiler,
runtime integration and all G01-G15 completion gates remain open.

Validation: native 73 tests pass (30.60 seconds), and mandatory C ASan/UBSan
plus TSan integration passes (134.07 seconds). Native Clippy, strict C syntax,
formatting, whitespace and inventory freshness pass: 21579 candidate sites
across 330 files. Logs: `/tmp/klassic-graph-retire-{native,sanitizer,clippy}.log`.
No ABI or emitted helper body changed, and no new native Windows/Darwin or
performance certification is claimed.


### Rooted scalar access owner

`RootedScalar` adds a 176-byte unique native control to ABI v4. The C owner
`gc_rooted_scalar` and shared direct emitter connect a retained receiver root to
canonical RootAccess, one typed scalar load/store, guard exit and metadata
release. ENTER and RELEASE use the heap lock; ACCESS and EXIT run outside locks
without polling or waiting. A forwarding edge updates the same root before a
clean retry, so the caller can poll or flush without retaining an interior
address. Scalar bits, including reference-shaped integers, never enter SATB,
remembered or marking paths.

The access step executes once. Its result and value survive failed EXIT or
RELEASE; replayed access requests request cleanup and cannot overwrite a later
scalar store. ABORT before access skips the operation, while late ABORT preserves
the committed result and still requires cleanup. A completed request cannot be
reset or replayed. Null ENTER returns a clean null-receiver outcome. The caller
retains the root and immutable bindings throughout, and drives lock domains and
scheduling; this owner does not attach threads, release roots or run a cycle.
Unlike a reference load, it discovers no private managed referent requiring
mark-admission accounting. A held receiver guard still excludes relocation copy
and phase/native transitions until EXIT.

The shared C/candidate suite exercises 768 OS-backed schedules across phase,
generation and operation variants. Forwarded-source payloads are decommitted
before canonical access. Cases cover busy forwarding, full root journals,
metadata exhaustion, typed reference-slot rejection, null, abort before/after
access, guard exit failure, metadata release failure and retained failed-ENTER
cleanup. Every access is replayed after an intervening scalar store to verify
that cleanup never repeats the operation. A separate ARM exact-memory oracle
checks payload and native-state effects rather than calling C as its oracle.
Ordinary compiler scalar call lowering and the generational runtime owner remain
unconnected; this is not default activation or completion of G05/G15.

Validation: native 73 tests pass (29.09 seconds), ABI 8 tests pass (0.44 seconds).
After adding the retained failed-ENTER cleanup schedule, the generated-code
owner suite passes again (31.03 seconds). Mandatory C ASan/UBSan plus TSan passes
(133.70 seconds). ARM passes 377 new independent exact-state cases, all 54391
helper calls, 27 API-model scenarios and 1536 context transitions; Mach-O exports
139 helpers. Native Clippy, strict C compilation, formatting, whitespace and
inventory freshness pass: 21695 candidate sites across 333 files. Logs:
`/tmp/klassic-rooted-scalar-{native,owner,abi,arm,sanitizer,clippy}.log`.
ARM emulation adds no native Darwin scheduling or hardware-ordering evidence.


### Existing-root value preparation for reference operations

`gc_rooted_value` and the shared direct `emit_value` entry reuse the rooted-load
mark/publication/cancellation owner for an already retained logical value. The
RootedLoad control layout and status/lock-domain protocol are unchanged. Both
root bindings name the same strong root, registry/field offset and value scratch
are zero, and the observation subcontrol supplies bindings only. The new entry
performs no receiver field read or self-heal. Mode-specific validation rejects
field inputs, mismatched roots, active receiver scratch and field-healing states
before changing producer accounting. Callers cannot switch entry points while a
request is live.

The shared owner captures required generation marks, resolves the root, publishes
mark/remap colors with remembered bits preserved, and balances its private
producer obligation on completion or cancellation. Existing registry-bound
admission and cancellation/rearm remain available. Null is a completed value,
not a null-receiver error. A DONE result is an owned root, not a raw address or a
permanent store-good certificate: the future store owner must preserve its own
phase admission, revalidate as needed, compose store colors and perform SATB and
remembered logging before publication. This implements desired-value preparation,
not the complete public store/CAS/exchange controller or compiler call lowering.

The shared OS-backed graph exercises 64 fresh dual-generation preparations with
real queue offers and null preparations, plus 1536 cancellation/rearm boundaries.
The earlier receiver field is null while the prepared root retains the child,
so successful preparation cannot come from rereading that field. Invalid
bindings leave requests and producer counts unchanged. Repeated preparations
preserve logical root revisions and balance every untransferred lease. An ARM
protocol model additionally compares complete fixture state for active-generation
combinations, cancellation, rearm, malformed inputs and a snapshot restart while
an offer is pending. It shares only independently tested leaf calls, not the
new dispatcher under test. Full runtime store integration remains open.

Validation: native 73 tests pass (28.84 seconds), mandatory C ASan/UBSan plus TSan
passes (136.50 seconds), and ARM passes 3586 new value-owner protocol comparisons
alongside the existing load owner. All 59606 helper calls, 27 API-model scenarios
and 1536 context transitions pass; Mach-O exports 140 helpers. No ABI layout
changed. Native Clippy, strict C compilation, formatting, whitespace and inventory
freshness pass: 21745 candidate sites across 333 files. Logs:
`/tmp/klassic-rooted-value-{native,arm,sanitizer,clippy}.log`.
No ordinary-program, native Windows/Darwin or performance completion is claimed.


### Prepared-root reference store commit

`gc_rooted_store_commit` and its shared direct emitter connect a completed
RootedMarkSet receipt to the existing guarded physical store. The caller retains
its own phase/private-producer admission continuously through preparation and
commit, preserves the desired root's logical referent, and owns the receiver and
observed-value roots plus distinct prepared entry leases. The receipt and a
nonzero producer count do not independently prove that caller ownership.

Under the receiver guard, the new entry validates the completed required marks,
context/snapshot and desired RootHandle, then atomically rereads encoded root
bits. It requests REPREPARE without mutation when mark/remap colors are stale.
For a valid root it composes the current store color, including remembered
parity, only in a local GuardedStore request and invokes the existing pre-barrier
and CAS. Null stays zero. The input desired scratch stays zero on every return;
only the transferred lease mask is copied back, including after a losing CAS.
No desired snapshot, heap address, root journal mutation, poll or lock escapes
this finite attempt. Later movement of the rooted referent is handled through
forwarding; copying encoded bits is not dereferencing a stale physical address.

The 2102 C/candidate cases use independent exact-effect checks for generation,
phase/color, previous/null values, root validity, mark receipts, stale colors,
buffer pressure and lease transfer. The existing OS-backed value pipeline now
holds a separate outer-store producer obligation before preparing the root,
drains its real mark offers, and performs 128 null-to-value and same-value
physical commits across 64 rounds. Real owner metadata leases pass into the
SATB buffer and a claimed/copied job before explicit fixture cleanup. This
exclusive fixture validates the transport and accounting; it is not production
buffer consumption or a cycle-completion proof. The direct path uses generated
value preparation and commit entries.

ARM independently checks 1055 full-state cases, including interference at the
CAS instruction after both logs are published. The losing CAS preserves the
competing field value and both transferred lease bits. Initial lowering exposed
the AArch64 signed frame-offset limit for the 160-byte temporary; indirect
access through a local frame pointer fixes it without changing the ABI.
The public store owner still needs to connect root observation, entry lease
admission, reprepare/full retries and cancellation through final cleanup.
Ordinary compiler/runtime integration and G01-G15 completion remain open.

Validation after the frame-access fix: native 73 tests pass (29.59 seconds).
The subsequently connected OS pipeline passes its generated-code test (33.61
seconds), and mandatory C ASan/UBSan plus TSan passes (139.41 seconds). ARM passes
1055 new commit cases and all 60661 helper calls, 27 API-model scenarios and 1536
context transitions. Mach-O exports 141 helpers. Native Clippy, strict C syntax,
formatting, whitespace and inventory freshness pass: 21879 candidate sites
across 336 files. Logs: `/tmp/klassic-root-store-{native,pipeline,arm,sanitizer,clippy}.log`.
These checks do not add native Windows/Darwin or performance certification.


## Rooted store attempt ownership (2026-09-28)

`RootedStoreAttempt` (368 bytes) now owns one physical store attempt in C and
shared direct code. ENTER obtains the receiver guard and two distinct buffer
entry leases under the already-held heap lock. ACCESS performs the prepared-root
commit once. EXIT drops the guard outside that lock; each subsequent RELEASE
performs at most one metadata release under the lock. Partial lease admission
requires guard exit before cleanup, including metadata exhaustion and corruption.
A completed attempt cannot be reset or repeat the payload update. ABORT before
ACCESS skips it; afterward it preserves the actual result and transfer mask.
Transferred tokens belong to buffers even when the physical CAS loses; the
attempt only releases its remaining obligations.

The OS-backed value pipeline now uses this owner for all 128 physical stores.
Its real buffer jobs claim, copy and acknowledge transferred entries before
resolving/releasing their tokens. The 416 C/candidate schedules also exercise
partial admission, failed exit/release, cancellation, replay and a consumer
finishing before the receiver lease is released. ARM has 745 independent
full-memory comparisons, including instruction-level CAS interference after
pre-barrier publication and subsequent cleanup with transferred leases intact.

This is one attempt, not the complete public store controller. The caller must
still retain receiver/desired/observed roots and continuous outer phase/private
producer admission. Root observation, flush/assist, reprepare and fresh-attempt
retry policy remain to be joined in that controller. No ordinary compiler path
has been switched to this ABI, and no G01-G15 capability is certified complete.

Validation: native 73 tests pass (30.16 seconds); the final pipeline integration
passes its emitted-code test (31.40 seconds). ABI 8 tests pass. Mandatory C
ASan/UBSan plus TSan passes (138.53 seconds). Full ARM replay passes 61330 helper
calls, 27 API-model scenarios and 1536 context transitions; its subsequently
expanded store-attempt model alone passes 745 comparisons. Mach-O exports 142
helpers. Native Clippy, strict C syntax, formatting and whitespace checks pass.
The access inventory is 22036 candidate sites across 339 files, not a coverage
certificate. Logs use `/tmp/klassic-store-attempt-`. Darwin/Windows native
execution and the required performance matrix remain separate unfinished gates.


## Composed rooted reference store (2026-09-28)

`RootedStore` (1712 bytes) now connects desired-root preparation, rooted field
observation and the physical store-attempt owner in C and shared direct code.
The runtime caller first admits its value child under the common registry lock.
On the first heap action the store acquires its own private-producer count;
value preparation releases only its nested count. The store retains its count
through observation, CAS and cleanup, including every clean retry. Terminal
cleanup releases exactly that count. Rejected malformed terminal controls do
not decrement another operation's count.

Every physical attempt starts from a fresh observation retained in a distinct
strong root. Receiver and desired logical referents remain fixed. CAS contention
releases the old guard and untransferred leases, resets the observation, and
reads again. Buffer/full metadata events request external consumption/assistance
only after guard cleanup. REPREPARE reconstructs a clean existing-root value
preparation while preserving its bindings and admitted snapshot. Queued mark
work and transferred buffer tokens retain independent ownership. These retry
points do not permit polling: cancellation must drain the operation first.
Preparation snapshot restart uses the existing registry admission and RESUME.

Cancellation during an observation finishes its finite access and cleanup.
Before physical ACCESS it aborts the attempt; after successful CAS it preserves
success through cleanup and never repeats the update. Completed/error controls are stable and cannot be reset. Cancelled controls
require the explicit rearm protocol described below. This is a composed internal operation;
ordinary compiler call lowering and OS lock/flush/assist/cancel/rearm dispatch
still require integration. Logical CAS/exchange,
bulk copies, weak handles and the full cycle coordinator remain separate work.

The 1728 C/candidate OS-backed schedules use real admission, roots/journals,
metadata tokens, generation queues and buffer consumers. They cover null and
nonnull desired/previous values, both-generation fresh mark offers, cancellation
boundaries, changed fields, stale desired colors, buffer pressure, partial entry
admission, null receivers and invalid field shapes. Queued marks are scanned and
finished, and consumers claim/copy/acknowledge buffers before releasing leases.
The prior 128-store OS pipeline now invokes this complete controller instead of
providing expected values and managing store admission/cleanup externally.

ARM independently drives 128 preparation/observation/retry/cancel schedules,
including instruction-level CAS interference after publication. It checks
producer/guard/token conservation, stable terminal requests, exact final values
and actual buffer consumption; the separate leaf models retain their exact
full-memory comparisons. There are also nonmutation checks for malformed
terminal flags. These deterministic tests do not establish native Darwin
scheduling, hardware memory ordering or complete concurrent cycle termination.

Final validation: native 73 tests pass (32.50 seconds), followed by the final
terminal-rejection cases in the emitted-code test (33.64 seconds). ABI 8 tests
pass. Mandatory ASan/UBSan plus TSan passes after the final control changes
(136.66 seconds). ARM passes 128 composed-store schedules / 2550 owner calls,
its two malformed-terminal cases, and all 64388 helper calls, 27 API-model
scenarios and 1536 context transitions. Mach-O exports 143 helpers. Clippy,
strict C syntax, formatting, whitespace and inventory freshness pass; inventory
contains 22311 candidate sites across 342 files. Logs:
`/tmp/klassic-rooted-store-{native-checked,final-cases,abi,sanitizer-checked,arm-checked,clippy-final}.log`.
No ordinary-program, native Windows/Darwin or performance completion is claimed.


## Store cancellation across poll and rearm (2026-09-28)

`GC_RLOAD_REARM` on the shared `gc_rooted_store` entry now rearms only a cancelled
store whose local obligations have all been discharged and whose context has
resumed Running. It validates both private-admission flags, preparation state,
six nested RootAccess controls and both untransferred entry leases. A completed
successful physical attempt cannot be rearmed. Rejection preserves the request
and external accounting. This extends the existing ABI without another helper.

Rearm keeps receiver/desired/observed root bindings and descriptor/field inputs;
it clears the entire old mark snapshot, queue/epoch inputs, observation token
and attempt state. It neither releases the retained roots nor adopts a phase.
The caller must obtain new registry admission for the value child before another
heap step. Consequently a field changed while the owner was parked is observed
again, and the desired root is prepared under the newly adopted snapshot.
Successful stores remain complete even if cancellation arrived during cleanup.

The OS-backed schedules now cover 64 cancellation boundaries while avoiding
repeated identical inputs in the other scenarios: 300 C/candidate schedules
replace the previous 1728 runs. In 68 cancelled cases a pending handshake cannot
ACK while the operation owns private work (an active access guard also rejects
polling). After cancellation and independent old-work consumption, ACK adopts
the new phase. Rearm is rejected while parked, then succeeds after END/resume.
Each case rejects 16 malformed/pending-obligation controls without mutation,
requires fresh admission, and checks that the subsequent store uses the changed
field observation. Both generations obtain new queue epochs in active cases.
This exclusive phase-adoption fixture is not a full cycle or color-wrap proof.

ARM extends the 128 composed-store schedules with 46 emitted worker/handshake,
rearm and admission sequences. It checks exact preserved memory on parked or
pending-obligation rejection, exact cleared control state on success, no heap
work before admission, fresh field tokens and real transferred-token cleanup.
The standard compiler/runtime dispatcher still needs to invoke this protocol;
G01-G15 and the native cross-OS/performance gates remain incomplete.

Validation: native 73 tests pass (33.31 seconds), including emitted x86 rearm
and Mach-O export coverage. Mandatory C ASan/UBSan plus TSan passes (141.02
seconds). ARM passes 46 rearm sequences within 128 composed-store schedules /
3272 owner calls, and the full replay passes 66260 helper calls, 27 API-model
scenarios and 1536 context transitions. Clippy, strict C syntax, formatting,
whitespace and inventory freshness pass: 22364 candidate sites / 342 files.
The helper export count remains 143. Logs use `/tmp/klassic-store-rearm-`.


## Native-lock store dispatch (2026-09-28)

Ordinary startup/allocation still uses the legacy heap and shadow roots. The
inspection of `emit_initialize_gc_heap`, `emit_gc_alloc_runtime` and
`compile_gc_write` confirms that swapping only the new store leaf would mix
incompatible reference/header/root ABIs. This change supplies an invocation
boundary for that migration, not ordinary-program activation.

`RootedStoreRuntime` (64 bytes) binds the heap pool, registry admission inputs,
heap/registry/both-generation queue locks and checked native lock/unlock
callbacks. `gc_rooted_store_dispatch` in C and shared direct code performs one
admission or controller action. Fresh/rearmed requests capture their snapshot
under the registry lock; RESTART recaptures and resumes there. Heap/queue work
uses the corresponding runtime lock with no nesting. ACCESS and cancellation
intent call no lock callbacks. Before acquiring any native lock, dispatch checks
that the owner is Running and has no access guard. Unlock may follow a bounded
leaf that just acquired a guard. Native adapters must return only after a
successful lock/unlock, and must not poll or invoke managed code.

The initially failing reprepare schedule exposed a distinction between a new
operation and value re-preparation inside an already admitted store. The latter
retains its outer phase and must use the heap action, without registry
readmission. The dispatch predicate now preserves that ownership. Fresh waiting,
invalid and exhausted admission paths preserve controls; snapshot recapture
keeps the existing counts rather than admitting them again.

The shared assembler now supports a native unary callback: SysV on Linux,
Win64 argument marshalling plus shadow space, and AArch64 argument/frame-link
handling. The Win64 fixture executes a native-ABI callback on its available
runner; Linux execution remains an ABI/POSIX model, not Windows OS validation.
ARM callback models clobber every caller-saved general register, check aligned
stacks and verify balanced lock/unlock calls.

The 300 store schedules now run the candidate through real native mutexes,
including heap, registry and both queue locks; existing semantics, cancellation
and rearm checks remain in force. A Young-owner scenario exercises the Young
queue path, while Old owners exercise the other lane. Twenty-two additional
C/candidate admission/rejection/recapture cases check callback selection and
nonmutation. ARM adds 128 dispatched schedules, 50 post-handshake rearm flows,
3404 owner calls and 1840 balanced native callback pairs to the existing leaf
and composed-operation models. Ordinary compiler integration, actual runtime
lock bindings/assist/poll dispatch and complete collection remain unfinished.

Validation after the readmission fix: native 73 tests pass (32.33 seconds).
The subsequent Win64 callback-ABI group passes 3 tests (0.76 seconds), and the
final emitted-code recapture cases pass (31.28 seconds). ABI 8 tests pass;
mandatory ASan/UBSan plus TSan passes with the final cases (135.38 seconds).
Full ARM replay passes 71326 helper calls, 27 API-model scenarios and 1536
context transitions. Mach-O exports 144 helpers. Clippy, strict C syntax,
formatting, whitespace and inventory freshness pass: 22512 candidate sites /
345 files. Logs use `/tmp/klassic-store-dispatch-`; final sanitizer output is
`/tmp/klassic-store-dispatch-sanitizer-checked.log`. Native Windows/Darwin OS and
performance certification remain open, as does ordinary-program activation.

## Shared-slot store dispatch contention (2026-09-28)

`gc_rooted_store_concurrency.h` runs the composed dispatcher with 1, 2 and 8
actual OS mutators against one shared reference field. Both C and generated
x86 entry points run the same fixture: 352 completed stores in total. Each
owner has distinct registered context, roots, journal and SATB/remset rings;
the heap, metadata lease pool and native heap/registry/queue locks are shared.
Thread-local callback checks reject nested locking and acquiring a lock while
that owner retains an access guard.

The first observation is synchronized after its guard has been released.
Owner zero clears the field before the other owners attempt their captured
CAS. Those owners finish cleanup and rendezvous before any can retry, forcing
all 16 designated stale-CAS attempts to lose across the C/candidate runs.
Subsequent stores run without that ordering. A legitimate observation WAIT
is retried only after checking that no guard remains. Fixed retained roots
keep the small graph live while real consumer jobs process buffer entries,
queue work is scanned and finished, and every transferred metadata lease is
released. All native lock acquisitions/releases balance at teardown.

This is a fixed-epoch contention fixture with post-join consumers. It does not
exercise a concurrent handshake or collector worker, relocation, phase reuse,
full rings or cycle termination. Those runtime scheduling obligations and the
ordinary-program ABI migration remain open. The test is mandatory in emitted
conformance and in both C sanitizer builds; it does not certify G02 or G06.

Validation: emitted x86 conformance passes (34.60 seconds); mandatory C
ASan/UBSan and TSan passes with the final fixture (137.64 seconds). A focused
TSan run and 20 C contention repetitions (7040 stores) also pass. Strict C
warnings, native clippy, formatting, whitespace and inventory freshness pass:
22622 candidate sites / 346 files. Logs use `/tmp/klassic-store-race-`;
`native-fixed.log` and `sanitizer-fixed.log` are the final conformance results.
The initial sanitizer run failed the fixture's overly restrictive return-status
assertion; that failure is superseded, not counted as passing evidence. No
runtime/emitter ABI changed, and no new native Windows/Darwin evidence is claimed.

## Concurrent store cancellation and handshake (2026-09-28)

The shared-slot fixture now has a second schedule with an actual native
coordinator and 1/2/8 OS owners. After each owner has observed the field and
released its observation guard, the coordinator publishes a newer snapshot
sequence and begins a handshake under the common registry mutex. A poll from
each still-admitted owner must return RETRY. Each owner then enters its physical
store attempt and cancels with the receiver guard and metadata leases live.
Only after bounded exit/release and the private-producer count reaching zero
may that owner acknowledge and park. The first attempted stores cannot change
the field.

While all owners are parked, the coordinator checks that any already offered
Old mark task and its single metadata lease remain owned: ACK is not queue
termination. END releases the rendezvous; owners poll back to Running, rearm,
and automatically admit the new snapshot before retrying the store. Rearm
while parked must reject without changing the request. The snapshot sequence
advances from 1 to 2, while mark epochs and colors remain unchanged. This does
not claim color reuse or retirement of old-phase work.

The C and emitted-store paths run 704 completed stores and 22 guarded
cancellation/resumption sequences across both schedules. Registry worker and
coordinator steps in this fixture use the C finite helpers; the candidate
store uses its generated dispatcher, including native locks and guarded cleanup.
Post-join consumers still perform the graph's buffer/queue work. Concurrent
collector consumers, ring-pressure assistance, actual cycle closure and the
ordinary-program heap/root ABI migration remain unfinished.

Validation: emitted x86 conformance passes (34.89 seconds), mandatory C
ASan/UBSan plus TSan passes (141.88 seconds), and 20 focused C repetitions pass
(14080 completed stores, 440 guarded cancellations/resumed admissions). Native
clippy, strict C syntax/warnings, formatting, whitespace and inventory freshness
pass: 22678 candidate sites / 346 files. Logs use
`/tmp/klassic-store-handshake-`. No runtime ABI or emitter changed; native
Windows/Darwin execution and full generational capability gates remain open.

## Concurrent barrier-buffer pressure recovery (2026-09-28)

A third shared-store OS schedule uses one-entry SATB and remembered rings and
an independent consumer thread with 1/2/8 mutators. Consumption starts only
after a real store FULL/FLUSH result, so pressure is required rather than
incidental. The store must have exited its access guard before waiting for
ring space; it then retries through the existing composed dispatcher with its
outer producer obligation intact.

The consumer CLAIMs and ACKs under the registry mutex and COPYs a complete entry
outside it. For SATB, guarded generation routing hands the entry's unique lease
to the Old queue on a successful offer, or releases it after DUPLICATE. For
remembered entries, it validates the reference field under an owner guard,
records that exact source slot in the current bitmap under the registry mutex,
and releases the entry lease before ACK. It also takes mark work, scans under
a guard, accounts for the self-edge by an explicit duplicate offer, finishes
the queue claim and releases its lease. Native heap/registry/queue locks are
never nested or acquired with a held guard.

Mutators and consumption overlap: the fixture requires completed consumer jobs
before all mutators finish, nonzero pressure, SATB and remembered processing,
mark scans, and all 16 stores per mutator completing. Rings are drained and the
precise single slot remains in the remembered map at worker join; final teardown
checks lease conservation. Across all three schedules, C/candidate execution
completes 1056 stores. Consumer operations use C finite helpers while the store
candidate is C or generated x86 code.

The graph is deliberately one fixed, strongly retained Old object; a SATB
referent's identity is checked before reusing its entry's owner-page lease.
This does not implement generic cross-page SATB resolution, Young reachability,
concurrent relocation, remset flips or collector termination. Pressure recovery
and handshake schedules are separate. Ordinary runtime startup/allocation/root
ABI migration and complete G01-G15 certification remain required.

Validation: generated x86 conformance passes (35.38 seconds); mandatory C
ASan/UBSan and TSan passes (144.51 seconds). A focused TSan execution and 20 C
repetitions (21120 completed stores over the three schedules) also pass.
Strict C warnings, native clippy, formatting, whitespace and inventory freshness
pass: 22758 candidate sites / 346 files. Logs use
`/tmp/klassic-store-pressure-`. Runtime/emitter ABI is unchanged, and these are
Linux OS results, not native Windows/Darwin or latency certification.

## Independent SATB referent leases across pages (2026-09-28)

The concurrent consumer no longer treats an entry's owner-page lease as the
lease for its overwritten value. While retaining the copied job and original
entry lease, it acquires a separate `ReferenceLease` for the old referent under
the heap mutex, enters that referent's guard, and routes it to each active
marking generation. A successful queue offer receives that new lease;
DUPLICATE/SKIP releases it after guard exit. The original owner lease survives
both generation handoffs and is released before the buffer ACK. Same-page
references now use this same path with independently owned tokens.

A fourth 1/2/8-mutator schedule uses an Old owner and a Young target in separate
committed pages with different incarnations, one-entry rings, and the concurrent
consumer. Assertions require distinct owner/referent pages and tokens. Mark
work resolves its own leased page before scanning; both Young and Old queues
must be scanned and empty at worker join. Remembered entries still register the
Old source slot, not the Young target's address. Both pages must have zero
metadata leases after all work completes. The existing same-page, cancellation
and pressure schedules continue to run, totaling 1408 stores per C/candidate
suite.

This extends the conformance consumer using the existing C reference-lease and
mark-routing runtime primitives; it does not add a new runtime ABI or activate
ordinary collection. Consumer leaves remain C while candidate stores execute
C/generated x86. Neither page moves in this schedule, metadata capacity remains
sufficient, and generic forwarding/retry, remset flip and termination ownership
remain separate integration work. G01-G15 and cross-OS/performance gates remain
incomplete.

Validation: generated x86 conformance passes (35.76 seconds); mandatory C
ASan/UBSan and TSan passes (142.18 seconds). Focused TSan and 20 C repetitions
(28160 stores across four schedules) pass. Strict C warnings, native clippy,
formatting, whitespace and inventory freshness pass: 22784 candidate sites /
346 files. Logs use `/tmp/klassic-store-crosspage-`. These are Linux results;
no new native Windows/Darwin, relocation or latency certification is claimed.

## Native-lock load dispatch (2026-09-28)

`gc_rooted_load_dispatch.h` now supplies the load-side runtime invocation
boundary in C. The shared direct dispatcher emitter generates both load and
store variants, preserving the existing store semantics. Load uses the existing
64-byte `RootedStoreRuntime` binding layout: heap pool, per-owner admission,
heap/common-registry/both-queue mutexes and native lock callbacks. No object,
root or runtime structure layout changes.

STEP admits fresh loads and recaptures RESTART under the registry mutex, then
executes one advertised heap/queue action. ACCESS and cancellation intent use
no native mutex callback. Heap/queue acquisition requires a Running context
without an active access guard. REARM only reopens cancelled controls after
Running resumes; a previously captured observation requests registry recapture
without rereading the receiver. Unlike store completion, load REARM on an
already completed request remains invalid, following the existing load owner
contract. Native lock callbacks retain the same lifetime, no-poll and no-managed-
callback constraints as the store entry.

The OS concurrency suite now follows each of its 1408 stores with a dispatched
load into an owned root. It checks retained null/target identities and zero
private counts/guards after completion while other mutators update the field.
Twenty-four exclusive post-join sequences reject missing admission and parked
rearm, cancel after observation, inject a field change under a guard, then
recapture and finish: the saved read result survives and self-healing cannot
overwrite the changed field. This injection tests the load's exact-token
semantics; it is not a production mutation path.

ARM adds 18 fresh/recapture/admission/rejection/rearm schedules with 16 balanced
native mutex pairs, aligned stacks and caller-saved register clobbering. The
Mach-O fixture exports 145 helpers. The ARM schedules exercise admission and
cleanup control; actual concurrent loads are exercised on Linux x86/C, not
native Darwin. Ordinary compiler lowering and the coherent startup/allocation/
root migration remain unconnected, as do the full collection and OS/performance
gates. This entry is an implementation boundary, not default activation.

Validation: all 73 native tests pass (50.18 seconds), mandatory C ASan/UBSan and
TSan passes (146.41 seconds), and full ARM replay passes 71358 helper calls,
27 API-model scenarios and 1536 shared-context transitions. Twenty C repetitions
pass 28160 paired load/store completions and 480 captured-load rearm schedules.
Native clippy, strict C warnings, formatting, whitespace and inventory freshness
pass: 22840 candidate sites / 347 files. Logs use `/tmp/klassic-load-dispatch-`.

## Bounded rooted-barrier rendezvous (2026-09-28)

`gc_barrier_poll` now connects cancellation, cleanup, owner polling and rearm in
C and shared direct emission. It accepts the existing runtime bindings, a live
load/store control and its kind. An active operation receives cancellation
intent and at most one cleanup step. Only terminal controls can reach the
registry mutex and worker POLL. Thus a live guard always exits through the
operation's finite ACCESS path before any blocking mutex acquisition.

PARK, nested-producer RETRY, SATB/remset flush, TLAB close and exhaustion are
explicit return statuses for the outer runtime; the helper contains no wait
loop or implicit assistance. After POLL resumes Running, only cancelled
operations rearm and return READY. READY still requires normal dispatcher
admission before managed work resumes. Completed/error/null operations return
their original terminal result after polling and cannot be replayed. The
caller must stop the interruption loop on READY or a completed result. This
entry neither starts nor ends the coordinator's handshake and never certifies
queue termination. Invalid runtime bindings may be found after cleanup has
already progressed; rejection is not rollback of that work.

The OS store schedule now invokes this runtime entry while guards and leases
are live, through cancellation, actual coordinator ACK/park and post-END rearm.
Captured-load resumption uses it too; completed loads and invalid kinds are
checked for request nonmutation. The runtime retains registered identity and
context checks under the common mutex. No runtime structure layout changes;
Mach-O exports 146 helpers.

Normal compiler startup, allocation and roots still use the legacy ABI. This
runtime entry removes duplicated interruption glue from conformance callers,
but does not complete that migration, implement general assistance/scheduling,
or establish G01-G15 or native cross-OS/performance completion.

Validation: all 73 native tests pass (36.36 seconds); mandatory C ASan/UBSan and
TSan passes (151.08 seconds). Twenty C repetitions pass 440 guarded-store
rendezvous and 480 captured-load rearm sequences alongside the load/store
traffic. ARM adds 9 terminal/assistance cases and cancelled-load rearm, checking
27 balanced caller-clobber mutex pairs in the combined load/poll schedules.
Full replay passes 71365 helper calls, 27 API-model scenarios and 1536 context
transitions. Strict C warnings, native clippy, formatting, whitespace and
inventory freshness pass: 22911 candidate sites / 349 files. Logs use
`/tmp/klassic-barrier-poll-`. ARM API emulation is not Darwin scheduling or
hardware-ordering evidence; native Windows/Darwin and latency gates remain open.

## Bounded barrier-poll assistance (2026-09-28)

`gc_barrier_assist` now handles the three finite owner actions returned by the
barrier rendezvous: close TLAB, publish SATB, and publish remembered entries.
C and the shared direct emitter use the existing runtime bindings and context.
All actions require Running without a guard. Publication uses the existing
SPSC leaf and no native lock; it exposes only the owner's already complete
prefix. It does not advance consumed cursors, acknowledge jobs or release
entry leases. A successful action returns RETRY so the caller polls again.

TLAB close acquires the same native heap mutex as the allocator. It resolves
the owning run head in constant time from the TLAB start and pool geometry,
checks bounds and ownership through the existing close leaf, and closes the
page without reclaiming objects or metadata. The owner must still retain any
initialization/publication obligations. Invalid requests preserve contexts,
pages and cursors, although a heap mutex may have been acquired and released.
No assistance call scans, allocates, waits or adopts a phase. Other assistance,
allocation pressure, exhaustion and general scheduling remain outer work.

C/candidate cases reject 96 malformed context, action, buffer, pool and page
bindings without data changes. Four load/store poll sequences start with a live
TLAB and complete unpublished lease-bearing entries: close, publish each ring,
then ACK/park. Until separate consumer COPY/processing/ACK, both original leases
and both unconsumed entries survive. The checks also require publication to
avoid native locks. This exercises protocol transport; it does not claim a
completed collection from the retained test entries.

The runtime ABI layout is unchanged; Mach-O exports 147 helpers. Ordinary
startup/allocation/root migration and G01-G15, native cross-OS and performance
completion remain outstanding.

Validation: all 73 native tests pass (36.21 seconds), mandatory C ASan/UBSan and
TSan passes (149.32 seconds), and 20 C repetitions pass 1920 nonmutation cases
and 80 close/publication/ACK/resume sequences. ARM adds 96 exact-state cases,
including multi-page run closure, with 14 balanced heap-lock pairs and native
caller-clobber checks. Full replay passes 71461 helper calls, 27 API-model
scenarios and 1536 shared-context transitions. Strict C warnings, native clippy,
formatting, whitespace and inventory freshness pass: 23036 candidate sites /
352 files. Logs use `/tmp/klassic-barrier-assist-`. ARM execution is modeled;
this adds no native Darwin/Windows or latency certification.


## Committed native root package publication

`gc_native_roots_publish_locked` now connects a committed, privately prepared
native root mapping to the attached mutator and common registry. C and shared
direct emitters validate package geometry, ownership, phase and private state
before linking the registration; rejected requests preserve all state. The
charge remains owned and committed through registration, retirement and reader
lease completion. This entry does not allocate, scan cells, wait, authenticate
a foreign mapping or publish the descriptor/heap bootstrap.

The OS-backed conformance owner now exercises reserve, native-state mapping,
commit, Running-state publication, duplicate rejection, reader-held retirement,
cleanup, unmap and budget release. It checks 36 cases for each implementation.
An ARM model independently specifies whole-memory changes for the same 36
admission cases, including duplicate rejection. This is startup root ownership
integration for conformance owners; ordinary generated programs still use the
legacy ABI and the full G01-G15 contract remains incomplete.

Validation on Linux x86-64: native crate 73 tests passed (36.92 s), mandatory
ASan/UBSan and TSan integration passed (147.57 s), and the complete ARM model
passed 71534 emitted calls, 27 API scenarios and 1536 context transitions.
Mach-O exports 148 helpers. Native clippy, strict C syntax/warnings, formatting,
whitespace and inventory freshness passed (23115 candidate sites / 354 files).
ARM emulation does not establish Darwin execution or hardware ordering.


## Startup, root growth and caller-return publication integration

The OS-backed page/allocation owner now uses the committed native root publisher
for all three dynamic scopes: paired root/descriptor startup, root capacity
growth during active marking, and the caller-owned return root. The emitted
harness passes its direct publisher into the owner and retains its executable
mapping until the owner finishes. Lifecycle requests obtain their scope from
the published registration instead of relying on ATTACH to fill a request.

This joins real package admission/preparation/commit to allocation, bounded
root/journal processing and disposal. The return root still resolves after the
callee packages have been unmapped. An independent ARM whole-memory oracle
checks that adding a second package repairs both owner and registry links while
preserving the first scope, cells, journal and metadata charge. Mandatory native
and sanitizer markers require the integrated path to execute. These owners
remain conformance callers; this does not change ordinary compiler startup or
close the collector cycle/ABI migration requirements.

Validation: native 73 tests passed (36.62 s); mandatory ASan/UBSan and TSan
passed (147.05 s); ARM emulation passed 71536 emitted calls, including linked
package growth. Native clippy, strict C warnings, formatting, whitespace and
inventory freshness passed (23121 candidate sites / 354 files). Darwin hardware
execution remains unverified by this model.


## Registered roots in the concurrent barrier owner

The shared-slot 1/2/8-mutator owner now attaches each root segment to the common
registry and reserves/stores its initial roots through the root API. It no
longer constructs already-active, unregistered segments. Those registrations
remain live across the emitted/C load/store dispatch, buffer pressure, guarded
cancellation and handshake schedules.

After joining mutators, the owner retires each scope, scans it under a counted
lease and processes both sides of every leased journal batch before ACK. Mark
handoffs acquire independent referent metadata leases and route to both retained
generation queues; null and canonical Old references use the generation filter.
The scanner can now follow the Old owner's actual Young child using a distinct
lease. Journal leases prevent cleanup until handoff finishes. Clearing and
FINISH precede detach; both thread and root inventories must be empty before
backing storage is freed.

This graph never moves objects or closes either marking epoch. Its handshake
changes only phase sequence, so the journal's generation/color fields must
match the consumer's adopted snapshot before using the retained queue epochs.
Root/journal teardown runs after mutator joins; this is not evidence of a
production concurrent root traversal, weak-handle handling, cycle termination
or ordinary compiler integration. The marker is required in both emitted native
and sanitizer suites.

Validation on Linux: standalone C heap suite passed; native 73 tests passed
(36.81 s); required ASan/UBSan and TSan passed (144.21 s). Native clippy, strict
C warnings, formatting, whitespace and inventory freshness passed (23175
candidate sites / 354 files). No ABI or emitted instruction implementation
changed; no new foreign-hardware execution evidence is claimed.


## Live registered-journal pressure

The pressure schedules now give each of 1/2/8 OS mutators an eight-entry root
journal and run a registered journal consumer alongside C/emitted load/store
dispatch. Journal consumption starts only after a real full-journal response;
the outer owner waits without a raw-access guard while retaining its counted
producer and roots. Store and load FLUSH both retry after the consumer frees
space. Every ring must wrap, and journal work must complete before all mutators
finish. Joined teardown still consumes any later entries and retires each scope.

Each consumer turn claims/copies at most two entries. Both previous and next
references route into the retained Young/Old queues before ACK; the journal
lease keeps the ring positions unavailable throughout processing. Finite mark
queue saturation now uses the existing counted per-record spill cells, and the
scanner refills before TAKE. This allows a parent to retain its claim while
handing off a distinct child's metadata lease. Pressure assertions require all
journal cursors caught up, no remaining registry lease, no queue overflow and
balanced metadata leases before teardown.

This remains a fixed, nonmoving graph with retained epochs. It adds live root
journal consumption, not a complete production root walk, epoch closure,
collector scheduling or compiler ABI migration. The new marker is mandatory
for emitted native tests and both sanitizer executions.

Validation: standalone C heap suite passed; native 73 tests passed (37.93 s);
mandatory ASan/UBSan and TSan passed (145.48 s). Native clippy, strict C warnings,
formatting, whitespace and inventory freshness passed (23194 candidate sites /
354 files). This change modifies composed conformance owners, not ABI layouts
or emitted instructions; foreign hardware execution remains unverified.


## Counted pending-reference forwarding

`gc_pending_follow_locked` is a shared C/direct runtime entry for one forwarding
edge of an externally counted strong journal/root-walk/worker item. It acquires
source metadata on first use, validates an existing token on later steps, and
reads forwarding without touching managed payload or headers. A destination
lease must be acquired before the current reference changes or the source lease
is released. BUSY and destination metadata pressure retain the current source;
ADVANCED retains the new reference and lease for the next bounded turn.

The caller continues to own its registered producer obligation. DONE is not a
mark completion or immobility certificate; the next access still requires a
canonical guard. Cleanup retains any failed source-release obligation in a
separate retired token. Cancellation releases metadata but cannot ACK unoffered
work. No new ABI layout was introduced; Mach-O now exports 149 helpers.

The live-journal owner uses this entry before generation routing, selecting the
C or emitted implementation with the rest of its barrier bindings. Its current
shared graph remains nonmoving. Separate conformance cases use an unmapped
managed base to verify two metadata-only forwarding hops, busy/retained/empty
states, destination failure and capacity recovery, invalid controls, token
validation and final release. ARM independently checks whole-memory changes,
including source/destination holder counts and token generations across retries.
A concurrent moving journal consumer and ordinary compiler integration remain
unfinished; this entry alone does not close those requirements.

Validation: native 73 tests passed (39.54 s), required ASan/UBSan and TSan passed
(148.32 s), and ARM passed 71548 emitted calls / 27 API scenarios / 1536 context
transitions. The pending-follow oracle covers 12 exact-state steps. Its initial
slot-layout mismatch was traced to the oracle placing generation after page;
the ABI defines generation first. Correcting that expected layout passed the
focused model and full ARM run. Strict C, native clippy, formatting, whitespace
and inventory freshness passed (23279 candidate sites / 357 files). ARM
emulation remains distinct from Darwin execution/hardware-ordering evidence.


## Pending journals across physically protected forwarding chains

The real two-hop relocation owner now also captures registered journal items
that still name the first copy after both source payload pages have been made
inaccessible. It clears that journal owner's root after COPY, then deliberately
occupies one of two metadata slots. Pending follow acquires the source but
cannot acquire the destination; the copied journal lease, source token and
unconsumed cursor must remain intact. Releasing the blocker allows two bounded
hops, each acquiring the next page before releasing the prior page.

Before ACK, the owner enters the final canonical guard and checks the original
MarkRecord identity and payload, exits the guard and releases its last metadata
lease. It repeats the operation for the deletion entry, then drains, clears and
retires the registered scope. All 64 real mapping/relocation graphs run this
path through C or the emitted follower. The direct code mapping is retained
until the whole chain test finishes, and both sanitizer runs require its marker.

The schedule is idle-phase and sequential after relocation; another canonical
root remains live. It proves protected-source access discipline and retained
journal/metadata ownership under capacity failure, not sole-root liveness,
concurrent relocation/journal scheduling, mark termination or completed cycles.

Validation: native 73 tests passed (37.60 s), mandatory ASan/UBSan and TSan
passed (150.37 s). Strict C warnings, native clippy, formatting, whitespace and
inventory freshness passed (23312 candidate sites / 357 files). The initial
emitted-harness argument wiring error was corrected before the successful native
run. No ABI/emitter implementation changed in this integration step.


## Pending follow to canonical guard retry

`gc_pending_access_locked` now bridges an already leased, externally counted
pending reference to a canonical access guard in C and shared direct emission.
It authenticates the current metadata token, requires an unguarded Running
context and empty access scratch, and tries one guard acquisition. Movement
published after pending-follow DONE returns ADVANCED with the private destination
discarded and the original reference/token unchanged. The caller recomputes
forwarding; WAIT/exhaustion likewise retain ownership without a guard.

READY requires unlock, bounded guarded work, EXIT outside the heap lock, then
metadata release under the heap lock. A duplicate entry with an active guard
is rejected unchanged. Neither success nor cancellation ACKs the external
journal/root/worker obligation. There is no new ABI layout; Mach-O exports 150
helpers. The live-journal owner now uses this entry and retries follow/access
when movement or a busy source prevents guard acquisition.

Conformance schedules publish the second forwarding edge after a successful
follow check, require private scratch discard, then follow and guard the final
copy. Invalid contexts/scratch retain every owned token. An independent ARM
oracle checks 23 full-memory transitions covering READY/EXIT, forwarded and busy
sources, null, exhausted source admission, token mismatch and malformed controls.
This closes the pending consumer's unchecked follow-to-access assumption; the
shared OS graph remains nonmoving, so it is not evidence of a complete moving
journal collector or ordinary compiler migration.

Validation: native 73 tests passed (37.14 s), mandatory ASan/UBSan and TSan passed
(150.42 s), and ARM passed 71571 emitted calls, 27 API scenarios and 1536 context
transitions. Pending access contributes 23 exact-state steps. The ARM test's
initial EXIT lookup used the wrong export name; correcting it to resolve_access
passed both focused and full reruns. Strict C warnings, native clippy,
formatting, whitespace and inventory freshness passed (23364 candidate sites /
359 files). Actual Darwin execution and hardware ordering remain unverified.


## OS mover between pending follow and access

In 32 of the protected relocation schedules, a registered journal consumer now
captures the intermediate reference, clears its root, acquires its metadata
lease and observes pending-follow DONE while that copy is still canonical. It
then enters native context state and waits for a distinct OS actor to copy the
object, install/publish forwarding and make the intermediate payload page
inaccessible. On returning to Running, C/emitted pending access must return
ADVANCED, preserve the original reference/token and discard all private access
scratch without entering a raw guard.

The owner then forces destination metadata pressure, retains its journal claim
through the failed attempt, follows the new edge and validates identity/payload
under the final guard. The deletion entry receives the same handoff before ACK
and scope retirement. The original 64 two-hop protected-source cases remain.
C and emitted native harnesses require the new mover marker; thread/control and
code mappings stay live through join and completion.

This is a deterministic two-actor interleaving, with the consumer paused in the
follow/access window. The phase is fixed and another canonical logical root
remains; it does not prove sole-root liveness, concurrent mark termination,
full-cycle coordination, actual foreign OS execution or ordinary compiler ABI
migration.

Validation: native crate 73 tests passed (40.19 s). After adding explicit native
context transitions around the join, the affected emitted concurrency suite
passed again (35.02 s), and the mandatory ASan/UBSan plus TSan suite passed on
the final version (143.15 s). Strict C warnings, native clippy, formatting,
whitespace and inventory freshness passed (23376 candidate sites / 359 files).
No ABI/emitted implementation changed; no new foreign OS result is claimed.
