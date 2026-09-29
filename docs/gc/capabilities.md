# Generational GC implementation status

The target is the complete G01-G15 contract in
`klassic-generational-zgc-instructions.md`, not the baseline collector.
No generational capability is certified complete yet. The default collector
has not been switched. The sections below the current audit retain historical
worktree evidence; their test counts, inventory totals and implementation gaps
are dated snapshots, not assertions about the current tree.

## Current contract audit (2026-09-29)

Update 2026-09-30: `--gc=generational` (M-G1) is the first ordinary compilation
path that calls these components: on direct Linux x86-64 it allocates heap
strings with the emitted `alloc_into` leaf, publishes them under a source guard,
and keeps frame references in RootCells. It performs no collection, rejects
enums and other targets, and the default collector is unchanged. No G01-G15
capability becomes complete through it. See
[the M-G1 plan and results](production-integration-plan.md).

The owned source-retirement receipt now also requires a bounded audit of all
registered SATB/remset owners. It checks one context per step, requires real
owner handoff and consumer ACK, and invalidates saved cursors before borrowing
on registration/detach, handshake-generation or phase changes. Tests free a
cursor's context and reuse registration storage at the same address. This
still relies on the cycle's no-new-target-reference protocol. See
[the registered-buffer report](native-buffer-incoming-2026-09-29.md).

A one-context SATB/remset quiescence check now requires native or active-handshake
owner handoff, fully drained private/published prefixes and completed consumer
ACKs. Concurrent COPY coverage checks that consumer-private cursor state is not
read. The whole-registry composition described above now supplies the buffer input
to source retirement. See [the buffer report](native-buffer-quiescence-2026-09-29.md).

Source metadata retirement now also requires a root-domain absence observer:
bounded atomic cell checks with real reader leases, journal/deletion drainage
and final exclusion of other reader snapshots. The graph fixture covers active
and retiring scopes, cancellation and blocked cleanup without granting marking
credit. See [the root-domain report](native-root-incoming-2026-09-29.md).

The source-retirement receipt now also requires both mark queues' incoming
audit: ordinary queued/in-flight tasks and spill cells, with metadata leases
protecting spill cursors. Epoch/serial/storage/overflow changes invalidate the
result. Runtime graph/copy fixtures cover pressure, cancellation, REFILL and
both generation lanes. See [the queue-audit report](native-queue-incoming-2026-09-29.md).

Owned source metadata retirement now requires a completed native scan audit
bound to its exact run, incarnation, phase and inventory serial. Missing/stale
or mismatched results preserve page/budget ownership; success consumes the
receipt. All owned retirement fixtures use this gate. Other retirement domains
and payload-uncommit proofs remain separate. See
[the retirement-gate report](native-retirement-gate-2026-09-29.md).

Mark-scan controls now register automatically with the runtime and remain
discoverable until explicit terminal unlink. A one-control-per-call incoming
audit checks captured child references and rejects stale inventory cursors.
The graph fixture covers two real claims, non-LIFO removal and lease-free child
findings. Worker ownership handoff and the combined source-retirement proof
remain external. See [the scan-inventory report](native-scan-incoming-2026-09-29.md).

The runtime now owns resumable mark-scan state through the parent claim,
canonical guarded read, bounded child buffer, per-child handoff and final
lease cleanup. The existing graph-cycle fixture uses it with real lease
exhaustion/recovery, cancellation and native intervals. Worker storage/admission,
capacity progress, source discovery and moving lifecycle coverage remain open.
See [the mark-scan report](native-mark-scan-2026-09-29.md).

Runtime marking now has a root-audit handoff controller: captured roots remain
counted through cancellation/retry and are acknowledged only after queue handoff
and lease cleanup. A nonempty cyclic graph fixture uses mapped runtime storage,
bounded scans, counted spill, deletion-journal handoff and owned final retirement.
It does not move live objects or establish production scheduling/retirement
proofs. See [the graph-cycle report](native-graph-cycle-2026-09-29.md).

Runtime-owned cycle controls now connect request admission, queue/mark/remset
epochs and handshake boundaries. Completion rechecks retirement roots and waits
for Idle adoption before satisfying tickets. The empty-graph lifecycle fixture
also covers Young cycles during Old marking and stale retirement re-audit; it
does not certify moving/promotion overlap or production graph service. See
[the native cycle report](native-cycle-controls-2026-09-29.md).

Initial work storage now belongs to runtime startup: charged native SATB/remset
rings, metadata lease slots and Young/Old queues, initialized outside locks and
bound only after commit. Disposal rejects pending transports/consumers, active
leases and queued work before releasing backing. Owned allocation and logical
record fixtures now use these mapped pools/queues. See
[the work bootstrap report](work-bootstrap-2026-09-29.md). Focused ASan/UBSan,
ten native atomic tests (51.69 s), mandatory C ASan/UBSan plus TSan (179.57 s),
strict C syntax, formatting, diff and inventory checks passed (27,508 sites /
444 files). Worker launch, capacity growth, remset/pin inventories, production
barrier/cycle routing, generated ownership controllers and default migration
remain open.

Logical record ownership now connects owned copy installation, bounded import
return, final arena sealing/unmap, budget release and inventory unlinking.
The real-allocation/copy fixtures finish with normal runtime disposal and exact
budget return rather than raw test-only teardown. Queued, claimed and finished
jobs retain their real physical-copy leases and block early retirement; late
invalid admissions race native disposal without reading freed metadata. See
[the logical lifetime report](logical-record-lifetime-2026-09-29.md). Focused
ASan/UBSan, ten native atomic tests (52.53 s), mandatory C ASan/UBSan plus TSan
(191.44 s), strict C syntax, formatting, diff and inventory checks passed
(27,444 sites / 443 files). These C ownership controllers require every copy
and record borrower to obey the new ownership ABI and the scheduler to supply
the whole-range death/retirement proof. Generated controllers, production
scheduler/proof services and default migration remain open.

Owned physical source retirement now connects the runtime's allocation runs to
embedded SourceRetire receipts. Six real-copy cases retain the logical record
mapping/charge across source uncommit, bitmap/table cleanup, failed OS retry,
tail reuse between bounded steps and whole-range recommit. See
[the ownership retirement report](owned-retirement-2026-09-29.md). Focused
ASan/UBSan, ten native atomic tests (51.27 s), mandatory C ASan/UBSan plus TSan
(179.26 s), strict C syntax, formatting, diff and inventory checks passed
(27,365 sites / 442 files). This C controller still requires the scheduler's
global retirement proof; logical-record reclamation and default integration
remain incomplete. The native harness uses generated allocation, not an emitted
version of this new ownership controller.

The C bootstrap now connects to real OS-backed Young allocation runs through
`gc_native_allocation.h`: runtime inventory, rounded metadata admission, native
preparation, TLAB binding and shared rooted allocation. Bound/closed runs cannot
use private cancellation, and retained runs prevent runtime disposal. See
[the allocation connection report](native-allocation-integration-2026-09-29.md)
for coverage and the explicit limits of fixture teardown. Focused ASan/UBSan,
ten native atomic tests (50.02 s), mandatory C ASan/UBSan plus TSan (178.01 s),
strict C syntax, formatting, diff and inventory checks passed (27,288 sites /
440 files). Generated x86 allocation runs against the mapped runtime packages;
provisioning itself remains C-only. No default migration or installed-run/
logical-record retirement completion is claimed.

The subsequent C bootstrap owner (`gc_native_runtime.h`) now owns the heap,
initial context and root/descriptor packages across private preparation,
explicit activation and retryable quiescent disposal. Ten injected lifecycle
cases cover allocation failures, budget exhaustion, failed release retries and
real root retirement (CLAIM/scan/ACK before CLEAN/FINISH). Focused ASan/UBSan,
ten native atomic tests (50.18 s), and mandatory C ASan/UBSan plus TSan
(175.90 s) passed. Strict C syntax, formatting, diff and inventory checks passed
(27,078 sites / 438 files). These are targeted checks after the full workspace
sweep below; the bootstrap remains C-only and is not called by default startup.
Worker services, other metadata inventories and connected compiler migration
remain outstanding.

The complete Linux workspace sweep after the native decommit revision passed
797 tests with zero failures/ignored, runtime build, all-target/all-feature
Clippy with warnings denied, formatting and inventory checks. See
[the current validation report](workspace-validation-native-decommit-2026-09-29.md).
[The default integration audit](default-integration-audit-2026-09-29.md) confirms
that normal allocation/root lowering still uses the legacy representation and
identifies the required production startup/ownership connection. These results
do not complete the default generational migration or cross-platform/performance
gates.

Direct payload decommit callbacks now exist for Linux x86-64 (mprotect/madvise
syscalls), Win64 (VirtualFree MEM_DECOMMIT), and Darwin ARM64 (libSystem
mprotect/madvise imports). They reject null, unaligned, empty and overflowing
ranges, preserve virtual reservation ownership on failure and normalize native
results. The Linux actual-copy retirement fixture now composes the generated
source controller with the generated OS callback instead of a C success callback.

Darwin now uses MADV_FREE in both C and generated code: XNU dispatches DONTNEED
to page deactivation and FREE to page killing. See the primary
[XNU behavior implementation](https://github.com/apple-oss-distributions/xnu/blob/main/osfmk/vm/vm_map.c)
and [advice constants](https://github.com/apple-oss-distributions/xnu/blob/main/bsd/sys/mman.h).
This correction does not substitute for actual Darwin execution or RSS measurement.
Linux and Windows behavior are unchanged by the Darwin-only C preprocessor branch.

Ten native atomic tests passed in 52.93 s. Four Windows tests passed in 0.71 s;
on this Linux host the execution test uses explicit OS models, including failed
decommit with retained backing and a noncanonical BOOL. Mach-O generation passed
with 193 exports and 18 imports. Formatting, diff and inventory checks passed
(26,985 sites / 436 files). ARM emulation passed 73,484 emitted calls, 27
API-model scenarios and 1,536 context transitions, including generated source
controller composition with Darwin protection/advice failure retries. This is
not native Darwin scheduling or hardware-ordering evidence. Mandatory C
ASan/UBSan plus TSan passed in 179.47 s.
Default runtime activation, bootstrap/bitmap emission, actual Windows/Darwin
execution and the full capability/performance gates remain open.

### Previous shared source controller revision

Source payload OS cleanup now has a shared PortableAsm controller matching the
C `gc_native_source_uncommit_entry` ABI: HeapPool, SourceUncommit and an explicit
native decommit callback. It validates request ownership, run geometry and all
page/tail states before invoking the callback. It normalizes the C bool's low
byte, preserves the receipt on failure, and clears only that receipt on success.
The C convenience wrapper still supplies the default OS callback when requested;
the shared entry requires a nonnull callback.

The native actual-copy/weak/source-retirement fixture now invokes this emitted
controller, including injected partial OS failure and real successful discard.
Ten native atomic tests passed in 45.54 s. Mach-O export passed with 192 entries
after updating its expected count. Header syntax, formatting, diff and inventory
checks passed (26,976 sites / 436 files). Eight ABI tests passed. ARM emulation
passed 73,475 emitted calls, 27 API-model scenarios and 1,536 context transitions.
The new adapter covers 35 admission cases, callback register clobbering, narrow
bool retry and actual emulator unmapping. Native Darwin execution and hardware
ordering are not established by this model. Mandatory C ASan/UBSan plus TSan
passed in 180.73 s.
Bootstrap/bitmap controllers remain C-only; default runtime
activation and actual cross-platform/performance gates remain incomplete.

### Previous mapped bitmap retirement revision

The C native bitmap owner now accepts a stable HeapPool/lease instead of depending
on private heap bootstrap ownership. Checked pool lease validation is shared
with bootstrap payload adapters. Page publication can use the supplied C or
emitted pool leaf; the native bitmap controller itself remains C.

SourceRetire BEGIN now hands actual OS-mapped bitmap backing to the bitmap
retirement adapter. It checks the exact request bound in the retained head,
incarnation, lease geometry and detached mapping. Failed unmap preserves the
request, mapping and charges. Successful unmap clears the detached receipt;
padding budget is released separately, and the existing bounded retirement
STEP releases logical bitmap bytes. The caller still supplies the complete
reference/color/quiescence proof before BEGIN.

The 16 actual-copy/weak/source-retirement/reuse lifecycles now use native mapped
source bitmaps, copied-request rejection, injected unmap failure and exact rounded
budget return. Focused ASan/UBSan also covers private bitmap and never-installed
cleanup regressions; focused TSan passed too. Ten native atomic tests passed
in 48.96 s. Header syntax, formatting, diff and inventory checks passed
(26,924 sites / 435 files). Mandatory C ASan/UBSan plus TSan passed in
174.28 s, requiring the mapped source-bitmap marker in both builds.
Ordinary lifetime inventories,
default runtime activation, shared emitted adapters and G01-G15 gates remain open.

### Previous source payload disposal revision

The C native source-uncommit adapter now performs OS payload cleanup after
real evacuation and SourceUncommit BEGIN. It validates the exact request address
bound in the head, all run geometry/incarnations and evacuated/tail states before
calling the shared native decommit leaf. It deliberately does not reject or
modify metadata leases: weak/forwarding users may retain metadata after payload
discard. OS failure preserves the receipt and accounting; success clears only
the receipt, leaving budget return to the locked FINISH operation.

The existing 16-object source-retirement OS fixture uses this adapter following
actual allocation, copying, forwarding publication and source drain. It rejects
copied requests, seven malformed request variants and stale tail incarnations.
POSIX partial protection failures retain weak metadata and block premature
FINISH; successful retries continue through weak following, metadata retirement
and same-range reuse. Sources in this fixture are Young; destinations exercise
both generations. This is C adapter execution even in the native harness, not
emitted OS-adapter parity or a production concurrent retirement proof.

Ten native atomic tests passed in 48.62 s. Header syntax, formatting, diff and
inventory checks passed (26,905 sites / 435 files). The 16-case focused
ASan/UBSan run passed. Mandatory C ASan/UBSan plus TSan passed in 174.50 s,
with the source-payload marker required in both builds.
Default compiler/runtime activation, shared emitted adapters and full G01-G15
validation remain open.

### Previous never-installed payload release revision

The C native PageRelease payload adapter now replaces fixture-only OS cleanup
for never-installed committed runs. It authenticates the detached request and
all page/tail geometry, state, identity, generation and unchanged allocation tops
before calling the OS. Success clears only the mapping receipt; the existing
locked FINISH operation returns payload and logical bitmap budget separately.
Failed OS cleanup leaves the request and accounting unchanged, even when POSIX
protection has already changed. Private lease cleanup reuses the same OS leaf.

The bitmap lifecycle fixture now drives this adapter for six actual Young/Old
releases. It rejects seven malformed request variants, changed allocation tops
and stale tail incarnations before an OS callback. Injected partial protection
failure prevents FINISH; retry completes native discard without premature credit.
Focused ASan/UBSan and TSan passed. Ten native atomic tests passed in 48.27 s.
Header syntax, formatting, diff and inventory checks passed (26,876 sites / 434
files). Mandatory C ASan/UBSan plus TSan passed in 175.20 s, with the new
payload-release marker required in both builds.
This is still a C adapter, not shared emitted cleanup or default collector
activation. Installed-source retirement and its bitmap lifetime remain separate.

### Previous bitmap backing revision

The C native bitmap owner now admits OS-rounded object-start backing for an
existing reserved page lease. Logical bitmap bytes remain charged by the pool;
a separate unique charge covers only rounding padding. Mapping precedes padding
commit and page publication. If page publication fails after padding commit,
retry and private unwind both retain the correct budget. Failed unmapping leaves
the backing and all outstanding charges owned. Published backing disposal accepts
only a matching never-installed PageRelease receipt, plus caller-established
quiescence; source-retirement disposal is not provided by this adapter.

Fourteen actual OS-backed Young/Old lifecycles cover padding limits, allocation
failure, unmap failure, publication retry/private unwind, detached receipt
validation and exact-quantum backing with no padding. Every lifecycle returns
heap and metadata accounting to its bootstrap baseline. Focused ASan/UBSan
and focused TSan passed. Ten native atomic tests passed in 48.14 s after fixing
a missing fixture include. Header syntax, formatting, diff and inventory checks
passed (26,845 sites / 433 files). Mandatory C ASan/UBSan plus TSan passed
in 177.18 s, requiring the bitmap marker in both builds.
This remains a C adapter, including when invoked by the native harness. Shared
emitted bootstrap, published payload OS disposal, retired-source backing
ownership and default runtime activation remain incomplete.

### Previous private payload mapping revision

The private C heap owner additionally validates exclusive unpublished PageLease
ranges against page state, incarnation, identity, generation and geometry before
OS commit/decommit. Commit zeroes the range before returning the mapping receipt;
decommit keeps that receipt and reservation budget on OS failure, including a
partially protected POSIX range. Repeated commit/decommit and stale/aborted leases
are rejected. Actual two-generation, two-pass dirty/decommit/recommit fixtures
check every byte for zero and reject pool abort while a mapping is retained.
Focused ASan/UBSan and ten native atomic tests passed (47.33 s); inventory
checks pass at 26,784 sites / 431 files. Focused TSan and mandatory C
ASan/UBSan plus TSan passed; the broad run completed in 174.39 s.
Bitmap backing, its OS-rounded budget, committed-page disposal and publication
are separate remaining integration work; these helpers do not reclaim published
pages or enable the default generational collector.


The C native adapter now owns private HeapPool bootstrap: a PROT_NONE/native
reservation with an unused first offset page, an OS-rounded zeroed page table,
and its metadata charge. Configuration is checked before allocation. Failed
preparation and partial disposal retain surviving mappings for retry; a pool
under disposal is no longer exposed. Disposal rejects outstanding page or budget
obligations and additionally requires external quiescence. This adapter is not
emitted bootstrap code and has not replaced default runtime initialization.

Focused ASan/UBSan and ten native atomic tests passed (50.53 s). Mandatory C
ASan/UBSan plus TSan passed (177.38 s), requiring the bootstrap marker in both
builds. Header syntax,
formatting, diff and inventory checks passed (26,755 sites / 431 files).
The following full-workspace result predates this bootstrap revision.

A subsequent broad Linux regression sweep passed all 797 workspace tests
(zero failed/ignored), runtime-library build and all-target/all-feature Clippy
with warnings denied. See [the dated validation report](workspace-validation-2026-09-29.md)
for per-suite counts and environment. Normal execution still uses the legacy
collector; this sweep does not close the generational integration or real
cross-platform/performance gates.

C and PortableAsm now compose root FINISH with native owner notification using
a checked 40-byte RootFinishNotification record. The initial binding must match
the registration's context. FINISH failure leaves the notification untouched;
success advances to a pending-notification stage before invoking the callback.
A failed wake preserves its full error value and returns NOTIFY_RETRY, distinct
from root cleanup failure. Retrying never replays FINISH or reads root backing,
registry, request or context memory. A completed notification does not wake again.
The unique control and private event must remain independently alive until the
notification succeeds, even when backing has already been disposed.

The 96 real cleanup/wait lifecycles now use the composed finisher, including 48
injected wake failures. A separate C/native case unmaps backing before notification
retry. ARM uses actual emitted root registration/retirement/cleanup, rejects 12
invalid bindings, clobbers native caller-saved registers, and unmaps backing
inside the first failed callback before retrying with NULL registry/request.

Event registration/routing and pending-notification service remain caller
obligations. Default runtime activation, broader scheduler integration, real
Windows/Darwin execution and the remaining G01-G15 gates are incomplete.

Validation: ten native atomic tests passed in 47.35 s and eight ABI tests
passed. Mandatory C ASan/UBSan plus TSan passed in 178.08 s with the completion
notification marker required in both builds. Mach-O generation passed with 191
exports. ARM emulation passed 73,437 emitted calls, 27 API-model scenarios and
1,536 context transitions. Cargo check, header syntax, formatting, diff and
inventory checks passed (26,685 sites / 429 files). Native Windows/Darwin and
hardware-ordering coverage remain separate open gates.

### Previous native package wait revision

C and PortableAsm now provide `native_pin_wait`: a registry-locked completion
recheck and exact-width arm of a private sticky u32 event. A native-blocked owner
with a retired package either observes completed cleanup and retries progress,
or clears its event under the same lock used by FINISH and notification. OS
park occurs after unlocking. A notification between unlock and park changes the
comparison word, so FUTEX_WAIT declines to sleep rather than losing the wake.
Every wake/interruption requires another predicate check; the event never
certifies disposal. Event storage is disjoint and outlives all users.

A 96-lifecycle pthread fixture prepares and retires real registered packages,
holds a delayed reader, runs cleanup on a collector thread, and completes unmap
and budget return. It covers completion before arming, notification before park
(with EAGAIN required), and concurrent park/notification. Native execution uses
the emitted arm, Linux futex park and sticky notify entries; C uses instrumented
syscall adapters. ARM covers 24 validation/completion cases, including an event
aligned to four but not eight bytes and unchanged neighboring sentinels.

The caller still owns event registration, notification routing and the outer
scheduler. This adds the root-cleanup wait protocol, not default runtime wiring
or Windows/Darwin OS wait integration. Remaining G01-G15 requirements and real
OS/performance gates are not certified complete.

Validation: focused C TSan passed all 96 lifecycles. Ten native atomic tests
passed in 47.50 s after making the C syscall adapters inline to avoid unused
function errors in the emitted-code harness. Mandatory C ASan/UBSan plus TSan
passed in 179.93 s. Mach-O generation passed with 190 exports; ARM emulation
passed 73,417 emitted calls, 27 API-model scenarios and 1,536 context transitions.
Cargo check, header syntax, formatting, diff and inventory checks passed
(26,620 sites / 427 files). Only the Linux fixture exercises actual OS wait and
notification here; ARM emulation is not native Darwin scheduling evidence.

### Previous root-storage disposal race revision

Root-storage disposal now acquires the segment's PREPARED state before reading
any non-atomic registration or journal metadata, in both C and PortableAsm.
FINISH already publishes PREPARED last with release ordering. Previously the
check read fields that the cleaner could reset concurrently before consulting
that receipt. An isolated reproduction restoring that old read order triggers
a TSan race on registration.scope against FINISH's reset (exit 66); the fixed
check passes the same workload (exit 0).

A 64-iteration pthread fixture holds a reader, retires the segment, releases the
reader on a collector thread, and runs CLEAN/clear/FINISH while the backing owner
polls. The owner unmaps immediately after the completion receipt, before joining
the collector. This requires exclusive backing ownership, prior owner retirement
and no concurrent reattachment/reuse; PREPARED is not an ownership claim. ARM
memory-read tracing verifies that live/retiring/cleaning states read only the
state word and return, and that successful checks read state first.

This fixes the discovered cleanup/disposal race before implementing notification
waits. Lost-wakeup prevention, production wait/notification services, normal
runtime activation and the remaining G01-G15 gates are still unfinished.

Validation: ten native atomic tests passed in 47.57 s. Mandatory C ASan/UBSan
plus TSan passed in 174.06 s with the race marker required in both builds.
Mach-O generation retained 189 exports. ARM emulation passed 73,393 emitted
calls, 27 API-model scenarios and 1,536 context transitions, including the new
read-order assertions. The emulator needed a translation-cache flush after
installing its read hook; the first trace attempt collected no reads and failed,
then focused and full runs passed. Header syntax, formatting, diff and inventory
checks passed (26,559 sites / 424 files). Real Darwin/Windows execution and
hardware-ordering coverage remain separate open gates.

### Previous shared native package progress revision

`native_pin_progress` now has a three-argument shared PortableAsm entry. It
uses the same runtime validation as close, binds the participant to the owner
context/handshake, exits guards before polling, and invokes shared worker and
close entries. Registry callbacks release their lock before any owner action;
worker outcomes are preserved across callback clobbers and mapped to distinct
service requests. The terminal path bypasses worker/context dependencies.

The native harness now runs the entire progress adapter as generated code for
all 40 package lifecycles and the guard/park/private-producer/TLAB case, instead
of composing emitted leaves in a C adapter. ARM instruction tests exercise
SATB/remembered publication requests, TLAB close, producer retry, repeated phase
parking, native entry, unmapped backing and budget retry. Both-generation guard
cases use generated progress and real shared handshake transitions.

This is shutdown integration, not default runtime activation. Production waits,
notifications, journal/reader service, bootstrap and compiler/root migration
remain unfinished, along with the real OS/performance gates. G01-G15 are not
certified complete.

Validation: ten native atomic tests passed in 47.35 s, and mandatory C
ASan/UBSan plus TSan passed in 175.29 s. Mach-O generation passed with 189
exports. ARM emulation passed 73,389 emitted calls, 27 API-model scenarios and
1,536 context transitions. Cargo check, header syntax, formatting, diff and
inventory checks passed (26,539 sites / 423 files). These are not actual
Windows/Darwin scheduling or hardware-ordering results.

### Previous C native package progress revision

The C `gc_native_pin_progress` adapter now connects shutdown to the actual
worker rendezvous protocol. It polls Running/safepoint owners, services close's
RESUME/NATIVE requests through registry-locked LEAVE/ENTER, and returns distinct
worker PARK, RETRY, buffer-publication, TLAB-close and exhaustion statuses.
Guard EXIT bypasses polling and locks. Each invocation has at most one close
action and two worker actions; it never spins or waits. Terminal close needs
neither a participant nor another worker transition.

All 40 package lifecycles now cover native-return exclusion during an active
handshake, phase acknowledgement/parking, coordinator END and resumed shutdown.
Existing journal obligations are handed to marking before advancing the test
phase; this fixture does not prove arbitrary old-phase journal routing. A
separate guard case retains the pin across PARK, private-producer RETRY and
CLOSE_TLAB, then completes root cleanup. The native harness supplies emitted
close and worker leaves to this C adapter. The adapter itself does not yet have
a PortableAsm entry and is not connected to default runtime call sites.

Scheduler services remain explicit: PARK waits for coordinator progress;
RETRY waits or services its outstanding producer/phase dependency; FLUSH and
CLOSE_TLAB perform their bounded owner work outside locks; root WAIT and OS
CLEANUP preserve the package until readers/journals or native cleanup progress.
Bootstrap, full scheduling, normal compiler/runtime migration and real
OS/performance gates remain incomplete. G01-G15 are not certified complete.

Validation: focused C ASan/UBSan package tests passed after handing outstanding
journals to marking before the injected phase changes. Ten native atomic tests
passed in 47.81 s. Mandatory C ASan/UBSan plus TSan passed in 170.11 s, with the
progress marker required in both builds. Full-header syntax, formatting, diff
and inventory checks passed (26,500 sites / 422 files). No emitter or ABI layout
changed in this revision; prior ARM evidence is not a test of this C adapter.

### Previous native package close revision

A shared `native_pin_close` driver now composes package shutdown through a
checked 56-byte NativePinRuntime binding. Each call performs one action:
RootPin cancellation (including guard EXIT before locks), registry-locked
retirement, native unmap, then heap-locked budget return. It requests worker
resumption or native entry instead of changing context state itself. Partial
RootPin completion and successful unmap return MORE, not terminal DONE; only
completed budget release makes the package terminal. Repeated terminal close
returns DONE without another lock, context read, unmap or budget credit.

All 40 package lifecycle fixtures now use the driver, with real independent
heap and registry mutexes, actual worker boundary transitions, delayed readers,
mark-journal handoff and failed-unmap retry. The ARM callback model additionally
clobbers caller-saved registers and removes disposed backing; both-generation
guard-close cases verify no lock callback before guard EXIT. The runtime still
must schedule WAIT/FLUSH/CLEANUP work and keep control/context/lock storage live.
Bootstrap, normal compiler/runtime migration and real OS/performance gates are
still incomplete; this does not certify any G01-G15 capability complete.

Validation: ten native atomic tests passed in 47.79 s, eight ABI tests passed,
and mandatory C ASan/UBSan plus TSan passed in 178.52 s. The close-driver marker
is required in both sanitizer builds. Mach-O generation passed with 188 exports;
ARM emulation passed 73,365 emitted calls, 27 API-model scenarios and 1,536
context transitions. Header syntax, formatting, diff and inventory checks
passed (26,461 sites / 421 files). Native Darwin/Windows execution and hardware
ordering remain unverified by these tests.

### Previous native package budget revision

NativePin now retains its budget obligation through a final shared
`native_pin_release` action under the heap lock. Only a successfully disposed
package (stage 5, no backing) can release its committed charge; success consumes
the charge and advances the owner to terminal stage 6. Invalid context,
remaining backing, inconsistent accounting and replay leave the obligation
unchanged. Context and control storage must remain live through this action;
no retired root storage is read. All 40 joined package fixtures invoke the
new owner action, including failed-budget retry and double-release rejection.

The ARM disposal fixture also calls release after actually unmapping its
backing, and 21 separate cases check context/owner validation and accounting
failures without changing the owner, charge or heap. This closes the explicit
package budget transaction; it does not implement the outer runtime's waits,
error-unwind scheduling, bootstrap ownership or normal compiler integration.
The G01-G15 completion gates remain open.

Validation: ten native atomic tests passed in 58.62 s; mandatory C ASan/UBSan
plus TSan passed in 180.39 s with the budget marker required in both builds.
Mach-O generation passed with 187 exports. ARM emulation passed 73,330 emitted
calls, 27 API-model scenarios and 1,536 context transitions. Cargo check,
formatting, diff and inventory checks passed (26,385 sites / 419 files).
These are not native Darwin/Windows or hardware-ordering results.

### Previous native package disposal revision

NativePin disposal now has a shared PortableAsm entry and a checked 24-byte
NativeMetadataOps layout. Disposal requires BlockedNative with no access guard,
a retired package and completed root-reader/journal cleanup. It invokes the
native unmap callback outside locks, keeps ownership and budget on failure,
and clears backing pointers only on success. Budget credit remains a separate
heap-lock operation. Repeated disposal is rejected without another OS call.

The ARM fault fixture exposed a missing portable-to-native callback argument
conversion. AArch64 now explicitly transfers V7/V6 to x0/x1 while preserving
the callback address and frame. The disposal test clobbers caller-saved
registers, supplies garbage above the bool return byte, and actually unmaps
its emulated backing so post-success backing reads fault. Its 25 cases cover
invalid inputs, incomplete cleanup and failed-unmap retry.

All 40 joined package fixtures use generated disposal in the native harness.
Native backing preparation and pin-inventory backing remain C owner paths;
public wait/unwind scheduling, normal compiler/runtime migration, and the
G01-G15 completion gates remain open.

Validation: ten native atomic tests passed in 46.09 s after the callback fix;
eight ABI tests and mandatory C ASan/UBSan plus TSan passed (172.20 s).
Mach-O generation passed with 186 exports. Full ARM emulation passed 73,303
emitted calls, 27 API-model scenarios and 1,536 context transitions. Formatting,
diff and inventory checks passed (26,357 sites / 419 files). ARM emulation does
not establish native Darwin/Windows execution, scheduling or hardware ordering.

### Previous native package owner revision

NativePin publication, private-root preparation and retirement now have shared
PortableAsm entries. The 344-byte owner and 24-byte borrowed publication request
have C/Rust layout checks. Publication binds a committed registered package;
preparation preserves the reserved root across journal pressure before handing
it to the shared RootPin owner. Retirement rejects live pins, cancels abandoned
empty reservations once and retains partial progress if registry retirement
must be retried.

All 40 native package fixtures use these generated owner entries in the native
harness, together with generated dispatcher/RootPin actions for the 24 acquired
pins. Duplicate publication, same-segment input and changed backing addresses
are rejected without owner mutation. ARM instruction tests also cover a failed
retirement after reservation cancellation and a successful retry. Native backing
allocation, OS disposal and pin-inventory backing remain C owner paths; public
wait/unwind scheduling and normal compiler/runtime migration are incomplete.
The G01-G15 completion gates remain open.

Validation: ten native atomic tests passed in 47.68 s and eight ABI tests
passed. Mandatory C ASan/UBSan plus TSan passed in 175.46 s with the new package
marker required in both builds. Mach-O generation passed with 185 exports.
ARM emulation passed 73,275 emitted calls, 27 API-model scenarios and 1,536
context transitions, including five native-package owner lifecycles. Full-header
syntax, formatting, diff and inventory checks passed (26,321 sites / 419 files).
These are not native Windows/Darwin scheduling or hardware-ordering results.

### Previous pin dispatcher emitter revision

The pin dispatcher now uses a shared PortableAsm implementation and a checked
32-byte PinRuntime ABI. It validates owner/runtime binding, drives sticky
cancellation, requests worker resumption before blocked root work and invokes
the heap mutex through native-ABI callbacks. Guard EXIT runs without a lock
callback; locked owner actions always release the mutex and preserve their
result across callback clobbers.

The generated-code harness runs six scenarios through the emitted dispatcher.
It also runs all 40 registered NativePin package lifecycles, with the 24 acquired
pins using that dispatcher and its emitted RootPin steps. The C package still
supplies native backing/publication/retirement and the mark-handoff fixture; actual worker
native entry, caller-root retirement, delayed readers, both-generation marking
and unmap retry remain exercised. Package-owner emission, public wait/unwind
loops and the normal generational runtime/compiler migration remain pending.
These are integration fixtures, not proof of the complete G01-G15 collector.

Validation: ten native atomic tests passed in 46.94 s, including the registered
package fixture with generated dispatch; all eight ABI tests passed. Mandatory
C ASan/UBSan plus TSan passed in 176.00 s. Mach-O generation passed with 182
exports. ARM emulation passed 73,195 emitted calls, 27 API-model scenarios and
1,536 context transitions; its 14 dispatched pin lifecycles made 34 balanced
native-callback pairs with caller-register clobbering. Full-header syntax,
formatting, diff and inventory checks passed (26,257 sites / 418 files).
Native Windows/Darwin execution and hardware ordering remain outstanding.

### Previous rooted-pin emitter revision

The rooted pin owner now has shared PortableAsm PREPARE, ENTER, EXIT, CLEANUP,
CANCEL and FINISH entries. Its 256-byte self-bound control and 32-byte borrowed
start request have C/Rust layout checks. PREPARE journals a copy into the
independent reserved root; ENTER follows canonical references and acquires page
admission while guarded; EXIT drops the ordinary guard before exposing the
native payload address. Cancellation retains failed cleanup obligations, and
FINISH retains the private root under journal pressure until release succeeds.

All nine existing root-pin lifecycle scenarios now accept either C or generated
owner entries, including source forwarding, selected-page retry, null roots,
native callback access, copied-control rejection and failed cleanup. ARM
instruction emulation adds 14 both-generation owner lifecycles. The registered
native package, mutex dispatcher, OS assistance/waits and FFI unwind loop remain
C-only or unfinished; the default runtime/compiler still uses the legacy heap.
The complete G01-G15 objective remains unachieved.

Validation: ten native atomic tests passed in 46.22 s and all eight ABI tests
passed. Mandatory C ASan/UBSan plus TSan passed in 174.14 s. Mach-O generation
passed with 181 exports. ARM emulation passed 72,957 emitted calls, 27 API-model
scenarios and 1,536 context transitions, including 250 calls in 14 rooted-pin
lifecycles. Full-header syntax, formatting, diff and inventory checks passed
(26,223 sites / 417 files). Native Windows/Darwin scheduling and hardware
ordering remain unverified by these emulator results.

### Previous pin source-release emitter revision

Pin-aware source drain and physical-uncommit adapters now have shared
PortableAsm entries. Their stable drain/uncommit controls and borrowed-input
request use checked C/Rust layouts. Drain captures registry, page, incarnation
and selection epoch on non-invalid progress, including WAIT; a reopened
selection cannot resume an old cursor. Uncommit authenticates the run head,
retains the embedded owner's address through OS work and credits payload bytes
only after OS success, exactly once.

The native generated-code harness now runs the real mapped source-release
fixture for both generations, including PROT_NONE source replacement on Linux,
intact destination values, stale/cross-registry rejection and double FINISH.
The ARM instruction fixture additionally checks WAIT capture and invalid-drain
preservation; its OS success is modeled, not an actual decommit. Scheduler
adoption, rooted/native pin owners, public cleanup and default collector
migration remain incomplete, as do the full G01-G15 gates.

Validation: eight ABI tests and ten native atomic tests passed (45.93 s for
the native suite). Mandatory C ASan/UBSan plus TSan passed in 174.10 s.
Mach-O generation passed with 175 exports. ARM emulation passed 72,707 emitted
calls, 27 API-model scenarios and 1,536 context transitions, including 56 calls
for source release in both generations. Full-header syntax, formatting, diff
and inventory checks passed (26,143 sites / 416 files). Native Windows/Darwin
execution and hardware ordering are not established by emulation.

### Previous pin relocation bridge revision

The pin-aware marked-relocation bridge now has a shared PortableAsm entry.
A 32-byte C/Rust request holds borrowed native controls; it does not acquire or
transfer their lifetime obligations. The entry authenticates exclusive page
selection, checks CLOSED state, generation, incarnation and source bounds,
then invokes the existing shared marked-claim path. WAIT from page admission
maps to CLOSED without enrolling a mover or changing the source cell.

The joined C/generated fixture tests pinned exclusion, foreign-page selection,
seven page/bounds failures, unsatisfied/stale mark epochs and an actual source
claim. It continues through source closure, retention and mover release. The
ARM model also executes this claim/cleanup sequence for both generations and
both admission orders. Physical source-release adapters, rooted pin owners,
public pin scheduling/unwinding and the normal runtime migration remain pending.

Validation: eight ABI tests and ten native atomic tests passed (44.65 s for
native tests), including generated x86-64 bridge execution. Mandatory C
ASan/UBSan plus TSan passed in 182.22 s with the bridge marker in both builds.
Mach-O generation passed with 173 exports. ARM emulation passed eight bridge
lifecycles within 72,651 emitted calls, 27 API-model scenarios and 1,536 context
transitions. Full-header syntax, formatting, diff and inventory checks passed
(26,091 sites / 415 files). These results do not certify native Darwin/Windows
execution, hardware ordering or the complete collector.

### Previous pin registry admission revision

Registry-mediated pin, unpin, select, reopen and exclusive-selection checks now
have shared PortableAsm entries. Each authenticates the borrowed lease and the
inventory's independent lease before invoking the common page gate. Separate
three-argument entries keep operation choice fixed without extending native
calling conventions. New acquisitions require ALLOCATING/CLOSED pages; cleanup
and selection checks remain legal after source evacuation while metadata is
retained.

The C and generated-code fixture now runs the registry admission operations,
including malformed selection requests, stale epochs, invalid bound leases and
post-move cleanup. An ARM instruction-emulation fixture exercises both admission
orders for both generations and both initially admissible page states. The
marked-relocation and source-release bridges, rooted pin owner/dispatcher and
normal runtime integration still need shared emission and scheduler adoption.

Validation: focused C ASan/UBSan passed, and mandatory C ASan/UBSan plus TSan
passed in 184.92 s with the new admission marker in both builds. Ten native
atomic tests passed in 46.47 s. Mach-O generation/export passed with 172
entries. ARM emulation passed eight pin admission/cleanup lifecycles within
72,587 emitted calls, 27 API-model scenarios and 1,536 context transitions.
Full-header syntax, formatting, diff and inventory checks passed (26,054 sites /
414 files). Emulation does not certify native Darwin scheduling or hardware
ordering; native Windows/Darwin execution remains outstanding.

### Previous pin registry binding revision

Pin registry binding and unbinding now have shared PortableAsm implementations.
The 64-byte page record and 24-byte registry are C/Rust ABI structures, with
cross-language size, alignment and field-offset checks. Binding authenticates
the borrowed metadata lease, validates inventory geometry and page state, then
acquires an independent lease before publishing the record. Unbinding resolves
both leases, rejects live pins/selection and releases only the registry's
lease, preserving admission serial/epoch history.

The same C/generated-owner fixture covers full-pool retry, invalid inventory
capacity, substituted lease tokens, wrong incarnation, exhausted selection
epoch, evacuated-page bind rejection and borrower release before inventory
cleanup. It also retains the marked-relocation bridge scenario. Registry
step/selection, rooted pin owners, scheduler integration and the default heap
migration remain unfinished; this is not a public pin API or a full GC cycle.

Validation: all eight GC ABI tests passed, including the added C/Rust layouts.
Ten native atomic tests passed in 51.21 s with generated x86-64 owner execution
and both-ISA lowering; Mach-O generation passed with 167 exports. Mandatory
C ASan/UBSan plus TSan passed in 217.03 s. Full-header syntax, formatting, diff
and inventory checks passed (26,018 sites / 414 files). Native Windows/Darwin
and AArch64 execution of these new registry owners is not claimed.

### Previous page pin emitter revision

Page pin admission now has a PortableAsm emitter alongside the shared relocation
gate emitter. It uses the existing permit/gate/request ABI, rejects selection
without closing pinned admission, authenticates the exclusive selection epoch,
and clears that epoch only after successful reopen. The native conformance
harness runs the same replay/exhaustion and 128 two-actor race cases as C with
the generated function pointer, including calls from the secondary thread.
The operation is included in both-ISA lowering and Mach-O export generation.
Higher-level pin owners and normal runtime/compiler integration remain pending.

Validation: ten native atomic tests passed in 63.41 s, including generated
x86-64 execution and both-ISA lowering. The Mach-O fixture passed with 165
exports and an explicit pin-page symbol check. Mandatory C ASan/UBSan plus
TSan passed in 218.93 s. Full-header syntax, formatting, diff and inventory
checks passed (25,963 sites / 413 files). These results do not claim native
Windows/Darwin or AArch64 execution of the new pin operation.

### Previous pin source release revision

Pin-aware source release now shares exclusive selection authentication with
the marked-relocation bridge. The bounded drain captures registry, page,
incarnation and selection epoch, rejecting a partial cursor after reopening
or registry substitution. The uncommit adapter binds its BEGIN/FINISH epoch,
checks the authenticated run head and retains the existing OS-success and
exact-once budget-credit protocol. Source metadata and virtual-range reuse
still require their independent retirement proofs.

The C fixture covers both generations, pin-held rejection without cursor or
page mutation, real forwarding publication after source access closes, stale
selection/foreign-page rejection, source decommit with intact destination
payload, and double-FINISH rejection. It retains selection and borrowed
metadata leases across OS work. These are scheduler integration entries;
ordinary runtime selection/decommit callers and shared emission are not yet
switched, and the full G01-G15 objective remains incomplete.

Validation: focused ASan/UBSan and full-header syntax passed. Mandatory C
ASan/UBSan plus TSan passed in 234.98 s, requiring the source-release marker
in each build. Formatting, diff and inventory checks passed (25,947 sites /
413 files). An initial standalone sanitizer launch exited 139 without a
diagnostic; the same binary and the final rebuilt fixture passed with the
repository-standard `setarch -R` setup. Its initial failure cause is unproven.
No shared emission or native Windows/Darwin execution is claimed.

### Previous mapped pin inventory revision

The generational C pin inventory now has a native backing owner with rounded
metadata-budget accounting, private preparation, committed lookup and shutdown
disposal. It validates the heap/page-count binding, owns a contiguous record
array and rejects disposal until every page record is unbound with no pins or
selection. Mapping failure permits budget abort; failed unmapping preserves
both allocation and accounting for retry. The bootstrap owner must publish one
inventory per heap and quiesce all users before shutdown.

All 40 joined NativePin package lifecycles now use this mapped, committed
inventory instead of stack-owned page records. They reserve root and inventory
budgets separately, retain inventory charges through root-package disposal,
unbind metadata leases at shutdown, then unmap and release the inventory.
The ordinary C runtime was rechecked: its heap still uses legacy 16-byte headers
and absolute references, so directly installing the offset-reference pin API
would violate its representation. Generational bootstrap/default heap migration,
shared emission and public pin scheduling/unwinding remain incomplete.

Validation: focused ASan/UBSan passed native inventory failure/teardown cases
and all 40 joined package lifecycles using committed mapped inventories.
Full-header syntax passed. Mandatory C ASan/UBSan plus TSan passed in 192.24 s,
requiring both inventory markers in each build. Formatting, diff and inventory
checks passed (25,871 sites / 411 files). No default-runtime switch, shared
emitter, ARM or native Windows/Darwin execution is claimed.

### Previous pin dispatcher revision

The C pin dispatcher now selects bounded owner actions under the page registry's
native heap mutex, exits source guards without acquiring a lock, and retains
sticky cancellation through guard exit and journal pressure. Cancellation never
returns READY. Blocked-native root work requests normal worker resumption;
WAIT/FLUSH/FULL/EXHAUSTED are guard-free, while GUARDED requires immediate EXIT
retry without polling. Wrong bindings and invalid thread states are rejected.

Six focused mutex-owner scenarios cover claimed/selected sources, guard exit,
cancellation, blocked cleanup, journal pressure and retirement-state rejection.
The joined native pin fixture now uses this dispatcher for all 24 acquired
package lifecycles, alongside its 16 abandoned-acquisition package cases and
both-generation journal handoff. The dispatcher does not allocate packages,
perform OS waits/assistance, implement FFI unwinding, retire root packages or
finish a collector cycle. Those outer runtime steps and the normal compiler/
runtime migration remain unfinished; this is not yet the public pin API.

Validation: focused ASan/UBSan passed the dispatcher and joined native owner
fixtures. Full-header syntax passed. Mandatory C ASan/UBSan plus TSan passed
in 189.91 s, requiring both dispatcher and 24-package markers in each build.
Formatting, diff and inventory checks passed (25,829 sites / 409 files).
RootPin gained only a C owner cancellation field; no shared emitted ABI or
native Windows/Darwin/ARM execution is claimed for this change.

### Previous pin mark handoff revision

NativePin's joined C lifecycle fixture now covers 40 combinations of idle,
Young mark, Old mark and overlapping mark phases with Young/Old referents.
Registered journal CLAIM/COPY retains both ring entries and its counted lease
while PendingMark routes each previous/next reference to the required queues.
Actual guarded descriptor scanning finishes transferred tasks and metadata
leases before journal ACK. Young routing skips Old objects; Old routing traces
both generations. The private pin journal is handed off before caller-scope
retirement, so its own publication demonstrably marks the referent.

The fixture no longer uses raw journal drain during native root cleanup.
Retiring storage rejects CLEAN while the journal claim is held; journal ACK
precedes CLEAN, because CLEANING storage cannot accept a new journal claim.
Source and private-root deletion claims still process their captured masks,
and final checks require the expected generation stamps and no leaked leases.
These tests keep each phase/epoch stable: cycle transitions, termination,
concurrent collector scheduling, public acquisition/error unwinding, shared
emission and normal runtime/compiler migration remain unproven or incomplete.

Validation: focused ASan/UBSan passed all 40 native pin phase/generation
lifecycles and nine rooted-pin scenarios. Full-header syntax passed. Mandatory
C ASan/UBSan plus TSan passed (221.61 s), with the 40-case marker required in
both builds. Formatting, diff and inventory checks passed (25,773 sites / 407
files). No new emitted-code, ARM or native Windows/Darwin result is claimed.

### Previous native package revision

The C NativePin owner now publishes an independently mapped root package,
reserves its private root and binds RootPin to it. Active pins and incomplete
root cleanup reject package retirement. After retirement, ordinary deletion,
reader, journal and cleaner obligations still gate disposal. Disposal requires
BlockedNative with no access guard; failed unmap preserves mapping and budget
charge for retry. Prepare pressure preserves ownership of the empty reserved
root, including the path that abandons acquisition before a pin exists.

Five joined lifecycle scenarios use real native root mappings, worker
registration/native transitions and leased scanning. The original caller scope
is fully retired and cleaned while the private pin remains reachable. Delayed
readers block cleanup; undrained journals block final retirement; injected
unmap failure retains accounting. The fixture uses an idle mark phase, so it
does not establish active-cycle root/journal handoff. Runtime startup and sole
pin-inventory allocation, the public safepoint/assist/error-unwind loop, shared
emission and all ordinary selection/retirement paths remain unfinished.

Validation: focused ASan/UBSan passed all five package lifecycles and the nine
rooted-pin scenarios. Full-header syntax validation passed; the mandatory C
ASan/UBSan plus TSan suite passed in 221.21 s, requiring the new native-pin
marker once per sanitizer build. Formatting, diff and inventory checks passed
(25,739 sites / 407 files). No shared-emitter, ARM or native Windows/Darwin
execution is claimed for this C owner integration.

### Previous rooted pin owner revision

The C `RootPin` owner now transfers an independently reserved root through the
root journal, follows canonical forwarding one rooted edge at a time and
acquires the page permit while the canonical source guard is held. It exposes
an address only after ordinary guard exit. The pin, private root and metadata
lease then survive a native interval. Cancellation drops pin/lease before
journaled root release; journal pressure or a blocked owner preserves the
remaining cleanup obligation. Self-bound controls reject copies, and null
inputs still require explicit root cleanup.

Nine fixture scenarios cover original-root overwrite, pending/published
forwarding, selection-before-pin and pin-before-selection, null input,
prepare/finish journal pressure, metadata failure and blocked cleanup. They
use real allocated payloads and a C callback, while native state transitions
are modeled; root scanning observes the independent canonical root. This is
not yet the public safepoint/assist loop, registered native storage package,
FFI error unwinder or normal collector integration. The shared emitter and
G13 gate remain incomplete.

Validation: focused ASan/UBSan passed all nine owner scenarios. The initial
whole-suite build exposed a return-code name collision with relocation phase;
renaming the new codes to `GC_RPIN_*` fixed it. Full-header syntax validation
and the mandatory C ASan/UBSan plus TSan suite then passed (222.31 s), requiring
the rooted pin marker in both sanitizer builds. Formatting, diff and inventory
checks passed (25,637 sites / 405 files). No shared-emitter, ARM or native
Windows/Darwin execution is claimed for this C owner addition.

### Previous registry binding revision

The C page pin registry binds one gate to each authenticated heap run-head
index and owns an independent metadata lease. Borrowed stale or mismatched
leases cannot reach the gate. Unbind rejects outstanding pins or selection;
rebind preserves serial history and advances the epoch. Full metadata pools
leave records unchanged. The marked-relocation bridge checks exclusive page
selection, source bounds/incarnation and generation before claiming a mover.
Tests exercise pin-before-selection, selected-page pin refusal, wrong-page
claims, independent lease lifetime, token replay and metadata pressure.

The registry must be the runtime's sole inventory for its heap. Its storage
owner, public rooted pin acquisition/address resolution, cleanup through native
callbacks and all ordinary selection/retirement callers are not wired yet.
These are C owner helpers, not shared emitted ABI or a completed G13 gate.

Validation: focused registry ASan/UBSan passed. The mandatory C suite passed
ASan/UBSan and TSan in 205.51 s, with all three page pin markers required once
per sanitizer build. Formatting, diff and inventory checks passed (25,492 sites
/ 403 files). No emitted/native ARM or actual Windows/Darwin result is claimed.

### Previous page admission revision

Page pin admission now has a C primitive (`gc_pin_page_locked`) using a
page-owned gate and nonce-authenticated stable permits. Selection skips a
pinned page without closing pin admission or waiting for blocked native code;
selection first excludes subsequent pins until the exclusive epoch reopens.
Permit copies, double unpin, stale permit reuse and stale selection epochs are
rejected. Epoch exhaustion preserves exclusion. Tests cover both ordered
interleavings and 128 mutex-serialized two-actor races.

This is admission infrastructure, not the public `gc_pin` API: independent
strong-root/metadata ownership, canonical destination resolution, native
cleanup, page lifecycle binding, all selection callers and shared emitter
integration remain outstanding. No raw address is returned by this primitive.
The default collector remains unchanged.

Validation: focused ASan/UBSan and TSan runs passed the ordered cases and all
128 two-actor races. TSan used the repository runner's `setarch -R` setup
(the initial direct launch failed during sanitizer address-map setup).
The mandatory C ASan/UBSan plus TSan suite passed in 211.99 s. Formatting,
diff and inventory checks passed (25,382 sites / 400 files). No emitter,
ARM or native Windows/Darwin validation is claimed for this C-only primitive.

### Previous weak owner dispatch revision

The joined mark-to-weak-to-relocation fixture now invokes the fixed weak-mark
owner itself in both implementation arms. Candidate BEGIN/DECIDE/RELEASE no
longer use a C admission wrapper around emitted phase callbacks. The generated
owner mapping stays live until the joined sequence finishes. Both generations,
canonical/moved weak referents, early-load RETRY, keep-alive marking and
closed-load SELECT continue through the emitted scan, per-entry processing and
relocation handoff. This closes the remaining owner-dispatch gap in this
conformance sequence, not the ordinary compiler/runtime integration gap.

Validation: the native emitted-code concurrency harness passed (60.18 s),
including the 24 joined cases with complete emitted owner dispatch. Mandatory
C ASan/UBSan plus TSan passed (251.58 s). Formatting, diff and inventory checks
passed (25,360 sites / 398 files). This change only rewires the C/native test
fixture; emitted helper code and common ABI layouts are unchanged. No new ARM
or actual Darwin/Windows execution result is claimed for this fixture update.

### Earlier weak handoff revision


The covered weak-scan to relocation-adoption handoff now has a fixed C entry
and shared PortableAsm emitter (`gc_weak_relocation_begin_locked`,
`gc_codegen/weak_relocation.rs`). It reuses the scan's completed-owner and
inventory validation, requires complete coverage and empty processor/work
state, and calls the embedded relocation phase BEGIN before releasing weak
claims or reopening admission. Rejected or waiting handoffs retain the closed
gate and coverage. Successful adoption publishes the handshake/phase first;
subsequent weak loads still wait for actor phase adoption.
The 24 joined mark/journal/weak/relocation lifecycles now use this complete
handoff in their candidate arm. An additional 384 rejected/deferred handoffs
compare all fixture, scan, relocation and gate state. ARM independently checks
40 exact-state success/rejection/wait/exhaustion cases. These checks preserve
the distinction between weak coverage and the scheduler's independent selection,
relocation-turn, remap reuse and cross-generation reclamation obligations.
The joined fixture's mark-owner wrapper remains C-controlled; the separate
mark-owner conformance suite already exercises its emitted implementation.
Normal-runtime integration and default collector migration remain incomplete.

Validation: native crate tests passed (73, 43.00 s). After adding the 384
preservation cases and their mandatory output marker, the focused native
concurrency harness passed (44.63 s). ABI tests passed (8); mandatory C
ASan/UBSan plus TSan passed (185.90 s). Clippy, formatting, diff and access
inventory checks passed (25,360 sites / 398 files). The Mach-O fixture exports
164 helpers. ARM emulation passed 72,435 helper calls across 27 API-model
scenarios and 1,536 context transitions, including the 40 new handoff steps.
Actual Darwin/Windows execution, hardware ordering and ordinary runtime
integration remain unverified by these conformance results.

### Earlier weak scan revision


Bounded whole-table weak scan control now has a fixed C entry and shared
PortableAsm emitter (`gc_weak_scan_step_locked`, `gc_codegen/weak_scan.rs`).
The unchanged `WeakScan` state and borrowed `WeakScanStart` request have common
C/Rust layouts with every field checked. BEGIN claims exclusive scan ownership
from a completed mark owner. NEXT examines one slot or consumes one completed
work item, invokes the embedded capture helper and retries cancelled extant
referents without advancing coverage. FINISH reopens admission only after full
coverage and empty work/processor state. ABORT uses emitted cancellation,
retains claims on cleanup failure and leaves admission closed even on success.
The 36 bounded scan lifecycles now use the complete candidate scan and per-entry
operations. Additional cases check 96 admission/geometry rejections and 4
abort-cleanup retries. The 192 two-OS-actor schedules and 24 joined mark/weak/
relocation lifecycles also use the emitted scan controller, including its
embedded capture, in the candidate arm. ARM checks independently model 184
steps across the complete capture/access/finish/scan chain, canonical movement,
cancelled-slot retry, empty/free entries, full coverage and abort failure/retry.
The relocation-adoption bridge and the joined fixture's mark-owner wrapper
remain C-controlled. The sole published weak inventory, phase freeze and
cross-generation reclamation exclusion remain caller/scheduler obligations;
normal-runtime integration and the default collector migration are unfinished.

Validation: native crate tests passed (73, 45.37 s), ABI tests passed (8), and
mandatory C ASan/UBSan plus TSan passed (186.22 s). Clippy, formatting, diff
and inventory checks passed (25,327 sites / 397 files). The Mach-O fixture
exports 163 helpers; ARM emulation passed 72,395 helper calls across 27
API-model scenarios and 1,536 shared-context transitions, including the 184
new exact-state weak-scan chain steps. Darwin/Windows execution, hardware
memory ordering and end-to-end normal-runtime behavior remain unverified by
these conformance results.

### Earlier weak finish revision


Weak COMMIT/CANCEL and private lease cleanup now have a fixed C entry and
shared PortableAsm emitter (`gc_weak_process_finish_locked`,
`gc_codegen/weak_finish.rs`). COMMIT authenticates the frozen owner and current
lease, then rechecks table/index/serial and the complete captured slot before
clearing a dead referent or transferring the live canonical lease. A disposed,
replaced or changed slot is left untouched while private work is cancelled.
Cleanup retains its exact remaining leases and processor claim on failure;
completed cleanup clears the claim exactly once. CANCEL and stage-5 retries
allow stale mark-owner state after guard exit. Completed-owner validation is
shared with the access emitter.
The fixed finish entry runs in the 40 per-entry lifecycles, 104 C/emitted
whole-state comparisons and 4 partial-cleanup retries. Release failures inside
COMMIT are injected through the C metadata callback; the retained retry uses
the fixed C/emitted finish entry. Independent fixed-entry cases also cover
cleanup failure before release and after a retired token has been released.
The 192 two-OS-actor weak-scan schedules and 24 mark-to-weak-to-relocation
lifecycles now use the complete emitted access and finish operations in their
candidate arm. Their weak-scan controller, scan-driven capture and mark-owner
wrapper remain C-controlled. The new ARM finish cases use an unmapped payload
address, independently checking that metadata-only finish never reads payload.
The per-entry capture/access/finish emission gap is closed at the conformance
helper level. This does not complete G14 or switch the default runtime: emitted
whole-table scan coordination, scheduling/reclamation exclusion and normal
collector integration remain required.

Validation: native crate tests passed (73, 42.39 s); after making the new finish
marker mandatory, the native concurrency harness passed again (50.16 s).
ABI tests passed (8), mandatory C ASan/UBSan plus TSan passed (179.91 s), and
Clippy passed. The Mach-O fixture exports 162 helpers. ARM emulation passed
72,211 helper calls across 27 API-model scenarios and 1,536 shared-context
transitions, including 66 new exact-state finish checks. Formatting, diff and
access inventory checks passed (25,237 sites / 396 files). Actual Darwin and
Windows execution, hardware ordering and end-to-end default-runtime behavior
remain unverified by these conformance results.

### Earlier weak access revision


Weak processing ENTER/OBSERVE/EXIT now has a fixed C entry and shared emitter
(`gc_weak_process_access_step`, `gc_codegen/weak_access.rs`). ENTER authenticates
the captured owner, cleans any retired lease, follows at most one forwarding
edge, and acquires a canonical guard. Retry/backpressure keeps independently
owned metadata and exposes no managed address. OBSERVE requires the completed
owner and held guard; it preserves the slot and mark record while recording a
live/dead result. EXIT bypasses mark-owner validation and clears access state
only after successful guard release, allowing stale work to unwind safely.
Capture and access share the same emitted completed-owner validation.
The 40 existing C/candidate weak lifecycles now drive these whole access
operations. Additional checks cover 96 rejected/retryable ENTER cases and 8
stale-owner exits, including failed EXIT preservation before correction/retry.
ARM exact-memory cases additionally exercise same-page forwarding pressure,
one-edge lease transfer, initializing objects and future mark stamps.
COMMIT/CANCEL, bounded weak-scan coordination and the joined cycle controller
remain C-controlled; default-runtime integration is still incomplete.

Validation: native crate tests passed (73, 48.68 s), ABI tests passed (8), and
mandatory C ASan/UBSan plus TSan passed (166.91 s). Clippy, formatting, diff
and access inventory checks passed (25,176 sites / 395 files). ARM emulation
passed 72,145 helper calls across 27 API-model scenarios and 1,536 shared-context
transitions, including 150 exact-state weak-access steps and the existing 64
capture cases after shared validation was factored out. The new ARM fixture's
initial forwarding-table field offset was corrected before the full passing
run. The Mach-O fixture exports 161 helpers. Actual Darwin/Windows execution,
hardware ordering, collector scheduling and default migration are not proved
by these helper tests.

### Earlier weak capture revision


Weak processing BEGIN now has a fixed C entry and shared PortableAsm emitter
(`gc_weak_process_capture_locked`, `gc_codegen/weak_capture.rs`). The common
C/Rust `WeakProcess` and borrowed `WeakProcessStart` layouts are checked field
by field. Capture requires a completed mark-end owner, the adopted phase,
matching registry/handshake/queue epochs, an unclaimed processor and a serial-
bound table entry. It authenticates the source lease, acquires an independent
metadata lease, then publishes the full source/phase snapshot and processor
claim. Invalid input, stale/empty entries, metadata pressure and exhaustion
leave the output and shared controls untouched. Capture adds neither a strong
root nor a mark; the caller still excludes cross-generation reclamation.
The C/emitted BEGIN entries run in 40 existing dead/live/disposal/cleanup
lifecycles and 120 additional failure-preservation cases. Remaining ENTER,
OBSERVE, EXIT, COMMIT and CANCEL coordination is still C-controlled with emitted
inner helpers. Covered weak scans, the joined cycle controller and the default
runtime have not been converted by this change.

Validation: native crate tests passed (73, 46.07 s), ABI tests passed (8),
and mandatory C ASan/UBSan plus TSan passed (169.82 s). Clippy, formatting,
diff and access inventory checks passed (25,079 sites / 394 files). The Mach-O
fixture exports 160 helpers; ARM emulation passed 71,995 helper calls across
27 API-model scenarios and 1,536 shared-context transitions, including 64
exact-state capture cases. The initial new ARM fixture used the wrong metadata
slot field order; correcting that test data resolved its normal-case rejection.
Actual Darwin/Windows execution and hardware memory ordering remain unverified.

### Earlier mark-end admission revision


Weak mark-end admission now has a fixed three-register ABI and shared
PortableAsm emitter. The existing 40-byte owner moved unchanged into the
common C/Rust ABI. BEGIN closes the runtime-bound gate before calling the
embedded mark-phase helper and rolls closure back on failure. DECIDE retains
closure through outcome adoption; RELEASE reopens only for adopted RETRY and
keeps SELECT closed for weak processing. Exclusive processor/scan claims still
prevent a new BEGIN. No strong work or weak liveness result is fabricated.
The existing 64 two-OS-actor load/end schedules and 20 failure-atomic cases now
run the complete C/emitted weak owner rather than only emitted phase callbacks.
Earlier load publication remains in the real root/journal termination predicate.
This removes the emitted admission-owner gap; per-entry weak processing, covered
scan coordination and default-runtime collection integration remain incomplete.
Validation: native crate tests passed (73, 47.01 s), including the full weak
owner under 64 two-actor schedules and 20 failure-preservation cases. ABI tests
passed (8), alongside Clippy, formatting, diff and inventory checks (24,993
sites / 393 files). Mandatory C ASan/UBSan and TSan passed (195.45 s). The Mach-O
fixture exports 159 entries; ARM emulation passed 71,931 helper calls / 27
API-model scenarios and 1,536 context transitions, including 42 exact-state weak
owner steps. The ARM SELECT case isolates adoption completion; actual SELECT
termination is exercised by the native fixture. Actual Darwin/Windows execution,
hardware ordering and default-runtime integration remain unverified/incomplete.


Weak-handle creation now has a fixed ABI and shared PortableAsm emitter. Its
32-byte borrowed request points to admission, context, a registered source root
and an initially empty output handle. Admission checks are shared with weak
load emission. Creation validates root registration/scope, reads a private
copy of the source request, checks table/serial capacity and acquires metadata
before publishing either the table entry or handle. The source root and journal
remain unchanged. Null sources create handles without metadata; pressure,
closure, stale source handles and exhaustion preserve table and output state.
The existing 48 creation cases and 128 serial-reuse lifecycles now run the
complete C/emitted creation entry rather than C creation with emitted inner
helpers. Weak new/load/release, forwarding and incoming auditing therefore have
shared emitted entries. Weak mark/processing/scan owners, startup coordination
and ordinary compiler/runtime collection integration remain unfinished; these
entries do not switch or certify the default collector.
Validation: native crate tests passed (73, 44.40 s), including the complete
creation entry and serial-reuse lifecycles. ABI tests passed (8), alongside
Clippy, formatting, diff and inventory checks (24,948 sites / 392 files).
Mandatory C ASan/UBSan and TSan passed (169.31 s). The Mach-O fixture exports
158 entries; ARM emulation passed 71,889 helper calls / 27 API-model scenarios
and 1,536 context transitions, including 35 exact-state creation/reuse steps.
Actual Darwin/Windows execution, native hardware ordering and default-runtime
integration remain unverified/incomplete.

### Earlier revisions (historical evidence)


Weak-table LOAD/RELEASE now have a fixed ABI and shared PortableAsm emitter.
The 48-byte borrowed request carries the full table/index/serial handle plus
admission, context and output-root addresses. Both operations validate table
geometry and slot identity through the same emitted validation used by weak
forwarding. LOAD delegates to registered-root/journal publication. RELEASE
invalidates the slot and returns metadata before adding the index to the free
list; it ignores load-only request fields and remains permitted during closure.
Neither operation changes its request or publishes an unrooted managed result.
The 64-case C/emitted fixture covers rooted load, closure/adoption waits,
journal pressure, stale/cross-table handles, metadata failure, repeated release
and serial reuse. The real OS source-retirement fixture also uses the generated
table release, including disposal while a separate source cleanup lease remains.
Creation and weak mark/processing owners are still C controllers; ordinary
compiler/runtime startup and collection-cycle integration remain incomplete.
Validation: native crate tests passed (73, 44.18 s), including 64 C/emitted
table-access cases and the OS lifecycle. ABI tests passed (8), together with
Clippy, formatting, diff and inventory checks (24,886 sites / 391 files).
Mandatory C ASan/UBSan and TSan passed (170.80 s). The Mach-O fixture exports
157 entries; ARM emulation passed 71,854 helper calls / 27 API-model scenarios
and 1,536 context transitions, including 41 exact-state table-access steps.
Actual Darwin/Windows execution, native hardware ordering and default-runtime
integration remain unverified/incomplete.


The whole-table weak incoming audit now has a fixed BEGIN/NEXT ABI and a shared
PortableAsm emitter. The existing 112-byte control moved unchanged into the
C/Rust ABI. BEGIN accepts only the target range initialized, and NEXT rejects a
mismatched table or stale capture. Each successful step captures state, inspects
one slot or closes the scan; findings retain the cursor until repaired. The
weak-follow and incoming emitters share table geometry validation. Neither
operation scans managed payload or closes/reopens admission on its own.
The 48-case fixture now runs the complete C versus emitted audit controller,
including metadata authentication, instead of only emitted inner callbacks.
The OS copy/uncommit/retire/recommit fixture likewise requires the emitted
audit before source retirement and retains the independent cleanup-lease WAIT
check. This removes the emitted-audit gap but not the owning cycle's duties:
sole published inventory, no new target references behind the cursor, other
reference audits, color retirement and default compiler/runtime integration.
Validation: native crate tests passed (73, 43.73 s), including the complete
C/emitted audit and OS lifecycle. ABI tests passed (8), together with Clippy,
formatting, diff and inventory checks (24,812 sites / 389 files). Mandatory C
ASan/UBSan and TSan passed (169.87 s). The Mach-O fixture exports 156 entries;
ARM emulation passed 71,813 helper calls / 27 API-model scenarios and 1,536
context transitions, including 90 exact-state incoming-audit steps plus an
emitted weak-follow repair. Actual Darwin/Windows execution and native hardware
ordering remain unverified. The ordinary runtime has not switched collectors.


Weak-slot forwarding now has a fixed three-register entry
`gc_weak_follow_step_locked(table, request, op)` and a shared PortableAsm emitter.
The table/entry/handle layouts moved unchanged into the common C/Rust ABI, with
a new 40-byte request retaining index, serial and independently owned cleanup.
The generated entry validates table geometry and slot identity, invokes its
embedded pending-follow helper, and transfers reference plus metadata ownership
back to the same slot before returning. It neither publishes strong work nor
reads payload. Existing caller liveness/reclamation exclusions still apply.
The 64-case C/emitted fixture covers transfer, retained/unmoved/busy forwarding,
metadata pressure and exhaustion, stale handles, null entries, deferred cleanup
and failed-state preservation. The exclusive OS copy/uncommit/retire/recommit
fixture now also calls this emitted entry, rather than only emitting its inner
forwarding callback. The whole-table incoming audit and weak phase/scan owners
remain C controllers, and the default compiler/runtime still needs migration.
Validation: 64 C/emitted entry cases and the OS lifecycle passed in the native
harness. The crate run passed 72 tests; its sole failure was the export-count
expectation (154 versus 155), corrected and verified by a targeted fixture
rerun. C/Rust ABI tests passed (8), as did Clippy, formatting and diff checks.
Mandatory C ASan/UBSan and TSan passed (169.48 s). The exported ARM64 helpers
passed 71,722 emulator calls / 27 API-model scenarios and 1,536 context
transitions, including 34 new exact-memory weak-follow steps. The Mach-O
fixture exports 155 entries. Source inventory is 24,741 sites / 388 files.
These results do not establish actual Darwin/Windows execution, native hardware
ordering or default-runtime integration.


The C `gc_weak_incoming` audit now captures the weak table's storage, capacity,
serial frontier, closed admission gate and target range. NEXT examines one slot
and authenticates live slot metadata without accessing payload or acquiring a
lease. FOUND reports a serial-bound handle and keeps the cursor at that slot;
following or disposing it requires rechecking the same slot. An unmoved or
retained reference stays FOUND. Reopened admission, changed geometry/storage or
new serials invalidate a captured scan, including a previously completed scan.
This requires the cycle to retain the sole published table and exclude creation
of target references behind the cursor, including forwarding publication into
the target. The gate alone does not establish that exclusion. DONE proves slot
absence only; it neither reopens admission nor replaces outstanding cleanup
leases, other reference inventories, source lifetime or color retirement proofs.
The 48-case C/candidate fixture checks bounded progress, retargeting, metadata
pressure, stale captures, invalid slot leases and explicit
release between calls for both generations. Its controller is C, with C or
emitted forwarding/metadata callbacks. The existing real OS lifecycle now
requires this audit before source metadata retirement; it also reaches audit
DONE while a deferred source lease still forces retirement WAIT. This is an
exclusive fixture, not a concurrent whole-runtime retirement certificate.
Validation: focused C incoming and OS retirement lifecycles passed. The native
C/emitted-helper harness passed (47.37 s), followed by mandatory C ASan/UBSan
and TSan (168.67 s). Formatting, diff and inventory checks passed (24,675 sites
across 386 files). These are Linux executions; actual Windows/Darwin execution,
an emitted audit controller and ordinary-runtime coordination remain open.


The C `gc_weak_table_follow_locked` controller now transfers one weak slot's
reference and metadata ownership across one forwarding edge under the common
weak/heap lock. It publishes no strong root or journal work and never accesses
managed payload. Metadata pressure preserves the slot. A failed source release
can leave the slot retargeted while a separate cleanup token retains the source;
explicit handle disposal does not dispose that cleanup obligation. The caller
must retain completed-mark liveness and reclamation exclusion; a forwarding
observation alone does not prove source or color retirement safe.
The existing 16-case exclusive OS lifecycle now includes a weak slot owning the
source lease. It performs real copy/publication and source payload uncommit,
retargets the weak slot using C or emitted pending-follow, retires metadata and
recommits the same source range with a different object identity. The canonical
object's identity and bytes survive. Cases also fill the metadata pool and
inject deferred source cleanup followed by handle disposal, proving that source
retirement waits for the independently held cleanup token. This fixture has
one object, one weak slot and exclusive ownership; it does not establish a
concurrent whole-table remap audit or ordinary-runtime retirement coordination.
Validation: the focused C lifecycle passed, followed by the native C/emitted
helper harness (42.75 s) and mandatory C ASan/UBSan plus TSan (163.77 s).
Formatting, diff and inventory checks passed (24,624 sites / 384 files).
These are Linux executions; actual Windows/Darwin execution remains unverified.


The C bridge `gc_weak_scan_relocate_begin_locked` now consumes a covered weak
scan directly under the common phase/weak/heap lock. Relocation BEGIN starts
its adoption handshake before the scan claim is released and weak admission
reopens. Incomplete coverage, a waiting mover gate or invalid/stale state cannot
reopen weak admission. The joined 24-case fixture now proceeds through both
relocation adoption rendezvous, checks that a weak load waits during adoption,
and checks cleared/live handles after adoption. This is a C controller calling
either C or emitted relocation helpers. Selection, exclusive relocation-turn
ownership, drained movers, remap-color reuse and cross-generation reclamation
exclusion remain independent prerequisites; the fixture does not perform
copying, source retirement or an ordinary runtime collection cycle.
Validation: the native C/emitted-helper harness passed (43.32 s), including
all 24 joined lifecycles. Mandatory C ASan/UBSan and TSan passed (163.51 s).
Formatting, diff and source inventory checks passed (24,603 sites / 383 files).
These runs establish Linux execution only; actual Windows/Darwin runs and
normal-runtime integration remain outstanding.

The following mark-to-weak results describe the preceding fixture revision.

A selected-generation mark-to-weak fixture now joins the previously separate
load/end and weak-scan paths. Two OS actors force load-before-close or
close-before-load. Successful early publication forces RETRY, then a real
registered journal CLAIM/COPY retains its lease while PendingMark hands off
references, mark OFFER sets the record stamp, and a guarded descriptor scan
finishes the scalar object job. Only then does journal ACK consume the entry.
A fresh leased root audit performs real mark handoffs before its completion
mask advances; the next end decision reaches SELECT and drives weak scan/reopen.
Records begin unmarked: no fixture write manufactures a successful mark stamp.
Cases retain the early strong root, clear it during marking (the existing
cycle's stamp correctly remains live), or admit no early load (both weak-only
objects clear). Canonical and pre-forwarded references run for Young and Old
separately, with the other generation idle. This joins one mark/weak epoch; it
does not finish selection, relocation, retirement, color reuse or an ordinary
runtime collection cycle, nor overlapping generation coordination.
Validation: native emitted-helper harness passed (43.14 s), including 24
joined C/candidate lifecycles: Young/Old, canonical/pre-forwarded, and no early
load/retained root/root cleared during marking. The mandatory C ASan/UBSan and
TSan suite passed (164.34 s). Formatting, diff and inventory checks passed.
The two-actor load/end ordering ran on Linux; subsequent drain and weak scan
run with exclusive fixture ownership. Actual Windows/Darwin execution and
ordinary-runtime integration remain unverified/incomplete.

The weak scan now has a joined OS-thread load/clear fixture. A collector
context drives capture, guarded observation, EXIT, commit and table coverage;
a distinct registered mutator context attempts loads before clear, after clear,
and immediately before reopen. Closed admission publishes nothing. After
FINISH, a cleared handle returns empty and a live handle publishes the canonical
reference plus journal work into that mutator's registered root. Explicit
release is forced both between observation and commit and after clear; stale
writeback is rejected and private metadata still returns. Hooks wait only
outside guards and the common mutex. Both generations and canonical/pre-forwarded
live objects are covered; the latter is not a concurrently moving object.
This fixture begins with a completed mark-end owner. The existing load/end
fixture separately verifies that pre-closure publication forces RETRY. The new selected-generation fixture joins retry/drain to weak processing.
Ordinary-runtime selection and reclamation are still missing; these fixtures
do not certify G14.
Validation: native emitted-helper harness passed (44.82 s), requiring all 192
two-actor schedules across both generations, two reference locations and three
release timings. The mandatory C ASan/UBSan and TSan suite passed (163.47 s).
Formatting, diff and inventory checks passed. These executions were on Linux;
Windows branches are implemented but actual Windows/Darwin execution remains
unverified, and the weak scan/processor owners remain C controllers.

A bounded C whole-table owner (`gc_weak_scan.h`) now drives the per-entry
processor and reopens admission only after coverage and private-lease cleanup.
NEXT examines at most one slot or consumes one completed item. The owner
captures table storage/capacity/serial and completed phase/epoch/rendezvous;
exclusive scan and processor claims reject competing owners. Metadata pressure
preserves the cursor. Cancelling an extant item retries that index rather than
counting it as covered. Aborting cleans private work but keeps the gate closed
for a fresh full scan. FINISH rejects incomplete cleanup and stale captures.
The contract requires this to be the runtime's entire, single published table
for the gate; aliases, growth and publication are excluded during the scan.
The native controls are trusted runtime inventory, not discovery of arbitrary
unregistered tables. Scheduler exclusion of cross-generation reclamation,
ordinary startup/selection wiring and emitted owner controllers remain open.
The joined fixture contains live, dead, cleared and free entries, plus a moved
live referent whose old header is invalid. After coverage, dead loads are empty
and live loads publish the canonical reference into a registered root/journal.
This does not certify a complete collection cycle or a concurrent mover race.
Validation: native emitted-helper harness passed (44.71 s), including 36
C/candidate scan lifecycles across both generations with metadata pressure,
explicit release, cancellation recapture, abort/rescan, cleanup retry, stale
finish rejection and pre-forwarded live retargeting. The mandatory C ASan/UBSan
and TSan suite passed (162.61 s). Formatting, diff and inventory checks passed.
The scan/processor controllers remain C owners; actual Windows/Darwin execution
and an emitted scan controller are not established by these Linux results.

A C per-entry weak-processing owner (`gc_weak_process.h`) now captures a
slot's table/index/serial and complete source state with an independent metadata
lease. It follows/acquires canonical access in bounded steps, observes liveness,
exits the guard, then revalidates the completed owner and source slot under the
common lock. DEAD clears reference/lease while preserving the handle; LIVE
transfers the independently held canonical lease/reference into the slot and
returns the old source lease. A released/replaced slot receives no writeback.
Cancellation and cleanup retries retain private leases until returned, including
failure after a dead slot has already been cleared. No strong root or mark stamp
is introduced. Table/owner/control storage remain live for the whole operation.
The scheduler must freeze the completed phase and prevent reclamation across
both generations throughout this interval; metadata retention alone is not
payload protection. Existing pending follow/access helpers document this weak
reclamation exclusion as an alternative to a counted strong producer.
This owner has not been emitted or wired into the ordinary runtime. The C
scan owner adds coverage and reopen; phase/selection integration remains open;
per-entry completion is not a completed weak phase or a completed collector.
Validation: the native emitted-helper harness passed (44.16 s) with 40
C/candidate real-object lifecycles across both generations: dead clear, live
lease adoption, metadata pressure, initialization retry, concurrent-disposal
ordering, stale owner cancellation and source/private cleanup failures. The
mandatory C ASan/UBSan and TSan suite passed (162.62 s). Formatting, diff and
inventory checks passed. That per-entry fixture starts with canonical objects; the new scan fixture
adds a pre-forwarded referent. The post-mark load/clear race is now covered separately; a full runtime cycle remains open. It uses
C owner code with emitted helpers, not an emitted weak-processing owner.

Canonical weak liveness now has a read-only C/shared-emitter operation,
`gc_guarded_weak_liveness`. Under an independently retained canonical guard and
metadata lifetime, it compares the stable record's selected-generation stamp
with the completed epoch. Young processing retains canonical Old objects;
INITIALIZING yields WAIT, and future stamps or invalid guards reject. No mark
stamp, lease, root or output changes. The C `gc_weak_mark_observe` entry binds
this observation to a completed Select owner, closed gate, matching runtime
phase, rendezvous and queue epoch. Native owner controls and selected stamps
must remain frozen throughout the guarded interval.
This is a decision only: the per-entry owner supplies serial/owner validation
after guard exit and independent metadata cleanup; the C scan owner adds
bounded coverage and reopen under the runtime-inventory contract.
The leaf does not grant permission to dereference an unprotected weak object or
prove that a source lease alone protects its payload from reclamation. The
normal runtime must prevent selection/reclamation while weak processing runs.
Validation: all 73 native tests passed (41.56 s), including 136 C/candidate
canonical/epoch/owner cases with full-state nonmutation checks. The mandatory C
ASan/UBSan and TSan suite passed (164.93 s). ARM64 emulation passed 52 new
whole-state liveness cases and 71,688 helper calls overall (27 API-model
scenarios, 1,536 shared-context transitions). Clippy, formatting, diff and
inventory checks passed. Emulation does not establish Darwin hardware behavior;
the owner-binding fixture alone does not establish a full weak scan/clear path.

Weak admission is now joined to the mark-end rendezvous by the C owner
`gc_weak_mark_end_locked`. Under the common lock it closes admission before
BEGIN, restores the gate if BEGIN fails, and retains closure through both
rendezvous/adoption steps. RETRY reopens only after ConcurrentMark adoption;
SELECT leaves it closed awaiting weak processing. The owner serializes both
generations through the shared gate. The bounded scan owner is the SELECT reopen path, after coverage and cleanup.
The scheduler must not start selection/reclamation from that outcome until
weak processing has finished; this owner does not issue a cycle/selection ticket.
The two-actor fixture uses registered roots, real journals, worker acknowledgments
and the existing end predicate: a load admitted first publishes strong work and
forces RETRY; a closure first makes the load WAIT and leaves the output empty.
This verifies admission ordering, not object liveness or weak-clear completion.
The wrapper dispatches to C/emitted mark-phase and weak-load helpers, while the
wrapper itself and ordinary-runtime integration still require backend work.
Final validation: the native emitted-helper harness passed (38.26 s), including
64 two-actor schedules across both generations and 20 failure-atomic cases.
The mandatory C ASan/UBSan and TSan rerun passed (157.38 s). The initial native
and sanitizer runs exposed an old nonnull-only audit-fixture assumption; the
new fixture now walks and audits its empty output roots explicitly. Formatting,
diff and inventory checks passed. These OS schedules ran on Linux and do not
prove a weak-clear race or actual Windows/Darwin execution.

Weak-table backing now uses rounded metadata charges and native OS mappings
through `gc_native_weak.h`. Capacity and heap-owner checks precede mapping;
free-list initialization runs outside locks/guards. A committed-package lookup
prevents publication of a pending charge. Live handles, leftover metadata and
malformed free lists reject disposal. Failed unmap preserves the package and
charge for retry, and budget RELEASE/ABORT is allowed only after unmapping.
Published table identity remains live until runtime shutdown has stopped all
handle users and borrowed scans: emptiness alone is not a lifetime proof.
These are C owner adapters using existing native metadata/budget helpers;
direct-backend startup/shutdown integration is still absent.
Validation: the emitted-helper native harness passed (38.39 s), including
six backing scenarios for budget limits, map failure, live-handle rejection,
free-list corruption and pending/committed unmap retries. The mandatory C
ASan/UBSan and TSan suite passed (155.59 s). Formatting, diff and inventory
checks passed. Actual Windows/Darwin execution and emitted weak-owner code
remain unverified/unimplemented respectively.

A bounded C runtime owner (`gc_weak_table.h`) now creates handles from a
registered strong root, resolves table/index/serial handles for rooted load,
and returns disposed entries to an O(1) free list. Creation reads the root in a
no-safepoint interval, acquires source metadata before publication and adds no
strong-root/journal work. Null handles own no metadata. Full tables, metadata
pressure, stale roots and serial exhaustion leave the output and ownership
state unchanged. Table identity and entry storage must remain live for the
runtime lifetime; serials never wrap. Disposed indices can be reused while
stale handles remain rejected. The owner dispatches to C or emitted root,
metadata-acquisition, weak-load and weak-release helpers; the table owner
itself has not yet been lowered to the direct backends. Runtime startup/shutdown
integration and collector-wide phase/selection coordination remain open.
The table is separate from the strong-root inventory and is not traversed as
strong work. Its contract requires the common weak/phase/root and heap locks.
Validation: the emitted-helper native harness passed (37.41 s), covering 48
C/candidate creation/pressure cases and 128 serial-reuse lifecycles, including
rooted promotion after clearing the input root. The mandatory C ASan/UBSan and
TSan suite passed (154.71 s). Formatting, diff and inventory checks passed.
These tests exercise the C table owner with C and emitted x86 helpers; they
do not establish an emitted table owner or ARM/Darwin/Windows table execution.

Weak promotion now has a bounded `gc_weak_load_into_locked` C/shared-emitter
entry and matching WeakLoadGate/WeakSlot/WeakLoadRequest layouts. A slot is bound
to its gate and serial; that gate binds the heap metadata pool, root registry
and handshake. With the common locks held, the leaf validates the registered
owner/output root, authenticates the weak slot's metadata token and object-start
bit, then publishes unresolved reference bits into the empty strong root and its
journal before returning. The weak slot retains its metadata lease and is never
scanned as a strong root by this operation. Failure and WAIT publish nothing.
The leaf neither reads managed payload nor returns an unrooted reference.

The owning weak subsystem must close the shared gate before final strong
termination, keep it closed while dead entries are processed, and clear every
dead entry before reopening. Loads admitted before closure leave counted root
journal work; later loads return WAIT. The new mark-end owner enforces closure
and retry adoption; the C scan supplies coverage/cleanup/reopen. Table startup/
shutdown, collector-wide phase/selection integration and an ordinary-runtime
selection/reclamation cycle remain open.
Admission ordering alone does not prove those responsibilities. G14 and
the ordinary compiler/runtime migration remain incomplete.
Explicit `gc_weak_release_locked` now authenticates slot ownership and its
metadata lease, returns that lease and invalidates the handle under the same
locks as weak-load. It accepts closed admission and preserves previously
published strong roots. Replays and stale ownership are rejected. This is
explicit disposal, not collector dead-entry clearing. The C/shared-emitter
suite adds 32 ownership/replay cases and 64 two-actor OS schedules covering
both load-before-dispose and dispose-before-load ordering.

Current release validation: all 73 native tests and the mandatory C
ASan/UBSan plus TSan suite passed. ARM64 emulation passed 34 combined
load/dispose steps and 20 release steps, with 71,636 helper calls overall
(27 API-model scenarios and 1,536 shared-context transitions). Native Clippy,
formatting, diff and regenerated inventory checks passed. These OS schedules
ran on Linux; actual Darwin/Windows execution is not established here.

Previous weak-load validation: 58 C/candidate cases passed, including slot/gate/runtime
binding rejection; all 73 native tests, eight ABI tests and the mandatory C
ASan/UBSan plus TSan suite passed. ARM64 emulation passed 31 new whole-state
weak-load steps and 71,613 helper calls overall (27 API-model scenarios and
1,536 shared-context transitions). Native Clippy, formatting, diff and inventory
checks passed. ARM emulation is not Darwin execution or a weak-clear OS race.


The joined source-retirement graph reuses 64 fully audited, detached ranges
(two per Young round) at the same virtual addresses. It reserves and OS-recommits
fresh bitmap-backed Young runs through the relocation-reserve path, binds each
TLAB, and checks new incarnation and identity ranges. Before allocation, the
empty bitmap rejects old object locations. Old-incarnation metadata requests
and replayed retirement receipts are rejected without changing the new page.

Each reused run then receives a seven-byte scalar through AllocationInto and a
registered root. Replacement mark records and forwarding tables are charged in
the graph's native package before managed allocation. Initialization and
publication precede guard-protected identity/payload inspection; the allocation
has the exact former object's address but a new identity and distinct MarkRecord.
The publication and deletion journal entries are consumed, the temporary root
scope is retired, and all leases are returned. Old-incarnation acquisition still
fails after replacement publication. The surviving moved original resolves with
its original identity and payload. Accounting includes returned and recommitted
bitmaps and the replacement metadata retained until fixture teardown.

Encoded references can name the replacement once its object-start bit is
published; the prior complete range audits remain the safety prerequisite.
This owner retains other source/forwarding state and does not prove whole-cycle
remap-color reuse, reopen relocation admission, finish the cycle ticket, or
activate the ordinary compiler's generational runtime.
The affected native OS-concurrency test passed (39.26 seconds), as did the
mandatory C ASan/UBSan and TSan suite. Both harnesses require the 64-replacement
marker. Native Clippy, formatting, diff and regenerated inventory checks passed.
No ABI or emitter implementation changed; unrelated suites were not rerun.

The pending-reference mark owner (`gc_pending_mark.h` and the shared
`pending_mark.rs` emitter) now drives SATB, journal, leased root-walk and child-scan
handoffs in the 1/2/8-mutator shared-slot conformance owner. It retains metadata
across forwarding retries, exits the canonical guard before taking a queue lock,
and transfers ownership through the generation's bounded queue/spill inventory.
Cancellation leaves the external producer outstanding; queue-epoch mismatch
preserves the offer until explicit cancellation. The consumer checks routing
and offer results before ACK, so terminal cleanup alone is not proof of marking.
The fixed-epoch owner exercises C and emitted x86 code, including cancellation
with a live guard and a stale queue epoch. This is not yet the ordinary compiler's
runtime owner and does not establish cycle termination or color reuse.

The protected-chain owner additionally consumes registered journal items during
both generations' marking phase. Across 64 real mappings, two historical
payload pages are inaccessible before the pending mark starts. With only two
metadata slots, destination pressure preserves the source token and journal
claim; retry follows both edges, routes the canonical object, and drains each
required generation's scalar job before ACK. A second journal item exercises
duplicate-offer cleanup, while canonical Old objects skip Young routing.
The shared-slot SATB consumer now uses the same owner and independently retains
its original slot metadata lease until both referent handoffs finish. These
owners retain other fixture roots and do not prove sole-root liveness or a full
cycle. Protected-chain execution covers C and emitted x86 on the Linux host;
Darwin/Windows execution and the ordinary compiler migration remain open.

Validation for this integration: all 73 `klassic-native` tests and all eight
ABI tests passed; the required C ASan/UBSan and TSan test passed. ARM64 emulation
passed 71,582 helper calls, including 11 new exact-state pending-mark steps;
this is not Darwin execution. Native Clippy, formatting and diff checks passed.
The regenerated access inventory contains 24,993 candidates across 393 files.
The subsequent SATB/protected-chain integration also passed all 73 native tests
and the required C sanitizer suite, with the new chain marker required by both
harnesses. No emitter or ABI layout changed in that integration; the ARM and ABI
results above are from the preceding owner implementation, not reruns of the
new OS-protected chain schedule. Clippy, formatting, diff and inventory checks
passed again.

The joined mark worker now authenticates its independent scan cursor through
`pending_access` under the heap lock and advances published edges through
`pending_follow`. The access helper discards its private forwarding destination;
the follow helper re-reads the edge under the heap lock. A failed destination acquisition preserves the
entire cursor, separately from the unchanged queue receipt. Existing schedules
move a partially scanned array once or twice, keep its cursor at byte 64 until
canonical retarget, force initial/destination metadata pressure and a busy
source gate, and reject stale receipts after serial rotation. The C and emitted
x86 callbacks use these same helpers in that owner. Ordinary program startup
and full-cycle completion remain outside this conformance integration.
Validation of the worker connection passed all 73 native tests and the required
C ASan/UBSan plus TSan suite (150.96 seconds), as well as native Clippy,
formatting, diff and the regenerated inventory check. ABI and ARM instruction
emission were unchanged; this run does not add ARM or Darwin execution evidence.

The same joined owner's child/control access, copied journal cursors and root-walk
cursors now use `pending_access` and `pending_follow` too. Access authentication
runs under the heap lock, and the private forwarding destination is discarded
before the edge is re-read for lease transfer. Journal and walk destination
pressure retains the complete current reference/token pair; the short-lived
child/control access variant explicitly releases its private lease on failure
while its caller retains the strong producer. A successful edge updates the
copied journal or walk cursor before any poll, leaving the immutable source ring
and external claim intact. This removes the owner's separate manual forwarding
acquisition/release paths; it does not supply a production cycle scheduler.
The affected native OS-concurrency test passed (37.78 seconds), including the
existing moved-journal and root-audit pressure schedules. The mandatory C
ASan/UBSan and TSan suite also passed. Diff and inventory checks passed; the
unchanged ABI/emitter and unrelated native tests were not rerun for this step.



The current source still routes ordinary x86 startup through
`emit_initialize_gc_heap` and `emit_gc_alloc_runtime` in
`crates/klassic-native/src/lib.rs`; the C collector still calls
`gc_rendezvous(gc_finish_mark_action)` in `runtime/gc/klassic_gc.c`.
The 32-byte offset-reference ABI is exercised by conformance owners, while
ordinary generated programs retain the legacy ABI. Passing those owners does
not establish any complete capability below.

| Contract | Current implementation evidence | Missing completion evidence / work |
| --- | --- | --- |
| G01 Young/Old, age, promotion | Generation-aware pages, mark-epoch-gated stable survivor age, bounded promotion remset handoff and Select-driven scalar/reference-bearing promotion and retained-source cases in the joined-worker graph; `gc_survivor_age.h`, `gc_promotion_remember.h` | Complete survivor accounting, adaptive tenuring and a real young-only cycle with promotion in ordinary programs |
| G02 N mutators + workers | Emitted thread lifecycle and page/mark owners, plus shared-slot store dispatch and guarded cancellation/handshake/resumption and one-entry buffer pressure with a concurrent consumer and distinct-page Old-to-Young SATB handoff for 1/2/8 OS mutators; `gc_worker.h`, `gc_pages_pipeline.h`, `gc_rooted_store_concurrency.h` | Default generational runtime running 1/2/8 mutators with background-cycle progress |
| G03 Both-generation concurrent mark | Queue, spill, sliced scan, phase and end checks; `gc_mark.h`, `gc_mark_scan_commit.h`, `gc_mark_phase.h`, `gc_mark_end.h`, rooted offer ownership in `gc_rooted_mark.h` and phase-scoped generation completion in `gc_rooted_mark_set.h`, and context-counted private load owners that gate handshake/termination | A production cycle owner connecting all roots, buffers, queues and in-flight work through termination |
| G04 Concurrent relocation | Guard/forwarding, copy and retirement primitives, plus Select-to-relocation and closed-gate Retire adoption; `gc_relocation_phase.h`, `gc_relocation_complete.h`, protected-source pipelines, rooted heap-finding repair and whole-pool forwarding/heap/root/inventory re-audits followed by independent source table/bitmap cleanup in the joined-worker graph | Ordinary concurrent mutators through complete selection, relocation and safe source reuse |
| G05 References/load barriers | C and shared emitted rooted-load owner with registered snapshot admission and cancellation/rearm over observation/mark/root-publication/receiver-heal leaves, canonical identity, healing, rooted scalar access with exactly-once cleanup and SeqCst logical CAS; `gc_rooted_load.h`, `gc_barrier.h`, `gc_root_access.h` | Ordinary-program use of the native-lock load dispatcher and receiver self-heal, public CAS controller, compiler lowering and bulk-copy integration |
| G06 Store barriers | Guarded SATB/remset prepublication and exact CAS with lease transfers, plus a composed counted controller for existing-root preparation, rooted observation, fresh-attempt retries, cancellation and clean post-handshake rearm with real buffer lease handoff and native-lock step dispatch; `gc_barrier.h`, `gc_rooted_store.h` | Ordinary runtime bindings, general assistance and use of the bounded barrier-poll/close/publication entries, logical CAS/exchange controllers and every compiler/native mutation path |
| G07 Double remembered sets | Inventory, leases, walks and moved-slot pipelines; `gc_remembered_inventory.h` | Production Young flip, pre-flip buffer completion and full young-cycle liveness evidence |
| G08 Concurrent roots | Nonmoving segments, journals, leased walks and retirement, registered scopes with concurrent eight-entry journal pressure, leased mark handoff and detach in the 1/2/8-mutator shared-slot owner, plus Retire root repair, exact-snapshot/revision re-audits and context buffer/consumer/private-producer gates, combined across the registered thread inventory under one rendezvous; `gc_root_lifetime.h`, `gc_root_audit.h`, `gc_mark_end.h` | All ordinary roots migrated, return/call boundaries connected and measured absence of global root traversal |
| G09 Old mark + Young cycle | Overlap models, protected-source promotion retaining Old work and alternating relocation admission; `gc_mark_move_pipeline.h`, `gc_relocation_turn.h` | Actual nested cycle coordination without losing Old work, including promotion |
| G10 Heap/TLAB/metadata | Pool admission, native mapping, allocation, release and paired startup; `gc_heap.h`, `gc_native_startup.h`, `gc_page_release.h`, leased page/object enumeration, cooperative audit and bounded source metadata retirement with exclusive OS-backed same-range reuse in `gc_heap_walk.h`, `gc_object_walk.h`, `gc_heap_audit.h` and `gc_source_retire.h` | Default heap owner, full config/large-page/pre-touch policy and production uncommit/recommit lifecycle |
| G11 Exhaustion/progress | Checked budgets, reserve, explicit pressure and retained cleanup ownership | Real allocation stall/wake/assist, full-cycle recovery and complete OOM/failure matrix |
| G12 Adaptive scheduling | Allocation samples, exact forecast, soft-target pressure connected to cycle request tickets; `gc_allocation_sample.h`, `gc_heap_pressure.h`, `gc_cycle_pressure.h`, `gc_cycle_mark_start.h`, `gc_cycle.h` | Multi-mutator/throughput history, worker-count/generation-capacity/tenuring adjustment and cycle execution |
| G13 Native boundary | Attach/detach, native state transitions and scoped root-storage owners | Generational native handles, public scoped pin/unpin, callbacks and cleanup across blocked I/O |
| G14 Weak handles | Bounded C/shared-emitter `gc_weak_load_into_locked` publishes into a registered output root/journal and rejects closed or mismatched admission; metadata and object-start authentication; explicit lease-returning release with load/dispose OS schedules; C indexed table owner creates from registered roots and rejects serial replay; C end owner closes admission across termination rendezvous; read-only C/shared-emitter guarded liveness and C completed-owner validation; C per-entry dead clear/live lease transfer with cancellation cleanup; C bounded whole-table scan and covered reopen; separate-context OS load/clear/release schedules; joined selected-generation journal/mark/re-audit/weak pipeline | Direct-backend table owner and startup/shutdown, cross-generation reclamation exclusion and normal-runtime weak-end/scan integration and ordinary-runtime mark/weak/selection/reclamation cycle |
| G15 Parity/diagnostics | C and generated x86 conformance, both-ISA lowering, ARM and Win64 API models, ABI/inventory checks | Default direct/C/LLVM integration, actual Windows/Darwin execution and dedicated-machine performance gates |

The public pin/unpin and scheduler entry points required by the instruction
file are likewise absent. `gc_cycle_step_locked` is a request ledger, not a
collector; `gc_mark_phase_step_locked` explicitly leaves epoch preparation,
color reuse and remset flip to its caller. `gc_heap_allocation_pressure` remains
a single-mutator forecast. None may be presented as completion of a full cycle.

The named `gc_generational`, `gc_differential` and `gc_latency` integration test
artifacts are not present. Existing `gc_abi`, `llvm_gc` and `llvm_gc_link` suites
must not be substituted for their requested whole-system scope. The current
access inventory contains 23376 candidate sites across 359 files (including test fixtures); classification
and coverage of all compiler access paths remain open.

The next implementation prerequisite is an owning generational runtime that
connects context/heap/registries, rooted public barriers, workers and full cycle
control. Ordinary allocation, initialization, root ownership and access lowering
must switch together to one consistent ABI. Adding or passing another leaf
fixture does not remove that prerequisite. See
[the compiler migration boundary](compiler-migration.md) for the current
interfaces and their explicit ownership obligations.

Prior full-workspace checkpoint, including ABI v4 counted load ownership (before registry snapshot capture): `cargo test --workspace` exits 0 with
797 passed, 0 failed and 0 ignored across 25 test/doc-test invocations. This
includes the required C sanitizer integration and LLVM backend/link suites on
this Linux host. `cargo build -p klassic-runtime`, `cargo fmt --check` and
`git diff --check` also exit 0. The workspace log is
`/tmp/klassic-heap-audit-owner-final-workspace.log`; build evidence is
`/tmp/klassic-cas-integration-runtime-build.log`. Zero-test doc invocations and
host-gated foreign-target cases do not establish target execution. This result
verifies that checkpoint, not completion of the contract.

## Baseline and execution environment

- Inspected HEAD: `9346bff7ecb9a29e26efed17069567ba29cc91c1`.
- Initial working tree: only the untracked implementation instruction document.
- Host: Linux x86-64, WSL2 kernel `6.18.33.2-microsoft-standard-WSL2`.
- Rust/Cargo: 1.96.0; Clang: Ubuntu 15.0.7.
- `setarch x86_64 -R true` works on this host. The test runner now checks this
  permission before using it, rather than only checking executable presence.
- `cargo run --quiet -- targets` advertises supported direct Linux x86-64 ELF,
  Windows x86-64 PE, and macOS AArch64 Mach-O. This is compiler support metadata,
  not evidence that the new collector works on any platform. Windows/macOS
  execution and dedicated-machine latency measurements have not been run.

## Task 0 boundary regressions

`runtime/gc/gc_boundary_test.c` includes the C runtime for white-box ownership
and scanner tests. Run through `runtime/gc/run_tests.sh`; each case is a fresh
process and the suite has separate ASan+UBSan and TSan binaries.

| Case | Failure observed before fix | Change |
| --- | --- | --- |
| `overflow` | Child returned success for overflowing payload size; assertion failed | Check header plus alignment addition before arithmetic |
| `attach` | Unattached read returned success; assertion failed | Raw read/write/slow-barrier require prior attachment and cannot park |
| `zero` | Region-end zero-payload object failed from-set membership assertion | Derive ownership from its header; accept reservation-end user address |
| `header` | TSan exit 66, race in `gc_trace` header size read | Atomic header load matches concurrent marker's atomic OR |
| `stats` | TSan exit 66, watermark/commit-count races with acquisition and TLAB flush | Lock handshake then heap data for snapshot; atomic collection counter |

The initial attach failing run stopped at the read assertion. Write rejection
is also tested after the fix, but was not separately claimed as reproduced.
The header test exercises the real scanner against atomic updates of the same
header. The statistics observer runs concurrently with allocation and collection.
These tests do not establish SATB, concurrent mutable-object relocation, or
bounded stop time.

Validation after the boundary changes:

- `runtime/gc/run_tests.sh`: exit 0, 14 case executions passed, 0 failed:
  each sanitizer runs the single-mutator suite (8 checks), multi-mutator suite
  (4 mutators x 100,000 allocations plus collection ticker), and 5 boundary cases.
- `cargo test -p klassic-native`: exit 0, 36 tests passed. The separate doc-test
  invocation ran 0 tests and is not coverage evidence.
- `cargo build -p klassic-runtime`: exit 0 (required static library generated).
- `cargo test --workspace`: exit 0, 750 tests passed, 0 failed/ignored. This
  includes 509 CLI tests, 11 LLVM backend tests, the sanitizer integration
  test, and the LLVM GC link test. Host-gated foreign-OS cases are not claimed
  as execution coverage.
- `cargo fmt --check`, `cargo clippy --all-targets --all-features -- -D warnings`,
  `git diff --check`, and `bash -n runtime/gc/run_tests.sh`: exit 0.
- The final attach case also checks the exported slow barrier. The zero case
  additionally verifies real relocation of the rooted region-end empty object
  and validity after a second cycle retires its source page. Both strengthened
  cases passed fresh ASan+UBSan and TSan runs (4 executions, exit 0).

The runner reports failures including sanitizer startup failures; no race
suppressions or skip-success path were added. This does not turn ASan into a
managed-heap logical UAF detector: page quarantine/protection is still required.

## Baseline pause and access audit

Observed in `runtime/gc/klassic_gc.c`:

- `gc_rendezvous` starts its pause timer only after every mutator parks. It
  excludes time-to-safepoint and earlier mutators' waiting time.
- MarkStart walks all roots. `gc_finish_mark_action` rescans roots, and
  `gc_mark_end` drains the remaining graph, frees ghost regions, sweeps all
  objects, selects relocation regions, and evacuates root targets while stopped.
- `gc_trace` budgets objects rather than slots, so a large pointer object can
  consume an unbounded amount of work relative to that object budget.
- Load-barrier healing and trace recoloring still use unconditional atomic
  stores. A generational deletion barrier and remembered sets do not exist.
- The statistics snapshot is synchronized, but is still O(committed regions)
  and counts published watermarks; it is not the final low-overhead telemetry.

Observed in the direct emitter: `portable_asm.rs` contains the shared allocation,
mark/trace/sweep, barrier, root, and relocation routines. `aarch64.rs` calls these
routines; the old claim that Mach-O has no moving GC is incorrect. The baseline
AArch64 pause measurement used `timing = false`; Task 2 now emits a real clock
(see the macOS evidence below). `emit_gc_mark_end` and
`emit_gc_stw_mark_complete` require the same traversal audit/removal as C.

`gc_test_hooks.h` now provides fixture-owned logical object IDs and a test-only
scanner seam. The header regression uses a condition-variable latch to start
the marker at that seam; production builds compile the hook away. Fixture
identity lookup is not production identity/reclamation machinery.

The [access inventory](access-inventory.md) and generated JSON cover 6,280
candidate sites across forty source files, including raw instructions, atomics, static
data, memory operations, roots, output calls and blocking boundaries. Source
hashes and `tools/gc_access_inventory.py --check` detect a stale index. Value
provenance classification and independent review are not certified complete.
Entry points for the remaining migration audit:

| Path | Funnels / sites requiring review |
| --- | --- |
| `portable_asm.rs` | `emit_gc_alloc_object`, box/unbox, load barrier, shadow push/pop, deep equality, all GC walkers |
| `lib.rs` | `compile_gc_write`, allocation calls, record/list/map/enum/closure/string lowering, builtin loops and OS calls |
| `aarch64.rs` | shared emitter adapters, `__gc_write`, object lowering, raw/interior temporaries and OS calls |
| `llvm.rs` | `push_root`, scalar boxes/record fields, inline atomic loads, entry/backedge polls, argument and return staging |
| `cbackend.rs` | separate subset/runtime ownership; must explicitly decide and test GC integration |
| `klassic_gc.c` | alloc/read/write/slow barrier, shadow roots, attach/detach, worker/handshake, copy/trace/heal actors |

## Task 1 worktree evidence

- `gc_abi.rs` / `gc_abi.h` implement colored reference arithmetic, committed-start
  validation hooks, coherent phase proposals, barrier comparison masks and the
  common root/buffer/mutator layouts. C/Rust conformance checks all size,
  alignment, offsets and constants, plus 32 store and 16 forgotten colors.
- `cargo test --test gc_abi`: 8 tests pass. The initial test failed to import the
  missing module; the forgotten-stamp regression subsequently failed under the
  too-restrictive encoding, then passed after adding the persistent slow state.
- `cargo test -p klassic-native gc_model -- --nocapture`: 14 tests pass. Access
  exploration covers 222 states per generation; graph exploration covers 6,157
  states, 19 terminals and 5,266 overlapping-mark states.
- Model regressions cover old queues across Young movement/promotion, root reuse
  before buffer flush, repeated minor cycles/null rewrite, mark-bit ownership
  before queue publication, dead-owner metadata, source reuse, copy/mark overlap
  and remset forgetting across color wrap. Injected faults must produce
  counterexamples rather than merely passing the correct algorithm.
- The copy/mark overlap regression failed with a retired-page reference before
  mark state moved to a shared nonmoving metadata record. The forgotten-slot
  graph regression failed when a two-cycle wrap incorrectly enabled a store
  fast path; double-bit remembered metadata prevents it.
- Model helpers are not runtime implementations. See
  [design and model bounds](generational-design.md) and
  [remaining proof obligations](invariants.md). No new ABI is connected to
  generated binaries or the C collector yet.
- Final `cargo test --workspace`: exit 0, 772 passed, 0 failed/ignored across
  25 test/doc-test invocations (zero-test doc suites are not coverage). This
  includes the C sanitizer runner and LLVM link/backend tests on this host.
- Final `cargo clippy --all-targets --all-features -- -D warnings`,
  `cargo fmt --check`, `git diff --check`, and inventory `--check`: exit 0.
  Clippy first rejected a nonminimal boolean in a fixture; it was simplified
  and the final lint run passed.

## Task 2 emitted concurrency evidence (partial)

- `PortableAsm` now emits 64-bit acquire load, release store, strong AcqRel CAS
  (Acquire on failure), AcqRel fetch-or, and a full fence. x86-64 uses aligned
  MOV, LOCK CMPXCHG and MFENCE; ARM64 uses LDAR/STLR, an LDAXR/STLXR retry loop
  and DMB ISH. Fetch-or shares a portable CAS loop. Register clobbers and
  alignment requirements are part of the trait contract.
- `cargo test -p klassic-native gc_atomic -- --nocapture`: 6 tests pass on this
  Linux x86-64 host. The C conformance driver executes the actual emitted
  instructions in two threads: 4,096 CAS winner, 4,096 fetch-or, and 4,096
  release/acquire publication trials. Golden encoding checks cover both ISAs;
  Win64 argument normalization and ARM64 lowering are generated here, but
  foreign-OS/ARM64 execution has **not** been performed.
- `gc_codegen/threads.rs` emits Linux x86-64 worker startup directly with clone,
  custom FS-based TLS, thread-only exit, and clear-tid futex join. Stack mapping,
  guard-page protection, unmapping, and private futex park/wake are also emitted
  syscalls. The driver starts two such workers (without pthread startup),
  checks their distinct TLS/context pointers, shared atomic updates, stack
  bounds/alignment, exact 32-bit clear-tid words and adjacent canaries, and
  preservation of all SysV nonvolatile GPRs at the parent entry points.
- The driver observes a real parked waiter through the kernel wake count, then
  tests predicate rechecking and publication-before-park (`EAGAIN`). Both guard
  pages fault in isolated child processes. Invalid clone flags produce `EINVAL`;
  invalid/overflowing stack sizes are rejected; mmap failure is returned; injected
  mprotect failure unmaps the reservation (checked against inaccessible mappings).
  Already-terminated join returns successfully. No sleep is used to force races.
- The first atomic harness failed to compile before the trait methods existed.
  The stack/park/wake extension likewise failed on missing emitters before its
  implementation. These are missing-implementation failures, not claims that
  a preexisting runtime race was reproduced. Subsequent compile/lint failures in
  fixtures were corrected before the passing run.
- The driver is an external compiler test: it loads generated bytes into RX
  memory and uses C only for setup/assertions. It does not add a C compiler,
  assembler or linker to the direct compilation path. Its own control gates and
  payloads use C atomics; the operations under test are emitted instructions.
  The separate legacy C sanitizer suites do not instrument these machine bytes.
- These are exercised code emitters, not yet connected to the normal program's
  GC startup. The fixture's worker payload is not a second `MutatorContext` ABI.
  Attaching mutators, context publication/retirement, repeated handshakes,
  portable TLS/lifecycle interfaces, and real Windows/macOS execution remain
  open. Windows and macOS adapter work is described below. Task 2 and G02
  are not complete.
- Regression validation: `cargo test --workspace` passed 778 tests with 0
  failures/ignored across 25 test/doc-test invocations, including the legacy C
  sanitizer runner. `cargo clippy --all-targets --all-features -- -D warnings`,
  formatting, diff whitespace, and inventory freshness checks pass. Clippy's
  constant-assertion finding in the host guard was corrected to a const block.

Instruction semantics were checked against the [Intel instruction manuals](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html)
and [Arm LDAXR documentation](https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/LDAXR--Load-Acquire-Exclusive-Register-).
Linux worker argument order and TLS/clear-tid behavior follow [clone(2)](https://man7.org/linux/man-pages/man2/clone.2.html);
parking uses the compare-and-block contract in [futex(2)](https://man7.org/linux/man-pages/man2/futex.2.html).

## Task 2 Windows adapter and clock evidence (partial)

- `gc_codegen/windows.rs` emits Win64 worker creation/bootstrap/join using
  CreateThread, TlsSetValue, WaitForSingleObject, GetExitCodeThread and
  CloseHandle. Bootstrap initializes TLS before calling the entry, clears it
  afterward, and propagates TLS errors as the thread exit code. Join closes the
  handle only after a successful wait and exit-code retrieval; failures retain
  ownership for retry. Launch metadata stays alive through join.
- Each worker owns an auto-reset event for park/wake. A notification made before
  parking persists, notifications may coalesce, and workers recheck their acquire
  predicate around waits. Waking several workers requires signaling their
  individual events. TLS index ownership is process-wide; indices cannot be
  freed while workers still use them. These are OS primitives, not yet mutator
  attach/detach, phase handshakes or the default collector's worker startup.
- The PE import list contains the nine added kernel32 APIs. Tests parse the PE
  section/import tables and check every emitted code-label/IAT displacement;
  the CLI PE structural regression also checks the complete new import list.
- The Win64 harness executes the actual generated instructions on this Linux
  x86-64 host against explicitly modeled APIs using `ms_abi` C functions and
  pthreads. It passes 32 two-worker lifecycles, independent TLS, aligned callback
  stacks, atomic updates, notify-before-park/auto-reset, startup/TLS failure,
  TLS teardown failure, and join retries after exit-code/close failure. The
  model poisons upper halves of DWORD/BOOL returns to check explicit 32-bit
  normalization, and verifies all modeled handles and TLS keys are released.
  This is **not Windows OS execution**. On a Windows host the same driver binds
  real kernel32 APIs, with a separately labeled result; injected API-model
  failures run only in the model.
- Windows monotonic time now uses QPC/QPF with stack-local scratch and a full
  128-bit multiply before division. API errors, invalid frequency/counter and
  quotient overflow return errors without publishing a timestamp. The driver
  checks 10,000 increasing modeled samples, 25 arithmetic boundary combinations
  and failure cases. Its Windows branch checks 10,000 real samples for monotonic
  order and elapsed progress; that branch has not been run here.
- The direct Windows GC clock is connected through a legacy-register shim, and
  the three Windows timing-disable gates are removed. Clock failure reports an
  error rather than silently publishing zero pause time. A regression first
  reproduced the old Linux-syscall tripwire on the Windows target and now passes.
  The existing Windows stress/poison CLI test additionally requires nonzero
  GC pause timestamps; execution of that host-gated test is still outstanding.
  The baseline pause interval definition and O(heap) stopped work are unchanged.
- `cargo test -p klassic-native gc_windows -- --nocapture`: 4 tests pass on this
  host (layout, GC-clock connection, PE import/relocation, API-model execution).
  An attempted local `powershell.exe` invocation returned `exec format error`;
  presence of the Windows files on `/mnt/c` is not usable Windows execution.
- Regression validation: `cargo test --workspace` passed 782 tests, 0 failed or
  ignored, across 25 test/doc-test invocations. This includes the C sanitizer
  integration and updated CLI PE-import test. Clippy with warnings denied,
  formatting, diff whitespace and inventory freshness checks pass. The final
  clock connection test additionally verifies both timestamp calls in all
  three generated stopped phases on Windows.

OS contracts were checked against Microsoft's [thread creation](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createthread),
[TLS usage](https://learn.microsoft.com/en-us/windows/win32/procthread/using-thread-local-storage),
[auto-reset events](https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-createeventa)
and [QPC documentation](https://learn.microsoft.com/en-us/windows/win32/api/profileapi/nf-profileapi-queryperformancecounter).
No latency or real Windows conformance claim follows from API-model execution.

## Task 2 Mach-O imports and macOS clock evidence (partial)

- The direct Mach-O writer now emits optional libSystem dylib imports, a
  file-backed `__DATA,__got` section, eager `LC_DYLD_INFO_ONLY` binding opcodes,
  undefined-symbol and indirect-symbol tables, and an updated ad-hoc signature.
  BSS follows the GOT's padded file page without contributing file bytes. Code
  addresses remain PC-relative; dyld binds each GOT slot before entry. Images
  without imports preserve the earlier zero-fill-only layout.
- ARM64 import calls use ADRP/ADD/LDR/BLR through x16. The assembler deduplicates
  repeated symbol references. The clock uses `_clock_gettime` with Darwin's
  `CLOCK_MONOTONIC = 6`, stack-local timespec storage, and checked nanosecond
  conversion. It saves volatile GPRs, all vector registers and FP/LR around the
  foreign call. API failure or invalid/overflowing time aborts with a diagnostic
  instead of publishing a zero timestamp.
- The AArch64 GC's `timing = false` and clock stub are removed. A cross-host
  compiler regression checks that logging emits six clock calls (start/end for
  each of three stopped phases) and that logging disabled emits no import.
  This changes the clock source; it does not remove stopped graph work or fix
  the legacy pause interval definition.
- `cargo test -p klassic-native macho::tests -- --nocapture`: 9 tests pass on
  Linux. They verify GOT/BSS separation with and without BSS, code address
  fixups, binding opcodes, symbol/indirect tables, segment ranges, and every
  signed-page hash including binding metadata. The initial import test failed
  because the writer had no import API. Two subsequent fixture assertions used
  incorrect hardcoded string lengths; those assertions now derive the lengths.
- Independent `llvm-objdump-15 --macho --bind --indirect-symbols --dylibs-used`
  recognizes the generated `_clock_gettime` binding to libSystem and its GOT
  slot. LLVM disassembly confirms the save/restore instructions, indirect call,
  checked arithmetic and failure branch encodings. Parser/disassembler evidence
  is not a dyld load or ARM execution result.
- A standalone generated Mach-O test runs 10,000 monotonic clock samples on
  macOS ARM64, with a process timeout. The existing macOS GC logging test now
  requires nonzero pause timestamps as well as collections. Both require an
  actual macOS ARM64 host and have **not** executed here. Worker/TLS/condition
  emission is described below; shared context lifecycle remains open.
- Final host regression: `cargo test --workspace` passed 786 tests, 0 failed or
  ignored, across 25 test/doc-test invocations. Clippy with warnings denied,
  formatting, diff whitespace and the 4,204-site inventory freshness check
  passed. Foreign-host gated execution tests are not included in that claim.

Format details follow Apple's [Mach-O loader definitions](https://github.com/apple-oss-distributions/xnu/blob/main/EXTERNAL_HEADERS/mach-o/loader.h)
and [symbol-table definitions](https://github.com/apple-oss-distributions/xnu/blob/main/EXTERNAL_HEADERS/mach-o/nlist.h).
The clock ID comes from Apple's [libc time header](https://github.com/apple-oss-distributions/Libc/blob/main/include/_time.h).
No external compiler, assembler, linker or codesign command is used to build
these direct-backend images.

## Task 2 macOS worker and synchronization evidence (partial)

- `gc_codegen/macos.rs` emits AAPCS64 helpers for pthread creation/join, TLS
  key creation/deletion and get/set, and per-worker notification events backed
  by a pthread mutex and condition. libSystem owns the native worker stack and
  guard. The launch record and TLS key must outlive successful join; returning
  from the bootstrap exits the worker only. TLS setup failure bypasses entry;
  teardown failure is returned in its exit status. These are OS adapters, not
  another mutator-context ABI or a language-level thread implementation.
- Notification predicates are protected by the mutex. Wake-before-park is
  retained; repeated wakes coalesce; park loops after spurious wakeups. Waits
  must be outside heap-access guards. Event storage must remain fixed and
  unpublished until initialization succeeds. Its ownership mask records each
  initialized OS object; failed rollback/destroy preserves the remaining bits
  for retry, without double destruction. Destruction requires quiescence.
- The ARM64 assembler resolves ADR label addresses for callbacks and thread
  thunks, with a checked signed 21-bit range. Mach-O binds all 14 pthread
  imports through the existing GOT machinery. LLVM independently parses each
  binding and indirect symbol in the exported worker image.
- `cargo test -p klassic-native gc_macos -- --nocapture` passes 3 tests on
  Linux: ABI layout, forward/backward ADR encodings, and standalone image
  generation. The initial missing-emitter test failed before implementation;
  a subsequent fixture usize/u64 mismatch was corrected before this run.
- `tools/gc_arm_emulation.py` executes the emitted ARM64 instructions using
  optional Unicorn 2.1.4 and a deterministic pthread API model. It passes
  **27 scenarios / 164 helper calls**: initialization rollback, partial cleanup
  and retry, duplicate init/destroy, startup failure, TLS setup/teardown
  failure, notification coalescing, wake-before-park, spurious wakes, lock/wait/
  signal/unlock errors, and first-error retention. It checks stack alignment,
  callee-saved GPRs, the reserved x18 register, int-return upper-bit handling,
  and full-width keys/pointers. The actual fixture callback and CAS-based OR
  also execute in both serial worker orders. This is not a pthread scheduler,
  a dyld loader, native ARM execution, or hardware memory-ordering evidence.
- A macOS ARM64 test executes the generated standalone image with two real
  pthread workers, separate TLS contexts, aligned stacks, notification events,
  shared atomic OR, join results and cleanup. It has a process timeout and
  **has not executed on this Linux host**. It does not force a real blocked
  waiter; native scheduling/publication tests and 1/2/8-mutator runtime
  progress remain outstanding. Normal GC startup, portable lifecycle entry
  points, shared context attach/detach and repeated handshakes are unwired.
- The CI cross-build job exports both worker and clock images on Linux; the
  macOS cross-execution job requires both images and executes each with a
  45-second timeout and an ARM64 host check. They are separate from the
  preexisting best-effort language fixture list, so a missing GC image fails
  the job. This workflow change is locally checked but has not run on CI.
- Host regression: `cargo test --workspace` passes **789 tests**, 0 failed or
  ignored, across 25 test/doc-test invocations. Clippy with warnings denied,
  formatting, diff whitespace and the 4,347-site inventory check pass. The
  host-gated macOS execution test is excluded from that count.

Reproduce the optional instruction-model check without adding dependencies to
the compiler (install `unicorn==2.1.4` into a separate Python environment):

```sh
KLASSIC_GC_MACOS_WORKER_DIR=/tmp/klassic-macos-workers cargo test -p klassic-native gc_macos_worker_fixture
python3 tools/gc_arm_emulation.py /tmp/klassic-macos-workers
llvm-objdump-15 --macho --bind --indirect-symbols --dylibs-used /tmp/klassic-macos-workers/worker.macho
```

Native signatures and LP64 object sizes follow Apple's
[pthread declarations](https://github.com/apple-oss-distributions/libpthread/blob/main/include/pthread/pthread.h)
and [pthread type definitions](https://github.com/apple-oss-distributions/libpthread/blob/main/include/sys/_pthread/_pthread_types.h).
This work does not complete Task 2 or G02.

## Task 2 C context and native lifecycle evidence (partial)

- The C collector now uses the shared `MutatorContext` for state, thread
  identity and the actual allocation TLAB. The private TLAB triple was removed.
  `gc_threads.h` provides `gc_thread_attach`, `gc_thread_current`,
  `gc_thread_detach`, `gc_enter_native` and `gc_leave_native`; legacy
  register/unregister APIs use the same owner and table. Thread identities
  never repeat; identity/handshake-generation exhaustion is diagnosed.
- Running/AtSafepoint/BlockedNative/Detaching/Detached transitions are real
  runtime state. The existing handshake mutex publishes private roots/TLABs
  and serializes membership. A BlockedNative owner is excluded from the
  expected park count; return to managed execution waits for any pending
  request. Enter/detach during a request still park as counted Running owners.
  Managed allocation, root mutation, raw reads/writes and polls reject blocked
  access. Lifecycle transitions reject the wrong owner/state or a nonzero
  access-guard epoch; this does not itself implement the future access guards.
- `gc_lifecycle_test.c` runs 1/2/8 real pthread owners blocked on a condition
  while the controller completes 32 collections per cohort. Roots are placed
  in inactive sparse regions; the test requires actual relocation while the
  owners stay blocked and checks their rooted payloads after native return.
  The first strengthened fixture intermittently failed for eight owners:
  its legacy heap budget left only one free region, and the existing selector
  deliberately chose no relocation. The fixture now reserves 32 regions of
  budget before starting owners, retaining the assertion that moves occurred.
  This corrects the test's precondition; it does not claim improved relocation
  under exhaustion or change the production selection policy.
  Thirty consecutive ASan+UBSan executions of the corrected fixture pass;
  each cohort records 32/64/256 actual relocations while its owners are blocked.
  Another 48 controlled handshake rounds hold a Running owner before poll,
  native entry or detach while a late attach and a native return wait on that
  exact request. Assertions check state, membership, count and completion.
  Test-only wait hooks publish latch observations without blocking under the
  handshake lock; no sleep forces the ordering. Ten rejection/exhaustion cases
  run in child processes created before the parent initializes the collector.
- `runtime/gc/run_tests.sh` passes **16 case executions**, 0 failed, in separate
  ASan+UBSan and TSan builds, including both executions of the moving-root
  lifecycle suite. The Cargo sanitizer integration now requires a compiler
  and checks that both lifecycle runs actually executed. No production build
  dependency, sanitizer suppression or skip-success path was added.
- `cargo test --workspace` passes 789 tests, 0 failed/ignored. After strengthening
  the moving-root fixture and correcting its headroom precondition,
  `cargo test --test llvm_gc` passes with both complete sanitizer builds.
  Clippy with warnings denied, formatting, diff whitespace and the 4,381-site
  inventory freshness check pass.
  The lifecycle assertions are validation of the new C path, not a claim that
  its previously absent API was already exercised before implementation.
- Generational phase adoption, buffer handoff, concurrent root segments and
  scanner-safe retirement remain unimplemented. `adopted_phase` remains the
  initial snapshot, and legacy stack roots are scanned only while their owner
  is quiescent. The native API forbids raw managed buffers across the boundary;
  handles/copy-out/scoped pins and compiler call-site wiring remain Task 4 work.
  Direct emitters still need this lifecycle integration. The existing stopped
  graph work and action-only pause timer remain; this does **not** establish
  G02, G08, G13 or a bounded-pause generational collector.

## Task 2 exact-width atomics and shared state publication (partial)

- PortableAsm now has 32-bit acquire load, release store and strong AcqRel CAS
  (Acquire on failure), alongside its 64-bit operations. This is required by
  the shared context's state at byte offset 4: a 64-bit access would overlap
  thread identity. Inputs to the u32 operations use their low 32 bits; results
  are zero-extended, including successful x86 CMPXCHG with a dirty high RAX.
  x86 uses MOV/LOCK CMPXCHG without REX.W; ARM64 uses W-register LDAR/STLR and
  LDAXR/STLXR with exclusive retry and CLREX on mismatch.
- `gc_context.h` and `gc_codegen/context.rs` implement the same finite state
  publication rule: validate ABI, expected state, zero access epoch and legal
  edge, then release-store the state. Failure preserves the entire context.
  The C runtime uses this step inside its existing coordinator mutex. The
  emitted body uses offsets derived from the common Rust ABI; neither side
  introduces another context layout. Status and edge rules are checked against
  an independent test matrix.
- `cargo test -p klassic-native gc_atomic -- --nocapture` passes 8 tests.
  The C driver executes actual Linux x86-64 emitted code for 4,096 additional
  u32 CAS-winner and 4,096 u32 release/acquire publication trials. It checks
  adjacent ABI fields, high-bit arguments/returns and a word at the last four
  bytes before a protected page, so a wide load followed by masking still
  fails. It compares 1,536 C/emitted context cases and every preserved byte.
  These tests address a missing primitive; no preexisting state corruption
  was claimed as reproduced before this implementation.
- ARM64 instruction emulation passes the same 1,536 context cases and the
  exact-width/high-bit/unmapped-neighbor checks. LLVM MC independently confirms
  the new instruction encodings. The standalone Mach-O fixture now also
  invokes the shared state body on an isolated context and validates state,
  version and thread-identity preservation. Its Linux-export/macOS-execution
  CI route remains configured. Actual ARM64 hardware and Windows execution
  have not been performed here; emulation does not prove weak memory ordering.
- `cargo test --workspace` passes **791 tests**, 0 failed/ignored, across 25
  invocations, including the real C lifecycle tests under both sanitizer builds.
  Clippy with warnings denied, formatting and diff checks pass. The inventory
  at this step indexed the C inline context implementation and portable emitter,
  and classified the u32 atomic memory operations (4,423 sites across 14 files).
- This primitive requires exclusive context ownership and the registry lock
  for registered contexts. It does not supply the coordinator, native TLS
  membership, participant counting, phase adoption, buffer handoff or scanner
  retirement. Direct lifecycle/startup integration remains open. Observing the
  same state after a self-edge cannot certify a new handshake generation.

## Task 2 shared handshake bookkeeping (partial)

- `HandshakeState` and `ThreadRegistration` have matching Rust/C layouts, checked
  by the ABI suite. `gc_handshake.h` and `gc_codegen/handshake.rs` implement
  register, acknowledge, resume, native entry/return, detach and request begin/end.
  They require an existing registry lock and unique, nonmoving registrations.
  A per-entry generation prevents duplicate counting and supports a new request
  before an owner resumes from the preceding request. Native owners are excluded;
  membership changes wait until the active request ends. Identity and generation
  exhaustion fail without wrapping. Rejected steps preserve all supplied bytes.
- The C runtime uses this common bookkeeping for its actual attach/detach,
  native transitions and collector rendezvous. Its previous independent counter
  updates are removed. The existing atomic fast-poll flag is published/cleared
  after begin/end under the same registry mutex. The generation identifies a
  handshake request; it is not generational phase or buffer adoption.
- On Linux x86-64, `cargo test -p klassic-native gc_ -- --nocapture` passes
  **34 tests**. The emitted-code driver checks **41,512** C/generated-code cases,
  including explicit protocol sequences, invalid states and preservation.
  Another test executes generated steps with **1/2/8 OS-thread owners and 384
  rounds**, forcing consecutive requests before resume and checking duplicate
  suppression, detach and progress outside the coordinator lock. The C fixture
  supplies OS locks/condition variables; its progress counter is test work,
  not managed-heap work or evidence for G02.
- The exported ARM64 helper passes instruction emulation of all **41,512** C
  before/after vectors, alongside the existing 1,536 context cases and 27 API
  scenarios. Export with `KLASSIC_GC_HANDSHAKE_VECTORS=/tmp/handshake.bin` during
  the native GC tests and pass `--handshake-vectors /tmp/handshake.bin` to
  `tools/gc_arm_emulation.py` with the exported macOS fixture directory.
  This checks generated instructions and preserved state, not ARM hardware
  memory ordering or actual Darwin/Windows lifecycle execution.
- The access inventory includes both handshake implementations: **4,532 sites
  across 16 files** at this step. Direct production registry assembly/startup,
  phase adoption, buffer transfer and concurrent scanner retirement remain open.
  The legacy C collector still has unbounded stopped graph work and action-only
  pause timing. No capability gate below is closed by this bookkeeping step.

## Task 2 Linux emitted owner lifecycle (partial)

- Linux registry synchronization now has emitted exact-width CAS/futex locking,
  release/wake-one unlock and sticky binary notifications. Each owner and the
  coordinator have separate event words. Under the registry lock a waiter checks
  its predicate, clears only its own event, unlocks, then waits on zero. A racing
  notification sets one, so wake-before-park returns EAGAIN. There is no wrapping
  notification counter. Futex errors are returned, not silently retried forever;
  only wake, EAGAIN and EINTR cause a contended lock retry.
- The native driver now runs **1/2/8 raw-clone owners for 384 additional rounds**.
  Their custom FS TLS points to the common `MutatorContext`. Generated worker
  code performs registration, poll/ack, resume and detach using the same emitted
  handshake operations and Linux synchronization helpers. Workers call no libc
  or C callbacks. The C parent owns storage and issues requests through generated
  operations; this is still a conformance fixture, not normal program startup.
- The test forces a second request before any owner resumes, checks progress
  outside the registry lock, TLS identity, stack alignment/range, adjacent-word
  preservation and kernel clear-tid join before freeing stacks. Misaligned futex
  failure is checked for lock, unlock and notify. Owner work remains a progress
  publication, not managed graph mutation or collector throughput.
- The targeted emitted-code test passes on Linux x86-64. Inventory coverage is
  now **4,536 sites across 16 files**. The final workspace run passes **791 tests**,
  0 failed/ignored across 25 invocations, including C ASan+UBSan and TSan suites.
  Clippy with warnings denied, formatting and diff checks pass.
  Windows/macOS need corresponding lifecycle
  execution, and all direct backends still need normal startup, native boundary,
  roots/buffers and generational phase integration before G02/G13 can close.

## Task 3 owned Young TLAB attempt (partial)

- `gc_heap.h` / `gc_codegen/heap.rs` now allocate actual initializing objects
  within owned, committed, prezeroed TLAB storage. They share descriptor, page,
  header, request and result layouts with Rust/C size/offset/constant assertions.
  Checked fixed-prefix/array-element arithmetic, alignment, owner/page bounds,
  object-start bits and identity arithmetic precede every mutation. Refill,
  invalid input and overflow leave context, page, payload, bitmap and output
  unchanged. Zero-byte payloads reserve a granule so their addresses stay inside
  the owner page even at an exact-fit boundary.
- Headers retain immutable payload size, descriptor id and logical identity,
  with an atomic INITIALIZING state. Release publication of the object-start
  bitmap and allocated prefix follows header initialization. No payload scan,
  memset, OS call, lock, poll or fallback allocation occurs in the leaf. The
  result is a private no-safepoint handoff requiring root adoption, not a public
  `gc_alloc_into` API or completed object publication.
- The native driver passes **2,722 C/x86 cases**, using independent 128-bit size
  arithmetic, all 32 color epochs and 64 phase pairs, descriptor alignment,
  bitmap bit 63, multiple bitmap words, identity exhaustion, exact fit and refill.
  The same C before/after memory vectors pass **ARM64 instruction emulation**.
  New portable unsigned wide multiplication and variable shift are executed by
  these tests; LLVM MC independently confirms the ARM instruction encodings.
- The required C sanitizer runner now includes `gc_heap_test.c` under both
  ASan+UBSan and TSan. Its producer/reader test checks concurrent header/bitmap
  publication over 32 rounds in addition to the conformance cases. ASan guards
  the fixture allocation; this is not a managed-source-retirement/UAF test.
- `cargo test --workspace` passes **791 tests**, 0 failed/ignored across 25
  invocations. The required C runner executes **18 cases** across the two
  sanitizer builds. After making the sequential fixtures retain immutable
  descriptors, both the emitted test and sanitizer integration test pass again.
  Clippy with warnings denied, formatting, diff and inventory checks pass.
- Export vectors with `KLASSIC_GC_TLAB_VECTORS=/tmp/tlab.bin` during native GC
  tests, export the macOS fixture, then pass `--tlab-vectors /tmp/tlab.bin` to
  `tools/gc_arm_emulation.py`. Native ARM64 and Windows validation remain open.
  The access inventory includes both allocation implementations: **4,628 sites
  across 18 files**.
- Descriptor registration/bitmap validation, page preparation/refill and id-range
  reservation, heap budgets/reserves, large-run reuse, rooted return, initialized
  publication/mark accounting and normal compiler integration remain open.
  All-phase color tests do not prove allocate-black semantics. The standard
  collector still uses legacy allocation; no G01-G15 gate closes here.

## Task 3 page reservations, budgets and TLAB ownership (partial)

- Shared `HeapPool` / `PageLease` operations in `gc_pages.h` and
  `gc_codegen/pages.rs` reserve contiguous 128 KiB slots, charge pending heap
  and bitmap bytes, preserve relocation headroom, and report soft-limit pressure.
  Commit transfers prepared resources to a Ready page; abort requires successful
  OS cleanup and no live resource ownership. Identity ranges and incarnations
  never roll back. Stale or shortened leases cannot release a reused run.
- `gc_tlab_page_locked` / its portable emitter bind Ready Young pages to empty
  contexts and close owned TLABs while retaining objects. `AllocationPage` now
  records the run-head index in every slot. Its Rust/C layout and the new pool
  layouts/constants are ABI-checked. Closed pages are not Free and cannot be
  reclaimed through preparation abort.
- **10,492 C/x86 pool cases** cover the 8-slot fragmentation patterns, mutator/
  relocation budgets, metadata limits, reservation rollback, adjacent aborted
  run reuse, counter exhaustion and stale/partial tokens. **60 C/x86 bind/close
  cases** check failure preservation and ownership. The same before/after vectors
  pass emitted **ARM64 instruction emulation**, alongside the updated 2,722 TLAB
  cases. This does not certify native ARM memory ordering.
- The native driver and required C sanitizer heap suite execute a real OS mapping
  pipeline: reserve a protected arena, prepare/commit a run and bitmap, bind a
  TLAB, allocate a payload spanning pages, close, and allocate from another run.
  It injects commitment failure, bitmap allocation failure and cleanup failure;
  quota survives failed cleanup until successful retry. The pipeline uses C OS
  callbacks around emitted metadata/allocation steps, not production direct
  startup. Only Linux execution has been performed here.
- Export with `KLASSIC_GC_POOL_VECTORS` and `KLASSIC_GC_BINDING_VECTORS` during
  the native GC tests; replay with the matching `--pool-vectors` and
  `--binding-vectors` options in `tools/gc_arm_emulation.py`. The inventory now
  covered **4,811 sites across 20 files** at this stage; the descriptor extension
  below brought it to 4,898 sites across 22 files, before the root extension.
- `cargo test --workspace` passes **791 tests**, 0 failed/ignored across 25
  invocations, including the extended heap suite under ASan+UBSan and TSan.
  Clippy with warnings denied, formatting, diff and inventory checks pass.
- Production OS preparation/refill, heap initialization/configuration, compiler
  descriptor integration, rooted result/initialization/mark publication, other metadata
  accounting and published-page reclamation remain open. Soft-limit pressure is
  not an adaptive scheduler. Reusing aborted preparations does not demonstrate
  large-object collection or source-address reuse after relocation. Default
  programs still use the legacy collector; all G01-G15 gates remain open.

## Task 3 immutable descriptors and bounded shape scans (partial)

- The 64-byte Rust/C `TypeDescriptor` now includes exact fixed/element bitmap
  word counts. Shared registry and scan request layouts/constants are ABI-tested.
  `gc_descriptors.h` and `gc_codegen/descriptors.rs` validate definitions, publish
  one immutable definition per ID with CAS, and acquire it for shape walks.
  Registration failure and invalid scans preserve caller storage.
- The scanner returns only reference-field byte offsets, with at most 256 bitmap
  candidates and 256 outputs per invocation. Scalar-only regions skip in constant
  work; mixed records, repeated mixed elements and partial final bytes are
  handled without inspecting payload values. A resume cursor carries no managed
  address. Actual collector jobs/rooting/guarded field loads are still absent.
- **2,539 registration and 12,498 scan cases** compare full C/x86 state, with
  independent complete offset expectations and budget/progress assertions.
  They cover every bitmap tail width, multiword and bit-63 boundaries, malformed
  lengths/alignment, duplicate IDs, invalid payloads/cursors, output canaries and
  logical 1 TiB scalar/reference arrays. The same vectors pass emitted ARM64
  instruction emulation, including the new unsigned division and variable shift
  operations. This is not native ARM memory-order evidence.
- **64 three-thread rounds** race two descriptor publishers and an acquiring
  reader. Exactly one publisher wins; the reader observes the complete immutable
  definition and bitmap. They run in the emitted native driver and the required
  C heap suite under separate ASan+UBSan and TSan builds. Native descriptor data
  is initialized after thread startup, so the registry publication is the tested
  visibility edge. There are no race suppressions.
- The real-page conformance pipeline now registers scalar-array and mixed-record
  descriptors, allocates both shapes through TLABs, and resolves each header's
  descriptor ID to the expected offsets. This connects the metadata pieces in a
  fixture; it does not make INITIALIZING payloads scannable or enable the default
  generational collector.
- Export with `KLASSIC_GC_DESCRIPTOR_VECTORS` / `KLASSIC_GC_SCAN_VECTORS`, then
  replay with `--descriptor-vectors` / `--scan-vectors` in
  `tools/gc_arm_emulation.py`. Updated TLAB and bind/close vectors also pass ARM
  replay after the descriptor layout extension.
- Validation after the pipeline and bitmap-tail extensions: `cargo build -p
  klassic-runtime` and `cargo test --workspace` exit 0; **791 passed, 0 failed,
  0 ignored** across 25 test invocations, including the required 18 C sanitizer
  executions. Clippy with warnings denied, formatting, diff and regenerated
  inventory checks pass. Standalone C sanitizer validation before the final
  tail/pipeline extension also reported 18 passed, 0 failed.
- Compiler-generated descriptor tables, production allocation lookup, native
  metadata budgeting/lifetime, root handoff, initialization/mark publication and
  collector scheduling remain open. Windows/macOS native execution remains
  outstanding. No G01-G15 completion claim follows from these component tests.

## Task 4 root storage, journal and finite scans (partial)

- Shared RootCell/RootSegment/RootChange/request layouts and state constants are
  Rust/C ABI-tested. C and portable emission implement owner-only empty cell
  reservation, scope/generation checks, raw unresolved reads, store/clear with
  old/new journal publication, and constant-work segment freezing. Journal-full,
  invalid inputs and generation/counter exhaustion preserve caller storage.
- Root and journal transfer functions copy at most 256 slots/entries into a
  caller's protected job storage. The SPSC journal uses release/acquire counters
  and does not expose the producer's private top to the consumer. Store publishes
  its change record before mutating a cell, including null and same-value stores.
  No operation traverses a graph, allocates storage, locks or polls.
- **963 storage, 16 root-scan and 737 journal-drain cases** compare complete
  C/x86 state. They exercise a 300-cell frame, stale/released handle rejection,
  owner/native-state misuse, full journals, physical ring wrap, high unsigned
  counters, exhaustion, bounded output and invalid-input preservation. The same
  before/after vectors pass emitted ARM64 instruction emulation. This does not
  verify ARM64 hardware ordering or native Darwin/Windows execution.
- **64 concurrent rounds** hold a root scanner after its first cell while the
  owner overwrites/clears the remaining roots and freezes the frame. A separate
  consumer drains a three-entry journal while it wraps. The union of observations
  must retain every initial root and every surviving replacement. The cases run
  in the emitted native driver and new mandatory `gc_roots_test` under separate
  ASan+UBSan and TSan builds. References in this contention fixture are symbolic;
  this is root/journal visibility evidence, not a moving-graph test.
- The real-page fixture now reserves empty output cells before allocation and
  transfers four actual TLAB results into them without polling. It then freezes
  and scans the segment, drains the changes and verifies the referenced object
  headers. Journal capacity is prepared before allocation; the production
  potentially-safepointing `gc_alloc_into` wrapper is still unimplemented.
- Export with `KLASSIC_GC_ROOT_VECTORS`, `KLASSIC_GC_ROOT_SCAN_VECTORS` and
  `KLASSIC_GC_ROOT_DRAIN_VECTORS`; replay with `--root-vectors`,
  `--root-scan-vectors` and `--root-drain-vectors`. The access inventory now
  covers **5,040 sites across 24 files**.
- Final `cargo test --workspace`: **791 passed, 0 failed/ignored**, 25 test
  invocations, with all **20 mandatory C sanitizer executions** (six suites).
  The first workspace run caught a stale expected pipeline-success string;
  updating it and requiring the new root-suite markers made the full rerun pass.
  `cargo build -p klassic-runtime`, Clippy with warnings denied, formatting,
  diff and inventory checks also pass. ARM replay covers the root operations;
  actual root/native pipeline execution here is Linux x86-64 only.
- At this stage the fixture retained storage until readers joined; the lifetime
  extension below now supplies scope IDs, leases, retirement/deletion claims and
  native cell cleanup/reuse. Production registry/frame discovery and phase
  integration remain open. Root-load remapping, mark work and termination,
  compiler rooting, global/weak handles, detach flush and native-boundary adoption
  are also outstanding. Normal programs still use legacy roots and GC.

## Task 4 root leases, retirement queue and native cell reuse (partial)

- Shared `RootRegistry`, `RootRegistration`, serial-checked `RootLease` and job
  layouts connect C and direct emission. Attach assigns nonwrapping scope IDs.
  Registered retirement captures the phase, freezes the used prefix and queues
  deletion work in constant time. Raw freeze rejects registered segments.
- A deletion claim spans at most 256 cells. Leased scans must advance through
  the claimed range before ACK, and ACK requires both captured mark obligations
  when Young and Old are active. Existing reader leases block cleanup even after
  all deletion work is acknowledged. No operation waits while holding the lock.
- Exclusive cleanup runs in chunks outside the registry lock. Finish requires
  its published cursor to reach the frozen limit, removes the queue entry and
  returns prepared native storage. Cell generation counters survive cleanup;
  reattachment changes scope. Stale handles and reused lease-control serials are
  rejected. This is root-cell reuse, not managed-page/forwarding reclamation.
- **1,230 lifetime, 147 cleanup and 384 leased-scan cases** compare complete C/x86
  state, including simultaneous mark masks, incomplete/skipped-range ACK,
  non-head queue removal, stale tokens, delayed readers and same-address cell
  reuse. The same vectors pass ARM64 instruction emulation, along with the
  updated prior root-operation vectors after the segment layout extension.
- **64 concurrent rounds** retain an actual reader lease while a separate actor
  finishes deletion jobs. Cleanup must wait for reader release. After finish,
  native cell pages become inaccessible and a stale scanner must reject its
  token before touching them. The test then restores access, reuses the same
  addresses and checks scope/generation/serial rejection. C ASan+UBSan/TSan and
  emitted Linux x86 execute the fixture. Its mark visitor checks symbolic root
  identities for both generations; it is not a production graph tracer.
- `KLASSIC_GC_ROOT_LIFE_VECTORS`, `KLASSIC_GC_ROOT_CLEAR_VECTORS` and
  `KLASSIC_GC_ROOT_LEASED_VECTORS` export replay data; matching script options are
  `--root-life-vectors`, `--root-clear-vectors`, `--root-leased-vectors`.
  Inventory now includes the lifetime metadata-expression emitter and covers
  **5,371 sites across 26 files**.
- Final validation: **791 passed, 0 failed/ignored** across 25 workspace test
  invocations, including **20 mandatory C sanitizer executions**. The final
  leased-progress/early-ACK checks are included. Runtime static-library build,
  Clippy with warnings denied, formatting, diff and inventory checks pass.
  This provides component evidence on Linux x86-64 and ARM instruction emulation,
  not target-OS or low-pause completion.
- Production active-root discovery, frame/context linking, segment allocation
  and metadata budgets, phase/journal handoff, mark visitors/termination,
  compiler rooting, native/global/weak handles and detach draining remain open.
  Native ARM64/Windows execution and all G01-G15 acceptance gates remain open.

## Task 4 bounded root inventory discovery (partial)

- A separate intrusive inventory now links registered segments. Attach inserts
  at the head; cleanup unlinks only after deletion work and reader leases finish.
  BEGIN and NEXT perform constant work under the same registry lock. A walker
  holds one reader lease on its current registration and captures the segment's
  used prefix. Scans copy at most 256 values outside the lock; NEXT requires scan
  completion and the caller's captured-generation completion certificate.
- New head insertions cannot extend an older walk: scopes decrease along its
  forward path. A removed unvisited node has already completed deletion work;
  publication journals retain roots created after BEGIN or beyond its captured
  prefix. The production coordinator must still preserve those phase obligations.
  This is a discovery component, not a complete mark/termination protocol.
- **1,883 walker and 18,331 captured-prefix scan cases** compare complete C/x86
  state, all 64 phase pairs, prefix sizes through 300 cells, multiple budgets,
  delayed retirement, same-slot reattachment, new-head publication, stale serials
  and invalid/exhausted-operation preservation. ARM64 instruction emulation
  replays every vector, plus the updated lifetime/cleanup/leased-scan fixtures.
- **64 actual-thread rounds** delay an emitted walker while another actor
  processes retirement, unlinks/reuses an unseen registration, inserts a new head
  and publishes through the root journal. Two independent walker controls retain
  separate leases. The union of symbolic deletion and traversal observations
  covers the initial inventory, and the journal retains the post-BEGIN root.
  Native Darwin/Windows execution and real moving-graph visitors remain open.
- Export via `KLASSIC_GC_ROOT_WALK_VECTORS` and
  `KLASSIC_GC_ROOT_WALK_SCAN_VECTORS`; replay with `--root-walk-vectors` and
  `--root-walk-scan-vectors`. Inventory covers **5,587 sites across 28 files**.
- Final workspace validation: **791 passed, 0 failed/ignored**, 25 test
  invocations. The six mandatory C suites complete **20 sanitizer executions**;
  the integration gate requires both new inventory case/race markers twice,
  once for ASan+UBSan and once for TSan. Runtime build, Clippy with warnings
  denied, formatting, diff and inventory checks pass. An unused Rust re-export
  found by the initial lint run was replaced with an explicit module path.
- Production root allocation/budgets, frame/context linking, phase scheduling
  and journal handoff, graph visitors/termination, compiler roots, native handles
  and detach draining remain outstanding. Normal programs still use legacy GC;
  this extension closes no complete G01-G15 gate.

## Task 4 root owner retention and detach progress (partial)

- Registered roots now link into their owning context. ATTACH inserts at the
  owner head; RETIRE removes any active entry in constant work and increments a
  checked retiring-root count. Its context address stays valid until FINISH.
  Reader/deletion leases and chunked cleanup still gate reuse. FINISH also waits
  for the root journal to be consumed before releasing that context obligation.
- Shared C/emitted handshake detach now returns WAIT for owned/retiring roots,
  an open TLAB, private barrier entries or unconsumed published entries. Invalid
  cursor ordering is rejected without mutation. A Detaching owner can acknowledge
  consecutive handshakes while remaining Detaching, so pending root/journal work
  cannot make it permanently unable to answer the next rendezvous.
- **1,261 lifecycle, 150 cleanup and 384 leased-scan cases** now include owner
  head/middle/tail removal, broken owner links, counter overflow and journal-gated
  completion. **41,539 handshake cases** cover resource-gated final detach and
  consecutive Detaching acknowledgements. C/x86 full-state comparison and ARM64
  instruction replay pass; the updated inventory walk vectors also pass replay.
- **64 actual-thread rounds** retain a root context through delayed readers and
  delayed journal consumption. After deletion ACK, cleanup first waits on the
  reader, then FINISH waits on the journal. Two handshakes complete during that
  wait. Context deregistration additionally waits for private/published barrier
  ownership counters; only then can the same context and cells be reused with
  fresh identities and cell generations. The root journal consumer runs real
  bounded drain code. The barrier-buffer portion models ownership counters,
  not SATB/remset processing; visitors still check symbolic values.
- Final `cargo test --workspace`: **791 passed, 0 failed/ignored**, 25 test
  invocations, including all **20 mandatory C sanitizer executions** across six
  suites. The detach race marker is required in both sanitizer builds. Runtime
  build, Clippy with warnings denied, formatting, diff and inventory checks pass.
  The inventory covers **5,674 sites in 28 files**. Initial test-marker assertions
  were updated for the expanded case counts; no case or sanitizer was skipped.
- The existing C runtime uses the strengthened final handshake guard. Emitted
  helpers are tested directly; normal direct compiler roots and the generational
  runtime detach driver are not connected yet. Root storage allocation/budgets,
  compiler frame/argument/return/static roots, phase adoption and mark/relocation
  work remain open. No G01-G15 gate is declared complete.

## Task 4 bounded detach orchestration (partial)

- `gc_detach_step_locked` composes existing membership/handshake and root
  lifetime operations. It acknowledges active rendezvous before owner cleanup,
  retires at most one root segment per call and attempts final deregistration
  only after the active owner list is empty. All native resources remain held
  on WAIT or an action return; no step scans roots or blocks under the lock.
- Explicit actions request TLAB close, private SATB/remset flush or phase adoption.
  They do not claim that work completed. TLAB close now accepts a Detaching
  owner; new binding still rejects that state. Emitted orchestration calls the
  shared emitted leaf routines using a private stack request. ARM frame-pointer
  copying now supports that local request address.
- **1,120 C/x86 full-state cases** cover all 64 phase pairs, zero through three
  owned segments, one-segment progress, retirement waits, final deregistration,
  failed action preservation, invalid membership and consecutive rendezvous.
  **62 TLAB bind/close cases** include close-without-rebinding during detach.
  Both updated suites pass ARM64 instruction emulation. Export progress vectors
  with `KLASSIC_GC_DETACH_VECTORS`, replay with `--detach-vectors`.
- The existing **64 concurrent detach rounds** now invoke the composed routine
  to retire roots, answer repeated handshakes and complete deregistration. The
  retained-context/reader/journal assertions remain in place. Root values are
  symbolic and barrier ownership uses test counters; actual generational mark
  consumers and phase adoption are still required.
- This implements bounded orchestration, not the complete production detach
  event loop. OS notification/wait wiring, real flush/phase actions, compiler
  frames and metadata allocation remain open. Normal programs still use legacy
  GC and every complete G01-G15 gate remains open.
- Final workspace validation: **791 passed, 0 failed/ignored**, 25 test
  invocations, including **20 mandatory C sanitizer executions** over six suites.
  The new progress and updated binding markers must appear in both sanitizer
  builds. Runtime build, Clippy with warnings denied, formatting, diff and
  inventory checks pass. The current inventory has **5,733 sites in 30 files**.

## Task 4/5 barrier buffer transport and real detach flush (partial)

- Shared C and emitted helpers append already prepared 48-byte `BarrierEntry`
  records, release-publish the private prefix and drain at most 256 entries into
  protected worker storage. Copy includes old value, owner, offset, incarnation,
  epoch and lease token. Full/invalid/exhausted operations preserve storage and
  input ownership; counters never wrap. The consumer ignores producer-private
  `top` and publishes consumption only after copying complete records.
- **2,340 producer and 1,244 drain cases** compare C/x86 full state: both context
  buffers, allowed owner states, guard-compatible leaf operations, empty/private
  prefixes, ring wrap, capacities through 300, bounded output, high counters and
  exhaustion. ARM64 instruction replay passes the same vectors. The first ARM
  drain replay exceeded its 20,000-instruction budget; sequential physical ring
  advancement and direct six-word copies fixed this without raising that limit.
- **64 concurrent ring rounds** carry 512 entries each through one- and
  three-entry rings. A forced private-prefix observation must remain empty;
  concurrent publication/wrap must preserve every field in order. Output canaries
  detect excess copying. The cases execute in C sanitizer and emitted drivers.
- The **64 concurrent detach rounds** now use actual append/publish/drain code
  for both buffers. They no longer emulate flush by assigning ownership counters.
  Delayed SATB drain still leaves remembered work blocking deregistration, and
  contexts/cells are reused only after both consumers complete.
- Export with `KLASSIC_GC_BUFFER_STEP_VECTORS` and
  `KLASSIC_GC_BUFFER_DRAIN_VECTORS`; replay via `--buffer-step-vectors` and
  `--buffer-drain-vectors`. Entry protection is a caller prerequisite: acquiring
  and releasing leases in production barriers, graph marking,
  remembered bitmap updates, phase handoff and in-flight termination remain open.
  Symbolic references/tokens are not evidence of moving-graph correctness. Normal
  programs still use legacy GC; no complete G01-G15 gate closes here.
- Final workspace validation: **791 passed, 0 failed/ignored**, 25 test
  invocations, including **20 mandatory C sanitizer executions** across six
  suites. Producer/drain and concurrent-ring success markers must occur in both
  ASan+UBSan and TSan builds. Runtime build, Clippy with warnings denied,
  formatting, diff and inventory checks pass. The inventory covers
  **5,826 sites across 32 files**.

## Task 3/5 page metadata leases and buffer ownership (partial)

- Added matching C/shared-emitter acquire, resolve and release operations under
  the page-pool heap lock. A bounded prepared slot table uses generation-tagged
  tokens and a free list; operations do constant work without allocation or
  traversal. `AllocationPage` now includes a holder count (104 bytes total), and
  C/Rust layout tests include all three new native control structures.
- Reservation skips a held page even if its state is incorrectly Free;
  commit/abort reject held pending pages before modifying ownership. Slot
  generation and holder count cannot wrap. Released or stale tokens cannot
  resolve or release a newer lease in the same slot. The token belongs to one
  stable pool, and metadata retention does not imply object liveness or pinning.
- **2,160 C/x86 lease cases** cover full tables, geometry, page identity,
  malformed tokens, overflow, repeated acquire/release and stale slot reuse.
  **10,496 pool cases** now include metadata retention. ARM64 instruction replay
  passes these and the regenerated **2,722 TLAB / 62 bind-close cases** with the
  expanded page layout. This is emulation, not native Darwin execution.
- **64 concurrent rounds** prepare real OS page mappings, acquire real leases,
  append/publish/drain alternating SATB/remset entries and delay the copied job.
  They check full-append rollback, retention after ring consumption, page reuse
  rejection while queued and in flight, duplicate release and stale-token
  rejection after the slot is reused. The premature-Free attempt is explicit
  fault injection, not published-page retirement. The C sanitizer gate requires
  the lease and concurrent-transfer success markers from both builds.
- Export with `KLASSIC_GC_METADATA_VECTORS`; replay with `--metadata-vectors`.
  Metadata table allocation/budget admission, production barriers and consumers,
  forwarding/source/color retirement, in-flight termination and actual
  Windows/macOS execution remain open. A zero lease count alone never authorizes
  reclamation. Normal programs still use legacy GC, and all G01-G15 remain open.
- Final validation: `cargo test --workspace` passes **791 tests**, 0 failed or
  ignored, across 25 invocations, including all **20 mandatory C sanitizer
  executions** across six suites. The lease cases and delayed-job fixture run
  under both ASan+UBSan and TSan. Native GC tests pass all 34 cases; runtime
  build, Clippy with warnings denied, formatting, diff and inventory checks
  pass. The current inventory covers **5,941 sites across 34 files**.

## Task 5/6 mark queue, heap scan and SATB graph execution (partial)

- Added common C/Rust `MarkRecord`, `MarkTask`, `MarkQueue`, `MarkRequest` and
  `MarkScan` layouts, plus C/shared-emitter scheduling and bounded atomic heap
  scans. OFFER commits a generation's logical mark stamp together with its job;
  FULL and DUPLICATE preserve input ownership. TAKE retains in-flight work,
  YIELD preserves cursor/lease and refreshes the claim serial, and FINISH rejects
  stale/duplicate claims and returns protection for release.
- **6,117 queue cases** cover both generation stamps, full-width epochs,
  duplicate/full preservation, FIFO ordering across free-slot reuse, cursor
  resumption, stale claims, malformed inputs and counter exhaustion. **1,567
  heap-slice cases** cover 0..256 actual reference slots, nulls, budgets,
  unchanged job cursor/output canaries, odd scalar payloads/terminal cursors,
  and rejection of initializing headers.
  They pass C/direct x86 execution and ARM64 instruction replay, including the
  nested emitted descriptor scanner and its ABI preservation.
- **64 two-worker graph rounds** allocate actual objects through the page/TLAB
  helpers, seed marking through root scans/journal, and follow a shared cyclic
  graph and a 1,024-slot array. Each round scans exactly **1,027 slots in 132
  slices**. A delayed private SATB entry preserves the only deleted edge; its
  object becomes marked after the normal queue empties, while the unreachable
  object remains unmarked. Real lease acquisition, duplicate cleanup, queue/job
  transfer and final release leave zero metadata holders.
- Export using `KLASSIC_GC_MARK_VECTORS` and `KLASSIC_GC_MARK_SCAN_VECTORS`;
  replay with `--mark-vectors` and `--mark-scan-vectors`. The C sanitizer test
  requires all three queue/slice/graph success markers in both sanitizer builds.
- This is a stationary-heap integration fixture with explicit publication and
  receiver lookup, not a production coordinator. Queue EMPTY is not a global
  termination certificate. Native task/record allocation and budgets, metadata-exhaustion progress,
  private queues/work stealing, initialization during marking,
  forwarding/receiver guards, phase adoption/termination, compiler barriers,
  remsets and actual Windows/macOS execution remain open. Normal programs still
  use legacy GC and no full G01-G15 gate closes here.
- Final `cargo test --workspace`: **791 passed, 0 failed/ignored**, 25 test
  invocations including **20 required C sanitizer executions** across six suites.
  Queue/slice/graph markers execute in both ASan+UBSan and TSan builds. All 34
  native GC tests, ARM replay, runtime build, Clippy with warnings denied,
  formatting, diff and inventory checks pass. Current inventory: **6,164 sites
  across 38 files**. The first slice test caught a missing emitted nested-helper
  return; a later review caught an overly strict FINISH cursor alignment check.
  Both are fixed and covered by the final runs above.

## Task 6/11 progress under mark-task saturation (partial)

- Reproduced the blocked dependency in a one-slot queue: the parent holds the
  only claim, child OFFER returns FULL, and the parent cannot correctly FINISH
  until its child is protected. Ordinary waiting would not create space.
- Added generation-specific overflow cells to each logical mark record and
  common C/shared-emitter OFFER_OR_SPILL and REFILL. Admission commits the mark
  stamp and lease-bearing deferred work together. CHECK counts deferred jobs;
  REFILL transfers one to an available slot and clears its cell. Existing
  overflow takes precedence over fresh offers. Neither helper allocates,
  recursively scans, traverses a list, grows an array or waits under the lock.
- **959 C/x86 overflow cases** cover both generations, all-claimed queues,
  capacities 1..4, full-width epochs, FIFO refill, repeated cell reuse,
  duplicate/failure preservation, malformed links and serial/count exhaustion.
  They pass ARM64 instruction replay, together with the regenerated **6,117
  queue / 1,567 heap-slice vectors** for the expanded ABI.
- The **64 two-worker real-heap graph rounds** now alternate **1/2/16 slots**.
  One-slot rounds require at least three successful child deferrals while all
  slots are claimed. Spill/refill counts match, both embedded generation cells
  clear, and all metadata leases release. The root/SATB deletion, shared cycle,
  1,024-slot array and odd scalar leaf retain their exact 1,027-slot/132-slice
  results. Both new success markers are mandatory in each C sanitizer build.
- Export via `KLASSIC_GC_MARK_SPILL_VECTORS`, replay via
  `--mark-spill-vectors`. MarkRecord is now **152 bytes** and MarkQueue **104
  bytes**; the extra per-object storage must be admitted by the production
  metadata budget. This addresses task-slot saturation with prepared records
  and available leases. Metadata allocation/lease exhaustion, native OOM,
  private queues/work stealing, production scheduling/termination and the
  remaining collector integration are still open. Default execution remains
  legacy GC; all G01-G15 remain open.
- Final validation: **791 workspace tests passed, 0 failed/ignored**, 25 test
  invocations including all **20 required C sanitizer executions** across six
  suites. Overflow and saturated-graph markers are required in both ASan+UBSan
  and TSan runs. All 34 native GC tests, ARM replay, runtime build, Clippy with
  warnings denied, formatting, diff and inventory checks pass. Inventory:
  **6,280 sites across 40 files**. Native Windows/macOS execution and performance
  measurements remain unverified in this environment.

## Capability gates

All rows below are **open**, including capabilities partially provided by the
legacy implementation. Passing legacy tests does not satisfy the new contract.

| Contract | Missing completion evidence / implementation tasks |
| --- | --- |
| G01 Young/Old, age, promotion | Tasks 3, 7, 9: young-only cycles and identity after promotion |
| G02 N mutators and workers | Task 2: emitted workers on three OSes; 1/2/8-mutator progress |
| G03 concurrent generational marking | Tasks 5-6: SATB/publication/in-flight termination |
| G04 concurrent relocation | Task 8: shared mutable graph, unique copy, retirement/failure |
| G05 generation colors | Task 1/5: ABI/model/encoding, both remap epochs |
| G06 generational store barrier | Task 5: previous value and precise source-slot recording |
| G07 doubled remsets | Task 7: flips, buffered ownership, moving owner and promotion |
| G08 concurrent roots | Task 4/6: segmented roots and bounded phase transitions |
| G09 overlapping cycles | Task 9: Old mark survives Young moves/promotion |
| G10 heap management | Task 3/11: dynamic budgets, large-run reuse, uncommit/pre-touch |
| G11 exhaustion/progress | Task 8/11: reserve, stalls, injected allocation/metadata failure |
| G12 adaptive control | Task 11: capacities, tenuring and worker adjustment |
| G13 native boundary | Task 4/10: handles, blocked state, scoped pin, callbacks |
| G14 weak handles | Task 10: termination/weak-load race and rooting |
| G15 parity/diagnostics | Task 12/13: direct three-OS execution, LLVM, versioned telemetry |

Final acceptance additionally requires the full failure matrix, independent
runtime/compiler/OS reviews, and the dedicated performance matrix (including
10,000-sample pause statistics, stalls, throughput and memory). No latency or
cross-platform completion claim is made by this report.

## Task 3/5 native metadata admission (partial)

- Added `MetadataCharge` and matching C/shared-emitter reserve/commit/abort/
  release operations. Page bitmap preparation and mark metadata now compete
  through the same pending/committed ledger. Cleanup failure retains its charge;
  wrong-owner/stage, counter overflow and page-table floor violations reject
  without mutation.
- The real graph fixture prepares budgeted native task, mark-record/spill and
  lease storage before allocating objects. It exercises one-byte-short denial,
  preparation rollback, exact-fit retry and release after worker/job retirement.
- Production record/lease allocation, growth/exhaustion recovery and all other
  metadata consumers still require integration. This is not G10/G11 completion
  or a change to the default legacy collector.
- Validation: 214 C/emitted accounting vectors, the same 214 ARM64
  instruction replays, and 64 graph admission/retirement rounds pass. Full
  `cargo test`: 791 passed, 0 failed/ignored, including both required C sanitizer
  builds. Clippy with warnings denied, formatting and inventory checks pass.
  ARM replay does not establish Darwin scheduling or hardware memory ordering.

## Task 3/5 allocator-owned stable mark records (partial)

- The generational TLAB allocator now consumes one pre-admitted `MarkArena`
  slot and initializes its identity before header/start publication. The
  32-byte header stores the immutable record pointer; record identity replaces
  the old in-header identity field without growing each physical header.
- Missing/full native storage returns a distinct metadata-refill status with
  unchanged heap, bitmap, output and records. Dirty cells and malformed arena
  geometry fail before mutation. Existing records cannot be moved or reset
  while object copies or mark jobs survive.
- The real graph fixture uses the allocator's records and follows header
  pointers instead of a six-object lookup. The scanner verifies the exact
  record, rejecting an alternate record with the same debug ID. Two physical
  copies sharing a record pass; concurrent relocation itself remains open.
- C/emitted allocation conformance now has 2,753 cases; bounded scan conformance
  has 1,570. ARM replay caught missing explicit flag tests after alignment masks;
  those checks now match both ISAs. Production OS admission/refill, lifetime
  reclamation, compiler lowering, and default generational enablement remain
  unfinished.
- Validation: 2,753 allocator and 1,570 scanner C/emitted cases plus matching
  ARM instruction replays pass. All 791 workspace tests pass with no failures or
  ignored tests, including both required C sanitizer builds; Clippy, formatting
  and the 6,359-site access inventory also pass. Actual Darwin/Windows execution
  and concurrent relocation are not established by these checks.

## Task 3/5 reference-directed metadata leases (partial)

- Added shared C/PortableAsm selection of a run head from a canonical reference,
  with valid colors, object-start bit, allocation prefix and page/run geometry
  checked before the existing token allocator is called. Selected tail pages
  retain the head metadata. Failures preserve all output/accounting state.
- The graph fixture no longer assumes first-page lease ownership. It reserves
  live runs at different indices, scans arrays spanning two pages, and resolves
  completed jobs' lease tokens for release. SATB owner retention uses the same
  reference acquisition helper.
- Canonicalization, strong reachability and the raw access guard remain caller
  obligations. This does not resolve stale references, implement forwarding,
  authorize address reuse, or enable the default generational runtime.
- Validation: 1,701 C/emitted cases and matching ARM64 instruction replays,
  plus 64 varying-run/multi-page graph rounds, pass. Full `cargo test`: 791
  passed, 0 failed/ignored, including both required C sanitizer builds. Clippy,
  formatting and the 6,454-site/44-file inventory pass. ARM emulation does not
  establish native Darwin scheduling or hardware ordering.

## Task 7 atomic forwarding publication (partial)

- Added matching C/shared-emitter CLAIM/READ/PUBLISH/RETAIN over a stable source
  key and one atomic tagged word. The winning ticket alone can publish; readers
  acquire the complete destination, and a published mapping cannot be undone.
- A two-worker fixture prepares actual candidate pages, pauses the winner at a
  partial copy, verifies Busy before publication, then checks all bytes through
  an acquire reader. The loser decommits its candidate; injected abandonment
  cleans the partial copy and retains the unchanged source. Copies share their
  mark record, including a mark arriving during copying.
- Source exclusion/epoch adoption, production bounded copy and allocation,
  promotion/remsets, nested forwarding resolution and retirement remain open.
  The fixture now uses the source guard protocol below; this is not G04
  completion or a switch of the default collector.
- Validation: the initial 1,669 C/emitted transitions and matching ARM64 instruction
  replays pass, with 64 two-worker copy/publication/cleanup rounds. Full
  `cargo test`: 791 passed, 0 failed/ignored, including both mandatory C sanitizer
  builds. Clippy, formatting and the 6,514-site/46-file inventory pass. Actual
  Windows/Darwin execution and compiler-wide source guard coverage remain open.

## Task 4/7 source access and relocation exclusion (partial)

- Added source-local ENTER/EXIT/SELECT/CHECK/REOPEN in C and the shared emitter.
  Guard admission races selection on one CAS word; every existing scope remains
  counted. Owner contexts carry an access marker, preventing state transitions
  while raw access remains live. Forwarding publication/retention now additionally
  rejects a gate that is open or still has users.
- The copy fixture now runs two delayed source users alongside two competing
  relocators. Late entry fails, copy waits for the old users' writes, and only
  a cleaned retained source can reopen. The source is no longer merely assumed
  quiescent at test startup.
- All production mutator/scanner/healer funnels still need these guards. This
  does not complete global phase adoption, compiler coverage, source/color
  retirement, real bounded copy/allocation or default generational enablement.
- Validation: 154 C/emitted source-access cases, 1,673 updated forwarding
  cases and matching ARM64 instruction replays pass. The 64 delayed-user/copy
  rounds pass in emitted code and both C sanitizer builds. Full `cargo test`: 791
  passed, 0 failed/ignored. Clippy, formatting and the 6,596-site/48-file inventory
  pass. No production latency or native Darwin/Windows result is claimed.

## Task 3/4/7 nonmoving source lookup (partial)

- AllocationPage now holds a stable run-head ForwardTable pointer (112-byte
  page ABI, checked mechanically). Young allocation reserves a zero physical
  source cell alongside its logical MarkRecord and publishes the sorted prefix
  before object-start/top publication. Missing/full table preparation leaves
  heap, arena, table and output unchanged and returns metadata refill.
- C and PortableAsm metadata-only lookup search an acquired prefix in at most
  32 probes, check the source incarnation and return a stable cell. Lifetime is
  protected by the existing page metadata lease; this does not keep an object
  alive or authorize source access until the separate guard is acquired.
- Actual graph offers and bounded marker scans now use the leased page's table
  before any header read. Shared C/emitted concurrent allocation tests use the
  same lookup/guard path. Graph metadata admission includes the table and cells.
- 2,329 lookup and 2,776 TLAB C/emitted cases pass, as do 32 concurrent append
  rounds and 64 graph rounds. ARM64 replay passes these vectors plus the changed
  page-pool, metadata-lease and reference-lease layouts. Contexts in the graph
  remain explicit Running stubs; production registry/phase adoption is open.
- Production metadata allocation/refill/retirement, nested forwarding and full
  mutator/scanner/healer integration remain unfinished. Default native output
  still uses the legacy collector. G01-G15 remain open.
- Final validation for this slice: full `cargo test` 791 passed, 0 failed/ignored; required ASan+UBSan and TSan suites passed, including both new integration markers. Clippy with warnings denied, formatting, diff checks and the 6,672-site/50-file access inventory passed. Native Darwin/Windows execution and production latency remain unverified.

## Task 4/7 bounded forwarding resolution (partial)

- Added a shared C/PortableAsm `ResolveRequest` / resolve-access step. It
  validates retained source metadata, reads one forwarding entry, and either
  acquires a source guard, returns one destination, or reports a retry without
  acquiring a guard. It never reads a managed header or walks an entire chain.
- EXIT retains the existing immediate-retry rule on CAS contention and clears
  scope keys only after guard release. Destination handoff remains explicitly
  no-safepoint until a prepared root accepts it; a full root journal requires
  discarding that private value before poll/retry from the original root.
- C/emitted conformance covers 1,494 complete-state cases. Existing competing
  copy races and actual mark-graph header/scanner accesses use the composed
  resolver. New 64-round A-to-B-to-C tests protect both old source pages and
  pass through prepared roots between hops. Sixteen rounds force journal-full
  failure and verify discard/retry. ARM64 instruction replay also passes.
- This is the bounded mechanism, not the full public load/store barrier.
  Production root/phase integration, self-healing, worker registration,
  metadata allocation/retirement and default generational enablement remain
  unfinished. Protected-source tests do not prove virtual-address reuse or
  native Darwin/Windows execution. G01-G15 remain open.
- Final validation: full `cargo test` 791 passed, 0 failed/ignored, including
  required ASan+UBSan and TSan execution of the resolver, chain/root-handoff,
  competing-copy and mark-graph fixtures. Clippy with warnings denied,
  formatting, diff checks and the 6,762-site/52-file inventory passed. Graph
  guard acquisition/retries now occur outside the heap/queue lock. The initial
  C fixture include triggered an unused-function error; the final fixture uses
  only its own two-root storage and passes the strict C warning gate.

## Task 8 bounded physical copying and publication (partial)

- Added C/PortableAsm physical-copy and complete-copy-publication leaves with
  a 72-byte owner-private RelocationCopy cursor. Every slice checks claim and
  source-gate drain, validates committed limits/nonoverlap and header state,
  and copies at most 256 payload bytes. The destination shares its source
  MarkRecord and remains Initializing until its exact payload is complete.
- Publication rejects fresh/partial cursors and revalidates the completed
  header and claim before forwarding CAS. Zero/odd payloads, both address
  orders, invalid extents/header/cursor state and untouched padding are covered
  by 4,335 C/emitted cases; 217 publication cases include all legal colors and
  rejection of premature/repeated publication. Both ARM64 vector replays pass.
- The 64 competing-relocator rounds now execute actual copy slices and
  publication through these callbacks, preserving delayed source-user writes,
  concurrent Old mark record updates, loser cleanup and partial abandonment.
  The protected-source two-hop/root-handoff fixture uses these callbacks too.
- Production candidate allocation/metadata admission, promotion/remset handoff,
  scheduler progress, source/color retirement and compiler-wide guard coverage
  remain unfinished. OS preparation/cleanup remains fixture glue. Default native
  output still uses the legacy collector; G01-G15 and latency remain open.
- Final validation: full `cargo test` 791 passed, 0 failed/ignored; required
  ASan+UBSan and TSan suites passed with the copy/publication markers. Clippy
  with warnings denied, formatting, diff checks and the 6,885-site/54-file
  inventory passed. ARM64 replay validates instruction behavior; native Darwin
  and Windows execution and production latency remain unverified.


## Task 8 pool-backed relocation targets (partial)

- C/PortableAsm target PREPARE derives a candidate from an exclusively owned,
  committed Ready/Closed Young or Old run. It preserves public top/start/table
  state until INSTALL verifies a completed header/cursor, appends a forwarding
  cell and release-publishes bitmap/top. Multiple objects share a destination
  run without creating fresh logical MarkRecords. Every rejection preserves
  the entire fixture; missing/full table and insufficient space have refill
  results.
- 240 C/emitted target cases and matching ARM64 instruction replays cover
  malformed geometry, occupied bitmap/table cells, partial/repeated install,
  both generations and sequential objects. The 64 protected two-hop chains now
  prepare destination runs via actual pool reserve/commit and install metadata
  through the target callback before forwarding.
- 32 OS-backed C/emitted rounds connect normal pool/TLAB allocation to packed
  relocation in Young/Old runs, including zero/page-sized payloads, run-tail
  object lookup, partial private cleanup and reads after source protection.
  Records retain the source allocator identity and Old epoch throughout.
- Collector ownership across slices, production OS preparation/table admission,
  native metadata refill, committed-candidate reclamation, promotion/remset
  handoff and source/color retirement remain open. Old physical placement in
  this fixture does not implement promotion policy or its barriers. Normal
  programs still use the legacy collector; G01-G15 are not complete.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  with 0 failed/ignored; required ASan+UBSan and TSan checks passed and each
  new target marker appeared in both sanitizer runs. Native GC filter passed
  34 tests; ARM64 replay passed all 240 target vectors. Clippy with warnings
  denied, formatting, diff checks and the 6,993-site/54-file inventory passed.
  Native Darwin/Windows scheduling and production latency remain unverified.


## Task 8 unpublished committed-run release (partial)

- Added a 64-byte PageRelease transaction and C/PortableAsm BEGIN/FINISH leaves.
  BEGIN requires a never-installed Ready run with no page owner/metadata lease
  and an empty optional forwarding table. It transfers bitmap/table cleanup
  ownership to the request and marks the head Releasing. Heap/bitmap budget
  remains charged until all external cleanup succeeds and FINISH returns the
  complete run to Free; identity/incarnation counters never rewind.
- Forward table deallocation uses the existing independent MetadataCharge
  release, so native table bytes and bitmap bytes are returned exactly once.
  Invalid geometry, nonempty tables, retained resources, repeated transitions
  and inconsistent tail pages preserve all state. 422 C/emitted cases and
  matching ARM64 instruction replays cover the transaction.
- 32 real OS rounds connect pool/TLAB sources, admitted target tables, actual
  bounded copying, failure cleanup, retained-source reopening and run reuse.
  Table budget/allocation failures and partial/complete uninstalled copies
  preserve source payload/identity. Cleanup failures keep budgets charged;
  successful decommit/free enables zeroed recommit with fresh keys, while stale
  release requests reject. Both single-page and multi-page runs are covered.
- This releases unpublished committed candidates only. Installed candidates,
  published sources, forwarding/color retirement, production OS cleanup and
  ownership scheduling, promotion/remsets, compiler-wide barriers and default
  Generational enablement remain unfinished. G01-G15 are still open.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored; ASan+UBSan and TSan passed with both new release markers
  required in each sanitizer run. Native GC filter passed 34 tests and ARM64
  replay passed 422 release vectors. Clippy with warnings denied, formatting,
  diff checks and the 7,125-site/56-file inventory passed. Native Windows/macOS
  scheduling and production latency remain unverified.


## Task 5 guarded reference-store commit (partial)

- Added a 160-byte GuardedStore ABI request and matching C/PortableAsm physical
  store helper. A live canonical source scope and descriptor-checked reference
  slot precede buffer/heap access. Expected/desired references, required new-value
  marking and entry metadata leases must already be prepared by the outer barrier.
- The helper records the overwritten nonnull referent during either generation's
  marking phases and records every Old source slot, including null writes.
  Preflight of both buffers precedes any mutation. Records are release-published
  before the physical acq_rel CAS; a losing CAS retains conservative entries and
  reports their transferred lease mask. It never returns an observed unrooted
  reference. FULL/EXHAUSTED/INVALID preserve all state.
- 448 C/emitted cases and ARM64 replay cover phase/color combinations, null and
  same-value writes, ring wrap/full, slot-shape validation and rejection paths.
  128 two-writer rounds verify unique winners, retry and log ownership. Controlled
  writer/healer/selector interleavings prove stale healing cannot overwrite the
  store and copying waits for guards, then preserves the new value (64 rounds).
- The 64 real OS mark-graph rounds now delete the sole child edge through this
  helper using independent access and entry leases. Published SATB survives the
  otherwise-empty queue, preserves the child and transfers/releases its actual
  metadata token. This replaces the fixture's separate manual buffer append and
  raw atomic null store.
- Outer rooted resolution/retry and desired-value marking, logical CAS, bulk and
  scalar accesses, remset consumption/flip/promotion, global phase termination,
  every compiler funnel and default enablement remain open. This is the physical
  commit portion, not completed G05/G06 or a complete public store barrier.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored. Required ASan+UBSan and TSan passed with all four new store
  markers required in both runs. Native GC filter passed 34 tests; ARM64 replay
  passed 448 store vectors. Clippy with warnings denied, formatting, diff checks
  and the 7,300-site/58-file inventory passed. Native Darwin/Windows execution
  and production latency remain unverified.


## Task 4/5 guarded load into a prepared root (partial)

- Added the 80-byte GuardedLoad request and C/PortableAsm field acquisition into
  an existing RootHandle. Scope/context/phase, header/page bounds and descriptor
  slot checks are shared with the physical store helper. The returned status
  exposes no unrooted reference; null or well-formed unresolved bits are handed
  directly to the root journal/cell while the source guard stays held.
- A dedicated internal root-store mode requires that guard and publishes the
  old/new root journal entry before the root value. The ordinary root API keeps
  its no-active-guard requirement. No marker suspension, allocation, poll or
  waiting is introduced. Rejection/full/exhausted journal preserves all visible
  state; retry requires EXIT, flush outside the guard and a fresh field read.
- 826 C/emitted load cases and matching ARM64 replay cover roots/journal, slot
  shape, null/colors, phase/guard validation and full-state preservation. The
  refactored 448 store and 963 ordinary root vectors also pass ARM64 replay.
- 64 real OS pool/TLAB rounds use actual receiver/output/replacement-child roots,
  descriptors, MarkRecords and metadata leases. Full-journal retries (32 rounds)
  discard the old read, cross a native transition and reread a changed field.
  After successful handoff the receiver moves and its old page becomes protected;
  the output root still resolves to the selected child's payload and identity.
- This is rooted acquisition of unresolved bits. Public forwarding/marking/healing
  orchestration, refill/retry scheduling, all compiler funnels, remset/promotion,
  published-page retirement and default enablement remain open. It does not
  establish a complete load barrier or finish G01-G15.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored; both new load markers are required in each successful
  ASan+UBSan and TSan run. Native GC filter passed 34 tests. ARM64 replay passed
  826 load, 448 store and 963 ordinary root vectors. Clippy with warnings denied,
  formatting, diff checks and the 7,328-site/58-file inventory passed. Native
  Darwin/Windows execution and production latency remain unverified.

## Task 4/7 rooted forwarding composition (partial)

- Added the 72-byte RootFollow request and C/PortableAsm helper. One bounded
  heap-locked call rereads a strong root, acquires source metadata, reads one
  forwarding cell and journals/stores a destination into that same root before
  releasing metadata. No raw reference/guard escapes; callers unlock before
  waiting, flushing or polling between calls.
- Null/empty/retained return DONE as an observation; a later heap access still
  requires resolution and its source guard. Busy returns WAIT. Journal-full
  returns FLUSH with the old root intact and discards the private destination.
  Acquisition consumes a token generation even on subsequent rejection. Normal
  paths balance holders; exceptional release failure retains an explicit cleanup
  obligation and blocks request reuse until cleanup succeeds.
- 1,336 C/candidate cases check complete-state effects and preservation, journal
  wrap/full/exhaustion, null/forwarding states, phase combinations, stale handles,
  invalid metadata and cleanup-token ownership with an unmapped heap base.
- 64 pool-backed two-hop chains now use this composition, including 16 full
  journal retries, protected old mappings and native transitions between hops.
  Another 64 actual pool/TLAB field-load rounds follow their output roots after
  receiver and referent relocation, wait on a claimed referent, protect both old
  source pages, and verify payload and allocator-owned identity under a new guard.
- Public phase/mark/heal orchestration, production scheduling, compiler funnels,
  promotion/remset, published-page retirement and default enablement remain open.
  These tests do not establish a complete load barrier or finish G01-G15.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored. All three new markers are required in both ASan+UBSan and
  TSan runs. Native GC filter passed 34 tests; ARM64 replay passed all 1,336
  forwarding vectors. Clippy with warnings denied, formatting, diff checks
  and the 7,390-site/60-file inventory passed. Native Darwin/Windows execution
  and production latency remain unverified.

## Task 5/6 guarded mark-work preparation (partial)

- Added a 112-byte GuardedMarkPrepare and C/PortableAsm preparation helper.
  Shared guarded-object validation with reference loads/stores checks the
  context/phase, scope/cell, page/header and stable MarkRecord. The unique input
  metadata lease moves to an empty pending MarkRequest only after all checks;
  failures preserve the request, guard, roots, heap and queue state.
- Preparation keeps the guard held. Callers EXIT before acquiring a queue lock,
  then OFFER/overflow transfers ownership on success. DUPLICATE keeps a separate
  token to resolve/release; it cannot release the first task's obligation.
  No allocation, poll, guard suspension, queue mutation or mark-stamp mutation
  is introduced into the guarded leaf.
- 628 C/candidate cases check complete-state ownership and preservation, all
  legal reference colors, both generations, phase combinations, empty payloads,
  claim/closed guards, invalid headers/keys and pending-output overwrite rejection.
- 64 two-worker root/SATB graph rounds now use preparation instead of fixture
  header reads/hand-built offers, including task saturation and overflow.
  Another 64 pool/TLAB rounds follow moved field-output roots, offer to either
  generation, check duplicate-token ownership, enter a fresh guard for actual
  scanning, and complete/release the claimed job with old source pages protected.
- Phase eligibility, pending-producer/global termination accounting, queued-job
  forwarding, production scheduling, compiler/default integration and other
  G01-G15 requirements remain incomplete. This is not a full public load barrier.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored, including all three new markers required in both ASan+UBSan
  and TSan builds. Native GC filter passed 34 tests. ARM64 replay passed 628
  preparation, 826 load and 448 store cases. Clippy with warnings denied,
  formatting, diff and the 7,428-site/62-file inventory passed. Native
  Darwin/Windows execution and production latency remain unverified.

## Task 6/7 moving in-flight mark jobs (partial)

- Added MarkRetarget (120 bytes) and C/PortableAsm claim retargeting under the
  generation queue lock. The authenticated task/claim adopt a prepared physical
  receiver with the same MarkRecord, preserving the processed aligned cursor,
  queue counts/links and both generation stamps. Serial rotation rejects copied
  old claims; the destination token transfers into the task while the old token
  transfers into an explicit retired output for release outside the queue lock.
- The helper reads only native metadata. No guard, heap access, poll, allocation
  or wait occurs there. Invalid inputs and serial exhaustion preserve every
  byte and both ownership obligations. Destination identity/metadata come from
  rooted forwarding and guarded preparation before retargeting.
- 587 C/candidate cases cover both generations, queue capacities/occupancies and
  claim positions, scan progress, large epochs, legal colors/same-address refresh,
  stale claims, malformed state and serial exhaustion, with full-state effects.
- 64 actual pool/TLAB rounds partially scan a 513-slot pointer array, relocate
  it in bounded slices, protect the old page, follow a root and retarget the
  claimed job. Remaining 1/7/64-slot slices visit every position exactly once.
  Three children are marked through a one-slot queue's overflow/refill path and
  finish under guards; all metadata holders balance and logical identities and
  generation stamps persist. Both Young and Old queues are exercised against
  a moved Young object; this does not establish complete overlapping cycles.
- Production worker/root staging, phase/producer/global termination scheduling,
  promotion/remset, published-page retirement, compiler/default migration and
  real OS/latency gates remain open. G01-G15 are not complete.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored. Both new markers are required in successful ASan+UBSan and
  TSan builds. Native GC filter passed 34 tests; ARM64 replay passed all 587
  retarget vectors. Clippy with warnings denied, formatting, diff checks and
  the 7,483-site/64-file inventory passed. Native Darwin/Windows execution and
  production latency remain unverified.

## Task 2/4/6 registered mark-worker rendezvous (partial)

- Added WorkerRequest and C/PortableAsm owner-side ATTACH/POLL/ENTER/LEAVE
  composition. Registration inherits the coordinator-selected snapshot, active
  handshake ACK adopts it, and native reentry adopts before managed execution
  resumes. Snapshot changes require a newer sequence; handshake generations
  remain separate. Active guards reject; open TLAB/private buffer prefixes
  return bounded actions for the caller to perform outside the registry lock.
- 2,097 C/candidate full-state cases cover color epochs/phase combinations,
  registration, native skip/reentry, ACK/duplicate/resume, invalid snapshots,
  sequence regression, identity exhaustion and preserved close/flush retries.
  Published unconsumed work remains a holder and is not treated as completed.
- The 64 actual two-worker graph rounds now register all three OS owners with
  unique runtime identities. One worker retains a parent claim through ACK;
  an idle worker stays native-blocked during the forced rendezvous and adopts
  on LEAVE. Polls occur after finite scans and child offers, outside raw guards.
  The owner root uses registry ATTACH, a real scan lease, journal handoff and
  deletion/cleanup retirement. Workers and owner detach before storage teardown.
- This removes Running-context/phase stubs from that graph fixture. The selected
  snapshots, OS scheduling/notifications and overall cycle policy are still
  test orchestration. Global producer/termination accounting, full Young/Old
  overlap, published-page/color retirement and compiler/default migration remain
  incomplete; G01-G15 are not finished.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored. Both new markers are required in successful ASan+UBSan and
  TSan runs. Native GC filter passed 34 tests; ARM64 replay passed all 2,097
  worker vectors. Clippy with warnings denied, formatting, diff checks and the
  7,559-site/66-file inventory passed. Native Darwin/Windows execution and
  production latency remain unverified.

## Task 6 root publication audit invalidation (partial)

- Added the atomic per-owner `root_revision` to the shared 160-byte context.
  C and emitted root STORE/RELEASE increment only after journal/cell publication,
  including null, same-value, guarded-load and forwarding writes. Overflow
  rejects before mutation; reads/reservations and failed writes preserve it.
- Extended independent success/failure expectations and exhaustion coverage:
  969 root operations, 828 guarded loads and 1,338 rooted forwarding cases.
  Ordinary journal races also check all 64 completed owner publications.
- In the 64 registered two-worker graph rounds, a late root write follows an
  empty-queue observation. An actual handshake freezes producers and rejects
  the stale pre-scan revision. Journal work is processed only after resuming;
  the subsequent end check compares the new audit and queue/SATB state before
  shutdown. Both reference installation and null deletion are covered.
- The revision is an invalidation mechanism, not a running-owner seqlock or
  stand-alone termination proof. Production audits must bind owner identity and
  cycle, cover the entire segment inventory and retain copied work until offered.
  Global retirement, other producers, remsets, cycle scheduling and default
  compiler/runtime migration remain open. G01-G15 are not complete.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored, including ASan+UBSan and TSan with the new graph marker
  required in both runs. Native GC filter passed 34 tests. ARM64 replay passed
  all 969 root, 828 load and 1,338 forwarding vectors. Clippy with warnings
  denied, formatting, diff checks and the 7,569-site/66-file inventory passed.
  Native Darwin/Windows execution and production latency remain unverified.

## Task 4/6 counted root journal handoff (partial)

- Added RootJournalRequest and C/PortableAsm CLAIM/COPY/ACK. Registered journal
  jobs retain a kind-4 lease and unconsumed ring prefix through bounded copying
  and mark handoff. Claims are exclusive; stale serials reject after control
  reuse; later producer publications stay pending. COPY runs outside the registry
  lock and does not read mutable registry counts or the private producer top.
- 930 independent whole-state C/candidate cases cover ring positions/capacities,
  256-entry bounds, active/retiring segments, early/double/stale ACK, full-width
  cursors, competing claims, later suffixes and preserved failure/exhaustion.
- All journal transfers in the registered two-worker graph now use this helper.
  In 64 rounds, a copied pending job survives a rendezvous with an empty queue
  and matching root revision. Its lease also blocks segment cleanup until ACK.
  Another 64 OS producer/consumer rounds verify stalled-copy ring retention,
  competing consumer exclusion and all 65 publications across repeated wrap.
- Whole-inventory audits, pending SATB/remset outputs, phase/global termination,
  production scheduling and default/compiler migration are still unfinished.
  These results do not establish complete G01-G15 or a global termination proof.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored, including ASan+UBSan and TSan with all three new journal
  markers required in both runs. Native GC filter passed 34 tests. ARM64 replay
  passed 930 journal and 1,261 lifetime vectors. Clippy with warnings denied,
  formatting, diff checks and the 7,704-site/68-file inventory passed. Native
  Darwin/Windows execution and production latency remain unverified.

## Task 2/6 counted SATB/remset buffer jobs (partial)

- Added BarrierJob and two embedded BufferConsumer controls to the 224-byte
  context. C/PortableAsm CLAIM/COPY/ACK retain the unconsumed ring prefix and
  unique context borrow through out-of-lock copying and phase-specific handoff.
  Thread detach waits for active controls even if a faulty cursor says consumed.
  Registration rejects leftover active controls.
- 1,263 independent whole-state cases cover both selectors, all live owner states,
  ring wrap/high counters, bounded copy, pending claims, early/double/stale ACK,
  later suffixes, serial exhaustion and simultaneous-selector isolation. The
  selector-tagged serial plus registered identity rejects cross-buffer and
  reused-context requests. Worker conformance grew to 2,101 cases.
- The 64 registered graph rounds retain a copied SATB job through a real end
  check with an empty mark queue and current root audit. The 64 metadata rounds
  now register real owners and alternate SATB/remset jobs, retain their actual
  page tokens while detach waits, then ACK and detach before storage teardown.
- Generation-specific visitors and remset flip, whole-root audits, the global
  cycle controller and compiler/default migration remain incomplete. This is
  not complete remembered-set processing or a G01-G15 completion claim.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored, including ASan+UBSan and TSan with the new markers required
  in both runs. The initial ARM64 maximum-copy replay hit the existing 20,000
  instruction cap; the emitter now retains record addresses in registers to
  remove repeated stack traffic. The optimized emitter passed the 34 native GC
  tests and all 1,263 ARM64 job vectors without raising that cap; 2,101 worker
  vectors also passed. Clippy, formatting, diff and 7,812-site/70-file inventory
  checks passed. Native Darwin/Windows execution and latency remain unverified.

## Task 4/6 generation-specific root inventory audit (partial)

- Added 160-byte RootAudit and separate Young/Old owner certificates in the
  272-byte context. C/PortableAsm BEGIN/NEXT compose leased inventory discovery
  with an empty, unclaimed-journal gate. The first owner revision sample survives
  later visits to its other segments. Failure/overflow preserves state.
- Finite CHECK/OWNER operations bind results to the full mark-queue epoch,
  generation, inventory boundary and owner identity/revision. Normal mark-phase
  transitions are accepted; later same-parity cycles, new attachments and root
  mutations reject. No root/journal traversal happens during the stopped check.
- 613 independent full-state cases cover both generations, phase/epoch combinations,
  multiple owners/segments, journal and scan completion, first-sample retention,
  separate generation certificates, stale cycle/owner/scope, serial exhaustion
  and underlying-walk failure rollback.
- The 64 registered two-worker graphs now execute actual inventory root scans
  and journal handoffs before end checks. A late root write invalidates its owner
  certificate; attaching an empty segment invalidates the inventory boundary
  without changing root_revision. Concurrent renewal and retirement allow the
  later root check to pass while pending SATB work remains separately visible.
- The global thread/producer predicate, cycle scheduling, complete concurrent
  Young/Old visitors and default/compiler migration remain incomplete. G01-G15
  are not complete; valid root certificates alone do not prove termination.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  0 failed/ignored, including ASan+UBSan and TSan with both audit markers required
  in both runs. Native GC filter passed 34 tests; ARM64 replay passed all 613
  audit vectors. Clippy with warnings denied, formatting, diff checks and the
  8,004-site/72-file inventory passed. Native Darwin/Windows execution and
  production latency remain unverified.


## Registered thread inventory and graph-wide owner checks (partial)

- Shared C and direct handshake steps now maintain a registry-owned intrusive
  list of every registered context. Attach/detach change a bounded number of
  links under the existing lock; detached entries leave no stale neighbor links.
  HandshakeState/ThreadRegistration are 64/56 bytes, checked by the ABI suite.
- 41,615 C/emitted full-state cases include all six three-owner removal orders,
  reattachment with fresh identity and 28 invalid-link cases. KLHST002 ARM replay
  includes the entire registry and all neighbors. Worker and detach fixtures
  normalize the added pointers, retaining independent expected-effect checks.
- The 64 real two-worker graph rounds use actual registered membership for
  owner/root-certificate and SATB/remset prefix/consumer checks before accepting
  the end condition. Root scans remain concurrent. Pending copied SATB work
  prevents completion even when the queue is empty and root certificates match.
- The combined predicate currently lives in the integration driver. Emitting
  and connecting the production cycle controller, full producer accounting,
  compiler barriers and default generational runtime remain open. No G01-G15
  completion or production pause bound is claimed by this change.
- Validation: full `cargo test` passed 791 tests across 25 invocations with
  zero failures/ignored tests; ASan+UBSan and TSan both executed the required
  graph-inventory marker. Native GC filter passed 34 tests; ARM64 replay passed
  41,615 handshake, 2,101 worker and 1,120 detach vectors. Clippy with warnings
  denied, formatting, diff checks and the 8,050-site/72-file inventory passed.
  Darwin/Windows hardware execution and production latency remain unverified.

## Shared mark-end predicate (partial)

- The combined registered-owner check is now shared C/PortableAsm code, replacing
  the integration-only predicate. MarkEndRequest binds it to the current active
  rendezvous generation. READY is read-only and expires when that rendezvous ends;
  the caller must retain the common lock through its phase transition.
- It reuses root audit and mark queue checks, validates complete member/native
  counts and identities, checks adopted snapshots for stopped managed owners,
  and rejects pending per-owner buffers, root retirement, leases and walkers.
  No roots or heap fields are scanned in the pause.
- 1,073 C/candidate cases independently require exact statuses and byte-for-byte
  preservation of all contexts and control records. All 64 registered two-worker
  graph rounds now call the shared implementation. Native blocked owners with an
  older snapshot are supported; future acknowledgements and omitted list nodes
  are rejected.
- Phase transitions, scheduling/notifications, other producer integration,
  complete generational cycles, compiler lowering and default migration remain
  incomplete. This helper's READY result does not establish the full G01-G15
  capability contract or a measured production pause bound.
- Validation: full `cargo test` passed 791 tests across 25 invocations with
  zero failures/ignored tests. The new conformance and shared graph markers are
  required in both ASan+UBSan and TSan runs. Native GC filter passed 34 tests;
  ARM64 replay passed all 1,073 mark-end vectors. Clippy with warnings denied,
  formatting, diff checks and the 8,161-site/74-file inventory passed. Native
  Darwin/Windows execution and production latency remain unverified.


## Mark-end phase transitions and owner adoption (partial)

- Shared C/PortableAsm coordination now publishes MarkEndCheck, waits for the
  registered producers and calls the common termination predicate. Pending work
  or stale root certificates return to ConcurrentMark; only READY selects Select.
- Before resuming any managed owner, a second rendezvous adopts the selected
  snapshot. END/BEGIN and publication occur under one lock, so no owner runs in
  between. Native reentry adopts the latest phase; snapshot/handshake counters
  reserve both increments and reject exhaustion before starting the boundary.
- The 64 real two-worker graph rounds now exercise actual retry and Select
  transitions. This exposed and fixed native entry racing a snapshot publication
  after poll: worker ENTER returns RETRY during that active newer rendezvous.
- 5,068 independent full-state cases cover phase preservation, native sets,
  pending/stale marking work, repeated control reuse, incomplete adoption,
  invalid inputs and the final nonwrapping counter pair. MarkPhaseControl is
  64 bytes, checked against the C ABI.
- Whole-cycle request tickets, scheduling/notifications, Young/Old overlap,
  relocation/retirement integration and default migration remain incomplete.
  G01-G15 and production pause/performance gates are not complete.
- Validation: full `cargo test` passed 791 tests across 25 invocations,
  zero failed/ignored. Both ASan+UBSan and TSan executed the required phase
  conformance and real-worker transition markers. Native GC filter passed 34
  tests; ARM64 replay passed 5,068 phase and 2,101 worker vectors. Clippy with
  warnings denied, formatting, diff checks and the 8,286-site/76-file inventory
  passed. Native Darwin/Windows execution and production latency remain open.

## Fresh-cycle tickets and actual C request/wait integration (partial)

- CycleLedger/CycleTicket and shared C/PortableAsm request/start/finish/poll
  operations now represent the cycle-start boundary. Requests after START
  target the next cycle; requests before it coalesce. Young and Major counters
  remain independent and never wrap. Invalid transitions preserve all state.
- The actual C worker now starts and finishes ledger jobs around its complete
  collection, and explicit collect/allocation fallback wait on tickets through
  a native blocking boundary. An earlier in-flight completion cannot satisfy a later
  request. Pressure-triggered requests keep the prior idle-only policy.
- 480 independent full-state cases cover both lanes, request coalescing,
  running-cycle requests, active/pending separation, invalid tickets and the
  last representable identities. The real collector additionally executes 16
  controlled cycle pairs proving later requests remain pending after the first
  cycle has completed.
- The current C collector still performs legacy full collections in the Major
  lane. Actual generational Young/Major scheduling, direct production wait and
  notification integration, complete cycle phases and default migration remain
  incomplete. This is not evidence that G01-G15 or latency gates are complete.
- Validation: full `cargo test` passed 791 tests across 25 invocations with
  zero failures/ignored tests. Both ASan+UBSan and TSan executed the 480-case
  conformance and 16-pair real-collector markers. Native GC filter passed 34
  tests, ARM64 replay passed all 480 vectors, and Clippy with warnings denied,
  formatting, diff checks and the 8,334-site/78-file inventory passed. Native
  Darwin/Windows scheduling and production latency remain unverified.


## Native-blocked C cycle waits (partial)

- The actual C collector's ticket await now sleeps on a completion condition,
  after entering native-blocked state and publishing the owner's root/TLAB
  boundary. The ticket predicate is rechecked under the request mutex before
  every wait; managed reentry still waits for any active handshake.
- Request and completion conditions are separate, and whole-cycle FINISH
  broadcasts completion to all waiters. Completed tickets return directly.
- 48 deterministic real-collector rounds cover wake-before-wait, spurious wakes
  and completion followed by a new handshake, while retaining a rooted value.
  Another 24 real 1/2/8-owner cohorts require every completion waiter to wake,
  reenter Running, preserve its rooted value and detach successfully.
- Another 24 cohorts mix 1/2/8 owners waiting for the running cycle with the
  same number waiting for its successor. With the successor held at START,
  current waiters must return and next-cycle waiters must recheck and sleep.
  Every owner retains a rooted value. Per-owner wait counters prevent spurious
  wakes by one owner from substituting for another reaching its first wait.
- The C collection algorithm remains legacy. Direct production wait integration,
  actual generational cycles, default migration and measured pause/stall costs
  remain incomplete. These synchronization tests do not establish G01-G15.
- Validation (Linux x86-64, 2026-09-27): `cargo test` exited 0 with 791 tests
  passing across 25 invocations, zero failures and zero ignored tests. Its
  required sanitizer gate ran both ASan+UBSan and TSan and required every new
  cycle-wait marker twice, including the mixed-ticket cohorts. Clippy with
  warnings denied, formatting, diff checks and the 8,338-site/78-file inventory
  also passed. This change adds no emitted instructions; these results do not
  establish native Darwin/Windows wait scheduling or production latency.

## Prepared mark-start boundary (partial)

- C and PortableAsm share START/ACTIVATE/RELEASE for an already prepared
  MarkStart snapshot. All managed owners acknowledge MarkStart and then
  ConcurrentMark before resuming; native owners adopt on return. No root/heap
  traversal is performed at these boundaries.
- The graph integration now registers both OS workers before this boundary and
  uses the shared helper for 64 actual graphs. Full-state phase cases grow from
  5,068 to 6,510, checking both generations, color preservation, native owners,
  stale/invalid calls, counter exhaustion and reuse by the mark-end protocol.
- This is phase adoption, not a completed cycle scheduler. Safe epoch and color
  preparation, remembered-set flip, retirement, production generational cycles
  and default native migration remain unfinished.
- Validation (Linux x86-64, 2026-09-27): native GC tests exited 0, 34 passed;
  ARM64 instruction replay matched all 6,510 phase vectors. `cargo test` exited
  0 with 791 passed across 25 invocations, zero failed/ignored, including the
  required ASan+UBSan/TSan suites and new graph-start marker in both. Clippy
  with warnings denied, formatting, diff checks and the 8,373-site/78-file
  inventory passed. Native Darwin/Windows execution and latency remain open.

## Precise remembered slot inventory (partial)

- `gc_remembered.h` and `gc_codegen/remembered.rs` implement two native slot
  bitmaps, constant-time FLIP, late old-buffer insertion into both banks,
  bounded TAKE and unique deferred ACK. Pending bits and active claims prevent
  local completion; current records survive a concurrent `keep=0` ACK.
- 6,638 full-state C/candidate cases cover bank/color combinations, duplicate
  records, retained edges, late insertions behind the scan cursor, repeated
  flips, stale phases, invalid calls, bounded progress and serial exhaustion.
- 64 existing guarded-store/relocation rounds now retain a copied remembered
  entry across the owner's move and a flip, resolve its captured owner via
  forwarding, and record/scan the destination slot across repeated flips.
  They use explicit native fixture ownership for page and metadata lifetimes.
- Global remembered-inventory termination, rooted Young mark handoff, promotion,
  dead-owner filtering, full page-move transfer/retirement and production
  generational activation remain open. G01/G07 are not certified complete.
- Validation (Linux x86-64, 2026-09-27): native GC tests exited 0 with 34
  passing; ARM64 replay matched all 6,638 remembered-set vectors. `cargo test`
  exited 0 with 791 tests across 25 invocations, zero failures/ignored, including
  the ABI layout check and both required ASan+UBSan/TSan executions of the new
  case and moved-slot markers. Clippy with warnings denied, formatting, diff
  checks and the 8,571-site/80-file inventory passed. Darwin/Windows hardware
  scheduling, whole Young cycles and performance gates remain unverified.

## Aggregate remembered work in shared mark end (partial)

- C and PortableAsm now register page inventories and maintain per-set counts
  for unflipped pages, pending bits and active claims. Global BEGIN is constant
  time; page flips occur separately. A phase high-water mark rejects an older
  global flip before partial page adoption can occur.
- The shared Young mark-end predicate reads these counts and phase freshness
  without scanning pages. Its phase coordinator retries instead of selecting
  when remembered work remains. These Young-only counts do not block Old end.
- 1,116 full-state inventory cases cover three-page registration, all unlink
  orders, deferred flip/record, late entries, claim accounting, bad links,
  counter consistency and phase ordering. End cases grow to 1,099 and phase
  cases to 6,528. Another 64 two-page boundary fixtures exercise all three
  sources of unfinished work and the final claim; 64 moved-buffer fixtures now
  use the registered wrapper through handoff, repeated flips and detach.
- Production Old-page registration, rooted Young marking handoff, promotion,
  dead-owner/page lifetime proof and complete cycles/default activation remain
  unfinished. This does not certify G01/G07 or hardware latency.
- Validation (Linux x86-64, 2026-09-27): native GC tests exited 0, 34 passed;
  ARM64 replay matched 1,116 inventory, 1,099 end and 6,528 phase vectors.
  `cargo test` exited 0 with 791 passed across 25 invocations, zero failures or
  ignored tests, including ABI layout checks and the required ASan+UBSan/TSan
  runs of all three new inventory markers. Clippy with warnings denied,
  formatting, diff checks and the 8,762-site/82-file inventory passed. Native
  Windows/macOS execution and whole-cycle/latency gates remain open.

## Retained remembered claim to rooted Young queue work (partial)

- C and PortableAsm `RememberedLoad` materialize a claimed Old slot into a
  reserved root while preserving the claim through journal flush/retry and
  subsequent referent processing. The leaf validates claim/slot/phase and
  canonical Old guarded access without reading unlocked inventory counters.
- 2,333 full-state cases cover all epoch colors and marking phases, null and
  unresolved references, invalid claim/receiver/phase inputs and full journal
  preservation. The ABI layout checker covers the new 104-byte request.
- 64 OS-backed integration rounds connect actual Old slot claims to Young mark
  queue offers and guarded scan completion. They include Old receiver movement
  between journal-full retry, Young forwarding waits after rooted load, source
  mapping protection, shared identity and metadata lease cleanup. Claim work
  remains visible until the queue owns the referent; ACK preserves the next
  cycle's current slot bit.
- The fixture controls scheduling and retains native inventory storage through
  teardown. Production dispatch, null/Old target classification, promotion,
  moved-inventory retirement, full cycles and default migration remain open.
  G01-G15 and actual Windows/macOS execution/performance are not certified.
- Validation (Linux x86-64, 2026-09-27): the 34 native GC tests passed with
  the new load leaf, and the emitted conformance test passed after adding the
  64 handoff rounds. ARM64 instruction replay matched all 2,333 remembered-load
  vectors. Final `cargo test` exited 0 with 791 passed across 25 invocations,
  zero failures/ignored, including the ABI layout test and required execution
  of both new markers under ASan+UBSan and TSan. Clippy with warnings denied,
  formatting, diff checks and the 8,814-site/84-file inventory passed. Logs:
  `/tmp/klassic-rload-native.log`, `/tmp/klassic-rload-arm.log`,
  `/tmp/klassic-rhandoff-native.log`, `/tmp/klassic-rhandoff-workspace.log`,
  `/tmp/klassic-rhandoff-clippy.log`. Emulation is not native Darwin execution.

## Canonical mark generation routing (partial)

- `GuardedMarkRoute` shares C/PortableAsm validation and lease transfer with
  guarded mark preparation. It skips null without heap access and canonical
  Old targets during Young marking. Old routing prepares both generations so
  Young intermediaries can be scanned for Old descendants. Nonnull SKIP keeps
  its input lease for release after guard EXIT; rejected calls preserve state.
- 4,695 full-state cases cover null/Young/Old targets, both collection kinds,
  phase colors, selected and other-generation phases, invalid references,
  canonical guard validation and exact lease ownership. The existing 628
  guarded-preparation cases also exercise the factored validation/transfer.
- Remembered integration retains its 64 Young mark handoffs and adds 64 null
  plus 64 Young-to-Old target moves. Generation classification follows rooted
  forwarding and canonical access. No-work ACK occurs after protection cleanup
  and cannot erase an interposed current remembered record.
- Production routing adoption, queue epoch coordination, promotion/overlap
  scheduling, complete inventories/retirement, default migration and actual
  OS/performance gates remain open. This is not G01-G15 completion.
- Validation (Linux x86-64, 2026-09-27): native GC tests exited 0 with 34
  passing; ARM64 instruction replay matched 4,695 route and 628 preparation
  vectors. Final `cargo test` exited 0 with 791 passed across 25 invocations,
  zero failures/ignored, including ABI layout and required execution of the
  new case/integration markers under both ASan+UBSan and TSan. Clippy with
  warnings denied, formatting, diff checks and the 8,844-site/86-file access
  inventory passed. Logs are `/tmp/klassic-route-{native,arm,workspace,clippy}.log`.
  Native Windows/macOS scheduling and dedicated latency gates remain open.

## Moved remembered claim and bank transfer (partial)

- Shared C/PortableAsm `RememberedTransfer` authenticates two registrations and
  one source claim, waits for destination epoch adoption, and transfers the
  claimed slot into destination pending work while carrying source current
  bits. It preserves destination claims/bits and unrelated source slots;
  aggregate work stays visible throughout the locked transaction.
- 1,769 full-state cases cover colors, boundaries, late bits, destination work,
  stale claims, malformed controls and failure preservation. An independent
  bitmap/count oracle checks successful mutations. A staged old-location
  record arriving after transfer is claimed/remapped again and coalesces with
  destination work; an empty source inventory is not retirement permission.
- The actual-page remembered pipeline performs 96 owner-move inventory
  transfers across null/Young/promoted-Old targets. Another 64 later prepared
  epochs flip both sets, find the retained canonical slot and mark its moved
  Young referent again through protected source metadata.
- These fixtures still own native storage and control phase preparation.
  Production owner lookup/dispatch, all-page coverage, promotion, staged-buffer
  and metadata retirement, default activation and full OS/latency gates remain
  unfinished. No complete Young collection or G07 completion is claimed.
- Validation (Linux x86-64, 2026-09-27): native GC tests exited 0 with 34
  passing; the final expanded emitted conformance run also passed. ARM64
  instruction replay matched 1,769 transfer and 1,116 inventory vectors.
  Final `cargo test` exited 0 with 791 passed across 25 invocations, zero
  failures/ignored, including ABI layout and required execution of all three
  transfer markers under both ASan+UBSan and TSan. Clippy with warnings denied,
  formatting, diff checks and the 8,945-site/88-file access inventory passed.
  Logs: `/tmp/klassic-rtransfer-native.log`,
  `/tmp/klassic-rtransfer-final-native.log`,
  `/tmp/klassic-rtransfer-{arm,final-arm,workspace,clippy}.log`.
  Emulation does not certify native Darwin execution or hardware ordering.

## Metadata-only remembered owner candidate (partial)

- Shared C/PortableAsm `RememberedOwner` authenticates a retained slot claim,
  checks Old-page metadata/incarnation and searches the sorted source table
  with at most 32 probes. It validates the candidate's start bit and excludes
  known header ranges without reading heap bytes or unlocked aggregate state.
  Output is an owner byte offset, relative field offset and borrowed native
  cell, not a managed pointer or strong-reference/liveness certificate.
- 5,936 full-state cases use an independent linear oracle for prefix, boundary,
  unpublished-suffix, claim, table and bitmap behavior. The 48-byte request is
  included in Rust/C ABI layout checks. Rejections preserve the entire state.
- 32 two-thread publication rounds keep the managed mapping inaccessible and
  expose table-before-start/top intervals. The new slot remains MISSING while
  existing-prefix queries succeed, then becomes discoverable after publication.
- 192 real-page remembered paths now use a nonzero field offset in a mixed
  scalar/reference parent. They include 96 lookups after protecting the old
  source mapping, movement to canonical page inventories and 64 later-epoch
  slot rereads/Young marks at the preserved relative offset.
- Production candidate-to-live-owner binding, descriptor-backed dispatch of all
  page inventories, dead-owner/metadata retirement, promotion, default migration
  and actual OS/performance gates remain incomplete. No G07 completion is claimed.
- Validation (Linux x86-64, 2026-09-27): native GC tests exited 0 with 34
  passing, followed by a passing emitted conformance run including the new
  publication and interior-slot integrations. ARM64 instruction replay matched
  all 5,936 owner vectors. Final `cargo test` exited 0 with 791 passed across
  25 invocations, zero failures/ignored, including ABI layout and required
  execution of all three new markers under both ASan+UBSan and TSan. Clippy
  with warnings denied, formatting, diff checks and the 9,061-site/90-file
  access inventory passed. Logs: `/tmp/klassic-rowner-{native,pipeline,arm,workspace,clippy}.log`.
  Native Windows/macOS hardware scheduling and latency remain unverified here.

### Registered-worker remembered handoff (partial G03/G07)

- The shared C/emitted mark-graph integration now includes a separate Old run
  and a Young child with no root, SATB or ordinary graph edge. In 32 Young
  rounds, real registered OS workers claim its previous-bank slot, find its
  owner, load into a registered worker root, offer the child, drain the root
  journal, retire the temporary segment, and ACK with next-cycle retention.
- Real mark-phase rendezvous reject both pending-slot and held-claim states
  while the mark queue is empty. The held-claim check also verifies current
  root audit certificates, isolating the remembered producer as the reason
  termination must retry. The child is unmarked before dispatch and marked
  afterward; the unreachable control object remains unmarked.
- All native fixture storage/Old owner liveness is retained through teardown.
  Setup relocation establishes the Old run before concurrent work begins.
  This is not production all-page dispatch, concurrent owner retirement,
  automatic promotion or a default-backend migration. Those remain required.
- Validation (Linux x86-64, 2026-09-27): final `cargo test` exited 0
  with 791 passed in 25 invocations, zero failures/ignored. The new marker
  is required under both ASan+UBSan and TSan; emitted x86-64 conformance also
  passed. Clippy (all targets/features, warnings denied), formatting, diff
  checks and the 9,061-site/90-file inventory passed. Logs:
  `/tmp/klassic-remworker-{native,workspace,clippy}.log`. No new emitted leaf
  or ABI was introduced; actual Windows/macOS execution remains unverified.

- The registered-worker integration additionally forces 32 rendezvous after
  guarded load but before mark offer. Claims and registered output roots
  survive phase adoption; the resumed offer uses the current phase. Sixteen
  rounds use a full one-entry journal to force both load and root-release
  backpressure. Each retry exits guards, drains one claimed journal record,
  polls, and retries without dropping the remembered claim or rooted value.
  The marker is mandatory in both sanitizer builds. This remains a controlled
  integration fixture, not default runtime dispatch or moving-owner liveness.
- Resumption validation (Linux x86-64, 2026-09-27): final `cargo test`
  exited 0, 791 passed across 25 invocations, zero failures/ignored; required
  C ASan+UBSan/TSan and emitted x86-64 conformance passed. Clippy with warnings
  denied, formatting, diff and unchanged 9,061-site/90-file inventory checks
  passed. Logs: `/tmp/klassic-remresume-{native,workspace,clippy}.log`.

### Bounded remembered-page dispatch (partial G03/G07)

- Added matching C/PortableAsm `RememberedWalk` BEGIN/NEXT/CANCEL leaves. Each
  step returns one page; a counted inventory borrow blocks DETACH and epoch
  BEGIN across polls. Prepending new pages remains permitted and cannot extend
  the cursor's original finite pass. DONE does not substitute for aggregate
  work/producer checks. A claimed slot has an independent lifetime after the
  cursor is released. Inventory ABI is now 80 bytes; cursor ABI is 40 bytes.
- 731 full-state cases exercise order/parity, two active cursors, prepend,
  detach/epoch exclusion, flip/claim handoff, cancellation and invalid inputs.
  Young end additionally rejects active cursors; 1,101 end cases verify the
  updated predicate, including Old-generation independence.
- In 32 Young real-worker rounds, a cursor flips an empty Old run, prevents
  its detachment during a real phase rendezvous, then locates a different
  run's slot and hands retention to the slot claim. Existing rooted resumption
  and journal-full cases still run. All cursor counts return to zero and the
  empty registration is detached at teardown. Both C sanitizer builds require
  the new markers, as does emitted x86 conformance.
- The consumer still uses fixture-held owner liveness and controlled scheduling.
  Production live-owner binding, processing every source of work through whole
  cycles, dead-page retirement, default migration and OS/performance gates
  remain incomplete. No G01-G15 completion is claimed.
- Validation (Linux x86-64, 2026-09-27): native GC tests passed 34/34;
  ARM64 replay matched all 19,514 updated walk/inventory/end/phase/transfer/
  owner/load vectors. Final `cargo test` exited 0 with 791 passed across 25
  invocations, zero failures/ignored, including ABI layout, both required
  sanitizer builds and emitted conformance. Clippy with warnings denied,
  formatting, diff checks and the 9,124-site/92-file inventory passed. Logs:
  `/tmp/klassic-rwalk-{allnative,arm,workspace,clippy}.log`. ARM instruction
  emulation does not establish macOS execution or hardware scheduling.

### Late remembered work after the first ACK (partial G03/G07)

- Removed the registered integration consumer's stop-after-one-job condition.
  Completed-job counters now measure progress without suppressing dispatch;
  pending/unflipped inventory counts determine available remembered work.
- In 32 Young rounds a previous-epoch record arrives after TAKE, while the
  original claim remains held. Its pending bit survives the first ACK and
  requires a second real-worker pass. Exactly 64 jobs finish, the Young child
  is scanned once per round, one current-bank bit is retained, and all root,
  journal, walker and metadata holders are released. Journal-full retries run
  for both jobs in the 16 one-entry-journal rounds.
- The new marker is required in emitted conformance and both C sanitizer
  builds. This extends the controlled worker integration; production owner
  binding, whole-cycle dispatch/retirement and default activation remain open.
- Validation (Linux x86-64, 2026-09-27): final `cargo test` exited 0
  with 791 passed in 25 invocations, zero failures/ignored. Both mandatory
  sanitizer builds and emitted x86 conformance ran the new replay marker.
  Clippy with warnings denied, formatting, diff and the unchanged
  9,124-site/92-file inventory checks passed. Logs:
  `/tmp/klassic-rreplay-{native,workspace,clippy}.log`. No ABI or emitted
  leaf changed; actual Windows/macOS scheduling remains unverified.

### Claim-derived page metadata (partial G07/G10)

- Added a 40-byte `RememberedLease` request and matching C/PortableAsm helper.
  It validates membership and the active claim before binding the registered
  range/incarnation to the heap's Old run head and acquiring a metadata lease.
  It neither needs nor manufactures an owner GcRef and never reads managed
  bytes. Output ownership and claim ownership remain separate.
- 842 full-state C/candidate cases cover multi-page run positions, states,
  stale claim/range/incarnation, unique outputs and preserved resource failures.
  Rust/C layout checks include the new request.
- The registered worker uses claim-derived metadata before owner lookup.
  Another 96 moving-source cases acquire after physical protection and reject
  the consumed original claim after slot transfer. Both markers are mandatory
  in emitted conformance and the C ASan+UBSan/TSan suites.
- This binds native storage only. Owner liveness, canonical field validity,
  production recovery/retirement, whole-cycle/default migration and actual
  platform/performance gates are not established by this helper.
- Validation (Linux x86-64, 2026-09-27): native GC tests passed 34/34;
  ARM64 replay matched all 842 new vectors. Final `cargo test` exited 0
  with 791 passed in 25 invocations, zero failures/ignored, including ABI
  layout, both mandatory sanitizer builds and emitted conformance. Clippy
  with warnings denied, formatting, diff and the 9,200-site/94-file inventory
  passed. Logs: `/tmp/klassic-rlease-{allnative,pipeline,arm,workspace,clippy}.log`.
  Actual Windows/macOS execution and latency gates remain unverified.

### Remembered worker metadata backpressure (partial G07/G11/G13)

- The registered integration consumer now handles a full metadata lease pool
  without aborting or dropping its claim. It keeps an empty registered output
  root, enters native-blocked state with no source guard or partial token,
  adopts the changed phase on reentry, and retries claim-derived acquisition.
- 32 Young rounds exhaust the actual 32-slot pool using independent API-acquired
  tokens. A real mark-end rendezvous retries while the claimant is blocked;
  freeing those tokens lets both the original and late remembered jobs finish.
  Existing rooted phase/journal-full checks and final holder accounting remain.
- The marker is required in emitted conformance and both C sanitizer builds.
  The fixture uses a controlled native gate; it does not establish production
  OS wait/wake, metadata reservation, starvation freedom or terminal OOM policy.
  Owner liveness, whole-cycle/default migration and OS/performance gates remain
  incomplete.
- Validation (Linux x86-64, 2026-09-27): final `cargo test` exited 0
  with 791 passed across 25 invocations, zero failures/ignored. The new
  pressure marker ran under both C ASan+UBSan/TSan and emitted conformance.
  Clippy with warnings denied, formatting, diff and unchanged 9,200-site/
  94-file inventory checks passed. Logs:
  `/tmp/klassic-rpressure-{native,workspace,clippy}.log`. No ABI or emitted leaf
  changed; native Windows/macOS scheduling and latency remain unverified.

### Production legacy repair preserves logical stores (partial G05/G06)

- Fixed the default shared direct emitter and LLVM's C runtime: both slow-load
  healing and mark-trace recoloring now compare-and-exchange the exact colored
  value originally observed. Failure never retries using the competing value;
  the load returns and the marker visits the original resolved observation.
- The C white-box test forces a registered writer's null or non-null store
  between observation and repair in both paths. The pre-fix runtime failed the
  first forced schedule by overwriting the null store; the fix passes 128 races.
  The marker is mandatory in both sanitizer builds.
- A Linux x86 execution test invokes the actual production PortableAsm slow-load
  and trace emitters, checks stale-observation failure and uncontended success,
  follows legacy forwarding, and verifies returned/marked references. Its mark
  callee is an explicit recording stub; it does not establish whole-cycle safety.
- These changes affect shipped legacy paths, not only generational leaf
  fixtures. They do not supply the still-missing legacy SATB pre-store barrier,
  source-write exclusion during relocation, generational default integration,
  real Windows/macOS execution or latency evidence.
- Validation (Linux x86-64): final `cargo test` exited 0 with 792 passed
  across 25 invocations, zero failures/ignored, including the new emitted
  runtime test and both mandatory C sanitizer builds. Clippy with warnings
  denied, formatting, diff and regenerated inventory (9204 sites / 94 files)
  passed. Logs: `/tmp/klassic-legacy-heal-{workspace,clippy}.log`.

### Legacy C mark-end retry removes stopped frontier drain (partial G03)

- The production C/LLVM collector now rescans roots and checks the worklist
  under its end rendezvous. Pending work leaves the same Mark phase/epoch
  active and resumes mutators; the collector drains it concurrently before a
  fresh end check. Ghost retirement and sweep run only after a quiescent empty
  check. The single marker is between quanta, and every quiescent owner has
  completed its barrier enqueue; this is the legacy termination argument,
  not the generational multi-worker/buffer/in-flight protocol.
- A forced schedule injects a retained live-parent job after the concurrent
  empty observation. The old implementation fails because it traces the job
  with the handshake still active. Sixteen real collection rounds now require
  a second end check, scan outside the pause, allow the Running mutator to
  progress during that scan, and preserve rooted child payload after movement.
  Both C sanitizer builds require this marker.
- Root traversal, sweep, selection and root evacuation remain stopped and
  unbounded. Direct ELF/PE/Mach-O still have their legacy synchronous drain and
  fallback, and the generational default/OS/latency gates remain incomplete.
  This change must not be described as globally bounded pauses or backend parity.
- Validation (Linux x86-64): `cargo test` exited 0 with 792 passed across
  25 invocations, no failures/ignored, including the late-frontier marker in
  both C ASan+UBSan and TSan builds. Clippy with warnings denied, formatting,
  diff and regenerated inventory (9201 sites / 94 files) passed. Logs:
  `/tmp/klassic-mark-retry-{workspace,clippy}.log`. No direct emitter behavior
  or external OS/performance result is claimed for this change.

### Production C trace quanta count fields (partial G03)

- The C/LLVM legacy collector now charges one unit per object opening and one
  per pointer slot, including null slots. It retains a private cursor between
  calls instead of scanning every field of a popped object in one quantum.
  Queue emptiness includes this cursor, so MarkEnd cannot retire pages while
  an object remains partially scanned. Zero budget does no work.
- This cursor belongs to the sole legacy marker. Its raw payload remains stable
  because legacy relocation starts only after mark completion; it is not the
  generational multi-worker lease/guard/remap protocol.
- The pre-fix runtime fails a 2048-slot record test at budget 1 by scanning the
  whole payload. The regression covers record/list layouts, budgets 0/1/7/31/512,
  non-null/mixed/all-null fields, skipped list length, a partial scan as the only
  remaining work, and actual collection/relocation with rooted values intact.
  Existing forced healing races and late-frontier retry checks remain active.
- Direct legacy trace still budgets whole objects. This does not complete
  backend parity, concurrent roots/sweep, generational default integration,
  OS execution or latency requirements.
- Validation (Linux x86-64): `cargo test` exited 0 with 792 passed across
  25 invocations, no failures/ignored, including both C sanitizer builds.
  The final explicit 1/7/31/512 budget rotation also passed separately under
  ASan+UBSan and TSan. Clippy with warnings denied, formatting, diff and
  inventory (9201 sites / 94 files) passed. Logs:
  `/tmp/klassic-trace-budget-{workspace,focused,clippy}.log`.

### Direct legacy field quanta and completion wiring (partial G03/G15)

- The shared ELF/PE/Mach-O `emit_gc_trace` now charges object openings and
  pointer fields, retaining a partial payload cursor/end in two runtime cells.
  It resumes that payload before popping another object, including all-null
  payloads, and keeps the exact observed-value CAS repair contract.
- The allocation driver and synchronous drain check both queued work and the
  partial cursor. MarkStart and from-scratch re-mark reset all scan state.
  The private cursor relies on legacy phase exclusion: relocation cannot run
  until marking finishes. It is not a concurrent moving-worker lease protocol.
- The emitted x86 harness executes the actual trace, drain and allocator:
  record/list fields with budgets 0/1/7/31/512, mixed/all-null payloads,
  partial-only drain, and four allocation quanta before MarkEnd may fire.
  A restart with an invalid old cursor proves cleanup before the rebuilt
  empty-root drain (root/clear-mark callees are explicit stubs in that check).
- `native_gc_large_pointer_list_survives_field_quanta` retains 2048 enum
  references across repeated actual collections, verifies their sum under
  ordinary and poison modes, and requires zero STW overflow fallbacks.
- This supersedes the earlier field-quantum entry's direct-backend limitation.
  Synchronous drain/fallback, root/sweep/selection pauses, generational default,
  native Windows/macOS execution and dedicated latency gates remain incomplete.
- Validation (Linux x86-64): final workspace run exited 0 with 793 passed
  across 25 invocations, no failures/ignored, including native/cross-build
  checks, the new large-list case, and both C sanitizer builds. The final
  emitted restart-cleanup case also passed separately. Clippy with warnings
  denied, formatting, diff and regenerated inventory (9214 sites / 94 files)
  passed. Logs: `/tmp/klassic-direct-trace-{workspace,focused,clippy-final}.log`.
  Native Windows/macOS execution and latency results remain unverified.

### Production C pre-store marking and black initialization (partial G03/G06)

- `klassic_gc_write` now protects the old and new referents during legacy Mark
  before CAS publication. A failed CAS returns the competing value, which is
  protected before retry. Null clears retain the overwritten edge; new values
  follow immutable legacy forwarding before becoming good-colored slots.
- Pointer-bearing allocation payloads are zeroed before black publication.
  Initializing a reused slot therefore cannot present stale/uninitialized bytes
  to the pre-barrier, and a store into a black owner marks its new child.
- Added mandatory `gc_store_test` to both C sanitizer builds (seven suites).
  Sixteen real cycles delete the last edge while the marker is gated before
  parent scanning, then require the old target marked before selection. The
  old color-only store fails this snapshot-preservation assertion.
  Thirty-two controlled no-collection schedules poison unused payload, allocate
  black, force competing-store CAS failure/retry, and check canonical publication
  of a fixture forwarding input. Existing healing regressions still require the
  logical store to win; their replacement is premarked to isolate healing.
- This is immediate assistance in the legacy collector, not generation-aware
  SATB/remset buffering. Direct store coverage, concurrent relocation source
  exclusion, general contention progress, generational default and OS/latency
  gates remain incomplete. The no-poll path relies on Running owners preventing
  phase transition until they reach the next safepoint.
- Validation (Linux x86-64): workspace `cargo test` exited 0 with 793 passed
  across 25 invocations, no failures/ignored. All seven C suites ran under
  ASan+UBSan and TSan; the final forwarding-input assertion also passed in a
  separate store-suite run under each sanitizer. Clippy with warnings denied,
  formatting, diff and inventory (9222 sites / 94 files) passed. Logs:
  `/tmp/klassic-store-barrier-{workspace,focused,clippy}.log`.

### Direct typed pointer-write marking funnel (partial G03/G06/G15)

- Added shared `emit_gc_store_pointer` and connected pointer-valued `__gc_write`
  lowering on ELF/PE and AArch64 Mach-O. During legacy Mark it resolves/marks
  the new target, marks the observed old target, and CAS-publishes the colored
  value. CAS failure rereads and marks the actual competing overwritten value.
  Null clears still run the old-value protection. Scalar lowering stays scalar.
- This synchronous no-poll entry uses the legacy single-owner phase exclusion
  and existing zero-on-region-reuse initialization. Its stack-borrowed address
  is not a general concurrent relocation guard. Outside Mark it retains the
  existing color-on-store behavior.
- The x86 machine-code harness uses the actual shared store and mark visitor
  for old/new marks, null clears, deduplication, and forwarding publication.
  A deterministic intervening store injected through the mark callee forces
  failed CAS and proves all three referents are marked before final publication;
  it does not simulate a thread-safe legacy mark queue.
- Native GC CLI regressions passed 14/14. Other reference-writing paths (lists,
  closures, runtime containers and bulk operations) still need complete store
  funnel coverage. Generation-aware buffering, moving-source exclusion,
  generational default and actual Windows/macOS/latency gates remain open.
- Validation (Linux x86-64): final workspace run exited 0 with 793 passed
  across 25 invocations, no failures/ignored, including emitted machine-code
  checks, native/cross-build regressions and all seven C suites under both
  sanitizers. Clippy with warnings denied, formatting, diff and regenerated
  inventory (9245 sites / 94 files) passed. Logs:
  `/tmp/klassic-direct-store-{workspace,focused,cli,clippy}.log`.
  Native Windows/macOS execution and latency gates remain unverified.

### AArch64 constructor stores use the marking funnel (partial G03/G06/G15)

- Record construction, cons-cell construction and list reversal now call the
  shared pointer-store entry for each reference field. A single trampoline
  preserves the caller's live registers; call sites preserve the argument
  registers and link register while forming the slot address. This closes
  these color-only stores under the existing no-poll legacy contract.
- This is still legacy immediate old/new marking, not generational SATB/remset
  buffering or a concurrent moving-source guard. Native macOS execution remains
  required; Linux cross-builds cannot prove the Darwin runtime behavior.
- Investigation of x86 runtime-map projections found a pre-existing
  representation mismatch; the following change removes that incorrect heap
  path rather than treating typed element storage as a heap-array address.
- Validation on Linux x86-64: the workspace suite passed 793 tests across
  25 invocations, including C sanitizer suites and AArch64 cross-builds.
  Clippy with warnings denied, formatting, diff checks and the regenerated
  inventory (9246 candidates / 94 files) passed. Logs:
  `/tmp/klassic-container-{workspace,native,clippy}.log`.

### Runtime-map projection provenance (Task 0 / partial G15)

- x86 runtime maps use `RuntimeList` metadata and typed element storage. They
  are not `HeapPointer`/`HeapString` addresses. Keys/values now create independent
  typed snapshot storage through the existing branch-copy helpers, preserve
  dynamic pair counts and skip inactive capacity. The unused heap-array helper,
  its unbarriered colored-pointer bulk copy and orphan length guard were removed.
- Regression `native_runtime_map_projections_snapshot_typed_cells` compares the
  evaluator and direct ELF output for strings, integer values, dynamic lengths
  including empty maps, and a snapshot retained across source-map assignment.
  It also executes with stress+poison flags. These non-heap values do not prove
  reference-barrier coverage or generational collection behavior.
- The inventory now distinguishes this typed root/storage family from heap
  slots. Migrating reference-bearing typed cells still requires root barriers;
  this correction does not complete SATB/remset, concurrent roots or any
  generational default/runtime gate.
- Validation: workspace 794 passed / 25 invocations, no failures or ignored;
  Clippy with warnings denied, formatting, diff and regenerated inventory
  (9224 candidate sites / 94 files) passed. Logs:
  `/tmp/klassic-map-projection-{focused,workspace,clippy}.log`.

### Generation-filtered worker handoff (partial G01/G03/G07/G08)

- The actual-worker graph integration now routes every root, journal, SATB and
  scan offer through `GuardedMarkRoute`. READY transfers the lease to queued
  work; Young-on-Old SKIP releases it after guard exit without queue work.
- In 32 Young rounds, a registered Old root is processed by leased scanning,
  publication journaling and deletion journaling. None may prematurely trace
  its only Young child. The existing pending/claimed remembered work still
  blocks termination and eventually marks that child. The other 32 Old rounds
  continue tracing Young intermediaries through the same dispatcher.
- C and emitted-code harnesses require the new `mark routing: 32 registered
  Old roots and journal deletions skip Young marking before remembered handoff
  PASS` result. This establishes integration coverage, not a generational
  default allocator/collector, moving-owner driver or production root policy.
- Validation: final workspace run exited 0 with 794 passed across 25 invocations,
  no failures/ignored, including the required routing marker under ASan+UBSan
  and TSan and the emitted x86 dispatcher. Clippy with warnings denied, format,
  diff and inventory checks passed. Logs:
  `/tmp/klassic-generation-route-{workspace,emitted,clippy}.log`.
- Two preliminary standalone ASan runs exited 139 without output. The same
  binary passed with verbose ASan logging and in gdb both with and without
  address randomization; no fault was reproduced in those runs. The initial
  cause remains unconfirmed and those failures are not counted as passes.

### Remembered worker resumes after owner relocation (partial G04/G07/G08/G13)

- In each of 32 Young graph rounds, the registered worker keeps its unique
  remembered claim while actually blocked on metadata pressure. The coordinator
  moves its Old owner through shared relocation leaves and decommits the source
  mapping before waking the worker. Native source metadata remains retained.
- The fixture dispatcher follows the one published edge without polling,
  replaces the source metadata token with a canonical token, and enters only
  the canonical guard. Both ordinary mark routing and remembered loads use
  the canonical receiver. Two original-reference root journal entries after
  decommit still classify as Old; the actual remembered field supplies the
  otherwise-unreachable Young child through two late-record passes.
- The emitted x86 test passed the new required `remembered movement: 32 blocked
  claims reload canonical Old fields after source decommit PASS` marker. This
  is a controlled single-edge integration schedule. Destination remset transfer,
  source retirement/reuse, arbitrary moving-chain retries and production
  generational default activation are still unimplemented here.
- Validation: final workspace exited 0 with 794 passed / 25 invocations, no
  failures or ignored tests. The movement marker is required under both C
  sanitizers and in the emitted x86 harness. Clippy with warnings denied,
  formatting, diff and inventory checks passed. Logs:
  `/tmp/klassic-remembered-move-{workspace,emitted,clippy}.log`.


### Real-worker remembered transfer (partial G04/G07)

- A completed source claim now invokes the existing shared transfer leaf under
  the inventory lock, after the canonical field read and rooted mark handoff.
  Destination previous/current bits become visible before source obligations
  disappear. The late source record still forces another scan on the destination;
  ACK of the consumed source receipt is rejected.
- In all 32 Young rounds the destination receives a fresh claim and retains its
  current bit after ACK. Source pending/current/claim counts become empty. With
  workers joined and captured buffers drained, source membership can detach,
  while dropping the still-live destination registration returns WAIT.
- Both C and emitted harnesses require `remembered transfer: 32 real-worker
  moved claims retain late records and rescan destination slots PASS`. This is
  integration of existing leaves with registered workers; production dispatch,
  native source retirement/address reuse, arbitrary moving chains and default
  generational activation remain open.
- Validation: workspace exited 0 with 794 passed / 25 invocations, no failures
  or ignored, including the required real-worker transfer marker in C under
  ASan+UBSan and TSan and in emitted x86 code. Clippy with warnings denied,
  formatting, diff and inventory checks passed. Logs:
  `/tmp/klassic-worker-transfer-{workspace,emitted,clippy}.log`.


### Buffered source record after completed transfer (partial G07/G08/G13)

- The graph now keeps a staged old-epoch source record in the registered
  mutator's real remembered ring, with an independently retained metadata
  token, until both initial source/destination scans have completed. This
  record survives the owner's move and source mapping decommit.
- MarkEnd rejects both the published ring entry and its copied, unacknowledged
  consumer job even when mark queues and remembered inventory are empty.
  Re-recording the source precedes token release and buffer ACK; actual workers
  then repeat source transfer and destination scanning. All 32 Young rounds
  finish with four scans, two transfers and only the destination current bit.
- C and emitted harnesses require `remembered retirement: 32 buffered source
  replays after transfer retain metadata and reject mark end PASS`. Capture
  before ring publication uses fixture preparation ownership. This is not a
  production store-barrier/default-collector or source-retirement policy;
  source DETACH still needs the outer driver's producer-lifetime proof.
- Validation: after fixing a C-only header-order dependency in the callback
  declaration, the final workspace run exited 0 with 794 passed / 25 invocations,
  no failures or ignored. Both C sanitizers and emitted x86 require the new
  marker. Clippy with warnings denied, formatting, diff and inventory checks
  passed. Logs: `/tmp/klassic-post-transfer-workspace-fixed.log`,
  `/tmp/klassic-post-transfer-emitted.log`, `/tmp/klassic-post-transfer-clippy.log`.

### Shared page-bound detach guard (partial G07/G10/G13)

- Added `GC_RI_DETACH_PAGE` to C and PortableAsm inventory dispatch. It binds the
  exact Old-page range/incarnation, requires Closed/unowned metadata with no
  leases, then applies existing empty-map/claim/epoch/walker checks. The request
  stays 24 bytes; only this opcode interprets its `phase` word as a native page
  pointer. No heap access, scan, allocation or polling is introduced.
- Added C/candidate cases for missing/misaligned page pointers, wrong bindings,
  generation/state, ownership, one/exhausted lease counts, live bits, walkers,
  and retry after a held lease drains. The inventory suite now has 1133 cases;
  failures compare all fixture bytes for preservation.
- The real-worker delayed-record scenario uses this guard and requires
  `remembered detach: 32 buffered and copied source leases block page-bound
  unlink PASS`. Ring tokens, copied-job tokens and later replayed bits each
  block unlink; final source removal succeeds after processing.
- This guards membership removal only. Producer redirection, metadata/color
  retirement and address reuse still require the outer production policy;
  generational default activation and native Windows/macOS gates remain open.
- Validation: workspace exited 0 with 794 passed across 25 invocations, no
  failures/ignored, including the 1133 C/candidate cases and required live-worker
  marker under both C sanitizers and emitted x86. Clippy with warnings denied,
  formatting, diff and regenerated inventory (9240 sites / 94 files) passed.
  Logs: `/tmp/klassic-page-detach-{workspace,emitted,clippy}.log`.

### Bounded rooted access handoff (partial G04/G08/G13)

- Added the 128-byte RootAccess ABI and equivalent C/PortableAsm operation:
  each call journals one forwarding hop into the same live root or hands out
  a canonical source guard and metadata lease. Busy/full-journal retries retain
  the root and release temporary leases; READY rejects reuse until ownership
  is discharged. No managed address is read by the helper.
- Full-state comparison covers 1298 cases. Another 32 two-edge chains exercise
  busy forwarding, journal backpressure, modeled native-state/phase changes,
  final guard ownership and release. Initial chain validation failed because
  its pages omitted identity_base/run_head; corrected page fixtures now pass.
- Workspace validation exited 0: 794 passed across 25 invocations, zero failed
  or ignored, including C ASan/UBSan, TSan and emitted x86 chain assertions.
  ABI layout, clippy with warnings denied, formatting and diff checks passed.
  Regenerated inventory contains 9297 candidate sites across 96 files.
- ARM64 instruction emulation replayed all 1298 root-access and 1133 remembered
  inventory vectors successfully. This is not native Darwin execution, hardware
  ordering or an OS scheduling proof. Logs:
  `/tmp/klassic-root-access-{workspace,clippy,arm}.log`.
- This helper was initially separate from the worker graph; the subsequent
  integration below connects remembered consumers. Default production barriers,
  marking/recoloring, retirement and native OS/performance gates remain open.


### Rooted two-edge remembered-worker access (partial G04/G07/G08/G13)

- Remembered consumers now register two root cells: an owner and an output.
  After predecessor lookup, owner publication precedes releasing the source
  metadata token. Each shared RootAccess call either advances that live root or
  acquires the canonical guard; retries drain one journal entry and poll with
  no guard. Load retries start at the retained canonical owner root.
- The 32 Young rounds now move the owner twice. A real registered-worker phase
  rendezvous occurs after the first rooted hop. The coordinator then moves the
  intermediate object again and decommits its mapping before worker resumption;
  the original source was already decommitted. The consumer follows the second
  hop, reads the final field, and hands the Young child into marking.
- The test requires 4 rooted advances across the two source passes, exact
  journal-pressure counts for capacities 1/2, 21 Old skips, balanced native
  leases and retirement of both root cells. Delayed original-source records
  still trigger transfer and a second destination scan before mark end.
- Journal offer/mark scan resolution follows this finite fixture chain without
  polling and assumes available lease capacity. General production scheduling,
  source/control retirement, address reuse and default activation remain open.
- Validation: workspace exited 0 with 794 passed across 25 invocations, zero
  failures/ignored. The new marker is required in emitted x86 and twice in C
  (ASan/UBSan and TSan). Clippy with warnings denied, formatting, diff checks
  and inventory regeneration (9297 sites / 96 files) passed. Logs:
  `/tmp/klassic-rooted-worker-{workspace,emitted,clippy}.log`.


### Rooted metadata pressure during relocation (partial G04/G11/G13)

- Replaced the remembered consumer's RootAccess metadata-full polling retry
  with the existing registered native wait protocol, shared with initial claim
  acquisition. The owner root and claim remain live; no guard or partially
  acquired lease crosses the wait. Reentry adopts the current phase before
  retrying the rooted access operation.
- The 32 Young rounds now exhaust the actual pool a second time, after the
  first rooted forwarding hop. A coordinator-owned set of 31 intermediate-page
  tokens plus the existing buffered-source token fills all 32 slots. The worker
  blocks, mark end rejects its outstanding work, and a second move/decommit
  happens before token release and notification. Each round requires one
  initial claim wait and one rooted-access wait, followed by the same complete
  scan, transfer, delayed-record replay and ownership cleanup assertions.
- This proves the controlled C/emitted worker integration. Production collector
  reservation, OOM policy, offer/scan pressure handling, retirement and default
  generational activation remain open.
- Validation: full workspace exited 0 with 794 passed across 25 invocations,
  zero failed/ignored. C ASan/UBSan, TSan and emitted x86 require the new rooted
  pressure marker. Clippy with warnings denied, formatting, diff checks and
  inventory regeneration (9297 sites / 96 files) passed. Logs:
  `/tmp/klassic-rooted-pressure-{workspace,emitted,clippy}.log`.


### Registered partial-scan claim movement (partial G03/G04/G08)

- Fixed the graph worker's ownership composition: its queue token is no longer
  handed to forwarding resolution, which can release source tokens. Each scan
  owns a distinct temporary access token. A changed canonical receiver is
  prepared under a guard, authenticated into the queue claim after guard exit,
  and followed by release of the retired queue token and fresh guard entry.
  The scanner reads the updated claim's receiver with the original cursor.
- The 64 Young/Old real-worker rounds now move the array after eight examined
  slots and a phase rendezvous with a live claim. Sixteen large-array rounds
  decommit the first source page; other objects in the run's tail stay mapped.
  Each round checks exactly one retarget, retained record/cursor, rejected stale
  YIELD/FINISH receipts, unchanged scan totals and no destination lease leak.
- This integrates existing shared prepare/retarget/resolve primitives in the
  worker fixture. General offer/scan metadata pressure, concurrent repeated
  moves, native metadata retirement and default activation remain open.
- Validation: workspace exited 0 with 794 passed across 25 invocations, zero
  failures/ignored. The moved-worker marker is required in C ASan/UBSan, TSan
  and emitted x86. Clippy with warnings denied, formatting, diff checks and
  inventory regeneration (9297 sites / 96 files) passed. Logs:
  `/tmp/klassic-worker-retarget-{workspace,emitted,clippy}.log`.


### Mark scan access-lease pressure (partial G03/G11/G13)

- Initial scan access-lease exhaustion now retains the exclusive queue claim
  across the registered native condition wait. Reentry adopts the current
  phase, verifies the complete claim is unchanged, and retries reference lookup
  before resolving/retargeting its physical receiver.
- Every one of the 64 Young/Old graph rounds fills the real pool with separately
  owned coordinator tokens after the first array slice. Existing queue/SATB
  tokens remain live. The array moves before scan retries; one or two workers
  block, and a phase rendezvous rejects termination before token release/wake.
  Existing retarget, stale-receipt, source-protection and final balance checks
  remain required.
- This covers initial scan access-token acquisition. Offer pressure, additional
  destination-token pressure inside forwarding, production reservation/OOM,
  retirement and default activation remain open.
- The first TSan run rejected a strict phase-increase assertion in the wait
  harness. A late waiter can already have adopted the rendezvous snapshot, so
  successful native return need not increase it again. The harness now checks
  exact registry/context phase agreement under the worker transition lock and
  nondecrease across the interval. The failed run is retained in
  `/tmp/klassic-scan-pressure-workspace.log`; post-fix validation is separate.
- Post-fix workspace validation exited 0 with 794 passed across 25 invocations,
  zero failed/ignored, including required scan-pressure markers in C ASan/UBSan,
  TSan and emitted x86. Clippy with warnings denied, formatting, diff checks and
  inventory regeneration (9297 sites / 96 files) passed. Final workspace log:
  `/tmp/klassic-scan-pressure-workspace-fixed.log`; focused and lint logs:
  `/tmp/klassic-scan-pressure-{emitted,clippy}.log`.


### Mark forwarding-destination pressure (partial G04/G11/G13)

- Access resolution can report a full destination lease pool with its temporary
  source token released and no returned guard/private destination. Scan dispatch
  retains the original queue claim across native wait and restarts from its
  original reference; successful canonical preparation/retarget still owns the
  queue-token replacement and retired-token release.
- Added 22 forced real-pool destination failures in one-task Young/Old rounds.
  Separate test-owned tokens fill the pool immediately before acquisition.
  Failure returns exactly one temporary token (32 active to 31), native wait
  preserves the complete claim, and mark end rejects outstanding work. Token
  release/wake permits the existing moved-array, cursor, stale-receipt and
  lease-balance checks to complete.
- This is the shared C/emitted worker fixture, with retained native metadata
  and strong producers. Offer pressure, arbitrary busy-chain scheduling,
  production reservation/OOM, retirement and default activation remain open.
- Validation: workspace exited 0 with 794 passed across 25 invocations, zero
  failures/ignored. The destination-pressure marker is required in C ASan/UBSan,
  TSan and emitted x86. Clippy with warnings denied, formatting, diff checks and
  inventory regeneration (9297 sites / 96 files) passed. Logs:
  `/tmp/klassic-destination-pressure-{workspace,emitted,clippy}.log`.


### Busy forwarding in registered mark scans (partial G03/G04/G13)

- Replaced the scan resolver's Busy spin with a retry result that releases its
  temporary lease and returns no guard/private destination. The owner retains
  its exclusive claim, polls the phase handshake, yields and retries, checking
  that claim bytes are unchanged.
- All 64 Young/Old move rounds withhold forwarding publication after target
  installation. The scan must report Busy and complete a real phase rendezvous
  with the source admission closed and forwarding still claimed. Mark end
  rejects the outstanding claim. Publication then permits existing pressure,
  protected-source, retarget, stale-receipt and full-scan checks to complete.
- Offer/control resolution still uses its fixture-only no-poll wrapper. General
  multihop budgeting, production offer retries, reserve/OOM, retirement and
  default activation remain open.
- Validation: workspace exited 0 with 794 passed across 25 invocations, zero
  failures/ignored. C ASan/UBSan, TSan and emitted x86 require the new Busy
  rendezvous marker. Clippy with warnings denied, formatting, diff checks and
  inventory regeneration (9297 sites / 96 files) passed. Logs:
  `/tmp/klassic-scan-busy-{workspace,emitted,clippy}.log`.


### Pending child roots and worker offer pressure (partial G03/G08/G11/G13)

- Registered eight persistent pending root cells per worker. Every nonnull
  bounded-scan output is published before polling, and its private buffer copy
  is cleared. Rooted offers reread after native waits; worker journal offers
  retain their unacknowledged copied claim through pressure retries.
- Parent cursors advance only after queue handoff, publication-journal commit,
  root clear and deletion-journal commit. Worker detach now retires and clears
  these root segments through the shared lifecycle. Root inventory assertions
  include the two persistent worker segments.
- All 64 Young/Old rounds force a rendezvous with a newly published child, then
  exhaust the real pool and require native wait, root retention and successful
  offer after token release. Earlier marking/relocation/buffer tests remain.
- Initial integration exposed journal-offer FULL handling, then exceeded the
  unchanged 40-second emitted alarm because it journaled null results. Round
  tracing showed progress through round 28. Omitting writes to already-empty
  roots for null scan results reduced the focused run to 12.64 seconds; all
  nonnull publication/deletion obligations remain. Failed logs:
  `/tmp/klassic-pending-roots-{emitted,emitted-fixed,trace}.log`; successful
  focused log: `/tmp/klassic-pending-roots-emitted-final.log`.
- These are C/emitted worker-fixture integrations. Coordinator/control pressure,
  cooperative offer Busy handling, arbitrary-chain budgets, production OOM,
  retirement and default generational activation remain open.
- Final validation: workspace exited 0 with 794 passed across 25 invocations,
  zero failures/ignored, including required pending-root/rooted-offer markers
  in C ASan/UBSan, TSan and emitted x86. Clippy with warnings denied, formatting,
  diff checks and inventory regeneration (9297 sites / 96 files) passed. Logs:
  `/tmp/klassic-pending-roots-{workspace,clippy}.log`.


### Busy worker offers across pending-child movement (partial G03/G04/G13)

- Rooted and leased-journal offers now request cooperative resolution. Busy
  releases temporary access ownership and returns for phase polling/yield;
  rooted callers discard private read results and reread their registered root.
  Journal callers keep their copied producer claim unacknowledged.
- All 64 Young/Old rounds copy/install the first pending child but delay its
  forwarding publication. A real rendezvous must complete with the original
  root intact, no child mark stamp yet, and forwarding still Busy. Publication
  then admits canonical queue handoff. The same target run later accepts the
  moved array; its forward table and heap-locked target operations support
  readers of the already-installed child.
- The first untraced focused run hit the existing alarm; its cause was not
  localized. Target-lock contract enforcement and optional stage tracing were
  followed by a 12.70-second successful focused run. Logs:
  `/tmp/klassic-offer-busy-{emitted,trace}.log`.
- Coordinator/control offer scheduling, bounded multihop progression, production
  reserve/OOM, source/control retirement and default activation remain open.

### Deterministic scan-pressure injection after child movement

- The untraced workspace run did not pass: its TSan heap suite remained live
  while the coordinator waited for `scan_metadata_wait`. A separate debugger
  reproduction stopped at round 17 with the coordinator at that wait and a
  worker already resolving the Busy array. Another worker can return a token
  after rendezvous-time exhaustion, allowing the array to bypass FULL. This
  invalidated the fixture's required-event assumption, not the lease capacity.
- Inject scan pressure under the lease mutex immediately before the claimed
  array's actual access acquisition. This guarantees that attempt sees FULL;
  the real native wait and retained claim still have to complete. Later
  rendezvous assertions allow unrelated owners to return tokens. Only the
  injector's separately owned tokens are released by the coordinator.
- The original stalled child was explicitly terminated after diagnosis; the
  workspace exited 101. It is not counted as a passing run. Debugger evidence:
  `/tmp/klassic-offer-busy-debug.log`; original workspace log:
  `/tmp/klassic-offer-busy-workspace.log`. This identifies a reproducible wait
  defect; it does not establish the cause of the earlier emitted alarm.
- Corrected validation: untraced emitted contention passed in 13.50 seconds;
  standalone C ASan/UBSan and TSan completed all 28 case executions with zero
  failures. The full workspace exited 0 with 794 passed across
  25 invocations, zero failed/ignored. Formatting and diff checks passed.
  Logs: `/tmp/klassic-offer-pressure-fixed-{emitted,c,workspace}.log`. No alarm
  deadline was increased, and no sanitizer or required marker was skipped.

### Remembered child queue handoff under metadata pressure

- The remembered-set worker still used the coordinator's immediate-capacity
  `graph_offer` after loading a Young child into its registered output root.
  Exhausting all 32 metadata slots at this handoff reproduced its READY
  assertion failure in emitted code (11.87 seconds, exit 101); evidence is
  `/tmp/klassic-rem-offer-before.log`.
- The worker now discards its private root-read value and uses the cooperative
  rooted offer, rereading after native waits or Busy polls. The remembered
  slot claim is not acknowledged before successful queue handoff.
- All 32 Young rounds force FULL before the first remembered output offer,
  complete a real rendezvous during native waiting, and check the output cell,
  unacknowledged remembered claim, absent child mark stamp and rejected mark
  termination. Releasing only the injected tokens admits the queue handoff
  and the existing source/destination replay sequence.
- This extends the shared C/emitted worker fixture; production coordinator
  scheduling, retirement, reserve/OOM and default migration remain open.
- Validation: focused emitted test passed in 13.28 seconds; full workspace
  exited 0 with 794 passed across 25 invocations, zero failed/ignored, including
  the required marker in both C sanitizers and emitted code. Clippy with
  warnings denied, formatting and diff checks passed. Logs:
  `/tmp/klassic-rem-offer-{fixed,workspace,clippy}.log`.

### Bounded rooted offers across two child relocations

- Rooted worker offers now call the shared RootAccess leaf once per step.
  ADVANCED journals the next reference into the registered root before polling;
  READY transfers its canonical guard and lease directly into the factored
  queue-routing handoff. Busy/FULL retry with no private destination; journal
  pressure drains one claimed entry before retrying the root.
- All 64 Young/Old rounds rendezvous after the first adoption, move the child
  again outside the rendezvous, then rendezvous after the second adoption.
  Both checks require the canonical root value, no child mark stamp yet, no
  target metadata lease, the parent still in flight and rejected termination.
  The shared target run now has three entries: both child copies and array.
- Focused emitted execution passed in 13.50 seconds with both forwarding
  adoptions and subsequent queue handoff. Log:
  `/tmp/klassic-rooted-offer-two-step.log`.
- The one-step bound applies to rooted offers. Immutable journal offers and
  claimed scans still use the existing finite no-poll chain resolver; their
  general chain budgets, coordinator scheduling, retirement and default
  generational integration remain open. This is worker-fixture integration,
  not a default collector or full G03/G04/G13 completion claim.
- Final validation: full workspace exited 0, 794 passed across 25 invocations,
  zero failed/ignored. Required two-edge marker ran in C ASan/UBSan, TSan and
  emitted x86. Clippy with warnings denied, formatting, diff checks and the
  inventory check (9297 sites / 96 files) passed. Logs:
  `/tmp/klassic-rooted-offer-{workspace,clippy}.log`.

### C runtime handshake and mutator timing (partial G15)

- Optional C logging now separates request-to-quiescence, stopped-through-
  release and per-mutator park-to-resume intervals from the retained legacy
  action timer. All duration writers hold the handshake mutex; logging reads
  counters atomically. Clock failure reports a diagnostic. No timing calls
  are added when logging is disabled.
- New `gc_timing_test.c` drives real handshake owners with a deterministic
  clock and checks staggered parking, no-request calls and native exclusion.
  It is required in both sanitizer variants by `run_tests.sh` and `llvm_gc`.
  The LLVM log regression also requires nonzero OS-clock timing fields.
- Focused ASan/UBSan, TSan and LLVM logging checks passed. A real Linux LLVM
  smoke execution recorded 34 rendezvous and 19 park intervals; full boundary
  definitions, observed numbers and limitations are in [benchmarks.md](benchmarks.md).
  This is legacy runtime observability, not generational completion or a
  dedicated-machine latency gate.
- Final validation: workspace exited 0 with 794 passed across 25 invocations,
  zero failed/ignored. The sanitizer runner now executes 30 cases (15 per
  sanitizer), including the required timing markers. Clippy, formatting, diff
  checks and inventory regeneration (9309 sites / 96 files) passed. Logs:
  `/tmp/klassic-gc-timing-{llvm,workspace,clippy}.log`.

### Continuous park intervals across consecutive requests

- Factored the C rendezvous operation from its mutex-owning wrapper without
  changing production locking. The timing regression invokes that operation
  twice while retaining the coordinator mutex between rounds, deterministically
  preventing mutators from resuming at the first release.
- Both real mutators acknowledge both generations. Two 1000-unit action
  intervals must produce one 2000-unit park sample per owner. This verifies
  the existing continuous-interval accounting rather than inferring it from
  single-round measurements. Both sanitizer variants require the new marker.
- Focused ASan/UBSan and TSan runs passed. The clock values are deterministic
  test inputs, not performance measurements; full generational latency and
  backend parity remain open.
- Final validation: full workspace exited 0 with 794 passed across 25
  invocations, zero failed/ignored, including both required sanitizer markers
  and the real-clock LLVM log regression. Formatting, diff checks and inventory
  regeneration (9309 sites / 96 files) passed. Full log:
  `/tmp/klassic-gc-timing-chain-workspace.log`.

### Bounded copied-journal offers with retained metadata (partial G03/G04/G13)

- Worker journal offers now resolve one forwarding edge per iteration. Their
  private COPY cursor advances only after acquiring destination metadata;
  the old token is released, the new reference/token is retained, then the
  worker polls. The immutable producer ring remains unconsumed under its
  counted kind-4 claim until both references reach the queue. Canonical READY
  transfers its guard/token directly to the existing guarded routing handoff.
- Busy preserves the current cursor/token. Destination FULL discards the
  private forwarding result and enters native waiting with the current COPY
  and its metadata token still owned. This is distinct from the earlier
  restart-from-original no-poll traversal.
- All 64 Young/Old rounds rendezvous after each of two COPY adoptions. They
  verify the copied value, token resolution, live producer claim, unchanged
  original ring entry and rejected termination. Exact second-hop acquisition
  also exhausts the pool, requiring a native wait/rendezvous with the cursor
  unchanged before separately owned injection tokens are returned.
- The first pressure run exposed an unrelated scan-notification assumption:
  earlier background scan waits could satisfy the later array wait counter.
  The fixture now distinguishes the partial-array claimant's wait notification
  from other scan waits, and journal destination notifications identify the
  controlled producer. Initial emitted failure log:
  `/tmp/klassic-journal-offer-pressure.log`; corrected focused run passed in
  12.85 seconds (`/tmp/klassic-journal-offer-pressure-fixed.log`).
- Native controls and forwarding tables remain fixture-owned until cleanup.
  This does not prove arbitrary source retirement or independent Young/Old
  cycles. Claimed scans still need bounded-chain resolution; production
  coordinator scheduling, reserve/OOM and default migration remain open.
- Final validation: full workspace exited 0 with 794 passed across 25
  invocations, zero failed/ignored, including the required journal-step and
  pressure marker in C ASan/UBSan, TSan and emitted x86. Clippy with warnings
  denied, formatting, diff checks and inventory check (9309 sites / 96 files)
  passed. Logs: `/tmp/klassic-journal-offer-{workspace,clippy}.log`.

### Bounded access cursors for authenticated scan claims

- A scan worker now owns a separate resumable reference/metadata cursor while
  the original authenticated queue claim remains unchanged. Each forwarding
  edge acquires destination metadata, transfers the cursor, and polls with
  no guard. Only canonical guarded preparation authorizes the final queue
  retarget; stale receipts are still rejected and the payload cursor survives.
- Busy and destination FULL retain both the original queue token and the
  separate access cursor. FULL discards only the private next result and
  enters native waiting. The 22 one-task pressure rounds now require all 32
  tokens still owned during waiting, replacing the earlier temporary-source
  release/31-token behavior. Coordinator/control resolution remains separate.
- All 64 array claims rendezvous after forwarding adoption but before
  retarget. Tests inspect the unchanged queue reference/serial/token/cursor
  and independently resolve the access cursor's token. In 48 small-array
  rounds the destination moves again between steps; the worker adopts both
  edges and performs exactly one final retarget. The target table now has
  four entries. The 16 large-array rounds still decommit the original page.
- Focused emitted test passed in 14.15 seconds, unchanged deadline. Log:
  `/tmp/klassic-scan-cursor-step.log`. This is shared C/emitted worker-fixture
  integration; native controls remain retained through teardown. General
  source retirement, independent generation overlap, production coordinator
  pressure handling and default generational activation remain unproved.
- Final validation: workspace exited 0 with 794 passed across 25 invocations,
  zero failed/ignored. Required scan-cursor marker executed in C ASan/UBSan,
  TSan and emitted x86. Clippy with warnings denied, formatting, diff checks
  and inventory check (9309 sites / 96 files) passed. Logs:
  `/tmp/klassic-scan-cursor-{workspace,clippy}.log`.

### Guarded generational slot repair (partial G04/G05/G15)

- Added the 56-byte `GuardedHeal` ABI and C/PortableAsm leaf. Under an already
  held receiver guard it validates the descriptor reference slot, exact phase,
  nonnull encoded inputs and desired mark/remap colors, preserving the
  observed remembered bits, then performs acq_rel CAS
  against the exact observed bits. CHANGED preserves a replacement and returns
  no unrooted observed reference. No SATB/remset entry is produced: this is
  physical repair of the same logical object, not a mutator store.
- Same-object equivalence, strong referent protection, mark work and forwarding
  metadata lifetime are explicit caller obligations. The leaf does not resolve
  references or certify retirement. A failed repair must not change the outer
  load's protected original result. The default legacy load is not switched.
- 1563 full-memory C/candidate cases cover 32 color epochs and eight phase
  state pairs, both owner generations, replacement/null conflicts, invalid
  slots/guards and admission
  closing after guard acquisition. The existing 64 store/move interleavings
  now use this candidate repair rather than a fixture-local raw CAS to reject
  stale healing after a real logical store; copying still waits for both guards.
- The moving graph repairs 64 actual array slots after their children move
  twice, rejects a repeated stale observation, then copies the repaired array.
  Forwarding controls and graph liveness remain fixture-retained; not every
  heap slot is repaired and no source-retirement proof is inferred.
- ABI tests: 8 passed. Focused x86 emitted integration: 14.62 seconds, passed.
  The Mach-O export now contains 70 helpers; ARM emulation replayed all 1563
  C memory/state vectors successfully. Actual Darwin scheduling and hardware
  ordering remain unverified. Logs/artifacts:
  `/tmp/klassic-guarded-heal-abi.log`,
  `/tmp/klassic-guarded-heal-final-{emitted,arm,arm-export}.log`,
  `/tmp/klassic-guarded-heal.vectors`, `/tmp/klassic-guarded-heal-arm/`.
- Review caught that using the entire current phase color would manufacture
  a remembered-set stamp without recording the slot. Repair now preserves
  the observed remembered bits and updates only mark/remap colors. The 1563
  cases exercise all three nonzero remembered-bit combinations and explicitly
  reject an attempted new stamp across a parity change. This was corrected
  before any default-runtime activation.
- Final validation after remembered-stamp correction: workspace exited 0
  with 794 passed across 25 invocations, zero failed/ignored, including required
  heal markers in C ASan/UBSan, TSan and emitted x86. ARM replay passed all
  1563 corrected vectors. Clippy across the workspace, all targets and all
  features with warnings denied, formatting, diff checks and inventory
  regeneration (9330 sites / 96 files) passed. Final logs:
  `/tmp/klassic-guarded-heal-final-{workspace,clippy,emitted,arm}.log`.

### Worker-driven graph repair (partial G04/G05/G15)

The moving-graph marker now requests `MarkScan.locations`, protects nonnull
children in its existing pending root cells, completes rooted queue offers and
publication journals, and reacquires its claimed receiver before bounded slot
repair. It retains only offsets and exact CAS comparison bits across polls.
Within the guard it atomically snapshots its still-reserved, uniquely owned
native root cells; no public root-load, poll or root mutation occurs there.
The pending roots keep referents strong and the fixture retains all forwarding
metadata. This is integration evidence, not production retirement or default
collector activation.

The first 64-round validation required every worker to repair the child before
its array claim yielded. The strengthened schedule now splits into 32 repaired
slots and 32 intervening guarded null stores: the worker must reject the latter
with CHANGED and preserve the deletion. Both outcomes include Young/Old mark
rounds and eight protected-source large-array runs. The second SATB entry and
its owner lease stay live through termination checks and bounded buffer copy,
and are consumed only after queue handoff. A stale coordinator CAS and the
initial/second array copies must preserve the exact winning value.
The existing separate guarded-store fixture still covers an intervening real
logical store. Validation: the workspace exited 0 with 794 passed across 25 invocations,
zero failed/ignored, including mandatory ASan+UBSan and TSan runs of the updated
moving graph and the emitted x86 fixture. Clippy with warnings denied, formatting,
diff and inventory checks passed. Logs: `/tmp/klassic-worker-heal-workspace.log`,
`/tmp/klassic-worker-heal-emitted.log`, `/tmp/klassic-worker-heal-clippy.log`.
No generated leaf instruction or ABI changed in this connection; this does not
add ARM native concurrency or hardware-ordering evidence.

Validation of the intervening-deletion schedule (including eight large-array
source-protection cases with deletion): workspace exited 0, 794 passed across
25 invocations, zero failed/ignored; required ASan+UBSan/TSan and generated x86
contention tests passed. Clippy, formatting, diff and inventory checks passed.
Logs: `/tmp/klassic-worker-conflict-{workspace,emitted,clippy}.log`. The focused
emitted log covers the initial small-array deletion schedule; the workspace
log covers the final schedule with large-array deletions.

### Physical source payload readiness (partial G04/G10/G15)

Added the 48-byte `SourceDrain` request and a C/shared-emitter metadata-only
scan bounded to 256 allocation-table entries. It requires a closed run with no
allocator owner, matching incarnation/top/table-length snapshots, and every
entry published with a closed, zero-reader gate. The scheduler must exclusively
exclude append/table replacement through OS cleanup; the complete allocation
table and destination lifetimes are caller invariants. It returns WAIT for
unfinished/retained entries and preserves invalid requests.

The moving graph invokes the candidate before 64 Old-source decommits, checks
WAIT before forwarding publication and DONE after, then protects actual source
mappings with metadata leases and remembered claims still outstanding. This
separates physical payload release from forwarding metadata retention. It does
not enable address/color reuse, budget credit or default collector activation.

Validation: eight C/Rust ABI tests; 107 full-memory C/x86 cases and AArch64
emulated replays; the workspace exited 0 with 794 passed / zero failed or
ignored across 25 invocations, including required ASan+UBSan/TSan and emitted
contention suites. Clippy, formatting, diff and inventory checks passed.
The Mach-O fixture exports 71 helpers; native Darwin scheduling/hardware
ordering remains unverified. Logs: `/tmp/klassic-source-drain-{abi,emitted,arm,
arm-export,workspace,clippy}.log`; vectors `/tmp/klassic-source-drain.vectors`.

### Sealed evacuated runs (partial G04/G10/G15)

`SourceDrain` now release-publishes `PAGE_EVACUATED` under the heap lock when
its bounded scan completes. An OS cleanup failure can leave the run sealed for
retry. Target append, allocation and guarded payload access reject the state;
physical source mappings can no longer be admitted as new copy targets after
the retirement scheduler releases its exclusive ownership. Existing private
candidates must still be excluded by that ownership before the scan starts.

Metadata/ref leases, remembered owner lookup and page-bound detachment retain
access to evacuated controls. Resolve can follow published forwarding but
never acquire a source guard there, even for malformed unfinished entries.
The real moving graph seals all 64 full Old-source runs and rejects target
PREPARE after their payload mappings become inaccessible. Start/forward tables
and virtual ranges remain retained: source/color reuse and default collector
integration remain open. Whole-run payload accounting is described below.

Sealed-state validation: workspace exited 0 with 794 passed, zero failed/ignored,
across 25 invocations, including mandatory ASan+UBSan/TSan and emitted x86
contention tests. ABI: eight passed. ARM emulation replayed 12,569 complete
memory/state vectors: source drain 112, resolver 1,519, metadata leases 2,163,
reference leases 1,703, remembered owner 5,937 and remembered inventory 1,135.
Clippy with warnings denied, formatting, diff and inventory checks passed
(9,355 sites / 96 files). Actual Darwin concurrency and hardware ordering are
not proven by emulation. Logs: `/tmp/klassic-source-seal-workspace.log`,
`/tmp/klassic-source-seal-final-emitted.log`, `/tmp/klassic-source-seal-arm.log`,
`/tmp/klassic-source-seal-abi.log`, `/tmp/klassic-source-seal-clippy.log`.

### Source payload accounting (partial G10/G11/G15)

Added `SourceUncommit` and C/shared-emitter BEGIN/FINISH. The head binds to the
stable request address, retaining budget until the OS-success flag is cleared.
FINISH credits only payload bytes once and publishes UNCOMMITTED; metadata and
virtual ranges remain retained. This blocks copied-request completion, premature
completion and repeated credit while preserving later forwarding resolution.
The graph now wraps its 64 full Old-source decommits with the candidate
transaction and checks unchanged metadata charge. Existing partial large-array
protection remains outside this whole-run accounting path. Actual OS retry
scheduling, source/color/address reuse and default activation are still open.

Source-uncommit validation: workspace exited 0 with 794 passed / zero failed
or ignored across 25 invocations, including required ASan+UBSan/TSan and x86
emitted contention. ABI: eight passed. ARM replayed 12,644 full-state vectors
(source uncommit 154; resolver 1,544; metadata 2,166; reference lease 1,705;
remembered owner 5,938; remembered inventory 1,137). The Mach-O fixture exports
72 helpers. Clippy, fmt, diff and inventory checks passed (9,360 sites / 96
files). Logs: `/tmp/klassic-source-uncommit-workspace.log`,
`/tmp/klassic-source-uncommit-emitted.log`, `/tmp/klassic-source-uncommit-arm.log`,
`/tmp/klassic-source-uncommit-abi.log`, `/tmp/klassic-source-uncommit-clippy.log`.
Native Darwin ordering/scheduling and final retirement are not established.

### Incoming forwarding dependencies (partial G04/G10/G15)

Added the bounded `ForwardIncoming` C/shared-emitter scan and 134 complete-state
conformance cases. It detects published destinations within a retained source
range and waits on unfinished publication. In 32 moving-graph chains, incoming
forwarding remains detectable after payload uncommit, zero metadata leases and
source remembered-inventory detachment. The model rejects middle-source
retirement until older forwarding sources retire.

The caller must freeze publication over the entire audit and retain tables and
target controls. This is a necessary dependency check, not a complete retirement
certificate. Production freeze coordination, audits of every reference holder,
source metadata/address reuse and default collector activation remain open.
ARM emulation passed 134 full-state vectors, 27 API-model scenarios (164 helper
calls) and 1,536 shared-context transitions. This does not establish native
Darwin concurrency or hardware ordering. The exported fixture has 73 helpers.

Validation: workspace exited 0 with 795 passed, zero failed/ignored across 25
invocations, including required ASan+UBSan/TSan and emitted x86 contention.
The ABI suite passed eight tests after adding the missing ForwardIncoming
C/Rust size, alignment and field-offset comparison. Clippy with warnings denied,
formatting, diff and inventory checks passed (9,360 sites / 96 files).
Logs: `/tmp/klassic-forward-incoming-workspace.log`,
`/tmp/klassic-forward-incoming-abi-final.log`,
`/tmp/klassic-forward-incoming-arm.log`,
`/tmp/klassic-forward-incoming-clippy.log`.

### Repairing registered roots during concurrent audit (partial G04/G08/G15)

Added `RootWalkHeal` in C and the shared emitter, authenticated by the active
scanner lease and captured prefix. It uses exact CAS, preserves observed color
bits, and leaves concurrent logical stores/releases intact. Same-object repair
does not add journal entries or change root_revision. Same logical identity,
strong protection and exclusion of address reuse remain caller obligations.

The moving graph now repairs its registered array root after relocation and
before the owner overwrites it, with exact destination/color and journal/revision
checks in all 64 rounds (including 16 protected large-array source pages).
The x86 candidate passed 804 complete-state cases, 64 controlled owner/scanner
store-release rounds and the moving graph. ARM emulation passed the same 804
state vectors plus 27 API-model scenarios / 164 helper calls and 1,536 context
transitions. ABI layout checks passed eight tests. The Mach-O fixture exports
74 helpers; emulation does not establish native Darwin scheduling or ordering.

RootAudit still certifies mark work, not complete physical retirement. Production
remap completion, late-publication coordination, source/color/address reuse,
default activation and the real-OS/performance gates remain open.

Final validation: workspace exited 0 with 795 passed / zero failed or ignored
across 25 invocations, including required ASan+UBSan/TSan (the new root cases
run in gc_roots_test, the moving graph in gc_heap_test) and emitted x86
contention. Both sanitizer outputs must contain all three new completion
markers. Clippy with warnings denied, strict C syntax/warning checks, fmt,
diff and inventory checks passed (9,409 sites / 96 files).
Logs: `/tmp/klassic-root-walk-heal-workspace-final.log`,
`/tmp/klassic-root-walk-heal-emitted-final.log`,
`/tmp/klassic-root-walk-heal-arm.log`, `/tmp/klassic-root-walk-heal-abi.log`,
`/tmp/klassic-root-walk-heal-clippy.log`.

### Durable root scan items (partial G03/G04/G08/G15)

RootWalk now retains one explicit pending snapshot, its original comparison bits
and index. C/shared-emitter CAPTURE/ACK prevent NEXT or bulk scan from bypassing
unoffered work. ACK matches the captured index as well as the segment lease
serial, rejecting a delayed ACK for an earlier item in that segment. The unique
walk owner must complete strong mark handoff and repair before acknowledging.

The moving graph's root audit now uses that pending record across metadata
handoff and a poll after each forwarding edge, then repairs and acknowledges.
Its journal COPY handoff also uses the bounded cursor path. In all 64 array
rounds, the root snapshot traverses one or two edges before repair, including
the existing protected-source scenarios. Mark-end rejection uses the walk's
existing counted lease; no logical root mutation/revision is needed for staging.
All native controls/objects are retained by the fixture; this does not establish
complete source retirement or independent generation overlap.

x86 candidate and ARM emulation passed 24,400 full-state vectors across the
changed root operations: item 2,749; walk 1,891; scan 18,335; heal 804; audit 621.
ARM also passed 27 API-model scenarios / 164 helper calls and 1,536 context
transitions. ABI layout checks passed eight tests. The fixture exports 75
helpers. Native Darwin scheduling/hardware ordering, production integration,
full retirement, default activation and performance gates remain open.

Final validation: workspace exited 0 with 795 passed / zero failed or ignored
across 25 invocations, including required ASan+UBSan/TSan and emitted x86
contention. Both sanitizer runs must report the item and graph markers as well
as the expanded walk/audit counts. Strict C warnings, Clippy with warnings
denied, fmt, diff and inventory checks passed (9,474 sites / 96 files).
Logs: `/tmp/klassic-root-walk-item-workspace.log`,
`/tmp/klassic-root-walk-item-emitted-final.log`,
`/tmp/klassic-root-walk-item-arm.log`, `/tmp/klassic-root-walk-item-abi.log`,
`/tmp/klassic-root-walk-item-clippy.log`.

### Root audit worker pressure and owner updates (partial G02/G03/G08/G11)

The audit now runs with an explicit executing context. In 64 moving-graph rounds,
a separate registered OS worker scans the actual owner's roots and survives
128 metadata-exhaustion waits: source acquisition without a token and destination
acquisition with the source token retained. Both waits cross real phase retry
rendezvous, preserve the pending snapshot, and keep mark completion blocked.
The worker adopts the current phase on reentry, completes handoff/repair/ACK,
detaches and is joined before its context storage goes away.

In 32 rounds (16 Young / 16 Old), the owner stores null while that worker is
blocked. Delayed repair preserves null, and journal publication/revision reflect
only the logical owner writes. Explicit republishing is required before a later
audit restores the physical root. The x86 emitted conformance harness passed
these schedules with the unchanged 40-second alarm (14.32 seconds total).
These fixture schedules do not prove native Windows/Darwin behavior, final
retirement, independent-generation fairness or default collector integration.

Validation: workspace exited 0 with 795 passed / zero failed or ignored across
25 invocations, including required ASan+UBSan/TSan and emitted x86 contention.
Both sanitizer builds must report the pressure and deletion markers. Clippy
with warnings denied, strict C warning/syntax checks, fmt, diff and inventory
checks passed (9,474 sites / 96 files). Logs:
`/tmp/klassic-root-audit-pressure-workspace.log`,
`/tmp/klassic-root-audit-pressure-emitted-final.log`,
`/tmp/klassic-root-audit-pressure-clippy.log`.
This change exercises existing helpers through a new concurrent C orchestration;
it adds no machine-code primitive or ABI layout and does not add native Darwin
or Windows execution evidence.

### Relocation admission and publication quiescence (partial G04/G10/G15)

Added C/shared-emitter RelocationGate with counted native job permits. Admission
closes before waiting for existing moves; existing holders can finish while new
movers are rejected. Closed/empty CHECK and a nonwrapping reopen epoch provide
the boundary needed by a publication audit. Stable controls and serial/epoch
requests reject copied permits, duplicate release and stale completion after
reuse. The caller remains responsible for completing copy/cleanup before release.

Every move in the moving graph now registers before claim and remains registered
through forwarding publication. 64 pending publications prevent source sealing
without advancing its scan; sealing then runs with admission closed and empty.
32 incoming-forwarding audits reject new admission and verify the same epoch
before reopening. Existing incoming dependencies remain retained. This supplies
the graph's publication freeze but does not prove complete source retirement or
enroll the default collector's production paths.

x86 candidate and ARM emulation passed 533 complete-state gate cases. ARM also
passed 27 API-model scenarios / 164 helper calls and 1,536 context transitions.
ABI checks passed eight tests; the Mach-O fixture exports 76 helpers. Native
Darwin ordering/scheduling, production cancellation/admission, default migration
and full retirement/performance gates remain open.

Relocation-gate validation completed: workspace exited 0 with 795 passed,
zero failed/ignored across 25 invocations, including mandatory ASan+UBSan/TSan
and emitted x86 contention. Clippy with warnings denied, strict C syntax/warnings,
fmt, diff and inventory checks passed (9,474 sites / 96 files).
Logs: `/tmp/klassic-relocation-gate-workspace.log`,
`/tmp/klassic-relocation-gate-clippy.log`,
`/tmp/klassic-relocation-gate-arm.log`.

### Private relocation cancellation (partial G04/G10/G11)

The OS-backed relocation pipeline now enrolls every source claim in the admission
gate and holds its permit through publication or private-copy cleanup and source
retention. An additional 32 rounds inject cancellation before target preparation,
after preparation, after seven copied bytes, and after complete copying before
installation. Each stage runs against both Young and Old target generations.
While cancellation is pending, closing admission rejects a new mover and CHECK
continues to wait. After cleanup, RETAIN and source access reopening, releasing
the permit makes the gate empty, but SourceDrain still refuses to seal the run
containing the retained object. Payload accounting stays unchanged.

The remaining four objects still move, reusing the uninstalled target range.
All five objects are resolved through their proper metadata leases and access
guards; full payload contents and shared logical mark identities are checked.
The original 32 fully moving/protected-source rounds remain in the fixture.
Both sanitizer runs and the emitted x86 harness require the new cancellation
marker. This is deterministic failure orchestration around existing runtime
leaves, not a production cancellation scheduler or an installed-target rollback
protocol. It adds no ABI or emitter primitive; native Darwin/Windows execution,
post-install failure handling, final retirement and default migration remain open.

Validation: the final workspace run exited 0 with 795 passed, zero failed/ignored
across 25 invocations. Required ASan+UBSan and TSan runs and emitted x86 contention
passed with the new cancellation marker enforced. Clippy with warnings denied,
strict C warnings/syntax, fmt, diff and inventory checks passed. The focused x86
run took 14.84 seconds with the unchanged 40-second alarm; the final workspace
run additionally covers the revised stage/generation cross product.
Logs: `/tmp/klassic-relocation-cancel-workspace.log`,
`/tmp/klassic-relocation-cancel-emitted.log`,
`/tmp/klassic-relocation-cancel-clippy.log`.

### Installed relocation recovery (partial G04/G10/G11)

The OS-backed relocation pipeline adds 32 interrupted-job rounds: sixteen after
INSTALL but before forwarding and sixteen after forwarding but before permit
release, evenly split between Young and Old targets. The original claimant keeps
its job resources and permit. Recovery publishes a pending destination or checks
the exact immutable forwarding receipt, then releases the permit once.
Destination metadata leases and guarded payload/identity verification span the
recovery. The gate remains nonempty until completion, and duplicate release
rejects. Repeated INSTALL and never-installed-run cleanup reject without changes
to installed page/table/bitmap, copy state or payload/metadata accounting.
Invalid-color publication also preserves the unpublished claim.

All 64 fully moving rounds now seal their sources via SourceDrain under closed,
empty admission before OS decommit. The 32 partial-cancellation rounds retain
the source and stop sealing at its retained object. Both sanitizer runs and the
emitted x86 harness require the installed-recovery marker. Focused x86 execution
passed in 14.93 seconds with the unchanged 40-second alarm.

This extends C orchestration around existing C/shared-emitter leaves, with no ABI
or machine-code primitive changes. It establishes a fixture recovery policy,
not production durable-job scheduling, generation-handoff failure recovery,
final metadata retirement, native Darwin/Windows behavior or default migration.

Final validation: workspace exited 0 with 795 passed, zero failed/ignored across
25 invocations, including required ASan+UBSan/TSan and emitted x86 contention.
Both sanitizer executions require the recovery marker. Clippy with warnings
denied, strict C warnings/syntax, fmt, diff and inventory checks passed
(9,474 sites / 96 files). Logs:
`/tmp/klassic-relocation-installed-workspace.log`,
`/tmp/klassic-relocation-installed-emitted.log`,
`/tmp/klassic-relocation-installed-clippy.log`.

### Installed-job ownership receipts (partial G04/G10/G11/G15)

Added the 88-byte RelocationJob ABI and matching C/shared-emitter BIND/CHECK
operations. Bound controls retain their native address, gate/permit serial and
epoch, source key/incarnation, claim ticket and exact colored destination.
Pending claims return WAIT; only the matching forwarding publication records
completion. Permit list membership checks reject copied permit controls;
self-address and incarnation checks reject copied/stale jobs. Repeated CHECK is
valid until permit release; subsequent completion attempts reject.

The C/x86 full-state suite has 689 cases covering every legal reference color,
open/closed admission, head/middle/tail and singleton permits, invalid links and
keys, pending/publication transitions, and stale completion after permit reuse.
The OS-backed pipeline uses the helper for 448 installed jobs and rejects copied
controls in its 32 interruption rounds. Both mandatory sanitizer executions and
the emitted harness require the case and pipeline markers. Focused x86 passed
in 16.12 seconds with the unchanged 40-second alarm; ABI tests passed eight tests.

This helper records and authenticates publication ownership; it does not prove
INSTALL or generation handoff, allocate job storage, prevent a scheduler from
creating two fresh controls for one claim, or implement production rescheduling.
Default activation, complete retirement, native Windows/Darwin and performance
gates remain open.

Final validation: workspace exited 0 with 795 passed, zero failed/ignored across
25 invocations, including mandatory ASan+UBSan/TSan and emitted x86 contention.
ARM replay passed all 689 complete-state job vectors, plus 27 API scenarios /
164 calls and 1,536 context transitions. The final Mach-O fixture export check
passed with 77 helpers after adding the new helper to its export list. Clippy
with warnings denied, strict C warnings/syntax, fmt, diff and inventory checks
passed (9,474 sites / 96 files). Logs:
`/tmp/klassic-relocation-job-workspace.log`,
`/tmp/klassic-relocation-job-emitted.log`,
`/tmp/klassic-relocation-job-arm.log`, `/tmp/klassic-relocation-job-abi.log`,
`/tmp/klassic-relocation-job-arm-export.log`,
`/tmp/klassic-relocation-job-clippy.log`.

### Permit-bound completion (partial G04/G10/G11/G15)

RelocationPermit grows from 40 to 48 bytes with an exclusive job binding. BIND
rejects a second live job on the permit, and CHECK/FINISH authenticate that exact
back-pointer. FINISH requires an already checked, exact publication and records
terminal state 3 while clearing the binding. Gate RELEASE rejects every bound
job, including a CHECK-completed one. Duplicate FINISH and subsequent CHECK
reject. This closes the enrolled job's early-permit-release path.

The full-state suites now cover 1,132 job cases and 535 gate cases. The 448-object
OS-backed pipeline requires rejection of a fresh duplicate binding, pending
FINISH and early RELEASE. Every move in the 64-round concurrent graph now keeps
its receipt alongside its stable permit record, checks publication, FINISHes,
then releases. Graph teardown checks terminal receipts and cleared permits.
Focused x86 passed in 15.47 seconds with the unchanged 40-second alarm. ARM replay
passed all 1,667 updated vectors, 27 API scenarios / 164 calls and 1,536 context
transitions. ABI checks passed eight tests; the Mach-O fixture remains 77 helpers.

These are shared runtime leaves and conformance integration. Production enrollment,
one permit per unique source claim, private-copy cleanup before binding, durable
scheduling, default migration, final retirement and native OS/performance gates
remain open. A second permit for the same claim is still forbidden by the
scheduler contract; the new binding specifically enforces one job per permit.

Final validation: workspace exited 0 with 795 passed, zero failed/ignored across
25 invocations, including mandatory ASan+UBSan/TSan and emitted x86 contention.
Both sanitizer executions require the expanded case markers and the new moving-
graph binding marker. Clippy with warnings denied, strict C warnings/syntax,
fmt, diff and inventory checks passed (9,474 sites / 96 files). Logs:
`/tmp/klassic-relocation-binding-workspace.log`,
`/tmp/klassic-relocation-binding-emitted.log`,
`/tmp/klassic-relocation-binding-arm.log`, `/tmp/klassic-relocation-binding-abi.log`,
`/tmp/klassic-relocation-binding-arm-export.log`,
`/tmp/klassic-relocation-binding-clippy.log`.

### Shared relocation completion funnel (partial G04/G10/G11/G15)

Added the 32-byte RelocationCompletion control and C/shared-emitter completion
funnel. Four resumable stages bind, publish/check, FINISH and release. Each stage
is bounded and retains ownership on rejection; terminal or copied controls
cannot replay completion. Already-published recovery uses the immutable native
receipt without touching the old object header. The emitter composes the existing
job, publication and gate code, so the ordering is emitted runtime behavior.

All 448 installed pipeline objects and every move in 64 concurrent graph rounds
now use this funnel in place of the fixture's hand-written completion sequence.
The 1,047 full-state cases cover every legal color, each stage, invalid controls,
publication interrupted before/after CAS, and invalid completed-copy headers
followed by repair/retry. Required sanitizer and emitted harness markers enforce
execution. Focused x86 passed in 16.40 seconds with the unchanged 40-second alarm.
ABI tests passed eight cases; the Mach-O fixture exports 78 helpers.

Production inspection confirms that ordinary direct output still uses the old
header/reference ABI and LLVM still calls the legacy C collector. This shared
runtime funnel is reusable migration code, not evidence of compiler activation.
Whole allocation/root/barrier lowering, production durable scheduling, target
INSTALL/generation handoff, final retirement, native OS execution and performance
gates remain open.

Final validation: workspace exited 0 with 795 passed, zero failed/ignored across
25 invocations, including mandatory ASan+UBSan/TSan and emitted x86 contention.
ARM replay passed all 1,047 complete-state vectors, plus 27 API scenarios / 164
calls and 1,536 context transitions. Clippy with warnings denied, strict C
warnings/syntax, fmt, diff and inventory checks passed (9,474 sites / 96 files).
Logs: `/tmp/klassic-relocation-complete-workspace.log`,
`/tmp/klassic-relocation-complete-emitted-final.log`,
`/tmp/klassic-relocation-complete-arm.log`, `/tmp/klassic-relocation-complete-abi.log`,
`/tmp/klassic-relocation-complete-arm-export.log`,
`/tmp/klassic-relocation-complete-clippy.log`.

### Shared relocation admission/claim (partial G02/G04/G10/G11/G15)

Added the 48-byte RelocationClaim ABI and C/shared-emitter entry funnel. It
acquires admission before the source CAS and derives the ticket from the permit
serial. Success retains the permit and returns ticket/epoch. Busy, retained,
forwarded, malformed-source-word and ticket-width failures release the acquired
permit; closed admission claims nothing. Admitted failures consume serials,
never reset or wrap them. SELECT/CHECK remains required before copying.

Installed-job validation now requires ticket == permit.serial, preventing another
permit from adopting a claim within the same canonical heap gate. All 480 source
claims in the OS-backed pipeline and every move in the 64-round concurrent graph
use the new entry funnel and propagate returned tickets through source draining,
copying, retention and completion. This replaces their fixed claim tickets.

The claim suite contains 431 full-state cases. The revised job suite contains
1,134 cases, including a claim presented with another permit and a mismatched
ticket. The completion suite remains 1,047 cases and is replayed because it
embeds the updated job emitter. In 64 actual two-thread metadata-actor rounds,
exactly one claimant retains a permit, the loser releases its permit, admission
closure rejects a retry, and the winner retains/reopens its source before release.
These are native metadata actors, not evidence of complete worker registration.
Both sanitizer executions and emitted x86 require the claim/race markers.

Focused x86 passed in 16.75 seconds with the unchanged 40-second alarm. ABI tests
passed eight cases; the Mach-O fixture exports 79 helpers. Source metadata/strong
liveness, canonical heap-gate setup, production scheduling, compiler activation,
final retirement and native OS/performance gates remain open.

Final validation: workspace exited 0 with 795 passed, zero failed/ignored across
25 invocations, including required ASan+UBSan/TSan and emitted x86 contention.
ARM replay passed all 2,612 updated entry/job/completion vectors, plus 27 API
scenarios / 164 calls and 1,536 context transitions. Clippy with warnings denied,
strict C warnings/syntax, fmt, diff and inventory checks passed
(9,474 sites / 96 files). Logs:
`/tmp/klassic-relocation-claim-workspace.log`,
`/tmp/klassic-relocation-claim-emitted-final.log`,
`/tmp/klassic-relocation-claim-arm.log`, `/tmp/klassic-relocation-claim-abi.log`,
`/tmp/klassic-relocation-claim-arm-export.log`,
`/tmp/klassic-relocation-claim-clippy.log`.

### Bounded private-copy discard (partial G04/G10/G11/G15)

Added the 40-byte RelocationDiscard cursor and matching C/shared-emitter cleanup
step. Calls verify the owned, drained source claim and unchanged private target
top, then clear at most 256 bytes. Partial and completed-but-uninstalled copies
are supported; untouched requests erase nothing. Zero-payload copies still clear
the header and minimum 16-byte payload reservation. Copied cursors, changed tops,
live source readers, invalid bounds and overlapping ranges reject unchanged.

386 complete-state cases cover boundary sizes across the 256-byte limit, partial
and complete copies, Ready/Closed targets, repeat completion, corrupted controls
and actual installed-top rejection. The OS-backed pipeline uses the helper in
48 cleanup jobs while retaining admission and source freeze. Fresh PREPARE follows
each retry, and all 32 installed-recovery rounds reject discard. Both mandatory
sanitizer executions and emitted x86 require the new case/pipeline markers.
Focused x86 passed in 16.59 seconds with the unchanged 40-second alarm; the final
workspace run also covers the fresh-PREPARE retry refinement. ARM replay passed
all 386 vectors, plus 27 API scenarios / 164 calls and 1,536 context transitions.
ABI checks passed eight tests; the Mach-O fixture exports 80 helpers.

This is the bounded cleanup step. Source retention/reopening and permit release
remain caller steps after DONE, and target exclusivity plus source metadata/
strong liveness remain caller obligations. Production cancellation scheduling,
compiler activation, complete retirement and native OS/performance gates remain
open. No payload/metadata accounting credit is issued for a private-range clear.
Final workspace validation passed: 795 tests across 25 test invocations,
zero failures or ignored tests. Formatting, diff whitespace and regenerated
access-inventory checks passed (9,474 candidate sites across 96 files).
Workspace log: `/tmp/klassic-relocate-discard-workspace.log`.

### Required LLVM validation tooling

Removed missing-clang success returns from LLVM differential/statistics and
hand-written IR link validation. Both supported-host suites now fail with an
explicit clang requirement. `cargo test --test llvm_backend --test llvm_gc_link`
passed 11 + 1 tests; running each resulting test executable with empty PATH and
KLASSIC_CLANG unset failed with exit 101 and the required-tool diagnostic.
Logs: `/tmp/klassic-llvm-required-validation.log` and
`/tmp/klassic-{llvm_backend,llvm_gc_link}-missing-clang.log`.
Legacy ABI documentation now identifies the existing shared direct emitter and
low-bit header marks correctly. This does not activate the generational ABI.

### Unbound relocation admission inspection (partial G04/G11/G15)

C and shared x86/ARM emitters implement non-mutating INSPECT for a live unbound
relocation permit. Private discard uses it before every slice in all 48 pipeline
cleanup jobs. Complete-state gate cases increased from 535 to 593, covering
valid head/middle/tail permits with open/closed admission, released permits,
corrupted nonce/links and installed-job binding rejection. Focused emitted x86
passed in 16.62 seconds with the unchanged 40-second alarm. ARM replay passed all
593 gate cases plus the existing 27 API scenarios / 164 calls and 1,536 context
transitions. The ARM harness case-count assertion was updated after its initial
run correctly rejected the stale expected count of 535. This does not yet supply
the production cancellation scheduler or default compiler ABI activation.

Final validation: workspace 795 passed / 0 failed / 0 ignored across 25 test
invocations, including required C ASan+UBSan and TSan validation. Clippy, strict
C syntax/warnings, formatting, diff whitespace and access inventory passed.
Logs: `/tmp/klassic-relocation-inspect-{workspace,emitted,arm,clippy}.log`.

### Shared private relocation cancellation (partial G04/G10/G11/G15)

The C and shared-emitter cancellation funnel now connects bounded private-copy
cleanup, source retention, source reopening and permit release. Every invocation
validates live unbound admission; cleanup remains limited to 256 bytes, followed
by separate constant-size metadata transitions. Installed destinations cannot be
cancelled. Invalid requests preserve ownership, and terminal/copy replay rejects.

769 complete-state cases cover untouched, prepared, partial and complete private
copies, zero payload, 256-byte boundaries, open/closed admission, stage-by-stage
invalid controls and active reopened-source readers. Generated x86 passed in
17.75 seconds with the unchanged 40-second alarm. ARM replay passed all 769 cases,
27 API scenarios / 164 helper calls and 1,536 context transitions. ABI checks
passed eight tests; the Mach-O fixture exports 81 helpers. Both required sanitizer
runs and emitted x86 require the new case and pipeline success markers.

All 32 OS-backed cancellation rounds now use the shared funnel, holding a source
reader guard across final permit release. All 32 installed recovery jobs reject
cancellation. The 16 private-copy retries preserve their source claim and use the
existing bounded-discard path. Production scheduling, default compiler activation,
complete retirement, native OS and performance gates remain open.

Final validation passed: workspace 795 tests across 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and generated x86. Clippy,
strict C syntax/warnings, formatting, diff whitespace and access inventory passed.
Logs: `/tmp/klassic-relocation-cancel-{workspace,emitted,arm,abi,export,clippy}.log`.

### Finite allocation/output-root handoff (partial G03/G08/G10/G15)

Added the 16-byte AllocationInto control, shared TLAB allocation/publication
entry, and non-mutating empty-root preflight. Journal capacity and revision
exhaustion are checked before allocation. Successful allocation publishes the
initializing reference into the journal and reserved root in one no-safepoint
interval. The OS-backed page pipeline now uses this entry for all four large,
zero-sized and mixed-layout allocations.

618 complete-state cases cover valid phases, size boundaries, occupied/stale
output roots, journal-full retry after consumption, exhaustion, TLAB and metadata
refill and allocation overflow. Existing allocation-leaf conformance supplies
the heap oracle; root and journal state deltas are checked separately. Emitted
x86 passed in 17.63 seconds with its unchanged 40-second alarm. ARM replay passed
618 allocation/root cases and 969 existing root cases, plus 27 API scenarios /
164 calls and 1,536 context transitions. ABI checks passed eight tests, and the
Mach-O fixture exports 82 helpers.

During integration, two stale caller types were corrected and the emitted test
harness's new code-unload operation was moved after its final page-pipeline use;
the earlier placement caused a post-case SIGSEGV. An isolated C unsanitized run
passed all 618 cases. A separate ad hoc ASan binary exited 139 without a report;
that attempt is not sanitizer evidence. Required sanitizer evidence is provided
only by the complete mandatory workspace suite recorded below.

This entry supplies the finite handoff; ordinary compiler ABI activation,
initialization completion and production allocation slow paths remain open.

Final required validation passed: workspace 795 tests / 25 invocations, zero
failures or ignored tests, including C ASan+UBSan/TSan with both new-case markers
and generated x86. Clippy, strict C syntax/warnings, formatting, diff whitespace
and inventory passed (9,476 sites / 96 files). Logs:
`/tmp/klassic-alloc-into-{workspace,emitted,arm,abi,export,clippy}.log`.

### Allocation handoff in the registered moving graph (partial G02/G08/G10/G15)

Replaced all 512 setup allocations across the 64 moving-graph rounds with the
shared AllocationInto entry. A registered OS observer holds root storage leases
before each allocation/deletion, scans concurrently, then claims the actual
single-entry journal. Delayed ACK forces 448 ROOT_FLUSH results before allocation;
TLAB, arena, results, empty root and revision remain unchanged. Sixty-four ninth
allocation attempts separately verify metadata exhaustion with an empty output
root and available journal capacity.

The observer checks and ACKs 1,088 journal handoffs (eight allocations and nine
root releases per round), then detaches and joins. Leased retirement scanning,
clearing and FINISH remove every construction root before the existing mark and
moving scenarios. The final thread-identity assertion increases from four to
five because of the additional real registered observer. Both mandatory C
sanitizer runs and generated x86 require the new integration success marker.
Focused emitted x86 passed in 18.41 seconds with the unchanged 40-second alarm.

Construction and these journal ACKs occur in explicitly checked idle phase zero.
This is not active-mark allocation coverage, production allocation scheduling or
ordinary compiler ABI activation. The shared emitted runtime instructions did
not change in this increment; native Darwin/Windows execution remains unverified.

Final validation passed: workspace 795 tests across 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and emitted x86 with the new
observer marker. Clippy, strict C syntax/warnings, formatting, diff whitespace
and inventory (9,476 sites / 96 files) passed. Logs:
`/tmp/klassic-alloc-graph-{workspace,emitted-final,clippy}.log`.

### Initializing-root routing wait (partial G03/G08/G10/G15)

C/shared mark routing now returns WAIT for authenticated INITIALIZING headers,
retaining the entire input and lease without reading payload or moving mark work.
Strict load/store/mark-prepare paths still require PUBLISHED headers. Existing
root/journal offer adapters handle WAIT outside their guard, preserving the
counted producer and retrying from its protected root or cursor.

Complete-state routing coverage increased from 4,695 to 4,987 cases: 128 owned
initialization waits and ready retries across generation/epoch combinations,
plus 36 malformed initialization/header cases. All 4,987 passed generated x86
and ARM replay. An additional 64 two-actor leaf races verify guard exit/reentry,
input preservation and payload visibility after release/acquire header readiness.
Focused x86 passed in 18.83 seconds with the unchanged 40-second alarm. ARM also
passed 27 API scenarios / 164 calls and 1,536 context transitions. Clippy passed.

The contention fixture supplies validated metadata/guards; it does not establish
registered active-mark allocation, mark-end retry integration, initialization
publication API, or production scheduling. Those and default compiler activation
remain open. Native Darwin execution/hardware ordering remains unverified.

Final workspace validation for initializing-root routing passed: 795 tests
across 25 test invocations, zero failures or ignored tests. The invocation
count includes empty doc-test suites; those are not additional coverage.
Evidence: `/tmp/klassic-initializing-route-workspace.log`. A subsequent
`cargo build -p klassic-runtime` passed, `cargo test --test gc_abi` passed
all eight tests, and the access inventory check passed (9,483 sites across
96 files). These checks do not establish default backend activation or
active-mark allocation integration.

### Allocation control alignment (partial G10/G15)

The TLAB leaf now rejects misaligned descriptor, result, page and object-start
bitmap addresses before accessing them. C and the shared x86/ARM emitter agree;
backing storage validity and lifetime remain caller obligations. Twenty-eight
new cases exercise all seven nonzero alignment residues for these four controls
and require complete fixture preservation on INVALID. The shared TLAB suite now
has 2,804 cases. Generated x86 passed (19.80 seconds, unchanged 40-second alarm),
and ARM replay passed all 2,804 plus 27 API scenarios / 164 calls and 1,536
context transitions. Strict C warnings, formatting, Clippy and the inventory
check (9,485 sites / 96 files) passed. Logs use the prefix
`/tmp/klassic-alloc-alignment-`. Initialization publication and active-mark
allocation integration remain open, as does ordinary default activation.

Required C ASan+UBSan and TSan validation also passed via
`cargo test --test llvm_gc` (107.24 seconds); the harness requires the new
2,804-case marker in both sanitizer runs. This increment used focused validation;
the previously recorded 795-test workspace result predates this alignment fix.

### Registered initializing roots at mark end (partial G03/G08/G15)

In 32 Old-mark moving-graph rounds, the array stays INITIALIZING until its
registered root has been scanned and its actual publication journal claimed and
copied. Both offers return BUSY through the graph adapter, release their source
guards and temporary metadata tokens, leave the mark record unstamped and keep
the queue empty. The copied journal receipt, entry, unconsumed ring and owner root
survive a real three-participant mark-end rendezvous. The shared end predicate
rejects completion and the phase coordinator resumes ConcurrentMark. Only then
does the owner release-publish readiness and offer/ACK the retained work. The
existing complete graph scan, movement, identity and termination checks follow.

Generated x86 passed in 18.21 seconds with the unchanged 40-second alarm. The new
marker is mandatory in the emitted harness and both C sanitizer runs. Runtime
instruction emission did not change in this increment; the earlier ARM routing
replay remains leaf evidence, not this registered-thread integration test.
Allocation still occurs during idle setup, all child fields are initialized
before readiness, and publication still uses a fixture release store. This does
not establish allocation during active marking, initialization cancellation or
a public initialization API. Ordinary compiler activation remains open.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and emitted x86 with
the new integration marker. Clippy, strict C syntax/warnings, formatting,
diff whitespace and inventory (9,485 sites / 96 files) passed. Logs:
`/tmp/klassic-initializing-graph-{workspace,emitted,clippy}.log`.

### Guarded initialization publication (partial G03/G08/G10/G15)

Added `gc_guarded_initialization_publish` in C and shared emission, borrowing the
existing guarded-mark request layout without transferring its lease or output.
It validates the canonical guard, phase, header, owner and allocating Young page,
and rejects selection, closed admission, malformed inputs and duplicate publish.
Only success release-publishes readiness. Payload initialization, child protection
and counted publication work remain caller obligations.

The 536 complete-state cases cover all 32 epoch combinations and eight symmetric
phase combinations, repeat publication and 24 invalid cases. C/candidate x86 and
ARM replay agree. The 64 two-actor initialization races now use this helper with
separate initializer/reader contexts and source guards; the initializer's release
orders reference/scalar writes before routing READY and payload reads. These
remain prevalidated fixture controls, not a full allocation scheduler.

Focused x86 passed in 18.07 seconds (unchanged 40-second alarm). ARM replay passed
536 vectors plus 27 API scenarios / 164 calls and 1,536 context transitions. The
Mach-O export fixture now contains 83 helpers. Native Darwin/Windows execution,
active-mark allocation, completion after TLAB closure, cancellation and ordinary
compiler activation remain unverified or incomplete. Logs use the prefix
`/tmp/klassic-initialization-`.

The first C sanitizer run exposed a fixture error after adding the initializer's
second source guard: ENTER/EXIT were incorrectly assumed to succeed on their
single CAS. The fixture now retries WAIT, yielding only for ENTER outside a
guard and retrying EXIT immediately while the guard remains held. The updated
emitted test passed in 19.30 seconds. This did not change the publication helper
or the 536 ARM vectors.

Final validation passed after the fixture retry fix: workspace 795 tests /
25 invocations, zero failures or ignored tests, including required C
ASan+UBSan/TSan and emitted x86. Clippy, strict C syntax/warnings, formatting,
diff whitespace and inventory (9,485 sites / 96 files) passed. Final logs:
`/tmp/klassic-initialization-workspace-final.log` and
`/tmp/klassic-initialization-emitted-retry.log`.

### Readiness publication in the OS allocation pipeline (partial G10/G15)

The page pipeline now publishes all four rooted allocations through the shared
initialization helper before TLAB closure: a page-sized scalar payload, a zero
payload and two mixed records with zero reference slots. It acquires an actual
page metadata lease from a finite pool, enters the allocator-installed forwarding
cell, publishes once, rejects duplicate publication without request mutation,
exits the guard and releases the lease. Root cells stay unchanged; the existing
root/journal drain verifies all four allocations and now requires PUBLISHED
headers. No temporary lease or context access epoch remains after each handoff.

The generated harness keeps reference-lease, metadata and source-access code
loaded until the final pipeline call. An initial harness compile failure from
overly broad call-site editing was corrected; the final emitted test passed in
21.04 seconds with its unchanged 40-second alarm and required integration marker.
Clippy, strict C warnings, formatting and diff whitespace passed. Runtime helper
instructions did not change, so this increment adds OS allocation integration,
not new ARM instruction evidence. Root-segment registration remains fixture
setup, allocation occurs in idle phase, and active-mark allocation, closed-TLAB
initialization ownership and ordinary default activation are still incomplete.

Required C ASan+UBSan/TSan passed via `cargo test --test llvm_gc` in
107.71 seconds, with the new marker required in both runs. Inventory check
passed at 9,485 sites / 96 files. This increment used focused validation; the
previous 795-test workspace result predates this pipeline integration. Logs:
`/tmp/klassic-initialization-pipeline-{emitted-final,sanitizers,clippy}.log`.

### Allocation-to-mark handoff under a marking snapshot (partial G03/G10/G15)

The four OS-backed page-pipeline allocations now adopt an explicit dual
ConcurrentMark snapshot before allocation, and their root journals preserve that
phase. Before readiness each actual allocation routes to WAIT without changing
the request or transferring its lease. The caller exits/reenters its source guard,
publishes readiness, then routes READY and transfers the lease into a capacity-one
Young or Old queue. TAKE remains BUSY until a guarded descriptor-driven scan is
finished; only FINISH permits lease release. The final root/journal drain checks
phase and selected-generation mark stamps, and every temporary lease/guard ends
empty. Mixed-record reference fields are zero, so no nonnull child publication is
claimed by this scenario.

Generated x86 passed in 17.94 seconds with the unchanged 40-second alarm and new
required marker. Runtime helper instructions did not change. This is a composed
allocation/publication/mark test with an explicit phase/context fixture, not
registered concurrent allocation scheduling or phase adoption proof. Real worker
allocation, closed-TLAB initialization ownership, cancellation and ordinary
compiler activation remain incomplete. Logs use the prefix
`/tmp/klassic-allocation-mark-handoff-`.

Required C ASan+UBSan/TSan passed in 104.17 seconds via
`cargo test --test llvm_gc`, including both occurrences of the new marker.
Clippy, strict C warnings, formatting, diff whitespace and inventory
(9,485 sites / 96 files) passed. This increment used focused validation;
the earlier 795-test workspace result predates this composed handoff.

### Registered allocation-owner lifecycle (partial G02/G03/G08/G10/G15)

The OS allocation pipeline no longer assigns its owner's thread identity,
adopted phase or active root-segment scope directly. Shared worker ATTACH
registers the actual calling owner and adopts the coordinator's supplied
ConcurrentMark snapshot; shared root ATTACH links its segment into both registry
and context inventories. Every allocation now completes both Young and Old
mark scans before deletion/publication work can be acknowledged.

Teardown uses RETIRE, CLAIM and leased root scanning with the captured dual-mark
mask, plus CLAIM/COPY of the actual publication journal. Both record epochs and
all copied references are checked before ACK. The journal lease prevents CLEAN
until its ACK; bounded clear/FINISH then empties both root/context inventories,
after which the owner transitions to Detaching and unregisters. No manually
activated root scope or raw unleased retirement scan remains in this pipeline.

Generated x86 passed in 17.53 seconds with the unchanged 40-second alarm; its
marker is required in generated and both C sanitizer runs. The snapshot is still
supplied by the fixture and marking is performed by the single registered owner.
This does not establish a running multi-worker allocation scheduler, phase-start
coordination, nonnull-child initialization or ordinary compiler activation.
Helper instructions are unchanged. Logs use the prefix
`/tmp/klassic-allocation-registration-`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and generated x86 with
the new marker. Clippy, strict C warnings, formatting, diff whitespace and
inventory (9,485 sites / 96 files) passed. Logs:
`/tmp/klassic-allocation-registration-{workspace,emitted,clippy}.log`.

### Coordinated mark boundaries around allocation (partial G03/G08/G10/G15)

The registered OS allocation pipeline now starts from idle rather than assigning
ConcurrentMark. For each generation the owner prepares a fresh-heap MarkStart
snapshot, calls shared START, acknowledges it through worker POLL, activates
ConcurrentMark, acknowledges the second rendezvous, then releases/resumes.
ACTIVATE and RELEASE return WAIT before their required acknowledgements. The
fresh heap has no prior colors, Old slots or TLABs requiring retirement.

Persistent capacity-one queues created for those starts now receive every actual
allocation's Young and Old work; per-object temporary queues were removed. After
both scans and leased root/journal retirement complete, shared root audits and
mark-end coordinator operations advance each generation to Select. DECIDE and
RELEASE also wait for their acknowledgements. The registered owner resumes after
all eight rendezvous and detaches only with empty root/context inventories.

Generated x86 passed in 17.87 seconds with its unchanged 40-second alarm and both
required boundary markers. Runtime helper instructions did not change. This is
one registered OS owner executing the coordinator and marker roles sequentially,
not a background-worker scheduler; Select is not a completed relocation/retire
cycle. Repeated color reuse, nonnull-child initialization, concurrent allocation
scheduling and default backend activation remain unfinished. Logs use the prefix
`/tmp/klassic-allocation-mark-phases-`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and emitted x86 with
the new boundary markers. Clippy, strict C warnings, formatting, diff whitespace
and inventory (9,485 sites / 96 files) passed. Logs:
`/tmp/klassic-allocation-mark-phases-{workspace,emitted,clippy}.log`.

### OS worker consumes allocation mark jobs (partial G02/G03/G10/G15)

The OS allocation pipeline now starts a separate registered OS worker after the
coordinated mark starts. The allocation owner publishes and offers all four
objects; the worker takes their eight Young/Old jobs, enters its own source
scope, scans, finishes and releases each transferred metadata lease. Its context
and thread identity come from shared ATTACH and it detaches/joins before root
retirement and final phase rendezvous.

A single-slot release/acquire command protocol serializes queue, lease and
registry ownership; the owner never mutates those controls while the worker owns
the command. A claimed-job handshake checks queue BUSY and owner guard absence
before allowing the scan. The owner performs synthetic progress work outside
guards while handing jobs off. Completion returns control before the next lease
acquisition/allocation. This is bounded backpressure integration, not a workload
throughput measurement or a general multi-mutator scheduler. Phase coordination
still occurs before worker startup and after join; worker polling during changing
phases, nonnull child publication and default activation remain incomplete.

Generated x86 passed in 17.72 seconds with its unchanged 40-second alarm and new
required worker marker. Runtime helper instructions are unchanged. Logs use the
prefix `/tmp/klassic-allocation-worker-`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and generated x86 with
the new worker marker. Clippy, strict C warnings, formatting, diff whitespace
and inventory (9,485 sites / 96 files) passed. Logs:
`/tmp/klassic-allocation-worker-{workspace,emitted,clippy}.log`.

### Live allocation-worker rendezvous (partial G02/G08/G10/G15)

After each page's allocation/mark work completes, the still-registered worker
and allocation owner now participate in a real shared handshake. Owner POLL
first returns CLOSE_TLAB while the TLAB remains open. After close, the coordinator
advances the snapshot sequence, begins the handshake and parks the owner; the
worker adopts and ACKs through its own POLL. The coordinator waits for both ACKs,
ends the handshake, and each participant resumes before the next allocation.
A release/acquire request/completion relay preserves exclusive registry access.

Both rendezvous preserve pending journal publication/consumption positions and
registered roots. Final journal validation now compares each entry with its
allocation-time snapshot, including entries from before the newer adoption.
The worker verifies all four park/resume requests before detach. Together with
mark starts/ends, the pipeline completes ten rendezvous. Generation states and
color epochs remain unchanged during these two added snapshot updates; this is
not epoch-wrap or repeated-cycle validation, nor a handshake during a live scan.

An initial C variable-name collision was corrected. Final generated x86 passed
in 20.43 seconds with the unchanged 40-second alarm and required rendezvous
marker. Helper instructions did not change. Multi-mutator scheduling, nonnull
child initialization and ordinary default activation remain incomplete. Logs
use the prefix `/tmp/klassic-allocation-rendezvous-`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and emitted x86 with
the new rendezvous marker. Clippy, strict C warnings, formatting, diff whitespace
and inventory (9,485 sites / 96 files) passed. Logs:
`/tmp/klassic-allocation-rendezvous-{workspace,emitted-final,clippy}.log`.

### Rooted nonnull initialization children (partial G03/G10/G15)

The two mixed records in the OS allocation pipeline now initialize both reference
fields from the preceding scalar allocation's still-owned root. Writes occur
under the initializer guard before release publication. Their scalar fields
contain deliberately non-reference values and reference-shaped bits, while the
registered descriptor selects exactly the two reference slots.

The OS worker exits the parent guard, retains the unchanged parent claim, acquires
a separate child metadata lease, looks up its actual forwarding cell and routes
under a child guard. Both children were already fully marked in each generation;
eight offers therefore return DUPLICATE even with the capacity-one queue occupied
by the parent. The pending request is preserved, only the child lease is released,
and the parent cursor advances after both handoffs. A two-slot lease pool permits
parent/child protection to coexist; no lease remains at completion.

Generated x86 passed in 17.95 seconds with the unchanged 40-second alarm and new
required child marker. The fixture now covers rooted nonnull initialization and
duplicate-child cleanup, not discovery of an unmarked child, child movement or
allocation with exhausted child metadata. General scheduling and ordinary default
activation remain incomplete. Helper instructions are unchanged. Logs use the
prefix `/tmp/klassic-initialization-children-`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and emitted x86 with
the child marker. Clippy, strict C warnings, formatting, diff whitespace and
inventory (9,485 sites / 96 files) passed. Logs:
`/tmp/klassic-initialization-children-{workspace,emitted,clippy}.log`.

### Newly discovered initialization children (partial G03/G10/G11/G15)

The scalar allocations now remain unmarked in Old after readiness publication;
their registered roots and unconsumed publication journals retain the work. They
are still marked in Young. When the OS worker scans each mixed parent in Old,
its first child offer returns FULL with the parent occupying the sole queue slot.
The shared spill helper transfers the child and its lease into counted overflow;
the repeated child reference then returns DUPLICATE without consuming that work.

After parent FINISH the queue remains BUSY. REFILL moves the deferred child into
the released slot, and the worker looks up/guards/scans/finishes that child before
replying to the owner. Thus six owner commands execute eight actual mark scans,
with two newly discovered Old children and six duplicate offers. Three prepared
lease slots cover the parent, deferred child and temporary duplicate input.
All roots/journals remain protected until the existing dual-mark retirement.

Generated x86 passed in 22.16 seconds with the unchanged 40-second alarm and new
required discovery marker. This covers fresh child discovery under task-slot
saturation, but not recursive arbitrary graphs, child movement, metadata refill
or general multi-mutator scheduling. Ordinary default activation remains open.
Helper instructions are unchanged. Logs use the prefix
`/tmp/klassic-initialization-discovery-`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and emitted x86 with
the discovery marker. Clippy, strict C warnings, formatting, diff whitespace
and inventory (9,485 sites / 96 files) passed. Logs:
`/tmp/klassic-initialization-discovery-{workspace,emitted,clippy}.log`.

### Child metadata pressure preserves pending work (partial G03/G10/G11/G13/G15)

The allocation owner now temporarily holds the third metadata slot during each
Old parent command. The parent claim and first spilled child consume the other
two; acquiring the duplicate child's lease returns FULL without changing its
request. The worker verifies that it holds no source guard and keeps both parent
and overflow work counted, then enters BlockedNative through the shared worker
protocol and yields registry/metadata ownership via the release/acquire relay.

The owner observes the registered native participant, a BUSY queue, unchanged
parent/child roots and all three occupied slots. It releases its own temporary
lease and signals capacity availability. The worker leaves native, reacquires
metadata, and completes the duplicate/parent/deferred-child work normally. Both
forced waits complete with no lost lease or native membership. No managed address
or live source guard is carried through the wait.

Generated x86 passed in 17.92 seconds with the unchanged 40-second alarm and new
required wait marker. This is recovery when another holder releases fixed-pool
capacity, not OS-backed pool growth, exhaustion OOM handling or a general
scheduler. Runtime helper instructions are unchanged; ordinary default activation
remains open. Logs use the prefix `/tmp/klassic-initialization-metadata-wait-`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan and emitted x86 with
the wait marker. Clippy, strict C warnings, formatting, diff whitespace and
inventory (9,485 sites / 96 files) passed. Logs:
`/tmp/klassic-initialization-metadata-wait-{workspace,emitted,clippy}.log`.

### Bounded admission of prepared metadata capacity (partial G10/G11/G15)

`gc_metadata_grow_locked` and the shared emitter admit at most 256 zeroed slots
per call into a stable lease table. Validation completes before any mutation;
existing slot indices, generations and active holders remain unchanged. The
caller must hold the metadata lock and have already reserved, committed and
budgeted the backing. This helper neither allocates backing nor implements an
OS growth policy or exhaustion/OOM recovery.

The OS allocation pipeline initially exposes two of three prepared slots, then
admits the third while its first object lease is live. Subsequent publication,
worker scans and lease release exercise that token after growth. Existing
three-slot pressure tests still exercise the two native capacity waits.

C and generated x86 run 180 complete-state cases covering occupancy, additions
through 256 slots, duplicate admission and invalid/dirty tails. ARM emulation
checks the same 180 independent expected states; the helper-specific emulator
instruction ceiling is 100,000 for its two bounded passes. This is instruction
validation, not Darwin execution. Clippy, strict C warnings and inventory checks
pass. Logs use `/tmp/klassic-metadata-grow-`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan. Formatting and diff
whitespace checks passed; inventory remains 9,485 sites across 96 files.

### Account OS-backed lease storage before publication (partial G10/G11/G15)

The allocation pipeline now reserves metadata budget before obtaining its lease
storage mapping, commits and zeroes the mapping before exposing slots, and charges
the entire mapping including unused capacity and padding. It keeps that charge
through the bounded admission of the third slot, both generations' worker scans,
and full-pool native waits. After joining the worker, draining roots/jobs and
checking zero active leases, it unmaps the storage before releasing its charge.

The C and emitted budget callbacks reject oversized admission unchanged, abort a
reservation after a simulated pre-mapping OS failure, and reject budget release
while the allocation pointer is still present. Actual mapping operations continue
to use the conformance OS adapter: this is not a production growth policy or
proof of recoverable real OS allocation failure. The ordinary compiler still
uses the legacy runtime. Generated x86 integration passed in 22.68 seconds.

The ARM metadata fixture's occupied slot values now follow the actual ABI order
(generation, page, incarnation, next_free); all 180 expectations pass, as do the
27 API scenarios and 1,536 shared-context transitions. Logs use the prefix
`/tmp/klassic-metadata-backing-`.

Final validation: workspace 795 tests / 25 invocations passed, no failures or
ignored tests, including mandatory C ASan+UBSan/TSan. The emitted run with
the required backing marker passed in 20.23 seconds. Clippy, strict C warnings,
formatting, diff whitespace and inventory (9,485 sites / 96 files) passed.

### Reusable native metadata backing adapter (partial G10/G11/G15)

`runtime/gc/gc_native_metadata.h` now owns the native backing operations rather
than embedding them solely in the pipeline fixture. It rounds requested bytes
before budget reservation, maps anonymous zeroed storage via POSIX mmap or
Windows VirtualAlloc, and clears the charge's allocation only after successful
unmap/release. The caller still owns reserve/commit/abort/release serialization,
blocked-state transitions, and the proof that no consumer remains at disposal.
OS calls must run outside heap locks and source guards.

Injected map failure preserves the pending charge for explicit abort. Failed
cleanup of either pending or committed storage preserves both the mapping and
charge; abort/release remain rejected until cleanup succeeds. C and generated
x86 budget callbacks exercise those paths with real Linux mappings, including
zeroed backing, dirty endpoints surviving failed cleanup, retry and accounting
returning to baseline. The OS allocation pipeline uses this adapter for its live
lease table. Emitted integration passed in 21.81 seconds with a required marker.

This implements a C OS adapter and its lifecycle contract, not direct emitted OS
allocation, a production metadata growth policy, or standard compiler activation.
Windows code is present but actual Windows execution remains unverified. Logs:
`/tmp/klassic-native-metadata-{emitted,workspace,clippy}.log`.

Final workspace validation: 795 tests / 25 invocations, zero failures or ignored
tests, including mandatory C ASan+UBSan/TSan. Strict C warnings, Clippy, formatting,
diff whitespace and inventory (9,485 sites / 96 files) passed.

### Direct Linux metadata syscalls (partial G10/G11/G15)

`gc_codegen/native_metadata.rs` emits Linux x86-64 mmap/munmap leaf routines with
no libc or external linker dependency. They implement the NativeMetadataOps
callback ABI, validate nonzero page-multiple sizes/aligned release addresses,
and translate kernel errors to NULL/false. They use only caller-saved registers.
The owner retains the separate budget, native-boundary and lifetime obligations.

The emitted harness now supplies these callbacks both to the backing lifecycle
failure tests and to the OS allocation/mark pipeline's live lease table. It checks
real mmap and munmap errno paths as well as injected map/cleanup failures. The
mapping stays live through slot admission, Young/Old scans and native capacity
waits, and is released after worker retirement. The initial emitted lifecycle
run passed in 17.97 seconds; full integration validation is recorded below.

Only Linux x86-64 has these direct OS callbacks so far. C adapters remain the
fallback in the other conformance harnesses; this does not claim standard
compiler activation, direct Windows/macOS metadata mapping or OS parity. Logs:
`/tmp/klassic-emitted-metadata-{emitted,workspace,clippy}.log`.

Final integration validation passed: workspace 795 tests / 25 invocations,
zero failures or ignored tests, including required C ASan+UBSan/TSan and the
emitted pipeline with its mandatory direct-syscall marker. Clippy, strict C
warnings, formatting, diff whitespace and inventory (9,485 / 96 files) passed.

### Win64 metadata backing entry points (partial G10/G11/G15)

The native metadata emitter now also provides Win64 map/unmap entry points.
Mapping calls VirtualAlloc with reserve+commit and read/write protection;
release passes size zero and MEM_RELEASE to VirtualFree. Both functions validate
page-aligned arguments and reserve aligned shadow space. Release inspects only
the defined 32-bit BOOL result, then normalizes every nonzero value to true.
VirtualFree is now in the PE import table; relocation tests inspect actual IAT
slots and the CLI image test's expected import list includes it.

The Windows worker harness executes these bytes against native kernel32 when
run on Windows, or an explicitly labeled POSIX API model on Linux. The model
checks arguments, zeroed storage, allocation failure and cleanup failure/retry;
it deliberately returns a dirty high half and a true BOOL with a zero low byte.
All four focused Windows tests passed on Linux. This is not evidence of Windows
kernel execution. The full allocation/mark harness on Windows still uses its C
metadata OS callbacks; connecting these emitted callbacks there remains open.

Initial workspace validation caught the outdated CLI import expectation; that
expectation was updated for the new required import. Logs use the prefix
`/tmp/klassic-win-metadata-`; final workspace results are recorded below.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including mandatory C ASan+UBSan/TSan, emitted x86 and PE
import/relocation checks. Clippy, formatting, diff whitespace and inventory
(9,485 sites / 96 files) passed. Final log: `/tmp/klassic-win-metadata-workspace-final.log`.

### Darwin ARM64 metadata backing entry points (partial G10/G11/G15)

The Darwin emitter now maps and releases metadata through eagerly bound
libSystem mmap/munmap imports. It validates Apple Silicon's 16 KiB granularity,
passes anonymous private read/write mapping arguments, converts pointer-width
MAP_FAILED to NULL, and converts the defined 32-bit munmap result to bool. The
entry points preserve callee-saved registers and aligned stack frames. The
Mach-O fixture now exports 86 helpers and binds 16 OS imports.

The native fixture obtains a mapping before worker startup, checks zeroed
storage, writes a sentinel, and verifies it after joining workers before release.
The ARM instruction emulator validates invalid arguments, mapping failure,
zeroed successful mapping, failed release with retained contents and retry. It
also checks the ABI under volatile-register clobbering and dirty upper int-return
bits. New backing cases and existing 180 metadata admission cases pass, alongside
27 API-model scenarios / 356 helper calls and 1,536 shared-context transitions.

This is modeled OS execution on Linux, not native Darwin execution or hardware
memory-ordering proof. The full Darwin allocation/mark owner and standard compiler
still need integration. The first fixture attempt exposed an outdated fixed
import count; it now checks the two added imports and the correct count. Logs:
`/tmp/klassic-arm-metadata-{export,emulation,workspace,clippy}.log`.

Final workspace validation passed: 795 tests / 25 invocations, zero failures
or ignored tests, including required C ASan+UBSan/TSan, emitted x86 and native
image construction checks. Clippy, formatting and whitespace checks passed;
updated access inventory records 9,507 candidate sites across 96 files.

### Wire emitted Windows metadata into the allocation owner (partial G10/G15)

The full emitted allocation/mark harness on Windows now uses the Win64 metadata
entry points for its live lease mapping. The generator packages the two entries
with a local import table, resolves internal relative references, and exposes
explicit import/entry offsets. The owner installs kernel32 pointers before
making the code executable and keeps it loaded through failure/retry tests,
lease-table growth, worker scans and retirement. It unloads the code only after
the backing has been released. A Windows-specific mandatory marker covers this
path when run on Windows.

The Win64 API-model harness now executes that exact locally relocated payload,
including binding imports before executable protection. Zero backing,
allocation failure, failed cleanup with retained dirty data, retry and BOOL
normalization pass with the new required payload marker. Linux emitted regression
passed in 18.57 seconds; the focused Win64 model passed in 0.56 seconds. This
checks the payload on Linux; actual Windows execution of the full pipeline and
standard compiler activation remain outstanding. Logs use the prefix
`/tmp/klassic-win-metadata-integration-`.

Final validation passed: workspace 795 tests / 25 invocations, no failures or
ignored tests, including mandatory C sanitizers. The focused Win64 payload test
with its required marker also passed. Clippy, formatting, diff whitespace and
access inventory (9,507 sites / 96 files) passed.

### Wire emitted Darwin metadata into the allocation owner (partial G10/G15)

The full AArch64 Darwin emitted harness now binds mmap/munmap addresses into a
local metadata payload before making it executable, then uses those generated
entries for the live lease table. It retains the payload until the mapping has
been released after worker retirement. The Darwin-specific completion marker is
required when running that target.

The payload generator replaces each import-address ADRP/ADD pair with ADR/NOP
addressing a local pointer slot. It checks import kinds and displacement bounds,
so no runtime page-address relocation is needed. The Mach-O export additionally
writes the exact payload and entry/import offsets used by the allocation harness.
The ARM emulator binds those bytes at a non-page-aligned address crossing a page
boundary and repeats zero-backing, invalid input, mapping failure, retained
contents after failed unmap and retry checks. Both the Mach-O helper entries and
local payload pass, alongside 180 metadata growth cases, 27 API-model scenarios /
368 helper calls, and 1,536 shared-context transitions.

These are Linux-hosted instruction/OS-model checks. Actual Darwin execution of
the full owner and standard compiler activation remain unverified/incomplete.
Logs use `/tmp/klassic-arm-metadata-local-`; exported image and payload are under
`/tmp/klassic-arm-metadata-local-fixture`.

Final validation: workspace 795 tests / 25 invocations passed, no failures or
ignored tests, including mandatory C ASan+UBSan/TSan and emitted x86/Win64 model
checks. Clippy, formatting, diff whitespace and inventory (9,509 sites across
96 files) passed. Native Darwin execution remains outstanding.

### Core allocation descriptors (partial G03/G10/G15)

`gc_builtin_descriptors.h` prepares stable immutable definitions for raw bytes,
packed pointer records and scalar-length-prefixed pointer lists. Registry IDs
1, 2 and 4 are reserved; descriptor IDs do not make the legacy/new reference ABIs
interchangeable. The owner prepares the package at its final address and retains
it through every registry/heap user; its reference bitmap is owned by the package.

C and emitted registration/scanning tests cover duplicate registration, zero
through 1,024 elements, bounded seven-slot scan chunks, exact reference offsets,
and invalid payload-size rejection without changing requests/output. The list
length is never scanned as a reference. The OS allocation/mark pipeline now uses
the common raw-byte definition, including page-sized and zero-byte payloads.
Ordinary compiler startup and mixed compiler layouts still need integration.
Logs use `/tmp/klassic-builtin-descriptors-`.

Validation passed: emitted x86 in 21.65 seconds; workspace 795 tests / 25
invocations, no failures or ignored tests, including mandatory C ASan+UBSan/TSan.
Strict C warnings, Clippy, formatting, whitespace and inventory (9,509 / 96 files)
also passed. Ordinary compiler startup remains unconnected.

### Emit builtin descriptor preparation (partial G10/G15)

The shared descriptor emitter now prepares the complete 200-byte builtin package
without C calls, allocation or polling. It writes each field and points the two
reference-element bitmaps at the package's own stable bitmap word. C/Rust layout
checks cover the package size and descriptor positions; fresh writable storage
at its final address remains a caller precondition.

The emitted harness uses this entry both before builtin registration/scanning
and in the OS allocation/mark pipeline. A complete-state comparison with the C
preparer covers every field before descriptors become visible. ARM emulation
checks all 200 bytes at three eight-byte-aligned locations with surrounding
canaries; all pass. The Mach-O fixture exports 87 helpers. Existing ARM metadata
and OS-model cases still pass (27 scenarios / 371 helper calls / 1,536 context
transitions). This supplies direct preparation, not standard compiler startup.
Logs use `/tmp/klassic-builtin-prepare-`.

Final validation passed: workspace 795 tests / 25 invocations, zero failures
or ignored tests, including mandatory C ASan+UBSan/TSan and emitted x86
preparation/registration/scanning. Strict C warnings, Clippy, formatting,
whitespace and inventory (9,512 sites / 96 files) passed.

### Connect compiler shape selection to descriptor definitions (partial G10/G15)

`gc_shape::BuiltinShape` now supplies both explicit legacy allocation tags and
new builtin descriptor templates. Shared allocation emission accepts a typed
shape; actual AArch64 record/cons/list-reversal construction and scalar boxing use
it. The descriptor preparer consumes the same definitions, with existing
C-versus-emitted full-package checks covering their representation.

The source audit also distinguishes AArch64's boxed all-reference heap records
from x86's StaticRecord/RuntimeRecord-label lowering. No common raw layout is
assumed across these paths. This is a compiler-to-descriptor shape connection,
not conversion of legacy pointers/headers or activation of generational roots.
Logs use `/tmp/klassic-compiler-shapes-`.

Validation passed: workspace 795 tests / 25 invocations, zero failures or ignored
tests, including mandatory C sanitizers and generated-code descriptor parity.
Clippy, formatting, whitespace and inventory (9,512 sites / 96 files) passed.

### Private budgeted descriptor-registry bootstrap (partial G10/G11/G15)

`gc_native_builtin_registry.h` prepares a registry, builtin definitions and
atomic slot table in one budgeted native mapping. Capacity arithmetic and page
rounding precede allocation. After reserving the charge, the owner prepares
outside locks/source guards; only successful preparation plus budget commit
permits publication. Allocation failure or failure at any of the three builtin
registrations leaves the charge/private storage available for cleanup and abort.

The OS allocation/mark pipeline now uses this mapping rather than stack-backed
registry slots and builtin definitions. Its registered owner enters native state
around preparation, commits the charge before starting its marker, and allocates
through the actual registered raw descriptor. The mapping outlives all mark
jobs, root retirement and managed-heap teardown; disposal precedes budget release.
The separate mixed test descriptor remains owner-controlled native storage.

C and emitted callback tests inject mapping failure and each registration failure,
check the exact partially populated slots and unchanged budget ownership, then
clean the private mapping before aborting. Success commits and later releases
back to baseline. This is reusable C owner orchestration with generated leaf/OS
callbacks; ordinary compiler startup and production scheduling remain incomplete.
Initial emitted integration passed in 17.32 seconds. Logs use
`/tmp/klassic-descriptor-bootstrap-`.

Final validation passed: workspace 795 tests / 25 invocations, no failures or
ignored tests, including mandatory C ASan+UBSan/TSan. The final emitted run using
the registered descriptor directly passed in 19.87 seconds. Strict C warnings,
Clippy, formatting, whitespace and inventory (9,512 sites / 96 files) passed.

### Resolve allocation shape by descriptor ID (partial G10/G15)

`AllocationShape` and `gc_allocation_shape` bridge a registry ID plus logical
payload bytes to an AllocationRequest's descriptor/elements fields. The C and
shared emitters acquire the immutable registry slot, check ID/range/alignment,
and require an exact fixed-prefix/repeated-element size match. No heap access,
allocation, root publication or poll occurs; page/output/mark-arena inputs are
preserved, and rejection leaves the complete request unchanged. Payload shape
validation is distinct from the allocator's header/alignment/space checks.

The OS owner now resolves both raw and mixed fixed-size allocations this way
before root reservation and AllocationInto. C and generated-code tests cover
62 valid/invalid ID and logical-size cases including zero, partial elements and
maximum offset boundaries. ARM emulation independently checks complete memory
states for those 62 cases. ABI layout validation passes; the Mach-O fixture now
exports 88 helpers. Existing ARM checks pass (27 scenarios / 433 helper calls /
1,536 context transitions). Standard compiler call lowering remains incomplete.
Logs use `/tmp/klassic-allocation-shape-`.

Final validation passed: workspace 795 tests / 25 invocations, no failures or
ignored tests, including mandatory C ASan+UBSan/TSan and emitted x86 shape
resolution and allocation handoff. Clippy, strict C warnings, formatting,
whitespace and inventory (9,520 sites / 96 files) passed.
