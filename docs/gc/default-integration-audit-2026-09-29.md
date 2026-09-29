# Default collector integration audit — 2026-09-29

The default compiler/runtime still uses the legacy collector. Direct OS payload
cleanup is now emitted for all three targets, but its conformance execution does
not activate the offset-reference collector in ordinary programs.

This audit follows the actual call sites after the native decommit revision. It
retains the full G01–G15 target and identifies the production connection that must
accompany further component work.

## Current boundaries

`gc_native_buffer_incoming.h` now composes per-context handoffs into a bounded
registered-buffer absence audit, mandatory in `NativeRunRetireCheck`. A saved
context address is borrowed only after monotonic identity/member snapshots
exclude removal/reuse. It supplies no retained context lease and relies on the
same no-new-target-reference exclusion as the other incoming observers. General
heap/handle/forwarding proofs and default compiler integration remain open.

`gc_native_buffer_quiescence.h` checks one stopped registered context's two
rings without reading a Running owner's private top or an active consumer's
COPY-owned cursor. It retains no context pointer or receipt. The bounded inventory observer above now composes it with source retirement;
this component does not switch the ordinary collector.

`gc_native_root_incoming.h` now supplies a mandatory root-domain input to the
owned source-retirement receipt. It scans one atomic cell per step, retains a
reader lease across segment traversal and waits for journal/deletion/reader
obligations. It does not mark roots or enforce the global no-new-target-reference
protocol; remaining domains and ordinary collector integration stay open.

The source-retirement receipt now composes scan-control and both-generation
queue audits. `gc_native_queue_incoming.h` protects overflow cursors with real
metadata leases and detects serial-preserving REFILL through overflow-count
changes. This adds queue evidence to the gate; it does not supply the remaining
retirement proofs, capacity progress, workers or default compiler migration.

Owned source metadata retirement now requires a completed `NativeRunRetireCheck`
bound to that run/incarnation, current phase and scan inventory. Existing owned
allocation/retirement fixtures use it before transferring metadata to SourceRetire.
The gate enforces the incoming domains listed above; payload uncommit and
remaining retirement domains retain separate whole-cycle proof requirements.

Mark-scan controls now enter a runtime inventory automatically and require
terminal unlink before storage reuse. `gc_native_scan_incoming.h` checks one
control per call and invalidates cursors on inventory changes. Worker ownership
handoff must serialize inspection with control updates. The owned retirement
receipt now consumes this input alongside queue, root and buffer observations.

`gc_native_mark_scan.h` now retains a runtime queue claim and bounded child
buffer through guarded scanning, child handoff and final lease release. It
replaces the graph fixture's manual scan ownership, including interruption and
resource-shortage paths. Worker-provisioned controls, their source-retirement
discovery and capacity/scheduling services still need production integration.

`gc_native_root_mark.h` binds counted root-audit items to the runtime queue and
pending-reference protocol, preserving items through cancellation and retries.
A nonempty graph fixture now reaches bounded queue/spill scanning, mark-end
retry, deletion-journal handoff and owned retirement. It explicitly proves death
of its single run after dropping all external roots; a production graph proof
and moving collector are still required. Ordinary compilation is unchanged.

`gc_native_cycle.h` now connects bootstrap-owned requests, queues and remembered
epochs to shared phase controls. An empty-graph fixture drives worker adoption,
overlapping mark lanes, stale retirement audits and final ticket completion.
The controller requires external complete graph/retirement proofs; production
graph service, worker scheduling and direct controller emission remain open.

Subsequent bootstrap work adds `gc_native_runtime.h`: a C owner composes heap
reservation, initial thread registration, root/descriptor preparation and paired
budget commit. Roots become visible only at explicit activation. Disposal retains
failed OS releases for retry and requires completed root retirement and external
quiescence. This is exercised in lifecycle fixtures; ordinary startup still does
not call it. Buffers, mark/pin inventories, worker launch and compiler migration
remain required before default activation.

`gc_native_allocation.h` now adds a stable per-runtime run inventory and connects
that bootstrap to real OS-backed Young TLABs. Admission charges rounded bitmap,
forward-table and mark-record storage; native preparation precedes publication
and binding. Its allocation entry uses the startup descriptor registry and
registered root segment with the existing shared allocation leaf. Pressure
statuses are returned without hiding retries or discarding existing roots.
Never-bound preparation can be cancelled with retryable native cleanup; bound
and closed runs cannot take that shortcut. Logical mark records remain retained
independently of physical page lifetime. This C controller and its C/generated
allocation tests are still not ordinary compiler/runtime startup, a scheduler,
root growth, or complete installed-run retirement.

Physical ownership now connects onward through `gc_native_allocation_retire.h`.
After evacuation, payload uncommit and the caller's complete source retirement
proof, the run's embedded SourceRetire request owns bitmap/table cleanup and
bounded range release. Shared logical records and their runtime inventory node
remain retained after source range reuse. The controller does not establish the
global retirement proof. `gc_native_records.h` now connects logical reclamation:
owned copy installation records arena imports, physical retirement returns them,
and only a physically retired arena with zero imported copies can be sealed,
unmapped and unlinked. Record borrowers must retain a physical-copy lease;
queued, claimed and completed-but-unreleased work therefore blocks the final
copy's retirement. The wholly-dead run seal requires an independent complete
range-absence proof, not merely mark bits or empty queues. These ownership
controllers execute allocation-through-shutdown fixtures; production proof
services, worker routing, generated controllers and default call sites remain
outstanding.

The initial mutator's buffers, metadata lease pool and both generation task
queues now come from `gc_native_work.h`, a single rounded/budgeted native startup
package. Admission precedes package mapping; free lists are initialized outside
locks before publication. Initial binding never replaces live buffer storage.
Runtime disposal rejects buffer/consumer work, active leases and nonempty queues
before native cleanup. Existing owned allocation/record-lifetime fixtures use
these mapped pools/queues rather than stack stand-ins. This still supplies no
automatic buffer processing, remset/pin inventory, OS worker launch, concurrent
cycle scheduler or default compiler routing.

| Boundary | Current source evidence | Required connected behavior |
| --- | --- | --- |
| Shared allocation lowering | `portable_asm.rs::emit_gc_alloc_dynamic` passes `shape.legacy_tag()` and calls the legacy allocation label; `emit_gc_alloc_object` documents a 16-byte header and user pointer | Supply attached context, descriptor registry, reserved output root and allocation request; initialize and publish before exposing the value |
| x86 allocation call site | `lib.rs::compile_gc_alloc` returns `NativeValue::HeapPointer` through that shared entry | Preserve a rooted GcRef across every slow-path poll, growth or collection, including argument staging and return paths |
| Shared root lowering | `portable_asm.rs::emit_gc_shadow_push_runtime` stores the address of a mutable stack slot; pop decrements the shadow-stack top | Nonmoving registered RootHandle cells, journaled writes, scope retirement and delayed backing disposal |
| C runtime startup/allocation | `klassic_gc.c::gc_init_once` reserves the legacy heap; `klassic_gc_alloc` writes two header words and returns `block + 16` | Runtime-owned HeapPool, descriptors, contexts, root backing, metadata inventories and worker/cycle services using the shared ABI |
| LLVM subset | `llvm.rs` emits `call ptr @klassic_gc_alloc` and `klassic_gc_shadow_push(ptr)` | The same new rooted allocation/publication and native-boundary ABI as direct backends |
| New allocation component | `gc_alloc_start.h::gc_try_allocate_start` reserves an empty handle and returns a rooted INITIALIZING object; failure cancels the empty cell | A real caller that handles ROOT_FULL/TLAB/budget/service results and retains live arguments while retrying |
| Native backing and cleanup | `gc_native_heap.h`, `gc_native_bitmap.h`, shared source-uncommit controller and direct OS callbacks operate in lifecycle fixtures | Stable runtime ownership through allocation, source retirement, failed OS retry and shutdown; no stack-fixture owner substitution |
| CLI and end-to-end gates | `src/cli.rs` has logging/stress flags but no generational selector; planned `gc_generational.rs`, `gc_differential.rs`, `gc_latency.rs` are absent | Default generational execution and explicit selector, full behavior/failure matrix, real OS and dedicated performance evidence |

## Integration consequence

Changing only the allocator target would mix two incompatible representations:
legacy raw/colored absolute user pointers and stack-slot roots with the shared
32-byte header, offset references and stable handles. The compiler's allocation,
root lifetime, initialization/publication and access barriers have to cross that
boundary together. Do not advertise component execution or legacy CLI stress
success as the new default collector.

The next production connection needs an owning startup/runtime instance that
provides the shared context, descriptor/root registries and heap to the compiler
allocation boundary. It must keep those owners alive through asynchronous cleanup
and supply the existing slow-path services before allocation lowering can use
`gc_try_allocate_start`. This is followed by root/value/access lowering and the
full cycle scheduler, not by replacing the old heap address in place.

Existing native startup/root publication components already exercise explicit
worker attach/native transitions and registered backing. They are reusable
building blocks; searches of `klassic_gc.c`, `lib.rs` and `aarch64.rs` found no
ordinary calls to `gc_try_allocate_start`, `gc_native_heap` or `native_startup`.
These missing production connections remain open work, not a user-input blocker.
