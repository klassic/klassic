# Concurrent ZGC for klassic (the "true ZGC" plan)

Status: **M0 (this document).** Owner decision (2026-07-09): go all the way
to a *truly concurrent* collector — a background GC thread that marks and
relocates while the compiled program keeps running, with O(roots) pauses
only at phase-transition handshakes. This is the execution model that makes
ZGC ZGC, and it is what the earlier work (the merged C collector,
`docs/llvm-backend-plan.md` M6) deliberately stopped short of.

Read `docs/zgc-plan.md` first: it is the algorithm spec, and its
"Thread-readiness" table (the simplification → MT-upgrade map) is the
skeleton of this plan.

## Where we are

The merged C collector (`runtime/gc/klassic_gc.c`) is a **single-threaded,
non-atomic** region collector with the ZGC *mechanisms* in place — colored
pointers, a self-healing load barrier, incremental marking driven in quanta
from `gc_alloc`, and compaction with in-object forwarding — but two gaps
versus true ZGC:

1. **Relocation is stop-the-world.** `gc_relocate` runs synchronously inside
   `gc_mark_end`; there is no `Relocate` phase, and the `R` color exists only
   in `gc_bad_mask` (it is never installed on a pointer). Pauses are therefore
   O(relocation-set), not O(roots).
2. **No concurrency.** One mutator, no GC thread, no atomics, no safepoints.
   The LLVM backend emits no safepoint polls.

The LLVM backend (`docs/llvm-backend-plan.md`, M7) drives this collector for
heap records: `klassic_gc_alloc`, shadow-stack precise roots, color-on-store,
and the inline load-barrier fast path are all emitted. klassic compiled
programs are single-threaded **today**, but the owner is making klassic
multi-threaded soon (directive, 2026-07-09), so this collector must be designed
for **N mutator threads** from the start — not the one-mutator shortcut. The
concurrency is therefore *many mutator threads + a background GC thread (or a
small GC pool)*: full multi-mutator ZGC. We follow the multi-thread upgrade
column of `docs/zgc-plan.md`'s thread-readiness table wholesale rather than
taking any single-mutator simplification we would have to unwind later.

## Target design

Many **mutator threads** and a background **GC thread** share the heap. Each
mutator registers itself in a **thread table** (its shadow stack, its active
TLAB region, its mask cache) on start and deregisters on exit. The load barrier
keeps every mutator's view consistent as the GC thread marks and relocates
underneath them. Phase transitions that must observe a consistent root set
(MarkStart, MarkEnd, RelocateStart) use a **handshake with all mutators**: the
GC thread raises the global request flag and waits until every registered
mutator has, at its next safepoint poll, run the handshake closure (scan *its
own* shadow stack into the worklist, reload its mask cache) and acked. A mutator
scanning its own stack avoids a cross-thread stack race. A blocked/allocating
mutator runs the handshake at its safepoint before parking. Everything else —
marking, tracing, evacuating — runs on the GC thread concurrently with all
mutators.

Correctness rests on the invariants ZGC already relies on, made thread-safe for
**multiple mutators** per the upgrade table in `docs/zgc-plan.md` (we take the
full multi-thread column, no single-mutator shortcuts):

- **Self-heal store → CAS** on the field (`lock cmpxchg`); accept-winner (two
  mutators, or a mutator and the GC thread, may heal the same slot).
- **Forwarding install → CAS** (first toucher wins; losers free their copy and
  read the winner's to-space address). Any mutator or the GC thread may be the
  first toucher.
- **Mark bit set → atomic OR** on `word0` (concurrent markers race to set it).
- **Masks** (`gc_good_color` / `gc_bad_mask` / `gc_strip_mask`) stay the
  `.data` source of truth; each mutator reloads its cached view only at a
  handshake (this is *why* they are memory cells, per the M2 spike).
- **Worklist** → per-mutator + GC-thread striped deques with work-stealing, or
  a single lock to start; overflow policy unchanged.
- **Regions** → per-mutator active TLAB regions (the region design is already
  TLAB-shaped); region acquisition and state transitions (FREE↔ACTIVE) take a
  lock or CAS on the region table.
- **Shadow stacks** → one per mutator, registered in the thread table; a
  handshake has each mutator walk its own.

## Milestones (each = one PR; ASan throughout, TSan from M4)

Discipline per PR (same as M6/M7): branch from main → `cargo fmt --check` +
`clippy -D warnings` + full `cargo test` green → the C collector's ASan/UBSan
suite green (`runtime/gc/run_tests.sh`, ASLR-pinned) → independent soundness
review of the concurrency-critical code → PR → CI green → squash merge →
verify on main. The evaluator stays the differential oracle for the LLVM path.

- **M1 — Restore incremental relocation (single thread).** Bring back the
  `Relocate` phase and the R-color scheme the C rewrite dropped: Idle/Relocate
  `good = R`, Mark `good = Mn`; the load-barrier slow path becomes
  phase-dependent (Idle: follow forwarding through ghost regions + recolor;
  Mark: mark + recolor; Relocate: evacuate-on-demand + follow forwarding +
  recolor to R); `gc_relocate` splits into `gc_relocate_start` (select set +
  fix roots + flip to R) and relocate quanta driven from `gc_alloc`
  (`gc_evacuate` copies up to a byte budget, installs forwarding); ghost
  (fully-evacuated) regions are freed at the **next** MarkEnd. Still one
  thread, still non-atomic — but every operation the multi-thread column will
  upgrade (self-heal store, forwarding install, mark-bit set, mask flip,
  worklist push/pop, region acquire) stays confined to a single named routine,
  so M2 can atomicize each in one place without chasing call sites (the
  thread-readiness directive). ASan tests: survivor identity across an
  *incremental* move, pointer-identity, no UAF, and that a load during Relocate
  evacuates-on-demand. This is the algorithmic core the GC thread will run.
- **M2 — Make the shared state thread-safe (still single-driver).** Convert
  the per-table entries from the upgrade map: atomic mark OR, CAS forwarding
  install (copy-then-CAS, free the loser), CAS self-heal, atomic phase/region
  state, a locked worklist. No GC thread yet — run the existing single-thread
  driver through the atomic paths and prove ASan/UBSan still green (atomics are
  a no-op correctness-wise without a second thread, but the code paths are now
  the real ones).
- **M3 — Thread table + safepoint polls in the LLVM codegen.** A C thread
  table registers each mutator (shadow stack base/top pointer, mask cache,
  active TLAB) on thread start and deregisters on exit (single-threaded programs
  register exactly one). The emitter emits a poll at loop back-edges and
  function entries: load the global `gc_handshake_requested` flag; if set,
  `call klassic_gc_handshake()`, which scans *this thread's* shadow stack into
  the worklist, reloads *this thread's* mask cache, and acks. With no GC thread
  the flag is never set, so polls are no-ops — differential oracle
  (eval == --backend llvm) must stay green and the hot-path cost must be a
  single predictable-not-taken branch. (Elide the poll only where provably no
  allocation/handshake can occur; otherwise poll on every back-edge.)
- **M4 — The GC thread + all-mutator handshake protocol.** `klassic_gc_init`
  spawns a background GC thread. Its loop: block until allocation pressure
  crosses the trigger → request the MarkStart handshake and **wait for every
  registered mutator to ack** (each scans its own roots) → mark concurrently
  (GC thread drains the worklist while all mutators' barriers push) → MarkEnd
  handshake (re-scan every mutator to fixpoint) → sweep → RelocateStart
  handshake (fix every mutator's roots, flip to R) → relocate concurrently (GC
  thread evacuates while every mutator's barrier evacuates-on-demand) → Idle.
  `gc_alloc` no longer runs quanta; it bumps in the calling thread's TLAB and
  blocks (fallback) only if the heap is exhausted mid-cycle. First TSan run
  here — with a genuinely multi-threaded C test harness (several pthreads
  churning while the GC thread cycles), since the mutator threads are what the
  concurrency protocol must handle.
- **M5 — Concurrency correctness: TSan + multi-thread stress.** ThreadSanitizer-
  clean under **several mutator threads** churning records while the GC thread
  relocates. Nail the races: two mutators (or a mutator and the GC thread)
  loading/healing the same field, both racing to install a forwarding word (CAS,
  first toucher wins), a handshake landing inside `gc_alloc`, TLAB/region
  acquisition contention, thread register/deregister vs an in-flight handshake.
  `gc_stress` (tiny trigger) run under TSan and ASan with N mutator pthreads.
- **M6 — Pauses + observability.** Confirm pauses are O(roots) all-mutator
  handshakes (measure with the clock shim); `--gc-log` reports collections,
  bytes, and max/total handshake pause at exit. Tune the trigger and
  backpressure so mutators rarely block.

## Risks and honest caveats

- **Codegen safepoints are invasive.** The LLVM backend is young (records just
  landed). Emitting correct polls at every back-edge/call, and a handshake that
  scans the shadow stack consistently, is the highest-risk work. It is gated
  behind M3's differential-oracle green bar.
- **Value grows with multi-threading.** Once klassic is multi-threaded, a
  concurrent collector is clearly worth it: a stop-the-world collector would
  freeze *every* mutator for the whole mark+relocate, whereas the concurrent
  design pauses each thread only for an O(roots) handshake. The design targets
  N mutators from the start precisely so the collector is ready when the
  language is. M6 still measures the hot-path poll cost so the trigger and
  poll placement can be tuned.
- **TSan is mandatory from M4.** Data races in a concurrent collector are
  silent and layout-sensitive (cf. the M6c ASan/ASLR heisenbug); TSan is the
  only reliable net, and the C collector being in C is what makes it usable.
