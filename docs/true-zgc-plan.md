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
programs are **single-threaded**, so the concurrency here is exactly *one
mutator thread + one background GC thread* — simpler than multi-mutator ZGC
(a single rendezvous partner), but genuinely concurrent.

## Target design

Two threads share the heap: the **mutator** (the compiled program) and one
**GC thread**. The load barrier keeps the mutator's view consistent as the GC
thread marks and relocates underneath it. Phase transitions that must observe
a consistent root set (MarkStart, MarkEnd, RelocateStart) use a **handshake**:
the GC thread raises a flag; the mutator, at its next safepoint poll, runs a
small handshake closure (scan its shadow-stack roots into the worklist, reload
its mask view) and acks; the GC thread proceeds. Everything else — marking,
tracing, evacuating — runs on the GC thread concurrently with the mutator.

Correctness rests on the invariants ZGC already relies on, now made
thread-safe per the upgrade table in `docs/zgc-plan.md`:

- **Self-heal store → CAS** on the field (`lock cmpxchg`); accept-winner.
- **Forwarding install → CAS** (first toucher wins; losers read the winner's
  to-space address). Copy-then-CAS; a lost race frees the loser's copy.
- **Mark bit set → atomic OR** on `word0`.
- **Masks** (`gc_good_color` / `gc_bad_mask` / `gc_strip_mask`) stay the
  `.data` source of truth; the mutator reloads its cached view only at a
  handshake (this is *why* they are memory cells, per the M2 spike).
- **Worklist** → a lock (single mutator + single GC thread makes contention
  low) or a striped lock-free deque; overflow policy unchanged.
- **Region table / bump** → atomics on region state transitions; region
  acquisition takes a lock/CAS. One mutator keeps this light.

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
  thread, still non-atomic. ASan tests: survivor identity across an
  *incremental* move, pointer-identity, no UAF, and that a load during Relocate
  evacuates-on-demand. This is the algorithmic core the GC thread will run.
- **M2 — Make the shared state thread-safe (still single-driver).** Convert
  the per-table entries from the upgrade map: atomic mark OR, CAS forwarding
  install (copy-then-CAS, free the loser), CAS self-heal, atomic phase/region
  state, a locked worklist. No GC thread yet — run the existing single-thread
  driver through the atomic paths and prove ASan/UBSan still green (atomics are
  a no-op correctness-wise without a second thread, but the code paths are now
  the real ones).
- **M3 — Safepoint polls in the LLVM codegen.** The emitter emits a poll at
  loop back-edges and function entries: load a global `gc_handshake_requested`
  flag; if set, `call klassic_gc_handshake()`. The handshake routine (in C)
  scans the mutator's shadow stack into the worklist and reloads the mutator's
  mask cache, then acks. With no GC thread the flag is never set, so polls are
  no-ops — differential oracle (eval == --backend llvm) must stay green and the
  hot-path cost must be a single predictable-not-taken branch. (Keep the poll
  out of tight leaf loops where provably no allocation/handshake can occur, if
  cheap to prove; otherwise poll on every back-edge.)
- **M4 — The GC thread + handshake protocol.** `klassic_gc_init` spawns a
  background GC thread. Its loop: block until allocation pressure crosses the
  trigger → request the MarkStart handshake (mutator scans roots) → mark
  concurrently (GC thread drains the worklist while mutator barriers push) →
  MarkEnd handshake (re-scan to fixpoint) → sweep → RelocateStart handshake
  (fix mutator roots, flip to R) → relocate concurrently (GC thread evacuates
  while the mutator's barrier evacuates-on-demand) → Idle. `gc_alloc` no longer
  runs quanta; it just bumps, and blocks (STW fallback) only if the heap is
  exhausted mid-cycle. First TSan run here.
- **M5 — Concurrency correctness: TSan + stress.** ThreadSanitizer-clean under
  a mutator churning records while the GC thread relocates. Nail the races:
  mutator load of a field the GC thread is mid-evacuating (CAS forwarding
  resolves it), self-heal contention, handshake landing inside `gc_alloc`,
  region acquisition vs sweep. Add `gc_stress`-style tests (tiny trigger so the
  GC thread cycles constantly) run under TSan and ASan.
- **M6 — Pauses + observability.** Confirm pauses are O(roots) handshakes
  (measure with the clock shim); `--gc-log` reports collections, bytes, and
  max/total handshake pause at exit. Tune the trigger and backpressure so the
  mutator rarely blocks.

## Risks and honest caveats

- **Codegen safepoints are invasive.** The LLVM backend is young (records just
  landed). Emitting correct polls at every back-edge/call, and a handshake that
  scans the shadow stack consistently, is the highest-risk work. It is gated
  behind M3's differential-oracle green bar.
- **Value vs complexity.** klassic programs are single-threaded; a background
  GC thread trades real pause reduction for real concurrency complexity
  (atomics, a second thread, poll overhead). The owner has chosen this
  explicitly; M6 will measure whether the pause win is worth the hot-path poll
  cost, and that data can inform keeping it default-on or opt-in.
- **TSan is mandatory from M4.** Data races in a concurrent collector are
  silent and layout-sensitive (cf. the M6c ASan/ASLR heisenbug); TSan is the
  only reliable net, and the C collector being in C is what makes it usable.
