# Concurrent ZGC for klassic (the "true ZGC" plan)

Status: **COMPLETE — M1 through M6 are all on `main`.** The collector is a
truly concurrent, multi-mutator ZGC: M1 (incremental relocation), M2
(thread-safe shared state), M3 (safepoint polls), M4 (background GC thread +
concurrent mark/relocate), M5 (per-thread shadow stacks/TLABs + a parked-count
all-mutator handshake + thread-safe init), and M6 (`--gc-log` pause
measurement). A background GC thread marks and relocates while N mutator
threads keep running; the only stop-the-world windows are the two O(roots)
phase-transition handshakes per cycle, and `--gc-log` reports their measured
cost. What remains is not a milestone but ongoing tuning (make the sweep
concurrent so even the handshake shrinks; trigger/backpressure tuning). Owner decision (2026-07-09): go all the way to a *truly
concurrent* collector — a background GC thread that marks and relocates while
the compiled program keeps running, with O(roots) pauses only at
phase-transition handshakes. This is the execution model that makes ZGC ZGC,
and it is what the earlier work (the merged C collector,
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
- **M4 — The GC thread + handshake protocol.** `klassic_gc_init` spawns a
  background GC thread. Its loop: block until allocation pressure crosses the
  trigger → MarkStart handshake → mark concurrently → MarkEnd handshake →
  sweep → RelocateStart handshake → relocate concurrently → Idle → signal
  waiting allocators. `gc_alloc` no longer runs quanta; it bumps and blocks
  (fallback) only if the heap is exhausted mid-cycle. First TSan run here.

  *Concrete design (worked out; deadlock-free lock ordering is the crux):*
  - **Two locks.** `g_gc_lock` guards the mutable collector data (worklist,
    to-space bump, region table, relocate cursor) and is taken only in short
    critical sections — the mutator takes it on the barrier SLOW path
    (mark_visit's worklist push, gc_evacuate's to-space bump), never the fast
    path — and by the GC thread. `g_hs_lock`/`g_hs_cond` is the phase-transition
    rendezvous, *separate* from `g_gc_lock` so the GC thread never holds the
    data lock while waiting for a mutator to park (which would wedge a mutator
    blocked on the data lock inside a barrier). The GC thread releases
    `g_gc_lock` before every handshake.
  - **Rendezvous handshake (O(roots) pause).** For a phase transition the GC
    thread raises `gc_handshake_requested` and waits on `g_hs_cond`. Each
    mutator, at its next poll, enters `klassic_gc_handshake`, signals "parked",
    and waits; the GC thread — now that the mutator is parked and touching no
    heap — performs the transition (atomic mask/phase flip; scan the parked
    mutator's shadow stack with mark_visit at MarkStart / re-scan at MarkEnd /
    relocate_root at RelocateStart), then releases the mutator, which reloads
    its mask view and resumes. Single mutator today; the parked-mutator model
    extends to N via a per-thread parked count and an ack barrier (thread
    table). This keeps mask/phase changes off the mutator's concurrent path —
    they happen only while it is parked.
  - **Concurrent mark/relocate.** Between handshakes the mutator runs; the GC
    thread drains the worklist (mark) / evacuates from-set quanta (relocate)
    under `g_gc_lock`, while the mutator's barrier pushes / evacuates-on-demand
    under the same lock. The M2 atomics (CAS forwarding, atomic mark bit) mean
    the header ops need no lock; the lock only serializes the worklist and
    to-space bump. Mark terminates when the worklist is empty AND a MarkEnd
    re-scan grays nothing new (fixpoint).
  - **M2 forward-looking fixes land here:** the follow-forwarding `block[1]`
    reads become acquire loads (paired with the release-CAS install), the
    evacuate un-bump becomes atomic, and `g_phase`/masks become atomic
    load/store (relaxed; the handshake supplies the ordering).
  - **First-version simplification (documented, not a shortcut to hide):**
    sweep + ghost-free + from-set selection run inside the MarkEnd pause (an
    O(heap) pause) to start; making the sweep concurrent is a follow-up. Mark
    and relocate — the expensive phases — are concurrent from the start.
  - **TSan is the driver:** a multi-threaded C harness (mutator pthread(s)
    churning records while the GC thread cycles on a tiny trigger) run under
    `-fsanitize=thread` (with `setarch -R` for the WSL2 ASLR quirk); every race
    TSan reports is fixed until clean, then ASan separately.
  - **Landed as (what was actually built):** a cycle is two O(roots)
    rendezvous pauses -- `gc_rendezvous(gc_mark_start)` then, after concurrent
    marking, `gc_rendezvous(gc_finish_mark_action)` (re-scan roots, drain,
    free ghosts, sweep, select from-set, fix roots) -- with concurrent mark
    between them and concurrent relocation (`gc_relocate_step` quanta) after
    the second. `g_gc_lock` guards the worklist + to-space bump + region
    table; `g_hs_lock`/`g_hs_cond` is the rendezvous; `g_wake_lock` the cycle
    requests. Three subtleties the implementation had to get right: (1)
    `klassic_gc_handshake` re-asserts `g_hs_parked` on *every* wait iteration,
    since consecutive rendezvous reuse the flag and a mutator waking into a
    fresh request must re-announce itself (a lost-park deadlock otherwise);
    (2) two evacuators can copy the same from-object, so `gc_evacuate_object`
    copies word0 + the immutable payload but sets the destination's word1 from
    the tag it already loaded rather than memcpy-ing the source's word1 (which
    a concurrent forwarding CAS is writing); (3) `klassic_gc_collect` waits for
    a *fresh* cycle (one that starts after the call) so it reclaims everything
    dead as of the call, not whatever an in-flight cycle happened to snapshot.
    `g_phase` / `g_from_set` / `g_bytes_since_cycle` / `g_relocations` and the
    word1 forwarding become atomic; `gc_relocate_step` bounds its walk by a
    `g_relocate_limit` snapshot rather than the racing `g_committed`. Verified
    clean across 48 ASan + 48 TSan parallel runs and the full `cargo test`.
- **M5 — N mutators: thread table + all-mutator handshake, TSan stress.**
  This is the milestone that turns the single-mutator M4 rendezvous into a
  real N-mutator one; the independent M4 review named it the load-bearing
  boundary. Concrete work, from that review:
  - **Handshake stops *all* mutators, not one.** `g_hs_parked` (an `int`)
    becomes a *parked count* compared against a *registered-mutator count*;
    `gc_rendezvous` runs `action()` only when every registered mutator has
    parked. Each mutator scans *its own* shadow stack at the handshake
    (avoids a cross-thread stack walk).
  - **Thread table.** Each mutator registers on start / deregisters on exit:
    its shadow-stack base+top, its active TLAB region, its mask cache. The
    global `g_shadow`/`g_shadow_top` and the single `g_heap_top` bump pointer
    become per-mutator (two mutators bumping one shared `g_heap_top` today
    would hand out overlapping objects).
  - **Publish masks atomically.** `gc_set_good` writes `gc_good_color` then
    `gc_bad_mask` as two stores; a running mutator's barrier fast path must
    never read a half-updated pair. Make the flip atomic (or double-buffer
    and swap a single pointer/epoch), and have each mutator reload its mask
    cache only at a handshake. Masks/`g_header_mark`/bump fields are mutated
    only inside a parked rendezvous, so single-mutator M4 is safe; N mutators
    need the atomic publish.
  - **Race targets (TSan-driven):** two mutators (or a mutator and the GC)
    loading/healing the same field, racing a forwarding CAS (first toucher
    wins), a handshake landing inside `gc_alloc`, TLAB/region acquisition
    contention, thread register/deregister vs an in-flight handshake. Also
    make the observability readers (`klassic_gc_live_region_count`,
    `klassic_gc_collection_count`) atomic. `gc_stress` (tiny trigger) run
    under TSan and ASan with N mutator pthreads until clean.
  - **Landed as (what was actually built):** a `Mutator` table (`g_mutators`,
    a `__thread g_self`) with a per-thread heap-allocated shadow stack and a
    per-thread TLAB; the main thread is registered by init, extra threads
    auto-register on first alloc/push and deregister on exit. The counted
    handshake uses `g_hs_parked_cnt` vs a `g_mutator_count` snapshot with a
    `g_hs_gen` generation; register/deregister take `g_hs_lock` and wait out
    an in-flight request (join) or park then leave (exit), so the snapshot
    never drifts. Two deviations from the sketch, both simplifying: (1) the GC
    thread scans every parked mutator's shadow stack itself (all are stopped,
    so no cross-thread stack race) rather than each mutator scanning its own;
    (2) `gc_set_good`'s two-store mask flip is left non-atomic because the
    counted handshake stops *every* mutator before the flip, so no barrier
    ever reads a half-updated pair -- the hazard the atomic-publish note
    guarded against cannot arise. Concurrent first-allocation init moved
    behind `pthread_once` (fixing a SEGV + the init-vs-first-alloc races), and
    `g_budget`'s unlocked pressure read + `grow_budget`'s write became
    relaxed-atomic. `runtime/gc/gc_mt_test.c` (several mutators, each rooting
    and re-reading its own survivor across concurrent moves) is wired into
    `run_tests.sh`; clean under ASan+UBSan and TSan, including 12x parallel
    (48 concurrent mutators + 12 GC threads) with zero races or deadlocks.
- **M6 — Pauses + observability. LANDED.** `--gc-log` is ported to the C
  collector (behind a `-DKLASSIC_GC_LOG` compile define threaded from the CLI
  flag through `link_llvm_program`, so a normal build carries no timing on the
  transition path). `gc_rendezvous` times each stop-the-world window -- the
  phase-transition `action()`, entered only once every mutator has parked --
  with `clock_gettime(CLOCK_MONOTONIC)`, and an `atexit` handler reports
  collections, relocations, bytes, and the pause count / max / total on stderr.
  A 200k-record churn shows pauses in the tens of microseconds (e.g.
  `pause_max=33314ns` across 68 handshakes), the evidence that the world stops
  only for O(roots)-ish handshakes, not for the mark or relocate. The one
  O(heap) component still inside the pause is the sweep (the documented M4
  first-version simplification); making it concurrent is the next tuning step
  and would shrink the measured max further. Covered by
  `gc_log_reports_pause_statistics` in `tests/llvm_backend.rs`.

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
