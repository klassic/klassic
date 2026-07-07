# ZGC-Style Low-Latency GC — Implementation Plan (x86_64 native backend)

Status: approved plan, execution starting. Scope decided by the owner:
x86_64 backend only (the shared Linux/Windows codegen in
`crates/klassic-native/src/lib.rs`); aarch64 is a separate epic (it has
no collector at all today). Rollout: direct replacement — no
`--gc legacy|zgc` dual mode; the collector is rewritten milestone by
milestone with the full test suite plus a differential oracle guarding
every PR. (`--gc-log` / `--gc-stress` / `--gc-poison` are observability
and debugging flags, not collector selectors, and do get added.)

## Motivation

The current collector is a precise, non-moving, stop-the-world
mark-sweep whose collections trigger only on segment exhaustion
(`gc_collect` has exactly one call site, inside `gc_alloc`). Pause time
is therefore proportional to the live set and strikes at unpredictable
allocation points — the classic latency-spike profile. The goal is
ZGC-style behavior: all pauses O(roots) and bounded (sub-millisecond as
a target), fragmentation eliminated by compaction, with collection work
paid incrementally alongside allocation.

## Survey summary (all verified against source, June 2026)

Current collector (x86_64):

- `gc_alloc(rdi=payload size, rsi=type tag) -> rax=user ptr`; 16-byte
  header at `ptr-16`: word0 = `size | mark bit @ 63`, word1 = type tag
  (1 = raw bytes, 2 = pointer record, 4 = pointer list; tags >= 2 mean
  every payload qword is a heap pointer or null, by construction).
- Heap: 1 MiB initial mmap segment, 1 MiB grow, 64-segment cap;
  first-fit non-splitting free list + bump; free-list reuse zeroes the
  payload (the d296fee lesson).
- Roots: a shadow stack (262144 entries) holding the **addresses** of
  rbp-relative slots — the collector reads doubly and can therefore
  *update* roots through the slot addresses, which is the single most
  important enabling fact for a moving collector; and a pin table
  (4096 entries) holding raw **values** with value-matched unpin — used
  only by call-argument staging, and incompatible with relocation.
- There is no OS-level threading: `thread { ... }` queues its body for
  sequential execution after main (no clone syscall exists in the
  backend). Exactly one mutator context exists today. (See
  "Thread-readiness" below — this must not be baked in.)
- Heap-pointer loads emitted by the compiler funnel through exactly two
  sites: the canonical field load in `compile_gc_read_qword`
  (serving `__gc_read_ptr` / `__gc_read_string`) and the map-entry copy
  load in `compile_gc_smap_keys_or_values`. rbp-slot reads are root
  reads, not heap loads. Heap-pointer stores funnel through one site
  (`compile_gc_write`). Static data never carries heap-pointer type, so
  "is this a heap pointer" is a clean heap-range property.
- Three hazard patterns hold raw heap pointers (or interior pointers)
  in collector-invisible places across potential allocation points:
  1. `compile_gc_write` keeps `base+offset` on the raw machine stack
     while compiling the value expression (which allocates for boxed
     fields);
  2. call-argument staging keeps evaluated args as raw machine-stack
     copies (plus a by-value pin) while sibling arguments evaluate;
  3. `==` / `assertResult` staging pushes the lhs heap pointer with
     `push_temp_reg` (not a root) across the rhs compile — a suspected
     latent use-after-free **today** (the lhs temporary's constructor
     root has already been popped when rhs construction can trigger a
     collection).

ZGC essentials adopted (classic single-generation, JEP 333 lineage):
colored pointers, load barrier on reference loads only, self-healing,
no store barrier (load-barrier-driven incremental-update marking under
the strong tricolor invariant), mark color alternating per cycle,
three O(roots) pauses (mark start / mark end / relocate start),
region-based heap with sparsest-first evacuation and forwarding,
large objects never relocated. We use the **masking** variant
(explicit color strip in the barrier) rather than multi-mapped memory:
masking needs no memfd/VA-reservation plumbing, works identically
through the Windows VirtualAlloc shim, and keeps the door open for
aarch64. The single-mutator adaptation replaces concurrent GC threads
with **bounded work quanta at allocation points** (LXR / LuaJIT-style
incremental scheduling): "concurrent" phases become interleaved quanta,
and the pause budget is the quantum size.

## Core design

Colored pointers:

- Color bits 60-62 of a pointer word: `M0 = 1<<60`, `M1 = 1<<61`,
  `R = 1<<62`; `COLOR_MASK = 7<<60`. mmap addresses are <= 47 bits so
  collision is impossible; bit 63 stays clear so pointer words never
  look negative.
- Pointers are colored **only inside heap object slots**. Registers,
  rbp slots, the shadow stack, and all temporaries hold raw addresses.
  This confines color handling to the two load sites, the one store
  site, `gc_deep_equal`'s two slot loads, and the collector itself —
  every other dereference (header reads, string bytes, etc.) is
  untouched.
- Good/bad masks flip per phase: during mark of parity b, good = `M_b`;
  otherwise good = `R`. Mark parity alternates per cycle so last
  cycle's pointers invalidate without any sweep over the heap.

Reserved registers: `r15 = bad_mask`, `r14 = good_color`,
`r13 = ~COLOR_MASK`. R12-R15 are entirely unused by the current
codegen and callee-saved on both SysV and Win64. The `.data` cells
`gc_good_color` / `gc_bad_mask` remain the source of truth; the
registers are caches reloaded at phase flips (and, later, at
safepoints — see thread-readiness).

Load barrier (fast path, four added instructions at the field-load
site):

```asm
mov  r10, rax            ; field address, kept for self-heal
mov  rax, [rax]          ; colored value
test rax, r15            ; any bad color bit?
jz   .ok
call gc_load_barrier_slow ; in: rax=value, r10=addr; out: rax=healed
.ok:
and  rax, r13            ; strip color -> raw (colored null -> 0)
```

The map-copy site keeps the healed value **unstripped** because it is
stored verbatim into a fresh pointer-list object. The shared slow path
is phase-dependent — Idle: forward through ghost regions, recolor;
Mark: mark + worklist push, recolor; Relocate: evacuate on demand,
recolor — and always self-heals by storing the healed value back
through the field address. The stub never calls `gc_alloc` (evacuation
bumps a dedicated to-space region), so it cannot recurse or run a
quantum.

Color-on-store: the single heap-pointer store site ORs the current
good color into pointer-typed values before the store, EXCEPT for a
null (a null slot must stay a raw 0 -- `0 | good_color` would be a
bogus non-null pointer, so the store guards on null).

Phase machine (state in a `.data` qword):
`Idle -> MarkStart (pause) -> Mark (quanta) -> MarkEnd (pause,
fixpoint-iterated) -> Relocate (quanta) -> Idle`.

- Quanta run inside `gc_alloc`, every 8 KiB of allocation; a mark
  quantum drains up to 512 worklist slots, a relocate quantum copies up
  to 8 KiB.
- MarkStart scans the shadow stack (through slot addresses) and pin
  table, flips the mark color, resets per-region live counters.
- MarkEnd re-scans roots; if anything new grays, marking continues
  (each pause stays O(roots)); when clean it frees last cycle's ghost
  regions, reclaims dead regions and large runs, selects the relocation
  set, evacuates any root-referenced targets, fixes roots, and flips to
  the relocate color.
- Trigger: proactive — a cycle starts when occupancy crosses 62.5% of
  the soft region budget at region-acquire time (replacing today's
  exhaustion-only trigger). If allocation outruns collection, the
  allocator degrades to finishing the cycle synchronously (correct,
  logged as an STW fallback) before growing the budget.

Regions:

- 128 KiB x 512 regions = 64 MiB reserved up front in one mmap /
  VirtualAlloc (lazily committed). Region index = `(ptr - base) >> 17`.
  Soft budget starts at 8 regions (1 MiB, preserving today's
  test-visible GC pressure) and doubles on stall up to the cap.
- Bump-only allocation in an active region; **the free list is deleted
  entirely** — compaction replaces it. Recycled regions are zeroed on
  acquisition (`rep stosb`).
- Large objects (block > 64 KiB) get dedicated contiguous region runs,
  are never relocated, and are reclaimed wholesale when unmarked.
- Evacuation headroom by invariant: the relocation set is chosen at
  MarkEnd only while `sum(live bytes of selected) <= free capacity -
  2 regions`, so relocation can never deadlock and no in-place
  fallback is needed. If there are no free regions the cycle is
  mark-only (still reclaims empty regions).

Forwarding is in-object: a forwarded object's header word0 becomes
`new_user_ptr | 1`. Block sizes are 16-aligned so header bits 0-3 are
free: bit 0 = forwarded, bits 1/2 = the two mark colors (no
mark-clearing pass needed; size extraction becomes `and reg, -16`;
bit 63 is vacated). A ghost (fully-evacuated) region is its own
forwarding table and is freed at the **next** cycle's MarkEnd, the
mark/remap fixpoint at which no live heap slot can still carry a stale
pointer into it.

Allocation during mark is allocate-black (new objects get the current
mark color and count toward region live bytes).

Soundness sketch (single mutator): every raw pointer the mutator can
hold originates from (a) `gc_alloc` — black during mark, (b) a
barriered load — marked during mark, or (c) a root — marked at
MarkStart, with later root writes drawing from (a)/(b)/(c) inductively.
So the strong tricolor invariant holds at every instruction and no
store barrier is needed. Relocation copies an object at most once (the
forwarding bit is checked before copying), and after the forwarding
word is installed every route to the object — barriered heap loads,
pause-fixed roots, and (post-hazard-fix) no raw temporaries across GC
points — yields the to-space address.

## Thread-readiness (owner directive: do not bake in single-thread)

Real threads do not exist in the backend today, but the design must not
make them impossible. Every single-mutator simplification is therefore
(a) localized to a named routine, and (b) documented here with its
multi-threading upgrade path. Reviewers should reject any PR that
scatters one of these assumptions across call sites.

| Simplification (today) | Where it lives | MT upgrade path |
|---|---|---|
| Self-heal is a plain store | `gc_load_barrier_slow` only | `lock cmpxchg` on the field; retry/accept-winner semantics (ZGC's own protocol) |
| Forwarding-word install is a plain store | `gc_maybe_evacuate` + relocate quantum | CAS install; first toucher wins, losers read the winner's address |
| Good/bad masks cached in r13-r15, flipped directly | phase-flip code in the two pauses | `.data` cells stay the source of truth; threads reload registers at a safepoint handshake (this is why the cells exist at all) |
| One global bump region | `gc_region_acquire` | per-thread active regions (the region design is already TLAB-shaped); region acquisition takes a lock or CAS on the region table |
| One shadow stack | prologue/epilogue emit + pause root scans | per-thread shadow stacks registered in a thread table; pauses walk all of them (requires a safepoint protocol to stop mutators first) |
| One mark worklist | `gc_mark_object` / mark quantum | striped per-thread worklists with work-stealing, or a lock; overflow policy unchanged |
| Quanta run only in `gc_alloc`, no synchronization | quantum hook in `gc_alloc` | each thread runs quanta at its own allocation points; phase transitions become a safepoint rendezvous |
| Region metadata plain loads/stores | region table helpers | atomics on state transitions (FREE->ACTIVE etc.) |

Two design choices were made *because* of this directive: the mask
source of truth lives in `.data` (registers are caches), and all color,
forwarding, and phase logic is concentrated in single named routines
rather than open-coded at call sites.

## Milestones (each = one PR, repo discipline throughout)

Discipline per PR: branch from main -> `cargo fmt --check` + `clippy -D
warnings` + full `cargo test` green -> independent review of the
riskiest register-level code -> PR -> all CI checks pass -> squash
merge -> verify merged content -> re-verify on main. The evaluator is
the semantic oracle everywhere (eval == native differential).

- **M1 — Latent use-after-free verdict (and fix if real).** Crafted
  repros loop `P(i,i) == P(i,i)` (and assertResult / nested-payload
  variants) ~100k times so collections inevitably strike during rhs
  construction while the lhs temporary sits unrooted on the machine
  stack. If output diverges from the evaluator or crashes, the fix
  (staging through rooted anonymous slots) ships in the same PR. This
  is a potential shipping bug today, independent of ZGC.
- **M2 — Observability + stress mode.** `--gc-log` (collections,
  allocs, bytes, max/total pause via the existing clock_gettime shim,
  one stderr line at exit) and `--gc-stress` (collect on **every**
  allocation — turns any unrooted-across-alloc bug into a
  deterministic crash). CI: a curated `gc_stress_` test subset per PR;
  full-suite stress nightly.
- **M3a — gc_write / gc_read staging fix.** Remove interior-pointer
  machine-stack staging: compile the value first, re-derive
  base+offset from a rooted slot afterward.
- **M3b — Call-argument staging rewrite (highest risk).** Replace
  pin + raw-stack copies with per-argument rooted anonymous slots;
  materialize argument registers (and >6-arg stack pushes,
  contiguously) from slots only after all arguments are evaluated.
  Invariant: no `push_temp` ever spans a sub-expression compile; a
  Rust-side debug assertion checks stack-offset balance. Independent
  register-level review required (this is where a previous attempt
  failed on rsp imbalance).
- **M3c — Delete gc_pin / gc_unpin / the pin table** once M3b removes
  the last user (kills the value-matched-unpin footgun permanently).
- **M4 — Region heap, direct replacement.** DONE. Segments, free list,
  and grow-ladder deleted; a single 64 MiB reservation (512 x 128 KiB
  regions, Linux demand-paged) + a per-region watermark array +
  bump-only allocation + whole-dead-region reclamation onto a
  free-region pool + a soft budget that starts at 8 regions (1 MiB, to
  preserve the old collection cadence) and doubles on allocation stall,
  capped at 512. Objects larger than a region take a contiguous region
  run carved from the uncommitted tail. Reused regions are zeroed on
  acquisition so object padding qwords the mark trace walks read as
  null (fresh tail regions are already mmap-zero). Mark stays STW.
  Known temporary regression: mixed live/dead regions are not
  compacted until M7 (documented; bounded by the same 64 MiB cap as
  today). Two further notes: (1) `gc_alloc_large` (the >128 KiB
  contiguous-run path) is implemented and verified by inspection but
  currently unreachable — every native heap-object materialization
  caps at 65 536 bytes, below the region size — so no
  native-compilable program reaches it yet; it also only ever carves
  fresh tail regions (never the single-region free pool), so reachable
  large-object churn would grow `committed_count` monotonically until
  M7. (2) The sweep seeds its free-region accumulator from the
  existing pool head, not zero, so regions freed in earlier cycles are
  not dropped (a leak that otherwise OOMs under `--gc-stress` with
  heavy turnover).
- **M5 — Colored pointers + load barriers (collections still atomic) +
  poison canary.** DONE. Color bits live in pointer bits 60-62 (M0=1<<60,
  M1=1<<61, R=1<<62); a colored pointer is non-canonical, so any
  unbarriered dereference faults. Reserved registers r13 = strip mask,
  r14 = good color, r15 = bad-color test mask, set once in the prologue
  (callee-saved, never reloaded in M5 since there is no phase flip yet).
  Color-on-store at the single write funnel (`compile_gc_write`) colors
  a heap-stored pointer with the good color, guarding NULL (a null slot
  stays a raw 0 -- coloring it would make a bogus non-null). The load
  barrier at the single read funnel (`compile_gc_read_qword`, only for
  HeapPointer/HeapString reads) tests the color, runs a slow-path stub
  on a bad color (strip + recolor + self-heal store), then strips to a
  raw pointer. The GC walkers (mark trace, deep-equal) strip colors off
  the slots they read. The `Map#keys`/`values` copy is a verbatim
  colored memcpy. Collections stay fully STW non-moving; the slow path
  does no marking/relocation/allocation, so it cannot recurse.
  `--gc-poison` sets r14 to the BAD color, so every heap slot holds a
  poisoned pointer, the slow path runs on every load, and any missed
  barrier faults deterministically. Coverage is doubly guaranteed:
  normal mode already faults on an unbarriered dereference (the good
  color M0 is equally non-canonical), catching a missing fast-path
  strip; poison catches a completely-missing barrier and exercises the
  slow path on every load. The poison suite gates every later milestone.
- **M6 — Incremental marking.** DONE. The collector is now non-atomic:
  the STW mark is split into a phase machine (Idle -> MarkStart ->
  Mark -> MarkEnd -> Idle) driven from gc_alloc. MarkStart (a short STW
  pause) flips the mark color (M0<->M1 each cycle; the reserved
  registers r14/r15 become caches reloaded here from the gc_good_color/
  gc_bad_mask cells), resets the worklist, and scans roots. The Mark
  phase runs incrementally in bounded quanta (gc_trace of
  GC_QUANTUM_POPS objects) once per GC_QUANTUM_BYTES allocated;
  recolor-on-trace rewrites each traced field slot to the new good
  color. A cycle starts proactively when bytes-since-cycle crosses half
  the soft budget. MarkEnd (a short STW pause) drains the remaining
  frontier to fixpoint and sweeps. allocate-black gives objects born
  during Mark a set header mark bit so the in-progress mark cannot
  reclaim them. The load-barrier slow path is phase-dependent: during
  Mark it also marks the loaded target (load-barrier-driven incremental
  update -- no store barrier, since every pointer the mutator can hold
  during Mark comes from a root, a barriered load, or a fresh
  allocation). Worklist overflow sets a flag that degrades the cycle to
  a from-scratch STW re-mark (logged as stw_fallbacks); a genuine
  over-capacity live frontier still aborts. The synchronous entry
  (do_collect, --gc-stress) finishes an in-progress cycle via MarkEnd.
  --gc-log pause timing now brackets the real O(roots)/O(heap-sweep)
  MarkStart and MarkEnd pauses. The sweep stays STW (moving/relocation
  is M7). Verified across ~100-cycle color-alternation runs with a
  rarely-read long-lived field, under normal / --gc-stress /
  --gc-poison / --gc-stress --gc-poison.
- **M7 — Evacuation + forwarding + self-healing (highest value,
  highest risk).** Sparsest-first selection, in-object forwarding,
  RelocateStart root fixup through shadow-stack slot addresses,
  mutator-assisted evacuation, ghost reclamation at next MarkEnd.
  Gated on M5's poison suite and M6 green under stress. An internal
  evacuation-off degrade switch is kept for one milestone as a
  bisection aid.
- **M8 — Tuning + docs.** Quantum/budget/selection-threshold tuning,
  final `--gc-log` fields, one generously-bounded pause assertion
  (250 ms — regression tripwire, not a benchmark), an
  architecture-rust.md section, and an issue for the aarch64 port.

## Testing strategy

- Differential oracle: the existing full suite stays green on every PR;
  a new `tests/gc_differential.rs` corpus (evaluate-first-then-diff)
  adds many-cycle churn, fragmentation-then-evacuation patterns, large
  objects, deep enum trees under `==`/`assertResult`, string
  interpolation loops, map-rehash churn, and >6-argument calls with
  allocating arguments.
- `--gc-stress` is the standing bug-shaker from M2 on (deterministic
  crash instead of size-dependent heisenbug) and the acceptance tool
  for the M3 rewrites.
- `--gc-poison` is the barrier-coverage proof from M5 on (a missed
  barrier is a guaranteed immediate fault).
- Pause measurement: clock_gettime through the existing shim (portable
  to the Windows shim, no calibration, cost only per pause). CI asserts
  functionality; pause numbers are reported, with a single
  generous-bound assertion.

## Top risks and mitigations

1. rsp imbalance in the M3b staging rewrite (a known past failure
   mode) -> slot-only staging, no cross-expression pushes, debug
   assertion, independent review, deep-recursion tests.
2. A missed load barrier on an unenumerated path -> M5 audit checklist
   + poison canary.
3. Lost objects during incremental mark -> quanta confined to
   `gc_alloc` with the post-M3 "everything live is rooted" invariant,
   MarkEnd root rescan, stress + poison.
4. Use-after-move from a missed M3-class hazard -> M7 gated on poison +
   stress; full-corpus stress (a full cycle per allocation) as the most
   hostile configuration.
5. Direct replacement means each milestone is immediately live ->
   pause-anywhere property: after any milestone, main is no worse than
   today (M1-M3 harden the current collector; M4+ are behavior-
   preserving with documented temporary regressions).
6. Hot-path throughput (+4 instructions per heap-pointer load) ->
   measured and reported; the latency-for-throughput trade is the
   owner-approved point of the project (ZGC's own cost is ~7-15%).
7. Windows VirtualAlloc shim differences -> regions are 64 KiB-aligned
   multiples, all reservation through the existing shim, and the
   Windows CI lanes run the same emitted code.

## References

- JEP 333 / 376 / 439 (ZGC; concurrent stack processing; generational)
- OpenJDK wiki: ZGC pointer metadata & multi-mapping
- "Deep Dive into ZGC" (TOPLAS 2022)
- Shenandoah (Brooks pointers, load-reference barriers) — considered
  and rejected as the primary design (per-object word + read
  indirection; JDK 13 removed it in favor of LRB)
- LXR (PLDI 2022) and LuaJIT's quad-color proposal — incremental
  scheduling prior art for the single-mutator adaptation
