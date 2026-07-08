# LLVM Backend Migration — Implementation Plan

Move the klassic native backend from ~44k lines of hand-emitted x86_64
machine code to an LLVM-based backend, and re-implement the garbage
collector in C (clang-compiled). This is the M0 survey + complete plan;
it contains no implementation code and plays the same role
`docs/zgc-plan.md` played for the GC work.

## Motivation

The just-completed ZGC-style collector (`docs/zgc-plan.md`, milestones
M0–M8, merged) is a working incremental + moving low-latency GC — but it
is hand-emitted x86_64 machine code, which cost real bugs of a class no
higher layer would allow: register clobbers (the M7 `r10`/`rdx`/`rsi`
bugs), by-hand frame-slot bookkeeping, missing instruction encoders. The
scariest property is that GC bugs surface as raw `SIGSEGV`s in the
shipped binary with no symbols, caught only by luck (a collection-count
tripwire) rather than by tooling.

Owner decision: relax the "no `cc`/`as`/`ld`, self-contained ELF"
constraint; depend on an external LLVM/clang toolchain and link. Prefer
an LLVM dependency over a Rust-runtime dependency. Move the whole backend
to LLVM in one committed direction. Rewrite the GC in C so it gets
ASan/UBSan/Valgrind and gdb symbols — "being unable to trace GC bugs is
scary." This is the industry-standard architecture (the GC is a C
runtime; the compiler emits calls into it), and it solves the GC's
debuggability and the hand-emitted backend's fragility at once.

The GC **algorithm** is already designed and battle-tested; this migrates
the substrate (asm → C + LLVM IR), not the design. `docs/zgc-plan.md` and
the GC section of `docs/architecture-rust.md` are the C port's spec.

## Survey summary (verified against source, July 2026)

- `crates/klassic-native/src/lib.rs` is 43,978 lines: hand-emitted
  x86_64, the ELF64 writer, 114 `emit_*_runtime` routines (~25 GC, the
  rest value display/format/string/sort/file/Windows-shim), and the
  `NativeValue` provenance model that lets the shadow-stack walker know
  which slots hold GC pointers. Alongside: `aarch64.rs` (4,040 lines, a
  second hand-written backend, no collector), `macho.rs` (528),
  `pe.rs` (374), `cbackend.rs` (532). No LLVM/inkwell/llvm-sys
  dependency today; the OS is reached through raw syscalls + a
  hand-written Win64 shim (~30 `emit_win_*`).
- The GC: region heap (64 MiB reservation, 128 KiB × 512 regions, bump
  alloc, soft budget), 16-byte header (`size | mark-parity | FWD`,
  `size = word0 & -16`; word1 = type tag), colored pointers (bits
  60–62 M0/M1/R, colored only inside heap slots), a 4-instruction load
  barrier keyed on reserved callee-saved registers r13=strip /
  r14=good / r15=bad (which are caches of the `.data` cells
  `gc_good_color`/`gc_bad_mask` — the cells are the source of truth),
  color-on-store at one site, a precise **shadow stack of rbp-slot
  addresses** (so roots can be updated in place — the enabling fact for
  moving), the Idle/Mark/Relocate phase machine, incremental no-store-
  barrier marking, and moving/compacting evacuation with the R-color
  scheme.
- `--backend c` (`cbackend.rs`, `src/main.rs:link_c_program`) already
  shells out to a C compiler and links a runtime staticlib
  (`clang -O2 out.c libklassic_runtime.a -lpthread -ldl -lm`). Coverage
  is ~1% (scalars/arith/if/while/println/recursion; no heap, GC,
  closures, enums, records, collections, stdlib) — a "fail cleanly
  outside the subset" demonstrator. Its `klassic-runtime` staticlib
  (`rt.rs`) already reserves a `push_root`/`pop_roots` root ABI (no-ops)
  "so generated code won't change when the collector lands."
- The stdlib is 17 `.kl` files inlined as source through the same
  pipeline, so it rides on language-feature parity — not a separate
  porting burden.
- Differential oracle: `tests/sample_programs.rs` runs every
  `test-programs/*.kl` through both the evaluator and the native
  `build`, diffing stdout/stderr. `tests/cli_smoke.rs` (~530 scenarios),
  `tests/language_regressions.rs`. This corpus gates every milestone.
- Dev box: clang 14, lld present; `llc`/`opt`/`llvm-config` NOT
  installed; clang consumes textual LLVM IR directly (`clang out.ll`).

## Key design decisions

### Integration mechanism → emit textual LLVM IR, compile with clang

Generate textual `.ll` and hand it to `clang` (which optimizes, codegens,
and links; `lld` as linker). Do **not** take a libLLVM link dependency
(inkwell/llvm-sys). Rationale:

- Textual IR is auditable and `git diff`-able — a reviewer can eyeball
  the barrier IR exactly as they reviewed the barrier asm. This is what
  made the GC migration safe; inkwell builds IR imperatively in-process
  and cannot be diffed or pasted into a review.
- No LLVM-version-locked library link and no `llvm-config`/libLLVM at
  build time. Only a `clang` binary at runtime — the same light coupling
  `--backend c` already has, and the shell-out-and-link plumbing already
  exists (`src/main.rs:link_c_program`).
- Cross-platform for free: `clang -target <triple>` + lld emits ELF /
  Mach-O / COFF from one IR.
- Costs (per-compile process spawn + IR parse; no compile-time-typed IR
  builder) are irrelevant for a batch AOT compiler and are covered by
  clang's IR verifier + the differential oracle. Keep lowering behind a
  thin "IR sink" trait so inkwell remains a possible swap if IR-text
  latency is ever measured to matter.
- Pin: target **opaque-pointer IR, LLVM ≥ 15** (opaque `ptr` is default
  from 15; this box has clang 14 — bump the toolchain in M1, or emit
  typed-pointer IR as a fallback). Pin the major version in CI; textual
  IR is otherwise stable across minors.

### Backend seed → emit LLVM IR directly for user code; reuse the C backend's runtime-linking pattern for the C runtime

Split seed. **User-code codegen: fresh LLVM IR, not extended C.** The C
backend covers ~1%; reaching full parity by extending a C-string emitter
re-derives everything the 44k-line backend knows, and C loses the two
things the migration most needs: precise GC-root control (C's optimizer
won't guarantee a raw pointer stays dead across a safepoint, and aliasing
fights the barrier) and colored-pointer addressing (no portable C for
`addrspace(1)` or the exact `and/icmp/br/call` barrier shape). LLVM IR is
a near-direct target: `NativeValue` → typed SSA (`i64`/`double`/`ptr`),
the barrier → explicit IR, GC pointers → `addrspace(1)`, roots → an
explicit shadow-stack array (1:1 port). **Runtime library: the C backend
is the seed** — its clang-compiled `staticlib` of C-ABI functions,
shell-out link, and stubbed root ABI (`klassic-runtime`) are the seed for
the non-GC runtime and the shadow-stack interface; the GC becomes a
sibling C library.

### GC in C + the barrier in IR

Port the ~25 `emit_gc_*_runtime` routines to C (`libklassic_gc.a`),
spec'd verbatim by `zgc-plan.md`: the region heap, `gc_alloc`, the phase
machine, mark quanta / `gc_trace` / `gc_mark_visit`, evacuation
(`gc_evacuate`, relocate quanta, `gc_relocate_fix_roots`), ghost-region
freeing, `gc_deep_equal`, the shadow-stack scan, and the phase-dependent
`gc_load_barrier_slow`. The payoff: the GC is now normal C — ASan/UBSan/
Valgrind, gdb line info, and standalone C unit tests (synthetic heaps +
roots → assert survivors, forwarding, color flips, ghost freeing). The
single-mutator simplifications and their MT-upgrade paths from the
plan's thread-readiness table port verbatim (they were localized to
named routines precisely so this substrate swap stays clean).

**The barrier register problem (the pivotal redesign).** The fast path
leans on globally reserved callee-saved registers r13/r14/r15. Under LLVM
you cannot reliably reserve global registers — the allocator owns the
GPRs. But those registers were only caches of the `.data` cells, which
were always the source of truth. So the masks move to global (or
thread-local) cells loaded in the fast path — correctness unchanged, cost
one hot L1-resident load per barrier. Emit the fast path as explicit IR
at the two load sites (color-OR at the one store site):

```llvm
%v   = load i64, ptr %slot
%bm  = load i64, ptr @gc_bad_mask
%bad = and i64 %v, %bm
%isb = icmp ne i64 %bad, 0
br i1 %isb, label %slow, label %ok
slow:
  %healed = call i64 @gc_load_barrier_slow(i64 %v, ptr %slot)
  br label %ok
ok:
  %val = phi i64 [ %v, %e ], [ %healed, %slow ]
  %raw = and i64 %val, -16140901064495857665   ; strip = ~COLOR_MASK, an immediate
```

`@gc_strip_mask` is a true constant → an IR immediate; only the bad mask
needs a load. Attribute `@gc_bad_mask` so clang cannot illegally hoist/CSE
the load or the color test across the self-healing slow-path call (the
call is not `readnone`; the slot store clobbers). **Optional perf lever
(measure, don't assume):** compile both the `.ll` and the C runtime with
`-ffixed-r13/-r14/-r15` and access masks via named-register globals,
restoring the reserved-register fast path — x86_64-specific and fragile,
kept behind the global-cell baseline and decided by the M2 measurement,
never a correctness dependency.

**LLVM GC infrastructure — surveyed and mostly rejected.** `gc.statepoint`
/ `gc.relocate` / `RewriteStatepointsForGC` is LLVM's real relocating-GC
support, but it is stop-the-world / safepoint-handshake shaped and
provides no load barrier — it clashes with ZGC's concurrent,
load-barrier-driven, lazily-remapping design and forces a heavyweight
whole-IR rewrite. **Not adopted for the barrier.** `addrspace(1)` is a
provenance tag only (no color semantics) — adopt it to document GC
pointers and keep verifier help, but carry colored heap slots as `i64`
and `inttoptr` only at deref (mirroring "colored only inside heap slots";
colored pointers are non-canonical). `llvm.gcroot`/`gc "shadow-stack"` is
structurally like our shadow stack but constrains roots to allocas and is
less transparent — deferred in favor of an explicit IR shadow stack. No
colored-pointer/read-barrier intrinsic exists; the barrier is explicit
IR (it was 4 instructions in asm).

### Precise roots → explicit shadow stack in IR (direct port)

Keep the shadow stack: a global `@gc_shadow_stack` array + `@gc_shadow_top`,
push/pop of heap-pointer-local slot addresses at scope entry/exit, roots
updated in place through those addresses at the O(roots) pauses — a
literal port of `emit_gc_shadow_push`/`emit_gc_shadow_pop_n`. Reject
`gc.statepoint` (STW-shaped, clashes with the incremental machine, hard
to debug — the whole point is debuggability, and it adds nothing the
shadow stack lacks). `llvm.gcroot` kept as a documented fallback.

### Container formats / targets → collapse to target triples (the biggest structural win)

`clang -target <triple>` + lld emits ELF / Mach-O / COFF from one IR, so
`macho.rs` (528), `pe.rs` (374), the ELF writer, the `emit_win_*` shim
(~30), the raw-syscall OS layer, and **`aarch64.rs` (4,040 lines, a whole
second backend)** collapse into a target-triple string. The aarch64 epic
and any "port the GC to aarch64" work evaporate: the C GC compiles per-arch
for free (its only arch-specific pieces are mmap/reservation = libc, and
the barrier IR = arch-neutral). Well over 10k lines of hand-written
arch/OS/container code replaced by triple selection + one C runtime.
Trade-off (owner-accepted): binaries move from raw-syscall self-contained
static to libc-linked; a `-static`/musl option can restore
near-self-contained binaries if wanted, not required.

## Migration strategy (green-at-each-step)

Dual backend behind `--backend llvm` (the CLI already has backend
selection). The hand-emitted x86_64 backend stays the default and stays
green throughout; the LLVM backend grows construct-by-construct, each
gated by the eval-vs-`--backend llvm` differential oracle (a construct is
"done" only when its corpus subset is byte-identical to the evaluator).
No big-bang. Per-PR discipline (verbatim from the GC work): branch from
main → `cargo fmt --check` + `clippy -D warnings` + full `cargo test`
green → independent review of the riskiest code → PR → CI green → squash
merge → verify on main.

## Milestones (each = one PR-sized, independently-green step)

- **M0 — Survey + this plan.** No code. (This document.)
- **M1 — Toolchain + skeleton + CI pin.** Add `--backend llvm`; emit a
  trivial `.ll` (main → 0, `println` of a constant) driven through
  `clang out.ll libklassic_runtime.a -o out`, reusing the existing link
  machinery. Pin the clang/LLVM major version in CI; decide the
  opaque-pointer floor (bump to clang ≥ 15, or typed-pointer fallback
  for 14). Oracle: hello-world parity.
- **M2 — Barrier feasibility SPIKE (highest technical risk, first).** A
  tiny `.ll` + C GC stub proving under `clang -O2`: bad-mask load from a
  global cell; `and/icmp/br` to a slow-path call; the self-heal store
  survives the optimizer (no illegal hoist/CSE across the call); strip to
  a raw pointer; `addrspace(1)` provenance without breaking mask math;
  and a **measured** global-cell-vs-`-ffixed`-register throughput delta.
  Decides reserved-register-vs-global-cell. **Independent review.**
- **M3 — Scalar core.** Int/Double/Bool/Unit, arithmetic/comparison/
  logical, val/mutable/assign, scalar println, top-level defs incl.
  recursion. Basic `DILocation` from spans. Oracle: scalar corpus.
- **M4 — Control flow.** if (stmt+expr), while, blocks/scoping, cleanup,
  ternary.
- **M5 — Functions, general.** >6 args / stack passing, mutual recursion,
  top-level lambdas as functions, assert/assertResult.
- **M6 — GC runtime in C (`libklassic_gc.a`).** Port the region heap +
  phase machine + mark + evacuate + `gc_load_barrier_slow` +
  `gc_deep_equal` + shadow-stack scan from asm to C. **C unit tests +
  ASan/UBSan** on synthetic heaps/roots. **Independent soundness review.**
  (Where "GC gets ASan/gdb" is realized.)
- **M7 — Heap + strings + barrier wired (highest risk after M2).** Emit
  `gc_alloc` calls, the explicit IR shadow stack, the barrier fast path
  at the two load sites, color-on-store at the one store site; runtime
  strings, concat, interpolation; link `libklassic_gc`. Port
  `--gc-stress`/`--gc-poison` to the LLVM backend as gates. **Independent
  review (barrier coverage — the poison canary is the proof: a missed
  barrier faults deterministically).**
- **M8 — Aggregates + display.** Lists/maps/sets/records (static +
  runtime); port the `emit_append_*_to_runtime`/sort/split/join display
  surface to C.
- **M9 — Enums + match.** Lower the by-pointer ABI, boxed scalar fields,
  barrier'd `__gc_read_*`; recursive enums. **Independent review
  (enum-payload roots).**
- **M10 — Closures / captures.** Capture environments are heap objects
  with GC roots. **Independent review.**
- **M11 — Stdlib + full-corpus parity.** The 17 `.kl` modules ride on
  M3–M10; close remaining native builtins (file/env/time/process/dir) as
  C functions. Run the ENTIRE corpus through `--backend llvm`; reach
  byte-for-byte parity with the hand-emitted backend on Linux x86_64.
- **M12 — Cross-target via triples.** aarch64-linux, aarch64-apple-darwin,
  x86_64-pc-windows-msvc — each is "add the triple + link libc + C
  runtime." Confirm macho.rs/pe.rs/aarch64.rs/emit_win_* redundant.
- **M13 — Flip default + ASan sign-off.** `--backend llvm` becomes
  default. Stand up an ASan/UBSan/Valgrind CI lane running the GC across
  the corpus under `--gc-stress`/`--gc-poison`; pause/latency parity.
  **Independent review = the "done" gate.**
- **M14 — Delete the hand-emitted backend.** Remove the x86_64 emitter,
  ELF writer, `macho.rs`, `pe.rs`, `aarch64.rs`, the `emit_*_runtime`
  asm, and the raw-syscall/`emit_win_*` shims. Final docs.

Milestones needing independent register/soundness review: M2, M6, M7,
M9, M10, M13.

## Testing strategy

- Differential oracle everywhere: `tests/sample_programs.rs` extended to
  diff eval vs `--backend llvm`; `cli_smoke` scenarios gain an
  `--backend llvm` lane per construct as it lands. Byte-identical
  stdout/stderr is the bar (float formatting, char-vs-byte string
  semantics, display must match the evaluator exactly — the C runtime
  already does this by construction in `rt.rs`).
- `--gc-stress` / `--gc-poison` ported from M7 — the deterministic
  bug-shaker and the barrier-coverage proof.
- Standalone C unit tests for `libklassic_gc` under ASan/UBSan/Valgrind
  (new — impossible with the asm version).
- Cross-exec lanes (`tests/cross-exec/`) for the M12 triples.
- The generous pause-bound assertion (`native_gc_pause_stays_bounded`)
  carried into the new backend.

## Top risks and mitigations

1. **Barrier-on-LLVM feasibility (#1).** No LLVM read-barrier/colored-
   pointer primitive; global registers can't be reserved. → M2 spike
   FIRST; global-cell masks as baseline; `-ffixed` registers as a
   measured optional optimization; the poison canary as coverage proof.
2. **Optimizer vs. barrier.** clang `-O2` could hoist/CSE the mask load
   or color test across the self-healing slow-path call. → correct global
   mutability/attributes (call not `readnone`) + poison canary; proven in
   M2.
3. **LLVM version / opaque pointers.** Opaque `ptr` default from 15; box
   has 14. → floor at LLVM 15 (bump toolchain) or typed-pointer fallback;
   pin the major version in CI.
4. **CI cost / build-time.** Each lane needs clang; per-compile is a
   spawn + parse. Acceptable for batch AOT; lighter than an inkwell build
   dependency. Cross lanes need sysroots/QEMU (partly present).
5. **libc dependency (runtime-model change).** Owner-accepted; offer a
   `-static`/musl path, don't require it.
6. **Debug-info quality.** Emit DILocation/DISubprogram from spans for
   user code; the C GC gets full DWARF from clang automatically (the
   whole motivation). User-code debug info parity is a stretch goal, not
   a gate.
7. **"Done" definition.** (a) byte-for-byte corpus parity with the
   hand-emitted backend on Linux x86_64; (b) builds+runs on aarch64
   (linux+macos) and Windows x86_64 via triples; (c) the C GC passes the
   corpus under ASan/UBSan/Valgrind with `--gc-stress`/`--gc-poison`; (d)
   pause/latency parity; (e) the hand-emitted backend/container
   writers/`aarch64.rs` are deleted.

## References

- `docs/zgc-plan.md` — the GC algorithm spec the C port must preserve
  verbatim (including the thread-readiness upgrade paths).
- `docs/architecture-rust.md` (GC section) — accurate ZGC description.
- `docs/native-backend-strategy.md` — the pre-existing "LLVM later,
  behind a boundary" position this plan executes.
- LLVM LangRef: `gc "..."` strategies, `llvm.gcroot`, address spaces;
  the statepoint docs (surveyed and deliberately not adopted for the
  barrier — STW-relocation-shaped, not ZGC-shaped).
