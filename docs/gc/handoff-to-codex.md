# Handoff to Codex: Generational ZGC (as of `ee90270`, 2026-10-23)

Read this first, then `docs/gc/g01-g15-status.md` (the authoritative scoreboard),
then the spec `klassic-generational-zgc-instructions.md`.

## 1. Where things stand

Branch `gc/generational-wip`, HEAD `ee90270`, **not pushed, not merged to
`main`** (base `9346bff`). Everything below is committed; the tree is clean
except an ignorable `tools/__pycache__/`.

An opt-in flag `--gc=generational` now runs a real generational collector on
the **production compile path** (it did not before: the earlier session built
~300 components that no ordinary program reached). The default collector is
untouched and its output is **byte-identical** to the baseline (checked with
`cmp` on 177 builds across Linux / Windows / AArch64 after every change).

**Scoreboard (honest): 0 of G01-G15 met, 10 partial, 5 open.** "Complete ZGC"
is not achieved. Do not claim any row met without a named test and, for
the end, the spec section 11 measurements (none exist yet).

What works today under the flag (x86-64 Linux; Windows x86-64 shares the code
without a worker; AArch64 Mach-O has its own simpler path):

- Heap strings and enums (mono and generic, nested, recursive, collection
  literal cells) allocate through the emitted `alloc_into` TLAB leaf, are
  published under a source-access guard, and live in two Young semispaces plus
  a two-half Old generation (promotion on second survival, full collection
  flips Old).
- Frame references are RootCells (LIFO-reused); heap fields hold raw payload
  addresses recognised by a range rule.
- Pause = flip + forward roots/remset/literal cells. Everything else is done
  lazily: a GC **worker thread** (`clone`) scans/copies with **CAS-claimed**
  copies into its own to-space pages, **concurrent marking** (bitmap, SATB
  pre-barrier in `field_store`), a **load barrier** (`field_load`, plus the
  scalar `__gc_read` funnel) relocates and heals from-space words, double-
  buffered remembered sets with a slot bitmap.
- `--gc-stress` evacuates before every allocation; `--gc-log` prints a
  `gc[generational]:` line with ~20 counters.

## 2. Map of the code

Read in this order:

| File | What |
|---|---|
| `crates/klassic-native/src/gc_codegen/production.rs` | `Runtime` struct (one mmap block), `Labels`, `emit()`; constants (page counts, heap layout) |
| `production/offsets.rs` | field-offset constants (`RT_*`) |
| `production/startup.rs` | mmap, page table, descriptors, root storage |
| `production/alloc.rs` | `gc_alloc` ABI (rdi=size, rsi=legacy tag -> rax payload), refill, quantum, OOM |
| `production/collect.rs` | the pause: flip, full-vs-young decision, `finish`/`quantum`, page recycle |
| `production/enums.rs` | `forward` (copy/promote), `field_load` barrier, `field_store` (write barrier), Cheney scan, `deep_equal` |
| `production/copy.rs`, `scan.rs` | lock-free CAS copy path and worker scan |
| `production/mark.rs` | mark start, bitmap trace, SATB |
| `production/remset.rs` | double-buffered remembered sets |
| `production/worker.rs` | clone/futex thread, ticket lock |
| `production/roots.rs` | RootCell adopt/pop/store/load, `--gc-log` heap walk |
| `crates/klassic-native/src/lib.rs` | search `gc_gen`: wiring (`prepare_generational_runtime`, `store_value_slot`/`load_value_slot`, field read path, literal root seed, print copy-out) |
| `crates/klassic-native/src/gc_codegen/macos_generational.rs` | AArch64 port (own wrapper `FlagAsm`; uses shadow stack, not RootCells) |
| `tests/gc_generational.rs` (+ `gc_generational/aarch64_macho.rs`) | integration tests |
| `crates/klassic-native/src/gc_barrier_harness_tests{.rs,/}` | machine-code harness (SATB / remset, with negative controls) |

All routines are written once over the `PortableAsm` trait
(`portable_asm.rs`). x86 flags semantics matter: AArch64 plain `sub`/`and`
do not set flags, which is why that port wraps the routines.

## 3. Rules of the repo and of this work

- Never `git add -A` (a `.mcp.json` with a secret exists). Add by explicit
  path. Commit/push only when asked; the owner asked for commits on this
  branch, no push.
- `cargo fmt --check`, `cargo clippy --all-targets --all-features -- -D
  warnings`, and full `cargo test` must be green. `cargo test` includes a
  ~5-minute sanitizer run (`c_gc_passes_under_sanitizers`); give it time.
- Files stay under 800 lines (`production.rs` is 717; split before growing).
- The **default path must stay byte-identical**: build a baseline release
  binary from `HEAD` in a scratch `git worktree`, `cmp` outputs of
  `test-programs/*.kl` and `examples/*.kl` for Linux, `--target
  x86_64-pc-windows-msvc`, `--target aarch64-apple-darwin`, with and without
  `--gc-log` / `--gc-stress`. Remove the worktree afterwards.
- The evaluator is the oracle; native output must equal evaluator output.

### The verification recipe that caught every real bug here

1. Add a temporary env-var default in `src/cli.rs` (two lines near the
   `gc_generational` / `gc_stress` locals in `parse_command_line`, mark them
   `// TMP-GEN-HOOK`) so `--gc=generational` and `--gc-stress` become the
   default.
2. `KLASSIC_TMP_GEN_DEFAULT=1 [KLASSIC_TMP_GEN_STRESS=1] cargo test --test
   cli_smoke` (skip `native_deep_recursion_with_per_frame_alloc_runs` under
   stress: it is quadratic). Expect ~16 failures, all **intended**: C-backend
   rejections, `--gc-poison` rejections, and tests that read the legacy
   `gc: collections=` log format. Anything else is a bug.
3. **Remove the hook** (`grep -c TMP-GEN src/cli.rs` must print 0) before
   committing. This was done every time; keep doing it.
4. Race loops for threaded changes: 50 plain + 50 stress + 20 under
   `taskset -c 0` of the worker test programs.

## 4. Traps already paid for (do not rediscover)

- `alloc_raw` / `forward` clobber the interrupted allocation's request
  (`alloc_size`, `elements`, tag, descriptor). `collect` saves and restores
  all four; a missed one produced records allocated as raw bytes.
- RESERVE of a RootCell never reissues a cell; frame cells are LIFO-reused
  (pop = STORE null). 2^20 cells now.
- Publishing an object needs ENTER -> publish -> EXIT per object; a standing
  guard breaks allocation (`access_epoch != 0`).
- Forwarding must be gated by the **from-space ranges**; otherwise already
  copied objects get copied again.
- Constant arguments are partially evaluated away, so a "garbage-heavy" test
  program must use a loop-variable-dependent depth to really allocate.
- The root journal has no consumer: drain (`consumed = published`) on FLUSH.
- Printing must copy heap bytes to a native buffer before `write`.
- AArch64: `bl` clobbers x30; pushes are 16 bytes; the legacy allocator
  wrapper must preserve x1-x12.
- Heap-field slots are untyped for generic enum payloads, hence the range
  rule instead of colored heap references (this is the G05 gap).

## 5. What remains, in dependency order

Authoritative detail is in `g01-g15-status.md`; summary:

1. **G05** per-generation colored references and remap epochs in heap fields
   (needs typed field access or a different encoding for generic payloads).
2. **G08 / G09** concurrent root scanning (today roots are forwarded inside
   the pause) and Old marking preserved across Young cycles.
3. **G02** more than one mutator thread; 2 / 8 mutator runs; handshakes and
   safepoints (reuse `gc_codegen/{handshake,threads,context}.rs`; the legacy
   N-mutator design in `docs/true-zgc-plan.md` is the model).
4. **G12** adaptive scheduling (start time, generation sizes, tenuring,
   worker count). One worker / fixed age is explicitly not G12.
5. **G13** native boundary: attach/detach, blocked state, handles, scoped pin.
6. **G14** runtime weak handles.
7. **G10 / G11** real heap management: hard/soft max, large objects
   (>128 KiB is a run-time error today), uncommit, pre-touch, relocation
   reserve, allocation stall.
8. **G15** LLVM/C backends, real arm64 hardware, and the spec section 11
   latency measurements, then the section 12 final report.
9. Worker + marking on Windows and Mach-O (both use the single-threaded path).

Known soft spots: the global ticket lock still covers the two pauses, phase
finish, `--gc-log` verify and the SATB slow path; marking does not yet drive
evacuation (copy is reachability-driven); SATB is reachable only through the
machine-code harness because the language has no heap-field assignment (a
language feature was deliberately not added); the thread-creation fallback
is untested; AArch64 has no worker and no copy-out print buffer; one
`debug_assert` (`imm < 4096`, `aarch64.rs:572`) fires in debug builds when
compiling `tests/cross-exec/36-stdlib-system-modules.kl` for AArch64
(pre-existing, release builds are fine).

## 6. Decisions waiting on the owner

- How to integrate `gc/generational-wip` into `main` (split into PRs, review
  order). Nothing has been pushed.
- The owner wants Klassic to become an **AI-only-grown language** later,
  modelled on `onion-lang/onion` (scheduled maintainer/reviewer/sensor/
  strategist runs, PR auto-merge behind required checks and a review panel,
  episode memory). Prerequisite in practice: `--gc=generational` in CI, the
  evaluator-vs-native differential test as the automatic gate, and the
  integration decision above. Do not start this without the owner.

## 7. Suggested first moves

1. Run `cargo test --test gc_generational` (24 tests, ~45 s) and the
   barrier harness (`cargo test -p klassic-native gc_barrier`) to confirm a
   green baseline.
2. Build a baseline binary for the byte-identity check before changing
   anything.
3. Pick the next row from section 5, write its failing test first (a named
   test is the unit of evidence), then implement, then run the recipe in
   section 3. Update `g01-g15-status.md` in the same commit.
