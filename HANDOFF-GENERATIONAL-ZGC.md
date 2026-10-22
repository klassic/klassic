# Handoff: Generational ZGC for Klassic

Written 2026-10-23 for Codex. **Already merged into `main` and pushed**:
`origin/main` = `5c17545` (fast-forward from `9346bff`). The working branch
`gc/generational-wip` still exists locally at the same commit; new work
should branch from `main`.

Read in this order: this file -> `docs/gc/g01-g15-status.md` (the scoreboard)
-> `klassic-generational-zgc-instructions.md` (the spec, 13 sections) ->
`docs/gc/concurrent-plan.md` and `docs/gc/production-integration-plan.md`
(what each milestone did) -> the code (section 3).

---

## 0. One-paragraph truth

A generational collector now runs on the **production compile path** behind
the opt-in flag `--gc=generational`. It is a real collector (Young + Old,
promotion, remembered sets, SATB marking, a GC worker thread, lock-free
copying, a load barrier), but it covers a narrow program subset and a fraction
of the spec: **0 of G01-G15 are met, 10 are partial, 5 are open**, no latency
has been measured, and the spec's final audit does not exist. Do not say
"complete" until `docs/gc/g01-g15-status.md` says so row by row with a named
test, and spec sections 11 and 12 are done. The default collector is
untouched and its output is byte-identical to before.

---

## 1. History and why the code looks like this

- Before this work an earlier session had committed ~300 files of components
  (`runtime/gc/gc_*.h`, `crates/klassic-native/src/gc_codegen/*.rs`, checkpoint
  `0d674e8`). **None of them was reachable from an ordinary compile**; ~211
  emitters were `#[allow(dead_code)]` and tested only in isolation. More
  isolated components do not advance G01-G15; one production path does.
- This session built that path in milestones (built on `gc/generational-wip`, now in `main`):

| Milestone | Commit(s) | What |
|---|---|---|
| M-G1 | `636f4c6` `f2e5fb7` `75ad3dc` | `--gc=generational` selector; heap strings through emitted `alloc_into` TLAB, source-access publish, RootCells; no collection |
| M-G2 | `1b47d3e` | stop-the-world semispace evacuation of RootCell referents |
| M-G3 | `1aea9f9` | enums (records, boxes, nested/generic, literal cells), Cheney scan |
| M-G4/5 | `b068762` | Old generation, promotion, remembered set + write barrier, full collection |
| M-G6.1-3 | `88e1aee` | lazy relocation behind a load barrier; 64-object quanta per allocation |
| PE | `a45bb96` | Windows x86-64 (VirtualAlloc leaf, Win64 write shim) |
| Mach-O | `ec0fc23` `78ea4f8` | AArch64 port (`macos_generational.rs`) |
| Worker | `e22296b` | GC worker thread (`clone`+futex), ticket lock |
| CAS + mark | `bb4669e` | lock-free CAS-claimed copying, worker-private to-space, concurrent marking, SATB |
| Status | `a8bd50d` `c19ddae` | G01-G15 mapping; SATB-unreachable note |
| G06/G07 | `c095461` `ee90270` | machine-code barrier harness; double-buffered remsets |

---

## 2. What you can run right now

```bash
cargo build                                   # debug binary: target/debug/klassic
target/debug/klassic --gc=generational --gc-log build prog.kl -o prog && ./prog
target/debug/klassic --gc=generational --gc-stress ...   # evacuate before EVERY alloc
target/debug/klassic --target x86_64-pc-windows-msvc --gc=generational ...   # PE (run via WSL interop)
target/debug/klassic --target aarch64-apple-darwin  --gc=generational ...    # Mach-O (emulator)
docs/gc/repro/run.sh                # 6 repro programs vs the evaluator, plain
docs/gc/repro/run.sh --gc-stress    # same under stress (all 6 OK as of HEAD)
cargo test --test gc_generational   # 24 tests, ~45 s
cargo test -p klassic-native gc_barrier   # 8 machine-code harness tests
```

`--gc=generational` is rejected (with a diagnostic) with `--backend c|llvm`
and with `--gc-poison`. `--gc-stress` is accepted.

`--gc-log` prints one stderr line `gc[generational]: ...` with these
counters: `collections full_collections healed promoted_bytes allocated_bytes
pages_used objects published worker_scanned worker_copies worker_lost
mutator_copies mutator_lost mark_cycles marked mark_drains unmarked_copied
remset_len remset_flips remset_flipped remset_kept remset_dropped`
(`objects == published` is an invariant the heap walk checks; `unmarked_copied`
must stay 0).

Programs that exercise the interesting paths are in `docs/gc/repro/`
(`promote-long-lived`, `string-garbage-loop`, `enum-equality-stress`,
`nested-generic-enum`, `deep-recursion-enums`, `print-multichunk`).

---

## 3. Architecture (what the code does)

### 3.1 Heap layout (one `mmap` heap + one `mmap` metadata block)

Heap offsets (`production.rs`): page size 128 KiB (`HEAP_PAGE_BYTES`, shift 17);
offset 0 is never a page.

```
[ page 1 .. page 512 ]  Young: 64 MiB, two semispaces of 256 pages (HALF)
[ Old half A | Old half B ]  2 x 64 MiB (OLD_HALF); promotion bumps in the current half
```

`YOUNG_END = (512+1) pages`; `HEAP_BYTES = YOUNG_END + 128 MiB`. The metadata
block (`Layout`/`L` in `production.rs`) holds the `Runtime` struct, 512
`AllocationPage` descriptors (112 B each), per-page object-start bitmaps,
forward tables/cells, the mark-record arena, root storage (2^20 RootCells of
32 B + journal), two remembered sets (2^20 entries each) + slot bitmap
(`rbits`), the mark bitmap (`mbits`, 1 bit/16 B), the mark stack (2^22).

### 3.2 Object model

- 32-byte header before the payload: `payload_bytes`, `descriptor_id`,
  `mark_record`, `state` (INITIALIZING=1 then PUBLISHED=2). Payload is 16-byte
  aligned, minimum 16 bytes. One object must fit one page (max payload 131040 B;
  larger is a run-time error).
- Descriptor ids = legacy shape tags: 1 raw bytes (strings, boxed scalars),
  2 pointer record (enum values, every slot is a reference word), 4 pointer
  list (first word is the length).
- **References in RootCells are encoded**: `(heap_offset << 16) | 0x1510`
  (0x1510 is the initial-phase color); 0 is null; bit 0 set marks an
  `EXTERNAL` immortal pointer outside the heap (static enum constants).
- **References inside heap fields are raw payload addresses**, recognised as
  references by a range rule (an address inside heap payload). This is
  because generic enum payloads travel through untyped (scalar) read/write
  funnels. It is the main reason G05 is only partial.
- Aging: `MarkRecord.age`. A Young object that survived one evacuation is
  promoted on the next. A full collection promotes everything live.

### 3.3 Roots

Frame slots of type HeapString/HeapPointer hold a **pointer to a RootCell**,
not the object. All loads/stores go through `load_value_slot`/`store_value_slot`
in `lib.rs` (calls `root_load`/`root_store` in `production/roots.rs`).
`emit_gc_shadow_push/pop_n` call `root_adopt`/`root_pop`. Cells are **reused
LIFO** (RESERVE never reissues a cell; pop = STORE null). Cell 0 is the
allocation scratch root. Collection-literal data cells are visited by the
backend `literals` routine (`emit_literal_root_seed`). The root journal has no
consumer: it is drained (`consumed = published`) when it reports FLUSH.

### 3.4 Allocation (`production/alloc.rs`)

The legacy ABI is kept so the 8 allocation sites did not change:
`rdi` = payload bytes, `rsi` = legacy tag -> `rax` = payload address.
Steps: advance an open relocation phase by one quantum; save request;
(`--gc-stress`: collect); publish the previous object; empty cell 0; size
guard; pick descriptor/element count from the tag; loop on `alloc_into`
(REFILL = close the current page and bind the next prepared one; ROOT_FLUSH =
drain the journal; semispace exhausted = collect once, then retry or OOM).
Objects are published **lazily** (at the next allocation). Each publish is
`source_access ENTER -> initialization publish -> EXIT` (a standing guard is
illegal: `access_epoch != 0` makes allocation fail).

### 3.5 The collection cycle (`production/collect.rs`, `enums.rs`)

```
idle --(Young semispace full, or --gc-stress)--> [mark start pause] (worker builds only)
     --> concurrent mark (worker traces from the root snapshot; SATB in field_store)
     --> relocation PAUSE: complete any open phase; finish mark; flip Young
         (and Old if a full collection is chosen); remset flip; forward cell
         referents of roots, remset (previous), literal cells; set the
         from-space ranges ry_lo/ry_hi (+ ro_lo/ro_hi for a full GC)
     --> relocation PHASE open: worker (or allocation quanta) runs Cheney over
         to-space chain A (mutator TLAB pages), chain B (worker private pages),
         and promoted Old objects; the load barrier relocates+heals on demand
     --> worker sets worker_done; the mutator's next allocation runs `finish`,
         which recycles from-space pages (zero payload/bitmaps/forward cells,
         READY under a new incarnation)
```

Full collection is chosen when Old could not absorb a worst-case promotion
(one Young semispace): Old flips too and every live object is promoted into
the other Old half. Live data exceeding a semispace / Old half is an
**honest OOM** with a clear message.

Forwarding record (CAS target): the source header's `descriptor_id` becomes
the copy's reference bits with `FWD16 = 0xf0f0` in the color bits
(`production/copy.rs`); `mark_record` was used by the earlier single-thread
path. Forwarding is gated by the from-space ranges - anything outside them is
never copied.

### 3.6 Concurrency (Linux x86-64 only)

- One worker thread (`worker.rs`: `clone` with its own 256 KiB stack, futex
  sleep, woken by the pause). If spawn fails, allocation quanta run instead.
- Copy claim: copy first, then CAS the source header descriptor from its id to
  the forwarding word. The loser turns its tentative copy into a published raw
  filler and uses the winner's reference. Heals are CAS against the stale word
  (a racing mutator store wins). The mutator's TLAB takes pages from the
  bottom of the semispace, the worker's private pages from the top (`bounds`
  packed CAS word). Promotion reserves Old space by CAS on `old_top` and
  writes the descriptor **last**; scanners wait for a non-zero descriptor.
- **One fair ticket lock remains** for: the relocation pause, mark start,
  phase finish, `--gc-log` verify, and the SATB slow path in `field_store`.
  Copying, allocation, roots and the load barrier are lock-free.
- Marking: bitmap + stack; snapshot-at-the-beginning; objects allocated
  during marking are born marked; `field_store` marks the overwritten
  reference; the relocation pause finishes the trace and drops remembered
  entries whose referent is unmarked. Marking does **not** yet decide what is
  evacuated (evacuation is still reachability-driven).

### 3.7 Remembered sets (`production/remset.rs`)

Two arrays + `rbits` slot bitmap. `field_store` appends an Old slot that now
holds a Young address (once per slot). A Young collection flips (current ->
previous), the pause consumes previous as roots and re-registers entries whose
slot still holds a Young address; a full collection discards both.

### 3.8 Ports

- **Windows x86-64**: same emission; `map` goes through a realigned
  VirtualAlloc leaf; heap strings print via the Win64 write shim. Single
  thread, no worker, no marking.
- **AArch64 Mach-O**: `gc_codegen/macos_generational.rs` (wrapper `FlagAsm`
  because AArch64 plain `sub`/`and` do not set flags like x86; legacy entry
  labels become wrappers preserving x1-x12). Uses the backend's existing
  shadow stack instead of RootCells; literals hook forwards shadow-stack
  slots in place. Single thread. Verified only with the unicorn emulator
  (`tools/run-macho-aarch64.py`), never on real arm64.

---

## 4. File map

| Path | Role |
|---|---|
| `crates/klassic-native/src/gc_codegen/production.rs` | `Runtime`, `Layout`, `Labels`, `emit()`, constants (717 lines) |
| `.../production/offsets.rs` | `RT_*` field offsets |
| `.../production/startup.rs` | mmap, page table, descriptors, root storage |
| `.../production/alloc.rs` | allocator, refill, quantum, OOM |
| `.../production/collect.rs` | the pause, phase `finish`/`quantum`, recycle |
| `.../production/enums.rs` | `forward`, `field_load`/`field_store` barriers, Cheney, `deep_equal` |
| `.../production/copy.rs` `scan.rs` | CAS copy path; resumable 3-chain scan |
| `.../production/mark.rs` | mark start, trace, SATB |
| `.../production/remset.rs` | double-buffered remsets |
| `.../production/worker.rs` | thread, futex, ticket lock |
| `.../production/roots.rs` | RootCell adopt/pop/store/load, `--gc-log` heap walk |
| `.../production/test_support.rs` | harness support |
| `crates/klassic-native/src/lib.rs` | wiring: search `gc_gen`, `prepare_generational_runtime`, `emit_generational_runtime`, `store_value_slot`, `load_value_slot`, `emit_literal_root_seed`, `emit_generational_write_heap_string`, `emit_generational_log_report` |
| `crates/klassic-native/src/gc_codegen/macos_generational.rs` | AArch64 port |
| `crates/klassic-native/src/gc_barrier_harness_tests.rs` + `/satb.rs`, `/remset.rs` | machine-code harness (a `#[cfg(test)]` hook in `NativeCodeGenerator::compile` swaps the source for hand-written code around the real `production::emit`) |
| `tests/gc_generational.rs` + `gc_generational/{aarch64_macho,remset_log}.rs` | integration tests |
| `src/cli.rs` `src/main.rs` | flag parsing; driver gate (every direct target allowed; `--backend c/llvm` and `--gc-poison` rejected) |
| `docs/gc/*` | `g01-g15-status.md`, `concurrent-plan.md`, `production-integration-plan.md`, `capabilities.md` (big historical inventory, dated), `repro/` |
| `runtime/gc/*.h`, `gc_codegen/*` (non-production) | the earlier component work; the production path reuses `roots`, `heap`, `alloc_into`, `source_access`, `initialization`, `descriptors`, `root_storage` |

Everything is written once over `PortableAsm` (`portable_asm.rs`, V0..V11
registers: V0=rax, V1=rcx, V2=rdx, V3=rbx (holds the runtime pointer in all
routines), V6=rsi, V7=rdi, V8-V11 = r8-r11). Routines end in `leave`/fallthrough;
callers add `ret`; relocations inside blobs are Rel32 to text labels only.

---

## 5. Rules of the repo and of this work

- **Never `git add -A`** (a `.mcp.json` holding a secret exists). Add by path.
  Commit and push only when asked. Note `main` is branch-protected on GitHub
  ("Changes must be made through a pull request"); the one direct push of
  this work (`9346bff..5c17545`) was accepted by the remote anyway, but new
  work should go through a PR.
- `cargo fmt --check`; `cargo clippy --all-targets --all-features -- -D
  warnings`; full `cargo test` (includes a ~5 min sanitizer test
  `c_gc_passes_under_sanitizers`) must be green. Rust 2024.
- Files < 800 lines (`production.rs` is 717; split before growing).
- Commit subjects imperative, < 72 chars. Update docs in the same commit.
- Any change touching codegen must keep the **default path byte-identical**:
  build a baseline release binary from `HEAD` in a scratch `git worktree`,
  `cmp` outputs of `test-programs/*.kl` and `examples/*.kl` for the Linux
  target, `--target x86_64-pc-windows-msvc`, `--target aarch64-apple-darwin`,
  each with and without `--gc-log`/`--gc-stress`; remove the worktree after.
  (177 builds were identical after every change.)
- The evaluator is the oracle (native must equal evaluator output). Beware:
  the evaluator overflows its stack on ~40000-deep recursion (hence
  `repro/deep-recursion-enums.expected`).
- Honesty rule: never claim a G-row without a named passing test. State
  what is *not* done.

### Verification recipe (it caught every real bug so far)

1. Temporarily add an env-var default to `src/cli.rs` in `parse_command_line`
   (two locals; mark both lines `// TMP-GEN-HOOK`):
   `let mut gc_generational = std::env::var_os("KLASSIC_TMP_GEN_DEFAULT").is_some();`
   `let mut gc_stress = std::env::var_os("KLASSIC_TMP_GEN_STRESS").is_some();`
2. `KLASSIC_TMP_GEN_DEFAULT=1 cargo test --test cli_smoke`, then again with
   `KLASSIC_TMP_GEN_STRESS=1` (skip
   `native_deep_recursion_with_per_frame_alloc_runs` under stress; it is
   quadratic because every allocation evacuates). Expect ~16 failures out of
   511, **all intended**: C-backend rejections, `--gc-poison` rejections, and
   tests that read the legacy `gc: collections=` log format. Any other
   failure is a bug.
3. **Remove the hook** (`grep -c TMP-GEN src/cli.rs` must print 0).
4. Threaded changes: run the worker test programs 50x plain, 50x stress,
   20x each under `taskset -c 0`; every run must match the evaluator and
   `objects == published`.

---

## 6. Traps already paid for (do not rediscover)

- `alloc_raw`/`forward` clobber the interrupted allocation's request
  (`alloc_size`, `elements`, shape tag, descriptor). `collect` saves/restores
  all four. A missed one allocated a record as raw bytes and lost its
  referents (nested `Some(Some("hi"))` under stress).
- Forwarding **must** be limited to the current from-space ranges, or objects
  already in to-space get copied again and a pending (unpublished) object ends
  up forwarded.
- RootCell `RESERVE` never reissues a cell -> LIFO reuse or loops exhaust
  the segment. 2^16 cells was too few for 40000-deep recursion; it is 2^20.
- Publishing needs ENTER/publish/EXIT per object; a held guard makes
  `try_tlab_allocate`, root steps and TLAB bind fail with INVALID.
- Constant arguments are partially evaluated away at compile time; a
  garbage-heavy test must use a loop-variable-dependent size.
- A fresh `Runtime` field must be added to `offsets.rs` and the `Runtime`
  struct together; a stale offset reads garbage silently.
- A register-flag bug on AArch64: `sub; jb` patterns need real flags (`FlagAsm`).
- Printing a heap string must copy the bytes to a native buffer first; the
  heap can move under a blocking `write`.
- Evaluating something with `println` of a raw managed pointer across an
  allocation point is the classic bug class: re-derive addresses from a root
  after every allocation.
- `__gc_write` is produced only by the enum desugar *after* type checking.
  There is no heap-field assignment in the language, so SATB's non-null
  overwrite branch is unreachable from Klassic source (hence the harness).
- In debug builds compiling `tests/cross-exec/36-stdlib-system-modules.kl`
  for AArch64 trips `debug_assert!(imm < 4096)` at `aarch64.rs:572`
  (pre-existing; release builds are fine).

---

## 7. Honest scoreboard (details in `docs/gc/g01-g15-status.md`)

| Row | Status | Biggest gap |
|---|---|---|
| G01 Young/Old, age, promotion | partial | fixed 2-survival age; `object_id` not preserved across moves |
| G02 N mutators + workers | partial | 1 mutator, 1 worker; none on PE/Mach-O |
| G03 concurrent marking, both generations | partial | marking does not drive evacuation; SATB only via harness; no per-generation marks |
| G04 concurrent relocation/compaction | partial | semispaces, not liveness-selected pages; two pauses remain |
| G05 generational colored refs + load barrier | partial | heap fields are raw addresses (range rule); no per-generation remap epochs |
| G06 store barrier | partial | tested through harness only; no color/epoch; no per-mutator buffers |
| G07 double remsets | partial | flip inside the pause; no per-mutator buffers/epochs |
| G08 concurrent root processing | **open** | all roots forwarded inside the pause |
| G09 Old mark vs Young collection | **open** | full collections are stop-the-world flips |
| G10 heap/TLAB/metadata | partial | fixed sizes; no hard/soft max, large objects (>128 KiB), uncommit, pre-touch |
| G11 exhaustion/progress | partial | no relocation reserve, no allocation stall |
| G12 adaptive control | **open** | fixed quantum 64, fixed triggers, 1 worker |
| G13 native boundary | **open** | only print copy-out; no attach/detach, handles, scoped pin |
| G14 weak handles | **open** | none |
| G15 backend parity/diagnostics | partial | C and LLVM unsupported; no real arm64; no measurements |

Spec section 11 (latency measurement) and section 12 (final report mapping
each G-row to implementation path + test + result) are **not started**.

---

## 8. Recommended next work, in dependency order

1. **Make marking drive evacuation** (G03/G04): select from-space pages or
   skip dead objects by the mark bitmap; reconcile with the semispace layout.
2. **G05**: typed/colored heap references. Options: make field access typed
   (stop using the untyped funnel for generic payloads), or tag reference
   words in heap fields and teach the scalar funnel to leave tagged words
   alone. Then per-generation remap epochs.
3. **G08/G09**: scan roots concurrently (handshake per mutator) and keep an
   Old mark alive across Young cycles (spec 5.3 phase table).
4. **G02**: more than one mutator thread. Reuse
   `gc_codegen/{handshake,threads,context}.rs` and the legacy N-mutator design
   in `docs/true-zgc-plan.md`. Needs per-mutator TLAB, root segments, SATB and
   remset buffers; replace the ticket lock where possible.
5. **G12/G13/G14**: scheduler (start time, generation sizes, tenuring,
   worker count), native boundary (attach/detach, blocked state, handles,
   scoped pin), runtime weak handles.
6. **G10/G11**: hard/soft max, large objects, uncommit, pre-touch,
   relocation reserve, allocation stall.
7. **G15**: LLVM and C backends; a real arm64 run; workers + marking on
   Windows and Mach-O; the spec section 11 measurements; then section 12.

For each: write the failing test first (a named test is the unit of
evidence), implement, run the verification recipe, update
`docs/gc/g01-g15-status.md` in the same commit.

---

## 9. Decisions waiting on the owner (Kota)

- **Integration is done**, by Kota's instruction: `gc/generational-wip` was
  fast-forwarded into `main` and pushed directly (20 commits; the diff against
  `9346bff` is 511 files / ~159k lines, almost all of it the earlier
  component work preserved in the WIP checkpoint `0d674e8`; this session's
  own changes are the commits after it). Full `cargo test` (26 suites),
  fmt and clippy were green on `main` at `5c17545`. It bypassed the PR rule
  noted in section 5, so reviewers have not seen it; expect to answer
  review-style questions about it.
- **AI-only-grown language.** Kota wants Klassic to be developed by AI only
  once the GC is stable, modelled on `onion-lang/onion` (README/AGENTS.md/
  CLAUDE.md there describe an "evolution loop": hourly Maintainer, half-hourly
  Reviewer, daily Sensor, weekly Strategist; PRs integrate only via
  auto-merge behind required checks and a review panel; `risk:ultrahigh`
  changes get a 48 h veto window; an "episode" is written to an
  `evolution-memory` repo each run; never push to `develop`/`main` directly).
  Practical prerequisites: `--gc=generational` in CI, the evaluator-vs-native
  differential test as an automatic gate, and the integration decision above.
  Do not start this without Kota.
- Language change: a heap-field assignment (mutable record/enum fields) would
  make SATB testable from source but touches the evaluator, checker and every
  backend. Kota chose **not** to add it for now.

---

## 10. Known soft spots (be suspicious here first)

- The thread-creation fallback is untested (no way to force `clone` to fail).
- CAS-loser path is lightly exercised (1 lost race across 140 runs).
- Stress runs never mark (a relocation phase is always open), so mark +
  stress interactions are untested.
- `--gc-log` verify takes the ticket lock and finishes the phase; it must not
  be used to observe a running phase.
- The heap-field range rule can misclassify a scalar that happens to look
  like a heap address (accepted; same rule as the legacy collector).
- Memory ordering was reasoned about, not model-checked. Run the race loops
  after any change under `worker.rs`/`copy.rs`/`scan.rs`/`mark.rs`.
- AArch64 has no copy-out print buffer (writes heap bytes to the kernel
  directly) and no full-collection assertion.
