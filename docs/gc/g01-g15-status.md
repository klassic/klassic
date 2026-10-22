# G01-G15 status against the production `--gc=generational` path

Date: 2026-10-23. Scope of "production path": the opt-in
`--gc=generational` flag (`docs/gc/production-integration-plan.md`,
`docs/gc/concurrent-plan.md`). The spec is
`klassic-generational-zgc-instructions.md` section 1. A capability is marked
**met** only if the production path implements it and a named test passes;
**partial** if a real but narrower form runs; **open** otherwise. Component
work under `gc_codegen/` and `runtime/gc/` that no ordinary program reaches
does not count here (see `capabilities.md` for that inventory).

| ID | Capability | Status | Implementation path | Evidence | Gap |
|---|---|---|---|---|---|
| G01 | Young/Old, age, promotion | partial | `production/{collect,enums}.rs`: two Young semispaces, Old halves, age in `MarkRecord`, promotion on second survival | `generational_promotes_long_lived_data_and_collects_old` | age is a fixed 2-survival rule; identity (`object_id`) is not preserved across moves |
| G02 | N mutators + GC workers | partial | `production/worker.rs`: one clone()d worker thread | `generational_worker_thread_relocates_concurrently` (+50 plain/50 stress/20 one-CPU runs) | exactly one mutator and one worker; no 2/8-mutator runs; Windows/Mach-O have no worker |
| G03 | Concurrent marking, both generations | partial | `production/mark.rs`: bitmap, worker trace, SATB mark of overwritten refs | `generational_worker_marks_concurrently_and_claims_copies_by_cas` | marking does not drive evacuation (copy is reachability-driven); SATB path unexercised by any program; no per-generation marking |
| G04 | Concurrent relocation/compaction | partial | `production/{copy,scan}.rs`: CAS-claimed copies, worker-private to-space, load-barrier relocation + heal | same test; race loops 120/120 correct | pages are semispaces, not selected by liveness; two pauses remain; copy loser path lightly tested |
| G05 | Generational colored refs + load barrier | partial | single-color references in roots; `field_load` barrier for from-space ranges | `generational_enums_survive_evacuation`, stress-default `cli_smoke` | no per-generation remap epochs; heap fields hold raw addresses (range rule), not colored refs |
| G06 | Generational store barrier | partial | `field_store` remembers Old->Young slots, marks overwritten value | `generational_promotes_long_lived_data_and_collects_old` | overwrite SATB untested (no program overwrites a heap reference); no color/epoch check |
| G07 | Double remembered sets | open | single remset array (`REMSET_ENTRIES`) | - | no current/previous sets, no flip-time buffers, entries move with objects only via rescan |
| G08 | Concurrent root processing | open | roots (RootCells, literal cells, remset) forwarded inside the pause | - | pause is O(roots); frames are one thread; no concurrent stack scan |
| G09 | Old mark vs Young collection | open | full collections are stop-the-world flips | - | no Old mark preserved across Young cycles |
| G10 | Heap/TLAB/metadata | partial | emitted `alloc_into` TLAB, 2^20 root cells, fixed 64+128 MiB heap | `generational_heap_exhaustion...`-class tests | fixed sizes; no hard/soft max, large objects (>128 KiB is an error), uncommit or pre-touch |
| G11 | Exhaustion and progress | partial | honest OOM when live data exceeds a semispace/Old half | `generational_live_data_beyond_a_semispace_is_an_honest_oom` | no relocation reserve, no allocation stall |
| G12 | Adaptive control, many workers | open | fixed quantum (64), fixed trigger (semispace full) | - | no scheduler, one worker (the spec says one worker / fixed age is not G12) |
| G13 | Native boundary | open | print path copies out of the heap; no handles/pins | `generational_prints_multi_chunk_heap_strings` | no attach/detach, blocked state, handles or scoped pin |
| G14 | Runtime weak handles | open | - | - | none |
| G15 | Backend parity and diagnostics | partial | Linux x86-64, Windows x86-64, AArch64 Mach-O (single-thread, no worker) | `generational_windows_build_matches_the_evaluator_when_runnable`, AArch64 emulator tests, `--gc-log` | C and LLVM backends unsupported; no real-hardware arm64 or latency measurement |

## Counts

Met: 0. Partial: 9 (G01, G02, G03, G04, G05, G06, G10, G11, G15). Open: 6 (G07, G08, G09, G12, G13, G14).

## What this does not claim

No latency numbers have been measured (spec section 11). The mutator still
pauses for the flip and root forwarding. The ticket lock remains for pauses,
phase finish and the SATB slow path. The spec's final completion report
(section 12) cannot be filed until every row is met with a named test and a
measurement.

## SATB cannot be reached from a Klassic program (2026-10-23)

`field_store` runs on every `__gc_write`, but those are emitted only by the
enum constructor, which writes into a freshly allocated record whose slots are
null, so the SATB branch that marks a non-null overwritten reference never
executes. `__gc_write` is a post-typecheck desugar product and cannot be
written in source (`undefined variable`); the language has no assignment to
a heap field. Exercising SATB therefore needs either a language feature
(mutable record/enum fields, which touches the evaluator, checker and every
backend) or a machine-code harness; the decision for this project was to keep
the language unchanged, so the SATB pre-barrier is **untested** and G06/G03
cannot move past partial until a harness exists.

## Next in order of dependency

1. G06/G03: a program that overwrites a heap reference, to exercise SATB.
2. G07: current/previous remembered sets.
3. G05: per-generation colored references in heap fields.
4. G08/G09: concurrent root scan, Old mark across Young cycles.
5. G12/G13/G14: scheduler, native boundary, weak handles.
6. G10/G11, then measurement (section 11) and the final audit.
