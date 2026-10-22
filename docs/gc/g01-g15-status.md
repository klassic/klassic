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
| G03 | Concurrent marking, both generations | partial | `production/mark.rs`: bitmap, worker trace, SATB mark of overwritten refs | `generational_worker_marks_concurrently_and_claims_copies_by_cas`; `satb_prebarrier_marks_the_overwritten_referent`, `without_the_prebarrier_the_same_overwrite_loses_the_object`, `overwriting_null_scalars_or_outside_a_cycle_marks_nothing` (harness) | marking does not drive evacuation (copy is reachability-driven); SATB is only exercised by the machine-code harness, never by a Klassic program; no per-generation marking |
| G04 | Concurrent relocation/compaction | partial | `production/{copy,scan}.rs`: CAS-claimed copies, worker-private to-space, load-barrier relocation + heal | same test; race loops 120/120 correct | pages are semispaces, not selected by liveness; two pauses remain; copy loser path lightly tested |
| G05 | Generational colored refs + load barrier | partial | single-color references in roots; `field_load` barrier for from-space ranges | `generational_enums_survive_evacuation`, stress-default `cli_smoke` | no per-generation remap epochs; heap fields hold raw addresses (range rule), not colored refs |
| G06 | Generational store barrier | partial | `field_store` marks the overwritten value (SATB) and remembers Old->Young slots (`production/{enums,remset}.rs`) | harness: `satb_prebarrier_marks_the_overwritten_referent` (+ negative control `without_the_prebarrier...`), `only_old_to_young_stores_register_and_a_slot_registers_once` (Old<-Young registers; null, non-heap word, Old referent, Young holder do not); `generational_promotes_long_lived_data_and_collects_old` | tested only through a hand-written harness program, not Klassic source; no color/epoch check; no per-mutator buffers |
| G07 | Double remembered sets | partial | `production/remset.rs`: two arrays, flip at Young collection start, previous consumed as roots, kept entries re-registered, slot bitmap dedupe, full collection discards both | harness: `entries_flip_survive_while_young_and_are_dropped_when_old_or_dead`, `an_entry_of_a_dead_holder_is_dropped_by_a_complete_mark`, `a_full_collection_discards_both_sets_and_new_stores_use_the_moved_slot`, `stores_racing_with_the_worker_keep_the_sets_bounded_and_the_values_alive`; source level: `remembered_sets_flip_once_per_young_collection`, `remembered_sets_stay_bounded_under_stress_and_full_collections_discard_them` | the flip happens in the pause by the one mutator, so there are no per-mutator buffers, epochs or concurrent consumption; Old->Old slot remap is not needed (Old moves only in a full collection, which discards the sets); AArch64/Mach-O keeps its own single set |
| G08 | Concurrent root processing | open | roots (RootCells, literal cells, remset) forwarded inside the pause | - | pause is O(roots); frames are one thread; no concurrent stack scan |
| G09 | Old mark vs Young collection | open | full collections are stop-the-world flips | - | no Old mark preserved across Young cycles |
| G10 | Heap/TLAB/metadata | partial | emitted `alloc_into` TLAB, 2^20 root cells, fixed 64+128 MiB heap | `generational_heap_exhaustion...`-class tests | fixed sizes; no hard/soft max, large objects (>128 KiB is an error), uncommit or pre-touch |
| G11 | Exhaustion and progress | partial | honest OOM when live data exceeds a semispace/Old half | `generational_live_data_beyond_a_semispace_is_an_honest_oom` | no relocation reserve, no allocation stall |
| G12 | Adaptive control, many workers | open | fixed quantum (64), fixed trigger (semispace full) | - | no scheduler, one worker (the spec says one worker / fixed age is not G12) |
| G13 | Native boundary | open | print path copies out of the heap; no handles/pins | `generational_prints_multi_chunk_heap_strings` | no attach/detach, blocked state, handles or scoped pin |
| G14 | Runtime weak handles | open | - | - | none |
| G15 | Backend parity and diagnostics | partial | Linux x86-64, Windows x86-64, AArch64 Mach-O (single-thread, no worker) | `generational_windows_build_matches_the_evaluator_when_runnable`, AArch64 emulator tests, `--gc-log` | C and LLVM backends unsupported; no real-hardware arm64 or latency measurement |

## Counts

Met: 0. Partial: 10 (G01, G02, G03, G04, G05, G06, G07, G10, G11, G15). Open: 5 (G08, G09, G12, G13, G14).

## What this does not claim

No latency numbers have been measured (spec section 11). The mutator still
pauses for the flip and root forwarding. The ticket lock remains for pauses,
phase finish and the SATB slow path. The spec's final completion report
(section 12) cannot be filed until every row is met with a named test and a
measurement.

## SATB is reached only through a machine-code harness (2026-10-23)

`field_store` runs on every `__gc_write`, but those are emitted only by the
enum constructor, which writes into a freshly allocated record whose slots are
null, so the SATB branch that marks a non-null overwritten reference never
executes for a Klassic program (`__gc_write` is a post-typecheck desugar
product; the language has no assignment to a heap field). The language stays
unchanged; instead `crates/klassic-native/src/gc_barrier_harness_tests*`
compiles the real production runtime around a hand-written instruction
sequence and observes mark bits, counters and remembered-set contents (see
`concurrent-plan.md`, "G06/G07 as implemented"). Mutation checks done while
writing it: removing the SATB mark, the slot-bit dedupe, the re-registration
of kept entries, and the full-collection discard each fail a named test.

## Next in order of dependency

1. G06/G03/G07: done to the extent above; remaining are per-mutator buffers,
   epochs and a SATB path reachable from source (needs mutable fields).
2. G05: per-generation colored references in heap fields.
3. G08/G09: concurrent root scan, Old mark across Young cycles.
4. G12/G13/G14: scheduler, native boundary, weak handles.
5. G10/G11, then measurement (section 11) and the final audit.
