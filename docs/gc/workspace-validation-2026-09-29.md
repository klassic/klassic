# Workspace regression validation — 2026-09-29

This earlier snapshot predates native heap/bootstrap and OS decommit changes.
See [the subsequent full sweep](workspace-validation-native-decommit-2026-09-29.md)
for the later tested revision.

The earlier worktree passed the broad Linux regression sweep: **797 tests
passed, zero failed, zero ignored**. This is regression evidence for the current
implementation, not certification of Generational ZGC or a release claim.

Environment: Linux 6.18.33.2-microsoft-standard-WSL2, x86_64; rustc 1.96.0
(ac68faa20 2026-05-25), cargo 1.96.0 (30a34c682 2026-05-25).
HEAD is `9346bff7ecb9a29e26efed17069567ba29cc91c1`; the tested GC changes are
uncommitted worktree changes, not the behavior of that baseline commit alone.

## Commands

| Command | Exit | Evidence |
|---|---:|---|
| `cargo build -p klassic-runtime` | 0 | Runtime static library available before C backend/link tests |
| `cargo clippy --all-targets --all-features -- -D warnings` | 0 | No warnings; 9.96 s |
| `cargo test --workspace` | 0 | 797 passing tests below |
| `cargo fmt --check` | 0 | Formatting passes |
| `python3 tools/gc_access_inventory.py --check` | 0 | 26,685 candidates / 429 source files |
| `git diff --check` | 0 | No whitespace errors |

The runtime build was confirmed while the workspace command was running and
before its C/LLVM link suites. The workspace process was polled to completion;
it was not restarted on observation timeouts. Raw session logs are
`/tmp/klassic-gc-workspace-20260929.log`,
`/tmp/klassic-gc-runtime-build-20260929.log`, and
`/tmp/klassic-gc-clippy-20260929.log`.

## Nonempty suites

| Suite | Passed | Failed | Ignored | Reported time |
|---|---:|---:|---:|---:|
| unittests src/main.rs | 19 | 0 | 0 | 0.00s |
| tests/book_examples.rs | 1 | 0 | 0 | 2.95s |
| tests/cli_smoke.rs | 511 | 0 | 0 | 27.20s |
| tests/gc_abi.rs | 8 | 0 | 0 | 0.45s |
| tests/language_regressions.rs | 44 | 0 | 0 | 0.02s |
| tests/llvm_backend.rs | 11 | 0 | 0 | 2.27s |
| tests/llvm_gc.rs | 1 | 0 | 0 | 168.14s |
| tests/llvm_gc_link.rs | 1 | 0 | 0 | 0.99s |
| tests/sample_programs.rs | 7 | 0 | 0 | 6.28s |
| klassic_eval | 35 | 0 | 0 | 0.31s |
| klassic_macro_peg | 4 | 0 | 0 | 0.00s |
| klassic_native | 73 | 0 | 0 | 42.84s |
| klassic_rewrite | 3 | 0 | 0 | 0.00s |
| klassic_runtime | 17 | 0 | 0 | 0.00s |
| klassic_span | 2 | 0 | 0 | 0.00s |
| klassic_syntax | 30 | 0 | 0 | 0.00s |
| klassic_types | 30 | 0 | 0 | 0.01s |

Eight doc-test targets contained zero tests. They contribute no correctness
coverage to the total. The `llvm_gc` test executes mandatory C ASan/UBSan and
TSan suites and verifies their markers; its Rust count of one is not the number
of C cases.

## Limits and remaining gates

- Default native and C collectors still use their legacy 16-byte headers
  (`crates/klassic-native/src/gc_layout.rs`, `runtime/gc/klassic_gc.h`). The
  32-byte offset-reference collector is not activated in normal compilation.
- The Windows worker test on this host uses explicit OS API models, as stated
  in `gc_windows_tests.rs`; it is not Windows kernel/loader execution. Mach-O
  fixture generation is not actual Darwin execution.
- No dedicated-machine latency/throughput/memory matrix was measured. CLI pause
  regression checks do not establish the required p99.9 target.
- Planned `tests/gc_generational.rs`, `tests/gc_differential.rs` and
  `tests/gc_latency.rs` are absent. Existing component/model tests are not
  substitutes for the completed collector's end-to-end gates.
- This sweep does not complete G01–G15. Continue default runtime/compiler
  integration, owner/event routing and full collector scheduling, followed by
  actual cross-platform and performance validation.
