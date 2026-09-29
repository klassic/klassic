# Workspace validation after native decommit — 2026-09-29

The worktree after heap bootstrap, bitmap ownership/retirement, shared source
cleanup and direct OS decommit changes passes **797 tests, zero failed, zero
ignored**. Parameterized C/native cases grew within existing Rust tests; the
unchanged Rust test count does not mean the earlier and current snapshots are
identical. This is regression evidence, not completion of Generational ZGC.

Environment: Linux 6.18.33.2-microsoft-standard-WSL2 x86_64; rustc 1.96.0
(ac68faa20 2026-05-25), cargo 1.96.0 (30a34c682 2026-05-25). HEAD remains
`9346bff7ecb9a29e26efed17069567ba29cc91c1`; the tested implementation includes the
uncommitted worktree changes.

| Command | Exit | Result |
| --- | ---: | --- |
| `cargo build -p klassic-runtime` | 0 | Completed before workspace tests |
| `cargo test --workspace` | 0 | All 797 tests passed |
| `cargo clippy --all-targets --all-features -- -D warnings` | 0 | No warnings; 12.25 s |
| `cargo fmt --check` | 0 | Passed |
| `python3 tools/gc_access_inventory.py --check` | 0 | 26,985 candidates / 436 files |
| `git diff --check` | 0 | Passed |

## Nonempty suites

| Suite | Passed | Failed | Ignored | Reported time |
| --- | ---: | ---: | ---: | ---: |
| unittests src/main.rs | 19 | 0 | 0 | 0.00s |
| tests/book_examples.rs | 1 | 0 | 0 | 3.31s |
| tests/cli_smoke.rs | 511 | 0 | 0 | 27.73s |
| tests/gc_abi.rs | 8 | 0 | 0 | 0.46s |
| tests/language_regressions.rs | 44 | 0 | 0 | 0.02s |
| tests/llvm_backend.rs | 11 | 0 | 0 | 2.26s |
| tests/llvm_gc.rs | 1 | 0 | 0 | 174.76s |
| tests/llvm_gc_link.rs | 1 | 0 | 0 | 0.99s |
| tests/sample_programs.rs | 7 | 0 | 0 | 6.14s |
| klassic_eval | 35 | 0 | 0 | 0.33s |
| klassic_macro_peg | 4 | 0 | 0 | 0.00s |
| klassic_native | 73 | 0 | 0 | 44.61s |
| klassic_rewrite | 3 | 0 | 0 | 0.00s |
| klassic_runtime | 17 | 0 | 0 | 0.00s |
| klassic_span | 2 | 0 | 0 | 0.00s |
| klassic_syntax | 30 | 0 | 0 | 0.00s |
| klassic_types | 30 | 0 | 0 | 0.00s |

Eight doc-test targets contain zero tests and add no coverage to this count.
The `llvm_gc` Rust test requires C ASan/UBSan and TSan cases and their markers;
its count of one is not the count of C cases. The native suite includes actual
Linux generated decommit, Windows API-model execution and Mach-O generation.
The prior focused ARM emulation evidence remains separately recorded in
[capabilities](capabilities.md); workspace Cargo tests do not execute that script.

Raw logs: `/tmp/klassic-os-decommit-workspace.log`,
`/tmp/klassic-os-decommit-runtime-build.log`, and
`/tmp/klassic-os-decommit-clippy.log`. All associated tool sessions completed;
none were restarted on an observation timeout.

## Scope still open

[The default integration audit](default-integration-audit-2026-09-29.md) verifies
that normal allocation/root lowering and C/LLVM entry points still use the legacy
representation. A production startup owner, connected rooted allocation/access
lowering and full cycle scheduling remain unfinished. The planned generational,
differential and latency end-to-end test files are still absent.

No actual Windows/Darwin execution or dedicated latency/throughput/RSS matrix is
established by this Linux sweep. It does not certify G01–G15, change the default
collector, or complete the active implementation objective.
