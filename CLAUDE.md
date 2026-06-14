# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Klassic is a statically typed object-functional programming language implemented
as a Rust 2024 Cargo workspace. The default developer path is Cargo-based and
builds the `klassic` executable. The language has Hindley-Milner inference,
row-polymorphic records, type classes (incl. higher-kinded examples), and a
lightweight theorem / trust / axiom surface.

## Common Commands

```bash
cargo build                                          # debug build
cargo build --release                                # release build
cargo test                                           # full test suite
cargo test --test cli_smoke <test_name>              # single integration test
cargo test -p klassic-macro-peg                      # macro-PEG crate only
cargo fmt --check                                    # formatting gate
cargo clippy --all-targets --all-features -- -D warnings
cargo run -- -e "1 + 2"                              # evaluate an expression
cargo run -- path/to/program.kl                      # evaluate a file
cargo run -- -f path/to/program.kl                   # equivalent to above
cargo run                                            # REPL (`:history`, `:exit`)
cargo run -- build path/to/program.kl -o program     # native build (host-detected target)
cargo run -- targets                                  # list known native targets
cargo run -- --backend c build program.kl -o program.c  # portable C backend (subset)
cargo run -- --target x86_64-unknown-linux-gnu build path/to/program.kl -o program
cargo run -- --warn-trust path/to/program.kl         # report trusted proofs
cargo run -- --deny-trust path/to/program.kl         # reject trusted proofs
```

## Architecture

### Compiler Pipeline

1. Source text
2. `klassic-span`: source files, spans, diagnostics
3. `klassic-syntax`: lexer, parser, untyped AST
4. `klassic-rewrite`: placeholder desugaring and syntax normalization
5. `klassic-types`: HM inference, record typing, typeclass constraints, proof checks
6. `klassic-eval`: evaluator, runtime builtins, modules, REPL/session state
7. `klassic-native`: native compiler — emits ELF64 for Linux x86_64 AND ad-hoc-signed Mach-O arm64 for Apple Silicon, both without `cc`/`as`/`ld`
8. Root `src/`: CLI argument handling and diagnostic presentation

The evaluator (`klassic-eval`) is the reference implementation. The native
compiler (`klassic-native`) reuses parse → rewrite → typecheck → proof analysis
and lowers a growing subset of programs to ELF executables. When a construct is
not yet supported by native codegen, it fails at build time with a
source-located diagnostic — there is no fallback to the evaluator.

### Crates

- `crates/klassic-span` — spans / diagnostics
- `crates/klassic-syntax` — parser + AST
- `crates/klassic-rewrite` — rewrite passes
- `crates/klassic-types` — static checking
- `crates/klassic-eval` — evaluator + builtins + REPL state
- `crates/klassic-native` — native code generator + ELF writer, and Mach-O arm64 writer (Apple Silicon)
- `crates/klassic-runtime` — shared runtime crate scaffold
- `crates/klassic-macro-peg` — standalone macro PEG parser/evaluator

### Tests

- Rust unit tests live inside each crate.
- Integration tests under `tests/`:
  - `tests/cli_smoke.rs` — CLI + native build behavior (largest; one test per scenario, often with temp `.kl` source and ELF output).
  - `tests/sample_programs.rs` — runs every program in `test-programs/` through both the evaluator and the native compiler when on Linux x86_64.
  - `tests/language_regressions.rs` — language-level regression suite.
- Klassic sample programs live under `test-programs/` (and `examples/`).
- `klassic-native` integration tests are gated: the ELF path uses `#[cfg(all(target_os = "linux", target_arch = "x86_64"))]` and the Mach-O path uses `#[cfg(all(target_os = "macos", target_arch = "aarch64"))]`.

### Roadmap

Long-term direction — multi-target native backends and a shared
standard library between evaluator and native — is captured in
`docs/roadmap-targets-stdlib.md`. The PR-sized execution plan that
sits alongside it is `klassic_claude_code_plan.md`. Read both before
starting work that touches the native target abstraction, the stdlib
module layout, or the builtin registry.

## Native Compiler Development Pattern

Most recent commit history is a long stream of small, focused additions to
`klassic-native`, each titled "Support X" or "Cover X" with one new
integration test in `tests/cli_smoke.rs` and a one-paragraph addendum to
`docs/architecture-rust.md`. When extending native coverage:

1. Probe with a small `.kl` snippet through `cargo run -- build` to find a gap.
2. Add the minimal native codegen change in `crates/klassic-native/src/lib.rs`.
3. Add a focused integration test in `tests/cli_smoke.rs` that asserts the generated executable's stdout/stderr (use temp paths keyed on `SystemTime`).
4. Update `docs/architecture-rust.md` with one or two sentences describing the new path.
5. Run `cargo fmt --check` and `cargo test`.

`crates/klassic-native/src/lib.rs` is intentionally a single very large file
(~40k lines). Stay consistent with that organization rather than splitting it.
Prefer `unsupported(span, "<feature>")` returning a `Diagnostic` for paths that
remain unimplemented.

## Workflow For Language Changes

When adding syntax or semantics:

1. Update `klassic-syntax` for parsing and AST shape.
2. Add or adjust rewrite behavior in `klassic-rewrite` when needed.
3. Extend `klassic-types` for static behavior.
4. Extend `klassic-eval` for evaluator behavior.
5. Extend `klassic-native` for native codegen (or leave it unsupported with a clear diagnostic).
6. Add focused tests in the relevant crate plus integration tests where the user-visible surface changes.
7. `cargo fmt --check && cargo test`.

## Conventions

- Rust 2024 edition. Avoid `unsafe` unless documented.
- Keep diagnostics source-span aware end-to-end.
- Tests must be hermetic; use temp directories for filesystem behavior. Do not hardcode sample outputs in the evaluator.
- Default to ASCII in source and docs unless the file already justifies Unicode.
- Prefer `rg` for source search.
- The default build and runtime path is native Rust — keep it that way.
- Commit subjects: imperative mood, under ~72 characters.

## Language Surface (quick reference)

- `val` (immutable) / `mutable` (reassignable) bindings.
- `def f(x) = ...` and `(x) => ...` lambdas; placeholders like `_ + 1`. Top-level defs may forward-reference each other (mutual recursion); `else` may start a continuation line.
- Space- / comma- / newline-separated collection literals: `[1 2 3]`, `%["a":1 "b":2]`, `%(1 2 3)`.
- String interpolation: `"Hello #{name}"`.
- `cleanup { ... }` clauses run after the associated expression.
- `module foo.bar { ... }` plus selective / aliased imports.
- Structural records (`record { x: 1; y: 2 }`) and nominal record declarations (`record Point { x: Int; y: Int }`), constructed positionally as `#Point(1, 2)`.
- Algebraic data types: `enum Option<a> { case Some(value: a); case None }` and Scala-style postfix pattern matching (`o match { case Some(v) => v; case None => 0 }`). Enums are real nominal types in the checker (match exhaustiveness and unreachable arms are diagnosed), and native builds compile monomorphic and shape-tracked generic enums — including recursion — through a per-frame by-pointer ABI; remaining native gaps (e.g. list-of-enum payloads) fail with source-located diagnostics.
- Extension methods: `extension <a>(this: List<a>) { def headOr(d) = ... }` adds dot-callable methods to existing types. Stdlib leans on this for `std.string`, `std.list`, `std.math`, `std.option`, `std.result`, `std.map`, `std.set`, `std.time`, `std.json`, `std.path`, `std.cli`, `std.dir`, `std.env`, `std.file`, `std.process`, `std.test`.
- Type classes with constraints, including higher-kinded examples.
- Proof surface: `axiom`, `theorem`, with `--warn-trust` / `--deny-trust` flags.
