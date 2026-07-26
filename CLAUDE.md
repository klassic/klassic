# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Klassic is a statically typed object-functional programming language implemented
as a Rust 2024 Cargo workspace (crate version 0.9.0). The `klassic` executable is
both an evaluator/REPL and a native compiler that writes executables **byte by
byte** — ELF64 on Linux x86_64, ad-hoc-signed Mach-O on Apple Silicon, PE64 on
Windows x86_64 — with no `cc`/`as`/`ld`/`codesign`/`link.exe` in the loop. The
language has Hindley-Milner inference, algebraic data types with exhaustive
`match`, row-polymorphic records, type classes (incl. higher-kinded), a standard
library written in Klassic itself, a precise/moving garbage collector inside
every native binary, and a lightweight theorem / trust / axiom surface.

## Common Commands

```bash
cargo build                                          # debug build
cargo build --release                                # release build
cargo test                                           # full workspace suite
cargo test --test cli_smoke <test_name>              # one integration test in a file
cargo test -p klassic-macro-peg                      # a single crate's tests
cargo fmt --check                                    # formatting gate
cargo clippy --all-targets --all-features -- -D warnings   # lint gate (warnings are errors)
cargo run -- -e "1 + 2"                              # evaluate an expression
cargo run -- path/to/program.kl                      # evaluate a file (also: -f path)
cargo run                                            # REPL (`:history`, `:exit`)
cargo run -- build path/to/program.kl -o program     # native build (host-detected target)
cargo run -- targets                                  # print the live target matrix
cargo run -- --target x86_64-pc-windows-msvc build program.kl -o program.exe   # cross-build
cargo run -- --backend c build program.kl -o program.c     # portable C source (subset)
cargo run -- --backend llvm build program.kl -o program    # LLVM IR -> clang (needs clang >= 15)
cargo run -- --warn-trust program.kl                 # report trusted proofs
cargo run -- --deny-trust program.kl                 # reject trusted proofs
cargo run -- --gc-log build program.kl -o program    # GC stats to stderr at exit
cargo run -- --gc-stress build program.kl -o program # collect before every alloc (bug-shaker)
cargo run -- --gc-poison build program.kl -o program # bad-color heap ptrs; unbarriered deref faults
runtime/gc/run_tests.sh                              # standalone C-GC tests (single + multi-thread)
```

The three GC flags apply to the hand-emitted x86_64 ELF backend. The LLVM/C-GC
tests (`tests/llvm_gc*.rs`, `tests/llvm_backend.rs`) are gated on a `clang >= 15`
being present (set `KLASSIC_CLANG` to pick one); without clang they skip so CI
stays green.

## Architecture

### Compiler Pipeline

1. Source text
2. `klassic-span`: source files, spans, diagnostics
3. `klassic-syntax`: lexer, parser, untyped AST
4. `klassic-rewrite`: placeholder desugaring and syntax normalization
5. `klassic-types`: HM inference, record typing, typeclass constraints, proof checks
6. `klassic-eval`: evaluator, runtime builtins, modules, REPL/session state
7. `klassic-native`: native code generation (multiple backends — see below)
8. Root `src/`: CLI argument handling (`src/cli.rs`) and diagnostic presentation

The evaluator (`klassic-eval`) is the **reference implementation** and semantic
oracle. Every native backend reuses parse → rewrite → typecheck → proof analysis
and lowers a *subset* of programs. When a construct is not yet supported by a
backend, it fails at build time with a source-located `Diagnostic` — there is no
fallback to the evaluator and never wrong code. Differential testing
(`eval == native`) is the correctness backstop.

### Crates

- `crates/klassic-span` — spans / diagnostics
- `crates/klassic-syntax` — parser + AST
- `crates/klassic-rewrite` — rewrite passes
- `crates/klassic-types` — static checking
- `crates/klassic-eval` — evaluator + builtins + REPL state (the oracle)
- `crates/klassic-native` — all native code generation (see backends below)
- `crates/klassic-runtime` — shared runtime crate scaffold
- `crates/klassic-macro-peg` — standalone macro PEG parser/evaluator

### Native backends (`crates/klassic-native/src/`)

Klassic has grown from one hand-emitted backend into several; they are separate
files, not one monolith:

- `lib.rs` — the **default** backend: a hand-emitted x86_64 direct-ELF code
  generator (~40k lines, intentionally one large file). Contains the by-pointer
  enum ABI and the complete hand-written **ZGC-style garbage collector** (colored
  pointers, an inline load barrier, a region heap, incremental marking, and
  moving evacuation). This is where the `--gc-*` flags live. Note: this GC is
  x86_64-only machine code; extracting its architecture-independent parts behind
  a `PortableAsm` emitter trait (so a future AArch64 backend can share the design)
  is an active refactoring direction — see `docs/superpowers/specs/` if present.
- `pe.rs` — PE64 writer (Windows x86_64), reusing `lib.rs`'s x86_64 codegen.
- `macho.rs` + `aarch64.rs` — Mach-O writer + AArch64 codegen (Apple Silicon).
  This is a **wholly separate** backend with no GC of its own.
- `cbackend.rs` — `--backend c`: emits a portable C translation unit (a subset).
- `llvm.rs` — `--backend llvm`: emits textual LLVM IR that `clang` compiles and
  links against `libklassic_runtime.a`, using the **C garbage collector** in
  `runtime/gc/`. Coverage grows milestone by milestone (`docs/llvm-backend-plan.md`);
  the hand-emitted backend stays default and untouched.

### The two garbage collectors

There are two distinct GC implementations — do not confuse them:

- **Hand-written ZGC in `lib.rs`** (x86_64 ELF/PE backend). Design in
  `docs/zgc-plan.md`. Object header is 16 bytes `[size | mark@bit63][type_tag]`;
  pointers are *colored* (bits 60-62) and non-canonical until stripped by the
  load barrier. Observability/debug flags: `--gc-log`, `--gc-stress`, `--gc-poison`.
  The ~25 `emit_gc_*_runtime` functions in `lib.rs` are the hand-emitted machine
  code for this collector.
- **C garbage collector** `runtime/gc/klassic_gc.{c,h}`, driven by the LLVM
  backend through the `klassic_gc_*` ABI (`klassic_gc_init`/`_alloc`/`_write`/
  `_read`/`_collect`/`shadow_push`/`shadow_pop_n`, the inline load-barrier fast
  path, and a `safepoint`/`handshake` protocol for the concurrent, N-mutator
  design). Its own C tests are `gc_test.c` / `gc_mt_test.c` (multi-thread) run by
  `runtime/gc/run_tests.sh`. Design in `docs/true-zgc-plan.md`.

### Tests

- Rust unit tests live inside each crate.
- Integration tests under `tests/`:
  - `tests/cli_smoke.rs` — CLI + hand-emitted native build behavior (largest; one
    test per scenario, usually a temp `.kl` source compiled to an executable whose
    stdout/stderr/exit code is asserted). Use temp paths keyed on `SystemTime`.
  - `tests/sample_programs.rs` — runs every program in `test-programs/` through
    both the evaluator and the native compiler on Linux x86_64.
  - `tests/language_regressions.rs` — language-level regression suite.
  - `tests/llvm_backend.rs`, `tests/llvm_gc.rs`, `tests/llvm_gc_link.rs` — the LLVM
    backend and its C-GC linking (gated on clang; see Common Commands).
  - `tests/cross-exec/` — cross-target execution fixtures.
- Klassic sample programs live under `test-programs/` and `examples/`.
- Native integration tests are cfg-gated by host: ELF path `#[cfg(all(target_os
  = "linux", target_arch = "x86_64"))]`, Mach-O path `#[cfg(all(target_os =
  "macos", target_arch = "aarch64"))]`.

### Docs

- `docs/architecture-rust.md` — running log of the hand-emitted backend's covered
  paths (append one or two sentences per native change).
- `docs/native-coverage.md`, `docs/native-backend-strategy.md` — native scope.
- `docs/zgc-plan.md` — the hand-written x86_64 GC plan (M0-M8, complete).
- `docs/true-zgc-plan.md` — the concurrent/N-mutator C GC plan.
- `docs/llvm-backend-plan.md` — the LLVM backend migration plan.
- `docs/roadmap-targets-stdlib.md` — long-term multi-target + shared-stdlib
  direction. Read it before touching the native target abstraction, the stdlib
  module layout, or the builtin registry.
- `docs/book/` — the user-facing Klassic Book.

## Native Compiler Development Pattern (hand-emitted `lib.rs`)

The commit history is largely small, focused additions to `lib.rs`, each with one
new integration test in `tests/cli_smoke.rs` and a one-paragraph addendum to
`docs/architecture-rust.md`. When extending native coverage:

1. Probe with a small `.kl` snippet through `cargo run -- build` to find a gap.
2. Add the minimal codegen change in `crates/klassic-native/src/lib.rs`.
3. Add a focused test in `tests/cli_smoke.rs` asserting the executable's output.
4. Update `docs/architecture-rust.md` with a sentence or two.
5. `cargo fmt --check && cargo test`.

`lib.rs` is intentionally a single very large file — stay consistent with that
organization rather than splitting existing instruction-emission function bodies.
(New, genuinely separable files alongside it — `aarch64.rs`, `cbackend.rs`,
`llvm.rs`, `pe.rs` — are the normal way to add a separable concern.) Prefer
`unsupported(span, "<feature>")` returning a `Diagnostic` for paths that remain
unimplemented. The same discipline applies to the LLVM backend (`llvm.rs`).

## Workflow For Language Changes

When adding syntax or semantics:

1. Update `klassic-syntax` for parsing and AST shape.
2. Add or adjust rewrite behavior in `klassic-rewrite` when needed.
3. Extend `klassic-types` for static behavior.
4. Extend `klassic-eval` for evaluator behavior (the oracle moves first).
5. Extend the native backends for codegen (or leave unsupported with a clear
   diagnostic).
6. Add focused tests in the relevant crate plus integration tests where the
   user-visible surface changes.
7. `cargo fmt --check && cargo test`.

## Conventions

- Rust 2024 edition. Avoid `unsafe` unless documented.
- Keep diagnostics source-span aware end-to-end.
- Tests must be hermetic; use temp directories for filesystem behavior. Do not
  hardcode sample outputs in the evaluator.
- Default to ASCII in source and docs unless the file already justifies Unicode.
- Prefer `rg` for source search.
- The default build and runtime path is native Rust — keep it that way.
- Commit subjects: imperative mood, under ~72 characters. CI must be green on the
  Rust-native path.

## Language Surface (quick reference)

- `val` (immutable) / `mutable` (reassignable) bindings.
- `def f(x) = ...` and `(x) => ...` lambdas; placeholders like `_ + 1`. Top-level
  defs may forward-reference each other (mutual recursion); `else` may start a
  continuation line.
- Space- / comma- / newline-separated collection literals: `[1 2 3]`,
  `%["a":1 "b":2]`, `%(1 2 3)`.
- String interpolation: `"Hello #{name}"`.
- `cleanup { ... }` clauses run after the associated expression.
- `module foo.bar { ... }` plus selective / aliased imports.
- Structural records (`record { x: 1; y: 2 }`) and nominal record declarations
  (`record Point { x: Int; y: Int }`), constructed positionally as `#Point(1, 2)`.
- Algebraic data types: `enum Option<a> { case Some(value: a); case None }` and
  Scala-style postfix pattern matching (`o match { case Some(v) => v; case None
  => 0 }`). Enums are real nominal types in the checker (match exhaustiveness and
  unreachable arms are diagnosed); native builds compile monomorphic and
  shape-tracked generic enums — including recursion — through a per-frame
  by-pointer ABI.
- Extension methods: `extension <a>(this: List<a>) { def headOr(d) = ... }` adds
  dot-callable methods. The stdlib (written in Klassic) leans on this for
  `std.string`, `std.list`, `std.math`, `std.option`, `std.result`, `std.map`,
  `std.set`, `std.time`, `std.json`, `std.path`, `std.cli`, `std.dir`, `std.env`,
  `std.file`, `std.process`, `std.test`.
- Type classes with constraints, including higher-kinded examples.
- Arithmetic operators are type-class-constrained, so unannotated generic
  arithmetic infers a polymorphic signature: `def add(x, y) = x + y` is
  `(a, a) -> a where Plus<a>` (works at Int / Double / String), and
  `def diff(x, y) = x - y` is `Num<a>` (numbers only). `add(true, false)` is a
  compile error (`missing instance for Plus<Boolean>`).
- Proof surface: `axiom`, `theorem`, with `--warn-trust` / `--deny-trust` flags.
