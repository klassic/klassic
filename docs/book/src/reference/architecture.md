# Architecture Overview

Klassic is implemented as a Rust workspace. The compiler pipeline is
broken into small crates, each with a single responsibility. The full
internal docs live at
[`docs/architecture-rust.md`](https://github.com/klassic/klassic/blob/main/docs/architecture-rust.md)
in the source tree; this page summarises the top-level shape.

## Pipeline

```
source.kl
   ↓
klassic-syntax     (lexer + parser + AST)
   ↓
klassic-rewrite    (placeholder desugaring + normalization)
   ↓
klassic-types      (Hindley-Milner inference + records + type classes + proofs)
   ↓
klassic-eval       (evaluator + builtins + REPL)
   ↓ (only for `klassic build`)
klassic-native     (x86_64 codegen + ELF64 / Mach-O arm64 / PE64 writers)
```

## Crates

| Crate | Purpose |
|---|---|
| `klassic-span` | Spans and human-readable diagnostics. |
| `klassic-syntax` | Lexer, parser, AST. |
| `klassic-rewrite` | Placeholder desugaring and normalization. |
| `klassic-types` | Type checking, type classes, proof checks. |
| `klassic-eval` | Evaluator, runtime builtins, REPL state. |
| `klassic-native` | x86_64/arm64 codegen and the ELF64, Mach-O arm64, and PE64 writers. |
| `klassic-runtime` | Shared runtime crate scaffold (work in progress). |
| `klassic-macro-peg` | Standalone Rust macro PEG implementation. |

## The native compiler

`klassic-native` reuses the parse → rewrite → type-check → proof
pipeline and lowers a growing subset of the resulting AST directly
to machine code:

- Direct OS-level I/O with no `libc`, `cc`, `as`, `ld`, or `codesign`
  involvement — raw syscalls on Linux and macOS, Win64 `kernel32.dll`
  import calls on Windows.
- Precise mark-and-sweep GC with multi-segment heap growth (see
  [Why a GC?](../gc/why.md)).
- Fixed-buffer representations for runtime strings / lists / records
  that the GC migration plan will replace with heap allocations.

When the native compiler hits an unsupported construct it emits a
source-located diagnostic and exits non-zero. There is no runtime
fallback to the evaluator.
