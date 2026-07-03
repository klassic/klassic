# Native Compiler Coverage

The native compiler lowers a growing slice of the language directly to
machine code for three tier-0 targets: direct ELF64 for Linux x86_64
(`linux-x86_64` / `x86_64-unknown-linux-gnu`), direct ad-hoc-signed
Mach-O arm64 for Apple Silicon macOS (`macos-aarch64` /
`aarch64-apple-darwin`), and direct PE64 for Windows x86_64
(`windows-x86_64` / `x86_64-pc-windows-msvc`). A target-less `build`
compiles for the detected host — including Windows hosts, which now
default to the direct PE64 backend. `klassic targets` lists the full
matrix, including the still-planned tier 1/2 targets.
The exhaustive feature matrix lives in
[`docs/native-coverage.md`](https://github.com/klassic/klassic/blob/main/docs/native-coverage.md)
in the source tree — broken down by section (core surface, function
calls, static folding, strings, records, runtime lists, file I/O,
process / environment / streams) plus a target matrix with the
per-target status and known limitations (for example, the Windows
target's OS-builtin coverage is ANSI-only — non-ASCII paths and
environment values/keys are unsupported).

Anything not yet supported fails at build time with a source-located
diagnostic; there is no silent fallback to the evaluator.

## Highlights

- Core integer / boolean / string / list expressions, control flow,
  recursive `def`s.
- Static folding for pure expressions, with mutable side effects
  preserved when a value can still be recovered statically.
- Fixed-buffer runtime strings, line lists, runtime lists, and
  runtime records, with a `String` payload read back out of an enum
  normalizing onto the GC heap as a `HeapString` and heap-backed `+`
  when a `HeapString` participates, including static and runtime
  string fragments; rooted content equality and `assertResult` for
  heap strings; and top-level or method-style `toString` /
  interpolation bridging from `HeapString` back to runtime `String`.
- Static maps and sets, plus runtime-key lookups that copy entries
  into runtime storage without losing the selected length.
- A handful of GC-heap primitives (`__gc_alloc`, `__gc_record`,
  `__gc_write`, `__gc_read` / `__gc_read_ptr` / `__gc_read_string` /
  `__gc_read_double`, `__gc_string` / `__gc_string_concat`,
  `__gc_int_to_string` / `__gc_double_to_string`) remain as
  compiler-internal dispatch tags synthesized only by the
  enum-lowering desugar pass -- not user-callable, since the
  typechecker no longer declares them. Every other heap-pointer-list
  and string-keyed-map primitive that used to be directly callable
  has been removed from the language entirely; ordinary `List<T>`
  and `Map<K, V>` values are the only way to build collections.
- Linux file / directory / process / environment / stdin / argv
  builtins via direct syscalls; the same builtin families are also
  fully covered on the Windows target via direct Win64 `kernel32.dll`
  import calls (ANSI-only: non-ASCII paths and environment
  values/keys are unsupported on Windows).
- Source-located stderr diagnostics for runtime failures (`assert`,
  `assertResult`, `head([])`, negative `sleep`, FileOutput / Dir
  errors).
