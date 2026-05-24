# Native Compiler Coverage

The native compiler lowers a growing slice of the language to ELF.
The exhaustive feature matrix lives in
[`docs/native-coverage.md`](https://github.com/klassic/klassic/blob/main/docs/native-coverage.md)
in the source tree — broken down by section (core surface, function
calls, static folding, strings, records, runtime lists, file I/O,
process / environment / streams).

Anything not yet supported fails at build time with a source-located
diagnostic; there is no silent fallback to the evaluator.

## Highlights

- Core integer / boolean / string / list expressions, control flow,
  recursive `def`s.
- Static folding for pure expressions, with mutable side effects
  preserved when a value can still be recovered statically.
- Fixed-buffer runtime strings, line lists, runtime lists, and
  runtime records, with explicit runtime `String` to `HeapString`
  lifting through `__gc_string` and heap-backed `+` when a `HeapString`
  participates, including static and runtime string fragments; rooted content
  equality and `assertResult` for heap strings; and
  top-level or method-style `toString` / interpolation bridging from
  `HeapString` back to runtime `String`.
- Static maps and sets, plus runtime-key lookups that copy entries
  into runtime storage without losing the selected length.
- High-level collection literals currently reject GC heap pointer values; use
  `__gc_list_ptr_*` until ordinary lists are heap-backed.
- GC helper calls that consume heap addresses reject plain `Int` arguments in
  native builds, even while the debug surface is source-typed through integers.
- Linux file / directory / process / environment / stdin / argv
  builtins via direct syscalls.
- Source-located stderr diagnostics for runtime failures (`assert`,
  `assertResult`, `head([])`, negative `sleep`, FileOutput / Dir
  errors).
