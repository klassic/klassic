# Building Executables

The native compiler emits a Linux x86_64 ELF directly — no `cc`,
`as`, or `ld` involvement. For a typical script that means a sub-10
KiB binary with sub-millisecond startup.

## Workflow

```bash
klassic build path/to/program.kl -o program
./program
```

You can make the target explicit with `--target linux-x86_64` or the standard
triple alias `--target x86_64-unknown-linux-gnu`, or use `--target native` on a
Linux x86_64 host. Linux x86_64 is currently the only implemented concrete
target, but keeping the target visible in the command line lets future targets
reuse the same build surface. Internally the target registry records the backend
and data layout as well as the OS/ABI/file format, so future backends can plug
into the same target-selection path instead of being hardcoded into the CLI.

That's the whole flow. The compiler runs the same parse → rewrite →
type-check → proof-check pipeline as the evaluator, then lowers a
supported subset to machine code and writes an ELF64 file.

## What gets compiled

The native compiler currently lowers a growing slice of the language:

- Arithmetic, boolean, and string-concatenation expressions.
- `if` (static and dynamic), `while`, `foreach`.
- `val` / `mutable` bindings, including reassignment.
- `def` functions, including recursive ones.
- Static and runtime collections (lists, maps, sets, records).
- `println`, `printlnError`, `assert`, `assertResult`.
- File I/O, environment variables, command-line args, stdin / stdout.
- `__gc_*` builtins for the GC heap.

See [Native Compiler Coverage](../reference/native-coverage.md) for
the exhaustive matrix.

## Unsupported constructs

If you hit something the native compiler does not yet handle, the
build emits a source-located diagnostic and exits non-zero. There is
no fallback to the evaluator at runtime — production binaries always
run through native code.

```bash
klassic build unsupported.kl -o out
# unsupported.kl:5:3: error: native: <feature>
```

Workarounds:

1. Re-shape the program to avoid the unsupported feature.
2. Run through the evaluator (`klassic <path>`) for one-off use.
3. Open an issue on GitHub if you think the feature should be added.

## Trust diagnostics

`--warn-trust` and `--deny-trust` work for `build` too:

```bash
klassic build --deny-trust proofs.kl -o proofs
```
