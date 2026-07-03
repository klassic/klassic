# Building Executables

The native compiler emits the executable container itself — no `cc`,
`as`, `ld`, or `codesign` involvement. For a typical script that means
a sub-10 KiB binary with sub-millisecond startup.

## Workflow

```bash
klassic build path/to/program.kl -o program
./program
```

A target-less `build` compiles for the **detected host**:

| Host | Backend | Output |
| --- | --- | --- |
| Linux x86_64 | direct ELF writer (most complete) | ELF64 talking to the kernel via raw syscalls |
| macOS arm64 | direct Mach-O writer, portable-C fallback | ad-hoc-signed Mach-O arm64 via `svc #0x80` |
| Windows x86_64 | direct PE64 writer | PE64 talking to the kernel via Win64 `kernel32.dll` import calls |
| other hosts | portable C backend | executable linked with the system `cc` |

To pick a target explicitly, pass `--target` with a canonical name or
triple — `linux-x86_64` / `x86_64-unknown-linux-gnu`,
`macos-aarch64` / `aarch64-apple-darwin`, and `windows-x86_64` /
`x86_64-pc-windows-msvc` are the implemented direct targets, and
`--target native` selects the host's. The Mach-O and PE64 targets both
cross-build from any host (the Mach-O writer embeds the code signature
itself; the PE64 writer needs no signature at all).
`klassic targets` prints the full matrix alongside planned targets.

On Apple Silicon, programs outside the Mach-O backend's growing subset
fall back transparently to the portable C backend (`--backend c`),
which links against the bundled `libklassic_runtime.a` — a
host-default `build` therefore succeeds for strictly more programs
than either backend alone.

That's the whole flow. The compiler runs the same parse → rewrite →
type-check → proof-check pipeline as the evaluator, then lowers a
supported subset to machine code and writes the executable.

## What gets compiled

On Linux x86_64, the direct backend lowers a wide slice of the
language:

- Arithmetic, boolean, and string-concatenation expressions.
- `if` (static and dynamic), `while`, `foreach`.
- `val` / `mutable` bindings, including reassignment.
- `def` functions, including recursive ones.
- Static and runtime collections (lists, maps, sets, records).
- Monomorphic and shape-tracked generic enums with `match`.
- `println`, `printlnError`, `assert`, `assertResult`.
- File I/O, environment variables, command-line args, stdin / stdout.

On macOS arm64, the younger direct backend covers Int/Bool/String
expressions, locals, `if` / `while`, annotated functions with
recursion, the string builtins, monomorphic enums and `match`, cons
lists, and records — and the portable C backend catches what it
cannot.

On Windows x86_64, the direct PE64 backend reuses the entire Linux
x86_64 codegen (the generated machine code is already Win64-ABI
compatible) and swaps in Win64 `kernel32.dll` import calls at the OS
boundary, so it covers the same core language plus the full OS-builtin
surface: file and directory I/O, environment variables, command-line
arguments, stdin, `sleep`, `stopwatch`, and `Time#nowMillis`. The one
Windows-specific limitation is that those OS calls go through the
ANSI (`A`-suffixed) Win32 functions, so non-ASCII paths and
environment values/keys are unsupported.

See [Native Compiler Coverage](../reference/native-coverage.md) for
the exhaustive matrix.

## Unsupported constructs

If you hit something the native compilers do not yet handle, the build
emits a source-located diagnostic and exits non-zero. There is no
fallback to the evaluator at runtime — production binaries always run
through native code.

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
