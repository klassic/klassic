# Hello World

Save this as `hello.kl`:

```kl
println("Hello, World!")
```

You can run it three ways.

## Evaluate from the command line

```bash
klassic -e 'println("Hello, World!")'
# Hello, World!
```

`-e` evaluates a single expression without ever touching the
filesystem.

## Run the file through the evaluator

```bash
klassic hello.kl
# Hello, World!
```

`klassic <path>` and `klassic -f <path>` are equivalent — both run
the program through the evaluator.

## Compile to a native executable

```bash
klassic build hello.kl -o hello
./hello
# Hello, World!
```

A target-less `build` compiles for the detected host, writing the
executable container itself with no external toolchain: a direct ELF64
on Linux x86_64, an ad-hoc-signed Mach-O arm64 on Apple Silicon macOS,
or a PE64 on Windows x86_64 (`klassic build hello.kl -o hello.exe`).
None of them depend on `libc`, so the resulting binary is a few KiB
and starts essentially instantly. See
[Building Executables](../native/building.md) for the full host/target
matrix.

On Linux:

```bash
file hello
# hello: ELF 64-bit LSB executable, x86-64 ...

ls -lh hello
# -rwxr-xr-x 1 you you ~10K hello
```

## When does the evaluator beat the compiler?

The native compiler only supports a slice of the language — see
[Native Compiler Coverage](../reference/native-coverage.md) for the
full matrix. If you reach for a feature it doesn't yet handle, the
build emits a source-located diagnostic and exits non-zero. There is
no silent fallback to the evaluator. For exploratory work, run
through the evaluator (`klassic <path>`) or the [REPL](./repl.md).
