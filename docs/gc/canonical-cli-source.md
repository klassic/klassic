# Canonical generational C and LLVM from the CLI

On Linux x86-64, the supported canonical subsets can be built as executables:

```sh
klassic --gc=generational --backend c build program.kl -o program
klassic --gc=generational --backend llvm build program.kl -o program
```

Outputs ending in `.c` (C backend) or `.ll` (LLVM backend) emit source instead.
Source emission does not require an external compiler. Executable builds require
Clang 15 or newer with opaque-pointer LLVM support, matching canonical runtime
sources/headers, and the matching GNU `libklassic_runtime.a` archive. The Linux
release bundles the runtime files beside its static musl CLI; generated programs
link against the host GNU libc.

The CLI accepts `--gc-stress`. It rejects `--gc-log` until the canonical runtime
reports collection statistics, and rejects `--gc-poison`, direct GC
heap/page/timing/log-format controls, and non-Linux-x86-64 targets. Executable linking also requires a Linux x86-64 host.
Module files are resolved beside the entry file and passed to the canonical
frontend. Unsupported constructs receive a source-located diagnostic. Compiler,
linker, frontend, and option errors preserve an existing executable.

For source checkouts, run `cargo build --workspace` before building executables.
Installed binaries search for runtime sources in `lib/klassic/gc` or `runtime/gc`
beside the CLI, then the build checkout. The Rust archive is discovered beside
the CLI or in its `lib` directory. These environment variables override discovery:

- `KLASSIC_GEN_CC`: C/LLVM compiler executable (otherwise Clang 18 through 15,
  then `clang`, are tried).
- `KLASSIC_GEN_RUNTIME_DIR`: matching canonical C source/header directory.
- `KLASSIC_RT_LIB`: matching GNU Rust runtime static archive.

Explicit invalid paths fail without falling back. A discovered installed runtime
with missing required files also fails, rather than borrowing checkout files.

This enables the existing canonical subset through the CLI. It does not change
the default collector or complete the GC foundation in #683.
