# Canonical generational source from the CLI

The supported canonical C and LLVM subsets can be emitted as source:

```sh
klassic --gc=generational --backend c build program.kl -o program.c
klassic --gc=generational --backend llvm build program.kl -o program.ll
```

The CLI accepts `--gc-stress` and `--gc-log` for this path. `--gc-poison`,
non-Linux-x86-64 targets, and executable output are explicitly rejected.
Module files are resolved beside the entry file and passed to the canonical
frontend. Unsupported constructs receive a source-located diagnostic.

This source-only step does not enable executable linking through the CLI,
change the default collector or complete the GC foundation in #683.
