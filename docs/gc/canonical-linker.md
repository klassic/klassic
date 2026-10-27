# Canonical C/LLVM linker

`link_generational_program` builds canonical C or LLVM source with the C
runtime and a caller-supplied Rust runtime archive. It requires an explicit
compiler, runtime directory and archive, and is supported on Linux x86-64.
Build products are prepared in a private directory beside the output and the
output is replaced only after a successful link.

This is a library API. It does not enable the executable CLI or make the
direct collector use the canonical C runtime. It is one part of #683, not
evidence that the full generational GC contract is complete.

Build the workspace before `cargo test --test generational_link` to supply the
Rust runtime archive. The Linux test requires Clang and executes both emitted
backends against the real collector; it also checks that a compiler failure
preserves the previous executable. `KLASSIC_GEN_SANITIZE=address` and
`=thread` enable sanitizer instrumentation for these executions.
