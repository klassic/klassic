# Canonical C source emission

`compile_source_to_c_generational` emits C for the supported scalar, UTF-8
string and structural-record subset using the rooted canonical C runtime.
`compile_source_to_c_generational_with_modules` additionally accepts explicit
module sources; their names and paths preserve diagnostic locations. The
source emitter returns a diagnostic for unsupported constructs.

This is a library API. It does not enable the C executable CLI, implement the
LLVM emitter, or replace the default direct collector. It does not establish
the complete generational GC contract in issue #683.

On Linux x86-64, `cargo test --test c_generational_backend` requires a C
compiler and Clang. It compiles generated objects and links an emitted program
to the actual C runtime. The execution test retains a record through stress
collections, exercises an imported captured module value and checks Float32
output. Build the workspace first to provide the Rust runtime archive.
`KLASSIC_GEN_SANITIZE=address` and `=thread` instrument the real-runtime
execution test; a sanitizer startup or execution error fails the test.
