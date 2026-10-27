# Canonical LLVM source emission

`compile_source_to_llvm_generational` emits rooted LLVM IR for the supported
scalar, UTF-8 string and structural-record subset against the canonical C
runtime. `compile_source_to_llvm_generational_with_modules` also accepts
explicit module sources and retains their diagnostic locations. Unsupported
constructs return a diagnostic.

This is a library API layered on the canonical C source emitter's shared
frontend. It does not enable the LLVM executable CLI or change the default
direct collector, and it does not establish the complete #683 GC contract.

On Linux x86-64, `cargo test --test llvm_generational` requires Clang. It
validates emitted IR and links a program to the actual C runtime, retaining a
record through stress collections and checking module capture and Float32
output. Build the workspace first to provide the Rust runtime archive.
`KLASSIC_GEN_SANITIZE=address` and `=thread` instrument the runtime execution
test; sanitizer startup or execution failures fail the test.
