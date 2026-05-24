# Native Backend Strategy

Klassic's native compiler should keep the default path self-contained while
leaving room for an optional LLVM backend later.

## Decision

- Keep the direct Rust x86_64 backend as the bootstrap backend.
- Treat target selection, backend choice, data layout, executable format, and
  platform constants as explicit metadata.
- Do not make LLVM a mandatory dependency for the default native build path.
- Add an LLVM backend only behind a backend boundary once the frontend,
  runtime contracts, and target metadata are stable enough to share.

## Why

The direct backend gives Klassic a small trusted path: parse, rewrite,
typecheck, lower, emit machine code, and write the executable without invoking
external assembler or linker tools. That matters for bootstrapping and eventual
self-hosting because the compiler can keep running in constrained environments.

LLVM is still attractive for broader platforms and optimizations. The risk is
making every build depend on LLVM before Klassic has a stable native IR and
runtime ABI. The near-term shape is therefore hybrid: the handwritten backend
proves the runtime model and acts as a bootstrap backend; a future LLVM backend
can reuse the same target registry and runtime contracts.

## Current Boundary

The native target registry records:

- compact target name and standard triple
- architecture
- selected backend
- data layout
- operating system and ABI
- executable format

The compiler resolves a per-compile target context before code generation. That
context pairs the target spec with platform constants, so backend selection and
syscall/ABI constants come from the same target metadata.

## Next Backend Steps

1. Keep hardening the direct Linux x86_64 backend until the GC/runtime surface is
   robust enough for larger programs.
2. Extract a stable low-level native IR only when repeated x86_64 emitter logic
   makes the split pay for itself.
3. Add non-x86_64 targets by extending the target registry first, then choosing
   whether each target uses a direct backend, LLVM, or both.
4. Keep LLVM optional unless the default bootstrap path has a replacement that
   is equally easy to build and audit.
