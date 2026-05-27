# Klassic Roadmap: Multi-Target Native And Shared Standard Library

This document captures the long-term direction for Klassic. The detailed
PR-sized plan lives in `klassic_claude_code_plan.md`; this file is the
shorter reference that ships with the repository and is intended to stay
up to date as work lands.

## Mission

Klassic should be pleasant as a small interpreter / scripting language,
and the same source plus the same standard library should be able to
target one or more native backends when a user wants a binary. To make
that work we need:

1. A single, shared standard library used by both the evaluator and the
   native compiler. The evaluator is the semantic oracle; the native
   compiler is an optimised executor, not an alternative language.
2. A native compiler architecture that is target-extensible without
   sprinkling `if linux && x86_64` branches across the codebase.
3. A runtime layer that grows incrementally — direct Linux syscalls
   today, portable runtime calls tomorrow — without breaking the
   existing direct ELF path.
4. Builtin definitions (name / arity / type / effects / native support)
   that live in one place and are consumed by the evaluator, the type
   checker, the native compiler, and the docs.

## Phased Milestones

The PR-sized roadmap (see `klassic_claude_code_plan.md` Section 19) is:

1. **PR 1 — Roadmap docs.** This file plus pointers from `CLAUDE.md`
   and `docs/native-coverage.md`. Sets shared direction before any
   refactor.
2. **PR 2 — Pipeline / trust analysis extraction.** Move the duplicated
   proof / trust analysis out of `klassic-eval` and `klassic-native`
   into a shared module so both crates share one implementation.
3. **PR 3 — `klassic-runtime` scaffold expansion.** Add value
   display / equality helpers, a shared `RuntimeError`, and a
   `SystemInterface` trait. Native does not link to it yet — this PR
   establishes the shared surface.
4. **PR 4 — Builtin manifest MVP.** A single descriptor table for
   builtin name / arity / type / effects / native support, consumed by
   evaluator registry, type checker, and native lowering.
5. **PR 5 — `TargetSpec` MVP.** Triple parsing, a `klassic targets`
   subcommand, a `build --target` flag, and structured diagnostics for
   unsupported targets. Existing `x86_64-unknown-linux-gnu` stays
   default.
6. **PR 6 — Stdlib module loader MVP.** Embed
   `stdlib/std/list.kl` (and friends) and let `import std.list.{range}`
   resolve in both evaluator and native build. Keep `stdlibRange` etc.
   as compatibility aliases.
7. **PR 7 — CLI scripting UX.** `klassic run file.kl -- args`, script
   argument plumbing, shebang skip, and REPL `:help`, `:load`, `:type`.
8. **PR 8 — Stdlib v0.2.** List / String / Math / Path modules,
   parity-tested across evaluator and native.
9. **PR 9 — Portable C backend MVP.** A small `Int`/`Bool`/`String`/
   `println`/`if`/`while`/`def` subset emitted as C source, behind a
   `--backend c` flag. The direct ELF path stays untouched.
10. **PR 10 — Runtime call path MVP.** `klassic-runtime` exposes a
    C-ABI shim (`klassic_rt_println_string`, `klassic_rt_args`,
    `klassic_rt_env_get`, file read/write) that the C backend calls.
11. **PR 11 — Stdlib `std.file` / `std.env` / `std.cli` / `std.csv` /
    `std.test`.** Bring scripting usefulness up.
12. **PR 12 — Second native target skeleton.** Either AArch64 Linux
    direct ELF or `wasm32-wasi`, whichever proves the `TargetSpec`
    abstraction with the least scope first.

PRs 1–5 are about plumbing, 6–8 are about user-visible language
surface, 9–10 are about portability, 11–12 are about reach. Each PR is
gated by `cargo fmt --check`, `cargo clippy --all-targets
--all-features -- -D warnings`, and `cargo test`, plus a manual
`cargo run -- -e "1 + 2"` smoke and (on Linux x86_64) a sample
native build.

## Target Expansion Tiers

Targets are tracked as tiers; status lives in
`docs/native-coverage.md#target-matrix`.

- **Tier 0** — `x86_64-unknown-linux-gnu`, direct ELF executable. The
  current default. Must stay supported and CI-tested at all times. No
  external `cc` / `as` / `ld` dependency.
- **Tier 1** — `x86_64-unknown-linux-musl` (alias of Tier 0 in
  practice, same direct syscall path), `aarch64-unknown-linux-gnu`
  (new emitter, starts from a small subset). Both Linux, both still
  direct ELF.
- **Tier 2** — `wasm32-wasi` (WASI imports for I/O and env, no
  threads / processes initially), `x86_64-apple-darwin` and
  `aarch64-apple-darwin` (via runtime call path / C backend, not
  direct syscalls), `x86_64-pc-windows-msvc` (same).

The key rule: do **not** add Mach-O or PE direct syscall paths. macOS
and Windows enter the matrix through the portable runtime call path
once that path exists.

## Runtime Sharing Strategy

`klassic-runtime` is the eventual home for everything the evaluator and
the native compiler genuinely share:

- value display and equality
- a single `RuntimeError` type with source-span fidelity
- `SystemInterface` and `FileSystemInterface` traits for stdin / stdout
  / env / fs / time / sleep / args / exit
- C-ABI shim functions (`klassic_rt_*`) the portable backends call

There are two execution paths:

1. **Direct syscall path** — Linux x86_64 today, plus possible AArch64
   Linux additions. This path inlines syscalls into the emitted
   executable and does not link `klassic-runtime` at all. It exists
   because writing a tiny tool that has no external dependencies is
   one of Klassic's selling points; this path must stay fast and small.
2. **Runtime call path** — macOS, Windows, Wasm, and the portable C
   backend. The native compiler emits calls into `klassic-runtime`,
   which is the only place that owns OS-specific behaviour. The
   evaluator uses the same runtime crate by linking to it as a Rust
   dependency.

The evaluator should converge on the runtime crate's primitives over
time, so evaluator and native always agree on display, equality, and
error model.

## Builtin Manifest Motivation

Today builtin information is scattered across:

- the evaluator's builtin registry
- the type checker's polymorphic declarations
- the native compiler's dispatch chain and unsupported-fallback lists
- `docs/native-coverage.md`
- stdlib reference docs

Adding a builtin currently means touching five places and keeping them
in sync by hand. The manifest unifies this. A `BuiltinDescriptor`
captures canonical name, module path, aliases, arity, signature,
purity, effects, error model, evaluator implementation key, native
support level, and short doc. The evaluator registry derives from the
manifest; the type checker installs signatures from it; the native
compiler reads `NativeSupport` to choose between direct lowering,
compile-time folding, runtime call, or a source-located unsupported
diagnostic; the docs generator (eventually) produces the coverage
tables from it.

This work lands in PR 4 and is the prerequisite for keeping evaluator
and native in lockstep as the stdlib grows.

## Prelude Compatibility And Deprecated Alias Policy

`stdlib/prelude.kl` is bundled into the binary and parsed as a separate
translation unit before user code (the evaluator and the native
compiler use the same prelude through `compile_source_with_prelude_*`).
Existing public names — `stdlibRange`, `stdlibFilter`, etc. — are
considered the v0.1 surface and stay supported.

The v0.2 surface (PR 6 onward) introduces module-qualified names:

```kl
import std.list.{range, filter, sum}
import std.string as String
```

Migration policy:

- v0.1 prelude names stay until at least one minor release after the
  v0.2 module surface ships.
- Each v0.1 name that has a v0.2 successor gets a comment in
  `stdlib/prelude.kl` pointing at the new path.
- No silent semantic drift: if a v0.2 implementation differs from its
  v0.1 prelude counterpart, the v0.1 alias is removed in the same PR
  rather than left to confuse callers. Renames without semantic change
  keep the alias.
- `klassic-eval` and `klassic-native` must reach the same answer for
  every prelude / stdlib function. Diverging implementations are a
  release blocker, not a TODO.

The release process records prelude-surface changes in
`docs/native-coverage.md` (and, once docs generation lands, in
manifest-derived tables).
