# Module & Import Improvements (Roadmap)

## Import Enhancements
- Aliases + selective together: `import foo.bar.{baz, qux} as fb` and per‑symbol aliasing `import Map.{size => msize}`.
- Exclude + alias combos: `import Map.{size => _, get} as M`.
- Wildcard with filters: `import Map.{* , size => _}` for all except size.
- Conflict detection: warn/error on name collisions (local > selective > wildcard) with `--strict-imports`.

## Selector & Resolution
- FQCN selector polish: support nested calls and spacing forms consistently.
- Prefer zero‑copy aliasing in module env (share map) to avoid duplication; document immutability expectations.

## Module Typing
- Persist user module type info (TScheme snapshots) alongside runtime export (e.g., `.klassic-mod.json`).
- On import, load persisted types; fall back to current runtime inference only if absent.
- Improve function arity/shape inference from closures (detect currying; infer `(a,b)=>c` vs `a=>b=>c`).

## Visibility & Re‑export
- Visibility keywords: `public`/`private` for `def`/`val`; export only public.
- Re‑export syntax: `export other.mod.{foo, bar}` and `export other.mod.*`.

## Initialization & Order
- `init { ... }` block per module; run once upon first import.
- Import graph + cycle detection; clear error for cycles and partial initialization.

## Multi‑file Modules
- Module path discovery: map `user.util` to `src/kl/user/util.kl` (configurable `KPATH`).
- Precompiled cache for modules to speed up repeated imports.

## Typeclass Integration
- Importing instances: `import show.instances.{ShowInt, ShowString}` to register instances into the environment.
- Duplicate instance policy: explicit selection or error under strict mode.

## Tooling & DX
- Lints: unused import, shadowed names, re‑export loops.
- sbt tasks: `klassic/modules` (graph), `klassic/import-lint` (checks), `klassic/clean-mod-cache`.
- Docs: update README with module/alias/selective patterns and gotchas.
