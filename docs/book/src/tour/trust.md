# Trust Surface

Klassic ships a lightweight surface for proof-style declarations.
You can mark some lemmas as `axiom`s (assumed) and prove others as
`theorem`s built on top.

```kl
axiom sortedBase(xs: List<Int>): { true }

theorem sortedAgain(xs: List<Int>): { true } =
  sortedBase(xs)
```

This compiles normally — Klassic does not run a real proof checker on
the body. The interesting part is the *trust graph*.

## CLI gates

```bash
klassic --warn-trust proofs.kl
klassic --deny-trust proofs.kl
```

- `--warn-trust` reports every theorem whose proof transitively
  depends on a trusted (axiom or trusted-theorem) declaration.
- `--deny-trust` rejects compilation when any reachable theorem
  depends on a trusted declaration.

The pair is meant for codebases where you want to say "no axioms in
production" while still allowing scaffolding during development.

## Adding trust

Mark a declaration as trusted with the `trust` keyword:

```kl
trust theorem foo(): { true } = assert(true)
```

This is informational — it doesn't change the compile semantics, only
the trust graph that the gates inspect.
