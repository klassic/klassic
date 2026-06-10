# Algebraic Data Types (`enum`)

Klassic's `enum` keyword declares a tagged union: every variant
becomes a constructor, and runtime values pattern match via a
Scala-style postfix `match` expression.

```klassic
enum Option<a> {
  case Some(value: a)
  case None
}

enum Result<a, e> {
  case Ok(value: a)
  case Err(message: e)
}
```

Each variant takes named typed parameters (`value: a`). Nullary
variants (`case None`) omit the parameter list entirely; they bind
the variant name to a ready-made value rather than a constructor
function.

## Constructing values

```klassic
val good = Ok(42)             // Result<Int, _>
val bad  = Err("nope")        // Result<_, String>
val maybe = Some("hi")        // Option<String>
val empty = None              // Option<_>
```

## Pattern matching

Postfix `match` dispatches on the variant tag and binds the
positional fields inside each arm body:

```klassic
def describe(o) = o match {
  case Some(value) => "got " + toString(value)
  case None        => "nothing"
}

println(describe(Some(7)))   // "got 7"
println(describe(None))      // "nothing"
```

The pattern variable names are local to each arm. Wildcard arms
`case _ => ...` accept any variant and bind no fields.

## Comparison with records

Records (`record Point { x: Int; y: Int }`) and enums are
complementary:

- A record has **one** shape known statically. Field access
  (`p.x`, `p.y`) is fixed.
- An enum has **several** shapes; the right one is decided at
  runtime, and `match` is the typed way to read the payload.

Use records for fixed-shape data; use enums when the value carries
either a payload or a tag (Option, Result, parse trees, ...).

## Native compilation

Native builds compile enums and `match`. Monomorphic enums lower to
GC records with short-circuit tag dispatch; generic enums compile
per instantiation, with payload shapes tracked through bindings,
control-flow joins, and annotated function boundaries — so recursive
functions over `Option<Int>`-style annotations work, and the checker
also diagnoses match exhaustiveness and unreachable arms ahead of
codegen. The remaining native gaps (for example a `List<SomeEnum>`
payload field) fail at build time with a source-located diagnostic,
never with wrong code.
