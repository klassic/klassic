# Control Flow

## `if` expressions

`if` is an expression — every branch must return the same type.

```kl
val n = 7
val parity = if (n > 0) "positive" else "non-positive"
```

Without an `else`, the value is `Unit`:

```kl
val debug = true
if (debug) println("hello")
```

## Ternary operator

`if` doubles as a ternary. There is no separate `?:` syntax.

## `while` loops

```kl
mutable i = 0
while (i < 10) {
  println(i)
  i += 1
}
```

`while` is an expression that returns `Unit`.

## `foreach`

```kl
foreach (x in [1, 2, 3]) {
  println(x)
}

foreach (name in ["alice", "bob"]) {
  println(name)
}
```

The native compiler unrolls `foreach` over static integer lists, so
small loops have zero per-iteration overhead.

## Blocks as expressions

Curly braces make a block. The last expression in the block becomes
the value of the block.

```kl
def compute() = 87

val score = {
  val raw = compute()
  val clamped = if (raw < 0) 0 else raw
  clamped
}
```

This pattern is handy for keeping local helpers tucked next to where
they are used.

## Match-on-shape

Klassic does not yet have an algebraic `match` expression. Use chained
`if` / `else` for pattern-style dispatch, or method-style calls on
records.
