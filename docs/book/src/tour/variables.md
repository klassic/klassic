# Variables and Values

Klassic has two kinds of bindings:

```kl
val pi = 3.14159        // immutable
mutable counter = 0     // reassignable
counter = counter + 1
counter += 1
```

`val` bindings cannot be reassigned. `mutable` bindings can.

## Type annotations

Klassic infers types Hindley-Milner style, so you rarely need
annotations. They are still allowed (and sometimes required at module
boundaries):

```kl
val name: String = "Klassic"
def square(n: Int): Int = n * n
```

## Built-in scalar types

| Type | Examples |
|---|---|
| `Int` | `0`, `42`, `-7`, `0xff` |
| `Long` | `42L`, `1_000_000_000L` |
| `Float` | `3.14F`, `1.0e9F` |
| `Double` | `3.14`, `1.0e9` |
| `Bool` | `true`, `false` |
| `String` | `"hello"`, `"interpolated #{x}"` |
| `Null` | `null` (singleton) |
| `Unit` | `()` (the only inhabitant) |

## Numeric literals

```kl
val big = 1000L             // L → Long
val pi = 3.14F              // F → Float, no suffix → Double
val mask = 0xFF             // hex (radix 16)
val avo = 6.022e23          // scientific notation → Double
val readable = 1_000_000    // `_` digit separators, anywhere between digits
```

## Operators

Standard arithmetic operators (`+`, `-`, `*`, `/`), comparison
(`==`, `!=`, `<`, `<=`, `>`, `>=`), and the boolean `&&` / `||` /
`!` work as you would expect. `&&` and `||` short-circuit. There is no
`%` operator — `%` introduces map and set literals; use `std.math`'s
`mod(a, b)` for the remainder.

```kl
val ok = (1 + 2) * 3 == 9 && true
println(ok)   // true
```
