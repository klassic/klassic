# Strings

Klassic strings are UTF-8. There are two flavours that interoperate
freely in the evaluator and through native compilation:

- **Static strings** — string literals and the result of pure folding.
  They live in the executable's `.data` section.
- **Heap strings** — produced by `__gc_string*` builtins (and, on the
  native compiler's roadmap, by every dynamic operation eventually).
  They live on the GC heap and survive arbitrary collections.

Both kinds work with `println` and the standard library helpers below.
The difference is mostly internal — heap strings can grow at runtime
without filling fixed-size scratch buffers.

## Literals and interpolation

```kl
val name = "Klassic"
val greeting = "Hello, #{name}!"
println(greeting)   // Hello, Klassic!
```

`#{...}` evaluates an arbitrary expression and splices its rendering
into the surrounding string.

## Concatenation

```kl
val parts = "foo" + "bar"
val mixed = "count = " + 42
```

## Common operations

| Function | Behaviour |
|---|---|
| `length(s)` | Byte length |
| `substring(s, i, j)` | Bytes `[i, j)` |
| `at(s, i)` | One-byte string at index |
| `trim(s)`, `trimLeft(s)`, `trimRight(s)` | Strip ASCII whitespace |
| `toLowerCase(s)`, `toUpperCase(s)` | ASCII case shift |
| `replace(s, from, to)` | Replace first occurrence |
| `replaceAll(s, from, to)` | Replace every occurrence |
| `startsWith(s, prefix)`, `endsWith(s, suffix)` | Boolean predicates |
| `contains(s, needle)`, `indexOf(s, needle)` | Membership / first index |
| `repeat(s, n)` | Concatenate `s` with itself `n` times |
| `reverse(s)` | UTF-8 aware reverse |
| `split(s, delimiter)` | Returns a list of segments |
| `join(parts, separator)` | Inverse of `split` |

Method-style calls work too:

```kl
val tidy = "  Klassic  ".trim().toUpperCase()
println(tidy)   // KLASSIC
```

## Formatting and padding

`format(template, args)` fills each `{}` placeholder from the `args`
list in order, rendering every element with `toString`. A literal
brace is written `{{`. `padStart` / `padEnd` widen a string to a
target width with a chosen pad string; both have method twins.

```kl
println(format("Hello, {}! You have {} messages.", ["Klassic", "3"]))
                              // Hello, Klassic! You have 3 messages.
println(format("{{not a slot}} {}", ["filled"]))
                              // {not a slot} filled
println(padStart("7", 3, "0"))    // 007
println(padEnd("7", 3, "."))      // 7..
println("42".padStart(5, " "))    // "   42"
```

The `args` list is homogeneous like every Klassic list, so format an
all-`Int` or all-`String` list — mix types by converting to strings
first.

## Parsing numbers

`String#parseInt` / `String#parseDouble` parse a string or abort on
bad input. The recoverable variants never crash: `toIntOr(d)` /
`toDoubleOr(d)` fall back to `d`, and `isInteger()` / `isFloat()`
report whether a parse would succeed.

```kl
println(String#parseInt("42"))      // 42
println(String#parseDouble("3.14")) // 3.14

println("42".toIntOr(0))            // 42
println("oops".toIntOr(-1))         // -1
println("3.14".toDoubleOr(0.0))     // 3.14
println("nope".toDoubleOr(0.0))     // 0.0

println("42".isInteger())           // true
println("4.2".isFloat())            // true
println("x".isInteger())            // false
```

The same operations are available as free functions under
`import std.string.{parseInt, parseDouble, parseIntOr, parseDoubleOr}`.

## Heap strings

When you need string values that grow at runtime (concatenating in a
loop, building output incrementally), reach for the GC builtins:

```kl
mutable greeting = __gc_string("Hello")
greeting = __gc_string_concat(greeting, __gc_string(", world!"))
println(greeting)
```

See [Heap-Allocated Strings](../gc/strings.md) for the full toolkit.
