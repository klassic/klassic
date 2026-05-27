# Standard Library Modules (v0.2)

The v0.2 standard library splits the historic `stdlib*` prelude
helpers into module-qualified namespaces. Modules ship as embedded
Klassic sources (see
[`stdlib/std/`](https://github.com/klassic/klassic/tree/main/stdlib/std)),
are loaded as separate translation units before user code runs, and
expose their members through `import` statements:

```klassic
import std.math.{mod, clamp, sign}
import std.list.{range, filter, sum}
import std.path.{basename, join}
```

The v0.1 `stdlibFoo` aliases stay available without an import —
nothing has been removed.

## std.list

Plain-Klassic list helpers. Members mirror the v0.1 `stdlib*`
prelude names without the prefix.

| Member | Example |
| --- | --- |
| `range(start, end)` | `range(0, 5)` → `[0, 1, 2, 3, 4]` |
| `rangeInclusive(start, end)` | `rangeInclusive(1, 4)` → `[1, 2, 3, 4]` |
| `take(xs, n)` / `drop(xs, n)` | `take([1, 2, 3], 2)` → `[1, 2]` |
| `filter(xs, p)` / `find(xs, p)` | `filter([1, 2, 3], (x) => x > 1)` |
| `any(xs, p)` / `all(xs, p)` / `count(xs, p)` | `any([1, 2, 3], (x) => x > 2)` |
| `replicate(n, x)` | `replicate(3, "x")` → `["x", "x", "x"]` |
| `sum(xs)` / `product(xs)` / `last(xs)` | `sum([1, 2, 3])` → `6` |

The module also installs extension methods on `List<a>` and
`List<Int>` so the same operations are reachable via dot syntax:

```klassic
println([1, 2, 3].lengthOf())     // 3
println([1, 2, 3].isEmptyList())  // false
println([1, 2, 3].headOr(0))      // 1
println([1, 2, 3].filterBy((x) => x > 1))
println([1, 2, 3, 4].total())     // 10  (List<Int>)
println([1, 2, 3, 4].productOf()) // 24  (List<Int>)
```

## std.string

String helpers ship exclusively as extension methods on `String`
so they coexist with the existing top-level `length` / `trim` /
... builtins:

```klassic
println("hello".upper())                  // "HELLO"
println("  hi  ".trimmed())               // "hi"
println("a,b,c".lines())                  // ["a,b,c"]
println("hi there".words())               // ["hi", "there"]
println("klassic".containsText("la"))     // true
println("klassic".startsWithText("kl"))   // true
println("klassic".lengthChars())          // 7
println("hello".reverseChars())           // "olleh"
```

## std.math

Plain helpers (`mod`, `min`, `max`, `clamp`, `isEven`, `isOdd`,
`sign`) plus an `extension (this: Int) { ... }` block that exposes
dot-style equivalents for integer-typed receivers:

```klassic
import std.math.{mod, clamp}
println(mod(17, 5))           // 2
println(clamp(15, 0, 10))     // 10

println(4.isEvenN())          // true
println(5.isOddN())           // true
println((-3).absValue())      // 3
println((-7).signOf())        // -1
println(15.clampTo(0, 10))    // 10
```

The dispatch key normalises Byte / Short / Int / Long to the same
`Int` bucket, so the extension methods apply to all four widths.

## std.path

POSIX-style path string helpers. Windows-style paths are not yet
supported.

```klassic
import std.path.{basename, dirname, fileExtension, join}

println(basename("/usr/lib/foo.txt"))      // "foo.txt"
println(dirname("/usr/lib/foo.txt"))       // "/usr/lib"
println(fileExtension("/usr/lib/foo.txt")) // ".txt"
println(join("/usr", "lib"))               // "/usr/lib"
```

The helper is called `fileExtension` rather than `extension` because
`extension` is reserved by the language's extension-method syntax.

## std.option

Lightweight Option/Maybe helpers backed by `null` for "none" and the
value itself for "some" — same convention `find` in `std.list`
already uses. Once Klassic gains ADTs the underlying representation
will change, but the function-style API will keep working.

```klassic
import std.option.{isSome, isNone, getOrElse, mapOption}

println(isSome(null))            // false
println(isSome(7))               // true
println(getOrElse(null, 99))     // 99
println(getOrElse("hi", "x"))    // "hi"
println(mapOption(7, (x) => x * 2)) // 14
println(mapOption(null, (x) => x))  // null
```

## std.result

`Result`-style success / failure helpers. The current implementation
uses two records (`#ROk`, `#RErr`) under the hood; the public
surface is the function helpers below. The plain identifiers `Ok` /
`Err` are reserved for the future ADT-based revision.

```klassic
import std.result.{ok, err, isOk, isErr, unwrapOr, mapResult}

val good = ok("payload")
val bad  = err("nope")

println(isOk(good))                  // true
println(isErr(bad))                  // true
println(unwrapOr(good, "default"))   // "payload"
println(unwrapOr(bad, "default"))    // "default"
```

## std.cli

Tiny CLI-argument helpers for scripts that read
`CommandLine#args()`. The module pairs naturally with
`klassic run file.kl -- <args>` (PR 7) — every member operates on
the `List<String>` that `--` populates.

```klassic
import std.cli.{flag, option, positionals}

val args = CommandLine#args()
val verbose = flag(args, "--verbose")        // Boolean
val output  = option(args, "--out")          // String or null
val files   = positionals(args)              // remaining non-flag args
```

`flag(args, name)` returns true iff `name` appears as one of the
tokens in `args`. `option(args, name)` finds the first occurrence
and returns the token that follows it (or `null` if absent).
`positionals(args)` drops everything that starts with `--` and
returns the rest; callers that need to pair flags with values
should use `flag` / `option` directly on the original list.

## std.test

Lightweight assertion helpers for scripting and small test programs.
Builds on the existing `assert` / `assertResult` builtins so failures
print a human-readable report instead of stopping the program with a
runtime error.

```klassic
import std.test.{expectTrue, expectEqualsInt, expectEqualsString}

expectTrue("simple", true)
expectEqualsInt("math",   6,        1 + 2 + 3)
expectEqualsString("name", "hello", "hel" + "lo")
```

A passing check prints `[ OK ] <name>` on stdout; a failing one
prints `[FAIL] <name>: <reason>` on stderr without aborting, so a
script can run a whole suite and aggregate results.

## Native availability

Stdlib modules are currently loaded only by the evaluator. The
native compiler (`klassic build file.kl`) emits a source-located
diagnostic when user code does `import std.<X>`:

```
error: module `std.list` is part of the standard library but is not yet
       available in native builds. Run the program with the evaluator
       (klassic file.kl) or wait for the native stdlib module loader
       described in docs/roadmap-targets-stdlib.md.
```

The bundled v0.1 `stdlibFoo` prelude helpers continue to work in
native builds — the diagnostic only fires on the new
module-qualified surface.
