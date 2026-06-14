# Standard Library Modules (v0.3)

The v0.3 standard library splits the historic `stdlib*` prelude
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
| `sort(xs)` / `sortBy(xs, key)` | `sort([3, 1, 2])` → `[1, 2, 3]` |
| `zip(xs, ys)` | `zip([1, 2], [3, 4])` → `[[1, 3], [2, 4]]` |
| `partition(xs, p)` | `partition([1, 2, 3], isEven)` → `[[2], [1, 3]]` |
| `groupBy(xs, key)` | groups into `{key, items}` records |
| `mkString(xs, sep)` | `mkString([1, 2, 3], "-")` → `"1-2-3"` |

The module also installs extension methods on `List<a>` and
`List<Int>` so the same operations are reachable via dot syntax:

```klassic
println([1, 2, 3].lengthOf())          // 3
println([1, 2, 3].isEmptyList())       // false
println([1, 2, 3].headOr(0))           // 1
println([1, 2, 3].filterBy((x) => x > 1))
println([1, 2, 3, 4].total())          // 10  (List<Int>)
println([1, 2, 3, 4].productOf())      // 24  (List<Int>)
println([3, 1, 4, 1, 5, 9].minimum())  // 1   (List<Int>)
println([3, 1, 4, 1, 5, 9].maximum())  // 9   (List<Int>)
println([1.5, 2.5, 3.5].totalDouble()) // 7.5 (List<Double>)
```

The higher-order helpers have matching method twins:

```klassic
println([5, 3, 8, 1].sorted())            // [1, 3, 5, 8] (List<Int>/<Double>)
println([1, 2, 3].zipWith([10, 20, 30]))  // [[1, 10], [2, 20], [3, 30]]
println([1, 2, 3].mkStringWith(" - "))    // 1 - 2 - 3
```

`.sortedBy(key)`, `.partitionBy(p)`, and `.groupedBy(key)` round out
the dot-callable set.

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
println("hello".takeChars(3))             // "hel"
println("hello".dropChars(2))             // "llo"
println("hello".sliceFromTo(1, 4))        // "ell"
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

The module also carries the math constants `pi()` and `e()`, the
angle conversions `degreesToRadians(d)` / `radiansToDegrees(r)`, and
the integer helpers `lcm(a, b)`, `hypot(x, y)`, `factorial(n)`, and
`isPrime(n)`:

```klassic
import std.math.{pi, degreesToRadians, lcm, hypot, factorial, isPrime}

println(degreesToRadians(180.0))   // 3.141592653589793
println(lcm(4, 6))                 // 12
println(hypot(3, 4))               // 5.0
println(factorial(5))              // 120
println(isPrime(7))                // true
```

`Double` receivers gain `.toRadians()` / `.toDegrees()` for the same
conversions in method form:

```klassic
println((180.0).toRadians())       // 3.141592653589793
```

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

ADT-based Option/Maybe with both function-style helpers and
extension methods on `Option<a>`:

```klassic
import std.option.{some, none, getOrElse}

val maybe = some(7)
println(getOrElse(maybe, 0))     // 7   (function-style)
println(maybe.getOrElse(0))      // 7   (method-style)

println(none().isNone())         // true
println(some("hi").isSome())     // true
println(some(2).map((x) => x * 10).getOrElse(0)) // 20
```

Beyond `getOrElse` / `map` / `flatMap` / `orElse`, the module ships
`filter(o, p)`, `toList(o)`, `fold(o, ifNone, f)`, and `ifPresent(o, f)`
as free functions, each with a matching method twin `.filter`,
`.toList`, `.fold`, and `.ifPresent`. The same clean `map` / `filter` /
`fold` / `flatMap` names live on `std.result` and `std.list` too;
co-import them and reach for the **method form**, which dispatches by
receiver type, or qualify the free function with an alias:

```klassic
import std.option.{some, none, filter, fold}

println(filter(some(7), (x) => x > 3).getOrElse(0)) // 7   (free fn)
println(fold(some(10), 0, (x) => x * 2))            // 20

println(some(7).filter((x) => x > 3).getOrElse(0))  // 7   (method)
println(some(5).fold(0, (x) => x + 1))              // 6
println(some(5).toList())                           // [5]
some(99).ifPresent((x) => println("got " + toString(x)))  // got 99
```

The underlying ADT lives in the same file:

```klassic
enum Option<a> {
  case Some(value: a)
  case None
}
```

Pattern-match on it directly when the helpers don't fit:

```klassic
val n = some(42) match {
  case Some(v) => v
  case None    => 0
}
```

## std.result

ADT-based Result/Either with both function-style helpers and
extension methods on `Result<a, e>`:

```klassic
import std.result.{ok, err}

val good = ok("payload")
val bad  = err("nope")

println(good.isOk())                   // true
println(bad.isErr())                   // true
println(good.unwrapOr("fallback"))     // "payload"
println(bad.unwrapOr("fallback"))      // "fallback"
println(good.map((x) => x + "!").unwrapOr("")) // "payload!"
```

The chaining surface is `map(r, f)` / `flatMap(r, f)` / `andThen(r, f)`,
`mapErr(r, g)`, `orElse(r, h)`, `filter(r, p, errVal)`,
`fold(r, onOk, onErr)`, and `toOption(r)`, with method twins `.map`,
`.flatMap`, `.andThen`, `.mapErr`, `.orElse`, `.filter`, `.fold`, and
`.toOption`. `map` / `filter` / `fold` / `flatMap` / `orElse` are the
same clean names `std.option` and `std.list` use; under co-import
prefer the method form, which dispatches by receiver type:

```klassic
import std.result.{ok, err, andThen, fold}

println(andThen(ok(4), (x) => ok(x * 2)).unwrapOr(0))     // 8
println(fold(err("boom"), (x) => "ok", (m) => "err " + m)) // err boom

println(ok(4).flatMap((x) => ok(x + 10)).unwrapOr(0))     // 14
println(err("e").orElse((m) => ok(7)).unwrapOr(0))        // 7
println(ok(3).toOption().getOrElse(0))                    // 3
```

The underlying ADT:

```klassic
enum Result<a, e> {
  case Ok(value: a)
  case Err(message: e)
}
```

Direct pattern matching is also available:

```klassic
val message = ok(42) match {
  case Ok(v)  => "got " + toString(v)
  case Err(m) => "error: " + m
}
```

## std.map / std.set

Method-style helpers on `Map<k, v>` and `Set<a>`. The members are
extension methods, so they become available through dot syntax once
the stdlib bundle is loaded — no explicit import required.

```klassic
val users = %["alice": 1, "bob": 2]
println(users.containsKey("alice"))           // true
println(users.get("bob"))                     // 2
println(users.getOrElse("missing", 0))        // 0
println(users.sizeOf())                       // 2

val tags = %("rust", "klassic")
println(tags.containsElement("rust"))          // true
println(tags.sizeOf())                         // 2
```

`getOrElse(key, fallback)` is a small convenience on top of the
builtin — the underlying `Map#get` returns the raw value (or fails).
`sizeOf()` and `isEmptyMap()` / `isEmptySet()` avoid name clashes
with the top-level `size` / `isEmpty` builtins.

`std.map` also adds the immutable transforms `mapValues(f)`,
`filterValues(p)`, and `merge(other)` (right-hand entries win on a key
clash). They build on the `Map#put` / `Map#remove` / `Map#fromPairs` /
`Map#empty` builtins, which return fresh maps rather than mutating:

```klassic
val prices = %["apple": 100, "pear": 150]
println(prices.put("kiwi", 80).getOrElse("kiwi", 0))    // 80
println(prices.mapValues((v) => v - 10).getOrElse("apple", 0)) // 90
println((%["a": 1]).merge(%["a": 9, "b": 2]).getOrElse("a", 0)) // 9
```

Sets carry the algebra builtins `Set#union` / `Set#intersect` /
`Set#subtract` (plus `Set#add` / `Set#remove` / `Set#fromList` /
`Set#toList` / `Set#empty`), each with a dot-callable twin:

```klassic
val a = %(1, 2, 3)
val b = %(3, 4, 5)
println(a.union(b).toList())       // [1, 2, 3, 4, 5]
println(a.intersect(b).toList())   // [3]
println(a.subtract(b).toList())    // [1, 2]
```

For traversal and reshaping, `std.map` adds `toPairs(m)` (alias
`entries(m)`) to materialise the entries as `[key, value]` lists in
insertion order, `fold(m, init, f)` (alias `foldMap`) to fold over
all entries with `f(acc, key, value)`, and `mapKeys(m, g)` to rewrite
every key. The method twins are `.toPairs()` / `.entries()`,
`.mapKeys(g)`, and the curried `.foldMap(init)(f)`:

```klassic
import std.map.{toPairs, fold, mapKeys}

val m = %["a": 1, "b": 2, "c": 3]
println(toPairs(m))                              // [[a, 1], [b, 2], [c, 3]]
println(fold(m, 0, (acc, k, v) => acc + v))      // 6
println(mapKeys(m, (k) => k + "!").keys())       // [a!, b!, c!]
println(m.foldMap(0)((acc, k, v) => acc + v))    // 6
```

`std.set` mirrors this with `setMap(s, f)`, `setFilter(s, p)`, and
`setFold(s, init, f)`, each with a method twin (`.setMap` / `.setFilter`
and the curried `.setFold(init)(f)`):

```klassic
import std.set.{setMap, setFilter, setFold}

val s = %(1, 2, 3, 4)
println(setMap(s, (x) => x * 10).toList())       // [10, 20, 30, 40]
println(setFilter(s, (x) => x > 2).toList())     // [3, 4]
println(setFold(s, 0, (acc, x) => acc + x))      // 10
println(s.setFold(0)((acc, x) => acc + x))       // 10
```

## std.dir

Wraps the existing `Dir#*` builtins. All members delegate to their
builtin counterpart.

```klassic
import std.dir.{current, home, temp, isDirectory, list}

println(current())            // current working directory
println(home())               // user's home directory
println(list("/tmp"))         // directory entries
```

## std.process

Wraps `Process#exit`, `CommandLine#args`, and the StandardInput
helpers under one module-qualified namespace.

```klassic
import std.process.{args, stdinAll, exitWith}

val files = args()            // CommandLine#args()
val text  = stdinAll()        // stdin contents
if(isEmpty(files)) exitWith(1)
```

## std.env

Wraps the existing `Environment#*` builtins with shorter
module-qualified names.

```klassic
import std.env.{get, exists, vars, getOrElse}

println(getOrElse("HOME", "/unknown"))   // env var or fallback
println(exists("DEBUG"))                  // Boolean
```

`get(name)` is equivalent to `Environment#get(name)` (returns "" or
the empty string when missing, matching the builtin behaviour),
`exists` to `Environment#exists`, `vars()` to `Environment#vars()`,
and `getOrElse(name, fallback)` returns the env var's value when
present, otherwise the supplied fallback.

## std.file

Wraps the existing `FileInput#` / `FileOutput#` builtins so callers
can write `import std.file.{readAll, write}` instead of typing the
hash-prefixed builtin names directly. All members delegate to the
corresponding builtin, so error semantics and native-mode coverage
are unchanged.

```klassic
import std.file.{readAll, writeLines, exists}

if(exists("config.txt")) {
  println(readAll("config.txt"))
}
writeLines("out.txt", ["hello", "world"])
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

## std.json

JSON parsing and rendering, written in pure Klassic on top of the ADT
stack. `parse` returns `Result<Json, String>` with a character
position in every error message; `stringify` renders compact JSON.

```klassic
import std.json.{parse, stringify}

val rendered = parse("{\"a\": [1, 2, null], \"b\": \"hi\"}") match {
  case Ok(j)  => stringify(j)   // {"a":[1,2,null],"b":"hi"}
  case Err(e) => "ERR: " + e
}
println(rendered)

parse("[1, 2") match {
  case Ok(_)  => println("unreachable")
  case Err(e) => println(e)     // expected `,` or `]` at position 5
}
```

The value ADT:

```klassic
enum Json {
  case JNull
  case JBool(b: Bool)
  case JNum(n: Int)
  case JStr(s: String)
  case JArr(items: List<Json>)
  case JObj(entries: List<JsonMember>)
}
```

v1 boundaries: numbers are integers, and `\uXXXX` escapes report an
error rather than decoding.

## std.time

Wall-clock helpers over the `Time#nowMillis` builtin. `formatIso`
renders UTC (`YYYY-MM-DDTHH:MM:SS.mmmZ`) for non-negative epoch
milliseconds using integer-only civil-date conversion.

```klassic
import std.time.{nowMillis, nowIso, formatIso, durationMillis}

println(formatIso(0))              // 1970-01-01T00:00:00.000Z
println(formatIso(1718000000123))  // 2024-06-10T06:13:20.123Z
println(durationMillis(100, 350))  // 250
println(nowIso())                  // the current instant
```

The same civil-date conversion powers the component accessors
`year` / `month` / `day` / `hour` / `minute` / `second`, the
`toDate(millis)` record (with `year`, `month`, `day`, `hour`,
`minute`, `second` fields), the `formatHuman(millis)` renderer
(`YYYY-MM-DD HH:MM:SS`), and `parseIso(s)` which turns an ISO-8601
string back into epoch milliseconds:

```klassic
import std.time.{year, month, day, toDate, formatHuman, parseIso}

val ms = 1718000000123
println(year(ms))                              // 2024
println(month(ms))                             // 6
println(day(ms))                               // 10
val d = toDate(ms)
println(toString(d.hour) + ":" + toString(d.minute)) // 6:13
println(formatHuman(ms))                       // 2024-06-10 06:13:20
println(parseIso("2024-06-10T06:13:20.123Z"))  // 1718000000123
```

## Native availability

Nearly every stdlib module compiles natively: `import std.<X>` splices
the module's declarations into the translation unit (selective, bulk,
and aliased imports all work), so `std.list`, `std.string`,
`std.math`, `std.option`, `std.result`, `std.time`, and the rest run
in `klassic build` output just like in the evaluator. The one current
exception is `std.json`, whose `List<Json>` payload fields the native
enum lowering does not support yet — importing it in a native build
keeps the source-located "not yet available in native builds"
diagnostic and runs fine under the evaluator.
