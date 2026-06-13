# Evaluator Builtins

A handful of builtins are available under the evaluator but not yet in
the native backend. Using them in a `klassic build` fails with the
source-located `native call target is not supported by the native
compiler yet` diagnostic; under `klassic`, `klassic -e`, and the REPL
they work as shown below.

## Floating-point math

These take and return `Double` (except `round`, which returns an
`Int`). They complement the native-friendly integer helpers
(`Math#powInt`, `Math#sqrtInt`, `Math#gcd`).

| Builtin | Example | Result |
|---|---|---|
| `round(x)` | `round(3.7)` | `4` (Int) |
| `pow(b, e)` | `pow(2.0, 10.0)` | `1024.0` |
| `sqrt(x)` | `sqrt(2.0)` | `1.4142135623730951` |
| `sin(x)` / `cos(x)` / `tan(x)` | `sin(0.0)` | `0.0` |
| `asin(x)` / `acos(x)` / `atan(x)` | `atan(1.0)` | `0.7853981633974483` |
| `atan2(y, x)` | `atan2(1.0, 1.0)` | `0.7853981633974483` |
| `exp(x)` | `exp(0.0)` | `1.0` |
| `log(x)` / `log10(x)` / `log2(x)` | `log2(8.0)` | `3.0` |

```kl
println(round(3.7))         // 4
println(pow(2.0, 10.0))     // 1024.0
println(log2(8.0))          // 3.0
println(atan2(1.0, 1.0))    // 0.7853981633974483
```

`String#parseDouble(s)` parses a floating-point literal, mirroring the
native-supported `String#parseInt`.

## Formatting and padding

`format`, `padStart`, and `padEnd` are covered on the
[Strings](../tour/strings.md) tour page — they are evaluator-only too.

## Recoverable parsing

The non-aborting parse builtins also live in the evaluator only:

```kl
println("42".toIntOr(0))      // 42
println("oops".toIntOr(-1))   // -1
println("4.2".isFloat())      // true
println("x".isInteger())      // false
```

See [Strings → Parsing numbers](../tour/strings.md) for the full
surface, including the `String#parseIntOr` / `String#parseDoubleOr`
builtin forms and the `std.string` free functions.

## Running a subprocess

`Process#run(command, args)` runs an external command and returns a
record `{ stdout, stderr, exitCode }`. `command` is the program name
(or path); `args` is a `List<String>` of arguments.

```kl
val result = Process#run("echo", ["hello"])
println(result.exitCode)                       // 0
println("[" + result.stdout + "]")             // [hello\n]
println("[" + result.stderr + "]")             // []
```

`stdout` and `stderr` are captured verbatim — note that `echo` appends
its own trailing newline. A non-zero `exitCode` is reported, not
thrown:

```kl
val failed = Process#run("false", [])
println(failed.exitCode)                        // 1
```
