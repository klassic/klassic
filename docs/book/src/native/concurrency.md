# Threads, Sleep, and Stopwatch

The native compiler implements three lightweight concurrency / timing
primitives. They are deliberately small — the goal is to cover the
80% of scripting use cases without a full runtime.

## Threads

```kl
thread(() => {
  println("from a queued thread")
})
println("from the main thread")
```

`thread(() => { ... })` queues the body to run after the main expression
finishes. The current native sample surface focuses on literal
bodies and lambda values; future iterations will widen what can be
launched from inside a thread.

You can also pass a saved lambda value:

```kl
val job = () => println("hello from job")
thread(job)
```

## Sleep

```kl
sleep(500)   // milliseconds
```

Both literal integer arguments and runtime integer values work. The
native code lowers to `nanosleep` (Windows target: `Sleep`). Negative
arguments emit a source-located diagnostic and exit non-zero.

## Stopwatch

```kl
val elapsed = stopwatch(() => {
  // do work
})
println("took #{elapsed} ms")
```

`stopwatch` accepts a literal `() => ...` lambda or a saved lambda
value, runs the body, and returns the elapsed milliseconds as an
`Int`. The native code lowers to `clock_gettime(CLOCK_MONOTONIC)`
(Windows target: `QueryPerformanceCounter` / `QueryPerformanceFrequency`).

## Wall-clock time

```kl
val nowMs = Time#nowMillis()
println("epoch ms: #{nowMs}")
```

`Time#nowMillis()` returns wall-clock milliseconds since the UNIX
epoch. Same surface in eval and native — the native code lowers to
`clock_gettime(CLOCK_REALTIME)` (Windows target:
`GetSystemTimeAsFileTime`, converted from its 100ns-tick FILETIME
epoch to unix millis). Use `stopwatch` for "how long did this take",
and `Time#nowMillis` for "what time is it right now" (timestamps, log
lines, deadlines).

## Integer exponentiation

```kl
println(Math#powInt(2, 10))  // 1024
println(Math#powInt(7, 0))   //    1
println(Math#powInt(-2, 3))  //   -8
```

`Math#powInt(base, exp)` computes `base^exp` for non-negative `exp`
using square-and-multiply (O(log exp) multiplies). Result wraps on
i64 overflow — same as the rest of Klassic's integer arithmetic.
Negative exponents are rejected at runtime with a stderr
diagnostic.

```kl
println(Math#sqrtInt(99))     //   9 (floor)
println(Math#sqrtInt(100))    //  10
println(Math#gcd(12, 18))     //   6
println(Math#gcd(-12, 18))    //   6 (operates on |a|, |b|)
```

`Math#sqrtInt(n)` floors the integer square root via Newton iteration
(O(log n) i64 divides). Negative `n` is a runtime error.
`Math#gcd(a, b)` is the Euclidean algorithm on the absolute values;
`Math#gcd(0, 0)` is `0`.

## String parsing

```kl
println(String#parseInt("42"))   // 42
println(String#parseInt("-7"))   // -7
println(String#parseInt("" + 5)) // 5
```

`String#parseInt(s)` reads a decimal integer (leading `+`/`-`
optional, no surrounding whitespace). Malformed input is a runtime
error. Native build folds literal/static-string arguments at compile
time and emits a strict parser for fixed-buffer runtime strings such
as concatenation or input-derived values.

## Pseudo-random numbers

```kl
Random#seed(42)
println(Random#nextInt(100))    // 69 — deterministic
println(Random#nextInt(100))    // 53
```

`Random#seed(s)` and `Random#nextInt(bound)` share a single LCG state
(Knuth's MMIX constants). The same seed produces the same sequence in
both eval and native modes — the cli_smoke suite pins this. The
generator is fine for shuffling and toy randomness; it is **not**
cryptographically secure.
