# Standard Prelude

Every Klassic program — whether you launch it via `klassic`,
`klassic -e`, the REPL, or `klassic build` — runs with a small set
of helpers loaded into the root scope. They live in
[`stdlib/prelude.kl`](https://github.com/klassic/klassic/blob/main/stdlib/prelude.kl)
and are bundled into the compiler at build time, so there is no
import to write.

The names are prefixed with `stdlib` to keep them out of the way of
user code. Once a real `import stdlib.{...}` story lands, the
prefix will move into a proper `stdlib.*` namespace.

The prelude is implemented in plain Klassic — no `__gc_*` builtins,
no native-only escape hatches — so every helper works identically
under the evaluator and the native compiler.

## Numeric helpers

| Helper | Type sketch | Example |
| --- | --- | --- |
| `stdlibMod(n, m)` | `(Int, Int) => Int` | `stdlibMod(17, 5)` → `2` |
| `stdlibIsEven(n)` | `(Int) => Boolean` | `stdlibIsEven(4)` → `true` |
| `stdlibIsOdd(n)` | `(Int) => Boolean` | `stdlibIsOdd(7)` → `true` |
| `stdlibMax(a, b)` | `(Int, Int) => Int` | `stdlibMax(10, 20)` → `20` |
| `stdlibMin(a, b)` | `(Int, Int) => Int` | `stdlibMin(10, 20)` → `10` |

`stdlibMod` exists because Klassic does not have a `%` operator —
`%` is a literal prefix for map and set syntax (`%["k": 1]`,
`%(1 2)`). `stdlibIsEven` / `stdlibIsOdd` are tiny conveniences
defined in terms of `stdlibMod`.

## List building

| Helper | Type sketch | Example |
| --- | --- | --- |
| `stdlibRange(start, end)` | `(Int, Int) => List<Int>` | `stdlibRange(0, 5)` → `[0, 1, 2, 3, 4]` |
| `stdlibRangeInclusive(start, end)` | `(Int, Int) => List<Int>` | `stdlibRangeInclusive(1, 4)` → `[1, 2, 3, 4]` |
| `stdlibReplicate(n, x)` | `(Int, 'a) => List<'a>` | `stdlibReplicate(3, 7)` → `[7, 7, 7]` |

Both range helpers count up by one. They yield `[]` when
`start >= end` (or `start > end` for the inclusive form), so they
are safe to feed into a `foldLeft` without an outer guard.

## List slicing

| Helper | Type sketch | Example |
| --- | --- | --- |
| `stdlibTake(xs, n)` | `(List<'a>, Int) => List<'a>` | `stdlibTake([10, 20, 30, 40, 50], 3)` → `[10, 20, 30]` |
| `stdlibDrop(xs, n)` | `(List<'a>, Int) => List<'a>` | `stdlibDrop([10, 20, 30, 40, 50], 2)` → `[30, 40, 50]` |
| `stdlibLast(xs)` | `(List<'a>) => 'a` | `stdlibLast([1, 2, 3])` → `3` |

`stdlibTake` and `stdlibDrop` clamp out-of-range arguments — `take`
returns at most as many elements as the list has; `drop` past the
end yields `[]`. `stdlibLast` assumes the list is non-empty (an
empty list would call `head` on `[]` and produce a runtime error).

## List querying

| Helper | Type sketch | Example |
| --- | --- | --- |
| `stdlibFilter(xs, p)` | `(List<'a>, ('a) => Boolean) => List<'a>` | `stdlibFilter([1, 2, 3, 4], stdlibIsEven)` → `[2, 4]` |
| `stdlibFind(xs, p)` | `(List<'a>, ('a) => Boolean) => 'a` | `stdlibFind([1, 2, 3, 4], (x) => x > 2)` → `3` |
| `stdlibAny(xs, p)` | `(List<'a>, ('a) => Boolean) => Boolean` | `stdlibAny([1, 2, 3], (x) => x > 2)` → `true` |
| `stdlibAll(xs, p)` | `(List<'a>, ('a) => Boolean) => Boolean` | `stdlibAll([2, 4, 6], stdlibIsEven)` → `true` |
| `stdlibCount(xs, p)` | `(List<'a>, ('a) => Boolean) => Int` | `stdlibCount([1, 2, 3, 4, 5], stdlibIsEven)` → `2` |

`stdlibFind` returns `null` if no element matches. The other
helpers all yield total values for any input.

## List reductions

| Helper | Type sketch | Example |
| --- | --- | --- |
| `stdlibSum(xs)` | `(List<Int>) => Int` | `stdlibSum([1, 2, 3, 4, 5])` → `15` |
| `stdlibProduct(xs)` | `(List<Int>) => Int` | `stdlibProduct([1, 2, 3, 4])` → `24` |

Both are thin wrappers around `foldLeft(xs)(seed)((acc, x) => ...)`
— exactly the form you would write by hand. Use them when you want
the call site to read like a verb rather than a fold.

## Sample program

```kl
println(stdlibRange(0, 5))
println(stdlibFilter([1, 2, 3, 4, 5, 6], (x) => stdlibIsEven(x)))
println(stdlibSum([1, 2, 3, 4, 5]))
println(stdlibCount([1, 2, 3, 4, 5], (x) => stdlibIsEven(x)))
println(stdlibLast([10, 20, 30]))
```

```text
[0, 1, 2, 3, 4]
[2, 4, 6]
15
2
30
```

A full smoke test that exercises every helper in the prelude lives
at
[`test-programs/stdlib_prelude.kl`](https://github.com/klassic/klassic/blob/main/test-programs/stdlib_prelude.kl)
— the sample harness runs it through the evaluator and the native
compiler, so any regression in either path trips a CI failure.

## How the prelude is loaded

The compiler bundles `stdlib/prelude.kl` via Rust's `include_str!`
and feeds it to the evaluator (or native compiler) as a *separate*
translation unit, then evaluates the user file. User-facing line
numbers stay 1-based: a runtime error on user line 2 still reports
`<file>:2:1:`, not a prelude-shifted line. See
[`src/main.rs`](https://github.com/klassic/klassic/blob/main/src/main.rs)
for the entry point and `klassic-native::compile_source_with_prelude_to_elf`
for the native-side span remap.
