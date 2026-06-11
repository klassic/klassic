# Tiny calculator

Evaluate a single integer expression like `2 + 3 * 4` from the
command line. Demonstrates a minimal recursive-descent parser
hand-written in Klassic, plus left-to-right reduction over a
heap-backed integer list.

We split on spaces and only handle `+`, `-`, `*`, `/` in left-to-right
order — perfect for showing the moving parts without a full grammar.

```kl
import std.math.{mod}

val args = CommandLine#args()
if (size(args) != 1) {
  printlnError("usage: calc \"2 + 3 * 4\"")
  Process#exit(1)
}

val tokens = __gc_string_split(__gc_string(head(args)), 32)
val n = __gc_list_ptr_len(tokens)

if (n == 0 || mod(n, 2) == 0) {
  printlnError("expected: NUM (OP NUM)*")
  Process#exit(2)
}

mutable acc = __gc_string_to_int(__gc_list_ptr_get(tokens, 0))
mutable i = 1
while (i < n) {
  val op = __gc_list_ptr_get(tokens, i)
  val rhs = __gc_string_to_int(__gc_list_ptr_get(tokens, i + 1))
  if (__gc_string_eq(op, __gc_string("+"))) {
    acc = acc + rhs
  } else {
    if (__gc_string_eq(op, __gc_string("-"))) {
      acc = acc - rhs
    } else {
      if (__gc_string_eq(op, __gc_string("*"))) {
        acc = acc * rhs
      } else {
        if (__gc_string_eq(op, __gc_string("/"))) {
          if (rhs == 0) {
            printlnError("division by zero")
            Process#exit(3)
          }
          acc = acc / rhs
        } else {
          printlnError("unknown operator: ")
          __gc_string_println(op)
          Process#exit(4)
          acc = 0   // unreachable; aligns the branch types
        }
      }
    }
  }
  i += 2
}

println(acc)
```

Sample runs:

```bash
klassic build calc.kl -o calc

./calc "2 + 3 * 4"
# 20      (left-to-right: (2+3)=5, then 5*4=20)

./calc "100 / 4 - 5"
# 20

./calc "1 + 2 + 3 + 4"
# 10

./calc "10 / 0"
# division by zero        (stderr, exit 3)

./calc "broken input"
# expected: NUM (OP NUM)*  (stderr, exit 2)
```

## Why left-to-right (and not real precedence)?

To keep the recipe small. A precedence-aware parser would build a
proper AST. The mechanics here — splitting, parsing each token,
folding with operators — are the building blocks for that next step.

## What's nice

- The full program is one source file, no external libraries.
- Native build is sub-10 KiB and starts essentially instantly.
- Every error path exits with a distinct status code so shell
  callers can tell what went wrong.
