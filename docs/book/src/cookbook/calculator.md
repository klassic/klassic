# Tiny calculator

Evaluate a single integer expression like `2 + 3 * 4` from the
command line. Demonstrates a minimal recursive-descent parser
hand-written in Klassic, plus left-to-right reduction over an
ordinary `List<String>` — automatic GC means the list, its string
elements, and every intermediate value are managed for you.

We split on spaces and only handle `+`, `-`, `*`, `/` in left-to-right
order — perfect for showing the moving parts without a full grammar.

```kl
val args = CommandLine#args()
if (size(args) != 1) {
  printlnError("usage: calc \"2 + 3 * 4\"")
  Process#exit(1)
}

val tokens = split(head(args), " ")
val n = size(tokens)

if (n == 0 || n - (n / 2) * 2 == 0) {
  printlnError("expected: NUM (OP NUM)*")
  Process#exit(2)
}

mutable acc = String#parseInt(head(tokens))
mutable rest = tail(tokens)
while (!isEmpty(rest)) {
  val op = head(rest)
  val rhs = String#parseInt(head(tail(rest)))
  if (op == "+") {
    acc = acc + rhs
  } else {
    if (op == "-") {
      acc = acc - rhs
    } else {
      if (op == "*") {
        acc = acc * rhs
      } else {
        if (op == "/") {
          if (rhs == 0) {
            printlnError("division by zero")
            Process#exit(3)
          }
          acc = acc / rhs
        } else {
          printlnError("unknown operator: " + op)
          Process#exit(4)
          acc = 0   // unreachable; aligns the branch types
        }
      }
    }
  }
  rest = tail(tail(rest))
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
- Every error path exits with a distinct status code so shell
  callers can tell what went wrong.
