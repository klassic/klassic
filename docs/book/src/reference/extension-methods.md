# Extension Methods

Klassic lets user code add new methods to existing types with the
`extension (this: T) { def name(args) = ... }` block. The receiver
is bound implicitly via the `this:` identifier in the header, so
method bodies can reference it without naming it as an extra
parameter:

```klassic
extension (this: String) {
  def shout()        = toUpperCase(this)
  def repeatN(n)     = if((n <= 0)) "" else this + this.repeatN(n - 1)
  def lines()        = split(this, "\n")
}

println("hi".shout())        // "HI"
println("ab".repeatN(3))     // "ababab"
println("a\nb\nc".lines())   // ["a", "b", "c"]
```

The compiler looks up the receiver's hardcoded builtin methods first
(`s.length()`, `xs.map(f)`, etc.), then a user-extension table keyed
by receiver type, before falling back to record-field access. That
ordering means user extensions cannot shadow a builtin method name
on the same type, but they can add brand-new method names freely.

## Generic receivers

Extension blocks accept type parameters before the receiver
parentheses:

```klassic
extension <a>(this: List<a>) {
  def headOr(d) = if(isEmpty(this)) d else head(this)
}

println([1, 2, 3].headOr(0))    // 1
println([].headOr(99))          // 99
```

The type variable is in scope across every method body and the
receiver annotation. Dispatch normalises the receiver to its outer
constructor (`List`, `Map`, `Set`, ...) and ignores generic
arguments, so the same extension serves `List<Int>`, `List<String>`,
and so on.

## Recursive method calls

Inside a method body, `this` and the receiver-side dispatch table
are the recursion handle:

```klassic
extension (this: String) {
  def replicateBy(n) = if((n <= 0)) "" else this + this.replicateBy(n - 1)
}
```

Function-form recursion (`replicateBy(n - 1)`) is also valid, but
the receiver must be supplied explicitly — every extension method
is desugared into a regular function whose first parameter is the
receiver.

## Native compilation

Native build (`klassic build file.kl`) supports extension methods. Each
method desugars into an ordinary function whose first parameter is the
receiver, so `"hi".shout()` compiles to the same code as `shout("hi")`
and produces identical output under the evaluator and a native
executable.

## Stdlib usage

The v0.2 standard library uses extension methods to expose
ergonomic dot-call surfaces alongside the existing function-style
prelude. See
[Standard Library Modules](./stdlib-modules.md) for the full list.
