# Type Classes

Klassic supports type classes with method dispatch resolved by the
type checker. Higher-kinded examples work too.

## Defining a class

```kl
typeclass Show<'a> where {
  show: ('a) => String
}
```

`'a` is a type variable. The class declares one method, `show`, of
the given signature.

## Implementing instances

```kl
instance Show<Int> where {
  def show(x: Int): String = "Int: " + x
}

instance Show<Bool> where {
  def show(b: Bool): String = if (b) "yes" else "no"
}
```

## Constrained polymorphism

Functions can require a class constraint two ways. Both compile to
the same thing.

### Inline shorthand (preferred when single-class)

```kl
def display<Show 'a>(x: 'a): String = show(x)
```

`<Show 'a>` declares the type variable `'a` and adds a `Show<'a>`
constraint in one go. Multiple constraints stack naturally:

```kl
typeclass Eq<'a> where {
  equals: ('a, 'a) => Boolean
}

instance Eq<Int> where {
  def equals(x: Int, y: Int): Boolean = x == y
}

def show_equal<Show 'a, Eq 'a>(x: 'a, y: 'a): String =
  if (equals(x, y)) show(x) + " equals " + show(y)
  else show(x) + " not equals " + show(y)
```

Different type variables can carry their own constraints:

```kl
def pair<Show 'a, Show 'b>(x: 'a, y: 'b): String =
  show(x) + " / " + show(y)
```

### Explicit `where` clause

The longer form is still accepted (and required for multi-argument
class constraints):

```kl
def display<'a>(x: 'a): String where Show<'a> = show(x)
```

Both forms compose — you can mix inline shorthand with extra `where`
constraints if that reads better:

```kl
def both<Show 'a>(x: 'a, y: 'a): String where Eq<'a> =
  if (equals(x, y)) show(x) else show(y)
```

When the type checker sees `display(42)`, it picks `Show<Int>` from
the available instances and inlines the right `show` implementation.

## Higher-kinded constraints

Type variables of kind `* -> *` work too:

```kl
typeclass Functor<'f> where {
  fmap: ('a) => ('b) => ('f<'a>) => 'f<'b>
}
```

You can use `instance Functor<List>` to declare how `fmap` lifts a
function over a list.

## Built-in operator classes

The arithmetic operators are themselves type-class-constrained, so you
get the same overloading for free — no `instance` declarations needed.
`+` requires `Plus` (the numeric types and `String`), while `-`, `*`,
`/`, and unary minus require `Num` (numbers only). An unannotated helper
therefore infers a *polymorphic* signature instead of being pinned to
`Int`:

```kl
def add(x, y) = x + y     // (a, a) -> a  where Plus<a>
println(add(1, 2))        // 3
println(add(1.0, 2.0))    // 3.0
println(add("a", "b"))    // ab

def diff(x, y) = x - y    // Num<a> — numbers only
println(diff(10, 3))      // 7
```

An operand the class doesn't cover is a compile-time error, exactly like
a missing user instance:

```kl
def add(x, y) = x + y
add(true, false)          // missing instance for Plus<Boolean>

def diff(x, y) = x - y
diff("a", "b")            // missing instance for Num<String>
```

A still-unconstrained operator variable — plain `1 + 2`, say — defaults
to `Int`, so existing integer code is unaffected.

## When to reach for type classes

- You want one name (`show`, `fmap`, `eq`) to resolve differently
  depending on the type at the call site.
- You want compile-time errors when a type lacks an instance.
- Records with fixed fields are simpler — only escalate to type
  classes when overloading really helps.
