## Klassic: Yet Another Klass-based Programming Language

[![Build Status](https://travis-ci.org/klassic/klassic.png?branch=master)](https://travis-ci.org/klassic/klassic)

Klassic **is** Yet Another Programming Language.  Although Klassic is a dynamically-typed
language currently, it will be a statically typed programming language.  Klassic has: 

* lexically-scoped variables
* first-class functions
* here document
* here expression(new!)
* etc.

Klassic will enable Object-Functional Programming.

## Syntax

### Variable Declaration

```
val one = 1
```

Declare variable `one` and `one` is bounded to `1`.  You can omit
semicolon(`;`) at the last of the declaration:

```
val name = expression [;]
```

### Anonymous Function

```
val add = (x, y) => x + y
```

Declare variable `add` and `add` is bounded to **the anonymous function** that
calculates `x + y`.  If an anonymous function has block body, you can write as
the following:

```
val printAndAdd = (x, y) => {
  println(x)
  println(y)
  x + y
}
```

Note that semicolon at the end of each expression of block can be omitted.

### Function Invocation

```
val add = (x, y) => x + y
println(add(1, 2))
```

A function can be invoked as the form `fun(p1, p2, ..., pn)`.  The evaluation
result of `fun` must be a function object.

### List Literal

```
val list1 = [1, 2, 3, 4, 5]
println(list1)
val list2 = [
  1
  2
  3
  4
  5
]
println(list2)
val list3 = [[1 2 3]
             [4 5 6]
             [7 8 9]]
```

A list literal can be expressed as the form `[e1, e2, ...,en]`.  Note that
separator characters have also line feeds and spaces in Klassic unlike other programming languages.

### Integer Literal

```
println(100)
println(200)
println(300)
```

Klassic supports integer literal.  The max value of integer literals is
`Int.MaxValue` in Scala and the min value of integer literals is `Int.MinValue`
in Scala.

### Long Literal

```
println(100L)
println(200L)
println(300L)
```

Klassic also supports long literal.  The suffix of long literal is `L`.  The max
value of long literals is `Long.MaxValue` in Scala and the min value of long
literals is `Long.MinValue` in Scala.
