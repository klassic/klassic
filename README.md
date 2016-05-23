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
