# Klassic: A Programming Language which enables Object-Functional Programming

![Build Status](https://github.com/klassic/klassic/actions/workflows/ci.yml/badge.svg)

Klassic is yet another statically typed programming language.  Klassic has: 

* Powerful Type System
  * based on Hindley-Milner type system
  * support object system based on row polymorphism
* Lexically-scoped Variables
* First-class Functions
* String Interpolation
  * found in Ruby, Scala, Kotlin, etc.
* Loop Expression
  * `while` and `foreach`
* Cleanup Expression
* Space-sensitive and Line-sensitive Syntax
  * list literals
  * map literals
  * set literals
* Java FFI
* , etc.

Klassic will enable object-functional programming.

## Instalattion and Quick Start
It requires Java 8 or later.

You can download the binary distribution (executable jar) from the [release page](https://github.com/klassic/klassic/releases/tag/releases%2Fv0.1.0-alpha).  Put the file on an directory and execute the klassic.jar by `java -jar` command:

```
$ java -jar klassic.jar

Usage: java -jar klassic.jar (-f <fileName> | -e <expression>)
<fileName>   : read a program from <fileName> and execute it
-e <expression> : evaluate <expression>
```

Write the folowing lines and save it to `hello.kl`

```scala
println("Hello, World!")
```

And run the interpreter by `java -jar klassic.jar hello.kl`:

```
$ java -jar klassic.jar hello.kl

Hello, World!
```

## Syntax

### Variable Declaration

```
val one = 1
```

Declare variable `one` and `one` is bound to `1`.  You can omit
semicolon(`;`) at the last of the declaration:

```
val name = expression [;]
```

A variable declared by `val` cannot change its value.  If you want to change the value of
the variable, use `mutable` instead:

```
mutable i = 1
i = i + 1 // OK
let j = 1
// j = j + 1 NG
```

### Function Literal

```
val add = (x, y) => x + y
```

Declare variable `add` and `add` is bounded to **the function literal** that
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

### Named Function

If you want to define recursive functions, anonymous function literal cannot be used.
Instead, you can use the notation for recursive functions:

```
def fact(n) = if(n < 2) 1 else n * fact(n - 1)
fact(0) // 1
fact(1) // 1
fact(2) // 2
fact(3) // 6
fact(4) // 24
fact(5) // 120
// The result of type inference of fact is : Int => Int
```

### Method Invocation

```
val list = new java.util.ArrayList
list->add(1)
list->add(2)
list->add(3)
list->add(4)
println(list)
```

Currently, only method invocations to Java objects are acceptable.  Boxing of primitive types 
is automatically done.

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
```

A list literal can be expressed as the form `[e1, e2, ...,en]`.  Note that
separator characters have also line feeds and spaces in Klassic unlike other programming languages.

```
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

The type of list literal is a instance of special type constructor `List<'a>`.

### Map Literal

```
val map = %["A": 1, "B": 2]
map Map#get "A" // => 1
map Map#get "B" // => 2
map Map#get "C" // => null
```

A map literal can be expressed as the form `%[k1:v1, ..., kn:vn]` (`kn` and `vn` are expressions).  Note that
separator characters also include line feeds and spaces in Klassic unlike other programmign languages:

```
val map2 = %[
  "A" : 1
  "b" : 2
]
```

The type of map literal is a instance of special type constructor `Map<'k, 'v>`.

### Set Literal

A map literal can be expressed as the form `%(v1, ..., vn)` (`vn` are expressions).  Note that
separator characters also include line feeds and spaces in Klassic unlike other programmign languages:

```
val set1 = %(1, 2, 3)
```

```
val set2 = %(1 2 3) // space is omitted
```

```
val set3 = %(
  1
  2
  3
)
```

The type of set literal is a instance of special type constructor `Set<'a>`.

### Cleanup Expression

Cleanup clauses are executed after the blocks are executed.  For example,

```
val i = 0
while(i < 10) {
  i = i + 1
} cleanup {
  println(i)
}
```

the above program prints `10`.  Each block can at most one cleanup clause.

### Numeric Literal

Klassic supports various literal.  The followings are explanations:

### Int

```
println(100)
println(200)
println(300)
```

The max value of Int literals is `Int.MaxValue` in Scala and the min value of integer literals is 
`Int.MinValue` in Scala.

### Byte

The suffix of byte literal is `BY`.  The max value of long literals is `Byte.MaxValue` in Scala and 
the min value of long literals is `Byte.MinValue` in Scala.

```
println(127BY)
println(-127BY)
println(100BY)
```

### Short

The suffix of short literal is `S`.  The max value of long literals is `Short.MaxValue` in Scala and 
the min value of long literals is `Short.MinValue` in Scala.

```
println(100S)
println(200S)
println(300S)
```

### Long

```
println(100L)
println(200L)
println(300L)
```

The suffix of long literal is `L`.  The max value of long literals is `Long.MaxValue` in Scala and 
the min value of long literals is `Long.MinValue` in Scala.

### Double

```
println(1.0)
println(1.5)
```

The max value of double literal is `Double.MaxValue` in Scala and the min value of double literal is `Double.MinValue`
in Scala.

### Float

```
println(1.0F)
println(1.5F)
```

The max value of float literal is `Float.MaxValue` in Scala and the min value of float literal is `Float.MinValue`
in Scala.

### Comment

Klassic provides two kinds of comment

### (Nestable) Block Comment

```
1 + /* nested
  /* comment */ here */ 2 // => 3
```

### Line comment

```
1 + // comment
    2 // => 3
```

## Type System

Klassic is a statically-typed programming language.  A characteristic of type
system of Klassic is `restricted` subtyping.  `restricted` means that implicit
upcast is not allowed and it must be specified explicitly if you need it.

### Hindley-Milner Type Inference

Klassic's type inference is based on HM.  It means that type annotations
is not required in many cases:

```
def fold_left(list) = (z) => (f) => {
  if(isEmpty(list)) z else fold_left(tail(list))(f(z, head(list)))(f)
}
// The result of type inference: List<'a> => 'b => (('b, 'a) => 'b) => 'b
```

### Row Polymorphism

Klassic has simple object system based on row polymorphism.  For example,

```
def add(o) = {
  o.x + o.y
}
```

the type of above program is inferred:

```
add: { x: Int; y: Int; ... } 
```

It means that `add` function accepts any object that has field `x` and field `y`.
Although it is not subtyping strictly, many situations that need subtyping are
covered.

### Type Cast

In some cases, escape hatches from type system are required. In such cases,
user can insert cast explicitly.

```
val s: * = (100 :> *) // 100 is casted to dynamic type ( `*` )
```

## Built-in Functions

Klassic supports some kind of built-in functions.

### Standard output Functions

- `println: (param:Any) => Any`  
    display the `param` into the standard output.  
    ```
    println("Hello, World!")
    ```

- `printlnError: (param:Any) => Any`  
    display the `param` into the standard error.  
    ```
    printlnError("Hello, World!")
    ```

### String Functions

- `substring: (s:String, begin:Int, end:Int) => String`  
    Returns a substring of the String `s`. The substring begins at the index `begin` and ends at the index `end` - 1.  
    ```
    substring("FOO", 0, 1) // => "F"
    ```

- `at: (s:String, index:Int) => String`  
    Returns a String with a character value at the index `index` of the String `s`.  
    ```
    at("BAR", 2) // => "R"
    ```

- `matches: (s:String, regex:String) => Boolean`  
    Returns true if the String `s` matches the regular expression `regex`, false otherwise.  
    ```
    val pattern = "[0-9]+"
    matches("199", pattern) // => true
    matches("a", pattern)   // => false
    ```

### Numeric Functions

- `sqrt: (value:Double) => Double`  
    Returns the square root of the Double `value`.
    ```
    sqrt(2.0) // => 1.4142135623730951
    sqrt(9.0) // => 3.0
    ```
- `int: (vaue:Double) => Int`  
    Returns the Double `value` as the Int value.
    ```
    int(3.14159265359) // => 3
    ```

- `double: (value:Int) => Double`  
    Returns the Int `value` as the Double value.  
    ```
    double(10) // => 10.0
    ```

- `floor: (value:Double) => Int`  
    Returns the truncated Double `value` as the Int value.
    ```
    floor(1.5) // => 1
    floor(-1.5) // => -1
    ```

- `ceil: (value:Double) => Int`  
    Returns the rounded-up Double `value` as the Int value.
    ```
    ceil(4.4)  // => 5
    ceil(4.5)  // => 5
    ceil(-4.4) // => -4
    ceil(-4.5) // => -4
    ```

- `abs: (value:Double) => Double`  
    Returns the absolute value of the Double `value`.
    ```
    abs(10.5)  // => 10.5
    abs(-10.5) // => 10.5
    ```

### List Functions

- `map: (list:List<'a>) => (fun:('a) => 'b) => List<'b>`  
    Returns a new List consisting of the results of applying the given function `fun` to the elements of the given List `list`.
    ```
    map([1 2 3])((x) => x + 1) // => [2 3 4]
    map([2 3 4]){x => x + 1}   // => [3 4 5]
    ```

- `head: (list:List<'a>) => List<'a>`  
  Returns the first element of the List `list`.
  ```
  head([1 2 3 4]) // => 1
  ```

- `tail: (list:List<'a>) => List<'a>`  
    Returns a new List consisting of the elements of the given List `list` except for the first element.
    ```
    tail([1 2 3 4]) // => [2 3 4]
    ```

- `cons: (value:'a) => (list:List<'a>) => List<'a>`  
    Creates a new List, the head of which is `value` and the tail of which is `list`.  
    ```
    cons(1)([2 3 4]) // => [1 2 3 4]
    ```

- `size: (list:List<'a>) => Int`  
    Returns the size of the List `list`.  
    ```
    size([1 2 3 4 5]) // => 5
    ```

- `isEmpty: (list:List<'a>) => Boolean`  
    Returns true if the List `list` is empty, false otherwise.  
    ```
    isEmpty([])       // => true
    isEmpty([1 2 3])  // => false
    ```

- `foldLeft: (list:List<'a>) => (acc:'b) => (fun:('b, 'a) => 'b) => 'b`  
    Applies a function `fun` to a start value `acc` and all elements of the List `list`, going left to right.
    ```
    foldLeft([1 2 3 4])(0)((x, y) => x + y)         // => 10
    foldLeft([1.0 2.0 3.0 4.0])(0.0){x, y => x + y} // => 10.0
    foldLeft([1.0 2.0 3.0 4.0])(1.0){x, y => x * y} // => 24.0
    ```

### Thread Functions

- `thread: (fun:() => Unit) => Unit`  
    Creates a new thread and starts runnng the passed argument function `fun` asynchronously.  
    ```
    thread(() => {
      sleep(1000)
      println("Hello from another thread.")
    })
    println("Hello from main thread.")
    // => "Hello from main thread."
    // => "Hello from another thread."
    ```

- `sleep: (millis:Int) => Unit`  
    Causes the current thread to sleep for the `millis` milliseconds.
    ```
    sleep(1000)
    ```

### Utility Functions

- `stopwatch: (fun:() => Unit) => Int`  
    Returns the time in milliseconds taken to evaluate the passed argument function `fun`.
    ```
    val time = stopwatch( => {
      sleep(1000)
      println("1")
    })
    println("it took #{time} milli seconds")
    ```

- `ToDo: () => Unit`  
    Throws `klassic.runtime.NotImplementedError` when evaluated.
    ```
    ToDo()  // => throw NotImplementedError
    ```

### Assertion Functions

- `assert: (condition:Boolean) => Unit`  
    Asserts that the `condtion` should be true, and throws `klassic.runtime.AssertionError` if the `condition` is false.
  ```
  assert(2 == 1 + 1)  // => OK
  assert(3 > 5)       // => NG: AssertionError
  ```

- `assertResult: (expected:Any)(actual:Any) => Unit`  
    Asserts that the `actual` value should be equal to the `expected` value, and throws `klassic.runtime.AssertionError` if the `actual` value is not equal to the `expected` value.
    ```
    val add = (x, y) => {
      x + y
    }
    assertResult(5)(add(2, 3))  // => OK
    assertResult(2)(add(1, 2))  // => NG: AssertionError
    ```

### Interoperating Functions

- `url: (value:String) => java.net.URL`  
    Creates new `java.net.URL` object from a String `value`.
    ```
    url("https://github.com/klassic/klassic")
    ```

- `uri: (value:String) => java.net.URI`  
    Creates new `java.net.URI` object from a String `value`.
    ```
    uri("https://github.com/klassic/klassic")
    ```

- `desktop: () => java.awt.Desktop`  
    Returns the Desktop instance of the current browser context via Java Desktop API.
    ```
    desktop()->browse(uri("https://github.com/klassic/klassic"))
    ```
