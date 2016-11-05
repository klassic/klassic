# Record Syntax

```
record Person(name: String, age: Int)

val person2 = Person("Kota Mizushima", 33)
val person2 = Person(name: "Kota Mizushima", age: 33) // named argument ?
```

# Run Syntax

```
val xs = ${
  x <- [1, 2, 3]
  y <- [4, 5, 6]
  
  yield (x, y)
}
println(xs.toList) // => [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)]
```