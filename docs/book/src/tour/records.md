# Records

Records are Klassic's flat data structures. They come in two flavours.

## Nominal records

Declared with `record Name { ... }`, they get a generated constructor
named after the type:

```kl
record Point {
  x: Int
  y: Int
}

val origin = #Point(0, 0)
println(origin.x)    // 0
println(origin.y)    // 0
```

## Structural records

Built inline with `record { ... }`, they don't need a name and are
typed structurally — any function expecting `{ x: Int; y: Int }`
accepts them:

```kl
val p = record { x: 10; y: 20 }
println(p.x + p.y)   // 30

def magnitude_squared(o) = o.x * o.x + o.y * o.y
println(magnitude_squared(p))                    // 500
println(magnitude_squared(record { x: 3; y: 4 }))  // 25
```

## Field selection through type classes

Klassic's row-polymorphic field types let generic functions accept any
record that has a particular field, regardless of what other fields it
also carries:

```kl
def name_of(o: { name: String }): String = o.name
println(name_of(record { name: "Klassic" }))
```

The expected-type-driven check looks for the fields the function
mentions and ignores the rest, so you can pass richer records freely.

## Method-style record fields

Records can carry lambda values that are called with the receiver:

```kl
val counter = record {
  n: 5;
  bump: (self) => self.n + 1
}
println(counter.bump(counter))
```
