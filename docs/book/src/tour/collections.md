# Collections

Klassic supports lists, maps, and sets as first-class literals. Their
separators can be commas, spaces, or newlines — pick whichever style
the surrounding code uses.

## Lists

```kl
val list1 = [1, 2, 3]
val list2 = [
  1
  2
  3
]
val list3 = [1 2 3]    // space-separated also works
```

### Common operations

| Function | Behaviour |
|---|---|
| `head(xs)` | First element. Empty list aborts. |
| `tail(xs)` | Every element except the head. |
| `size(xs)` | Element count. |
| `isEmpty(xs)` | Boolean. |
| `contains(xs, value)` | Membership. |
| `cons(value, xs)` | Prepend; method-style: `value #cons xs`. |
| `map(xs, f)` | Apply `f` to each element. |
| `foldLeft(xs)(initial)(f)` | Curried left fold. |
| `join(xs, sep)` | Concatenate string elements. |

```kl
val numbers = [1, 2, 3, 4, 5]
val doubled = numbers.map(_ * 2)
println(doubled)             // [2, 4, 6, 8, 10]

val sum = foldLeft(numbers)(0)((acc, n) => acc + n)
println(sum)                 // 15
```

## Maps

```kl
val ages = %["alice": 30, "bob": 27, "carol": 42]

println(ages.getOrElse("alice", 0))      // 30
println(ages.getOrElse("dave", 0))       // 0 — the default
println(ages.containsKey("dave"))        // false
println(ages.size())                     // 3

foreach (name in ages.keys()) {
  println(name + " is " + toString(ages.getOrElse(name, 0)))
}
```

### Map helpers

| Method | Behaviour |
|---|---|
| `m.getOrElse(key, default)` | The value for `key`, or `default` if absent — typed as the value type |
| `m.keys()` | The keys as `List<K>`, in insertion order |
| `m.values()` | The values as `List<V>`, in insertion order |
| `m.get(key)` | The value, or `null` if absent |
| `m.containsKey(key)` / `m.containsValue(value)` | Boolean |
| `m.size()` / `m.isEmpty()` | Self-explanatory |

`keys()` and `values()` return real lists, so they iterate with
`foreach` and feed the `std.list` helpers. Reach for `getOrElse` over
`get` when you have a sensible default — it keeps the result a plain
value instead of a nullable one.

## Sets

```kl
val tags = %("rust", "klassic", "compiler", "rust")
println(tags.size())              // 3 — duplicates collapse
println(tags.contains("rust"))    // true
```

### Set helpers

| Function | Behaviour |
|---|---|
| `Set#contains(s, value)` | Boolean |
| `Set#size(s)`, `Set#isEmpty(s)` | Self-explanatory |

## Static vs runtime

Most collection literals fold to compile-time constants if all their
elements are also static. The native compiler handles dynamic
collections through the GC heap and through fixed-buffer runtime
representations — see [Heap-Backed Lists](../gc/lists.md) and
[String-Keyed Maps](../gc/maps.md) when you need to grow collections
at runtime.
