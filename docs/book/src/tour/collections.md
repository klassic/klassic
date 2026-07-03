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

### Sorting, zipping, grouping

`std.list` ships higher-order helpers, each with a dot-callable
twin. `sort` / `.sorted()` order a list of numbers ascending; `zip` /
`.zipWith(ys)` pair two equal-typed lists into 2-element lists;
`groupBy` / `.groupedBy(key)` bucket by a key into `{key, items}`
records; `mkString` / `.mkStringWith(sep)` render with `toString` and
a separator.

```kl
import std.list.{sort, zip, groupBy, mkString}
import std.math.{mod}

println([5, 3, 8, 1].sorted())                    // [1, 3, 5, 8]
println(sort([5, 3, 8, 1]))                       // [1, 3, 5, 8]
println(zip([1, 2, 3], [10, 20, 30]))             // [[1, 10], [2, 20], [3, 30]]
println(groupBy([1, 2, 3, 4, 5, 6], (n) => mod(n, 2)))
                                  // [#(1, [1, 3, 5]), #(0, [2, 4, 6])]
println(mkString([1, 2, 3], " - "))               // 1 - 2 - 3
```

`zip` stops at the shorter list, and both inputs must share an element
type (each pair `[x, y]` is itself a list). `sortBy(xs, key)` /
`.sortedBy(key)` and `partition(xs, p)` / `.partitionBy(p)` round out
the set.

### Slicing, scanning, indexing

A second wave of `std.list` helpers covers the slicing and scanning
patterns. `flatten` concatenates a list of lists; `takeWhile` /
`dropWhile` split on a predicate from the front; `chunk(n)` packs the
list into fixed-size sublists; `slice(from, to)` is the half-open
`[from, to)` window; `scanLeft(init, f)` is a fold that keeps every
intermediate accumulator.

```kl
import std.list.{flatten, takeWhile, chunk, slice, scanLeft}

println(flatten([[1, 2], [3], [4, 5]]))          // [1, 2, 3, 4, 5]
println(takeWhile([1, 2, 3, 1], (x) => x < 3))   // [1, 2]
println(chunk([1, 2, 3, 4, 5], 2))               // [[1, 2], [3, 4], [5]]
println(slice([10, 20, 30, 40, 50], 1, 4))       // [20, 30, 40]
println(scanLeft([1, 2, 3], 0, (a, b) => a + b)) // [0, 1, 3, 6]
```

`maxBy(xs, key)` / `minBy(xs, key)` pick the element with the
greatest / smallest key, and `zipWithIndex` pairs each element with
its zero-based index into a list of `{item, index}` records:

```kl
import std.list.{zipWithIndex, maxBy}

println(maxBy([3, 1, 4, 1, 5], (x) => x))        // 5
foreach (p in zipWithIndex(["a", "b", "c"])) {
  println(toString(p.index) + ": " + p.item)     // 0: a / 1: b / 2: c
}
```

Each has a dot-callable twin: `.flatten` is implicit through the free
function, while `.takeWhileBy(p)`, `.dropWhileBy(p)`, `.chunked(n)`,
`.sliced(from, to)`, `.maxByKey(key)`, `.minByKey(key)`, and
`.withIndex()` mirror the free functions. `.scanned(init)` is curried,
so the folding function comes second:

```kl
println([1, 2, 3, 4, 5].chunked(2))              // [[1, 2], [3, 4], [5]]
println([10, 20, 30, 40].sliced(1, 3))           // [20, 30]
println([1, 2, 3].scanned(0)((a, b) => a + b))   // [0, 1, 3, 6]
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

### Building and transforming maps

Maps are immutable: `put` and `remove` return a fresh map rather than
mutating the receiver. `mapValues` rewrites every value, and `merge`
overlays another map (the right-hand entries win on a key clash).

```kl
val prices = %["apple": 100, "pear": 150]
val withKiwi = prices.put("kiwi", 80)
println(withKiwi.getOrElse("kiwi", 0))            // 80

val cheaper = prices.mapValues((v) => v - 10)
println(cheaper.getOrElse("apple", 0))            // 90

val merged = (%["a": 1, "b": 2]).merge(%["b": 20, "c": 30])
println(merged.keys())                            // [a, b, c]
println(merged.getOrElse("b", 0))                 // 20
```

`Map#fromPairs([[k, v], ...])` builds a map from 2-element lists, and
`Map#empty()` gives the empty map to fold into. `filterValues(p)`
keeps only the entries whose value satisfies `p`.

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

### Set algebra

Sets support the usual algebra, with method twins for chaining.
`add` / `remove` return a fresh set; `union` / `intersect` /
`subtract` combine two; `toList` materialises the members in order.

```kl
val a = %(1, 2, 3)
val b = %(3, 4, 5)
println(a.union(b).toList())       // [1, 2, 3, 4, 5]
println(a.intersect(b).toList())   // [3]
println(a.subtract(b).toList())    // [1, 2]
```

`Set#fromList(xs)` collapses duplicates into a set, and `Set#empty()`
seeds an empty one.

## Static vs runtime

Most collection literals fold to compile-time constants if all their
elements are also static. The native compiler handles dynamic
collections through fixed-buffer runtime representations, growing
them automatically as elements are added — you never manage that
storage directly.
