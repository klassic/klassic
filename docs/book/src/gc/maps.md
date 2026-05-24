# String-Keyed Maps

`smap` is the heap-backed dictionary in Klassic native code. Keys are
heap strings; values are arbitrary heap pointers (often heap strings,
but could be lists, records, anything). The implementation is a
tag-4 pointer list with interleaved `[k0, v0, k1, v1, ...]` entries.

## Quick tour

```kl
mutable directory = __gc_smap_new()

directory = __gc_smap_set(directory, __gc_string("alice"), __gc_string("CEO"))
directory = __gc_smap_set(directory, __gc_string("bob"),   __gc_string("CFO"))
directory = __gc_smap_set(directory, __gc_string("carol"), __gc_string("manager"))

println(__gc_smap_size(directory))                       // 3
println(__gc_smap_has(directory, __gc_string("alice")))  // true
println(__gc_smap_get_string(directory, __gc_string("alice")))  // "CEO"

directory = __gc_smap_set(directory, __gc_string("alice"), __gc_string("Founder"))
println(__gc_smap_get_string(directory, __gc_string("alice")))  // "Founder"
println(__gc_smap_size(directory))                       // 3 — replaced, not added

println(__gc_smap_get(directory, __gc_string("missing")))  // 0 (null)
```

## API

| Builtin | Returns | Description |
|---|---|---|
| `__gc_smap_new()` | map | Empty map. |
| `__gc_smap_size(m)` | Int | Number of pairs. |
| `__gc_smap_has(m, key)` | Bool | True iff `key` is present. |
| `__gc_smap_get(m, key)` | value or `0` | Stored value, or null when absent. |
| `__gc_smap_get_string(m, key)` | HeapString | Stored value when it is known to be a heap string. |
| `__gc_smap_set(m, key, value)` | map | Fresh map with the pair installed. |
| `__gc_smap_keys(m)` | list_ptr of strings | Every key in insertion order. |
| `__gc_smap_values(m)` | list_ptr | Every value in insertion order. |

## Functional updates

`set` always returns a new map. To mutate-in-place style, capture in a
`mutable`:

```kl
mutable counts = __gc_smap_new()
foreach (word in tokens) {
  val current = __gc_smap_get(counts, word)
  // the value cell carries a single Int
  val cell = __gc_alloc(8)
  val next = if (current == 0) 1 else __gc_read(current, 0) + 1
  __gc_write(cell, 0, next)
  counts = __gc_smap_set(counts, word, cell)
}
```

(For pure integer counts it's often simpler to keep two parallel
`__gc_list_ptr` / `__gc_list_int` lists than to allocate a tiny cell
per value.)

## Iterating

`__gc_smap_keys` / `__gc_smap_values` return tag-4 pointer lists, so
all the regular list helpers apply:

```kl
val keys = __gc_smap_keys(directory)
foreach (i in [0, 1, 2]) {
  val k = __gc_list_ptr_get_string(keys, i)
  val v = __gc_smap_get_string(directory, k)
  println(k + __gc_string(": ") + v)
}
```

## Performance

The current implementation is linear in the map size for `has`, `get`,
and `set`. That's fine for small maps (a few hundred pairs) and for
configuration-style data. A hashed implementation is on the roadmap;
see [Roadmap](./roadmap.md).
