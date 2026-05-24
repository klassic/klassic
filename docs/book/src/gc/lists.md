# Heap-Backed Lists

Two flavours of list live on the GC heap:

- `__gc_list_int(n)` — a list of `Int` slots.
- `__gc_list_ptr(n)` — a list of heap pointers (HeapString, lists,
  records, anything allocated through the GC).

Both share the same shape: a length prefix at offset 0 followed by
`n` 8-byte slots. Pointer lists use a different GC tag so the mark
phase walks the slots transitively while skipping the leading length.

## Integer lists

```kl
mutable scores = __gc_list_int(0)
scores = __gc_list_int_push(scores, 10)
scores = __gc_list_int_push(scores, 20)
scores = __gc_list_int_push(scores, 30)
__gc_list_int_println(scores)                // [10, 20, 30]
println(__gc_list_int_sum(scores))           // 60
println(__gc_list_int_min(scores))           // 10
println(__gc_list_int_max(scores))           // 30
```

| Builtin | Notes |
|---|---|
| `__gc_list_int(n)` | n zero-init Int slots |
| `__gc_list_int_push(lst, v)` | functional append (returns a fresh list) |
| `__gc_list_int_pop(lst)` | functional drop-last (empty aborts) |
| `__gc_list_int_get(lst, i)` / `_set(lst, i, v)` | bounds-checked access |
| `__gc_list_int_reverse(lst)` | fresh reversed list |
| `__gc_list_concat(a, b)` | concatenate two int lists |
| `__gc_list_int_sum(lst)` / `_min(lst)` / `_max(lst)` | aggregate; min/max abort on empty |
| `__gc_list_int_println(lst)` | print as `[a, b, c]\n` |
| `__gc_list_int_to_string(lst, sep)` | render with separator (returns HeapString) |

## Pointer lists

```kl
mutable parts = __gc_list_ptr(0)
parts = __gc_list_ptr_push(parts, __gc_string("hello"))
parts = __gc_list_ptr_push(parts, __gc_string("world"))
println(__gc_list_ptr_join(parts, __gc_string(", ")))  // "hello, world"
```

| Builtin | Notes |
|---|---|
| `__gc_list_ptr(n)` | n zero-init pointer slots (tag 4) |
| `__gc_list_ptr_len(lst)` | length stored at offset 0 |
| `__gc_list_ptr_push(lst, ptr)` | functional append |
| `__gc_list_ptr_pop(lst)` | functional drop-last |
| `__gc_list_ptr_get(lst, i)` / `_set(lst, i, ptr)` | bounds-checked access |
| `__gc_list_ptr_get_string(lst, i)` | string-typed bounds-checked read when the slot stores a heap string |
| `__gc_list_ptr_reverse(lst)` | fresh reversed list |
| `__gc_list_ptr_concat(a, b)` | concatenate two pointer lists |
| `__gc_list_ptr_join(parts, sep)` | join heap-string elements with separator |

## Tracing semantics

Pointer lists hold every appended pointer alive — the mark phase
walks each slot through `gc_mark_visit`. Combined with the
shadow-stack auto-rooting on the list's binding slot, that means a
single `val` is enough:

```kl
val children = build_list()   // returns a __gc_list_ptr
// no need for __gc_pin; the val keeps every element alive
do_some_collections()
```

## Functional update style

Heap lists are immutable from the user's perspective. Each "mutating"
operation (`push`, `pop`, `concat`, `reverse`) returns a fresh list
and leaves the original untouched. To grow a list in a loop, capture
each step in a `mutable`:

```kl
mutable acc = __gc_list_int(0)
foreach (n in [1, 2, 3, 4, 5]) {
  acc = __gc_list_int_push(acc, n * n)
}
__gc_list_int_println(acc)   // [1, 4, 9, 16, 25]
```

The collector reclaims dropped intermediate versions as long as no
other binding still holds them.
