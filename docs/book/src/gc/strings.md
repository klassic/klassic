# Heap-Allocated Strings

The `__gc_string*` family stores string data on the GC heap. Heap
strings carry a length prefix (`[len: i64][bytes]`) and integrate
with `println` natively — you don't need to call a special print
helper.

## Building a string

```kl
val literal = __gc_string("Hello")              // copy from .data
val lifted  = __gc_string(FileInput#all("message.txt"))
val empty   = __gc_string_alloc(64)             // 64 zero bytes
val number  = __gc_int_to_string(42)            // "42"
```

`__gc_string` accepts either a static literal or a fixed-buffer runtime
`String`, so file input, stdin, and other runtime string helpers can be lifted
onto the GC heap when you want heap-backed composition.

## Composition

```kl
val a = __gc_string("foo")
val b = __gc_string("bar")
val ab = __gc_string_concat(a, b)               // "foobar"
val bang = ab + __gc_string("!")                // heap-backed `+`
val tagged = "tag:" + bang                      // static/runtime fragments ok
println(ab == __gc_string("foobar"))            // content equality
assertResult(__gc_string("foobar"))(ab)         // content assertion
val text = toString(ab)                         // fixed-buffer String bridge
val method = ab.toString()                      // method-style bridge
println(length(text))                           // ordinary String helper
val message = "heap=#{ab}"                      // interpolation bridge

val tag  = __gc_string_repeat(__gc_string("-"), 10)   // "----------"
val mid  = __gc_string_substring(__gc_string("klassic"), 1, 5) // "lass"
```

Heap string equality roots the left-hand value while the right-hand side is
evaluated, so content checks remain safe even when the right side allocates or
collects before returning its string.

## Predicates and search

```kl
val s = __gc_string("the quick brown fox")
println(__gc_string_contains(s, __gc_string("brown")))    // true
println(__gc_string_starts_with(s, __gc_string("the")))   // true
println(__gc_string_ends_with(s, __gc_string("fox")))     // true
println(__gc_string_index_of(s, 113))                     // 4 (q)
```

## Cleanup helpers

```kl
__gc_string_println(__gc_string_trim(__gc_string("   spaced   ")))  // "spaced"
__gc_string_println(__gc_string_to_lower(__gc_string("Klassic")))   // "klassic"
__gc_string_println(__gc_string_to_upper(__gc_string("Klassic")))   // "KLASSIC"
```

## Replacing and splitting

```kl
val before = __gc_string("foo bar foo")
val after = __gc_string_replace(before, __gc_string("foo"), __gc_string("baz"))
println(after)                                  // "baz bar baz"

val parts = __gc_string_split(__gc_string("a,b,c"), 44)  // 44 = ','
println(__gc_list_ptr_len(parts))               // 3
```

`__gc_string_lines(s)` is a shortcut for splitting on `'\n'` (byte 10).

## Byte-level access

```kl
val s = __gc_string_alloc(5)
__gc_string_set_byte(s, 0, 72)   // 'H'
__gc_string_set_byte(s, 1, 105)  // 'i'
__gc_string_set_byte(s, 2, 33)   // '!'
println(__gc_string_len(s))      // 5
println(__gc_string_get_byte(s, 0))  // 72
```

`set_byte` and `get_byte` are bounds-checked; out-of-range accesses
emit `klassic gc: index out of bounds` and exit.

## Integer parsing

```kl
println(__gc_string_to_int(__gc_string("42")))      // 42
println(__gc_string_to_int(__gc_string("-7")))      // -7
println(__gc_string_to_int(__gc_string("100x")))    // 100 — stops at non-digit
println(__gc_string_to_int(__gc_string("")))        // 0
```

The parser is permissive: invalid input becomes 0 instead of
throwing. The full reference is in
[GC Builtins Reference](./builtins-reference.md).
