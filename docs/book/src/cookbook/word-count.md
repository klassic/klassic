# Word count

Read a file, count how many times each word appears, print the
result. Demonstrates file I/O, splitting strings, and the heap-backed
`smap` dictionary.

```kl
val args = CommandLine#args()
if (size(args) != 1) {
  printlnError("usage: wc <path>")
  Process#exit(1)
}

val path = head(args)
val raw = FileInput#readAll(path)

// Trim the trailing newline, then split on the space character (32).
// Real-world tools would handle punctuation; this recipe focuses on
// the GC plumbing.
val words = __gc_string_split(__gc_string_trim(__gc_string(raw)), 32)

mutable counts = __gc_smap_new()
mutable i = 0
val n = __gc_list_ptr_len(words)
while (i < n) {
  val word = __gc_list_ptr_get(words, i)
  if (__gc_string_len(word) > 0) {
    val current = __gc_smap_get(counts, word)
    val next_int = if (current == 0) 1 else __gc_read(current, 0) + 1
    val cell = __gc_alloc(8)
    __gc_write(cell, 0, next_int)
    counts = __gc_smap_set(counts, word, cell)
  }
  i += 1
}

val keys = __gc_smap_keys(counts)
mutable j = 0
val k = __gc_list_ptr_len(keys)
while (j < k) {
  val key = __gc_list_ptr_get(keys, j)
  val cell = __gc_smap_get(counts, key)
  val count = __gc_read(cell, 0)
  println(__gc_string_concat(
    __gc_string_concat(key, __gc_string(": ")),
    __gc_int_to_string(count)
  ))
  j += 1
}
```

Sample run:

```bash
echo "alpha beta alpha gamma beta alpha" > sample.txt
klassic build wc.kl -o wc
./wc sample.txt
# alpha: 3
# beta: 2
# gamma: 1
```

## What's happening

1. `__gc_string(raw)` lifts the file content onto the GC heap.
2. `__gc_string_split(..., 32)` cuts on space and returns a tag-4
   list of fresh heap strings.
3. `__gc_smap_new() / _set / _get` build a string-keyed dictionary.
   Values are stored as 8-byte cells holding the running count;
   each `_set` returns a fresh map.
4. `__gc_smap_keys` extracts every key as a list, which we then
   walk to print the totals.
