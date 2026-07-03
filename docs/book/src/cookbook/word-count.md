# Word count

Read a file, count how many times each word appears, print the
result. Demonstrates file I/O, string splitting, and `Map<String,
Int>` from `std.map` — Klassic's automatic GC means none of this
touches a heap pointer directly. Run it with the evaluator
(`klassic wc.kl -- <path>`); building `Map#put`/`Map#empty` to a
native executable is on the native compiler's coverage roadmap, not
supported yet.

```kl
val args = CommandLine#args()
if (size(args) != 1) {
  printlnError("usage: wc <path>")
  Process#exit(1)
}

val path = head(args)
val raw = FileInput#readAll(path)

mutable counts = Map#empty()
foreach (word in raw.trim().words()) {
  counts = Map#put(counts, word, counts.getOrElse(word, 0) + 1)
}

foreach (pair in counts.toPairs()) {
  println(head(pair) + ": " + toString(head(tail(pair))))
}
```

Sample run:

```bash
echo "alpha beta alpha gamma beta alpha" > sample.txt
klassic wc.kl -- sample.txt
# alpha: 3
# beta: 2
# gamma: 1
```

## What's happening

1. `raw.trim().words()` (from `std.string`) strips the trailing
   newline and splits on whitespace, dropping empty runs — no
   manual byte-32 comparisons.
2. `Map#empty()` / `Map#put` build an immutable `Map<String, Int>`;
   `counts.getOrElse(word, 0)` reads the running count (defaulting
   to `0` for a word seen for the first time), and each `put`
   returns a fresh map.
3. `counts.toPairs()` (from `std.map`) extracts every `[key,
   value]` pair in insertion order, which we walk to print the
   totals.
