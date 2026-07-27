# Word count

Read a file, count how many times each word appears, print the
result. Demonstrates file I/O, string splitting, and `Map<String,
Int>` from `std.map` — Klassic's automatic GC means none of this
touches a heap pointer directly. Runs under the evaluator
(`klassic wc.kl -- <path>`) and builds to a native executable on every
target.

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

foreach (entry in counts.toPairs()) {
  println(entry.key + ": " + entry.value)
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
3. `counts.toPairs()` (from `std.map`) extracts every entry in
   insertion order as a `MapEntry` with `.key` and `.value`, which
   we walk to print the totals.
