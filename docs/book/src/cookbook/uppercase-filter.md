# Uppercase filter

A Unix-style filter: read every line from stdin, uppercase it, write
to stdout. Pairs nicely with `cat`, pipes, and shell redirection.

```kl
val lines = StandardInput#lines()
foreach (line in lines) {
  println(line.toUpperCase())
}
```

Build and run:

```bash
klassic build upper.kl -o upper

echo -e "hello\nworld" | ./upper
# HELLO
# WORLD

./upper < /etc/hostname
```

## Heap-string variant

If you want the result to live on the GC heap (handy when you
chain through other heap operations), use the `__gc_*` family:

```kl
val lines = StandardInput#lines()
foreach (line in lines) {
  val heap = __gc_string(line)
  println(__gc_string_to_upper(heap))
}
```

`__gc_string(line)` copies the runtime string onto the heap;
`__gc_string_to_upper` returns a fresh heap string. `println`
dispatches naturally because the result is a `HeapString`.

## Adding line numbers

```kl
mutable n = 1
val lines = StandardInput#lines()
foreach (line in lines) {
  println("#{n}: #{line.toUpperCase()}")
  n += 1
}
```

```bash
seq 5 | ./upper
# 1: 1
# 2: 2
# 3: 3
# 4: 4
# 5: 5
```
