# Cleanup Blocks

`cleanup` runs after the associated expression finishes, similar to a
`finally` clause:

```kl
mutable i = 0
while (i < 10) {
  i += 1
} cleanup {
  println("loop finished at i = #{i}")
}
```

The cleanup body runs once, after the `while` loop exits — by normal
termination or otherwise.

## Why use it?

The most common idiom is "release this resource regardless of how the
preceding expression ends":

```kl
FileOutput#write("notes.txt", "hello")
val content = {
  FileInput#readAll("notes.txt")
} cleanup {
  FileOutput#delete("notes.txt")
}
println(content)
```

The native compiler tracks `cleanup` blocks across both pure and
effectful evaluation. Their effects are preserved even when the
surrounding expression is otherwise statically folded.
