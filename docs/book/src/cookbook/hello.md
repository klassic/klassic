# Hello, World variations

Three ways to print "Hello, World!" — each illustrates a different
slice of the runtime.

## Static literal

```kl
println("Hello, World!")
```

The simplest possible program. The string lives in the binary's
data section; the native compiler emits a single write to stdout (a
`write` syscall on Linux/macOS, a `WriteFile` call on Windows) plus a
newline.

## With interpolation

```kl
val target = "World"
println("Hello, #{target}!")
```

`#{...}` splices any expression into the surrounding string. The
result is still a runtime string in a fixed buffer (until the
[Phase A](../gc/roadmap.md) migration completes).

## On the GC heap

```kl
val greeting = __gc_string_concat(
  __gc_string("Hello, "),
  __gc_string("World!")
)
println(greeting)
```

The result is a heap-allocated string (`HeapString`). `println`
dispatches automatically to a byte-emitting path — no
`__gc_string_println` needed. This is the path you reach for when
you want to build strings dynamically (see
[Heap-Allocated Strings](../gc/strings.md)).

## Trying them

```bash
klassic -e 'println("Hello, World!")'

# or
echo 'println("Hello, World!")' > hello.kl
klassic build hello.kl -o hello
./hello
```
