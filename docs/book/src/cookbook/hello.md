# Hello, World variations

Two ways to print "Hello, World!" — each illustrates a different
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

`#{...}` splices any expression into the surrounding string, building
it dynamically at runtime. Klassic's automatic GC manages the result
the same way it manages every other value — there's no separate API
to reach for when a string needs to live on the heap.

## Trying them

```bash
klassic -e 'println("Hello, World!")'

# or
echo 'println("Hello, World!")' > hello.kl
klassic build hello.kl -o hello
./hello
```
