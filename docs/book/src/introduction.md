# Introduction

**Klassic** is a statically typed object-functional programming language
implemented in Rust. It runs through an evaluator for fast iteration and
through a Linux x86_64 native compiler that produces standalone ELF
executables — no runtime dependency on `libc`, `cc`, `as`, or `ld`.

## What you can do today

- **Write small programs in a familiar functional / OOP hybrid.**
  Records, type classes (including higher-kinded examples), row-polymorphic
  field selection, mutable `var` slots, and immutable `val` bindings all
  cooperate.

- **Compile to a single ELF binary.** `klassic build hello.kl -o hello`
  emits a Linux x86_64 executable that talks directly to the kernel
  through syscalls.

- **Use a real garbage collector.** Native binaries embed a precise
  mark-and-sweep GC (multi-segment heap that grows up to 64 MiB) that
  manages heap-allocated strings, lists, and maps.

- **Write proofs, kind of.** A lightweight `axiom` / `theorem` surface
  models trust dependencies; `--warn-trust` and `--deny-trust` shape how
  proof graphs flow through your build.

## A taste

```kl
def greet(name: String): String =
  "Hello, " + name + "!"

println(greet("Klassic"))

val ages = %["alice": 30, "bob": 27]
foreach (entry in ages) {
  println(entry)
}
```

## How this book is organized

- 🚀 **Getting Started** — install, run your first `hello.kl`, and
  meet the REPL.
- 🧭 **Language Tour** — one chapter per syntactic surface (variables,
  functions, records, type classes, …).
- 🍳 **Cookbook** — six runnable recipes that solve real scripting
  tasks (filter, word count, calculator, …).
- ⚙️ **Native Compilation** — the workflow for producing standalone
  Linux ELF executables.
- 🧠 **The GC Heap** — why the runtime has its own collector, how to
  use heap-backed strings / lists / maps, and a reference for every
  `__gc_*` debug builtin.
- 📚 **Reference** — the comprehensive native compiler coverage and
  architecture overview.

## Status

Klassic is alive and shipping features in small commits. The native
compiler covers a growing slice of the language; if you hit something
unsupported, the build emits a source-located diagnostic instead of
silently falling back to the evaluator. The GC currently exposes 67
debug builtins that let you write practical text-processing scripts on
the heap; the long-term plan (see [Roadmap](./gc/roadmap.md)) is to
make the heap the default home for every language value.
