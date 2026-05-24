# Klassic

Klassic is a statically typed object-functional programming language written in
Rust. The implementation builds a native `klassic` executable with Cargo.

📖 **User documentation:** [The Klassic Book](https://klassic.github.io/klassic/)
(or build it locally — see [`docs/book/`](docs/book/)).

## Features

- Hindley-Milner style type inference with annotations and generalized schemes
- Row-polymorphic records and record field selection
- First-class functions, closures, named recursive functions, and mutable locals
- Type classes, constrained polymorphism, and repository-backed higher-kinded examples
- Lightweight theorem / trust / axiom surface with `--warn-trust` and `--deny-trust`
- String interpolation, comments, cleanup clauses, loops, ternary expressions, and casts
- List, map, and set literals with comma, space, or newline separators
- Pure Rust file, directory, string, list, map, set, time, and thread helpers
- Native CLI and REPL
- Precise mark-and-sweep garbage collector with multi-segment heap growth, exposed through 65 `__gc_*` debug builtins
- Standalone Rust macro PEG subsystem

## Build And Test

Install a recent Rust toolchain, then use the normal Cargo workflow:

```bash
cargo build
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
cargo run -- -e "1 + 2"
```

Build an optimized native executable:

```bash
cargo build --release
./target/release/klassic -e "1 + 2"
```

## CLI

Evaluate an expression:

```bash
klassic -e '1 + 2'
```

Run a file:

```bash
klassic path/to/program.kl
klassic -f path/to/program.kl
```

Build a Linux x86_64 native executable:

```bash
klassic build path/to/program.kl -o program
./program
```

Pass `--target linux-x86_64` or `--target x86_64-unknown-linux-gnu` to select
the native target explicitly, or `--target native` on a matching host. Linux
x86_64 is currently the only implemented target.

Start the REPL:

```bash
klassic
```

REPL commands include `:history` and `:exit`.

Trust diagnostics:

```bash
klassic --warn-trust proofs.kl
klassic --deny-trust proofs.kl
```

`--warn-trust` reports trusted proof dependencies. `--deny-trust` rejects any
proof graph that depends on a trusted theorem or axiom.

## Native Compiler Coverage

The native compiler lowers a growing slice of the language directly to a
selected native target. The only implemented target is currently Linux x86_64,
selectable as `linux-x86_64`, `x86_64-unknown-linux-gnu`, or `native` on a
matching host, and it emits ELF64 without an external linker. Highlights:

- Core integer / boolean / string / list expressions, control flow, and
  recursive `def`s (including annotated `String` and `List<String>` parameters).
- Static folding for pure expressions, with mutable side effects preserved when
  a value can still be recovered statically.
- Fixed-buffer runtime strings, line lists, runtime lists, and runtime records,
  including dynamic `if` branches that merge compatible runtime values.
- Static maps and sets, plus runtime-key lookups that copy entries into runtime
  storage without losing the selected length.
- Linux file / directory / process / environment / stdin / argv builtins via
  direct syscalls, with virtual filesystem tracking for static paths.
- Source-located stderr diagnostics for runtime failures (`assert`,
  `assertResult`, `head([])`, negative `sleep`, FileOutput / Dir errors).

Anything not yet supported fails at build time rather than falling back to the
evaluator. See [`docs/native-coverage.md`](docs/native-coverage.md) for the
exhaustive feature matrix.

## Native Garbage Collector

Generated native executables embed a precise mark-and-sweep collector that
manages a private heap separate from the static `.data` buffers used elsewhere
by the codegen. The heap starts at 1 MiB and grows in 1 MiB segments via
additional `mmap` calls (up to 64 segments / 64 MiB) when even a post-collection
retry cannot satisfy the bump path. Each block carries a 16-byte header (size +
mark bit, type tag) with five tag values: free, raw bytes, pointer record,
pointer array, and pointer list (where the first qword is a length the mark
phase skips). Roots come from three sources: a static 1024-entry pin table, an
8192-entry shadow stack of every `HeapPointer`-typed stack slot, and mutable
`HeapPointer` slot reassignment paths.

Source programs reach the heap through a 65-builtin debug surface:

- Allocation: `__gc_alloc(size)`, `__gc_record(num_fields)`,
  `__gc_array(num_slots)`, `__gc_list_int(n)`, `__gc_list_ptr(n)`.
- Heap strings: `__gc_string("text")`, `__gc_string_alloc(n)`,
  `__gc_string_concat(a, b)`, `__gc_string_substring(s, start, end)`,
  `__gc_string_repeat(s, n)`, `__gc_string_replace(s, from, to)`,
  `__gc_string_trim(s)`, `__gc_string_to_lower(s)`,
  `__gc_string_to_upper(s)`, `__gc_string_println(g)`,
  `__gc_string_len(s)`, `__gc_string_eq(a, b)`,
  `__gc_string_get_byte(s, idx)`, `__gc_string_set_byte(s, idx, byte)`,
  `__gc_string_starts_with(s, prefix)`, `__gc_string_ends_with(s, suffix)`,
  `__gc_string_contains(haystack, needle)`,
  `__gc_string_index_of(s, byte)`, `__gc_string_to_int(s)`,
  `__gc_string_split(s, sep_byte)`, `__gc_string_lines(s)`, and
  `__gc_int_to_string(n)`.
- Int-list helpers: `__gc_list_int_get` / `_set` / `_push` / `_pop` /
  `_reverse` / `_sum` / `_min` / `_max` / `_println` / `_to_string`, plus
  `__gc_list_concat(a, b)`.
- Pointer-list helpers: `__gc_list_ptr_len`, `_get`, `_get_string`, `_set`,
  `_push`, `_pop`, `_reverse`, `_concat`, and `_join`.
- String-keyed maps: `__gc_smap_new`, `_size`, `_has`, `_get`, `_get_string`,
  `_set`, `_keys`, and `_values`.
- Raw pointer access: `__gc_read(addr, offset)`,
  `__gc_read_ptr(addr, offset)`, `__gc_read_string(addr, offset)`, and
  `__gc_write(addr, offset, value)` (which double as record-field and array-slot
  access).
- Roots and observability: `__gc_pin(addr)` / `__gc_unpin(addr)`,
  `__gc_collect()`, `__gc_collect_count()`, `__gc_segment_count()`,
  `__gc_pointer_count(addr)`.

All length-aware list and string-byte operations bounds-check through a shared
`gc_bounds_error` subroutine that prints `klassic gc: index out of bounds` and
exits with status 1; OOM, segment-limit, root-overflow, worklist-overflow, and
shadow-overflow paths each print their own dedicated diagnostic.

## Quick Start

Create `hello.kl`:

```klassic
println("Hello, World!")
```

Run it:

```bash
cargo run -- hello.kl
```

## Syntax Examples

### Variables

```klassic
val one = 1

mutable i = 1
i = i + 1
i += 1
```

`val` bindings are immutable. `mutable` bindings can be reassigned.

### Functions

```klassic
val add = (x, y) => x + y

def fact(n) =
  if(n < 2) 1 else n * fact(n - 1)

println(add(1, 2))
println(fact(5))
```

### Blocks And Cleanup

```klassic
mutable i = 0
while(i < 10) {
  i += 1
} cleanup {
  println(i)
}
```

Cleanup clauses run after the associated expression finishes.

### Collections

```klassic
val list1 = [1, 2, 3]
val list2 = [
  1
  2
  3
]

val map = %["A": 1, "B": 2]

val set1 = %(1, 2, 3)
val set2 = %(
  1
  2
  3
)
```

Lists, maps, and sets accept commas, spaces, and line breaks as separators where
the language grammar allows collection separators.

### Strings

```klassic
val name = "Klassic"
println("Hello, #{name}!")
println(substring("abcdef", 1, 3))
```

### Records

```klassic
record Point {
  x: Int
  y: Int
}

val p = Point(10, 20)
println(p.x + p.y)

def add_xy(o) = o.x + o.y
println(add_xy(record { x = 1; y = 2 }))
```

Record field access participates in the Rust type checker's row-polymorphic
constraints, including nominal records used through structural field functions.
Call-site lambdas are checked against expected function types, so reducers such
as `foldLeft(xs)([])((acc, e) => e #cons acc)` infer the empty accumulator from
the reducer result instead of escaping through `*`.

### Modules And Imports

```klassic
module math.demo {
  def double(x) = x * 2
}

import math.demo.{double}
println(double(21))
```

### Type Classes

```klassic
typeclass Show<'a> where {
  show: ('a) => String
}

instance Show<Int> where {
  def show(x: Int): String = "Int: " + x
}

def display<'a>(x: 'a): String where Show<'a> = show(x)

println(display(42))
```

### Trust Surface

```klassic
axiom sortedBase(xs: List<Int>): { true }

theorem sortedAgain(xs: List<Int>): { true } =
  sortedBase(xs)
```

This compiles normally, warns under `--warn-trust`, and fails under
`--deny-trust` because it depends on an axiom.

## Repository Layout

- `src/`: native `klassic` CLI binary
- `crates/klassic-span`: source spans and diagnostics
- `crates/klassic-syntax`: lexer, parser, and AST
- `crates/klassic-rewrite`: placeholder desugaring and syntax normalization
- `crates/klassic-types`: type inference, records, type classes, and proof checks
- `crates/klassic-eval`: evaluator, runtime behavior, builtins, modules, and REPL state
- `crates/klassic-runtime`: shared runtime crate scaffold
- `crates/klassic-macro-peg`: Rust macro PEG implementation
- `tests/`: Rust integration tests and `.kl` golden harnesses
- `test-programs/`: sample Klassic programs used by the test harness
- `docs/`: architecture and native-coverage notes
- `docs/book/`: mdBook source for [The Klassic Book](https://klassic.github.io/klassic/)

## Development

Use `rg` for source search and keep changes covered by Rust tests:

```bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
cargo build --release
cargo test -p klassic-macro-peg
```

The main build/test path is Rust-only.
