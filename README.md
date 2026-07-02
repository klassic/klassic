# Klassic

A statically typed object-functional language whose compiler writes
native executables byte by byte — ELF on Linux, ad-hoc-signed Mach-O
on Apple Silicon, PE64 on Windows — with no `cc`, `as`, `ld`,
`codesign`, or `link.exe` anywhere in the loop.

```bash
$ cat fib.kl
def fib(n: Int): Int = if (n < 2) n else fib(n - 1) + fib(n - 2)
println(fib(25))

$ klassic build fib.kl -o fib && ./fib
75025

$ klassic --target x86_64-pc-windows-msvc build fib.kl -o fib.exe
$ file fib fib.exe
fib:     ELF 64-bit LSB executable, x86-64, statically linked
fib.exe: PE32+ executable (console) x86-64, for MS Windows
```

Hindley–Milner inference, algebraic data types with exhaustive
`match`, row-polymorphic records, type classes, a standard library
written in Klassic itself, a precise garbage collector inside every
binary, and a typed REPL.

📖 **[The Klassic Book](https://klassic.github.io/klassic/)** is the
documentation: installation, a full language tour, native compilation,
and the GC.

## Install

Linux (x86_64) or macOS:

```bash
curl -fsSL https://raw.githubusercontent.com/klassic/klassic/main/install.sh | sh
```

Windows (PowerShell):

```powershell
irm https://raw.githubusercontent.com/klassic/klassic/main/install.ps1 | iex
```

(or grab the zip from the
[releases](https://github.com/klassic/klassic/releases) by hand.)

From source: `cargo install --git https://github.com/klassic/klassic`

| Host | `klassic build` | Cross-builds |
| --- | --- | --- |
| Linux x86_64 | direct ELF64 | any of the three targets |
| macOS arm64 | direct Mach-O, portable-C fallback | any of the three targets |
| Windows x86_64 | direct PE64 | any of the three targets |

`klassic targets` prints the live matrix.

## A taste

```klassic
enum Shape {
  case Circle(r: Double)
  case Rect(w: Double, h: Double)
}

def describe(s: Shape): String = s match {
  case Circle(r) if r > 100.0 => "a big circle"
  case Circle(r)              => "a circle"
  case Rect(w, h)             => "a #{w}x#{h} box"
}

println(describe(Rect(3.0, 4.0)))   // a 3.0x4.0 box

val ages = %["alice": 30, "bob": 27]
foreach (name in ["alice", "bob"]) {
  println("#{name} is #{ages.getOrElse(name, 0)}")
}
```

The [language tour](https://klassic.github.io/klassic/tour/variables.html)
walks through the whole surface — records, type classes, modules,
cleanup clauses, and the `axiom`/`theorem` trust surface.

## CLI

```bash
klassic program.kl                 # run through the evaluator
klassic -e '1 + 2'                 # evaluate an expression
klassic                            # typed REPL (3: Int)
klassic build program.kl -o out    # native executable for the host
klassic --target windows-x86_64 build program.kl -o out.exe
klassic --backend c build program.kl -o out.c   # portable C99
klassic targets                    # target matrix
klassic --warn-trust proofs.kl     # report trusted proofs
```

## Going deeper

- [Native compilation](https://klassic.github.io/klassic/native/building.html)
  and the exhaustive [coverage matrix](docs/native-coverage.md)
- [The GC heap](https://klassic.github.io/klassic/gc/why.html) and its
  `__gc_*` debug builtins
- [Architecture notes](docs/architecture-rust.md) — how the compiler
  pipeline and the three binary writers fit together

## Development

Rust workspace; the usual Cargo workflow is the whole build:

```bash
cargo build
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
```

Crates live under `crates/` (`klassic-syntax`, `klassic-types`,
`klassic-eval`, `klassic-native`, …); sample programs under
`test-programs/`; the book source under `docs/book/`.
