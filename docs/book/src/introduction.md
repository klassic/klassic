<div class="kl-hero">
  <p class="kl-eyebrow">The Klassic Book</p>
  <h1 class="kl-wordmark">Forged straight<br>into <em>machine code</em>.</h1>
  <p class="kl-tagline">
    Klassic is a statically typed object-functional language whose
    compiler writes executables byte by byte — ELF on Linux,
    ad-hoc-signed Mach-O on Apple Silicon, PE64 on Windows — with no
    <code>cc</code>, <code>as</code>, <code>ld</code>,
    <code>codesign</code>, or <code>link.exe</code> anywhere in the
    loop.
  </p>
  <ul class="kl-chips">
    <li>Hindley–Milner inference</li>
    <li>ADTs + match</li>
    <li>precise GC</li>
    <li>linux · macos · windows</li>
    <li>zero toolchain</li>
  </ul>
</div>

<div class="kl-terminal">
  <div class="kl-terminal-bar">
    <span></span><span></span><span></span>
    <em class="kl-terminal-title">klassic — 80×14</em>
  </div>
  <pre><code><span class="kl-prompt">$</span> curl -fsSL https://raw.githubusercontent.com/klassic/klassic/main/install.sh | sh
<span class="kl-dim">installed: klassic 0.7.0 -&gt; ~/.klassic/bin</span>
<span class="kl-prompt">$</span> cat fib.kl
<span class="kl-out">def fib(n: Int): Int = if (n &lt; 2) n else fib(n - 1) + fib(n - 2)
println(fib(25))</span>
<span class="kl-prompt">$</span> klassic build fib.kl -o fib && ./fib
<span class="kl-out">75025</span>
<span class="kl-prompt">$</span> klassic --target x86_64-pc-windows-msvc build fib.kl -o fib.exe
<span class="kl-prompt">$</span> file fib fib.exe
<span class="kl-out">fib:     ELF 64-bit LSB executable, x86-64, statically linked
fib.exe: PE32+ executable (console) x86-64, for MS Windows</span></code></pre>
</div>

The same `build` command targets the detected host — a direct ELF64
writer on Linux x86_64, a direct Mach-O arm64 writer (including the
embedded ad-hoc code signature Apple Silicon requires) on macOS, a
direct PE64 writer on Windows x86_64 — and `--target` cross-builds any
of the three from any machine, as the transcript above shows. Programs
the young Mach-O backend cannot lower yet fall back transparently to
the portable C backend, and everything runs through the evaluator for
instant iteration.

## Why Klassic?

<div class="kl-cards">
  <div class="kl-card">
    <span class="kl-card-glyph">type system</span>
    <h3>Inference that works for you</h3>
    <p>Hindley–Milner inference with generalized schemes, nominal
    algebraic data types with exhaustiveness checking, row-polymorphic
    records, and type classes — annotations are for APIs, not for
    appeasing the compiler.</p>
  </div>
  <div class="kl-card">
    <span class="kl-card-glyph">backend</span>
    <h3>No toolchain, anywhere</h3>
    <p>The compiler emits machine code and the executable container
    itself: ELF64 via direct syscalls on Linux, signed Mach-O arm64 via
    <code>svc #0x80</code> on macOS, PE64 calling
    <code>kernel32.dll</code> through a hand-built import table on
    Windows. Sub-10&nbsp;KiB binaries, sub-millisecond startup.</p>
  </div>
  <div class="kl-card">
    <span class="kl-card-glyph">memory</span>
    <h3>A real garbage collector</h3>
    <p>Native binaries embed a precise mark-and-sweep collector
    with a growable multi-segment heap — strings, lists, maps, records,
    and enums all live on it.</p>
  </div>
  <div class="kl-card">
    <span class="kl-card-glyph">pattern matching</span>
    <h3>Enums and match, compiled</h3>
    <p><code>enum Tree { case Leaf(v: Int); case Branch(l: Tree, r: Tree) }</code>
    — construction, nested patterns, guards, and recursive functions
    over them compile natively on all three targets.</p>
  </div>
  <div class="kl-card">
    <span class="kl-card-glyph">stdlib</span>
    <h3>A standard library in Klassic</h3>
    <p><code>std.list</code>, <code>std.string</code>, <code>std.option</code>,
    <code>std.result</code>, <code>std.json</code>, <code>std.time</code>
    and more — written in the language itself and shared between the
    evaluator and native builds.</p>
  </div>
  <div class="kl-card">
    <span class="kl-card-glyph">proofs</span>
    <h3>A trust surface</h3>
    <p>A lightweight <code>axiom</code> / <code>theorem</code> layer
    models trust dependencies; <code>--warn-trust</code> and
    <code>--deny-trust</code> decide how much faith your build is
    allowed to take on.</p>
  </div>
</div>

## A taste

```kl
enum Shape {
  case Circle(r: Double)
  case Rect(w: Double, h: Double)
}

def describe(s: Shape): String = s match {
  case Circle(r) if r > 100.0 => "a big circle"
  case Circle(r)              => "a circle"
  case Rect(w, h)             => "a " + toString(w) + "x" + toString(h) + " box"
}

println(describe(Rect(3.0, 4.0)))   // a 3.0x4.0 box

val ages = %["alice": 30, "bob": 27]
println(ages.get("alice"))          // 30

foreach (name in ["alice", "bob"]) {
  println(name + " is here")
}
```

## How this book is organized

- 🚀 **Getting Started** — install with one command, run your first
  `hello.kl`, and meet the typed REPL.
- 🧭 **Language Tour** — one chapter per syntactic surface (variables,
  functions, records, type classes, …).
- 🍳 **Cookbook** — six runnable recipes that solve real scripting
  tasks (filter, word count, calculator, …).
- ⚙️ **Native Compilation** — producing standalone executables for
  Linux, Apple Silicon, and Windows.
- 📚 **Reference** — the comprehensive native compiler coverage and
  architecture overview.

## Status

Klassic is alive and shipping features in small commits. Three direct
native backends are implemented — Linux x86_64 (the most complete),
Windows x86_64 (sharing the same codegen, down to file, directory,
environment, and stdin support), and Apple Silicon macOS (growing
fast, with a portable-C fallback) — and anything the native compilers
cannot lower fails with a source-located diagnostic instead of
silently falling back to the evaluator. Releases ship as static
binaries for Linux, both macOS architectures, and Windows; the
installer verifies itself by running a Klassic program on your
machine.
