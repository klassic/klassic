# Modules and Imports

A Klassic module is a file whose first line declares a dotted module
name. The body of the file — everything after the header — belongs to
that module:

```kl
module math.demo

def double(x) = x * 2
def triple(x) = x * 3
```

Save that as `math/demo.kl` next to your program: imports resolve
modules from neighboring files, mapping `math.demo` to
`math/demo.kl` (or `math.demo.kl`). Both the evaluator and native
builds do this resolution.

```kl
import math.demo.{double, triple}

println(double(21))   // 42
println(triple(7))    // 21
```

## Selective import

Pick out exactly what you need:

```kl
import math.demo.{double}

println(double(10))   // 20
```

## Module aliases

Bind the whole module to a shorter name and access members with a dot:

```kl
import math.demo as M

println(M.double(10))   // 20
println(M.triple(10))   // 30
```

> **Note:** aliased imports currently work in native builds
> (`klassic build`) but not yet in the evaluator — a known parity gap.
> Selective imports work everywhere.

## Deeper names

Dotted names nest as deep as your directory tree:

```kl
module outer.inner

def f() = 42
```

stored as `outer/inner.kl`, imported with:

```kl
import outer.inner.{f}

println(f())   // 42
```

## The standard library

The same syntax brings in the bundled standard library — no files
needed on disk:

```kl
import std.math.{max, clamp}
import std.list.{range}

println(max(3, 9))       // 9
println(clamp(99, 0, 10)) // 10
println(range(0, 4))     // [0, 1, 2, 3]
```

`std.option` and `std.result` ship real enums (`Some` / `None`,
`Ok` / `Err`) plus the helpers and extension methods you would
expect; `std.json` and `std.time` are written in pure Klassic on top
of them.
