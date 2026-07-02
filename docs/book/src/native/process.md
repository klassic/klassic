# Standard Streams and the Environment

Generated executables observe the process environment that they are
launched in — not the environment that built them.

## Standard input

```kl
val all = StandardInput#all()
println("Read #{length(all)} bytes")

val lines = StandardInput#lines()
foreach (line in lines) {
  println("> " + line)
}
```

Aliases: `stdin()` and `stdinLines()` work as shorthand for
`StandardInput#all()` and `StandardInput#lines()`.

## Standard output / error

```kl
println("normal output")        // → stdout
printlnError("warning")         // → stderr
```

## Command-line arguments

```kl
val rest = CommandLine#args()
println("got #{size(rest)} arguments")
foreach (a in rest) {
  println(a)
}
```

`args()` is shorthand for `CommandLine#args()`. Both forms strip
`argv[0]` (the binary path).

## Process exit

```kl
if (size(args) == 0) {
  printlnError("usage: tool <input>")
  Process#exit(1)
}
```

`exit(code)` is also accepted.

## Environment variables

```kl
if (Environment#exists("DEBUG")) {
  println("DEBUG=" + Environment#get("DEBUG"))
}

foreach (entry in Environment#vars()) {
  println(entry)
}
```

Aliases:
- `getEnv(name)` for `Environment#get`
- `hasEnv(name)` for `Environment#exists`
- `env()` for `Environment#vars`

On the Windows target, `Environment#get` / `Environment#exists` resolve
names case-insensitively (matching Windows' own semantics — the OS
stores `Path`, not `PATH`), while on Linux and macOS lookups are
case-sensitive.

## Sample CLI tool

```kl
val args = CommandLine#args()
if (size(args) != 1) {
  printlnError("usage: cat <path>")
  Process#exit(1)
}

val path = head(args)
if (!FileOutput#exists(path)) {
  printlnError("error: " + path + " does not exist")
  Process#exit(2)
}

println(FileInput#readAll(path))
```

```bash
klassic build cat.kl -o kat
./kat hello.kl
```
