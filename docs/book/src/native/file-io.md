# File and Directory I/O

Native binaries talk directly to the kernel through `open`, `read`,
`write`, `unlink`, `mkdir`, `getdents`, and friends.

## Reading

```kl
val content = FileInput#readAll("notes.txt")
println(content)
```

Or stream it line-by-line:

```kl
val lines = FileInput#readLines("notes.txt")
foreach (line in lines) {
  println(line)
}
```

`FileInput#readAll` and `FileInput#readLines` accept both static
literal paths and runtime string paths.

## Writing

```kl
FileOutput#write("output.txt", "Hello!\n")
FileOutput#append("output.txt", "Another line.\n")
```

You can also write a list of lines in one call:

```kl
FileOutput#writeLines("output.txt", ["one", "two", "three"])
```

## Existence and deletion

```kl
if (FileOutput#exists("scratch.txt")) {
  FileOutput#delete("scratch.txt")
}
```

## Directories

```kl
Dir#mkdir("/tmp/klassic-demo")
Dir#mkdirs("/tmp/klassic-demo/nested/path")

if (Dir#exists("/tmp/klassic-demo")) {
  foreach (entry in Dir#list("/tmp/klassic-demo")) {
    println(entry)
  }
}
```

| Function | Behaviour |
|---|---|
| `Dir#exists(path)` | Boolean |
| `Dir#isFile(path)`, `Dir#isDirectory(path)` | Boolean |
| `Dir#mkdir(path)` | Single directory |
| `Dir#mkdirs(path)` | Whole chain (`mkdir -p`) |
| `Dir#delete(path)` | Single directory |
| `Dir#list(path)` | Entries (no parent path prefix) |
| `Dir#listFull(path)` | Entries with the directory prefix |
| `Dir#copy(src, dst)`, `Dir#move(src, dst)` | Recursive |
| `Dir#current()` | `getcwd` |
| `Dir#home()` | `$HOME` |
| `Dir#temp()` | `$TMPDIR`, falling back to `/tmp` |

## Callback style

`FileInput#open` runs a callback with an open stream and closes it
afterwards:

```kl
FileInput#open("config.txt", (stream) => {
  println(FileInput#readAll(stream))
})
```

The native compiler tracks the stream's path through the callback so
`FileInput#readAll(stream)`, `FileInput#readLines(stream)`, and `cleanup` blocks all
compose correctly.

## Errors

Failing syscalls (missing files, permission denied, target already
exists) emit a source-located diagnostic to stderr and exit with a
non-zero status. There is no exception machinery — fatal failures are
fatal.
