# Greeter with arguments

A native CLI that takes a name as a single positional argument and
prints a friendly greeting. Demonstrates `CommandLine#args()`,
graceful error exits, and string concatenation.

```kl
val args = CommandLine#args()

if (size(args) != 1) {
  printlnError("usage: greet <name>")
  Process#exit(1)
}

val name = head(args)
println("Hello, " + name + "!")
```

Build and run:

```bash
klassic build greet.kl -o greet
./greet Klassic
# Hello, Klassic!

./greet
# usage: greet <name>          (on stderr)
echo "exit: $?"
# exit: 1
```

## Variations

### Default when no argument is given

```kl
val args = CommandLine#args()
val name = if (size(args) == 0) "stranger" else head(args)
println("Hello, " + name + "!")
```

### Multiple names

```kl
val args = CommandLine#args()
foreach (name in args) {
  println("Hello, " + name + "!")
}
```

```bash
./greet alice bob carol
# Hello, alice!
# Hello, bob!
# Hello, carol!
```
