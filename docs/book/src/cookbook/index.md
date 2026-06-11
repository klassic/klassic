# Cookbook

Bite-sized, runnable Klassic programs that solve common scripting
tasks. Each recipe is a complete `.kl` file you can drop into a
folder and run.

| Recipe | What it teaches |
|---|---|
| [Hello, World variations](./hello.md) | Static vs heap-allocated strings, `println` end-to-end. |
| [Greeter with arguments](./greeter.md) | `CommandLine#args()`, error exits, native CLI. |
| [Uppercase filter](./uppercase-filter.md) | `stdin` / `stdout`, line-by-line processing. |
| [Word count](./word-count.md) | File I/O, splitting, heap-backed `smap`. |
| [Config from environment](./config-env.md) | Environment variables, defaults, `Map#get`. |
| [Tiny calculator](./calculator.md) | Recursive parsing, lambda-based reducer, error reporting. |

Every recipe builds with the standard:

```bash
klassic build recipe.kl -o recipe
./recipe ...
```
