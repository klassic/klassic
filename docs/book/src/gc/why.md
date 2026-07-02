# Why a GC?

Klassic compiles directly to a native executable (ELF64 on Linux,
Mach-O arm64 on macOS, PE64 on Windows) without `libc`, so it cannot
lean on `malloc` / `free` from the system. At the same time, real
programs need values whose lifetime extends past the stack frame that
built them: a string returned from `String#concat`, a list grown
inside a loop, a dictionary that records counts.

The native compiler embeds its own precise mark-and-sweep collector
to give those values a managed home. Concretely:

- The runtime starts with a 1 MiB heap (`mmap`'d at startup on Linux
  and macOS; `VirtualAlloc`'d on Windows).
- When even a post-collection retry can't satisfy a request, the
  allocator `mmap`s another 1 MiB segment. Up to 64 segments
  total — `64 MiB` — before the program exits with
  `klassic gc: heap segment limit reached`.
- Allocations carry a 16-byte header (size + mark bit, type tag).
  Five tag values cover free blocks, raw bytes, fixed-shape pointer
  records, indexed pointer arrays, and length-prefixed pointer lists.
- Roots come from three sources: a static 1024-entry pin table, the
  shadow stack of every `HeapPointer` / `HeapString` slot in scope,
  and the slot-identity tracking that lets `mutable` rebinds work.

For a deeper dive into the runtime layout, see
[Architecture Overview](../reference/architecture.md).

## The user-visible promise

You should rarely have to think about the collector. Every
heap-allocated builtin (`__gc_string`, `__gc_list_int`,
`__gc_smap_set`, ...) returns a value that the surrounding code
auto-roots through the shadow stack. As long as the value is
reachable from a `val` / `mutable` binding, an argument, or a
captured closure, it survives any number of intervening collections.

What you *do* need to know:

- Heap-aware operations exit with a dedicated diagnostic on overflow
  (`out of memory`, `heap segment limit reached`, `index out of
  bounds`). They do not return invalid pointers.
- Collections may run inside any heap-allocating builtin, so the
  caller should not hold raw pointers across `gc_alloc`-style
  operations — bind to a `val` slot first.
