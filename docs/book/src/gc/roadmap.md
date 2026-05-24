# Roadmap

The GC runtime is feature-complete as a debug surface — 65+
builtins, full bounds checking, multi-segment heap growth,
introspection helpers. The remaining work is to **make the GC heap
the default home for every language value** so the `__gc_*` prefix
disappears from user code.

## Phases

### Phase A — Strings on the heap

Today, dynamic string operations (`s + t` with a runtime operand,
string interpolation `"#{x}"`, file reads) write into fixed 64 KiB
`.data` scratch buffers. The migration replaces those buffers with
heap allocations.

Status:
- ✅ `__gc_string_*` returns `HeapString` (a marker variant of the
  `HeapPointer` ABI).
- ✅ `println(heap_string)` dispatches to a byte-emitting path.
- ✅ `__gc_string(runtime_string)` copies fixed-buffer runtime `String`
  values onto the GC heap as an explicit migration bridge.
- 🚧 `+` on dynamic operands → produce `HeapString`.
- 🚧 String interpolation → produce `HeapString`.
- 🚧 `FileInput#all` / `readAll` returns → migrate to `HeapString`.
- 🚧 Existing `RuntimeString` paths retired.

### Phase B — Lists on the heap

The same migration for runtime lists: `RuntimeList`,
`RuntimeLinesList`, the `runtime_list_kind` machinery in the native
compiler. Replace fixed-buffer storage with `__gc_list_*` and
`__gc_list_ptr_*` allocations.

### Phase C — Records on the heap

`RuntimeRecord` field storage migrates to heap allocations. Each
record value becomes a tag-2 (pointer record) or tag-3 (pointer
array) heap object whose fields are accessed by qword offset.

### Phase D — Map / set literals on the heap

`%[k1: v1, k2: v2]` and `%(a, b, c)` literal expressions lower to
`__gc_smap_*` calls (and an analogous `__gc_set_*` family) instead
of static map labels.

### Phase E — Type system integration

Two flavours of integration are possible:

1. **Implicit** — every reference type is heap-backed; the type
   system already handles it transparently.
2. **Explicit** — a `Heap<T>` annotation lets users opt in / out.

Phase E is the most invasive; it interacts with type inference,
generalization, and capture analysis. We expect to land it last.

## What "GC complete" means

When phases A–D land, every standard language operation produces and
consumes heap-backed values without the user invoking a `__gc_*`
builtin. At that point Klassic-on-x64 is a fully GC'd language in
the same sense as Go or Java: the heap is the canonical place for
non-trivial values, and the runtime manages reclamation.

## Performance considerations

Heap allocation is more flexible than fixed buffers but adds
overhead: every dynamic string concat is now an `mmap`-backed bump
allocation. For most scripts this is invisible (allocations are
nanoseconds, the heap is sized to avoid frequent collections), but
hot loops that previously reused a fixed buffer in place may want
explicit accumulator patterns.

The collector itself is precise mark-sweep: pause-based, but
predictable. A future generational variant could shrink pause times
on heap-heavy workloads.

## Practical use cases unlocked along the way

Even before all phases land, the GC heap already supports practical
programs through the explicit `__gc_*` API:

- Text processing (split / replace / join / case operations).
- Dynamic dictionaries via `__gc_smap_*`.
- Functional list update patterns (`push` / `pop` / `concat` /
  `reverse` / `to_string`).
- Streaming file I/O integrated with heap-allocated accumulators.

See [GC Builtins Reference](./builtins-reference.md) for the
complete API surface.
