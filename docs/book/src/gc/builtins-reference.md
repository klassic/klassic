# GC Builtins Reference

This is the comprehensive list of every `__gc_*` builtin. They are
debug-prefixed because the long-term goal is to lift these
capabilities into ordinary language values; today they expose the
runtime GC heap directly so practical programs can use it.

The native compiler tracks values produced by GC allocation helpers separately
from plain integers. Passing an ordinary `Int` to a GC helper that expects a
heap address is rejected at build time instead of producing an invalid native
binary.

## Object Model

| Tag | Symbol | Layout | Trace behaviour |
|---|---|---|---|
| 0 | `free` | reserved for free-list entries | not traced |
| 1 | `RAW_BYTES` | opaque bytes (heap strings, int lists) | not traced |
| 2 | `POINTER_RECORD` | packed pointer slots, fixed shape | every payload qword traced |
| 3 | `POINTER_ARRAY` | packed pointer slots, indexed | every payload qword traced |
| 4 | `POINTER_LIST` | `[len: i64][ptr_0, ptr_1, ...]` | first qword skipped, rest traced |

## Raw allocation

| Builtin | Returns | Description |
|---|---|---|
| `__gc_alloc(size)` | HeapPointer | Reserve `size` raw bytes. |
| `__gc_record(num_fields)` | HeapPointer | Reserve `num_fields` pointer slots (tag 2). |
| `__gc_array(num_slots)` | HeapPointer | Reserve `num_slots` pointer slots (tag 3). |
| `__gc_collect()` | Unit | Run a stop-the-world cycle. |

## Memory access

| Builtin | Returns | Description |
|---|---|---|
| `__gc_read(addr, byte_offset)` | Int | Read a raw qword. |
| `__gc_read_ptr(addr, byte_offset)` | HeapPointer | Read a raw qword and keep it tagged as a heap pointer for later GC helpers. |
| `__gc_read_string(addr, byte_offset)` | HeapString | Read a raw qword and keep it tagged as a heap string. |
| `__gc_write(addr, byte_offset, value)` | Unit | Write an `Int` or heap pointer qword. |
| `__gc_pin(addr)` | HeapPointer | Add to the static root table. |
| `__gc_unpin(addr)` | Unit | Remove the first matching root entry. |

## Heap strings

| Builtin | Returns | Description |
|---|---|---|
| `__gc_string(text)` | HeapString | Copy a static literal or runtime `String` onto the heap. |
| `__gc_string_alloc(n)` | HeapString | n-byte zero-filled heap string. |
| `__gc_string_concat(a, b)` | HeapString | Join two heap strings. |
| `__gc_string_repeat(s, n)` | HeapString | Repeat. Negative `n` aborts. |
| `__gc_string_substring(s, start, end)` | HeapString | Bytes `[start, end)`. |
| `__gc_string_replace(s, from, to)` | HeapString | Replace every occurrence. |
| `__gc_string_trim(s)` | HeapString | Strip ASCII whitespace. |
| `__gc_string_to_lower(s)` | HeapString | ASCII A–Z → a–z. |
| `__gc_string_to_upper(s)` | HeapString | ASCII a–z → A–Z. |
| `__gc_string_println(g)` | Unit | Bytes plus newline. (Prefer `println(g)`.) |
| `__gc_string_len(s)` | Int | Byte length. |
| `__gc_string_eq(a, b)` | Bool | Byte equality. |
| `__gc_string_starts_with(s, prefix)` | Bool | Length + bytes. |
| `__gc_string_ends_with(s, suffix)` | Bool | Length + bytes. |
| `__gc_string_contains(s, needle)` | Bool | Naive search. |
| `__gc_string_index_of(s, byte)` | Int | First index, or -1. |
| `__gc_string_to_int(s)` | Int | Permissive base-10 parse. |
| `__gc_int_to_string(n)` | HeapString | Decimal render. |
| `__gc_string_get_byte(s, idx)` | Int | Bounds-checked byte read. |
| `__gc_string_set_byte(s, idx, byte)` | Unit | Bounds-checked byte write. |
| `__gc_string_split(s, sep_byte)` | list_ptr | Split → list of heap strings. |
| `__gc_string_lines(s)` | list_ptr | `__gc_string_split(s, 10)`. |

## Heap-backed integer lists

| Builtin | Returns | Description |
|---|---|---|
| `__gc_list_int(n)` | list | n zero-init Int slots. |
| `__gc_list_int_get(lst, idx)` | Int | Bounds-checked read. |
| `__gc_list_int_set(lst, idx, v)` | Unit | Bounds-checked write. |
| `__gc_list_int_push(lst, v)` | list | Functional append. |
| `__gc_list_int_pop(lst)` | list | Functional drop-last; empty aborts. |
| `__gc_list_int_reverse(lst)` | list | Fresh reversed list. |
| `__gc_list_concat(a, b)` | list | Concatenate two int lists. |
| `__gc_list_int_sum(lst)` | Int | Sum (0 for empty). |
| `__gc_list_int_min(lst)` | Int | Empty aborts. |
| `__gc_list_int_max(lst)` | Int | Empty aborts. |
| `__gc_list_int_println(lst)` | Unit | `[a, b, c]\n`. |
| `__gc_list_int_to_string(lst, sep)` | HeapString | Render with separator. |

## Heap-backed pointer lists

| Builtin | Returns | Description |
|---|---|---|
| `__gc_list_ptr(n)` | list | n zero-init pointer slots (tag 4). |
| `__gc_list_ptr_len(lst)` | Int | Length at offset 0. |
| `__gc_list_ptr_get(lst, idx)` | HeapPointer | Bounds-checked read. |
| `__gc_list_ptr_get_string(lst, idx)` | HeapString | Bounds-checked read when the slot stores a heap string. |
| `__gc_list_ptr_set(lst, idx, ptr)` | Unit | Bounds-checked write. |
| `__gc_list_ptr_push(lst, ptr)` | list | Functional append. |
| `__gc_list_ptr_pop(lst)` | list | Functional drop-last; empty aborts. |
| `__gc_list_ptr_reverse(lst)` | list | Fresh reversed list. |
| `__gc_list_ptr_concat(a, b)` | list | Concatenate two pointer lists. |
| `__gc_list_ptr_join(parts, sep)` | HeapString | Join heap-string elements. |

## String-keyed maps

| Builtin | Returns | Description |
|---|---|---|
| `__gc_smap_new()` | map | Empty map. |
| `__gc_smap_size(m)` | Int | Pair count. |
| `__gc_smap_has(m, key)` | Bool | Membership. |
| `__gc_smap_get(m, key)` | HeapPointer | Value, or 0 (null) when absent. |
| `__gc_smap_set(m, key, value)` | map | Fresh map (replace or append). |
| `__gc_smap_keys(m)` | list_ptr | Every key. |
| `__gc_smap_values(m)` | list_ptr | Every value. |

## Observability

| Builtin | Returns | Description |
|---|---|---|
| `__gc_collect_count()` | Int | Number of mark-and-sweep cycles. |
| `__gc_segment_count()` | Int | Number of mmap'd heap segments. |
| `__gc_pointer_count(addr)` | Int | Slot count derived from a record / array / list header. |

## Diagnostics

Every fatal runtime error prints a single-line message to stderr and
exits with status 1:

| Trigger | Message |
|---|---|
| Allocation impossible after collect + heap growth | `klassic gc: out of memory` |
| Segment table reached its 64-entry cap | `klassic gc: heap segment limit reached` |
| `__gc_pin` exceeded the 1024-entry root table | `klassic gc: root table overflow` |
| Mark worklist exceeded 4096 entries | `klassic gc: mark worklist overflow` |
| Shadow stack exceeded 8192 entries | `klassic gc: shadow stack overflow` |
| Out-of-bounds index in any length-aware op | `klassic gc: index out of bounds` |
