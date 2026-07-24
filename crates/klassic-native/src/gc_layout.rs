//! Architecture-independent GC design constants and object-format ABI.
//!
//! This module exists to give the collector's design a single home that
//! is not itself x86-64 machine code. Everything here is a *value* or a
//! *documented convention*, never an instruction sequence -- the actual
//! codegen (register allocation, addressing modes, syscall encoding)
//! stays in `lib.rs`, which today is the only backend that implements
//! this design. `crates/klassic-native/src/aarch64.rs` is a wholly
//! separate Mach-O/Apple-Silicon backend with no GC of its own; if it,
//! or a future direct-ELF AArch64 Linux backend (see
//! `docs/roadmap-targets-stdlib.md`, Tier 1), ever adopts this same
//! precise/concurrent collector design, everything in this file is what
//! it would need to agree on to stay object-format-compatible with the
//! x86-64 backend -- the constants below, and the layouts described in
//! prose where a value can't stand alone.
//!
//! This is a deliberately narrow first slice of "architecture
//! independence," not a claim that codegen itself has been unified. A
//! full shared pseudo-instruction IR that both backends lower from
//! (Kota's stated long-term preference -- see
//! `docs/superpowers/specs/2026-07-24-precise-concurrent-gc-design.md`,
//! "ARM64 note") is a separate, much larger undertaking spanning the
//! whole ~40k-line instruction-emission surface, not just the GC. What
//! belongs here is exactly the part that's already, truly independent
//! of any instruction set: the numbers and the format they describe.
//!
//! ## Object header layout
//!
//! Every heap allocation is `[block]`, addressed by its own start; the
//! pointer handed to user code (the "user pointer") is `block + 16`:
//!
//! ```text
//! [block +  0] : u64  total_size | (mark_bit << 63)
//! [block +  8] : u64  type_tag   (see the GC_TYPE_* constants below)
//! [block + 16..total_size] : payload (user pointer starts here)
//! ```
//!
//! `total_size` includes the 16-byte header. Bit 63 of the first header
//! word is the mark bit: **set** means "the last mark phase proved this
//! object live" (masked off before being used as a size), **clear**
//! means "unmarked" -- which sweep treats as dead and free-lists, and
//! which is also the header state a genuinely fresh allocation gets
//! (marking happens on the *next* cycle's mark phase, not at allocation
//! time). A free block has its header rewritten as a free-list node
//! instead: `[block + 16]` becomes the intrusive `next` pointer, so a
//! block must be at least 24 bytes total to ever go through the free
//! list.
//!
//! ## Pointer coloring (bit 63 of a *value*, distinct from the header's mark bit)
//!
//! Every heap pointer this backend ever hands to user code has bit 63
//! of the pointer *value itself* set (real Linux x86-64 heap/mmap
//! addresses are always far below 2^47, so this bit is free to
//! repurpose, and this is a value-level property of pointers, unrelated
//! to the mark bit living in the header word bit 63 of the pointee's
//! own memory -- two different "bit 63"s, two different addresses).
//! Dereferencing such a pointer requires resolving it through the load
//! barrier first: mask off bit 63, then look it up in the forwarding
//! table; a hit means the object has since moved and resolves to the
//! *new* address, a miss means the masked address is already current.
//!
//! ## Forwarding table
//!
//! Two parallel, equal-length, growable arrays of `u64` (`old_addr`,
//! `new_addr`), plus a count and a capacity. Entries are append-only:
//! once written, an entry is never overwritten in place except by the
//! post-compaction compression step, which repoints a prior entry's
//! `new_addr` at a fresher entry's `new_addr` when the same object has
//! been relocated more than once. Old backing arrays from a table
//! growth, and old heap segments emptied by compaction, are
//! deliberately never freed -- see the load barrier and compaction
//! comments in `lib.rs` for why (address-reuse aliasing and lock-free
//! concurrent-reader safety).
//!
//! ## Heap segments
//!
//! The heap is a growable list of up to [`GC_MAX_SEGMENTS`] mmap'd
//! regions, each described by three `u64`s: `{ base, top, end }`. Only
//! the last (highest-index) segment is ever the active bump-allocation
//! target; `top` for every other segment is a frozen snapshot of where
//! its bump pointer stood when the next segment was created.
//!
//! ## Type tags
//!
//! Stored in the header's second word (`[block + 8]`), verbatim, not
//! bit-packed with anything else. See the `GC_TYPE_*` constants below
//! for the four values in use and what each means for mark-phase
//! tracing.

/// Heap size used by the GC. Picked small so unit tests can exercise GC
/// reclamation without needing to allocate megabytes.
pub const GC_HEAP_SIZE: u64 = 1 << 20; // 1 MiB
/// Default size of each follow-on segment allocated when the initial
/// heap fills up after a collection.
pub const GC_GROW_SIZE: u64 = 1 << 20; // 1 MiB
/// Maximum number of distinct mmap'd segments the GC can ever own.
/// Each segment is 1 MiB (or more for an oversized allocation), so the
/// total reachable budget is GC_MAX_SEGMENTS * GC_GROW_SIZE = 64 MiB.
pub const GC_MAX_SEGMENTS: usize = 64;
/// Number of pointer slots in the static GC root table. Each slot is
/// either zero (free) or holds a heap pointer pinned via `__gc_pin`.
pub const GC_ROOT_TABLE_LEN: usize = 4096;
/// Maximum number of stack-frame heap-pointer slots tracked at any one
/// time on the main thread's shadow stack. Sized for deep recursion:
/// every live frame of a function with heap-pointer parameters holds
/// roots here (param slots plus any rooted temporaries), so the cap
/// bounds recursion depth for string- and enum-carrying functions.
/// Overflow stays a clean abort. The tables are zero-filled `.data`, so
/// growing them grows every emitted binary -- keep proportionate.
pub const GC_SHADOW_STACK_LEN: usize = 32768;
/// Maximum number of objects that can be queued for tracing during a
/// single mark phase. The mark worklist must hold the breadth-first
/// frontier of the live object graph; deep recursion with per-frame
/// heap values makes the live set proportional to recursion depth.
/// Marking aborts with an error message if the worklist overflows.
pub const GC_MARK_WORKLIST_LEN: usize = 65536;
/// Combined byte size of the root table, shadow stack, and mark
/// worklist -- the three fixed-size `.data`-backed GC tables.
pub const GC_TABLES_BYTES: u64 =
    ((GC_ROOT_TABLE_LEN + GC_SHADOW_STACK_LEN + GC_MARK_WORKLIST_LEN) * 8) as u64;
/// How many `clone()`-spawned threads can register their own shadow
/// sub-stack. The *main* thread never occupies a slot here -- it keeps
/// using the main shadow stack exactly as in the single-threaded case,
/// so single-threaded programs (the overwhelming majority) take the
/// exact same code path as always. A slot is only consulted for a
/// thread whose current stack pointer falls inside a registered range.
pub const MAX_CLONED_THREADS: usize = 4;
/// Shadow-stack depth available to each *individual* cloned thread.
/// Deliberately smaller than [`GC_SHADOW_STACK_LEN`] -- multi-thread
/// support proves the routing and bookkeeping are race-free, not that
/// it scales to deep recursion on every spawned thread.
pub const CLONED_THREAD_SHADOW_LEN: usize = 4096;
/// Initial capacity of the forwarding table. `zgc_forward_grow` doubles
/// it on demand, so this only controls how soon the first growth
/// happens, not a hard cap.
pub const ZGC_FORWARD_INITIAL_CAPACITY: usize = 64;
/// How many objects `__native_zgc_relocate_many_test` relocates in one
/// (single-threaded, deterministic) run. Deliberately well past
/// [`ZGC_FORWARD_INITIAL_CAPACITY`] so the test forces multiple
/// `zgc_forward_grow` doublings (64 -> 128 -> 256).
pub const ZGC_MANY_TEST_COUNT: usize = 200;
/// Largest payload size any single allocation may request, chosen so
/// `total_size` (payload + 16-byte header, page-rounded) can never
/// overflow a `u64` or trip a signed-comparison edge case.
pub const GC_MAX_PAYLOAD_SIZE: u64 = i64::MAX as u64 - 31 - 4095;
/// Largest byte length a single heap string's content may have.
pub const GC_MAX_STRING_ALLOC_SIZE: u64 = GC_MAX_PAYLOAD_SIZE - 15;
/// Largest number of pointer slots a single pointer-array/record
/// payload may hold.
pub const GC_MAX_POINTER_SLOT_COUNT: u64 = GC_MAX_PAYLOAD_SIZE / 8;
/// Largest number of elements a single heap-backed pointer list may
/// hold (payload minus the length prefix, divided by 8).
pub const GC_MAX_LIST_LENGTH: u64 = (GC_MAX_PAYLOAD_SIZE - 8) / 8;

/// Type tag: a free block. Never written by an allocator -- sweep
/// writes this into a block's header the moment it links it onto the
/// free list, as an explicit "not live data" marker distinct from any
/// real type.
pub const GC_TYPE_FREE: u64 = 0;
/// Type tag: raw bytes, no pointer fields. The mark phase never
/// recurses into this payload -- it's opaque as far as the collector's
/// object graph is concerned (heap strings and `__gc_alloc`'s raw
/// blocks use this tag).
pub const GC_TYPE_RAW_BYTES: u64 = 1;
/// Type tag: a "pointer record" whose payload is a packed, fixed-length
/// array of heap pointers that the mark phase recurses into.
pub const GC_TYPE_POINTER_RECORD: u64 = 2;
/// Type tag: a variable-length pointer array -- same tracing as
/// [`GC_TYPE_POINTER_RECORD`], distinguished only for callers that
/// care about the size-computation path, not by the mark phase.
pub const GC_TYPE_POINTER_ARRAY: u64 = 3;
/// Type tag: a heap-backed pointer list. Payload is `[len: i64, ptr_0,
/// ptr_1, ...]` -- the first qword is an integer length and must be
/// skipped by the mark phase; the remaining payload is a packed
/// pointer table identical to [`GC_TYPE_POINTER_ARRAY`] for tracing
/// purposes.
pub const GC_TYPE_POINTER_LIST: u64 = 4;
