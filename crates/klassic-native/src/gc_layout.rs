//! Architecture-independent GC design constants and object-format ABI.
//!
//! This module is the single, non-x86-64 home for the collector's
//! *design*: every value here is a number or a documented convention,
//! never an instruction sequence. The actual code generation (register
//! allocation, addressing modes, syscall/shim encoding) stays in
//! `lib.rs`, which is currently the only backend that emits this
//! collector. If a future direct-ELF AArch64 backend (or any other
//! architecture) ever adopts the same precise/moving collector, this
//! file is exactly the set of facts it must agree on to stay
//! object-format compatible with the x86-64 backend -- the constants
//! below and the layout described in prose where a value cannot stand
//! alone.
//!
//! This is a deliberately narrow slice of "architecture independence":
//! the numbers and the format they describe, extracted out of the
//! instruction-emission code. A full shared instruction-emission
//! abstraction that both backends lower through (see
//! `docs/superpowers/specs/` and `docs/zgc-plan.md`) is a separate,
//! larger effort; what belongs *here* is only the part that is already
//! truly independent of any instruction set.
//!
//! ## Object header layout
//!
//! Every heap allocation is `[block]`, addressed by its own start; the
//! pointer handed to user code (the "user pointer") is `block + 16`:
//!
//! ```text
//! [block +  0] : u64  total_size (16-aligned) | header low bits (see below)
//! [block +  8] : u64  type_tag  (see the GC_TYPE_* constants)
//! [block + 16 .. total_size] : payload (user pointer starts here)
//! ```
//!
//! `total_size` includes the 16-byte header and is 16-aligned, so the
//! low 4 bits of header word0 are free and repurposed (see `GC_FWD` /
//! `GC_HMARK0` / `GC_HMARK1`). The size is recovered with `and reg, -16`.
//!
//! ## Pointer coloring (bits 60-62 of a *value*, not the header)
//!
//! Every heap pointer stored in a heap slot carries a color in bits
//! 60-62 of the pointer *value* (real Linux x86-64 heap/mmap addresses
//! are <= 47 bits, so these bits are free). A colored pointer is
//! non-canonical, so dereferencing one without stripping the color
//! faults -- that fault is the load-barrier coverage guarantee. The load
//! barrier masks the color off (`GC_COLOR_STRIP`) and, on a bad color
//! (`GC_COLOR_BAD_MASK`), runs its slow path. This is a value-level
//! property of pointers in heap slots, entirely distinct from the
//! per-object *mark* bits that live in the header word (below).

/// Region granularity for the region-based heap. Objects bump-allocate
/// within a region; whole dead regions are the unit of reclamation.
pub const GC_REGION_SIZE: u64 = 1 << 17; // 128 KiB
pub const GC_REGION_SHIFT: u8 = 17;

/// The heap is one up-front virtual reservation of this many regions
/// (64 MiB). Linux demand-pages it, so only touched regions cost
/// physical memory; the soft budget below bounds how many the mutator
/// may touch before a collection.
pub const GC_RESERVE_REGIONS: u64 = 512;
pub const GC_RESERVE_BYTES: u64 = GC_REGION_SIZE * GC_RESERVE_REGIONS;

/// Initial soft budget: 8 regions = 1 MiB. Doubles on allocation stall,
/// capped at `GC_RESERVE_REGIONS`.
pub const GC_INITIAL_BUDGET_REGIONS: u64 = 8;

/// The mark worklist holds the breadth-first frontier of the live object
/// graph; deep recursion with per-frame heap values makes the live set
/// proportional to recursion depth.
pub const GC_MARK_WORKLIST_LEN: usize = 262144;

/// Maximum number of stack-frame heap-pointer slots tracked at once.
/// Sized for deep recursion (every live frame with heap-pointer
/// parameters holds roots here); the GC tables are lazily-backed mmap,
/// so a large cap reserves address space without committing pages until
/// used. Sized to comfortably exceed the C-call-stack frame count, so a
/// deep recursion trips the stack-overflow probe (a clean diagnostic)
/// before this cap.
pub const GC_SHADOW_STACK_LEN: usize = 262144;

/// Total bytes for the single mmap backing the GC's runtime tables: the
/// shadow stack (the sole root source) and the mark worklist.
pub const GC_TABLES_BYTES: u64 = ((GC_SHADOW_STACK_LEN + GC_MARK_WORKLIST_LEN) * 8) as u64;

pub const GC_MAX_PAYLOAD_SIZE: u64 = i64::MAX as u64 - 31 - 4095;
pub const GC_MAX_STRING_ALLOC_SIZE: u64 = GC_MAX_PAYLOAD_SIZE - 15;
pub const GC_MAX_POINTER_SLOT_COUNT: u64 = GC_MAX_PAYLOAD_SIZE / 8;
pub const GC_MAX_LIST_LENGTH: u64 = (GC_MAX_PAYLOAD_SIZE - 8) / 8;

/// Type tag stored in the second header word. 0 marks a free block, 1
/// marks a raw-bytes payload (no pointer fields), 2 marks a "pointer
/// record" whose payload is a packed array of heap pointers the mark
/// phase recurses into.
pub const GC_TYPE_RAW_BYTES: u64 = 1;
pub const GC_TYPE_POINTER_RECORD: u64 = 2;
/// Heap-backed pointer list: payload is `[len: i64, ptr_0, ptr_1, ...]`.
/// The first qword is an integer length the mark phase must skip; the
/// remaining payload is a packed pointer table traced like a record.
pub const GC_TYPE_POINTER_LIST: u64 = 4;

/// ZGC colored-pointer bits (bits 60-62; see the module doc). Any
/// pointer with one set is non-canonical, so an unbarriered dereference
/// faults -- the barrier-coverage guarantee.
pub const GC_COLOR_M0: u64 = 1 << 60;
pub const GC_COLOR_M1: u64 = 1 << 61;
pub const GC_COLOR_R: u64 = 1 << 62;
pub const GC_COLOR_MASK: u64 = 7 << 60;
/// Strip mask: clears the color bits, leaving the raw address.
pub const GC_COLOR_STRIP: u64 = !GC_COLOR_MASK;
/// Bad-color test mask: a good (M0) pointer ANDs to zero; a poisoned
/// (M1) or relocation-flagged (R) pointer ANDs non-zero and takes the
/// load-barrier slow path.
pub const GC_COLOR_BAD_MASK: u64 = GC_COLOR_M1 | GC_COLOR_R;

/// M6 incremental-marking tuning. A mark quantum traces up to
/// `GC_QUANTUM_POPS` worklist objects; the driver runs one quantum per
/// `GC_QUANTUM_BYTES` allocated during the Mark phase. A cycle starts
/// proactively when live regions exceed 5/8 of the soft budget.
pub const GC_QUANTUM_POPS: u64 = 512;
pub const GC_QUANTUM_BYTES: u64 = 8192;

pub const GC_PHASE_IDLE: u64 = 0;
pub const GC_PHASE_MARK: u64 = 1;
/// M7 relocation phase: live objects are evacuated out of the sparsest
/// regions so those regions can be freed (heap compaction).
pub const GC_PHASE_RELOCATE: u64 = 2;

/// M7 object-header low bits (size is 16-aligned, so bits 0-3 of header
/// word0 are free). bit0 = forwarded (word0 then holds the new user
/// pointer `| GC_FWD`); bits 1-2 = the two alternating mark bits (which
/// parity is "current" this cycle lives in the `gc_header_mark` cell).
/// The mark bit moved off bit 63 so word0 can hold a forwarding pointer.
pub const GC_FWD: u64 = 1;
pub const GC_HMARK0: u64 = 1 << 1;
pub const GC_HMARK1: u64 = 1 << 2;
pub const GC_HMARK_BOTH: u64 = GC_HMARK0 | GC_HMARK1;
