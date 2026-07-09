/* klassic_gc.h -- the klassic garbage collector, in C.
 *
 * A C re-implementation of the ZGC-style region-based collector that the
 * hand-emitted x86_64 backend carries as machine code (see
 * docs/zgc-plan.md and the GC section of docs/architecture-rust.md, which
 * are the spec). The LLVM backend calls these functions instead of
 * emitting the collector inline; being C, the GC is debuggable with
 * ASan/UBSan/Valgrind/gdb -- the point of the migration.
 *
 * Object layout: a 16-byte header precedes each user pointer.
 *   word0 = [ size | flags ]  where the size is 16-aligned so the low 4
 *           bits are flags: bit0 = FWD (forwarded), bits 1-2 = the two
 *           alternating mark bits. size = word0 & -16 -- kept intact even
 *           when forwarded, so the from-space stays linearly walkable.
 *   word1 = type tag (KLASSIC_GC_RAW_BYTES / _POINTER_RECORD /
 *           _POINTER_LIST), OR, once the object is forwarded, the new user
 *           pointer (the tag is dead by then). Tags >= POINTER_RECORD have
 *           an all-pointer payload; a POINTER_LIST skips a leading length
 *           qword.
 * The user pointer is block + 16.
 *
 * The collector is a ZGC-style region heap with colored pointers + a load
 * barrier (M6b), incremental marking (M6c), and stop-the-world compacting
 * evacuation with in-object forwarding (M6d): sparse regions are copied
 * into a fresh to-space and every root and live field is rewritten to the
 * moved location. Lazy/concurrent relocation via the R-color barrier is a
 * later refinement (see docs/zgc-plan.md).
 */
#ifndef KLASSIC_GC_H
#define KLASSIC_GC_H

#include <stddef.h>
#include <stdint.h>

/* Type tags (word1). */
#define KLASSIC_GC_RAW_BYTES 1
#define KLASSIC_GC_POINTER_RECORD 2
#define KLASSIC_GC_POINTER_LIST 4

/* Header flag bits (low 4 bits of word0; size is 16-aligned). */
#define KLASSIC_GC_FWD 1u
#define KLASSIC_GC_HMARK0 2u
#define KLASSIC_GC_HMARK1 4u
#define KLASSIC_GC_HMARK_BOTH (KLASSIC_GC_HMARK0 | KLASSIC_GC_HMARK1)

/* Initialize the heap (idempotent). Reserves the region address space. */
void klassic_gc_init(void);

/* Allocate `size` payload bytes for an object of `type_tag`; returns the
 * user pointer (past the 16-byte header). Never returns NULL: on
 * exhaustion it prints an error and exits. */
void *klassic_gc_alloc(uint64_t size, uint64_t type_tag);

/* Precise roots: push the ADDRESS of a stack slot holding a heap pointer,
 * so the collector can both read and (once moving) update the root. Pop
 * the last `count` entries when a scope exits. */
void klassic_gc_shadow_push(void **slot_addr);
void klassic_gc_shadow_pop_n(uint64_t count);

/* Force a collection (stop-the-world mark-sweep for now). */
void klassic_gc_collect(void);

/* --- Colored pointers + load barrier ----------------------------- *
 * Heap-object SLOTS hold color-tagged pointers (bits 60-62); registers,
 * roots, and the shadow stack hold raw (stripped) pointers. The mask
 * cells are dso_local globals so the LLVM-emitted fast path folds the
 * bad-mask load into a `testq` memory operand (see the M2 spike). A
 * pointer whose color is "bad" is non-canonical, so an unbarriered
 * dereference faults -- the barrier-coverage guarantee. These are the
 * values the LLVM codegen's inline fast path reads; the slow path here
 * is a normal C function (default memory semantics, never readnone, so
 * clang cannot CSE a slot load across it). */
extern uint64_t gc_good_color;  /* the good color stored pointers carry */
extern uint64_t gc_bad_mask;    /* a pointer is "bad" iff (color & bad) != 0 */
extern uint64_t gc_strip_mask;  /* AND with this to strip a color -> raw */

/* Slow path: in: `value` = the bad-colored slot value, `slot` = the
 * field address it was loaded from. Returns the raw (stripped, and in
 * later milestones remapped) pointer, self-healing the slot to the good
 * color. */
uint64_t klassic_gc_load_barrier_slow(uint64_t value, void **slot);

/* Read a heap pointer slot through the barrier (fast path inline, slow
 * path out of line). The LLVM backend emits the fast path directly; this
 * C entry point exists for the runtime and the tests. */
void *klassic_gc_read(void **slot);

/* Store a heap pointer into a slot, coloring it good (null stays null). */
void klassic_gc_write(void **slot, void *value);

/* Test/observability hooks. */
uint64_t klassic_gc_collection_count(void);
uint64_t klassic_gc_live_region_count(void);
uint64_t klassic_gc_relocation_count(void); /* objects evacuated so far */

#endif /* KLASSIC_GC_H */
