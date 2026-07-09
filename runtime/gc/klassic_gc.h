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
 *           bits are flags: bit0 = FWD (forwarded; word0 then holds the
 *           new user pointer | FWD), bits 1-2 = the two alternating mark
 *           bits. size = word0 & -16.
 *   word1 = type tag (KLASSIC_GC_RAW_BYTES / _POINTER_RECORD /
 *           _POINTER_LIST). Tags >= POINTER_RECORD have an all-pointer
 *           payload; a POINTER_LIST skips a leading length qword.
 * The user pointer is block + 16.
 *
 * This header tracks the collector's growth milestone by milestone; the
 * current file implements the non-moving region mark-sweep foundation
 * (ZGC plan M4 equivalent). Colored pointers + load barrier, incremental
 * marking, and moving evacuation land in later commits.
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

/* Test/observability hooks. */
uint64_t klassic_gc_collection_count(void);
uint64_t klassic_gc_live_region_count(void);

#endif /* KLASSIC_GC_H */
