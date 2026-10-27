/* Barrier-correct reference-range copy for the canonical RootCell runtime. */
#ifndef KLASSIC_GEN_BULK_H
#define KLASSIC_GEN_BULK_H
#include "klassic_gen_runtime.h"
#ifdef __cplusplus
extern "C" {
#endif
/* Copy slot_count references between published REF_RECORD objects (kind 2).
 * Offsets are bytes and must be 8-aligned, including for an empty copy. Both
 * complete ranges are checked before the first destination store. Null, wrong
 * kind, overflow, or out-of-range receivers fail with a diagnostic.
 *
 * Inputs are live owner-affine roots. Independent internal roots retain their
 * initial logical objects across every poll and GC move. Overlap within that
 * same logical object has memmove order; each slot uses a real source load
 * barrier and destination pre-store/SATB/remembered barrier. Concurrent writers
 * may interleave between slots: this operation is not an atomic snapshot.
 * No raw reference/payload address escapes. Use from the Running mutator state.
 * A fatal resource/OS error after copying starts may leave the already copied
 * directional prefix; it does not roll back successful stores. */
void kg_copy_refs(const RootCell *destination,uint64_t destination_offset,
                  const RootCell *source,uint64_t source_offset,uint64_t slot_count);
#ifdef __cplusplus
}
#endif
#endif
