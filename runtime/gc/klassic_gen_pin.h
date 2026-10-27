/* Owner-affine pinned byte borrows for the canonical C runtime. */
#ifndef KLASSIC_GEN_PIN_H
#define KLASSIC_GEN_PIN_H
#include "klassic_gen_runtime.h"
#ifdef __cplusplus
extern "C" {
#endif
typedef struct KgPinnedBytes KgPinnedBytes;
typedef struct KgPinnedBytes KgPinnedObject;
/* These opaque pointer values are monotonic identity capabilities, not control
 * mapping addresses. Never dereference or free them. An old capability remains
 * invalid even if native control storage is mapped again at the same address. */
/* The input is an owned RootCell containing RAW_BYTES or null. The pin takes
 * an independent strong root and may collect before returning. Release every
 * pin before its owner detaches/shuts down. Reference-record payloads cannot
 * be exposed through this byte API. */
KgPinnedBytes *kg_pin_bytes(const RootCell *root);
/* Obtain the byte address only after kg_native_enter(). The address and size
 * stay fixed until unpin; finish using it before unpin or owner detach. Native
 * callers synchronize concurrent byte access themselves. Null yields NULL/0. */
void *kg_pinned_bytes_address(const KgPinnedBytes *pin);
uint64_t kg_pinned_bytes_size(const KgPinnedBytes *pin);
/* Call in Running state after kg_native_exit(). This ends the borrow, removes
 * its actual page permit and independent root, and retires native control. */
void kg_unpin_bytes(KgPinnedBytes *pin);
/* Pin either RAW_BYTES or REF_RECORD. Reference fields remain managed: their
 * addresses/encoded references cannot be borrowed directly in Native code. */
KgPinnedObject *kg_pin_object(const RootCell *root);
uint64_t kg_pinned_object_size(const KgPinnedObject *pin);
/* Caller supplies owned, live RootCells. Both Running and Native callers are
 * supported. A Native call leaves/reenters through the actual runtime boundary
 * and preserves Native state on return; load/store use the shared rooted
 * barriers, journals, observed root and publication admission. */
void kg_pinned_load_ref_into(const KgPinnedObject *pin,uint64_t byte_offset,RootCell *out);
void kg_pinned_store_ref(const KgPinnedObject *pin,uint64_t byte_offset,const RootCell *value);
void kg_unpin_object(KgPinnedObject *pin);
#ifdef __cplusplus
}
#endif
#endif
