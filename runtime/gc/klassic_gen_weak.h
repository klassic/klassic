/* Shareable runtime weak handles; results are published into owned roots. */
#ifndef KLASSIC_GEN_WEAK_H
#define KLASSIC_GEN_WEAK_H
#include "klassic_gen_runtime.h"
#ifdef __cplusplus
extern "C" {
#endif
/* Values contain no native addresses. Runtime incarnation and slot serial
 * reject stale copies after release, index reuse, and runtime reinitialization. */
typedef struct { uint64_t runtime,index,serial; } KgWeakHandle;
/* Source remains rooted across Native backing growth and phase adoption.
 * Null is a valid weak referent. The handle never becomes a strong root. */
KgWeakHandle kg_weak_new(const RootCell *source);
/* Clear the caller-owned output, then atomically publish any surviving
 * referent through the real root journal. False also rejects stale handles.
 * May poll/assist/collect; no unrooted GcRef or managed address is returned. */
bool kg_weak_load_into(KgWeakHandle handle,RootCell *out);
/* Exactly one concurrent release wins. Clearing a dead referent preserves
 * the handle; explicit release invalidates its serial and frees its index. */
bool kg_weak_release(KgWeakHandle handle);
#ifdef __cplusplus
}
#endif
#endif
