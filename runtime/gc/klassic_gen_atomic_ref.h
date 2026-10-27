/* Atomic logical-reference operations for the canonical RootCell ABI. */
#ifndef KLASSIC_GEN_ATOMIC_REF_H
#define KLASSIC_GEN_ATOMIC_REF_H
#include "klassic_gen_runtime.h"
#ifdef __cplusplus
extern "C" {
#endif
/* All inputs and output are live owner-affine roots of a Running mutator.
 * Initial logical inputs are retained independently, so observed_out may alias
 * an input cell. A complete load barrier publishes the old value into output.
 * Success and mismatch linearize at the actual SeqCst slot CAS/read. Expected
 * comparison uses the protected nonmoving MarkRecord identity, including when
 * colored/forwarded representations differ. It never compares debug ObjectIds.
 * The receiver must contain a reference slot at the aligned byte offset.
 * Contention, healing, and movement retry from a fresh rooted observation.
 * A successful heap mutation is never repeated during cleanup or output repair. */
bool kg_compare_exchange_ref_into(const RootCell *receiver,uint64_t byte_offset,
    const RootCell *expected,const RootCell *desired,RootCell *observed_out);
void kg_exchange_ref_into(const RootCell *receiver,uint64_t byte_offset,
    const RootCell *desired,RootCell *observed_out);
#ifdef __cplusplus
}
#endif
#endif
