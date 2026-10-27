/* Canonical RootCell ABI for the C/LLVM generational mutator adapter.
 * This API never returns a raw movable heap address. Roots are native, stable
 * owner-affine handles; keep them alive through every potentially polling call.
 * Initialization uses the shared offset-reference heap/descriptor/mark metadata,
 * never the legacy klassic_gc object header or pointer-stack collector. */
#ifndef KLASSIC_GEN_RUNTIME_H
#define KLASSIC_GEN_RUNTIME_H
#include "gc_abi.h"
#ifdef __cplusplus
extern "C" {
#endif
/* Configure before init. Zero sizes choose runtime defaults. Flags: bit 0
 * stress collection, bit 1 diagnostics; bit 2 root/heap verification.
 * Minimum retains real OS backing as reusable committed-free pages. Other unsupported controls
 * must produce an explicit diagnostic, never silently change collector mode. */
void kg_configure(uint64_t hard_max,uint64_t min_heap,uint64_t soft_max,uint64_t flags);
/* Monotonic delay from actual FREE observation; configure before init.
 * The embedding adapter defaults to zero; CLI supplies its common default. */
void kg_configure_uncommit_delay(uint64_t nanoseconds);
void kg_init(void);
void kg_shutdown(void);
/* Additional OS threads explicitly attach to the initialized shared runtime.
 * Release their owned roots before detach. The bootstrap owner uses shutdown. */
bool kg_try_thread_attach(void);
void kg_thread_attach(void);
void kg_thread_detach(void);
/* Borrow a live source root across threads, then retain it in an owned root.
 * The source owner keeps its cell alive until this call returns. */
void kg_root_import(RootCell *out,const RootCell *source);
uint64_t kg_committed_bytes(void);
/* Explicit verification uses the same real checker as flag bit 2. It neither
 * roots otherwise dead objects nor grants marking or retirement credit. */
void kg_verify(void);
uint64_t kg_verification_count(void);
void kg_poll(void);
void kg_native_enter(void);
void kg_native_exit(void);
RootCell *kg_root_new(void);
void kg_root_copy(RootCell *out,const RootCell *source);
void kg_root_clear(RootCell *root);
void kg_root_release(RootCell *root);
void kg_alloc_into(uint64_t bytes,uint64_t kind,RootCell *out);
void kg_store_ref(const RootCell *base,uint64_t byte_offset,const RootCell *value);
void kg_load_ref_into(const RootCell *base,uint64_t byte_offset,RootCell *out);
void kg_store_i64(const RootCell *base,uint64_t byte_offset,uint64_t bits);
uint64_t kg_load_i64(const RootCell *base,uint64_t byte_offset);
#ifdef __cplusplus
}
#endif
#endif
