/* Actual backing retention, distinct from the allocator's active-run charge.
 * A cached page remains OS-committed inside the same heap reservation, contains
 * no live object, and is protected until a new actual lease reuses it. */
#ifndef KLASSIC_GEN_POLICY_H
#define KLASSIC_GEN_POLICY_H
#include "klassic_gen_runtime_internal.h"
uint64_t kg_policy_init(KgRuntime *);
uint64_t kg_policy_clock(void);
void kg_policy_poll(KgRuntime *,const MutatorContext *,uint64_t now);
void kg_policy_before_prepare(KgRuntime *,KgRun *,const MutatorContext *);
void kg_policy_after_prepare(KgRuntime *,KgRun *);
void kg_policy_os_enter(KgRuntime *,const MutatorContext *);
void kg_policy_os_leave(KgRuntime *);
uint64_t kg_policy_commit(NativeHeapOwner *,PageLease *);
bool kg_policy_decommit(void *,size_t);
uint64_t kg_policy_source_uncommit(KgRuntime *,SourceUncommit *,const MutatorContext *);
void kg_policy_begin_shutdown(KgRuntime *);
uint64_t kg_policy_dispose_native(KgRuntime *);
/* Call with the heap mutex. This is actual OS-backed bytes, including the
 * retained-free and mapped preparation domains; HeapPool.committed remains
 * the separate active-run allocation charge. */
uint64_t kg_policy_committed_locked(KgRuntime *);
uint64_t kg_policy_check_locked(KgRuntime *);
void kg_policy_verified(KgRuntime *,uint64_t,uint64_t,uint64_t,uint64_t);
uint64_t kg_policy_verifications_locked(KgRuntime *);
enum { KG_VERIFY_OK=0,KG_VERIFY_WAIT=1,KG_VERIFY_INVALID=2 };
/* Single phase-owner actor, no guard/lock on entry. Traversal takes finite
 * registry/metadata steps and guards individual objects outside stopped phases.
 * WAIT is real capacity pressure and must retry after shared work progresses. */
uint64_t kg_verify_runtime(KgRuntime *,MutatorContext *);
#endif
