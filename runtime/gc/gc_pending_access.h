/* Acquire a canonical guard for an externally counted pending reference with
 * its current metadata lease already held. A weak-processing owner may use
 * the completed-epoch reclamation exclusion documented in pending-follow
 * instead of a strong producer. Heap lock held, no prior raw guard.
 * The unique current reference/lease stay immutable until EXIT and cleanup.
 * access starts with only scope.context set. All controls are live/disjoint.
 *
 * READY transfers a live guard: unlock, access, then gc_resolve_access(EXIT)
 * outside the heap lock before releasing current.lease under the heap lock.
 * No wait/poll/native transition is allowed with that guard.
 *
 * Movement can occur after pending-follow DONE. FORWARDED is converted to
 * ADVANCED without adopting the private destination: access scratch is reset,
 * current's original counted reference/lease remain held, and the caller uses
 * pending-follow again before retrying access. Every other non-READY result
 * similarly retains current and owns no guard. Null DONE requires no lease.
 * This neither marks nor ACKs the external journal/root/worker obligation.
 */
#ifndef KLASSIC_GC_PENDING_ACCESS_H
#define KLASSIC_GC_PENDING_ACCESS_H
#include "gc_pending_follow.h"
#include "gc_root_access.h"
typedef uint64_t (*PendingAccessor)(MetadataLeasePool *,const ReferenceLease *,ResolveRequest *);
static inline uint64_t gc_pending_access_locked(MetadataLeasePool *g,const ReferenceLease *q,ResolveRequest *a){
    if(!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||!a||((uintptr_t)a&7)||
        a->reference||a->destination||a->scope.source_offset||a->scope.incarnation||a->scope.held||a->scope.ticket||
        !a->scope.context||(a->scope.context&7))return GC_RF_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)a->scope.context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RF_INVALID;
    if(!q->reference)return q->lease.page||q->lease.incarnation||q->lease.token?GC_RF_INVALID:GC_RF_DONE;
    MetadataLeaseRequest checked=q->lease;
    if(gc_metadata_step_locked(g,&checked,GC_ML_RESOLVE)!=GC_ML_OK||checked.page!=q->lease.page||checked.incarnation!=q->lease.incarnation)return GC_RF_INVALID;
    a->reference=q->reference;
    uint64_t status=gc_resolve_access((void *)(uintptr_t)checked.page,a,GC_RESOLVE_ENTER);
    if(status==GC_RESOLVE_OK)return GC_RAC_READY;
    *a=(ResolveRequest){.scope={.context=(uintptr_t)ctx}};
    return status==GC_RESOLVE_FORWARDED?GC_RF_ADVANCED:status==GC_RESOLVE_WAIT?GC_RF_WAIT:status==GC_RESOLVE_EXHAUSTED?GC_RF_EXHAUSTED:GC_RF_INVALID;
}
#endif
