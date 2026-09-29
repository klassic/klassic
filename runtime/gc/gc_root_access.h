/* One bounded root -> forwarding handoff OR canonical access guard. The caller
 * holds the heap/lease lock and uniquely owns the live root/control storage.
 * No polling, allocation or managed-memory access occurs. A successful READY
 * transfers the native lease and guard to the caller: unlock, access through
 * the guard, EXIT outside the heap lock, then release the lease under the lock.
 * Never poll, flush or enter native while READY's guard is held.
 *
 * Other normal returns own no guard/lease. ADVANCED journals the destination
 * into the same root, permitting a poll between arbitrary forwarding edges.
 * WAIT/FLUSH/FULL leave the original root protected for retry. Cleanup failure
 * retains follow.cleanup explicitly; use root-follow CLEANUP before retry.
 * This does not mark, recolor, self-heal a field or establish retirement. */
#ifndef KLASSIC_GC_ROOT_ACCESS_H
#define KLASSIC_GC_ROOT_ACCESS_H
#include "gc_root_follow.h"
#include "gc_resolve.h"
#define GC_RAC_READY 8u
static inline uint64_t gc_root_access_locked(MetadataLeasePool *g,RootAccess *q){
    if(!q||((uintptr_t)q&7))return GC_RF_INVALID;
    ResolveRequest *a=&q->access;SourceAccessRequest *scope=&a->scope;
    if(a->reference||a->destination||scope->context||scope->source_offset||scope->incarnation||scope->held||scope->ticket)return GC_RF_INVALID;
    RootFollow *f=&q->follow;
    if(f->cleanup.page||f->cleanup.incarnation||f->cleanup.token)return GC_RF_CLEANUP_REQUIRED;
    if(!f->context||(f->context&7)||f->root.value)return GC_RF_INVALID;
    MutatorContext *ctx=(MutatorContext *)(uintptr_t)f->context;RootRequest root=f->root;
    if(gc_root_step(ctx,&root,GC_ROOT_READ_RAW)!=GC_ROOT_OK)return GC_RF_INVALID;
    if(!root.value)return GC_RF_DONE;
    ReferenceLease source={.reference=root.value};uint64_t status=gc_reference_lease_locked(g,&source);
    if(status!=GC_ML_OK)return status==GC_ML_FULL?GC_RF_METADATA_FULL:status==GC_ML_EXHAUSTED?GC_RF_EXHAUSTED:GC_RF_INVALID;
    f->cleanup=source.lease;
    *a=(ResolveRequest){.reference=root.value,.scope={.context=f->context}};
    status=gc_resolve_access((AllocationPage *)(uintptr_t)source.lease.page,a,GC_RESOLVE_ENTER);
    if(status==GC_RESOLVE_OK)return GC_RAC_READY;
    uint64_t result=GC_RF_INVALID;
    if(status==GC_RESOLVE_FORWARDED){
        root.value=a->destination;status=gc_root_step(ctx,&root,GC_ROOT_STORE);
        result=status==GC_ROOT_OK?GC_RF_ADVANCED:status==GC_ROOT_FLUSH?GC_RF_FLUSH:status==GC_ROOT_EXHAUSTED?GC_RF_EXHAUSTED:GC_RF_INVALID;
    }else if(status==GC_RESOLVE_WAIT)result=GC_RF_WAIT;
    else if(status==GC_RESOLVE_EXHAUSTED)result=GC_RF_EXHAUSTED;
    *a=(ResolveRequest){0};
    if(gc_metadata_step_locked(g,&f->cleanup,GC_ML_RELEASE)!=GC_ML_OK)return GC_RF_CLEANUP_REQUIRED;
    return result;
}
/* Ordered cleanup for one READY result. EXIT runs outside the heap lock and
 * retains the metadata lease; WAIT retains the guard and requires immediate
 * retry without polling. Successful EXIT clears all access scratch. RELEASE
 * runs under the heap lock only after EXIT, with no other active owner guard.
 * A failed release retains the exact lease for retry. The root remains owned
 * throughout, and successful RELEASE permits reuse of this RootAccess control.
 * No operation polls, locks, unmaps or accesses managed payload. */
#define GC_RAC_EXIT 0u
#define GC_RAC_RELEASE 1u
typedef uint64_t (*RootAccessCleanup)(MetadataLeasePool *,RootAccess *,uint64_t);
static inline uint64_t gc_root_access_cleanup(MetadataLeasePool *g,RootAccess *q,uint64_t op){
    if(op>GC_RAC_RELEASE||!q||((uintptr_t)q&7))return GC_RF_INVALID;
    RootFollow *f=&q->follow;ResolveRequest *a=&q->access;
    if(!f->context||(f->context&7)||!f->cleanup.page||(f->cleanup.page&7)||
        !f->cleanup.incarnation||!(f->cleanup.token&UINT32_MAX)||!(f->cleanup.token>>32))return GC_RF_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)f->context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id)return GC_RF_INVALID;
    if(op==GC_RAC_EXIT){
        if(a->scope.context!=f->context||a->scope.incarnation!=f->cleanup.incarnation||!a->scope.held)return GC_RF_INVALID;
        uint64_t result=gc_resolve_access((void *)(uintptr_t)f->cleanup.page,a,GC_RESOLVE_EXIT);
        if(result==GC_RESOLVE_OK)*a=(ResolveRequest){0};
        return result;
    }
    if(!g||((uintptr_t)g&7)||a->reference||a->destination||a->scope.context||a->scope.source_offset||
        a->scope.incarnation||a->scope.held||a->scope.ticket||atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RF_INVALID;
    return gc_metadata_step_locked(g,&f->cleanup,GC_ML_RELEASE)==GC_ML_OK?GC_RF_DONE:GC_RF_CLEANUP_REQUIRED;
}
#endif
