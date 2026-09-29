/* Bounded owner for ONE generation of an externally counted strong pending
 * journal/root-walk/worker reference. The caller retains that producer until
 * both required generation handoffs, repairs and cleanup finish; this controller
 * does not establish admission or ACK it. Capture queue/epoch/phase under the
 * phase protocol; keep bindings and context stable. Only current.reference is
 * initialized, all other scratch zero. Owner/control storage never moves.
 *
 * ENTER, RELEASE, CANCEL: heap lock. OFFER: selected queue lock. ROUTE, EXIT:
 * outside locks, with the live guard. Never poll/lock with that guard. A failed
 * follow/access retains its leases; retries consume one forwarding edge per
 * turn. EXIT must finish before queue handoff or cancellation release.
 *
 * Stage/status/operation contract matches GC_RM_*. COMPLETE requires checking
 * route_result/offer_result: INITIALIZING/INVALID are not successful marking.
 * Stale epoch preserves the pending offer for cancellation/recapture. Spill is
 * counted and uses the same generation lock. CANCELLED never permits dropping
 * an unprocessed external producer. RESET preserves the canonical reference.
 */
#ifndef KLASSIC_GC_PENDING_MARK_H
#define KLASSIC_GC_PENDING_MARK_H
#include "gc_pending_access.h"
#include "gc_rooted_mark.h"
static inline uint64_t gc_pending_mark_enter(MetadataLeasePool *pool,PendingMark *q){
    if(!q->context||(q->context&7))return GC_RF_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RF_INVALID;
    ResolveRequest *a=&q->access;
    if(a->reference||a->destination||a->scope.context||a->scope.source_offset||a->scope.incarnation||a->scope.held||a->scope.ticket)return GC_RF_INVALID;
    uint64_t result=gc_pending_follow_locked(pool,&q->current,&q->retired);
    if(result!=GC_RF_DONE)return result;
    a->scope.context=q->context;result=gc_pending_access_locked(pool,&q->current,a);
    if(result!=GC_RAC_READY)*a=(ResolveRequest){0};
    return result;
}
static inline uint64_t gc_pending_mark_cleanup(MetadataLeasePool *pool,PendingMark *q,uint64_t op){
    if(op>GC_RAC_RELEASE||!q->context||(q->context&7))return GC_RF_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id)return GC_RF_INVALID;
    ResolveRequest *a=&q->access;
    if(op==GC_RAC_EXIT){
        if(!q->current.lease.page||(q->current.lease.page&7)||a->scope.context!=q->context||
            a->scope.incarnation!=q->current.lease.incarnation||!a->scope.held)return GC_RF_INVALID;
        uint64_t result=gc_resolve_access((void *)(uintptr_t)q->current.lease.page,a,GC_RESOLVE_EXIT);
        if(result==GC_RESOLVE_OK)*a=(ResolveRequest){0};return result;
    }
    if(!pool||((uintptr_t)pool&7)||a->reference||a->destination||a->scope.context||a->scope.source_offset||a->scope.incarnation||a->scope.held||a->scope.ticket||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RF_INVALID;
    MetadataLeaseRequest *leases[]={&q->retired,&q->current.lease};
    for(unsigned i=0;i<2;i++)if(leases[i]->page||leases[i]->incarnation||leases[i]->token)
        if(gc_metadata_step_locked(pool,leases[i],GC_ML_RELEASE)!=GC_ML_OK)return GC_RF_CLEANUP_REQUIRED;
    return GC_RF_DONE;
}
typedef struct {
    uint64_t (*enter)(MetadataLeasePool *,PendingMark *);
    uint64_t (*route)(GuardedMarkRoute *);
    uint64_t (*cleanup)(MetadataLeasePool *,PendingMark *,uint64_t);
    uint64_t (*offer)(MarkQueue *,MarkRequest *,uint64_t);
} PendingMarkOps;
static inline uint64_t gc_pending_mark_step(MetadataLeasePool *pool,PendingMark *q,uint64_t op,const PendingMarkOps *ops){
    if(!q||((uintptr_t)q&7)||!ops||!ops->enter||!ops->route||!ops->cleanup||!ops->offer||
        op>GC_RM_RESET||q->stage>6)return GC_RF_INVALID;
    if(op==GC_RM_ENTER){
        if(q->stage)return q->stage==1?GC_RAC_READY:q->stage==2?GC_RM_EXIT_REQUIRED:
            q->stage==3?GC_RM_OFFER_REQUIRED:q->stage==4?GC_RM_RELEASE_REQUIRED:
            q->stage==5?GC_RM_COMPLETE:GC_RM_CANCELLED;
        if(!q->queue||(q->queue&7)||q->generation>GC_OLD||!q->epoch||
            !gc_abi_valid_phase((PhaseSnapshot){q->phase}))return GC_RF_INVALID;
        uint64_t phase=(q->phase>>(q->generation*4))&15;
        if(phase<GC_MARK_START||phase>GC_MARK_END_CHECK)return GC_RF_INVALID;
        uint64_t r=ops->enter(pool,q);
        if(r==GC_RAC_READY)q->stage=1;
        else if(r==GC_RF_DONE){q->route_result=GC_ROUTE_SKIP;q->stage=5;return GC_RM_COMPLETE;}
        return r;
    }
    if(op==GC_RM_ROUTE){
        if(q->stage==2)return GC_RM_EXIT_REQUIRED;
        if(q->stage!=1)return GC_RF_INVALID;
        q->route=(GuardedMarkRoute){.generation=q->generation,.prepare={
            .scope=(uintptr_t)&q->access.scope,.page=q->current.lease.page,
            .reference=q->current.reference,.phase=q->phase,.lease=q->current.lease}};
        q->route_result=ops->route(&q->route);q->stage=2;
        return GC_RM_EXIT_REQUIRED;
    }
    if(op==GC_RM_EXIT){
        if(q->stage!=2)return GC_RF_INVALID;
        if(ops->cleanup(NULL,q,GC_RAC_EXIT)!=GC_RF_DONE)return GC_RM_EXIT_REQUIRED;
        q->stage=q->route_result==GC_ROUTE_READY?3:4;
        return q->stage==3?GC_RM_OFFER_REQUIRED:GC_RM_RELEASE_REQUIRED;
    }
    if(q->stage==1)return GC_RAC_READY;
    if(q->stage==2)return GC_RM_EXIT_REQUIRED;
    if(op==GC_RM_OFFER){
        if(q->stage!=3)return GC_RF_INVALID;
        MarkQueue *queue=(void *)(uintptr_t)q->queue;
        if(queue->generation!=q->generation||queue->epoch!=q->epoch)return GC_RM_STALE;
        uint64_t r=ops->offer(queue,&q->route.prepare.output,GC_MARK_OFFER);
        q->offer_result=r;
        if(r==GC_MARK_OK){
            q->current.lease=(MetadataLeaseRequest){0};q->route=(GuardedMarkRoute){0};
            q->stage=5;return GC_RM_COMPLETE;
        }
        if(r==GC_MARK_DUPLICATE){q->stage=4;return GC_RM_RELEASE_REQUIRED;}
        return r==GC_MARK_FULL?GC_RM_FULL:r==GC_MARK_EXHAUSTED?GC_RM_EXHAUSTED:GC_RF_INVALID;
    }
    if(op==GC_RM_RELEASE||op==GC_RM_CANCEL){
        if(op==GC_RM_RELEASE&&q->stage!=4)return GC_RF_INVALID;
        if(op==GC_RM_CANCEL&&q->stage>4)return q->stage==5?GC_RM_COMPLETE:GC_RM_CANCELLED;
        MetadataLeaseRequest *lease=&q->current.lease;
        if(lease->page||lease->incarnation||lease->token||q->retired.page||q->retired.incarnation||q->retired.token){
            uint64_t r=ops->cleanup(pool,q,GC_RAC_RELEASE);
            if(r!=GC_RF_DONE)return r;
        }
        q->route=(GuardedMarkRoute){0};q->stage=op==GC_RM_CANCEL?6:5;
        return op==GC_RM_CANCEL?GC_RM_CANCELLED:GC_RM_COMPLETE;
    }
    if(q->stage!=5&&q->stage!=6)return GC_RF_INVALID;
    q->stage=q->route_result=q->offer_result=0;return GC_RF_DONE;
}
typedef uint64_t (*PendingMarker)(MetadataLeasePool *,PendingMark *,uint64_t);
static inline uint64_t gc_pending_mark(MetadataLeasePool *pool,PendingMark *q,uint64_t op){
    const PendingMarkOps ops={gc_pending_mark_enter,gc_guarded_mark_route,gc_pending_mark_cleanup,gc_mark_spill_locked};
    return gc_pending_mark_step(pool,q,op,&ops);
}
#endif
