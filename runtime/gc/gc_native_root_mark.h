/* One runtime-owned audit item -> canonical pending mark -> queue handoff.
 * Stable unique control; caller serializes its operations and retains the
 * runtime, registered worker context and audit. BEGIN/ACK use registry+queue
 * locks; ENTER/RELEASE/CANCEL use the heap lock; OFFER uses the queue lock.
 * ROUTE/EXIT run outside locks. Never poll or acquire a lock with a raw guard.
 * The audit's counted walker remains live through all retries and cancellation.
 * This entry does not scan children, finish an audit or run a scheduler.
 */
#ifndef KLASSIC_GC_NATIVE_ROOT_MARK_H
#define KLASSIC_GC_NATIVE_ROOT_MARK_H
#include "gc_native_cycle.h"
#include "gc_pending_mark.h"
typedef struct {
    uint64_t self,generation,stage;
    NativeRuntimeOwner *runtime;
    RootWalkItem item;
    PendingMark pending;
} NativeRootMark;
static inline bool gc_native_root_mark_valid(const NativeRootMark *q){
    return q&&!((uintptr_t)q&7)&&q->self==(uintptr_t)q&&q->stage==1&&
        gc_native_cycle_valid(q->runtime,q->generation)&&
        q->item.walk==(uintptr_t)&q->runtime->cycle[q->generation].marking_audit.walk;
}
static inline uint64_t gc_native_root_mark_begin_locked(NativeRuntimeOwner *r,unsigned gen,
        MutatorContext *worker,NativeRootMark *q){
    if(!q||((uintptr_t)q&7)||q->self||!gc_native_cycle_valid(r,gen)||!worker||((uintptr_t)worker&7)||
       worker->abi_version!=GC_ABI_VERSION||!worker->thread_id||
       atomic_load_explicit(&worker->state,memory_order_acquire)!=GC_RUNNING||
       atomic_load_explicit(&worker->access_epoch,memory_order_acquire))return GC_RWI_INVALID;
    RootAudit *audit=&r->cycle[gen].marking_audit;MarkQueue *queue=&gc_native_runtime_work(r)->queues[gen];
    uint64_t phase=(r->roots.phase>>(gen*4))&15;
    if(phase<GC_MARK_START||phase>GC_MARK_END_CHECK||audit->state!=1||audit->generation!=gen||
       audit->epoch!=queue->epoch||audit->queue!=(uintptr_t)queue||
       atomic_load_explicit(&worker->adopted_phase,memory_order_acquire)!=r->roots.phase)return GC_RWI_INVALID;
    RootWalkItem item={(uintptr_t)&audit->walk,audit->walk.lease.serial,0};
    uint64_t status=gc_root_walk_item(&item,GC_RWI_CAPTURE);
    if(status!=GC_RWI_ITEM)return status;
    *q=(NativeRootMark){.self=(uintptr_t)q,.generation=gen,.stage=1,.runtime=r,.item=item,
        .pending={.context=(uintptr_t)worker,.queue=(uintptr_t)queue,.generation=gen,
            .epoch=queue->epoch,.phase=r->roots.phase,.current={.reference=audit->walk.pending_reference}}};
    return GC_RWI_ITEM;
}
/* Cancellation retains the captured root. RESET can retry with a newly adopted
 * phase, but never changes the selected generation's epoch or drops its item. */
static inline uint64_t gc_native_root_mark_step(NativeRootMark *q,uint64_t op,PendingMarker mark){
    if(!gc_native_root_mark_valid(q)||!mark)return GC_RF_INVALID;
    RootWalk *walk=(void *)(uintptr_t)q->item.walk;
    if(!walk->active||!walk->pending||walk->lease.serial!=q->item.serial||walk->pending_index!=q->item.index)
        return GC_RF_INVALID;
    PendingMark *pending=&q->pending;
    if(op==GC_RM_RESET){
        MarkQueue *queue=&gc_native_runtime_work(q->runtime)->queues[q->generation];
        if(pending->epoch!=queue->epoch)return GC_RM_STALE;
        MutatorContext *worker=(void *)(uintptr_t)pending->context;
        uint64_t phase=q->runtime->roots.phase,lane=(phase>>(q->generation*4))&15;
        if(lane<GC_MARK_START||lane>GC_MARK_END_CHECK||
           atomic_load_explicit(&worker->adopted_phase,memory_order_acquire)!=phase)return GC_RM_STALE;
        uint64_t status=mark(&gc_native_runtime_work(q->runtime)->leases,pending,op);
        if(status==GC_RF_DONE)pending->phase=phase;
        return status;
    }
    uint64_t status=mark(&gc_native_runtime_work(q->runtime)->leases,pending,op);
    /* Pending-follow has already acquired the destination before returning. */
    walk->pending_reference=pending->current.reference;
    return status;
}
static inline uint64_t gc_native_root_mark_ack_locked(NativeRootMark *q){
    if(!gc_native_root_mark_valid(q))return GC_RWI_INVALID;
    PendingMark *p=&q->pending;RootWalk *walk=(void *)(uintptr_t)q->item.walk;
    MarkQueue *queue=&gc_native_runtime_work(q->runtime)->queues[q->generation];
    if(p->stage!=5||p->epoch!=queue->epoch||
       (p->route_result!=GC_ROUTE_SKIP&&!(p->route_result==GC_ROUTE_READY&&
        (p->offer_result==GC_MARK_OK||p->offer_result==GC_MARK_DUPLICATE)))||
       p->current.lease.token||p->retired.token||p->access.scope.held)return GC_RWI_WAIT;
    if(!walk->active||walk->pending!=1||walk->lease.serial!=q->item.serial||walk->pending_index!=q->item.index)
        return GC_RWI_INVALID;
    if(walk->pending_observed&&p->current.reference){
        RootWalkHeal heal={.walk=q->item.walk,.serial=q->item.serial,.index=q->item.index,
            .expected=walk->pending_observed,.desired=(p->current.reference&~UINT64_C(0xffff))|(walk->pending_observed&0xffff)};
        uint64_t result=gc_root_walk_heal(&heal);
        if(result!=GC_RWH_OK&&result!=GC_RWH_CHANGED)return GC_RWI_INVALID;
    }
    uint64_t result=gc_root_walk_item(&q->item,GC_RWI_ACK);
    if(result==GC_RWI_ITEM)q->stage=2;
    return result;
}
#endif
