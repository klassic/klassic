/* Root -> canonical guard -> generation-filtered mark offer owner.
 * Unique live native controls, stable roots/registry/queue storage, and a counted
 * producer obligation are caller-owned throughout. Capture generation, epoch,
 * phase and queue under cycle admission; zero all remaining scratch initially.
 * ENTER/RELEASE/CANCEL use the heap lock, OFFER uses the selected queue lock;
 * ROUTE/EXIT run outside both. Never acquire a lock, wait or poll with a guard.
 *
 * The route's lease fields alias access.follow.cleanup inside this ONE owner;
 * they are never independent obligations. Only OFFER OK transfers ownership to
 * the queue. Duplicate/rejection retains ownership until exact metadata release.
 * EXIT retries do not reroute or re-offer. FULL/EXHAUSTED preserve pending work.
 * offer may use ordinary OFFER or OFFER_OR_SPILL under the same queue lock;
 * both use operation zero and the same ownership/status contract.
 * Changed queue epoch returns STALE without modifying work; the caller must
 * cancel/recapture while retaining its root and producer obligation.
 *
 * COMPLETE requires checking route_result and offer_result. WAIT/INVALID are
 * cleaned results, not successful marking. CANCELLED is not mark completion.
 * C and shared direct emitters implement this protocol. Ordinary compiler
 * lowering remains separate. It does not finish a cycle,
 * recolor a root, heal a field or discharge the caller's producer accounting.
 */
#ifndef KLASSIC_GC_ROOTED_MARK_H
#define KLASSIC_GC_ROOTED_MARK_H
#include "gc_root_access.h"
#include "gc_mark_route.h"
#include "gc_mark_spill.h"
typedef struct {
    uint64_t (*enter)(MetadataLeasePool *,RootAccess *);
    uint64_t (*route)(GuardedMarkRoute *);
    RootAccessCleanup cleanup;
    uint64_t (*offer)(MarkQueue *,MarkRequest *,uint64_t);
} RootedMarkOps;
enum { GC_RM_ENTER=0,GC_RM_ROUTE=1,GC_RM_EXIT=2,GC_RM_OFFER=3,GC_RM_RELEASE=4,GC_RM_CANCEL=5,GC_RM_RESET=6 };
enum { GC_RM_OFFER_REQUIRED=9,GC_RM_RELEASE_REQUIRED=10,GC_RM_EXIT_REQUIRED=11,
    GC_RM_COMPLETE=12,GC_RM_CANCELLED=13,GC_RM_STALE=14,GC_RM_FULL=15,GC_RM_EXHAUSTED=16 };
static inline uint64_t gc_rooted_mark_step(MetadataLeasePool *pool,RootedMark *q,uint64_t op,const RootedMarkOps *ops){
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
        uint64_t r=ops->enter(pool,&q->access);
        if(r==GC_RAC_READY)q->stage=1;
        else if(r==GC_RF_DONE){q->route_result=GC_ROUTE_SKIP;q->stage=5;return GC_RM_COMPLETE;}
        return r;
    }
    if(op==GC_RM_ROUTE){
        if(q->stage==2)return GC_RM_EXIT_REQUIRED;
        if(q->stage!=1)return GC_RF_INVALID;
        q->route=(GuardedMarkRoute){.generation=q->generation,.prepare={
            .scope=(uintptr_t)&q->access.access.scope,.page=q->access.follow.cleanup.page,
            .reference=q->access.access.reference,.phase=q->phase,.lease=q->access.follow.cleanup}};
        q->route_result=ops->route(&q->route);q->stage=2;
        return GC_RM_EXIT_REQUIRED;
    }
    if(op==GC_RM_EXIT){
        if(q->stage!=2)return GC_RF_INVALID;
        if(ops->cleanup(NULL,&q->access,GC_RAC_EXIT)!=GC_RF_DONE)return GC_RM_EXIT_REQUIRED;
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
            q->access.follow.cleanup=(MetadataLeaseRequest){0};q->route=(GuardedMarkRoute){0};
            q->stage=5;return GC_RM_COMPLETE;
        }
        if(r==GC_MARK_DUPLICATE){q->stage=4;return GC_RM_RELEASE_REQUIRED;}
        return r==GC_MARK_FULL?GC_RM_FULL:r==GC_MARK_EXHAUSTED?GC_RM_EXHAUSTED:GC_RF_INVALID;
    }
    if(op==GC_RM_RELEASE||op==GC_RM_CANCEL){
        if(op==GC_RM_RELEASE&&q->stage!=4)return GC_RF_INVALID;
        if(op==GC_RM_CANCEL&&q->stage>4)return q->stage==5?GC_RM_COMPLETE:GC_RM_CANCELLED;
        MetadataLeaseRequest *lease=&q->access.follow.cleanup;
        if(lease->page||lease->incarnation||lease->token){
            uint64_t r=ops->cleanup(pool,&q->access,GC_RAC_RELEASE);
            if(r!=GC_RF_DONE)return r;
        }
        q->route=(GuardedMarkRoute){0};q->stage=op==GC_RM_CANCEL?6:5;
        return op==GC_RM_CANCEL?GC_RM_CANCELLED:GC_RM_COMPLETE;
    }
    if(q->stage!=5&&q->stage!=6)return GC_RF_INVALID;
    q->stage=q->route_result=q->offer_result=0;return GC_RF_DONE;
}
typedef uint64_t (*RootedMarker)(MetadataLeasePool *,RootedMark *,uint64_t);
static inline uint64_t gc_rooted_mark(MetadataLeasePool *pool,RootedMark *q,uint64_t op){
    const RootedMarkOps ops={gc_root_access_locked,gc_guarded_mark_route,gc_root_access_cleanup,gc_mark_step_locked};
    return gc_rooted_mark_step(pool,q,op,&ops);
}
static inline uint64_t gc_rooted_mark_spill(MetadataLeasePool *pool,RootedMark *q,uint64_t op){
    const RootedMarkOps ops={gc_root_access_locked,gc_guarded_mark_route,gc_root_access_cleanup,gc_mark_spill_locked};
    return gc_rooted_mark_step(pool,q,op,&ops);
}
#endif
