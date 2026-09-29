/* Owning rooted-field observation protocol, also emitted for both ISAs.
 * Ordinary compiler call lowering is not connected yet.
 * Native controls are unique, nonmoving and disjoint. The caller retains both
 * root handles, registry and pool storage. Receiver/output handles must differ.
 * Initialize stage/result, receiver access/cleanup and both root value scratch
 * fields to zero. Keep input bindings and callback semantics unchanged through
 * COMPLETE; only this owner mutates its stage and pending result.
 *
 * ENTER/RELEASE/RESET run under the heap lock. OBSERVE/EXIT run outside it.
 * READY and EXIT_REQUIRED prohibit polling, waiting, native transitions or
 * taking the heap lock. Only successful EXIT permits RELEASE under that lock.
 * COMPLETE exposes result (a GC_ROOT_* status), with no guard or page lease.
 * FLUSH from ENTER is already clean; FLUSH in result is exposed only after
 * cleanup. RESET preserves the roots and permits a fresh receiver/field read.
 * Output.value is an exact rooted observation token, never a canonical address.
 * Forwarding/marking/healing of the output and cycle admission remain separate.
 */
#ifndef KLASSIC_GC_ROOTED_OBSERVATION_H
#define KLASSIC_GC_ROOTED_OBSERVATION_H
#include "gc_root_access.h"
#include "gc_barrier.h"
typedef struct {
    uint64_t (*enter)(MetadataLeasePool *,RootAccess *);
    GuardedObserver observe;
    RootAccessCleanup cleanup;
} RootedObservationOps;
enum { GC_RO_ENTER=0,GC_RO_OBSERVE=1,GC_RO_EXIT=2,GC_RO_RELEASE=3,GC_RO_RESET=4 };
/* 0..8 retain RootFollow/RootAccess statuses. DONE means a null receiver,
 * not a successful field read. A COMPLETE request is stable until RESET. */
enum { GC_RO_RELEASE_REQUIRED=9,GC_RO_EXIT_REQUIRED=10,GC_RO_COMPLETE=11 };
static inline uint64_t gc_rooted_observation_step(MetadataLeasePool *pool,RootedObservation *q,
        uint64_t op,const RootedObservationOps *ops){
    if(!q||((uintptr_t)q&7)||!ops||!ops->enter||!ops->observe||!ops->cleanup||op>GC_RO_RESET||q->stage>4)
        return GC_RF_INVALID;
    if(op==GC_RO_ENTER){
        if(q->stage)return q->stage==1?GC_RAC_READY:q->stage==2?GC_RO_EXIT_REQUIRED:q->stage==3?GC_RO_RELEASE_REQUIRED:GC_RO_COMPLETE;
        if(q->output.value||q->output.handle.cell==q->receiver.follow.root.handle.cell)return GC_RF_INVALID;
        uint64_t r=ops->enter(pool,&q->receiver);
        if(r==GC_RAC_READY)q->stage=1;
        return r;
    }
    if(op==GC_RO_OBSERVE){
        if(q->stage==2)return GC_RO_EXIT_REQUIRED;
        if(q->stage!=1)return GC_RF_INVALID;
        MutatorContext *ctx=(void *)(uintptr_t)q->receiver.follow.context;
        GuardedLoad field={.scope=(uintptr_t)&q->receiver.access.scope,
            .page=q->receiver.follow.cleanup.page,.registry=q->registry,.byte_offset=q->byte_offset,
            .phase=atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire),.output=q->output};
        q->result=ops->observe(&field);
        q->output.value=field.output.value;q->stage=2;
        return GC_RO_EXIT_REQUIRED;
    }
    if(op==GC_RO_EXIT){
        if(q->stage!=2)return GC_RF_INVALID;
        if(ops->cleanup(NULL,&q->receiver,GC_RAC_EXIT)!=GC_RF_DONE)return GC_RO_EXIT_REQUIRED;
        q->stage=3;return GC_RO_RELEASE_REQUIRED;
    }
    if(q->stage==1)return GC_RAC_READY;
    if(q->stage==2)return GC_RO_EXIT_REQUIRED;
    if(op==GC_RO_RELEASE){
        if(q->stage==4)return GC_RO_COMPLETE;
        if(q->stage!=3&&q->stage!=0)return GC_RF_INVALID;
        uint64_t r=ops->cleanup(pool,&q->receiver,GC_RAC_RELEASE);
        if(r!=GC_RF_DONE)return r;
        if(!q->stage)return GC_RF_DONE; /* failed ENTER cleanup; no read occurred */
        q->stage=4;return GC_RO_COMPLETE;
    }
    if(q->stage!=4)return GC_RF_INVALID;
    q->output.value=0;q->result=0;q->stage=0;return GC_RF_DONE;
}
typedef uint64_t (*RootedObserver)(MetadataLeasePool *,RootedObservation *,uint64_t);
static inline uint64_t gc_rooted_observation(MetadataLeasePool *pool,RootedObservation *q,uint64_t op){
    const RootedObservationOps ops={gc_root_access_locked,gc_guarded_observe_into,gc_root_access_cleanup};
    return gc_rooted_observation_step(pool,q,op,&ops);
}
#endif
