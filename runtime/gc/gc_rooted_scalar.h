/* Rooted scalar field operation: canonical receiver -> one access -> cleanup.
 * The context OS owner retains its root and immutable input bindings, metadata
 * and descriptor storage throughout. Scalar bits have no GC reference meaning.
 * ENTER/RELEASE run under the heap lock; ACCESS/EXIT run outside locks, without
 * polling, waiting or native transitions. ENTER advances at most one forwarding
 * edge and may return a clean WAIT/FLUSH/FULL for caller poll/assist and retry.
 * READY/EXIT_REQUIRED retain a guard; RELEASE_REQUIRED retains only metadata.
 * COMPLETE exposes result and (for a successful load) value, never an address.
 * Failed cleanup retains the exact obligation and cannot repeat an access.
 * ABORT before ACCESS skips the operation; after ACCESS it only drives cleanup
 * and preserves the committed result. No reset/replay of a completed request.
 * Null ENTER returns RF_DONE without accessing payload; the caller owns that
 * null result. This owner does not release roots or implement scheduling.
 */
#ifndef KLASSIC_GC_ROOTED_SCALAR_H
#define KLASSIC_GC_ROOTED_SCALAR_H
#include "gc_root_access.h"
#include "gc_barrier.h"
typedef struct {
    uint64_t (*enter)(MetadataLeasePool *,RootAccess *);
    uint64_t (*scalar)(GuardedScalar *,uint64_t);
    RootAccessCleanup cleanup;
} RootedScalarOps;
enum { GC_RSC_ENTER=0,GC_RSC_ACCESS=1,GC_RSC_EXIT=2,GC_RSC_RELEASE=3,GC_RSC_ABORT=4 };
enum { GC_RSC_RELEASE_REQUIRED=9,GC_RSC_EXIT_REQUIRED=10,GC_RSC_COMPLETE=11 };
enum { GC_RSC_ABORTED=2 }; /* result; 0/1 are GC_SCALAR_OK/INVALID */
static inline uint64_t gc_rooted_scalar_step(MetadataLeasePool *pool,RootedScalar *q,uint64_t op,const RootedScalarOps *ops){
    if(!q||((uintptr_t)q&7)||!ops||!ops->enter||!ops->scalar||!ops->cleanup||op>GC_RSC_ABORT||q->stage>4||q->operation>GC_SCALAR_STORE)return GC_RF_INVALID;
    if(op==GC_RSC_ENTER||op==GC_RSC_ABORT){
        if(q->stage==4)return GC_RSC_COMPLETE;
        if(q->stage==3)return GC_RSC_RELEASE_REQUIRED;
        if(q->stage==2)return GC_RSC_EXIT_REQUIRED;
        if(op==GC_RSC_ABORT){
            if(q->stage==1){q->result=GC_RSC_ABORTED;q->stage=2;return GC_RSC_EXIT_REQUIRED;}
            if(q->receiver.follow.cleanup.page||q->receiver.follow.cleanup.incarnation||q->receiver.follow.cleanup.token)return GC_RF_CLEANUP_REQUIRED;
            q->result=GC_RSC_ABORTED;q->stage=4;return GC_RSC_COMPLETE;
        }
        if(q->stage==1)return GC_RAC_READY;
        uint64_t r=ops->enter(pool,&q->receiver);
        if(r==GC_RAC_READY)q->stage=1;
        return r;
    }
    if(op==GC_RSC_ACCESS){
        if(q->stage==2)return GC_RSC_EXIT_REQUIRED;
        if(q->stage!=1)return GC_RF_INVALID;
        MutatorContext *ctx=(void *)(uintptr_t)q->receiver.follow.context;
        GuardedScalar field={(uintptr_t)&q->receiver.access.scope,q->receiver.follow.cleanup.page,
            q->registry,q->byte_offset,atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire),q->value};
        q->result=ops->scalar(&field,q->operation);q->value=field.value;q->stage=2;
        return GC_RSC_EXIT_REQUIRED;
    }
    if(op==GC_RSC_EXIT){
        if(q->stage!=2)return GC_RF_INVALID;
        if(ops->cleanup(NULL,&q->receiver,GC_RAC_EXIT)!=GC_RF_DONE)return GC_RSC_EXIT_REQUIRED;
        q->stage=3;return GC_RSC_RELEASE_REQUIRED;
    }
    if(q->stage==1)return GC_RAC_READY;
    if(q->stage==2)return GC_RSC_EXIT_REQUIRED;
    if(q->stage==4)return GC_RSC_COMPLETE;
    uint64_t r=ops->cleanup(pool,&q->receiver,GC_RAC_RELEASE);
    if(r!=GC_RF_DONE)return r;
    if(!q->stage)return GC_RF_DONE;
    q->stage=4;return GC_RSC_COMPLETE;
}
typedef uint64_t (*RootedScalarStep)(MetadataLeasePool *,RootedScalar *,uint64_t);
static inline uint64_t gc_rooted_scalar(MetadataLeasePool *pool,RootedScalar *q,uint64_t op){
    const RootedScalarOps ops={gc_root_access_locked,gc_guarded_scalar,gc_root_access_cleanup};
    return gc_rooted_scalar_step(pool,q,op,&ops);
}
#endif
