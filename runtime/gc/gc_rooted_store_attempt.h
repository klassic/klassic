/* One physical rooted-store attempt with explicit guard/entry-lease ownership.
 * Inputs: receiver.follow bindings, marks, commit.registry/byte_offset/expected.
 * All other fields start zero. Controls are unique, native, live and disjoint.
 * The outer store retains receiver, desired and observed strong roots, keeps
 * the desired logical referent fixed, and owns phase/private-producer admission
 * from value preparation through cleanup. A lease is not that liveness proof.
 *
 * ENTER holds the heap lock already; it acquires a canonical receiver guard and
 * at most two entry leases without taking another lock. Unlock before ACCESS
 * or EXIT. Never poll/flush/wait or take a lock while a guard is held. RELEASE
 * holds the heap lock and releases at most one lease per call. READY requests
 * ACCESS; EXIT_REQUIRED requests EXIT; RELEASE_REQUIRED requests RELEASE.
 * An ENTER retry with no guard retains RootAccess's clean retry semantics;
 * failed ENTER cleanup uses RELEASE before retry. RF_DONE at ENTER means null.
 *
 * Entry-admission failures are reported in result only after complete cleanup.
 * ACCESS executes once. Transferred leases belong to buffers even if CAS loses;
 * this owner discards those local obligations without releasing their tokens.
 * ABORT before ACCESS skips the commit; after ACCESS it preserves its result.
 * COMPLETE exposes result and commit.transferred, with no owned guard/lease.
 * Store RETRY/FULL/REPREPARE never implicitly repeats a commit. The outer owner
 * must obtain fresh observation/preparation/admission as appropriate and start
 * a new attempt after cleanup. No reset of this completed request.
 */
#ifndef KLASSIC_GC_ROOTED_STORE_ATTEMPT_H
#define KLASSIC_GC_ROOTED_STORE_ATTEMPT_H
#include "gc_root_access.h"
#include "gc_rooted_store_commit.h"
typedef struct {
    uint64_t (*enter)(MetadataLeasePool *,RootAccess *);
    uint64_t (*metadata)(MetadataLeasePool *,MetadataLeaseRequest *,uint64_t);
    RootedStoreCommitter commit;
    RootAccessCleanup cleanup;
} RootedStoreAttemptOps;
enum { GC_STA_ENTER=0,GC_STA_ACCESS=1,GC_STA_EXIT=2,GC_STA_RELEASE=3,GC_STA_ABORT=4 };
enum { GC_STA_RELEASE_REQUIRED=9,GC_STA_EXIT_REQUIRED=10,GC_STA_COMPLETE=11 };
/* 0..6 in result retain GuardedStore statuses. */
enum { GC_STA_ABORTED=7,GC_STA_METADATA_FULL=8,GC_STA_METADATA_EXHAUSTED=9,GC_STA_METADATA_ERROR=10 };
static inline uint64_t gc_rooted_store_attempt_step(MetadataLeasePool *pool,RootedStoreAttempt *q,uint64_t op,const RootedStoreAttemptOps *ops){
    if(!q||((uintptr_t)q&7)||!ops||!ops->enter||!ops->metadata||!ops->commit||!ops->cleanup||
        op>GC_STA_ABORT||q->stage>5||q->release_index>2)return GC_RF_INVALID;
    if(q->stage==5)return GC_STA_COMPLETE;
    if(op==GC_STA_ENTER||op==GC_STA_ABORT){
        if(q->stage>=3)return GC_STA_RELEASE_REQUIRED;
        if(q->stage==2)return GC_STA_EXIT_REQUIRED;
        if(op==GC_STA_ABORT){
            q->result=GC_STA_ABORTED;
            if(q->stage==1){q->stage=2;return GC_STA_EXIT_REQUIRED;}
            if(q->receiver.follow.cleanup.page||q->receiver.follow.cleanup.incarnation||q->receiver.follow.cleanup.token){q->stage=3;return GC_STA_RELEASE_REQUIRED;}
            q->stage=5;return GC_STA_COMPLETE;
        }
        if(q->stage==1)return GC_RAC_READY;
        if(q->result||q->release_index||q->commit.scope||q->commit.page||q->commit.phase||q->commit.desired||q->commit.transferred)return GC_RF_INVALID;
        for(unsigned i=0;i<2;i++)if(q->entries[i].page||q->entries[i].incarnation||q->entries[i].token)return GC_RF_INVALID;
        uint64_t r=ops->enter(pool,&q->receiver);
        if(r!=GC_RAC_READY)return r;
        for(unsigned i=0;i<2;i++){
            q->entries[i]=(MetadataLeaseRequest){q->receiver.follow.cleanup.page,q->receiver.follow.cleanup.incarnation,0};
            r=ops->metadata(pool,&q->entries[i],GC_ML_ACQUIRE);
            if(r!=GC_ML_OK){
                q->result=r==GC_ML_FULL?GC_STA_METADATA_FULL:r==GC_ML_EXHAUSTED?GC_STA_METADATA_EXHAUSTED:GC_STA_METADATA_ERROR;
                q->stage=2;return GC_STA_EXIT_REQUIRED;
            }
        }
        MutatorContext *ctx=(void *)(uintptr_t)q->receiver.follow.context;
        q->commit.scope=(uintptr_t)&q->receiver.access.scope;q->commit.page=q->receiver.follow.cleanup.page;
        q->commit.phase=atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire);
        BarrierEntry *entries[]={&q->commit.satb,&q->commit.remembered};
        for(unsigned i=0;i<2;i++)*entries[i]=(BarrierEntry){{q->commit.expected},{{q->receiver.access.reference},q->commit.byte_offset,
            q->receiver.follow.cleanup.incarnation,q->commit.phase,q->entries[i].token}};
        q->stage=1;return GC_RAC_READY;
    }
    if(op==GC_STA_ACCESS){
        if(q->stage==2)return GC_STA_EXIT_REQUIRED;
        if(q->stage!=1)return GC_RF_INVALID;
        q->result=ops->commit((void *)(uintptr_t)q->marks,&q->commit);
        for(unsigned i=0;i<2;i++)if(q->commit.transferred&(UINT64_C(1)<<i))q->entries[i]=(MetadataLeaseRequest){0};
        q->stage=2;return GC_STA_EXIT_REQUIRED;
    }
    if(op==GC_STA_EXIT){
        if(q->stage!=2)return GC_RF_INVALID;
        if(ops->cleanup(NULL,&q->receiver,GC_RAC_EXIT)!=GC_RF_DONE)return GC_STA_EXIT_REQUIRED;
        q->stage=3;return GC_STA_RELEASE_REQUIRED;
    }
    if(q->stage==1)return GC_RAC_READY;
    if(q->stage==2)return GC_STA_EXIT_REQUIRED;
    if(!q->stage)return ops->cleanup(pool,&q->receiver,GC_RAC_RELEASE);
    if(q->stage==3){
        if(ops->cleanup(pool,&q->receiver,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_STA_RELEASE_REQUIRED;
        q->stage=4;return GC_STA_RELEASE_REQUIRED;
    }
    if(q->release_index==2)return GC_RF_INVALID;
    MetadataLeaseRequest *entry=&q->entries[q->release_index];
    if(entry->token&&ops->metadata(pool,entry,GC_ML_RELEASE)!=GC_ML_OK)return GC_STA_RELEASE_REQUIRED;
    *entry=(MetadataLeaseRequest){0};q->release_index++;
    if(q->release_index<2)return GC_STA_RELEASE_REQUIRED;
    q->stage=5;return GC_STA_COMPLETE;
}
typedef uint64_t (*RootedStoreAttempter)(MetadataLeasePool *,RootedStoreAttempt *,uint64_t);
static inline uint64_t gc_rooted_store_attempt(MetadataLeasePool *pool,RootedStoreAttempt *q,uint64_t op){
    const RootedStoreAttemptOps ops={gc_root_access_locked,gc_metadata_step_locked,gc_rooted_store_commit,gc_root_access_cleanup};
    return gc_rooted_store_attempt_step(pool,q,op,&ops);
}
#endif
