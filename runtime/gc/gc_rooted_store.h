/* Bounded reference-store controller. Inputs are value's existing-root bindings
 * and admitted snapshot, plus observation's receiver/descriptor/output bindings.
 * All other controls start zero. Caller owns three live roots: receiver, desired
 * (logical referents fixed) and a distinct writable observed-value root. Admit
 * value through RootedLoadAdmission before starting; on a preparation RESTART,
 * recapture under that same registry lock then RESUME. Do not move/copy controls.
 *
 * The controller holds an additional private producer from its first HEAP step
 * through terminal cleanup, including value preparation and all clean retries.
 * next() uses rooted-load lock domains/statuses. No poll/transition is permitted
 * until DONE/CANCELLED/ERROR/NULL; cancel and drain first. FLUSH/WAIT/exhaustion
 * expose only guard-free retry points but retain producer admission. Buffer and
 * queue consumption/assistance remain the runtime caller's responsibility.
 *
 * Each physical CAS follows a fresh rooted observation. On contention the
 * observed root stays alive until replaced by the next journaled observation.
 * Successful CAS is never repeated, even when cancellation races with cleanup.
 * On abort before the physical access, it is skipped. Successful controls cannot
 * be reset. After CANCELLED and resumption of Running, REARM under the heap
 * lock clears obsolete observations and snapshots, retaining only root/input
 * bindings. Admit value again under the registry lock before the next HEAP.
 * Rearm neither polls nor acknowledges a handshake and never releases roots,
 * queued marks or transferred buffer tokens: their owners still hold them.
 */
#ifndef KLASSIC_GC_ROOTED_STORE_H
#define KLASSIC_GC_ROOTED_STORE_H
#include "gc_rooted_load.h"
#include "gc_rooted_store_attempt.h"
typedef struct {
    RootedLoader value;
    RootedObserver observe;
    RootedStoreAttempter attempt;
    RootAccessCleanup cleanup;
} RootedStoreOps;
enum { GC_RS_VALUE=0,GC_RS_OBSERVE=1,GC_RS_INIT=2,GC_RS_ATTEMPT=3,GC_RS_RETRY=4,
    GC_RS_COMPLETE=5,GC_RS_ABORTED=6,GC_RS_FAILED=7,GC_RS_NULL=8 };
static inline uint64_t gc_rooted_store_next(const RootedStore *q){
    if(!q||((uintptr_t)q&7)||q->stage>8||q->cancel>1||q->admitted>1)return GC_RLOAD_INVALID;
    if(q->stage>=5)return q->stage==5?GC_RLOAD_DONE:q->stage==6?GC_RLOAD_CANCELLED:q->stage==7?GC_RLOAD_ERROR:GC_RLOAD_NULL_RECEIVER;
    if(!q->stage)return gc_rooted_load_next(&q->value);
    if(q->stage==1){
        if(q->observation.stage>4)return GC_RLOAD_INVALID;
        if(q->observation.stage==1||q->observation.stage==2)return GC_RLOAD_ACCESS;
    }
    if(q->stage==3){
        if(q->attempt.stage>5)return GC_RLOAD_INVALID;
        if(q->attempt.stage==1||q->attempt.stage==2)return GC_RLOAD_ACCESS;
    }
    return GC_RLOAD_HEAP;
}
/* Only a cancelled, fully cleaned operation can cross a poll and restart.
 * Do not infer cleanup from the terminal stage alone: validate every local
 * access/lease, including nested preparation and failed-attempt ownership. */
static inline uint64_t gc_rooted_store_rearm(RootedStore *q){
    RootedLoad *v=&q->value;
    if(q->stage!=GC_RS_ABORTED||q->cancel!=1||q->admitted||v->admitted||v->fault||
        (v->stage!=GC_RLOAD_COMPLETE&&v->stage!=GC_RLOAD_ABORTED)||
        (v->marks.state!=0&&v->marks.state!=2&&v->marks.state!=3)||
        (q->attempt.stage!=0&&q->attempt.stage!=5)||
        (q->attempt.stage==5&&q->attempt.result==GC_STORE_OK))return GC_RLOAD_INVALID;
    uint64_t context=v->observation.receiver.follow.context;
    if(!context||(context&7)||q->observation.receiver.follow.context!=context)return GC_RLOAD_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
        atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RLOAD_INVALID;
    const RootAccess *accesses[]={&v->observation.receiver,&v->publication,&v->marks.jobs[0].access,
        &v->marks.jobs[1].access,&q->observation.receiver,&q->attempt.receiver};
    for(unsigned i=0;i<6;i++)if(!gc_rooted_load_clean_access(accesses[i]))return GC_RLOAD_INVALID;
    for(unsigned i=0;i<2;i++)if(q->attempt.entries[i].page||q->attempt.entries[i].incarnation||q->attempt.entries[i].token)return GC_RLOAD_INVALID;
    RootRequest desired=v->observation.output,receiver=q->observation.receiver.follow.root,observed=q->observation.output;
    desired.value=receiver.value=observed.value=0;
    RootedStore fresh={.value.observation={.receiver.follow={.context=context,.root=desired},.output=desired},
        .observation={.receiver.follow={.context=context,.root=receiver},.registry=q->observation.registry,
            .byte_offset=q->observation.byte_offset,.output=observed}};
    *q=fresh;return GC_RLOAD_HEAP;
}
static inline uint64_t gc_rooted_store_step_inner(MetadataLeasePool *pool,RootedStore *q,uint64_t op,const RootedStoreOps *ops){
    uint64_t action=gc_rooted_store_next(q);
    if(action==GC_RLOAD_INVALID||!ops||!ops->value||!ops->observe||!ops->attempt||!ops->cleanup)return GC_RLOAD_INVALID;
    if(op==GC_RLOAD_REARM&&q->stage==GC_RS_ABORTED)return gc_rooted_store_rearm(q);
    if(q->stage>=5)return action;
    if(op==GC_RLOAD_CANCEL){
        q->cancel=1;
        if(!q->stage)(void)ops->value(NULL,&q->value,GC_RLOAD_CANCEL);
        return gc_rooted_store_next(q);
    }
    if(op==GC_RLOAD_RESUME){
        if(q->stage||q->cancel)return GC_RLOAD_INVALID;
        return ops->value(pool,&q->value,op);
    }
    if(op!=action||op==GC_RLOAD_RESTART)return GC_RLOAD_INVALID;
    if(!q->admitted&&!q->cancel){
        if(q->stage||op!=GC_RLOAD_HEAP||q->value.stage||!q->value.admitted)return GC_RLOAD_INVALID;
        uint64_t context=q->value.observation.receiver.follow.context;
        if(!context||(context&7)||q->observation.receiver.follow.context!=context||
            q->observation.output.handle.cell==q->observation.receiver.follow.root.handle.cell||
            q->observation.output.handle.cell==q->value.observation.output.handle.cell)return GC_RLOAD_INVALID;
        MutatorContext *ctx=(void *)(uintptr_t)context;
        if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
            atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
            atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RLOAD_INVALID;
        uint64_t count=atomic_load_explicit(&ctx->private_producers,memory_order_acquire);
        if(!count)return GC_RLOAD_INVALID;
        if(count==UINT64_MAX)return GC_RLOAD_EXHAUSTED;
        atomic_store_explicit(&ctx->private_producers,count+1,memory_order_release);q->admitted=1;
    }
    if(!q->stage){
        q->last=ops->value(pool,&q->value,op);
        if(q->last==GC_RLOAD_DONE)q->stage=q->cancel?GC_RS_ABORTED:GC_RS_OBSERVE;
        else if(q->last==GC_RLOAD_CANCELLED)q->stage=GC_RS_ABORTED;
        else if(q->last==GC_RLOAD_ERROR||q->last==GC_RLOAD_NULL_RECEIVER)q->stage=GC_RS_FAILED;
        else return q->last;
        return gc_rooted_store_next(q);
    }
    if(q->stage==GC_RS_OBSERVE){
        RootedObservation *o=&q->observation;
        if(q->cancel&&op==GC_RLOAD_HEAP){
            if(gc_rooted_load_lease(&o->receiver)&&ops->cleanup(pool,&o->receiver,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
            q->stage=GC_RS_ABORTED;return GC_RLOAD_CANCELLED;
        }
        if(!o->stage&&gc_rooted_load_lease(&o->receiver)&&ops->cleanup(pool,&o->receiver,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
        uint64_t stage=o->stage;
        q->last=ops->observe(pool,o,stage==0?GC_RO_ENTER:stage==1?GC_RO_OBSERVE:stage==2?GC_RO_EXIT:GC_RO_RELEASE);
        if(q->last==GC_RO_COMPLETE){
            if(o->result==GC_ROOT_OK)q->stage=GC_RS_INIT;
            else if(o->result==GC_ROOT_FLUSH){
                if(ops->observe(pool,o,GC_RO_RESET)==GC_RF_DONE)return GC_RLOAD_FLUSH;
                q->stage=GC_RS_FAILED;
            }else q->stage=GC_RS_FAILED;
        }else if(!stage){
            if(q->last==GC_RF_DONE)q->stage=GC_RS_NULL;
            else{
                uint64_t retry=gc_rooted_load_retry(q->last);
                if(retry!=GC_RLOAD_INVALID)return retry;
                if(q->last!=GC_RAC_READY&&q->last!=GC_RF_CLEANUP_REQUIRED)q->stage=GC_RS_FAILED;
            }
        }
        return gc_rooted_store_next(q);
    }
    if(q->stage==GC_RS_INIT){
        if(q->cancel){q->stage=GC_RS_ABORTED;return GC_RLOAD_CANCELLED;}
        q->attempt=(RootedStoreAttempt){.receiver.follow={.context=q->observation.receiver.follow.context,.root=q->observation.receiver.follow.root},
            .marks=(uintptr_t)&q->value.marks,.commit={.registry=q->observation.registry,.byte_offset=q->observation.byte_offset,.expected=q->observation.output.value}};
        q->stage=GC_RS_ATTEMPT;return GC_RLOAD_HEAP;
    }
    if(q->stage==GC_RS_ATTEMPT){
        RootedStoreAttempt *a=&q->attempt;uint64_t stage=a->stage;
        if(!stage&&gc_rooted_load_lease(&a->receiver)){
            if(ops->attempt(pool,a,GC_STA_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
        }
        uint64_t child=(q->cancel&&stage<2)?GC_STA_ABORT:stage==0?GC_STA_ENTER:stage==1?GC_STA_ACCESS:stage==2?GC_STA_EXIT:GC_STA_RELEASE;
        q->last=ops->attempt(pool,a,child);
        if(q->last==GC_STA_COMPLETE){
            q->result=a->result;
            if(q->result==GC_STORE_OK)q->stage=GC_RS_COMPLETE;
            else if(q->cancel||q->result==GC_STA_ABORTED)q->stage=GC_RS_ABORTED;
            else if(q->result==GC_STORE_FULL||q->result==GC_STORE_EXHAUSTED||q->result==GC_STORE_RETRY||q->result==GC_STORE_REPREPARE||q->result==GC_STA_METADATA_FULL||q->result==GC_STA_METADATA_EXHAUSTED){
                q->stage=GC_RS_RETRY;
                return q->result==GC_STORE_FULL?GC_RLOAD_FLUSH:q->result==GC_STA_METADATA_FULL?GC_RLOAD_METADATA_FULL:
                    q->result==GC_STORE_EXHAUSTED||q->result==GC_STA_METADATA_EXHAUSTED?GC_RLOAD_EXHAUSTED:GC_RLOAD_HEAP;
            }else q->stage=GC_RS_FAILED;
        }else if(!stage){
            if(q->last==GC_RF_DONE)q->stage=GC_RS_NULL;
            else{
                uint64_t retry=gc_rooted_load_retry(q->last);
                if(retry!=GC_RLOAD_INVALID)return retry;
                if(q->last!=GC_RAC_READY&&q->last!=GC_STA_EXIT_REQUIRED&&q->last!=GC_RF_CLEANUP_REQUIRED)q->stage=GC_RS_FAILED;
            }
        }
        return gc_rooted_store_next(q);
    }
    if(q->cancel){q->stage=GC_RS_ABORTED;return GC_RLOAD_CANCELLED;}
    if(ops->observe(pool,&q->observation,GC_RO_RESET)!=GC_RF_DONE)return GC_RLOAD_INVALID;
    if(q->result==GC_STORE_REPREPARE){
        RootedLoad *v=&q->value;
        RootedLoad fresh={.observation={.receiver.follow={.context=v->observation.receiver.follow.context,.root=v->observation.output},.output=v->observation.output},
            .marks={.queues={v->marks.queues[0],v->marks.queues[1]},.epochs={v->marks.epochs[0],v->marks.epochs[1]},.phase=v->marks.phase}};
        *v=fresh;q->stage=GC_RS_VALUE;
    }else q->stage=GC_RS_OBSERVE;
    return GC_RLOAD_HEAP;
}
static inline uint64_t gc_rooted_store_step(MetadataLeasePool *pool,RootedStore *q,uint64_t op,const RootedStoreOps *ops){
    uint64_t r=gc_rooted_store_step_inner(pool,q,op,ops);
    if((r==GC_RLOAD_DONE||r==GC_RLOAD_CANCELLED||r==GC_RLOAD_ERROR||r==GC_RLOAD_NULL_RECEIVER)&&
        q&&((uintptr_t)q&7)==0&&q->stage>=GC_RS_COMPLETE&&q->stage<=GC_RS_NULL&&q->admitted==1){
        MutatorContext *ctx=(void *)(uintptr_t)q->value.observation.receiver.follow.context;
        uint64_t count=atomic_load_explicit(&ctx->private_producers,memory_order_acquire);
        if(!count)return GC_RLOAD_INVALID;
        atomic_store_explicit(&ctx->private_producers,count-1,memory_order_release);q->admitted=0;
    }
    return r;
}
typedef uint64_t (*RootedStorer)(MetadataLeasePool *,RootedStore *,uint64_t);
static inline uint64_t gc_rooted_store(MetadataLeasePool *pool,RootedStore *q,uint64_t op){
    const RootedStoreOps ops={gc_rooted_value,gc_rooted_observation,gc_rooted_store_attempt,gc_root_access_cleanup};
    return gc_rooted_store_step(pool,q,op,&ops);
}
#endif
