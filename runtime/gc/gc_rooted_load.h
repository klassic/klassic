/* Finite owner for rooted field observation -> required generation marks ->
 * guarded result-root color publication -> exact receiver-field self-heal.
 * C and shared direct emitters implement
 * this internal controller. It is not yet compiler call lowering.
 *
 * Initialize only observation's input bindings and marks.queues/epochs/phase;
 * zero all other state. The caller uniquely owns these nonmoving controls and
 * both live roots, preserves their logical referents after observation, retains
 * metadata/descriptor lifetimes, and holds cycle admission responsibility.
 * Snapshot/queues are captured under that admission. The owner automatically
 * counts private work in context.private_producers on its first HEAP action,
 * including all clean retries until completion/error/cancellation. Only the
 * context OS owner updates that count, permitting nested native controls.
 * Cancel and finish cleanup before polling for a phase/native transition. Only in
 * RESTART may marks.queues/epochs/phase be recaptured, with the SAME output root.
 * No receiver field is reread after successful observation, including restart.
 *
 * next() specifies the next lock domain. HEAP uses the heap/lease lock; QUEUE
 * uses marks.current's queue lock; ACCESS runs outside locks with no poll, wait,
 * native transition or lock acquisition. Every step is bounded. CANCEL only
 * records intent, takes no lock and does not abandon an active guard. Continue
 * next() until CANCELLED. An in-flight observation may finish while cancelling;
 * caller roots remain owned. EXIT/release failures retain exact obligations.
 * After cancellation, REARM uses the heap lock and requires Running again.
 * Keep the same root bindings/referents throughout; REARM requests RESTART
 * for a captured value, or retries observation when no successful read exists.
 * WAIT/FLUSH/METADATA_FULL/EXHAUSTED are clean retry events. In particular, a
 * field-read FLUSH is returned only after releasing the receiver guard/lease.
 *
 * DONE returns an owned root, never a raw address or a durable color receipt.
 * Any later payload use must enter through RootAccess again. This controller
 * reacquires the receiver and attempts exact CAS with the original field token.
 * A competing field update wins without replacing the retained load result.
 * This controller does not acknowledge a handshake, release roots,
 * discharge queued/buffered work, or certify retirement/termination. It releases
 * only its private producer count. A clean retry still retains that count.
 */
#ifndef KLASSIC_GC_ROOTED_LOAD_H
#define KLASSIC_GC_ROOTED_LOAD_H
#include "gc_rooted_observation.h"
#include "gc_rooted_mark_publish.h"
#include "gc_rooted_load_heal.h"
typedef struct {
    RootedObserver observe;
    RootedMarkSetter mark;
    uint64_t (*enter)(MetadataLeasePool *,RootAccess *);
    RootedMarkPublisher publish;
    RootAccessCleanup cleanup;
    RootedLoadHealer heal;
} RootedLoadOps;
enum { GC_RLOAD_HEAP=32,GC_RLOAD_ACCESS=33,GC_RLOAD_QUEUE=34,GC_RLOAD_DONE=35,
    GC_RLOAD_CANCELLED=36,GC_RLOAD_RESTART=37,GC_RLOAD_WAIT=38,GC_RLOAD_FLUSH=39,
    GC_RLOAD_METADATA_FULL=40,GC_RLOAD_EXHAUSTED=41,GC_RLOAD_ERROR=42,
    GC_RLOAD_NULL_RECEIVER=43,GC_RLOAD_INVALID=44,GC_RLOAD_CANCEL=45,GC_RLOAD_RESUME=46,GC_RLOAD_REARM=47 };
enum { GC_RLOAD_OBSERVING=0,GC_RLOAD_MARK_INIT=1,GC_RLOAD_MARKING=2,
    GC_RLOAD_ROOT_ENTER=3,GC_RLOAD_ROOT_PUBLISH=4,GC_RLOAD_ROOT_EXIT=5,
    GC_RLOAD_ROOT_RELEASE=6,GC_RLOAD_RESTART_CLEAN=7,GC_RLOAD_RECAPTURE=8,
    GC_RLOAD_COMPLETE=9,GC_RLOAD_ABORTED=10,GC_RLOAD_FAILED=11,GC_RLOAD_NULL=12,GC_RLOAD_HEAL_ENTER=13,GC_RLOAD_HEAL=14,GC_RLOAD_HEAL_EXIT=15,GC_RLOAD_HEAL_RELEASE=16 };
static inline bool gc_rooted_load_lease(const RootAccess *a){
    return a->follow.cleanup.page||a->follow.cleanup.incarnation||a->follow.cleanup.token;
}
static inline uint64_t gc_rooted_load_next(const RootedLoad *q){
    if(!q||((uintptr_t)q&7)||q->stage>GC_RLOAD_HEAL_RELEASE||q->cancel>1||q->fault>1||q->admitted>1)return GC_RLOAD_INVALID;
    if(q->stage==GC_RLOAD_COMPLETE)return GC_RLOAD_DONE;
    if(q->stage==GC_RLOAD_ABORTED)return GC_RLOAD_CANCELLED;
    if(q->stage==GC_RLOAD_FAILED)return GC_RLOAD_ERROR;
    if(q->stage==GC_RLOAD_NULL)return GC_RLOAD_NULL_RECEIVER;
    uint64_t action=GC_RLOAD_HEAP;
    if(q->stage==GC_RLOAD_OBSERVING){
        if(q->observation.stage>4)return GC_RLOAD_INVALID;
        if(q->observation.stage==1||q->observation.stage==2)action=GC_RLOAD_ACCESS;
    }else if(q->stage==GC_RLOAD_MARKING){
        if(q->marks.current>1)return GC_RLOAD_INVALID;
        uint64_t stage=q->marks.jobs[q->marks.current].stage;
        if(stage==1||stage==2)action=GC_RLOAD_ACCESS;
        else if(stage==3)action=GC_RLOAD_QUEUE;
    }else if(q->stage==GC_RLOAD_ROOT_PUBLISH||q->stage==GC_RLOAD_ROOT_EXIT||q->stage==GC_RLOAD_HEAL||q->stage==GC_RLOAD_HEAL_EXIT)action=GC_RLOAD_ACCESS;
    else if(q->stage==GC_RLOAD_RECAPTURE&&!q->cancel)return GC_RLOAD_RESTART;
    return q->cancel&&action!=GC_RLOAD_ACCESS?GC_RLOAD_HEAP:action;
}
static inline uint64_t gc_rooted_load_retry(uint64_t r){
    return r==GC_RF_WAIT?GC_RLOAD_WAIT:r==GC_RF_FLUSH?GC_RLOAD_FLUSH:
        r==GC_RF_METADATA_FULL?GC_RLOAD_METADATA_FULL:r==GC_RF_EXHAUSTED?GC_RLOAD_EXHAUSTED:
        r==GC_RF_ADVANCED?GC_RLOAD_HEAP:GC_RLOAD_INVALID;
}
/* Rearm after cancellation, under the heap lock and only after the worker
 * has resumed Running. It does not ACK or discharge producer obligations.
 * Captured observations request fresh cycle admission without rereading the
 * receiver. An uncompleted/failed observation starts a fresh read instead. */
static inline bool gc_rooted_load_clean_access(const RootAccess *a){
    const ResolveRequest *r=&a->access;
    return !gc_rooted_load_lease(a)&&!r->reference&&!r->destination&&
        !r->scope.context&&!r->scope.source_offset&&!r->scope.incarnation&&!r->scope.held&&!r->scope.ticket;
}
static inline uint64_t gc_rooted_load_rearm(MetadataLeasePool *pool,RootedLoad *q,const RootedLoadOps *ops){
    if(q->stage!=GC_RLOAD_ABORTED||q->cancel!=1||q->fault||q->admitted||
        (q->observation.stage!=0&&q->observation.stage!=3&&q->observation.stage!=4)||
        (q->marks.state!=0&&q->marks.state!=3))return GC_RLOAD_INVALID;
    uint64_t context=q->observation.receiver.follow.context;
    if(!context||(context&7))return GC_RLOAD_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
        atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RLOAD_INVALID;
    const RootAccess *accesses[]={&q->observation.receiver,&q->publication,&q->marks.jobs[0].access,&q->marks.jobs[1].access};
    for(unsigned i=0;i<4;i++)if(!gc_rooted_load_clean_access(accesses[i]))return GC_RLOAD_INVALID;
    if(q->marks.state==3){
        uint64_t r=ops->mark(pool,&q->marks,GC_MS_RESET);
        if(r!=GC_RF_DONE)return GC_RLOAD_INVALID;
    }
    bool captured=q->observation.stage>=3&&q->observation.result==GC_ROOT_OK;
    q->publication=(RootAccess){0};q->cancel=q->last=q->retry_event=0;
    if(captured){q->observation.stage=4;q->stage=GC_RLOAD_RECAPTURE;return GC_RLOAD_RESTART;}
    q->observation.output.value=0;q->observation.stage=q->observation.result=0;
    q->stage=GC_RLOAD_OBSERVING;return GC_RLOAD_HEAP;
}
static inline uint64_t gc_rooted_load_step_inner(MetadataLeasePool *pool,RootedLoad *q,uint64_t op,const RootedLoadOps *ops,bool root_value){
    uint64_t action=gc_rooted_load_next(q);
    if(action==GC_RLOAD_INVALID||!ops||!ops->observe||!ops->mark||!ops->enter||!ops->publish||!ops->cleanup||!ops->heal)return GC_RLOAD_INVALID;
    if(root_value){
        const RootedObservation *o=&q->observation;
        if(q->stage>=GC_RLOAD_HEAL_ENTER||o->registry||o->byte_offset||o->output.value||o->result||
            (o->stage!=0&&o->stage!=4)||!gc_rooted_load_clean_access(&o->receiver)||
            o->receiver.follow.root.segment!=o->output.segment||
            o->receiver.follow.root.handle.cell!=o->output.handle.cell||
            o->receiver.follow.root.handle.reuse_generation!=o->output.handle.reuse_generation||
            o->receiver.follow.root.handle.scope!=o->output.handle.scope||o->receiver.follow.root.value)return GC_RLOAD_INVALID;
    }
    if(op==GC_RLOAD_REARM)return gc_rooted_load_rearm(pool,q,ops);
    if(action==GC_RLOAD_DONE||action==GC_RLOAD_CANCELLED||action==GC_RLOAD_ERROR||action==GC_RLOAD_NULL_RECEIVER)return action;
    if(op==GC_RLOAD_CANCEL){q->cancel=1;return gc_rooted_load_next(q);}
    if(op==GC_RLOAD_RESUME){
        if(q->stage!=GC_RLOAD_RECAPTURE||q->cancel)return GC_RLOAD_INVALID;
        q->stage=GC_RLOAD_MARK_INIT;return GC_RLOAD_HEAP;
    }
    if(op!=action||op==GC_RLOAD_RESTART)return GC_RLOAD_INVALID;
    if(!q->admitted&&!q->cancel&&(q->stage==GC_RLOAD_OBSERVING||q->stage==GC_RLOAD_MARK_INIT)&&op==GC_RLOAD_HEAP){
        uint64_t context=q->observation.receiver.follow.context;
        if(!context||(context&7))return GC_RLOAD_INVALID;
        MutatorContext *ctx=(void *)(uintptr_t)context;
        if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
            atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
            atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RLOAD_INVALID;
        uint64_t count=atomic_load_explicit(&ctx->private_producers,memory_order_acquire);
        if(count==UINT64_MAX)return GC_RLOAD_EXHAUSTED;
        // Only this context's OS owner may change its private producer count.
        atomic_store_explicit(&ctx->private_producers,count+1,memory_order_release);q->admitted=1;
    }
    if(q->cancel&&op==GC_RLOAD_HEAP){
        RootAccess *a=(q->stage==GC_RLOAD_OBSERVING||q->stage>=GC_RLOAD_HEAL_ENTER)?&q->observation.receiver:&q->publication;
        if(gc_rooted_load_lease(a)&&ops->cleanup(pool,a,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
        if(q->marks.state==1||q->marks.state==2){
            q->last=ops->mark(pool,&q->marks,GC_MS_CANCEL);
            if(q->last!=GC_MS_CANCELLED)return GC_RLOAD_HEAP;
        }
        q->stage=q->fault?GC_RLOAD_FAILED:GC_RLOAD_ABORTED;return gc_rooted_load_next(q);
    }
    if(q->stage==GC_RLOAD_OBSERVING){
        if(root_value){
            // The input already owns the logical value. No field is observed.
            q->observation.stage=4;q->stage=GC_RLOAD_MARK_INIT;return GC_RLOAD_HEAP;
        }
        uint64_t stage=q->observation.stage;
        if(!stage&&gc_rooted_load_lease(&q->observation.receiver)){
            if(ops->cleanup(pool,&q->observation.receiver,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
        }
        uint64_t child=stage==0?GC_RO_ENTER:stage==1?GC_RO_OBSERVE:stage==2?GC_RO_EXIT:GC_RO_RELEASE;
        q->last=ops->observe(pool,&q->observation,child);
        if(q->last==GC_RO_COMPLETE){
            if(q->observation.result==GC_ROOT_OK){q->stage=GC_RLOAD_MARK_INIT;return GC_RLOAD_HEAP;}
            if(q->observation.result==GC_ROOT_FLUSH){
                q->last=ops->observe(pool,&q->observation,GC_RO_RESET);
                if(q->last==GC_RF_DONE)return GC_RLOAD_FLUSH;
            }
            q->stage=GC_RLOAD_FAILED;return GC_RLOAD_ERROR;
        }
        if(!stage){
            if(q->last==GC_RF_DONE){q->stage=GC_RLOAD_NULL;return GC_RLOAD_NULL_RECEIVER;}
            uint64_t retry=gc_rooted_load_retry(q->last);
            if(retry!=GC_RLOAD_INVALID)return retry;
            if(q->last==GC_RF_CLEANUP_REQUIRED)return GC_RLOAD_HEAP;
            if(q->last!=GC_RAC_READY){q->stage=GC_RLOAD_FAILED;return GC_RLOAD_ERROR;}
        }
        return gc_rooted_load_next(q);
    }
    if(q->stage==GC_RLOAD_MARK_INIT){
        q->marks.context=q->observation.receiver.follow.context;
        q->marks.root=q->observation.output;q->marks.root.value=0;
        q->last=ops->mark(pool,&q->marks,GC_MS_INIT);
        if(q->last==GC_MS_NEXT)q->stage=GC_RLOAD_MARKING;
        else if(q->last==GC_MS_DONE)q->stage=GC_RLOAD_ROOT_ENTER;
        else if(q->last==GC_MS_RESTART)q->stage=GC_RLOAD_RECAPTURE;
        else q->stage=GC_RLOAD_FAILED;
        return gc_rooted_load_next(q);
    }
    if(q->stage==GC_RLOAD_MARKING){
        RootedMark *job=&q->marks.jobs[q->marks.current];
        if(!job->stage&&gc_rooted_load_lease(&job->access)){
            q->stage=GC_RLOAD_RESTART_CLEAN;return GC_RLOAD_HEAP;
        }
        uint64_t child=job->stage==0?GC_RM_ENTER:job->stage==1?GC_RM_ROUTE:
            job->stage==2?GC_RM_EXIT:job->stage==3?GC_RM_OFFER:GC_RM_RELEASE;
        q->last=ops->mark(pool,&q->marks,child);
        if(q->last==GC_MS_DONE)q->stage=GC_RLOAD_ROOT_ENTER;
        else if(q->last==GC_MS_RESTART||q->last==GC_RF_CLEANUP_REQUIRED)q->stage=GC_RLOAD_RESTART_CLEAN;
        else if(q->last==GC_MS_WAIT)return GC_RLOAD_WAIT;
        else if(q->last==GC_RM_FULL||q->last==GC_RM_EXHAUSTED){
            // Pending offers retain a lease. Cancel before exposing a retry.
            q->retry_event=q->last==GC_RM_FULL?GC_RLOAD_WAIT:GC_RLOAD_EXHAUSTED;
            q->stage=GC_RLOAD_RESTART_CLEAN;
        }else if(q->last==GC_MS_INVALID||q->last==GC_RF_INVALID){
            q->fault=1;q->cancel=1;return gc_rooted_load_next(q);
        }else{
            uint64_t retry=gc_rooted_load_retry(q->last);
            if(retry!=GC_RLOAD_INVALID)return retry;
        }
        return gc_rooted_load_next(q);
    }
    if(q->stage==GC_RLOAD_RESTART_CLEAN){
        if(q->marks.state==1||q->marks.state==2){
            q->last=ops->mark(pool,&q->marks,GC_MS_CANCEL);
            if(q->last!=GC_MS_CANCELLED)return GC_RLOAD_HEAP;
        }
        if(q->marks.state==3){
            q->last=ops->mark(pool,&q->marks,GC_MS_RESET);
            if(q->last!=GC_RF_DONE)return GC_RLOAD_HEAP;
        }
        q->stage=GC_RLOAD_RECAPTURE;
        uint64_t event=q->retry_event;q->retry_event=0;return event?event:GC_RLOAD_RESTART;
    }
    if(q->stage==GC_RLOAD_ROOT_ENTER){
        if(gc_rooted_load_lease(&q->publication)){
            if(ops->cleanup(pool,&q->publication,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
        }
        MutatorContext *ctx=(void *)(uintptr_t)q->marks.context;
        if(atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=q->marks.phase){
            q->stage=GC_RLOAD_RESTART_CLEAN;return GC_RLOAD_HEAP;
        }
        q->publication.follow.context=q->marks.context;q->publication.follow.root=q->marks.root;
        q->last=ops->enter(pool,&q->publication);
        if(q->last==GC_RAC_READY)q->stage=GC_RLOAD_ROOT_PUBLISH;
        else if(q->last==GC_RF_DONE)q->stage=GC_RLOAD_COMPLETE;
        else if(q->last!=GC_RF_CLEANUP_REQUIRED){
            uint64_t retry=gc_rooted_load_retry(q->last);
            if(retry!=GC_RLOAD_INVALID)return retry;
            q->stage=GC_RLOAD_FAILED;
        }
        return gc_rooted_load_next(q);
    }
    if(q->stage==GC_RLOAD_ROOT_PUBLISH){
        if(!q->cancel)q->last=ops->publish(&q->marks,&q->publication);
        q->stage=GC_RLOAD_ROOT_EXIT;return GC_RLOAD_ACCESS;
    }
    if(q->stage==GC_RLOAD_ROOT_EXIT){
        if(ops->cleanup(NULL,&q->publication,GC_RAC_EXIT)!=GC_RF_DONE)return GC_RLOAD_ACCESS;
        q->stage=GC_RLOAD_ROOT_RELEASE;return GC_RLOAD_HEAP;
    }
    if(q->stage==GC_RLOAD_ROOT_RELEASE){
        if(ops->cleanup(pool,&q->publication,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
        MutatorContext *ctx=(void *)(uintptr_t)q->marks.context;
        if(atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=q->marks.phase)q->stage=GC_RLOAD_RESTART_CLEAN;
        else q->stage=q->last==GC_HEAL_OK?(root_value?GC_RLOAD_COMPLETE:GC_RLOAD_HEAL_ENTER):q->last==GC_HEAL_CHANGED?GC_RLOAD_ROOT_ENTER:GC_RLOAD_FAILED;
        return gc_rooted_load_next(q);
    }
    if(q->stage==GC_RLOAD_HEAL_ENTER){
        RootAccess *a=&q->observation.receiver;
        if(gc_rooted_load_lease(a)&&ops->cleanup(pool,a,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
        MutatorContext *ctx=(void *)(uintptr_t)q->marks.context;
        if(atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=q->marks.phase){q->stage=GC_RLOAD_RESTART_CLEAN;return GC_RLOAD_HEAP;}
        q->last=ops->enter(pool,a);
        if(q->last==GC_RAC_READY)q->stage=GC_RLOAD_HEAL;
        else if(q->last!=GC_RF_CLEANUP_REQUIRED){
            uint64_t retry=gc_rooted_load_retry(q->last);
            if(retry!=GC_RLOAD_INVALID)return retry;
            q->stage=GC_RLOAD_FAILED;
        }
        return gc_rooted_load_next(q);
    }
    if(q->stage==GC_RLOAD_HEAL){
        if(!q->cancel)q->last=ops->heal(q);
        q->stage=GC_RLOAD_HEAL_EXIT;return GC_RLOAD_ACCESS;
    }
    if(q->stage==GC_RLOAD_HEAL_EXIT){
        if(ops->cleanup(NULL,&q->observation.receiver,GC_RAC_EXIT)!=GC_RF_DONE)return GC_RLOAD_ACCESS;
        q->stage=GC_RLOAD_HEAL_RELEASE;return GC_RLOAD_HEAP;
    }
    if(q->stage==GC_RLOAD_HEAL_RELEASE){
        if(ops->cleanup(pool,&q->observation.receiver,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RLOAD_HEAP;
        MutatorContext *ctx=(void *)(uintptr_t)q->marks.context;
        if(atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=q->marks.phase)q->stage=GC_RLOAD_RESTART_CLEAN;
        else q->stage=q->last==GC_HEAL_OK||q->last==GC_HEAL_CHANGED?GC_RLOAD_COMPLETE:
            q->last==GC_LOAD_HEAL_RETRY?GC_RLOAD_ROOT_ENTER:GC_RLOAD_FAILED;
        return gc_rooted_load_next(q);
    }
    return GC_RLOAD_INVALID;
}
static inline uint64_t gc_rooted_load_step_mode(MetadataLeasePool *pool,RootedLoad *q,uint64_t op,const RootedLoadOps *ops,bool root_value){
    uint64_t r=gc_rooted_load_step_inner(pool,q,op,ops,root_value);
    if((r==GC_RLOAD_DONE||r==GC_RLOAD_CANCELLED||r==GC_RLOAD_ERROR||r==GC_RLOAD_NULL_RECEIVER)&&q->admitted){
        MutatorContext *ctx=(void *)(uintptr_t)q->observation.receiver.follow.context;
        uint64_t count=atomic_load_explicit(&ctx->private_producers,memory_order_acquire);
        if(!count)return GC_RLOAD_INVALID; // retain the obligation on corrupt accounting
        atomic_store_explicit(&ctx->private_producers,count-1,memory_order_release);q->admitted=0;
    }
    return r;
}
static inline uint64_t gc_rooted_load_step(MetadataLeasePool *pool,RootedLoad *q,uint64_t op,const RootedLoadOps *ops){
    return gc_rooted_load_step_mode(pool,q,op,ops,false);
}
typedef uint64_t (*RootedLoader)(MetadataLeasePool *,RootedLoad *,uint64_t);
static inline uint64_t gc_rooted_load(MetadataLeasePool *pool,RootedLoad *q,uint64_t op){
    const RootedLoadOps ops={gc_rooted_observation,gc_rooted_mark_set,gc_root_access_locked,gc_rooted_mark_publish,gc_root_access_cleanup,gc_rooted_load_heal};
    return gc_rooted_load_step(pool,q,op,&ops);
}
/* Prepare an existing strong root for a later reference operation. Same unique
 * control, statuses, admission, cancellation/rearm and lock domains as LOAD.
 * Initialize receiver.follow.context, receiver.follow.root and output to the
 * SAME retained root (value scratch zero); registry/byte_offset remain zero.
 * The observation subcontrol is input binding storage: no field is read/healed.
 * Never switch entry points while a request is live. Keep the logical referent
 * fixed through completion/restart. DONE owns a canonical mark/remap-published
 * root, NOT a raw reference or permanent store-good certificate. Remembered
 * bits are preserved. A subsequent store must retain its own phase admission,
 * revalidate/canonicalize as needed, compose store colors and perform logging.
 * Completion/cancel releases this preparation's private count only.
 */
static inline uint64_t gc_rooted_value(MetadataLeasePool *pool,RootedLoad *q,uint64_t op){
    const RootedLoadOps ops={gc_rooted_observation,gc_rooted_mark_set,gc_root_access_locked,gc_rooted_mark_publish,gc_root_access_cleanup,gc_rooted_load_heal};
    return gc_rooted_load_step_mode(pool,q,op,&ops,true);
}
#endif
