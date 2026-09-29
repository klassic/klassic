/* Complete the required generation marks for ONE retained output root.
 * Caller captures queues/epochs/phase under cycle admission, owns the context,
 * root and counted producer obligation, and preserves these bindings until
 * CANCEL/RESET. Preserve the root's logical referent until using the completed
 * marks; forwarding/recoloring may change physical bits, but replacing the
 * referent requires new admission. No field is reread on restart. Queue epochs are checked by each
 * RootedMark OFFER under its queue lock, never read here under the heap lock.
 *
 * INIT/CANCEL/RESET use the heap lock; child ENTER/RELEASE use the heap lock,
 * ROUTE/EXIT run outside locks, OFFER uses current generation's queue lock.
 * READY/EXIT_REQUIRED forbid poll, lock acquisition and native transitions.
 * DONE means required mark admissions only, not load-barrier completion,
 * recoloring/healing, a root scan acknowledgement or collector termination.
 * Marks already admitted survive cancellation: the queues own those leases.
 * Native controls are unique, nonmoving and initially zero except inputs.
 */
#ifndef KLASSIC_GC_ROOTED_MARK_SET_H
#define KLASSIC_GC_ROOTED_MARK_SET_H
#include "gc_rooted_mark.h"
enum { GC_MS_INIT=7,GC_MS_CANCEL=8,GC_MS_RESET=9 };
enum { GC_MS_DONE=17,GC_MS_NEXT=18,GC_MS_RESTART=19,GC_MS_WAIT=20,GC_MS_INVALID=21,GC_MS_CANCELLED=22 };
static inline uint64_t gc_rooted_mark_set_step(MetadataLeasePool *pool,RootedMarkSet *q,uint64_t op,RootedMarker step){
    if(!q||((uintptr_t)q&7)||!step||op>GC_MS_RESET||op==GC_RM_CANCEL||op==GC_RM_RESET||q->state>3)
        return GC_MS_INVALID;
    if(op==GC_MS_RESET){
        if(q->state!=2&&q->state!=3)return GC_MS_INVALID;
        q->jobs[0]=(RootedMark){0};q->jobs[1]=(RootedMark){0};
        q->state=q->required=q->completed=q->current=0;return GC_RF_DONE;
    }
    if(!q->context||(q->context&7))return GC_MS_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(op==GC_MS_INIT){
        if(q->state||q->root.value||ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
            atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
            atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||!gc_abi_valid_phase((PhaseSnapshot){q->phase}))return GC_MS_INVALID;
        if(atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=q->phase)return GC_MS_RESTART;
        uint64_t required=0;
        for(unsigned generation=0;generation<2;generation++){
            uint64_t phase=(q->phase>>(generation*4))&15;
            if(phase>=GC_MARK_START&&phase<=GC_MARK_END_CHECK){
                if(!q->queues[generation]||(q->queues[generation]&7)||!q->epochs[generation])return GC_MS_INVALID;
                required|=UINT64_C(1)<<generation;
            }
        }
        for(unsigned generation=0;generation<2;generation++)q->jobs[generation]=(RootedMark){
            .access.follow={.context=q->context,.root=q->root},.queue=q->queues[generation],
            .generation=generation,.epoch=q->epochs[generation],.phase=q->phase};
        q->required=required;q->completed=0;q->current=required&2?1:0;q->state=required?1:2;
        return required?GC_MS_NEXT:GC_MS_DONE;
    }
    if(q->state==2){
        if(op==GC_MS_CANCEL){q->state=3;return GC_MS_CANCELLED;}
        return atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)==q->phase?GC_MS_DONE:GC_MS_RESTART;
    }
    if(q->state==3)return GC_MS_CANCELLED;
    if(q->state!=1||q->current>1)return GC_MS_INVALID;
    RootedMark *job=&q->jobs[q->current];
    if(op==GC_MS_CANCEL){
        uint64_t r=step(pool,job,GC_RM_CANCEL);
        if(r!=GC_RM_CANCELLED&&r!=GC_RM_COMPLETE)return r;
        q->state=3;return GC_MS_CANCELLED;
    }
    // Existing guards must always reach EXIT, even after invalid control input.
    // Clean pending work must be cancelled before adopting a new snapshot.
    if(job->stage!=1&&job->stage!=2&&op!=GC_RM_EXIT&&op!=GC_RM_RELEASE&&
       atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=q->phase)return GC_MS_RESTART;
    uint64_t r=step(pool,job,op);
    if(r==GC_RM_STALE)return GC_MS_RESTART;
    if(r!=GC_RM_COMPLETE)return r;
    if(job->route_result==GC_ROUTE_WAIT){
        (void)step(pool,job,GC_RM_RESET);return GC_MS_WAIT;
    }
    if(job->route_result!=GC_ROUTE_SKIP&&
       (job->route_result!=GC_ROUTE_READY||(job->offer_result!=GC_MARK_OK&&job->offer_result!=GC_MARK_DUPLICATE)))return GC_MS_INVALID;
    q->completed|=UINT64_C(1)<<q->current;
    if(atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=q->phase)return GC_MS_RESTART;
    if(q->completed==q->required){q->state=2;return GC_MS_DONE;}
    q->current=(q->required&~q->completed)&2?1:0;
    return GC_MS_NEXT;
}
typedef uint64_t (*RootedMarkSetter)(MetadataLeasePool *,RootedMarkSet *,uint64_t);
/* Public owner uses counted overflow to avoid waiting for a queue slot. */
static inline uint64_t gc_rooted_mark_set(MetadataLeasePool *pool,RootedMarkSet *q,uint64_t op){
    return gc_rooted_mark_set_step(pool,q,op,gc_rooted_mark_spill);
}
#endif
