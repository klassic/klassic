/* Capture a load's phase and generation queues under the COMMON thread/root/
 * queue registry lock, before taking the heap lock. The context OS owner owns
 * the load and live strong roots; registry/participant/queues are retained by
 * the runtime. Only active generation queues are dereferenced. Admission holds
 * a counted private producer through subsequent load steps, so epoch retirement
 * cannot pass that owner's ACK before completion/cancellation. BEGIN may still
 * publish a new snapshot; finish/cancel private work before acknowledging it.
 *
 * Admit an untouched/clean-retry observation or a RESTART request. On success
 * call RESUME for RESTART, otherwise continue HEAP. WAIT/INVALID/EXHAUSTED are
 * nonmutating, including queue inputs. WAIT requires releasing this lock; an
 * already admitted owner must cancel before polling. No poll, allocation, root
 * read, heap access or scan occurs here. This does not start/finish a GC cycle.
 */
#ifndef KLASSIC_GC_ROOTED_LOAD_ADMIT_H
#define KLASSIC_GC_ROOTED_LOAD_ADMIT_H
#include "gc_rooted_load.h"
#include "gc_worker.h"
enum { GC_LA_OK=0,GC_LA_WAIT=1,GC_LA_INVALID=2,GC_LA_EXHAUSTED=3 };
typedef uint64_t (*RootedLoadAdmitter)(const RootedLoadAdmission *,RootedLoad *);
static inline uint64_t gc_rooted_load_admit_locked(const RootedLoadAdmission *a,RootedLoad *q){
    if(!a||((uintptr_t)a&7)||!q||((uintptr_t)q&7)||q->cancel||q->fault||q->admitted>1||q->marks.state||
        (q->stage!=GC_RLOAD_OBSERVING&&q->stage!=GC_RLOAD_RECAPTURE))return GC_LA_INVALID;
    if(q->stage==GC_RLOAD_OBSERVING?(q->observation.stage||q->observation.output.value):
        (q->observation.stage!=4||q->observation.result!=GC_ROOT_OK))return GC_LA_INVALID;
    if(q->observation.receiver.follow.root.value)return GC_LA_INVALID;
    const RootAccess *accesses[]={&q->observation.receiver,&q->publication,&q->marks.jobs[0].access,&q->marks.jobs[1].access};
    for(unsigned i=0;i<4;i++)if(!gc_rooted_load_clean_access(accesses[i]))return GC_LA_INVALID;
    uint64_t context=q->observation.receiver.follow.context;
    if(!context||(context&7)||!a->registry||(a->registry&7)||!a->handshake||(a->handshake&7)||
        !a->participant||(a->participant&7))return GC_LA_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)context;
    RootRegistry *registry=(void *)(uintptr_t)a->registry;
    HandshakeState *h=(void *)(uintptr_t)a->handshake;
    ThreadRegistration *p=(void *)(uintptr_t)a->participant;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
        atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||
        p->context!=context||p->registry!=a->handshake||p->registered!=1||p->identity!=ctx->thread_id||
        !h->members||!h->head||h->native>h->members||h->active>1||p->identity>h->last_identity||
        p->acknowledged_generation>h->generation||
        (h->active?(!h->generation||h->expected!=h->members-h->native||h->acknowledged>h->expected):
         (h->expected||h->acknowledged))||!gc_abi_valid_phase((PhaseSnapshot){registry->phase}))return GC_LA_INVALID;
    if(h->active||p->acknowledged_generation!=h->generation||
        atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=registry->phase)return GC_LA_WAIT;
    uint64_t queues[2]={0},epochs[2]={0};
    for(unsigned g=0;g<2;g++){
        uint64_t phase=(registry->phase>>(4*g))&15;
        if(phase>=GC_MARK_START&&phase<=GC_MARK_END_CHECK){
            if(!a->queues[g]||(a->queues[g]&7))return GC_LA_INVALID;
            MarkQueue *queue=(void *)(uintptr_t)a->queues[g];
            if(queue->generation!=g||!queue->epoch)return GC_LA_INVALID;
            queues[g]=a->queues[g];epochs[g]=queue->epoch;
        }
    }
    uint64_t count=atomic_load_explicit(&ctx->private_producers,memory_order_acquire);
    if(q->admitted){if(!count)return GC_LA_INVALID;}
    else if(count==UINT64_MAX)return GC_LA_EXHAUSTED;
    for(unsigned g=0;g<2;g++){q->marks.queues[g]=queues[g];q->marks.epochs[g]=epochs[g];}
    q->marks.phase=registry->phase;
    if(!q->admitted){atomic_store_explicit(&ctx->private_producers,count+1,memory_order_release);q->admitted=1;}
    return GC_LA_OK;
}
#endif
