/* One registered context's SATB/remset quiescence, under the registry lock.
 * This is an instantaneous prerequisite, not a retained retirement receipt.
 * Caller owns live h/p/context storage for the call. It must keep producers
 * stopped, exclude new claims and preserve registration until consuming DONE.
 * No pointers, reference bits or scanner borrows escape this function.
 *
 * Running owners return WAIT before any producer-private field is read.
 * Native state, or an acknowledged active handshake, supplies the owner
 * handoff; the registry lock prevents reentry/resume during inspection.
 * An active consumer returns WAIT before reading its COPY-owned cursor.
 * Registered jobs must be used exclusively: raw drains can leave invisible
 * worker snapshots after advancing consumed and are not covered here.
 *
 * Both rings must be completely drained, including unpublished entries.
 * This conservative check is independent of a source range. It does not
 * certify other contexts, roots, queues, native handles, heap incoming edges,
 * old colors or earlier payload-uncommit authorization. No whole-registry
 * walk or buffer scanning is performed during a handshake.
 */
#ifndef KLASSIC_GC_NATIVE_BUFFER_QUIESCENCE_H
#define KLASSIC_GC_NATIVE_BUFFER_QUIESCENCE_H
#include "gc_handshake.h"
#include "gc_buffers.h"
enum { GC_NBQ_DONE=0,GC_NBQ_WAIT=1,GC_NBQ_INVALID=2 };
static inline uint64_t gc_native_buffer_quiescence_locked(
        const HandshakeState *h,const ThreadRegistration *p){
    if(!h||!p||((uintptr_t)h&7)||((uintptr_t)p&7)||
       !h->members||!h->head||h->native>h->members||h->active>1||
       p->registered!=1||p->registry!=(uintptr_t)h||!p->identity||
       p->identity>h->last_identity||!p->context||(p->context&7)||
       p->acknowledged_generation>h->generation)return GC_NBQ_INVALID;
    if(h->active?(!h->generation||h->expected!=h->members-h->native||
                  h->acknowledged>h->expected):(h->expected||h->acknowledged))return GC_NBQ_INVALID;
    const MutatorContext *ctx=(const void *)(uintptr_t)p->context;
    if(ctx->abi_version!=GC_ABI_VERSION||ctx->thread_id!=p->identity)return GC_NBQ_INVALID;
    uint64_t state=atomic_load_explicit(&ctx->state,memory_order_acquire);
    if(state==GC_RUNNING||state==GC_DETACHING)return GC_NBQ_WAIT;
    if(state!=GC_BLOCKED_NATIVE&&state!=GC_AT_SAFEPOINT)return GC_NBQ_INVALID;
    if(state==GC_BLOCKED_NATIVE&&!h->native)return GC_NBQ_INVALID;
    if(state==GC_AT_SAFEPOINT&&(!h->active||p->acknowledged_generation!=h->generation))return GC_NBQ_WAIT;
    if(atomic_load_explicit(&ctx->private_producers,memory_order_acquire)||
       atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_NBQ_WAIT;
    const BufferCursor *buffers[]={&ctx->satb,&ctx->remembered};
    const BufferConsumer *consumers[]={&ctx->satb_consumer,&ctx->remembered_consumer};
    for(unsigned i=0;i<2;i++){
        const BufferConsumer *c=consumers[i];
        if(c->serial)return GC_NBQ_WAIT; /* COPY may concurrently write cursor. */
        if(c->end||c->cursor)return GC_NBQ_INVALID;
        const BufferCursor *b=buffers[i];
        if(!gc_barrier_buffer_valid(b))return GC_NBQ_INVALID;
        uint64_t top=b->top;
        uint64_t published=atomic_load_explicit(&b->published,memory_order_acquire);
        uint64_t consumed=atomic_load_explicit(&b->consumed,memory_order_acquire);
        if(consumed>published||published>top||top-consumed>b->capacity)return GC_NBQ_INVALID;
        if(consumed!=published||published!=top)return GC_NBQ_WAIT;
    }
    return GC_NBQ_DONE;
}
#endif
