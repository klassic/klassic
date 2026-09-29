/* Registered SATB/remset consumer jobs. CLAIM and ACK use the SAME lock as
 * thread registration/detach; COPY runs outside it, bounded by 256 entries.
 * Each context embeds two unique controls. Active claims and the unconsumed
 * ring prefix retain context/storage and phase/reference/metadata obligations.
 * Never mix this consumer with raw gc_barrier_buffer_drain. Buffer storage and
 * geometry stay fixed while a claim exists; one owner operates each job.
 * ACK certifies complete phase-specific processing and token transfer/release.
 * No heap access, waiting, polling or allocation occurs in these steps. */
#ifndef KLASSIC_GC_BUFFER_JOB_H
#define KLASSIC_GC_BUFFER_JOB_H
#include "gc_buffers.h"
#define GC_BJ_CLAIM 0u
#define GC_BJ_COPY 1u
#define GC_BJ_ACK 2u
#define GC_BJ_OK 0u
#define GC_BJ_INVALID 1u
#define GC_BJ_EMPTY 2u
#define GC_BJ_WAIT 3u
#define GC_BJ_EXHAUSTED 4u
static inline uint64_t gc_buffer_job_step(MutatorContext *ctx,BarrierJob *q,uint64_t op){
    if(op>GC_BJ_ACK||ctx->abi_version!=GC_ABI_VERSION||!q->identity||q->identity!=ctx->thread_id||q->selector>1||
        atomic_load_explicit(&ctx->state,memory_order_acquire)>GC_DETACHING)return GC_BJ_INVALID;
    BufferCursor *b=q->selector?&ctx->remembered:&ctx->satb;
    BufferConsumer *c=q->selector?&ctx->remembered_consumer:&ctx->satb_consumer;
    if(!gc_barrier_buffer_valid(b))return GC_BJ_INVALID;
    uint64_t begin=atomic_load_explicit(&b->consumed,memory_order_acquire);
    uint64_t published=atomic_load_explicit(&b->published,memory_order_acquire);
    if(begin>published||published-begin>b->capacity)return GC_BJ_INVALID;
    if(op==GC_BJ_CLAIM){
        if(c->serial)return GC_BJ_WAIT;
        if(c->end||c->cursor||!q->capacity||q->capacity>256)return GC_BJ_INVALID;
        if(begin==published)return GC_BJ_EMPTY;
        if(c->last_serial>=UINT64_MAX/2)return GC_BJ_EXHAUSTED;
        uint64_t n=published-begin;if(n>q->capacity)n=q->capacity;
        c->serial=(++c->last_serial)*2+q->selector;c->end=begin+n;c->cursor=begin;
        q->serial=c->serial;q->begin=begin;q->end=c->end;q->written=0;
        return GC_BJ_OK;
    }
    if(!q->serial||q->serial!=c->serial||!c->last_serial||c->last_serial>UINT64_MAX/2||
        c->serial!=c->last_serial*2+q->selector||q->begin!=begin||q->end!=c->end||
        q->end<=begin||q->end>published||q->end-begin>256)return GC_BJ_INVALID;
    uint64_t n=q->end-begin;
    if(op==GC_BJ_COPY){
        if(c->cursor!=begin||q->written||!q->output||(q->output&7)||q->capacity<n||q->capacity>256||
            q->output>UINT64_MAX-q->capacity*sizeof(BarrierEntry))return GC_BJ_INVALID;
        BarrierEntry *entries=(BarrierEntry *)(uintptr_t)b->entries,*out=(BarrierEntry *)(uintptr_t)q->output;
        for(uint64_t i=0;i<n;i++)out[i]=entries[(begin+i)%b->capacity];
        q->written=n;c->cursor=q->end;return GC_BJ_OK;
    }
    if(c->cursor!=q->end||q->written!=n)return GC_BJ_WAIT;
    atomic_store_explicit(&b->consumed,q->end,memory_order_release);
    c->serial=c->end=c->cursor=0;return GC_BJ_OK;
}
#endif
