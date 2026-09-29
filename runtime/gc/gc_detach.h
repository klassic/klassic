/* One bounded owner step under the SAME root/thread registry lock. The caller
 * has entered Detaching, owns the context, and keeps all control storage live.
 * MORE means notify/retry after dropping the lock; WAIT means service/wait for
 * collector progress outside it. Action returns require the indicated outside-
 * lock operation (TLAB close uses the heap lock). Never treat an action as DONE.
 * No step scans roots, processes a journal or waits while holding the lock.
 */
#ifndef KLASSIC_GC_DETACH_H
#define KLASSIC_GC_DETACH_H
#include "gc_handshake.h"
#include "gc_root_lifetime.h"
#define GC_DT_DONE 0u
#define GC_DT_MORE 1u
#define GC_DT_WAIT 2u
#define GC_DT_INVALID 3u
#define GC_DT_EXHAUSTED 4u
#define GC_DT_CLOSE_TLAB 5u
#define GC_DT_FLUSH_SATB 6u
#define GC_DT_FLUSH_REMEMBERED 7u
#define GC_DT_ADOPT_PHASE 8u
static inline uint64_t gc_detach_step_locked(const DetachRequest *q) {
    if (!q->roots || (q->roots&7) || !q->handshake || (q->handshake&7) ||
        !q->participant || (q->participant&7)) return GC_DT_INVALID;
    RootRegistry *g=(RootRegistry *)(uintptr_t)q->roots;
    HandshakeState *h=(HandshakeState *)(uintptr_t)q->handshake;
    ThreadRegistration *p=(ThreadRegistration *)(uintptr_t)q->participant;
    if (!p->context || (p->context&7)) return GC_DT_INVALID;
    MutatorContext *ctx=(MutatorContext *)(uintptr_t)p->context;
    if (atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_DETACHING) return GC_DT_INVALID;
    if (h->active) {
        uint64_t result=gc_handshake_step_locked(h,p,GC_HS_ACK);
        return result==GC_HS_OK?GC_DT_MORE:result==GC_HS_DUPLICATE?GC_DT_WAIT:GC_DT_INVALID;
    }
    // RESUME validates membership, guard, ABI, counters and request generation;
    // Detaching stays Detaching, without changing native ownership.
    if (gc_handshake_step_locked(h,p,GC_HS_RESUME)!=GC_HS_OK) return GC_DT_INVALID;
    if (ctx->tlab_start || ctx->tlab_top || ctx->tlab_end) return GC_DT_CLOSE_TLAB;
    BufferCursor *buffers[]={&ctx->satb,&ctx->remembered};
    for(unsigned i=0;i<2;i++) {
        uint64_t published=atomic_load_explicit(&buffers[i]->published,memory_order_acquire);
        uint64_t consumed=atomic_load_explicit(&buffers[i]->consumed,memory_order_acquire);
        if (published>buffers[i]->top || consumed>published) return GC_DT_INVALID;
        if (published!=buffers[i]->top) return i?GC_DT_FLUSH_REMEMBERED:GC_DT_FLUSH_SATB;
    }
    if (ctx->root_segments) {
        if (atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=g->phase) return GC_DT_ADOPT_PHASE;
        if (ctx->root_segments&7) return GC_DT_INVALID;
        RootRegistration *r=(RootRegistration *)(uintptr_t)ctx->root_segments;
        RootLifeRequest retire={.registration=(uintptr_t)r,.scope=r->scope,.context=(uintptr_t)ctx};
        uint64_t result=gc_root_life_locked(g,&retire,GC_RL_RETIRE);
        return result==GC_RL_OK?GC_DT_MORE:result==GC_RL_EXHAUSTED?GC_DT_EXHAUSTED:GC_DT_INVALID;
    }
    uint64_t result=gc_handshake_step_locked(h,p,GC_HS_DETACH);
    return result==GC_HS_OK?GC_DT_DONE:result==GC_HS_WAIT?GC_DT_WAIT:GC_DT_INVALID;
}
#endif
