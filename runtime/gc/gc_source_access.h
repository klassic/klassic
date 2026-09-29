/* Source-local admission shared by EVERY actor: mutator, scanner and healer.
 * A strong reference and metadata/cell lifetime are protected separately. The
 * unique owner-private scope cannot nest or cross polling/blocking/callbacks.
 * No source address may escape the scope or be used after successful EXIT.
 * ENTER/SELECT linearize on access; an entrant racing claim/close is counted.
 * Claim winner SELECTs, then CHECK must succeed before ANY copy. PUBLISH and
 * RETAIN also check the closed zero-count state. REOPEN is only for a retained
 * source after private-copy cleanup. Published sources never reopen.
 * Each step performs at most one CAS. WAIT on ENTER is handled outside a guard.
 * WAIT on EXIT retains the guard and must be retried immediately, without
 * polling/yielding/waiting for a worker. Once closed, each failed exit CAS
 * witnesses another guard leaving (or closing), so new entrants cannot starve
 * draining. CHECK callers wait outside every guard and lock. Thread-state
 * publication already rejects access_epoch. No registry membership is added by
 * this leaf; every heap actor must be registered and use this same protocol.
 */
#ifndef KLASSIC_GC_SOURCE_ACCESS_H
#define KLASSIC_GC_SOURCE_ACCESS_H
#include "gc_abi.h"
#define GC_SA_ENTER 0u
#define GC_SA_EXIT 1u
#define GC_SA_SELECT 2u
#define GC_SA_CHECK 3u
#define GC_SA_REOPEN 4u
#define GC_SA_OK 0u
#define GC_SA_INVALID 1u
#define GC_SA_WAIT 2u
#define GC_SA_EXHAUSTED 3u
static inline uint64_t gc_source_access_step(ForwardCell *g,SourceAccessRequest *q,uint64_t op) {
    if(op>GC_SA_REOPEN||!g->source_offset||(g->source_offset&15)||g->source_offset>GC_MAX_OFFSET||!g->incarnation||
        q->source_offset!=g->source_offset||q->incarnation!=g->incarnation)return GC_SA_INVALID;
    if(op<=GC_SA_EXIT) {
        if(q->ticket||!q->context||(q->context&7))return GC_SA_INVALID;
        MutatorContext *ctx=(MutatorContext *)(uintptr_t)q->context;
        if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING)return GC_SA_INVALID;
        uint64_t epoch=atomic_load_explicit(&ctx->access_epoch,memory_order_acquire);
        if(op==GC_SA_ENTER) {
            if(q->held||epoch)return GC_SA_INVALID;
            uint64_t forwarding=atomic_load_explicit(&g->word,memory_order_acquire);
            if(forwarding&&forwarding!=3)return GC_SA_WAIT;
            uint64_t expected=atomic_load_explicit(&g->access,memory_order_acquire);
            if(expected&GC_SOURCE_CLOSED)return GC_SA_WAIT;
            if(expected==GC_SOURCE_CLOSED-1)return GC_SA_EXHAUSTED;
            if(!atomic_compare_exchange_strong_explicit(&g->access,&expected,expected+1,memory_order_acq_rel,memory_order_acquire))return GC_SA_WAIT;
            atomic_store_explicit(&ctx->access_epoch,g->incarnation,memory_order_release);q->held=(uintptr_t)g;
        } else {
            if(q->held!=(uintptr_t)g||epoch!=g->incarnation)return GC_SA_INVALID;
            uint64_t expected=atomic_load_explicit(&g->access,memory_order_acquire);
            if(!(expected&~GC_SOURCE_CLOSED))return GC_SA_INVALID;
            if(!atomic_compare_exchange_strong_explicit(&g->access,&expected,expected-1,memory_order_acq_rel,memory_order_acquire))return GC_SA_WAIT;
            atomic_store_explicit(&ctx->access_epoch,0,memory_order_release);q->held=0;
        }
        return GC_SA_OK;
    }
    if(q->context||q->held)return GC_SA_INVALID;
    uint64_t forwarding=atomic_load_explicit(&g->word,memory_order_acquire);
    if(op==GC_SA_REOPEN) {
        if(q->ticket||forwarding!=3)return GC_SA_INVALID;
        uint64_t expected=GC_SOURCE_CLOSED;
        return atomic_compare_exchange_strong_explicit(&g->access,&expected,0,memory_order_acq_rel,memory_order_acquire)?GC_SA_OK:GC_SA_INVALID;
    }
    if(!q->ticket||q->ticket>(UINT64_MAX>>2)||forwarding!=((q->ticket<<2)|1))return GC_SA_INVALID;
    uint64_t expected=atomic_load_explicit(&g->access,memory_order_acquire);
    if(op==GC_SA_CHECK)return expected==GC_SOURCE_CLOSED?GC_SA_OK:GC_SA_WAIT;
    if(expected&GC_SOURCE_CLOSED)return GC_SA_OK;
    return atomic_compare_exchange_strong_explicit(&g->access,&expected,expected|GC_SOURCE_CLOSED,memory_order_acq_rel,memory_order_acquire)?GC_SA_OK:GC_SA_WAIT;
}
#endif
