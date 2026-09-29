/* Shared finite handshake steps. ALL operations require the registry lock;
 * this code neither acquires it nor blocks. Caller owns live nonmoving storage
 * for h, p and p->context, and publishes notifications only after a successful
 * step. Each registered context has exactly one registry entry; copying a live
 * entry or using it with another coordinator violates this contract. Reuse must
 * wait for detach and all external scanner borrows to finish. This bookkeeping
 * does not establish those scanner lifetimes. No heap/roots/buffers are scanned
 * here. The intrusive newest-first inventory is maintained under this lock.
 * Enumerators hold the same lock for the whole traversal; a linked entry is
 * not an outside-lock context lease. Register/detach touch at most two neighbors.
 * Generation is NOT color/phase
 * adoption. Private producers forbid owner state changes until cleanup. Failures (including duplicate acknowledgement) change no memory. */
#ifndef KLASSIC_GC_HANDSHAKE_H
#define KLASSIC_GC_HANDSHAKE_H
#include "gc_context.h"

#define GC_HS_REGISTER 0u
#define GC_HS_ACK 1u
#define GC_HS_RESUME 2u
#define GC_HS_ENTER_NATIVE 3u
#define GC_HS_LEAVE_NATIVE 4u
#define GC_HS_DETACH 5u
#define GC_HS_BEGIN 6u
#define GC_HS_END 7u
#define GC_HS_OK 0u
#define GC_HS_WAIT 1u
#define GC_HS_INVALID 2u
#define GC_HS_EXHAUSTED 3u
#define GC_HS_DUPLICATE 4u
#define GC_HS_INCOMPLETE 5u

static inline uint64_t gc_handshake_step_locked(
    HandshakeState *h, ThreadRegistration *p, uint64_t operation
) {
    if (operation > GC_HS_END || h->native > h->members || h->active > 1 || (!!h->head!=!!h->members) ||
        (h->active ? (!h->generation || h->expected != h->members - h->native ||
                      h->acknowledged > h->expected) :
                     (h->expected || h->acknowledged))) return GC_HS_INVALID;
    if (operation == GC_HS_BEGIN) {
        if (h->active) return GC_HS_WAIT;
        if (h->generation == UINT64_MAX) return GC_HS_EXHAUSTED;
        h->generation++;
        h->expected = h->members - h->native;
        h->active = 1;
        return GC_HS_OK;
    }
    if (operation == GC_HS_END) {
        if (!h->active) return GC_HS_INVALID;
        if (h->acknowledged != h->expected) return GC_HS_INCOMPLETE;
        h->active = h->expected = h->acknowledged = 0;
        return GC_HS_OK;
    }
    if ((operation == GC_HS_ACK) != (h->active != 0)) return GC_HS_WAIT;
    if (!p || (operation != GC_HS_REGISTER && p->registered != 1) || !p->context)
        return GC_HS_INVALID;
    MutatorContext *ctx = (MutatorContext *)(uintptr_t)p->context;
    if (ctx->abi_version != GC_ABI_VERSION ||
        atomic_load_explicit(&ctx->access_epoch, memory_order_acquire)) return GC_HS_INVALID;
    if(atomic_load_explicit(&ctx->private_producers,memory_order_acquire))
        return operation==GC_HS_REGISTER?GC_HS_INVALID:GC_HS_WAIT;
    uint64_t state = atomic_load_explicit(&ctx->state, memory_order_acquire);
    if (operation == GC_HS_REGISTER) {
        if (p->registered || p->registry || p->previous || p->next || state != GC_RUNNING || ctx->thread_id) return GC_HS_INVALID;
        if(ctx->satb_consumer.serial||ctx->satb_consumer.end||ctx->remembered_consumer.serial||ctx->remembered_consumer.end)return GC_HS_INVALID;
        if (h->members == UINT64_MAX || h->last_identity == UINT64_MAX) return GC_HS_EXHAUSTED;
        ThreadRegistration *head=(ThreadRegistration *)(uintptr_t)h->head;
        if(head&&((h->head&7)||head==p||head->registry!=(uintptr_t)h||head->registered!=1||head->previous||
            !head->identity||head->identity>h->last_identity))return GC_HS_INVALID;
        ctx->thread_id = ++h->last_identity;
        p->identity = ctx->thread_id;
        p->acknowledged_generation = h->generation;
        p->registered = 1;
        p->registry=(uintptr_t)h;p->previous=0;p->next=h->head;
        if(head)head->previous=(uintptr_t)p;
        h->head=(uintptr_t)p;
        h->members++;
        return GC_HS_OK;
    }
    if (p->registry!=(uintptr_t)h || !p->identity || p->identity != ctx->thread_id ||
        p->acknowledged_generation > h->generation) return GC_HS_INVALID;
    if (operation == GC_HS_ACK) {
        if (state != GC_RUNNING && state != GC_AT_SAFEPOINT && state != GC_DETACHING) return GC_HS_INVALID;
        if (p->acknowledged_generation == h->generation)
            return state == GC_AT_SAFEPOINT || state == GC_DETACHING ? GC_HS_DUPLICATE : GC_HS_INVALID;
        if (h->acknowledged >= h->expected) return GC_HS_INVALID;
        if (state != GC_DETACHING) atomic_store_explicit(&ctx->state, GC_AT_SAFEPOINT, memory_order_release);
        p->acknowledged_generation = h->generation;
        h->acknowledged++;
    } else if (operation == GC_HS_RESUME) {
        if ((state > GC_AT_SAFEPOINT && state != GC_DETACHING) || p->acknowledged_generation != h->generation)
            return GC_HS_INVALID;
        if (state != GC_DETACHING) atomic_store_explicit(&ctx->state, GC_RUNNING, memory_order_release);
    } else if (operation == GC_HS_ENTER_NATIVE) {
        if (state != GC_RUNNING || h->native == h->members ||
            p->acknowledged_generation != h->generation) return GC_HS_INVALID;
        atomic_store_explicit(&ctx->state, GC_BLOCKED_NATIVE, memory_order_release);
        h->native++;
    } else if (operation == GC_HS_LEAVE_NATIVE) {
        if (state != GC_BLOCKED_NATIVE || !h->native) return GC_HS_INVALID;
        atomic_store_explicit(&ctx->state, GC_RUNNING, memory_order_release);
        p->acknowledged_generation = h->generation;
        h->native--;
    } else {
        if (state != GC_DETACHING || h->members == h->native) return GC_HS_INVALID;
        if (ctx->root_segments || ctx->retiring_roots || ctx->tlab_start || ctx->tlab_top || ctx->tlab_end)
            return GC_HS_WAIT;
        if(ctx->satb_consumer.serial||ctx->remembered_consumer.serial)return GC_HS_WAIT;
        BufferCursor *buffers[] = {&ctx->satb, &ctx->remembered};
        for (unsigned i = 0; i < 2; i++) {
            BufferCursor *b = buffers[i];
            uint64_t published = atomic_load_explicit(&b->published, memory_order_acquire);
            uint64_t consumed = atomic_load_explicit(&b->consumed, memory_order_acquire);
            if (published > b->top || consumed > published) return GC_HS_INVALID;
            if (published != b->top || consumed != published) return GC_HS_WAIT;
        }
        ThreadRegistration *prev=(ThreadRegistration *)(uintptr_t)p->previous,*next=(ThreadRegistration *)(uintptr_t)p->next;
        if((h->members==1&&(prev||next))||(h->members>1&&!prev&&!next)||
            (prev&&((p->previous&7)||prev->registry!=(uintptr_t)h||prev->registered!=1||prev->next!=(uintptr_t)p||prev->identity<=p->identity))||
            (!prev&&h->head!=(uintptr_t)p)||
            (next&&((p->next&7)||next->registry!=(uintptr_t)h||next->registered!=1||next->previous!=(uintptr_t)p||!next->identity||next->identity>=p->identity)))return GC_HS_INVALID;
        if(prev)prev->next=p->next;else h->head=p->next;
        if(next)next->previous=p->previous;
        atomic_store_explicit(&ctx->state, GC_DETACHED, memory_order_release);
        h->members--;
        *p = (ThreadRegistration){0};
    }
    return GC_HS_OK;
}
#endif
