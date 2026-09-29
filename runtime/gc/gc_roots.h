/* Nonmoving root storage and pre-mutation journal publication. No operation
 * here polls, allocates, resolves forwarding or grants a managed address.
 * Native storage is prepared/live/disjoint; the owner is the sole producer.
 * Segment owner/scope/cells/capacity/journal remain immutable through retirement.
 * Scope IDs never repeat. A scanner must acquire a storage lease before use;
 * retirement is NOT permission to free/reuse storage. gc_root_lifetime.h supplies
 * registered segment leases/retirement; the caller must finish deletion work
 * and phase/job handoff before reclaiming managed pages.
 * RootChange references are phase-lifetime holders until consumed by mark work.
 */
#ifndef KLASSIC_GC_ROOTS_H
#define KLASSIC_GC_ROOTS_H
#include "gc_abi.h"
#define GC_ROOT_RESERVE 0u
#define GC_ROOT_STORE 1u
#define GC_ROOT_RELEASE 2u
#define GC_ROOT_READ_RAW 3u
#define GC_ROOT_RETIRE 4u
/* Non-mutating preflight for allocation into an already reserved empty root.
 * Valid only within the same owner no-safepoint interval as publication. */
#define GC_ROOT_PREPARE_EMPTY 5u
#define GC_ROOT_OK 0u
#define GC_ROOT_INVALID 1u
#define GC_ROOT_FULL 2u
#define GC_ROOT_FLUSH 3u
#define GC_ROOT_EXHAUSTED 4u

static inline bool gc_root_segment_valid(const RootSegment *s) {
    return s->owner && s->scope && s->cells && !(s->cells & 7) && s->capacity &&
        s->capacity <= UINT32_MAX && s->cells <= UINT64_MAX - s->capacity * sizeof(RootCell);
}

/* Reserve publishes an empty output cell before any potentially allocating
 * value-producing call. STORE's input is already protected; returning FLUSH
 * never permits an unrooted input to cross a poll. READ_RAW returns unresolved
 * bits for a bounded no-safepoint transfer, not the public root-load barrier. */
static inline uint64_t gc_root_step_impl(MutatorContext *ctx, RootRequest *q, uint64_t op, bool guarded_store) {
    if (op > GC_ROOT_PREPARE_EMPTY || ctx->abi_version != GC_ABI_VERSION || !ctx->thread_id ||
        atomic_load_explicit(&ctx->state, memory_order_acquire) != GC_RUNNING ||
        (guarded_store ? (op != GC_ROOT_STORE || !atomic_load_explicit(&ctx->access_epoch, memory_order_acquire)) :
                         !!atomic_load_explicit(&ctx->access_epoch, memory_order_acquire)) || !q->segment || (q->segment & 7))
        return GC_ROOT_INVALID;
    RootSegment *s = (RootSegment *)(uintptr_t)q->segment;
    if (!gc_root_segment_valid(s) || s->owner != ctx->thread_id ||
        atomic_load_explicit(&s->state, memory_order_acquire) != GC_ROOT_SEGMENT_ACTIVE) return GC_ROOT_INVALID;
    uint64_t used = atomic_load_explicit(&s->used, memory_order_acquire);
    if (used > s->capacity) return GC_ROOT_INVALID;
    if (op == GC_ROOT_RETIRE) {
        if (s->registration) return GC_ROOT_INVALID;
        // Unregistered fixture/adapter storage only. Registered segments use
        // gc_root_life_locked to freeze and enqueue in one locked transaction.
        atomic_store_explicit(&s->state, GC_ROOT_SEGMENT_RETIRING, memory_order_release);
        return GC_ROOT_OK;
    }
    if (op == GC_ROOT_RESERVE) {
        if (used == s->capacity) return GC_ROOT_FULL;
        RootCell *cell = (RootCell *)(uintptr_t)s->cells + used;
        if (cell->active || atomic_load_explicit(&cell->value, memory_order_acquire)) return GC_ROOT_INVALID;
        if (cell->reuse_generation == UINT64_MAX) return GC_ROOT_EXHAUSTED;
        cell->reuse_generation++;
        cell->active = 1;
        q->handle = (RootHandle){(uintptr_t)cell, cell->reuse_generation, s->scope};
        atomic_store_explicit(&s->used, used + 1, memory_order_release);
        return GC_ROOT_OK;
    }
    if (q->handle.scope != s->scope || q->handle.cell < s->cells ||
        q->handle.cell - s->cells >= used * sizeof(RootCell) ||
        (q->handle.cell - s->cells) % sizeof(RootCell)) return GC_ROOT_INVALID;
    RootCell *cell = (RootCell *)(uintptr_t)q->handle.cell;
    if (!cell->active || !q->handle.reuse_generation || q->handle.reuse_generation != cell->reuse_generation)
        return GC_ROOT_INVALID;
    uint64_t previous = atomic_load_explicit(&cell->value, memory_order_acquire);
    if (op == GC_ROOT_READ_RAW) { q->value = previous; return GC_ROOT_OK; }
    if (!s->journal || (s->journal & 7)) return GC_ROOT_INVALID;
    BufferCursor *b = (BufferCursor *)(uintptr_t)s->journal;
    uint64_t top = b->top, published = atomic_load_explicit(&b->published, memory_order_acquire);
    uint64_t consumed = atomic_load_explicit(&b->consumed, memory_order_acquire);
    if (!b->entries || (b->entries & 7) || !b->capacity || b->capacity > UINT32_MAX ||
        b->entries > UINT64_MAX - b->capacity * sizeof(RootChange) || top != published ||
        consumed > top || top - consumed > b->capacity) return GC_ROOT_INVALID;
    if (top == UINT64_MAX) return GC_ROOT_EXHAUSTED;
    if (top - consumed == b->capacity) return GC_ROOT_FLUSH;
    uint64_t revision = atomic_load_explicit(&ctx->root_revision, memory_order_acquire);
    if (revision == UINT64_MAX) return GC_ROOT_EXHAUSTED;
    if (op == GC_ROOT_PREPARE_EMPTY) return previous ? GC_ROOT_INVALID : GC_ROOT_OK;
    uint64_t next = op == GC_ROOT_RELEASE ? 0 : q->value;
    RootChange *entry = (RootChange *)(uintptr_t)b->entries + top % b->capacity;
    *entry = (RootChange){previous, next, s->scope,
        atomic_load_explicit(&ctx->adopted_phase, memory_order_acquire)};
    b->top = top + 1;
    atomic_store_explicit(&b->published, top + 1, memory_order_release);
    atomic_store_explicit(&cell->value, next, memory_order_release);
    if (op == GC_ROOT_RELEASE) cell->active = 0;
    atomic_store_explicit(&ctx->root_revision, revision + 1, memory_order_release);
    return GC_ROOT_OK;
}
static inline uint64_t gc_root_step(MutatorContext *ctx, RootRequest *q, uint64_t op) {
    return gc_root_step_impl(ctx,q,op,false);
}
/* Cancel an empty owner root without adding a no-op journal entry. This does
 * not reclaim/reuse the slot or shrink the published used range. Scanners see
 * the same null value; earlier journal work and root_revision remain untouched.
 * A nonempty or stale handle is rejected. Owner/context/native storage obey the
 * same contract as root_step; no poll or source guard is allowed here. */
static inline uint64_t gc_root_cancel_empty(MutatorContext *ctx,const RootRequest *q){
    RootRequest read=*q;
    uint64_t status=gc_root_step(ctx,&read,GC_ROOT_READ_RAW);
    if(status!=GC_ROOT_OK||read.value)return GC_ROOT_INVALID;
    ((RootCell *)(uintptr_t)read.handle.cell)->active=0;
    return GC_ROOT_OK;
}
/* Internal handoff only: caller has validated the unique live source scope.
 * No temporary guard suspension, allocation, flush or poll is permitted. */
static inline uint64_t gc_root_store_guarded(MutatorContext *ctx,RootRequest *q) {
    return gc_root_step_impl(ctx,q,GC_ROOT_STORE,true);
}
/* Both finite transfer operations return OK/INVALID/MORE = 0/1/2. They copy
 * into a caller-owned in-flight job, not unprotected scratch across a safepoint.
 * All root/job/journal references participate in phase and retirement handoff.
 * Invalid input preserves storage. At most 256 slots/entries per invocation. */
static inline uint64_t gc_root_scan(RootScan *q) {
    if (!q->segment || (q->segment & 7) || !q->output || (q->output & 7) ||
        !q->capacity || q->capacity > 256 || q->output > UINT64_MAX - q->capacity * 8) return GC_ROOT_INVALID;
    RootSegment *s = (RootSegment *)(uintptr_t)q->segment;
    if (!gc_root_segment_valid(s)) return GC_ROOT_INVALID;
    uint64_t state = atomic_load_explicit(&s->state, memory_order_acquire);
    if (state != GC_ROOT_SEGMENT_ACTIVE && state != GC_ROOT_SEGMENT_RETIRING) return GC_ROOT_INVALID;
    uint64_t used = atomic_load_explicit(&s->used, memory_order_acquire);
    if (used > s->capacity || q->cursor > used) return GC_ROOT_INVALID;
    uint64_t n = used - q->cursor;
    if (n > q->capacity) n = q->capacity;
    uint64_t *out = (uint64_t *)(uintptr_t)q->output;
    RootCell *cells = (RootCell *)(uintptr_t)s->cells;
    for (uint64_t i = 0; i < n; i++) out[i] = atomic_load_explicit(&cells[q->cursor + i].value, memory_order_acquire);
    q->cursor += n; q->written = n;
    return q->cursor == used ? GC_ROOT_OK : 2;
}

static inline uint64_t gc_root_drain(RootDrain *q) {
    if (!q->buffer || (q->buffer & 7) || !q->output || (q->output & 7) ||
        !q->capacity || q->capacity > 256 || q->output > UINT64_MAX - q->capacity * sizeof(RootChange))
        return GC_ROOT_INVALID;
    BufferCursor *b = (BufferCursor *)(uintptr_t)q->buffer;
    if (!b->entries || (b->entries & 7) || !b->capacity || b->capacity > UINT32_MAX ||
        b->entries > UINT64_MAX - b->capacity * sizeof(RootChange)) return GC_ROOT_INVALID;
    uint64_t consumed = atomic_load_explicit(&b->consumed, memory_order_acquire);
    uint64_t published = atomic_load_explicit(&b->published, memory_order_acquire);
    if (consumed > published || published - consumed > b->capacity) return GC_ROOT_INVALID;
    uint64_t n = published - consumed;
    if (n > q->capacity) n = q->capacity;
    RootChange *entries = (RootChange *)(uintptr_t)b->entries, *out = (RootChange *)(uintptr_t)q->output;
    for (uint64_t i = 0; i < n; i++) out[i] = entries[(consumed + i) % b->capacity];
    q->written = n;
    atomic_store_explicit(&b->consumed, consumed + n, memory_order_release);
    return consumed + n == published ? GC_ROOT_OK : 2;
}
#endif
