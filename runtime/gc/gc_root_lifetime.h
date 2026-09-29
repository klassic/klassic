/* Segment registry/retirement bookkeeping. All lifecycle steps require one
 * registry lock, but never acquire it or wait. Prepared/live/disjoint native
 * storage is owned by the caller. Registration and lease CONTROL records are
 * unique and noncopied, and belong to exactly one registry. Requests may be
 * copied for retries, but each live lease has one job owner. Scan/clear/release
 * must never run concurrently using the same lease control record. Tokens are
 * serial-checked, including after the control slot is reused.
 * The coordinator must hand off every root/job/journal holder before changing
 * phase or reclaiming managed pages; these steps implement native segment life.
 * The root and handshake registries share one lock for context membership.
 * ctx.root_segments is the owner's active-list head; retiring_roots is a count
 * retaining the context until deletion, readers, cleanup and journals finish.
 */
#ifndef KLASSIC_GC_ROOT_LIFETIME_H
#define KLASSIC_GC_ROOT_LIFETIME_H
#include "gc_roots.h"
#define GC_RL_ATTACH 0u
#define GC_RL_ACQUIRE 1u
#define GC_RL_RELEASE 2u
#define GC_RL_RETIRE 3u
#define GC_RL_CLAIM 4u
#define GC_RL_ACK 5u
#define GC_RL_CLEAN 6u
#define GC_RL_FINISH 7u
#define GC_RL_OK 0u
#define GC_RL_INVALID 1u
#define GC_RL_WAIT 2u
#define GC_RL_EXHAUSTED 3u

static inline uint64_t gc_root_life_locked(RootRegistry *g, RootLifeRequest *q, uint64_t op) {
    if (op > 7 || !q->registration || (q->registration & 7) || (!!g->head != !!g->retiring)) return GC_RL_INVALID;
    RootRegistration *r = (RootRegistration *)(uintptr_t)q->registration;
    if (!r->segment || (r->segment & 7) || r->leases > g->leases) return GC_RL_INVALID;
    RootSegment *s = (RootSegment *)(uintptr_t)r->segment;
    uint64_t state = atomic_load_explicit(&s->state, memory_order_acquire);
    uint64_t used = atomic_load_explicit(&s->used, memory_order_acquire);
    if (!s->owner || !s->cells || (s->cells & 7) || !s->capacity || s->capacity > UINT32_MAX ||
        s->cells > UINT64_MAX - s->capacity * sizeof(RootCell) || used > s->capacity) return GC_RL_INVALID;
    if (op == GC_RL_ATTACH || op == GC_RL_RETIRE) {
        if (!q->context || (q->context & 7)) return GC_RL_INVALID;
        MutatorContext *ctx = (MutatorContext *)(uintptr_t)q->context;
        uint64_t thread_state = atomic_load_explicit(&ctx->state, memory_order_acquire);
        if (ctx->abi_version != GC_ABI_VERSION || ctx->thread_id != s->owner ||
            atomic_load_explicit(&ctx->access_epoch, memory_order_acquire) ||
            (thread_state != GC_RUNNING && !(op == GC_RL_RETIRE && thread_state == GC_DETACHING)) ||
            atomic_load_explicit(&ctx->adopted_phase, memory_order_acquire) != g->phase) return GC_RL_INVALID;
    }
    if (op == GC_RL_ATTACH) {
        if (r->registered || r->scope || r->leases || state || used || s->scope || s->registration ||
            r->limit || r->cursor || r->phase || r->required_mask || r->claim_serial || r->claim_end ||
            r->previous || r->next || r->inventory_previous || r->inventory_next || r->context || r->owner_previous || r->owner_next || r->journal_serial || r->journal_end || atomic_load_explicit(&r->cleanup_cursor, memory_order_acquire)) return GC_RL_INVALID;
        if (g->last_scope == UINT64_MAX || g->active == UINT64_MAX) return GC_RL_EXHAUSTED;
        RootRegistration *first = (RootRegistration *)(uintptr_t)g->inventory;
        if (first && ((g->inventory & 7) || first == r || first->inventory_previous)) return GC_RL_INVALID;
        MutatorContext *ctx = (MutatorContext *)(uintptr_t)q->context;
        RootRegistration *owned = (RootRegistration *)(uintptr_t)ctx->root_segments;
        if (owned && ((ctx->root_segments & 7) || owned == r || owned->owner_previous ||
            owned->context != q->context || owned->registered != 1)) return GC_RL_INVALID;
        r->scope = s->scope = q->scope = ++g->last_scope;
        r->registered = 1; s->registration = (uintptr_t)r; g->active++;
        r->inventory_next = g->inventory;
        if (first) first->inventory_previous = (uintptr_t)r;
        g->inventory = (uintptr_t)r;
        r->context = q->context; r->owner_next = ctx->root_segments;
        if (owned) owned->owner_previous = (uintptr_t)r;
        ctx->root_segments = (uintptr_t)r;
        atomic_store_explicit(&s->state, GC_ROOT_SEGMENT_ACTIVE, memory_order_release);
        return GC_RL_OK;
    }
    if (r->registered != 1 || !r->scope || q->scope != r->scope || s->scope != r->scope ||
        s->registration != (uintptr_t)r || state < 1 || state > 3) return GC_RL_INVALID;
    if (op == GC_RL_RETIRE) {
        if (state != 1 || !g->active || r->claim_serial || r->previous || r->next) return GC_RL_INVALID;
        if (g->retiring == UINT64_MAX) return GC_RL_EXHAUSTED;
        RootRegistration *head = (RootRegistration *)(uintptr_t)g->head;
        if (head && ((g->head & 7) || head == r || head->previous)) return GC_RL_INVALID;
        uint64_t young = g->phase & 15, old = (g->phase >> 4) & 15;
        if (young > 7 || old > 7) return GC_RL_INVALID;
        MutatorContext *ctx = (MutatorContext *)(uintptr_t)q->context;
        if (r->context != q->context) return GC_RL_INVALID;
        if (ctx->retiring_roots == UINT64_MAX) return GC_RL_EXHAUSTED;
        RootRegistration *prev = (RootRegistration *)(uintptr_t)r->owner_previous;
        RootRegistration *next = (RootRegistration *)(uintptr_t)r->owner_next;
        if ((prev && ((r->owner_previous & 7) || prev->owner_next != (uintptr_t)r || prev->context != q->context)) ||
            (!prev && ctx->root_segments != (uintptr_t)r) ||
            (next && ((r->owner_next & 7) || next->owner_previous != (uintptr_t)r || next->context != q->context))) return GC_RL_INVALID;
        if (prev) prev->owner_next = r->owner_next; else ctx->root_segments = r->owner_next;
        if (next) next->owner_previous = r->owner_previous;
        r->owner_previous = r->owner_next = 0;
        ctx->retiring_roots++;
        r->limit = used; r->cursor = 0; r->phase = g->phase;
        r->required_mask = (young >= 1 && young <= 3 ? 1u : 0u) | (old >= 1 && old <= 3 ? 2u : 0u);
        r->next = g->head; if (head) head->previous = (uintptr_t)r;
        g->head = (uintptr_t)r; g->active--; g->retiring++;
        atomic_store_explicit(&s->state, GC_ROOT_SEGMENT_RETIRING, memory_order_release);
        return GC_RL_OK;
    }
    if (!q->lease || (q->lease & 7)) return GC_RL_INVALID;
    RootLease *l = (RootLease *)(uintptr_t)q->lease;
    if (op == GC_RL_ACQUIRE || op == GC_RL_CLAIM || op == GC_RL_CLEAN) {
        if (l->kind) return GC_RL_INVALID;
        if (op == GC_RL_ACQUIRE) {
            if (state != 1) return GC_RL_INVALID;
        } else {
            if (state != 2 || !g->retiring || r->limit != used || r->cursor > r->limit) return GC_RL_INVALID;
            if (r->claim_serial) return GC_RL_WAIT;
            if (op == GC_RL_CLAIM && r->cursor == r->limit) return GC_RL_WAIT;
            if (op == GC_RL_CLEAN && (r->cursor != r->limit || r->leases)) return GC_RL_WAIT;
        }
        if (g->last_lease == UINT64_MAX || g->leases == UINT64_MAX || r->leases == UINT64_MAX) return GC_RL_EXHAUSTED;
        if (op == GC_RL_CLEAN) {
            RootRegistration *prev = (RootRegistration *)(uintptr_t)r->inventory_previous;
            RootRegistration *next = (RootRegistration *)(uintptr_t)r->inventory_next;
            if ((prev && ((r->inventory_previous & 7) || prev->inventory_next != (uintptr_t)r)) ||
                (!prev && g->inventory != (uintptr_t)r) ||
                (next && ((r->inventory_next & 7) || next->inventory_previous != (uintptr_t)r))) return GC_RL_INVALID;
            // No remaining reader pins this node. Repair a walker's predecessor
            // before cleanup begins; completed deletion jobs cover skipped roots.
            if (prev) prev->inventory_next = r->inventory_next; else g->inventory = r->inventory_next;
            if (next) next->inventory_previous = r->inventory_previous;
            r->inventory_previous = r->inventory_next = 0;
        }
        uint64_t kind = op == GC_RL_ACQUIRE ? 1 : op == GC_RL_CLAIM ? 2 : 3;
        *l = (RootLease){(uintptr_t)r, r->scope, ++g->last_lease, kind, 0};
        q->serial = l->serial; r->leases++; g->leases++;
        if (op == GC_RL_CLAIM) {
            r->claim_serial = l->serial; l->cursor = r->cursor;
            r->claim_end = r->limit - r->cursor > 256 ? r->cursor + 256 : r->limit;
            q->begin = r->cursor; q->end = r->claim_end; q->mask = r->required_mask; q->phase = r->phase;
        } else if (op == GC_RL_CLEAN) {
            r->claim_serial = l->serial;
            atomic_store_explicit(&r->cleanup_cursor, 0, memory_order_release);
            atomic_store_explicit(&s->state, GC_ROOT_SEGMENT_CLEANING, memory_order_release);
        }
        return GC_RL_OK;
    }
    uint64_t kind = op == GC_RL_RELEASE ? 1 : op == GC_RL_ACK ? 2 : 3;
    if (l->kind != kind || !q->serial || q->serial != l->serial || l->registration != (uintptr_t)r ||
        l->scope != r->scope || !r->leases || !g->leases) return GC_RL_INVALID;
    if (op == GC_RL_RELEASE) {
        if (state == 3) return GC_RL_INVALID;
    } else if (op == GC_RL_ACK) {
        if (state != 2 || r->claim_serial != l->serial || q->begin != r->cursor || q->end != r->claim_end ||
            q->phase != r->phase || q->end > r->limit || q->end <= q->begin) return GC_RL_INVALID;
        if (q->mask != r->required_mask || l->cursor != r->claim_end) return GC_RL_WAIT;
        // Caller certifies all copied values were processed for BOTH captured
        // mark obligations before ACK. Queue-empty is not this certification.
        r->cursor = r->claim_end; r->claim_serial = r->claim_end = 0;
    } else {
        if (state != 3 || r->claim_serial != l->serial || r->leases != 1 || !g->retiring) return GC_RL_INVALID;
        if (atomic_load_explicit(&r->cleanup_cursor, memory_order_acquire) != r->limit) return GC_RL_WAIT;
        if (!r->context || (r->context & 7)) return GC_RL_INVALID;
        MutatorContext *ctx = (MutatorContext *)(uintptr_t)r->context;
        if (ctx->abi_version != GC_ABI_VERSION || ctx->thread_id != s->owner || !ctx->retiring_roots) return GC_RL_INVALID;
        if (s->journal) {
            if (s->journal & 7) return GC_RL_INVALID;
            BufferCursor *journal = (BufferCursor *)(uintptr_t)s->journal;
            // Read consumed first: another owner segment may still publish.
            uint64_t consumed = atomic_load_explicit(&journal->consumed, memory_order_acquire);
            uint64_t published = atomic_load_explicit(&journal->published, memory_order_acquire);
            if (consumed > published) return GC_RL_INVALID;
            if (consumed != published) return GC_RL_WAIT;
        }
        RootRegistration *prev = (RootRegistration *)(uintptr_t)r->previous, *next = (RootRegistration *)(uintptr_t)r->next;
        if ((prev && ((r->previous & 7) || prev->next != (uintptr_t)r)) ||
            (!prev && g->head != (uintptr_t)r) ||
            (next && ((r->next & 7) || next->previous != (uintptr_t)r))) return GC_RL_INVALID;
        if (prev) prev->next = r->next; else g->head = r->next;
        if (next) next->previous = r->previous;
        g->retiring--; g->leases--; l->kind = 0;
        ctx->retiring_roots--;
        uint64_t segment = r->segment;
        // No reader/job remains; only the exclusive cleaner used this storage.
        *r = (RootRegistration){.segment = segment};
        s->scope = s->registration = 0;
        atomic_store_explicit(&s->used, 0, memory_order_release);
        atomic_store_explicit(&s->state, GC_ROOT_SEGMENT_PREPARED, memory_order_release);
        return GC_RL_OK;
    }
    r->leases--; g->leases--; l->kind = 0;
    return GC_RL_OK;
}

/* Exclusive cleaner, outside registry lock; no scanner can acquire CLEANING.
 * The cleaner alone owns its request/control record until FINISH. Generation
 * counters are preserved, so reused cells cannot revive stale handles. */
static inline uint64_t gc_root_clear_chunk(RootLifeRequest *q) {
    if (!q->registration || (q->registration & 7) || !q->lease || (q->lease & 7)) return GC_RL_INVALID;
    RootLease *l = (RootLease *)(uintptr_t)q->lease;
    if (l->kind != 3 || !q->serial || l->serial != q->serial || l->registration != q->registration || l->scope != q->scope)
        return GC_RL_INVALID;
    RootRegistration *r = (RootRegistration *)(uintptr_t)q->registration;
    if (!r->registered || r->scope != q->scope || r->claim_serial != q->serial || r->leases != 1 || !r->segment)
        return GC_RL_INVALID;
    RootSegment *s = (RootSegment *)(uintptr_t)r->segment;
    uint64_t cursor = atomic_load_explicit(&r->cleanup_cursor, memory_order_acquire);
    if (atomic_load_explicit(&s->state, memory_order_acquire) != 3 || cursor > r->limit || r->limit > s->capacity)
        return GC_RL_INVALID;
    uint64_t end = r->limit - cursor > 256 ? cursor + 256 : r->limit;
    RootCell *cells = (RootCell *)(uintptr_t)s->cells;
    for (uint64_t i = cursor; i < end; i++) {
        atomic_store_explicit(&cells[i].value, 0, memory_order_release);
        cells[i].active = 0;
    }
    atomic_store_explicit(&r->cleanup_cursor, end, memory_order_release);
    return end == r->limit ? GC_RL_OK : GC_RL_WAIT;
}

static inline uint64_t gc_root_scan_leased(RootScanJob *q) {
    if (!q->lease || (q->lease & 7)) return GC_ROOT_INVALID;
    RootLease *l = (RootLease *)(uintptr_t)q->lease;
    if (!q->serial || q->serial != l->serial || (l->kind != 1 && l->kind != 2) || !l->registration)
        return GC_ROOT_INVALID;
    const RootRegistration *r = (const RootRegistration *)(uintptr_t)l->registration;
    if (r->registered != 1 || r->scope != l->scope || r->segment != q->scan.segment) return GC_ROOT_INVALID;
    if (l->kind == 2 && (r->claim_serial != l->serial || l->cursor != q->scan.cursor ||
        l->cursor >= r->claim_end || q->scan.capacity > r->claim_end - l->cursor)) return GC_ROOT_INVALID;
    uint64_t status = gc_root_scan(&q->scan);
    if (status != GC_ROOT_INVALID && l->kind == 2) l->cursor = q->scan.cursor;
    return status;
}
#endif
