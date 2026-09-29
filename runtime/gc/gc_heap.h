/* Generational TLAB allocation; independent of the legacy header/encoding.
 * This leaf never polls, locks, refills, zeroes a payload, or calls an allocator.
 * The owning Running thread supplies its committed, prezeroed, nonselected
 * page. No other actor changes that page or its object-start bits while owned.
 * Readers acquire the start bitmap / allocated_top before reading a header.
 * Descriptor/bitmap lifetimes and page/id-range uniqueness are established by
 * the slow path, not by dereferencing arbitrary untrusted pointers here.
 * Native descriptor/page/output/bitmap alignment is checked before access;
 * their backing storage and lifetime remain caller obligations.
 * Output is owner-private and must become rooted before the next safepoint.
 * INITIALIZING objects must not escape to other owners or be scanned as ready.
 * Root adoption, initialization completion and mark publication are subsequent
 * operations; merely returning a colored reference does not perform them. */
#ifndef KLASSIC_GC_HEAP_H
#define KLASSIC_GC_HEAP_H
#include "gc_abi.h"
#define GC_ALLOC_OK 0u
#define GC_ALLOC_REFILL 1u
#define GC_ALLOC_INVALID 2u
#define GC_ALLOC_OVERFLOW 3u
#define GC_ALLOC_METADATA_REFILL 4u

/* Bind (op 0) a prepared Young page to an empty TLAB, or close (op 1) this
 * owner's TLAB without reclaiming its objects. Caller owns ctx and heap lock.
 * Neither operation can wait or safepoint. Closed pages remain in the pool
 * until the separate collector retirement protocol makes them reusable. */
static inline uint64_t gc_tlab_page_locked(MutatorContext *ctx, AllocationPage *p, uint64_t op) {
    uint64_t state = atomic_load_explicit(&ctx->state, memory_order_acquire);
    if (op > 1 || ctx->abi_version != GC_ABI_VERSION || !ctx->thread_id ||
        (state != GC_RUNNING && !(op == 1 && state == GC_DETACHING)) ||
        atomic_load_explicit(&ctx->access_epoch, memory_order_acquire) ||
        p->generation != GC_YOUNG || !p->incarnation || !p->identity_base || !p->starts || (p->starts & 7) ||
        !p->start || p->start >= p->limit || p->limit > GC_MAX_OFFSET ||
        ((p->heap_base | p->start | p->limit) & 15) || p->heap_base > UINT64_MAX - p->limit ||
        p->starts_words < ((p->limit - p->start + 1023) >> 10) ||
        p->identity_base > UINT64_MAX - ((p->limit - p->start) / 16 - 1)) return GC_ALLOC_INVALID;
    uint64_t start = p->heap_base + p->start, end = p->heap_base + p->limit;
    if (!op) {
        if (ctx->tlab_start || ctx->tlab_top || ctx->tlab_end || p->owner ||
            atomic_load_explicit(&p->state, memory_order_acquire) != GC_PAGE_READY ||
            atomic_load_explicit(&p->allocated_top, memory_order_acquire) != p->start)
            return GC_ALLOC_INVALID;
        p->owner = ctx->thread_id;
        ctx->tlab_start = ctx->tlab_top = start;
        ctx->tlab_end = end;
        atomic_store_explicit(&p->state, GC_PAGE_ALLOCATING, memory_order_release);
    } else {
        if (p->owner != ctx->thread_id || ctx->tlab_start != start || ctx->tlab_end != end ||
            ctx->tlab_top < start || ctx->tlab_top > end || (ctx->tlab_top & 15) ||
            atomic_load_explicit(&p->state, memory_order_acquire) != GC_PAGE_ALLOCATING ||
            atomic_load_explicit(&p->allocated_top, memory_order_acquire) != ctx->tlab_top - p->heap_base)
            return GC_ALLOC_INVALID;
        p->owner = 0;
        ctx->tlab_start = ctx->tlab_top = ctx->tlab_end = 0;
        atomic_store_explicit(&p->state, GC_PAGE_CLOSED, memory_order_release);
    }
    return GC_ALLOC_OK;
}

static inline uint64_t gc_try_tlab_allocate(MutatorContext *ctx, const AllocationRequest *r) {
    if (ctx->abi_version != GC_ABI_VERSION || !ctx->thread_id ||
        atomic_load_explicit(&ctx->state, memory_order_acquire) != GC_RUNNING ||
        atomic_load_explicit(&ctx->access_epoch, memory_order_acquire) ||
        !r->descriptor || (r->descriptor & 7) || !r->output || (r->output & 7)) return GC_ALLOC_INVALID;
    const TypeDescriptor *d = (const TypeDescriptor *)(uintptr_t)r->descriptor;
    if (!d->id || d->id > UINT32_MAX || !d->alignment || d->alignment > 16 ||
        (d->alignment & (d->alignment - 1)) || (!d->element_bytes && r->elements)) return GC_ALLOC_INVALID;
    if (d->element_bytes && r->elements > (UINT64_MAX - d->fixed_bytes) / d->element_bytes)
        return GC_ALLOC_OVERFLOW;
    uint64_t payload = d->fixed_bytes + r->elements * d->element_bytes;
    if (payload > UINT64_MAX - 15) return GC_ALLOC_OVERFLOW;
    uint64_t rounded = (payload + 15) & ~UINT64_C(15);
    if (!rounded) rounded = 16;
    if (rounded > UINT64_MAX - sizeof(ObjectHeader)) return GC_ALLOC_OVERFLOW;
    uint64_t total = rounded + sizeof(ObjectHeader);
    PhaseSnapshot phase = {atomic_load_explicit(&ctx->adopted_phase, memory_order_acquire)};
    if (!gc_abi_valid_phase(phase)) return GC_ALLOC_INVALID;
    if (!r->page) return GC_ALLOC_REFILL;
    if (r->page & 7) return GC_ALLOC_INVALID;
    AllocationPage *p = (AllocationPage *)(uintptr_t)r->page;
    if (atomic_load_explicit(&p->state, memory_order_acquire) != GC_PAGE_ALLOCATING ||
        p->generation != GC_YOUNG || p->owner != ctx->thread_id || !p->incarnation ||
        !p->identity_base || !p->starts || (p->starts & 7) || !p->start || p->start >= p->limit ||
        p->limit > GC_MAX_OFFSET || ((p->heap_base | p->start | p->limit) & 15) ||
        p->heap_base > UINT64_MAX - p->limit ||
        ctx->tlab_start != p->heap_base + p->start ||
        ctx->tlab_end != p->heap_base + p->limit || ctx->tlab_top < ctx->tlab_start ||
        ctx->tlab_top > ctx->tlab_end || (ctx->tlab_top & 15)) return GC_ALLOC_INVALID;
    uint64_t offset = ctx->tlab_top - p->heap_base;
    if (atomic_load_explicit(&p->allocated_top, memory_order_acquire) != offset) return GC_ALLOC_INVALID;
    if (total > ctx->tlab_end - ctx->tlab_top) return GC_ALLOC_REFILL;
    uint64_t user = offset + sizeof(ObjectHeader);
    uint64_t granule = (user - p->start) >> 4;
    uint64_t word = granule >> 6, bit = UINT64_C(1) << (granule & 63);
    if (word >= p->starts_words) return GC_ALLOC_INVALID;
    if (p->identity_base > UINT64_MAX - granule) return GC_ALLOC_OVERFLOW;
    _Atomic uint64_t *start = (_Atomic uint64_t *)(uintptr_t)p->starts + word;
    uint64_t before = atomic_load_explicit(start, memory_order_acquire);
    if (before & bit) return GC_ALLOC_INVALID;
    /* Arena backing was admitted/zeroed outside this leaf, is exclusively
     * bumped by this owner, and outlives all physical copies and mark jobs.
     * Retirement never resets a used record while any such user survives. */
    if (!r->marks) return GC_ALLOC_METADATA_REFILL;
    if (r->marks & 7) return GC_ALLOC_INVALID;
    MarkArena *arena = (MarkArena *)(uintptr_t)r->marks;
    if (!arena->records || (arena->records & 7) || !arena->capacity ||
        arena->capacity > UINT64_MAX / sizeof(MarkRecord) ||
        arena->records > UINT64_MAX - arena->capacity * sizeof(MarkRecord) ||
        arena->used > arena->capacity) return GC_ALLOC_INVALID;
    if (arena->used == arena->capacity) return GC_ALLOC_METADATA_REFILL;
    MarkRecord *record = (MarkRecord *)(uintptr_t)arena->records + arena->used;
    /* Check the complete constant-size free cell, without type-punning. */
    const unsigned char *raw = (const unsigned char *)record;
    for (size_t i = 0; i < sizeof(*record); i++) if (raw[i]) return GC_ALLOC_INVALID;
    /* The run owns an immutable native table, installed before allocation.
     * It can never grow/move in place while readers retain its metadata lease. */
    if (!p->forward_table) return GC_ALLOC_METADATA_REFILL;
    if (p->forward_table & 7) return GC_ALLOC_INVALID;
    ForwardTable *table = (ForwardTable *)(uintptr_t)p->forward_table;
    uint64_t used = atomic_load_explicit(&table->used, memory_order_acquire);
    if (!table->cells || (table->cells & 7) || !table->capacity ||
        table->capacity > UINT32_MAX || table->cells > UINT64_MAX - table->capacity * sizeof(ForwardCell) ||
        used > table->capacity) return GC_ALLOC_INVALID;
    if (used == table->capacity) return GC_ALLOC_METADATA_REFILL;
    ForwardCell *cells = (ForwardCell *)(uintptr_t)table->cells, *cell = &cells[used];
    if (used && (cells[used-1].source_offset >= user || cells[used-1].incarnation != p->incarnation))
        return GC_ALLOC_INVALID;
    if (cell->source_offset || cell->incarnation || atomic_load_explicit(&cell->word, memory_order_relaxed) ||
        atomic_load_explicit(&cell->access, memory_order_relaxed)) return GC_ALLOC_INVALID;
    uint64_t id = p->identity_base + granule;
    ObjectHeader *header = (ObjectHeader *)(uintptr_t)ctx->tlab_top;
    cell->source_offset = user;
    cell->incarnation = p->incarnation;
    atomic_store_explicit(&table->used, used+1, memory_order_release);
    record->object_id = id;
    arena->used++;
    header->payload_bytes = payload;
    header->descriptor_id = d->id;
    header->mark_record = (uintptr_t)record;
    atomic_store_explicit(&header->state, GC_OBJECT_INITIALIZING, memory_order_release);
    /* Exclusive producer; other actors may only acquire-load this bitmap. */
    atomic_store_explicit(start, before | bit, memory_order_release);
    ctx->tlab_top += total;
    atomic_store_explicit(&p->allocated_top, offset + total, memory_order_release);
    uint64_t allocated=atomic_load_explicit(&ctx->allocated_bytes,memory_order_relaxed);
    atomic_store_explicit(&ctx->allocated_bytes,allocated>UINT64_MAX-total?UINT64_MAX:allocated+total,memory_order_release);
    AllocationResult *out = (AllocationResult *)(uintptr_t)r->output;
    out->reference = (user << 16) | gc_abi_color(phase);
    out->object_id = id;
    return GC_ALLOC_OK;
}
#endif
