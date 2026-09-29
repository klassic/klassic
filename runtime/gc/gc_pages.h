/* Heap-lock protected reservation transaction. No OS calls, allocation or
 * mutator waits occur here. Pool/table/lease storage is live and disjoint.
 * FREE ranges are never previously published, or have completed the separate
 * source/color retirement protocol. This API cannot reclaim a published page.
 * After reserve, the owner commits/zeroes the range and allocates a zero start
 * bitmap outside the lock. Commit transfers those resources to the pool.
 * Abort is legal only AFTER successful external cleanup; failed OS cleanup
 * retains the reservation, ownership and budget until retried. */
#ifndef KLASSIC_GC_PAGES_H
#define KLASSIC_GC_PAGES_H
#include "gc_abi.h"
#define GC_POOL_RESERVE 0u
#define GC_POOL_COMMIT 1u
#define GC_POOL_ABORT 2u
#define GC_POOL_OK 0u
#define GC_POOL_INVALID 1u
#define GC_POOL_HEAP_LIMIT 2u
#define GC_POOL_METADATA_LIMIT 3u
#define GC_POOL_FRAGMENTED 4u
#define GC_POOL_EXHAUSTED 5u

static inline uint64_t gc_pool_step_locked(HeapPool *h, PageLease *l, uint64_t op) {
    if (op > GC_POOL_ABORT || !h->pages || !h->page_count ||
        h->page_count >= (GC_MAX_OFFSET >> GC_HEAP_PAGE_SHIFT) || (h->heap_base & 15) ||
        h->heap_base > UINT64_MAX - ((h->page_count + 1) << GC_HEAP_PAGE_SHIFT) ||
        !h->hard_max || h->hard_max > (h->page_count << GC_HEAP_PAGE_SHIFT) ||
        h->soft_max > h->hard_max || h->relocation_reserve > h->hard_max ||
        h->committed > h->hard_max || h->pending > h->hard_max - h->committed ||
        h->metadata_committed < h->page_count * sizeof(AllocationPage) ||
        h->metadata_committed > h->metadata_max ||
        h->metadata_pending > h->metadata_max - h->metadata_committed ||
        !l->page_count || l->page_count > h->page_count || l->generation > GC_OLD ||
        l->relocation > 1 || l->committed_mapping > 1) return GC_POOL_INVALID;
    uint64_t bytes = l->page_count << GC_HEAP_PAGE_SHIFT;
    uint64_t metadata = bytes >> 7; // one object-start bit / 16 bytes
    AllocationPage *pages = (AllocationPage *)(uintptr_t)h->pages;
    if (op == GC_POOL_RESERVE) {
        if (l->stage || l->starts || l->committed_mapping || l->head_index || l->incarnation || l->identity_base)
            return GC_POOL_INVALID;
        uint64_t limit = h->hard_max - (l->relocation ? 0 : h->relocation_reserve);
        uint64_t used = h->committed + h->pending;
        if (used > limit || bytes > limit - used) return GC_POOL_HEAP_LIMIT;
        if (metadata > h->metadata_max - h->metadata_committed - h->metadata_pending)
            return GC_POOL_METADATA_LIMIT;
        uint64_t granules = bytes >> 4;
        if (h->last_identity > UINT64_MAX - granules || h->last_incarnation == UINT64_MAX)
            return GC_POOL_EXHAUSTED;
        uint64_t run = 0, first = 0;
        for (uint64_t i = 0; i < h->page_count; i++) {
            if (atomic_load_explicit(&pages[i].state, memory_order_acquire) == GC_PAGE_FREE && !pages[i].metadata_leases) run++;
            else run = 0;
            if (run == l->page_count) { first = i + 1 - run; break; }
        }
        if (run != l->page_count) return GC_POOL_FRAGMENTED;
        uint64_t identity = h->last_identity + 1, incarnation = h->last_incarnation + 1;
        for (uint64_t i = first; i < first + run; i++) {
            AllocationPage *p = &pages[i];
            p->heap_base = h->heap_base;
            p->start = (i + 1) << GC_HEAP_PAGE_SHIFT;
            p->limit = (first + run + 1) << GC_HEAP_PAGE_SHIFT;
            p->incarnation = incarnation;
            p->identity_base = identity;
            p->owner = 0;
            p->generation = l->generation;
            p->starts = p->starts_words = p->forward_table = 0;
            p->run_head = first;
            atomic_store_explicit(&p->allocated_top, p->start, memory_order_release);
            atomic_store_explicit(&p->state, i == first ? GC_PAGE_PREPARING : GC_PAGE_RUN_TAIL, memory_order_release);
        }
        h->pending += bytes;
        h->metadata_pending += metadata;
        h->last_identity += granules;
        h->last_incarnation = incarnation;
        h->pressure = used + bytes > h->soft_max;
        l->head_index = first;
        l->incarnation = incarnation;
        l->identity_base = identity;
        l->stage = 1;
        return GC_POOL_OK;
    }
    if (l->stage != 1 || l->head_index > h->page_count - l->page_count ||
        h->pending < bytes || h->metadata_pending < metadata ||
        (op == GC_POOL_COMMIT ? (!l->committed_mapping || !l->starts || (l->starts & 7)) :
                               (l->committed_mapping || l->starts)))
        return GC_POOL_INVALID;
    for (uint64_t i = l->head_index; i < l->head_index + l->page_count; i++) {
        AllocationPage *p = &pages[i];
        if (p->metadata_leases || p->heap_base != h->heap_base || p->run_head != l->head_index ||
            p->start != ((i + 1) << GC_HEAP_PAGE_SHIFT) ||
            p->limit != ((l->head_index + l->page_count + 1) << GC_HEAP_PAGE_SHIFT) ||
            p->incarnation != l->incarnation || p->identity_base != l->identity_base ||
            p->generation != l->generation || atomic_load_explicit(&p->state, memory_order_acquire) !=
            (i == l->head_index ? GC_PAGE_PREPARING : GC_PAGE_RUN_TAIL)) return GC_POOL_INVALID;
    }
    h->pending -= bytes;
    h->metadata_pending -= metadata;
    if (op == GC_POOL_COMMIT) {
        AllocationPage *p = &pages[l->head_index];
        p->starts = l->starts;
        p->starts_words = metadata >> 3;
        h->committed += bytes;
        h->metadata_committed += metadata;
        atomic_store_explicit(&p->state, GC_PAGE_READY, memory_order_release);
        l->starts = l->committed_mapping = 0; // resources transferred to the page
        l->stage = 2;
    } else {
        for (uint64_t i = l->head_index; i < l->head_index + l->page_count; i++)
            atomic_store_explicit(&pages[i].state, GC_PAGE_FREE, memory_order_release);
        l->stage = 3;
    }
    h->pressure = h->committed + h->pending > h->soft_max;
    return GC_POOL_OK;
}
#endif
