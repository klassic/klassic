/* Admission for nonmoving metadata storage, using the SAME heap lock and
 * pending/committed budget as page tables and start bitmaps (gc_pages.h).
 * A charge is unique live native storage, never copied/replayed or concurrently
 * mutated. bytes is the actual requested storage capacity, including padding;
 * callers check count/stride arithmetic before reservation. Native allocator
 * bookkeeping outside that capacity is not measured by this byte budget.
 *
 * RESERVE precedes allocation outside the lock. On success, prepare/zero the
 * storage and set allocation before COMMIT. No consumer may see it earlier.
 * On failure, first clean up any allocation and set allocation=0, then ABORT.
 * RELEASE follows the independent lifetime proof AND successful deallocation;
 * allocation must already be zero. Failed cleanup retains the charge for retry.
 * bytes/owner stay immutable while held. This is accounting, not a lifetime
 * validator: callers must retire all records/jobs before releasing storage.
 * No allocation, traversal, wait, or managed reference access occurs here.
 */
#ifndef KLASSIC_GC_BUDGET_H
#define KLASSIC_GC_BUDGET_H
#include "gc_abi.h"
#define GC_BUDGET_RESERVE 0u
#define GC_BUDGET_COMMIT 1u
#define GC_BUDGET_ABORT 2u
#define GC_BUDGET_RELEASE 3u
#define GC_BUDGET_OK 0u
#define GC_BUDGET_INVALID 1u
#define GC_BUDGET_LIMIT 2u
static inline uint64_t gc_budget_step_locked(HeapPool *h,MetadataCharge *q,uint64_t op) {
    if(op>GC_BUDGET_RELEASE || !h->page_count || h->page_count>UINT64_MAX/sizeof(AllocationPage) ||
        h->metadata_committed<h->page_count*sizeof(AllocationPage) || h->metadata_committed>h->metadata_max ||
        h->metadata_pending>h->metadata_max-h->metadata_committed || !q->bytes) return GC_BUDGET_INVALID;
    if(op==GC_BUDGET_RESERVE) {
        if(q->owner || q->allocation || q->stage) return GC_BUDGET_INVALID;
        if(q->bytes>h->metadata_max-h->metadata_committed-h->metadata_pending) return GC_BUDGET_LIMIT;
        h->metadata_pending+=q->bytes;q->owner=(uintptr_t)h;q->stage=1;
    } else {
        if(q->owner!=(uintptr_t)h || q->stage!=(op==GC_BUDGET_RELEASE?2:1)) return GC_BUDGET_INVALID;
        if(op==GC_BUDGET_RELEASE) {
            if(q->allocation || q->bytes>h->metadata_committed-h->page_count*sizeof(AllocationPage)) return GC_BUDGET_INVALID;
            h->metadata_committed-=q->bytes;q->stage=3;
        } else {
            if(q->bytes>h->metadata_pending) return GC_BUDGET_INVALID;
            if(op==GC_BUDGET_COMMIT) {
                if(!q->allocation || (q->allocation&7) || q->allocation>UINT64_MAX-q->bytes) return GC_BUDGET_INVALID;
                h->metadata_committed+=q->bytes;q->stage=2;
            } else {
                if(q->allocation) return GC_BUDGET_INVALID;
                q->stage=3;
            }
            h->metadata_pending-=q->bytes;
        }
    }
    return GC_BUDGET_OK;
}
#endif
