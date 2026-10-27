/* Metadata leases, serialized by the heap lock shared with gc_pool_step_locked.
 * Native pool/heap/page-table storage is prepared before use. Slot backing is
 * stable throughout each locked operation; no caller retains a raw slot pointer
 * after it. An owning runtime may grow backing while preserving every logical
 * index/generation and publishing slots/capacity together under this same lock.
 * The old mapping remains owned until all old pointer borrows have ended. A
 * free slot has page=incarnation=0 and a one-based next_free link; initialize
 * this list outside pause/lock-critical traversal. Tokens belong to ONE pool.
 * Acquiring a token retains page metadata, not object liveness or immobility.
 * The unique token obligation may move through buffers/jobs but is released
 * exactly once after their use. Generation wrap is rejected. A zero holder
 * count is only ONE prerequisite of the separate page/forwarding retirement
 * protocol; this module never reclaims a published page.
 */
#ifndef KLASSIC_GC_METADATA_H
#define KLASSIC_GC_METADATA_H
#include "gc_abi.h"
#define GC_ML_ACQUIRE 0u
#define GC_ML_RELEASE 1u
#define GC_ML_RESOLVE 2u
#define GC_ML_OK 0u
#define GC_ML_INVALID 1u
#define GC_ML_FULL 2u
#define GC_ML_EXHAUSTED 3u
static inline uint64_t gc_metadata_step_locked(MetadataLeasePool *g,MetadataLeaseRequest *q,uint64_t op) {
    if(op>2 || !g->heap || (g->heap&7) || !g->slots || (g->slots&7) || !g->capacity ||
        g->capacity>UINT32_MAX || g->slots>UINT64_MAX-g->capacity*sizeof(MetadataLeaseSlot) ||
        g->active>g->capacity || g->free_head>g->capacity || (!!g->free_head != (g->active<g->capacity))) return GC_ML_INVALID;
    HeapPool *h=(HeapPool *)(uintptr_t)g->heap;
    if(!h->pages || (h->pages&7) || !h->page_count || h->page_count>UINT64_MAX/sizeof(AllocationPage) ||
        h->pages>UINT64_MAX-h->page_count*sizeof(AllocationPage)) return GC_ML_INVALID;
    MetadataLeaseSlot *slots=(MetadataLeaseSlot *)(uintptr_t)g->slots,*s;
    uint64_t index;
    if(op==GC_ML_ACQUIRE) {
        if(q->token) return GC_ML_INVALID;
        if(!g->free_head) return GC_ML_FULL;
        index=g->free_head;s=&slots[index-1];
        if(s->page || s->incarnation || s->generation>UINT32_MAX || s->next_free>g->capacity || s->next_free==index ||
            (!!s->next_free != (g->active+1<g->capacity))) return GC_ML_INVALID;
        if(s->generation==UINT32_MAX) return GC_ML_EXHAUSTED;
    } else {
        index=q->token&UINT32_MAX;uint64_t generation=q->token>>32;
        if(!index || index>g->capacity || !generation || !g->active) return GC_ML_INVALID;
        s=&slots[index-1];
        if(s->generation!=generation || !s->page || !s->incarnation || s->next_free) return GC_ML_INVALID;
        if(op==GC_ML_RELEASE && (q->page!=s->page || q->incarnation!=s->incarnation)) return GC_ML_INVALID;
    }
    uint64_t address=op==GC_ML_ACQUIRE?q->page:s->page;
    uint64_t incarnation=op==GC_ML_ACQUIRE?q->incarnation:s->incarnation;
    if(!incarnation || address<h->pages || address-h->pages>=h->page_count*sizeof(AllocationPage) ||
        (address-h->pages)%sizeof(AllocationPage)) return GC_ML_INVALID;
    AllocationPage *p=(AllocationPage *)(uintptr_t)address;
    if(p->incarnation!=incarnation) return GC_ML_INVALID;
    if(op==GC_ML_ACQUIRE) {
        uint64_t state=atomic_load_explicit(&p->state,memory_order_acquire);
        if((state!=GC_PAGE_ALLOCATING && state!=GC_PAGE_CLOSED && state!=GC_PAGE_EVACUATED&&state!=GC_PAGE_UNCOMMITTED) ||
            p->run_head!=(address-h->pages)/sizeof(AllocationPage)) return GC_ML_INVALID;
        if(p->metadata_leases==UINT64_MAX) return GC_ML_EXHAUSTED;
        g->free_head=s->next_free;g->active++;p->metadata_leases++;
        s->generation++;s->page=address;s->incarnation=incarnation;s->next_free=0;
        q->token=(s->generation<<32)|index;
    } else {
        if(!p->metadata_leases) return GC_ML_INVALID;
        if(op==GC_ML_RESOLVE) {q->page=address;q->incarnation=incarnation;return GC_ML_OK;}
        p->metadata_leases--;g->active--;s->page=s->incarnation=0;
        s->next_free=g->free_head;g->free_head=index;*q=(MetadataLeaseRequest){0};
    }
    return GC_ML_OK;
}
#endif
