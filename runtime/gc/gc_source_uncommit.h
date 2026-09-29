/* Heap-lock protected accounting transaction for an EVACUATED run. Native
 * controls/requests are live and disjoint. Run geometry/tails are immutable
 * allocator metadata; the scheduler owns this run's OS cleanup exclusively.
 * BEGIN binds page.owner to this exact request address and keeps all charges.
 * Unlock before OS decommit. Clear committed_mapping ONLY after OS success.
 * FINISH releases payload bytes once, retains bitmap/table metadata and the
 * virtual range, and publishes UNCOMMITTED. Failure keeps ownership/charges
 * for retry. No heap access, OS calls, allocation or range traversal here.
 * No private candidate or source actor may survive the prior evacuation seal.
 */
#ifndef KLASSIC_GC_SOURCE_UNCOMMIT_H
#define KLASSIC_GC_SOURCE_UNCOMMIT_H
#include "gc_abi.h"
enum { GC_SU_BEGIN=0, GC_SU_FINISH=1 };
enum { GC_SU_OK=0, GC_SU_INVALID=1 };
static inline uint64_t gc_source_uncommit_locked(HeapPool *h,SourceUncommit *q,uint64_t op){
    if(op>1||!h->pages||(h->pages&7)||!h->page_count||h->page_count>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
        h->pages>UINT64_MAX-h->page_count*sizeof(AllocationPage)||(h->heap_base&15)||
        h->heap_base>UINT64_MAX-((h->page_count+1)<<GC_HEAP_PAGE_SHIFT)||
        !h->hard_max||h->hard_max>(h->page_count<<GC_HEAP_PAGE_SHIFT)||h->soft_max>h->hard_max||
        h->relocation_reserve>h->hard_max||h->committed>h->hard_max||h->pending>h->hard_max-h->committed||
        !q->page_count||q->page_count>h->page_count||q->head_index>h->page_count-q->page_count||
        !q->incarnation||q->stage!=op||q->committed_mapping||q->owner!=(op?(uintptr_t)h:0))return GC_SU_INVALID;
    uint64_t bytes=q->page_count<<GC_HEAP_PAGE_SHIFT;
    AllocationPage *p=(AllocationPage *)(uintptr_t)h->pages+q->head_index;
    if(h->committed<bytes||p->heap_base!=h->heap_base||p->run_head!=q->head_index||
        p->start!=((q->head_index+1)<<GC_HEAP_PAGE_SHIFT)||p->limit!=((q->head_index+q->page_count+1)<<GC_HEAP_PAGE_SHIFT)||
        p->incarnation!=q->incarnation||!p->identity_base||p->generation>GC_OLD||
        atomic_load_explicit(&p->state,memory_order_acquire)!=GC_PAGE_EVACUATED||
        p->owner!=(op?(uintptr_t)q:0))return GC_SU_INVALID;
    if(!op){p->owner=(uintptr_t)q;q->owner=(uintptr_t)h;q->stage=1;q->committed_mapping=1;}
    else {h->committed-=bytes;h->pressure=h->committed+h->pending>h->soft_max;
        p->owner=0;atomic_store_explicit(&p->state,GC_PAGE_UNCOMMITTED,memory_order_release);q->stage=2;}
    return GC_SU_OK;
}
#endif
