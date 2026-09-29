/* Heap-lock protected release of a committed, NEVER installed run. Caller has
 * sole collector/preparation ownership and has discarded every private copy
 * job before BEGIN. Ready + unchanged top + empty table proves no installed
 * object, not absence of arbitrary private raw users: that is caller-owned.
 * Published/Closed/Allocating runs require the separate retirement protocol.
 *
 * BEGIN detaches bitmap/table ownership into this unique request and marks the
 * head Releasing. The range stays charged and unavailable to reserve/leases.
 * Unlock before OS decommit and freeing bitmap/table backing. Clear each output
 * only AFTER its cleanup succeeds. Table allocations retain their independent
 * MetadataCharge until free + budget RELEASE; this leaf releases bitmap bytes
 * only. FINISH requires all resources cleared, then uncharges committed bytes
 * and bitmap bytes and publishes Free. Failed cleanup holds ownership/budget
 * for retry; neither BEGIN nor FINISH accesses managed heap memory or allocates.
 * Request key/owner/stage cannot be copied, replayed or concurrently advanced.
 */
#ifndef KLASSIC_GC_PAGE_RELEASE_H
#define KLASSIC_GC_PAGE_RELEASE_H
#include "gc_pages.h"
enum { GC_RELEASE_BEGIN=0, GC_RELEASE_FINISH=1 };
enum { GC_RELEASE_OK=0, GC_RELEASE_INVALID=1 };
static inline uint64_t gc_page_release_locked(HeapPool *h,PageRelease *q,uint64_t op){
    if(op>1||!h->pages||(h->pages&7)||!h->page_count||h->page_count>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
        h->pages>UINT64_MAX-h->page_count*sizeof(AllocationPage)||(h->heap_base&15)||
        h->heap_base>UINT64_MAX-((h->page_count+1)<<GC_HEAP_PAGE_SHIFT)||
        !h->hard_max||h->hard_max>(h->page_count<<GC_HEAP_PAGE_SHIFT)||h->soft_max>h->hard_max||
        h->relocation_reserve>h->hard_max||h->committed>h->hard_max||h->pending>h->hard_max-h->committed||
        h->metadata_committed<h->page_count*sizeof(AllocationPage)||h->metadata_committed>h->metadata_max||
        h->metadata_pending>h->metadata_max-h->metadata_committed||!q->page_count||q->page_count>h->page_count||
        q->head_index>h->page_count-q->page_count||!q->incarnation)return GC_RELEASE_INVALID;
    uint64_t bytes=q->page_count<<GC_HEAP_PAGE_SHIFT,bitmap=bytes>>7;
    if(h->committed<bytes||h->metadata_committed-h->page_count*sizeof(AllocationPage)<bitmap||
        q->stage!=op||q->starts||q->forward_table||q->committed_mapping||
        q->owner!=(op?(uintptr_t)h:0))return GC_RELEASE_INVALID;
    AllocationPage *pages=(AllocationPage *)(uintptr_t)h->pages,*head=&pages[q->head_index];
    uint64_t end=q->head_index+q->page_count;
    if(!head->identity_base||head->generation>GC_OLD)return GC_RELEASE_INVALID;
    for(uint64_t i=q->head_index;i<end;i++){
        AllocationPage *p=&pages[i];
        if(p->heap_base!=h->heap_base||p->run_head!=q->head_index||p->start!=((i+1)<<GC_HEAP_PAGE_SHIFT)||
            p->limit!=((end+1)<<GC_HEAP_PAGE_SHIFT)||p->incarnation!=q->incarnation||
            p->identity_base!=head->identity_base||p->generation!=head->generation||p->owner||p->metadata_leases||
            atomic_load_explicit(&p->allocated_top,memory_order_acquire)!=p->start||
            atomic_load_explicit(&p->state,memory_order_acquire)!=(i==q->head_index?(op?GC_PAGE_RELEASING:GC_PAGE_READY):GC_PAGE_RUN_TAIL)||
            ((op||i!=q->head_index)&&(p->starts||p->starts_words||p->forward_table)))return GC_RELEASE_INVALID;
    }
    if(!op){
        if(!head->starts||(head->starts&7)||head->starts_words!=(bitmap>>3)||head->starts>UINT64_MAX-bitmap)return GC_RELEASE_INVALID;
        if(head->forward_table){
            if(head->forward_table&7)return GC_RELEASE_INVALID;
            const ForwardTable *table=(const ForwardTable *)(uintptr_t)head->forward_table;
            if(!table->cells||(table->cells&7)||!table->capacity||table->capacity>UINT32_MAX||
                table->cells>UINT64_MAX-table->capacity*sizeof(ForwardCell)||
                atomic_load_explicit(&table->used,memory_order_acquire))return GC_RELEASE_INVALID;
        }
        q->owner=(uintptr_t)h;q->starts=head->starts;q->forward_table=head->forward_table;q->committed_mapping=1;q->stage=1;
        head->starts=head->starts_words=head->forward_table=0;
        atomic_store_explicit(&head->state,GC_PAGE_RELEASING,memory_order_release);
    }else{
        h->committed-=bytes;h->metadata_committed-=bitmap;
        for(uint64_t i=q->head_index;i<end;i++)atomic_store_explicit(&pages[i].state,GC_PAGE_FREE,memory_order_release);
        h->pressure=h->committed+h->pending>h->soft_max;q->stage=2;
    }
    return GC_RELEASE_OK;
}
#endif
