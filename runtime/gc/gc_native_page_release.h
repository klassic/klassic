/* OS cleanup between PageRelease BEGIN and FINISH. The unique request and
 * page run are exclusively owned and remain stable throughout this call.
 * BEGIN's independent quiescence proof is required; page state alone does not
 * prove absence of private users. No heap mutex or source guard may be held;
 * attached callers run in native state. This never retires an installed page.
 */
#ifndef KLASSIC_GC_NATIVE_PAGE_RELEASE_H
#define KLASSIC_GC_NATIVE_PAGE_RELEASE_H
#include "gc_native_heap.h"
#include "gc_page_release.h"
typedef bool (*NativePageDecommitter)(void *,size_t);
/* NULL selects the native OS implementation. A supplied adapter must retain
 * the virtual reservation on failure and discard payload on success. */
static inline uint64_t gc_native_page_release_payload(NativeHeapOwner *owner,
        PageRelease *request,NativePageDecommitter decommit){
    HeapPool *heap=gc_native_heap_prepared(owner);
    if(!heap||!request||((uintptr_t)request&7)||request->stage!=1||
       request->owner!=(uintptr_t)heap||request->committed_mapping!=1||
       !request->incarnation||!request->page_count||request->page_count>heap->page_count||
       request->head_index>heap->page_count-request->page_count)return GC_NATIVE_METADATA_INVALID;
    const AllocationPage *pages=(void *)(uintptr_t)heap->pages;
    const AllocationPage *head=&pages[request->head_index];
    uint64_t end=request->head_index+request->page_count;
    if(!head->identity_base||head->generation>GC_OLD)return GC_NATIVE_METADATA_INVALID;
    for(uint64_t i=request->head_index;i<end;i++){
        const AllocationPage *page=&pages[i];
        if(atomic_load_explicit(&page->state,memory_order_acquire)!=
           (i==request->head_index?GC_PAGE_RELEASING:GC_PAGE_RUN_TAIL)||
           page->heap_base!=heap->heap_base||page->run_head!=request->head_index||
           page->start!=((i+1)<<GC_HEAP_PAGE_SHIFT)||page->limit!=((end+1)<<GC_HEAP_PAGE_SHIFT)||
           page->incarnation!=request->incarnation||page->identity_base!=head->identity_base||
           page->generation!=head->generation||page->owner||page->metadata_leases||
           page->starts||page->starts_words||page->forward_table||
           atomic_load_explicit(&page->allocated_top,memory_order_acquire)!=page->start)
            return GC_NATIVE_METADATA_INVALID;
    }
    void *address=(void *)(uintptr_t)(heap->heap_base+((request->head_index+1)<<GC_HEAP_PAGE_SHIFT));
    size_t bytes=(size_t)(request->page_count<<GC_HEAP_PAGE_SHIFT);
    if(!(decommit?decommit:gc_native_heap_decommit_range)(address,bytes))return GC_NATIVE_METADATA_OS_ERROR;
    request->committed_mapping=0;return GC_NATIVE_METADATA_OK;
}
#endif
