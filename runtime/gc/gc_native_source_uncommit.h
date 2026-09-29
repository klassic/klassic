/* Native payload disposal after evacuation seal and SourceUncommit BEGIN.
 * The scheduler exclusively owns this stable request and run, keeps the heap
 * reservation alive, and has excluded all payload actors. Metadata leases may
 * remain: forwarding/weak users retain metadata through separate retirement.
 * No locks or source guards may be held; attached callers run in native state.
 */
#ifndef KLASSIC_GC_NATIVE_SOURCE_UNCOMMIT_H
#define KLASSIC_GC_NATIVE_SOURCE_UNCOMMIT_H
#include "gc_native_page_release.h"
#include "gc_source_uncommit.h"
static inline uint64_t gc_native_source_uncommit_payload(HeapPool *heap,
        SourceUncommit *request,NativePageDecommitter decommit){
    if(!heap||((uintptr_t)heap&7)||!request||((uintptr_t)request&7)||
       !heap->heap_base||(heap->heap_base&15)||!heap->pages||(heap->pages&7)||
       !heap->page_count||heap->page_count>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
       heap->pages>UINT64_MAX-heap->page_count*sizeof(AllocationPage)||
       heap->heap_base>UINT64_MAX-((heap->page_count+1)<<GC_HEAP_PAGE_SHIFT)||
       request->stage!=1||request->owner!=(uintptr_t)heap||request->committed_mapping!=1||
       !request->incarnation||!request->page_count||request->page_count>heap->page_count||
       request->head_index>heap->page_count-request->page_count)return GC_NATIVE_METADATA_INVALID;
    const AllocationPage *pages=(void *)(uintptr_t)heap->pages;
    const AllocationPage *head=&pages[request->head_index];
    uint64_t end=request->head_index+request->page_count;
    if(!head->identity_base||head->generation>GC_OLD)return GC_NATIVE_METADATA_INVALID;
    for(uint64_t i=request->head_index;i<end;i++){
        const AllocationPage *page=&pages[i];
        if(atomic_load_explicit(&page->state,memory_order_acquire)!=
           (i==request->head_index?GC_PAGE_EVACUATED:GC_PAGE_RUN_TAIL)||
           page->owner!=(i==request->head_index?(uintptr_t)request:0)||
           page->heap_base!=heap->heap_base||page->run_head!=request->head_index||
           page->start!=((i+1)<<GC_HEAP_PAGE_SHIFT)||page->limit!=((end+1)<<GC_HEAP_PAGE_SHIFT)||
           page->incarnation!=request->incarnation||page->identity_base!=head->identity_base||
           page->generation!=head->generation)return GC_NATIVE_METADATA_INVALID;
    }
    void *address=(void *)(uintptr_t)(heap->heap_base+((request->head_index+1)<<GC_HEAP_PAGE_SHIFT));
    size_t bytes=(size_t)(request->page_count<<GC_HEAP_PAGE_SHIFT);
    if(!(decommit?decommit:gc_native_heap_decommit_range)(address,bytes))return GC_NATIVE_METADATA_OS_ERROR;
    request->committed_mapping=0;return GC_NATIVE_METADATA_OK;
}
/* Shared emitted ABI takes an explicit native callback on every platform. */
typedef uint64_t (*SourcePayloadUncommitter)(HeapPool *,SourceUncommit *,NativePageDecommitter);
static inline uint64_t gc_native_source_uncommit_entry(HeapPool *heap,SourceUncommit *request,NativePageDecommitter decommit){
    if(!decommit)return GC_NATIVE_METADATA_INVALID;
    return gc_native_source_uncommit_payload(heap,request,decommit);
}
#endif
