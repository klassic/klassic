/* OS backing for a reserved page's object-start bitmap. The pool already
 * charges logical bitmap bytes; this unique owner charges only OS padding.
 * Exactly one bitmap owner may bind a lease. Owner, heap, lease and allocator
 * operations remain stable and disjoint until final release.
 * *_locked calls require the heap mutex. Mapping/disposal run outside locks
 * and source guards, in native state for attached callers. Published bitmap
 * disposal requires a detached PageRelease or SourceRetire receipt and its
 * independent lifetime proof. Heap storage outlives the bitmap owner.
 */
#ifndef KLASSIC_GC_NATIVE_BITMAP_H
#define KLASSIC_GC_NATIVE_BITMAP_H
#include "gc_native_heap.h"
#include "gc_page_release.h"
#include "gc_source_retire.h"
typedef struct {
    uint64_t self,stage,bytes,mapping;
    HeapPool *heap;
    PageLease *lease;
    MetadataCharge padding;
} NativeBitmapOwner;
/* Stages: 1 admitted, 2 mapped, 3 published, 4 unmapped, 5 released. */
static inline bool gc_native_bitmap_valid(const NativeBitmapOwner *owner){
    return owner&&!((uintptr_t)owner&7)&&owner->self==(uintptr_t)owner&&
        owner->heap&&!((uintptr_t)owner->heap&7)&&owner->lease&&
        !((uintptr_t)owner->lease&7)&&owner->bytes;
}
static inline uint64_t gc_native_bitmap_reserve_locked(NativeBitmapOwner *owner,
        HeapPool *heap,PageLease *lease,const NativeMetadataOps *ops){
    if(!owner||((uintptr_t)owner&7)||!ops||!ops->map||!ops->unmap||
       !gc_native_pool_lease_valid(heap,lease)||lease->starts)return GC_BUDGET_INVALID;
    const unsigned char *raw=(const void *)owner;
    for(size_t i=0;i<sizeof(*owner);i++)if(raw[i])return GC_BUDGET_INVALID;
    uint64_t logical=lease->page_count*(GC_HEAP_PAGE_BYTES>>7),bytes=0;
    if(gc_native_metadata_size(logical,ops->quantum,&bytes)!=GC_NATIVE_METADATA_OK)return GC_BUDGET_INVALID;
    MetadataCharge padding={.bytes=bytes-logical};
    if(padding.bytes){
        uint64_t status=gc_budget_step_locked(heap,&padding,GC_BUDGET_RESERVE);
        if(status!=GC_BUDGET_OK)return status;
    }
    *owner=(NativeBitmapOwner){.self=(uintptr_t)owner,.stage=1,.bytes=bytes,
        .heap=heap,.lease=lease,.padding=padding};
    return GC_BUDGET_OK;
}
static inline uint64_t gc_native_bitmap_prepare(NativeBitmapOwner *owner,const NativeMetadataOps *ops){
    if(!gc_native_bitmap_valid(owner)||owner->stage!=1||owner->mapping||!ops||!ops->map||
       !gc_native_pool_lease_valid(owner->heap,owner->lease)||owner->lease->starts)
        return GC_NATIVE_METADATA_INVALID;
    owner->mapping=(uintptr_t)ops->map((size_t)owner->bytes);
    if(!owner->mapping)return GC_NATIVE_METADATA_OS_ERROR;
    /* Retain even an invalid allocator result for disposal; never publish it. */
    if((owner->mapping&7)||owner->mapping>UINT64_MAX-owner->bytes)return GC_NATIVE_METADATA_INVALID;
    owner->lease->starts=owner->mapping;owner->stage=2;
    return GC_NATIVE_METADATA_OK;
}
typedef uint64_t (*NativeBitmapPublisher)(HeapPool *,PageLease *,uint64_t);
static inline uint64_t gc_native_bitmap_publish_with_locked(NativeBitmapOwner *owner,NativeBitmapPublisher publish){
    if(!publish||!gc_native_bitmap_valid(owner)||owner->stage!=2||!owner->mapping||
       owner->lease->starts!=owner->mapping||!owner->lease->committed_mapping||
       !gc_native_pool_lease_valid(owner->heap,owner->lease))return GC_POOL_INVALID;
    if(owner->padding.bytes&&owner->padding.stage==1){
        /* The padding charge refers to the same mapping; it never owns a
         * second allocation. Its receipt is cleared only after full unmap. */
        owner->padding.allocation=owner->mapping;
        uint64_t status=gc_budget_step_locked(owner->heap,&owner->padding,GC_BUDGET_COMMIT);
        if(status!=GC_BUDGET_OK){owner->padding.allocation=0;return GC_POOL_INVALID;}
    }
    uint64_t status=publish(owner->heap,owner->lease,GC_POOL_COMMIT);
    if(status==GC_POOL_OK)owner->stage=3;
    return status;
}
static inline uint64_t gc_native_bitmap_publish_locked(NativeBitmapOwner *owner){
    return gc_native_bitmap_publish_with_locked(owner,gc_pool_step_locked);
}
/* Caller independently quiesces private users before disposal. A release
 * receipt proves detachment, not absence of outstanding borrowed pointers. */
static inline uint64_t gc_native_bitmap_dispose(NativeBitmapOwner *owner,PageRelease *release,
        const NativeMetadataOps *ops){
    if(!gc_native_bitmap_valid(owner)||owner->stage<1||owner->stage>3||!ops||!ops->unmap)
        return GC_NATIVE_METADATA_INVALID;
    PageLease *lease=owner->lease;
    if(owner->stage==3){
        if(!release||((uintptr_t)release&7)||release->stage!=1||
           release->owner!=(uintptr_t)owner->heap||release->head_index!=lease->head_index||
           release->page_count!=lease->page_count||release->incarnation!=lease->incarnation||
           release->starts!=owner->mapping)return GC_NATIVE_METADATA_INVALID;
    }else if(release||!gc_native_pool_lease_valid(owner->heap,lease)||
             (lease->starts&&lease->starts!=owner->mapping))return GC_NATIVE_METADATA_INVALID;
    if(owner->mapping&&!ops->unmap((void *)(uintptr_t)owner->mapping,(size_t)owner->bytes))
        return GC_NATIVE_METADATA_OS_ERROR;
    if(owner->stage==3)release->starts=0;else lease->starts=0;
    owner->mapping=owner->padding.allocation=0;owner->stage=4;
    return GC_NATIVE_METADATA_OK;
}
/* SourceRetire BEGIN follows the complete source/reference/color lifetime
 * proof and binds the exact request in the head. Payload is already discarded.
 * The head remains retained until bitmap/table cleanup and final STEP; freed
 * tail prefixes are deliberately not inspected. OS failure preserves all
 * receipts and both logical/padding charges. */
static inline uint64_t gc_native_bitmap_retire(NativeBitmapOwner *owner,SourceRetire *retire,
        const NativeMetadataOps *ops){
    if(!gc_native_bitmap_valid(owner)||owner->stage!=3||!owner->mapping||!ops||!ops->unmap||
       !retire||((uintptr_t)retire&7)||retire->stage!=1||retire->owner!=(uintptr_t)owner->heap||
       retire->head_index!=owner->lease->head_index||retire->page_count!=owner->lease->page_count||
       retire->incarnation!=owner->lease->incarnation||retire->starts!=owner->mapping||
       !retire->cursor||retire->cursor>retire->page_count||
       !owner->heap->pages||(owner->heap->pages&7)||retire->head_index>=owner->heap->page_count)
        return GC_NATIVE_METADATA_INVALID;
    const AllocationPage *head=(void *)(uintptr_t)owner->heap->pages;
    head+=retire->head_index;
    if(atomic_load_explicit(&head->state,memory_order_acquire)!=GC_PAGE_RELEASING||
       head->owner!=(uintptr_t)retire||head->incarnation!=retire->incarnation||
       head->run_head!=retire->head_index||head->starts||head->starts_words||head->forward_table)
        return GC_NATIVE_METADATA_INVALID;
    if(!ops->unmap((void *)(uintptr_t)owner->mapping,(size_t)owner->bytes))return GC_NATIVE_METADATA_OS_ERROR;
    retire->starts=owner->mapping=owner->padding.allocation=0;owner->stage=4;
    return GC_NATIVE_METADATA_OK;
}
static inline uint64_t gc_native_bitmap_release_locked(NativeBitmapOwner *owner){
    if(!gc_native_bitmap_valid(owner)||owner->stage!=4||owner->mapping)return GC_BUDGET_INVALID;
    if(owner->padding.bytes){
        uint64_t op=owner->padding.stage==1?GC_BUDGET_ABORT:GC_BUDGET_RELEASE;
        uint64_t status=gc_budget_step_locked(owner->heap,&owner->padding,op);
        if(status!=GC_BUDGET_OK)return status;
    }
    owner->stage=5;return GC_BUDGET_OK;
}
#endif
