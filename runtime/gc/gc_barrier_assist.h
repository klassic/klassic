/* Bounded owner assistance for barrier-poll action results. Publish a complete
 * unpublished buffer prefix (never drain/ACK it), or close the owner's TLAB
 * under the native heap mutex. Caller retains context/runtime/storage and
 * owns the complete entries and TLAB; no INITIALIZING object may lose its
 * root/publication obligation when the TLAB closes. No wait, allocation,
 * reclamation, mark work or phase adoption occurs. RETRY means poll again,
 * not that the handshake or underlying barrier is complete.
 * Flush needs no mutex; close uses the same heap lock as the pool/allocator.
 * No lock acquisition is permitted with a guard. Rejections preserve context,
 * pages and buffer cursors; lock callbacks may still have run. */
#ifndef KLASSIC_GC_BARRIER_ASSIST_H
#define KLASSIC_GC_BARRIER_ASSIST_H
#include "gc_barrier_poll.h"
#include "gc_heap.h"
#include "gc_buffers.h"
typedef uint64_t (*BarrierAssister)(const RootedStoreRuntime *,MutatorContext *,uint64_t);
static inline uint64_t gc_barrier_assist(const RootedStoreRuntime *d,MutatorContext *ctx,uint64_t action){
    if(!d||((uintptr_t)d&7)||!ctx||((uintptr_t)ctx&7)||action<GC_BP_FLUSH_SATB||action>GC_BP_CLOSE_TLAB||
        ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RLOAD_INVALID;
    if(action!=GC_BP_CLOSE_TLAB)return gc_barrier_buffer_step(ctx,NULL,action==GC_BP_FLUSH_SATB?GC_BB_PUBLISH_SATB:GC_BB_PUBLISH_REMEMBERED)==GC_BB_OK?GC_BP_RETRY:GC_RLOAD_INVALID;
    if(!d->heap_pool||(d->heap_pool&7)||!d->heap_lock||!d->lock||!d->unlock)return GC_RLOAD_INVALID;
    GcNativeLock lock=(GcNativeLock)(uintptr_t)d->lock,unlock=(GcNativeLock)(uintptr_t)d->unlock;
    lock(d->heap_lock);uint64_t result=GC_RLOAD_INVALID;
    MetadataLeasePool *pool=(void *)(uintptr_t)d->heap_pool;
    if(pool->heap&&!(pool->heap&7)){
        HeapPool *heap=(void *)(uintptr_t)pool->heap;
        if(heap->pages&&!(heap->pages&7)&&heap->page_count&&heap->page_count<(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)&&
            heap->pages<=UINT64_MAX-heap->page_count*sizeof(AllocationPage)&&ctx->tlab_start>=heap->heap_base){
            uint64_t offset=ctx->tlab_start-heap->heap_base,index=offset>>GC_HEAP_PAGE_SHIFT;
            if(index&&index<=heap->page_count&&!(offset&(GC_HEAP_PAGE_BYTES-1))){
                AllocationPage *page=(AllocationPage *)(uintptr_t)heap->pages+index-1;
                if(page->heap_base==heap->heap_base&&page->start==offset&&page->run_head==index-1&&
                    page->limit<=((heap->page_count+1)<<GC_HEAP_PAGE_SHIFT)&&gc_tlab_page_locked(ctx,page,1)==GC_ALLOC_OK)result=GC_BP_RETRY;
            }
        }
    }
    unlock(d->heap_lock);return result;
}
#endif
