/* Private OS backing owner for the offset-reference heap. No mutator, root,
 * descriptor or worker is attached here. Prepare once before publication;
 * disposal requires external quiescence and all page/metadata obligations
 * returned. Never substitute this reservation for a live legacy heap.
 * Controls/config/ops are disjoint and stable. OS calls run outside locks,
 * in native state if attached. Failed cleanup keeps every surviving mapping.
 */
#ifndef KLASSIC_GC_NATIVE_HEAP_H
#define KLASSIC_GC_NATIVE_HEAP_H
#include "gc_native_metadata.h"
#include "gc_pages.h"
typedef struct { uint64_t hard_max,soft_max,relocation_reserve,metadata_max; } NativeHeapConfig;
typedef struct {
    NativeMetadataOps metadata;
    void *(*reserve)(size_t);
    bool (*release)(void *,size_t);
} NativeHeapOps;
/* 0 empty, 1 preparation attempted, 2 prepared/private, 3 disposed,
 * 4 disposal in progress (never publish a partially disposed pool). */
typedef struct {
    uint64_t self,stage,reservation,reservation_bytes,table,table_bytes;
    HeapPool heap;
} NativeHeapOwner;
static inline void *gc_native_heap_reserve(size_t bytes){
#ifdef _WIN32
    return VirtualAlloc(NULL,bytes,MEM_RESERVE,PAGE_NOACCESS);
#else
    void *p=mmap(NULL,bytes,PROT_NONE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
    return p==MAP_FAILED?NULL:p;
#endif
}
static inline NativeHeapOps gc_native_heap_ops(void){
    return (NativeHeapOps){gc_native_metadata_ops(),gc_native_heap_reserve,gc_native_metadata_unmap};
}
static inline bool gc_native_heap_ops_valid(const NativeHeapOps *ops){
    return ops&&ops->reserve&&ops->release&&ops->metadata.map&&ops->metadata.unmap&&
        ops->metadata.quantum>=8&&!(ops->metadata.quantum&(ops->metadata.quantum-1))&&
        !(GC_HEAP_PAGE_BYTES&(ops->metadata.quantum-1));
}
static inline uint64_t gc_native_heap_prepare(NativeHeapOwner *owner,const NativeHeapConfig *config,const NativeHeapOps *ops){
    if(!owner||((uintptr_t)owner&7)||!config||!gc_native_heap_ops_valid(ops))return GC_NATIVE_METADATA_INVALID;
    const unsigned char *raw=(const void *)owner;
    for(size_t i=0;i<sizeof(*owner);i++)if(raw[i])return GC_NATIVE_METADATA_INVALID;
    if(!config->hard_max||(config->hard_max&(GC_HEAP_PAGE_BYTES-1))||
       config->hard_max>=GC_MAX_OFFSET-GC_HEAP_PAGE_BYTES||config->soft_max>config->hard_max||
       config->relocation_reserve>config->hard_max)return GC_NATIVE_METADATA_INVALID;
    uint64_t count=config->hard_max>>GC_HEAP_PAGE_SHIFT,table_bytes=0;
    if(count>UINT64_MAX/sizeof(AllocationPage)||
       gc_native_metadata_size(count*sizeof(AllocationPage),ops->metadata.quantum,&table_bytes)!=GC_NATIVE_METADATA_OK||
       table_bytes>config->metadata_max)return GC_NATIVE_METADATA_INVALID;
    owner->self=(uintptr_t)owner;owner->stage=1;
    owner->reservation_bytes=config->hard_max+GC_HEAP_PAGE_BYTES;owner->table_bytes=table_bytes;
    owner->heap=(HeapPool){.page_count=count,.hard_max=config->hard_max,.soft_max=config->soft_max,
        .relocation_reserve=config->relocation_reserve,.metadata_max=config->metadata_max};
    owner->reservation=(uintptr_t)ops->reserve((size_t)owner->reservation_bytes);
    if(!owner->reservation)return GC_NATIVE_METADATA_OS_ERROR;
    if((owner->reservation&15)||owner->reservation>UINT64_MAX-owner->reservation_bytes)return GC_NATIVE_METADATA_INVALID;
    owner->table=(uintptr_t)ops->metadata.map((size_t)table_bytes);
    if(!owner->table)return GC_NATIVE_METADATA_OS_ERROR;
    if((owner->table&7)||owner->table>UINT64_MAX-table_bytes)return GC_NATIVE_METADATA_INVALID;
    /* map supplies zeroed native storage; atomics are initialized privately. */
    AllocationPage *pages=(void *)(uintptr_t)owner->table;
    for(uint64_t i=0;i<count;i++){
        atomic_init(&pages[i].state,GC_PAGE_FREE);atomic_init(&pages[i].allocated_top,0);
    }
    owner->heap.heap_base=owner->reservation;owner->heap.pages=owner->table;
    owner->heap.metadata_committed=table_bytes;owner->stage=2;
    return GC_NATIVE_METADATA_OK;
}
static inline HeapPool *gc_native_heap_prepared(NativeHeapOwner *owner){
    return owner&& !((uintptr_t)owner&7)&&owner->self==(uintptr_t)owner&&owner->stage==2?&owner->heap:NULL;
}
static inline uint64_t gc_native_heap_dispose(NativeHeapOwner *owner,const NativeHeapOps *ops){
    if(!owner||((uintptr_t)owner&7)||owner->self!=(uintptr_t)owner||
       (owner->stage!=1&&owner->stage!=2&&owner->stage!=4)||!gc_native_heap_ops_valid(ops))return GC_NATIVE_METADATA_INVALID;
    if(owner->stage==2){
        const HeapPool *h=&owner->heap;
        if(!owner->reservation||!owner->table||owner->reservation_bytes<=GC_HEAP_PAGE_BYTES||
           owner->reservation_bytes>GC_MAX_OFFSET||(owner->reservation_bytes&(GC_HEAP_PAGE_BYTES-1))||
           h->heap_base!=owner->reservation||h->pages!=owner->table||
           h->page_count!=(owner->reservation_bytes>>GC_HEAP_PAGE_SHIFT)-1||
           h->hard_max!=owner->reservation_bytes-GC_HEAP_PAGE_BYTES||
           h->page_count>owner->table_bytes/sizeof(AllocationPage)||h->committed||h->pending||
           h->metadata_pending||h->metadata_committed!=owner->table_bytes)return GC_NATIVE_METADATA_INVALID;
        const AllocationPage *pages=(void *)(uintptr_t)owner->table;
        for(uint64_t i=0;i<h->page_count;i++)
            if(atomic_load_explicit(&pages[i].state,memory_order_acquire)!=GC_PAGE_FREE||
               pages[i].metadata_leases||pages[i].starts||pages[i].forward_table)return GC_NATIVE_METADATA_INVALID;
    }
    owner->stage=4;
    if(owner->reservation){
        if(!ops->release((void *)(uintptr_t)owner->reservation,(size_t)owner->reservation_bytes))return GC_NATIVE_METADATA_OS_ERROR;
        owner->reservation=owner->heap.heap_base=0;
    }
    if(owner->table){
        if(!ops->metadata.unmap((void *)(uintptr_t)owner->table,(size_t)owner->table_bytes))return GC_NATIVE_METADATA_OS_ERROR;
        owner->table=owner->heap.pages=owner->heap.metadata_committed=0;
    }
    owner->heap=(HeapPool){0};owner->stage=3;return GC_NATIVE_METADATA_OK;
}
/* Only an exclusive, unpublished reservation lease may use these adapters.
 * The pool reservation remains charged throughout OS failures. No locks or
 * source guards may be held; attached callers must be in native state.
 * Bitmap allocation/publication is a separate transaction. */
static inline bool gc_native_pool_lease_valid(const HeapPool *heap,const PageLease *lease){
    if(!heap||((uintptr_t)heap&7)||!heap->heap_base||(heap->heap_base&15)||
       !heap->pages||(heap->pages&7)||!heap->page_count||
       heap->page_count>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
       heap->pages>UINT64_MAX-heap->page_count*sizeof(AllocationPage)||
       heap->heap_base>UINT64_MAX-((heap->page_count+1)<<GC_HEAP_PAGE_SHIFT)||!lease||((uintptr_t)lease&7)||lease->stage!=1||!lease->page_count||
       lease->page_count>heap->page_count||lease->head_index>heap->page_count-lease->page_count||
       lease->committed_mapping>1)return false;
    const AllocationPage *pages=(void *)(uintptr_t)heap->pages;
    for(uint64_t i=lease->head_index;i<lease->head_index+lease->page_count;i++){
        const AllocationPage *page=&pages[i];
        if(atomic_load_explicit(&page->state,memory_order_acquire)!=
           (i==lease->head_index?GC_PAGE_PREPARING:GC_PAGE_RUN_TAIL)||
           page->heap_base!=heap->heap_base||page->run_head!=lease->head_index||
           page->incarnation!=lease->incarnation||page->identity_base!=lease->identity_base||
           page->generation!=lease->generation||page->start!=((i+1)<<GC_HEAP_PAGE_SHIFT)||
           page->limit!=((lease->head_index+lease->page_count+1)<<GC_HEAP_PAGE_SHIFT))return false;
    }
    return true;
}
static inline bool gc_native_heap_lease_valid(NativeHeapOwner *owner,const PageLease *lease){
    return gc_native_pool_lease_valid(gc_native_heap_prepared(owner),lease);
}
static inline uint64_t gc_native_heap_commit_lease(NativeHeapOwner *owner,PageLease *lease){
    if(!gc_native_heap_lease_valid(owner,lease)||lease->committed_mapping)return GC_NATIVE_METADATA_INVALID;
    void *address=(void *)(uintptr_t)(owner->reservation+((lease->head_index+1)<<GC_HEAP_PAGE_SHIFT));
    size_t bytes=(size_t)(lease->page_count<<GC_HEAP_PAGE_SHIFT);
#ifdef _WIN32
    if(VirtualAlloc(address,bytes,MEM_COMMIT,PAGE_READWRITE)!=address)return GC_NATIVE_METADATA_OS_ERROR;
#else
    if(mprotect(address,bytes,PROT_READ|PROT_WRITE))return GC_NATIVE_METADATA_OS_ERROR;
#endif
    /* Explicit zeroing also makes reuse independent of OS discard semantics. */
    unsigned char *data=address;
    for(size_t i=0;i<bytes;i++)data[i]=0;
    lease->committed_mapping=1;return GC_NATIVE_METADATA_OK;
}
/* Internal OS leaf. Call only on an authenticated, exclusively owned range
 * within the reserved heap. Failure may leave it inaccessible; ownership and
 * accounting must remain held until retry completes. */
static inline bool gc_native_heap_decommit_range(void *address,size_t bytes){
#ifdef _WIN32
    if(!VirtualFree(address,bytes,MEM_DECOMMIT))return false;
#else
    /* On either failure keep the lease charged. A retry tolerates a range
     * already protected by the first call; never expose it before success. */
    if(mprotect(address,bytes,PROT_NONE))return false;
#ifdef __APPLE__
    /* Darwin DONTNEED deactivates; FREE permits discarding anonymous contents. */
    if(madvise(address,bytes,MADV_FREE))return false;
#else
    if(madvise(address,bytes,MADV_DONTNEED))return false;
#endif
#endif
    return true;
}
static inline uint64_t gc_native_heap_decommit_lease_with(NativeHeapOwner *owner,PageLease *lease,
        bool (*decommit)(void *,size_t)){
    if(!gc_native_heap_lease_valid(owner,lease)||!lease->committed_mapping)return GC_NATIVE_METADATA_INVALID;
    void *address=(void *)(uintptr_t)(owner->reservation+((lease->head_index+1)<<GC_HEAP_PAGE_SHIFT));
    size_t bytes=(size_t)(lease->page_count<<GC_HEAP_PAGE_SHIFT);
    if(!(decommit?decommit:gc_native_heap_decommit_range)(address,bytes))return GC_NATIVE_METADATA_OS_ERROR;
    lease->committed_mapping=0;return GC_NATIVE_METADATA_OK;
}
static inline uint64_t gc_native_heap_decommit_lease(NativeHeapOwner *owner,PageLease *lease){
    return gc_native_heap_decommit_lease_with(owner,lease,NULL);
}
#endif
