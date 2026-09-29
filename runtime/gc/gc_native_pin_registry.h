/* Runtime-owned pin inventory backing, one package per generational heap.
 * Reserve the rounded metadata charge under the heap mutex; map and initialize
 * outside locks/guards in native state, then COMMIT before publication. The
 * metadata pool, heap and page table must outlive the package. Only the runtime
 * bootstrap/shutdown owner may publish or dispose it; quiesce all users first.
 * Every bound record must be unbound before disposal. An empty inventory is not
 * proof that borrowed control pointers have stopped existing. Failed unmapping
 * keeps both backing and budget charge; RELEASE/ABORT follows successful unmap.
 * This owner is for the offset-reference heap, not the legacy absolute heap.
 */
#ifndef KLASSIC_GC_NATIVE_PIN_REGISTRY_H
#define KLASSIC_GC_NATIVE_PIN_REGISTRY_H
#include "gc_native_metadata.h"
#include "gc_pin_registry.h"
static inline uint64_t gc_native_pin_registry_size(uint64_t pages,uint64_t quantum,uint64_t *bytes){
    if(!pages||pages>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
       pages>(UINT64_MAX-sizeof(PinPageRegistry))/sizeof(PinPageRecord))return GC_NATIVE_METADATA_INVALID;
    return gc_native_metadata_size(sizeof(PinPageRegistry)+pages*sizeof(PinPageRecord),quantum,bytes);
}
static inline uint64_t gc_native_pin_registry_prepare(MetadataCharge *charge,MetadataLeasePool *pool,
                                                       const NativeMetadataOps *ops){
    if(!gc_native_metadata_charge_valid(charge,ops)||!pool||((uintptr_t)pool&7)||
       !pool->heap||(pool->heap&7)||pool->heap!=charge->owner)return GC_NATIVE_METADATA_INVALID;
    const HeapPool *heap=(void *)(uintptr_t)pool->heap;uint64_t bytes=0;
    if(!heap->pages||(heap->pages&7)||heap->page_count>UINT64_MAX/sizeof(AllocationPage)||
       heap->pages>UINT64_MAX-heap->page_count*sizeof(AllocationPage)||
       gc_native_pin_registry_size(heap->page_count,ops->quantum,&bytes)!=GC_NATIVE_METADATA_OK||
       charge->bytes<bytes)return GC_NATIVE_METADATA_INVALID;
    uint64_t status=gc_native_metadata_prepare(charge,ops);
    if(status!=GC_NATIVE_METADATA_OK)return status;
    PinPageRegistry *registry=(void *)(uintptr_t)charge->allocation;
    *registry=(PinPageRegistry){pool,(PinPageRecord *)(registry+1),heap->page_count};
    for(uint64_t i=0;i<registry->capacity;i++)registry->records[i]=(PinPageRecord){0};
    return GC_NATIVE_METADATA_OK;
}
/* Fixed-prefix lookup after COMMIT; does not scan page records or allocate. */
static inline PinPageRegistry *gc_native_pin_registry_committed(const MetadataCharge *charge){
    if(!charge||((uintptr_t)charge&7)||charge->stage!=2||!charge->owner||(charge->owner&7)||
       !charge->allocation||(charge->allocation&7)||charge->bytes<sizeof(PinPageRegistry)||
       charge->allocation>UINT64_MAX-charge->bytes)return NULL;
    PinPageRegistry *registry=(void *)(uintptr_t)charge->allocation;
    if(!registry->pool||((uintptr_t)registry->pool&7)||registry->pool->heap!=charge->owner||
       registry->records!=(PinPageRecord *)(registry+1)||!registry->capacity||
       registry->capacity>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
       registry->capacity>(charge->bytes-sizeof(*registry))/sizeof(PinPageRecord))return NULL;
    const HeapPool *heap=(void *)(uintptr_t)charge->owner;
    return registry->capacity==heap->page_count?registry:NULL;
}
static inline uint64_t gc_native_pin_registry_dispose(MetadataCharge *charge,const NativeMetadataOps *ops){
    if(!gc_native_metadata_charge_valid(charge,ops)||(charge->stage!=1&&charge->stage!=2)||
       !charge->allocation||(charge->allocation&(ops->quantum-1)))return GC_NATIVE_METADATA_INVALID;
    /* The same geometry validation applies to successfully prepared private
     * backing before COMMIT. Never publish this temporary committed view. */
    MetadataCharge view=*charge;view.stage=2;
    PinPageRegistry *registry=gc_native_pin_registry_committed(&view);
    if(!registry)return GC_NATIVE_METADATA_INVALID;
    for(uint64_t i=0;i<registry->capacity;i++){
        PinPageRecord *record=&registry->records[i];
        if(record->lifetime.page||record->lifetime.incarnation||record->lifetime.token||
           record->admission.head||record->admission.active||record->admission.closed)
            return GC_NATIVE_METADATA_INVALID;
    }
    return gc_native_metadata_dispose(charge,ops);
}
#endif
