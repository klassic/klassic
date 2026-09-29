/* Runtime-owned, nonmoving page pin inventory. Exactly one registry covers a
 * heap, with one record per page-table index; run tails use the head record.
 * Storage is prepared outside locks and retained through all users. The heap
 * lock serializes this inventory, metadata leases and relocation selection.
 * A record owns its own metadata lease, independent of the caller's lease.
 * Binding does not keep objects alive: a public pin must also own a strong root.
 * All page selection/retirement callers must consult this same inventory.
 * Record/registry layouts are shared with the native emitter.
 */
#ifndef KLASSIC_GC_PIN_REGISTRY_H
#define KLASSIC_GC_PIN_REGISTRY_H
#include "gc_pin_page.h"
#include "gc_metadata.h"
#define GC_PR_FULL 4u
/* Authenticate a borrowed lease without modifying it. Output is private. */
static inline PinPageRecord *gc_pin_record_locked(PinPageRegistry *r,
                                                  const MetadataLeaseRequest *lease){
    if(!r||((uintptr_t)r&7)||!r->pool||((uintptr_t)r->pool&7)||
       !r->records||((uintptr_t)r->records&7)||!r->capacity||
       r->capacity>UINT64_MAX/sizeof(PinPageRecord)||
       (uintptr_t)r->records>UINT64_MAX-r->capacity*sizeof(PinPageRecord)||
       !lease||((uintptr_t)lease&7))return NULL;
    MetadataLeaseRequest resolved=*lease;
    if(gc_metadata_step_locked(r->pool,&resolved,GC_ML_RESOLVE)!=GC_ML_OK||
       resolved.page!=lease->page||resolved.incarnation!=lease->incarnation)return NULL;
    HeapPool *heap=(void *)(uintptr_t)r->pool->heap;
    if(r->capacity!=heap->page_count)return NULL;
    uint64_t index=(resolved.page-heap->pages)/sizeof(AllocationPage);
    return &r->records[index];
}
static inline uint64_t gc_pin_registry_bind_locked(PinPageRegistry *r,
                                                   const MetadataLeaseRequest *lease){
    PinPageRecord *record=gc_pin_record_locked(r,lease);
    if(!record)return GC_RG_INVALID;
    if(record->lifetime.page||record->lifetime.incarnation||record->lifetime.token||
       record->admission.head||record->admission.active||record->admission.closed)
        return GC_RG_INVALID;
    AllocationPage *page=(void *)(uintptr_t)lease->page;
    uint64_t state=atomic_load_explicit(&page->state,memory_order_acquire);
    if(state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED)return GC_RG_INVALID;
    /* Preserve serial/epoch across unbind/rebind, including the same page
     * incarnation, so old requests cannot authenticate a new permit. */
    if(record->admission.epoch==UINT64_MAX)return GC_RG_EXHAUSTED;
    MetadataLeaseRequest lifetime={lease->page,lease->incarnation,0};
    uint64_t status=gc_metadata_step_locked(r->pool,&lifetime,GC_ML_ACQUIRE);
    if(status!=GC_ML_OK)return status==GC_ML_FULL?GC_PR_FULL:
        status==GC_ML_EXHAUSTED?GC_RG_EXHAUSTED:GC_RG_INVALID;
    record->lifetime=lifetime;record->admission.epoch++;return GC_RG_OK;
}
static inline PinPageRecord *gc_pin_bound_record_locked(PinPageRegistry *r,
                                                        const MetadataLeaseRequest *lease){
    PinPageRecord *record=gc_pin_record_locked(r,lease);
    if(!record||record->lifetime.page!=lease->page||
       record->lifetime.incarnation!=lease->incarnation)return NULL;
    MetadataLeaseRequest held=record->lifetime;
    if(gc_metadata_step_locked(r->pool,&held,GC_ML_RESOLVE)!=GC_ML_OK||
       held.page!=record->lifetime.page||held.incarnation!=record->lifetime.incarnation)return NULL;
    return record;
}
static inline uint64_t gc_pin_registry_step_locked(PinPageRegistry *r,
    const MetadataLeaseRequest *lease,RelocationGateRequest *q,uint64_t op){
    PinPageRecord *record=gc_pin_bound_record_locked(r,lease);
    if(!record)return GC_RG_INVALID;
    if(op==GC_PP_PIN||op==GC_PP_SELECT){
        AllocationPage *page=(void *)(uintptr_t)lease->page;
        uint64_t state=atomic_load_explicit(&page->state,memory_order_acquire);
        if(state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED)return GC_RG_INVALID;
    }
    return gc_pin_page_locked(&record->admission,q,op);
}
/* Authenticate the exclusive selection epoch under the same heap lock. */
static inline uint64_t gc_pin_selection_locked(PinPageRegistry *r,const MetadataLeaseRequest *lease,
                                               const RelocationGateRequest *selection){
    PinPageRecord *record=gc_pin_bound_record_locked(r,lease);
    if(!record||!selection||((uintptr_t)selection&7)||selection->permit||selection->serial||
       !selection->epoch||selection->epoch!=record->admission.epoch)return GC_RG_INVALID;
    return gc_relocation_gate_locked(&record->admission,NULL,GC_RG_CHECK);
}
/* Only the inventory owner may unbind after excluding future acquisitions.
 * Failed lease release retains the entire record for cleanup retry. */
static inline uint64_t gc_pin_registry_unbind_locked(PinPageRegistry *r,
                                                     const MetadataLeaseRequest *lease){
    PinPageRecord *record=gc_pin_bound_record_locked(r,lease);
    if(!record)return GC_RG_INVALID;
    if(record->admission.active||record->admission.head||record->admission.closed)return GC_RG_WAIT;
    return gc_metadata_step_locked(r->pool,&record->lifetime,GC_ML_RELEASE)==GC_ML_OK?
        GC_RG_OK:GC_RG_INVALID;
}
#endif
