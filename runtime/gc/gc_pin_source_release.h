/* Pin-aware physical source release under the heap lock. The scheduler retains
 * the borrowed metadata lease and exclusive page selection throughout all
 * slices and OS cleanup. Native controls are unique, stable and not copied.
 * Captured registry/page/incarnation/epoch reject cursor reuse after selection
 * reopens. Restart from a fresh cursor after recapturing the complete inventory.
 * Uncommit BEGIN retains selection across unlocked OS work; clear the embedded
 * committed_mapping only after OS success, then FINISH under the heap lock.
 * These steps do not prove mark termination, source/color retirement, or permit
 * metadata/virtual-address reuse. All ordinary scheduler paths must use the
 * same pin inventory. No OS call, allocation or managed access occurs here.
 */
#ifndef KLASSIC_GC_PIN_SOURCE_RELEASE_H
#define KLASSIC_GC_PIN_SOURCE_RELEASE_H
#include "gc_pin_registry.h"
#include "gc_source_drain.h"
#include "gc_source_uncommit.h"
static inline uint64_t gc_pin_source_drain_locked(PinPageRegistry *r,const MetadataLeaseRequest *lease,
    const RelocationGateRequest *selection,PinSourceDrain *q){
    if(!q||((uintptr_t)q&7))return GC_SD_INVALID;
    uint64_t selected=gc_pin_selection_locked(r,lease,selection);
    if(selected!=GC_RG_OK)return selected==GC_RG_WAIT?GC_SD_WAIT:GC_SD_INVALID;
    if(q->epoch){
        if(q->registry!=(uintptr_t)r||q->page!=lease->page||q->incarnation!=lease->incarnation||
           q->epoch!=selection->epoch)return GC_SD_INVALID;
    }else if(q->registry||q->page||q->incarnation||q->scan.cursor)return GC_SD_INVALID;
    uint64_t status=gc_source_drain((void *)(uintptr_t)lease->page,&q->scan);
    if(status!=GC_SD_INVALID&&!q->epoch){
        q->registry=(uintptr_t)r;q->page=lease->page;q->incarnation=lease->incarnation;q->epoch=selection->epoch;
    }
    return status;
}
static inline uint64_t gc_pin_source_uncommit_locked(PinPageRegistry *r,const MetadataLeaseRequest *lease,
    const RelocationGateRequest *selection,PinSourceUncommit *q,uint64_t op){
    if(!q||((uintptr_t)q&7)||op>GC_SU_FINISH||gc_pin_selection_locked(r,lease,selection)!=GC_RG_OK)
        return GC_SU_INVALID;
    if(op==GC_SU_BEGIN){if(q->registry||q->epoch)return GC_SU_INVALID;}
    else if(q->registry!=(uintptr_t)r||q->epoch!=selection->epoch)return GC_SU_INVALID;
    HeapPool *heap=(void *)(uintptr_t)r->pool->heap;
    if(q->work.head_index!=(lease->page-heap->pages)/sizeof(AllocationPage)||
       q->work.incarnation!=lease->incarnation)return GC_SU_INVALID;
    uint64_t status=gc_source_uncommit_locked(heap,&q->work,op);
    if(status==GC_SU_OK&&op==GC_SU_BEGIN){q->registry=(uintptr_t)r;q->epoch=selection->epoch;}
    return status;
}
static inline uint64_t gc_pin_source_drain_entry_locked(PinPageRegistry *r,const PinSourceRequest *q){
    if(!q||((uintptr_t)q&7))return GC_SD_INVALID;
    return gc_pin_source_drain_locked(r,(void *)(uintptr_t)q->lease,
        (void *)(uintptr_t)q->selection,(void *)(uintptr_t)q->work);
}
static inline uint64_t gc_pin_source_uncommit_entry_locked(PinPageRegistry *r,const PinSourceRequest *q,uint64_t op){
    if(!q||((uintptr_t)q&7))return GC_SU_INVALID;
    return gc_pin_source_uncommit_locked(r,(void *)(uintptr_t)q->lease,
        (void *)(uintptr_t)q->selection,(void *)(uintptr_t)q->work,op);
}
#endif
