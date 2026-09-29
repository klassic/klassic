/* Claim an object only while its page has exclusive selection admission.
 * The page lease, selection request and mark record remain owner-private.
 * The claim's ordinary source lifetime/mark/termination obligations still
 * apply. Hold page selection until every selected object's mover finishes;
 * releasing an individual mover does not reopen the page to native pins.
 */
#ifndef KLASSIC_GC_PIN_RELOCATION_H
#define KLASSIC_GC_PIN_RELOCATION_H
#include "gc_pin_registry.h"
#include "gc_relocation_claim.h"
static inline uint64_t gc_pin_marked_relocation_claim_locked(PinPageRegistry *r,
    const MetadataLeaseRequest *lease,const RelocationGateRequest *selection,
    RelocationGate *movers,MarkedRelocation *q){
    if(!q||((uintptr_t)q&7)||!q->queue||(q->queue&7))return GC_RCL_INVALID;
    uint64_t selected=gc_pin_selection_locked(r,lease,selection);
    if(selected!=GC_RG_OK)return selected==GC_RG_WAIT?GC_RCL_CLOSED:GC_RCL_INVALID;
    AllocationPage *page=(void *)(uintptr_t)lease->page;
    const MarkQueue *queue=(void *)(uintptr_t)q->queue;
    if(atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_CLOSED||
       page->generation>GC_OLD||queue->generation!=page->generation||
       q->claim.incarnation!=lease->incarnation||page->start>UINT64_MAX-sizeof(ObjectHeader)||
       q->claim.source_offset<page->start+sizeof(ObjectHeader)||
       q->claim.source_offset>=atomic_load_explicit(&page->allocated_top,memory_order_acquire)||
       q->claim.source_offset>=page->limit)return GC_RCL_INVALID;
    return gc_marked_relocation_claim_locked(movers,q);
}
/* Two-argument native entry; this control neither acquires nor transfers leases. */
static inline uint64_t gc_pin_relocation_claim_locked(PinPageRegistry *r,const PinRelocationClaim *q){
    if(!q||((uintptr_t)q&7))return GC_RCL_INVALID;
    return gc_pin_marked_relocation_claim_locked(r,(void *)(uintptr_t)q->lease,
        (void *)(uintptr_t)q->selection,(void *)(uintptr_t)q->movers,(void *)(uintptr_t)q->work);
}
#endif
