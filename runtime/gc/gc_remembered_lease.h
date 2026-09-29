/* A retained slot claim to its Old run-head metadata under the SAME registry/
 * heap lock as inventory, allocation and metadata leases. Native pool, request,
 * registration/set and unique claim storage are live/disjoint. No concurrent
 * ACK/reuse may occur. The claim identifies the page before an owner GcRef is
 * known: validate inventory membership, serial, epoch and exact run incarnation
 * before acquiring a lease. The output must be empty and acquires exactly one
 * release obligation. All failures preserve request, inventory and pool state.
 *
 * Only native metadata is read. No colored reference is invented, no start bit
 * or managed payload is read, and no liveness is inferred from the lease. The
 * run can contain scalar/header/padding slot candidates: a subsequent owner
 * lookup and canonical descriptor/liveness checks remain necessary. Physical
 * source mappings may be protected; this only retains native metadata. It
 * neither consumes the claim nor decides whether a dead owner can be dropped.
 */
#ifndef KLASSIC_GC_REMEMBERED_LEASE_H
#define KLASSIC_GC_REMEMBERED_LEASE_H
#include "gc_remembered_inventory.h"
#include "gc_metadata.h"
static inline uint64_t gc_remembered_lease_locked(MetadataLeasePool *g,RememberedLease *q){
    if(!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||q->lease.page||q->lease.incarnation||q->lease.token||
       !q->registration||(q->registration&7)||!q->claim||(q->claim&7))return GC_ML_INVALID;
    RememberedRegistration *r=(RememberedRegistration *)(uintptr_t)q->registration;
    RememberedInventoryRequest check={q->registration,q->claim,0};
    if(gc_remembered_inventory_step_locked((RememberedInventory *)(uintptr_t)r->registry,&check,GC_RI_VALIDATE)!=GC_RS_OK)return GC_ML_INVALID;
    RememberedSet *s=(RememberedSet *)(uintptr_t)r->set;RememberedJob *job=(RememberedJob *)(uintptr_t)q->claim;
    if(!job->serial||job->serial!=s->active_serial||job->offset!=s->active_offset||job->phase!=s->epoch||job->incarnation!=s->incarnation||
       !g->heap||(g->heap&7))return GC_ML_INVALID;
    HeapPool *h=(HeapPool *)(uintptr_t)g->heap;
    if(!h->pages||(h->pages&7)||!h->page_count||h->page_count>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
       h->pages>UINT64_MAX-h->page_count*sizeof(AllocationPage)||(h->heap_base&15)||
       h->heap_base>UINT64_MAX-((h->page_count+1)<<GC_HEAP_PAGE_SHIFT)||
       ((s->start|s->limit)&(GC_HEAP_PAGE_BYTES-1))||s->limit>((h->page_count+1)<<GC_HEAP_PAGE_SHIFT))return GC_ML_INVALID;
    uint64_t index=s->start>>GC_HEAP_PAGE_SHIFT;
    if(!index||index>h->page_count)return GC_ML_INVALID;
    index--;AllocationPage *page=(AllocationPage *)(uintptr_t)h->pages+index;
    if(page->run_head!=index||page->heap_base!=h->heap_base||page->start!=s->start||page->limit!=s->limit||
       page->generation!=GC_OLD||page->incarnation!=s->incarnation||!page->identity_base)return GC_ML_INVALID;
    MetadataLeaseRequest lease={(uintptr_t)page,s->incarnation,0};
    uint64_t result=gc_metadata_step_locked(g,&lease,GC_ML_ACQUIRE);
    if(result==GC_ML_OK)q->lease=lease;
    return result;
}
#endif
