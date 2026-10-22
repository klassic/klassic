/* One-page enumeration under the same heap lock as allocation/metadata leases.
 * Caller captures pool/heap/pages/page_count in a unique zero-cursor request and
 * retains those native controls through the whole walk. Geometry cannot change.
 * PAGE advances cursor and transfers one metadata lease into q.lease. Consume
 * the page outside the lock as appropriate, then release that exact lease under
 * the lock before NEXT. Metadata lifetime is not payload access or liveness.
 * All other results own no new lease. FULL/EXHAUSTED/WAIT/INVALID preserve q.
 * Empty READY and FREE pages and large-run tails consume one MORE step. Pending
 * preparation/release (or an installed private READY payload) returns WAIT.
 * Published run heads, including evacuated/uncommitted sources, return PAGE.
 *
 * This is enumeration, not a heap snapshot or a retirement certificate. Owner
 * phase obligations must exclude range reuse and introduction of references
 * into the audited range across the whole walk. New allocations/relocations and
 * root/buffer/job/handle/forwarding work need the owning cycle's protocol.
 * No managed memory, OS call, polling or unbounded table traversal occurs here.
 */
#ifndef KLASSIC_GC_HEAP_WALK_H
#define KLASSIC_GC_HEAP_WALK_H
#include "gc_metadata.h"
enum { GC_HW_PAGE=0,GC_HW_MORE=1,GC_HW_DONE=2,GC_HW_INVALID=3,GC_HW_FULL=4,GC_HW_EXHAUSTED=5,GC_HW_WAIT=6 };
typedef uint64_t (*HeapWalker)(MetadataLeasePool *,GcHeapWalk *);
static inline uint64_t gc_heap_walk_locked(MetadataLeasePool *g,GcHeapWalk *q){
    if(!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||q->pool!=(uintptr_t)g||
       !g->heap||(g->heap&7)||q->heap!=g->heap||q->lease.page||q->lease.incarnation||q->lease.token)return GC_HW_INVALID;
    HeapPool *h=(HeapPool *)(uintptr_t)g->heap;
    if(!h->pages||(h->pages&7)||!h->page_count||h->page_count>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
       h->pages>UINT64_MAX-h->page_count*sizeof(AllocationPage)||q->pages!=h->pages||
       q->page_count!=h->page_count||q->cursor>h->page_count)return GC_HW_INVALID;
    if(q->cursor==h->page_count)return GC_HW_DONE;
    AllocationPage *pages=(AllocationPage *)(uintptr_t)h->pages,*p=&pages[q->cursor];
    uint64_t state=atomic_load_explicit(&p->state,memory_order_acquire);
    if(state>GC_PAGE_UNCOMMITTED)return GC_HW_INVALID;
    if(state==GC_PAGE_PREPARING||state==GC_PAGE_RELEASING)return GC_HW_WAIT;
    if(state==GC_PAGE_FREE||state==GC_PAGE_READY||state==GC_PAGE_RUN_TAIL){
        if(p->metadata_leases)return GC_HW_INVALID;
        if(state==GC_PAGE_READY&&atomic_load_explicit(&p->allocated_top,memory_order_acquire)!=p->start)return GC_HW_WAIT;
        if(state==GC_PAGE_RUN_TAIL){
            if(p->run_head>=q->cursor||!p->incarnation)return GC_HW_INVALID;
            AllocationPage *head=&pages[p->run_head];
            uint64_t head_state=atomic_load_explicit(&head->state,memory_order_acquire);
            if(head->run_head!=p->run_head||head->incarnation!=p->incarnation||
               head_state==GC_PAGE_FREE||head_state==GC_PAGE_RUN_TAIL||head_state>GC_PAGE_UNCOMMITTED)return GC_HW_INVALID;
        }
        q->cursor++;return GC_HW_MORE;
    }
    MetadataLeaseRequest lease={(uintptr_t)p,p->incarnation,0};
    uint64_t result=gc_metadata_step_locked(g,&lease,GC_ML_ACQUIRE);
    if(result!=GC_ML_OK)return result==GC_ML_FULL?GC_HW_FULL:result==GC_ML_EXHAUSTED?GC_HW_EXHAUSTED:GC_HW_INVALID;
    q->lease=lease;q->cursor++;return GC_HW_PAGE;
}
#endif
