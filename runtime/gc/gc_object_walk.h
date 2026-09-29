/* One immutable object key per call under the heap/metadata lock. This borrows
 * an existing page lease, authenticating its token on every call; it never
 * acquires or transfers a lease and never reads managed payload. Native inputs
 * and table backing remain live/disjoint under the page lease. Caller captures
 * page/incarnation/table/used under the lock and starts cursor at zero.
 * ITEM advances cursor and returns only a byte offset. Consume then clear that
 * output before retry. It is NOT a rooted GcRef, liveness proof or raw address.
 * Forwarded/claimed/retained keys are also enumerated; the owner must separately
 * resolve and guard live payloads or inspect retained forwarding dependencies.
 *
 * An appended table returns WAIT without changing state, including at end.
 * Owner must restart with the new used count or exclude appends for the whole
 * audit. Metadata leases exclude retirement, not append or payload movement.
 * The whole-heap phase/retirement obligations of gc_heap_walk.h still apply.
 * DONE covers this captured key sequence only. INVALID leaves all state intact.
 */
#ifndef KLASSIC_GC_OBJECT_WALK_H
#define KLASSIC_GC_OBJECT_WALK_H
#include "gc_metadata.h"
enum { GC_OW_ITEM=0,GC_OW_DONE=1,GC_OW_INVALID=2,GC_OW_WAIT=3 };
typedef uint64_t (*ObjectWalker)(MetadataLeasePool *,ObjectWalk *);
static inline uint64_t gc_object_walk_locked(MetadataLeasePool *g,ObjectWalk *q){
    if(!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||!q->lease||(q->lease&7)||q->source_offset)return GC_OW_INVALID;
    const MetadataLeaseRequest *lease=(const void *)(uintptr_t)q->lease;
    if(lease->page!=q->page||lease->incarnation!=q->incarnation)return GC_OW_INVALID;
    MetadataLeaseRequest checked={.token=lease->token};
    if(gc_metadata_step_locked(g,&checked,GC_ML_RESOLVE)!=GC_ML_OK||
       checked.page!=q->page||checked.incarnation!=q->incarnation)return GC_OW_INVALID;
    const AllocationPage *p=(const void *)(uintptr_t)checked.page;
    uint64_t state=atomic_load_explicit(&p->state,memory_order_acquire);
    if((state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED&&state!=GC_PAGE_EVACUATED&&state!=GC_PAGE_UNCOMMITTED)||
       !p->start||p->start>=p->limit||p->limit>GC_MAX_OFFSET||((p->start|p->limit)&15)||
       !q->table||(q->table&7)||q->table!=p->forward_table)return GC_OW_INVALID;
    const ForwardTable *t=(const void *)(uintptr_t)q->table;
    if(!t->cells||(t->cells&7)||!t->capacity||t->capacity>UINT32_MAX||
       t->cells>UINT64_MAX-t->capacity*sizeof(ForwardCell)||q->used>t->capacity||q->cursor>q->used)return GC_OW_INVALID;
    uint64_t used=atomic_load_explicit(&t->used,memory_order_acquire);
    if(used>t->capacity)return GC_OW_INVALID;
    if(q->used!=used)return GC_OW_WAIT;
    if(q->cursor==q->used)return GC_OW_DONE;
    const ForwardCell *cells=(const void *)(uintptr_t)t->cells,*cell=&cells[q->cursor];
    uint64_t top=atomic_load_explicit(&p->allocated_top,memory_order_acquire),offset=cell->source_offset;
    if(top<p->start||top>p->limit||(top&15)||offset<p->start+sizeof(ObjectHeader)||offset>=top||(offset&15)||
       cell->incarnation!=q->incarnation||(q->cursor&&cells[q->cursor-1].source_offset>=offset))return GC_OW_INVALID;
    q->source_offset=offset;q->cursor++;return GC_OW_ITEM;
}
#endif
