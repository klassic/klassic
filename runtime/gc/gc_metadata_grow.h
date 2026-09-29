/* Bounded admission of a prepared tail into a stable metadata-lease table.
 * Caller holds the heap/lease lock and has already reserved, committed, zeroed
 * and budgeted backing through new_capacity. The slots address never moves;
 * existing tokens keep their indices/generations. No allocation, OS call, poll
 * or unbounded scan occurs here. At most 256 new cells are checked then linked.
 * Failure preserves the pool and entire tail. Success changes only new free
 * links, free_head and capacity; active holders/pages are untouched.
 */
#ifndef KLASSIC_GC_METADATA_GROW_H
#define KLASSIC_GC_METADATA_GROW_H
#include "gc_metadata.h"
static inline uint64_t gc_metadata_grow_locked(MetadataLeasePool *g,uint64_t new_capacity){
    if(!g||((uintptr_t)g&7)||!g->heap||(g->heap&7)||!g->slots||(g->slots&7)||
        !g->capacity||g->capacity>UINT32_MAX||new_capacity<=g->capacity||new_capacity>UINT32_MAX||
        new_capacity-g->capacity>256||g->slots>UINT64_MAX-new_capacity*sizeof(MetadataLeaseSlot)||
        g->active>g->capacity||g->free_head>g->capacity||!!g->free_head!=(g->active<g->capacity))return GC_ML_INVALID;
    MetadataLeaseSlot *slots=(MetadataLeaseSlot *)(uintptr_t)g->slots;
    for(uint64_t i=g->capacity;i<new_capacity;i++)
        if(slots[i].page||slots[i].incarnation||slots[i].generation||slots[i].next_free)return GC_ML_INVALID;
    for(uint64_t i=g->capacity;i<new_capacity;i++)slots[i].next_free=i+1<new_capacity?i+2:g->free_head;
    g->free_head=g->capacity+1;g->capacity=new_capacity;
    return GC_ML_OK;
}
#endif
