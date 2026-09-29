/* Initialize fresh zeroed native storage before publication. The caller owns
 * the complete writable mapping and a disjoint immutable spec. No registered
 * readers may exist. This runs outside locks/guards, in native state when the
 * owner is attached; its cell initialization loop is not a stopped interval. */
#ifndef KLASSIC_GC_ROOT_STORAGE_H
#define KLASSIC_GC_ROOT_STORAGE_H
#include "gc_abi.h"
#define GC_ROOT_STORAGE_OK 0u
#define GC_ROOT_STORAGE_INVALID 1u
typedef uint64_t (*RootStoragePreparer)(RootStorage *,const RootStorageSpec *);
static inline uint64_t gc_root_storage_prepare(RootStorage *s,const RootStorageSpec *q){
    if(!s||((uintptr_t)s&7)||!q->owner||!q->cells||q->cells>UINT32_MAX||!q->entries||q->entries>UINT32_MAX)
        return GC_ROOT_STORAGE_INVALID;
    uint64_t required=sizeof(*s)+q->cells*sizeof(RootCell)+q->entries*sizeof(RootChange);
    if(q->bytes<required||(uintptr_t)s>UINT64_MAX-q->bytes)return GC_ROOT_STORAGE_INVALID;
    RootCell *cells=(RootCell *)(s+1);
    s->registration.segment=(uintptr_t)&s->segment;
    atomic_init(&s->registration.cleanup_cursor,0);
    s->segment.owner=q->owner;s->segment.cells=(uintptr_t)cells;s->segment.capacity=q->cells;
    s->segment.journal=(uintptr_t)&s->journal;
    atomic_init(&s->segment.used,0);atomic_init(&s->segment.state,GC_ROOT_SEGMENT_PREPARED);
    s->journal.entries=(uintptr_t)(cells+q->cells);s->journal.capacity=q->entries;
    atomic_init(&s->journal.published,0);atomic_init(&s->journal.consumed,0);
    for(uint64_t i=0;i<q->cells;i++)atomic_init(&cells[i].value,0);
    return GC_ROOT_STORAGE_OK;
}
/* Exclusive backing owner; exclude concurrent attachment/reuse. After this
 * owner retires the segment, FINISH's final release to PREPARED publishes all
 * cleaner writes. Check that receipt BEFORE any non-atomic registration word.
 * Never use this to race a fresh attachment or infer ownership of a mapping. */
typedef uint64_t (*RootStorageDisposalCheck)(const RootStorage *);
static inline uint64_t gc_root_storage_can_dispose(const RootStorage *storage){
    if(!storage||((uintptr_t)storage&7))return GC_ROOT_STORAGE_INVALID;
    const RootRegistration *r=&storage->registration;const RootSegment *s=&storage->segment;
    if(atomic_load_explicit(&s->state,memory_order_acquire)!=GC_ROOT_SEGMENT_PREPARED)
        return GC_ROOT_STORAGE_INVALID;
    if((r->segment&&r->segment!=(uintptr_t)s)||r->scope||r->registered||r->leases||r->limit||r->cursor||
        r->phase||r->required_mask||r->claim_serial||r->claim_end||r->previous||r->next||
        r->inventory_previous||r->inventory_next||r->context||r->owner_previous||r->owner_next||
        r->journal_serial||r->journal_end||atomic_load_explicit(&r->cleanup_cursor,memory_order_acquire)||
        s->scope||s->registration||atomic_load_explicit(&s->used,memory_order_acquire))
        return GC_ROOT_STORAGE_INVALID;
    uint64_t consumed=atomic_load_explicit(&storage->journal.consumed,memory_order_acquire);
    uint64_t published=atomic_load_explicit(&storage->journal.published,memory_order_acquire);
    if(consumed!=published||storage->journal.top!=published)return GC_ROOT_STORAGE_INVALID;
    return GC_ROOT_STORAGE_OK;
}
#endif
