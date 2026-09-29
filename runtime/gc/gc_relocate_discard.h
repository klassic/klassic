/* Bounded private-copy cleanup, heap lock held. Caller retains the winning
 * relocation permit, source metadata/strong liveness and both mappings, and
 * exclusively owns target append/cleanup until this request finishes. Source
 * SELECT/CHECK must already have drained all actors. No installed target may
 * be discarded: its unchanged allocation top is checked on every call.
 * The stable unique control and copy inputs cannot be copied/rewound/changed.
 * Clear at most 256 bytes per call; retain/reopen the source and release the
 * permit only after DONE. The copy receipt stays intact for diagnostics.
 * No allocation, polling, OS cleanup or metadata/accounting mutation occurs.
 */
#ifndef KLASSIC_GC_RELOCATE_DISCARD_H
#define KLASSIC_GC_RELOCATE_DISCARD_H
#include "gc_relocate.h"
enum { GC_DISCARD_DONE=0,GC_DISCARD_INVALID=1,GC_DISCARD_MORE=2 };
static inline uint64_t gc_relocate_discard_locked(AllocationPage *p,RelocationDiscard *q){
    if(!q->source||(q->source&7)||!q->copy||(q->copy&7)||
        (q->owner?q->owner!=(uintptr_t)q:q->cursor||q->bytes))return GC_DISCARD_INVALID;
    ForwardCell *source=(ForwardCell *)(uintptr_t)q->source;
    RelocationCopy *copy=(RelocationCopy *)(uintptr_t)q->copy;
    if(source->source_offset<48||(source->source_offset&15)||source->source_offset>GC_MAX_OFFSET||
        !copy->ticket||copy->ticket>(UINT64_MAX>>2)||!source->incarnation||copy->source_incarnation!=source->incarnation||
        atomic_load_explicit(&source->word,memory_order_acquire)!=((copy->ticket<<2)|1)||
        atomic_load_explicit(&source->access,memory_order_acquire)!=GC_SOURCE_CLOSED||copy->stage>2)return GC_DISCARD_INVALID;
    uint64_t top=atomic_load_explicit(&p->allocated_top,memory_order_acquire),state=atomic_load_explicit(&p->state,memory_order_acquire);
    if((state!=GC_PAGE_READY&&state!=GC_PAGE_CLOSED)||p->owner||!p->incarnation||
        !p->start||p->start>=p->limit||p->limit>GC_MAX_OFFSET||((p->heap_base|p->start|p->limit|top)&15)||
        top<p->start||top>p->limit||(state==GC_PAGE_READY&&top!=p->start)||
        p->heap_base>UINT64_MAX-p->limit||copy->heap_base!=p->heap_base)return GC_DISCARD_INVALID;
    if(copy->destination){
        if(copy->destination!=top+sizeof(ObjectHeader)||copy->destination_limit!=p->limit||p->limit-top<48)return GC_DISCARD_INVALID;
    }else if(copy->destination_limit||copy->stage)return GC_DISCARD_INVALID;
    uint64_t bytes=0;
    if(copy->stage){
        uint64_t offset=source->source_offset;
        if(offset<48||(offset&15)||copy->source_limit>GC_MAX_OFFSET||offset>=copy->source_limit||
            copy->heap_base>UINT64_MAX-copy->source_limit)return GC_DISCARD_INVALID;
        const ObjectHeader *h=(const ObjectHeader *)(uintptr_t)(copy->heap_base+offset-sizeof(ObjectHeader));
        if(atomic_load_explicit(&h->state,memory_order_acquire)!=GC_OBJECT_PUBLISHED||h->payload_bytes>copy->source_limit-offset||
            !h->descriptor_id||h->descriptor_id>UINT32_MAX||!h->mark_record||(h->mark_record&7))return GC_DISCARD_INVALID;
        uint64_t rounded=(h->payload_bytes+15)&~UINT64_C(15);if(!rounded)rounded=16;
        if(rounded>copy->source_limit-offset||rounded>p->limit-copy->destination||
            (offset+rounded>top&&copy->destination+rounded>offset-sizeof(ObjectHeader))||
            (copy->stage==1?(!copy->cursor||copy->cursor>=h->payload_bytes):copy->cursor!=h->payload_bytes))return GC_DISCARD_INVALID;
        bytes=sizeof(ObjectHeader)+rounded;
    }else if(copy->cursor)return GC_DISCARD_INVALID;
    if((q->owner&&q->bytes!=bytes)||q->cursor>bytes||(q->cursor&15))return GC_DISCARD_INVALID;
    uint64_t count=bytes-q->cursor;if(count>256)count=256;
    if(count){unsigned char *destination=(unsigned char *)(uintptr_t)(p->heap_base+top+q->cursor);for(uint64_t i=0;i<count;i++)destination[i]=0;}
    q->owner=(uintptr_t)q;q->bytes=bytes;q->cursor+=count;
    return q->cursor==bytes?GC_DISCARD_DONE:GC_DISCARD_MORE;
}
#endif
