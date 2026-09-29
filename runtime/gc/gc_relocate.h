/* Bounded physical copy owned by the unique forwarding claimant. Native
 * request/cell storage is live, disjoint and owner-private. The caller retains
 * source metadata + liveness and an exclusively owned, committed destination
 * reservation until publication or abandonment. Limits are immutable committed
 * reservation ends, not a substitute for those lifetime/ownership obligations.
 *
 * The source gate must be closed/drained by ALL actors; checking its acquire
 * word orders their writes before these non-atomic byte reads. The source stays
 * closed across calls. No raw address escapes a call; every slice recomputes it.
 * Source and private candidate remain protected across any between-slice poll.
 * A request cannot be copied, replayed, rewound, or concurrently advanced.
 *
 * Stage 0 requires a zero candidate header. Stage 1 advances an unfinished
 * payload; stage 2 is complete (idempotent while this claim is still owned).
 * Each successful step copies at most budget <= 256 payload bytes, plus fixed
 * header initialization. The logical MarkRecord pointer is shared, never copied
 * or reset. Completion release-publishes the destination header, NOT forwarding.
 * Caller must install destination start/table metadata and promotion/remset
 * state before forwarding publication. Partial/losing candidates stay private
 * and are reclaimed before source retention. No allocation or GC poll here.
 */
#ifndef KLASSIC_GC_RELOCATE_H
#define KLASSIC_GC_RELOCATE_H
#include "gc_abi.h"
#include "gc_forward.h"
#define GC_COPY_MAX_BYTES UINT64_C(256)
enum { GC_COPY_DONE=0, GC_COPY_INVALID=1, GC_COPY_MORE=2, GC_COPY_WAIT=3 };
enum { GC_TARGET_PREPARE=0, GC_TARGET_INSTALL=1 };
enum { GC_TARGET_OK=0, GC_TARGET_INVALID=1, GC_TARGET_REFILL=2, GC_TARGET_METADATA_REFILL=3 };
/* Heap lock held, and this collector exclusively owns the target run from
 * PREPARE through INSTALL or private-range cleanup. Readers may lease already
 * installed objects; no allocator, selector or other copier may append to this
 * run during that ownership. This is an external scheduler obligation, just as
 * retaining the committed mapping is; a request is not an ownership token.
 * The pool committed/zeroed the run and admitted its stable forward table.
 * PREPARE derives a private candidate from the current top without publishing
 * it. Copy checks its actual size against the returned end. INSTALL publishes
 * only a complete header, then table -> bitmap -> top -> Closed. It never
 * creates a MarkRecord or changes logical identity. Before forwarding, the
 * caller must also finish generation-specific mark/remset handoff.
 * Rejection changes nothing. Abandonment before INSTALL keeps top unchanged;
 * the owner must clear its dirty private range before reusing it (the bounded
 * gc_relocate_discard_locked cursor provides that step). An installed
 * candidate cannot be rolled back by this API. No OS calls, polling or locks.
 */
static inline uint64_t gc_relocate_target_locked(AllocationPage *p,RelocationCopy *q,uint64_t op) {
    uint64_t state=atomic_load_explicit(&p->state,memory_order_acquire);
    uint64_t top=atomic_load_explicit(&p->allocated_top,memory_order_acquire);
    if(op>GC_TARGET_INSTALL||(state!=GC_PAGE_READY&&state!=GC_PAGE_CLOSED)||p->owner||
        !p->incarnation||!p->identity_base||p->generation>GC_OLD||!p->start||p->start>=p->limit||
        p->limit>GC_MAX_OFFSET||((p->heap_base|p->start|p->limit|top)&15)||
        p->heap_base>UINT64_MAX-p->limit||q->heap_base!=p->heap_base||
        top<p->start||top>p->limit||(state==GC_PAGE_READY&&top!=p->start)||
        !p->starts||(p->starts&7)||p->starts_words<((p->limit-p->start+1023)>>10)||
        p->starts_words>UINT64_MAX/8||p->starts>UINT64_MAX-p->starts_words*8)return GC_TARGET_INVALID;
    if((!op&&(q->stage||q->cursor||q->destination||q->destination_limit))||
        (op&&(q->stage!=2||q->destination!=top+sizeof(ObjectHeader)||q->destination_limit!=p->limit)))return GC_TARGET_INVALID;
    if(p->limit-top<48)return GC_TARGET_REFILL;
    uint64_t destination=top+sizeof(ObjectHeader),granule=(destination-p->start)>>4;
    _Atomic uint64_t *start=(_Atomic uint64_t *)(uintptr_t)p->starts+(granule>>6);
    uint64_t bit=UINT64_C(1)<<(granule&63),before=atomic_load_explicit(start,memory_order_acquire);
    if(before&bit)return GC_TARGET_INVALID;
    if(!p->forward_table)return GC_TARGET_METADATA_REFILL;
    if(p->forward_table&7)return GC_TARGET_INVALID;
    ForwardTable *table=(ForwardTable *)(uintptr_t)p->forward_table;
    uint64_t used=atomic_load_explicit(&table->used,memory_order_acquire);
    if(!table->cells||(table->cells&7)||!table->capacity||table->capacity>UINT32_MAX||
        table->cells>UINT64_MAX-table->capacity*sizeof(ForwardCell)||used>table->capacity)return GC_TARGET_INVALID;
    if(used==table->capacity)return GC_TARGET_METADATA_REFILL;
    ForwardCell *cells=(ForwardCell *)(uintptr_t)table->cells,*cell=&cells[used];
    if(used&&(cells[used-1].source_offset>=destination||cells[used-1].incarnation!=p->incarnation))return GC_TARGET_INVALID;
    if(cell->source_offset||cell->incarnation||atomic_load_explicit(&cell->word,memory_order_relaxed)||
        atomic_load_explicit(&cell->access,memory_order_relaxed))return GC_TARGET_INVALID;
    if(!op){q->destination=destination;q->destination_limit=p->limit;return GC_TARGET_OK;}
    const ObjectHeader *header=(const ObjectHeader *)(uintptr_t)(p->heap_base+top);
    if(atomic_load_explicit(&header->state,memory_order_acquire)!=GC_OBJECT_PUBLISHED||
        header->payload_bytes!=q->cursor||q->cursor>p->limit-destination||
        !header->descriptor_id||header->descriptor_id>UINT32_MAX||!header->mark_record||(header->mark_record&7))return GC_TARGET_INVALID;
    uint64_t rounded=(q->cursor+15)&~UINT64_C(15);if(!rounded)rounded=16;
    if(rounded>p->limit-destination)return GC_TARGET_INVALID;
    cell->source_offset=destination;cell->incarnation=p->incarnation;
    atomic_store_explicit(&table->used,used+1,memory_order_release);
    atomic_store_explicit(start,before|bit,memory_order_release);
    atomic_store_explicit(&p->allocated_top,destination+rounded,memory_order_release);
    atomic_store_explicit(&p->state,GC_PAGE_CLOSED,memory_order_release);
    return GC_TARGET_OK;
}
static inline uint64_t gc_relocate_copy(ForwardCell *g,RelocationCopy *q) {
    if(!g->incarnation||q->source_incarnation!=g->incarnation||!q->ticket||q->ticket>(UINT64_MAX>>2)||
        atomic_load_explicit(&g->word,memory_order_acquire)!=((q->ticket<<2)|1))return GC_COPY_INVALID;
    if(atomic_load_explicit(&g->access,memory_order_acquire)!=GC_SOURCE_CLOSED)return GC_COPY_WAIT;
    uint64_t source=g->source_offset,destination=q->destination;
    if(source<48||destination<48||((source|destination|q->heap_base)&15)||
        q->source_limit>GC_MAX_OFFSET||q->destination_limit>GC_MAX_OFFSET||
        source>=q->source_limit||destination>=q->destination_limit||
        q->heap_base>UINT64_MAX-q->source_limit||q->heap_base>UINT64_MAX-q->destination_limit||
        !q->budget||q->budget>GC_COPY_MAX_BYTES||q->stage>2)return GC_COPY_INVALID;
    const ObjectHeader *from=(const ObjectHeader *)(uintptr_t)(q->heap_base+source-sizeof(ObjectHeader));
    if(atomic_load_explicit(&from->state,memory_order_acquire)!=GC_OBJECT_PUBLISHED)return GC_COPY_INVALID;
    uint64_t payload=from->payload_bytes;
    if(payload>q->source_limit-source||!from->descriptor_id||from->descriptor_id>UINT32_MAX||
        !from->mark_record||(from->mark_record&7))return GC_COPY_INVALID;
    uint64_t rounded=(payload+15)&~UINT64_C(15);if(!rounded)rounded=16;
    if(rounded>q->source_limit-source||rounded>q->destination_limit-destination||
        (source+rounded>destination-sizeof(ObjectHeader)&&destination+rounded>source-sizeof(ObjectHeader)))return GC_COPY_INVALID;
    ObjectHeader *to=(ObjectHeader *)(uintptr_t)(q->heap_base+destination-sizeof(ObjectHeader));
    uint64_t state=atomic_load_explicit(&to->state,memory_order_acquire);
    if(!q->stage){
        if(q->cursor||state||to->payload_bytes||to->descriptor_id||to->mark_record)return GC_COPY_INVALID;
    }else{
        if(to->payload_bytes!=payload||to->descriptor_id!=from->descriptor_id||to->mark_record!=from->mark_record)return GC_COPY_INVALID;
        if(q->stage==2)return q->cursor==payload&&state==GC_OBJECT_PUBLISHED?GC_COPY_DONE:GC_COPY_INVALID;
        if(!q->cursor||q->cursor>=payload||state!=GC_OBJECT_INITIALIZING)return GC_COPY_INVALID;
    }
    // Every rejection precedes heap, cursor and stage mutation.
    if(!q->stage){
        to->payload_bytes=payload;to->descriptor_id=from->descriptor_id;to->mark_record=from->mark_record;
        atomic_store_explicit(&to->state,GC_OBJECT_INITIALIZING,memory_order_release);
    }
    uint64_t n=payload-q->cursor;if(n>q->budget)n=q->budget;
    const unsigned char *src=(const unsigned char *)(uintptr_t)(q->heap_base+source);
    unsigned char *dst=(unsigned char *)(uintptr_t)(q->heap_base+destination);
    for(uint64_t i=0;i<n;i++)dst[q->cursor+i]=src[q->cursor+i];
    q->cursor+=n;q->stage=q->cursor==payload?2:1;
    if(q->stage==2)atomic_store_explicit(&to->state,GC_OBJECT_PUBLISHED,memory_order_release);
    return q->stage==2?GC_COPY_DONE:GC_COPY_MORE;
}
/* Complete-copy publication. Destination metadata and promotion/remset handoff
 * must already be installed by the caller. Validation touches no payload bytes
 * and never advances a partial copy; only the final forwarding CAS can mutate. */
static inline uint64_t gc_relocate_publish(ForwardCell *g,RelocationCopy *q,uint64_t color) {
    GcRef destination;
    if(q->stage!=2||color>UINT16_MAX||!gc_abi_encode_ref(q->destination,(uint16_t)color,&destination))return GC_COPY_INVALID;
    uint64_t status=gc_relocate_copy(g,q);
    if(status!=GC_COPY_DONE)return status;
    ForwardRequest publish={g->source_offset,q->source_incarnation,q->ticket,destination.bits};
    return gc_forward_step(g,&publish,GC_FWD_PUBLISH)==GC_FWD_OK?GC_COPY_DONE:GC_COPY_INVALID;
}
#endif
