/* Bounded metadata-only readiness scan for physical source payload release.
 * Caller exclusively owns the closed run's retirement scheduling, excluding
 * allocation/target append and table replacement from the first slice through
 * OS cleanup. Its complete allocation table and metadata stay live. Every
 * allocated object has exactly one table entry (the allocator's invariant).
 * Published forwarding is immutable and its destination remains protected.
 * Each slice holds the heap lock. DONE seals the run as EVACUATED, preventing
 * later target append or TLAB binding. Cleanup failure leaves it sealed for
 * retry. DONE permits payload decommit under that ownership, NOT virtual-range reuse,
 * metadata release, color reuse or budget credit. Old references still resolve
 * through the retained start bitmap/table. No heap address is dereferenced.
 * Cursor is uniquely owned and cannot be forged, rewound or reused on another
 * run. WAIT retains the first unfinished entry; INVALID preserves the request.
 */
#ifndef KLASSIC_GC_SOURCE_DRAIN_H
#define KLASSIC_GC_SOURCE_DRAIN_H
#include "gc_forward.h"
enum { GC_SD_DONE=0, GC_SD_INVALID=1, GC_SD_MORE=2, GC_SD_WAIT=3 };
static inline uint64_t gc_source_drain(AllocationPage *p,SourceDrain *q){
    uint64_t state=atomic_load_explicit(&p->state,memory_order_acquire);
    if((state!=GC_PAGE_CLOSED&&state!=GC_PAGE_EVACUATED)||
        (state==GC_PAGE_EVACUATED&&q->cursor!=q->used)||p->owner||
        !p->incarnation||q->incarnation!=p->incarnation||!p->start||p->start>=p->limit||
        p->limit>GC_MAX_OFFSET||((p->start|p->limit|q->top)&15)||q->top<p->start||q->top>p->limit||
        q->top!=atomic_load_explicit(&p->allocated_top,memory_order_acquire)||
        !p->forward_table||(p->forward_table&7)||!q->budget||q->budget>256)return GC_SD_INVALID;
    const ForwardTable *t=(const ForwardTable *)(uintptr_t)p->forward_table;
    if(!t->cells||(t->cells&7)||!t->capacity||t->capacity>UINT32_MAX||
        t->cells>UINT64_MAX-t->capacity*sizeof(ForwardCell)||q->used>t->capacity||
        q->used!=atomic_load_explicit(&t->used,memory_order_acquire)||q->cursor>q->used)return GC_SD_INVALID;
    const ForwardCell *cells=(const ForwardCell *)(uintptr_t)t->cells;
    uint64_t cursor=q->cursor,examined=0,status=GC_SD_DONE;
    while(cursor<q->used&&examined<q->budget){
        const ForwardCell *cell=&cells[cursor];uint64_t source=cell->source_offset;
        if(source<p->start+32||source>=q->top||(source&15)||cell->incarnation!=q->incarnation||
            (cursor&&cells[cursor-1].source_offset>=source))return GC_SD_INVALID;
        uint64_t word=atomic_load_explicit(&cell->word,memory_order_acquire);examined++;
        if(!word||word==3||((word&3)==1&&(word>>2))){status=GC_SD_WAIT;break;}
        if((word&3)!=2||!gc_forward_destination(word&~UINT64_C(3),source))return GC_SD_INVALID;
        if(atomic_load_explicit(&cell->access,memory_order_acquire)!=GC_SOURCE_CLOSED){status=GC_SD_WAIT;break;}
        cursor++;
    }
    if(status!=GC_SD_WAIT&&cursor<q->used)status=GC_SD_MORE;
    if(status==GC_SD_DONE)atomic_store_explicit(&p->state,GC_PAGE_EVACUATED,memory_order_release);
    q->cursor=cursor;q->examined=examined;return status;
}
#endif
