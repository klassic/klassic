/* Necessary incoming-forwarding check, NOT a complete retirement certificate.
 * Caller retains the scanned table, target range and all controls, freezes
 * relocation publication AND forwarding compression across the whole audit,
 * and excludes table append or
 * replacement. Mutator/root/heap/buffer/job/native references need their own
 * audits. A zero lease count does not replace this check. No payload access.
 * At most 256 entries per call; claimed publication returns WAIT. FOUND leaves
 * the cursor at its source and returns a scalar source offset, never an address.
 * Output must be consumed before retry. INVALID changes nothing. Each unique
 * cursor belongs to one captured incarnation/table length and one target range.
 */
#ifndef KLASSIC_GC_FORWARD_INCOMING_H
#define KLASSIC_GC_FORWARD_INCOMING_H
#include "gc_forward.h"
enum { GC_FI_DONE=0, GC_FI_INVALID=1, GC_FI_MORE=2, GC_FI_FOUND=3, GC_FI_WAIT=4 };
static inline uint64_t gc_forward_incoming(const ForwardTable *t,ForwardIncoming *q){
    if(!t->cells||(t->cells&7)||!t->capacity||t->capacity>UINT32_MAX||
        t->cells>UINT64_MAX-t->capacity*sizeof(ForwardCell)||!q->incarnation||
        q->used>t->capacity||q->used!=atomic_load_explicit(&t->used,memory_order_acquire)||
        q->cursor>q->used||!q->budget||q->budget>256||q->source_offset||
        !q->target_start||q->target_start>=q->target_limit||q->target_limit>GC_MAX_OFFSET||
        ((q->target_start|q->target_limit)&15))return GC_FI_INVALID;
    const ForwardCell *cells=(const ForwardCell *)(uintptr_t)t->cells;
    uint64_t cursor=q->cursor,examined=0,source=0,status=GC_FI_DONE;
    while(cursor<q->used&&examined<q->budget){
        const ForwardCell *cell=&cells[cursor];uint64_t offset=cell->source_offset;
        if(!offset||offset>GC_MAX_OFFSET||(offset&15)||cell->incarnation!=q->incarnation||
            (cursor&&cells[cursor-1].source_offset>=offset))return GC_FI_INVALID;
        uint64_t word=atomic_load_explicit(&cell->word,memory_order_acquire);examined++;
        if(word&&word!=3){
            if((word&3)==1&&(word>>2)){status=GC_FI_WAIT;break;}
            uint64_t ref=word&~UINT64_C(3);
            if((word&3)!=2||!gc_forward_destination(ref,offset))return GC_FI_INVALID;
            uint64_t destination=ref>>16;
            if(destination>=q->target_start&&destination<q->target_limit){status=GC_FI_FOUND;source=offset;break;}
        }
        cursor++;
    }
    if(status==GC_FI_DONE&&cursor<q->used)status=GC_FI_MORE;
    q->cursor=cursor;q->examined=examined;q->source_offset=source;return status;
}
#endif
