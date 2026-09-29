/* Precise two-bank slot inventory, under the registry/phase lock. Native maps
 * start zeroed and remain disjoint/live; geometry and incarnation are fixed.
 * One bit represents one aligned eight-byte slot relative to start. Callers
 * validate the Old owner's descriptor and resolve its canonical page before
 * RECORD; a metadata lease protects this inventory and the source ownership.
 * RECORD phase is the captured BarrierEntry phase, not the consumer's phase.
 * Late previous-epoch entries enter BOTH banks, so even a passed scan cursor
 * cannot lose them. Current entries enter current only. Counts include every
 * previous bit; TAKE claims one slot and retains it until ACK after rooted
 * load/Young handoff. ACK keep=1 carries a still-Young edge into the next cycle.
 * A claim is unique per page; different pages can be processed in parallel.
 * No heap dereference or traversal occurs under this lock.
 *
 * FLIP is O(1), only at a prepared Young MarkStart with all producers adopting
 * that boundary. The reusable bank must already be empty, with no active job.
 * Before a SECOND flip, all older buffer/jobs must be acknowledged: bitmap
 * emptiness alone does not prove this. CHECK must join the global termination
 * predicate along with buffers, roots, mark queues and in-flight workers.
 * Page movement, dead-owner validation and color retirement are caller duties.
 * Jobs/native controls and maps never alias; API errors preserve all state.
 */
#ifndef KLASSIC_GC_REMEMBERED_H
#define KLASSIC_GC_REMEMBERED_H
#include "gc_abi.h"
#define GC_RS_RECORD 0u
#define GC_RS_FLIP 1u
#define GC_RS_TAKE 2u
#define GC_RS_ACK 3u
#define GC_RS_CHECK 4u
#define GC_RS_OK 0u
#define GC_RS_INVALID 1u
#define GC_RS_WAIT 2u
#define GC_RS_EMPTY 3u
#define GC_RS_MORE 4u
#define GC_RS_EXHAUSTED 5u
#define GC_RS_STALE 6u
static inline uint64_t gc_remembered_step_locked(RememberedSet *s,RememberedJob *q,uint64_t op) {
    if(op>4||!s||((uintptr_t)s&7)||!q||((uintptr_t)q&7)||!s->start||s->start>=s->limit||
       ((s->start|s->limit)&7)||s->limit>GC_MAX_OFFSET||!s->incarnation||s->current>1||
       !s->words||s->words!=((s->limit-s->start)/8+63)/64||
       !s->bits[0]||!s->bits[1]||((s->bits[0]|s->bits[1])&7)||s->bits[0]==s->bits[1]||
       s->bits[0]>UINT64_MAX-s->words*8||s->bits[1]>UINT64_MAX-s->words*8||
       !gc_abi_valid_phase((PhaseSnapshot){s->epoch})||!(s->epoch>>16)||
       !gc_abi_valid_phase((PhaseSnapshot){s->last_phase})||s->last_phase<s->epoch||
       s->current_count>(s->limit-s->start)/8||s->pending>(s->limit-s->start)/8||s->cursor>=s->words||
       s->active_serial>s->last_serial||(!s->active_serial&&s->active_offset)||
       (s->active_serial&&(s->active_offset<s->start||s->active_offset>=s->limit||(s->active_offset&7))))return GC_RS_INVALID;
    if(s->previous_epoch&&(!gc_abi_valid_phase((PhaseSnapshot){s->previous_epoch})||
       !(s->previous_epoch>>16)||(s->previous_epoch>>16)>=(s->epoch>>16)||
       !((s->previous_epoch^s->epoch)&0x1000)))return GC_RS_INVALID;
    if(!s->previous_epoch&&(s->pending||s->active_serial))return GC_RS_INVALID;
    uint64_t *current=(uint64_t *)(uintptr_t)s->bits[s->current];
    uint64_t *previous=(uint64_t *)(uintptr_t)s->bits[s->current^1];
    if(op==GC_RS_CHECK)return s->pending||s->active_serial?GC_RS_WAIT:GC_RS_EMPTY;
    if(op==GC_RS_FLIP) {
        if(q->serial||q->incarnation!=s->incarnation||!gc_abi_valid_phase((PhaseSnapshot){q->phase})||
           (q->phase&15)!=GC_MARK_START||(q->phase>>16)<=(s->last_phase>>16)||
           !((q->phase^s->epoch)&0x1000))return GC_RS_INVALID;
        if(s->pending||s->active_serial)return GC_RS_WAIT;
        s->previous_epoch=s->epoch;s->epoch=s->last_phase=q->phase;s->current^=1;
        s->pending=s->current_count;s->current_count=0;s->cursor=0;return GC_RS_OK;
    }
    if(op==GC_RS_RECORD||op==GC_RS_ACK) {
        if(q->incarnation!=s->incarnation||q->offset<s->start||q->offset>=s->limit||(q->offset&7))return GC_RS_INVALID;
        uint64_t index=(q->offset-s->start)/8,mask=UINT64_C(1)<<(index%64);index/=64;
        bool late=false;
        if(op==GC_RS_RECORD) {
            if(q->serial||!gc_abi_valid_phase((PhaseSnapshot){q->phase})||!(q->phase>>16))return GC_RS_INVALID;
            if(q->phase<s->epoch) {
                if(!s->previous_epoch||q->phase<s->previous_epoch)return GC_RS_STALE;
                if((q->phase^s->previous_epoch)&0x1000)return GC_RS_INVALID;
                late=true;
            }else if((q->phase^s->epoch)&0x1000)return GC_RS_INVALID;
        }else if(!q->serial||q->serial!=s->active_serial||q->offset!=s->active_offset||q->phase!=s->epoch||q->keep>1)return GC_RS_INVALID;
        if(op==GC_RS_RECORD||q->keep)if(!(current[index]&mask)){current[index]|=mask;s->current_count++;}
        if(late&&!(previous[index]&mask)){previous[index]|=mask;s->pending++;}
        if(op==GC_RS_RECORD){if(q->phase>s->last_phase)s->last_phase=q->phase;}
        else{s->active_serial=s->active_offset=0;}
        return GC_RS_OK;
    }
    if(q->serial||!q->budget||q->budget>256)return GC_RS_INVALID;
    if(s->active_serial)return GC_RS_WAIT;
    if(!s->pending)return GC_RS_EMPTY;
    if(s->last_serial==UINT64_MAX)return GC_RS_EXHAUSTED;
    uint64_t cursor=s->cursor;
    for(uint64_t n=0;n<q->budget;n++) {
        uint64_t word=previous[cursor];
        if(word) {
            unsigned bit=0;while(!(word&(UINT64_C(1)<<bit)))bit++;
            uint64_t offset=s->start+(cursor*64+bit)*8;
            if(offset>=s->limit)return GC_RS_INVALID;
            previous[cursor]=word&~(UINT64_C(1)<<bit);s->pending--;
            s->active_serial=++s->last_serial;s->active_offset=offset;s->cursor=cursor;
            q->offset=offset;q->incarnation=s->incarnation;q->phase=s->epoch;q->serial=s->active_serial;q->keep=0;
            return GC_RS_OK;
        }
        if(++cursor==s->words)cursor=0;
        if(cursor==s->cursor)return GC_RS_INVALID; // count/map corruption
    }
    s->cursor=cursor;return GC_RS_MORE;
}
#endif
