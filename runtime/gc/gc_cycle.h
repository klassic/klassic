/* Cycle request ledger. All operations hold the scheduler/request lock.
 * The nonmoving ledger outlives every ticket and may not be reset/reused while
 * any ticket exists. Young/Major are independent lanes: a Major implementation
 * must perform a full major collection, not satisfy it with a Young cycle.
 * Requests before the next START coalesce; requests after START target another
 * cycle. FINISH is called only after that entire cycle completes (not mark-end).
 * This leaf neither runs collections nor waits. The caller supplies wakeups
 * required by its wait policy and polls/parks owners outside this lock. */
#ifndef KLASSIC_GC_CYCLE_H
#define KLASSIC_GC_CYCLE_H
#include "gc_abi.h"
#define GC_CYCLE_YOUNG 0u
#define GC_CYCLE_MAJOR 1u
#define GC_CY_REQUEST 0u
#define GC_CY_START 1u
#define GC_CY_FINISH 2u
#define GC_CY_POLL 3u
#define GC_CY_OK 0u
#define GC_CY_WAIT 1u
#define GC_CY_INVALID 2u
#define GC_CY_EXHAUSTED 3u
#define GC_CY_EMPTY 4u
static inline uint64_t gc_cycle_step_locked(CycleLedger *g,CycleTicket *q,uint64_t op) {
    if(op>3||!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||q->kind>1)return GC_CY_INVALID;
    for(unsigned i=0;i<2;i++)if(g->completed[i]>g->started[i]||g->started[i]-g->completed[i]>1||
        g->started[i]>g->requested[i]||g->requested[i]-g->started[i]>1)return GC_CY_INVALID;
    uint64_t k=q->kind;
    if(op<=GC_CY_START) {
        if(q->ledger||q->cycle)return GC_CY_INVALID;
        if(op==GC_CY_REQUEST) {
            if(g->started[k]==UINT64_MAX)return GC_CY_EXHAUSTED;
            g->requested[k]=g->started[k]+1;
            q->cycle=g->requested[k];q->ledger=(uintptr_t)g;
        }else{
            if(g->started[k]!=g->completed[k])return GC_CY_WAIT;
            if(g->requested[k]==g->started[k])return GC_CY_EMPTY;
            g->started[k]++;q->cycle=g->started[k];q->ledger=(uintptr_t)g;
        }
        return GC_CY_OK;
    }
    if(q->ledger!=(uintptr_t)g||!q->cycle||q->cycle>g->requested[k])return GC_CY_INVALID;
    if(op==GC_CY_POLL)return q->cycle<=g->completed[k]?GC_CY_OK:GC_CY_WAIT;
    if(q->cycle!=g->started[k]||g->started[k]==g->completed[k])return GC_CY_INVALID;
    g->completed[k]=g->started[k];return GC_CY_OK;
}
#endif
