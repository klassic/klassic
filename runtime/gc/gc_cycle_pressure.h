/* Connect a coherent heap forecast to a request-time cycle ticket. The caller
 * holds the cycle ledger lock and supplies either the heap lock or a private
 * immutable heap/sample snapshot. No lock is acquired here. Inputs and ticket
 * have distinct, stable storage. The ticket must be fresh even on QUIET.
 * QUIET never reads ledger contents; REQUEST validates them through cycle_step.
 * Wake the scheduler after REQUESTED, outside locks. This neither starts nor
 * completes a cycle, chooses its generation, waits, or certifies color reuse. */
#ifndef KLASSIC_GC_CYCLE_PRESSURE_H
#define KLASSIC_GC_CYCLE_PRESSURE_H
#include "gc_cycle.h"
#include "gc_heap_pressure.h"
#define GC_CP_REQUESTED 0u
#define GC_CP_INVALID 2u
#define GC_CP_EXHAUSTED 3u
#define GC_CP_QUIET 4u
typedef uint64_t (*CyclePressureStep)(CycleLedger *,CycleTicket *,const CyclePressure *);
static inline uint64_t gc_cycle_pressure_locked(CycleLedger *g,CycleTicket *ticket,const CyclePressure *q){
    if(!g||((uintptr_t)g&7)||!ticket||((uintptr_t)ticket&7)||!q||((uintptr_t)q&7)||
       ticket->kind>1||ticket->ledger||ticket->cycle)return GC_CP_INVALID;
    uint64_t pressure=gc_heap_allocation_pressure((const void *)(uintptr_t)q->heap,
        (const void *)(uintptr_t)q->sample,q->horizon_ns);
    if(pressure==GC_PRESSURE_QUIET)return GC_CP_QUIET;
    if(pressure!=GC_PRESSURE_REQUEST)return GC_CP_INVALID;
    return gc_cycle_step_locked(g,ticket,GC_CY_REQUEST);
}
#endif
