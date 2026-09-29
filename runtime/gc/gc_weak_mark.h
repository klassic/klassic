/* Weak admission owner for mark termination. Caller holds the common
 * weak/phase/root/queue lock through each call; the callback is the matching
 * nonblocking mark-phase helper. One owner exclusively controls this gate and
 * MarkPhaseControl. Other generations must wait while the gate is owned.
 * BEGIN closes admission before the mark-end rendezvous and rolls back on a
 * failed BEGIN. Earlier loads are rooted/journaled and remain in the existing
 * end predicate. RETRY reopens only after ConcurrentMark adoption. SELECT
 * leaves admission closed for weak processing; only a covered scan may reopen.
 * processor/scan identify exclusive live native controls; cleanup retains claims.
 * This does not certify dead entries, perform weak clear, or finish a cycle.
 * States: 0 idle, 1 deciding, 2 adopting outcome, 3 awaiting weak processing.
 */
#ifndef KLASSIC_GC_WEAK_MARK_H
#define KLASSIC_GC_WEAK_MARK_H
#include "gc_weak_load.h"
#include "gc_mark_phase.h"
typedef uint64_t (*WeakMarkPhaseStep)(RootRegistry *,MarkPhaseControl *,uint64_t);
static inline uint64_t gc_weak_mark_end_locked(RootRegistry *registry,WeakMarkControl *owner,
        uint64_t op,WeakMarkPhaseStep step){
    if(op>GC_MP_RELEASE||!registry||((uintptr_t)registry&7)||!owner||((uintptr_t)owner&7)||!step||
        !owner->gate||((uintptr_t)owner->gate&7)||!owner->mark||((uintptr_t)owner->mark&7)||owner->stage>3)
        return GC_MP_INVALID;
    WeakLoadGate *gate=owner->gate;MarkPhaseControl *mark=owner->mark;
    if(gate->registry!=(uintptr_t)registry||gate->handshake!=mark->handshake||
        !gate->pool||(gate->pool&7)||gate->closed>1)return GC_MP_INVALID;
    if(op==GC_MP_BEGIN){
        if(owner->stage||owner->processor||owner->scan)return GC_MP_INVALID;
        if(gate->closed)return GC_MP_WAIT;
        gate->closed=1;
        uint64_t result=step(registry,mark,op);
        if(result==GC_MP_ADVANCED)owner->stage=1;
        else gate->closed=0;
        return result;
    }
    if(!gate->closed||owner->stage!=(op==GC_MP_DECIDE?1u:2u)||
        mark->state!=(op==GC_MP_DECIDE?1u:2u))return GC_MP_INVALID;
    uint64_t result=step(registry,mark,op);
    if(op==GC_MP_DECIDE&&result==GC_MP_ADVANCED)owner->stage=2;
    else if(op==GC_MP_RELEASE){
        if(result==GC_MP_RETRY){owner->stage=0;gate->closed=0;}
        else if(result==GC_MP_SELECT)owner->stage=3;
    }
    return result;
}
typedef uint64_t (*WeakMarkStepper)(RootRegistry *,WeakMarkControl *,uint64_t);
static inline uint64_t gc_weak_mark_step_locked(RootRegistry *registry,WeakMarkControl *owner,uint64_t op){
    return gc_weak_mark_end_locked(registry,owner,op,gc_mark_phase_step_locked);
}
#endif
