/* Admit a requested cycle and its prepared mark-start rendezvous together.
 * Caller holds BOTH request and registry locks (or exclusive startup ownership)
 * throughout. Inputs/control/ledger/ticket are native, disjoint and stable.
 * Queue epochs, color reuse and Young remset preparation precede this call;
 * they are NOT inferred from the request ledger. Kind 0 starts Young marking;
 * kind 1 starts Old marking for a Major owner which still owes the FULL major
 * cycle. Neither success nor mark completion permits premature ticket FINISH.
 * On failure no shared state changes. A private ledger preview makes phase
 * admission the last fallible operation. Notify owners only after unlocking. */
#ifndef KLASSIC_GC_CYCLE_MARK_START_H
#define KLASSIC_GC_CYCLE_MARK_START_H
#include "gc_cycle.h"
#include "gc_mark_phase.h"
typedef uint64_t (*CycleMarkStarter)(CycleLedger *,CycleTicket *,const CycleMarkStart *);
static inline uint64_t gc_cycle_mark_start_locked(CycleLedger *g,CycleTicket *ticket,const CycleMarkStart *q){
    if(!g||((uintptr_t)g&7)||!ticket||((uintptr_t)ticket&7)||!q||((uintptr_t)q&7)||
       !q->control||(q->control&7)||!q->registry||(q->registry&7))return GC_CY_INVALID;
    MarkPhaseControl *control=(void *)(uintptr_t)q->control;
    if(ticket->kind>1||control->generation!=ticket->kind)return GC_CY_INVALID;
    CycleLedger preview=*g;CycleTicket admitted=*ticket;
    uint64_t result=gc_cycle_step_locked(&preview,&admitted,GC_CY_START);
    if(result!=GC_CY_OK)return result;
    result=gc_mark_phase_step_locked((void *)(uintptr_t)q->registry,control,GC_MP_START);
    if(result!=GC_MP_ADVANCED)return result;
    g->started[ticket->kind]=admitted.cycle;
    ticket->ledger=(uintptr_t)g;ticket->cycle=admitted.cycle;
    return GC_CY_OK;
}
#endif
