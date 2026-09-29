#ifndef KLASSIC_GC_NATIVE_CYCLE_STATE_H
#define KLASSIC_GC_NATIVE_CYCLE_STATE_H
#include "gc_abi.h"
/* Runtime-private stable controls; not the shared emitted ABI. */
typedef struct {
    CycleTicket ticket;
    RootAudit marking_audit,retirement_audit;
    MarkPhaseControl marking;
    RelocationGate gate;
    RelocationPhaseControl moving,retiring;
    uint64_t completing,finish_phase,finish_rendezvous;
} NativeCycleState;
#endif
