/* Fair inter-generation relocation admission under the common scheduler/heap
 * lock. REQUEST coalesces pending requests. ACQUIRE chooses the sole pending
 * generation, or alternates when both are pending; a fresh request during an
 * active turn remains pending for the next turn. One unique receipt owns the
 * turn. All native controls/gates remain live and disjoint until RELEASE.
 *
 * Acquisition requires the generation's closed, drained RelocationGate and
 * reserves its next epoch. The owner initially reopens that gate to admit its jobs.
 * RELEASE requires that exact reopened epoch closed and drained again. The
 * RESUME reopens an exact closed/drained epoch while retaining the turn, for
 * bounded failure recovery before the full turn finishes. Later direct REOPEN
 * calls would invalidate the receipt: use RESUME after the initial reopen. The
 * owner additionally completes phase adoption, incoming-reference/retirement
 * audits before releasing; gate drainage alone does not prove retirement.
 * Every mover participates in this turn and the selected gate. Old marking
 * may continue while Young owns relocation: this lock covers metadata steps,
 * never the entire collection. No wait, traversal, heap access or OS call.
 *
 * The coordinator cannot be reset while receipts/requests exist. Counters never
 * wrap. Rejections preserve all state. Fairness is at completed turn boundaries,
 * not a wall-clock bound on an owner that stops making progress. This admission
 * controller is not a complete cycle executor or adaptive scheduler.
 */
#ifndef KLASSIC_GC_RELOCATION_TURN_H
#define KLASSIC_GC_RELOCATION_TURN_H
#include "gc_relocation_gate.h"
enum { GC_RT_REQUEST=0,GC_RT_ACQUIRE=1,GC_RT_RELEASE=2,GC_RT_RESUME=3 };
enum { GC_RT_OK=0,GC_RT_WAIT=1,GC_RT_INVALID=2,GC_RT_EXHAUSTED=3,GC_RT_EMPTY=4 };
typedef uint64_t (*RelocationTurnStep)(RelocationCoordinator *,RelocationTurn *,uint64_t);
static inline uint64_t gc_relocation_turn_locked(RelocationCoordinator *g,RelocationTurn *q,uint64_t op){
    if(op>3||!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||q->generation>1||
       g->pending[0]>1||g->pending[1]>1||g->active>2||g->turn>1||
       (g->active?(!g->serial||!g->gate||(g->gate&7)||g->gate_epoch<2||g->turn!=((g->active-1)^1)):
           (g->gate||g->gate_epoch)))return GC_RT_INVALID;
    uint64_t k=q->generation;
    if(op<=GC_RT_ACQUIRE){
        if(q->coordinator||q->serial)return GC_RT_INVALID;
        if(op==GC_RT_REQUEST){g->pending[k]=1;return GC_RT_OK;}
        if(g->active)return GC_RT_WAIT;
        if(!g->pending[k])return GC_RT_EMPTY;
        if(g->pending[k^1]&&g->turn!=k)return GC_RT_WAIT;
        if(!q->gate||(q->gate&7))return GC_RT_INVALID;
        RelocationGate *gate=(RelocationGate *)(uintptr_t)q->gate;
        uint64_t status=gc_relocation_gate_locked(gate,NULL,GC_RG_CHECK);
        if(status!=GC_RG_OK)return status==GC_RG_WAIT?GC_RT_WAIT:GC_RT_INVALID;
        if(g->serial==UINT64_MAX||gate->epoch==UINT64_MAX)return GC_RT_EXHAUSTED;
        g->active=k+1;g->turn=k^1;g->pending[k]=0;g->gate=q->gate;g->gate_epoch=gate->epoch+1;
        q->coordinator=(uintptr_t)g;q->serial=++g->serial;return GC_RT_OK;
    }
    if(q->coordinator!=(uintptr_t)g||!q->serial||q->serial!=g->serial||g->active!=k+1||q->gate!=g->gate)return GC_RT_INVALID;
    RelocationGate *gate=(RelocationGate *)(uintptr_t)g->gate;
    if(gate->epoch!=g->gate_epoch)return GC_RT_INVALID;
    uint64_t status=gc_relocation_gate_locked(gate,NULL,GC_RG_CHECK);
    if(status!=GC_RG_OK)return status==GC_RG_WAIT?GC_RT_WAIT:GC_RT_INVALID;
    if(op==GC_RT_RESUME){
        if(gate->epoch==UINT64_MAX)return GC_RT_EXHAUSTED;
        (void)gc_relocation_gate_locked(gate,NULL,GC_RG_REOPEN);
        g->gate_epoch=gate->epoch;return GC_RT_OK;
    }
    g->active=g->gate=g->gate_epoch=0;q->coordinator=q->serial=0;return GC_RT_OK;
}
#endif
