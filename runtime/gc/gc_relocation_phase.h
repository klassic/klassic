/* Select -> RelocateStart -> ConcurrentRelocate, common phase/heap lock held.
 * The scheduler owns the generation's exclusive relocation turn, the frozen
 * selection and every native control through completion. Before BEGIN it has
 * proved remap-color reuse safe; this operation cannot derive that certificate
 * from a drained gate. Other-generation marking is preserved. Concurrent
 * relocation/retirement of the other generation is excluded.
 *
 * BEGIN changes the selected remap bit and starts adoption with admission still
 * closed. ACTIVATE requires all managed actors to ACK, publishes Concurrent-
 * Relocate and starts a second rendezvous without a resume gap. RELEASE waits
 * for that adoption before reopening the drained gate and releasing owners.
 * Native actors adopt on reentry. All actors use the worker handshake, including
 * raw-access/private-producer checks; a coordinator never acknowledges for them.
 * Notify/wait outside the lock. No root scan, payload copy, allocation or OS work.
 * This starts relocation; it neither retires sources/colors nor finishes a cycle.
 * Fresh/reusable states 0/3, adopting start 1, adopting relocation 2.
 */
#ifndef KLASSIC_GC_RELOCATION_PHASE_H
#define KLASSIC_GC_RELOCATION_PHASE_H
#include "gc_handshake.h"
#include "gc_relocation_gate.h"
enum { GC_RP_BEGIN=0,GC_RP_ACTIVATE=1,GC_RP_RELEASE=2 };
enum { GC_RP_ADVANCED=0,GC_RP_WAIT=1,GC_RP_INVALID=2,GC_RP_EXHAUSTED=3,GC_RP_STALE=4,GC_RP_RELOCATING=5,GC_RP_RETIRING=6 };
typedef uint64_t (*RelocationPhaseStep)(RootRegistry *,RelocationPhaseControl *,uint64_t);
static inline bool gc_relocation_phase_valid(const RootRegistry *g,const RelocationPhaseControl *q){
    if(!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||!q->handshake||(q->handshake&7)||
       !q->gate||(q->gate&7)||q->generation>1||!gc_abi_valid_phase((PhaseSnapshot){g->phase}))return false;
    HandshakeState *h=(HandshakeState *)(uintptr_t)q->handshake;
    if(h->active>1||h->native>h->members||!!h->head!=!!h->members||h->acknowledged>h->expected||
       (h->active?(!h->generation||h->expected!=h->members-h->native):(h->expected||h->acknowledged)))return false;
    return true;
}
static inline uint64_t gc_relocation_phase_step_locked(RootRegistry *g,RelocationPhaseControl *q,uint64_t op){
    if(op>2||!gc_relocation_phase_valid(g,q))return GC_RP_INVALID;
    HandshakeState *h=(HandshakeState *)(uintptr_t)q->handshake;
    RelocationGate *gate=(RelocationGate *)(uintptr_t)q->gate;
    unsigned shift=(unsigned)q->generation*4;
    uint64_t phase=(g->phase>>shift)&15;
    if(op==GC_RP_BEGIN){
        if((q->state!=0&&q->state!=3)||phase!=GC_SELECT)return GC_RP_INVALID;
        if(h->active||((g->phase>>(4-shift))&15)>=GC_RELOCATE_START)return GC_RP_WAIT;
    }else{
        if(q->state!=op||!q->rendezvous||!q->gate_epoch)return GC_RP_INVALID;
        if(q->phase!=g->phase||q->rendezvous!=h->generation||q->gate_epoch!=gate->epoch)return GC_RP_STALE;
        if(!h->active||phase!=(op==GC_RP_ACTIVATE?GC_RELOCATE_START:GC_CONCURRENT_RELOCATE))return GC_RP_INVALID;
        if(h->acknowledged!=h->expected)return GC_RP_WAIT;
    }
    uint64_t status=gc_relocation_gate_locked(gate,NULL,GC_RG_CHECK);
    if(status!=GC_RG_OK)return status==GC_RG_WAIT?GC_RP_WAIT:GC_RP_INVALID;
    if(gate->epoch==UINT64_MAX)return GC_RP_EXHAUSTED;
    if(op==GC_RP_RELEASE){
        (void)gc_relocation_gate_locked(gate,NULL,GC_RG_REOPEN);
        (void)gc_handshake_step_locked(h,NULL,GC_HS_END);
        q->state=3;q->gate_epoch=gate->epoch;return GC_RP_RELOCATING;
    }
    uint64_t reserve=op==GC_RP_BEGIN?2:1;
    if(h->generation>UINT64_MAX-reserve||(g->phase>>16)>(UINT64_MAX>>16)-reserve)return GC_RP_EXHAUSTED;
    if(op==GC_RP_ACTIVATE)(void)gc_handshake_step_locked(h,NULL,GC_HS_END);
    (void)gc_handshake_step_locked(h,NULL,GC_HS_BEGIN);
    g->phase=((g->phase+UINT64_C(0x10000))&~(UINT64_C(15)<<shift))|((uint64_t)(op==GC_RP_BEGIN?GC_RELOCATE_START:GC_CONCURRENT_RELOCATE)<<shift);
    if(op==GC_RP_BEGIN)g->phase^=UINT64_C(1)<<(10+q->generation);
    q->state=op+1;q->phase=g->phase;q->rendezvous=h->generation;q->gate_epoch=gate->epoch;
    return GC_RP_ADVANCED;
}
/* A fresh control with the same layout owns this separate boundary. BEGIN
 * requires all selected work installed/published or retained/cleaned and the
 * admission gate closed/drained. The scheduler owns that completion proof and
 * the exclusive generation turn; drainage alone cannot prove selection done.
 * It publishes Retire without changing colors. RELEASE waits for actor adoption
 * and keeps the captured gate CLOSED, including when its epoch is exhausted.
 * Root/heap/buffer/job/handle/forwarding audits and retirement still run after
 * this boundary. RETIRING never authorizes reclamation, parity reuse or FINISH.
 * No payload/root traversal or wait occurs under the lock. Fresh state 0,
 * adopting Retire 1, released 2; control/native metadata stay stable and live.
 */
enum { GC_RR_BEGIN=0,GC_RR_RELEASE=1 };
static inline uint64_t gc_relocation_retire_step_locked(RootRegistry *g,RelocationPhaseControl *q,uint64_t op){
    if(op>1||!gc_relocation_phase_valid(g,q))return GC_RP_INVALID;
    HandshakeState *h=(HandshakeState *)(uintptr_t)q->handshake;
    RelocationGate *gate=(RelocationGate *)(uintptr_t)q->gate;
    unsigned shift=(unsigned)q->generation*4;
    uint64_t phase=(g->phase>>shift)&15;
    if(op==GC_RR_BEGIN){
        if(q->state||q->rendezvous||q->phase||q->gate_epoch||phase!=GC_CONCURRENT_RELOCATE)return GC_RP_INVALID;
        if(h->active||((g->phase>>(4-shift))&15)>=GC_RELOCATE_START)return GC_RP_WAIT;
    }else{
        if(q->state!=1||!q->rendezvous||!q->gate_epoch)return GC_RP_INVALID;
        if(q->phase!=g->phase||q->rendezvous!=h->generation||q->gate_epoch!=gate->epoch)return GC_RP_STALE;
        if(!h->active||phase!=GC_RETIRE)return GC_RP_INVALID;
        if(h->acknowledged!=h->expected)return GC_RP_WAIT;
    }
    uint64_t status=gc_relocation_gate_locked(gate,NULL,GC_RG_CHECK);
    if(status!=GC_RG_OK)return status==GC_RG_WAIT?GC_RP_WAIT:GC_RP_INVALID;
    if(op==GC_RR_RELEASE){
        (void)gc_handshake_step_locked(h,NULL,GC_HS_END);q->state=2;return GC_RP_RETIRING;
    }
    if(h->generation==UINT64_MAX||(g->phase>>16)==(UINT64_MAX>>16))return GC_RP_EXHAUSTED;
    (void)gc_handshake_step_locked(h,NULL,GC_HS_BEGIN);
    g->phase=((g->phase+UINT64_C(0x10000))&~(UINT64_C(15)<<shift))|(UINT64_C(7)<<shift);
    q->state=1;q->phase=g->phase;q->rendezvous=h->generation;q->gate_epoch=gate->epoch;
    return GC_RP_ADVANCED;
}
#endif
