/* Mark-start/end phase coordinator, under the common registry/queue lock.
 * START consumes an already prepared MarkStart snapshot and queue epoch. The
 * scheduler publishes that snapshot under this same lock immediately before
 * START; preparation, mark-color reuse safety and Young remset flip are caller
 * obligations, not inferred from an empty queue. START captures the first
 * rendezvous. ACTIVATE waits for its acknowledgements then publishes
 * ConcurrentMark and starts the second rendezvous without a resume gap.
 * RELEASE returns MARKING after adoption. No root traversal occurs here.
 * BEGIN publishes MarkEndCheck and starts a rendezvous. DECIDE waits for all
 * producers, applies the shared end predicate, then selects ConcurrentMark or
 * Select. It restarts the handshake without releasing the lock: nobody resumes
 * between the decision and adoption of the new phase. RELEASE ends this second
 * rendezvous only when all managed owners have ACKed via gc_worker_step_locked.
 * Native owners adopt on reentry. Caller notifies after successful steps and
 * waits/assists outside the lock. No draining or root/heap scans occur here.
 * One coordinator owns this control and both phase boundaries. Snapshot and
 * handshake counters reserve two increments at BEGIN; they never wrap.
 * START reserves two handshake increments and one snapshot increment. It does
 * not allocate/clear an epoch, retire colors, complete cycles or issue tickets.
 * Control states: 0 fresh, 1 deciding end, 2 adopting end outcome, 3 released,
 * 4 adopting prepared start, 5 adopting ConcurrentMark after activation. */
#ifndef KLASSIC_GC_MARK_PHASE_H
#define KLASSIC_GC_MARK_PHASE_H
#include "gc_mark_end.h"
#define GC_MP_BEGIN 0u
#define GC_MP_DECIDE 1u
#define GC_MP_RELEASE 2u
#define GC_MP_START 3u
#define GC_MP_ACTIVATE 4u
#define GC_MP_ADVANCED 0u
#define GC_MP_WAIT 1u
#define GC_MP_INVALID 2u
#define GC_MP_EXHAUSTED 3u
#define GC_MP_STALE 4u
#define GC_MP_RETRY 5u
#define GC_MP_SELECT 6u
#define GC_MP_MARKING 7u
static inline uint64_t gc_mark_phase_step_locked(RootRegistry *g,MarkPhaseControl *q,uint64_t op) {
    if(op>4||!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||!q->handshake||(q->handshake&7)||
       !q->audit||(q->audit&7)||q->generation>1||!gc_abi_valid_phase((PhaseSnapshot){g->phase}))return GC_MP_INVALID;
    HandshakeState *h=(HandshakeState *)(uintptr_t)q->handshake;
    if(h->active>1||h->native>h->members||!!h->head!=!!h->members||h->acknowledged>h->expected||
       (h->active?(!h->generation||h->expected!=h->members-h->native):(h->expected||h->acknowledged)))return GC_MP_INVALID;
    RootAudit *audit=(RootAudit *)(uintptr_t)q->audit;
    if(!audit->queue||(audit->queue&7))return GC_MP_INVALID;
    MarkQueue *queue=(MarkQueue *)(uintptr_t)audit->queue;
    if(!queue->epoch||queue->generation!=q->generation)return GC_MP_INVALID;
    unsigned shift=(unsigned)q->generation*4;
    uint64_t phase=(g->phase>>shift)&15;
    if(op==GC_MP_START) {
        if((q->state!=0&&q->state!=3)||phase!=1)return GC_MP_INVALID;
        if(h->active)return GC_MP_WAIT;
        if(h->generation>UINT64_MAX-2||(g->phase>>16)==(UINT64_MAX>>16))return GC_MP_EXHAUSTED;
        (void)gc_handshake_step_locked(h,NULL,GC_HS_BEGIN);
        q->state=4;q->rendezvous=h->generation;q->phase=g->phase;q->epoch=queue->epoch;q->outcome=0;
        return GC_MP_ADVANCED;
    }
    if(op==GC_MP_BEGIN) {
        if((q->state!=0&&q->state!=3)||phase!=2)return GC_MP_INVALID;
        if(h->active)return GC_MP_WAIT;
        if(h->generation>UINT64_MAX-2||(g->phase>>16)>(UINT64_MAX>>16)-2)return GC_MP_EXHAUSTED;
        // The same prevalidated registry makes BEGIN infallible here.
        (void)gc_handshake_step_locked(h,NULL,GC_HS_BEGIN);
        g->phase=((g->phase+UINT64_C(0x10000))&~(UINT64_C(15)<<shift))|(UINT64_C(3)<<shift);
        q->state=1;q->rendezvous=h->generation;q->phase=g->phase;q->epoch=queue->epoch;q->outcome=0;
        return GC_MP_ADVANCED;
    }
    if((op==GC_MP_DECIDE?q->state!=1:op==GC_MP_ACTIVATE?q->state!=4:
        (q->state!=2&&q->state!=5))||!q->rendezvous||!q->epoch)return GC_MP_INVALID;
    if(q->rendezvous!=h->generation||q->phase!=g->phase||q->epoch!=queue->epoch)return GC_MP_STALE;
    if(!h->active)return GC_MP_INVALID;
    if(h->acknowledged!=h->expected)return GC_MP_WAIT;
    if(op==GC_MP_RELEASE) {
        if((q->outcome!=2&&q->outcome!=4)||phase!=q->outcome)return GC_MP_INVALID;
        if(q->state==5&&q->outcome!=2)return GC_MP_INVALID;
        bool starting=q->state==5;
        (void)gc_handshake_step_locked(h,NULL,GC_HS_END);q->state=3;
        if(starting)return GC_MP_MARKING;
        return q->outcome==2?GC_MP_RETRY:GC_MP_SELECT;
    }
    if(phase!=(op==GC_MP_ACTIVATE?1u:3u)||q->outcome)return GC_MP_INVALID;
    if(h->generation==UINT64_MAX||(g->phase>>16)==(UINT64_MAX>>16))return GC_MP_EXHAUSTED;
    uint64_t next=2;
    if(op==GC_MP_DECIDE) {
        MarkEndRequest end={q->handshake,q->audit,q->rendezvous,q->remembered};
        uint64_t result=gc_mark_end_check_locked(g,&end,0);
        if(result==GC_ME_INVALID)return GC_MP_INVALID;
        next=result==GC_ME_READY?4:2;
    }
    // END and BEGIN are atomic with respect to owners because the lock remains
    // held. All failure gates precede them; BEGIN has reserved counter space.
    (void)gc_handshake_step_locked(h,NULL,GC_HS_END);
    (void)gc_handshake_step_locked(h,NULL,GC_HS_BEGIN);
    g->phase=((g->phase+UINT64_C(0x10000))&~(UINT64_C(15)<<shift))|(next<<shift);
    q->state=op==GC_MP_ACTIVATE?5:2;q->rendezvous=h->generation;q->phase=g->phase;q->outcome=next;
    return GC_MP_ADVANCED;
}
#endif
