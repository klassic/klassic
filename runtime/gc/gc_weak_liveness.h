/* Read-only canonical weak referent observation, after strong termination.
 * The weak-processing owner keeps admission closed and prevents selection or
 * reclamation of this referent until processing finishes. It owns an independent
 * authenticated metadata lease, a canonical access guard and stable record
 * storage. These protections do not make the weak referent strongly reachable.
 * Selected queue epoch/stamps are frozen; no queue lock may be acquired here.
 * Neither request, lease, mark stamps nor guard changes. DEAD is a decision,
 * not permission to clear/reclaim later without revalidating the owner and
 * slot serial. EXIT the guard before metadata cleanup or locking.
 */
#ifndef KLASSIC_GC_WEAK_LIVENESS_H
#define KLASSIC_GC_WEAK_LIVENESS_H
#include "gc_mark_prepare.h"
#include "gc_weak_mark.h"
#define GC_WEAK_LIVE 0u
#define GC_WEAK_DEAD 1u
#define GC_WEAK_LIVENESS_INVALID 2u
#define GC_WEAK_LIVENESS_WAIT 3u
typedef uint64_t (*WeakLiveness)(const GuardedMarkPrepare *,uint64_t,uint64_t);
static inline uint64_t gc_guarded_weak_liveness(const GuardedMarkPrepare *q,uint64_t generation,uint64_t epoch){
    if(!q||((uintptr_t)q&7)||generation>GC_OLD||!epoch||
        !gc_abi_valid_phase((PhaseSnapshot){q->phase})||((q->phase>>(4*generation))&15)!=GC_SELECT)return GC_WEAK_LIVENESS_INVALID;
    MarkRecord *record;uint64_t state;
    if(!gc_mark_prepare_record_state(q,&record,true,&state))return GC_WEAK_LIVENESS_INVALID;
    if(state==GC_OBJECT_INITIALIZING)return GC_WEAK_LIVENESS_WAIT;
    if(generation==GC_YOUNG&&((AllocationPage *)(uintptr_t)q->page)->generation==GC_OLD)return GC_WEAK_LIVE;
    uint64_t stamp=generation==GC_YOUNG?record->young_epoch:record->old_epoch;
    if(stamp>epoch)return GC_WEAK_LIVENESS_INVALID;
    return stamp==epoch?GC_WEAK_LIVE:GC_WEAK_DEAD;
}
/* Owner/phase/epoch-bound entry. The caller freezes these native controls for
 * the guarded interval, and retains weak-slot identity separately. A Select
 * phase alone cannot substitute for the completed mark-end owner. */
static inline bool gc_weak_mark_owner_valid(const WeakMarkControl *owner,uint64_t phase){
    if(!owner||((uintptr_t)owner&7)||owner->stage!=3||!owner->gate||((uintptr_t)owner->gate&7)||
        !owner->mark||((uintptr_t)owner->mark&7))return false;
    const WeakLoadGate *gate=owner->gate;const MarkPhaseControl *mark=owner->mark;
    if(gate->closed!=1||!gate->registry||(gate->registry&7)||!gate->handshake||(gate->handshake&7)||
        mark->handshake!=gate->handshake||mark->state!=3||mark->outcome!=GC_SELECT||mark->generation>1||
        !mark->epoch||mark->phase!=phase||!mark->audit||(mark->audit&7))return false;
    const RootRegistry *registry=(void *)(uintptr_t)gate->registry;
    const HandshakeState *h=(void *)(uintptr_t)gate->handshake;
    const RootAudit *audit=(void *)(uintptr_t)mark->audit;
    if(registry->phase!=mark->phase||h->active||!mark->rendezvous||h->generation!=mark->rendezvous||
        !audit->queue||(audit->queue&7))return false;
    const MarkQueue *queue=(void *)(uintptr_t)audit->queue;
    if(queue->epoch!=mark->epoch||queue->generation!=mark->generation)return false;
    return true;
}
static inline uint64_t gc_weak_mark_observe(const WeakMarkControl *owner,
        const GuardedMarkPrepare *q,WeakLiveness probe){
    if(!q||((uintptr_t)q&7)||!probe||!gc_weak_mark_owner_valid(owner,q->phase))return GC_WEAK_LIVENESS_INVALID;
    return probe(q,owner->mark->generation,owner->mark->epoch);
}
#endif
