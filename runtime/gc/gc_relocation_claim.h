/* One bounded relocation admission + source claim, heap lock held. Caller owns
 * source metadata/liveness and supplies a fresh, stable, disjoint permit. The
 * source is NOT frozen by CLAIM: winner must SELECT/CHECK all access before
 * copying. No target has been prepared yet, so losers can release immediately.
 * The gate serial supplies the nonzero 62-bit ticket, never caller replay data.
 * OWNED leaves a live permit and returns ticket/epoch. Every other result leaves
 * no newly acquired permit and zero outputs. Failed admitted attempts consume a
 * gate serial, including ticket-width exhaustion; serials never roll back/wrap.
 * No polling, allocation, waiting, OS calls or heap payload access occur here.
 */
#ifndef KLASSIC_GC_RELOCATION_CLAIM_H
#define KLASSIC_GC_RELOCATION_CLAIM_H
#include "gc_relocation_gate.h"
#include "gc_forward.h"
enum { GC_RCL_OWNED=0,GC_RCL_INVALID=1,GC_RCL_BUSY=2,GC_RCL_RETAINED=3,
    GC_RCL_FORWARDED=4,GC_RCL_EXHAUSTED=5,GC_RCL_CLOSED=6 };
static inline uint64_t gc_relocation_claim_locked(RelocationGate *g,RelocationClaim *q){
    if(!q->permit||(q->permit&7)||!q->source||(q->source&7)||q->ticket||q->epoch||
        !q->source_offset||(q->source_offset&15)||q->source_offset>GC_MAX_OFFSET||!q->incarnation)return GC_RCL_INVALID;
    ForwardCell *source=(ForwardCell *)(uintptr_t)q->source;
    if(source->source_offset!=q->source_offset||source->incarnation!=q->incarnation)return GC_RCL_INVALID;
    RelocationGateRequest admission={.permit=q->permit};
    uint64_t status=gc_relocation_gate_locked(g,&admission,GC_RG_ACQUIRE);
    if(status==GC_RG_WAIT)return GC_RCL_CLOSED;
    if(status==GC_RG_EXHAUSTED)return GC_RCL_EXHAUSTED;
    if(status!=GC_RG_OK)return GC_RCL_INVALID;
    uint64_t result=GC_RCL_EXHAUSTED;
    if(admission.serial<=(UINT64_MAX>>2)){
        ForwardRequest claim={q->source_offset,q->incarnation,admission.serial,0};
        status=gc_forward_step(source,&claim,GC_FWD_CLAIM);
        if(status==GC_FWD_OK){q->ticket=admission.serial;q->epoch=admission.epoch;return GC_RCL_OWNED;}
        result=status==GC_FWD_BUSY?GC_RCL_BUSY:status==GC_FWD_RETAINED?GC_RCL_RETAINED:
            status==GC_FWD_PUBLISHED?GC_RCL_FORWARDED:GC_RCL_INVALID;
    }
    if(gc_relocation_gate_locked(g,&admission,GC_RG_RELEASE)!=GC_RG_OK)return GC_RCL_INVALID;
    return result;
}
/* Post-termination selection funnel, with the generation queue and heap/gate
 * locks held. The cycle owner previously captured this SAME canonical source
 * and native record under a guard, retains their lifetimes, and proves source
 * generation membership. No payload address crosses that capture boundary.
 * Queue/record epoch checks do not independently prove termination, pin state,
 * source lifetime or phase/color reuse safety. Those remain cycle obligations.
 * NOT_SURVIVED is not a reclamation certificate. STALE requires recapture.
 * The selection suffix is immutable. The embedded claim follows the exact
 * permit/serial/loser cleanup contract above; only matching live marks reach it. */
#define GC_RCL_NOT_SURVIVED 7u
#define GC_RCL_STALE 8u
typedef uint64_t (*MarkedRelocationClaimer)(RelocationGate *,MarkedRelocation *);
static inline uint64_t gc_marked_relocation_claim_locked(RelocationGate *g,MarkedRelocation *q){
    if(!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||!q->queue||(q->queue&7)||
       !q->record||(q->record&7)||!q->epoch)return GC_RCL_INVALID;
    const MarkQueue *queue=(const void *)(uintptr_t)q->queue;
    const MarkRecord *record=(const void *)(uintptr_t)q->record;
    if(queue->generation>GC_OLD||!queue->epoch||!record->object_id)return GC_RCL_INVALID;
    if(queue->epoch!=q->epoch)return GC_RCL_STALE;
    uint64_t stamp=queue->generation==GC_YOUNG?record->young_epoch:record->old_epoch;
    if(stamp>q->epoch)return GC_RCL_STALE;
    if(stamp!=q->epoch)return GC_RCL_NOT_SURVIVED;
    return gc_relocation_claim_locked(g,&q->claim);
}
#endif
