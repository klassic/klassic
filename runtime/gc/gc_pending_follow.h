/* One forwarding edge for a counted strong pending reference (journal item,
 * root-walk item or worker job), under the heap lock. The external producer
 * remains registered/counted until the caller completes its actual handoff;
 * a metadata lease alone is not a strong root or a termination obligation.
 * A weak-processing owner may instead retain a completed-epoch reclamation
 * exclusion across this call and every retry; it must not count weak work as
 * strong or let selection/reclamation proceed while that exclusion is held.
 * A post-mark weak retargeting owner can use the same exclusion after moving;
 * completed liveness and no premature metadata/source reuse remain required.
 * Controls/pool/metadata are live, unique, disjoint and stable. The caller may
 * initialize current.reference once, then only this entry changes it. No raw
 * guard may be held; this entry never reads managed memory or waits.
 *
 * Fresh current.lease and retired are zero. STEP retains current metadata on
 * WAIT/FULL/INVALID; a successful edge acquires destination metadata BEFORE
 * replacing the current reference or releasing the source. ADVANCED allows
 * rescheduling while retaining the external pending obligation and current
 * lease. DONE retains that lease and still requires guarded canonical access:
 * a concurrent mover can start after this check. No color/epoch is retired.
 *
 * A failed source release preserves retired and returns CLEANUP_REQUIRED.
 * Release that exact token with the metadata API before retrying. Likewise,
 * cancellation must clean retired and current.lease before dropping control;
 * it does NOT authorize ACK of unprocessed journal/root/worker work.
 */
#ifndef KLASSIC_GC_PENDING_FOLLOW_H
#define KLASSIC_GC_PENDING_FOLLOW_H
#include "gc_root_follow.h"
typedef uint64_t (*PendingFollower)(MetadataLeasePool *,ReferenceLease *,MetadataLeaseRequest *);
static inline uint64_t gc_pending_follow_locked(MetadataLeasePool *g,ReferenceLease *q,MetadataLeaseRequest *retired){
    if(!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||!retired||((uintptr_t)retired&7))return GC_RF_INVALID;
    if(retired->page||retired->incarnation||retired->token)return GC_RF_CLEANUP_REQUIRED;
    if(!q->reference)return q->lease.page||q->lease.incarnation||q->lease.token?GC_RF_INVALID:GC_RF_DONE;
    uint64_t status;
    if(!q->lease.page&&!q->lease.incarnation&&!q->lease.token){
        status=gc_reference_lease_locked(g,q);
        if(status!=GC_ML_OK)return status==GC_ML_FULL?GC_RF_METADATA_FULL:status==GC_ML_EXHAUSTED?GC_RF_EXHAUSTED:GC_RF_INVALID;
    }else{
        MetadataLeaseRequest checked=q->lease;
        if(gc_metadata_step_locked(g,&checked,GC_ML_RESOLVE)!=GC_ML_OK||checked.page!=q->lease.page||checked.incarnation!=q->lease.incarnation)return GC_RF_INVALID;
    }
    AllocationPage *page=(void *)(uintptr_t)q->lease.page;
    if(!page->forward_table||(page->forward_table&7))return GC_RF_INVALID;
    ForwardLookup lookup={q->reference>>16,q->lease.incarnation,0};
    if(gc_forward_lookup((void *)(uintptr_t)page->forward_table,&lookup)!=GC_FL_OK)return GC_RF_INVALID;
    ForwardRequest read={lookup.source_offset,lookup.incarnation,0,0};
    status=gc_forward_step((void *)(uintptr_t)lookup.cell,&read,GC_FWD_READ);
    if(status==GC_FWD_BUSY)return GC_RF_WAIT;
    if(status==GC_FWD_EMPTY||status==GC_FWD_RETAINED)return GC_RF_DONE;
    if(status!=GC_FWD_OK)return GC_RF_INVALID;
    ReferenceLease destination={.reference=read.destination};
    status=gc_reference_lease_locked(g,&destination);
    if(status!=GC_ML_OK)return status==GC_ML_FULL?GC_RF_METADATA_FULL:status==GC_ML_EXHAUSTED?GC_RF_EXHAUSTED:GC_RF_INVALID;
    *retired=q->lease;*q=destination;
    return gc_metadata_step_locked(g,retired,GC_ML_RELEASE)==GC_ML_OK?GC_RF_ADVANCED:GC_RF_CLEANUP_REQUIRED;
}
#endif
