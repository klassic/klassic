/* One weak forwarding edge under the common weak/heap lock, outside guards.
 * The scheduler retains the completed-mark liveness/reclamation exclusion;
 * metadata ownership alone does not establish that the referent is alive.
 * Table, pool, callback and cleanup storage are live, disjoint native controls.
 * The slot owns current metadata throughout: a destination lease is acquired
 * before replacing its reference and releasing the source. No strong root,
 * journal publication, payload access, allocation or polling occurs here.
 *
 * CLEANUP_REQUIRED may already have retargeted the slot. Retain the exact
 * retired token and release it via the same pool's metadata API before retry.
 * Explicit handle release can dispose the new slot meanwhile; it cannot own
 * this separate retired token. Never discard cleanup on stale handle failure.
 * DONE is a forwarding observation, not a whole-table retirement certificate;
 * later access still needs canonical resolution and a finite guard. A caller
 * auditing retirement must exclude future edges into its retiring source.
 */
#ifndef KLASSIC_GC_WEAK_FOLLOW_H
#define KLASSIC_GC_WEAK_FOLLOW_H
#include "gc_weak_table.h"
#include "gc_pending_follow.h"
static inline uint64_t gc_weak_table_follow_locked(WeakTable *table,WeakHandle handle,
        MetadataLeaseRequest *retired,PendingFollower follow){
    if(!retired||((uintptr_t)retired&7)||!follow)return GC_RF_INVALID;
    if(retired->page||retired->incarnation||retired->token)return GC_RF_CLEANUP_REQUIRED;
    WeakEntry *entry=gc_weak_table_entry_locked(table,handle);
    if(!entry)return GC_RF_INVALID;
    if(!entry->slot.reference)return entry->slot.lease.page||entry->slot.lease.incarnation||entry->slot.lease.token?
        GC_RF_INVALID:GC_RF_DONE;
    if(!entry->slot.lease.page||!entry->slot.lease.incarnation||!entry->slot.lease.token)return GC_RF_INVALID;
    ReferenceLease current={entry->slot.reference,entry->slot.lease};
    uint64_t result=follow((void *)(uintptr_t)table->gate->pool,&current,retired);
    entry->slot.reference=current.reference;entry->slot.lease=current.lease;
    return result;
}
/* Fixed three-register ABI for direct backends. The request retains cleanup
 * independently of slot identity; no retry silently disposes that obligation. */
typedef uint64_t (*WeakTableFollower)(WeakTable *,WeakFollowRequest *,uint64_t);
static inline uint64_t gc_weak_follow_step_locked(WeakTable *table,WeakFollowRequest *q,uint64_t op){
    if(op||!q||((uintptr_t)q&7))return GC_RF_INVALID;
    WeakHandle handle={(uintptr_t)table,q->index,q->serial};
    return gc_weak_table_follow_locked(table,handle,&q->retired,gc_pending_follow_locked);
}
#endif
