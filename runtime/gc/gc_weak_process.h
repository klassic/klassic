/* One weak entry: capture -> follow/access -> observe -> EXIT -> commit.
 * BEGIN/ENTER/COMMIT/CANCEL hold the common weak/heap lock, without a guard.
 * OBSERVE/EXIT run outside locks with the owned guard; never poll in a guard.
 * The exclusive worker retains table/owner/pool/context storage. The scheduler
 * freezes the completed phase and excludes selection/reclamation for this weak
 * processing interval (also against the other generation). A metadata lease
 * alone is NOT that reclamation protection. Moves may occur; follow/access
 * retries one edge at a time and keep independent metadata ownership.
 *
 * Explicit handle release may race between stages. Serial and complete source
 * slot are revalidated at COMMIT. A disappeared/replaced entry is not touched.
 * DEAD clears the referent/lease, preserving handle identity. LIVE adopts the
 * canonical reference/lease, releasing old source metadata. Neither marks or
 * publishes a strong root. Cleanup failure retains its exact private leases.
 * This does not scan a whole table, reopen admission, or authorize reclamation.
 * Stages: 0 fresh,1 captured,2 guarded,3 observed,4 exited,5 cleanup,6 complete.
 */
#ifndef KLASSIC_GC_WEAK_PROCESS_H
#define KLASSIC_GC_WEAK_PROCESS_H
#include "gc_weak_table.h"
#include "gc_weak_liveness.h"
#include "gc_pending_access.h"
#include <string.h>
enum { GC_WP_OK=0,GC_WP_WAIT=1,GC_WP_INVALID=2,GC_WP_FULL=3,GC_WP_EXHAUSTED=4,
    GC_WP_ADVANCED=5,GC_WP_DEAD=6,GC_WP_LIVE=7,GC_WP_STALE=8,GC_WP_EMPTY=9,GC_WP_CLEANUP=10 };

typedef struct {
    uint64_t (*acquire)(MetadataLeasePool *,ReferenceLease *);
    PendingFollower follow;PendingAccessor access;
    uint64_t (*resolve)(const AllocationPage *,ResolveRequest *,uint64_t);
    WeakLiveness observe;
    uint64_t (*metadata)(MetadataLeasePool *,MetadataLeaseRequest *,uint64_t);
} WeakProcessOps;
static inline bool gc_weak_process_ops(const WeakProcessOps *ops){
    return ops&&ops->acquire&&ops->follow&&ops->access&&ops->resolve&&ops->observe&&ops->metadata;
}
static inline bool gc_weak_process_context(const MutatorContext *ctx){
    return ctx&&!((uintptr_t)ctx&7)&&ctx->abi_version==GC_ABI_VERSION&&ctx->thread_id&&
        atomic_load_explicit(&ctx->state,memory_order_acquire)==GC_RUNNING;
}
static inline bool gc_weak_process_owner(const WeakProcess *q){
    return q->owner->processor==(uintptr_t)q&&gc_weak_mark_owner_valid(q->owner,q->phase)&&q->owner->mark->epoch==q->epoch&&
        q->owner->mark->generation==q->generation&&q->owner->mark->rendezvous==q->rendezvous&&
        q->table->gate==q->owner->gate&&q->owner->gate->pool==(uintptr_t)q->pool&&
        atomic_load_explicit(&q->context->adopted_phase,memory_order_acquire)==q->phase;
}
static inline uint64_t gc_weak_process_begin_locked(WeakTable *table,WeakMarkControl *owner,
        MutatorContext *ctx,WeakHandle handle,WeakProcess *q,const WeakProcessOps *ops){
    WeakProcess zero={0};
    if(!q||((uintptr_t)q&7)||memcmp(q,&zero,sizeof(zero))||!gc_weak_process_ops(ops)||
        !gc_weak_process_context(ctx)||atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||
        !gc_weak_table_valid(table)||!gc_weak_mark_owner_valid(owner,atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire))||
        table->gate!=owner->gate||owner->processor||(owner->scan&&owner->scan!=(uintptr_t)q))return GC_WP_INVALID;
    WeakEntry *e=gc_weak_table_entry_locked(table,handle);
    if(!e)return GC_WP_STALE;
    if(!e->slot.reference)return e->slot.lease.page||e->slot.lease.incarnation||e->slot.lease.token?GC_WP_INVALID:GC_WP_EMPTY;
    MetadataLeasePool *pool=(void *)(uintptr_t)table->gate->pool;
    MetadataLeaseRequest checked=e->slot.lease;
    if(ops->metadata(pool,&checked,GC_ML_RESOLVE)!=GC_ML_OK||
        checked.page!=e->slot.lease.page||checked.incarnation!=e->slot.lease.incarnation)return GC_WP_INVALID;
    ReferenceLease current={.reference=e->slot.reference};uint64_t status=ops->acquire(pool,&current);
    if(status!=GC_ML_OK)return status==GC_ML_FULL?GC_WP_FULL:status==GC_ML_EXHAUSTED?GC_WP_EXHAUSTED:GC_WP_INVALID;
    *q=(WeakProcess){.table=table,.owner=owner,.pool=pool,.context=ctx,.handle=handle,.source=e->slot,
        .phase=owner->mark->phase,.epoch=owner->mark->epoch,.generation=owner->mark->generation,
        .rendezvous=owner->mark->rendezvous,.stage=1,.current=current};
    owner->processor=(uintptr_t)q;
    return GC_WP_OK;
}
/* Fixed entry for native emission. BEGIN only; request stays borrowed and unchanged.
 * Caller holds the common weak/heap lock and freezes the completed mark owner. */
typedef uint64_t (*WeakProcessCapture)(WeakProcess *,const WeakProcessStart *,uint64_t);
static inline uint64_t gc_weak_process_capture_locked(WeakProcess *q,const WeakProcessStart *start,uint64_t op){
    if(!start||((uintptr_t)start&7)||op)return GC_WP_INVALID;
    const WeakProcessOps ops={gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked};
    return gc_weak_process_begin_locked(start->table,start->owner,start->context,start->handle,q,&ops);
}
static inline uint64_t gc_weak_process_enter_locked(WeakProcess *q,const WeakProcessOps *ops){
    if(!q||((uintptr_t)q&7)||q->stage!=1||!gc_weak_process_ops(ops)||!gc_weak_process_context(q->context)||
        atomic_load_explicit(&q->context->access_epoch,memory_order_acquire)||!gc_weak_process_owner(q))return GC_WP_INVALID;
    if(q->retired.token&&ops->metadata(q->pool,&q->retired,GC_ML_RELEASE)!=GC_ML_OK)return GC_WP_CLEANUP;
    uint64_t result=ops->follow(q->pool,&q->current,&q->retired);
    if(result!=GC_RF_DONE)return result==GC_RF_ADVANCED?GC_WP_ADVANCED:result==GC_RF_WAIT?GC_WP_WAIT:
        result==GC_RF_METADATA_FULL?GC_WP_FULL:result==GC_RF_EXHAUSTED?GC_WP_EXHAUSTED:
        result==GC_RF_CLEANUP_REQUIRED?GC_WP_CLEANUP:GC_WP_INVALID;
    q->access=(ResolveRequest){.scope={.context=(uintptr_t)q->context}};
    result=ops->access(q->pool,&q->current,&q->access);
    if(result==GC_RAC_READY){q->stage=2;return GC_WP_OK;}
    q->access=(ResolveRequest){0};
    return result==GC_RF_ADVANCED?GC_WP_ADVANCED:result==GC_RF_WAIT?GC_WP_WAIT:
        result==GC_RF_EXHAUSTED?GC_WP_EXHAUSTED:GC_WP_INVALID;
}
static inline uint64_t gc_weak_process_observe(WeakProcess *q,const WeakProcessOps *ops){
    if(!q||((uintptr_t)q&7)||q->stage!=2||!gc_weak_process_ops(ops)||!gc_weak_process_context(q->context)||
        !gc_weak_process_owner(q))return GC_WP_INVALID;
    GuardedMarkPrepare access={.scope=(uintptr_t)&q->access.scope,.page=q->current.lease.page,
        .phase=q->phase,.reference=q->current.reference,.lease=q->current.lease};
    uint64_t status=gc_weak_mark_observe(q->owner,&access,ops->observe);
    if(status==GC_WEAK_LIVENESS_WAIT)return GC_WP_WAIT;
    if(status!=GC_WEAK_LIVE&&status!=GC_WEAK_DEAD)return GC_WP_INVALID;
    q->result=status==GC_WEAK_LIVE?GC_WP_LIVE:GC_WP_DEAD;q->stage=3;return q->result;
}
/* EXIT is allowed even after a stale owner: guard cleanup must remain possible. */
static inline uint64_t gc_weak_process_exit(WeakProcess *q,const WeakProcessOps *ops){
    if(!q||((uintptr_t)q&7)||(q->stage!=2&&q->stage!=3)||!gc_weak_process_ops(ops))return GC_WP_INVALID;
    uint64_t result=ops->resolve((void *)(uintptr_t)q->current.lease.page,&q->access,GC_RESOLVE_EXIT);
    if(result!=GC_RESOLVE_OK)return GC_WP_INVALID;
    q->access=(ResolveRequest){0};q->stage=q->stage==3?4:1;return GC_WP_OK;
}
/* ENTER holds the common lock without a guard; OBSERVE/EXIT run outside it.
 * EXIT deliberately bypasses owner validation so stale work can drop its guard. */
enum { GC_WPA_ENTER=0,GC_WPA_OBSERVE=1,GC_WPA_EXIT=2 };
typedef uint64_t (*WeakProcessAccess)(WeakProcess *,uint64_t,uint64_t);
static inline uint64_t gc_weak_process_access_step(WeakProcess *q,uint64_t op,uint64_t reserved){
    if(reserved||op>GC_WPA_EXIT)return GC_WP_INVALID;
    const WeakProcessOps ops={gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked};
    if(op==GC_WPA_ENTER)return gc_weak_process_enter_locked(q,&ops);
    if(op==GC_WPA_OBSERVE)return gc_weak_process_observe(q,&ops);
    return gc_weak_process_exit(q,&ops);
}
static inline uint64_t gc_weak_process_cleanup_locked(WeakProcess *q,const WeakProcessOps *ops){
    if(q->owner->processor!=(uintptr_t)q)return GC_WP_INVALID;
    if(q->retired.token&&ops->metadata(q->pool,&q->retired,GC_ML_RELEASE)!=GC_ML_OK)return GC_WP_CLEANUP;
    if(q->current.lease.token&&ops->metadata(q->pool,&q->current.lease,GC_ML_RELEASE)!=GC_ML_OK)return GC_WP_CLEANUP;
    q->current=(ReferenceLease){0};q->stage=6;q->owner->processor=0;return q->result;
}
static inline uint64_t gc_weak_process_commit_locked(WeakProcess *q,const WeakProcessOps *ops){
    if(!q||((uintptr_t)q&7)||(q->stage!=4&&q->stage!=5)||!gc_weak_process_ops(ops)||
        !gc_weak_process_context(q->context)||atomic_load_explicit(&q->context->access_epoch,memory_order_acquire))return GC_WP_INVALID;
    if(q->stage==5)return gc_weak_process_cleanup_locked(q,ops);
    if(!gc_weak_process_owner(q)||(q->result!=GC_WP_LIVE&&q->result!=GC_WP_DEAD))return GC_WP_INVALID;
    MetadataLeaseRequest checked=q->current.lease;
    if(ops->metadata(q->pool,&checked,GC_ML_RESOLVE)!=GC_ML_OK||checked.page!=q->current.lease.page||
        checked.incarnation!=q->current.lease.incarnation)return GC_WP_INVALID;
    WeakEntry *e=gc_weak_table_entry_locked(q->table,q->handle);
    if(!e||memcmp(&e->slot,&q->source,sizeof(q->source))){q->result=GC_WP_STALE;q->stage=5;return gc_weak_process_cleanup_locked(q,ops);}
    checked=e->slot.lease;
    if(ops->metadata(q->pool,&checked,GC_ML_RESOLVE)!=GC_ML_OK||checked.page!=e->slot.lease.page||
        checked.incarnation!=e->slot.lease.incarnation)return GC_WP_INVALID;
    if(ops->metadata(q->pool,&e->slot.lease,GC_ML_RELEASE)!=GC_ML_OK)return GC_WP_CLEANUP;
    if(q->result==GC_WP_LIVE){
        e->slot.reference=q->current.reference;e->slot.lease=q->current.lease;q->current=(ReferenceLease){0};
    }else e->slot.reference=0;
    q->stage=5;return gc_weak_process_cleanup_locked(q,ops);
}
static inline uint64_t gc_weak_process_cancel_locked(WeakProcess *q,const WeakProcessOps *ops){
    if(!q||((uintptr_t)q&7)||(q->stage!=1&&q->stage!=4&&q->stage!=5)||!gc_weak_process_ops(ops)||
        !gc_weak_process_context(q->context)||atomic_load_explicit(&q->context->access_epoch,memory_order_acquire))return GC_WP_INVALID;
    if(q->stage!=5){q->result=GC_WP_STALE;q->stage=5;}
    return gc_weak_process_cleanup_locked(q,ops);
}
/* Both operations hold the common weak/heap lock outside every access guard.
 * Cleanup retries retain the processor claim until all private leases leave. */
enum { GC_WPF_COMMIT=0,GC_WPF_CANCEL=1 };
typedef uint64_t (*WeakProcessFinish)(WeakProcess *,uint64_t,uint64_t);
static inline uint64_t gc_weak_process_finish_locked(WeakProcess *q,uint64_t op,uint64_t reserved){
    if(reserved||op>GC_WPF_CANCEL)return GC_WP_INVALID;
    const WeakProcessOps ops={gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked};
    return op==GC_WPF_COMMIT?gc_weak_process_commit_locked(q,&ops):gc_weak_process_cancel_locked(q,&ops);
}
#endif
