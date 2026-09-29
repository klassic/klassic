#ifndef KLASSIC_GC_SURVIVOR_AGE_H
#define KLASSIC_GC_SURVIVOR_AGE_H
#include "gc_abi.h"
#define GC_AGE_YOUNG 0u
#define GC_AGE_PROMOTE 1u
#define GC_AGE_INVALID 2u
/* Young-cycle owner holds its queue/cycle lock and a lifetime protection for
 * this stable record. Caller proves the object survived the specified Young
 * cycle and is still Young. No Old-cycle/physical-copy event may call this as
 * a survival event. Cycle IDs are nonzero, monotonic and never reused; retries
 * in the same cycle do not increment age. Threshold is fixed for that cycle.
 * The return is a decision only: promotion, Old marking/remset obligations and
 * actual placement remain the owner's responsibility. All copies share this
 * record, so moving payloads cannot reset age. Pinned/retained survivors must
 * also be accounted by the owner. INVALID is completely nonmutating. */
typedef uint64_t (*SurvivorAge)(MarkRecord *,uint64_t,uint64_t);
static inline uint64_t gc_survivor_age_locked(MarkRecord *record,uint64_t cycle,uint64_t threshold){
    if(!record||((uintptr_t)record&7)||!record->object_id||!cycle||!threshold||threshold>GC_MAX_AGE||
        record->age>GC_MAX_AGE||(!record->age_epoch!=!record->age)||cycle<record->age_epoch)return GC_AGE_INVALID;
    if(cycle!=record->age_epoch){
        if(record->age<GC_MAX_AGE)record->age++;
        record->age_epoch=cycle;
    }
    return record->age>=threshold?GC_AGE_PROMOTE:GC_AGE_YOUNG;
}
/* Selection-phase entry: caller holds the Young queue/cycle lock, proves
 * termination for this epoch and retains this canonical Young object's record.
 * No payload/mark-record address may be borrowed from an unguarded old copy.
 * A matching mark stamp is required; an older stamp leaves age unchanged.
 * Old queues and future stamps are rejected. Repeated live selection is
 * idempotent via age_epoch. NOT_SURVIVED is not permission to reclaim storage.
 * Post-snapshot allocations and cross-generation survivors require their own
 * cycle admission; this leaf does not establish phase or retirement ownership. */
#define GC_AGE_NOT_SURVIVED 3u
typedef uint64_t (*MarkedSurvivorAge)(const MarkQueue *,MarkRecord *,uint64_t);
static inline uint64_t gc_marked_survivor_age_locked(const MarkQueue *queue,MarkRecord *record,uint64_t threshold){
    if(!queue||((uintptr_t)queue&7)||!record||((uintptr_t)record&7)||queue->generation!=GC_YOUNG||
       !queue->epoch||!record->object_id||!threshold||threshold>GC_MAX_AGE||record->young_epoch>queue->epoch)return GC_AGE_INVALID;
    if(record->young_epoch!=queue->epoch)return GC_AGE_NOT_SURVIVED;
    return gc_survivor_age_locked(record,queue->epoch,threshold);
}
#endif
