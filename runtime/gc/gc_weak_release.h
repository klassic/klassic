/* Explicit handle disposal under the same weak/heap locks as weak-load.
 * Controls are live/disjoint; slot belongs to this gate and serial. Release is
 * allowed while admission is closed. The owning table must retire any borrowed
 * scan claim before reusing storage, and issue a never-repeated serial on reuse.
 * This does not decide that an object is dead or clear an extant weak handle:
 * it invalidates the handle itself and returns its metadata lease. Null/cleared
 * handles have no lease. Rejection preserves both slot and metadata inventory.
 */
#ifndef KLASSIC_GC_WEAK_RELEASE_H
#define KLASSIC_GC_WEAK_RELEASE_H
#include "gc_weak_load.h"
#define GC_WEAK_RELEASED 0u
static inline uint64_t gc_weak_release_locked(WeakLoadGate *gate,WeakSlot *slot,uint64_t serial){
    if(!gate||((uintptr_t)gate&7)||!slot||((uintptr_t)slot&7)||!serial||
        !gate->pool||(gate->pool&7)||gate->closed>1||slot->gate!=(uintptr_t)gate||slot->serial!=serial)return GC_WEAK_INVALID;
    if(slot->reference){
        MetadataLeaseRequest checked=slot->lease;
        MetadataLeasePool *pool=(void *)(uintptr_t)gate->pool;
        if(gc_metadata_step_locked(pool,&checked,GC_ML_RESOLVE)!=GC_ML_OK||
            checked.page!=slot->lease.page||checked.incarnation!=slot->lease.incarnation)return GC_WEAK_INVALID;
        if(gc_metadata_step_locked(pool,&slot->lease,GC_ML_RELEASE)!=GC_ML_OK)return GC_WEAK_INVALID;
    }else if(slot->lease.page||slot->lease.incarnation||slot->lease.token)return GC_WEAK_INVALID;
    *slot=(WeakSlot){0};return GC_WEAK_RELEASED;
}
typedef uint64_t (*WeakReleaser)(WeakLoadGate *,WeakSlot *,uint64_t);
#endif
