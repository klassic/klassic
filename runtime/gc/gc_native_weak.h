/* Nonmoving weak-table backing. Reserve rounded bytes under the heap lock,
 * map/initialize outside locks and guards (native boundary), then COMMIT before
 * exposing the table. The gate and metadata pool outlive this package.
 * Disposal is only for an unpublished package or runtime shutdown after all
 * mutators, workers, handle users and borrowed scans are quiescent. Empty does
 * NOT prove that stale handles have ceased to exist. Never recycle a published
 * table identity during a runtime's lifetime. Linear initialization/disposal
 * validation is outside pause/lock-critical work. Failed unmap retains both
 * mapping and charge; RELEASE/ABORT follows successful unmap only.
 */
#ifndef KLASSIC_GC_NATIVE_WEAK_H
#define KLASSIC_GC_NATIVE_WEAK_H
#include "gc_native_metadata.h"
#include "gc_weak_table.h"
static inline uint64_t gc_native_weak_size(uint64_t capacity,uint64_t quantum,uint64_t *bytes){
    if(!capacity||capacity>UINT32_MAX)return GC_NATIVE_METADATA_INVALID;
    return gc_native_metadata_size(sizeof(WeakTable)+capacity*sizeof(WeakEntry),quantum,bytes);
}
static inline uint64_t gc_native_weak_prepare(MetadataCharge *charge,WeakLoadGate *gate,
        uint64_t capacity,const NativeMetadataOps *ops){
    uint64_t bytes=0;
    if(!gc_native_metadata_charge_valid(charge,ops)||!gate||((uintptr_t)gate&7)||
        !gate->pool||(gate->pool&7)||gate->closed>1||
        ((MetadataLeasePool *)(uintptr_t)gate->pool)->heap!=charge->owner||
        gc_native_weak_size(capacity,ops->quantum,&bytes)!=GC_NATIVE_METADATA_OK||charge->bytes<bytes)
        return GC_NATIVE_METADATA_INVALID;
    uint64_t status=gc_native_metadata_prepare(charge,ops);
    if(status!=GC_NATIVE_METADATA_OK)return status;
    WeakTable *t=(void *)(uintptr_t)charge->allocation;
    *t=(WeakTable){gate,(WeakEntry *)(t+1),capacity,1,0,0};
    for(uint64_t i=0;i<capacity;i++)t->entries[i]=(WeakEntry){.next_free=i+1<capacity?i+2:0};
    return GC_NATIVE_METADATA_OK;
}
/* Read-only committed-package lookup under the common lock. Storage is trusted
 * and was initialized above; no entry scan or OS call. */
static inline WeakTable *gc_native_weak_committed(const MetadataCharge *charge){
    if(!charge||((uintptr_t)charge&7)||charge->stage!=2||!charge->owner||(charge->owner&7)||
        !charge->allocation||(charge->allocation&7)||charge->bytes<sizeof(WeakTable)||
        charge->allocation>UINT64_MAX-charge->bytes)return NULL;
    WeakTable *t=(void *)(uintptr_t)charge->allocation;
    if(!gc_weak_table_valid(t)||t->entries!=(WeakEntry *)(t+1)||
        sizeof(*t)+t->capacity*sizeof(WeakEntry)>charge->bytes||
        ((MetadataLeasePool *)(uintptr_t)t->gate->pool)->heap!=charge->owner)return NULL;
    return t;
}
static inline uint64_t gc_native_weak_dispose(MetadataCharge *charge,const NativeMetadataOps *ops){
    if(!gc_native_metadata_charge_valid(charge,ops)||(charge->stage!=1&&charge->stage!=2)||
        !charge->allocation||(charge->allocation&(ops->quantum-1))||charge->bytes<sizeof(WeakTable)||
        charge->allocation>UINT64_MAX-charge->bytes)return GC_NATIVE_METADATA_INVALID;
    WeakTable *t=(void *)(uintptr_t)charge->allocation;
    if(!gc_weak_table_valid(t)||t->active||t->entries!=(WeakEntry *)(t+1)||
        sizeof(*t)+t->capacity*sizeof(WeakEntry)>charge->bytes||
        ((MetadataLeasePool *)(uintptr_t)t->gate->pool)->heap!=charge->owner)return GC_NATIVE_METADATA_INVALID;
    for(uint64_t i=0;i<t->capacity;i++){
        WeakEntry *e=&t->entries[i];
        if(e->slot.gate||e->slot.serial||e->slot.reference||e->slot.lease.page||e->slot.lease.incarnation||e->slot.lease.token||
            e->next_free>t->capacity)return GC_NATIVE_METADATA_INVALID;
    }
    uint64_t index=t->free_head;
    for(uint64_t i=0;i<t->capacity;i++){
        if(!index)return GC_NATIVE_METADATA_INVALID;
        index=t->entries[index-1].next_free;
    }
    if(index)return GC_NATIVE_METADATA_INVALID;
    return gc_native_metadata_dispose(charge,ops);
}
#endif
