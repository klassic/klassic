/* Private, nonmoving root storage. Reserve its rounded metadata charge before
 * mapping outside locks/guards in native state. Commit the charge before root
 * registration. Retirement, journal drain and every scanner lease must finish
 * before unmapping; only successful unmapping permits budget RELEASE/ABORT.
 * The registration node shares this mapping and its lifetime/budget.
 * An initializer failure may leave private header writes in an owned mapping;
 * retain the charge and dispose that mapping before ABORT. Failed unmapping
 * retains both resources for retry.
 * Preparation does not register a scope or attach a mutator. */
#ifndef KLASSIC_GC_NATIVE_ROOTS_H
#define KLASSIC_GC_NATIVE_ROOTS_H
#include "gc_native_metadata.h"
#include "gc_root_storage.h"
#include "gc_root_lifetime.h"
typedef RootStorage NativeRoots;
typedef uint64_t (*NativeRootDisposer)(MetadataCharge *);
/* Reserved charge; immutable spec.bytes must equal the rounded charge. */
typedef uint64_t (*NativeRootPreparer)(MetadataCharge *,const RootStorageSpec *);
static inline uint64_t gc_native_roots_size(uint64_t cells,uint64_t entries,uint64_t quantum,uint64_t *bytes){
    if(!cells||cells>UINT32_MAX||!entries||entries>UINT32_MAX)return GC_NATIVE_METADATA_INVALID;
    return gc_native_metadata_size(sizeof(NativeRoots)+cells*sizeof(RootCell)+entries*sizeof(RootChange),quantum,bytes);
}
static inline uint64_t gc_native_roots_prepare(MetadataCharge *charge,uint64_t owner,
        uint64_t cells,uint64_t entries,const NativeMetadataOps *ops,RootStoragePreparer prepare){
    uint64_t bytes=0;
    if(!charge||((uintptr_t)charge&7)||!owner||!ops||!prepare||gc_native_roots_size(cells,entries,ops->quantum,&bytes)!=GC_NATIVE_METADATA_OK||charge->bytes<bytes)
        return GC_NATIVE_METADATA_INVALID;
    uint64_t status=gc_native_metadata_prepare(charge,ops);
    if(status!=GC_NATIVE_METADATA_OK)return status;
    RootStorageSpec spec={owner,cells,entries,charge->bytes};
    return prepare((void *)(uintptr_t)charge->allocation,&spec)==GC_ROOT_STORAGE_OK?
        GC_NATIVE_METADATA_OK:GC_NATIVE_METADATA_INVALID;
}
/* Exclusive backing owner only: no concurrent attach or new publication may
 * start. This rejects live/retiring/leased storage; it is not a substitute for
 * registry synchronization or permission to inspect a foreign mapping. Once
 * FINISH unlinks every registry/owner node, the private owner may unmap outside
 * the registry lock. OS failure keeps the complete package and charge intact. */
static inline uint64_t gc_native_roots_dispose(MetadataCharge *charge,const NativeMetadataOps *ops,RootStorageDisposalCheck check){
    if(!check||!gc_native_metadata_charge_valid(charge,ops)||(charge->stage!=1&&charge->stage!=2)||
        !charge->allocation||(charge->allocation&(ops->quantum-1))||charge->bytes<sizeof(NativeRoots))
        return GC_NATIVE_METADATA_INVALID;
    if(check((const void *)(uintptr_t)charge->allocation)!=GC_ROOT_STORAGE_OK)return GC_NATIVE_METADATA_INVALID;
    return gc_native_metadata_dispose(charge,ops);
}
/* Publish a successfully prepared, committed root package under the common
 * registry mutex, after this attached owner has resumed Running/adopted phase.
 * The caller exclusively owns the charge/mapping until ATTACH succeeds, then
 * retains it through full root retirement and scanner/journal completion.
 * Check the fixed package geometry, not every cell: preparation established
 * the zeroed cells outside the lock. No allocation, heap lock, scan or wait.
 * Charge accounting is unchanged. Descriptor/heap startup publication remains
 * the bootstrap owner's separate responsibility. Rejection changes no state. */
typedef uint64_t (*NativeRootPublisher)(RootRegistry *,MutatorContext *,const MetadataCharge *);
static inline uint64_t gc_native_roots_publish_locked(RootRegistry *registry,MutatorContext *ctx,const MetadataCharge *charge){
    if(!registry||((uintptr_t)registry&7)||!ctx||((uintptr_t)ctx&7)||!charge||((uintptr_t)charge&7)||
        charge->stage!=2||!charge->owner||(charge->owner&7)||!charge->allocation||(charge->allocation&7)||
        charge->bytes<sizeof(RootStorage)||charge->allocation>UINT64_MAX-charge->bytes)return GC_RL_INVALID;
    RootStorage *s=(void *)(uintptr_t)charge->allocation;
    if(s->registration.segment!=(uintptr_t)&s->segment||s->segment.cells!=(uintptr_t)(s+1)||
        s->segment.journal!=(uintptr_t)&s->journal||!s->segment.capacity||s->segment.capacity>UINT32_MAX||
        !s->journal.capacity||s->journal.capacity>UINT32_MAX||
        sizeof(*s)+s->segment.capacity*sizeof(RootCell)+s->journal.capacity*sizeof(RootChange)>charge->bytes||
        s->journal.entries!=(uintptr_t)(s+1)+s->segment.capacity*sizeof(RootCell)||
        s->journal.top||atomic_load_explicit(&s->journal.published,memory_order_acquire)||atomic_load_explicit(&s->journal.consumed,memory_order_acquire)||
        gc_root_storage_can_dispose(s)!=GC_ROOT_STORAGE_OK)return GC_RL_INVALID;
    RootLifeRequest attach={.registration=(uintptr_t)&s->registration,.context=(uintptr_t)ctx};
    return gc_root_life_locked(registry,&attach,GC_RL_ATTACH);
}
#endif
