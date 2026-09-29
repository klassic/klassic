/* Private startup admission under the heap lock. Reserve both metadata
 * packages before any native mapping or publication. Preparation and disposal
 * still run outside locks/guards; each resulting charge is committed, aborted
 * or released independently by its unique owner. This does not attach a
 * mutator, initialize the heap, or publish a usable runtime.
 * All control storage is live, disjoint and exclusively owned by the caller. */
#ifndef KLASSIC_GC_NATIVE_STARTUP_H
#define KLASSIC_GC_NATIVE_STARTUP_H
#include "gc_native_roots.h"
#include "gc_native_builtin_registry.h"
static inline uint64_t gc_native_startup_reserve_locked(HeapPool *heap,
        MetadataCharge *roots,MetadataCharge *descriptors,uint64_t cells,
        uint64_t entries,uint64_t capacity,uint64_t quantum){
    if(!heap||((uintptr_t)heap&7)||!roots||((uintptr_t)roots&7)||
        !descriptors||((uintptr_t)descriptors&7)||roots==descriptors)
        return GC_BUDGET_INVALID;
    if(roots->bytes||roots->owner||roots->allocation||roots->stage||
        descriptors->bytes||descriptors->owner||descriptors->allocation||descriptors->stage)
        return GC_BUDGET_INVALID;
    uint64_t root_bytes=0,descriptor_bytes=0;
    if(gc_native_roots_size(cells,entries,quantum,&root_bytes)!=GC_NATIVE_METADATA_OK||
        gc_native_builtin_registry_size(capacity,quantum,&descriptor_bytes)!=GC_NATIVE_METADATA_OK||
        root_bytes>UINT64_MAX-descriptor_bytes)return GC_BUDGET_INVALID;
    MetadataCharge total={.bytes=root_bytes+descriptor_bytes};
    uint64_t status=gc_budget_step_locked(heap,&total,GC_BUDGET_RESERVE);
    if(status!=GC_BUDGET_OK)return status;
    /* Transfer the single private reservation into two disjoint charges.
     * Their byte sum is unchanged; the temporary never escapes this call. */
    *roots=(MetadataCharge){.bytes=root_bytes,.owner=total.owner,.stage=1};
    *descriptors=(MetadataCharge){.bytes=descriptor_bytes,.owner=total.owner,.stage=1};
    return GC_BUDGET_OK;
}
typedef uint64_t (*NativeStartupPreparer)(const NativeStartupRequest *,uint64_t);
typedef uint64_t (*NativeStartupReserver)(HeapPool *,const NativeStartupRequest *);
static inline uint64_t gc_native_startup_request_locked(HeapPool *heap,const NativeStartupRequest *q){
    if(!q||((uintptr_t)q&7))return GC_BUDGET_INVALID;
    return gc_native_startup_reserve_locked(heap,(void *)(uintptr_t)q->roots,
        (void *)(uintptr_t)q->descriptors,q->cells,q->entries,q->capacity,q->quantum);
}
/* One private preparation attempt outside locks/guards, in native state for
 * attached callers. No budget commit or root registration occurs here.
 * On failure, either charge may retain a mapping: dispose every owned mapping
 * before ABORT. Do not retry preparation on a partially prepared package.
 * Native callbacks may supply direct generated leaves; NULL uses the C entry. */
typedef struct {
    const NativeMetadataOps *native;
    NativeRootPreparer roots;
    NativeBuiltinRegistryPreparer descriptors;
    RootStoragePreparer initialize_roots;
    void (*initialize_descriptors)(BuiltinDescriptors *);
    uint64_t (*register_descriptor)(const DescriptorRegistry *,const TypeDescriptor *);
} NativeStartupPreparationOps;
static inline uint64_t gc_native_startup_prepare(const NativeStartupRequest *q,uint64_t owner,
        const NativeStartupPreparationOps *ops){
    if(!q||((uintptr_t)q&7)||!owner||!ops||!ops->native||
        (!ops->roots&&!ops->initialize_roots)||
        (!ops->descriptors&&(!ops->initialize_descriptors||!ops->register_descriptor)))
        return GC_NATIVE_METADATA_INVALID;
    MetadataCharge *roots=(void *)(uintptr_t)q->roots,*descriptors=(void *)(uintptr_t)q->descriptors;
    if(roots==descriptors||!gc_native_metadata_charge_valid(roots,ops->native)||
        !gc_native_metadata_charge_valid(descriptors,ops->native)||
        roots->stage!=1||descriptors->stage!=1||roots->allocation||descriptors->allocation||
        roots->owner!=descriptors->owner||q->quantum!=ops->native->quantum)
        return GC_NATIVE_METADATA_INVALID;
    uint64_t rb=0,db=0;
    if(gc_native_roots_size(q->cells,q->entries,q->quantum,&rb)!=GC_NATIVE_METADATA_OK||
        gc_native_builtin_registry_size(q->capacity,q->quantum,&db)!=GC_NATIVE_METADATA_OK||
        roots->bytes!=rb||descriptors->bytes!=db)return GC_NATIVE_METADATA_INVALID;
    RootStorageSpec spec={owner,q->cells,q->entries,roots->bytes};
    uint64_t result=ops->roots?ops->roots(roots,&spec):
        gc_native_roots_prepare(roots,owner,q->cells,q->entries,ops->native,ops->initialize_roots);
    if(result!=GC_NATIVE_METADATA_OK)return result;
    return ops->descriptors?ops->descriptors(descriptors,q->capacity):
        gc_native_builtin_registry_prepare(descriptors,q->capacity,ops->native,
            ops->initialize_descriptors,ops->register_descriptor);
}
/* Account both prepared packages under the heap lock before publication.
 * Only the request's charge pointers are consumed; preparation already checked
 * its sizing fields. This does not prove preparation or retire any mapping.
 * The unique owner must have successfully initialized both packages. All
 * checks precede changes; INVALID leaves both charges and the heap unchanged. */
typedef uint64_t (*NativeStartupCommitter)(HeapPool *,const NativeStartupRequest *);
static inline uint64_t gc_native_startup_commit_locked(HeapPool *heap,const NativeStartupRequest *q){
    if(!heap||((uintptr_t)heap&7)||!q||((uintptr_t)q&7))return GC_BUDGET_INVALID;
    MetadataCharge *roots=(void *)(uintptr_t)q->roots,*descriptors=(void *)(uintptr_t)q->descriptors;
    if(!roots||((uintptr_t)roots&7)||!descriptors||((uintptr_t)descriptors&7)||roots==descriptors||
        !heap->page_count||heap->page_count>UINT64_MAX/sizeof(AllocationPage)||
        heap->metadata_committed<heap->page_count*sizeof(AllocationPage)||heap->metadata_committed>heap->metadata_max||
        heap->metadata_pending>heap->metadata_max-heap->metadata_committed)return GC_BUDGET_INVALID;
    uint64_t remaining=heap->metadata_pending;
    MetadataCharge *charges[]={roots,descriptors};
    for(unsigned i=0;i<2;i++){
        const MetadataCharge *c=charges[i];
        if(!c->bytes||c->owner!=(uintptr_t)heap||c->stage!=1||!c->allocation||(c->allocation&7)||
            c->allocation>UINT64_MAX-c->bytes||c->bytes>remaining)return GC_BUDGET_INVALID;
        remaining-=c->bytes;
    }
    /* The charges own independent mappings. Check half-open ranges only after
     * both end additions were proved nonoverflowing; adjacency is valid. */
    if(roots->allocation<descriptors->allocation+descriptors->bytes&&
        descriptors->allocation<roots->allocation+roots->bytes)return GC_BUDGET_INVALID;
    heap->metadata_committed+=heap->metadata_pending-remaining;
    heap->metadata_pending=remaining;roots->stage=descriptors->stage=2;
    return GC_BUDGET_OK;
}
/* Return both private startup charges only after all owned mappings have been
 * successfully disposed outside the lock. Caller proves no published consumer
 * remains. Mixed reserved/committed charges are supported; each contributes to
 * its own budget. Validate the entire pair before returning either reservation.
 * INVALID retains both charges unchanged, including when one mapping remains.
 * Success consumes both obligations (stage 3); replay is invalid. This performs
 * no unmap, retirement, wait or lifecycle proof. Heap lock and exclusive charge
 * ownership are required, as for paired commit. */
typedef uint64_t (*NativeStartupReleaser)(HeapPool *,const NativeStartupRequest *);
static inline uint64_t gc_native_startup_release_locked(HeapPool *heap,const NativeStartupRequest *q){
    if(!heap||((uintptr_t)heap&7)||!q||((uintptr_t)q&7))return GC_BUDGET_INVALID;
    MetadataCharge *roots=(void *)(uintptr_t)q->roots,*descriptors=(void *)(uintptr_t)q->descriptors;
    if(!roots||((uintptr_t)roots&7)||!descriptors||((uintptr_t)descriptors&7)||roots==descriptors||
        !heap->page_count||heap->page_count>UINT64_MAX/sizeof(AllocationPage)||
        heap->metadata_committed<heap->page_count*sizeof(AllocationPage)||heap->metadata_committed>heap->metadata_max||
        heap->metadata_pending>heap->metadata_max-heap->metadata_committed)return GC_BUDGET_INVALID;
    uint64_t pending=heap->metadata_pending,committed=heap->metadata_committed-heap->page_count*sizeof(AllocationPage);
    MetadataCharge *charges[]={roots,descriptors};
    for(unsigned i=0;i<2;i++){
        const MetadataCharge *c=charges[i];
        if(!c->bytes||c->owner!=(uintptr_t)heap||c->allocation||(c->stage!=1&&c->stage!=2))return GC_BUDGET_INVALID;
        uint64_t *available=c->stage==1?&pending:&committed;
        if(c->bytes>*available)return GC_BUDGET_INVALID;
        *available-=c->bytes;
    }
    heap->metadata_pending=pending;heap->metadata_committed=committed+heap->page_count*sizeof(AllocationPage);
    roots->stage=descriptors->stage=3;return GC_BUDGET_OK;
}
#endif
