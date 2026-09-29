/* Private initial work-storage package. Reserve the rounded charge before
 * mapping outside locks in native state; COMMIT before binding/publication.
 * Free-list preparation is outside pause/lock-critical traversal. The package
 * and its controls never move while jobs, leases or buffers can refer to it.
 * Epoch 1 initializes valid empty queues; starting a cycle remains separate.
 */
#ifndef KLASSIC_GC_NATIVE_WORK_H
#define KLASSIC_GC_NATIVE_WORK_H
#include "gc_native_metadata.h"
#include "gc_mark.h"
#include "gc_metadata.h"
typedef struct { uint64_t buffers,leases,tasks; } NativeWorkConfig;
typedef struct {
    MetadataLeasePool leases;
    MarkQueue queues[2];
    uint64_t satb,remembered,buffer_capacity;
} NativeWorkStorage;
static inline uint64_t gc_native_work_size(const NativeWorkConfig *config,uint64_t quantum,uint64_t *bytes){
    if(!config||!config->buffers||config->buffers>UINT32_MAX||!config->leases||config->leases>UINT32_MAX||
       !config->tasks||config->tasks>UINT32_MAX)return GC_NATIVE_METADATA_INVALID;
    /* Each bounded count is at most UINT32_MAX, so this fixed sum fits u64. */
    return gc_native_metadata_size(sizeof(NativeWorkStorage)+2*config->buffers*sizeof(BarrierEntry)+
        config->leases*sizeof(MetadataLeaseSlot)+2*config->tasks*sizeof(MarkTask),quantum,bytes);
}
static inline uint64_t gc_native_work_prepare(MetadataCharge *charge,HeapPool *heap,
        const NativeWorkConfig *config,const NativeMetadataOps *ops){
    uint64_t bytes=0;
    if(!charge||((uintptr_t)charge&7)||!heap||((uintptr_t)heap&7)||!ops||
       gc_native_work_size(config,ops->quantum,&bytes)||charge->bytes!=bytes||charge->owner!=(uintptr_t)heap)
        return GC_NATIVE_METADATA_INVALID;
    uint64_t status=gc_native_metadata_prepare(charge,ops);if(status)return status;
    NativeWorkStorage *storage=(void *)(uintptr_t)charge->allocation;
    uint64_t cursor=(uintptr_t)(storage+1);
    storage->satb=cursor;cursor+=config->buffers*sizeof(BarrierEntry);
    storage->remembered=cursor;cursor+=config->buffers*sizeof(BarrierEntry);
    storage->buffer_capacity=config->buffers;
    storage->leases=(MetadataLeasePool){(uintptr_t)heap,cursor,config->leases,1,0};
    MetadataLeaseSlot *slots=(void *)(uintptr_t)cursor;cursor+=config->leases*sizeof(MetadataLeaseSlot);
    for(uint64_t i=0;i<config->leases;i++)slots[i].next_free=i+1==config->leases?0:i+2;
    for(unsigned generation=0;generation<2;generation++){
        storage->queues[generation]=(MarkQueue){.tasks=cursor,.capacity=config->tasks,.free_head=1,.epoch=1,.generation=generation};
        MarkTask *tasks=(void *)(uintptr_t)cursor;cursor+=config->tasks*sizeof(MarkTask);
        for(uint64_t i=0;i<config->tasks;i++)tasks[i].next=i+1==config->tasks?0:i+2;
    }
    return GC_NATIVE_METADATA_OK;
}
/* Initial private context only. This never replaces live transport backing. */
static inline uint64_t gc_native_work_bind(MutatorContext *ctx,const MetadataCharge *charge){
    if(!ctx||((uintptr_t)ctx&7)||!charge||charge->stage!=2||!charge->allocation||
       ctx->abi_version!=GC_ABI_VERSION||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_BLOCKED_NATIVE)
        return GC_NATIVE_METADATA_INVALID;
    BufferCursor *buffers[]={&ctx->satb,&ctx->remembered};
    BufferConsumer *consumers[]={&ctx->satb_consumer,&ctx->remembered_consumer};
    for(unsigned i=0;i<2;i++)if(buffers[i]->entries||buffers[i]->capacity||buffers[i]->top||
        atomic_load_explicit(&buffers[i]->published,memory_order_acquire)||
        atomic_load_explicit(&buffers[i]->consumed,memory_order_acquire)||
        consumers[i]->last_serial||consumers[i]->serial||consumers[i]->end||consumers[i]->cursor)return GC_NATIVE_METADATA_INVALID;
    const NativeWorkStorage *storage=(void *)(uintptr_t)charge->allocation;
    ctx->satb.entries=storage->satb;ctx->remembered.entries=storage->remembered;
    ctx->satb.capacity=ctx->remembered.capacity=storage->buffer_capacity;
    return GC_NATIVE_METADATA_OK;
}
/* External quiescence and no new admission are prerequisites. This constant
 * check is not a collection termination predicate or a scan of private jobs. */
static inline bool gc_native_work_quiescent(NativeWorkStorage *storage){
    return storage&&!storage->leases.active&&
        gc_mark_step_locked(&storage->queues[0],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY&&
        gc_mark_step_locked(&storage->queues[1],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY;
}
#endif
