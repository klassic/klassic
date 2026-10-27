/* Runtime-owned Young run provisioning. Controls remain stable/disjoint until
 * the run, forwarding metadata AND logical mark records have retired. Mark
 * records can outlive this physical run after copying: closing a TLAB never
 * frees them. This C controller connects bootstrap ownership to rooted shared
 * allocation; it does not yet implement collector scheduling or root growth.
 *
 * *_locked calls require the heap mutex and exclusive context ownership.
 * prepare/cleanup run outside locks/guards in BlockedNative. Publication is
 * separate from native preparation and TLAB binding. No ordinary allocator
 * may take a READY run still owned by this preparation transaction.
 * cancel is only for NEVER bound runs. Installed runs must use full source
 * retirement and independent logical-record lifetime proofs, never cancel.
 */
#ifndef KLASSIC_GC_NATIVE_ALLOCATION_H
#define KLASSIC_GC_NATIVE_ALLOCATION_H
#include "gc_native_runtime.h"
#include "gc_native_bitmap.h"
#include "gc_native_page_release.h"
#include "gc_alloc_start.h"
#define GC_NATIVE_RUN_OS_ERROR 6u
/* Admission uses GC_POOL_* statuses; other operations return OK/INVALID or
 * OS_ERROR. Allocation itself preserves GC_ALLOC_* pressure results. */
typedef struct NativeAllocationRun {
    uint64_t self;
    _Atomic uint64_t stage; /* publishes private preparation before inventory readers */
    uint64_t previous,next;
    NativeRuntimeOwner *runtime;
    PageLease lease;
    NativeBitmapOwner bitmap;
    MetadataCharge forwarding,mark_records;
    MarkArena marks;
    PageRelease release;
    SourceRetire retirement;
    uint64_t imports,import_users,retirement_cursor,retirement_limit;
} NativeAllocationRun;
/* 1 reserved, 2 prepared, 3 READY, 4 bound, 5 cancelling,
 * 6 native cleanup complete, 7 released, 8 closed/collector-owned,
 * 9 source metadata retiring, 10 physical run retired/records retained,
 * 11 records sealed for disposal. import_users counts
 * installed copies in other/same runs under the heap lock. Original objects
 * remain implicitly owned until this run's complete physical retirement. */
static inline bool gc_native_allocation_valid(const NativeAllocationRun *run){
    return run&&!((uintptr_t)run&7)&&run->self==(uintptr_t)run&&
        gc_native_runtime_valid(run->runtime)&&run->runtime->stage==3;
}
static inline AllocationPage *gc_native_allocation_page(NativeAllocationRun *run){
    return (AllocationPage *)(uintptr_t)run->runtime->backing.heap.pages+run->lease.head_index;
}
static inline bool gc_native_allocation_actor_native(const NativeAllocationRun *run,const MutatorContext *actor){
    return gc_native_allocation_valid(run)&&actor&&!((uintptr_t)actor&7)&&
        actor->abi_version==GC_ABI_VERSION&&actor->thread_id&&
        atomic_load_explicit(&actor->state,memory_order_acquire)==GC_BLOCKED_NATIVE&&
        !atomic_load_explicit(&actor->access_epoch,memory_order_acquire);
}
static inline bool gc_native_allocation_native(const NativeAllocationRun *run){
    return gc_native_allocation_valid(run)&&gc_native_allocation_actor_native(run,&run->runtime->context);
}
/* Shared heap admission for a private mutator run or collector target. No
 * existing TLAB/guard belongs to this independent transaction; a collector
 * reserves disjoint pages without reading another actor's private context.
 * Callers separately own their actor and its native-boundary protocol. */
static inline uint64_t gc_native_allocation_reserve_kind_locked(NativeAllocationRun *run,
        NativeRuntimeOwner *runtime,uint64_t pages,uint64_t capacity,uint64_t generation,uint64_t relocation,const NativeMetadataOps *ops){
    if(!run||((uintptr_t)run&7)||!gc_native_runtime_valid(runtime)||runtime->stage!=3||
       !ops||!ops->map||!ops->unmap||!pages||!capacity||capacity>UINT32_MAX||generation>GC_OLD||relocation>1||
       !gc_native_heap_prepared(&runtime->backing))return GC_POOL_INVALID;
    const unsigned char *raw=(const void *)run;
    for(size_t i=0;i<sizeof(*run);i++)if(raw[i])return GC_POOL_INVALID;
    uint64_t forward_bytes=0,mark_bytes=0;
    if(gc_native_metadata_size(sizeof(ForwardTable)+capacity*(sizeof(ForwardCell)+sizeof(uint64_t)),ops->quantum,&forward_bytes)||
       gc_native_metadata_size(capacity*sizeof(MarkRecord),ops->quantum,&mark_bytes))return GC_POOL_INVALID;
    NativeAllocationRun *first=(void *)(uintptr_t)runtime->allocations;
    if(first&&(!gc_native_allocation_valid(first)||first->runtime!=runtime||first->previous))return GC_POOL_INVALID;
    HeapPool *heap=&runtime->backing.heap;
    run->lease=(PageLease){.page_count=pages,.generation=generation,.relocation=relocation};
    uint64_t status=gc_pool_step_locked(heap,&run->lease,GC_POOL_RESERVE);
    if(status){*run=(NativeAllocationRun){0};return status;}
    status=gc_native_bitmap_reserve_locked(&run->bitmap,heap,&run->lease,ops);
    if(!status){
        run->forwarding.bytes=forward_bytes;
        status=gc_budget_step_locked(heap,&run->forwarding,GC_BUDGET_RESERVE);
    }
    if(!status){
        run->mark_records.bytes=mark_bytes;
        status=gc_budget_step_locked(heap,&run->mark_records,GC_BUDGET_RESERVE);
    }
    if(status){
        /* No OS allocation or publication occurred. Return admitted charges
         * under this same lock; incarnation/identity counters stay monotonic. */
        MetadataCharge *charges[]={&run->mark_records,&run->forwarding,&run->bitmap.padding};
        for(unsigned i=0;i<3;i++)if(charges[i]->stage==1)
            if(gc_budget_step_locked(heap,charges[i],GC_BUDGET_ABORT)!=GC_BUDGET_OK)return GC_POOL_INVALID;
        if(gc_pool_step_locked(heap,&run->lease,GC_POOL_ABORT)!=GC_POOL_OK)return GC_POOL_INVALID;
        *run=(NativeAllocationRun){0};
        return status==GC_BUDGET_LIMIT?GC_POOL_METADATA_LIMIT:GC_POOL_INVALID;
    }
    run->self=(uintptr_t)run;run->stage=1;run->runtime=runtime;run->marks.capacity=capacity;
    run->next=runtime->allocations;if(first)first->previous=(uintptr_t)run;
    runtime->allocations=(uintptr_t)run;return GC_POOL_OK;
}
static inline uint64_t gc_native_allocation_reserve_generation_locked(NativeAllocationRun *run,
        NativeRuntimeOwner *runtime,uint64_t pages,uint64_t capacity,uint64_t generation,const NativeMetadataOps *ops){
    return gc_native_allocation_reserve_kind_locked(run,runtime,pages,capacity,generation,0,ops);
}
static inline uint64_t gc_native_allocation_reserve_target_locked(NativeAllocationRun *run,
        NativeRuntimeOwner *runtime,uint64_t pages,uint64_t capacity,uint64_t generation,const NativeMetadataOps *ops){
    return gc_native_allocation_reserve_kind_locked(run,runtime,pages,capacity,generation,1,ops);
}
static inline uint64_t gc_native_allocation_reserve_locked(NativeAllocationRun *run,
        NativeRuntimeOwner *runtime,uint64_t pages,uint64_t capacity,const NativeMetadataOps *ops){
    if(!gc_native_runtime_valid(runtime)||runtime->context.tlab_start||
       runtime->context.tlab_top||runtime->context.tlab_end||
       atomic_load_explicit(&runtime->context.access_epoch,memory_order_acquire))return GC_POOL_INVALID;
    uint64_t state=atomic_load_explicit(&runtime->context.state,memory_order_acquire);
    if(state!=GC_RUNNING&&state!=GC_BLOCKED_NATIVE)return GC_POOL_INVALID;
    return gc_native_allocation_reserve_generation_locked(run,runtime,pages,capacity,GC_YOUNG,ops);
}
/* OS preparation belongs to the actual calling native worker. A collector
 * worker need not borrow or change the mutator context to prepare Old targets.
 * The unique private run and its budget reservation remain owned throughout. */
typedef uint64_t (*NativeRunCommitter)(NativeHeapOwner *,PageLease *);
/* Each successful OS suboperation remains owned after a later failure. Retry
 * resumes only missing backing; it never zeroes/publishes an existing package. */
static inline uint64_t gc_native_allocation_prepare_native_with(NativeAllocationRun *run,const NativeMetadataOps *ops,
        const MutatorContext *actor,NativeRunCommitter commit){
    if(!gc_native_allocation_valid(run)||!actor||((uintptr_t)actor&7)||
       actor->abi_version!=GC_ABI_VERSION||!actor->thread_id||
       atomic_load_explicit(&actor->state,memory_order_acquire)!=GC_BLOCKED_NATIVE||
       atomic_load_explicit(&actor->access_epoch,memory_order_acquire)||run->stage!=1||
       (run->bitmap.stage!=1&&run->bitmap.stage!=2)||
       (run->bitmap.mapping&&run->bitmap.stage!=2)||
       run->forwarding.stage!=1||run->mark_records.stage!=1)return GC_POOL_INVALID;
    uint64_t status=0;
    if(!run->lease.committed_mapping)status=(commit?commit:gc_native_heap_commit_lease)(&run->runtime->backing,&run->lease);
    if(!status&&run->bitmap.stage==1)status=gc_native_bitmap_prepare(&run->bitmap,ops);
    if(!status&&!run->forwarding.allocation)status=gc_native_metadata_prepare(&run->forwarding,ops);
    if(!status&&!run->mark_records.allocation)status=gc_native_metadata_prepare(&run->mark_records,ops);
    if(status)return status==GC_NATIVE_METADATA_OS_ERROR?GC_NATIVE_RUN_OS_ERROR:GC_POOL_INVALID;
    ForwardTable *table=(void *)(uintptr_t)run->forwarding.allocation;
    table->cells=(uintptr_t)(table+1);table->capacity=run->marks.capacity;atomic_init(&table->used,0);
    run->imports=table->cells+table->capacity*sizeof(ForwardCell);
    run->marks.records=run->mark_records.allocation;
    run->stage=2;return GC_POOL_OK;
}
static inline uint64_t gc_native_allocation_prepare_native(NativeAllocationRun *run,const NativeMetadataOps *ops,const MutatorContext *actor){
    return gc_native_allocation_prepare_native_with(run,ops,actor,NULL);
}
static inline uint64_t gc_native_allocation_prepare(NativeAllocationRun *run,const NativeMetadataOps *ops){
    return gc_native_allocation_valid(run)?gc_native_allocation_prepare_native(run,ops,&run->runtime->context):GC_POOL_INVALID;
}
static inline uint64_t gc_native_allocation_publish_locked(NativeAllocationRun *run){
    if(!gc_native_allocation_valid(run)||run->stage!=2||
       !gc_native_pool_lease_valid(&run->runtime->backing.heap,&run->lease))return GC_POOL_INVALID;
    MetadataCharge *charges[]={&run->forwarding,&run->mark_records};
    for(unsigned i=0;i<2;i++){
        if(charges[i]->stage==1&&gc_budget_step_locked(&run->runtime->backing.heap,charges[i],GC_BUDGET_COMMIT)!=GC_BUDGET_OK)
            return GC_POOL_INVALID;
        if(charges[i]->stage!=2)return GC_POOL_INVALID;
    }
    /* Install immutable table while the page is still private PREPARING. */
    gc_native_allocation_page(run)->forward_table=run->forwarding.allocation;
    if(gc_native_bitmap_publish_locked(&run->bitmap)!=GC_POOL_OK)return GC_POOL_INVALID;
    run->stage=3;return GC_POOL_OK;
}
static inline uint64_t gc_native_allocation_bind_context_locked(NativeAllocationRun *run,MutatorContext *context){
    if(!gc_native_allocation_valid(run)||!context||run->stage!=3||
       gc_tlab_page_locked(context,gc_native_allocation_page(run),0)!=GC_ALLOC_OK)return GC_POOL_INVALID;
    run->stage=4;return GC_POOL_OK;
}
static inline uint64_t gc_native_allocation_bind_locked(NativeAllocationRun *run){
    return gc_native_allocation_valid(run)?gc_native_allocation_bind_context_locked(run,&run->runtime->context):GC_POOL_INVALID;
}
typedef uint64_t (*NativeAllocationStarter)(MutatorContext *,const AllocationStart *);
/* Fresh output controls; never overwrite a live output handle. All other live
 * inputs must stay rooted while the caller services returned pressure. Failure
 * may leave a cancelled empty handle, which the caller discards before retry.
 * This finite entry does not poll, refill, initialize or publish the object. */
static inline uint64_t gc_native_allocation_start(NativeAllocationRun *run,uint64_t descriptor_id,
        uint64_t payload_bytes,RootRequest *root,AllocationResult *output,NativeAllocationStarter allocate){
    if(!gc_native_allocation_valid(run)||run->stage!=4||!allocate||!root||((uintptr_t)root&7)||
       !output||((uintptr_t)output&7)||root->segment||root->handle.cell||root->handle.scope||
       root->handle.reuse_generation||root->value)return GC_ALLOC_INVALID;
    NativeRuntimeOwner *runtime=run->runtime;
    if(!runtime->context.root_segments)return GC_ALLOC_ROOT_FULL;
    RootRegistration *registration=(void *)(uintptr_t)runtime->context.root_segments;
    NativeBuiltinRegistry *descriptors=(void *)(uintptr_t)runtime->descriptor_charge.allocation;
    root->segment=registration->segment;
    AllocationRequest request={.page=(uintptr_t)gc_native_allocation_page(run),
        .marks=(uintptr_t)&run->marks,.output=(uintptr_t)output};
    AllocationShape shape={descriptor_id,payload_bytes,(uintptr_t)&request};
    AllocationStart start={(uintptr_t)&descriptors->registry,(uintptr_t)&shape,(uintptr_t)root};
    return allocate(&runtime->context,&start);
}
static inline uint64_t gc_native_allocation_close_context_locked(NativeAllocationRun *run,MutatorContext *context){
    if(!gc_native_allocation_valid(run)||!context||run->stage!=4||
       gc_tlab_page_locked(context,gc_native_allocation_page(run),1)!=GC_ALLOC_OK)return GC_POOL_INVALID;
    run->stage=8;return GC_POOL_OK;
}
static inline uint64_t gc_native_allocation_close_locked(NativeAllocationRun *run){
    return gc_native_allocation_valid(run)?gc_native_allocation_close_context_locked(run,&run->runtime->context):GC_POOL_INVALID;
}
static inline uint64_t gc_native_allocation_cancel_locked(NativeAllocationRun *run){
    if(!gc_native_allocation_valid(run)||run->stage<1||run->stage>3)return GC_POOL_INVALID;
    if(run->stage==3){
        run->release=(PageRelease){.head_index=run->lease.head_index,.page_count=run->lease.page_count,
            .incarnation=run->lease.incarnation};
        if(gc_page_release_locked(&run->runtime->backing.heap,&run->release,GC_RELEASE_BEGIN)!=GC_RELEASE_OK)return GC_POOL_INVALID;
    }else if(!gc_native_pool_lease_valid(&run->runtime->backing.heap,&run->lease))return GC_POOL_INVALID;
    run->stage=5;return GC_POOL_OK;
}
static inline uint64_t gc_native_allocation_cleanup_native_with(NativeAllocationRun *run,const NativeMetadataOps *ops,
        const MutatorContext *actor,NativePageDecommitter decommit){
    if(!gc_native_allocation_actor_native(run,actor)||run->stage!=5)return GC_POOL_INVALID;
    uint64_t status=0;
    if(run->release.stage){
        if(run->release.committed_mapping)status=gc_native_page_release_payload(&run->runtime->backing,&run->release,decommit);
    }else if(run->lease.committed_mapping)status=gc_native_heap_decommit_lease_with(&run->runtime->backing,&run->lease,decommit);
    else if(decommit){
        /* A backing policy may already own cached pages activated before a
         * failed payload commit. The exact PREPARING lease still owns this
         * range even though its active-run commit never completed. Give the
         * optional policy a chance to restore/discard that physical prefix. */
        NativeHeapOwner *owner=&run->runtime->backing;
        if(!gc_native_heap_lease_valid(owner,&run->lease))return GC_POOL_INVALID;
        void *address=(void *)(uintptr_t)(owner->reservation+((run->lease.head_index+1)<<GC_HEAP_PAGE_SHIFT));
        if(!decommit(address,(size_t)(run->lease.page_count<<GC_HEAP_PAGE_SHIFT)))status=GC_NATIVE_METADATA_OS_ERROR;
    }
    if(!status&&run->bitmap.stage<4)
        status=gc_native_bitmap_dispose(&run->bitmap,run->release.stage?&run->release:NULL,ops);
    if(!status){
        /* No published consumer exists: either BEGIN detached this table or
         * the run never left PREPARING. Keep its receipt on failed unmap. */
        if(!run->release.stage)gc_native_allocation_page(run)->forward_table=0;
        if(run->forwarding.allocation)status=gc_native_metadata_dispose(&run->forwarding,ops);
        if(!status)run->release.forward_table=0;
    }
    if(!status&&run->mark_records.allocation)status=gc_native_metadata_dispose(&run->mark_records,ops);
    if(status)return status==GC_NATIVE_METADATA_OS_ERROR?GC_NATIVE_RUN_OS_ERROR:GC_POOL_INVALID;
    run->marks.records=run->imports=0;run->stage=6;return GC_POOL_OK;
}
static inline uint64_t gc_native_allocation_cleanup_native(NativeAllocationRun *run,const NativeMetadataOps *ops,const MutatorContext *actor){
    return gc_native_allocation_cleanup_native_with(run,ops,actor,NULL);
}
static inline uint64_t gc_native_allocation_cleanup(NativeAllocationRun *run,const NativeMetadataOps *ops){
    return gc_native_allocation_valid(run)?gc_native_allocation_cleanup_native(run,ops,&run->runtime->context):GC_POOL_INVALID;
}
static inline bool gc_native_allocation_links_valid(NativeAllocationRun *run){
    NativeAllocationRun *prev=(void *)(uintptr_t)run->previous,*next=(void *)(uintptr_t)run->next;
    return !(prev&&(!gc_native_allocation_valid(prev)||prev->runtime!=run->runtime||prev->next!=(uintptr_t)run))&&
        !(!prev&&run->runtime->allocations!=(uintptr_t)run)&&
        !(next&&(!gc_native_allocation_valid(next)||next->runtime!=run->runtime||next->previous!=(uintptr_t)run));
}
static inline void gc_native_allocation_unlink(NativeAllocationRun *run){
    NativeAllocationRun *prev=(void *)(uintptr_t)run->previous,*next=(void *)(uintptr_t)run->next;
    if(prev)prev->next=run->next;else run->runtime->allocations=run->next;
    if(next)next->previous=run->previous;
    run->previous=run->next=0;run->stage=7;
}
static inline uint64_t gc_native_allocation_finish_locked(NativeAllocationRun *run){
    if(!gc_native_allocation_valid(run)||run->stage!=6||!gc_native_allocation_links_valid(run))return GC_POOL_INVALID;
    HeapPool *heap=&run->runtime->backing.heap;
    MetadataCharge *charges[]={&run->forwarding,&run->mark_records};
    for(unsigned i=0;i<2;i++)if(charges[i]->stage==1||charges[i]->stage==2)
        if(gc_budget_step_locked(heap,charges[i],charges[i]->stage==1?GC_BUDGET_ABORT:GC_BUDGET_RELEASE)!=GC_BUDGET_OK)return GC_POOL_INVALID;
    if(run->bitmap.stage!=5&&gc_native_bitmap_release_locked(&run->bitmap)!=GC_BUDGET_OK)return GC_POOL_INVALID;
    if(run->release.stage){
        if(gc_page_release_locked(heap,&run->release,GC_RELEASE_FINISH)!=GC_RELEASE_OK)return GC_POOL_INVALID;
    }else if(gc_pool_step_locked(heap,&run->lease,GC_POOL_ABORT)!=GC_POOL_OK)return GC_POOL_INVALID;
    gc_native_allocation_unlink(run);return GC_POOL_OK;
}
#endif
