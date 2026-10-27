/* Logical record lifetime for runtime-owned allocation runs. All copy installs
 * into these runs MUST pass through this wrapper. Every imported physical
 * object owns one heap-lock-protected reference to its record arena, stored in
 * a parallel table inside the charged forwarding mapping. Original allocations
 * retain their arena implicitly until complete physical retirement.
 *
 * Record borrowers (including pending/queued/claimed/finished mark jobs) retain
 * a metadata lease on a physical copy until their last record use. Whole-range
 * retirement excludes all such borrowers before returning imports. Thus an
 * arena is disposable only after its own run retires and its final installed
 * copy retires. Queue emptiness and original-page reclamation are insufficient.
 * Arbitrary unregistered native record pointers violate this ownership ABI.
 */
#ifndef KLASSIC_GC_NATIVE_RECORDS_H
#define KLASSIC_GC_NATIVE_RECORDS_H
#include "gc_native_allocation.h"
#include "gc_relocate.h"
/* Own the private completed copy and source protection through this call.
 * Same heap lock as target installation, import release and record sealing.
 * The originating arena's TLAB must be closed before copying, so marks.used
 * is immutable and published by the close operation's heap-lock handoff.
 * Failure rolls back the private import; forwarding publication follows later. */
static inline uint64_t gc_native_allocation_install_copy_locked(NativeAllocationRun *target,
        NativeAllocationRun *owner,RelocationCopy *copy){
    if(!gc_native_allocation_valid(target)||!gc_native_allocation_valid(owner)||
       target->runtime!=owner->runtime||(target->stage!=3&&target->stage!=8)||
       (owner->stage!=8&&owner->stage!=9&&owner->stage!=10)||
       owner->mark_records.stage!=2||!owner->marks.records||!owner->marks.used||
       owner->marks.used>owner->marks.capacity||owner->marks.records!=owner->mark_records.allocation||
       owner->import_users==UINT64_MAX||!copy||((uintptr_t)copy&7)||copy->stage!=2||
       target->forwarding.stage!=2||!target->imports)return GC_TARGET_INVALID;
    AllocationPage *page=gc_native_allocation_page(target);
    uint64_t top=atomic_load_explicit(&page->allocated_top,memory_order_acquire);
    if(copy->heap_base!=page->heap_base||copy->destination_limit!=page->limit||
       top<page->start||top>page->limit||page->limit-top<48||
       copy->destination!=top+sizeof(ObjectHeader))return GC_TARGET_INVALID;
    ForwardTable *table=(void *)(uintptr_t)target->forwarding.allocation;
    uint64_t index=atomic_load_explicit(&table->used,memory_order_acquire);
    if(table->capacity!=target->marks.capacity||index>=table->capacity)return GC_TARGET_METADATA_REFILL;
    uint64_t *imports=(void *)(uintptr_t)target->imports;
    if(imports[index])return GC_TARGET_INVALID;
    const ObjectHeader *header=(void *)(uintptr_t)(page->heap_base+top);
    if(header->mark_record<owner->marks.records||
       header->mark_record-owner->marks.records>=owner->marks.used*sizeof(MarkRecord)||
       (header->mark_record-owner->marks.records)%sizeof(MarkRecord))return GC_TARGET_INVALID;
    imports[index]=(uintptr_t)owner;owner->import_users++;
    uint64_t status=gc_relocate_target_locked(page,copy,GC_TARGET_INSTALL);
    if(status!=GC_TARGET_OK){imports[index]=0;owner->import_users--;}
    else target->stage=8;
    return status;
}
/* Seal the entire logical arena against new imports. Complete physical
 * retirement and zero imported copies imply no protected record borrowers,
 * provided every actor obeyed the lease/installation ownership ABI above. */
static inline uint64_t gc_native_records_begin_locked(NativeAllocationRun *run){
    if(!gc_native_allocation_valid(run)||run->stage!=10||run->retirement.stage!=2||
       run->mark_records.stage!=2||!run->mark_records.allocation||
       run->marks.records!=run->mark_records.allocation)return GC_SR_INVALID;
    if(run->import_users)return GC_SR_WAIT;
    run->stage=11;return GC_SR_DONE;
}
static inline uint64_t gc_native_records_dispose_native(NativeAllocationRun *run,const NativeMetadataOps *ops,const MutatorContext *actor){
    if(!gc_native_allocation_actor_native(run,actor)||run->stage!=11||run->import_users)return GC_POOL_INVALID;
    uint64_t status=gc_native_metadata_dispose(&run->mark_records,ops);
    if(status)return status==GC_NATIVE_METADATA_OS_ERROR?GC_NATIVE_RUN_OS_ERROR:GC_POOL_INVALID;
    /* Keep the lock-published sealed stage unchanged during OS cleanup. Copy
     * installers short-circuit on that stage before reading these pointers;
     * the unique disposer alone touches them until locked FINISH. */
    run->marks.records=0;return GC_POOL_OK;
}
static inline uint64_t gc_native_records_dispose(NativeAllocationRun *run,const NativeMetadataOps *ops){
    return gc_native_allocation_valid(run)?gc_native_records_dispose_native(run,ops,&run->runtime->context):GC_POOL_INVALID;
}
static inline uint64_t gc_native_records_finish_locked(NativeAllocationRun *run){
    if(!gc_native_allocation_valid(run)||run->stage!=11||run->import_users||run->marks.records||run->mark_records.allocation||
       !gc_native_allocation_links_valid(run))return GC_POOL_INVALID;
    if(gc_budget_step_locked(&run->runtime->backing.heap,&run->mark_records,GC_BUDGET_RELEASE)!=GC_BUDGET_OK)return GC_POOL_INVALID;
    gc_native_allocation_unlink(run);return GC_POOL_OK;
}
#endif
