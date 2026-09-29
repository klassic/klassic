/* Connect owned allocation runs to SourceRetire AFTER the scheduler proves
 * whole-range source/reference/color retirement and completes payload uncommit.
 * The controller checks ownership receipts, not that global proof. Hold the
 * heap/registry/both-generation locks for BEGIN, heap lock for STEP; run
 * cleanup outside locks in native state. The
 * embedded request is unique and stable, so copies cannot claim its page.
 *
 * Physical retirement deliberately retains the logical mark-record mapping,
 * charge and runtime inventory node. Destination copies and queued tasks can
 * still use those records after the source address has been reused. Imports
 * are returned only after the complete whole-range proof, in bounded steps.
 */
#ifndef KLASSIC_GC_NATIVE_ALLOCATION_RETIRE_H
#define KLASSIC_GC_NATIVE_ALLOCATION_RETIRE_H
#include "gc_native_allocation.h"
#include "gc_native_scan_incoming.h"
#include "gc_native_queue_incoming.h"
#include "gc_native_root_incoming.h"
#include "gc_native_buffer_incoming.h"
/* Bound receipt for native scan controls, both mark queues, root storage
 * and registered buffer owners.
 * The caller still proves
 * every other retirement domain and prevents new target references throughout
 * the audit and BEGIN. Registry+heap+both-generation serialization binds consumption to the
 * same phase, allocation incarnation and scan inventory. Each NEXT inspects at
 * most one control with the exclusive worker handoff required by that audit.
 * This receipt is necessary, never a substitute for the whole-cycle proof. */
typedef struct {
    uint64_t self,incarnation,phase,consumed;
    NativeAllocationRun *run;
    NativeScanIncoming scans;
    NativeQueueIncoming queues;
    NativeRootIncoming roots;
    NativeBufferIncoming buffers;
} NativeRunRetireCheck;
static inline uint64_t gc_native_allocation_retire_check_begin_locked(NativeAllocationRun *run,NativeRunRetireCheck *check){
    if(!check||((uintptr_t)check&7)||check->self||!gc_native_allocation_valid(run)||run->stage!=8)return GC_NSI_INVALID;
    AllocationPage *page=gc_native_allocation_page(run);
    if(page->incarnation!=run->lease.incarnation||page->owner)return GC_NSI_INVALID;
    uint64_t status=gc_native_scan_incoming_begin_locked(run->runtime,page->start,page->limit,&check->scans);
    if(status==GC_NSI_MORE){
        status=gc_native_queue_incoming_begin_locked(run->runtime,page->start,page->limit,&check->queues);
        if(status!=GC_NSI_MORE)check->scans=(NativeScanIncoming){0};
    }
    if(status==GC_NSI_MORE){
        check->self=(uintptr_t)check;check->run=run;check->incarnation=run->lease.incarnation;
        check->phase=run->runtime->roots.phase;check->consumed=0;
    }
    return status;
}
static inline uint64_t gc_native_allocation_retire_check_next_locked(NativeRunRetireCheck *check){
    if(!check||((uintptr_t)check&7)||check->self!=(uintptr_t)check||check->consumed||
       !gc_native_allocation_valid(check->run)||check->run->stage!=8)return GC_NSI_INVALID;
    NativeAllocationRun *run=check->run;AllocationPage *page=gc_native_allocation_page(run);
    if(check->incarnation!=run->lease.incarnation||check->incarnation!=page->incarnation||
       check->phase!=run->runtime->roots.phase)return GC_NSI_RESTART;
    if(check->scans.runtime!=run->runtime||check->scans.target_start!=page->start||check->scans.target_limit!=page->limit)
        return GC_NSI_INVALID;
    if(check->queues.runtime!=run->runtime||check->queues.target_start!=page->start||check->queues.target_limit!=page->limit)
        return GC_NSI_INVALID;
    uint64_t status=gc_native_scan_incoming_next_locked(&check->scans);
    if(status!=GC_NSI_DONE)return status;
    status=gc_native_queue_incoming_next_locked(&check->queues);
    if(status!=GC_NSI_DONE)return status;
    /* Start lazily: a queue/scan finding must not unnecessarily pin roots. */
    if(!check->roots.self)return gc_native_root_incoming_begin_locked(run->runtime,page->start,page->limit,&check->roots);
    if(check->roots.runtime!=run->runtime||check->roots.target_start!=page->start||check->roots.target_limit!=page->limit)
        return GC_NSI_INVALID;
    status=gc_native_root_incoming_next_locked(&check->roots);
    if(status!=GC_NSI_DONE)return status;
    if(!check->buffers.self)return gc_native_buffer_incoming_begin_locked(run->runtime,page->start,page->limit,&check->buffers);
    if(check->buffers.runtime!=run->runtime||check->buffers.target_start!=page->start||check->buffers.target_limit!=page->limit)
        return GC_NSI_INVALID;
    return gc_native_buffer_incoming_next_locked(&check->buffers);
}
/* Mandatory before dropping/resetting an incomplete or stale receipt, even
 * after a phase change. Queue/root cursors may own metadata/reader leases. */
static inline uint64_t gc_native_allocation_retire_check_cancel_locked(NativeRunRetireCheck *check){
    if(!check||((uintptr_t)check&7)||check->self!=(uintptr_t)check||check->consumed)return GC_NSI_INVALID;
    if(check->roots.self){
        uint64_t released=gc_native_root_incoming_cancel_locked(&check->roots);
        if(released!=GC_NSI_DONE)return released;
    }
    uint64_t status=gc_native_queue_incoming_cancel_locked(&check->queues);
    if(status==GC_NSI_DONE)check->consumed=1;
    return status;
}
/* Alternate seal for a wholly dead CLOSED run. The caller must already prove
 * absence of every strong/weak/root/job/native reference and every incoming
 * heap reference from outside this dead range, and exclude new actors. Edges
 * entirely inside the wholly dead range retire with it. This does NOT derive death from mark
 * bits, an empty queue or zero leases; it connects that stronger proof to OS
 * uncommit. No payload traversal or state fabrication by the caller is needed.
 * Live moved runs use gc_source_drain instead. Hold the heap lock. */
static inline uint64_t gc_native_allocation_dead_seal_locked(NativeAllocationRun *run){
    if(!gc_native_allocation_valid(run)||run->stage!=8)return GC_SR_INVALID;
    AllocationPage *page=gc_native_allocation_page(run);
    if(page->incarnation!=run->lease.incarnation||page->owner||
       page->starts!=run->bitmap.mapping||page->forward_table!=run->forwarding.allocation||
       atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_CLOSED)return GC_SR_INVALID;
    if(page->metadata_leases)return GC_SR_WAIT;
    atomic_store_explicit(&page->state,GC_PAGE_EVACUATED,memory_order_release);
    return GC_SR_DONE;
}
static inline uint64_t gc_native_allocation_retire_begin_locked(NativeAllocationRun *run,NativeRunRetireCheck *check){
    if(!gc_native_allocation_valid(run)||run->stage!=8||run->bitmap.stage!=3||
       run->forwarding.stage!=2||!run->forwarding.allocation||run->retirement.stage)return GC_SR_INVALID;
    AllocationPage *page=gc_native_allocation_page(run);
    if(page->incarnation!=run->lease.incarnation||page->starts!=run->bitmap.mapping||
       page->forward_table!=run->forwarding.allocation||
       atomic_load_explicit(&page->state,memory_order_acquire)!=GC_PAGE_UNCOMMITTED)return GC_SR_INVALID;
    if(!check)return GC_SR_WAIT;
    if(((uintptr_t)check&7)||check->self!=(uintptr_t)check||check->run!=run||check->consumed)return GC_SR_INVALID;
    if(check->scans.stage!=2||check->scans.cursor||check->scans.remaining||check->scans.observed)return GC_SR_WAIT;
    if(check->queues.stage!=3||check->queues.pin.token||check->queues.observed)return GC_SR_WAIT;
    if(check->roots.stage!=2||check->roots.lease.kind||check->roots.observed)return GC_SR_WAIT;
    if(check->buffers.stage!=2||check->buffers.cursor||check->buffers.remaining)return GC_SR_WAIT;
    uint64_t checked=gc_native_allocation_retire_check_next_locked(check);
    if(checked!=GC_NSI_DONE)return checked==GC_NSI_INVALID?GC_SR_INVALID:GC_SR_WAIT;
    const ForwardTable *table=(void *)(uintptr_t)run->forwarding.allocation;
    uint64_t used=atomic_load_explicit(&table->used,memory_order_acquire);
    if(!run->imports||table->capacity!=run->marks.capacity||used>table->capacity)return GC_SR_INVALID;
    run->retirement=(SourceRetire){.head_index=run->lease.head_index,.page_count=run->lease.page_count,
        .incarnation=run->lease.incarnation};
    uint64_t status=gc_source_retire_locked(&run->runtime->backing.heap,&run->retirement,GC_SR_BEGIN);
    if(status==GC_SR_MORE){run->stage=9;run->retirement_limit=used;check->consumed=1;}
    return status;
}
static inline bool gc_native_allocation_retirement_owned(NativeAllocationRun *run){
    if(!gc_native_allocation_valid(run)||run->stage!=9)return false;
    SourceRetire *q=&run->retirement;
    if(q->stage!=1||q->owner!=(uintptr_t)&run->runtime->backing.heap||
       q->head_index!=run->lease.head_index||q->page_count!=run->lease.page_count||
       q->incarnation!=run->lease.incarnation||!q->cursor||q->cursor>q->page_count)return false;
    const AllocationPage *page=gc_native_allocation_page(run);
    return page->incarnation==q->incarnation&&page->owner==(uintptr_t)q&&
        atomic_load_explicit(&page->state,memory_order_acquire)==GC_PAGE_RELEASING;
}
/* Return at most one imported arena reference. The lease/global retirement
 * proof excludes every private or queued user of this physical copy. An arena
 * remains alive while its count is nonzero; clear the unique slot before this
 * count can permit sealing. No payload access or native allocation here. */
static inline uint64_t gc_native_allocation_retire_import_locked(NativeAllocationRun *run){
    if(!gc_native_allocation_retirement_owned(run)||!run->imports||
       run->retirement_cursor>run->retirement_limit)return GC_SR_INVALID;
    if(run->retirement_cursor==run->retirement_limit)return GC_SR_DONE;
    uint64_t *slot=(uint64_t *)(uintptr_t)run->imports+run->retirement_cursor;
    NativeAllocationRun *owner=(void *)(uintptr_t)*slot;
    if(owner){
        if(!gc_native_allocation_valid(owner)||owner->runtime!=run->runtime||!owner->import_users||
           owner->mark_records.stage!=2||!owner->mark_records.allocation||owner->stage>10)return GC_SR_INVALID;
        *slot=0;owner->import_users--;
    }
    run->retirement_cursor++;
    return run->retirement_cursor==run->retirement_limit?GC_SR_DONE:GC_SR_MORE;
}
static inline uint64_t gc_native_allocation_retire_cleanup(NativeAllocationRun *run,const NativeMetadataOps *ops){
    if(!gc_native_allocation_native(run)||!gc_native_allocation_retirement_owned(run)||
       (run->bitmap.stage!=3&&run->bitmap.stage!=4&&run->bitmap.stage!=5)||
       (run->forwarding.stage!=2&&run->forwarding.stage!=3)||
       run->retirement.forward_table!=run->forwarding.allocation||
       run->retirement_cursor!=run->retirement_limit)return GC_POOL_INVALID;
    uint64_t status=0;
    if(run->bitmap.stage==3)status=gc_native_bitmap_retire(&run->bitmap,&run->retirement,ops);
    if(!status&&run->forwarding.allocation){
        status=gc_native_metadata_dispose(&run->forwarding,ops);
        if(!status)run->retirement.forward_table=run->imports=0;
    }
    return status==GC_NATIVE_METADATA_OS_ERROR?GC_NATIVE_RUN_OS_ERROR:status?GC_POOL_INVALID:GC_POOL_OK;
}
/* At most one tail plus the last head. Failed native cleanup leaves WAIT and
 * all credits held; repeated STEP never dereferences already-reused tails. */
static inline uint64_t gc_native_allocation_retire_step_locked(NativeAllocationRun *run){
    if(!gc_native_allocation_retirement_owned(run))return GC_SR_INVALID;
    if(run->retirement.starts||run->retirement.forward_table)return GC_SR_WAIT;
    if(run->bitmap.stage!=4&&run->bitmap.stage!=5)return GC_SR_INVALID;
    if(run->forwarding.allocation)return GC_SR_INVALID;
    HeapPool *heap=&run->runtime->backing.heap;
    if(run->forwarding.stage==2&&gc_budget_step_locked(heap,&run->forwarding,GC_BUDGET_RELEASE)!=GC_BUDGET_OK)return GC_SR_INVALID;
    if(run->forwarding.stage!=3)return GC_SR_INVALID;
    if(run->bitmap.stage!=5&&gc_native_bitmap_release_locked(&run->bitmap)!=GC_BUDGET_OK)return GC_SR_INVALID;
    uint64_t status=gc_source_retire_locked(heap,&run->retirement,GC_SR_STEP);
    if(status==GC_SR_DONE)run->stage=10;
    return status;
}
#endif
