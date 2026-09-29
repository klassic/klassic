#ifndef KLASSIC_GC_NATIVE_ALLOCATION_CASES_H
#define KLASSIC_GC_NATIVE_ALLOCATION_CASES_H
#include "gc_native_allocation.h"
#include "gc_native_allocation_retire.h"
#include "gc_native_records.h"
#include "gc_native_source_uncommit.h"
#include "gc_initialization.h"
#include "gc_buffer_job.h"
#include "gc_root_journal.h"
#include "gc_metadata.h"
#include "gc_forward_table.h"
#include "gc_source_access.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static unsigned gc_run_maps,gc_run_unmaps,gc_run_fail_map,gc_run_fail_unmap;
static void *gc_run_map(size_t bytes){
    if(++gc_run_maps==gc_run_fail_map)return NULL;
    return gc_native_metadata_map(bytes);
}
static bool gc_run_unmap(void *p,size_t bytes){
    if(++gc_run_unmaps==gc_run_fail_unmap)return false;
    return gc_native_metadata_unmap(p,bytes);
}
static void gc_allocation_fixture_start(NativeRuntimeOwner *runtime,NativeHeapOps *ops,uint64_t cells,uint64_t journal){
    NativeRuntimeConfig config={{4*GC_HEAP_PAGE_BYTES,2*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES,64*ops->metadata.quantum},cells,journal,64,{8,8,8}};
    assert(gc_native_runtime_prepare(runtime,&config,ops)==GC_NATIVE_METADATA_OK);
    assert(gc_native_runtime_activate(runtime)==GC_NATIVE_METADATA_OK);
}
static void gc_allocation_fixture_native(NativeRuntimeOwner *runtime,bool enter){
    assert(gc_worker_step_locked(&runtime->roots,&runtime->worker,enter?GC_WK_ENTER_NATIVE:GC_WK_LEAVE_NATIVE)==GC_WK_OK);
}
/* Fixtures have exclusive control ownership and supply the remaining range
 * retirement proof. This helper executes all mandatory incoming audits. */
static void gc_allocation_fixture_retire_reject(NativeAllocationRun *run,NativeRunRetireCheck *check,uint64_t expected){
    NativeAllocationRun before;AllocationPage page;HeapPool heap;
    memcpy(&before,run,sizeof(before));memcpy(&page,gc_native_allocation_page(run),sizeof(page));
    memcpy(&heap,&run->runtime->backing.heap,sizeof(heap));
    assert(gc_native_allocation_retire_begin_locked(run,check)==expected);
    assert(!memcmp(&before,run,sizeof(before))&&!memcmp(&page,gc_native_allocation_page(run),sizeof(page))&&
        !memcmp(&heap,&run->runtime->backing.heap,sizeof(heap)));
}
static uint64_t gc_allocation_fixture_retire_begin(NativeAllocationRun *run){
    NativeRunRetireCheck check={0};
    NativeRuntimeOwner *r=run->runtime;
    assert(atomic_load(&r->context.state)==GC_BLOCKED_NATIVE);
    /* Real token-free barrier work makes the mandatory buffer gate observable. */
    gc_allocation_fixture_native(r,false);
    uint64_t selector=run->lease.incarnation&1;BarrierEntry empty={0},out={0};
    assert(gc_barrier_buffer_step(&r->context,&empty,selector)==GC_BB_OK);
    assert(gc_barrier_buffer_step(&r->context,NULL,selector+2)==GC_BB_OK);
    gc_allocation_fixture_native(r,true);
    gc_allocation_fixture_retire_reject(run,NULL,GC_SR_WAIT);
    assert(gc_native_allocation_retire_check_begin_locked(run,&check)==GC_NSI_MORE);
    gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);
    uint64_t status;do{status=gc_native_allocation_retire_check_next_locked(&check);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_WAIT&&check.buffers.remaining==1);
    gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);
    BarrierJob pending={.identity=r->context.thread_id,.selector=selector,.output=(uintptr_t)&out,.capacity=1};
    assert(gc_buffer_job_step(&r->context,&pending,GC_BJ_CLAIM)==GC_BJ_OK);
    assert(gc_buffer_job_step(&r->context,&pending,GC_BJ_COPY)==GC_BJ_OK);
    assert(gc_native_allocation_retire_check_next_locked(&check)==GC_NSI_WAIT);
    gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);
    assert(!out.previous.bits&&!out.slot.owner.bits&&!out.slot.metadata_lease);
    assert(gc_buffer_job_step(&r->context,&pending,GC_BJ_ACK)==GC_BJ_OK);
    do{status=gc_native_allocation_retire_check_next_locked(&check);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_DONE);
    NativeRunRetireCheck copy=check;gc_allocation_fixture_retire_reject(run,&copy,GC_SR_INVALID);
    check.incarnation++;gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);check.incarnation--;
    check.phase^=UINT64_C(0x10000);gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);check.phase^=UINT64_C(0x10000);
    check.scans.serial^=1;gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);check.scans.serial^=1;
    check.queues.serial[1]^=1;gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);check.queues.serial[1]^=1;
    check.roots.last_scope^=1;gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);check.roots.last_scope^=1;
    check.buffers.last_identity^=1;gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);check.buffers.last_identity^=1;
    check.buffers.members^=1;gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);check.buffers.members^=1;
    check.buffers.stage=1;gc_allocation_fixture_retire_reject(run,&check,GC_SR_WAIT);check.buffers.stage=2;
    check.buffers.target_limit-=16;gc_allocation_fixture_retire_reject(run,&check,GC_SR_INVALID);check.buffers.target_limit+=16;
    check.scans.target_limit-=16;gc_allocation_fixture_retire_reject(run,&check,GC_SR_INVALID);check.scans.target_limit+=16;
    check.run=NULL;gc_allocation_fixture_retire_reject(run,&check,GC_SR_INVALID);check.run=run;
    status=gc_native_allocation_retire_begin_locked(run,&check);
    assert(status==GC_SR_MORE&&check.consumed==1);
    assert(gc_native_allocation_retire_check_next_locked(&check)==GC_NSI_INVALID);
    gc_allocation_fixture_retire_reject(run,&check,GC_SR_INVALID);
    return status;
}
/* No collection is active in these fixtures. Check each captured phase before
 * acknowledging: empty queues are not a substitute for mark obligations. */
static void gc_allocation_fixture_drain(NativeRuntimeOwner *runtime){
    RootStorage *storage=(void *)(uintptr_t)runtime->root_charge.allocation;
    RootLease lease={0};RootChange changes[16];
    RootJournalRequest q={.registration=(uintptr_t)&storage->registration,.scope=storage->segment.scope,
        .lease=(uintptr_t)&lease,.output=(uintptr_t)changes,.capacity=16};
    uint64_t status;
    while((status=gc_root_journal_step(&runtime->roots,&q,GC_RJ_CLAIM))==GC_RJ_OK){
        assert(gc_root_journal_step(&runtime->roots,&q,GC_RJ_COPY)==GC_RJ_OK);
        for(uint64_t i=0;i<q.written;i++)assert(!(changes[i].phase&255));
        assert(gc_root_journal_step(&runtime->roots,&q,GC_RJ_ACK)==GC_RJ_OK);
    }
    assert(status==GC_RJ_EMPTY);
}
static void gc_allocation_fixture_roots_finish(NativeRuntimeOwner *runtime){
    assert(atomic_load(&runtime->context.state)==GC_RUNNING);
    gc_allocation_fixture_drain(runtime);
    RootStorage *storage=(void *)(uintptr_t)runtime->root_charge.allocation;
    RootLease lease={0};RootLifeRequest q={.registration=(uintptr_t)&storage->registration,
        .scope=storage->segment.scope,.context=(uintptr_t)&runtime->context,.lease=(uintptr_t)&lease};
    assert(gc_root_life_locked(&runtime->roots,&q,GC_RL_RETIRE)==GC_RL_OK);
    if(atomic_load(&storage->segment.used)){
        assert(gc_root_life_locked(&runtime->roots,&q,GC_RL_CLAIM)==GC_RL_OK&&q.mask==0);
        uint64_t values[16];RootScanJob scan={.scan={.segment=(uintptr_t)&storage->segment,.output=(uintptr_t)values,.capacity=q.end-q.begin},
            .lease=(uintptr_t)&lease,.serial=q.serial};
        assert(gc_root_scan_leased(&scan)==GC_ROOT_OK);
        assert(gc_root_life_locked(&runtime->roots,&q,GC_RL_ACK)==GC_RL_OK);
    }
    assert(gc_root_life_locked(&runtime->roots,&q,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&q)==GC_RL_OK);
    assert(gc_root_life_locked(&runtime->roots,&q,GC_RL_FINISH)==GC_RL_OK);
}
static void gc_allocation_fixture_publish(NativeAllocationRun *run,RootRequest *root,uint64_t bytes){
    NativeRuntimeOwner *runtime=run->runtime;AllocationPage *page=gc_native_allocation_page(run);
    assert(gc_root_step(&runtime->context,root,GC_ROOT_READ_RAW)==GC_ROOT_OK);
    MetadataLeasePool *pool=&gc_native_runtime_work(runtime)->leases;
    MetadataLeaseRequest lease={(uintptr_t)page,page->incarnation,0};
    assert(gc_metadata_step_locked(pool,&lease,GC_ML_ACQUIRE)==GC_ML_OK);
    ForwardLookup lookup={root->value>>16,page->incarnation,0};
    assert(gc_forward_lookup((void *)(uintptr_t)page->forward_table,&lookup)==GC_FL_OK);
    SourceAccessRequest scope={.context=(uintptr_t)&runtime->context,.source_offset=lookup.source_offset,.incarnation=page->incarnation};
    assert(gc_source_access_step((void *)(uintptr_t)lookup.cell,&scope,GC_SA_ENTER)==GC_SA_OK);
    GuardedMarkPrepare publish={.scope=(uintptr_t)&scope,.page=(uintptr_t)page,
        .phase=atomic_load(&runtime->context.adopted_phase),.reference=root->value,.lease=lease};
    ObjectHeader *header=(void *)(uintptr_t)(page->heap_base+lookup.source_offset-sizeof(ObjectHeader));
    assert(header->payload_bytes==bytes&&atomic_load(&header->state)==GC_OBJECT_INITIALIZING);
    assert(header->mark_record==run->marks.records+(run->marks.used-1)*sizeof(MarkRecord));
    const unsigned char *data=(const void *)(header+1);
    for(uint64_t i=0;i<bytes;i++)assert(!data[i]);
    if(bytes>=8){
        NativeBuiltinRegistry *registry=(void *)(uintptr_t)runtime->descriptor_charge.allocation;
        InitializationField field={(uintptr_t)&registry->registry,0,UINT64_C(0x123456789abcdef0),0};
        assert(gc_guarded_initialization_store(&publish,&field)==GC_INIT_OK);
    }
    assert(gc_guarded_initialization_publish(&publish)==GC_INIT_OK);
    assert(atomic_load(&header->state)==GC_OBJECT_PUBLISHED);
    assert(gc_source_access_step((void *)(uintptr_t)lookup.cell,&scope,GC_SA_EXIT)==GC_SA_OK);
    assert(gc_metadata_step_locked(pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
}
static void gc_run_native_allocation_cases(NativeAllocationStarter allocate){
    /* The inventory must retain other preparations when a middle run leaves. */
    {
        NativeRuntimeOwner runtime={0};NativeAllocationRun runs[3]={{0}};NativeHeapOps ops=gc_native_heap_ops();
        gc_allocation_fixture_start(&runtime,&ops,8,8);
        uint64_t baseline=runtime.backing.heap.metadata_committed;
        for(unsigned i=0;i<3;i++)assert(gc_native_allocation_reserve_locked(&runs[i],&runtime,1,1,&ops.metadata)==GC_POOL_OK);
        assert(runtime.allocations==(uintptr_t)&runs[2]&&runs[1].previous==(uintptr_t)&runs[2]&&runs[1].next==(uintptr_t)&runs[0]);
        gc_allocation_fixture_native(&runtime,true);
        const unsigned order[]={1,2,0};
        for(unsigned i=0;i<3;i++){
            NativeAllocationRun *run=&runs[order[i]];
            assert(gc_native_allocation_prepare(run,&ops.metadata)==GC_POOL_OK);
            assert(gc_native_allocation_cancel_locked(run)==GC_POOL_OK);
            assert(gc_native_allocation_cleanup(run,&ops.metadata)==GC_POOL_OK);
            assert(gc_native_allocation_finish_locked(run)==GC_POOL_OK);
            if(!i)assert(runs[2].next==(uintptr_t)&runs[0]&&runs[0].previous==(uintptr_t)&runs[2]);
        }
        assert(!runtime.allocations&&runtime.backing.heap.metadata_committed==baseline);
        gc_allocation_fixture_native(&runtime,false);gc_allocation_fixture_roots_finish(&runtime);
        assert(gc_native_runtime_dispose(&runtime,&ops)==GC_NATIVE_METADATA_OK);
    }
    /* Each admission fails before OS calls and returns all pending credits. */
    for(unsigned fault=0;fault<5;fault++){
        NativeRuntimeOwner runtime={0};NativeAllocationRun run={0};NativeHeapOps ops=gc_native_heap_ops();
        gc_allocation_fixture_start(&runtime,&ops,8,8);
        HeapPool *heap=&runtime.backing.heap;uint64_t baseline=heap->metadata_committed,maximum=heap->metadata_max;
        if(fault<4)heap->metadata_max=baseline+fault*ops.metadata.quantum;
        uint64_t status=gc_native_allocation_reserve_locked(&run,&runtime,fault==4?4:1,2,&ops.metadata);
        if(fault<3)assert(status==GC_POOL_METADATA_LIMIT);
        else if(fault==4)assert(status==GC_POOL_HEAP_LIMIT);
        else{
            assert(status==GC_POOL_OK);assert(gc_native_allocation_cancel_locked(&run)==GC_POOL_OK);
            gc_allocation_fixture_native(&runtime,true);
            assert(gc_native_allocation_cleanup(&run,&ops.metadata)==GC_POOL_OK);
            assert(gc_native_allocation_finish_locked(&run)==GC_POOL_OK);
            gc_allocation_fixture_native(&runtime,false);
        }
        assert(!runtime.allocations&&!heap->pending&&!heap->metadata_pending&&heap->metadata_committed==baseline);
        heap->metadata_max=maximum;gc_allocation_fixture_roots_finish(&runtime);
        assert(gc_native_runtime_dispose(&runtime,&ops)==GC_NATIVE_METADATA_OK);
    }
    /* Three map failures; three unmap failures at both private and READY
     * stages; partial publication is retryable or cancellable. */
    for(unsigned mode=0;mode<12;mode++){
        NativeRuntimeOwner runtime={0};NativeAllocationRun run={0};NativeHeapOps ops=gc_native_heap_ops();
        gc_allocation_fixture_start(&runtime,&ops,8,8);
        uint64_t baseline=runtime.backing.heap.metadata_committed;
        NativeMetadataOps injected=ops.metadata;injected.map=gc_run_map;injected.unmap=gc_run_unmap;
        gc_run_maps=gc_run_unmaps=gc_run_fail_map=gc_run_fail_unmap=0;
        assert(gc_native_allocation_reserve_locked(&run,&runtime,2,2,&injected)==GC_POOL_OK);
        NativeAllocationRun copied=run;assert(!gc_native_allocation_valid(&copied));
        assert(runtime.allocations==(uintptr_t)&run);
        assert(gc_native_allocation_prepare(&run,&injected)==GC_POOL_INVALID&&!gc_run_maps);
        gc_allocation_fixture_native(&runtime,true);
        if(mode<3)gc_run_fail_map=mode+1;
        assert(gc_native_allocation_prepare(&run,&injected)==(mode<3?GC_NATIVE_RUN_OS_ERROR:GC_POOL_OK));
        if(mode>=6){
            if(mode>=9){
                HeapPool *heap=&runtime.backing.heap;uint64_t pending=heap->pending;heap->pending=0;
                assert(gc_native_allocation_publish_locked(&run)==GC_POOL_INVALID&&run.stage==2);
                assert(run.forwarding.stage==2&&run.mark_records.stage==2);
                heap->pending=pending;
            }
            if(mode!=10)assert(gc_native_allocation_publish_locked(&run)==GC_POOL_OK);
        }
        assert(gc_native_allocation_bind_locked(&run)==GC_POOL_INVALID); /* still native */
        assert(gc_native_allocation_cancel_locked(&run)==GC_POOL_OK);
        assert(gc_native_allocation_finish_locked(&run)==GC_POOL_INVALID);
        if(mode>=3&&mode<=8)gc_run_fail_unmap=1+(mode-3)%3;
        HeapPool before=runtime.backing.heap;
        uint64_t status=gc_native_allocation_cleanup(&run,&injected);
        if(mode>=3&&mode<=8){
            assert(status==GC_NATIVE_RUN_OS_ERROR&&run.stage==5);
            assert(!memcmp(&before,&runtime.backing.heap,sizeof(before)));
            assert(gc_native_allocation_finish_locked(&run)==GC_POOL_INVALID);
            gc_run_fail_unmap=0;assert(gc_native_allocation_cleanup(&run,&injected)==GC_POOL_OK);
        }else assert(status==GC_POOL_OK);
        assert(gc_native_allocation_finish_locked(&run)==GC_POOL_OK);
        assert(gc_native_allocation_finish_locked(&run)==GC_POOL_INVALID);
        assert(!runtime.allocations&&!runtime.backing.heap.committed&&!runtime.backing.heap.pending&&
            !runtime.backing.heap.metadata_pending&&runtime.backing.heap.metadata_committed==baseline);
        gc_allocation_fixture_native(&runtime,false);gc_allocation_fixture_roots_finish(&runtime);
        assert(gc_native_runtime_dispose(&runtime,&ops)==GC_NATIVE_METADATA_OK);
    }
    /* Real bootstrap -> backing -> TLAB -> shared rooted allocation and guarded
     * readiness. After registered root/journal retirement, these scalar-only
     * fixtures have no heap edges, jobs, handles or other actors. That complete
     * dead-range proof permits normal owned teardown and budget return. */
    for(unsigned mode=0;mode<5;mode++){
        NativeRuntimeOwner runtime={0};NativeAllocationRun run={0};NativeHeapOps ops=gc_native_heap_ops();
        gc_allocation_fixture_start(&runtime,&ops,mode==3?1:8,mode==2?1:8);
        uint64_t baseline=runtime.backing.heap.metadata_committed;
        assert(gc_native_allocation_reserve_locked(&run,&runtime,mode==1?2:1,mode==4?1:2,&ops.metadata)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,true);
        assert(gc_native_allocation_prepare(&run,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_allocation_publish_locked(&run)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,false);
        assert(gc_native_allocation_bind_locked(&run)==GC_POOL_OK);
        RootRequest root={0};AllocationResult output={0};uint64_t bytes=mode==1?GC_HEAP_PAGE_BYTES+17:mode==0?0:17;
        assert(gc_native_allocation_start(&run,GC_DESC_RAW_BYTES,bytes,&root,&output,allocate)==GC_ALLOC_OK);
        assert(root.value==output.reference&&output.object_id&&run.marks.used==1);
        gc_allocation_fixture_publish(&run,&root,bytes);
        uint64_t top=runtime.context.tlab_top;
        assert(gc_native_allocation_start(&run,GC_DESC_RAW_BYTES,0,&root,&output,allocate)==GC_ALLOC_INVALID);
        RootRequest second={0};AllocationResult result={UINT64_MAX,UINT64_MAX};
        uint64_t status=gc_native_allocation_start(&run,GC_DESC_RAW_BYTES,mode==1?GC_HEAP_PAGE_BYTES:17,&second,&result,allocate);
        uint64_t expected=mode==1?GC_ALLOC_REFILL:mode==2?GC_ALLOC_ROOT_FLUSH:mode==3?GC_ALLOC_ROOT_FULL:mode==4?GC_ALLOC_METADATA_REFILL:GC_ALLOC_OK;
        assert(status==expected);
        if(status){
            assert(runtime.context.tlab_top==top&&run.marks.used==1&&result.reference==UINT64_MAX);
            if(second.handle.cell)assert(!((RootCell *)(uintptr_t)second.handle.cell)->active);
        }else{
            assert(result.object_id!=output.object_id);gc_allocation_fixture_publish(&run,&second,17);
        }
        if(mode==2){
            gc_allocation_fixture_drain(&runtime);second=(RootRequest){0};
            assert(gc_native_allocation_start(&run,GC_DESC_RAW_BYTES,17,&second,&result,allocate)==GC_ALLOC_OK);
            gc_allocation_fixture_publish(&run,&second,17);
        }
        assert(gc_root_step(&runtime.context,&root,GC_ROOT_READ_RAW)==GC_ROOT_OK&&root.value==output.reference);
        assert(gc_native_allocation_cancel_locked(&run)==GC_POOL_INVALID);
        assert(gc_native_allocation_close_locked(&run)==GC_POOL_OK);
        assert(gc_native_allocation_cancel_locked(&run)==GC_POOL_INVALID);
        gc_allocation_fixture_roots_finish(&runtime);
        assert(gc_native_runtime_dispose(&runtime,&ops)==GC_NATIVE_METADATA_INVALID&&runtime.stage==3);
        root=second=(RootRequest){0};output=result=(AllocationResult){0};
        assert(gc_native_allocation_dead_seal_locked(&run)==GC_SR_DONE);
        SourceUncommit dead={.head_index=run.lease.head_index,.page_count=run.lease.page_count,.incarnation=run.lease.incarnation};
        assert(gc_source_uncommit_locked(&runtime.backing.heap,&dead,GC_SU_BEGIN)==GC_SU_OK);
        gc_allocation_fixture_native(&runtime,true);
        assert(gc_native_source_uncommit_payload(&runtime.backing.heap,&dead,NULL)==GC_NATIVE_METADATA_OK);
        assert(gc_source_uncommit_locked(&runtime.backing.heap,&dead,GC_SU_FINISH)==GC_SU_OK);
        assert(gc_allocation_fixture_retire_begin(&run)==GC_SR_MORE);
        do{status=gc_native_allocation_retire_import_locked(&run);assert(status==GC_SR_MORE||status==GC_SR_DONE);}while(status==GC_SR_MORE);
        assert(gc_native_allocation_retire_cleanup(&run,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_allocation_retire_step_locked(&run)==GC_SR_DONE);
        assert(gc_native_records_begin_locked(&run)==GC_SR_DONE);
        assert(gc_native_records_dispose(&run,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_records_finish_locked(&run)==GC_POOL_OK);
        assert(!runtime.allocations&&!runtime.backing.heap.committed&&runtime.backing.heap.metadata_committed==baseline);
        assert(gc_native_runtime_dispose(&runtime,&ops)==GC_NATIVE_METADATA_OK);
    }
    puts("native allocation runs: bootstrap-owned backing, rooted allocation, large payload and retryable cancellation PASS");
}
#endif
