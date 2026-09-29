#ifndef KLASSIC_GC_NATIVE_ALLOCATION_RETIRE_CASES_H
#define KLASSIC_GC_NATIVE_ALLOCATION_RETIRE_CASES_H
#include "gc_native_allocation_cases.h"
#include "gc_native_allocation_retire.h"
#include "gc_native_records.h"
#include "gc_mark.h"
#include "gc_native_source_uncommit.h"
#include "gc_source_drain.h"
#include "gc_relocate.h"
#include "gc_forward_incoming.h"
#include "gc_resolve.h"
#include <pthread.h>
#include <sched.h>
typedef struct {
    NativeAllocationRun *run;
    pthread_mutex_t heap_lock;
    _Atomic unsigned attempts,done;
} NativeRecordSealRace;
static void *gc_record_seal_probe(void *arg){
    NativeRecordSealRace *race=arg;RelocationCopy stale={0};
    do{
        assert(!pthread_mutex_lock(&race->heap_lock));
        assert(gc_native_allocation_install_copy_locked(race->run,race->run,&stale)==GC_TARGET_INVALID);
        assert(gc_native_records_begin_locked(race->run)==GC_SR_INVALID);
        assert(!pthread_mutex_unlock(&race->heap_lock));
        atomic_fetch_add_explicit(&race->attempts,1,memory_order_release);
    }while(!atomic_load_explicit(&race->done,memory_order_acquire));
    return NULL;
}
/* Native disposal leaves the lock-published sealed state immutable. Even late
 * invalid admissions must reject before reading metadata being unmapped. */
static void gc_record_seal_dispose_race(NativeAllocationRun *run,const NativeMetadataOps *ops){
    NativeRecordSealRace race={.run=run};pthread_t thread;
    assert(!pthread_mutex_init(&race.heap_lock,NULL));
    assert(!pthread_create(&thread,NULL,gc_record_seal_probe,&race));
    while(atomic_load_explicit(&race.attempts,memory_order_acquire)<32)sched_yield();
    assert(gc_native_records_dispose(run,ops)==GC_POOL_OK);
    unsigned after=atomic_load_explicit(&race.attempts,memory_order_acquire);
    while(atomic_load_explicit(&race.attempts,memory_order_acquire)-after<32)sched_yield();
    atomic_store_explicit(&race.done,1,memory_order_release);
    assert(!pthread_join(thread,NULL));assert(!pthread_mutex_destroy(&race.heap_lock));
}
/* One scalar object, one registered root, no other actors/jobs/handles. The
 * root and its journal move away from the source, both tables are audited,
 * and native controls stop carrying usable source references before BEGIN.
 * This supplies this fixture's retirement proof, not a general cycle audit. */
static void gc_run_native_allocation_retire_cases(NativeAllocationStarter allocate){
    for(unsigned round=0;round<6;round++){
        NativeRuntimeOwner runtime={0};NativeAllocationRun source={0},target={0},reused={0},tail={0};
        NativeHeapOps ops=gc_native_heap_ops();
        NativeRuntimeConfig config={{6*GC_HEAP_PAGE_BYTES,3*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES,64*ops.metadata.quantum},8,8,64,{8,8,8}};
        assert(gc_native_runtime_prepare(&runtime,&config,&ops)==GC_NATIVE_METADATA_OK);
        assert(gc_native_runtime_activate(&runtime)==GC_NATIVE_METADATA_OK);
        HeapPool *heap=&runtime.backing.heap;
        uint64_t baseline=heap->metadata_committed;
        assert(gc_native_allocation_reserve_locked(&source,&runtime,1+round%3,1,&ops.metadata)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,true);
        assert(gc_native_allocation_prepare(&source,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_allocation_publish_locked(&source)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,false);
        assert(gc_native_allocation_bind_locked(&source)==GC_POOL_OK);
        RootRequest root={0};AllocationResult output={0};
        assert(gc_native_allocation_start(&source,GC_DESC_RAW_BYTES,17,&root,&output,allocate)==GC_ALLOC_OK);
        gc_allocation_fixture_publish(&source,&root,17);
        assert(gc_native_allocation_close_locked(&source)==GC_POOL_OK);
        assert(gc_native_allocation_retire_begin_locked(&source,NULL)==GC_SR_INVALID);
        uint64_t identity=output.object_id,record=source.marks.records;
        assert(gc_native_allocation_reserve_locked(&target,&runtime,1,1,&ops.metadata)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,true);
        assert(gc_native_allocation_prepare(&target,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_allocation_publish_locked(&target)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,false);
        AllocationPage *page=gc_native_allocation_page(&source),*destination=gc_native_allocation_page(&target);
        ForwardTable *table=(void *)(uintptr_t)source.forwarding.allocation;
        ForwardCell *cell=(void *)(uintptr_t)table->cells;
        ForwardRequest claim={.source_offset=root.value>>16,.incarnation=page->incarnation,.ticket=1};
        assert(gc_forward_step(cell,&claim,GC_FWD_CLAIM)==GC_FWD_OK);
        SourceAccessRequest select={.source_offset=claim.source_offset,.incarnation=page->incarnation,.ticket=1};
        assert(gc_source_access_step(cell,&select,GC_SA_SELECT)==GC_SA_OK);
        assert(gc_source_access_step(cell,&select,GC_SA_CHECK)==GC_SA_OK);
        RelocationCopy copy={heap->heap_base,page->incarnation,1,page->limit,0,0,7,0,0};
        assert(gc_relocate_target_locked(destination,&copy,GC_TARGET_PREPARE)==GC_TARGET_OK);
        uint64_t status;
        do{status=gc_relocate_copy(cell,&copy);assert(status==GC_COPY_MORE||status==GC_COPY_DONE);}while(status==GC_COPY_MORE);
        assert(gc_native_allocation_install_copy_locked(&target,&target,&copy)==GC_TARGET_INVALID);
        assert(!source.import_users&&!target.import_users);
        ObjectHeader *candidate=(void *)(uintptr_t)(heap->heap_base+copy.destination-sizeof(ObjectHeader));
        atomic_store(&candidate->state,GC_OBJECT_INITIALIZING);
        assert(gc_native_allocation_install_copy_locked(&target,&source,&copy)==GC_TARGET_INVALID);
        assert(!source.import_users&&!*(uint64_t *)(uintptr_t)target.imports);
        atomic_store(&candidate->state,GC_OBJECT_PUBLISHED);candidate=NULL;
        assert(gc_native_allocation_install_copy_locked(&target,&source,&copy)==GC_TARGET_OK);
        assert(source.import_users==1&&target.stage==8);
        assert(gc_native_allocation_install_copy_locked(&target,&source,&copy)==GC_TARGET_INVALID);
        assert(source.import_users==1);
        uint64_t color=gc_abi_color((PhaseSnapshot){atomic_load(&runtime.context.adopted_phase)});
        assert(gc_relocate_publish(cell,&copy,color)==GC_COPY_DONE);
        root.value=(copy.destination<<16)|color;
        assert(gc_root_step(&runtime.context,&root,GC_ROOT_STORE)==GC_ROOT_OK);
        gc_allocation_fixture_drain(&runtime);output.reference=0;
        SourceDrain drain={page->incarnation,atomic_load(&page->allocated_top),1,0,1,0};
        assert(gc_source_drain(page,&drain)==GC_SD_DONE);
        SourceUncommit payload={.head_index=source.lease.head_index,.page_count=source.lease.page_count,.incarnation=page->incarnation};
        assert(gc_source_uncommit_locked(heap,&payload,GC_SU_BEGIN)==GC_SU_OK);
        gc_allocation_fixture_native(&runtime,true);
        assert(gc_native_source_uncommit_payload(heap,&payload,NULL)==GC_NATIVE_METADATA_OK);
        assert(gc_source_uncommit_locked(heap,&payload,GC_SU_FINISH)==GC_SU_OK);
        for(unsigned i=0;i<2;i++){
            ForwardTable *t=(void *)(uintptr_t)(i?target.forwarding.allocation:source.forwarding.allocation);
            AllocationPage *p=i?destination:page;
            ForwardIncoming incoming={p->incarnation,atomic_load(&t->used),page->start,page->limit,0,1,0,0};
            assert(gc_forward_incoming(t,&incoming)==GC_FI_DONE);
        }
        assert(gc_allocation_fixture_retire_begin(&source)==GC_SR_MORE);
        assert(gc_native_allocation_retire_step_locked(&source)==GC_SR_WAIT);
        assert(gc_native_allocation_retire_cleanup(&source,&ops.metadata)==GC_POOL_INVALID);
        assert(gc_native_allocation_retire_import_locked(&source)==GC_SR_DONE);
        NativeAllocationRun copied=source;
        assert(gc_native_allocation_retire_cleanup(&copied,&ops.metadata)==GC_POOL_INVALID);
        uint64_t before=heap->metadata_committed,physical=source.bitmap.bytes+source.forwarding.bytes;
        NativeMetadataOps injected=ops.metadata;injected.unmap=gc_run_unmap;
        gc_run_unmaps=0;gc_run_fail_unmap=round/2; /* none, bitmap, table */
        status=gc_native_allocation_retire_cleanup(&source,&injected);
        if(round>=2){
            assert(status==GC_NATIVE_RUN_OS_ERROR&&heap->metadata_committed==before);
            assert(gc_native_allocation_retire_step_locked(&source)==GC_SR_WAIT);
            assert(source.mark_records.allocation==record&&source.mark_records.stage==2);
            gc_run_fail_unmap=0;
            assert(gc_native_allocation_retire_cleanup(&source,&injected)==GC_POOL_OK);
        }else assert(status==GC_POOL_OK);
        do{
            status=gc_native_allocation_retire_step_locked(&source);assert(status==GC_SR_MORE||status==GC_SR_DONE);
            if(status==GC_SR_MORE){
                assert(!tail.self);
                assert(gc_native_allocation_reserve_locked(&tail,&runtime,1,1,&ops.metadata)==GC_POOL_OK);
                assert(tail.lease.head_index==source.lease.head_index+1);
                assert(gc_native_allocation_prepare(&tail,&ops.metadata)==GC_POOL_OK);
                assert(gc_native_allocation_publish_locked(&tail)==GC_POOL_OK);
            }
        }while(status==GC_SR_MORE);
        if(tail.self){
            assert(gc_native_allocation_cancel_locked(&tail)==GC_POOL_OK);
            assert(gc_native_allocation_cleanup(&tail,&ops.metadata)==GC_POOL_OK);
            assert(gc_native_allocation_finish_locked(&tail)==GC_POOL_OK);
        }
        assert(source.stage==10&&heap->metadata_committed==before-physical);
        assert(source.mark_records.stage==2&&source.mark_records.allocation==record&&source.marks.used==1);
        assert(gc_native_records_begin_locked(&source)==GC_SR_WAIT);
        assert(gc_native_allocation_retire_step_locked(&source)==GC_SR_INVALID);
        assert(gc_native_allocation_cancel_locked(&source)==GC_POOL_INVALID);
        assert(gc_native_allocation_reserve_locked(&reused,&runtime,source.lease.page_count,1,&ops.metadata)==GC_POOL_OK);
        assert(reused.lease.head_index==source.lease.head_index&&reused.lease.incarnation>source.lease.incarnation);
        assert(gc_native_allocation_prepare(&reused,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_allocation_publish_locked(&reused)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,false);
        ResolveRequest read={.reference=root.value,.scope={.context=(uintptr_t)&runtime.context}};
        assert(gc_resolve_access(destination,&read,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        ObjectHeader *header=(void *)(uintptr_t)(heap->heap_base+(root.value>>16)-32);
        assert(header->mark_record==record&&((MarkRecord *)(uintptr_t)record)->object_id==identity);
        uint64_t scalar;memcpy(&scalar,header+1,sizeof(scalar));assert(scalar==UINT64_C(0x123456789abcdef0));
        assert(gc_resolve_access(destination,&read,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        header=NULL;
        assert(gc_native_allocation_cancel_locked(&reused)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,true);
        assert(gc_native_allocation_cleanup(&reused,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_allocation_finish_locked(&reused)==GC_POOL_OK);
        gc_allocation_fixture_native(&runtime,false);
        /* A real queued/claimed/finished job retains the last physical copy's
         * metadata lease even after root removal and queue completion. */
        NativeWorkStorage *work=gc_native_runtime_work(&runtime);MetadataLeasePool *pool=&work->leases;
        MetadataLeaseRequest lease={(uintptr_t)destination,destination->incarnation,0};
        assert(gc_metadata_step_locked(pool,&lease,GC_ML_ACQUIRE)==GC_ML_OK);
        MarkQueue *queue=&work->queues[round%2];
        MarkRequest job={.record=record,.reference=root.value,.incarnation=lease.incarnation,.metadata_lease=lease.token};
        assert(gc_mark_step_locked(queue,&job,GC_MARK_OFFER)==GC_MARK_OK);lease=(MetadataLeaseRequest){0};
        NativeRunRetireCheck queued={0};
        assert(gc_native_allocation_retire_check_begin_locked(&target,&queued)==GC_NSI_MORE);
        uint64_t audit_status;do{audit_status=gc_native_allocation_retire_check_next_locked(&queued);}while(audit_status==GC_NSI_MORE);
        assert(audit_status==GC_NSI_FOUND&&queued.queues.generation==round%2);
        gc_allocation_fixture_roots_finish(&runtime);
        assert(gc_native_runtime_dispose(&runtime,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(gc_native_allocation_dead_seal_locked(&target)==GC_SR_WAIT);
        assert(gc_native_records_begin_locked(&source)==GC_SR_WAIT);
        assert(gc_mark_step_locked(queue,&job,GC_MARK_TAKE)==GC_MARK_OK);
        assert(gc_native_allocation_retire_check_next_locked(&queued)==GC_NSI_FOUND);
        assert(gc_native_allocation_dead_seal_locked(&target)==GC_SR_WAIT);
        assert(gc_mark_step_locked(queue,&job,GC_MARK_FINISH)==GC_MARK_OK);
        do{audit_status=gc_native_allocation_retire_check_next_locked(&queued);}while(audit_status==GC_NSI_MORE);
        assert(audit_status==GC_NSI_WAIT&&!queued.queues.observed); /* Running owner has not handed off. */
        gc_allocation_fixture_native(&runtime,true);
        do{audit_status=gc_native_allocation_retire_check_next_locked(&queued);}while(audit_status==GC_NSI_MORE);
        assert(audit_status==GC_NSI_DONE);
        gc_allocation_fixture_native(&runtime,false);
        assert(gc_native_allocation_retire_check_cancel_locked(&queued)==GC_NSI_DONE);
        assert(gc_native_allocation_dead_seal_locked(&target)==GC_SR_WAIT);
        lease=(MetadataLeaseRequest){(uintptr_t)destination,job.incarnation,job.metadata_lease};
        assert(gc_metadata_step_locked(pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
        job=(MarkRequest){0};root=(RootRequest){0};read=(ResolveRequest){0};copy=(RelocationCopy){0};
        /* No roots/journals/heap edges/jobs/weak/native users or incoming
         * forwarding survive. There are no concurrent producers in this
         * fixture. This is the whole-range dead proof, not queue-empty alone. */
        assert(gc_native_allocation_dead_seal_locked(&target)==GC_SR_DONE);
        SourceUncommit dead={.head_index=target.lease.head_index,.page_count=target.lease.page_count,.incarnation=target.lease.incarnation};
        assert(gc_source_uncommit_locked(heap,&dead,GC_SU_BEGIN)==GC_SU_OK);
        gc_allocation_fixture_native(&runtime,true);
        assert(gc_native_source_uncommit_payload(heap,&dead,NULL)==GC_NATIVE_METADATA_OK);
        assert(gc_source_uncommit_locked(heap,&dead,GC_SU_FINISH)==GC_SU_OK);
        assert(gc_allocation_fixture_retire_begin(&target)==GC_SR_MORE);
        assert(gc_native_records_begin_locked(&source)==GC_SR_WAIT);
        assert(gc_native_allocation_retire_import_locked(&target)==GC_SR_DONE);
        assert(!source.import_users);
        assert(gc_native_allocation_retire_import_locked(&target)==GC_SR_DONE&&!source.import_users);
        assert(gc_native_records_begin_locked(&source)==GC_SR_DONE);
        assert(gc_native_records_begin_locked(&source)==GC_SR_INVALID);
        uint64_t charged=heap->metadata_committed;
        if(round&1){
            gc_run_unmaps=0;gc_run_fail_unmap=1;
            assert(gc_native_records_dispose(&source,&injected)==GC_NATIVE_RUN_OS_ERROR);
            assert(source.stage==11&&source.mark_records.allocation==record&&heap->metadata_committed==charged);
            assert(gc_native_records_finish_locked(&source)==GC_POOL_INVALID);
            gc_run_fail_unmap=0;
        }
        record=0;
        gc_record_seal_dispose_race(&source,&ops.metadata);
        assert(gc_native_records_finish_locked(&source)==GC_POOL_OK);
        assert(heap->metadata_committed==charged-source.mark_records.bytes);
        assert(gc_native_records_finish_locked(&source)==GC_POOL_INVALID);
        assert(gc_native_allocation_retire_cleanup(&target,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_allocation_retire_step_locked(&target)==GC_SR_DONE);
        assert(gc_native_records_begin_locked(&target)==GC_SR_DONE);
        assert(gc_native_records_dispose(&target,&ops.metadata)==GC_POOL_OK);
        assert(gc_native_records_finish_locked(&target)==GC_POOL_OK);
        assert(!runtime.allocations&&!heap->committed&&!heap->pending&&!heap->metadata_pending&&heap->metadata_committed==baseline);
        assert(gc_native_runtime_dispose(&runtime,&ops)==GC_NATIVE_METADATA_OK);
    }
    puts("native owned retirement: source backing returns credits while copied logical records survive range reuse PASS");
    puts("native logical records: imported copies and queued leases delay final unmap, budgets and runtime shutdown complete PASS");
}
#endif
