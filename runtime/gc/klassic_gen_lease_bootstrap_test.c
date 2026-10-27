/* Private-constructor budget probe. All mappings/charges/cleanup are real;
 * no managed liveness/phase proof is credited by these structural checks. */
#include "klassic_gen_runtime.c"
#include "klassic_gen_leases.h"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
static void budget_case(int argc,char **argv){
    KgRuntime *r=&kg_runtime;r->os=gc_native_heap_ops();
    assert(!pthread_mutex_init(&r->lock,NULL)&&!pthread_cond_init(&r->changed,NULL));
    NativeRuntimeConfig config={
        .heap={UINT64_C(16)*1024*1024,UINT64_C(8)*1024*1024,UINT64_C(1024)*1024,0},
        .root_cells=128,.root_changes=256,.descriptor_capacity=64,.work={256,1,1024}};
    uint64_t table,roots,descriptors,work,legacy_work,initial,owner;
    NativeWorkConfig legacy_config={256,1024,1024};
    bool baseline=argc>2&&!strcmp(argv[2],"baseline");
    assert(!gc_native_metadata_size((config.heap.hard_max/GC_HEAP_PAGE_BYTES)*sizeof(AllocationPage),r->os.metadata.quantum,&table));
    assert(!gc_native_roots_size(config.root_cells,config.root_changes,r->os.metadata.quantum,&roots));
    assert(!gc_native_builtin_registry_size(config.descriptor_capacity,r->os.metadata.quantum,&descriptors));
    assert(!gc_native_work_size(&legacy_config,r->os.metadata.quantum,&legacy_work));
    assert(!gc_native_work_size(&config.work,r->os.metadata.quantum,&work));
    assert(!gc_native_metadata_size(legacy_config.leases*sizeof(MetadataLeaseSlot),r->os.metadata.quantum,&initial));
    assert(!gc_native_metadata_size(sizeof(KgLeaseBacking),r->os.metadata.quantum,&owner));
    uint64_t legacy_before=table+roots+descriptors+legacy_work;
    uint64_t before=table+roots+descriptors+work;
    uint64_t extra=argc>1?(uint64_t)strtoull(argv[1],NULL,10):initial+owner+work-legacy_work;
    if(baseline){config.work=legacy_config;before=legacy_before;}
    config.heap.metadata_max=legacy_before+extra;
    assert(gc_native_runtime_prepare(&r->native,&config,&r->os)==GC_NATIVE_METADATA_OK);
    assert(r->native.backing.heap.metadata_committed==before&&!r->native.backing.heap.metadata_pending);
    fprintf(stderr,"old constructor exact budget=%llu work=%llu new prefix=%llu owner=%llu extra=%llu\n",
        (unsigned long long)legacy_before,(unsigned long long)(baseline?legacy_work:work),(unsigned long long)initial,(unsigned long long)owner,(unsigned long long)extra);
    if(!baseline){
        kg_lease_initialize(r,&r->native.context,legacy_config.leases,NULL);
        assert(r->native.backing.heap.metadata_committed==before+initial+owner);
        assert(!r->native.backing.heap.metadata_pending);
        fprintf(stderr,"stable constructor actual delta=%llu\n",(unsigned long long)(r->native.backing.heap.metadata_committed-legacy_before));
        kg_lease_dispose_native(r,&r->native.context);
        assert(r->native.backing.heap.metadata_committed==before&&!r->native.backing.heap.metadata_pending);
    }
    assert(gc_native_runtime_dispose(&r->native,&r->os)==GC_NATIVE_METADATA_OK);
    assert(!pthread_cond_destroy(&r->changed)&&!pthread_mutex_destroy(&r->lock));
    puts("actual constructor budget and OS cleanup PASS");
}

static uint64_t test_quantum=4096;
static unsigned meta_mappings,vm_reservations,reserve_calls,commit_calls;
static unsigned reserve_failures,commit_after_effect_failures;
static MetadataLeaseSlot *old_slots;
static MetadataLeaseSlot old_first;
static uint64_t old_capacity,before_prefix,owner_bytes,initial_bytes;
static void *aligned_mapping(size_t bytes,int protection){
    assert(bytes&&!(bytes&(test_quantum-1))&&bytes<=SIZE_MAX-test_quantum);
    size_t total=bytes+(size_t)test_quantum;
    void *raw=mmap(NULL,total,protection,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
    assert(raw!=MAP_FAILED);
    uintptr_t base=(uintptr_t)raw,aligned=(base+test_quantum-1)&~(test_quantum-1);
    size_t prefix=(size_t)(aligned-base),suffix=total-prefix-bytes;
    if(prefix)assert(!munmap(raw,prefix));
    if(suffix)assert(!munmap((void *)(aligned+bytes),suffix));
    return (void *)aligned;
}
static void *aligned_metadata_map(size_t bytes){meta_mappings++;return aligned_mapping(bytes,PROT_READ|PROT_WRITE);}
static bool aligned_metadata_unmap(void *address,size_t bytes){
    assert(meta_mappings);if(munmap(address,bytes))return false;meta_mappings--;return true;
}
static void *aligned_reserve(size_t bytes){vm_reservations++;return aligned_mapping(bytes,PROT_NONE);}
static bool aligned_release(void *address,size_t bytes){
    assert(vm_reservations);if(munmap(address,bytes))return false;vm_reservations--;return true;
}
static void original_pool_still_private(void){
    MetadataLeasePool *pool=kg_leases(&kg_runtime);
    assert(pool->slots==(uintptr_t)old_slots&&pool->capacity==old_capacity&&pool->free_head==1&&!pool->active);
    assert(!memcmp(old_slots,&old_first,sizeof(old_first)));
    assert(kg_runtime.native.backing.heap.metadata_committed==before_prefix+owner_bytes);
}
static void *faulted_reserve(size_t bytes){
    reserve_calls++;original_pool_still_private();
    assert(!kg_runtime.native.backing.heap.metadata_pending);
    if(reserve_failures){
        reserve_failures--;
        /* An actual OS failure with no returned reservation, not a lost map. */
        assert(mmap(NULL,0,PROT_NONE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0)==MAP_FAILED);
        return NULL;
    }
    return aligned_reserve(bytes);
}
static bool faulted_commit(void *address,size_t bytes){
    commit_calls++;original_pool_still_private();
    assert(kg_runtime.native.backing.heap.metadata_pending==initial_bytes);
    assert(bytes==initial_bytes);
    assert(kgl_commit(address,bytes));
    if(commit_after_effect_failures){commit_after_effect_failures--;return false;}
    return true;
}
static void core_native(KgRuntime *r,bool enter){
    kg_lock(r);uint64_t status=gc_worker_step_locked(&r->native.roots,&r->native.worker,
        enter?GC_WK_ENTER_NATIVE:GC_WK_LEAVE_NATIVE);kg_unlock(r);assert(status==GC_WK_OK);
}
static void retire_empty_run(KgRuntime *r,NativeAllocationRun *run){
    AllocationPage *page=gc_native_allocation_page(run);
    ForwardTable *table=(void *)(uintptr_t)run->forwarding.allocation;
    /* A real bound-and-closed TLAB contains no allocated objects. With the
     * exclusive fixture owner, empty roots/queues/buffers and zero leases,
     * the wholly-dead range proof is structural rather than guessed mark bits. */
    assert(run->stage==8&&!run->marks.used&&!atomic_load(&table->used));
    assert(atomic_load(&page->allocated_top)==page->start&&!page->metadata_leases);
    assert(!atomic_load(&r->native.context.access_epoch)&&!atomic_load(&r->native.context.private_producers));
    assert(!r->native.mark_scan_count&&!r->native.roots.leases&&!r->native.roots.walkers);
    RootStorage *roots=(void *)(uintptr_t)r->native.root_charge.allocation;
    assert(!atomic_load(&roots->segment.used)&&!roots->journal.top);
    NativeWorkStorage *work=gc_native_runtime_work(&r->native);
    assert(gc_mark_step_locked(&work->queues[0],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
    assert(gc_mark_step_locked(&work->queues[1],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
    kg_lock(r);assert(gc_native_allocation_dead_seal_locked(run)==GC_SR_DONE);
    SourceUncommit uncommit={.head_index=run->lease.head_index,.page_count=run->lease.page_count,.incarnation=run->lease.incarnation};
    assert(gc_source_uncommit_locked(&r->native.backing.heap,&uncommit,GC_SU_BEGIN)==GC_SU_OK);kg_unlock(r);
    assert(gc_native_source_uncommit_payload(&r->native.backing.heap,&uncommit,NULL)==GC_NATIVE_METADATA_OK);
    kg_lock(r);assert(gc_source_uncommit_locked(&r->native.backing.heap,&uncommit,GC_SU_FINISH)==GC_SU_OK);
    NativeRunRetireCheck check={0};assert(gc_native_allocation_retire_check_begin_locked(run,&check)==GC_NSI_MORE);
    uint64_t status;do{status=gc_native_allocation_retire_check_next_locked(&check);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_DONE);assert(gc_native_allocation_retire_begin_locked(run,&check)==GC_SR_MORE);
    assert(gc_native_allocation_retire_import_locked(run)==GC_SR_DONE);kg_unlock(r);
    assert(gc_native_allocation_retire_cleanup_native(run,&r->os.metadata,&r->native.context)==GC_POOL_OK);
    kg_lock(r);do{status=gc_native_allocation_retire_step_locked(run);}while(status==GC_SR_MORE);
    assert(status==GC_SR_DONE);assert(gc_native_records_begin_locked(run)==GC_SR_DONE);kg_unlock(r);
    assert(gc_native_records_dispose_native(run,&r->os.metadata,&r->native.context)==GC_POOL_OK);
    kg_lock(r);assert(gc_native_records_finish_locked(run)==GC_POOL_OK);kg_unlock(r);
    assert(!r->native.allocations&&!r->native.backing.heap.committed&&!r->native.backing.heap.pending);
}
static void exercise_prepared_padding(KgRuntime *r){
    assert(test_quantum==65536);
    assert(gc_native_runtime_activate(&r->native)==GC_NATIVE_METADATA_OK);core_native(r,true);
    NativeAllocationRun run={0};kg_lock(r);
    assert(gc_native_allocation_reserve_target_locked(&run,&r->native,1,4,GC_YOUNG,&r->os.metadata)==GC_POOL_OK);kg_unlock(r);
    assert(gc_native_allocation_prepare_native(&run,&r->os.metadata,&r->native.context)==GC_POOL_OK);
    kg_lock(r);assert(gc_native_allocation_publish_locked(&run)==GC_POOL_OK);kg_unlock(r);
    core_native(r,false);kg_lock(r);
    assert(gc_native_allocation_bind_context_locked(&run,&r->native.context)==GC_POOL_OK);
    assert(gc_native_allocation_close_context_locked(&run,&r->native.context)==GC_POOL_OK);kg_unlock(r);core_native(r,true);
    AllocationPage *page=gc_native_allocation_page(&run);MetadataLeasePool *pool=kg_leases(r);
    MetadataLeaseRequest held[2048]={0};uint64_t issued=0,base=pool->slots;
    uint64_t budget=r->native.backing.heap.metadata_committed;
    for(uint64_t expected=1024;expected<=2048;expected+=256){
        kg_lock(r);assert(pool->capacity==expected);
        while(issued<expected){
            held[issued]=(MetadataLeaseRequest){.page=(uintptr_t)page,.incarnation=page->incarnation};
            assert(gc_metadata_step_locked(pool,&held[issued],GC_ML_ACQUIRE)==GC_ML_OK);issued++;
        }
        assert(!pool->free_head&&pool->active==issued&&page->metadata_leases==issued);kg_unlock(r);
        if(expected==2048)break;
        assert(kg_lease_grow_native(r,&r->native.context)==KG_LEASE_OK);kg_lock(r);
        assert(pool->slots==base&&pool->capacity==expected+256);
        assert(r->native.backing.heap.metadata_committed==budget&&!r->native.backing.heap.metadata_pending);
        for(uint64_t i=0;i<issued;i++){
            uint64_t token=held[i].token;assert(gc_metadata_step_locked(pool,&held[i],GC_ML_RESOLVE)==GC_ML_OK);
            assert(held[i].token==token&&held[i].page==(uintptr_t)page&&held[i].incarnation==page->incarnation);
        }
        kg_unlock(r);
    }
    kg_lock(r);for(uint64_t i=0;i<issued;i++)assert(gc_metadata_step_locked(pool,&held[i],GC_ML_RELEASE)==GC_ML_OK);
    assert(!pool->active&&!page->metadata_leases);kg_unlock(r);retire_empty_run(r,&run);
    core_native(r,false);RootStorage *roots=(void *)(uintptr_t)r->native.root_charge.allocation;
    assert(!atomic_load(&roots->segment.used)&&!roots->journal.top);
    RootLease lease={0};RootLifeRequest request={.registration=(uintptr_t)&roots->registration,.scope=roots->segment.scope,
        .context=(uintptr_t)&r->native.context,.lease=(uintptr_t)&lease};
    kg_lock(r);assert(gc_root_life_locked(&r->native.roots,&request,GC_RL_RETIRE)==GC_RL_OK);
    assert(gc_root_life_locked(&r->native.roots,&request,GC_RL_CLEAN)==GC_RL_OK);kg_unlock(r);
    assert(gc_root_clear_chunk(&request)==GC_RL_OK);kg_lock(r);
    assert(gc_root_life_locked(&r->native.roots,&request,GC_RL_FINISH)==GC_RL_OK);kg_unlock(r);core_native(r,true);
    assert(commit_calls==1);
}
static void bootstrap_case(const char *mode){
    bool large=!strcmp(mode,"quantum64");test_quantum=large?65536:4096;
    reserve_failures=!strcmp(mode,"reserve-retry")?3:!strcmp(mode,"reserve-permanent")?32:0;
    commit_after_effect_failures=!strcmp(mode,"commit-retry")?3:0;
    KgRuntime *r=&kg_runtime;r->os=gc_native_heap_ops();
    r->os.metadata=(NativeMetadataOps){test_quantum,aligned_metadata_map,aligned_metadata_unmap};
    r->os.reserve=aligned_reserve;r->os.release=aligned_release;
    assert(!pthread_mutex_init(&r->lock,NULL)&&!pthread_cond_init(&r->changed,NULL));
    NativeRuntimeConfig config={{UINT64_C(16)*1024*1024,UINT64_C(8)*1024*1024,UINT64_C(1024)*1024,UINT64_C(8)*1024*1024},128,256,64,{256,1,1024}};
    assert(gc_native_runtime_prepare(&r->native,&config,&r->os)==GC_NATIVE_METADATA_OK);
    MetadataLeasePool *pool=kg_leases(r);old_slots=(void *)(uintptr_t)pool->slots;old_first=old_slots[0];old_capacity=pool->capacity;
    before_prefix=r->native.backing.heap.metadata_committed;
    assert(!gc_native_metadata_size(sizeof(KgLeaseBacking),test_quantum,&owner_bytes));
    assert(!gc_native_metadata_size(1024*sizeof(MetadataLeaseSlot),test_quantum,&initial_bytes));
    if(!strcmp(mode,"generation-reject"))old_slots[0].generation=1;
    KgLeaseOps operations={faulted_reserve,faulted_commit,gc_native_heap_decommit_range,aligned_release};
    kg_lease_initialize(r,&r->native.context,1024,&operations);
    assert(pool->capacity==1024&&!pool->active&&pool->free_head==1&&pool->slots!=(uintptr_t)old_slots);
    assert(!memcmp(old_slots,&old_first,sizeof(old_first)));
    assert(r->native.backing.heap.metadata_committed==before_prefix+owner_bytes+initial_bytes);
    assert(!r->native.backing.heap.metadata_pending);
    if(large)exercise_prepared_padding(r);
    kg_lease_dispose_native(r,&r->native.context);
    assert(r->native.backing.heap.metadata_committed==before_prefix&&!r->native.backing.heap.metadata_pending);
    assert(gc_native_runtime_dispose(&r->native,&r->os)==GC_NATIVE_METADATA_OK);
    assert(!meta_mappings&&!vm_reservations);
    assert(!pthread_cond_destroy(&r->changed)&&!pthread_mutex_destroy(&r->lock));
    printf("lease bootstrap: %s real reservation/commit, token generation, charge and complete OS cleanup PASS\n",mode);
}
int main(int argc,char **argv){
    if(argc>1&&!strcmp(argv[1],"budget"))budget_case(argc-1,argv+1);
    else bootstrap_case(argc>1?argv[1]:"reserve-retry");return 0;
}
