/* Actual OS backing and tokens: exercise public operations while old native
 * holders occupy the complete current pool. No synthetic active/full counters. */
#include "klassic_gen_runtime.c"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>

static KgLeaseOps real_lease_ops;
static NativeMetadataOps real_metadata_ops;
static unsigned fail_commit,fail_decommit,fail_release,fail_map,fail_unmap;
static uintptr_t unmap_target;
static bool lease_commit_probe(void *address,size_t bytes){
    MutatorContext *ctx=kg_current_context(&kg_runtime);
    assert((ctx&&atomic_load_explicit(&ctx->state,memory_order_acquire)==GC_BLOCKED_NATIVE)||
        (kg_owned_driver&&atomic_load_explicit(&kg_owned_driver->actor.state,memory_order_acquire)==GC_BLOCKED_NATIVE));
    if(fail_commit){fail_commit--;return false;}return real_lease_ops.commit(address,bytes);
}
static bool lease_decommit_probe(void *address,size_t bytes){
    if(fail_decommit){fail_decommit--;assert(!mprotect(address,bytes,PROT_NONE));return false;}
    return real_lease_ops.decommit(address,bytes);
}
static bool lease_release_probe(void *address,size_t bytes){
    if(fail_release){fail_release--;return false;}return real_lease_ops.release(address,bytes);
}
static void *lease_map_probe(size_t bytes){
    if(fail_map){fail_map--;return NULL;}return real_metadata_ops.map(bytes);
}
static bool lease_unmap_probe(void *address,size_t bytes){
    if((uintptr_t)address==unmap_target&&fail_unmap){fail_unmap--;return false;}
    return real_metadata_ops.unmap(address,bytes);
}
static MetadataLeaseRequest take_page(KgRuntime *r,AllocationPage *page){
    MetadataLeaseRequest lease={.page=(uintptr_t)page,.incarnation=page->incarnation};
    uint64_t status=gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_ACQUIRE);
    assert(status==GC_ML_OK);return lease;
}
static void actual_full_public_paths(bool faults){
    KgRuntime *r=&kg_runtime;RootCell *leaf=kg_root_new(),*record=kg_root_new(),*out=kg_root_new(),*fresh=kg_root_new();
    kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,771);kg_alloc_into(8,2,record);kg_store_ref(record,0,leaf);
    KgLeaseBacking *owner=r->lease_backing;uint64_t base=kg_leases(r)->slots;
    real_lease_ops=owner->ops;real_metadata_ops=r->os.metadata;
    if(faults){
        owner->ops.commit=lease_commit_probe;owner->ops.decommit=lease_decommit_probe;owner->ops.release=lease_release_probe;
        r->os.metadata.map=lease_map_probe;r->os.metadata.unmap=lease_unmap_probe;
        fail_map=1;fail_commit=1;fail_decommit=1;fail_release=1;
    }
    MetadataLeaseRequest held[12000]={0};uint64_t count=0;
    kg_lock(r);MetadataLeasePool *pool=kg_leases(r);assert(pool->capacity==1024&&!pool->active);
    ReferenceLease source={.reference=atomic_load_explicit(&leaf->value,memory_order_acquire)};
    assert(gc_reference_lease_locked(pool,&source)==GC_ML_OK);AllocationPage *page=(void *)(uintptr_t)source.lease.page;
    held[count++]=source.lease;source.lease=(MetadataLeaseRequest){0};
    while(count<pool->capacity)held[count++]=take_page(r,page);
    assert(!pool->free_head&&pool->active==count);kg_unlock(r);
    /* This used to clean-cancel/retry forever with 1024 retained leases. */
    kg_load_ref_into(record,0,out);assert(kg_load_i64(out,0)==771);
    kg_lock(r);assert(pool->slots==base&&pool->capacity==1280&&pool->active==count);kg_unlock(r);
    while(count<8193){
        kg_lock(r);bool full=!pool->free_head;
        if(!full)held[count++]=take_page(r,page);kg_unlock(r);
        if(full)kg_lease_pressure(r);
    }
    kg_lock(r);
    for(uint64_t i=0;i<count;i++){
        MetadataLeaseRequest resolve={.token=held[i].token};assert(!gc_metadata_step_locked(pool,&resolve,GC_ML_RESOLVE));
        assert(resolve.page==held[i].page&&resolve.incarnation==held[i].incarnation);
    }
    MetadataLeaseRequest stale=held[0];assert(!gc_metadata_step_locked(pool,&held[0],GC_ML_RELEASE));held[0]=take_page(r,page);
    assert((uint32_t)held[0].token==(uint32_t)stale.token&&held[0].token!=stale.token);
    assert(gc_metadata_step_locked(pool,&stale,GC_ML_RESOLVE)==GC_ML_INVALID);
    while(count<pool->capacity)held[count++]=take_page(r,page);
    uint64_t before=pool->capacity;assert(!pool->free_head&&pool->slots==base);kg_unlock(r);
    /* Allocation must obtain its initialization lease BEFORE publishing an
     * INITIALIZING object; it may grow while this old page stays pinned. */
    kg_alloc_into(8,1,fresh);kg_store_i64(fresh,0,991);assert(kg_load_i64(fresh,0)==991);
    kg_lock(r);assert(pool->capacity>before&&pool->slots==base&&!kg_current_initializing(r));
    kg_unlock(r);
    /* Each controller must clean its own guard/producer before Native growth.
     * Force genuine FULL separately at follow, scalar, store, import, verify. */
    for(unsigned path=0;path<5;path++){
        kg_lock(r);while(count<pool->capacity)held[count++]=take_page(r,page);
        before=pool->capacity;assert(!pool->free_head);kg_unlock(r);
        if(path==0)kg_root_copy(out,leaf);
        else if(path==1)assert(kg_load_i64(out,0)==771);
        else if(path==2)kg_store_ref(record,0,leaf);
        else if(path==3)kg_root_import(out,leaf);
        else kg_verify();
        kg_lock(r);assert(pool->capacity==before+256&&pool->slots==base&&pool->active==count);kg_unlock(r);
    }
    kg_lock(r);
    for(uint64_t i=0;i<count;i++)assert(!gc_metadata_step_locked(pool,&held[i],GC_ML_RELEASE));
    assert(!pool->active&&!page->metadata_leases);
    if(faults){unmap_target=owner->chunks->control.allocation;fail_unmap=1;}
    kg_unlock(r);
    kg_root_release(fresh);kg_root_release(out);kg_root_release(record);kg_root_release(leaf);kg_shutdown();
    assert(!r->lease_backing&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    if(faults)assert(!fail_map&&!fail_commit&&!fail_decommit&&!fail_release&&!fail_unmap);
    puts(faults?"stable lease growth: OS failures preserve active tokens, exact backing charges and final release PASS":
        "stable lease growth: public FULL recovery, 8193 tokens, stable generations and reserved initialization PASS");
}
typedef struct {unsigned count;AllocationPage *page;pthread_barrier_t *ready,*go;} GrowJob;
static void *concurrent_grow(void *argument){
    GrowJob *job=argument;kg_thread_attach();kg_native_enter();
    int status=pthread_barrier_wait(job->ready);assert(!status||status==PTHREAD_BARRIER_SERIAL_THREAD);
    status=pthread_barrier_wait(job->go);assert(!status||status==PTHREAD_BARRIER_SERIAL_THREAD);kg_native_exit();
    for(;;){
        kg_lease_pressure(&kg_runtime);kg_lock(&kg_runtime);
        if(kg_leases(&kg_runtime)->free_head){MetadataLeaseRequest lease=take_page(&kg_runtime,job->page);
            assert(!gc_metadata_step_locked(kg_leases(&kg_runtime),&lease,GC_ML_RELEASE));kg_unlock(&kg_runtime);break;}
        kg_unlock(&kg_runtime);
    }
    kg_thread_detach();return NULL;
}
static void concurrent_owners(unsigned count){
    KgRuntime *r=&kg_runtime;RootCell *leaf=kg_root_new();kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,771);
    pthread_barrier_t ready,go;assert(!pthread_barrier_init(&ready,NULL,count+1));assert(!pthread_barrier_init(&go,NULL,count+1));
    pthread_t threads[8];GrowJob jobs[8];kg_native_enter();
    kg_lock(r);ReferenceLease source={.reference=atomic_load_explicit(&leaf->value,memory_order_acquire)};
    assert(!gc_reference_lease_locked(kg_leases(r),&source));AllocationPage *page=(void *)(uintptr_t)source.lease.page;
    MetadataLeaseRequest held[1024]={0};held[0]=source.lease;source.lease=(MetadataLeaseRequest){0};
    for(unsigned i=1;i<1024;i++)held[i]=take_page(r,page);kg_unlock(r);
    uint64_t base=kg_leases(r)->slots;
    for(unsigned i=0;i<count;i++){jobs[i]=(GrowJob){count,page,&ready,&go};assert(!pthread_create(&threads[i],NULL,concurrent_grow,&jobs[i]));}
    int status=pthread_barrier_wait(&ready);assert(!status||status==PTHREAD_BARRIER_SERIAL_THREAD);
    status=pthread_barrier_wait(&go);assert(!status||status==PTHREAD_BARRIER_SERIAL_THREAD);
    for(unsigned i=0;i<count;i++)assert(!pthread_join(threads[i],NULL));kg_native_exit();
    kg_lock(r);assert(kg_leases(r)->slots==base&&kg_leases(r)->capacity==1280&&kg_leases(r)->active==1024);
    for(unsigned i=0;i<1024;i++)assert(!gc_metadata_step_locked(kg_leases(r),&held[i],GC_ML_RELEASE));
    assert(!kg_leases(r)->active&&!page->metadata_leases);kg_unlock(r);
    assert(!pthread_barrier_destroy(&ready)&&!pthread_barrier_destroy(&go));kg_root_release(leaf);kg_shutdown();
    printf("stable lease growth: %u actual registered growers publish one native tail and return every token PASS\n",count);
}
static pthread_barrier_t prepare_entered,prepare_release;
static void lease_barrier(pthread_barrier_t *barrier){
    int status=pthread_barrier_wait(barrier);assert(!status||status==PTHREAD_BARRIER_SERIAL_THREAD);
}
static bool delayed_commit(void *address,size_t bytes){
    lease_barrier(&prepare_entered);lease_barrier(&prepare_release);
    return real_lease_ops.commit(address,bytes);
}
static void *delayed_control(size_t bytes){
    lease_barrier(&prepare_entered);lease_barrier(&prepare_release);
    return real_metadata_ops.map(bytes);
}
typedef struct {pthread_barrier_t ready,go,finished,cleanup;uint64_t status;} PrepareJob;
static void *prepare_owner(void *argument){
    PrepareJob *job=argument;kg_thread_attach();kg_native_enter();
    lease_barrier(&job->ready);lease_barrier(&job->go);
    job->status=kg_lease_grow_native(&kg_runtime,kg_current_context(&kg_runtime));
    lease_barrier(&job->finished);lease_barrier(&job->cleanup);
    kg_native_exit();kg_thread_detach();return NULL;
}
static void concurrent_prepare(bool budget_limit){
    KgRuntime *r=&kg_runtime;RootCell *leaf=kg_root_new();kg_alloc_into(8,1,leaf);
    PrepareJob job={0};pthread_t thread;
    assert(!pthread_barrier_init(&job.ready,NULL,2)&&!pthread_barrier_init(&job.go,NULL,2)&&
        !pthread_barrier_init(&job.finished,NULL,2)&&!pthread_barrier_init(&job.cleanup,NULL,2)&&
        !pthread_barrier_init(&prepare_entered,NULL,2)&&!pthread_barrier_init(&prepare_release,NULL,2));
    kg_native_enter();assert(!pthread_create(&thread,NULL,prepare_owner,&job));lease_barrier(&job.ready);
    kg_lock(r);MetadataLeasePool *pool=kg_leases(r);KgLeaseBacking *owner=r->lease_backing;
    ReferenceLease source={.reference=atomic_load_explicit(&leaf->value,memory_order_acquire)};
    assert(!gc_reference_lease_locked(pool,&source));AllocationPage *page=(void *)(uintptr_t)source.lease.page;
    MetadataLeaseRequest held[1280]={0};held[0]=source.lease;source.lease=(MetadataLeaseRequest){0};
    for(unsigned i=1;i<1024;i++)held[i]=take_page(r,page);
    uint64_t base=pool->slots,committed=r->native.backing.heap.metadata_committed,maximum=r->native.backing.heap.metadata_max;
    real_lease_ops=owner->ops;real_metadata_ops=r->os.metadata;
    if(budget_limit){
        /* Admit exactly the new control, then deny its real tail charge. */
        r->native.backing.heap.metadata_max=committed+r->os.metadata.quantum;r->os.metadata.map=delayed_control;
    }else owner->ops.commit=delayed_commit;
    kg_unlock(r);lease_barrier(&job.go);lease_barrier(&prepare_entered);
    /* The grower is in an actual Native OS prepare. Existing tokens remain
     * usable; its eventual free-chain tail must use this updated free head. */
    kg_lock(r);assert(owner->growing&&pool->capacity==1024&&pool->slots==base);
    MetadataLeaseRequest stale=held[1023];assert(!gc_metadata_step_locked(pool,&held[1023],GC_ML_RELEASE));
    assert(!gc_metadata_step_locked(pool,&held[1022],GC_ML_RELEASE));held[1022]=take_page(r,page);
    assert(gc_metadata_step_locked(pool,&stale,GC_ML_RESOLVE)==GC_ML_INVALID);
    kg_unlock(r);lease_barrier(&prepare_release);lease_barrier(&job.finished);
    kg_lock(r);assert(job.status==KG_LEASE_OK&&!owner->growing&&pool->slots==base&&pool->active==1023);
    assert(pool->capacity==(budget_limit?1024:1280));
    if(budget_limit)assert(r->native.backing.heap.metadata_committed==committed&&!r->native.backing.heap.metadata_pending);
    r->native.backing.heap.metadata_max=maximum;owner->ops=real_lease_ops;r->os.metadata=real_metadata_ops;
    uint64_t count=1023;while(pool->free_head)held[count++]=take_page(r,page);
    assert(count==pool->capacity&&pool->active==count);
    for(uint64_t i=0;i<count;i++)assert(!gc_metadata_step_locked(pool,&held[i],GC_ML_RELEASE));
    assert(!pool->active&&!page->metadata_leases);kg_unlock(r);
    lease_barrier(&job.cleanup);assert(!pthread_join(thread,NULL));kg_native_exit();
    assert(!pthread_barrier_destroy(&job.ready)&&!pthread_barrier_destroy(&job.go)&&
        !pthread_barrier_destroy(&job.finished)&&!pthread_barrier_destroy(&job.cleanup)&&
        !pthread_barrier_destroy(&prepare_entered)&&!pthread_barrier_destroy(&prepare_release));
    kg_root_release(leaf);kg_shutdown();assert(!r->native.backing.heap.metadata_committed);
    puts(budget_limit?"stable lease growth: real tail budget LIMIT observes concurrent returned token and cleans pending ownership PASS":
        "stable lease growth: OS prepare preserves old generation reuse and links the actual concurrent free head PASS");
}
static void actual_budget_limits(void){
    KgRuntime *r=&kg_runtime;RootCell *leaf=kg_root_new();kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,99);
    kg_lock(r);MetadataLeasePool *pool=kg_leases(r);ReferenceLease source={.reference=atomic_load_explicit(&leaf->value,memory_order_acquire)};
    assert(!gc_reference_lease_locked(pool,&source));AllocationPage *page=(void *)(uintptr_t)source.lease.page;
    MetadataLeaseRequest held[1024];held[0]=source.lease;source.lease=(MetadataLeaseRequest){0};
    for(unsigned i=1;i<1024;i++)held[i]=take_page(r,page);
    MetadataLeaseSlot original[1024];memcpy(original,(void *)(uintptr_t)pool->slots,sizeof(original));
    uint64_t committed=r->native.backing.heap.metadata_committed,maximum=r->native.backing.heap.metadata_max,base=pool->slots;
    kg_unlock(r);kg_native_enter();
    for(unsigned allow_control=0;allow_control<2;allow_control++){
        kg_lock(r);r->native.backing.heap.metadata_max=committed+allow_control*r->os.metadata.quantum;kg_unlock(r);
        assert(kg_lease_grow_native(r,kg_current_context(r))==KG_LEASE_LIMIT);
        kg_lock(r);assert(pool->slots==base&&pool->capacity==1024&&pool->active==1024&&!pool->free_head&&
            !((KgLeaseBacking *)r->lease_backing)->growing&&r->native.backing.heap.metadata_committed==committed&&
            !r->native.backing.heap.metadata_pending&&!memcmp(original,(void *)(uintptr_t)base,sizeof(original)));
        for(unsigned i=0;i<1024;i++){
            MetadataLeaseRequest resolve={.token=held[i].token};assert(!gc_metadata_step_locked(pool,&resolve,GC_ML_RESOLVE));
            assert(resolve.page==held[i].page&&resolve.incarnation==held[i].incarnation);
        }
        r->native.backing.heap.metadata_max=maximum;kg_unlock(r);
    }
    assert(!kg_lease_grow_native(r,kg_current_context(r)));kg_native_exit();
    assert(kg_load_i64(leaf,0)==99);kg_lock(r);
    for(unsigned i=0;i<1024;i++)assert(!gc_metadata_step_locked(pool,&held[i],GC_ML_RELEASE));
    assert(!pool->active&&!page->metadata_leases);kg_unlock(r);kg_root_release(leaf);kg_shutdown();
    assert(!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    puts("stable lease growth: real control and tail budget LIMIT preserve old tokens and permit explicit retry PASS");
}
int main(int argc,char **argv){
    kg_configure(UINT64_C(8)*1024*1024,0,UINT64_C(4)*1024*1024,0);kg_init();
    if(argc>1&&(!strcmp(argv[1],"2")||!strcmp(argv[1],"8")))concurrent_owners((unsigned)strtoul(argv[1],NULL,10));
    else if(argc>1&&(!strcmp(argv[1],"prepare")||!strcmp(argv[1],"limit-race")))concurrent_prepare(!strcmp(argv[1],"limit-race"));
    else if(argc>1&&!strcmp(argv[1],"limits"))actual_budget_limits();
    else actual_full_public_paths(argc>1&&!strcmp(argv[1],"faults"));return 0;
}
