/* #675: real partial-free capacity, multi-lease controllers and budget races. */
static void demand_before_identity(void *,void *);
#define KG_ATOMIC_BEFORE_IDENTITIES_HOOK(r,q) demand_before_identity(r,q)
#include "klassic_gen_runtime.c"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
static MetadataLeaseRequest held[4096];
static uint64_t held_count,initial_capacity,initial_base;
static RootCell *fill_before_identity;
static unsigned spare_at_identity;
static void take_rest(KgRuntime *r,RootCell *root,unsigned spare){
    kg_lock(r);MetadataLeasePool *pool=kg_leases(r);initial_capacity=pool->capacity;initial_base=pool->slots;
    ReferenceLease source={.reference=atomic_load(&root->value)};assert(!gc_reference_lease_locked(pool,&source));held[held_count++]=source.lease;
    for(;;){MetadataLeaseRequest token={.page=source.lease.page,.incarnation=source.lease.incarnation};uint64_t status=gc_metadata_step_locked(pool,&token,GC_ML_ACQUIRE);
        if(status==GC_ML_FULL)break;assert(!status&&held_count<4096);held[held_count++]=token;}
    for(unsigned i=0;i<spare;i++)assert(!gc_metadata_step_locked(pool,&held[--held_count],GC_ML_RELEASE));
    assert(pool->capacity-pool->active==spare);kg_unlock(r);
}
static void release_old(KgRuntime *r){
    kg_lock(r);MetadataLeasePool *pool=kg_leases(r);assert(pool->slots==initial_base);
    for(uint64_t i=0;i<held_count;i++)if(held[i].token){MetadataLeaseRequest token={.token=held[i].token};
        assert(!gc_metadata_step_locked(pool,&token,GC_ML_RESOLVE)&&token.page==held[i].page&&token.incarnation==held[i].incarnation);
        assert(!gc_metadata_step_locked(pool,&held[i],GC_ML_RELEASE));}
    held_count=0;kg_unlock(r);
}
static void demand_before_identity(void *runtime,void *operation){
    (void)operation;if(fill_before_identity){RootCell *root=fill_before_identity;fill_before_identity=NULL;take_rest(runtime,root,spare_at_identity);}
}
static void begin_idle(KgRuntime *r,uint64_t *ownership){kg_native_enter();*ownership=kg_driver_cleanup_enter(r,NULL);assert(!*ownership);kg_native_exit();}
static void end_idle(KgRuntime *r,uint64_t ownership){kg_native_enter();kg_driver_cleanup_leave(r,ownership);kg_native_exit();}
static void finish(void){kg_shutdown();assert(!kg_runtime.native.backing.heap.metadata_committed&&!kg_runtime.native.backing.heap.metadata_pending);}
static void actual_controller(const char *kind,unsigned spare){
    KgRuntime *r=&kg_runtime;RootCell *receiver=kg_root_new(),*a=kg_root_new(),*b=kg_root_new(),*out=kg_root_new();
    kg_alloc_into(8,1,a);kg_store_i64(a,0,17);kg_alloc_into(8,1,b);kg_store_i64(b,0,23);kg_alloc_into(8,2,receiver);kg_store_ref(receiver,0,a);
    uint64_t ownership;begin_idle(r,&ownership);
    if(!strcmp(kind,"cas")){fill_before_identity=receiver;spare_at_identity=spare;
        assert(kg_compare_exchange_ref_into(receiver,0,a,b,out)&&kg_load_i64(out,0)==17);assert(!fill_before_identity);
    }else if(!strcmp(kind,"store")){take_rest(r,receiver,spare);kg_store_ref(receiver,0,b);}
    else{take_rest(r,a,spare);KgPinnedBytes *pin=kg_pin_bytes(a);kg_native_enter();
        assert(kg_pinned_bytes_size(pin)==8&&*(uint64_t *)kg_pinned_bytes_address(pin)==17);kg_native_exit();kg_unpin_bytes(pin);}
    kg_lock(r);assert(kg_leases(r)->capacity>initial_capacity&&kg_leases(r)->slots==initial_base);kg_unlock(r);
    release_old(r);end_idle(r,ownership);
    if(strcmp(kind,"pin")){kg_load_ref_into(receiver,0,out);assert(kg_load_i64(out,0)==23);}
    kg_root_release(out);kg_root_release(b);kg_root_release(a);kg_root_release(receiver);finish();
    printf("#675 actual %s spare%u: demand-aware progress, old tokens and final budgets0 PASS\n",kind,spare);
}
static void native_limits(bool index_limit){
    KgRuntime *r=&kg_runtime;RootCell *root=kg_root_new();kg_alloc_into(8,1,root);uint64_t ownership;begin_idle(r,&ownership);take_rest(r,root,1);kg_native_enter();
    KgLeaseBacking *owner=r->lease_backing;kg_lock(r);uint64_t maximum=r->native.backing.heap.metadata_max,limit=owner->maximum;
    uint64_t committed=r->native.backing.heap.metadata_committed;
    if(index_limit)owner->maximum=kg_leases(r)->capacity;else r->native.backing.heap.metadata_max=committed;kg_unlock(r);
    assert(kg_lease_grow_native_min(r,kg_context(r),5)==KG_LEASE_LIMIT);
    kg_lock(r);assert(kg_leases(r)->capacity==initial_capacity&&kg_leases(r)->capacity-kg_leases(r)->active==1&&
        !owner->growing&&r->native.backing.heap.metadata_committed==committed&&!r->native.backing.heap.metadata_pending);
    /* Real releases meet the complete demand, even though no new backing is
     * permitted. One returned slot must not falsely satisfy a five-slot call. */
    for(unsigned i=0;i<4;i++)assert(!gc_metadata_step_locked(kg_leases(r),&held[--held_count],GC_ML_RELEASE));kg_unlock(r);
    assert(kg_lease_grow_native_min(r,kg_context(r),5)==KG_LEASE_OK);
    kg_lock(r);assert(kg_leases(r)->capacity==initial_capacity);owner->maximum=limit;r->native.backing.heap.metadata_max=maximum;kg_unlock(r);
    kg_native_exit();release_old(r);end_idle(r,ownership);kg_root_release(root);finish();
    puts(index_limit?"#675 actual index LIMIT preserves partial free slots until full demand is returned PASS":"#675 actual budget LIMIT preserves partial free slots until full demand is returned PASS");
}
static void direct_growth(void){
    KgRuntime *r=&kg_runtime;RootCell *root=kg_root_new();kg_alloc_into(8,1,root);uint64_t ownership;begin_idle(r,&ownership);take_rest(r,root,5);kg_native_enter();
    assert(kg_lease_grow_native_min(r,kg_context(r),5)==KG_LEASE_OK);assert(kg_leases(r)->capacity==initial_capacity);
    uint64_t status,steps=0;do{status=kg_lease_grow_native_min(r,kg_context(r),1000);assert(status==KG_LEASE_OK||status==KG_LEASE_WAIT);assert(++steps<8);}while(status==KG_LEASE_WAIT);
    kg_lock(r);assert(kg_leases(r)->capacity-kg_leases(r)->active>=1000&&steps>1);kg_unlock(r);
    kg_native_exit();release_old(r);end_idle(r,ownership);kg_root_release(root);finish();puts("#675 bounded prefix growth: exact-demand no-op and multiple real native prefixes PASS");
}
static pthread_barrier_t prepare_entered,prepare_release;
static NativeMetadataOps original_metadata;
static KgLeaseOps original_lease_ops;
static unsigned pause_commit,fail_commit;
static void barrier(pthread_barrier_t *b){int s=pthread_barrier_wait(b);assert(!s||s==PTHREAD_BARRIER_SERIAL_THREAD);}
static void *delayed_map(size_t bytes){barrier(&prepare_entered);barrier(&prepare_release);return original_metadata.map(bytes);}
static bool delayed_commit(void *address,size_t bytes){
    if(pause_commit){pause_commit=0;barrier(&prepare_entered);barrier(&prepare_release);}
    bool done=original_lease_ops.commit(address,bytes);
    if(done&&fail_commit){fail_commit--;return false;}return done;
}
typedef struct {pthread_barrier_t ready,go,finished,cleanup;uint64_t status;} DemandJob;
static void *demand_grower(void *argument){
    DemandJob *job=argument;kg_thread_attach();kg_native_enter();barrier(&job->ready);barrier(&job->go);
    job->status=kg_lease_grow_native_min(&kg_runtime,kg_context(&kg_runtime),5);
    barrier(&job->finished);barrier(&job->cleanup);kg_native_exit();kg_thread_detach();return NULL;
}
static void concurrent_release(unsigned released,bool budget_limit){
    KgRuntime *r=&kg_runtime;RootCell *root=kg_root_new();kg_alloc_into(8,1,root);uint64_t ownership;begin_idle(r,&ownership);kg_native_enter();
    DemandJob job={0};pthread_t thread;assert(!pthread_barrier_init(&job.ready,NULL,2)&&!pthread_barrier_init(&job.go,NULL,2)&&
        !pthread_barrier_init(&job.finished,NULL,2)&&!pthread_barrier_init(&job.cleanup,NULL,2)&&
        !pthread_barrier_init(&prepare_entered,NULL,2)&&!pthread_barrier_init(&prepare_release,NULL,2));
    assert(!pthread_create(&thread,NULL,demand_grower,&job));barrier(&job.ready);take_rest(r,root,1);
    KgLeaseBacking *owner=r->lease_backing;kg_lock(r);uint64_t maximum=r->native.backing.heap.metadata_max,committed=r->native.backing.heap.metadata_committed;
    original_metadata=r->os.metadata;original_lease_ops=owner->ops;
    if(budget_limit){r->native.backing.heap.metadata_max=committed+r->os.metadata.quantum;r->os.metadata.map=delayed_map;}
    else{pause_commit=fail_commit=1;owner->ops.commit=delayed_commit;}kg_unlock(r);
    barrier(&job.go);barrier(&prepare_entered);kg_lock(r);assert(owner->growing&&kg_leases(r)->capacity==initial_capacity);
    for(unsigned i=0;i<released;i++)assert(!gc_metadata_step_locked(kg_leases(r),&held[--held_count],GC_ML_RELEASE));kg_unlock(r);
    barrier(&prepare_release);barrier(&job.finished);kg_lock(r);
    assert(job.status==(budget_limit&&released<4?KG_LEASE_LIMIT:KG_LEASE_OK)&&!owner->growing&&!r->native.backing.heap.metadata_pending);
    if(budget_limit)assert(kg_leases(r)->capacity==initial_capacity&&r->native.backing.heap.metadata_committed==committed&&kg_leases(r)->capacity-kg_leases(r)->active==released+1);
    else assert(kg_leases(r)->capacity>initial_capacity&&!fail_commit);
    r->native.backing.heap.metadata_max=maximum;r->os.metadata=original_metadata;owner->ops=original_lease_ops;kg_unlock(r);
    release_old(r);kg_driver_cleanup_leave(r,ownership);barrier(&job.cleanup);assert(!pthread_join(thread,NULL));kg_native_exit();
    assert(!pthread_barrier_destroy(&job.ready)&&!pthread_barrier_destroy(&job.go)&&!pthread_barrier_destroy(&job.finished)&&!pthread_barrier_destroy(&job.cleanup)&&
        !pthread_barrier_destroy(&prepare_entered)&&!pthread_barrier_destroy(&prepare_release));kg_root_release(root);finish();
    printf("#675 actual Native prepare/concurrent return%u: %s and unique backing cleanup PASS\n",released,budget_limit?"budget minimum recheck":"post-effect commit retry");
}
int main(int argc,char **argv){
    assert(argc==2);kg_configure(32*1024*1024,0,16*1024*1024,0);kg_init();
    if(!strncmp(argv[1],"cas",3))actual_controller("cas",(unsigned)atoi(argv[1]+3));
    else if(!strncmp(argv[1],"store",5))actual_controller("store",(unsigned)atoi(argv[1]+5));
    else if(!strcmp(argv[1],"pin1"))actual_controller("pin",1);
    else if(!strcmp(argv[1],"budget"))native_limits(false);else if(!strcmp(argv[1],"index"))native_limits(true);
    else if(!strcmp(argv[1],"prefix"))direct_growth();else if(!strcmp(argv[1],"return3"))concurrent_release(3,true);
    else if(!strcmp(argv[1],"return4"))concurrent_release(4,true);else if(!strcmp(argv[1],"prepare"))concurrent_release(2,false);
    else abort();return 0;
}
