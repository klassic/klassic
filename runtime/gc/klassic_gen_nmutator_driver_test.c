/* Real N-owner driver regression. The fixture uses actual runtime registration,
 * roots, barriers, pressure and Native transitions; no phase/liveness/retirement
 * receipt is fabricated. Native metadata pins intentionally race real detach. */
#include <stdatomic.h>
#ifndef _WIN32
#include <pthread.h>
static _Atomic unsigned test_mutex_destroyed,test_late_lock_calls;
static _Atomic unsigned test_prelock_entered,test_prelock_release;
static _Thread_local unsigned test_prelock_subject;
static int test_mutex_lock(pthread_mutex_t *mutex);
static int test_mutex_destroy(pthread_mutex_t *mutex);
static void test_attach_prelock(void *runtime);
#define pthread_mutex_lock test_mutex_lock
#define pthread_mutex_destroy test_mutex_destroy
#define KG_ATTACH_PRELOCK_HOOK(runtime) test_attach_prelock(runtime)
#endif
#include "klassic_gen_runtime.c"
#ifndef _WIN32
#undef pthread_mutex_lock
#undef pthread_mutex_destroy
#endif
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
#include <stdatomic.h>
#ifndef _WIN32
#include <pthread.h>
#include <sched.h>

static int test_mutex_lock(pthread_mutex_t *mutex){
    if(mutex==&kg_runtime.lock&&atomic_load_explicit(&test_mutex_destroyed,memory_order_acquire))
        atomic_fetch_add_explicit(&test_late_lock_calls,1,memory_order_relaxed);
    return pthread_mutex_lock(mutex);
}
static int test_mutex_destroy(pthread_mutex_t *mutex){
    int status=pthread_mutex_destroy(mutex);
    if(mutex==&kg_runtime.lock){assert(!status);atomic_fetch_add_explicit(&test_mutex_destroyed,1,memory_order_release);}
    return status;
}
static void test_attach_prelock(void *runtime){
    assert(runtime==&kg_runtime);
    if(!test_prelock_subject)return;
    atomic_store_explicit(&test_prelock_entered,1,memory_order_release);
    while(!atomic_load_explicit(&test_prelock_release,memory_order_acquire))sched_yield();
}
typedef struct {
    unsigned index,rounds;
    _Atomic unsigned *ready,*go;
    uint64_t identity;
} WorkerInput;
static void *pressure_owner(void *raw){
    WorkerInput *input=raw;kg_thread_attach();
    MutatorContext *ctx=kg_current_context(&kg_runtime);assert(ctx);
    input->identity=ctx->thread_id;
    RootCell *number=kg_root_new(),*alias=kg_root_new(),*record=kg_root_new();
    RootCell *observed=kg_root_new(),*scratch=kg_root_new(),*empty=kg_root_new();
    uint64_t value=UINT64_C(0xcafe000000000000)+input->index;
    kg_alloc_into(8,1,number);kg_store_i64(number,0,value);kg_root_copy(alias,number);
    kg_alloc_into(16,2,record);kg_store_ref(record,0,number);kg_store_ref(record,8,number);
    kg_root_clear(number);
    kg_native_enter();atomic_fetch_add_explicit(input->ready,1,memory_order_release);
    while(!atomic_load_explicit(input->go,memory_order_acquire))sched_yield();
    kg_native_exit();
    for(unsigned i=0;i<input->rounds;i++){
        kg_alloc_into(128,1,scratch);kg_store_i64(scratch,0,i);
        if(!(i%7)){
            /* After promotion, delete/restore an Old->Young slot through this
             * owner's real SATB/remset buffers, including the previous value. */
            kg_alloc_into(8,1,number);kg_store_i64(number,0,value+i);
            kg_store_ref(record,8,number);kg_store_ref(record,8,empty);
            kg_store_ref(record,8,number);kg_root_clear(number);
        }
        if(!(i%17)){
            kg_driver_pressure(&kg_runtime,KG_PRESSURE_METADATA);
            kg_store_i64(alias,0,value^i);
            kg_load_ref_into(record,0,observed);
            assert(kg_load_i64(observed,0)==(value^i));
            kg_store_i64(alias,0,value);kg_root_clear(observed);
        }
        if(!(i%11)){kg_native_enter();kg_native_exit();}
        kg_root_clear(scratch);kg_poll();
    }
    kg_load_ref_into(record,0,observed);assert(kg_load_i64(observed,0)==value);
    kg_root_release(empty);kg_root_release(scratch);kg_root_release(observed);
    kg_root_release(record);kg_root_release(alias);kg_root_release(number);
    kg_thread_detach();return NULL;
}
static void pressure(unsigned count,unsigned rounds){
    pthread_t workers[8];WorkerInput inputs[8];assert(count&&count<=8);
    _Atomic unsigned ready=0,go=0;
    kg_native_enter();
    for(unsigned i=0;i<count;i++){
        inputs[i]=(WorkerInput){i,rounds,&ready,&go,0};
        assert(!pthread_create(&workers[i],NULL,pressure_owner,&inputs[i]));
    }
    while(atomic_load_explicit(&ready,memory_order_acquire)!=count)sched_yield();
    atomic_store_explicit(&go,1,memory_order_release);
    for(unsigned i=0;i<count;i++)assert(!pthread_join(workers[i],NULL));
    kg_native_exit();
    for(unsigned i=0;i<count;i++){
        assert(inputs[i].identity&&inputs[i].identity!=kg_current_context(&kg_runtime)->thread_id);
        for(unsigned j=0;j<i;j++)assert(inputs[i].identity!=inputs[j].identity);
    }
    kg_lock(&kg_runtime);assert(kg_runtime.native.handshake.members==1);
    assert(kg_runtime.native.cycles.completed[0]&&kg_runtime.native.cycles.completed[1]);kg_unlock(&kg_runtime);
}
typedef struct {_Atomic unsigned ready,go,entered,done;uint64_t identity;} DetachInput;
static void *detach_owner(void *raw){
    DetachInput *input=raw;kg_thread_attach();input->identity=kg_current_context(&kg_runtime)->thread_id;
    kg_native_enter();atomic_store_explicit(&input->ready,1,memory_order_release);
    while(!atomic_load_explicit(&input->go,memory_order_acquire))sched_yield();
    kg_native_exit();atomic_store_explicit(&input->entered,1,memory_order_release);
    kg_thread_detach();atomic_store_explicit(&input->done,1,memory_order_release);return NULL;
}
static void pinned_detach(void){
    pthread_t worker;DetachInput input={0};kg_native_enter();
    assert(!pthread_create(&worker,NULL,detach_owner,&input));
    while(!atomic_load_explicit(&input.ready,memory_order_acquire))sched_yield();
    kg_lock(&kg_runtime);MutatorContext *ctx=kg_context_acquire_after_locked(&kg_runtime,0);
    assert(ctx&&ctx->thread_id==input.identity);kg_unlock(&kg_runtime);
    atomic_store_explicit(&input.go,1,memory_order_release);
    while(!atomic_load_explicit(&input.entered,memory_order_acquire))sched_yield();
    /* The actual owner has begun detach. Its final unregister/storage cleanup
     * must remain excluded while a real driver package pin retains it. */
    for(unsigned i=0;i<10000;i++)sched_yield();
    assert(!atomic_load_explicit(&input.done,memory_order_acquire));
    kg_lock(&kg_runtime);assert(ctx->thread_id==input.identity);
    assert(kg_runtime.native.handshake.members==2);kg_context_release_locked(&kg_runtime,ctx);kg_unlock(&kg_runtime);
    assert(!pthread_join(worker,NULL));assert(atomic_load_explicit(&input.done,memory_order_acquire));
    kg_native_exit();
}
static void actual_quantum(KgRuntime *r,KgDriver *d){
    kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);
    assert(!atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire));
}
static void publication_retirement(void){
    KgRuntime *r=&kg_runtime;
    RootCell *number=kg_root_new(),*record=kg_root_new(),*observed=kg_root_new();
    kg_alloc_into(8,1,number);kg_store_i64(number,0,4242);
    kg_alloc_into(8,2,record);kg_store_ref(record,0,number);kg_root_clear(number);
    kg_enter_native(r);uint64_t borrowed=kg_driver_cleanup_enter(r,NULL);assert(!borrowed);
    KgDriver *d=r->driver;kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);
    uint64_t turns=0;
    /* Stop after the actual owner was inspected Native, immediately before
     * the next bounded buffer NEXT would consume final absence. */
    while(d->state!=12||!d->retirement.self||!d->retirement.buffers.self||
          d->retirement.buffers.remaining||d->retirement.buffers.stage!=1){
        assert(++turns<UINT64_C(2000000));actual_quantum(r,d);
    }
    KgRun *source=d->cursor;assert(source&&source->native.stage==8&&!source->native.retirement.stage);
    assert(!r->native.handshake.active);
    kgd_native(d,true);kg_leave_native(r);
    kg_root_publication_begin(r);
    RootedLoad q={.observation={.registry=(uintptr_t)kg_descriptors(r),.byte_offset=0}};
    kg_lock(r);q.observation.receiver.follow.context=(uintptr_t)kg_current_context(r);
    q.observation.receiver.follow.root=kg_root_request_locked(r,record);
    q.observation.output=kg_root_request_locked(r,observed);kg_unlock(r);
    RootedLoadAdmission admission;RootedStoreRuntime binding=kg_dispatch(r,&admission);
    assert(gc_rooted_load_dispatch(&binding,&q,GC_RLD_STEP)==GC_RLOAD_HEAP);
    assert(q.admitted&&!atomic_load_explicit(&kg_current_context(r)->access_epoch,memory_order_acquire));
    kg_lock(r);assert(kg_root_publications_locked(r)==1);
    assert(d->retirement_root_revision==kg_root_revision_locked(r));kg_unlock(r);
    actual_quantum(r,d);
    assert(source->native.stage==8&&!source->native.retirement.stage&&d->retirement.self);
    /* Complete a REAL outside-lock guarded field observation. Its exact root
     * publication changes the shared revision, invalidating the saved audit. */
    kgd_native(d,true);
    for(;;){
        uint64_t status=gc_rooted_load_dispatch(&binding,&q,GC_RLD_STEP);
        if(status==GC_RLOAD_DONE)break;
        assert(status!=GC_RLOAD_INVALID&&status!=GC_RLOAD_ERROR&&status!=GC_RLOAD_EXHAUSTED&&
            status!=GC_RLOAD_NULL_RECEIVER&&status!=GC_RLOAD_CANCELLED);
        assert(++turns<UINT64_C(2000000));
    }
    kg_root_publication_end(r);kg_lock(r);
    assert(!kg_root_publications_locked(r)&&d->retirement_root_revision!=kg_root_revision_locked(r));kg_unlock(r);
    kg_enter_native(r);actual_quantum(r,d);
    assert(!d->retirement.self&&!source->native.retirement.stage);
    while(d->state||d->background_old||d->requested[0]||d->requested[1]){
        assert(++turns<UINT64_C(2000000));actual_quantum(r,d);
    }
    kgd_native(d,true);kg_driver_cleanup_leave(r,borrowed);kg_leave_native(r);
    assert(kg_load_i64(observed,0)==4242);
    kg_root_release(observed);kg_root_release(record);kg_root_release(number);
}
typedef struct {
    _Atomic unsigned ready,start,admitted,observe,completed,finish;
    uint64_t result;
} PeerPublication;
static void *publication_peer(void *raw){
    PeerPublication *input=raw;kg_thread_attach();KgRuntime *r=&kg_runtime;
    RootCell *number=kg_root_new(),*record=kg_root_new(),*observed=kg_root_new();
    kg_alloc_into(8,1,number);kg_store_i64(number,0,8484);
    kg_alloc_into(8,2,record);kg_store_ref(record,0,number);kg_root_clear(number);
    kg_enter_native(r);atomic_store_explicit(&input->ready,1,memory_order_release);
    while(!atomic_load_explicit(&input->start,memory_order_acquire))sched_yield();
    kg_leave_native(r);kg_root_publication_begin(r);
    RootedLoad q={.observation={.registry=(uintptr_t)kg_descriptors(r),.byte_offset=0}};
    kg_lock(r);q.observation.receiver.follow.context=(uintptr_t)kg_current_context(r);
    q.observation.receiver.follow.root=kg_root_request_locked(r,record);
    q.observation.output=kg_root_request_locked(r,observed);kg_unlock(r);
    RootedLoadAdmission admission;RootedStoreRuntime binding=kg_dispatch(r,&admission);
    assert(gc_rooted_load_dispatch(&binding,&q,GC_RLD_STEP)==GC_RLOAD_HEAP);
    assert(q.admitted&&!atomic_load_explicit(&kg_current_context(r)->access_epoch,memory_order_acquire));
    atomic_store_explicit(&input->admitted,1,memory_order_release);
    /* A deterministic pause between actual admission and ACCESS, without any
     * raw payload guard or unprotected value. Owner-private work stays owned. */
    while(!atomic_load_explicit(&input->observe,memory_order_acquire))sched_yield();
    uint64_t steps=0;
    for(;;){
        uint64_t status=gc_rooted_load_dispatch(&binding,&q,GC_RLD_STEP);
        if(status==GC_RLOAD_DONE)break;
        assert(status!=GC_RLOAD_INVALID&&status!=GC_RLOAD_ERROR&&status!=GC_RLOAD_EXHAUSTED&&
            status!=GC_RLOAD_NULL_RECEIVER&&status!=GC_RLOAD_CANCELLED);
        assert(++steps<UINT64_C(2000000));
    }
    kg_root_publication_end(r);kg_enter_native(r);
    atomic_store_explicit(&input->completed,1,memory_order_release);
    while(!atomic_load_explicit(&input->finish,memory_order_acquire))sched_yield();
    kg_leave_native(r);input->result=kg_load_i64(observed,0);
    kg_root_release(observed);kg_root_release(record);kg_root_release(number);kg_thread_detach();return NULL;
}
static void peer_publication_retirement(void){
    KgRuntime *r=&kg_runtime;pthread_t peer;PeerPublication input={0};
    kg_enter_native(r);assert(!pthread_create(&peer,NULL,publication_peer,&input));
    while(!atomic_load_explicit(&input.ready,memory_order_acquire))sched_yield();
    uint64_t borrowed=kg_driver_cleanup_enter(r,NULL);assert(!borrowed);KgDriver *d=r->driver;
    kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);uint64_t turns=0;
    while(d->state!=12||!d->retirement.self||!d->retirement.buffers.self||
          d->retirement.buffers.remaining||d->retirement.buffers.stage!=1){
        assert(++turns<UINT64_C(2000000));actual_quantum(r,d);
    }
    KgRun *source=d->cursor;assert(source&&source->native.stage==8&&!source->native.retirement.stage);
    assert(!r->native.handshake.active);
    uint64_t primary_revision=atomic_load_explicit(&r->native.context.root_revision,memory_order_acquire);
    kgd_native(d,true);atomic_store_explicit(&input.start,1,memory_order_release);
    while(!atomic_load_explicit(&input.admitted,memory_order_acquire))sched_yield();
    kg_lock(r);assert(kg_root_publications_locked(r)==1);
    assert(d->retirement_root_revision==kg_root_revision_locked(r));kg_unlock(r);
    actual_quantum(r,d);assert(source->native.stage==8&&!source->native.retirement.stage&&d->retirement.self);
    kgd_native(d,true);atomic_store_explicit(&input.observe,1,memory_order_release);
    while(!atomic_load_explicit(&input.completed,memory_order_acquire))sched_yield();
    kg_lock(r);assert(!kg_root_publications_locked(r)&&d->retirement_root_revision!=kg_root_revision_locked(r));
    assert(primary_revision==atomic_load_explicit(&r->native.context.root_revision,memory_order_acquire));kg_unlock(r);
    actual_quantum(r,d);assert(!d->retirement.self&&!source->native.retirement.stage);
    while(d->state||d->background_old||d->requested[0]||d->requested[1]){
        assert(++turns<UINT64_C(2000000));actual_quantum(r,d);
    }
    kgd_native(d,true);kg_driver_cleanup_leave(r,borrowed);
    atomic_store_explicit(&input.finish,1,memory_order_release);
    assert(!pthread_join(peer,NULL));assert(input.result==8484);kg_leave_native(r);
}
static _Atomic unsigned prelock_rejected;
static void *prelock_attach_owner(void *unused){
    (void)unused;test_prelock_subject=1;
    assert(!kg_try_thread_attach());assert(!kg_current_context(&kg_runtime));
    atomic_store_explicit(&prelock_rejected,1,memory_order_release);return NULL;
}
static void *prelock_releaser(void *unused){
    (void)unused;
    while(!(atomic_load_explicit(&kg_admission_gate,memory_order_acquire)&1))sched_yield();
    /* This is the real CAS-acquired pre-lock reader, not a manufactured count.
     * Shutdown must retain its actual mutex until this observer releases it. */
    assert(atomic_load_explicit(&kg_admission_gate,memory_order_acquire)==3);
    assert(!atomic_load_explicit(&test_mutex_destroyed,memory_order_acquire));
    atomic_store_explicit(&test_prelock_release,1,memory_order_release);return NULL;
}
static void shutdown_prelock_race(void){
    pthread_t attaching,releaser;
    assert(!pthread_create(&attaching,NULL,prelock_attach_owner,NULL));
    while(!atomic_load_explicit(&test_prelock_entered,memory_order_acquire))sched_yield();
    assert(atomic_load_explicit(&kg_admission_gate,memory_order_acquire)==2);
    assert(!pthread_create(&releaser,NULL,prelock_releaser,NULL));
    kg_shutdown();assert(!pthread_join(attaching,NULL));assert(!pthread_join(releaser,NULL));
    assert(atomic_load_explicit(&prelock_rejected,memory_order_acquire));
    assert(atomic_load_explicit(&test_mutex_destroyed,memory_order_acquire)==1);
    assert(!kg_try_thread_attach());
    assert(!pthread_create(&attaching,NULL,prelock_attach_owner,NULL));
    assert(!pthread_join(attaching,NULL));
    assert(!atomic_load_explicit(&test_late_lock_calls,memory_order_acquire));
}
static void graph_admission_rendezvous(void){
    KgRuntime *r=&kg_runtime;RootCell *number=kg_root_new();
    kg_alloc_into(8,1,number);kg_store_i64(number,0,9191);
    kg_enter_native(r);uint64_t borrowed=kg_driver_cleanup_enter(r,NULL);assert(!borrowed);KgDriver *d=r->driver;
    kgd_native(d,true);kg_leave_native(r);
    kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);actual_quantum(r,d);
    /* Actual MarkStart publication waits for this Running owner's own ACK.
     * The worker may inspect metadata, but cannot complete it on our behalf. */
    kg_lock(r);assert(r->native.handshake.active&&r->native.handshake.expected==1);kg_unlock(r);
    kgd_native(d,true);kg_driver_cleanup_leave(r,borrowed);
    for(unsigned i=0;i<1024;i++){assert(!kgd_acquire_for(d,true));kg_yield();}
    uint64_t turns=0;while(!kgd_acquire(d)){assert(++turns<UINT64_C(2000000));kg_yield();}
    kg_lock(r);assert(r->native.handshake.active);kg_unlock(r);kgd_leave(d);
    kg_driver_pressure(r,KG_PRESSURE_METADATA);assert(kg_load_i64(number,0)==9191);kg_root_release(number);
}
static void fill_lease_prefix(KgRuntime *r,AllocationPage *page,MetadataLeaseRequest *held,uint64_t *count){
    kg_lock(r);MetadataLeasePool *pool=kg_leases(r);
    while(pool->free_head){
        assert(*count<2048);held[*count]=(MetadataLeaseRequest){.page=(uintptr_t)page,.incarnation=page->incarnation};
        assert(gc_metadata_step_locked(pool,&held[*count],GC_ML_ACQUIRE)==GC_ML_OK);(*count)++;
    }
    assert(pool->active==pool->capacity);kg_unlock(r);
}
static void worker_lease_growth(void){
    KgRuntime *r=&kg_runtime;RootCell *number=kg_root_new(),*record=kg_root_new(),*observed=kg_root_new();
    kg_alloc_into(8,1,number);kg_store_i64(number,0,8877);kg_alloc_into(8,2,record);kg_store_ref(record,0,number);
    kg_enter_native(r);uint64_t ownership=kg_driver_cleanup_enter(r,NULL);assert(!ownership);KgDriver *d=r->driver;
    kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);uint64_t steps=0;
    while(d->state!=3){assert(++steps<UINT64_C(2000000));actual_quantum(r,d);}
    NativeWorkStorage *work=gc_native_runtime_work(&r->native);
    while(!work->queues[GC_YOUNG].queued){
        assert(++steps<UINT64_C(2000000));kg_lock(r);kgd_adopt(d);kg_unlock(r);(void)kgd_roots(d,false);
    }
    kg_lock(r);uint64_t reference=atomic_load_explicit(&record->value,memory_order_acquire);
    uint64_t index=(reference>>16)>>GC_HEAP_PAGE_SHIFT;
    AllocationPage *pages=(void *)(uintptr_t)r->native.backing.heap.pages;
    AllocationPage *page=&pages[pages[index-1].run_head];
    uint64_t base=kg_leases(r)->slots,capacity=kg_leases(r)->capacity;kg_unlock(r);
    MetadataLeaseRequest held[2048]={0};uint64_t count=0;fill_lease_prefix(r,page,held,&count);
    /* Queue admission was real RootAudit/NativeRootMark work. Its claim owns
     * the original token; the separate canonical receiver needs a new token
     * and must retain the registered scan while growing from actual FULL. */
    kgd_scan(d,GC_YOUNG);kg_lock(r);
    assert(kg_leases(r)->slots==base&&kg_leases(r)->capacity>capacity&&!r->native.mark_scan_count);
    capacity=kg_leases(r)->capacity;kg_unlock(r);
    fill_lease_prefix(r,page,held,&count);
    kgd_reference(d,reference,r->native.roots.phase);kg_lock(r);
    assert(kg_leases(r)->slots==base&&kg_leases(r)->capacity>capacity);
    for(uint64_t i=0;i<count;i++){
        uint64_t token=held[i].token;assert(gc_metadata_step_locked(kg_leases(r),&held[i],GC_ML_RESOLVE)==GC_ML_OK);
        assert(held[i].token==token&&held[i].page==(uintptr_t)page&&held[i].incarnation==page->incarnation);
        assert(gc_metadata_step_locked(kg_leases(r),&held[i],GC_ML_RELEASE)==GC_ML_OK);
    }
    kg_unlock(r);
    while(d->state||d->background_old||d->requested[0]||d->requested[1]){
        assert(++steps<UINT64_C(2000000));actual_quantum(r,d);
    }
    kgd_native(d,true);kg_driver_cleanup_leave(r,ownership);kg_leave_native(r);
    kg_load_ref_into(record,0,observed);assert(kg_load_i64(observed,0)==8877);
    kg_root_release(observed);kg_root_release(record);kg_root_release(number);
}
int main(int argc,char **argv){
    unsigned count=argc>1?(unsigned)strtoul(argv[1],NULL,10):2;
    bool pin=argc>2&&!strcmp(argv[2],"pin");
    bool publication=argc>2&&!strcmp(argv[2],"publication");
    bool peer_publication=argc>2&&!strcmp(argv[2],"peer-publication");
    bool prelock=argc>2&&!strcmp(argv[2],"prelock");
    bool rendezvous=argc>2&&!strcmp(argv[2],"rendezvous");
    bool lease_worker=argc>2&&!strcmp(argv[2],"lease-worker");
    kg_configure(UINT64_C(16)*1024*1024,0,UINT64_C(8)*1024*1024,0);kg_init();
    if(prelock){shutdown_prelock_race();puts("N-mutator driver: actual prelock admission survives shutdown and rejects late mutex access PASS");return 0;}
    if(lease_worker)worker_lease_growth();
    else if(rendezvous)graph_admission_rendezvous();
    else if(pin)pinned_detach();else if(publication)publication_retirement();
    else if(peer_publication)peer_publication_retirement();else pressure(count,80);
    kg_verify();kg_shutdown();
    puts(lease_worker?"N-mutator driver: actual pending mark and registered scan receiver retain work through lease growth PASS":
        rendezvous?"N-mutator driver: actual rendezvous excludes graph work and permits metadata assistance PASS":
        peer_publication?"N-mutator driver: actual secondary root publication restarts retirement with primary revision unchanged PASS":
        publication?"N-mutator driver: actual rooted publication excludes early retirement and restarts incoming proof PASS":
        pin?"N-mutator driver: actual pinned context retains registration and rings through detach PASS":
        "N-mutator driver: actual owner buffers, moving aliases, pressure, Native handoff and shutdown PASS");return 0;
}
#else
int main(void){return 77;}
#endif
