#ifndef KLASSIC_GC_ROOTED_STORE_DISPATCH_CASES_H
#define KLASSIC_GC_ROOTED_STORE_DISPATCH_CASES_H
#include "gc_rooted_store_dispatch.h"
#include "gc_rooted_store_cases.h"
typedef struct {
#ifdef _WIN32
    CRITICAL_SECTION mutex;
#else
    pthread_mutex_t mutex;
#endif
    unsigned held,acquired,released;
} StoreNativeMutex;
static StoreNativeMutex store_native_locks[4];
static RootedStoreDispatcher store_native_dispatch;
static MutatorContext *store_native_context;
static unsigned store_native_held,store_native_accesses;
static void store_native_lock(uintptr_t address){
    StoreNativeMutex *lock=(void *)address;
    assert(!store_native_held&&!atomic_load(&store_native_context->access_epoch));
#ifdef _WIN32
    EnterCriticalSection(&lock->mutex);
#else
    assert(!pthread_mutex_lock(&lock->mutex));
#endif
    assert(!lock->held);lock->held=1;lock->acquired++;store_native_held=1;
}
static void store_native_unlock(uintptr_t address){
    StoreNativeMutex *lock=(void *)address;
    assert(store_native_held&&lock->held);lock->held=0;lock->released++;store_native_held=0;
#ifdef _WIN32
    LeaveCriticalSection(&lock->mutex);
#else
    assert(!pthread_mutex_unlock(&lock->mutex));
#endif
}
static RootedStoreRuntime store_native_runtime(MetadataLeasePool *pool,const RootedLoadAdmission *admission){
    return (RootedStoreRuntime){(uintptr_t)pool,(uintptr_t)admission,(uintptr_t)&store_native_locks[0],(uintptr_t)&store_native_locks[1],
        {(uintptr_t)&store_native_locks[2],(uintptr_t)&store_native_locks[3]},(uintptr_t)store_native_lock,(uintptr_t)store_native_unlock};
}
/* Adapt the existing semantic schedules. They deliberately supply their own
 * admission, while the additional cases below cover automatic admission. */
static uint64_t store_native_adapter(MetadataLeasePool *pool,RootedStore *q,uint64_t op){
    uint64_t action=gc_rooted_store_next(q);
    if(action==GC_RLOAD_INVALID)return action;
    if(op>=GC_RLOAD_HEAP&&op<=GC_RLOAD_QUEUE&&action!=op&&q->stage<GC_RS_COMPLETE)return GC_RLOAD_INVALID;
    store_native_context=(void *)(uintptr_t)q->value.observation.receiver.follow.context;
    RootedStoreRuntime runtime=store_native_runtime(pool,NULL);
    unsigned before=0;for(unsigned i=0;i<4;i++)before+=store_native_locks[i].acquired;
    uint64_t r=store_native_dispatch(&runtime,q,op==GC_RLOAD_CANCEL?GC_RSD_CANCEL:op==GC_RLOAD_REARM?GC_RSD_REARM:GC_RSD_STEP);
    unsigned after=0;for(unsigned i=0;i<4;i++)after+=store_native_locks[i].acquired;
    assert(!store_native_held);
    if(op==GC_RLOAD_ACCESS){assert(before==after);store_native_accesses++;}
    return r;
}
static void gc_run_rooted_store_dispatch_cases(RootedStoreDispatcher candidate){
    memset(store_native_locks,0,sizeof(store_native_locks));store_native_held=store_native_accesses=0;
    for(unsigned i=0;i<4;i++){
#ifdef _WIN32
        InitializeCriticalSection(&store_native_locks[i].mutex);
#else
        assert(!pthread_mutex_init(&store_native_locks[i].mutex,NULL));
#endif
    }
    store_native_dispatch=candidate;
    gc_run_rooted_store_cases(store_native_adapter);
    for(unsigned impl=0;impl<2;impl++)for(unsigned fault=0;fault<11;fault++){
        RootedStoreDispatcher dispatch=impl?candidate:gc_rooted_store_dispatch;
        FollowFixture f;follow_fixture(&f,0x10000,1);store_native_context=&f.ctx;
        RootCell cells[2]={{.value=atomic_load(&f.cell.value),.reuse_generation=1,.active=1},{.reuse_generation=1,.active=1}};
        f.segment.cells=(uintptr_t)cells;f.segment.capacity=2;f.segment.used=2;f.q.root.handle.cell=(uintptr_t)cells;
        RootRequest observed=f.q.root;observed.handle.cell=(uintptr_t)&cells[1];
        RootedStore q={.value.observation={.receiver.follow=f.q,.output=f.q.root},.observation={.receiver.follow=f.q,.output=observed}};
        HandshakeState hs={.members=1,.last_identity=17};ThreadRegistration participant={.context=(uintptr_t)&f.ctx,.identity=17,.registered=1,.registry=(uintptr_t)&hs};hs.head=(uintptr_t)&participant;
        RootRegistry registry={.phase=0x10000};RootedLoadAdmission admission={(uintptr_t)&registry,(uintptr_t)&hs,(uintptr_t)&participant,{0,0}};
        RootedStoreRuntime runtime=store_native_runtime(&f.pool,&admission);
        uint64_t expected=GC_RLOAD_INVALID;
        if(!fault)expected=GC_RLOAD_HEAP;
        else if(fault==1){assert(gc_handshake_step_locked(&hs,NULL,GC_HS_BEGIN)==GC_HS_OK);expected=GC_RLOAD_WAIT;}
        else if(fault==2)runtime.registry_lock=0;
        else if(fault==3)runtime.lock=0;
        else if(fault==4)runtime.unlock=0;
        else if(fault==5)runtime.admission=0;
        else if(fault==6)atomic_store(&f.ctx.access_epoch,27);
        else if(fault==7)atomic_store(&f.ctx.state,GC_AT_SAFEPOINT);
        else if(fault==8){atomic_store(&f.ctx.private_producers,UINT64_MAX);expected=GC_RLOAD_EXHAUSTED;}
        else if(fault==9)participant.context=0;
        else{
            q.value.stage=GC_RLOAD_RECAPTURE;q.value.observation.stage=4;
            q.value.admitted=q.admitted=1;atomic_store(&f.ctx.private_producers,2);
            q.value.marks.phase=UINT64_MAX;expected=GC_RLOAD_HEAP;
        }
        RootedStore saved=q;unsigned locks=store_native_locks[1].acquired;
        assert(dispatch(&runtime,&q,GC_RSD_STEP)==expected&&!store_native_held);
        bool invoked=fault==0||fault==1||fault==8||fault==9||fault==10;
        assert(store_native_locks[1].acquired==locks+(unsigned)invoked);
        if(!fault||fault==10){
            assert(q.value.admitted==1&&q.admitted==(fault==10)&&q.value.marks.phase==registry.phase&&atomic_load(&f.ctx.private_producers)==(fault==10?2u:1u));
            assert(q.value.stage==(fault==10?GC_RLOAD_MARK_INIT:GC_RLOAD_OBSERVING));
            unsigned before=store_native_locks[0].acquired;
            assert(dispatch(&runtime,&q,GC_RSD_CANCEL)==GC_RLOAD_HEAP);
            assert(store_native_locks[0].acquired==before);
            assert(dispatch(&runtime,&q,GC_RSD_STEP)==GC_RLOAD_CANCELLED&&store_native_locks[0].acquired==before+1);
            assert(!atomic_load(&f.ctx.private_producers));
        }else assert(!memcmp(&q,&saved,sizeof(q)));
    }
    assert(store_native_accesses>0);
    for(unsigned i=0;i<4;i++){
        assert(store_native_locks[i].acquired&&store_native_locks[i].acquired==store_native_locks[i].released&&!store_native_locks[i].held);
#ifdef _WIN32
        DeleteCriticalSection(&store_native_locks[i].mutex);
#else
        assert(!pthread_mutex_destroy(&store_native_locks[i].mutex));
#endif
    }
    puts("rooted store dispatch: native heap/registry/both-queue locks, guard-free acquisition and automatic admission PASS");
}
#endif
