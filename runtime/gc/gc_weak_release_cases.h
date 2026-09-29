#ifndef KLASSIC_GC_WEAK_RELEASE_CASES_H
#define KLASSIC_GC_WEAK_RELEASE_CASES_H
#include "gc_weak_release.h"
#include "gc_weak_load_cases.h"
typedef struct {
    WeakLoadFixture fixture;WeakLoader load;WeakReleaser release;
    _Atomic unsigned ready,turn;unsigned load_first;uint64_t loaded,released;
#ifdef _WIN32
    CRITICAL_SECTION lock;
#else
    pthread_mutex_t lock;
#endif
} WeakReleaseRace;
typedef struct {WeakReleaseRace *race;unsigned loading;} WeakReleaseActor;
static void weak_release_yield(void){
#ifdef _WIN32
    SwitchToThread();
#else
    sched_yield();
#endif
}
#ifdef _WIN32
static DWORD WINAPI weak_release_actor(void *argument){
#else
static void *weak_release_actor(void *argument){
#endif
    WeakReleaseActor *a=argument;WeakReleaseRace *r=a->race;
    unsigned order=a->loading==r->load_first?0:1;
    atomic_fetch_add_explicit(&r->ready,1,memory_order_release);
    while(atomic_load_explicit(&r->ready,memory_order_acquire)!=2||atomic_load_explicit(&r->turn,memory_order_acquire)!=order)weak_release_yield();
#ifdef _WIN32
    EnterCriticalSection(&r->lock);
#else
    assert(!pthread_mutex_lock(&r->lock));
#endif
    if(a->loading)r->loaded=r->load(&r->fixture.gate,&r->fixture.request,0);
    else r->released=r->release(&r->fixture.gate,&r->fixture.slot,11);
#ifdef _WIN32
    LeaveCriticalSection(&r->lock);
#else
    assert(!pthread_mutex_unlock(&r->lock));
#endif
    atomic_store_explicit(&r->turn,order+1,memory_order_release);return 0;
}
static void gc_run_weak_release_cases(WeakReleaser candidate,WeakLoader load_candidate){
    for(unsigned impl=0;impl<2;impl++){
        WeakReleaser release=impl?candidate:gc_weak_release_locked;
        WeakLoader load=impl?load_candidate:gc_weak_load_into_locked;
        for(unsigned mode=0;mode<16;mode++){
            WeakLoadFixture w;weak_load_fixture(&w);uint64_t serial=11,expected=GC_WEAK_INVALID;
            WeakLoadGate *gate=&w.gate;WeakSlot *slot=&w.slot;
            switch(mode){
            case 0:expected=GC_WEAK_RELEASED;break;
            case 1:w.gate.closed=1;expected=GC_WEAK_RELEASED;break;
            case 2:assert(gc_metadata_step_locked(&w.f.pool,&w.slot.lease,GC_ML_RELEASE)==GC_ML_OK);w.slot.reference=0;expected=GC_WEAK_RELEASED;break;
            case 3:w.gate.closed=2;break;
            case 4:serial++;break;
            case 5:w.slot.gate+=8;break;
            case 6:w.slot.lease.token+=UINT64_C(1)<<32;break;
            case 7:w.slot.lease.page+=8;break;
            case 8:w.slot.lease.incarnation++;break;
            case 9:w.slot.reference=0;break;
            case 10:w.slot.lease=(MetadataLeaseRequest){0};break;
            case 11:serial=0;break;
            case 12:gate=NULL;break;
            case 13:slot=(void *)((char *)&w.slot+1);break;
            case 14:assert(load(&w.gate,&w.request,0)==GC_WEAK_LOADED);expected=GC_WEAK_RELEASED;break;
            case 15:assert(release(&w.gate,&w.slot,11)==GC_WEAK_RELEASED);break;
            }
            WeakLoadFixture before;memcpy(&before,&w,sizeof(w));
            assert(release(gate,slot,serial)==expected);
            if(expected==GC_WEAK_INVALID)assert(!memcmp(&before,&w,sizeof(w)));
            else{
                WeakSlot empty={0};assert(!memcmp(&w.slot,&empty,sizeof(empty)));
                assert(!w.f.pool.active&&!w.f.page.metadata_leases);
                assert(!memcmp(&w.f.cell,&before.f.cell,sizeof(w.f.cell))&&!memcmp(&w.f.journal,&before.f.journal,sizeof(w.f.journal)));
                assert(!memcmp(&w.output,&before.output,sizeof(w.output)));
                if(mode==14)assert(atomic_load(&w.f.cell.value)&&atomic_load(&w.f.journal.published)==1&&!atomic_load(&w.f.journal.consumed));
                memcpy(&before,&w,sizeof(w));
                assert(release(&w.gate,&w.slot,11)==GC_WEAK_INVALID&&!memcmp(&before,&w,sizeof(w)));
            }
        }
        for(unsigned round=0;round<32;round++){
            WeakReleaseRace r={.load=load,.release=release,.load_first=round&1};weak_load_fixture(&r.fixture);
            uint64_t reference=r.fixture.slot.reference;WeakReleaseActor actors[]={{&r,1},{&r,0}};
#ifdef _WIN32
            InitializeCriticalSection(&r.lock);HANDLE threads[2];
            for(unsigned i=0;i<2;i++){threads[i]=CreateThread(NULL,0,weak_release_actor,&actors[i],0,NULL);assert(threads[i]);}
            for(unsigned i=0;i<2;i++){assert(WaitForSingleObject(threads[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(threads[i]));}
            DeleteCriticalSection(&r.lock);
#else
            assert(!pthread_mutex_init(&r.lock,NULL));pthread_t threads[2];
            for(unsigned i=0;i<2;i++)assert(!pthread_create(&threads[i],NULL,weak_release_actor,&actors[i]));
            for(unsigned i=0;i<2;i++)assert(!pthread_join(threads[i],NULL));
            assert(!pthread_mutex_destroy(&r.lock));
#endif
            assert(r.released==GC_WEAK_RELEASED&&r.loaded==(r.load_first?GC_WEAK_LOADED:GC_WEAK_INVALID));
            assert(!r.fixture.f.pool.active&&!r.fixture.f.page.metadata_leases&&!r.fixture.slot.serial);
            assert(atomic_load(&r.fixture.f.cell.value)==(r.load_first?reference:0));
            assert(atomic_load(&r.fixture.f.journal.published)==r.load_first&&!atomic_load(&r.fixture.f.journal.consumed));
        }
    }
    puts("weak release: 32 C/candidate ownership and replay cases, 64 two-actor load/dispose schedules PASS");
}
#endif
