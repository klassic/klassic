#ifndef KLASSIC_GC_ROOT_JOURNAL_CONCURRENCY_H
#define KLASSIC_GC_ROOT_JOURNAL_CONCURRENCY_H
#include "gc_root_journal_cases.h"
#ifdef _WIN32
#include <windows.h>
typedef HANDLE JournalThread;
#else
#include <pthread.h>
#include <sched.h>
typedef pthread_t JournalThread;
#endif
typedef struct {
    RootRegistry g;RootRegistration r;RootSegment s;MutatorContext ctx;RootCell cell;
    BufferCursor buffer;RootChange entries[3];RootJournalStep step;
    _Atomic uint64_t copied,resume,finished;
#ifdef _WIN32
    CRITICAL_SECTION lock;
#else
    pthread_mutex_t lock;
#endif
} JournalRace;
static void journal_lock(JournalRace *r){
#ifdef _WIN32
    EnterCriticalSection(&r->lock);
#else
    assert(!pthread_mutex_lock(&r->lock));
#endif
}
static void journal_unlock(JournalRace *r){
#ifdef _WIN32
    LeaveCriticalSection(&r->lock);
#else
    assert(!pthread_mutex_unlock(&r->lock));
#endif
}
static void journal_yield(void){
#ifdef _WIN32
    SwitchToThread();
#else
    sched_yield();
#endif
}
static uint64_t journal_locked(JournalRace *r,RootJournalRequest *q,uint64_t op){
    journal_lock(r);uint64_t result=r->step(&r->g,q,op);journal_unlock(r);return result;
}
static uint64_t journal_reference(uint64_t id){return id?((id<<20)|0x1510):0;}
#ifdef _WIN32
static DWORD WINAPI journal_consumer(void *arg)
#else
static void *journal_consumer(void *arg)
#endif
{
    JournalRace *r=arg;RootLease lease={0};RootChange output[2];uint64_t processed=0;
    RootJournalRequest q={.registration=(uintptr_t)&r->r,.lease=(uintptr_t)&lease,.scope=r->r.scope,.output=(uintptr_t)output,.capacity=2};
    for(;;){uint64_t result=journal_locked(r,&q,GC_RJ_CLAIM);
        if(result==GC_RJ_EMPTY){if(atomic_load_explicit(&r->finished,memory_order_acquire)&&atomic_load(&r->buffer.consumed)==65)break;journal_yield();continue;}
        assert(result==GC_RJ_OK);assert(r->step(&r->g,&q,GC_RJ_COPY)==GC_RJ_OK);
        if(!processed){atomic_store_explicit(&r->copied,1,memory_order_release);while(!atomic_load_explicit(&r->resume,memory_order_acquire))journal_yield();}
        for(uint64_t i=0;i<q.written;i++){
            assert(output[i].previous==journal_reference(processed));processed++;
            assert(output[i].next==journal_reference(processed==65?0:processed));
            assert(output[i].scope==q.scope&&output[i].phase==0x22);
        }
        assert(journal_locked(r,&q,GC_RJ_ACK)==GC_RJ_OK);
    }
    assert(processed==65);return 0;
}
static void gc_run_root_journal_races(RootJournalStep step,uint64_t (*roots)(MutatorContext *,RootRequest *,uint64_t)){
    for(unsigned round=0;round<64;round++){
        JournalRace r={.step=step};r.g.phase=0x22;r.ctx.abi_version=GC_ABI_VERSION;r.ctx.thread_id=17;atomic_store(&r.ctx.adopted_phase,0x22);
#ifdef _WIN32
        InitializeCriticalSection(&r.lock);
#else
        assert(!pthread_mutex_init(&r.lock,NULL));
#endif
        r.buffer=(BufferCursor){.entries=(uintptr_t)r.entries,.capacity=3};
        r.s=(RootSegment){.owner=17,.cells=(uintptr_t)&r.cell,.capacity=1,.journal=(uintptr_t)&r.buffer};
        r.r.segment=(uintptr_t)&r.s;RootLease cleanup={0};RootLifeRequest life={.registration=(uintptr_t)&r.r,.lease=(uintptr_t)&cleanup,.context=(uintptr_t)&r.ctx};
        assert(gc_root_life_locked(&r.g,&life,GC_RL_ATTACH)==GC_RL_OK);
        RootRequest root={.segment=(uintptr_t)&r.s};assert(roots(&r.ctx,&root,GC_ROOT_RESERVE)==GC_ROOT_OK);
        root.value=journal_reference(1);assert(roots(&r.ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
        JournalThread worker;
#ifdef _WIN32
        worker=CreateThread(NULL,0,journal_consumer,&r,0,NULL);assert(worker);
#else
        assert(!pthread_create(&worker,NULL,journal_consumer,&r));
#endif
        while(!atomic_load_explicit(&r.copied,memory_order_acquire))journal_yield();
        for(uint64_t i=2;i<=3;i++){root.value=journal_reference(i);assert(roots(&r.ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);}
        root.value=journal_reference(4);assert(roots(&r.ctx,&root,GC_ROOT_STORE)==GC_ROOT_FLUSH);
        assert(atomic_load(&r.buffer.consumed)==0&&atomic_load(&r.ctx.root_revision)==3);
        RootLease other={0};RootJournalRequest competing={.registration=(uintptr_t)&r.r,.lease=(uintptr_t)&other,.scope=r.r.scope,.capacity=1};
        assert(journal_locked(&r,&competing,GC_RJ_CLAIM)==GC_RJ_WAIT);
        atomic_store_explicit(&r.resume,1,memory_order_release);
        for(uint64_t i=4;i<=65;i++){
            root.value=journal_reference(i);uint64_t result;
            do{result=roots(&r.ctx,&root,i==65?GC_ROOT_RELEASE:GC_ROOT_STORE);if(result==GC_ROOT_FLUSH)journal_yield();}while(result==GC_ROOT_FLUSH);
            assert(result==GC_ROOT_OK);
        }
        atomic_store_explicit(&r.finished,1,memory_order_release);
#ifdef _WIN32
        assert(WaitForSingleObject(worker,30000)==WAIT_OBJECT_0);assert(CloseHandle(worker));
#else
        assert(!pthread_join(worker,NULL));
#endif
        assert(!r.g.leases&&!r.r.leases&&!r.r.journal_serial&&atomic_load(&r.buffer.consumed)==65&&atomic_load(&r.ctx.root_revision)==65);
        assert(gc_root_life_locked(&r.g,&life,GC_RL_RETIRE)==GC_RL_OK);
        assert(gc_root_life_locked(&r.g,&life,GC_RL_CLAIM)==GC_RL_OK);
        uint64_t value=1;RootScanJob scan={.scan={(uintptr_t)&r.s,0,(uintptr_t)&value,1,0},.lease=(uintptr_t)&cleanup,.serial=life.serial};
        assert(gc_root_scan_leased(&scan)==GC_ROOT_OK&&!value);
        assert(gc_root_life_locked(&r.g,&life,GC_RL_ACK)==GC_RL_OK);
        assert(gc_root_life_locked(&r.g,&life,GC_RL_CLEAN)==GC_RL_OK);assert(gc_root_clear_chunk(&life)==GC_RL_OK);
        assert(gc_root_life_locked(&r.g,&life,GC_RL_FINISH)==GC_RL_OK);
#ifdef _WIN32
        DeleteCriticalSection(&r.lock);
#else
        assert(!pthread_mutex_destroy(&r.lock));
#endif
    }
    puts("root journal: 64 concurrent producer/copy, full-ring retention and competing-consumer rounds PASS");
}
#endif
