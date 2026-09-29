#ifndef KLASSIC_GC_ROOT_LIFETIME_CONCURRENCY_H
#define KLASSIC_GC_ROOT_LIFETIME_CONCURRENCY_H
#include "gc_root_lifetime_cases.h"
#include "gc_roots_concurrency.h"
#ifndef _WIN32
#include <sys/mman.h>
#endif
typedef struct {
    RootRegistry g; RootRegistration r; RootSegment s; MutatorContext ctx; RootLease leases[2];
    RootLife step; RootClear clear; RootLeasedScan scan;
    _Atomic uint64_t ready, resume;
#ifdef _WIN32
    CRITICAL_SECTION lock;
#else
    pthread_mutex_t lock;
#endif
} RootLifetimeRace;
static void life_lock(RootLifetimeRace *r) {
#ifdef _WIN32
    EnterCriticalSection(&r->lock);
#else
    assert(pthread_mutex_lock(&r->lock)==0);
#endif
}
static void life_unlock(RootLifetimeRace *r) {
#ifdef _WIN32
    LeaveCriticalSection(&r->lock);
#else
    assert(pthread_mutex_unlock(&r->lock)==0);
#endif
}
static uint64_t life_locked(RootLifetimeRace *r,RootLifeRequest *q,uint64_t op) {
    life_lock(r);uint64_t status=r->step(&r->g,q,op);life_unlock(r);return status;
}
#ifdef _WIN32
static DWORD WINAPI life_reader(void *arg)
#else
static void *life_reader(void *arg)
#endif
{
    RootLifetimeRace *r=arg;uint64_t values[256];
    RootScanJob job={.scan={(uintptr_t)&r->s,0,(uintptr_t)values,256,0},
        .lease=(uintptr_t)&r->leases[0],.serial=r->leases[0].serial};
    assert(r->scan(&job)==2&&job.scan.written==256);
    for(unsigned i=0;i<256;i++)assert(values[i]==((uint64_t)(i+1)<<20|0x1510));
    atomic_store_explicit(&r->ready,1,memory_order_release);
    while(!atomic_load_explicit(&r->resume,memory_order_acquire))root_yield();
    assert(r->scan(&job)==0&&job.scan.written==44);
    for(unsigned i=0;i<44;i++)assert(values[i]==((uint64_t)(i+257)<<20|0x1510));
    RootLifeRequest q={.registration=(uintptr_t)&r->r,.lease=(uintptr_t)&r->leases[0],
        .serial=job.serial,.scope=r->leases[0].scope};
    assert(life_locked(r,&q,GC_RL_RELEASE)==0);
    return 0;
}
static void life_process_retired(RootLifetimeRace *r,RootLifeRequest *q) {
    while(r->r.cursor<r->r.limit){
        assert(life_locked(r,q,GC_RL_CLAIM)==0);
        uint64_t values[256];RootScanJob job={.scan={(uintptr_t)&r->s,q->begin,(uintptr_t)values,q->end-q->begin,0},
            .lease=q->lease,.serial=q->serial};
        assert(r->scan(&job)==(q->end==r->r.limit?0u:2u));
        assert(job.scan.cursor==q->end);
        // Stand-in mark visitor validates every retained root for both captured
        // generations. Actual tracing/phase termination remain separate tasks.
        for(uint64_t i=0;i<job.scan.written;i++) if(values[i])assert(values[i]==((q->begin+i+1)<<20|0x1510));
        assert(q->mask==3);q->mask=1;assert(life_locked(r,q,GC_RL_ACK)==GC_RL_WAIT);
        q->mask=3;assert(life_locked(r,q,GC_RL_ACK)==0);
    }
}
static void gc_run_root_lifetime_races(RootLife step,RootClear clear,RootLeasedScan scan) {
    RootLifetimeRace r={.step=step,.clear=clear,.scan=scan};r.g.phase=0x22;
    r.ctx.abi_version=GC_ABI_VERSION;r.ctx.thread_id=17;atomic_store(&r.ctx.adopted_phase,0x22);
#ifdef _WIN32
    InitializeCriticalSection(&r.lock);
#else
    assert(pthread_mutex_init(&r.lock,NULL)==0);
#endif
    for(unsigned round=0;round<64;round++){
        size_t bytes=16384;
#ifdef _WIN32
        RootCell *cells=VirtualAlloc(NULL,bytes,MEM_RESERVE|MEM_COMMIT,PAGE_READWRITE);assert(cells);
#else
        RootCell *cells=mmap(NULL,bytes,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);assert(cells!=MAP_FAILED);
#endif
        r.s=(RootSegment){.owner=17,.cells=(uintptr_t)cells,.capacity=300};
        r.r=(RootRegistration){.segment=(uintptr_t)&r.s};
        memset(r.leases,0,sizeof(r.leases));atomic_store(&r.ready,0);atomic_store(&r.resume,0);
        RootLifeRequest q={.registration=(uintptr_t)&r.r,.context=(uintptr_t)&r.ctx,.lease=(uintptr_t)&r.leases[0]};
        assert(life_locked(&r,&q,0)==0);assert(q.scope==round*2+1);
        for(unsigned i=0;i<300;i++){atomic_store(&cells[i].value,((uint64_t)(i+1)<<20)|0x1510);cells[i].active=1;cells[i].reuse_generation=12;}
        atomic_store_explicit(&r.s.used,300,memory_order_release);
        assert(life_locked(&r,&q,1)==0);uint64_t old_serial=q.serial,old_scope=q.scope;
        RootThread reader;
#ifdef _WIN32
        reader=CreateThread(NULL,0,life_reader,&r,0,NULL);assert(reader);
#else
        assert(pthread_create(&reader,NULL,life_reader,&r)==0);
#endif
        while(!atomic_load_explicit(&r.ready,memory_order_acquire))root_yield();
        assert(life_locked(&r,&q,3)==0);q.lease=(uintptr_t)&r.leases[1];
        life_process_retired(&r,&q);
        assert(life_locked(&r,&q,6)==GC_RL_WAIT); // reader, even after deletion ACKs, blocks cleanup
        assert(atomic_load(&cells[299].value)==(UINT64_C(300)<<20|0x1510));
        atomic_store_explicit(&r.resume,1,memory_order_release);
#ifdef _WIN32
        assert(WaitForSingleObject(reader,30000)==WAIT_OBJECT_0);assert(CloseHandle(reader));
#else
        assert(pthread_join(reader,NULL)==0);
#endif
        assert(life_locked(&r,&q,6)==0);assert(life_locked(&r,&q,7)==GC_RL_WAIT);
        assert(clear(&q)==GC_RL_WAIT);assert(clear(&q)==0);assert(life_locked(&r,&q,7)==0);
        uint64_t output=0;RootScanJob stale={.scan={(uintptr_t)&r.s,0,(uintptr_t)&output,1,0},.lease=(uintptr_t)&r.leases[0],.serial=old_serial};
#ifdef _WIN32
        DWORD previous;assert(VirtualProtect(cells,bytes,PAGE_NOACCESS,&previous));
#else
        assert(mprotect(cells,bytes,PROT_NONE)==0);
#endif
        assert(scan(&stale)==GC_ROOT_INVALID); // must reject before touching the protected cell pages
#ifdef _WIN32
        assert(VirtualProtect(cells,bytes,PAGE_READWRITE,&previous));
#else
        assert(mprotect(cells,bytes,PROT_READ|PROT_WRITE)==0);
#endif
        assert(life_locked(&r,&q,0)==0);assert(q.scope==old_scope+1);
        RootRequest reserve={.segment=(uintptr_t)&r.s};assert(gc_root_step(&r.ctx,&reserve,0)==0);
        assert(reserve.handle.reuse_generation==13);reserve.handle.scope=old_scope;
        assert(gc_root_step(&r.ctx,&reserve,3)==GC_ROOT_INVALID);
        q.lease=(uintptr_t)&r.leases[0];assert(life_locked(&r,&q,1)==0);assert(q.serial!=old_serial);
        assert(scan(&stale)==GC_ROOT_INVALID);assert(life_locked(&r,&q,2)==0);
        assert(life_locked(&r,&q,3)==0);q.lease=(uintptr_t)&r.leases[1];life_process_retired(&r,&q);
        assert(life_locked(&r,&q,6)==0);assert(clear(&q)==0);assert(life_locked(&r,&q,7)==0);
        assert(!r.g.active&&!r.g.retiring&&!r.g.leases&&!r.g.head);
#ifdef _WIN32
        assert(VirtualFree(cells,0,MEM_RELEASE));
#else
        assert(munmap(cells,bytes)==0);
#endif
    }
#ifdef _WIN32
    DeleteCriticalSection(&r.lock);
#else
    assert(pthread_mutex_destroy(&r.lock)==0);
#endif
    puts("root retirement: 64 delayed-reader, dual-mark ACK, protected-page and same-address reuse rounds PASS");
}
#endif
