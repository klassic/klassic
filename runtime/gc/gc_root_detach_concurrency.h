#ifndef KLASSIC_GC_ROOT_DETACH_CONCURRENCY_H
#define KLASSIC_GC_ROOT_DETACH_CONCURRENCY_H
#include "gc_root_lifetime_concurrency.h"
#include "gc_detach.h"
#include "gc_buffers_cases.h"
typedef uint64_t (*RootDetachHandshake)(HandshakeState *, ThreadRegistration *, uint64_t);
typedef uint64_t (*DetachProgress)(const DetachRequest *);
typedef struct {
    RootLifetimeRace roots;
    HandshakeState handshake;
    ThreadRegistration registration;
    RootDetachHandshake step;
    RootDrainer drain;
    DetachProgress progress;
    BarrierBufferStep buffer_step;
    BarrierBufferDrain buffer_drain;
    BarrierEntry satb_entries[3],remembered_entries[3];
    uint64_t entry_base;
    BufferCursor journal;
    RootChange entries[4];
    RootCell cells[2];
} RootDetachRace;

static uint64_t root_detach_progress(RootDetachRace *r) {
    DetachRequest q={(uintptr_t)&r->roots.g,(uintptr_t)&r->handshake,(uintptr_t)&r->registration};
    life_lock(&r->roots);uint64_t result=r->progress(&q);life_unlock(&r->roots);return result;
}

static uint64_t root_detach_step(RootDetachRace *r, uint64_t operation) {
    life_lock(&r->roots);
    uint64_t result=r->step(&r->handshake, &r->registration, operation);
    life_unlock(&r->roots);
    return result;
}
#ifdef _WIN32
static DWORD WINAPI root_detach_reader(void *arg)
#else
static void *root_detach_reader(void *arg)
#endif
{
    RootDetachRace *r=arg;
    RootLifetimeRace *roots=&r->roots;
    uint64_t values[2];
    RootScanJob job={.scan={(uintptr_t)&roots->s, 0, (uintptr_t)values, 2, 0},
        .lease=(uintptr_t)&roots->leases[0], .serial=roots->leases[0].serial};
    assert(roots->scan(&job)==0 && job.scan.written==2);
    for(unsigned i=0;i<2;i++)assert(values[i]==((uint64_t)(i+1)<<20|0x1510));
    atomic_store_explicit(&roots->ready, 1, memory_order_release);
    while(atomic_load_explicit(&roots->resume, memory_order_acquire)<1)root_yield();
    RootLifeRequest q={.registration=(uintptr_t)&roots->r, .lease=(uintptr_t)&roots->leases[0],
        .serial=job.serial, .scope=roots->leases[0].scope};
    assert(life_locked(roots, &q, GC_RL_RELEASE)==0);
    atomic_store_explicit(&roots->ready, 2, memory_order_release);
    while(atomic_load_explicit(&roots->resume, memory_order_acquire)<2)root_yield();
    RootChange changes[4];RootDrain drain={(uintptr_t)&r->journal, (uintptr_t)changes, 4, 0};
    assert(r->drain(&drain)==0 && drain.written==2);
    // The consumer owns copied references after consumed advances. This fixture
    // visits symbolic identities; production phase/mark handoff is separate.
    for(unsigned i=0;i<2;i++) {
        assert(changes[i].previous==0 && changes[i].next==((uint64_t)(i+1)<<20|0x1510));
        assert(changes[i].phase==0x22 && changes[i].scope==roots->s.scope);
    }
    atomic_store_explicit(&roots->ready,3,memory_order_release);
    while(atomic_load_explicit(&roots->resume,memory_order_acquire)<3)root_yield();
    BarrierEntry entries[2];
    BarrierDrain work={(uintptr_t)&roots->ctx.satb,(uintptr_t)entries,2,0};
    assert(r->buffer_drain(&work)==0&&work.written==2);
    for(unsigned i=0;i<2;i++){BarrierEntry expected=buffer_entry(r->entry_base+i);assert(!memcmp(&expected,&entries[i],sizeof(expected)));}
    atomic_store_explicit(&roots->ready,4,memory_order_release);
    while(atomic_load_explicit(&roots->resume,memory_order_acquire)<4)root_yield();
    work.buffer=(uintptr_t)&roots->ctx.remembered;work.capacity=1;
    for(unsigned i=0;i<2;i++) {
        assert(r->buffer_drain(&work)==(i?0u:2u)&&work.written==1);
        BarrierEntry expected=buffer_entry(r->entry_base+2+i);assert(!memcmp(&expected,&entries[0],sizeof(expected)));
    }
    return 0;
}

static void gc_run_root_detach_races(RootLife life, RootClear clear, RootLeasedScan scan,
        RootDetachHandshake handshake, RootDrainer drain, DetachProgress progress,
        BarrierBufferStep buffer_step,BarrierBufferDrain buffer_drain) {
    RootDetachRace r={0};
    RootLifetimeRace *roots=&r.roots;
    roots->step=life;roots->clear=clear;roots->scan=scan;roots->g.phase=0x22;
    r.step=handshake;r.drain=drain;r.progress=progress;
    r.buffer_step=buffer_step;r.buffer_drain=buffer_drain;
#ifdef _WIN32
    InitializeCriticalSection(&roots->lock);
#else
    assert(pthread_mutex_init(&roots->lock,NULL)==0);
#endif
    for(unsigned round=0;round<64;round++) {
        roots->ctx=(MutatorContext){.abi_version=GC_ABI_VERSION};
        roots->ctx.satb=(BufferCursor){.entries=(uintptr_t)r.satb_entries,.capacity=3};
        roots->ctx.remembered=(BufferCursor){.entries=(uintptr_t)r.remembered_entries,.capacity=3};
        r.entry_base=round*4;
        atomic_store(&roots->ctx.adopted_phase,0x22);
        r.registration=(ThreadRegistration){.context=(uintptr_t)&roots->ctx};
        assert(root_detach_step(&r,GC_HS_REGISTER)==0);
        assert(roots->ctx.thread_id==round+1);
        for(unsigned i=0;i<4;i++){BarrierEntry entry=buffer_entry(r.entry_base+i);assert(buffer_step(&roots->ctx,&entry,i/2)==0);}
        roots->s=(RootSegment){.owner=roots->ctx.thread_id,.cells=(uintptr_t)r.cells,
            .capacity=2,.journal=(uintptr_t)&r.journal};
        roots->r=(RootRegistration){.segment=(uintptr_t)&roots->s};
        memset(roots->leases,0,sizeof(roots->leases));
        r.journal=(BufferCursor){.entries=(uintptr_t)r.entries,.capacity=4};
        RootLifeRequest q={.registration=(uintptr_t)&roots->r,.context=(uintptr_t)&roots->ctx,
            .lease=(uintptr_t)&roots->leases[0]};
        assert(life_locked(roots,&q,GC_RL_ATTACH)==0);
        for(unsigned i=0;i<2;i++) {
            RootRequest request={.segment=(uintptr_t)&roots->s};
            assert(gc_root_step(&roots->ctx,&request,GC_ROOT_RESERVE)==0);
            assert(request.handle.reuse_generation==round+1);
            request.value=((uint64_t)(i+1)<<20)|0x1510;
            assert(gc_root_step(&roots->ctx,&request,GC_ROOT_STORE)==0);
        }
        assert(life_locked(roots,&q,GC_RL_ACQUIRE)==0);
        atomic_store(&roots->ready,0);atomic_store(&roots->resume,0);
        RootThread reader;
#ifdef _WIN32
        reader=CreateThread(NULL,0,root_detach_reader,&r,0,NULL);assert(reader);
#else
        assert(pthread_create(&reader,NULL,root_detach_reader,&r)==0);
#endif
        while(atomic_load_explicit(&roots->ready,memory_order_acquire)<1)root_yield();
        life_lock(roots);
        assert(gc_context_transition_locked(&roots->ctx,GC_RUNNING,GC_DETACHING)==0);
        life_unlock(roots);
        assert(root_detach_step(&r,GC_HS_DETACH)==GC_HS_WAIT);
        assert(root_detach_progress(&r)==GC_DT_FLUSH_SATB);
        assert(buffer_step(&roots->ctx,NULL,GC_BB_PUBLISH_SATB)==0);
        assert(root_detach_progress(&r)==GC_DT_FLUSH_REMEMBERED);
        assert(buffer_step(&roots->ctx,NULL,GC_BB_PUBLISH_REMEMBERED)==0);
        assert(root_detach_progress(&r)==GC_DT_MORE);
        assert(!roots->ctx.root_segments && roots->ctx.retiring_roots==1);
        q.lease=(uintptr_t)&roots->leases[1];life_process_retired(roots,&q);
        assert(life_locked(roots,&q,GC_RL_CLEAN)==GC_RL_WAIT);
        assert(root_detach_progress(&r)==GC_DT_WAIT);
        assert(root_detach_step(&r,GC_HS_DETACH)==GC_HS_WAIT);
        atomic_store_explicit(&roots->resume,1,memory_order_release);
        while(atomic_load_explicit(&roots->ready,memory_order_acquire)<2)root_yield();
        assert(life_locked(roots,&q,GC_RL_CLEAN)==0);assert(clear(&q)==0);
        assert(life_locked(roots,&q,GC_RL_FINISH)==GC_RL_WAIT); // journal still owns entries
        assert(root_detach_step(&r,GC_HS_DETACH)==GC_HS_WAIT);
        assert(roots->ctx.retiring_roots==1 && r.registration.registered==1);
        for(unsigned cycle=0;cycle<2;cycle++) {
            assert(root_detach_step(&r,GC_HS_BEGIN)==GC_HS_OK);
            assert(r.handshake.expected==1);
            assert(root_detach_progress(&r)==GC_DT_MORE);
            assert(root_detach_progress(&r)==GC_DT_WAIT);
            assert(root_detach_step(&r,GC_HS_END)==GC_HS_OK);
            assert(root_detach_step(&r,GC_HS_RESUME)==GC_HS_OK);
            assert(atomic_load(&roots->ctx.state)==GC_DETACHING);
        }
        atomic_store_explicit(&roots->resume,2,memory_order_release);
        while(atomic_load_explicit(&roots->ready,memory_order_acquire)<3)root_yield();
        assert(life_locked(roots,&q,GC_RL_FINISH)==0);
        assert(!roots->ctx.retiring_roots && !roots->g.inventory);
        assert(root_detach_progress(&r)==GC_DT_WAIT); // actual SATB/remset entries still queued
        atomic_store_explicit(&roots->resume,3,memory_order_release);
        while(atomic_load_explicit(&roots->ready,memory_order_acquire)<4)root_yield();
        assert(root_detach_progress(&r)==GC_DT_WAIT); // remembered entries still queued
        atomic_store_explicit(&roots->resume,4,memory_order_release);
#ifdef _WIN32
        assert(WaitForSingleObject(reader,30000)==WAIT_OBJECT_0);assert(CloseHandle(reader));
#else
        assert(pthread_join(reader,NULL)==0);
#endif
        assert(root_detach_progress(&r)==GC_DT_DONE);
        assert(!r.handshake.members && !r.registration.registered);
        assert(atomic_load(&roots->ctx.state)==GC_DETACHED);
    }
#ifdef _WIN32
    DeleteCriticalSection(&roots->lock);
#else
    assert(pthread_mutex_destroy(&roots->lock)==0);
#endif
    puts("root detach: 64 retained-context, delayed-reader/journal and buffer-ownership rounds PASS");
}
#endif
