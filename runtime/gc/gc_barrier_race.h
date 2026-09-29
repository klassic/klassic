#ifndef KLASSIC_GC_BARRIER_RACE_H
#define KLASSIC_GC_BARRIER_RACE_H
#include "gc_barrier_cases.h"
#include "gc_source_access_cases.h"
#include "gc_metadata_concurrency.h"
typedef struct {
    StoreFixture *f;ForwardCell *cell;GuardedStorer store;SourceAccess access;
    _Atomic uint64_t *ready,*go;uint64_t result;
} StoreActor;
#ifdef _WIN32
static DWORD WINAPI store_actor(void *argument)
#else
static void *store_actor(void *argument)
#endif
{
    StoreActor *a=argument;uint64_t status;
    do{status=a->access(a->cell,&a->f->scope,GC_SA_ENTER);if(status==GC_SA_WAIT)metadata_yield();}while(status==GC_SA_WAIT);
    assert(status==GC_SA_OK);atomic_fetch_add_explicit(a->ready,1,memory_order_release);
    // Deliberate controlled preemption with live scopes, never a GC poll/wait.
    while(!atomic_load_explicit(a->go,memory_order_acquire)){}
    a->result=a->store(&a->f->q);
    do{status=a->access(a->cell,&a->f->scope,GC_SA_EXIT);}while(status==GC_SA_WAIT);
    assert(status==GC_SA_OK);return 0;
}
static void gc_run_guarded_store_races(GuardedStorer store,SourceAccess access){
    unsigned late=0;
    for(unsigned round=0;round<128;round++){
        StoreFixture a,b;uint64_t old=(UINT64_C(128)<<16)|0x1510;
        store_fixture(&a,0x22,GC_OLD,old);store_fixture(&b,0x22,GC_OLD,old);
        a.scope.held=b.scope.held=0;atomic_store(&a.ctx.access_epoch,0);atomic_store(&b.ctx.access_epoch,0);atomic_store(&a.cell.access,0);
        b.q.page=(uintptr_t)&a.page;b.q.registry=(uintptr_t)&a.registry;b.q.desired=(UINT64_C(160)<<16)|0x1510;b.ctx.thread_id=18;
        // Lease tokens are per-producer native obligations in this fixed-lifetime
        // fixture. The real graph fixture acquires/releases actual pool tokens.
        b.q.satb.slot.metadata_lease=13;b.q.remembered.slot.metadata_lease=14;
        _Atomic uint64_t ready=0,go=0;StoreActor actors[2]={{&a,&a.cell,store,access,&ready,&go,99},{&b,&a.cell,store,access,&ready,&go,99}};
        MetadataThread threads[2];
#ifdef _WIN32
        for(unsigned i=0;i<2;i++){threads[i]=CreateThread(NULL,0,store_actor,&actors[i],0,NULL);assert(threads[i]);}
#else
        for(unsigned i=0;i<2;i++)assert(pthread_create(&threads[i],NULL,store_actor,&actors[i])==0);
#endif
        while(atomic_load_explicit(&ready,memory_order_acquire)!=2)metadata_yield();atomic_store_explicit(&go,1,memory_order_release);
#ifdef _WIN32
        for(unsigned i=0;i<2;i++){assert(WaitForSingleObject(threads[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(threads[i]));}
#else
        for(unsigned i=0;i<2;i++)assert(pthread_join(threads[i],NULL)==0);
#endif
        assert((actors[0].result==GC_STORE_OK&&actors[1].result==GC_STORE_RETRY)||(actors[1].result==GC_STORE_OK&&actors[0].result==GC_STORE_RETRY));
        unsigned winner=actors[0].result==GC_STORE_OK?0:1;
        assert(atomic_load((_Atomic uint64_t *)(a.heap+48))==actors[winner].f->q.desired);
        for(unsigned i=0;i<2;i++){
            StoreFixture *f=actors[i].f;uint64_t logged=f->q.transferred;
            assert(logged==0||logged==3);if(i==winner)assert(logged==3);else if(logged==3)late++;
            assert(f->ctx.satb.top==(logged?1u:0u)&&f->ctx.remembered.top==f->ctx.satb.top);
            assert(atomic_load(&f->ctx.satb.published)==f->ctx.satb.top&&atomic_load(&f->ctx.remembered.published)==f->ctx.remembered.top);
            if(logged){assert(!memcmp(&f->satb[0],&f->q.satb,sizeof(BarrierEntry)));assert(!memcmp(&f->remembered[0],&f->q.remembered,sizeof(BarrierEntry)));}
            assert(!f->scope.held&&!atomic_load(&f->ctx.access_epoch));
        }
        assert(!atomic_load(&a.cell.access));
    }
    printf("guarded store late CAS conflicts observed: %u\n",late);
    puts("guarded store: 128 two-writer CAS winner, retry and conservative-log ownership rounds PASS");
}
#endif
