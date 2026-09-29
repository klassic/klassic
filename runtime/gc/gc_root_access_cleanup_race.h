#ifndef KLASSIC_GC_ROOT_ACCESS_CLEANUP_RACE_H
#define KLASSIC_GC_ROOT_ACCESS_CLEANUP_RACE_H
#include "gc_root_access_cases.h"
#include "gc_barrier_race.h"
typedef struct {
    RootAccess *access;RootAccessCleanup cleanup;
    _Atomic uint64_t *ready,*go;uint64_t waits;
} AccessCleanupActor;
#ifdef _WIN32
static DWORD WINAPI access_cleanup_actor(void *argument)
#else
static void *access_cleanup_actor(void *argument)
#endif
{
    AccessCleanupActor *a=argument;
    atomic_fetch_add_explicit(a->ready,1,memory_order_release);
    // Controlled scheduling with a live guard, not a GC poll or native wait.
    while(!atomic_load_explicit(a->go,memory_order_acquire)){}
    uint64_t result;
    do{
        result=a->cleanup(NULL,a->access,GC_RAC_EXIT);
        if(result==GC_RF_WAIT)a->waits++;
    }while(result==GC_RF_WAIT); // immediate retry: never yield with this guard
    assert(result==GC_RF_DONE);return 0;
}
static void gc_run_root_access_cleanup_races(RootAccessor candidate_access,RootAccessCleanup candidate_cleanup,
        SourceAccess source,ForwardStep forward){
    AccessFixture *f=calloc(2,sizeof(*f));assert(f);uint64_t waits=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned round=0;round<128;round++){
        RootAccessor access=impl?candidate_access:gc_root_access_locked;
        RootAccessCleanup cleanup=impl?candidate_cleanup:gc_root_access_cleanup;
        access_fixture(&f[0],0x22,1);access_fixture(&f[1],0x22,1);
        f[1].f.ctx.thread_id=f[1].f.segment.owner=18;
        MetadataLeasePool *pool=&f[0].f.pool;
        // Independent strong root cells/contexts resolve to the same source.
        assert(access(pool,&f[0].q)==GC_RAC_READY&&access(pool,&f[1].q)==GC_RAC_READY);
        assert(pool->active==2&&f[0].f.page.metadata_leases==2&&atomic_load(&f[0].f.forward.access)==2);
        uint64_t rooted=atomic_load(&f[0].f.cell.value);
        ForwardRequest claim={f[0].f.forward.source_offset,27,1,0};
        assert(forward(&f[0].f.forward,&claim,GC_FWD_CLAIM)==GC_FWD_OK);
        SourceAccessRequest selection={0,claim.source_offset,27,0,1};
        _Atomic uint64_t ready=0,go=0;AccessCleanupActor actors[2];MetadataThread threads[2];
        for(unsigned i=0;i<2;i++){
            actors[i]=(AccessCleanupActor){&f[i].q,cleanup,&ready,&go,0};
#ifdef _WIN32
            threads[i]=CreateThread(NULL,0,access_cleanup_actor,&actors[i],0,NULL);assert(threads[i]);
#else
            assert(pthread_create(&threads[i],NULL,access_cleanup_actor,&actors[i])==0);
#endif
        }
        while(atomic_load_explicit(&ready,memory_order_acquire)!=2)metadata_yield();
        if(!(round&1)){
            assert(source(&f[0].f.forward,&selection,GC_SA_SELECT)==GC_SA_OK);
            assert(source(&f[0].f.forward,&selection,GC_SA_CHECK)==GC_SA_WAIT);
        }
        atomic_store_explicit(&go,1,memory_order_release);
        if(round&1){
            uint64_t result;
            do{result=source(&f[0].f.forward,&selection,GC_SA_SELECT);}while(result==GC_SA_WAIT);
            assert(result==GC_SA_OK);
        }
#ifdef _WIN32
        for(unsigned i=0;i<2;i++){assert(WaitForSingleObject(threads[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(threads[i]));}
#else
        for(unsigned i=0;i<2;i++)assert(pthread_join(threads[i],NULL)==0);
#endif
        // Join returns exclusive control ownership. Guard drain permits copying,
        // but cannot release metadata or erase either actor's strong root.
        assert(source(&f[0].f.forward,&selection,GC_SA_CHECK)==GC_SA_OK);
        assert(pool->active==2&&f[0].f.page.metadata_leases==2);
        for(unsigned i=0;i<2;i++){
            ResolveRequest empty={0};assert(!memcmp(&f[i].q.access,&empty,sizeof(empty)));
            assert(!atomic_load(&f[i].f.ctx.access_epoch)&&f[i].q.follow.cleanup.token);
            assert(atomic_load(&f[i].f.cell.value)==rooted);waits+=actors[i].waits;
            assert(cleanup(pool,&f[i].q,GC_RAC_RELEASE)==GC_RF_DONE);
            assert(pool->active==1-i&&f[0].f.page.metadata_leases==1-i);
        }
        // Selection is still claimed: a fresh access waits without leaking a
        // guard or metadata lease, even though the old controls are reusable.
        assert(access(pool,&f[0].q)==GC_RF_WAIT&&!pool->active&&!f[0].q.follow.cleanup.token);
        assert(atomic_load(&f[0].f.forward.access)==GC_SOURCE_CLOSED);
    }
    free(f);
    printf("root cleanup concurrent EXIT retries observed: %llu\n",(unsigned long long)waits);
    puts("root cleanup concurrency: 256 C/candidate two-reader exits preserve selection drain, roots and metadata ownership PASS");
}
#endif
