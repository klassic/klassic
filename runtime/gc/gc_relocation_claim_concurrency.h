/* Native metadata actors contend under the scheduler's heap lock. Managed
 * source lifetime is fixture-owned; these actors neither access payload nor
 * claim production worker-registration or compiler integration coverage. */
#ifndef KLASSIC_GC_RELOCATION_CLAIM_CONCURRENCY_H
#define KLASSIC_GC_RELOCATION_CLAIM_CONCURRENCY_H
#include "gc_relocation_claim_cases.h"
#include "gc_metadata_concurrency.h"
#include "gc_source_access.h"
typedef struct {
    RelocationGate gate;ForwardCell source;RelocationPermit permits[2];RelocationClaim claims[2];
    uint64_t results[2];_Atomic uint64_t ready,go;RelocationClaimer start;MetadataRace mutex;
} ClaimRace;
typedef struct {ClaimRace *race;unsigned index;} ClaimActor;
#ifdef _WIN32
static DWORD WINAPI relocation_claim_actor(void *argument)
#else
static void *relocation_claim_actor(void *argument)
#endif
{
    ClaimActor *actor=argument;ClaimRace *r=actor->race;
    atomic_fetch_add_explicit(&r->ready,1,memory_order_release);
    while(!atomic_load_explicit(&r->go,memory_order_acquire))metadata_yield();
    metadata_lock(&r->mutex);r->results[actor->index]=r->start(&r->gate,&r->claims[actor->index]);metadata_unlock(&r->mutex);
    return 0;
}
static void gc_run_relocation_claim_races(RelocationClaimer start,
        uint64_t (*gate)(RelocationGate *,RelocationGateRequest *,uint64_t),
        uint64_t (*forward)(ForwardCell *,ForwardRequest *,uint64_t),
        uint64_t (*access)(ForwardCell *,SourceAccessRequest *,uint64_t)){
    for(unsigned round=0;round<64;round++){
        ClaimRace r={.gate={.epoch=round+1},.source={.source_offset=16,.incarnation=round+1},.start=start};
#ifdef _WIN32
        InitializeCriticalSection(&r.mutex.lock);
#else
        assert(pthread_mutex_init(&r.mutex.lock,NULL)==0);
#endif
        MetadataThread threads[2];ClaimActor actors[2]={{&r,0},{&r,1}};
        for(unsigned i=0;i<2;i++){
            r.claims[i]=(RelocationClaim){.permit=(uintptr_t)&r.permits[i],.source=(uintptr_t)&r.source,.source_offset=16,.incarnation=round+1};
#ifdef _WIN32
            threads[i]=CreateThread(NULL,0,relocation_claim_actor,&actors[i],0,NULL);assert(threads[i]);
#else
            assert(pthread_create(&threads[i],NULL,relocation_claim_actor,&actors[i])==0);
#endif
        }
        while(atomic_load_explicit(&r.ready,memory_order_acquire)!=2)metadata_yield();
        atomic_store_explicit(&r.go,1,memory_order_release);
        for(unsigned i=0;i<2;i++){
#ifdef _WIN32
            assert(WaitForSingleObject(threads[i],INFINITE)==WAIT_OBJECT_0);assert(CloseHandle(threads[i]));
#else
            assert(pthread_join(threads[i],NULL)==0);
#endif
        }
        unsigned winner=r.results[0]==GC_RCL_OWNED?0:1,loser=1-winner;
        assert(r.results[winner]==GC_RCL_OWNED&&r.results[loser]==GC_RCL_BUSY);
        RelocationClaim *owned=&r.claims[winner];
        assert(owned->ticket==1&&owned->epoch==round+1&&atomic_load(&r.source.word)==5);
        assert(!r.claims[loser].ticket&&!r.claims[loser].epoch&&!r.permits[loser].serial);
        assert(r.gate.last_serial==2&&r.gate.active==1&&r.gate.head==owned->permit&&!r.permits[winner].previous&&!r.permits[winner].next);
        metadata_lock(&r.mutex);
        assert(gate(&r.gate,NULL,GC_RG_CLOSE)==GC_RG_WAIT);
        assert(start(&r.gate,&r.claims[loser])==GC_RCL_CLOSED&&r.gate.last_serial==2);
        SourceAccessRequest select={0,16,round+1,0,owned->ticket};
        assert(access(&r.source,&select,GC_SA_SELECT)==GC_SA_OK&&access(&r.source,&select,GC_SA_CHECK)==GC_SA_OK);
        ForwardRequest retained={16,round+1,owned->ticket,0};
        assert(forward(&r.source,&retained,GC_FWD_RETAIN)==GC_FWD_OK);
        select.ticket=0;assert(access(&r.source,&select,GC_SA_REOPEN)==GC_SA_OK);
        RelocationGateRequest release={owned->permit,owned->ticket,owned->epoch};
        assert(gate(&r.gate,&release,GC_RG_RELEASE)==GC_RG_OK);
        assert(gate(&r.gate,NULL,GC_RG_CHECK)==GC_RG_OK);
        metadata_unlock(&r.mutex);
#ifdef _WIN32
        DeleteCriticalSection(&r.mutex.lock);
#else
        assert(pthread_mutex_destroy(&r.mutex.lock)==0);
#endif
    }
    puts("relocation claim concurrency: 64 two-actor rounds preserve one winner and release every losing permit PASS");
}
#endif
