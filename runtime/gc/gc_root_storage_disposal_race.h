/* FINISH on a collector thread races the exclusive backing owner's polling.
 * Unmap immediately after the acquired completion receipt, before join. */
#ifndef KLASSIC_GC_ROOT_STORAGE_DISPOSAL_RACE_H
#define KLASSIC_GC_ROOT_STORAGE_DISPOSAL_RACE_H
#include "gc_root_storage.h"
#include "gc_root_lifetime.h"
#include "gc_native_metadata.h"
#include <pthread.h>
#include <sched.h>
#include <assert.h>
#include <stdio.h>
typedef struct {
    RootRegistry registry;MutatorContext context;
    RootLease reader,cleaner;RootLifeRequest reading,cleaning;
    _Atomic unsigned go,ready;
} RootDisposalRace;
static void *gc_root_disposal_finish_thread(void *arg){
    RootDisposalRace *r=arg;
    while(!atomic_load_explicit(&r->go,memory_order_acquire))sched_yield();
    assert(gc_root_life_locked(&r->registry,&r->reading,GC_RL_RELEASE)==GC_RL_OK);
    assert(gc_root_life_locked(&r->registry,&r->cleaning,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&r->cleaning)==GC_RL_OK);
    atomic_store_explicit(&r->ready,1,memory_order_release);
    /* No registry metadata may be inspected by the owner during this window. */
    for(unsigned i=0;i<32;i++)sched_yield();
    assert(gc_root_life_locked(&r->registry,&r->cleaning,GC_RL_FINISH)==GC_RL_OK);
    /* FINISH's PREPARED release was the last access to backing. */
    return NULL;
}
static void gc_run_root_disposal_race(RootStorageDisposalCheck check){
    NativeMetadataOps native=gc_native_metadata_ops();
    for(unsigned attempt=0;attempt<64;attempt++){
        RootStorage *storage=native.map(native.quantum);assert(storage);
        RootDisposalRace r={.registry={.phase=0x10000},
            .context={.abi_version=GC_ABI_VERSION,.thread_id=17,.adopted_phase=0x10000}};
        RootStorageSpec spec={17,1,1,native.quantum};
        assert(gc_root_storage_prepare(storage,&spec)==GC_ROOT_STORAGE_OK);
        RootLifeRequest life={.registration=(uintptr_t)&storage->registration,.context=(uintptr_t)&r.context};
        assert(gc_root_life_locked(&r.registry,&life,GC_RL_ATTACH)==GC_RL_OK);
        r.reading=(RootLifeRequest){.registration=life.registration,.scope=life.scope,.lease=(uintptr_t)&r.reader};
        assert(gc_root_life_locked(&r.registry,&r.reading,GC_RL_ACQUIRE)==GC_RL_OK);
        assert(gc_root_life_locked(&r.registry,&life,GC_RL_RETIRE)==GC_RL_OK);
        r.cleaning=(RootLifeRequest){.registration=life.registration,.scope=life.scope,.lease=(uintptr_t)&r.cleaner};
        assert(gc_root_life_locked(&r.registry,&r.cleaning,GC_RL_CLEAN)==GC_RL_WAIT);
        assert(check(storage)==GC_ROOT_STORAGE_INVALID);
        pthread_t collector;assert(!pthread_create(&collector,NULL,gc_root_disposal_finish_thread,&r));
        atomic_store_explicit(&r.go,1,memory_order_release);
        while(!atomic_load_explicit(&r.ready,memory_order_acquire))sched_yield();
        unsigned retries=0;
        while(check(storage)!=GC_ROOT_STORAGE_OK){assert(++retries<1000000);sched_yield();}
        assert(native.unmap(storage,native.quantum));
        assert(!pthread_join(collector,NULL));
        assert(!r.registry.inventory&&!r.registry.retiring&&!r.registry.leases&&!r.context.retiring_roots);
    }
    puts("root disposal race: 64 reader-release/FINISH races acquire completion before metadata and unmap PASS");
}
#endif
