/* Forced table/start/top publication order. All native metadata is retained
 * through join; the managed mapping remains inaccessible for the entire test. */
#ifndef KLASSIC_GC_REMEMBERED_OWNER_RACE_H
#define KLASSIC_GC_REMEMBERED_OWNER_RACE_H
#include "gc_remembered_owner_cases.h"
#include "gc_metadata_concurrency.h"
typedef struct {OwnerFixture f;RememberedOwnerFinder find;_Atomic uint64_t stage,ack;} OwnerAppendRace;
static void owner_race_query(OwnerAppendRace *r,unsigned object,uint64_t expected){
    RememberedOwner q=r->f.q;uint64_t slot=4096+96*object+40;
    r->f.claim.offset=r->f.set.active_offset=slot;
    assert(r->find(&q)==expected);
    if(expected==GC_RO_OK)assert(q.owner==4096+96*object+32&&q.byte_offset==8&&q.cell==(uintptr_t)&r->f.cells[object]);
    else assert(!q.owner&&!q.byte_offset&&!q.cell);
}
#ifdef _WIN32
static DWORD WINAPI owner_append_reader(void *argument)
#else
static void *owner_append_reader(void *argument)
#endif
{
    OwnerAppendRace *r=argument;
    for(unsigned i=1;i<33;i++){
        while(atomic_load_explicit(&r->stage,memory_order_acquire)<2*i-1)metadata_yield();
        owner_race_query(r,i-1,GC_RO_OK);owner_race_query(r,i,GC_RO_MISSING);
        atomic_store_explicit(&r->ack,2*i-1,memory_order_release);
        while(atomic_load_explicit(&r->stage,memory_order_acquire)<2*i)metadata_yield();
        owner_race_query(r,i,GC_RO_OK);atomic_store_explicit(&r->ack,2*i,memory_order_release);
    }
    return 0;
}
static void gc_run_remembered_owner_races(RememberedOwnerFinder find){
    OwnerAppendRace *r=calloc(1,sizeof(*r));assert(r);r->find=find;
    void *mapping=pool_vm_reserve(16384); // deliberately never committed
    for(unsigned round=0;round<32;round++){
        owner_fixture(&r->f,1,4136);r->f.page.heap_base=(uintptr_t)mapping;atomic_store(&r->f.page.state,GC_PAGE_ALLOCATING);
        memset(r->f.cells+1,0,32*sizeof(ForwardCell));atomic_store(&r->stage,0);atomic_store(&r->ack,0);
#ifdef _WIN32
        HANDLE reader=CreateThread(NULL,0,owner_append_reader,r,0,NULL);assert(reader);
#else
        pthread_t reader;assert(pthread_create(&reader,NULL,owner_append_reader,r)==0);
#endif
        for(unsigned i=1;i<33;i++){
            uint64_t offset=4096+96*i+32;r->f.cells[i].source_offset=offset;r->f.cells[i].incarnation=17;
            atomic_store_explicit(&r->f.table.used,i+1,memory_order_release);
            atomic_store_explicit(&r->stage,2*i-1,memory_order_release);
            while(atomic_load_explicit(&r->ack,memory_order_acquire)<2*i-1)metadata_yield();
            uint64_t bit=(offset-4096)/16;
            atomic_fetch_or_explicit(&r->f.starts[bit/64],UINT64_C(1)<<(bit%64),memory_order_release);
            atomic_store_explicit(&r->f.page.allocated_top,4096+96*(i+1),memory_order_release);
            atomic_store_explicit(&r->stage,2*i,memory_order_release);
            while(atomic_load_explicit(&r->ack,memory_order_acquire)<2*i)metadata_yield();
        }
#ifdef _WIN32
        assert(WaitForSingleObject(reader,INFINITE)==WAIT_OBJECT_0);assert(CloseHandle(reader));
#else
        assert(pthread_join(reader,NULL)==0);
#endif
        assert(atomic_load(&r->ack)==64);
    }
    pool_vm_release(mapping,16384);free(r);
    puts("remembered owner: 32 append publication rounds reject unpublished slots without reading protected heap PASS");
}
#endif
