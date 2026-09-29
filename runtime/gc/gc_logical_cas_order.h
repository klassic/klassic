#ifndef KLASSIC_GC_LOGICAL_CAS_ORDER_H
#define KLASSIC_GC_LOGICAL_CAS_ORDER_H
#include "gc_barrier_race.h"
/* Memory-order litmus, not a concurrent collector: both payloads, records and
 * native controls have fixed lifetimes. The auxiliary SeqCst writes inject a
 * competing field value directly; the operation under test is logical CAS.
 * Each actor guards its observed receiver with its own context. */
typedef struct {
    StoreFixture *f;_Atomic uint64_t *other,*go,*done;
    GuardedLogicalCas cas;SourceAccess access;uint64_t result;
} CasOrderActor;
#ifdef _WIN32
static DWORD WINAPI cas_order_actor(void *argument)
#else
static void *cas_order_actor(void *argument)
#endif
{
    CasOrderActor *a=argument;
    for(uint64_t round=1;round<=512;round++){
        while(atomic_load_explicit(a->go,memory_order_acquire)<round)metadata_yield();
        assert(a->access(&a->f->cell,&a->f->scope,GC_SA_ENTER)==GC_SA_OK);
        atomic_store_explicit(a->other,(UINT64_C(96)<<16)|0x1510,memory_order_seq_cst);
        const MarkRecord *expected=(round&1)?&a->f->record:NULL;
        a->result=a->cas(&a->f->q,expected,NULL);
        assert(a->access(&a->f->cell,&a->f->scope,GC_SA_EXIT)==GC_SA_OK);
        atomic_fetch_add_explicit(a->done,1,memory_order_release);
    }
    return 0;
}
static void gc_run_logical_cas_order(GuardedLogicalCas candidate,SourceAccess access){
    StoreFixture *f=calloc(2,sizeof(*f));assert(f);
    for(unsigned impl=0;impl<2;impl++){
        _Atomic uint64_t go=0,done=0;CasOrderActor actors[2];MetadataThread threads[2];
        for(unsigned i=0;i<2;i++){
            store_fixture(&f[i],0,GC_YOUNG,0);f[i].q.desired=0;
            f[i].ctx.thread_id=17+i;f[i].scope.held=0;
            atomic_store(&f[i].ctx.access_epoch,0);atomic_store(&f[i].cell.access,0);
            actors[i]=(CasOrderActor){&f[i],(_Atomic uint64_t *)(f[1-i].heap+48),&go,&done,
                impl?candidate:gc_guarded_compare_exchange,access,99};
#ifdef _WIN32
            threads[i]=CreateThread(NULL,0,cas_order_actor,&actors[i],0,NULL);assert(threads[i]);
#else
            assert(pthread_create(&threads[i],NULL,cas_order_actor,&actors[i])==0);
#endif
        }
        for(uint64_t round=1;round<=512;round++){
            for(unsigned i=0;i<2;i++)atomic_store_explicit((_Atomic uint64_t *)(f[i].heap+48),0,memory_order_relaxed);
            atomic_store_explicit(&go,round,memory_order_release);
            while(atomic_load_explicit(&done,memory_order_acquire)!=round*2)metadata_yield();
            uint64_t unseen=(round&1)?GC_STORE_MISMATCH:GC_STORE_OK;
            for(unsigned i=0;i<2;i++){
                assert(actors[i].result==unseen||actors[i].result==GC_STORE_RETRY);
                assert(!f[i].q.transferred&&!f[i].scope.held&&!atomic_load(&f[i].ctx.access_epoch));
            }
            // SC store X precedes CAS Y, and SC store Y precedes CAS X.
            // Both comparisons cannot precede the other's store in the SC order.
            assert(actors[0].result!=unseen||actors[1].result!=unseen);
        }
#ifdef _WIN32
        for(unsigned i=0;i<2;i++){assert(WaitForSingleObject(threads[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(threads[i]));}
#else
        for(unsigned i=0;i<2;i++)assert(pthread_join(threads[i],NULL)==0);
#endif
    }
    free(f);
    puts("logical CAS ordering: 1024 C/candidate two-thread SeqCst success and mismatch rounds PASS");
}
#endif
