/* Deterministic floating-Young boundary regression using the actual driver.
 * No phase, epoch, liveness bit or collector receipt is manufactured. */
#include "klassic_gen_runtime.c"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
static void actual_quantum(KgRuntime *r,KgDriver *d){
    kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);
    assert(!atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire));
}
int main(int argc,char **argv){
    kg_configure(UINT64_C(8)*1024*1024,0,UINT64_C(4)*1024*1024,4);kg_init();
    KgRuntime *r=&kg_runtime;RootCell *a=kg_root_new(),*b=kg_root_new();
    kg_alloc_into(8,1,a);kg_store_i64(a,0,771);
    for(unsigned i=0;i<4;i++)kg_driver_pressure(r,KG_PRESSURE_STRESS);
    kg_alloc_into(8,2,b);kg_store_ref(b,0,a);kg_root_clear(a);
    kg_enter_native(r);uint64_t borrowed=kg_driver_cleanup_enter(r,NULL);KgDriver *d=r->driver;
    assert(!borrowed&&kg_owned_driver==d&&d->busy);
    kgd_native(d,false);kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);
    uint64_t turns=0;
    while(d->generation!=GC_YOUNG||d->state!=6){assert(++turns<UINT64_C(2000000));actual_quantum(r,d);}
    /* B was genuinely marked by the Young snapshot. Remove its application
     * root only after that real MarkEnd handshake has released. Old starts
     * afterwards, so B is physical floating garbage absent from its roots. */
    assert(!r->native.handshake.active);
    kg_leave_native(r);kg_root_clear(b);kg_enter_native(r);
    kg_lock(r);uint64_t old=r->native.cycles.completed[GC_OLD];
    uint64_t visits=d->old_young_visits;d->requested[GC_OLD]=1;kg_unlock(r);
    while(d->state||d->background_old||d->requested[0]||d->requested[1]){
        assert(++turns<UINT64_C(2000000));actual_quantum(r,d);
        if(argc>1&&!strcmp(argv[1],"cancel-reader")&&d->state==3&&d->generation==GC_OLD&&
           d->old_young_started&&d->old_young_cursor)
            (void)kg_driver_cleanup_enter(r,d->old_young_cursor);
    }
    kg_lock(r);assert(r->native.cycles.completed[GC_OLD]>old&&d->old_young_visits>visits);
    uint64_t old_objects=0;
    for(NativeAllocationRun *n=(void *)(uintptr_t)r->native.allocations;n;n=(void *)(uintptr_t)n->next){
        if(atomic_load_explicit(&n->stage,memory_order_acquire)<3)continue;
        AllocationPage *page=gc_native_allocation_page(n);uint64_t state=atomic_load_explicit(&page->state,memory_order_acquire);
        if(page->generation!=GC_OLD||(state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED))continue;
        ForwardTable *table=(void *)(uintptr_t)page->forward_table;
        ForwardCell *cells=(void *)(uintptr_t)table->cells;
        for(uint64_t i=0;i<atomic_load_explicit(&table->used,memory_order_acquire);i++)
            old_objects+=!atomic_load_explicit(&cells[i].word,memory_order_acquire);
    }
    assert(old_objects);kg_unlock(r);
    kgd_native(d,true);kg_driver_cleanup_leave(r,borrowed);kg_leave_native(r);
    kg_root_release(a);kg_root_release(b);kg_shutdown();
    puts("canonical C floating Young boundary: actual Old handoff, retention and rootless shutdown PASS");return 0;
}
