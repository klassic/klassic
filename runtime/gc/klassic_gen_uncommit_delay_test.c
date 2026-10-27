#include "klassic_gen_runtime.c"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
#include <time.h>
static uint64_t failed_calls;
static bool injected(void *address,size_t bytes){
    KgRuntime *r=&kg_runtime;kg_lock(r);KgPolicy *p=kg_policy(r);
    uint64_t index=(((uintptr_t)address-r->native.backing.heap.heap_base)>>GC_HEAP_PAGE_SHIFT)-1;
    assert(p->states[index]==(KG_BACKED|KG_DROPPING|KG_SWEEPING));
    assert(p->dropping==GC_HEAP_PAGE_BYTES&&p->committed>=p->floor+GC_HEAP_PAGE_BYTES);
    kg_unlock(r);failed_calls++;
    if(failed_calls==1)return false;
    bool result=gc_native_heap_decommit_range(address,bytes);
    return failed_calls==2?false:result;
}
static bool permanent(void *address,size_t bytes){
    (void)address;(void)bytes;KgRuntime *r=&kg_runtime;kg_lock(r);KgPolicy *p=kg_policy(r);
    assert(p->dropping==GC_HEAP_PAGE_BYTES&&p->committed>=p->floor+GC_HEAP_PAGE_BYTES);kg_unlock(r);
    if(++failed_calls==32)fprintf(stderr,"delay permanent: 32 callbacks retained actual backing/drop ownership\n");
    return false;
}
static void drive(KgRuntime *r){
    KgDriver *d=r->driver;kg_lock(r);d->requested[0]=d->requested[1]=1;kg_unlock(r);
    for(uint64_t steps=0;;steps++){
        assert(steps<1000000);kg_lock(r);bool done=!d->state&&!d->background_old&&!d->requested[0]&&!d->requested[1];
        if(!done)kgd_adopt(d);kg_unlock(r);if(done)break;kgd_step(d);kgd_native(d,true);
    }
}
static uint64_t committed(KgRuntime *r){kg_lock(r);uint64_t bytes=kg_policy(r)->committed;kg_unlock(r);return bytes;}
static _Atomic unsigned race_ready,race_go,race_reserved,race_done,race_release;
static RootCell *race_root;
static KgMutator *race_mutator;
static void *race_child(void *unused){
    (void)unused;kg_thread_attach();race_mutator=kg_current_mutator(&kg_runtime);race_root=kg_root_new();
    kg_native_enter();atomic_store_explicit(&race_ready,1,memory_order_release);
    while(!atomic_load_explicit(&race_go,memory_order_acquire))kg_yield();
    kg_native_exit();kg_alloc_into(8,1,race_root);assert(kg_load_i64(race_root,0)==0);kg_store_i64(race_root,0,UINT64_C(0xabc01234));
    kg_native_enter();atomic_store_explicit(&race_done,1,memory_order_release);
    while(!atomic_load_explicit(&race_release,memory_order_acquire))kg_yield();
    kg_native_exit();kg_root_release(race_root);kg_thread_detach();return NULL;
}
static bool competing(void *address,size_t bytes){
    KgRuntime *r=&kg_runtime;HeapPool *heap=&r->native.backing.heap;
    uint64_t index=(((uintptr_t)address-heap->heap_base)>>GC_HEAP_PAGE_SHIFT)-1;
    atomic_store_explicit(&race_go,1,memory_order_release);uint64_t started=kg_policy_clock();
    for(;;){
        kg_lock(r);AllocationPage *page=(AllocationPage *)(uintptr_t)heap->pages+index;
        bool reserved=atomic_load_explicit(&page->state,memory_order_acquire)==GC_PAGE_PREPARING;
        if(reserved){
            assert(kg_policy(r)->states[index]==(KG_BACKED|KG_DROPPING|KG_SWEEPING));
            assert(atomic_load_explicit(&race_mutator->context->state,memory_order_acquire)==GC_BLOCKED_NATIVE);
            assert(!atomic_load_explicit(&race_mutator->context->access_epoch,memory_order_acquire));
        }
        kg_unlock(r);if(reserved)break;
        assert(!atomic_load_explicit(&race_done,memory_order_acquire));
        assert(kg_policy_clock()-started<UINT64_C(2000000000));kg_yield();
    }
    assert(!atomic_load_explicit(&race_done,memory_order_acquire));atomic_store_explicit(&race_reserved,1,memory_order_release);
    return gc_native_heap_decommit_range(address,bytes);
}
int main(int argc,char **argv){
    assert(kg_policy_ticks_to_ns(0,1)==0&&kg_policy_ticks_to_ns(1,0)==0);
    assert(kg_policy_ticks_to_ns(UINT64_MAX,UINT64_C(1000000000))==UINT64_MAX);
    assert(kg_policy_ticks_to_ns(UINT64_MAX,1)==0);
    assert(kg_policy_ticks_to_ns((UINT64_MAX/UINT64_C(1000000000))*UINT64_C(100000000)+UINT64_C(70955162),UINT64_C(100000000))==0);
    const char *mode=argc>1?argv[1]:"deterministic";bool wall=!strcmp(mode,"wall"),race=!strcmp(mode,"race");
    uint64_t delay=wall?UINT64_C(50000000):UINT64_C(300000000000);
    kg_configure(UINT64_C(8)<<20,GC_HEAP_PAGE_BYTES,UINT64_C(6)<<20,4);
    kg_configure_uncommit_delay(delay);kg_init();KgRuntime *r=&kg_runtime;
    RootCell *root=kg_root_new();pthread_t child;
    if(race){assert(!pthread_create(&child,NULL,race_child,NULL));while(!atomic_load_explicit(&race_ready,memory_order_acquire)){kg_poll();kg_yield();}}
    kg_enter_native(r);uint64_t ownership=kg_driver_cleanup_enter(r,NULL);
    kg_leave_native(r);kg_alloc_into(UINT64_C(1)<<20,1,root);kg_store_i64(root,0,42);kg_root_clear(root);kg_enter_native(r);
    drive(r);const MutatorContext *actor=kg_driver_cleanup_actor(r);KgPolicy *p=kg_policy(r);
    uint64_t before=committed(r);assert(before>p->floor&&r->native.cycles.completed[0]&&r->native.cycles.completed[1]);
    if(wall){
        uint64_t start=kg_policy_clock();assert(start);kg_policy_poll(r,actor,start);kg_driver_cleanup_leave(r,ownership);
        bool decreased=false;uint64_t elapsed=0;
        while(elapsed<UINT64_C(3000000000)){
            uint64_t bytes=committed(r),now=kg_policy_clock();assert(now>=start);elapsed=now-start;
            if(elapsed<delay)assert(bytes==before);
            if(bytes<before){assert(elapsed>=delay);decreased=true;}
            if(bytes==GC_HEAP_PAGE_BYTES)break;
            struct timespec pause={0,1000000};nanosleep(&pause,NULL);
        }
        assert(decreased&&committed(r)==GC_HEAP_PAGE_BYTES);
        kg_leave_native(r);
    }else if(race){
        uint64_t start=UINT64_C(1000000000);kg_policy_poll(r,actor,start);r->idle_decommit=competing;
        kg_policy_poll(r,actor,start+delay);assert(atomic_load_explicit(&race_reserved,memory_order_acquire));
        r->idle_decommit=gc_native_heap_decommit_range;
        kg_driver_cleanup_leave(r,ownership);kg_leave_native(r);
        while(!atomic_load_explicit(&race_done,memory_order_acquire)){kg_poll();kg_yield();}
        kg_root_import(root,race_root);assert(kg_load_i64(root,0)==UINT64_C(0xabc01234));
        atomic_store_explicit(&race_release,1,memory_order_release);kg_native_enter();assert(!pthread_join(child,NULL));kg_native_exit();
    }else{
        uint64_t start=UINT64_C(1000000000);kg_policy_poll(r,actor,start);
        uint64_t observed=p->observations;assert(observed);
        kg_policy_poll(r,actor,0);kg_policy_poll(r,actor,start+delay-1);assert(committed(r)==before);
        kg_policy_poll(r,actor,start-1);assert(p->clock_resets);
        kg_policy_poll(r,actor,start+delay-2);assert(committed(r)==before);
        r->idle_decommit=!strcmp(mode,"permanent")?permanent:injected;
        kg_policy_poll(r,actor,start+delay-1);assert(failed_calls==3&&committed(r)==before-GC_HEAP_PAGE_BYTES);
        r->idle_decommit=gc_native_heap_decommit_range;
        /* A real reserve/abort changes incarnation without reaching prepare.
         * The next actual FREE observation must start a fresh waiting period. */
        PageLease lease={.page_count=1,.generation=GC_YOUNG};HeapPool *heap=&r->native.backing.heap;
        kg_lock(r);assert(gc_pool_step_locked(heap,&lease,GC_POOL_RESERVE)==GC_POOL_OK);
        uint64_t index=lease.head_index;bool cached=(p->states[index]&KG_CACHED)!=0;
        assert(gc_pool_step_locked(heap,&lease,GC_POOL_ABORT)==GC_POOL_OK);kg_unlock(r);
        for(uint64_t i=0;i<p->pages;i++)kg_policy_poll(r,actor,start+delay);
        if(cached)assert(kg_policy_times(p)[index].at==start+delay);
        assert(committed(r)>=p->floor);kg_lock(r);assert(kg_policy_check_locked(r)==KG_VERIFY_OK);kg_unlock(r);
        kg_driver_cleanup_leave(r,ownership);kg_leave_native(r);
        /* Actual reuse leaves a live payload accessible through normal guards. */
        kg_alloc_into(8,1,root);kg_store_i64(root,0,UINT64_C(0x12345678));assert(kg_load_i64(root,0)==UINT64_C(0x12345678));
    }
    kg_verify();kg_root_release(root);kg_shutdown();
    printf("canonical C delayed uncommit: %s actual clock/FREE lifetime/minimum/OS retry/shutdown PASS\n",mode);return 0;
}
