/* Actual facade weak handles, roots, cycles and native ownership. */
#include <stdatomic.h>
static void weak_shutdown_after_drop(void *);
static void weak_commit_observed(void *,void *);
static void weak_load_wait_observed(void *);
static void weak_before_load(void *);
static void weak_driver_stop_checked(void *);
static void weak_driver_worker_stepped(void *);
static void weak_driver_before_terminal(void *);
static void weak_driver_terminal(void *);
static void weak_observe_trace(const void *,uint64_t,uint64_t,uint64_t);
#define KG_WEAK_AFTER_DROP_HOOK(r) weak_shutdown_after_drop(r)
#define KG_WEAK_PROCESS_COMMIT_HOOK(r,q) weak_commit_observed(r,q)
#define KG_WEAK_LOAD_WAIT_HOOK(r,h) weak_load_wait_observed(r)
#define KG_WEAK_BEFORE_LOAD_HOOK(r) weak_before_load(r)
#define KG_WEAK_OBSERVE_HOOK(a,g,e,s) weak_observe_trace(a,g,e,s)
#include "klassic_gen_runtime.c"
#define KG_DRIVER_AFTER_STOP_CHECK_HOOK(d) weak_driver_stop_checked(d)
#define KG_DRIVER_AFTER_WORKER_STEP_HOOK(d) weak_driver_worker_stepped(d)
#define KG_DRIVER_BEFORE_TERMINAL_HOOK(d) weak_driver_before_terminal(d)
#define KG_DRIVER_TERMINAL_HOOK(d) weak_driver_terminal(d)
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
#include <stdlib.h>
static void weak_major(void){kg_driver_pressure(&kg_runtime,KG_PRESSURE_METADATA);}
static void weak_young(void){
    KgRuntime *r=&kg_runtime;kg_lock(r);KgDriver *d=r->driver;uint64_t target=r->native.cycles.completed[0]+1;d->requested[0]=1;kg_unlock(r);kgd_wait(r,target,0);
}
#include "klassic_gen_weak_race_test.h"
int main(int argc,char **argv){
    if(argc>1&&!strcmp(argv[1],"transport")){weak_transport_counterexample();return 0;}
    if(argc>1&&!strcmp(argv[1],"gate")){weak_closed_gate();return 0;}
    if(argc>1&&!strcmp(argv[1],"faults")){weak_faults();return 0;}
    if(argc>1&&!strcmp(argv[1],"shutdown-scan")){weak_shutdown_scan();return 0;}
    if(argc>1&&(!strcmp(argv[1],"2")||!strcmp(argv[1],"8"))){weak_nowners((unsigned)strtoul(argv[1],NULL,10));return 0;}
    bool growth=argc>1&&!strcmp(argv[1],"growth");
    kg_configure(UINT64_C(16)*1024*1024,0,UINT64_C(8)*1024*1024,4);kg_init();
    RootCell *source=kg_root_new(),*out=kg_root_new();
    kg_alloc_into(8,1,source);kg_store_i64(source,0,7777);KgWeakHandle dead=kg_weak_new(source);kg_root_clear(source);
    weak_drain_idle_transport();weak_major();assert(!kg_weak_load_into(dead,out));assert(kg_weak_release(dead));assert(!kg_weak_release(dead));
    kg_alloc_into(8,1,source);kg_store_i64(source,0,8888);KgWeakHandle live=kg_weak_new(source);
    assert(live.index==dead.index&&live.serial!=dead.serial);assert(!kg_weak_load_into(dead,out));
    kg_root_clear(source);assert(kg_weak_load_into(live,out)&&kg_load_i64(out,0)==8888);
    weak_major();assert(kg_load_i64(out,0)==8888);assert(kg_weak_load_into(live,source)&&kg_load_i64(source,0)==8888);
    kg_root_clear(out);kg_root_clear(source);weak_drain_idle_transport();weak_major();assert(!kg_weak_load_into(live,out));assert(kg_weak_release(live));
    kg_alloc_into(8,1,source);kg_store_i64(source,0,9999);weak_major();weak_major();
    KgWeakHandle old=kg_weak_new(source);kg_root_clear(source);weak_young();
    assert(kg_weak_load_into(old,out)&&kg_load_i64(out,0)==9999);kg_root_clear(out);weak_drain_idle_transport();weak_major();
    assert(!kg_weak_load_into(old,out));assert(kg_weak_release(old));
    if(growth){
        enum { COUNT=8193 };KgWeakHandle *handles=calloc(COUNT,sizeof(*handles));assert(handles);
        kg_alloc_into(8,1,source);kg_store_i64(source,0,123456);
        kg_lock(&kg_runtime);WeakEntry *base=kgw_owner(&kg_runtime)->table.entries;kg_unlock(&kg_runtime);
        for(unsigned i=0;i<COUNT;i++)handles[i]=kg_weak_new(source);
        kg_lock(&kg_runtime);assert(kgw_owner(&kg_runtime)->table.entries==base&&kgw_owner(&kg_runtime)->table.capacity>=COUNT);
        assert(kg_leases(&kg_runtime)->capacity>8192);kg_unlock(&kg_runtime);
        atomic_store_explicit(&weak_diag_serial,handles[0].serial,memory_order_release);
        weak_major();for(unsigned i=0;i<COUNT;i+=257)assert(kg_weak_load_into(handles[i],out)&&kg_load_i64(out,0)==123456);
        weak_epoch_trace("before-clear",source,out);kg_root_clear(source);kg_root_clear(out);weak_epoch_trace("after-clear",source,out);weak_drain_idle_transport();weak_major();weak_epoch_trace("after-major",source,out);
        for(unsigned i=0;i<COUNT;i++){assert(!kg_weak_load_into(handles[i],out));assert(kg_weak_release(handles[i]));}
        free(handles);
    }
    kg_verify();kg_root_release(out);kg_root_release(source);kg_shutdown();
    assert(!kg_weak_inventory&&!kg_runtime.native.allocations&&!kg_runtime.native.backing.heap.metadata_committed&&!kg_runtime.native.backing.heap.metadata_pending);
    puts("C weak runtime: actual weak-only collection, rooted keepalive, Young/Old movement, serial ABA and final budgets0 PASS");
    return 0;
}
