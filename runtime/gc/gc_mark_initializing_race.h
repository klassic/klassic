/* Header-publication contention with prevalidated fixture metadata/guards.
 * This checks the leaf contract; it is not a registered allocation scheduler.
 */
#ifndef KLASSIC_GC_MARK_INITIALIZING_RACE_H
#define KLASSIC_GC_MARK_INITIALIZING_RACE_H
#include "gc_mark_route_cases.h"
#include "gc_source_access.h"
#include "gc_initialization.h"
#ifdef _WIN32
#include <windows.h>
#else
#include <pthread.h>
#include <sched.h>
#endif
typedef struct {
    RouteFixture f;MarkRouter route;
    uint64_t (*access)(ForwardCell *,SourceAccessRequest *,uint64_t);
    _Atomic uint64_t waited;uint64_t scalar;
} InitializingRace;
static void initializing_yield(void){
#ifdef _WIN32
    SwitchToThread();
#else
    sched_yield();
#endif
}
static void initializing_access(uint64_t (*access)(ForwardCell *,SourceAccessRequest *,uint64_t),
        ForwardCell *cell,SourceAccessRequest *scope,uint64_t op){
    uint64_t status;
    while((status=access(cell,scope,op))!=GC_SA_OK){
        assert(status==GC_SA_WAIT);
        // EXIT retains a live guard on a lost CAS: retry without polling/yield.
        if(op==GC_SA_ENTER)initializing_yield();
    }
}
#ifdef _WIN32
static DWORD WINAPI initializing_reader(void *argument)
#else
static void *initializing_reader(void *argument)
#endif
{
    InitializingRace *r=argument;StoreFixture *s=&r->f.f.field;
    for(;;){
        GuardedMarkRoute before=r->f.q;uint64_t status=r->route(&r->f.q);
        if(status==GC_ROUTE_READY)break;
        assert(status==GC_ROUTE_WAIT&&!memcmp(&before,&r->f.q,sizeof(before)));
        assert(!s->record.young_epoch&&!s->record.old_epoch);
        initializing_access(r->access,&s->cell,&s->scope,GC_SA_EXIT);
        assert(!atomic_load(&s->ctx.access_epoch));
        atomic_store_explicit(&r->waited,1,memory_order_release);
        initializing_yield();
        initializing_access(r->access,&s->cell,&s->scope,GC_SA_ENTER);
    }
    /* READY's acquire of the published header orders all initializer writes. */
    const uint64_t *payload=(const uint64_t *)(s->heap+48);
    assert(atomic_load_explicit((const _Atomic uint64_t *)payload,memory_order_relaxed)==((UINT64_C(48)<<16)|0x1510)&&
        !atomic_load_explicit((const _Atomic uint64_t *)(payload+1),memory_order_relaxed)&&payload[2]==r->scalar);
    assert(!r->f.q.prepare.lease.token&&r->f.q.prepare.output.metadata_lease);
    initializing_access(r->access,&s->cell,&s->scope,GC_SA_EXIT);
    return 0;
}
static void gc_run_mark_initializing_races(MarkRouter route,
        uint64_t (*access)(ForwardCell *,SourceAccessRequest *,uint64_t),
        uint64_t (*publish)(const GuardedMarkPrepare *)){
    for(unsigned round=0;round<64;round++){
        InitializingRace r={.route=route,.access=access,.scalar=round+123};
        route_fixture(&r.f,round%2,1,0x70022|((round/2)<<8));
        r.f.f.field.page.owner=18;atomic_store(&r.f.f.field.page.state,GC_PAGE_ALLOCATING);
        MutatorContext initializer={.abi_version=GC_ABI_VERSION,.thread_id=18,.adopted_phase=r.f.q.prepare.phase};
        SourceAccessRequest scope={(uintptr_t)&initializer,48,27,0,0};
        GuardedMarkPrepare publication=r.f.q.prepare;publication.scope=(uintptr_t)&scope;
        ObjectHeader *h=(ObjectHeader *)(r.f.f.field.heap+16);
        atomic_store(&h->state,GC_OBJECT_INITIALIZING);
#ifdef _WIN32
        HANDLE thread=CreateThread(NULL,0,initializing_reader,&r,0,NULL);assert(thread);
#else
        pthread_t thread;assert(pthread_create(&thread,NULL,initializing_reader,&r)==0);
#endif
        while(!atomic_load_explicit(&r.waited,memory_order_acquire))initializing_yield();
        initializing_access(access,&r.f.f.field.cell,&scope,GC_SA_ENTER);
        uint64_t *payload=(uint64_t *)(r.f.f.field.heap+48);
        atomic_store_explicit((_Atomic uint64_t *)payload,(UINT64_C(48)<<16)|0x1510,memory_order_relaxed);
        atomic_store_explicit((_Atomic uint64_t *)(payload+1),0,memory_order_relaxed);payload[2]=r.scalar;
        assert(publish(&publication)==GC_INIT_OK);
        initializing_access(access,&r.f.f.field.cell,&scope,GC_SA_EXIT);
#ifdef _WIN32
        assert(WaitForSingleObject(thread,INFINITE)==WAIT_OBJECT_0);assert(CloseHandle(thread));
#else
        assert(pthread_join(thread,NULL)==0);
#endif
        assert(!atomic_load(&r.f.f.field.ctx.access_epoch)&&!atomic_load(&r.f.f.field.cell.access));
        assert(r.f.q.prepare.output.record==(uintptr_t)&r.f.f.field.record);
    }
    puts("mark initialization: 64 two-actor waits preserve input ownership until release/acquire ready publication PASS");
}
#endif
