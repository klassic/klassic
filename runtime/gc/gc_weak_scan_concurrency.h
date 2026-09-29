#ifndef KLASSIC_GC_WEAK_SCAN_CONCURRENCY_H
#define KLASSIC_GC_WEAK_SCAN_CONCURRENCY_H
#include "gc_weak_scan_cases.h"
/* Two distinct registered context owners. Test hooks wait only outside guards
 * and outside the common lock. The collector has already completed mark end;
 * the frozen fixture excludes reclamation while this weak phase is in flight.
 */
typedef struct {
    WeakScanFixture fixture;WeakScan scan;WeakProcessOps process;WeakTableOps table;
    WeakProcessAccess access;WeakProcessFinish finish;WeakScanStepper scan_step;
    RootedLoadAdmission admission;RootRequest output;_Atomic unsigned stage;
    unsigned release_dead;uint64_t final_dead,final_live;
#ifdef _WIN32
    CRITICAL_SECTION lock;
#else
    pthread_mutex_t lock;
#endif
} WeakScanRace;
static void weak_scan_race_lock(WeakScanRace *r){
#ifdef _WIN32
    EnterCriticalSection(&r->lock);
#else
    assert(!pthread_mutex_lock(&r->lock));
#endif
}
static void weak_scan_race_unlock(WeakScanRace *r){
#ifdef _WIN32
    LeaveCriticalSection(&r->lock);
#else
    assert(!pthread_mutex_unlock(&r->lock));
#endif
}
static void weak_scan_race_wait(WeakScanRace *r,unsigned stage,unsigned actor){
    assert(!atomic_load_explicit(&r->fixture.r.phase.e.f.ctx[actor].access_epoch,memory_order_acquire));
    while(atomic_load_explicit(&r->stage,memory_order_acquire)!=stage)weak_release_yield();
}
static void weak_scan_race_hook(WeakScanRace *r,unsigned stage){
    atomic_store_explicit(&r->stage,stage,memory_order_release);weak_scan_race_wait(r,stage+1,0);
}
#ifdef _WIN32
static DWORD WINAPI weak_scan_race_worker(void *argument){
#else
static void *weak_scan_race_worker(void *argument){
#endif
    WeakScanRace *r=argument;WeakScanFixture *f=&r->fixture;
    MutatorContext *ctx=&f->r.phase.e.f.ctx[0];
    weak_scan_race_lock(r);
    WeakScanStart start={&f->table,&f->r.owner,ctx};
    assert(r->scan_step(&r->scan,&start,GC_WS_BEGIN)==GC_WP_OK);
    weak_scan_race_unlock(r);unsigned turns=0;
    for(;;){
        assert(++turns<32);weak_scan_race_lock(r);
        uint64_t result=r->scan_step(&r->scan,NULL,GC_WS_NEXT);
        weak_scan_race_unlock(r);
        if(result==GC_WS_DONE)break;
        if(result==GC_WP_ADVANCED)continue;
        assert(result==GC_WP_OK);unsigned hops=0;
        do{
            assert(++hops<4);weak_scan_race_lock(r);
            result=r->access(&r->scan.work,GC_WPA_ENTER,0);
            weak_scan_race_unlock(r);
        }while(result==GC_WP_ADVANCED);
        assert(result==GC_WP_OK);
        result=r->access(&r->scan.work,GC_WPA_OBSERVE,0);
        assert(result==(r->scan.cursor?GC_WP_DEAD:GC_WP_LIVE));
        assert(r->access(&r->scan.work,GC_WPA_EXIT,0)==GC_WP_OK);
        if(r->scan.cursor==1)weak_scan_race_hook(r,1);
        weak_scan_race_lock(r);
        assert(r->finish(&r->scan.work,GC_WPF_COMMIT,0)==(r->release_dead==1&&r->scan.cursor==1?GC_WP_STALE:result));
        weak_scan_race_unlock(r);
        if(r->scan.cursor==1)weak_scan_race_hook(r,3);
    }
    weak_scan_race_hook(r,5);
    weak_scan_race_lock(r);
    assert(r->scan_step(&r->scan,NULL,GC_WS_FINISH)==GC_WS_DONE);
    weak_scan_race_unlock(r);
    atomic_store_explicit(&r->stage,7,memory_order_release);return 0;
}
#ifdef _WIN32
static DWORD WINAPI weak_scan_race_loader(void *argument){
#else
static void *weak_scan_race_loader(void *argument){
#endif
    WeakScanRace *r=argument;WeakScanFixture *f=&r->fixture;AuditFixture *a=&f->r.phase.e.f;
    MutatorContext *ctx=&a->ctx[1];WeakHandle dead={(uintptr_t)&f->table,2,12},live={(uintptr_t)&f->table,1,11};
    weak_scan_race_wait(r,1,1);weak_scan_race_lock(r);
    assert(f->entries[1].slot.reference&&f->r.weak.f.pool.active==3);
    assert(gc_weak_table_load_locked(&f->table,dead,&r->admission,ctx,&r->output,&r->table)==GC_WEAK_WAIT);
    assert(!atomic_load(&a->cells[1].value)&&!atomic_load(&a->buffers[1].published));
    if(r->release_dead==1)assert(gc_weak_table_release_locked(&f->table,dead,&r->table)==GC_WEAK_RELEASED);
    weak_scan_race_unlock(r);atomic_store_explicit(&r->stage,2,memory_order_release);
    weak_scan_race_wait(r,3,1);weak_scan_race_lock(r);
    assert(!f->entries[1].slot.reference&&f->entries[1].slot.serial==(r->release_dead==1?0u:12u)&&f->r.weak.f.pool.active==1);
    assert(gc_weak_table_load_locked(&f->table,dead,&r->admission,ctx,&r->output,&r->table)==(r->release_dead==1?GC_WEAK_INVALID:GC_WEAK_WAIT));
    if(r->release_dead==2)assert(gc_weak_table_release_locked(&f->table,dead,&r->table)==GC_WEAK_RELEASED);
    assert(!atomic_load(&a->cells[1].value)&&!atomic_load(&a->buffers[1].published));
    weak_scan_race_unlock(r);atomic_store_explicit(&r->stage,4,memory_order_release);
    weak_scan_race_wait(r,5,1);weak_scan_race_lock(r);
    assert(f->table.gate->closed&&r->scan.stage==2&&!f->r.owner.processor);
    assert(gc_weak_table_load_locked(&f->table,live,&r->admission,ctx,&r->output,&r->table)==GC_WEAK_WAIT);
    assert(!atomic_load(&a->cells[1].value)&&!atomic_load(&a->buffers[1].published));
    weak_scan_race_unlock(r);atomic_store_explicit(&r->stage,6,memory_order_release);
    weak_scan_race_wait(r,7,1);weak_scan_race_lock(r);
    r->final_dead=gc_weak_table_load_locked(&f->table,dead,&r->admission,ctx,&r->output,&r->table);
    assert(r->final_dead==(r->release_dead?GC_WEAK_INVALID:GC_WEAK_EMPTY));
    assert(!atomic_load(&a->cells[1].value)&&!atomic_load(&a->buffers[1].published));
    r->final_live=gc_weak_table_load_locked(&f->table,live,&r->admission,ctx,&r->output,&r->table);
    assert(r->final_live==GC_WEAK_LOADED&&r->output.value==f->live);
    assert(atomic_load(&a->cells[1].value)==f->live&&atomic_load(&a->buffers[1].published)==1&&!atomic_load(&a->buffers[1].consumed));
    assert(a->changes[1].previous==0&&a->changes[1].next==f->live);
    weak_scan_race_unlock(r);return 0;
}
static void gc_run_weak_scan_concurrency(const WeakProcessOps *candidate,const WeakTableOps *table_candidate,WeakProcessAccess access_candidate,WeakProcessFinish finish_candidate,WeakScanStepper scan_candidate){
    const WeakProcessOps c={gc_reference_lease_locked,gc_pending_follow_locked,gc_pending_access_locked,
        gc_resolve_access,gc_guarded_weak_liveness,gc_metadata_step_locked};
    const WeakTableOps tc={gc_root_step,gc_reference_lease_locked,gc_weak_load_into_locked,gc_weak_release_locked};
    for(unsigned impl=0;impl<2;impl++)for(unsigned generation=0;generation<2;generation++)
    for(unsigned mode=0;mode<6;mode++)for(unsigned round=0;round<8;round++){
        WeakScanRace r={0};weak_scan_fixture(&r.fixture,generation,mode&1);
        r.scan_step=impl?scan_candidate:gc_weak_scan_step_locked;
        r.access=impl?access_candidate:gc_weak_process_access_step;r.finish=impl?finish_candidate:gc_weak_process_finish_locked;
        r.process=impl?*candidate:c;r.table=impl?*table_candidate:tc;r.release_dead=mode>>1;
        AuditFixture *a=&r.fixture.r.phase.e.f;
        r.admission=(RootedLoadAdmission){.registry=(uintptr_t)&a->g,.handshake=(uintptr_t)&r.fixture.r.phase.e.h,
            .participant=(uintptr_t)&r.fixture.r.phase.e.p[1]};
        r.output=(RootRequest){.segment=(uintptr_t)&a->s[1],.handle={(uintptr_t)&a->cells[1],1,a->s[1].scope}};
#ifdef _WIN32
        InitializeCriticalSection(&r.lock);HANDLE worker=CreateThread(NULL,0,weak_scan_race_worker,&r,0,NULL);
        HANDLE loader=CreateThread(NULL,0,weak_scan_race_loader,&r,0,NULL);assert(worker&&loader);
        assert(WaitForSingleObject(worker,30000)==WAIT_OBJECT_0&&WaitForSingleObject(loader,30000)==WAIT_OBJECT_0);
        assert(CloseHandle(worker)&&CloseHandle(loader));DeleteCriticalSection(&r.lock);
#else
        assert(!pthread_mutex_init(&r.lock,NULL));pthread_t worker,loader;
        assert(!pthread_create(&worker,NULL,weak_scan_race_worker,&r));assert(!pthread_create(&loader,NULL,weak_scan_race_loader,&r));
        assert(!pthread_join(worker,NULL)&&!pthread_join(loader,NULL));assert(!pthread_mutex_destroy(&r.lock));
#endif
        assert(r.scan.stage==3&&!r.fixture.table.gate->closed&&!r.fixture.r.owner.processor&&!r.fixture.r.owner.scan);
        assert(!atomic_load(&a->ctx[0].access_epoch)&&!atomic_load(&a->ctx[1].access_epoch));
        for(unsigned i=0;i<4;i++)if(r.fixture.entries[i].slot.serial){
            WeakHandle h={(uintptr_t)&r.fixture.table,i+1,r.fixture.entries[i].slot.serial};
            assert(gc_weak_table_release_locked(&r.fixture.table,h,&r.table)==GC_WEAK_RELEASED);
        }
        assert(!r.fixture.r.weak.f.pool.active&&!r.fixture.r.weak.f.page.metadata_leases);
        free(r.fixture.heap);
    }
    puts("weak scan race: 192 two-actor C/candidate clear, closed-load, reopened-root and release schedules PASS");
}
#endif
