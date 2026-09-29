#ifndef KLASSIC_GC_WEAK_MARK_CASES_H
#define KLASSIC_GC_WEAK_MARK_CASES_H
#include "gc_weak_mark.h"
#include "gc_mark_phase_cases.h"
#include "gc_weak_release_cases.h"
typedef struct {
    PhaseFixture phase;WeakLoadFixture weak;WeakMarkControl owner;
    RootedLoadAdmission admission;RootRequest output;WeakLoadRequest request;
    WeakMarkPhaseStep step;WeakLoader load;WeakMarkStepper owner_step;
    _Atomic unsigned ready,turn;unsigned load_first;uint64_t loaded,begun;
#ifdef _WIN32
    CRITICAL_SECTION lock;
#else
    pthread_mutex_t lock;
#endif
} WeakMarkRace;
typedef struct {WeakMarkRace *race;unsigned loading;} WeakMarkActor;
static void weak_mark_fixture_phase(WeakMarkRace *r,unsigned generation,uint64_t phase){
    memset(r,0,sizeof(*r));phase_fixture(&r->phase,phase,generation,0);weak_load_fixture(&r->weak);
    AuditFixture *a=&r->phase.e.f;
    for(unsigned i=0;i<3;i++)atomic_store(&a->cells[i].value,0);
    uint64_t audit=gc_root_audit_step_locked(&a->g,&a->a,GC_RA_BEGIN);
    while(audit==GC_RA_ITEM){
        uint64_t value=1;RootWalkScan scan={(uintptr_t)&a->a.walk,a->a.walk.lease.serial,(uintptr_t)&value,1,0};
        assert(gc_root_walk_scan(&scan)==0&&scan.written==1&&!value);
        a->a.walk.completed_mask=a->a.walk.mask;
        audit=gc_root_audit_step_locked(&a->g,&a->a,GC_RA_NEXT);
    }
    assert(audit==GC_RA_DONE);
    for(unsigned i=0;i<2;i++)atomic_store(&a->ctx[i].state,GC_RUNNING);
    r->weak.gate.registry=(uintptr_t)&a->g;r->weak.gate.handshake=(uintptr_t)&r->phase.e.h;
    r->owner=(WeakMarkControl){.gate=&r->weak.gate,.mark=&r->phase.q};
    r->admission=(RootedLoadAdmission){.registry=(uintptr_t)&a->g,.handshake=(uintptr_t)&r->phase.e.h,.participant=(uintptr_t)&r->phase.e.p[0]};
    r->output=(RootRequest){.segment=(uintptr_t)&a->s[0],.handle={(uintptr_t)&a->cells[0],1,a->s[0].scope}};
    r->request=(WeakLoadRequest){(uintptr_t)&r->admission,(uintptr_t)&a->ctx[0],(uintptr_t)&r->weak.slot,11,(uintptr_t)&r->output};
}
static void weak_mark_fixture(WeakMarkRace *r,unsigned generation){weak_mark_fixture_phase(r,generation,0x90022);}

#ifdef _WIN32
static DWORD WINAPI weak_mark_actor(void *argument){
#else
static void *weak_mark_actor(void *argument){
#endif
    WeakMarkActor *a=argument;WeakMarkRace *r=a->race;unsigned order=a->loading==r->load_first?0:1;
    atomic_fetch_add_explicit(&r->ready,1,memory_order_release);
    while(atomic_load_explicit(&r->ready,memory_order_acquire)!=2||atomic_load_explicit(&r->turn,memory_order_acquire)!=order)weak_release_yield();
#ifdef _WIN32
    EnterCriticalSection(&r->lock);
#else
    assert(!pthread_mutex_lock(&r->lock));
#endif
    if(a->loading)r->loaded=r->load(&r->weak.gate,&r->request,0);
    else r->begun=r->owner_step?r->owner_step(&r->phase.e.f.g,&r->owner,GC_MP_BEGIN):gc_weak_mark_end_locked(&r->phase.e.f.g,&r->owner,GC_MP_BEGIN,r->step);
#ifdef _WIN32
    LeaveCriticalSection(&r->lock);
#else
    assert(!pthread_mutex_unlock(&r->lock));
#endif
    atomic_store_explicit(&r->turn,order+1,memory_order_release);return 0;
}
static void gc_run_weak_mark_cases(WeakMarkPhaseStep candidate,WeakLoader load_candidate,WeakMarkStepper owner_candidate){
    for(unsigned impl=0;impl<2;impl++){
        WeakMarkPhaseStep step=impl?candidate:gc_mark_phase_step_locked;
        WeakMarkStepper owner_step=impl?owner_candidate:gc_weak_mark_step_locked;
        WeakLoader load=impl?load_candidate:gc_weak_load_into_locked;
        for(unsigned generation=0;generation<2;generation++)for(unsigned round=0;round<16;round++){
            WeakMarkRace r;weak_mark_fixture(&r,generation);r.step=step;r.owner_step=owner_step;r.load=load;r.load_first=round&1;
            WeakMarkActor actors[]={{&r,1},{&r,0}};
#ifdef _WIN32
            InitializeCriticalSection(&r.lock);HANDLE threads[2];
            for(unsigned i=0;i<2;i++){threads[i]=CreateThread(NULL,0,weak_mark_actor,&actors[i],0,NULL);assert(threads[i]);}
            for(unsigned i=0;i<2;i++){assert(WaitForSingleObject(threads[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(threads[i]));}
            DeleteCriticalSection(&r.lock);
#else
            assert(!pthread_mutex_init(&r.lock,NULL));pthread_t threads[2];
            for(unsigned i=0;i<2;i++)assert(!pthread_create(&threads[i],NULL,weak_mark_actor,&actors[i]));
            for(unsigned i=0;i<2;i++)assert(!pthread_join(threads[i],NULL));
            assert(!pthread_mutex_destroy(&r.lock));
#endif
            assert(r.begun==GC_MP_ADVANCED&&r.owner.stage==1&&r.weak.gate.closed);
            assert(r.loaded==(r.load_first?GC_WEAK_LOADED:GC_WEAK_WAIT));
            AuditFixture *a=&r.phase.e.f;
            assert(atomic_load(&a->buffers[0].published)==r.load_first);
            assert(atomic_load(&a->cells[0].value)==(r.load_first?r.weak.slot.reference:0));
            assert(owner_step(&a->g,&r.owner,GC_MP_DECIDE)==GC_MP_WAIT);
            phase_ack(&r.phase);
            assert(owner_step(&a->g,&r.owner,GC_MP_DECIDE)==GC_MP_ADVANCED);
            assert(r.phase.q.outcome==(r.load_first?2u:4u)&&r.owner.stage==2&&r.weak.gate.closed);
            assert(owner_step(&a->g,&r.owner,GC_MP_RELEASE)==GC_MP_WAIT);
            assert(load(&r.weak.gate,&r.request,0)==GC_WEAK_WAIT);
            phase_ack(&r.phase);
            assert(owner_step(&a->g,&r.owner,GC_MP_RELEASE)==(r.load_first?GC_MP_RETRY:GC_MP_SELECT));
            phase_resume(&r.phase);
            assert(r.owner.stage==(r.load_first?0u:3u)&&r.weak.gate.closed==!r.load_first);
            if(!r.load_first){
                assert(load(&r.weak.gate,&r.request,0)==GC_WEAK_WAIT);
                assert(owner_step(&a->g,&r.owner,GC_MP_BEGIN)==GC_MP_INVALID);
                assert(!atomic_load(&a->cells[0].value));
            }else{
                /* The published root and journal remain pending strong work;
                 * this test never fabricates an ACK to force termination. */
                assert(atomic_load(&a->buffers[0].published)==1&&!atomic_load(&a->buffers[0].consumed));
                assert(atomic_load(&a->cells[0].value)==r.weak.slot.reference);
            }
            assert(gc_weak_release_locked(&r.weak.gate,&r.weak.slot,11)==GC_WEAK_RELEASED);
        }
        for(unsigned fault=0;fault<10;fault++){
            WeakMarkRace r;weak_mark_fixture(&r,0);uint64_t op=GC_MP_BEGIN,expected=GC_MP_INVALID;
            switch(fault){
            case 0:r.weak.gate.registry+=8;break;
            case 1:r.weak.gate.handshake+=8;break;
            case 2:r.owner.stage=3;break;
            case 3:r.weak.gate.closed=2;break;
            case 4:r.weak.gate.pool=0;break;
            case 5:r.weak.gate.closed=1;expected=GC_MP_WAIT;break;
            case 6:r.phase.q.generation=2;break;
            case 7:r.phase.e.h.generation=UINT64_MAX;expected=GC_MP_EXHAUSTED;break;
            case 8:r.owner.stage=1;op=GC_MP_DECIDE;break;
            case 9:op=3;break;
            }
            WeakMarkRace before;memcpy(&before,&r,sizeof(r));
            assert(owner_step(&r.phase.e.f.g,&r.owner,op)==expected);
            assert(!memcmp(&before,&r,sizeof(r)));
        }
    }
    puts("weak mark admission: 64 two-actor load/end schedules and 20 failure-atomic C/emitted owner cases PASS");
}
#endif
