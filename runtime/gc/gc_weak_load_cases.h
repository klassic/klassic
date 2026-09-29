#ifndef KLASSIC_GC_WEAK_LOAD_CASES_H
#define KLASSIC_GC_WEAK_LOAD_CASES_H
#include "gc_weak_load.h"
#include "gc_root_follow_cases.h"
#include "gc_root_lifetime.h"
typedef struct {
    FollowFixture f;RootRegistry roots;RootRegistration registration;
    HandshakeState handshake;ThreadRegistration participant;RootedLoadAdmission admission;
    RootRequest output;WeakSlot slot;WeakLoadGate gate;WeakLoadRequest request;
} WeakLoadFixture;
static void weak_load_fixture(WeakLoadFixture *w){
    memset(w,0,sizeof(*w));follow_fixture(&w->f,0x10022,1);
    uint64_t reference=atomic_load(&w->f.cell.value);
    w->f.cell=(RootCell){0};w->f.journal.top=0;
    atomic_store(&w->f.journal.published,0);atomic_store(&w->f.journal.consumed,0);
    w->f.segment=(RootSegment){.owner=w->f.ctx.thread_id,.cells=(uintptr_t)&w->f.cell,.capacity=1,.journal=(uintptr_t)&w->f.journal};
    w->roots.phase=0x10022;w->registration.segment=(uintptr_t)&w->f.segment;
    RootLifeRequest life={.registration=(uintptr_t)&w->registration,.context=(uintptr_t)&w->f.ctx};
    assert(gc_root_life_locked(&w->roots,&life,GC_RL_ATTACH)==GC_RL_OK);
    w->output.segment=(uintptr_t)&w->f.segment;
    assert(gc_root_step(&w->f.ctx,&w->output,GC_ROOT_RESERVE)==GC_ROOT_OK);
    w->handshake=(HandshakeState){.generation=1,.members=1,.last_identity=w->f.ctx.thread_id,.head=(uintptr_t)&w->participant};
    w->participant=(ThreadRegistration){.context=(uintptr_t)&w->f.ctx,.identity=w->f.ctx.thread_id,
        .registry=(uintptr_t)&w->handshake,.registered=1,.acknowledged_generation=1};
    w->admission=(RootedLoadAdmission){.registry=(uintptr_t)&w->roots,.handshake=(uintptr_t)&w->handshake,.participant=(uintptr_t)&w->participant};
    ReferenceLease held={.reference=reference};assert(gc_reference_lease_locked(&w->f.pool,&held)==GC_ML_OK);
    w->slot=(WeakSlot){.gate=(uintptr_t)&w->gate,.serial=11,.reference=reference,.lease=held.lease};
    w->gate=(WeakLoadGate){.pool=(uintptr_t)&w->f.pool,.registry=(uintptr_t)&w->roots,.handshake=(uintptr_t)&w->handshake};
    w->request=(WeakLoadRequest){(uintptr_t)&w->admission,(uintptr_t)&w->f.ctx,(uintptr_t)&w->slot,11,(uintptr_t)&w->output};
}
static void gc_run_weak_load_cases(WeakLoader candidate){
    for(unsigned impl=0;impl<2;impl++)for(unsigned mode=0;mode<29;mode++){
        WeakLoader step=impl?candidate:gc_weak_load_into_locked;
        WeakLoadFixture w;weak_load_fixture(&w);uint64_t expected=GC_WEAK_INVALID,op=0;
        switch(mode){
        case 0:expected=GC_WEAK_LOADED;break;
        case 1:assert(gc_metadata_step_locked(&w.f.pool,&w.slot.lease,GC_ML_RELEASE)==GC_ML_OK);w.slot.reference=0;expected=GC_WEAK_EMPTY;break;
        case 2:w.gate.closed=1;expected=GC_WEAK_WAIT;break;
        case 3:w.request.serial++;break;
        case 4:w.slot.lease.token+=UINT64_C(1)<<32;break;
        case 5:w.slot.lease.page+=8;break;
        case 6:atomic_store(&w.f.cell.value,w.slot.reference);break;
        case 7:w.f.journal.top=w.f.journal.capacity;atomic_store(&w.f.journal.published,w.f.journal.top);expected=GC_WEAK_FLUSH;break;
        case 8:atomic_store(&w.f.ctx.root_revision,UINT64_MAX);expected=GC_WEAK_EXHAUSTED;break;
        case 9:w.f.segment.registration=0;break;
        case 10:atomic_store(&w.f.ctx.adopted_phase,0x20022);expected=GC_WEAK_WAIT;break;
        case 11:atomic_store(&w.f.ctx.state,GC_BLOCKED_NATIVE);break;
        case 12:w.handshake.active=1;w.handshake.generation=2;w.handshake.expected=1;expected=GC_WEAK_WAIT;break;
        case 13:w.participant.registered=0;break;
        case 14:atomic_store(&w.f.ctx.access_epoch,27);break;
        case 15:w.f.ctx.thread_id=0;break;
        case 16:w.gate.closed=2;break;
        case 17:w.output.handle.reuse_generation++;break;
        case 18:op=1;break;
        case 19:w.slot.reference=0;break;
        case 20:w.registration.context=0;break;
        case 21:w.participant.acknowledged_generation=2;break;
        case 22:w.slot.reference|=1;break;
        case 23:w.slot.reference+=UINT64_C(131072)<<16;break;
        case 24:atomic_store(&w.f.starts[0],0);break;
        case 25:w.f.page.starts=0;break;
        case 26:w.slot.gate+=8;break;
        case 27:w.gate.registry+=8;break;
        case 28:w.gate.handshake+=8;break;
        }
        WeakLoadFixture before;memcpy(&before,&w,sizeof(w));
        uint64_t status=step(&w.gate,&w.request,op);
        if(status!=expected){fprintf(stderr,"weak load mode %u impl %u got %llu expected %llu\n",mode,impl,(unsigned long long)status,(unsigned long long)expected);abort();}
        if(status!=GC_WEAK_LOADED)assert(!memcmp(&before,&w,sizeof(w)));
        else{
            assert(atomic_load(&w.f.cell.value)==w.slot.reference&&w.output.value==w.slot.reference);
            assert(w.f.journal.top==1&&atomic_load(&w.f.journal.published)==1&&!atomic_load(&w.f.journal.consumed));
            assert(!w.f.entries[0].previous&&w.f.entries[0].next==w.slot.reference&&w.f.entries[0].phase==w.roots.phase);
            assert(atomic_load(&w.f.ctx.root_revision)==atomic_load(&before.f.ctx.root_revision)+1);
            assert(!memcmp(&w.slot,&before.slot,sizeof(w.slot))&&!memcmp(&w.f.pool,&before.f.pool,sizeof(w.f.pool)));
            assert(step(&w.gate,&w.request,0)==GC_WEAK_INVALID); // output must be empty
            w.gate.closed=1;memcpy(&before,&w,sizeof(w));
            assert(step(&w.gate,&w.request,0)==GC_WEAK_WAIT&&!memcmp(&before,&w,sizeof(w)));
        }
    }
    puts("weak load: 58 C/candidate rooted publication, closed admission, stale handle, journal pressure and metadata rejection cases PASS");
}
#endif
