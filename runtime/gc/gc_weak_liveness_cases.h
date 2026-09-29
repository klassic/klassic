#ifndef KLASSIC_GC_WEAK_LIVENESS_CASES_H
#define KLASSIC_GC_WEAK_LIVENESS_CASES_H
#include "gc_weak_liveness.h"
#include "gc_mark_prepare_cases.h"
#include "gc_weak_mark_cases.h"
static void gc_run_weak_liveness_cases(WeakLiveness candidate){
    for(unsigned impl=0;impl<2;impl++){
        WeakLiveness step=impl?candidate:gc_guarded_weak_liveness;
        const uint64_t stamps[]={0,60,61,62};
        for(unsigned gen=0;gen<2;gen++)for(unsigned object=0;object<2;object++)
        for(unsigned stamp=0;stamp<4;stamp++)for(unsigned init=0;init<2;init++){
            PrepareFixture f;prepare_fixture(&f,0x90044,object);
            f.field.record.young_epoch=f.field.record.old_epoch=stamps[stamp];
            if(init)atomic_store(&((ObjectHeader *)(f.field.heap+16))->state,GC_OBJECT_INITIALIZING);
            PrepareFixture before;memcpy(&before,&f,sizeof(f));
            uint64_t expected=init?GC_WEAK_LIVENESS_WAIT:gen==0&&object==1?GC_WEAK_LIVE:
                stamps[stamp]>61?GC_WEAK_LIVENESS_INVALID:stamps[stamp]==61?GC_WEAK_LIVE:GC_WEAK_DEAD;
            assert(step(&f.q,gen,61)==expected&&!memcmp(&before,&f,sizeof(f)));
        }
        for(unsigned fault=0;fault<20;fault++){
            PrepareFixture f;prepare_fixture(&f,0x90044,0);const GuardedMarkPrepare *q=&f.q;
            uint64_t gen=0,epoch=61;
            switch(fault){
            case 0:q=NULL;break;case 1:q=(void *)((char *)q+1);break;case 2:gen=2;break;case 3:epoch=0;break;
            case 4:f.q.phase=0x90022;atomic_store(&f.field.ctx.adopted_phase,f.q.phase);break;
            case 5:atomic_store(&f.field.ctx.adopted_phase,0);break;
            case 6:atomic_store(&f.field.ctx.access_epoch,0);break;
            case 7:f.field.scope.held=0;break;case 8:atomic_store(&f.field.cell.word,2);break;
            case 9:f.q.reference=1;break;case 10:f.q.lease.incarnation++;break;case 11:f.q.lease.token=0;break;
            case 12:f.field.record.object_id=0;break;case 13:((ObjectHeader *)(f.field.heap+16))->mark_record=0;break;
            case 14:f.q.output.record=8;break;case 15:f.field.record.young_epoch=62;break;
            case 16:f.q.page=0;break;case 17:f.field.page.generation=2;break;case 18:f.field.page.incarnation++;break;
            case 19:atomic_store(&((ObjectHeader *)(f.field.heap+16))->state,7);break;
            }
            PrepareFixture before;memcpy(&before,&f,sizeof(f));
            assert(step(q,gen,epoch)==GC_WEAK_LIVENESS_INVALID&&!memcmp(&before,&f,sizeof(f)));
        }
        for(unsigned fault=0;fault<16;fault++){
            WeakMarkRace r;weak_mark_fixture(&r,0);RootRegistry *g=&r.phase.e.f.g;
            assert(gc_weak_mark_end_locked(g,&r.owner,GC_MP_BEGIN,gc_mark_phase_step_locked)==GC_MP_ADVANCED);phase_ack(&r.phase);
            assert(gc_weak_mark_end_locked(g,&r.owner,GC_MP_DECIDE,gc_mark_phase_step_locked)==GC_MP_ADVANCED);phase_ack(&r.phase);
            assert(gc_weak_mark_end_locked(g,&r.owner,GC_MP_RELEASE,gc_mark_phase_step_locked)==GC_MP_SELECT);phase_resume(&r.phase);
            PrepareFixture f;prepare_fixture(&f,g->phase,0);f.field.record.young_epoch=r.phase.q.epoch;
            WeakLiveness probe=step;uint64_t expected=GC_WEAK_LIVENESS_INVALID;
            switch(fault){
            case 0:expected=GC_WEAK_LIVE;break;
            case 1:f.field.record.young_epoch=0;expected=GC_WEAK_DEAD;break;
            case 2:atomic_store(&((ObjectHeader *)(f.field.heap+16))->state,GC_OBJECT_INITIALIZING);expected=GC_WEAK_LIVENESS_WAIT;break;
            case 3:r.owner.stage=2;break;case 4:r.weak.gate.closed=0;break;
            case 5:f.q.phase^=0x10000;break;case 6:r.phase.e.h.generation++;break;
            case 7:r.phase.q.epoch++;break;case 8:r.phase.e.f.queue.epoch++;break;
            case 9:r.phase.q.outcome=2;break;case 10:r.phase.q.state=2;break;
            case 11:g->phase+=0x10000;break;case 12:r.phase.e.h.active=1;break;
            case 13:r.phase.q.generation=1;break;case 14:r.phase.q.handshake+=8;break;case 15:probe=NULL;break;
            }
            PrepareFixture before;memcpy(&before,&f,sizeof(f));WeakMarkRace rb;memcpy(&rb,&r,sizeof(r));
            assert(gc_weak_mark_observe(&r.owner,&f.q,probe)==expected);
            assert(!memcmp(&before,&f,sizeof(f))&&!memcmp(&rb,&r,sizeof(r)));
        }
    }
    puts("weak liveness: 136 C/candidate canonical epoch, guard and completed-owner observations preserve all state PASS");
}
#endif
