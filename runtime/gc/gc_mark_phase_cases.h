#ifndef KLASSIC_GC_MARK_PHASE_CASES_H
#define KLASSIC_GC_MARK_PHASE_CASES_H
#include "gc_mark_end_cases.h"
#include "gc_mark_phase.h"
#include "gc_worker.h"
typedef uint64_t (*MarkPhaseStepper)(RootRegistry *,MarkPhaseControl *,uint64_t);
typedef struct {EndFixture e;MarkPhaseControl q;} PhaseFixture;
static uint64_t phase_pointers[64];static unsigned phase_pointer_count,phase_cases;static FILE *phase_vectors;
static void phase_fixture(PhaseFixture *f,uint64_t phase,unsigned gen,unsigned native) {
    memset(f,0,sizeof(*f));end_fixture(&f->e,phase,gen,3,native);
    assert(gc_handshake_step_locked(&f->e.h,NULL,GC_HS_END)==0);
    for(unsigned i=0;i<2;i++)if(!(native&(1u<<i))){WorkerRequest q={(uintptr_t)&f->e.h,(uintptr_t)&f->e.p[i]};assert(gc_worker_step_locked(&f->e.f.g,&q,GC_WK_POLL)==GC_WK_OK);}
    f->q=(MarkPhaseControl){.handshake=(uintptr_t)&f->e.h,.audit=(uintptr_t)&f->e.f.a,.generation=gen,.remembered=(uintptr_t)&f->e.remembered};
}
static uint64_t phase_normalize(PhaseFixture *d,const PhaseFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<phase_pointer_count;i++){unsigned char *p=(unsigned char *)d+phase_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void phase_clone(PhaseFixture *d,const PhaseFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=phase_normalize(d,s);
    for(unsigned i=0;i<phase_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+phase_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void phase_effect(const PhaseFixture *before,const PhaseFixture *after,uint64_t op,uint64_t result,uint64_t target) {
    PhaseFixture e;memcpy(&e,before,sizeof(e));HandshakeState *h=&e.e.h;RootRegistry *g=&e.e.f.g;
    if(result==GC_MP_ADVANCED){unsigned shift=(unsigned)e.q.generation*4;
        h->generation++;h->expected=h->members-h->native;h->active=1;h->acknowledged=0;
        if(op!=GC_MP_START)g->phase=((g->phase+0x10000)&~(UINT64_C(15)<<shift))|((op==GC_MP_BEGIN?3:target)<<shift);
        e.q.state=op==GC_MP_START?4:op==GC_MP_ACTIVATE?5:op==GC_MP_BEGIN?1:2;e.q.rendezvous=h->generation;e.q.phase=g->phase;
        if(op==GC_MP_BEGIN||op==GC_MP_START){e.q.epoch=e.e.f.queue.epoch;e.q.outcome=0;}else e.q.outcome=target;
    }else if(result==GC_MP_RETRY||result==GC_MP_SELECT||result==GC_MP_MARKING){h->active=h->expected=h->acknowledged=0;e.q.state=3;}
    assert(!memcmp(&e,after,sizeof(e)));
}
static void phase_compare(PhaseFixture *a,MarkPhaseStepper step,uint64_t op,uint64_t expected,uint64_t target) {
    PhaseFixture c,ba,bc,x,y;phase_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->e.f.g,&a->q,op),reference=gc_mark_phase_step_locked(&c.e.f.g,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"mark phase case %u op %llu got %llu C %llu expected %llu\n",phase_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    phase_effect(&ba,a,op,actual,target);phase_effect(&bc,&c,op,reference,target);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(phase_normalize(&x,a)==phase_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(phase_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,phase_normalize(&x,&c),phase_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,phase_vectors)==1);assert(fwrite(&x,sizeof(x),1,phase_vectors)==1);assert(fwrite(&y,sizeof(y),1,phase_vectors)==1);}phase_cases++;
}
static void phase_ack(PhaseFixture *f) {
    for(unsigned i=0;i<2;i++)if(atomic_load(&f->e.f.ctx[i].state)!=GC_BLOCKED_NATIVE){
        WorkerRequest q={(uintptr_t)&f->e.h,(uintptr_t)&f->e.p[i]};assert(gc_worker_step_locked(&f->e.f.g,&q,GC_WK_POLL)==GC_WK_PARK);
    }
}
static void phase_resume(PhaseFixture *f) {
    for(unsigned i=0;i<2;i++)if(atomic_load(&f->e.f.ctx[i].state)!=GC_BLOCKED_NATIVE){
        WorkerRequest q={(uintptr_t)&f->e.h,(uintptr_t)&f->e.p[i]};assert(gc_worker_step_locked(&f->e.f.g,&q,GC_WK_POLL)==GC_WK_OK);
        assert(atomic_load(&f->e.f.ctx[i].adopted_phase)==f->e.f.g.phase);
    }
}
static void gc_run_mark_phase_cases(MarkPhaseStepper step) {
    assert(end_pointer_count);phase_pointer_count=end_pointer_count;memcpy(phase_pointers,end_pointers,phase_pointer_count*8);
    phase_pointers[phase_pointer_count++]=offsetof(PhaseFixture,q.handshake);phase_pointers[phase_pointer_count++]=offsetof(PhaseFixture,q.audit);phase_pointers[phase_pointer_count++]=offsetof(PhaseFixture,q.remembered);assert(phase_pointer_count<64);
    PhaseFixture a;phase_cases=0;const char *path=getenv("KLASSIC_GC_MARK_PHASE_VECTORS");
    if(path){phase_vectors=fopen(path,"wb");assert(phase_vectors);assert(fwrite("KLMPHS01",8,1,phase_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(PhaseFixture,e.f.g),offsetof(PhaseFixture,q),phase_pointer_count};assert(fwrite(header,sizeof(header),1,phase_vectors)==1);assert(fwrite(phase_pointers,phase_pointer_count*8,1,phase_vectors)==1);}
    for(unsigned gen=0;gen<2;gen++)for(unsigned ep=0;ep<32;ep++)for(unsigned native=0;native<4;native++)for(unsigned busy=0;busy<3;busy++) {
        phase_fixture(&a,0x90000|(ep<<8)|(gen?(0x20|(ep%8)):(2|((ep%8)<<4))),gen,native);
        if(busy==1){a.e.f.ctx[0].satb.top=1;atomic_store(&a.e.f.ctx[0].satb.published,1);}
        if(busy==2)atomic_fetch_add(&a.e.f.ctx[0].root_revision,1);
        phase_compare(&a,step,GC_MP_BEGIN,GC_MP_ADVANCED,3);
        for(unsigned i=0;i<2;i++)if(!(native&(1u<<i))) {
            WorkerRequest q={(uintptr_t)&a.e.h,(uintptr_t)&a.e.p[i]};
            assert(gc_worker_step_locked(&a.e.f.g,&q,GC_WK_ENTER_NATIVE)==GC_WK_RETRY);
            assert(atomic_load(&a.e.f.ctx[i].state)==GC_RUNNING);
        }
        if(native!=3)phase_compare(&a,step,GC_MP_DECIDE,GC_MP_WAIT,0);
        phase_ack(&a);uint64_t target=busy?2:4;
        phase_compare(&a,step,GC_MP_DECIDE,GC_MP_ADVANCED,target);
        if(native!=3)phase_compare(&a,step,GC_MP_RELEASE,GC_MP_WAIT,0);
        phase_ack(&a);phase_compare(&a,step,GC_MP_RELEASE,busy?GC_MP_RETRY:GC_MP_SELECT,0);phase_resume(&a);
        // Retrying reuses the same control; native entries stay blocked until
        // explicit LEAVE, and managed entries have adopted before resuming.
        if(busy){phase_compare(&a,step,GC_MP_BEGIN,GC_MP_ADVANCED,3);phase_ack(&a);
            phase_compare(&a,step,GC_MP_DECIDE,GC_MP_ADVANCED,2);phase_ack(&a);phase_compare(&a,step,GC_MP_RELEASE,GC_MP_RETRY,0);phase_resume(&a);}
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<35;fault++) {
        phase_fixture(&a,0x90022,gen,0);uint64_t op=GC_MP_BEGIN,expected=GC_MP_INVALID;
        if(fault>=16){assert(gc_mark_phase_step_locked(&a.e.f.g,&a.q,GC_MP_BEGIN)==0);phase_ack(&a);op=GC_MP_DECIDE;}
        if(fault>=29){assert(gc_mark_phase_step_locked(&a.e.f.g,&a.q,GC_MP_DECIDE)==0);phase_ack(&a);op=GC_MP_RELEASE;}
        switch(fault){case 0:op=5;break;case 1:a.q.handshake=0;break;case 2:a.q.handshake++;break;
        case 3:a.q.audit=0;break;case 4:a.q.audit++;break;case 5:a.q.generation=2;break;
        case 6:a.e.f.g.phase|=0xe000;break;case 7:a.e.h.active=2;break;case 8:a.e.f.a.queue=0;break;
        case 9:a.e.f.queue.epoch=0;break;case 10:a.e.f.queue.generation=1-gen;break;case 11:a.q.state=1;break;
        case 12:a.e.f.g.phase=0x90011;break;case 13:assert(gc_handshake_step_locked(&a.e.h,NULL,GC_HS_BEGIN)==0);expected=GC_MP_WAIT;break;
        case 14:a.e.h.generation=UINT64_MAX-1;expected=GC_MP_EXHAUSTED;break;
        case 15:a.e.f.g.phase=(UINT64_MAX&~UINT64_C(0xffff))|0x22;expected=GC_MP_EXHAUSTED;break;
        case 16:a.q.state=0;break;case 17:a.q.rendezvous=0;break;case 18:a.q.epoch=0;break;
        case 19:a.q.rendezvous++;expected=GC_MP_STALE;break;case 20:a.q.phase^=0x10000;expected=GC_MP_STALE;break;
        case 21:a.q.epoch++;expected=GC_MP_STALE;break;case 22:assert(gc_handshake_step_locked(&a.e.h,NULL,GC_HS_END)==0);break;
        case 23:a.e.h.acknowledged=0;expected=GC_MP_WAIT;break;case 24:a.q.outcome=2;break;
        case 25:a.e.h.generation=a.q.rendezvous=UINT64_MAX;expected=GC_MP_EXHAUSTED;break;
        case 26:a.e.f.g.phase=(UINT64_MAX&~UINT64_C(0xffff))|(a.e.f.g.phase&0xffff);a.q.phase=a.e.f.g.phase;expected=GC_MP_EXHAUSTED;break;
        case 27:a.e.f.ctx[0].abi_version++;break;
        case 28:a.q.phase=a.e.f.g.phase=0x90022;break;
        case 29:a.q.outcome=0;break;case 30:a.q.outcome=2;break;case 31:a.q.state=3;break;
        case 32:a.q.rendezvous++;expected=GC_MP_STALE;break;case 33:a.e.h.acknowledged=0;expected=GC_MP_WAIT;break;
        case 34:a.q.phase=a.e.f.g.phase=0x90033;break;}
        phase_compare(&a,step,op,expected,0);
    }
    // The last reservable pair succeeds all the way through adoption; neither
    // snapshot nor handshake generation wraps on the final boundary.
    for(unsigned gen=0;gen<2;gen++) {
        phase_fixture(&a,((UINT64_MAX>>16)-2)*UINT64_C(0x10000)|0x22,gen,0);
        a.e.h.generation=UINT64_MAX-2;
        for(unsigned i=0;i<2;i++)a.e.p[i].acknowledged_generation=a.e.h.generation;
        phase_compare(&a,step,GC_MP_BEGIN,GC_MP_ADVANCED,3);phase_ack(&a);
        phase_compare(&a,step,GC_MP_DECIDE,GC_MP_ADVANCED,4);phase_ack(&a);
        phase_compare(&a,step,GC_MP_RELEASE,GC_MP_SELECT,0);phase_resume(&a);
        assert(a.e.h.generation==UINT64_MAX&&(a.e.f.g.phase>>16)==(UINT64_MAX>>16));
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned ep=0;ep<32;ep++)for(unsigned native=0;native<4;native++) {
        phase_fixture(&a,0x90000|(ep<<8)|(gen?(0x10|(ep%8)):(1|((ep%8)<<4))),gen,native);
        phase_compare(&a,step,GC_MP_START,GC_MP_ADVANCED,1);
        if(native!=3)phase_compare(&a,step,GC_MP_ACTIVATE,GC_MP_WAIT,0);
        phase_ack(&a);
        phase_compare(&a,step,GC_MP_ACTIVATE,GC_MP_ADVANCED,2);
        if(native!=3)phase_compare(&a,step,GC_MP_RELEASE,GC_MP_WAIT,0);
        phase_ack(&a);phase_compare(&a,step,GC_MP_RELEASE,GC_MP_MARKING,0);phase_resume(&a);
        // The same control immediately supports the first end check.
        phase_compare(&a,step,GC_MP_BEGIN,GC_MP_ADVANCED,3);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<13;fault++) {
        phase_fixture(&a,0x90011,gen,0);uint64_t op=GC_MP_START,expected=GC_MP_INVALID;
        if(fault>=5){assert(gc_mark_phase_step_locked(&a.e.f.g,&a.q,GC_MP_START)==0);phase_ack(&a);op=GC_MP_ACTIVATE;}
        switch(fault){
        case 0:a.q.state=4;break;
        case 1:assert(gc_handshake_step_locked(&a.e.h,NULL,GC_HS_BEGIN)==0);expected=GC_MP_WAIT;break;
        case 2:a.e.h.generation=UINT64_MAX-1;expected=GC_MP_EXHAUSTED;break;
        case 3:a.e.f.g.phase=(UINT64_MAX&~UINT64_C(0xffff))|0x11;expected=GC_MP_EXHAUSTED;break;
        case 4:a.e.f.g.phase=0x90022;break;
        case 5:a.q.state=1;break;
        case 6:a.q.phase^=0x10000;expected=GC_MP_STALE;break;
        case 7:a.q.epoch++;expected=GC_MP_STALE;break;
        case 8:a.q.outcome=2;break;
        case 9:a.q.phase=a.e.f.g.phase=0x90022;break;
        case 10:a.q.rendezvous=a.e.h.generation=UINT64_MAX;expected=GC_MP_EXHAUSTED;break;
        case 11:a.q.phase=a.e.f.g.phase=(UINT64_MAX&~UINT64_C(0xffff))|0x11;expected=GC_MP_EXHAUSTED;break;
        case 12:assert(gc_mark_phase_step_locked(&a.e.f.g,&a.q,GC_MP_ACTIVATE)==0);phase_ack(&a);
            op=GC_MP_RELEASE;a.q.outcome=4;a.q.phase=a.e.f.g.phase=0x90044;break;
        }
        phase_compare(&a,step,op,expected,0);
    }
    for(unsigned gen=0;gen<2;gen++) {
        phase_fixture(&a,((UINT64_MAX>>16)-1)*UINT64_C(0x10000)|0x11,gen,0);
        a.e.h.generation=UINT64_MAX-2;
        for(unsigned i=0;i<2;i++)a.e.p[i].acknowledged_generation=a.e.h.generation;
        phase_compare(&a,step,GC_MP_START,GC_MP_ADVANCED,1);phase_ack(&a);
        phase_compare(&a,step,GC_MP_ACTIVATE,GC_MP_ADVANCED,2);phase_ack(&a);
        phase_compare(&a,step,GC_MP_RELEASE,GC_MP_MARKING,0);phase_resume(&a);
        assert(a.e.h.generation==UINT64_MAX&&(a.e.f.g.phase>>16)==(UINT64_MAX>>16));
        phase_compare(&a,step,GC_MP_BEGIN,GC_MP_EXHAUSTED,0);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned stage=0;stage<3;stage++) {
        phase_fixture(&a,0x91022,gen,0);end_remembered_setup(&a.e,stage);
        phase_compare(&a,step,GC_MP_BEGIN,GC_MP_ADVANCED,3);phase_ack(&a);
        phase_compare(&a,step,GC_MP_DECIDE,GC_MP_ADVANCED,gen?4:2);phase_ack(&a);
        phase_compare(&a,step,GC_MP_RELEASE,gen?GC_MP_SELECT:GC_MP_RETRY,0);phase_resume(&a);
    }
    if(phase_vectors){assert(fclose(phase_vectors)==0);phase_vectors=NULL;}
    printf("mark phase %u C/candidate decision, retry, adoption and preservation cases PASS\n",phase_cases);
}
#endif
