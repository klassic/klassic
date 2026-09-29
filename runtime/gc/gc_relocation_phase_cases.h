#ifndef KLASSIC_GC_RELOCATION_PHASE_CASES_H
#define KLASSIC_GC_RELOCATION_PHASE_CASES_H
#include "gc_relocation_phase.h"
#include "gc_worker.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef struct {RootRegistry roots;HandshakeState h;RelocationGate gate;RelocationPhaseControl q;MutatorContext ctx[2];ThreadRegistration p[2];} RelocationPhaseFixture;
static unsigned relocation_phase_cases;
static void relocation_phase_fixture(RelocationPhaseFixture *f,unsigned generation,unsigned epochs,unsigned native){
    memset(f,0,sizeof(*f));f->roots.phase=0x90000|(epochs<<8)|(generation?0x42:0x24);
    f->gate=(RelocationGate){.epoch=8,.closed=1};
    f->q=(RelocationPhaseControl){.handshake=(uintptr_t)&f->h,.gate=(uintptr_t)&f->gate,.generation=generation};
    for(unsigned i=0;i<2;i++){
        f->ctx[i].abi_version=GC_ABI_VERSION;f->p[i].context=(uintptr_t)&f->ctx[i];
        WorkerRequest worker={(uintptr_t)&f->h,(uintptr_t)&f->p[i]};
        assert(gc_worker_step_locked(&f->roots,&worker,GC_WK_ATTACH)==GC_WK_OK);
        if(native&(1u<<i))assert(gc_worker_step_locked(&f->roots,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
    }
}
static void relocation_phase_compare(RelocationPhaseFixture *f,RelocationPhaseStep step,uint64_t op,uint64_t result){
    RelocationPhaseFixture expected;memcpy(&expected,f,sizeof(expected));
    if(result==GC_RP_ADVANCED){
        unsigned shift=(unsigned)expected.q.generation*4;
        expected.h.generation++;expected.h.active=1;expected.h.expected=expected.h.members-expected.h.native;expected.h.acknowledged=0;
        expected.roots.phase=((expected.roots.phase+0x10000)&~(UINT64_C(15)<<shift))|((op+5)<<shift);
        if(!op)expected.roots.phase^=UINT64_C(1)<<(10+expected.q.generation);
        expected.q.state=op+1;expected.q.rendezvous=expected.h.generation;
        expected.q.phase=expected.roots.phase;expected.q.gate_epoch=expected.gate.epoch;
    }else if(result==GC_RP_RELOCATING){
        expected.gate.epoch++;expected.gate.closed=0;expected.q.gate_epoch=expected.gate.epoch;expected.q.state=3;
        expected.h.active=expected.h.expected=expected.h.acknowledged=0;
    }
    uint64_t actual=step(&f->roots,&f->q,op);
    if(actual!=result||memcmp(f,&expected,sizeof(*f))){fprintf(stderr,"relocation phase case %u op %llu got %llu expected %llu\n",relocation_phase_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)result);abort();}
    relocation_phase_cases++;
}
static void relocation_phase_ack(RelocationPhaseFixture *f){
    for(unsigned i=0;i<2;i++)if(atomic_load(&f->ctx[i].state)!=GC_BLOCKED_NATIVE){
        WorkerRequest worker={(uintptr_t)&f->h,(uintptr_t)&f->p[i]};
        // Neither an outstanding address guard nor private producer can ACK.
        atomic_store(&f->ctx[i].access_epoch,1);
        assert(gc_worker_step_locked(&f->roots,&worker,GC_WK_POLL)==GC_WK_INVALID);
        atomic_store(&f->ctx[i].access_epoch,0);atomic_store(&f->ctx[i].private_producers,1);
        assert(gc_worker_step_locked(&f->roots,&worker,GC_WK_POLL)==GC_WK_RETRY);
        atomic_store(&f->ctx[i].private_producers,0);
        assert(gc_worker_step_locked(&f->roots,&worker,GC_WK_POLL)==GC_WK_PARK);
        assert(atomic_load(&f->ctx[i].adopted_phase)==f->roots.phase);
    }
}
static void gc_run_relocation_phase_cases(RelocationPhaseStep step){
    relocation_phase_cases=0;RelocationPhaseFixture f;
    for(unsigned gen=0;gen<2;gen++)for(unsigned epochs=0;epochs<32;epochs++)for(unsigned native=0;native<4;native++){
        relocation_phase_fixture(&f,gen,epochs,native);uint64_t previous=f.roots.phase;
        relocation_phase_compare(&f,step,GC_RP_BEGIN,GC_RP_ADVANCED);
        RelocationPermit permit={0};RelocationGateRequest request={(uintptr_t)&permit,0,0};
        assert(gc_relocation_gate_locked(&f.gate,&request,GC_RG_ACQUIRE)==GC_RG_WAIT);
        if(native!=3)relocation_phase_compare(&f,step,GC_RP_ACTIVATE,GC_RP_WAIT);
        relocation_phase_ack(&f);relocation_phase_compare(&f,step,GC_RP_ACTIVATE,GC_RP_ADVANCED);
        assert(gc_relocation_gate_locked(&f.gate,&request,GC_RG_ACQUIRE)==GC_RG_WAIT);
        if(native!=3)relocation_phase_compare(&f,step,GC_RP_RELEASE,GC_RP_WAIT);
        relocation_phase_ack(&f);relocation_phase_compare(&f,step,GC_RP_RELEASE,GC_RP_RELOCATING);
        assert(gc_relocation_gate_locked(&f.gate,&request,GC_RG_ACQUIRE)==GC_RG_OK);
        assert(gc_relocation_gate_locked(&f.gate,&request,GC_RG_RELEASE)==GC_RG_OK);
        for(unsigned i=0;i<2;i++){
            WorkerRequest worker={(uintptr_t)&f.h,(uintptr_t)&f.p[i]};
            if(native&(1u<<i))assert(atomic_load(&f.ctx[i].adopted_phase)==previous);
            assert(gc_worker_step_locked(&f.roots,&worker,native&(1u<<i)?GC_WK_LEAVE_NATIVE:GC_WK_POLL)==GC_WK_OK);
            assert(atomic_load(&f.ctx[i].adopted_phase)==f.roots.phase);
        }
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<32;fault++){
        relocation_phase_fixture(&f,gen,0,0);uint64_t op=GC_RP_BEGIN,result=GC_RP_INVALID;
        if(fault>=16){assert(step(&f.roots,&f.q,GC_RP_BEGIN)==GC_RP_ADVANCED);relocation_phase_ack(&f);op=GC_RP_ACTIVATE;}
        if(fault>=27){assert(step(&f.roots,&f.q,GC_RP_ACTIVATE)==GC_RP_ADVANCED);relocation_phase_ack(&f);op=GC_RP_RELEASE;}
        switch(fault){
        case 0:op=3;break;case 1:f.q.handshake=0;break;case 2:f.q.handshake++;break;
        case 3:f.q.gate=0;break;case 4:f.q.gate++;break;case 5:f.q.generation=2;break;
        case 6:f.roots.phase|=0xe000;break;case 7:f.h.active=2;break;case 8:f.h.native=3;break;
        case 9:f.q.state=1;break;case 10:f.roots.phase=0x90022;break;
        case 11:f.roots.phase=0x90000|(gen?0x45:0x54);result=GC_RP_WAIT;break;
        case 12:f.gate.closed=0;result=GC_RP_WAIT;break;
        case 13:f.gate.epoch=UINT64_MAX;result=GC_RP_EXHAUSTED;break;
        case 14:f.h.generation=UINT64_MAX-1;result=GC_RP_EXHAUSTED;break;
        case 15:f.roots.phase=(UINT64_MAX&~UINT64_C(0xffff))|(gen?0x42:0x24);result=GC_RP_EXHAUSTED;break;
        case 16:f.q.state=0;break;case 17:f.q.rendezvous=0;break;case 18:f.q.gate_epoch=0;break;
        case 19:f.q.phase++;result=GC_RP_STALE;break;
        case 20:f.q.rendezvous++;result=GC_RP_STALE;break;
        case 21:f.gate.epoch++;result=GC_RP_STALE;break;
        case 22:f.h.active=f.h.expected=f.h.acknowledged=0;break;
        case 23:f.h.acknowledged=0;result=GC_RP_WAIT;break;
        case 24:f.gate.closed=0;result=GC_RP_WAIT;break;
        case 25:f.h.generation=f.q.rendezvous=UINT64_MAX;result=GC_RP_EXHAUSTED;break;
        case 26:f.q.phase=f.roots.phase=(UINT64_MAX&~UINT64_C(0xffff))|(f.roots.phase&0xffff);result=GC_RP_EXHAUSTED;break;
        case 27:f.q.state=3;break;case 28:f.q.rendezvous++;result=GC_RP_STALE;break;
        case 29:f.h.acknowledged=0;result=GC_RP_WAIT;break;
        case 30:f.gate.epoch=f.q.gate_epoch=UINT64_MAX;result=GC_RP_EXHAUSTED;break;
        case 31:f.gate.closed=2;break;
        }
        relocation_phase_compare(&f,step,op,result);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned empty=0;empty<2;empty++){
        relocation_phase_fixture(&f,gen,31,3);
        if(empty){memset(&f.h,0,sizeof(f.h));memset(f.ctx,0,sizeof(f.ctx));memset(f.p,0,sizeof(f.p));}
        f.h.generation=UINT64_MAX-2;f.roots.phase=((UINT64_MAX>>16)-2)<<16|(f.roots.phase&0xffff);
        f.gate.epoch=UINT64_MAX-1;
        relocation_phase_compare(&f,step,GC_RP_BEGIN,GC_RP_ADVANCED);
        relocation_phase_compare(&f,step,GC_RP_ACTIVATE,GC_RP_ADVANCED);
        relocation_phase_compare(&f,step,GC_RP_RELEASE,GC_RP_RELOCATING);
        assert(f.gate.epoch==UINT64_MAX&&f.h.generation==UINT64_MAX);
    }
    assert(relocation_phase_cases==1228);
    puts("relocation phase: 1228 C/candidate adoption, remap, exclusion and exact rejection cases PASS");
}
static void relocation_retire_fixture(RelocationPhaseFixture *f,unsigned gen,unsigned epochs,unsigned native){
    relocation_phase_fixture(f,gen,epochs,native);
    f->roots.phase+=UINT64_C(2)<<(gen*4);
    for(unsigned i=0;i<2;i++)atomic_store(&f->ctx[i].adopted_phase,f->roots.phase);
}
static void relocation_retire_compare(RelocationPhaseFixture *f,RelocationPhaseStep step,uint64_t op,uint64_t result){
    RelocationPhaseFixture expected;memcpy(&expected,f,sizeof(expected));
    if(result==GC_RP_ADVANCED){
        unsigned shift=(unsigned)expected.q.generation*4;
        expected.h.generation++;expected.h.active=1;expected.h.expected=expected.h.members-expected.h.native;expected.h.acknowledged=0;
        expected.roots.phase=((expected.roots.phase+0x10000)&~(UINT64_C(15)<<shift))|(UINT64_C(7)<<shift);
        expected.q.state=1;expected.q.rendezvous=expected.h.generation;
        expected.q.phase=expected.roots.phase;expected.q.gate_epoch=expected.gate.epoch;
    }else if(result==GC_RP_RETIRING){
        expected.q.state=2;expected.h.active=expected.h.expected=expected.h.acknowledged=0;
    }
    uint64_t actual=step(&f->roots,&f->q,op);
    if(actual!=result||memcmp(f,&expected,sizeof(*f))){fprintf(stderr,"relocation retire case %u op %llu got %llu expected %llu\n",relocation_phase_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)result);abort();}
    relocation_phase_cases++;
}
static void gc_run_relocation_retire_cases(RelocationPhaseStep step){
    relocation_phase_cases=0;RelocationPhaseFixture f;
    for(unsigned gen=0;gen<2;gen++)for(unsigned epochs=0;epochs<32;epochs++)for(unsigned native=0;native<4;native++){
        relocation_retire_fixture(&f,gen,epochs,native);uint64_t previous=f.roots.phase;
        RelocationPermit permit={0};RelocationGateRequest request={(uintptr_t)&permit,0,0};
        if(!native){
            assert(gc_relocation_gate_locked(&f.gate,NULL,GC_RG_REOPEN)==GC_RG_OK);
            assert(gc_relocation_gate_locked(&f.gate,&request,GC_RG_ACQUIRE)==GC_RG_OK);
            assert(gc_relocation_gate_locked(&f.gate,NULL,GC_RG_CLOSE)==GC_RG_WAIT);
            relocation_retire_compare(&f,step,GC_RR_BEGIN,GC_RP_WAIT);
            assert(gc_relocation_gate_locked(&f.gate,&request,GC_RG_RELEASE)==GC_RG_OK);
            request.serial=request.epoch=0;
        }
        relocation_retire_compare(&f,step,GC_RR_BEGIN,GC_RP_ADVANCED);
        assert(gc_relocation_gate_locked(&f.gate,&request,GC_RG_ACQUIRE)==GC_RG_WAIT);
        if(native!=3)relocation_retire_compare(&f,step,GC_RR_RELEASE,GC_RP_WAIT);
        relocation_phase_ack(&f);relocation_retire_compare(&f,step,GC_RR_RELEASE,GC_RP_RETIRING);
        assert(gc_relocation_gate_locked(&f.gate,&request,GC_RG_ACQUIRE)==GC_RG_WAIT);
        assert(gc_abi_color((PhaseSnapshot){previous})==gc_abi_color((PhaseSnapshot){f.roots.phase}));
        for(unsigned i=0;i<2;i++){
            WorkerRequest worker={(uintptr_t)&f.h,(uintptr_t)&f.p[i]};
            if(native&(1u<<i))assert(atomic_load(&f.ctx[i].adopted_phase)==previous);
            assert(gc_worker_step_locked(&f.roots,&worker,native&(1u<<i)?GC_WK_LEAVE_NATIVE:GC_WK_POLL)==GC_WK_OK);
            assert(atomic_load(&f.ctx[i].adopted_phase)==f.roots.phase);
        }
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<32;fault++){
        relocation_retire_fixture(&f,gen,0,0);uint64_t op=GC_RR_BEGIN,result=GC_RP_INVALID;
        if(fault>=16){assert(step(&f.roots,&f.q,GC_RR_BEGIN)==GC_RP_ADVANCED);relocation_phase_ack(&f);op=GC_RR_RELEASE;}
        switch(fault){
        case 0:op=2;break;case 1:f.q.handshake=0;break;case 2:f.q.gate=0;break;case 3:f.q.generation=2;break;
        case 4:f.q.state=1;break;case 5:f.q.phase=1;break;case 6:f.q.rendezvous=1;break;case 7:f.q.gate_epoch=1;break;
        case 8:f.roots.phase=0x90000|(gen?0x42:0x24);break;
        case 9:f.roots.phase=0x90000|(gen?0x67:0x76);result=GC_RP_WAIT;break;
        case 10:f.gate.closed=0;result=GC_RP_WAIT;break;
        case 11:f.gate.head=(uintptr_t)&f.p[0];f.gate.active=f.gate.last_serial=1;result=GC_RP_WAIT;break;
        case 12:f.gate.epoch=0;break;case 13:f.gate.closed=2;break;
        case 14:f.h.generation=UINT64_MAX;result=GC_RP_EXHAUSTED;break;
        case 15:f.roots.phase=(UINT64_MAX&~UINT64_C(0xffff))|(f.roots.phase&0xffff);result=GC_RP_EXHAUSTED;break;
        case 16:f.q.state=0;break;case 17:f.q.rendezvous=0;break;case 18:f.q.gate_epoch=0;break;
        case 19:f.q.phase++;result=GC_RP_STALE;break;case 20:f.q.rendezvous++;result=GC_RP_STALE;break;
        case 21:f.gate.epoch++;result=GC_RP_STALE;break;case 22:f.h.active=f.h.expected=f.h.acknowledged=0;break;
        case 23:f.h.acknowledged=0;result=GC_RP_WAIT;break;case 24:f.gate.closed=0;result=GC_RP_WAIT;break;
        case 25:f.gate.closed=2;break;case 26:f.q.phase=f.roots.phase-=UINT64_C(1)<<(gen*4);break;
        case 27:f.h.active=2;break;case 28:f.h.native=3;break;case 29:f.h.expected=f.h.acknowledged=1;break;
        case 30:f.q.state=2;break;case 31:f.roots.phase|=0xe000;break;
        }
        relocation_retire_compare(&f,step,op,result);
    }
    for(unsigned gen=0;gen<2;gen++)for(unsigned empty=0;empty<2;empty++){
        relocation_retire_fixture(&f,gen,31,3);
        if(empty){memset(&f.h,0,sizeof(f.h));memset(f.ctx,0,sizeof(f.ctx));memset(f.p,0,sizeof(f.p));}
        f.h.generation=UINT64_MAX-1;f.roots.phase=((UINT64_MAX>>16)-1)<<16|(f.roots.phase&0xffff);
        f.gate.epoch=UINT64_MAX;
        relocation_retire_compare(&f,step,GC_RR_BEGIN,GC_RP_ADVANCED);
        relocation_retire_compare(&f,step,GC_RR_RELEASE,GC_RP_RETIRING);
        assert(f.gate.closed&&f.gate.epoch==UINT64_MAX&&f.h.generation==UINT64_MAX);
    }
    assert(relocation_phase_cases==840);
    puts("relocation retire: 840 C/candidate drainage, adoption, closed-gate and exact rejection cases PASS");
}
#endif
