#ifndef KLASSIC_GC_RELOCATION_TURN_CASES_H
#define KLASSIC_GC_RELOCATION_TURN_CASES_H
#include "gc_relocation_turn.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void turn_unchanged(RelocationTurnStep step,RelocationCoordinator *g,RelocationTurn *q,RelocationGate *gate,uint64_t op,uint64_t expected){
    RelocationCoordinator before=*g;RelocationTurn input=*q;RelocationGate gb=*gate;
    assert(step(g,q,op)==expected);
    assert(!memcmp(g,&before,sizeof(before))&&!memcmp(q,&input,sizeof(input))&&!memcmp(gate,&gb,sizeof(gb)));
}
static void gc_run_relocation_turn_cases(RelocationTurnStep candidate){
    for(unsigned impl=0;impl<2;impl++){
        RelocationTurnStep step=impl?candidate:gc_relocation_turn_locked;
        RelocationCoordinator g={.turn=impl};RelocationGate gates[2]={{.epoch=1,.closed=1},{.epoch=1,.closed=1}};
        unsigned granted[2]={0};
        for(unsigned round=0;round<64;round++){
            unsigned k=(round+impl)%2;
            RelocationTurn requests[2]={{.generation=0,.gate=(uintptr_t)&gates[0]},{.generation=1,.gate=(uintptr_t)&gates[1]}};
            for(unsigned i=0;i<2;i++)assert(step(&g,&requests[i],GC_RT_REQUEST)==GC_RT_OK);
            turn_unchanged(step,&g,&requests[k^1],&gates[k^1],GC_RT_ACQUIRE,GC_RT_WAIT);
            assert(step(&g,&requests[k],GC_RT_ACQUIRE)==GC_RT_OK);granted[k]++;
            RelocationTurn stale=requests[k],another={.generation=k,.gate=(uintptr_t)&gates[k]};
            assert(g.active==k+1&&g.turn==(k^1)&&g.serial==round+1&&g.gate_epoch==gates[k].epoch+1);
            assert(step(&g,&another,GC_RT_REQUEST)==GC_RT_OK&&g.pending[k]);
            turn_unchanged(step,&g,&requests[k^1],&gates[k^1],GC_RT_ACQUIRE,GC_RT_WAIT);
            turn_unchanged(step,&g,&requests[k],&gates[k],GC_RT_RELEASE,GC_RT_INVALID);
            assert(gc_relocation_gate_locked(&gates[k],NULL,GC_RG_REOPEN)==GC_RG_OK);
            turn_unchanged(step,&g,&requests[k],&gates[k],GC_RT_RELEASE,GC_RT_WAIT);
            RelocationPermit permit={0};RelocationGateRequest job={.permit=(uintptr_t)&permit};
            assert(gc_relocation_gate_locked(&gates[k],&job,GC_RG_ACQUIRE)==GC_RG_OK);
            assert(gc_relocation_gate_locked(&gates[k],NULL,GC_RG_CLOSE)==GC_RG_WAIT);
            turn_unchanged(step,&g,&requests[k],&gates[k],GC_RT_RELEASE,GC_RT_WAIT);
            turn_unchanged(step,&g,&requests[k],&gates[k],GC_RT_RESUME,GC_RT_WAIT);
            assert(gc_relocation_gate_locked(&gates[k],&job,GC_RG_RELEASE)==GC_RG_OK);
            uint64_t serial=g.serial,epoch=g.gate_epoch;
            assert(step(&g,&requests[k],GC_RT_RESUME)==GC_RT_OK&&g.serial==serial&&g.gate_epoch==epoch+1&&g.active==k+1);
            turn_unchanged(step,&g,&requests[k],&gates[k],GC_RT_RELEASE,GC_RT_WAIT);
            assert(gc_relocation_gate_locked(&gates[k],NULL,GC_RG_CLOSE)==GC_RG_OK);
            assert(step(&g,&requests[k],GC_RT_RELEASE)==GC_RT_OK&&!g.active&&!g.gate&&!g.gate_epoch);
            assert(!requests[k].coordinator&&!requests[k].serial&&g.pending[0]&&g.pending[1]);
            turn_unchanged(step,&g,&stale,&gates[k],GC_RT_RELEASE,GC_RT_INVALID);
        }
        assert(granted[0]==32&&granted[1]==32);
        // A sole requester is never blocked just because the other has priority.
        for(unsigned k=0;k<2;k++){
            RelocationGate gate={.epoch=1,.closed=1};g=(RelocationCoordinator){.turn=k^1};
            RelocationTurn q={.generation=k,.gate=(uintptr_t)&gate};
            assert(step(&g,&q,GC_RT_REQUEST)==GC_RT_OK&&step(&g,&q,GC_RT_ACQUIRE)==GC_RT_OK);
            assert(gc_relocation_gate_locked(&gate,NULL,GC_RG_REOPEN)==GC_RG_OK);
            assert(gc_relocation_gate_locked(&gate,NULL,GC_RG_CLOSE)==GC_RG_OK);
            assert(step(&g,&q,GC_RT_RELEASE)==GC_RT_OK);
        }
        for(unsigned fault=0;fault<23;fault++){
            RelocationGate gate={.epoch=1,.closed=1};g=(RelocationCoordinator){.pending={1,0}};
            RelocationTurn q={.gate=(uintptr_t)&gate};uint64_t expected=GC_RT_INVALID,op=GC_RT_ACQUIRE;
            switch(fault){
                case 0:g.pending[0]=2;break;case 1:g.active=3;break;case 2:g.turn=2;break;
                case 3:q.generation=2;break;case 4:q.coordinator=1;break;case 5:q.serial=1;break;
                case 6:q.gate=0;break;case 7:gate.closed=0;expected=GC_RT_WAIT;break;
                case 8:gate.epoch=0;break;case 9:g.serial=UINT64_MAX;expected=GC_RT_EXHAUSTED;break;
                case 10:gate.epoch=UINT64_MAX;expected=GC_RT_EXHAUSTED;break;
                case 11:g.pending[0]=0;expected=GC_RT_EMPTY;break;
                case 12:g.pending[1]=1;g.turn=1;expected=GC_RT_WAIT;break;
                case 13:g.gate=(uintptr_t)&gate;break;case 14:g.gate_epoch=1;break;
                case 15:g.pending[1]=2;break;case 16:gate.active=gate.last_serial=1;break;
                case 17:q.gate++;break;case 18:op=4;break;
                default:g.active=1;g.serial=1;g.gate=(uintptr_t)&gate;g.gate_epoch=2;
                    if(fault>=20)g.turn=1;
                    if(fault==20)expected=GC_RT_WAIT;
                    if(fault==21)g.serial=0;
                    if(fault==22){op=GC_RT_RESUME;gate.epoch=g.gate_epoch=UINT64_MAX;q.coordinator=(uintptr_t)&g;q.serial=1;expected=GC_RT_EXHAUSTED;}
            }
            turn_unchanged(step,&g,&q,&gate,op,expected);
        }
    }
    puts("relocation turns: 128 C/candidate alternating admissions, active-job drainage and stale receipts PASS");
    puts("relocation turn rejection: 46 C/candidate unchanged malformed, exhausted and pending-priority cases PASS");
}
#endif
