#ifndef KLASSIC_GC_CYCLE_PRESSURE_CASES_H
#define KLASSIC_GC_CYCLE_PRESSURE_CASES_H
#include "gc_cycle_pressure.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_cycle_pressure_cases(CyclePressureStep candidate){
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++){
        CyclePressureStep step=impl?candidate:gc_cycle_pressure_locked;
        for(unsigned kind=0;kind<2;kind++)for(unsigned busy=0;busy<2;busy++)for(unsigned pending=0;pending<2;pending++)for(unsigned pressure=0;pressure<2;pressure++){
            HeapPool heap={.hard_max=1000,.soft_max=600,.committed=500};
            AllocationSample sample={0x1000,17,1,100,1,pressure?100:99};
            CyclePressure q={(uintptr_t)&heap,(uintptr_t)&sample,1};
            CycleLedger g={.requested={5,5},.started={5,5},.completed={5,5}};
            g.completed[kind]-=busy;g.requested[kind]+=pending;
            CycleLedger expected=g;CycleTicket t={.kind=kind},want=t;
            if(pressure){expected.requested[kind]=6;want.ledger=(uintptr_t)&g;want.cycle=6;}
            HeapPool h=heap;AllocationSample s=sample;CyclePressure saved=q;
            assert(step(&g,&t,&q)==(pressure?GC_CP_REQUESTED:GC_CP_QUIET));
            assert(!memcmp(&g,&expected,sizeof(g))&&!memcmp(&t,&want,sizeof(t)));
            assert(!memcmp(&heap,&h,sizeof(h))&&!memcmp(&sample,&s,sizeof(s))&&!memcmp(&q,&saved,sizeof(q)));count++;
        }
        for(unsigned kind=0;kind<2;kind++)for(unsigned pressure=0;pressure<2;pressure++)for(unsigned fault=0;fault<8;fault++){
            HeapPool heap={.hard_max=1000,.soft_max=600,.committed=500};
            AllocationSample sample={0x1000,17,1,100,1,pressure?100:99};
            CyclePressure q={(uintptr_t)&heap,(uintptr_t)&sample,1};
            CycleLedger g={0};CycleTicket t={.kind=kind};uint64_t expected=GC_CP_INVALID;
            switch(fault){
            case 0:t.kind=2;break;case 1:t.ledger=1;break;case 2:t.cycle=1;break;
            case 3:q.horizon_ns=0;break;case 4:sample.elapsed_ns=0;break;
            case 5:g.requested[kind]=g.started[kind]=g.completed[kind]=UINT64_MAX;expected=pressure?GC_CP_EXHAUSTED:GC_CP_QUIET;break;
            case 6:g.started[kind]=1;expected=pressure?GC_CP_INVALID:GC_CP_QUIET;break;
            default:heap.hard_max=0;break;
            }
            CycleLedger before=g;CycleTicket saved=t;
            assert(step(&g,&t,&q)==expected);
            assert(!memcmp(&g,&before,sizeof(g))&&!memcmp(&t,&saved,sizeof(t)));count++;
        }
        for(unsigned fault=0;fault<6;fault++){
            CycleLedger g={0};CycleTicket t={0};CyclePressure q={0};
            assert(step(fault<2?(void *)(uintptr_t)fault:&g,
                fault>=2&&fault<4?(void *)(uintptr_t)(fault-2):&t,
                fault>=4?(void *)(uintptr_t)(fault-4):&q)==GC_CP_INVALID);count++;
        }
    }
    assert(count==108);
    puts("cycle pressure: 108 C/candidate quiet, coalesced, next-cycle and rejection cases PASS");
}
#endif
