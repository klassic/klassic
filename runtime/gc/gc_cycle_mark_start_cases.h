#ifndef KLASSIC_GC_CYCLE_MARK_START_CASES_H
#define KLASSIC_GC_CYCLE_MARK_START_CASES_H
#include "gc_cycle_mark_start.h"
#include "gc_mark_phase_cases.h"
static void gc_run_cycle_mark_start_cases(CycleMarkStarter candidate){
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned gen=0;gen<2;gen++)for(unsigned fault=0;fault<10;fault++){
        CycleMarkStarter step=impl?candidate:gc_cycle_mark_start_locked;
        PhaseFixture f;phase_fixture(&f,0x90011,gen,0);
        CycleLedger ledger={0};ledger.requested[gen]=1;
        CycleTicket ticket={.kind=gen};
        CycleMarkStart q={(uintptr_t)&f.e.f.g,(uintptr_t)&f.q};
        uint64_t expected=GC_CY_OK;
        switch(fault){
        case 0:break;
        case 1:ledger.requested[gen]=0;expected=GC_CY_EMPTY;break;
        case 2:ledger.started[gen]=1;expected=GC_CY_WAIT;break;
        case 3:assert(gc_handshake_step_locked(&f.e.h,NULL,GC_HS_BEGIN)==0);expected=GC_CY_WAIT;break;
        case 4:f.e.f.g.phase=0x90022;expected=GC_CY_INVALID;break;
        case 5:f.q.generation=1-gen;expected=GC_CY_INVALID;break;
        case 6:f.e.h.generation=UINT64_MAX;expected=GC_CY_EXHAUSTED;break;
        case 7:ticket.ledger=1;expected=GC_CY_INVALID;break;
        case 8:q.control=0;expected=GC_CY_INVALID;break;
        case 9:q.registry=0;expected=GC_CY_INVALID;break;
        }
        PhaseFixture before=f;CycleLedger want=ledger;CycleTicket output=ticket;CycleMarkStart saved=q;
        assert(step(&ledger,&ticket,&q)==expected);
        if(expected==GC_CY_OK){
            phase_effect(&before,&f,GC_MP_START,GC_MP_ADVANCED,1);
            want.started[gen]=1;output.ledger=(uintptr_t)&ledger;output.cycle=1;
            assert(!ledger.completed[gen]);
        }else assert(!memcmp(&f,&before,sizeof(f)));
        assert(!memcmp(&ledger,&want,sizeof(ledger))&&!memcmp(&ticket,&output,sizeof(ticket))&&!memcmp(&q,&saved,sizeof(q)));count++;
    }
    assert(count==40);
    puts("cycle mark start: 40 C/candidate atomic ledger/rendezvous admission cases PASS");
}
#endif
