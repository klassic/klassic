#ifndef KLASSIC_GC_CYCLE_CASES_H
#define KLASSIC_GC_CYCLE_CASES_H
#include "gc_cycle.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*CycleStepper)(CycleLedger *,CycleTicket *,uint64_t);
typedef struct {CycleLedger g;CycleTicket q,saved[4];} CycleFixture;
static const uint64_t cycle_pointers[]={offsetof(CycleFixture,q.ledger),offsetof(CycleFixture,saved[0].ledger),offsetof(CycleFixture,saved[1].ledger),offsetof(CycleFixture,saved[2].ledger),offsetof(CycleFixture,saved[3].ledger)};
static unsigned cycle_cases;static FILE *cycle_vectors;
static uint64_t cycle_normalize(CycleFixture *d,const CycleFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<5;i++){unsigned char *p=(unsigned char *)d+cycle_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void cycle_clone(CycleFixture *d,const CycleFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=cycle_normalize(d,s);
    for(unsigned i=0;i<5;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+cycle_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void cycle_effect(const CycleFixture *before,const CycleFixture *after,uint64_t op,uint64_t result){
    CycleFixture e;memcpy(&e,before,sizeof(e));uint64_t k=e.q.kind;
    if(result==GC_CY_OK){
        if(op==GC_CY_REQUEST){e.g.requested[k]=before->g.started[k]+1;e.q.cycle=e.g.requested[k];e.q.ledger=(uintptr_t)&after->g;}
        if(op==GC_CY_START){e.g.started[k]++;e.q.cycle=e.g.started[k];e.q.ledger=(uintptr_t)&after->g;}
        if(op==GC_CY_FINISH)e.g.completed[k]=e.q.cycle;
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void cycle_compare(CycleFixture *a,CycleStepper step,uint64_t op,uint64_t expected){
    CycleFixture c,ba,bc,x,y;cycle_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_cycle_step_locked(&c.g,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"cycle case %u op %llu got %llu C %llu expected %llu\n",cycle_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    cycle_effect(&ba,a,op,actual);cycle_effect(&bc,&c,op,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(cycle_normalize(&x,a)==cycle_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(cycle_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,cycle_normalize(&x,&c),cycle_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,cycle_vectors)==1);assert(fwrite(&x,sizeof(x),1,cycle_vectors)==1);assert(fwrite(&y,sizeof(y),1,cycle_vectors)==1);}cycle_cases++;
}
static void gc_run_cycle_cases(CycleStepper step){
    CycleFixture a;cycle_cases=0;const char *path=getenv("KLASSIC_GC_CYCLE_VECTORS");
    if(path){cycle_vectors=fopen(path,"wb");assert(cycle_vectors);assert(fwrite("KLCYCL01",8,1,cycle_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(CycleFixture,g),offsetof(CycleFixture,q),5};assert(fwrite(header,sizeof(header),1,cycle_vectors)==1);assert(fwrite(cycle_pointers,sizeof(cycle_pointers),1,cycle_vectors)==1);}
    const uint64_t bases[]={0,17,UINT64_MAX-2};
    for(unsigned kind=0;kind<2;kind++)for(unsigned other=0;other<4;other++)for(unsigned base=0;base<3;base++){
        memset(&a,0,sizeof(a));a.g.requested[kind]=a.g.started[kind]=a.g.completed[kind]=bases[base];
        a.g.requested[1-kind]=other==3?2:other?1:0;a.g.started[1-kind]=other>=2?1:0;
#define FRESH() (a.q=(CycleTicket){.kind=kind})
#define CY(op,status) cycle_compare(&a,step,op,status)
        FRESH();CY(GC_CY_REQUEST,GC_CY_OK);a.saved[0]=a.q;
        FRESH();CY(GC_CY_REQUEST,GC_CY_OK);a.saved[1]=a.q;assert(a.saved[0].cycle==a.saved[1].cycle);
        FRESH();CY(GC_CY_START,GC_CY_OK);a.saved[2]=a.q;
        a.q=a.saved[0];CY(GC_CY_POLL,GC_CY_WAIT);
        FRESH();CY(GC_CY_REQUEST,GC_CY_OK);a.saved[3]=a.q;assert(a.saved[3].cycle==a.saved[0].cycle+1);
        CY(GC_CY_FINISH,GC_CY_INVALID);
        FRESH();CY(GC_CY_START,GC_CY_WAIT);
        a.q=a.saved[2];CY(GC_CY_FINISH,GC_CY_OK);
        a.q=a.saved[0];CY(GC_CY_POLL,GC_CY_OK);
        a.q=a.saved[1];CY(GC_CY_POLL,GC_CY_OK);
        a.q=a.saved[3];CY(GC_CY_POLL,GC_CY_WAIT);
        FRESH();CY(GC_CY_START,GC_CY_OK);a.saved[2]=a.q;
        a.q=a.saved[3];CY(GC_CY_POLL,GC_CY_WAIT);
        a.q=a.saved[2];CY(GC_CY_FINISH,GC_CY_OK);
        a.q=a.saved[3];CY(GC_CY_POLL,GC_CY_OK);CY(GC_CY_FINISH,GC_CY_INVALID);
        FRESH();CY(GC_CY_START,GC_CY_EMPTY);
        FRESH();CY(GC_CY_REQUEST,base==2?GC_CY_EXHAUSTED:GC_CY_OK);
#undef FRESH
#undef CY
    }
    for(unsigned kind=0;kind<2;kind++)for(unsigned fault=0;fault<24;fault++) {
        memset(&a,0,sizeof(a));a.g.requested[kind]=3;a.g.started[kind]=2;a.g.completed[kind]=1;
        a.q=(CycleTicket){.kind=kind};uint64_t op=GC_CY_REQUEST,expected=GC_CY_INVALID;
        switch(fault){case 0:op=4;break;case 1:a.q.kind=2;break;case 2:a.q.ledger=(uintptr_t)&a.g;break;case 3:a.q.cycle=1;break;
        case 4:a.g.completed[kind]=3;break;case 5:a.g.completed[kind]=0;break;
        case 6:a.g.requested[kind]=1;break;case 7:a.g.requested[kind]=4;break;
        case 8:a.g.completed[1-kind]=1;break;case 9:a.g.requested[1-kind]=a.g.started[1-kind]=2;break;
        case 10:a.g.started[1-kind]=1;break;case 11:a.g.requested[1-kind]=2;break;
        case 12:op=GC_CY_POLL;break;case 13:op=GC_CY_FINISH;a.q.ledger=(uintptr_t)&a.g;break;
        case 14:op=GC_CY_POLL;a.q.ledger=(uintptr_t)&a.g;a.q.cycle=4;break;
        case 15:op=GC_CY_POLL;a.q.ledger=(uintptr_t)&a.q;a.q.cycle=1;break;
        case 16:op=GC_CY_FINISH;a.q.ledger=(uintptr_t)&a.g;a.q.cycle=1;break;
        case 17:a.g.started[kind]=a.g.completed[kind]=a.g.requested[kind]=UINT64_MAX;expected=GC_CY_EXHAUSTED;break;
        case 18:a.g.started[kind]=a.g.requested[kind]=UINT64_MAX;a.g.completed[kind]=UINT64_MAX-1;expected=GC_CY_EXHAUSTED;break;
        case 19:a.g.requested[kind]=UINT64_MAX;a.g.started[kind]=a.g.completed[kind]=UINT64_MAX-1;expected=GC_CY_OK;break;
        case 20:a.g.started[kind]=a.g.requested[kind]=UINT64_MAX;a.g.completed[kind]=UINT64_MAX-1;op=GC_CY_START;expected=GC_CY_WAIT;break;
        case 21:a.g.started[kind]=a.g.completed[kind]=a.g.requested[kind]=UINT64_MAX;op=GC_CY_START;expected=GC_CY_EMPTY;break;
        case 22:a.q.ledger=(uintptr_t)&a.g;a.q.cycle=1;op=GC_CY_POLL;expected=GC_CY_OK;break;
        case 23:a.q.ledger=(uintptr_t)&a.g;a.q.cycle=3;op=GC_CY_POLL;expected=GC_CY_WAIT;break;}
        cycle_compare(&a,step,op,expected);
    }
    if(cycle_vectors){assert(fclose(cycle_vectors)==0);cycle_vectors=NULL;}
    printf("cycle tickets %u C/candidate request, start, completion and preservation cases PASS\n",cycle_cases);
}
#endif
