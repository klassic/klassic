#ifndef KLASSIC_GC_RELOCATION_GATE_CASES_H
#define KLASSIC_GC_RELOCATION_GATE_CASES_H
#include "gc_relocation_gate.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RelocationGateStep)(RelocationGate *,RelocationGateRequest *,uint64_t);
typedef struct {RelocationGate g;RelocationGateRequest q;RelocationPermit p[4];} GateFixture;
#define GP(i) offsetof(GateFixture,p[i].gate),offsetof(GateFixture,p[i].previous),offsetof(GateFixture,p[i].next),offsetof(GateFixture,p[i].job)
static const uint64_t gate_pointers[]={offsetof(GateFixture,g.head),offsetof(GateFixture,q.permit),GP(0),GP(1),GP(2),GP(3)};
#undef GP
static unsigned gate_cases;static FILE *gate_vectors;
static uint64_t gate_normalize(GateFixture *dst,const GateFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(gate_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+gate_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void gate_select(GateFixture *f,unsigned i){f->q=(RelocationGateRequest){(uintptr_t)&f->p[i],f->p[i].serial,f->p[i].epoch};}
static void gate_fixture(GateFixture *f){memset(f,0,sizeof(*f));f->g.epoch=1;gate_select(f,0);}
static unsigned gate_index(const GateFixture *f,uint64_t ptr){assert(ptr>=(uintptr_t)f->p&&ptr<(uintptr_t)(f->p+4));return (unsigned)((ptr-(uintptr_t)f->p)/sizeof(RelocationPermit));}
static void gate_compare(GateFixture *a,RelocationGateStep step,uint64_t op,uint64_t expected_status){
    GateFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=gate_normalize(&c,a);
    for(unsigned i=0;i<sizeof(gate_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+gate_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(op==GC_RG_CLOSE&&(expected_status==GC_RG_OK||expected_status==GC_RG_WAIT))expected.g.closed=1;
    if(expected_status==GC_RG_OK){
        if(op==GC_RG_ACQUIRE){unsigned i=gate_index(&c,c.q.permit);RelocationPermit *p=&expected.p[i];
            *p=(RelocationPermit){(uintptr_t)&c.g,++expected.g.last_serial,c.g.epoch,0,c.g.head,0};
            if(c.g.head)expected.p[gate_index(&c,c.g.head)].previous=c.q.permit;
            expected.g.head=c.q.permit;expected.g.active++;expected.q.serial=p->serial;expected.q.epoch=p->epoch;
        }else if(op==GC_RG_RELEASE){unsigned i=gate_index(&c,c.q.permit);RelocationPermit p=c.p[i];
            if(p.previous)expected.p[gate_index(&c,p.previous)].next=p.next;else expected.g.head=p.next;
            if(p.next)expected.p[gate_index(&c,p.next)].previous=p.previous;
            expected.g.active--;expected.p[i]=(RelocationPermit){0};
        }else if(op==GC_RG_REOPEN){expected.g.epoch++;expected.g.closed=0;}
    }
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_relocation_gate_locked(&c.g,&c.q,op);
    if(actual!=expected_status||reference!=expected_status){fprintf(stderr,"relocation gate case %u op %llu got %llu C %llu expected %llu\n",gate_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected_status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(gate_normalize(&x,a)==gate_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(gate_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t rec[]={op,reference,gate_normalize(&x,&c),gate_normalize(&y,&c)};
        assert(fwrite(rec,sizeof(rec),1,gate_vectors)==1);assert(fwrite(&x,sizeof(x),1,gate_vectors)==1);assert(fwrite(&y,sizeof(y),1,gate_vectors)==1);}
    gate_cases++;
}
static void gate_seed(GateFixture *f,unsigned count){gate_fixture(f);for(unsigned i=0;i<count;i++){gate_select(f,i);assert(gc_relocation_gate_locked(&f->g,&f->q,GC_RG_ACQUIRE)==GC_RG_OK);}}
static void gc_run_relocation_gate_cases(RelocationGateStep step){
    GateFixture f;gate_cases=0;const char *path=getenv("KLASSIC_GC_RELOCATION_GATE_VECTORS");
    if(path){gate_vectors=fopen(path,"wb");assert(gate_vectors);assert(fwrite("KLRGAT01",8,1,gate_vectors)==1);
        uint64_t h[]={sizeof(f),offsetof(GateFixture,g),offsetof(GateFixture,q),sizeof(gate_pointers)/8};
        assert(fwrite(h,sizeof(h),1,gate_vectors)==1);assert(fwrite(gate_pointers,sizeof(gate_pointers),1,gate_vectors)==1);}
    const unsigned orders[6][3]={{0,1,2},{0,2,1},{1,0,2},{1,2,0},{2,0,1},{2,1,0}};
    for(unsigned order=0;order<6;order++)for(unsigned boundary=0;boundary<4;boundary++){
        gate_fixture(&f);for(unsigned i=0;i<3;i++){gate_select(&f,i);gate_compare(&f,step,0,0);}
        for(unsigned pos=0;pos<=3;pos++){
            if(pos==boundary){gate_compare(&f,step,2,pos==3?0:2);gate_compare(&f,step,3,pos==3?0:2);
                gate_select(&f,3);gate_compare(&f,step,0,2);if(pos<3)gate_compare(&f,step,4,2);}
            if(pos<3){gate_select(&f,orders[order][pos]);gate_compare(&f,step,1,0);gate_compare(&f,step,1,1);
                gate_compare(&f,step,3,f.g.closed&&!f.g.active?0:2);}
        }
        assert(f.g.closed&&!f.g.active&&!f.g.head);gate_compare(&f,step,4,0);assert(f.g.epoch==2);
        gate_compare(&f,step,3,2);gate_compare(&f,step,4,1);
    }
    for(unsigned op=0;op<6;op++)for(unsigned fault=0;fault<6;fault++){
        gate_seed(&f,(op==1||op==5)?1:0);
        switch(fault){case 0:f.g.epoch=0;break;case 1:f.g.closed=2;break;case 2:f.g.active=f.g.last_serial+1;break;
        case 3:f.g.head|=1;break;case 4:f.g.active=0;f.g.head=(uintptr_t)&f.p[0];break;
        case 5:f.g.active=1;f.g.last_serial=1;f.g.head=0;break;}
        gate_compare(&f,step,op,1);
    }
    gate_fixture(&f);gate_compare(&f,step,6,1);
    for(unsigned fault=0;fault<28;fault++){
        gate_seed(&f,3);gate_select(&f,1);
        switch(fault){case 0:f.q.permit=0;break;case 1:f.q.permit++;break;case 2:f.q.serial=0;break;
        case 3:f.q.serial++;break;case 4:f.q.epoch++;break;case 5:f.p[1].gate=0;break;case 6:f.p[1].serial=0;break;
        case 7:f.p[1].serial=f.q.serial=4;break;case 8:f.p[1].epoch++;break;case 9:f.p[1].previous++;break;
        case 10:f.p[1].next++;break;case 11:f.g.active=1;break;case 12:f.p[1].previous=f.p[1].next=0;break;
        case 13:gate_select(&f,2);f.g.head=(uintptr_t)&f.p[0];break;case 14:f.p[2].gate=0;break;
        case 15:f.p[2].epoch++;break;case 16:f.p[2].serial=1;break;case 17:f.p[2].serial=4;break;
        case 18:f.p[2].next=0;break;case 19:f.p[0].gate=0;break;case 20:f.p[0].epoch++;break;
        case 21:f.p[0].serial=0;break;case 22:f.p[0].serial=2;break;case 23:f.p[0].previous=0;break;
        case 24:f.p[1].previous=(uintptr_t)&f.p[1];break;case 25:f.p[1].next=(uintptr_t)&f.p[1];break;case 26:f.q.epoch=0;break;case 27:f.p[1].job=1;break;}
        gate_compare(&f,step,GC_RG_INSPECT,GC_RG_INVALID);
        gate_compare(&f,step,1,1);
    }
    for(unsigned fault=0;fault<15;fault++){
        gate_seed(&f,1);gate_select(&f,1);
        switch(fault){case 0:f.q.permit=0;break;case 1:f.q.permit++;break;case 2:f.q.serial=1;break;case 3:f.q.epoch=1;break;
        case 4:f.p[1].gate=1;break;case 5:f.p[1].serial=1;break;case 6:f.p[1].epoch=1;break;
        case 7:f.p[1].previous=1;break;case 8:f.p[1].next=1;break;case 9:f.p[0].gate=0;break;
        case 10:f.p[0].epoch++;break;case 11:f.p[0].previous=(uintptr_t)&f.p[2];break;
        case 12:f.p[0].serial=0;break;case 13:f.p[0].serial=2;break;case 14:f.p[1].job=1;break;}
        gate_compare(&f,step,0,1);
    }
    gate_fixture(&f);gate_compare(&f,step,0,0);f.p[3]=f.p[0];gate_select(&f,3);gate_compare(&f,step,1,1);
    gate_select(&f,0);gate_compare(&f,step,1,0);gate_select(&f,0);gate_compare(&f,step,0,0);
    f.q.serial=1;gate_compare(&f,step,1,1);gate_select(&f,0);gate_compare(&f,step,1,0);
    gate_fixture(&f);f.g.last_serial=UINT64_MAX;gate_compare(&f,step,0,3);
    gate_fixture(&f);f.g.epoch=UINT64_MAX;f.g.closed=1;gate_compare(&f,step,4,3);
    gate_fixture(&f);f.g.closed=1;gate_compare(&f,step,0,2);gate_compare(&f,step,3,0);gate_compare(&f,step,2,0);
    for(unsigned closed=0;closed<2;closed++)for(unsigned count=1;count<=3;count++)for(unsigned i=0;i<count;i++){
        gate_seed(&f,count);gate_select(&f,i);f.g.closed=closed;
        gate_compare(&f,step,GC_RG_INSPECT,GC_RG_OK);
        assert(gc_relocation_gate_locked(&f.g,&f.q,GC_RG_RELEASE)==GC_RG_OK);
        gate_compare(&f,step,GC_RG_INSPECT,GC_RG_INVALID);
    }
    assert(gate_cases==593);if(gate_vectors){assert(!fclose(gate_vectors));gate_vectors=NULL;}
    puts("relocation gate 593 C/candidate admission, drain, stale permit and epoch cases PASS");
}
#endif
