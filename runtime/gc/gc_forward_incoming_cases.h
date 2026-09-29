#ifndef KLASSIC_GC_FORWARD_INCOMING_CASES_H
#define KLASSIC_GC_FORWARD_INCOMING_CASES_H
#include "gc_forward_incoming.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*ForwardIncomingScanner)(const ForwardTable *,ForwardIncoming *);
typedef struct {ForwardTable t;ForwardIncoming q;ForwardCell cells[8];} IncomingFixture;
static FILE *incoming_vectors;static unsigned incoming_cases;
static void incoming_fixture(IncomingFixture *f){
    memset(f,0,sizeof(*f));f->t=(ForwardTable){(uintptr_t)f->cells,8,8};
    f->q=(ForwardIncoming){7,8,0x80000,0x90000,0,3,99,0};
    for(unsigned i=0;i<8;i++)f->cells[i]=(ForwardCell){0x20020+i*48,7,((UINT64_C(0x50000)+i*16)<<16)|0x1512,GC_SOURCE_CLOSED};
}
static uint64_t incoming_normalize(IncomingFixture *f,const IncomingFixture *owner){
    if(f->t.cells>=(uintptr_t)owner&&f->t.cells-(uintptr_t)owner<sizeof(*owner)){f->t.cells-=(uintptr_t)owner;return 1;}return 0;
}
static void incoming_compare(IncomingFixture *a,ForwardIncomingScanner scan,uint64_t status,uint64_t cursor,uint64_t examined,uint64_t source){
    IncomingFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));if(incoming_normalize(&c,a))c.t.cells+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(status!=GC_FI_INVALID){expected.q.cursor=cursor;expected.q.examined=examined;expected.q.source_offset=source;}
    uint64_t actual=scan(&a->t,&a->q),reference=gc_forward_incoming(&c.t,&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"incoming case %u got %llu C %llu expected %llu\n",incoming_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(incoming_normalize(&x,a)==incoming_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(incoming_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,incoming_normalize(&x,&c),incoming_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,incoming_vectors)==1);assert(fwrite(&x,sizeof(x),1,incoming_vectors)==1);assert(fwrite(&y,sizeof(y),1,incoming_vectors)==1);}
    incoming_cases++;
}
static void gc_run_forward_incoming_cases(ForwardIncomingScanner scan){
    IncomingFixture a;incoming_cases=0;const char *path=getenv("KLASSIC_GC_FORWARD_INCOMING_VECTORS");
    if(path){incoming_vectors=fopen(path,"wb");assert(incoming_vectors);assert(fwrite("KLFINC01",8,1,incoming_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(IncomingFixture,t),offsetof(IncomingFixture,q),1},pointer=offsetof(IncomingFixture,t.cells);
        assert(fwrite(header,sizeof(header),1,incoming_vectors)==1);assert(fwrite(&pointer,8,1,incoming_vectors)==1);}
    incoming_fixture(&a);incoming_compare(&a,scan,GC_FI_MORE,3,3,0);incoming_compare(&a,scan,GC_FI_MORE,6,3,0);
    incoming_compare(&a,scan,GC_FI_DONE,8,2,0);incoming_compare(&a,scan,GC_FI_DONE,8,0,0);
    for(unsigned i=0;i<8;i++)for(unsigned mode=0;mode<3;mode++){
        incoming_fixture(&a);a.q.target_start=0x50000+i*16;a.q.target_limit=a.q.target_start+16;a.q.budget=mode==0?1:mode==1?3:256;
        while(i-a.q.cursor>=a.q.budget)incoming_compare(&a,scan,GC_FI_MORE,a.q.cursor+a.q.budget,a.q.budget,0);
        incoming_compare(&a,scan,GC_FI_FOUND,i,i-a.q.cursor+1,a.cells[i].source_offset);
        incoming_compare(&a,scan,GC_FI_INVALID,0,0,0); // an unconsumed finding cannot be overwritten
    }
    incoming_fixture(&a);a.q.budget=256;a.q.target_start=0x4fff0;a.q.target_limit=0x50000;
    incoming_compare(&a,scan,GC_FI_DONE,8,8,0); // upper boundary is exclusive
    for(unsigned i=0;i<8;i++){
        incoming_fixture(&a);a.q.budget=256;uint64_t word=atomic_load(&a.cells[i].word);atomic_store(&a.cells[i].word,5);
        incoming_compare(&a,scan,GC_FI_WAIT,i,i+1,0);atomic_store(&a.cells[i].word,word);
        incoming_compare(&a,scan,GC_FI_DONE,8,8-i,0);
    }
    incoming_fixture(&a);a.q.budget=256;for(unsigned i=0;i<8;i++)atomic_store(&a.cells[i].word,i%2?3:0);
    incoming_compare(&a,scan,GC_FI_DONE,8,8,0);
    for(unsigned fault=0;fault<28;fault++){
        incoming_fixture(&a);
        switch(fault){case 0:a.t.cells=0;break;case 1:a.t.cells++;break;case 2:a.t.capacity=0;break;
        case 3:a.t.capacity=UINT64_MAX;break;case 4:a.t.cells=UINT64_MAX-7;break;case 5:a.q.incarnation=0;break;
        case 6:a.q.used=9;break;case 7:atomic_store(&a.t.used,7);break;case 8:a.q.cursor=9;break;
        case 9:a.q.budget=0;break;case 10:a.q.budget=257;break;case 11:a.q.source_offset=16;break;
        case 12:a.q.target_start=0;break;case 13:a.q.target_limit=a.q.target_start;break;case 14:a.q.target_limit=UINT64_MAX;break;
        case 15:a.q.target_start++;break;case 16:a.q.target_limit++;break;case 17:a.cells[0].source_offset=0;break;
        case 18:a.cells[1].source_offset++;break;case 19:a.cells[1].source_offset=GC_MAX_OFFSET+1;break;
        case 20:a.cells[1].incarnation++;break;case 21:a.cells[1].source_offset=a.cells[0].source_offset;break;
        case 22:atomic_store(&a.cells[1].word,1);break;case 23:atomic_store(&a.cells[1].word,2);break;
        case 24:atomic_store(&a.cells[1].word,(a.cells[1].source_offset<<16)|0x1512);break;
        case 25:atomic_store(&a.cells[1].word,(UINT64_C(0x50000)<<16)|0x1502);break;
        case 26:atomic_store(&a.cells[1].word,4);break;case 27:atomic_store(&a.cells[2].word,1);break;}
        incoming_compare(&a,scan,GC_FI_INVALID,0,0,0);
    }
    incoming_fixture(&a);a.q.used=0;atomic_store(&a.t.used,0);incoming_compare(&a,scan,GC_FI_DONE,0,0,0);
    if(incoming_vectors){assert(fclose(incoming_vectors)==0);incoming_vectors=NULL;}
    printf("forward incoming %u C/candidate bounded range, pending publication and preservation cases PASS\n",incoming_cases);
}
#endif
