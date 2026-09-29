#ifndef KLASSIC_GC_RELOCATION_CLAIM_CASES_H
#define KLASSIC_GC_RELOCATION_CLAIM_CASES_H
#include "gc_relocation_claim.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RelocationClaimer)(RelocationGate *,RelocationClaim *);
typedef struct {RelocationGate g;RelocationPermit p[3];ForwardCell source;RelocationClaim q;} ClaimFixture;
#define CLP(i) offsetof(ClaimFixture,p[i].gate),offsetof(ClaimFixture,p[i].previous),offsetof(ClaimFixture,p[i].next),offsetof(ClaimFixture,p[i].job)
static const uint64_t claim_pointers[]={offsetof(ClaimFixture,g.head),CLP(0),CLP(1),CLP(2),offsetof(ClaimFixture,q.permit),offsetof(ClaimFixture,q.source)};
#undef CLP
static unsigned claim_cases;static FILE *claim_vectors;
static uint64_t claim_normalize(ClaimFixture *dst,const ClaimFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(claim_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+claim_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void claim_fixture(ClaimFixture *f,unsigned count){
    memset(f,0,sizeof(*f));f->g.epoch=11;
    for(unsigned i=0;i<count;i++){RelocationGateRequest q={.permit=(uintptr_t)&f->p[i]};assert(gc_relocation_gate_locked(&f->g,&q,0)==0);}
    f->source=(ForwardCell){.source_offset=16,.incarnation=7};
    f->q=(RelocationClaim){.permit=(uintptr_t)&f->p[2],.source=(uintptr_t)&f->source,.source_offset=16,.incarnation=7};
}
static void claim_compare(ClaimFixture *a,RelocationClaimer step,uint64_t status,bool admitted){
    ClaimFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=claim_normalize(&c,a);
    for(unsigned i=0;i<sizeof(claim_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+claim_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(admitted)expected.g.last_serial++;
    if(status==GC_RCL_OWNED){
        assert(admitted);expected.p[2]=(RelocationPermit){(uintptr_t)&c.g,expected.g.last_serial,c.g.epoch,0,c.g.head,0};
        if(c.g.head){unsigned i=(unsigned)((c.g.head-(uintptr_t)c.p)/sizeof(RelocationPermit));assert(i<2);expected.p[i].previous=(uintptr_t)&c.p[2];}
        expected.g.head=(uintptr_t)&c.p[2];expected.g.active++;expected.q.ticket=expected.g.last_serial;expected.q.epoch=c.g.epoch;
        atomic_store(&expected.source.word,(expected.q.ticket<<2)|1);
    }
    uint64_t actual=step(&a->g,&a->q),reference=gc_relocation_claim_locked(&c.g,&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"relocation claim case %u got %llu C %llu expected %llu\n",claim_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(claim_normalize(&x,a)==claim_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(claim_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t rec[]={0,reference,claim_normalize(&x,&c),claim_normalize(&y,&c)};
        assert(fwrite(rec,sizeof(rec),1,claim_vectors)==1);assert(fwrite(&x,sizeof(x),1,claim_vectors)==1);assert(fwrite(&y,sizeof(y),1,claim_vectors)==1);}
    claim_cases++;
}
static void gc_run_relocation_claim_cases(RelocationClaimer step){
    ClaimFixture f;claim_cases=0;const char *path=getenv("KLASSIC_GC_RELOCATION_CLAIM_VECTORS");
    if(path){claim_vectors=fopen(path,"wb");assert(claim_vectors);assert(fwrite("KLRCLM01",8,1,claim_vectors)==1);
        uint64_t h[]={sizeof(f),offsetof(ClaimFixture,g),offsetof(ClaimFixture,q),sizeof(claim_pointers)/8};
        assert(fwrite(h,sizeof(h),1,claim_vectors)==1);assert(fwrite(claim_pointers,sizeof(claim_pointers),1,claim_vectors)==1);}
    const uint64_t words[]={0,5,3,(UINT64_C(32)<<16)|0x1512,1,2,4};
    const uint64_t results[]={0,2,3,4,1,1,1};
    for(unsigned count=0;count<3;count++)for(unsigned closed=0;closed<2;closed++)for(unsigned serial=0;serial<6;serial++)for(unsigned word=0;word<7;word++){
        claim_fixture(&f,count);f.g.closed=closed;const uint64_t serials[]={count,17,(UINT64_MAX>>2)-1,UINT64_MAX>>2,UINT64_MAX-1,UINT64_MAX};
        f.g.last_serial=serials[serial];atomic_store(&f.source.word,words[word]);
        bool admitted=!closed&&f.g.last_serial!=UINT64_MAX;
        uint64_t status=closed?GC_RCL_CLOSED:f.g.last_serial>=(UINT64_MAX>>2)?GC_RCL_EXHAUSTED:results[word];
        claim_compare(&f,step,status,admitted);
    }
    for(unsigned count=0;count<3;count++)for(unsigned y=0;y<2;y++)for(unsigned o=0;o<2;o++)for(unsigned r=0;r<4;r++)for(unsigned m=1;m<4;m++){
        claim_fixture(&f,count);atomic_store(&f.source.word,(UINT64_C(32)<<16)|(0x100u<<y)|(0x400u<<o)|(0x1000u<<r)|(m<<4)|2);
        claim_compare(&f,step,GC_RCL_FORWARDED,true);
    }
    for(unsigned fault=0;fault<24;fault++){
        claim_fixture(&f,1);switch(fault){case 0:f.q.permit=0;break;case 1:f.q.permit++;break;case 2:f.q.source=0;break;
        case 3:f.q.source++;break;case 4:f.q.ticket=1;break;case 5:f.q.epoch=1;break;case 6:f.q.source_offset=0;break;
        case 7:f.q.source_offset++;break;case 8:f.q.source_offset=GC_MAX_OFFSET+1;break;case 9:f.q.incarnation=0;break;
        case 10:f.source.source_offset+=16;break;case 11:f.source.incarnation++;break;case 12:f.g.epoch=0;break;case 13:f.g.closed=2;break;
        case 14:f.g.active=0;break;case 15:f.g.head++;break;case 16:f.p[2].serial=1;break;case 17:f.p[2].job=1;break;
        case 18:f.p[2].gate=1;break;case 19:f.p[2].previous=1;break;case 20:f.p[2].next=1;break;case 21:f.p[2].epoch=1;break;
        case 22:f.g.last_serial=0;break;case 23:f.p[0].previous=(uintptr_t)&f.p[2];break;}
        claim_compare(&f,step,GC_RCL_INVALID,false);
    }
    const uint64_t access[]={0,1,GC_SOURCE_CLOSED,GC_SOURCE_CLOSED|4};
    for(unsigned i=0;i<4;i++){claim_fixture(&f,0);atomic_store(&f.source.access,access[i]);claim_compare(&f,step,0,true);claim_compare(&f,step,1,false);}
    claim_fixture(&f,0);atomic_store(&f.source.word,5);claim_compare(&f,step,2,true);claim_compare(&f,step,2,true);
    assert(!f.g.active&&!f.g.head&&f.g.last_serial==2&&!f.q.ticket&&!f.p[2].serial);
    f.g.closed=1;claim_compare(&f,step,6,false);
    assert(claim_cases==431);if(claim_vectors){assert(!fclose(claim_vectors));claim_vectors=NULL;}
    puts("relocation claim 431 C/candidate admission, winner, loser cleanup and ticket exhaustion cases PASS");
}
static void gc_run_marked_relocation_cases(MarkedRelocationClaimer candidate){
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned gen=0;gen<2;gen++)for(unsigned stamp=4;stamp<=6;stamp++)for(unsigned state=0;state<4;state++){
        ClaimFixture f;claim_fixture(&f,0);MarkQueue queue={.epoch=5,.generation=gen};
        MarkRecord record={.object_id=17,.young_epoch=gen?9:stamp,.old_epoch=gen?stamp:9};
        MarkedRelocation q={.claim=f.q,.queue=(uintptr_t)&queue,.record=(uintptr_t)&record,.epoch=5};
        uint64_t expected=stamp<5?GC_RCL_NOT_SURVIVED:stamp>5?GC_RCL_STALE:GC_RCL_OWNED;
        if(state==1){f.g.closed=1;if(stamp==5)expected=GC_RCL_CLOSED;}
        if(state==2){atomic_store(&f.source.word,5);if(stamp==5)expected=GC_RCL_BUSY;}
        if(state==3){queue.epoch=6;expected=GC_RCL_STALE;}
        ClaimFixture before=f;MarkedRelocation saved=q;MarkQueue queue_before=queue;MarkRecord record_before=record;
        MarkedRelocationClaimer step=impl?candidate:gc_marked_relocation_claim_locked;
        assert(step(&f.g,&q)==expected);
        if(expected==GC_RCL_OWNED){
            before.g.last_serial=before.g.active=1;before.g.head=(uintptr_t)&f.p[2];
            before.p[2]=(RelocationPermit){(uintptr_t)&f.g,1,11,0,0,0};atomic_store(&before.source.word,5);
            saved.claim.ticket=1;saved.claim.epoch=11;
        }else if(expected==GC_RCL_BUSY)before.g.last_serial=1;
        assert(!memcmp(&f,&before,sizeof(f))&&!memcmp(&q,&saved,sizeof(q)));
        assert(!memcmp(&queue,&queue_before,sizeof(queue))&&!memcmp(&record,&record_before,sizeof(record)));count++;
    }
    for(unsigned impl=0;impl<2;impl++)for(unsigned fault=0;fault<10;fault++){
        ClaimFixture f;claim_fixture(&f,0);MarkQueue queue={.epoch=5};MarkRecord record={.object_id=17,.young_epoch=5};
        MarkedRelocation q={.claim=f.q,.queue=(uintptr_t)&queue,.record=(uintptr_t)&record,.epoch=5};
        RelocationGate *gate=&f.g;MarkedRelocation *input=&q;
        switch(fault){case 0:gate=NULL;break;case 1:gate=(void *)((uintptr_t)gate+1);break;
        case 2:input=NULL;break;case 3:input=(void *)((uintptr_t)input+1);break;
        case 4:q.queue=0;break;case 5:q.record++;break;case 6:q.epoch=0;break;
        case 7:queue.generation=2;break;case 8:queue.epoch=0;break;case 9:record.object_id=0;break;}
        ClaimFixture before=f;MarkedRelocation saved=q;
        assert((impl?candidate:gc_marked_relocation_claim_locked)(gate,input)==GC_RCL_INVALID);
        assert(!memcmp(&f,&before,sizeof(f))&&!memcmp(&q,&saved,sizeof(q)));count++;
    }
    assert(count==68);
    puts("marked relocation: 68 C/candidate survivor, stale, claim and nonmutation cases PASS");
}
#endif
