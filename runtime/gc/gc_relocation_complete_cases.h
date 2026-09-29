#ifndef KLASSIC_GC_RELOCATION_COMPLETE_CASES_H
#define KLASSIC_GC_RELOCATION_COMPLETE_CASES_H
#include "gc_relocation_complete.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RelocationCompleter)(RelocationGate *,RelocationCompletion *);
typedef struct {RelocationGate g;RelocationPermit p;ForwardCell source;RelocationJob j;RelocationCopy copy;
    RelocationCompletion q;_Alignas(16) unsigned char heap[256];} CompleteFixture;
static const uint64_t complete_pointers[]={offsetof(CompleteFixture,g.head),offsetof(CompleteFixture,p.gate),
    offsetof(CompleteFixture,p.previous),offsetof(CompleteFixture,p.next),offsetof(CompleteFixture,p.job),
    offsetof(CompleteFixture,j.owner),offsetof(CompleteFixture,j.gate),offsetof(CompleteFixture,j.permit),offsetof(CompleteFixture,j.source),
    offsetof(CompleteFixture,copy.heap_base),offsetof(CompleteFixture,q.owner),offsetof(CompleteFixture,q.job),offsetof(CompleteFixture,q.copy),
    offsetof(CompleteFixture,heap)+16+offsetof(ObjectHeader,mark_record),offsetof(CompleteFixture,heap)+128+offsetof(ObjectHeader,mark_record)};
static unsigned complete_cases;static FILE *complete_vectors;
static uint64_t complete_normalize(CompleteFixture *dst,const CompleteFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(complete_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+complete_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void complete_fixture(CompleteFixture *f){
    memset(f,0,sizeof(*f));f->g.epoch=9;RelocationGateRequest request={.permit=(uintptr_t)&f->p};
    assert(gc_relocation_gate_locked(&f->g,&request,GC_RG_ACQUIRE)==GC_RG_OK);
    f->source=(ForwardCell){.source_offset=48,.incarnation=7,.word=5,.access=GC_SOURCE_CLOSED};
    f->j=(RelocationJob){.permit=(uintptr_t)&f->p,.source=(uintptr_t)&f->source,.ticket=1,.destination=(UINT64_C(160)<<16)|0x1510};
    f->copy=(RelocationCopy){(uintptr_t)f->heap,7,1,256,160,256,256,7,2};
    f->q=(RelocationCompletion){.job=(uintptr_t)&f->j,.copy=(uintptr_t)&f->copy};
    for(unsigned off=16;off<=128;off+=112){
        ObjectHeader *h=(ObjectHeader *)(f->heap+off);*h=(ObjectHeader){.payload_bytes=7,.descriptor_id=1,.mark_record=(uintptr_t)(f->heap+240),.state=GC_OBJECT_PUBLISHED};
        for(unsigned i=0;i<7;i++)((unsigned char *)(h+1))[i]=(unsigned char)(17*i+3);
    }
}
static void complete_compare(CompleteFixture *a,RelocationCompleter step,uint64_t status){
    CompleteFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=complete_normalize(&c,a);
    for(unsigned i=0;i<sizeof(complete_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+complete_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(status==GC_RC_MORE){
        if(c.q.stage==0){expected.q.owner=(uintptr_t)&c.q;expected.q.stage=1;
            expected.j.owner=(uintptr_t)&c.j;expected.j.gate=(uintptr_t)&c.g;expected.j.serial=c.p.serial;expected.j.epoch=c.p.epoch;
            expected.j.source_offset=c.source.source_offset;expected.j.incarnation=c.source.incarnation;expected.j.state=1;expected.p.job=(uintptr_t)&c.j;
        }else if(c.q.stage==1){atomic_store(&expected.source.word,c.j.destination|2);expected.j.state=2;expected.q.stage=2;}
        else{assert(c.q.stage==2);expected.j.state=3;expected.p.job=0;expected.q.stage=3;}
    }else if(status==GC_RC_DONE){expected.g.head=expected.g.active=0;expected.p=(RelocationPermit){0};expected.q.stage=4;}
    uint64_t actual=step(&a->g,&a->q),reference=gc_relocation_complete_locked(&c.g,&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"relocation complete case %u stage %llu got %llu C %llu expected %llu\n",complete_cases,(unsigned long long)before.q.stage,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(complete_normalize(&x,a)==complete_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(complete_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t rec[]={0,reference,complete_normalize(&x,&c),complete_normalize(&y,&c)};
        assert(fwrite(rec,sizeof(rec),1,complete_vectors)==1);assert(fwrite(&x,sizeof(x),1,complete_vectors)==1);assert(fwrite(&y,sizeof(y),1,complete_vectors)==1);}
    complete_cases++;
}
static void complete_seed(CompleteFixture *f,unsigned stage){complete_fixture(f);for(unsigned i=0;i<stage;i++)assert(gc_relocation_complete_locked(&f->g,&f->q)==GC_RC_MORE);}
static void gc_run_relocation_complete_cases(RelocationCompleter step){
    CompleteFixture f;complete_cases=0;const char *path=getenv("KLASSIC_GC_RELOCATION_COMPLETE_VECTORS");
    if(path){complete_vectors=fopen(path,"wb");assert(complete_vectors);assert(fwrite("KLRCMP01",8,1,complete_vectors)==1);
        uint64_t h[]={sizeof(f),offsetof(CompleteFixture,g),offsetof(CompleteFixture,q),sizeof(complete_pointers)/8};
        assert(fwrite(h,sizeof(h),1,complete_vectors)==1);assert(fwrite(complete_pointers,sizeof(complete_pointers),1,complete_vectors)==1);}
    for(unsigned closed=0;closed<2;closed++)for(unsigned published=0;published<2;published++)
    for(unsigned young=0;young<2;young++)for(unsigned old=0;old<2;old++)for(unsigned remap=0;remap<4;remap++)for(unsigned remembered=1;remembered<4;remembered++){
        complete_fixture(&f);f.g.closed=closed;f.j.destination=(UINT64_C(160)<<16)|(0x100u<<young)|(0x400u<<old)|(0x1000u<<remap)|(remembered<<4);
        complete_compare(&f,step,GC_RC_MORE);
        if(published){atomic_store(&f.source.word,f.j.destination|2);atomic_store(&((ObjectHeader *)(f.heap+16))->state,0);}
        complete_compare(&f,step,GC_RC_MORE);complete_compare(&f,step,GC_RC_MORE);complete_compare(&f,step,GC_RC_DONE);complete_compare(&f,step,GC_RC_INVALID);
    }
    for(unsigned stage=0;stage<4;stage++)for(unsigned fault=0;fault<16;fault++){
        complete_seed(&f,stage);switch(fault){
        case 0:f.q.job=0;break;case 1:f.q.job++;break;case 2:f.q.copy=0;break;case 3:f.q.copy++;break;
        case 4:f.q.owner=stage?(uintptr_t)&f.j:1;break;case 5:f.q.stage=4;break;case 6:f.j.source=0;break;case 7:f.j.source++;break;
        case 8:f.j.ticket=0;break;case 9:f.copy.ticket++;break;case 10:f.copy.stage=1;break;case 11:f.copy.destination+=16;break;
        case 12:f.copy.source_incarnation++;break;case 13:f.g.active=0;break;case 14:f.g.epoch=0;break;case 15:f.p.serial=0;break;}
        complete_compare(&f,step,GC_RC_INVALID);
    }
    for(unsigned fault=0;fault<4;fault++){
        complete_seed(&f,1);ObjectHeader *source=(ObjectHeader *)(f.heap+16),*destination=(ObjectHeader *)(f.heap+128);
        switch(fault){case 0:atomic_store(&source->state,0);break;case 1:destination->descriptor_id=2;break;case 2:destination->mark_record=0;break;case 3:f.copy.budget=0;break;}
        complete_compare(&f,step,GC_RC_INVALID);
        atomic_store(&source->state,GC_OBJECT_PUBLISHED);destination->descriptor_id=1;destination->mark_record=(uintptr_t)(f.heap+240);f.copy.budget=256;
        complete_compare(&f,step,GC_RC_MORE);complete_compare(&f,step,GC_RC_MORE);complete_compare(&f,step,GC_RC_DONE);
    }
    for(unsigned fault=0;fault<7;fault++){
        complete_seed(&f,fault==0?2:fault<4?3:1);
        switch(fault){case 0:f.j.state=1;break;case 1:f.j.state=2;break;case 2:f.j.owner=0;break;case 3:f.j.gate=0;break;
        case 4:atomic_store(&f.source.word,((UINT64_C(192)<<16)|0x1512));break;case 5:atomic_store(&f.source.word,3);break;case 6:atomic_store(&f.source.access,0);break;}
        complete_compare(&f,step,GC_RC_INVALID);
    }
    assert(complete_cases==1047);if(complete_vectors){assert(!fclose(complete_vectors));complete_vectors=NULL;}
    puts("relocation completion 1047 C/candidate resumable publication, finish and release cases PASS");
}
#endif
