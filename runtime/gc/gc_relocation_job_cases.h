#ifndef KLASSIC_GC_RELOCATION_JOB_CASES_H
#define KLASSIC_GC_RELOCATION_JOB_CASES_H
#include "gc_relocation_job.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RelocationJobStep)(RelocationGate *,RelocationJob *,uint64_t);
typedef struct {RelocationGate g;RelocationPermit p[3];ForwardCell source;RelocationJob j;} JobFixture;
#define JP(i) offsetof(JobFixture,p[i].gate),offsetof(JobFixture,p[i].previous),offsetof(JobFixture,p[i].next),offsetof(JobFixture,p[i].job)
static const uint64_t job_pointers[]={offsetof(JobFixture,g.head),JP(0),JP(1),JP(2),
    offsetof(JobFixture,j.owner),offsetof(JobFixture,j.gate),offsetof(JobFixture,j.permit),offsetof(JobFixture,j.source)};
#undef JP
static unsigned job_cases;static FILE *job_vectors;
static uint64_t job_normalize(JobFixture *dst,const JobFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(job_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+job_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void job_fixture(JobFixture *f){
    memset(f,0,sizeof(*f));f->g.epoch=17;
    for(unsigned i=0;i<3;i++){RelocationGateRequest q={.permit=(uintptr_t)&f->p[i]};assert(gc_relocation_gate_locked(&f->g,&q,GC_RG_ACQUIRE)==GC_RG_OK);}
    f->source=(ForwardCell){.source_offset=16,.incarnation=7,.word=9,.access=GC_SOURCE_CLOSED};
    f->j=(RelocationJob){.permit=(uintptr_t)&f->p[1],.source=(uintptr_t)&f->source,.ticket=2,.destination=(UINT64_C(32)<<16)|0x1510};
}
static void job_compare(JobFixture *a,RelocationJobStep step,uint64_t op,uint64_t status){
    JobFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=job_normalize(&c,a);
    for(unsigned i=0;i<sizeof(job_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+job_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(status==GC_RLJ_OK){if(op==GC_RLJ_BIND){
        expected.j.owner=(uintptr_t)&c.j;expected.j.gate=(uintptr_t)&c.g;RelocationPermit *permit=(RelocationPermit *)(uintptr_t)c.j.permit;expected.j.serial=permit->serial;expected.j.epoch=permit->epoch;
        expected.j.source_offset=c.source.source_offset;expected.j.incarnation=c.source.incarnation;expected.j.state=1;
        unsigned index=(unsigned)(((uintptr_t)permit-(uintptr_t)c.p)/sizeof(RelocationPermit));expected.p[index].job=(uintptr_t)&c.j;
    }else if(op==GC_RLJ_CHECK)expected.j.state=2;
    else{expected.j.state=3;unsigned index=(unsigned)((c.j.permit-(uintptr_t)c.p)/sizeof(RelocationPermit));expected.p[index].job=0;}}
    uint64_t actual=step(&a->g,&a->j,op),reference=gc_relocation_job_locked(&c.g,&c.j,op);
    if(actual!=status||reference!=status){fprintf(stderr,"relocation job case %u op %llu got %llu C %llu expected %llu\n",job_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(job_normalize(&x,a)==job_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(job_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t rec[]={op,reference,job_normalize(&x,&c),job_normalize(&y,&c)};
        assert(fwrite(rec,sizeof(rec),1,job_vectors)==1);assert(fwrite(&x,sizeof(x),1,job_vectors)==1);assert(fwrite(&y,sizeof(y),1,job_vectors)==1);}
    job_cases++;
}
static void gc_run_relocation_job_cases(RelocationJobStep step){
    JobFixture f;job_cases=0;const char *path=getenv("KLASSIC_GC_RELOCATION_JOB_VECTORS");
    if(path){job_vectors=fopen(path,"wb");assert(job_vectors);assert(fwrite("KLRJOB01",8,1,job_vectors)==1);
        uint64_t h[]={sizeof(f),offsetof(JobFixture,g),offsetof(JobFixture,j),sizeof(job_pointers)/8};
        assert(fwrite(h,sizeof(h),1,job_vectors)==1);assert(fwrite(job_pointers,sizeof(job_pointers),1,job_vectors)==1);}
    for(unsigned closed=0;closed<2;closed++)for(unsigned young=0;young<2;young++)for(unsigned old=0;old<2;old++)for(unsigned remap=0;remap<4;remap++)for(unsigned remembered=1;remembered<4;remembered++){
        job_fixture(&f);f.g.closed=closed;f.j.destination=(UINT64_C(32)<<16)|(0x100u<<young)|(0x400u<<old)|(0x1000u<<remap)|(remembered<<4);
        job_compare(&f,step,0,0);job_compare(&f,step,0,1);job_compare(&f,step,1,2);job_compare(&f,step,2,1);
        atomic_store(&f.source.word,f.j.destination|2);job_compare(&f,step,2,1);job_compare(&f,step,1,0);job_compare(&f,step,1,0);
        RelocationGateRequest q={f.j.permit,f.j.serial,f.j.epoch};assert(gc_relocation_gate_locked(&f.g,&q,GC_RG_RELEASE)==GC_RG_INVALID);
        job_compare(&f,step,2,0);job_compare(&f,step,2,1);
        assert(gc_relocation_gate_locked(&f.g,&q,GC_RG_RELEASE)==GC_RG_OK);
        job_compare(&f,step,1,1);
    }
    for(unsigned op=0;op<3;op++)for(unsigned fault=0;fault<42;fault++){
        job_fixture(&f);if(op)assert(gc_relocation_job_locked(&f.g,&f.j,0)==0);
        if(op==2){atomic_store(&f.source.word,f.j.destination|2);assert(gc_relocation_job_locked(&f.g,&f.j,1)==0);}
        switch(fault){
        case 0:f.g.epoch=0;break;case 1:f.g.closed=2;break;case 2:f.g.active=0;break;case 3:f.g.last_serial=0;break;
        case 4:f.g.head=0;break;case 5:f.g.head++;break;case 6:f.j.permit=0;break;case 7:f.j.permit++;break;
        case 8:f.j.source=0;break;case 9:f.j.source++;break;case 10:f.j.ticket=0;break;case 11:f.j.ticket=UINT64_MAX;break;
        case 12:f.p[1].gate=0;break;case 13:f.p[1].serial=0;break;case 14:f.p[1].serial=4;break;case 15:f.p[1].epoch++;break;
        case 16:f.p[1].previous++;break;case 17:f.p[1].next++;break;case 18:f.g.active=1;break;case 19:f.p[1].previous=f.p[1].next=0;break;
        case 20:f.p[2].gate=0;break;case 21:f.p[2].epoch++;break;case 22:f.p[2].serial=2;break;case 23:f.p[2].serial=4;break;
        case 24:f.p[2].next=0;break;case 25:f.p[0].gate=0;break;case 26:f.p[0].epoch++;break;case 27:f.p[0].serial=0;break;
        case 28:f.p[0].serial=2;break;case 29:f.p[0].previous=0;break;case 30:f.source.source_offset=0;break;
        case 31:f.source.source_offset++;break;case 32:f.source.source_offset=GC_MAX_OFFSET+1;break;case 33:f.source.incarnation=0;break;
        case 34:atomic_store(&f.source.access,0);break;case 35:f.j.destination=0;break;case 36:f.j.destination=(UINT64_C(16)<<16)|0x1510;break;
        case 37:f.j.destination|=1;break;case 38:f.j.destination+=(UINT64_C(1)<<16);break;case 39:atomic_store(&f.source.word,3);break;case 40:f.p[1].job=op?0:1;break;case 41:f.p[1].job=(uintptr_t)&f.source;break;}
        job_compare(&f,step,op,1);
    }
    for(unsigned fault=0;fault<9;fault++){
        job_fixture(&f);switch(fault){case 0:f.j.owner=1;break;case 1:f.j.gate=1;break;case 2:f.j.serial=1;break;
        case 3:f.j.epoch=1;break;case 4:f.j.source_offset=1;break;case 5:f.j.incarnation=1;break;case 6:f.j.state=1;break;
        case 7:atomic_store(&f.source.word,0);break;case 8:atomic_store(&f.source.word,13);break;}job_compare(&f,step,0,1);
    }
    for(unsigned op=1;op<3;op++)for(unsigned fault=0;fault<12;fault++){
        job_fixture(&f);assert(gc_relocation_job_locked(&f.g,&f.j,0)==0);
        if(op==2){atomic_store(&f.source.word,f.j.destination|2);assert(gc_relocation_job_locked(&f.g,&f.j,1)==0);}
        switch(fault){case 0:f.j.owner=(uintptr_t)&f.source;break;case 1:f.j.gate=0;break;case 2:f.j.serial++;break;
        case 3:f.j.epoch++;break;case 4:f.j.source_offset+=16;break;case 5:f.j.incarnation++;break;case 6:f.j.state=0;break;
        case 7:f.j.state=3;break;case 8:atomic_store(&f.source.word,0);break;case 9:atomic_store(&f.source.word,13);break;
        case 10:atomic_store(&f.source.word,((UINT64_C(48)<<16)|0x1512));break;case 11:f.j.state=op==1?2:1;break;}
        job_compare(&f,step,op,1);
    }
    job_fixture(&f);job_compare(&f,step,3,1);
    job_fixture(&f);job_compare(&f,step,0,0);
    atomic_store(&f.source.word,f.j.destination|2);assert(gc_relocation_job_locked(&f.g,&f.j,1)==0);assert(gc_relocation_job_locked(&f.g,&f.j,2)==0);
    RelocationGateRequest q={f.j.permit,f.j.serial,f.j.epoch};assert(gc_relocation_gate_locked(&f.g,&q,1)==0);
    q=(RelocationGateRequest){.permit=(uintptr_t)&f.p[1]};assert(gc_relocation_gate_locked(&f.g,&q,0)==0);
    job_compare(&f,step,1,1);
    for(unsigned i=0;i<3;i+=2){
        job_fixture(&f);f.j.permit=(uintptr_t)&f.p[i];f.j.ticket=f.p[i].serial;atomic_store(&f.source.word,(f.j.ticket<<2)|1);job_compare(&f,step,0,0);job_compare(&f,step,1,2);
        atomic_store(&f.source.word,f.j.destination|2);job_compare(&f,step,1,0);
    }
    job_fixture(&f);f.j.permit=(uintptr_t)&f.p[2];f.j.ticket=f.p[2].serial;atomic_store(&f.source.word,(f.j.ticket<<2)|1);f.g.head=(uintptr_t)&f.p[0];job_compare(&f,step,0,1);
    job_fixture(&f);
    for(unsigned i=1;i<3;i++){q=(RelocationGateRequest){(uintptr_t)&f.p[i],f.p[i].serial,f.p[i].epoch};assert(gc_relocation_gate_locked(&f.g,&q,1)==0);}
    f.j.permit=(uintptr_t)&f.p[0];f.j.ticket=f.p[0].serial;atomic_store(&f.source.word,(f.j.ticket<<2)|1);job_compare(&f,step,0,0);job_compare(&f,step,1,2);
    job_fixture(&f);assert(gc_relocation_job_locked(&f.g,&f.j,0)==0);
    f.j=(RelocationJob){.permit=(uintptr_t)&f.p[1],.source=(uintptr_t)&f.source,.ticket=2,.destination=(UINT64_C(32)<<16)|0x1510};
    job_compare(&f,step,0,1);
    job_fixture(&f);f.j.permit=(uintptr_t)&f.p[0];job_compare(&f,step,0,1);
    job_fixture(&f);f.j.ticket=1;job_compare(&f,step,0,1);
    assert(job_cases==1134);if(job_vectors){assert(!fclose(job_vectors));job_vectors=NULL;}
    puts("relocation job 1134 C/candidate ownership, pending publication and stale completion cases PASS");
}
#endif
