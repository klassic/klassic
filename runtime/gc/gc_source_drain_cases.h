#ifndef KLASSIC_GC_SOURCE_DRAIN_CASES_H
#define KLASSIC_GC_SOURCE_DRAIN_CASES_H
#include "gc_source_drain.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*SourceDrainer)(AllocationPage *,SourceDrain *);
typedef struct {AllocationPage p;SourceDrain q;ForwardTable table;ForwardCell cells[8];} SourceDrainFixture;
static const uint64_t source_drain_pointers[]={offsetof(SourceDrainFixture,p.forward_table),offsetof(SourceDrainFixture,table.cells)};
static void source_drain_fixture(SourceDrainFixture *f){
    memset(f,0,sizeof(*f));f->p.start=0x20000;f->p.limit=0x40000;f->p.incarnation=7;
    atomic_store(&f->p.state,GC_PAGE_CLOSED);atomic_store(&f->p.allocated_top,f->p.start+8*48);
    f->p.forward_table=(uintptr_t)&f->table;f->table.cells=(uintptr_t)f->cells;f->table.capacity=8;atomic_store(&f->table.used,8);
    f->q=(SourceDrain){7,atomic_load(&f->p.allocated_top),8,0,3,99};
    for(unsigned i=0;i<8;i++){
        f->cells[i].source_offset=f->p.start+i*48+32;f->cells[i].incarnation=7;
        atomic_store(&f->cells[i].access,GC_SOURCE_CLOSED);
        atomic_store(&f->cells[i].word,((UINT64_C(0x50000)+i*48)<<16)|0x1512);
    }
}
static uint64_t source_drain_normalize(SourceDrainFixture *dst,const SourceDrainFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<2;i++){
        unsigned char *p=(unsigned char *)dst+source_drain_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void source_drain_copy(SourceDrainFixture *dst,const SourceDrainFixture *src){
    memcpy(dst,src,sizeof(*dst));uint64_t mask=source_drain_normalize(dst,src);
    for(unsigned i=0;i<2;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+source_drain_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static FILE *source_drain_vectors;static unsigned source_drain_cases;
static void source_drain_compare(SourceDrainFixture *a,SourceDrainer scan,uint64_t status,uint64_t cursor,uint64_t examined){
    SourceDrainFixture c,before,x,y;source_drain_copy(&c,a);memcpy(&before,&c,sizeof(c));
    uint64_t actual=scan(&a->p,&a->q),reference=gc_source_drain(&c.p,&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"source drain case %u: got %llu C %llu expected %llu\n",source_drain_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    if(status==GC_SD_INVALID)assert(!memcmp(&before,&c,sizeof(c)));
    else {assert(a->q.cursor==cursor&&a->q.examined==examined);before.q.cursor=cursor;before.q.examined=examined;if(status==GC_SD_DONE)atomic_store(&before.p.state,GC_PAGE_EVACUATED);assert(!memcmp(&before,&c,sizeof(c)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(source_drain_normalize(&x,a)==source_drain_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    source_drain_cases++;
}
static void source_drain_case(SourceDrainFixture *a,SourceDrainer scan,uint64_t status,uint64_t cursor,uint64_t examined){
    SourceDrainFixture before,after;memcpy(&before,a,sizeof(before));
    source_drain_compare(a,scan,status,cursor,examined);
    if(source_drain_vectors){memcpy(&after,a,sizeof(after));uint64_t record[]={0,status,source_drain_normalize(&before,a),source_drain_normalize(&after,a)};
        assert(fwrite(record,sizeof(record),1,source_drain_vectors)==1);assert(fwrite(&before,sizeof(before),1,source_drain_vectors)==1);assert(fwrite(&after,sizeof(after),1,source_drain_vectors)==1);}
}
static void gc_run_source_drain_cases(SourceDrainer scan){
    SourceDrainFixture a;source_drain_cases=0;const char *path=getenv("KLASSIC_GC_SOURCE_DRAIN_VECTORS");
    if(path){source_drain_vectors=fopen(path,"wb");assert(source_drain_vectors);assert(fwrite("KLSRCD01",8,1,source_drain_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(SourceDrainFixture,p),offsetof(SourceDrainFixture,q),2};
        assert(fwrite(header,sizeof(header),1,source_drain_vectors)==1);assert(fwrite(source_drain_pointers,sizeof(source_drain_pointers),1,source_drain_vectors)==1);}
    source_drain_fixture(&a);source_drain_case(&a,scan,GC_SD_MORE,3,3);source_drain_case(&a,scan,GC_SD_MORE,6,3);source_drain_case(&a,scan,GC_SD_DONE,8,2);source_drain_case(&a,scan,GC_SD_DONE,8,0);
    source_drain_fixture(&a);a.q.used=0;a.q.top=a.p.start;atomic_store(&a.p.allocated_top,a.q.top);atomic_store(&a.table.used,0);source_drain_case(&a,scan,GC_SD_DONE,0,0);
    for(unsigned i=0;i<8;i++)for(unsigned mode=0;mode<5;mode++){
        source_drain_fixture(&a);a.q.budget=256;uint64_t word=atomic_load(&a.cells[i].word);
        if(mode<3)atomic_store(&a.cells[i].word,mode==0?0:mode==1?5:3);
        else atomic_store(&a.cells[i].access,mode==3?GC_SOURCE_CLOSED+1:0);
        source_drain_case(&a,scan,GC_SD_WAIT,i,i+1);
        atomic_store(&a.cells[i].word,word);atomic_store(&a.cells[i].access,GC_SOURCE_CLOSED);
        source_drain_case(&a,scan,GC_SD_DONE,8,8-i);
    }
    for(unsigned fault=0;fault<22;fault++){
        source_drain_fixture(&a);
        switch(fault){case 0:atomic_store(&a.p.state,GC_PAGE_ALLOCATING);break;case 1:a.p.owner=1;break;
        case 2:a.q.incarnation++;break;case 3:a.p.start=0;break;case 4:a.p.limit=a.p.start;break;
        case 5:a.q.top++;break;case 6:a.q.top-=16;break;case 7:a.p.forward_table++;break;
        case 8:a.q.budget=0;break;case 9:a.q.budget=257;break;case 10:a.table.cells++;break;
        case 11:a.table.capacity=0;break;case 12:a.q.used++;break;case 13:a.q.cursor=9;break;
        case 14:a.cells[1].source_offset++;break;case 15:a.cells[1].incarnation++;break;
        case 16:a.cells[1].source_offset=a.cells[0].source_offset;break;case 17:atomic_store(&a.cells[1].word,1);break;
        case 18:atomic_store(&a.cells[1].word,2);break;case 19:atomic_store(&a.cells[1].word,(a.cells[1].source_offset<<16)|0x1512);break;
        case 20:a.table.cells=UINT64_MAX-7;break;case 21:atomic_store(&a.cells[1].word,0x500001513);break;}
        source_drain_case(&a,scan,GC_SD_INVALID,0,0);
    }
    for(unsigned fault=0;fault<5;fault++){
        source_drain_fixture(&a);atomic_store(&a.p.state,GC_PAGE_EVACUATED);a.q.cursor=a.q.used;
        switch(fault){case 0:a.q.cursor=0;break;case 1:a.q.incarnation++;break;
        case 2:atomic_fetch_add(&a.p.allocated_top,16);break;case 3:atomic_store(&a.table.used,7);break;
        case 4:atomic_store(&a.p.state,8);break;}
        source_drain_case(&a,scan,GC_SD_INVALID,0,0);
    }
    if(source_drain_vectors){assert(fclose(source_drain_vectors)==0);source_drain_vectors=NULL;}
    printf("source drain %u C/candidate bounded metadata readiness cases PASS\n",source_drain_cases);
}
#endif
