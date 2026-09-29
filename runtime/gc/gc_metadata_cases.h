#ifndef KLASSIC_GC_METADATA_CASES_H
#define KLASSIC_GC_METADATA_CASES_H
#include "gc_metadata.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*MetadataStep)(MetadataLeasePool *,MetadataLeaseRequest *,uint64_t);
typedef struct {MetadataLeasePool g;MetadataLeaseRequest q;HeapPool h;MetadataLeaseSlot slots[4];AllocationPage pages[3];} MetadataFixture;
static const uint64_t metadata_pointers[]={offsetof(MetadataFixture,g.heap),offsetof(MetadataFixture,g.slots),offsetof(MetadataFixture,q.page),
    offsetof(MetadataFixture,h.pages),offsetof(MetadataFixture,slots[0].page),offsetof(MetadataFixture,slots[1].page),
    offsetof(MetadataFixture,slots[2].page),offsetof(MetadataFixture,slots[3].page)};
static FILE *metadata_vectors;static unsigned metadata_cases;
static void metadata_fixture(MetadataFixture *f,unsigned capacity) {
    memset(f,0,sizeof(*f));f->g=(MetadataLeasePool){(uintptr_t)&f->h,(uintptr_t)f->slots,capacity,1,0};
    f->h.pages=(uintptr_t)f->pages;f->h.page_count=3;
    for(unsigned i=0;i<capacity;i++)f->slots[i].next_free=i+1<capacity?i+2:0;
    for(unsigned i=0;i<3;i++){f->pages[i].incarnation=27+i;f->pages[i].run_head=i;atomic_store(&f->pages[i].state,GC_PAGE_CLOSED);}
    f->q=(MetadataLeaseRequest){(uintptr_t)&f->pages[0],27,0};
}
static uint64_t metadata_normalize(MetadataFixture *dst,const MetadataFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<sizeof(metadata_pointers)/8;i++) {
        unsigned char *p=(unsigned char *)dst+metadata_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void metadata_copy(MetadataFixture *dst,const MetadataFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=metadata_normalize(dst,src);
    for(unsigned i=0;i<sizeof(metadata_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+metadata_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void metadata_compare(MetadataFixture *a,MetadataFixture *c,MetadataStep step,uint64_t op,uint64_t expected) {
    MetadataFixture before,x,y;memcpy(&before,c,sizeof(before));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_metadata_step_locked(&c->g,&c->q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"metadata case %u op %llu got %llu C %llu expected %llu\n",metadata_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected)assert(!memcmp(&before,c,sizeof(before)));
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(metadata_normalize(&x,a)==metadata_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(metadata_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={op,reference,metadata_normalize(&x,c),metadata_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,metadata_vectors)==1);assert(fwrite(&x,sizeof(x),1,metadata_vectors)==1);assert(fwrite(&y,sizeof(y),1,metadata_vectors)==1);}
    metadata_cases++;
}
static void gc_run_metadata_cases(MetadataStep step) {
    MetadataFixture a,c;metadata_cases=0;const char *path=getenv("KLASSIC_GC_METADATA_VECTORS");
    if(path){metadata_vectors=fopen(path,"wb");assert(metadata_vectors);assert(fwrite("KLMETA01",8,1,metadata_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(MetadataFixture,g),offsetof(MetadataFixture,q),sizeof(metadata_pointers)/8};
        assert(fwrite(header,sizeof(header),1,metadata_vectors)==1);assert(fwrite(metadata_pointers,sizeof(metadata_pointers),1,metadata_vectors)==1);}
#define META(op,result) metadata_compare(&a,&c,step,op,result)
    for(unsigned capacity=1;capacity<=4;capacity++)for(unsigned page=0;page<3;page++)for(unsigned old=0;old<2;old++)for(unsigned closed=0;closed<2;closed++) {
        metadata_fixture(&a,capacity);a.pages[page].generation=old;atomic_store(&a.pages[page].state,closed?GC_PAGE_CLOSED:GC_PAGE_ALLOCATING);metadata_copy(&c,&a);
        for(unsigned round=0;round<4;round++){
            uint64_t tokens[4];
            for(unsigned i=0;i<capacity;i++){
                a.q=(MetadataLeaseRequest){(uintptr_t)&a.pages[page],27+page,0};c.q=(MetadataLeaseRequest){(uintptr_t)&c.pages[page],27+page,0};
                META(0,0);tokens[i]=a.q.token;assert(a.g.active==i+1&&a.pages[page].metadata_leases==i+1);
                assert(a.q.token>>32==round+1);
            }
            a.q.token=c.q.token=0;META(0,GC_ML_FULL);
            for(unsigned i=0;i<capacity;i++){
                a.q=c.q=(MetadataLeaseRequest){.token=tokens[i]};META(2,0);
                assert(a.q.page==(uintptr_t)&a.pages[page]&&a.q.incarnation==27+page);
                META(1,0);META(1,GC_ML_INVALID);assert(a.pages[page].metadata_leases==capacity-i-1);
            }
        }
    }
    for(unsigned state=GC_PAGE_EVACUATED;state<=GC_PAGE_UNCOMMITTED;state++){
        metadata_fixture(&a,1);atomic_store(&a.pages[0].state,state);metadata_copy(&c,&a);
        META(GC_ML_ACQUIRE,GC_ML_OK);META(GC_ML_RESOLVE,GC_ML_OK);META(GC_ML_RELEASE,GC_ML_OK);
    }
    // Released token must not resolve or release a later lease in the same slot.
    metadata_fixture(&a,1);metadata_copy(&c,&a);META(0,0);uint64_t stale=a.q.token;META(1,0);
    a.q=(MetadataLeaseRequest){(uintptr_t)&a.pages[0],27,0};c.q=(MetadataLeaseRequest){(uintptr_t)&c.pages[0],27,0};META(0,0);
    uint64_t fresh=a.q.token;assert(fresh!=stale);a.q.token=c.q.token=stale;META(1,1);META(2,1);
    a.q.token=c.q.token=fresh;META(1,0);
    for(unsigned fault=0;fault<30;fault++) {
        metadata_fixture(&a,2);uint64_t expected=1,op=0;
        switch(fault){case 0:op=3;break;case 1:a.g.heap=0;break;case 2:a.g.heap++;break;case 3:a.g.slots=0;break;
        case 4:a.g.slots++;break;case 5:a.g.capacity=0;break;case 6:a.g.capacity=UINT64_MAX;break;
        case 7:a.g.slots=UINT64_MAX-7;break;case 8:a.g.active=3;break;case 9:a.g.free_head=3;break;
        case 10:a.g.free_head=0;break;case 11:a.h.pages=0;break;case 12:a.h.pages++;break;case 13:a.h.page_count=0;break;
        case 14:a.h.page_count=UINT64_MAX;break;case 15:a.h.pages=UINT64_MAX-7;break;case 16:a.q.token=1;break;
        case 17:a.slots[0].page=(uintptr_t)&a.pages[0];break;case 18:a.slots[0].incarnation=1;break;
        case 19:a.slots[0].generation=UINT32_MAX;expected=GC_ML_EXHAUSTED;break;
        case 20:a.slots[0].generation=UINT64_MAX;break;case 21:a.slots[0].next_free=1;break;
        case 22:a.slots[0].next_free=0;break;case 23:a.q.page++;break;case 24:a.q.incarnation=0;break;
        case 25:a.q.incarnation++;break;case 26:atomic_store(&a.pages[0].state,GC_PAGE_RUN_TAIL);break;
        case 27:a.pages[0].run_head=1;break;case 28:a.pages[0].metadata_leases=UINT64_MAX;expected=GC_ML_EXHAUSTED;break;
        case 29:a.q.page=(uintptr_t)&a.pages[3];break;}
        metadata_copy(&c,&a);META(op,expected);
    }
    for(unsigned fault=0;fault<12;fault++) {
        metadata_fixture(&a,2);assert(step(&a.g,&a.q,0)==0);
        switch(fault){case 0:a.q.token=0;break;case 1:a.q.token&=UINT32_MAX;break;
        case 2:a.q.token=(UINT64_C(1)<<32)|3;break;case 3:a.q.token+=UINT64_C(1)<<32;break;
        case 4:a.q.page++;break;case 5:a.q.incarnation++;break;case 6:a.pages[0].metadata_leases=0;break;
        case 7:a.pages[0].incarnation++;break;case 8:a.slots[0].next_free=1;break;case 9:a.slots[0].page=0;break;
        case 10:a.slots[0].incarnation=0;break;case 11:a.slots[0].page=(uintptr_t)&a.pages[3];break;}
        metadata_copy(&c,&a);META(1,1);
    }
    if(metadata_vectors){assert(fclose(metadata_vectors)==0);metadata_vectors=NULL;}
    printf("metadata leases %u C/candidate token, retention and reuse cases PASS\n",metadata_cases);
#undef META
}
#endif
