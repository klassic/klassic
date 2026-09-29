#ifndef KLASSIC_GC_REFERENCE_LEASE_CASES_H
#define KLASSIC_GC_REFERENCE_LEASE_CASES_H
#include "gc_reference_lease.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*ReferenceAcquirer)(MetadataLeasePool *,ReferenceLease *);
typedef struct {MetadataLeasePool g;ReferenceLease q;HeapPool h;AllocationPage pages[8];MetadataLeaseSlot slots[2];_Atomic uint64_t starts[1024];} ReferenceFixture;
static const uint64_t reference_pointers[]={offsetof(ReferenceFixture,g.heap),offsetof(ReferenceFixture,g.slots),offsetof(ReferenceFixture,q.lease.page),offsetof(ReferenceFixture,h.pages),offsetof(ReferenceFixture,slots[0].page),offsetof(ReferenceFixture,slots[1].page),
    offsetof(ReferenceFixture,pages[0].starts),offsetof(ReferenceFixture,pages[1].starts),offsetof(ReferenceFixture,pages[2].starts),offsetof(ReferenceFixture,pages[3].starts),offsetof(ReferenceFixture,pages[4].starts),offsetof(ReferenceFixture,pages[5].starts),offsetof(ReferenceFixture,pages[6].starts),offsetof(ReferenceFixture,pages[7].starts)};
static unsigned reference_cases;static FILE *reference_vectors;
static void reference_fixture(ReferenceFixture *f,unsigned head,unsigned run,unsigned old,unsigned closed,uint64_t offset) {
    memset(f,0,sizeof(*f));f->g=(MetadataLeasePool){(uintptr_t)&f->h,(uintptr_t)f->slots,2,1,0};f->slots[0].next_free=2;
    f->h=(HeapPool){.pages=(uintptr_t)f->pages,.page_count=8,.heap_base=UINT64_C(0x100000000)};
    for(unsigned i=head;i<head+run;i++) {
        AllocationPage *p=&f->pages[i];p->heap_base=f->h.heap_base;p->start=(i+1)*GC_HEAP_PAGE_BYTES;
        p->limit=(head+run+1)*GC_HEAP_PAGE_BYTES;p->run_head=head;p->incarnation=27;p->identity_base=1024;p->generation=old;
        atomic_store(&p->state,i==head?(closed?GC_PAGE_CLOSED:GC_PAGE_ALLOCATING):GC_PAGE_RUN_TAIL);
    }
    AllocationPage *r=&f->pages[head];r->starts=(uintptr_t)f->starts;r->starts_words=run*GC_HEAP_PAGE_BYTES/1024;atomic_store(&r->allocated_top,r->limit);
    uint64_t granule=(offset-r->start)/16;atomic_store(&f->starts[granule/64],UINT64_C(1)<<(granule%64));
    f->q.reference=(offset<<16)|0x1510;
}
static uint64_t reference_normalize(ReferenceFixture *dst,const ReferenceFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<sizeof(reference_pointers)/8;i++) {
        unsigned char *p=(unsigned char *)dst+reference_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void reference_copy(ReferenceFixture *dst,const ReferenceFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=reference_normalize(dst,src);
    for(unsigned i=0;i<sizeof(reference_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+reference_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void reference_compare(ReferenceFixture *a,ReferenceFixture *c,ReferenceAcquirer acquire,uint64_t expected) {
    ReferenceFixture before_a,before,x,y;memcpy(&before_a,a,sizeof(*a));memcpy(&before,c,sizeof(*c));
    uint64_t actual=acquire(&a->g,&a->q),reference=gc_reference_lease_locked(&c->g,&c->q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"reference lease case %u got %llu C %llu expected %llu\n",reference_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected){assert(!memcmp(&before,c,sizeof(before)));assert(!memcmp(&before_a,a,sizeof(before_a)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(reference_normalize(&x,a)==reference_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(reference_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={0,reference,reference_normalize(&x,c),reference_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,reference_vectors)==1);assert(fwrite(&x,sizeof(x),1,reference_vectors)==1);assert(fwrite(&y,sizeof(y),1,reference_vectors)==1);}
    reference_cases++;
}
static void gc_run_reference_lease_cases(ReferenceAcquirer acquire) {
    ReferenceFixture a,c;reference_cases=0;const char *path=getenv("KLASSIC_GC_REFERENCE_LEASE_VECTORS");
    if(path){reference_vectors=fopen(path,"wb");assert(reference_vectors);assert(fwrite("KLREFL01",8,1,reference_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(ReferenceFixture,g),offsetof(ReferenceFixture,q),sizeof(reference_pointers)/8};
        assert(fwrite(header,sizeof(header),1,reference_vectors)==1);assert(fwrite(reference_pointers,sizeof(reference_pointers),1,reference_vectors)==1);}
    for(unsigned head=0;head<5;head++)for(unsigned run=1;run<=4;run++)for(unsigned page=head;page<head+run;page++)for(unsigned old=0;old<2;old++)for(unsigned closed=0;closed<2;closed++)for(unsigned position=0;position<4;position++) {
        uint64_t within=position==0?(page==head?32:0):position==1?32:position==2?1008:GC_HEAP_PAGE_BYTES-16;
        reference_fixture(&a,head,run,old,closed,(page+1)*GC_HEAP_PAGE_BYTES+within);reference_copy(&c,&a);
        reference_compare(&a,&c,acquire,GC_ML_OK);
        assert(a.q.lease.page==(uintptr_t)&a.pages[head]&&a.q.lease.incarnation==27&&a.pages[head].metadata_leases==1);
        reference_compare(&a,&c,acquire,GC_ML_INVALID); // unique output cannot be overwritten
        assert(gc_metadata_step_locked(&a.g,&a.q.lease,GC_ML_RELEASE)==0);assert(gc_metadata_step_locked(&c.g,&c.q.lease,GC_ML_RELEASE)==0);
        assert(!a.g.active&&!a.pages[head].metadata_leases);
    }
    for(unsigned state=GC_PAGE_EVACUATED;state<=GC_PAGE_UNCOMMITTED;state++)for(unsigned page=1;page<=2;page++){
        reference_fixture(&a,1,2,1,1,(page+1)*GC_HEAP_PAGE_BYTES+32);
        atomic_store(&a.pages[1].state,state);reference_copy(&c,&a);
        reference_compare(&a,&c,acquire,GC_ML_OK);
        assert(a.q.lease.page==(uintptr_t)&a.pages[1]);
    }
    for(unsigned fault=0;fault<53;fault++) {
        reference_fixture(&a,1,3,0,1,3*GC_HEAP_PAGE_BYTES+32);AllocationPage *r=&a.pages[1],*p=&a.pages[2];uint64_t expected=GC_ML_INVALID;
        switch(fault){case 0:a.q.lease.page=8;break;case 1:a.q.lease.incarnation=1;break;case 2:a.q.lease.token=1;break;
        case 3:a.g.heap=0;break;case 4:a.g.heap++;break;case 5:a.q.reference=0;break;case 6:a.q.reference|=1;break;
        case 7:a.q.reference&=~UINT64_C(0x30);break;case 8:a.q.reference|=0x200;break;case 9:a.q.reference|=0x800;break;
        case 10:a.q.reference|=0x2000;break;case 11:a.q.reference+=UINT64_C(1)<<16;break;
        case 12:a.h.pages=0;break;case 13:a.h.pages++;break;case 14:a.h.page_count=0;break;
        case 15:a.h.page_count=UINT64_MAX;break;case 16:a.h.pages=UINT64_MAX-7;break;case 17:a.h.heap_base++;break;
        case 18:a.h.heap_base=UINT64_MAX-15;break;case 19:a.q.reference=(UINT64_C(32)<<16)|0x1510;break;
        case 20:a.q.reference=(UINT64_C(9)*GC_HEAP_PAGE_BYTES<<16)|0x1510;break;case 21:p->run_head=8;break;
        case 22:r->run_head=0;break;case 23:atomic_store(&r->state,GC_PAGE_READY);break;case 24:r->start+=16;break;
        case 25:r->limit=r->start;break;case 26:r->limit=10*GC_HEAP_PAGE_BYTES;break;case 27:r->limit--;break;
        case 28:r->incarnation=0;break;case 29:r->generation=2;break;case 30:r->identity_base=0;break;
        case 31:p->heap_base+=16;break;case 32:p->start+=16;break;case 33:p->limit+=GC_HEAP_PAGE_BYTES;break;
        case 34:p->incarnation++;break;case 35:p->identity_base++;break;case 36:p->generation=1;break;
        case 37:atomic_store(&p->state,GC_PAGE_CLOSED);break;case 38:atomic_store(&r->allocated_top,r->limit+16);break;
        case 39:atomic_store(&r->allocated_top,3*GC_HEAP_PAGE_BYTES+32);break;case 40:r->starts=0;break;
        case 41:r->starts++;break;case 42:r->starts_words--;break;case 43:memset(a.starts,0,sizeof(a.starts));break;
        case 44:a.slots[0].generation=UINT32_MAX;expected=GC_ML_EXHAUSTED;break;
        case 45:a.g.active=2;a.g.free_head=0;expected=GC_ML_FULL;break;
        case 46:atomic_store(&r->allocated_top,r->start-16);break;case 47:atomic_store(&r->allocated_top,r->limit-1);break;
        case 48:r->starts_words=UINT64_MAX;break;case 49:r->starts=UINT64_MAX-7;break;
        case 50:a.q.reference=(r->start<<16)|0x1510;break;case 51:r->heap_base+=16;break;
        case 52:r->starts_words=UINT64_MAX/8;break;}
        reference_copy(&c,&a);reference_compare(&a,&c,acquire,expected);
    }
    for(unsigned y=0;y<2;y++)for(unsigned o=0;o<2;o++)for(unsigned remap=0;remap<4;remap++)for(unsigned remembered=1;remembered<=3;remembered++) {
        reference_fixture(&a,0,1,0,1,GC_HEAP_PAGE_BYTES+32);
        a.q.reference=(a.q.reference&~UINT64_C(0xffff))|(UINT64_C(0x100)<<y)|(UINT64_C(0x400)<<o)|(UINT64_C(0x1000)<<remap)|(remembered<<4);
        reference_copy(&c,&a);reference_compare(&a,&c,acquire,GC_ML_OK);
    }
    if(reference_vectors){assert(fclose(reference_vectors)==0);reference_vectors=NULL;}
    printf("reference leases %u C/candidate run-head, start-bit and failure cases PASS\n",reference_cases);
}
#endif
