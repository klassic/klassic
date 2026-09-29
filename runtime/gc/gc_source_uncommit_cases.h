#ifndef KLASSIC_GC_SOURCE_UNCOMMIT_CASES_H
#define KLASSIC_GC_SOURCE_UNCOMMIT_CASES_H
#include "gc_source_uncommit.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*SourceUncommitter)(HeapPool *,SourceUncommit *,uint64_t);
typedef struct {HeapPool h;SourceUncommit q;AllocationPage pages[4];} UncommitFixture;
static const uint64_t uncommit_pointers[]={offsetof(UncommitFixture,h.pages),offsetof(UncommitFixture,q.owner),
    offsetof(UncommitFixture,pages[0].owner),offsetof(UncommitFixture,pages[1].owner),offsetof(UncommitFixture,pages[2].owner),offsetof(UncommitFixture,pages[3].owner)};
static FILE *uncommit_vectors;static unsigned uncommit_cases;
static void uncommit_fixture(UncommitFixture *f,unsigned head,unsigned count,unsigned gen){
    memset(f,0,sizeof(*f));f->h=(HeapPool){.heap_base=UINT64_C(0x100000000),.pages=(uintptr_t)f->pages,.page_count=4,
        .hard_max=4*GC_HEAP_PAGE_BYTES,.soft_max=GC_HEAP_PAGE_BYTES,.committed=4*GC_HEAP_PAGE_BYTES,.pressure=1,
        .metadata_max=8192,.metadata_committed=6000,.last_identity=100000,.last_incarnation=27};
    f->q=(SourceUncommit){.head_index=head,.page_count=count,.incarnation=27};
    for(unsigned i=0;i<4;i++)atomic_store(&f->pages[i].state,GC_PAGE_CLOSED);
    for(unsigned i=head;i<head+count;i++)f->pages[i]=(AllocationPage){.heap_base=f->h.heap_base,.start=(i+1)*GC_HEAP_PAGE_BYTES,
        .limit=(head+count+1)*GC_HEAP_PAGE_BYTES,.incarnation=27,.identity_base=1000,.generation=gen,
        .state=i==head?GC_PAGE_EVACUATED:GC_PAGE_RUN_TAIL,.allocated_top=(i+1)*GC_HEAP_PAGE_BYTES+48,.run_head=head};
    f->pages[head].metadata_leases=5;f->pages[head].starts=0x8000;f->pages[head].starts_words=count*128;f->pages[head].forward_table=0x9000;
}
static uint64_t uncommit_normalize(UncommitFixture *d,const UncommitFixture *s){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(uncommit_pointers)/8;i++){
        unsigned char *p=(unsigned char *)d+uncommit_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void uncommit_clone(UncommitFixture *d,const UncommitFixture *s){
    memcpy(d,s,sizeof(*d));uint64_t mask=uncommit_normalize(d,s);
    for(unsigned i=0;i<sizeof(uncommit_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)d+uncommit_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);
    }
}
static void uncommit_check(const UncommitFixture *before,const UncommitFixture *after,uint64_t op,uint64_t status){
    UncommitFixture expected;memcpy(&expected,before,sizeof(expected));
    if(status==GC_SU_OK){AllocationPage *p=&expected.pages[expected.q.head_index];
        if(!op){p->owner=(uintptr_t)&after->q;expected.q.owner=(uintptr_t)&after->h;expected.q.stage=1;expected.q.committed_mapping=1;}
        else{expected.h.committed-=expected.q.page_count*GC_HEAP_PAGE_BYTES;expected.h.pressure=expected.h.committed+expected.h.pending>expected.h.soft_max;
            p->owner=0;atomic_store(&p->state,GC_PAGE_UNCOMMITTED);expected.q.stage=2;}
    }
    assert(!memcmp(&expected,after,sizeof(expected)));
}
static void uncommit_compare(UncommitFixture *a,SourceUncommitter step,uint64_t op,uint64_t expected){
    UncommitFixture c,ba,bc,x,y;uncommit_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->h,&a->q,op),reference=gc_source_uncommit_locked(&c.h,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"source uncommit %u got %llu C %llu expected %llu\n",uncommit_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    uncommit_check(&ba,a,op,actual);uncommit_check(&bc,&c,op,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(uncommit_normalize(&x,a)==uncommit_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(uncommit_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,uncommit_normalize(&x,&c),uncommit_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,uncommit_vectors)==1);assert(fwrite(&x,sizeof(x),1,uncommit_vectors)==1);assert(fwrite(&y,sizeof(y),1,uncommit_vectors)==1);}
    uncommit_cases++;
}
static void gc_run_source_uncommit_cases(SourceUncommitter step){
    UncommitFixture a;uncommit_cases=0;const char *path=getenv("KLASSIC_GC_SOURCE_UNCOMMIT_VECTORS");
    if(path){uncommit_vectors=fopen(path,"wb");assert(uncommit_vectors);assert(fwrite("KLSUNC01",8,1,uncommit_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(UncommitFixture,h),offsetof(UncommitFixture,q),sizeof(uncommit_pointers)/8};
        assert(fwrite(header,sizeof(header),1,uncommit_vectors)==1);assert(fwrite(uncommit_pointers,sizeof(uncommit_pointers),1,uncommit_vectors)==1);}
    for(unsigned gen=0;gen<2;gen++)for(unsigned head=0;head<4;head++)for(unsigned count=1;count<=4-head;count++){
        uncommit_fixture(&a,head,count,gen);uncommit_compare(&a,step,GC_SU_BEGIN,GC_SU_OK);
        uncommit_compare(&a,step,GC_SU_BEGIN,GC_SU_INVALID);
        uncommit_compare(&a,step,GC_SU_FINISH,GC_SU_INVALID); // failed/unattempted OS cleanup
        a.q.committed_mapping=0;
        SourceUncommit copied=a.q,saved=copied;UncommitFixture before;memcpy(&before,&a,sizeof(a));
        assert(step(&a.h,&copied,GC_SU_FINISH)==GC_SU_INVALID);assert(!memcmp(&before,&a,sizeof(a))&&!memcmp(&saved,&copied,sizeof(copied)));
        uncommit_compare(&a,step,GC_SU_FINISH,GC_SU_OK);uncommit_compare(&a,step,GC_SU_FINISH,GC_SU_INVALID);
    }
    for(unsigned op=0;op<2;op++)for(unsigned fault=0;fault<27;fault++){
        uncommit_fixture(&a,0,3,0);if(op){assert(gc_source_uncommit_locked(&a.h,&a.q,0)==GC_SU_OK);a.q.committed_mapping=0;}
        switch(fault){case 0:a.h.pages=0;break;case 1:a.h.pages++;break;case 2:a.h.page_count=0;break;
        case 3:a.h.page_count=UINT64_MAX;break;case 4:a.h.pages=UINT64_MAX-7;break;case 5:a.h.heap_base++;break;
        case 6:a.h.heap_base=UINT64_MAX-15;break;case 7:a.h.hard_max=0;break;case 8:a.h.hard_max=5*GC_HEAP_PAGE_BYTES;break;
        case 9:a.h.soft_max=a.h.hard_max+1;break;case 10:a.h.relocation_reserve=a.h.hard_max+1;break;
        case 11:a.h.committed=a.h.hard_max+1;break;case 12:a.h.pending=a.h.hard_max;break;
        case 13:a.q.page_count=0;break;case 14:a.q.page_count=5;break;case 15:a.q.head_index=2;break;
        case 16:a.q.incarnation=0;break;case 17:a.h.committed=3*GC_HEAP_PAGE_BYTES-1;break;case 18:a.q.stage++;break;
        case 19:a.q.committed_mapping=1;break;case 20:a.q.owner=op?0:(uintptr_t)&a.h;break;
        case 21:a.pages[0].identity_base=0;break;case 22:a.pages[0].generation=2;break;
        case 23:a.q.incarnation++;break;case 24:atomic_store(&a.pages[0].state,GC_PAGE_CLOSED);break;
        case 25:a.pages[0].owner=8;break;case 26:a.pages[0].limit+=GC_HEAP_PAGE_BYTES;break;}
        uncommit_compare(&a,step,op,GC_SU_INVALID);
    }
    if(uncommit_vectors){assert(fclose(uncommit_vectors)==0);uncommit_vectors=NULL;}
    printf("source uncommit %u C/candidate cleanup ownership, retry and accounting cases PASS\n",uncommit_cases);
}
#endif
