#ifndef KLASSIC_GC_PAGE_RELEASE_CASES_H
#define KLASSIC_GC_PAGE_RELEASE_CASES_H
#include "gc_page_release.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*PageReleaser)(HeapPool *,PageRelease *,uint64_t);
typedef struct {HeapPool h;PageRelease q;AllocationPage pages[4];ForwardTable table;ForwardCell cells[2];_Atomic uint64_t starts[512];} ReleaseFixture;
#define RELEASE_PAGE_POINTERS(i) offsetof(ReleaseFixture,pages[i].starts),offsetof(ReleaseFixture,pages[i].forward_table)
static const uint64_t release_pointers[]={offsetof(ReleaseFixture,h.pages),offsetof(ReleaseFixture,q.owner),offsetof(ReleaseFixture,q.starts),offsetof(ReleaseFixture,q.forward_table),offsetof(ReleaseFixture,table.cells),RELEASE_PAGE_POINTERS(0),RELEASE_PAGE_POINTERS(1),RELEASE_PAGE_POINTERS(2),RELEASE_PAGE_POINTERS(3)};
#undef RELEASE_PAGE_POINTERS
static unsigned release_cases;static FILE *release_vectors;
static void release_fixture(ReleaseFixture *f,unsigned head,unsigned count,unsigned gen){
    memset(f,0,sizeof(*f));uint64_t bytes=(uint64_t)count*GC_HEAP_PAGE_BYTES;
    f->h=(HeapPool){.heap_base=UINT64_C(0x100000000),.pages=(uintptr_t)f->pages,.page_count=4,.hard_max=4*GC_HEAP_PAGE_BYTES,
        .soft_max=GC_HEAP_PAGE_BYTES,.relocation_reserve=GC_HEAP_PAGE_BYTES,.committed=bytes,
        .metadata_max=8192,.metadata_committed=sizeof(f->pages)+bytes/128+sizeof(f->table)+sizeof(f->cells),
        .last_identity=100000,.last_incarnation=27,.pressure=bytes>GC_HEAP_PAGE_BYTES};
    f->q=(PageRelease){.head_index=head,.page_count=count,.incarnation=27};
    f->table=(ForwardTable){(uintptr_t)f->cells,2,0};
    for(unsigned i=head;i<head+count;i++)f->pages[i]=(AllocationPage){.heap_base=f->h.heap_base,.start=(i+1)*GC_HEAP_PAGE_BYTES,
        .limit=(head+count+1)*GC_HEAP_PAGE_BYTES,.incarnation=27,.identity_base=1000,.generation=gen,
        .state=i==head?GC_PAGE_READY:GC_PAGE_RUN_TAIL,.allocated_top=(i+1)*GC_HEAP_PAGE_BYTES,.run_head=head};
    f->pages[head].starts=(uintptr_t)f->starts;f->pages[head].starts_words=bytes/1024;f->pages[head].forward_table=(uintptr_t)&f->table;
}
static uint64_t release_normalize(ReleaseFixture *f,const ReleaseFixture *owner){
    uint64_t mask=0;
    for(unsigned i=0;i<sizeof(release_pointers)/sizeof(*release_pointers);i++){
        unsigned char *p=(unsigned char *)f+release_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)owner&&v-(uintptr_t)owner<sizeof(*owner)){v-=(uintptr_t)owner;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void release_clone(ReleaseFixture *d,const ReleaseFixture *s){
    memcpy(d,s,sizeof(*d));uint64_t mask=release_normalize(d,s);
    for(unsigned i=0;i<sizeof(release_pointers)/sizeof(*release_pointers);i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)d+release_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);
    }
}
static void release_check(const ReleaseFixture *before,const ReleaseFixture *after,uint64_t op,uint64_t status){
    ReleaseFixture e;memcpy(&e,before,sizeof(e));
    if(!status){
        AllocationPage *head=&e.pages[e.q.head_index];
        if(!op){e.q.owner=(uintptr_t)&after->h;e.q.starts=head->starts;e.q.forward_table=head->forward_table;
            e.q.committed_mapping=1;e.q.stage=1;head->starts=head->starts_words=head->forward_table=0;atomic_store(&head->state,GC_PAGE_RELEASING);}
        else{e.h.committed-=e.q.page_count*GC_HEAP_PAGE_BYTES;e.h.metadata_committed-=e.q.page_count*GC_HEAP_PAGE_BYTES/128;
            for(uint64_t i=e.q.head_index;i<e.q.head_index+e.q.page_count;i++)atomic_store(&e.pages[i].state,GC_PAGE_FREE);
            e.h.pressure=e.h.committed+e.h.pending>e.h.soft_max;e.q.stage=2;}
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void release_compare(ReleaseFixture *a,PageReleaser step,uint64_t op,uint64_t expected){
    ReleaseFixture c,ba,bc,x,y;release_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->h,&a->q,op),reference=gc_page_release_locked(&c.h,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"page release case %u op %llu got %llu C %llu expected %llu\n",release_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    release_check(&ba,a,op,actual);release_check(&bc,&c,op,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(release_normalize(&x,a)==release_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(release_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,release_normalize(&x,&c),release_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,release_vectors)==1);assert(fwrite(&x,sizeof(x),1,release_vectors)==1);assert(fwrite(&y,sizeof(y),1,release_vectors)==1);}
    release_cases++;
}
static void release_ready(ReleaseFixture *f,unsigned op){
    release_fixture(f,0,3,0);
    if(op){assert(gc_page_release_locked(&f->h,&f->q,0)==GC_RELEASE_OK);f->q.starts=f->q.forward_table=f->q.committed_mapping=0;}
}
static void gc_run_page_release_cases(PageReleaser step){
    ReleaseFixture a;release_cases=0;const char *path=getenv("KLASSIC_GC_PAGE_RELEASE_VECTORS");
    if(path){release_vectors=fopen(path,"wb");assert(release_vectors);assert(fwrite("KLPREL01",8,1,release_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(ReleaseFixture,h),offsetof(ReleaseFixture,q),sizeof(release_pointers)/sizeof(*release_pointers)};
        assert(fwrite(header,sizeof(header),1,release_vectors)==1);assert(fwrite(release_pointers,sizeof(release_pointers),1,release_vectors)==1);}
    for(unsigned gen=0;gen<2;gen++)for(unsigned head=0;head<4;head++)for(unsigned count=1;count<=4-head;count++)for(unsigned table=0;table<2;table++){
        release_fixture(&a,head,count,gen);if(!table)a.pages[head].forward_table=0;
        release_compare(&a,step,0,GC_RELEASE_OK);release_compare(&a,step,0,GC_RELEASE_INVALID);
        release_compare(&a,step,1,GC_RELEASE_INVALID);
        a.q.committed_mapping=0;release_compare(&a,step,1,GC_RELEASE_INVALID);
        a.q.starts=0;if(table)release_compare(&a,step,1,GC_RELEASE_INVALID);
        a.q.forward_table=0;release_compare(&a,step,1,GC_RELEASE_OK);release_compare(&a,step,1,GC_RELEASE_INVALID);
    }
    for(unsigned op=0;op<2;op++)for(unsigned fault=0;fault<35;fault++){
        release_ready(&a,op);
        switch(fault){case 0:a.h.pages=0;break;case 1:a.h.pages++;break;case 2:a.h.page_count=0;break;
        case 3:a.h.page_count=UINT64_MAX;break;case 4:a.h.pages=UINT64_MAX-7;break;case 5:a.h.heap_base++;break;
        case 6:a.h.heap_base=UINT64_MAX-15;break;case 7:a.h.hard_max=0;break;case 8:a.h.hard_max=5*GC_HEAP_PAGE_BYTES;break;
        case 9:a.h.soft_max=a.h.hard_max+1;break;case 10:a.h.relocation_reserve=a.h.hard_max+1;break;
        case 11:a.h.committed=a.h.hard_max+1;break;case 12:a.h.pending=a.h.hard_max;break;
        case 13:a.h.metadata_committed=sizeof(a.pages)-1;break;case 14:a.h.metadata_committed=a.h.metadata_max+1;break;
        case 15:a.h.metadata_pending=a.h.metadata_max;break;case 16:a.q.page_count=0;break;case 17:a.q.page_count=5;break;
        case 18:a.q.head_index=2;break;case 19:a.q.incarnation=0;break;case 20:a.h.committed--;break;
        case 21:a.h.metadata_committed=sizeof(a.pages)+3*GC_HEAP_PAGE_BYTES/128-1;break;
        case 22:a.q.stage++;break;case 23:a.q.starts=8;break;case 24:a.q.forward_table=8;break;case 25:a.q.committed_mapping=1;break;
        case 26:a.q.owner=op?0:(uintptr_t)&a.h;break;case 27:a.pages[0].identity_base=0;break;case 28:a.pages[0].generation=2;break;
        case 29:a.q.incarnation++;break;case 30:a.pages[0].starts_words=0;break;
        case 31:atomic_store(&a.pages[0].state,GC_PAGE_CLOSED);break;case 32:a.pages[0].owner=1;break;
        case 33:a.pages[0].metadata_leases=1;break;case 34:atomic_fetch_add(&a.pages[0].allocated_top,48);break;}
        // A cleared head word count is required in FINISH; corrupt it instead.
        if(fault==30&&op)a.pages[0].starts_words=1;
        release_compare(&a,step,op,GC_RELEASE_INVALID);
    }
    for(unsigned op=0;op<2;op++)for(unsigned index=0;index<3;index++)for(unsigned fault=0;fault<13;fault++){
        release_ready(&a,op);AllocationPage *p=&a.pages[index];
        switch(fault){case 0:p->heap_base++;break;case 1:p->run_head++;break;case 2:p->start+=16;break;
        case 3:p->limit+=GC_HEAP_PAGE_BYTES;break;case 4:p->incarnation++;break;
        case 5:p->identity_base++;if(!index)a.pages[2].identity_base++;break;
        case 6:p->generation^=1;if(!index)a.pages[2].generation^=1;break;
        case 7:p->owner=1;break;case 8:p->metadata_leases=1;break;case 9:atomic_fetch_add(&p->allocated_top,16);break;
        case 10:atomic_store(&p->state,GC_PAGE_FREE);break;case 11:p->starts=1;break;case 12:p->forward_table=1;break;}
        release_compare(&a,step,op,GC_RELEASE_INVALID);
    }
    for(unsigned fault=0;fault<10;fault++){
        release_ready(&a,0);
        switch(fault){case 0:a.pages[0].starts=0;break;case 1:a.pages[0].starts=UINT64_MAX-7;break;
        case 2:a.pages[0].starts_words++;break;case 3:a.table.cells=0;break;case 4:a.table.cells++;break;
        case 5:a.table.capacity=0;break;case 6:a.table.capacity=UINT64_MAX;break;case 7:a.table.cells=UINT64_MAX-7;break;
        case 8:atomic_store(&a.table.used,1);break;case 9:atomic_store(&a.table.used,UINT64_MAX);break;}
        release_compare(&a,step,0,GC_RELEASE_INVALID);
    }
    for(unsigned op=0;op<2;op++){release_ready(&a,op);release_compare(&a,step,2,GC_RELEASE_INVALID);}
    // Another admitted reservation keeps pressure set when this run is freed.
    release_fixture(&a,0,1,0);a.h.pending=2*GC_HEAP_PAGE_BYTES;
    release_compare(&a,step,0,GC_RELEASE_OK);a.q.starts=a.q.forward_table=a.q.committed_mapping=0;
    release_compare(&a,step,1,GC_RELEASE_OK);assert(a.h.pressure==1&&a.h.pending==2*GC_HEAP_PAGE_BYTES);
    if(release_vectors){assert(!fclose(release_vectors));release_vectors=NULL;}
    printf("page release %u C/candidate ownership, cleanup and accounting cases PASS\n",release_cases);
}
#endif
