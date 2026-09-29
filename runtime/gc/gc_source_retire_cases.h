#ifndef KLASSIC_GC_SOURCE_RETIRE_CASES_H
#define KLASSIC_GC_SOURCE_RETIRE_CASES_H
#include "gc_source_retire.h"
#include "gc_pages.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef struct {HeapPool h;SourceRetire q;AllocationPage pages[4];uint64_t starts[512],table[3];} RetireFixture;
#define SR_PTR(i) offsetof(RetireFixture,pages[i].owner),offsetof(RetireFixture,pages[i].starts),offsetof(RetireFixture,pages[i].forward_table)
static const uint64_t sr_pointers[]={offsetof(RetireFixture,h.pages),offsetof(RetireFixture,q.owner),offsetof(RetireFixture,q.starts),offsetof(RetireFixture,q.forward_table),SR_PTR(0),SR_PTR(1),SR_PTR(2),SR_PTR(3)};
#undef SR_PTR
static unsigned sr_cases;
static void sr_fixture(RetireFixture *f,unsigned head,unsigned count,unsigned gen){
    memset(f,0,sizeof(*f));f->h=(HeapPool){.heap_base=UINT64_C(0x100000000),.pages=(uintptr_t)f->pages,.page_count=4,
        .hard_max=4*GC_HEAP_PAGE_BYTES,.soft_max=GC_HEAP_PAGE_BYTES,.metadata_max=16384,
        .metadata_committed=sizeof(f->pages)+4096,.last_identity=10000,.last_incarnation=27};
    f->q=(SourceRetire){.head_index=head,.page_count=count,.incarnation=27};
    for(unsigned i=0;i<4;i++)atomic_store(&f->pages[i].state,GC_PAGE_UNCOMMITTED);
    for(unsigned i=head;i<head+count;i++)f->pages[i]=(AllocationPage){.heap_base=f->h.heap_base,.start=(i+1)*GC_HEAP_PAGE_BYTES,
        .limit=(head+count+1)*GC_HEAP_PAGE_BYTES,.incarnation=27,.identity_base=1000,.generation=gen,
        .state=i==head?GC_PAGE_UNCOMMITTED:GC_PAGE_RUN_TAIL,.allocated_top=(i+1)*GC_HEAP_PAGE_BYTES+48,.run_head=head};
    f->pages[head].starts=(uintptr_t)f->starts;f->pages[head].starts_words=count*128;f->pages[head].forward_table=(uintptr_t)f->table;
}
static uint64_t sr_normalize(RetireFixture *d,const RetireFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<sizeof(sr_pointers)/8;i++){unsigned char *p=(unsigned char *)d+sr_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void sr_clone(RetireFixture *d,const RetireFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=sr_normalize(d,s);
    for(unsigned i=0;i<sizeof(sr_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+sr_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void sr_effect(const RetireFixture *before,const RetireFixture *after,uint64_t op,uint64_t result){
    RetireFixture e;memcpy(&e,before,sizeof(e));
    if(result==GC_SR_MORE||result==GC_SR_DONE){
        uintptr_t base=before->h.pages - offsetof(RetireFixture,pages);AllocationPage *head=&e.pages[e.q.head_index];
        if(!op){e.q.owner=base+offsetof(RetireFixture,h);e.q.stage=e.q.cursor=1;e.q.starts=head->starts;e.q.forward_table=head->forward_table;
            head->owner=base+offsetof(RetireFixture,q);head->starts=head->starts_words=head->forward_table=0;atomic_store(&head->state,GC_PAGE_RELEASING);
        }else{
            if(e.q.cursor<e.q.page_count){atomic_store(&e.pages[e.q.head_index+e.q.cursor].state,GC_PAGE_FREE);e.q.cursor++;}
            if(result==GC_SR_DONE){e.h.metadata_committed-=e.q.page_count*1024;head->owner=0;atomic_store(&head->state,GC_PAGE_FREE);e.q.stage=2;}
        }
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void sr_compare(RetireFixture *a,SourceRetirer step,uint64_t op,uint64_t expected){
    RetireFixture c,ba,bc,x,y;sr_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->h,&a->q,op),reference=gc_source_retire_locked(&c.h,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"source retire %u op %llu got %llu C %llu expected %llu\n",sr_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    sr_effect(&ba,a,op,actual);sr_effect(&bc,&c,op,reference);memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(sr_normalize(&x,a)==sr_normalize(&y,&c)&&!memcmp(&x,&y,sizeof(x)));sr_cases++;
}
static void gc_run_source_retire_cases(SourceRetirer step){
    RetireFixture a;sr_cases=0;
    for(unsigned gen=0;gen<2;gen++)for(unsigned head=0;head<4;head++)for(unsigned count=1;count<=4-head;count++){
        sr_fixture(&a,head,count,gen);a.pages[head].metadata_leases=1;sr_compare(&a,step,GC_SR_BEGIN,GC_SR_WAIT);
        a.pages[head].metadata_leases=0;sr_compare(&a,step,GC_SR_BEGIN,GC_SR_MORE);
        sr_compare(&a,step,GC_SR_STEP,GC_SR_WAIT);a.q.starts=0;sr_compare(&a,step,GC_SR_STEP,GC_SR_WAIT);a.q.forward_table=0;
        SourceRetire copied=a.q,saved=copied;RetireFixture before;memcpy(&before,&a,sizeof(before));
        assert(step(&a.h,&copied,GC_SR_STEP)==GC_SR_INVALID&&!memcmp(&before,&a,sizeof(a))&&!memcmp(&saved,&copied,sizeof(saved)));
        while(a.q.stage==1){unsigned index=(unsigned)(a.q.head_index+a.q.cursor);
            sr_compare(&a,step,GC_SR_STEP,a.q.cursor+1>=count?GC_SR_DONE:GC_SR_MORE);
            if(a.q.stage==1){a.pages[index].incarnation++;atomic_store(&a.pages[index].state,GC_PAGE_PREPARING);}
        }
        sr_compare(&a,step,GC_SR_STEP,GC_SR_INVALID);assert(!a.h.committed);
    }
    for(unsigned op=0;op<2;op++)for(unsigned fault=0;fault<34;fault++){
        sr_fixture(&a,0,3,0);if(op){assert(gc_source_retire_locked(&a.h,&a.q,0)==GC_SR_MORE);a.q.starts=a.q.forward_table=0;}
        uint64_t expected=GC_SR_INVALID;
        switch(fault){case 0:a.h.pages=0;break;case 1:a.h.pages++;break;case 2:a.h.page_count=0;break;
        case 3:a.h.page_count=UINT64_MAX;break;case 4:a.h.pages=UINT64_MAX-7;break;
        case 5:a.h.heap_base++;break;case 6:a.h.heap_base=UINT64_MAX-15;break;case 7:a.h.hard_max=0;break;
        case 8:a.h.hard_max=5*GC_HEAP_PAGE_BYTES;break;case 9:a.h.soft_max=a.h.hard_max+1;break;
        case 10:a.h.relocation_reserve=a.h.hard_max+1;break;case 11:a.h.committed=a.h.hard_max+1;break;
        case 12:a.h.pending=a.h.hard_max+1;break;case 13:a.h.metadata_committed=0;break;
        case 14:a.h.metadata_committed=a.h.metadata_max+1;break;case 15:a.h.metadata_pending=a.h.metadata_max;break;
        case 16:a.q.page_count=0;break;case 17:a.q.page_count=5;break;case 18:a.q.head_index=2;break;
        case 19:a.q.incarnation=0;break;case 20:a.q.stage++;break;case 21:a.q.owner=op?0:(uintptr_t)&a.h;break;
        case 22:a.pages[0].identity_base=0;break;case 23:a.pages[0].generation=2;break;
        case 24:a.pages[0].incarnation++;break;case 25:atomic_store(&a.pages[0].state,GC_PAGE_CLOSED);break;
        case 26:a.pages[0].limit+=GC_HEAP_PAGE_BYTES;break;
        case 27:a.q.starts=1;if(op)expected=GC_SR_WAIT;break;
        case 28:a.q.forward_table=1;if(op)expected=GC_SR_WAIT;break;
        case 29:a.q.cursor=op?0:1;break;case 30:a.pages[0].metadata_leases=1;expected=GC_SR_WAIT;break;
        case 31:a.pages[0].owner=8;break;case 32:a.pages[0].forward_table++;break;
        case 33:a.h.metadata_committed=sizeof(a.pages)+3*1024-1;break;}
        sr_compare(&a,step,op,expected);
    }
    for(unsigned fault=0;fault<11;fault++){
        sr_fixture(&a,0,4,1);assert(gc_source_retire_locked(&a.h,&a.q,0)==GC_SR_MORE);a.q.starts=a.q.forward_table=0;
        AllocationPage *p=&a.pages[1];
        switch(fault){case 0:p->heap_base++;break;case 1:p->run_head++;break;case 2:p->start+=16;break;
        case 3:p->limit+=16;break;case 4:p->incarnation++;break;case 5:p->identity_base++;break;
        case 6:p->generation=0;break;case 7:p->owner=1;break;case 8:p->metadata_leases=1;break;
        case 9:p->starts=8;break;case 10:atomic_store(&p->state,GC_PAGE_FREE);break;}
        sr_compare(&a,step,GC_SR_STEP,GC_SR_INVALID);
    }
    // Real pool reservation of a released prefix cannot be clobbered by the
    // remainder of the older source transaction. It owns a new incarnation.
    sr_fixture(&a,0,4,0);sr_compare(&a,step,GC_SR_BEGIN,GC_SR_MORE);a.q.starts=a.q.forward_table=0;
    sr_compare(&a,step,GC_SR_STEP,GC_SR_MORE);
    PageLease replacement={.page_count=1,.generation=GC_OLD};assert(gc_pool_step_locked(&a.h,&replacement,GC_POOL_RESERVE)==GC_POOL_OK);
    assert(replacement.head_index==1&&replacement.incarnation==28);AllocationPage saved_page=a.pages[1];
    sr_compare(&a,step,GC_SR_STEP,GC_SR_MORE);sr_compare(&a,step,GC_SR_STEP,GC_SR_DONE);
    assert(!memcmp(&saved_page,&a.pages[1],sizeof(saved_page))&&a.h.pending==GC_HEAP_PAGE_BYTES&&a.h.metadata_pending==1024);
    assert(a.h.metadata_committed==sizeof(a.pages));assert(gc_pool_step_locked(&a.h,&replacement,GC_POOL_ABORT)==GC_POOL_OK);
    printf("source retire %u C/candidate bounded cleanup, lease, replay and prefix reuse cases PASS\n",sr_cases);
}
#endif
