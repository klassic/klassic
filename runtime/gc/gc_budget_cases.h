#ifndef KLASSIC_GC_BUDGET_CASES_H
#define KLASSIC_GC_BUDGET_CASES_H
#include "gc_budget.h"
#include "gc_pages_cases.h"
typedef uint64_t (*BudgetStep)(HeapPool *,MetadataCharge *,uint64_t);
typedef struct {HeapPool h;MetadataCharge q;} BudgetFixture;
static unsigned budget_cases;
static FILE *budget_vectors;
static uint64_t budget_normalize(BudgetFixture *dst,const BudgetFixture *src) {
    if(dst->q.owner==(uintptr_t)&src->h){dst->q.owner=offsetof(BudgetFixture,h);return 1;}return 0;
}
static void budget_copy(BudgetFixture *dst,const BudgetFixture *src) {
    memcpy(dst,src,sizeof(*dst));if(budget_normalize(dst,src))dst->q.owner=(uintptr_t)&dst->h;
}
static void budget_compare(BudgetFixture *a,BudgetFixture *c,BudgetStep step,uint64_t op,uint64_t expected) {
    BudgetFixture before_a,before,x,y;memcpy(&before_a,a,sizeof(*a));memcpy(&before,c,sizeof(*c));
    uint64_t actual=step(&a->h,&a->q,op),reference=gc_budget_step_locked(&c->h,&c->q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"budget case %u op %llu got %llu C %llu expected %llu\n",budget_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected){assert(!memcmp(&before,c,sizeof(before)));assert(!memcmp(&before_a,a,sizeof(before_a)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(budget_normalize(&x,a)==budget_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(budget_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={op,reference,budget_normalize(&x,c),budget_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,budget_vectors)==1);assert(fwrite(&x,sizeof(x),1,budget_vectors)==1);assert(fwrite(&y,sizeof(y),1,budget_vectors)==1);}
    budget_cases++;
}
static void budget_fixture(BudgetFixture *f) {
    memset(f,0,sizeof(*f));f->h.page_count=8;f->h.metadata_committed=8*sizeof(AllocationPage);
    f->h.metadata_max=f->h.metadata_committed+4096;f->q.bytes=152;
}
static void gc_run_budget_cases(BudgetStep step,PoolStep pool) {
    BudgetFixture a,c;budget_cases=0;const char *path=getenv("KLASSIC_GC_BUDGET_VECTORS");
    if(path){budget_vectors=fopen(path,"wb");assert(budget_vectors);assert(fwrite("KLBUDG01",8,1,budget_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(BudgetFixture,h),offsetof(BudgetFixture,q),1,offsetof(BudgetFixture,q.owner)};
        assert(fwrite(header,sizeof(header),1,budget_vectors)==1);}
#define BUDGET(op,result) budget_compare(&a,&c,step,op,result)
    const uint64_t sizes[]={1,8,32,sizeof(MarkRecord),4096,UINT64_C(1)<<32,UINT64_MAX-8*sizeof(AllocationPage)};
    for(unsigned i=0;i<sizeof(sizes)/8;i++)for(unsigned slack=0;slack<3;slack++)for(unsigned pending=0;pending<2;pending++) {
        budget_fixture(&a);a.q.bytes=sizes[i];a.h.metadata_max=a.h.metadata_committed+sizes[i];
        if(slack==0)a.h.metadata_max--;if(slack==2&&a.h.metadata_max<UINT64_MAX)a.h.metadata_max++;
        a.h.metadata_pending=pending;budget_copy(&c,&a);
        uint64_t expected=a.h.metadata_pending>a.h.metadata_max-a.h.metadata_committed?GC_BUDGET_INVALID:a.q.bytes>a.h.metadata_max-a.h.metadata_committed-a.h.metadata_pending?GC_BUDGET_LIMIT:GC_BUDGET_OK;
        BUDGET(0,expected);
        if(!expected){assert(a.h.metadata_pending==pending+sizes[i]&&a.q.stage==1);BUDGET(2,0);assert(a.h.metadata_pending==pending);BUDGET(2,1);}
    }
    for(unsigned finish=0;finish<2;finish++)for(unsigned i=0;i<6;i++) {
        budget_fixture(&a);a.q.bytes=sizes[i];a.h.metadata_max=UINT64_MAX;budget_copy(&c,&a);
        BUDGET(0,0);BUDGET(0,1);BUDGET(3,1);BUDGET(1,1); // no prepared storage yet
        a.q.allocation=c.q.allocation=0x10000;BUDGET(2,1); // failed cleanup retains reservation
        if(finish){BUDGET(1,0);BUDGET(1,1);BUDGET(2,1);BUDGET(3,1);a.q.allocation=c.q.allocation=0;BUDGET(3,0);BUDGET(3,1);}
        else {a.q.allocation=c.q.allocation=0;BUDGET(2,0);BUDGET(1,1);BUDGET(2,1);}
        assert(!a.h.metadata_pending&&a.h.metadata_committed==8*sizeof(AllocationPage));
    }
    for(unsigned fault=0;fault<18;fault++) {
        budget_fixture(&a);uint64_t op=0;
        switch(fault){case 0:op=4;break;case 1:a.h.page_count=0;break;case 2:a.h.page_count=UINT64_MAX;break;
        case 3:a.h.metadata_committed--;break;case 4:a.h.metadata_committed=a.h.metadata_max+1;break;
        case 5:a.h.metadata_pending=4097;break;case 6:a.q.bytes=0;break;case 7:a.q.owner=8;break;
        case 8:a.q.allocation=8;break;case 9:a.q.stage=1;break;
        default:assert(step(&a.h,&a.q,0)==0);op=1;
            switch(fault){case 10:a.q.owner+=8;break;case 11:a.q.stage=2;break;
            case 12:a.h.metadata_pending--;break;case 13:a.q.allocation=9;break;
            case 14:a.q.allocation=UINT64_MAX-7;break;
            case 15:a.q.stage=2;a.q.allocation=0;op=3;break; // cannot subtract the page table floor
            case 16:a.q.stage=3;op=2;break;case 17:op=3;break;}}
        budget_copy(&c,&a);BUDGET(op,1);
    }
    if(budget_vectors){assert(fclose(budget_vectors)==0);budget_vectors=NULL;}
    // Competing page/record reservations share one ledger. Denial cannot
    // modify either transaction or the heap. A failed external allocation is
    // rolled back before a new page can consume the returned capacity.
    for(unsigned order=0;order<2;order++) {
        PoolFixture f;pool_fixture(&f,0);uint64_t floor=f.h.metadata_committed;
        f.h.metadata_max=floor+GC_HEAP_PAGE_BYTES/128+sizeof(MarkRecord);
        MetadataCharge record={.bytes=sizeof(MarkRecord)},other={.bytes=sizeof(MarkRecord)};
        if(order){assert(step(&f.h,&record,0)==0);assert(pool(&f.h,&f.l,0)==0);}
        else {assert(pool(&f.h,&f.l,0)==0);assert(step(&f.h,&record,0)==0);}
        HeapPool before=f.h;assert(step(&f.h,&other,0)==GC_BUDGET_LIMIT);assert(!memcmp(&before,&f.h,sizeof(before))&&!other.stage);
        PageLease next={.page_count=1};assert(pool(&f.h,&next,0)==GC_POOL_METADATA_LIMIT);assert(!next.stage);
        assert(step(&f.h,&record,2)==0);assert(pool(&f.h,&f.l,2)==0);assert(!f.h.metadata_pending&&f.h.metadata_committed==floor);
        assert(step(&f.h,&other,0)==0);other.allocation=0x10000;assert(step(&f.h,&other,1)==0);
        assert(f.h.metadata_committed==floor+sizeof(MarkRecord));other.allocation=0;assert(step(&f.h,&other,3)==0);
        assert(f.h.metadata_committed==floor);
    }
    printf("metadata budget %u C/candidate admission, cleanup and shared-page limit cases PASS\n",budget_cases);
#undef BUDGET
}
#endif
