#ifndef KLASSIC_GC_MARK_SPILL_CASES_H
#define KLASSIC_GC_MARK_SPILL_CASES_H
#include "gc_mark_spill.h"
#include "gc_mark_cases.h"
typedef uint64_t (*MarkSpill)(MarkQueue *,MarkRequest *,uint64_t);
static FILE *mark_spill_vectors;static unsigned mark_spill_cases;
static void spill_equal(MarkFixture *a,MarkFixture *c) {
    MarkFixture x,y;memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(mark_normalize(&x,a)==mark_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
}
static void spill_compare(MarkFixture *a,MarkFixture *c,MarkSpill step,uint64_t op,uint64_t expected) {
    MarkFixture before,x,y;memcpy(&before,c,sizeof(before));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_mark_spill_locked(&c->g,&c->q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"mark spill %u op %llu got %llu C %llu expected %llu\n",mark_spill_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected)assert(!memcmp(&before,c,sizeof(before)));spill_equal(a,c);
    if(mark_spill_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={op,reference,mark_normalize(&x,c),mark_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,mark_spill_vectors)==1);assert(fwrite(&x,sizeof(x),1,mark_spill_vectors)==1);assert(fwrite(&y,sizeof(y),1,mark_spill_vectors)==1);}
    mark_spill_cases++;
}
static void spill_core(MarkFixture *a,MarkFixture *c,MarkStep core,uint64_t op,uint64_t expected) {
    assert(core(&a->g,&a->q,op)==expected);assert(gc_mark_step_locked(&c->g,&c->q,op)==expected);spill_equal(a,c);
}
static void spill_pending(MarkFixture *f,MarkStep core,MarkSpill spill) {
    mark_fixture(f,1);assert(core(&f->g,&f->q,0)==0);assert(core(&f->g,&f->q,1)==0);
    MarkRequest parent=f->q;mark_input(f,1);assert(spill(&f->g,&f->q,0)==0);f->q=parent;assert(core(&f->g,&f->q,2)==0);f->q=(MarkRequest){0};
}
static void gc_run_mark_spill_cases(MarkStep core,MarkSpill step) {
    MarkFixture a,c;mark_spill_cases=0;const char *path=getenv("KLASSIC_GC_MARK_SPILL_VECTORS");
    if(path){mark_spill_vectors=fopen(path,"wb");assert(mark_spill_vectors);assert(fwrite("KLMSPL01",8,1,mark_spill_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(MarkFixture,g),offsetof(MarkFixture,q),sizeof(mark_pointers)/8};
        assert(fwrite(header,sizeof(header),1,mark_spill_vectors)==1);assert(fwrite(mark_pointers,sizeof(mark_pointers),1,mark_spill_vectors)==1);}
#define SPILL(op,result) spill_compare(&a,&c,step,op,result)
#define CORE(op,result) spill_core(&a,&c,core,op,result)
    for(unsigned cap=1;cap<=4;cap++)for(unsigned gen=0;gen<2;gen++)for(unsigned high=0;high<2;high++) {
        mark_fixture(&a,cap);a.g.generation=gen;a.g.epoch=high?(UINT64_C(1)<<40):1;mark_copy(&c,&a);
        for(unsigned round=0;round<4;round++) {
            SPILL(1,GC_MARK_EMPTY);MarkRequest claims_a[4],claims_c[4];
            for(unsigned i=0;i<cap;i++){mark_input(&a,i);mark_input(&c,i);SPILL(0,0);CORE(1,0);claims_a[i]=a.q;claims_c[i]=c.q;}
            for(unsigned i=cap;i<5;i++) {
                mark_input(&a,i);mark_input(&c,i);CORE(0,GC_MARK_FULL);SPILL(0,0);
                assert(a.g.in_flight==cap&&a.g.queued==0&&a.g.overflow_count==i+1-cap);
                assert((gen?a.records[i].old_epoch:a.records[i].young_epoch)==a.g.epoch);
                mark_input(&a,i);mark_input(&c,i);SPILL(0,GC_MARK_DUPLICATE);SPILL(1,GC_MARK_FULL);CORE(4,GC_MARK_BUSY);
            }
            a.q=claims_a[0];c.q=claims_c[0];a.q.cursor=c.q.cursor=7;CORE(2,0);CORE(4,GC_MARK_BUSY);
            for(unsigned i=cap;i<5;i++){
                SPILL(1,0);assert(a.g.overflow_count==4-i&&a.g.queued==1);
                MarkTask zero={0};assert(!memcmp(gen?&a.records[i].old_spill:&a.records[i].young_spill,&zero,sizeof(zero)));
                a.q=c.q=(MarkRequest){0};CORE(1,0);assert(a.q.record==(uintptr_t)&a.records[i]);CORE(2,0);
            }
            for(unsigned i=1;i<cap;i++){a.q=claims_a[i];c.q=claims_c[i];CORE(2,0);}
            CORE(4,GC_MARK_EMPTY);SPILL(1,GC_MARK_EMPTY);assert(!a.g.overflow_head&&!a.g.overflow_tail);
            a.g.epoch++;c.g.epoch++;
        }
    }
    // Invalid overflow admission preserves the unmarked child and its lease.
    for(unsigned fault=0;fault<13;fault++){
        mark_fixture(&a,1);assert(core(&a.g,&a.q,0)==0);assert(core(&a.g,&a.q,1)==0);mark_input(&a,1);
        uint64_t op=0,expected=GC_MARK_INVALID;
        if(fault<8)((uint64_t *)&a.records[1].young_spill)[fault]=1;
        else if(fault==8){a.g.last_serial=UINT64_MAX;expected=GC_MARK_EXHAUSTED;}
        else if(fault==9)op=2;
        else if(fault==10){a.g.overflow_head=(uintptr_t)&a.records[2];a.g.overflow_tail=a.g.overflow_head;a.g.overflow_count=1;}
        else if(fault==11){a.g.overflow_head=1;a.g.overflow_tail=1;a.g.overflow_count=1;}
        else {a.g.overflow_head=(uintptr_t)&a.records[2];a.g.overflow_tail=a.g.overflow_head;a.g.overflow_count=UINT64_MAX-1;expected=GC_MARK_EXHAUSTED;}
        mark_copy(&c,&a);SPILL(op,expected);
    }
    for(unsigned fault=0;fault<15;fault++){
        spill_pending(&a,core,step);MarkTask *s=&a.records[1].young_spill;
        switch(fault){case 0:s->state=0;break;case 1:s->serial=0;break;case 2:s->serial++;break;
        case 3:s->record=0;break;case 4:s->reference=0;break;case 5:s->incarnation=0;break;case 6:s->metadata_lease=0;break;
        case 7:s->cursor=8;break;case 8:s->next=1;break;case 9:s->next=a.g.overflow_head;break;
        case 10:a.g.overflow_tail=(uintptr_t)&a.records[2];break;case 11:a.records[1].young_epoch++;break;
        case 12:a.tasks[0].state=2;break;case 13:a.tasks[0].next=1;break;case 14:a.g.overflow_count=2;break;}
        mark_copy(&c,&a);SPILL(1,GC_MARK_INVALID);
    }
    // Existing overflow gets precedence over a fresh offer even with a free slot.
    spill_pending(&a,core,step);mark_copy(&c,&a);mark_input(&a,2);mark_input(&c,2);SPILL(0,0);
    assert(a.g.queued==0&&a.g.overflow_count==2);SPILL(1,0);a.q=c.q=(MarkRequest){0};CORE(1,0);assert(a.q.record==(uintptr_t)&a.records[1]);CORE(2,0);
    SPILL(1,0);a.q=c.q=(MarkRequest){0};CORE(1,0);assert(a.q.record==(uintptr_t)&a.records[2]);CORE(2,0);CORE(4,GC_MARK_EMPTY);
    if(mark_spill_vectors){assert(fclose(mark_spill_vectors)==0);mark_spill_vectors=NULL;}
    printf("mark spill %u C/candidate saturation, refill and preservation cases PASS\n",mark_spill_cases);
#undef SPILL
#undef CORE
}
#endif
