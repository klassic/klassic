#ifndef KLASSIC_GC_MARK_RETARGET_CASES_H
#define KLASSIC_GC_MARK_RETARGET_CASES_H
#include "gc_mark_cases.h"
#include "gc_mark_retarget.h"
typedef uint64_t (*MarkRetargeter)(MarkQueue *,MarkRetarget *);
typedef struct {MarkFixture mark;MarkRetarget q;} RetargetFixture;
static uint64_t retarget_pointers[40];static unsigned retarget_pointer_count,retarget_cases;static FILE *retarget_vectors;
static void retarget_fixture(RetargetFixture *f,unsigned capacity,unsigned occupied,unsigned which,unsigned generation,uint64_t epoch){
    memset(f,0,sizeof(*f));mark_fixture(&f->mark,capacity);f->mark.g.generation=generation;f->mark.g.epoch=epoch;
    for(unsigned i=0;i<occupied;i++){mark_input(&f->mark,i);assert(gc_mark_step_locked(&f->mark.g,&f->mark.q,GC_MARK_OFFER)==GC_MARK_OK);}
    for(unsigned i=0;i<which;i++){assert(gc_mark_step_locked(&f->mark.g,&f->mark.q,GC_MARK_TAKE)==GC_MARK_OK);assert(gc_mark_step_locked(&f->mark.g,&f->mark.q,GC_MARK_YIELD)==GC_MARK_OK);}
    assert(gc_mark_step_locked(&f->mark.g,&f->mark.q,GC_MARK_TAKE)==GC_MARK_OK);
    f->q=(MarkRetarget){.job=(uintptr_t)&f->mark.q,.destination={.record=f->mark.q.record,
        .reference=(UINT64_C(1024)<<16)|0x8630,.incarnation=37,.metadata_lease=101}};
}
static uint64_t retarget_normalize(RetargetFixture *f,const RetargetFixture *owner){
    uint64_t mask=0;for(unsigned i=0;i<retarget_pointer_count;i++){
        unsigned char *p=(unsigned char *)f+retarget_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)owner&&v-(uintptr_t)owner<sizeof(*owner)){v-=(uintptr_t)owner;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void retarget_clone(RetargetFixture *d,const RetargetFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=retarget_normalize(d,s);
    for(unsigned i=0;i<retarget_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)d+retarget_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);
    }
}
static void retarget_effect(const RetargetFixture *before,const RetargetFixture *after,uint64_t result){
    RetargetFixture e;memcpy(&e,before,sizeof(e));
    if(result==GC_MARK_OK){MarkRequest *j=&e.mark.q;MarkTask *t=&e.mark.tasks[j->index-1];
        e.q.retired=*j;e.q.retired.index=e.q.retired.serial=e.q.retired.cursor=0;
        j->reference=t->reference=e.q.destination.reference;j->incarnation=t->incarnation=e.q.destination.incarnation;
        j->metadata_lease=t->metadata_lease=e.q.destination.metadata_lease;t->cursor=j->cursor;
        e.mark.g.last_serial++;j->serial=t->serial=e.mark.g.last_serial;e.q.destination=(MarkRequest){0};
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void retarget_compare(RetargetFixture *a,MarkRetargeter step,uint64_t expected){
    RetargetFixture c,ba,bc,x,y;retarget_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->mark.g,&a->q),reference=gc_mark_retarget_locked(&c.mark.g,&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"mark retarget case %u got %llu C %llu expected %llu\n",retarget_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    retarget_effect(&ba,a,actual);retarget_effect(&bc,&c,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(retarget_normalize(&x,a)==retarget_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(retarget_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,retarget_normalize(&x,&c),retarget_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,retarget_vectors)==1);assert(fwrite(&x,sizeof(x),1,retarget_vectors)==1);assert(fwrite(&y,sizeof(y),1,retarget_vectors)==1);}
    retarget_cases++;
}
static void gc_run_mark_retarget_cases(MarkRetargeter step,MarkStep mark){
    RetargetFixture a;retarget_cases=retarget_pointer_count=0;
    for(unsigned i=0;i<sizeof(mark_pointers)/8;i++)retarget_pointers[retarget_pointer_count++]=offsetof(RetargetFixture,mark)+mark_pointers[i];
    const uint64_t pointers[]={offsetof(RetargetFixture,q.job),offsetof(RetargetFixture,q.destination.record),offsetof(RetargetFixture,q.retired.record)};
    for(unsigned i=0;i<sizeof(pointers)/8;i++)retarget_pointers[retarget_pointer_count++]=pointers[i];
    const char *path=getenv("KLASSIC_GC_MARK_RETARGET_VECTORS");
    if(path){retarget_vectors=fopen(path,"wb");assert(retarget_vectors);assert(fwrite("KLMRTG01",8,1,retarget_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(RetargetFixture,mark.g),offsetof(RetargetFixture,q),retarget_pointer_count};
        assert(fwrite(header,sizeof(header),1,retarget_vectors)==1);assert(fwrite(retarget_pointers,8,retarget_pointer_count,retarget_vectors)==retarget_pointer_count);}
    const uint64_t cursors[]={0,8,128,4096};
    for(unsigned cap=1;cap<=4;cap++)for(unsigned used=1;used<=cap;used++)for(unsigned which=0;which<used;which++)
        for(unsigned generation=0;generation<2;generation++)for(unsigned high=0;high<3;high++)for(unsigned cursor=0;cursor<4;cursor++){
            retarget_fixture(&a,cap,used,which,generation,high?((uint64_t)high<<40):1);a.mark.q.cursor=cursors[cursor];
            MarkRequest stale=a.mark.q;retarget_compare(&a,step,GC_MARK_OK);
            assert(mark(&a.mark.g,&stale,GC_MARK_FINISH)==GC_MARK_INVALID);
            assert(mark(&a.mark.g,&stale,GC_MARK_YIELD)==GC_MARK_INVALID);
            assert(a.mark.g.in_flight==1&&a.mark.g.queued==used-1&&a.mark.q.cursor==cursors[cursor]);
        }
    for(unsigned young=0;young<2;young++)for(unsigned old=0;old<2;old++)for(unsigned remap=0;remap<4;remap++)for(unsigned remember=1;remember<4;remember++){
        retarget_fixture(&a,1,1,0,1,27);
        // Even a same-address/color refresh with an independent lease rotates
        // the claim; a copied pre-handoff claim must not regain ownership.
        a.q.destination.reference=(a.mark.q.reference&~UINT64_C(0xffff))|(UINT64_C(0x100)<<young)|(UINT64_C(0x400)<<old)|(UINT64_C(0x1000)<<remap)|(remember<<4);
        a.q.destination.incarnation=a.mark.q.incarnation;retarget_compare(&a,step,GC_MARK_OK);
    }
    for(unsigned fault=0;fault<58;fault++){
        retarget_fixture(&a,2,1,0,0,1);uint64_t result=GC_MARK_INVALID;MarkRequest *j=&a.mark.q;MarkTask *t=&a.mark.tasks[0];
        switch(fault){case 0:a.mark.g.tasks=0;break;case 1:a.mark.g.tasks++;break;case 2:a.mark.g.capacity=0;break;
        case 3:a.mark.g.capacity=UINT64_MAX;break;case 4:a.mark.g.epoch=0;break;case 5:a.mark.g.generation=2;break;
        case 6:a.mark.g.queued=3;break;case 7:a.mark.g.in_flight=3;break;case 8:a.mark.g.free_head=3;break;
        case 9:a.mark.g.head=3;break;case 10:a.mark.g.tail=3;break;case 11:a.mark.g.overflow_head=1;break;
        case 12:a.q.job=0;break;case 13:a.q.job++;break;case 14:a.q.retired.record=8;break;
        case 15:a.q.retired.reference=1;break;case 16:a.q.retired.incarnation=1;break;case 17:a.q.retired.metadata_lease=1;break;
        case 18:a.q.retired.index=1;break;case 19:a.q.retired.serial=1;break;case 20:a.q.retired.cursor=1;break;
        case 21:j->index=0;break;case 22:j->index=3;break;case 23:j->serial=0;break;case 24:j->serial++;break;
        case 25:j->record=0;break;case 26:j->record++;break;case 27:j->incarnation=0;break;case 28:j->metadata_lease=0;break;
        case 29:j->cursor=1;break;case 30:a.q.destination.record=(uintptr_t)&a.mark.records[1];break;
        case 31:a.q.destination.incarnation=0;break;case 32:a.q.destination.metadata_lease=0;break;
        case 33:a.q.destination.metadata_lease=j->metadata_lease;break;case 34:a.q.destination.index=1;break;
        case 35:a.q.destination.serial=1;break;case 36:a.q.destination.cursor=8;break;case 37:a.q.destination.reference=0;break;
        case 38:a.q.destination.reference|=0x100;break;case 39:j->reference=0;break;case 40:j->reference|=1;break;
        case 41:t->state=1;break;case 42:t->next=1;break;case 43:t->serial++;break;case 44:t->record++;break;
        case 45:t->reference++;break;case 46:t->incarnation++;break;case 47:t->metadata_lease++;break;
        case 48:t->cursor=8;break;case 49:a.mark.g.last_serial=0;break;case 50:a.mark.records[0].object_id=0;break;
        case 51:a.mark.records[0].young_epoch=0;break;case 52:a.mark.records[0].young_epoch=2;break;
        case 53:a.mark.g.last_serial=UINT64_MAX;result=GC_MARK_EXHAUSTED;break;case 54:a.mark.g.in_flight=0;break;
        case 55:a.mark.g.overflow_head=a.mark.g.overflow_tail=(uintptr_t)&a.mark.records[1];a.mark.g.overflow_count=UINT64_MAX;break;
        case 56:a.mark.g.free_head=0;break;case 57:a.mark.g.tasks=UINT64_MAX-7;break;}
        retarget_compare(&a,step,result);
    }
    retarget_fixture(&a,1,1,0,0,1);a.mark.g.last_serial=UINT64_MAX-1;retarget_compare(&a,step,GC_MARK_OK);
    assert(a.mark.q.serial==UINT64_MAX);
    if(retarget_vectors){assert(!fclose(retarget_vectors));retarget_vectors=NULL;}
    printf("mark retarget %u C/candidate cursor, claim and lease handoff cases PASS\n",retarget_cases);
}
#endif
