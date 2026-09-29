#ifndef KLASSIC_GC_MARK_CASES_H
#define KLASSIC_GC_MARK_CASES_H
#include "gc_mark.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*MarkStep)(MarkQueue *,MarkRequest *,uint64_t);
typedef struct {MarkQueue g;MarkRequest q;MarkTask tasks[4];MarkRecord records[5];} MarkFixture;
#define MARK_SPILL_POINTERS(i) offsetof(MarkFixture,records[i].young_spill.next),offsetof(MarkFixture,records[i].young_spill.record),offsetof(MarkFixture,records[i].old_spill.next),offsetof(MarkFixture,records[i].old_spill.record)
static const uint64_t mark_pointers[]={offsetof(MarkFixture,g.tasks),offsetof(MarkFixture,q.record),
    offsetof(MarkFixture,tasks[0].record),offsetof(MarkFixture,tasks[1].record),offsetof(MarkFixture,tasks[2].record),offsetof(MarkFixture,tasks[3].record),offsetof(MarkFixture,g.overflow_head),offsetof(MarkFixture,g.overflow_tail),
    MARK_SPILL_POINTERS(0),MARK_SPILL_POINTERS(1),MARK_SPILL_POINTERS(2),MARK_SPILL_POINTERS(3),MARK_SPILL_POINTERS(4)};
#undef MARK_SPILL_POINTERS
static FILE *mark_vectors;static unsigned mark_cases;
static void mark_input(MarkFixture *f,unsigned i){f->q=(MarkRequest){.record=(uintptr_t)&f->records[i],.reference=((uint64_t)(i+1)<<20)|0x1510,.incarnation=17,.metadata_lease=i+1};}
static void mark_fixture(MarkFixture *f,unsigned capacity) {
    memset(f,0,sizeof(*f));f->g=(MarkQueue){.tasks=(uintptr_t)f->tasks,.capacity=capacity,.free_head=1,.epoch=1};
    for(unsigned i=0;i<capacity;i++)f->tasks[i].next=i+1<capacity?i+2:0;
    for(unsigned i=0;i<5;i++)f->records[i].object_id=i+1;
    mark_input(f,0);
}
static uint64_t mark_normalize(MarkFixture *dst,const MarkFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<sizeof(mark_pointers)/8;i++){
        unsigned char *p=(unsigned char *)dst+mark_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void mark_copy(MarkFixture *dst,const MarkFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=mark_normalize(dst,src);
    for(unsigned i=0;i<sizeof(mark_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+mark_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void mark_compare(MarkFixture *a,MarkFixture *c,MarkStep step,uint64_t op,uint64_t expected) {
    MarkFixture before,x,y;memcpy(&before,c,sizeof(before));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_mark_step_locked(&c->g,&c->q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"mark case %u op %llu got %llu C %llu expected %llu\n",mark_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected)assert(!memcmp(&before,c,sizeof(before)));
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(mark_normalize(&x,a)==mark_normalize(&y,c));
    if(memcmp(&x,&y,sizeof(x))){fprintf(stderr,"mark state mismatch case %u\n",mark_cases);abort();}
    if(mark_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={op,reference,mark_normalize(&x,c),mark_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,mark_vectors)==1);assert(fwrite(&x,sizeof(x),1,mark_vectors)==1);assert(fwrite(&y,sizeof(y),1,mark_vectors)==1);}
    mark_cases++;
}
static void gc_run_mark_cases(MarkStep step) {
    MarkFixture a,c;mark_cases=0;const char *path=getenv("KLASSIC_GC_MARK_VECTORS");
    if(path){mark_vectors=fopen(path,"wb");assert(mark_vectors);assert(fwrite("KLMARK01",8,1,mark_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(MarkFixture,g),offsetof(MarkFixture,q),sizeof(mark_pointers)/8};
        assert(fwrite(header,sizeof(header),1,mark_vectors)==1);assert(fwrite(mark_pointers,sizeof(mark_pointers),1,mark_vectors)==1);}
#define MARK(op,result) mark_compare(&a,&c,step,op,result)
    for(unsigned cap=1;cap<=4;cap++)for(unsigned gen=0;gen<2;gen++)for(unsigned high=0;high<3;high++) {
        mark_fixture(&a,cap);a.g.generation=gen;a.g.epoch=high?((uint64_t)high<<40):1;mark_copy(&c,&a);
        for(unsigned round=0;round<4;round++) {
            MARK(4,GC_MARK_EMPTY);
            for(unsigned i=0;i<cap;i++){
                mark_input(&a,i);mark_input(&c,i);MARK(0,0);
                assert(a.g.queued==i+1&&!a.q.reference);
                assert((gen?a.records[i].old_epoch:a.records[i].young_epoch)==a.g.epoch);
                assert((gen?a.records[i].young_epoch:a.records[i].old_epoch)==0);
                mark_input(&a,i);mark_input(&c,i);MARK(0,GC_MARK_DUPLICATE);
            }
            mark_input(&a,cap);mark_input(&c,cap);MARK(0,GC_MARK_FULL);
            assert((gen?a.records[cap].old_epoch:a.records[cap].young_epoch)==0);
            a.q=c.q=(MarkRequest){0};MARK(1,0);MARK(4,GC_MARK_BUSY);
            MarkRequest stale_a=a.q,stale_c=c.q;
            a.q.cursor=c.q.cursor=256;MARK(3,0);assert(!a.g.in_flight&&a.g.queued==cap);
            for(unsigned i=0;i<cap;i++) {
                a.q=c.q=(MarkRequest){0};MARK(1,0);
                assert(a.q.record==(uintptr_t)&a.records[i+1<cap?i+1:0]);
                if(i+1==cap){assert(a.q.cursor==256&&a.q.serial!=stale_a.serial);
                    MarkRequest fresh_a=a.q,fresh_c=c.q;a.q=stale_a;c.q=stale_c;MARK(2,GC_MARK_INVALID);a.q=fresh_a;c.q=fresh_c;}
                MARK(4,GC_MARK_BUSY);a.q.cursor=c.q.cursor=512;MARK(2,0);
                assert(a.q.metadata_lease&&a.q.reference&&!a.q.index&&!a.q.serial);MARK(2,GC_MARK_INVALID);
            }
            a.q=c.q=(MarkRequest){0};MARK(1,GC_MARK_EMPTY);MARK(4,GC_MARK_EMPTY);
            a.g.epoch++;c.g.epoch++;
        }
    }
    mark_fixture(&a,1);mark_copy(&c,&a);MARK(0,0);MARK(1,0);a.q.cursor=c.q.cursor=7;MARK(2,0);
    // Independently test malformed input, overflow and future-cycle stamps.
    for(unsigned fault=0;fault<37;fault++) {
        mark_fixture(&a,2);uint64_t expected=GC_MARK_INVALID,op=0;
        switch(fault){case 0:op=5;break;case 1:a.g.tasks=0;break;case 2:a.g.tasks++;break;
        case 3:a.g.capacity=0;break;case 4:a.g.capacity=UINT64_MAX;break;case 5:a.g.tasks=UINT64_MAX-7;break;
        case 6:a.g.epoch=0;break;case 7:a.g.generation=2;break;case 8:a.g.queued=3;break;case 9:a.g.in_flight=3;break;
        case 10:a.g.free_head=3;break;case 11:a.g.head=3;break;case 12:a.g.tail=3;break;case 13:a.g.free_head=0;break;
        case 14:a.g.head=1;break;case 15:a.g.tail=1;break;case 16:a.q.record=0;break;case 17:a.q.record++;break;
        case 18:a.q.reference=0;break;case 19:a.q.reference|=UINT64_C(1)<<16;break;
        case 20:a.q.reference|=1;break;case 21:a.q.reference&=~UINT64_C(0x300);break;case 22:a.q.reference|=0x300;break;
        case 23:a.q.incarnation=0;break;case 24:a.q.metadata_lease=0;break;case 25:a.q.index=1;break;
        case 26:a.q.serial=1;break;case 27:a.q.cursor=8;break;case 28:a.records[0].object_id=0;break;case 29:a.records[0].young_epoch=2;break;
        case 30:a.g.last_serial=UINT64_MAX;expected=GC_MARK_EXHAUSTED;break;
        case 31:a.tasks[0].next=1;break;case 32:a.tasks[0].state=1;break;case 33:a.tasks[0].serial=1;break;
        case 34:a.tasks[0].reference=1;break;case 35:a.tasks[0].next=0;break;case 36:a.tasks[0].next=3;break;}
        mark_copy(&c,&a);MARK(op,expected);
    }
    for(unsigned fault=0;fault<13;fault++){
        mark_fixture(&a,2);assert(step(&a.g,&a.q,0)==0);assert(step(&a.g,&a.q,1)==0);
        uint64_t op=2,expected=GC_MARK_INVALID;
        switch(fault){case 0:a.q.index=0;break;case 1:a.q.index=3;break;case 2:a.q.serial++;break;
        case 3:a.q.record++;break;case 4:a.q.reference++;break;case 5:a.q.incarnation++;break;case 6:a.q.metadata_lease++;break;
        case 7:a.q.cursor=1;op=GC_MARK_YIELD;break;case 8:a.tasks[0].cursor=8;break;case 9:a.tasks[0].next=1;break;
        case 10:a.tasks[0].state=1;break;case 11:a.g.last_serial=0;break;case 12:a.g.last_serial=UINT64_MAX;op=3;expected=GC_MARK_EXHAUSTED;break;}
        mark_copy(&c,&a);MARK(op,expected);
    }
    for(unsigned color=0;color<65536;color+=17){mark_fixture(&a,1);a.q.reference=(UINT64_C(16)<<16)|color;mark_copy(&c,&a);MARK(0,gc_abi_valid_metadata((uint16_t)color)?0:GC_MARK_INVALID);}
    if(mark_vectors){assert(fclose(mark_vectors)==0);mark_vectors=NULL;}
    printf("mark queue %u C/candidate stamp, ownership and preservation cases PASS\n",mark_cases);
#undef MARK
}
#endif
