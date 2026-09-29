#ifndef KLASSIC_GC_FORWARD_CASES_H
#define KLASSIC_GC_FORWARD_CASES_H
#include "gc_forward.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*ForwardStep)(ForwardCell *,ForwardRequest *,uint64_t);
typedef struct {ForwardCell g;ForwardRequest q;} ForwardFixture;
static unsigned forward_cases;static FILE *forward_vectors;
static void forward_fixture(ForwardFixture *f) {
    memset(f,0,sizeof(*f));f->g.source_offset=16;f->g.incarnation=7;f->q=(ForwardRequest){16,7,1,0};
}
static void forward_compare(ForwardFixture *a,ForwardFixture *c,ForwardStep step,uint64_t op,uint64_t expected) {
    ForwardFixture before_a,before;memcpy(&before_a,a,sizeof(*a));memcpy(&before,c,sizeof(*c));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_forward_step(&c->g,&c->q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"forward case %u op %llu got %llu C %llu expected %llu\n",forward_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    assert(!memcmp(a,c,sizeof(*a)));
    if(expected){assert(!memcmp(&before,c,sizeof(before)));assert(!memcmp(&before_a,a,sizeof(before_a)));}
    if(forward_vectors){uint64_t record[]={op,reference,0,0};assert(fwrite(record,sizeof(record),1,forward_vectors)==1);assert(fwrite(&before,sizeof(before),1,forward_vectors)==1);assert(fwrite(c,sizeof(*c),1,forward_vectors)==1);}
    forward_cases++;
}
static void gc_run_forward_cases(ForwardStep step) {
    ForwardFixture a,c;forward_cases=0;const char *path=getenv("KLASSIC_GC_FORWARD_VECTORS");
    if(path){forward_vectors=fopen(path,"wb");assert(forward_vectors);assert(fwrite("KLFWD001",8,1,forward_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(ForwardFixture,g),offsetof(ForwardFixture,q),0};assert(fwrite(header,sizeof(header),1,forward_vectors)==1);}
#define FWD(op,result) forward_compare(&a,&c,step,op,result)
    const uint64_t sources[]={16,GC_HEAP_PAGE_BYTES,GC_MAX_OFFSET&~UINT64_C(15)};
    const uint64_t incarnations[]={1,UINT64_C(1)<<63,UINT64_MAX};
    const uint64_t tickets[]={1,UINT64_C(1)<<32,UINT64_MAX>>2};
    const uint64_t colors[]={0x1510,0x2a20,0x8930};
    for(unsigned s=0;s<3;s++)for(unsigned i=0;i<3;i++)for(unsigned t=0;t<3;t++)for(unsigned k=0;k<3;k++)for(unsigned retain=0;retain<2;retain++) {
        forward_fixture(&a);a.g.source_offset=a.q.source_offset=sources[s];a.g.incarnation=a.q.incarnation=incarnations[i];a.q.ticket=0;memcpy(&c,&a,sizeof(c));
        FWD(1,GC_FWD_EMPTY);a.q.ticket=c.q.ticket=tickets[t];FWD(0,0);FWD(0,GC_FWD_BUSY);
        a.q.ticket=c.q.ticket=0;FWD(1,GC_FWD_BUSY);
        atomic_store(&a.g.access,GC_SOURCE_CLOSED);atomic_store(&c.g.access,GC_SOURCE_CLOSED);
        a.q.ticket=c.q.ticket=tickets[t]==1?2:1;FWD(3,GC_FWD_INVALID);
        a.q.ticket=c.q.ticket=tickets[t];
        if(retain){FWD(3,0);FWD(3,1);FWD(0,GC_FWD_RETAINED);a.q.ticket=c.q.ticket=0;FWD(1,GC_FWD_RETAINED);}
        else {
            uint64_t dest=((sources[s]==16?32:16)<<16)|colors[k];a.q.destination=c.q.destination=dest;
            FWD(2,0);FWD(2,1);a.q.destination=c.q.destination=0;FWD(3,1);FWD(0,GC_FWD_PUBLISHED);
            a.q.ticket=c.q.ticket=0;FWD(1,0);assert(a.q.destination==dest);
        }
    }
    for(unsigned y=0;y<2;y++)for(unsigned o=0;o<2;o++)for(unsigned r=0;r<4;r++)for(unsigned rem=1;rem<4;rem++) {
        forward_fixture(&a);atomic_store(&a.g.word,5);atomic_store(&a.g.access,GC_SOURCE_CLOSED);a.q.destination=(UINT64_C(32)<<16)|(0x100u<<y)|(0x400u<<o)|(0x1000u<<r)|(rem<<4);memcpy(&c,&a,sizeof(c));FWD(2,0);
        a.q.ticket=c.q.ticket=0;a.q.destination=c.q.destination=0;FWD(1,0);
    }
    for(unsigned fault=0;fault<20;fault++) {
        forward_fixture(&a);uint64_t op=0;
        switch(fault){case 0:op=4;break;case 1:a.g.source_offset=0;break;case 2:a.g.source_offset++;break;
        case 3:a.g.source_offset=UINT64_MAX;break;case 4:a.g.incarnation=0;break;case 5:a.q.source_offset+=16;break;
        case 6:a.q.incarnation++;break;case 7:a.q.ticket=0;break;case 8:a.q.ticket=UINT64_MAX;break;
        case 9:a.q.destination=1;break;case 10:op=1;break;
        default:op=2;atomic_store(&a.g.word,5);a.q.destination=(UINT64_C(32)<<16)|0x1510;
            switch(fault){case 11:a.q.destination=0;break;case 12:a.q.destination=(UINT64_C(16)<<16)|0x1510;break;
            case 13:a.q.destination|=1;break;case 14:a.q.destination|=UINT64_C(1)<<16;break;
            case 15:a.q.destination&=~UINT64_C(0x30);break;case 16:a.q.destination|=0x200;break;
            case 17:a.q.destination|=0x800;break;case 18:a.q.destination|=0x2000;break;
            case 19:a.q.ticket=2;break;}}
        memcpy(&c,&a,sizeof(c));FWD(op,GC_FWD_INVALID);
    }
    const uint64_t bad_words[]={1,2,4,6,7,UINT64_MAX,((UINT64_C(16)<<16)|0x1512)};
    for(unsigned i=0;i<sizeof(bad_words)/8;i++)for(unsigned op=0;op<2;op++) {
        forward_fixture(&a);atomic_store(&a.g.word,bad_words[i]);if(op)a.q.ticket=0;memcpy(&c,&a,sizeof(c));FWD(op,GC_FWD_INVALID);
    }
    // Owning the ticket is insufficient before source admission closes/drains.
    for(unsigned readers=0;readers<2;readers++) {
        forward_fixture(&a);atomic_store(&a.g.word,5);atomic_store(&a.g.access,readers?GC_SOURCE_CLOSED+1:0);
        a.q.destination=(UINT64_C(32)<<16)|0x1510;memcpy(&c,&a,sizeof(c));FWD(2,GC_FWD_INVALID);
        a.q.destination=c.q.destination=0;FWD(3,GC_FWD_INVALID);
    }
    if(forward_vectors){assert(fclose(forward_vectors)==0);forward_vectors=NULL;}
    printf("forwarding %u C/candidate claim, publication and retention cases PASS\n",forward_cases);
#undef FWD
}
#endif
