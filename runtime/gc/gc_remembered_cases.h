#ifndef KLASSIC_GC_REMEMBERED_CASES_H
#define KLASSIC_GC_REMEMBERED_CASES_H
#include "gc_remembered.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RememberedStep)(RememberedSet *,RememberedJob *,uint64_t);
typedef struct {RememberedSet s;RememberedJob q;uint64_t bits[2][5];} RememberedFixture;
static unsigned remembered_cases;static FILE *remembered_vectors;
static const uint64_t remembered_pointers[]={offsetof(RememberedFixture,s.bits[0]),offsetof(RememberedFixture,s.bits[1])};
static uint64_t remembered_normalize(RememberedFixture *d,const RememberedFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<2;i++){uint64_t v=d->s.bits[i];if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){d->s.bits[i]=v-(uintptr_t)s;mask|=UINT64_C(1)<<i;}}return mask;}
static void remembered_clone(RememberedFixture *d,const RememberedFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=remembered_normalize(d,s);for(unsigned i=0;i<2;i++)if(mask&(1u<<i))d->s.bits[i]+=(uintptr_t)d;}
static void remembered_fixture(RememberedFixture *f,unsigned ep,unsigned bank){memset(f,0,sizeof(*f));
    f->s=(RememberedSet){.start=4096,.limit=4096+257*8,.incarnation=19,.bits={(uintptr_t)f->bits[0],(uintptr_t)f->bits[1]},.words=5,.current=bank,.epoch=0x10022|(ep<<8),.last_phase=0x10022|(ep<<8)};
    f->q=(RememberedJob){.offset=4096,.incarnation=19,.phase=f->s.epoch};}
static unsigned remembered_count(const uint64_t *bits){unsigned count=0;for(unsigned i=0;i<5;i++)for(unsigned j=0;j<64;j++)count+=(unsigned)((bits[i]>>j)&1);return count;}
static void remembered_effect(const RememberedFixture *before,const RememberedFixture *after,uint64_t op,uint64_t result){
    RememberedFixture e;memcpy(&e,before,sizeof(e));RememberedSet *s=&e.s;RememberedJob *q=&e.q;
    if(result==GC_RS_OK){
        if(op==GC_RS_RECORD||op==GC_RS_ACK){unsigned bit=(unsigned)((q->offset-s->start)/8);uint64_t mask=UINT64_C(1)<<(bit%64);
            if(op==GC_RS_RECORD||q->keep)e.bits[s->current][bit/64]|=mask;
            if(op==GC_RS_RECORD){if(q->phase<s->epoch)e.bits[1-s->current][bit/64]|=mask;if(q->phase>s->last_phase)s->last_phase=q->phase;}
            else s->active_serial=s->active_offset=0;
        }else if(op==GC_RS_FLIP){s->previous_epoch=s->epoch;s->epoch=s->last_phase=q->phase;s->current=1-s->current;s->cursor=0;
        }else if(op==GC_RS_TAKE){unsigned bit=(unsigned)((after->q.offset-s->start)/8);assert(e.bits[1-s->current][bit/64]&(UINT64_C(1)<<(bit%64)));
            e.bits[1-s->current][bit/64]&=~(UINT64_C(1)<<(bit%64));s->active_serial=++s->last_serial;s->active_offset=after->q.offset;s->cursor=bit/64;
            q->offset=s->active_offset;q->incarnation=s->incarnation;q->phase=s->epoch;q->serial=s->active_serial;q->keep=0;
        }
        s->current_count=remembered_count(e.bits[s->current]);s->pending=remembered_count(e.bits[1-s->current]);
    }else if(result==GC_RS_MORE)s->cursor=(s->cursor+q->budget)%s->words;
    assert(!memcmp(&e,after,sizeof(e)));
}
static void remembered_compare(RememberedFixture *a,RememberedStep step,uint64_t op,uint64_t expected){
    RememberedFixture c,ba,bc,x,y;remembered_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->s,&a->q,op),reference=gc_remembered_step_locked(&c.s,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"remembered case %u op %llu got %llu C %llu expected %llu\n",remembered_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    remembered_effect(&ba,a,op,actual);remembered_effect(&bc,&c,op,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(remembered_normalize(&x,a)==remembered_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(remembered_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,remembered_normalize(&x,&c),remembered_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,remembered_vectors)==1);assert(fwrite(&x,sizeof(x),1,remembered_vectors)==1);assert(fwrite(&y,sizeof(y),1,remembered_vectors)==1);}remembered_cases++;
}
static void gc_run_remembered_cases(RememberedStep step){
    RememberedFixture a;remembered_cases=0;const char *path=getenv("KLASSIC_GC_REMEMBERED_VECTORS");
    if(path){remembered_vectors=fopen(path,"wb");assert(remembered_vectors);assert(fwrite("KLREM001",8,1,remembered_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(RememberedFixture,s),offsetof(RememberedFixture,q),2};assert(fwrite(header,sizeof(header),1,remembered_vectors)==1);assert(fwrite(remembered_pointers,sizeof(remembered_pointers),1,remembered_vectors)==1);}
    const unsigned offsets[]={0,63,64,255,256};
    for(unsigned ep=0;ep<32;ep++)for(unsigned bank=0;bank<2;bank++)for(unsigned k=0;k<5;k++){
        remembered_fixture(&a,ep,bank);a.q.offset+=offsets[k]*8;uint64_t offset=a.q.offset,phase=a.q.phase;
#define RS(op,status) remembered_compare(&a,step,op,status)
        RS(GC_RS_RECORD,GC_RS_OK);RS(GC_RS_RECORD,GC_RS_OK);assert(a.s.current_count==1);
        a.q=(RememberedJob){.incarnation=19,.phase=(phase+0x10000)^0x1000};a.q.phase=(a.q.phase&~UINT64_C(15))|1;
        RS(GC_RS_FLIP,GC_RS_OK);RS(GC_RS_CHECK,GC_RS_WAIT);
        a.q=(RememberedJob){.budget=1};for(unsigned i=0;i<offsets[k]/64;i++)RS(GC_RS_TAKE,GC_RS_MORE);
        RS(GC_RS_TAKE,GC_RS_OK);RememberedJob claim=a.q;assert(claim.offset==offset);RS(GC_RS_CHECK,GC_RS_WAIT);
        a.q=(RememberedJob){.budget=1};RS(GC_RS_TAKE,GC_RS_WAIT);
        // An old buffered record arrives after this slot was already taken.
        a.q=(RememberedJob){.offset=offset,.incarnation=19,.phase=phase};RS(GC_RS_RECORD,GC_RS_OK);
        assert(a.s.pending==1&&a.s.current_count==1);
        a.q=claim;a.q.keep=k%2;RS(GC_RS_ACK,GC_RS_OK);RS(GC_RS_ACK,GC_RS_INVALID);
        a.q=(RememberedJob){.budget=1};RS(GC_RS_TAKE,GC_RS_OK);a.q.keep=1;RS(GC_RS_ACK,GC_RS_OK);RS(GC_RS_CHECK,GC_RS_EMPTY);
        a.q=(RememberedJob){.incarnation=19,.phase=(a.s.epoch+0x10000)^0x1000};RS(GC_RS_FLIP,GC_RS_OK);
        a.q=(RememberedJob){.offset=offset,.incarnation=19,.phase=phase};RS(GC_RS_RECORD,GC_RS_STALE);
        a.q=(RememberedJob){.budget=256};RS(GC_RS_TAKE,GC_RS_OK);assert(a.q.offset==offset);a.q.keep=0;RS(GC_RS_ACK,GC_RS_OK);RS(GC_RS_CHECK,GC_RS_EMPTY);
        assert(!a.s.current_count&&!a.s.pending);a.q=(RememberedJob){.budget=1};RS(GC_RS_TAKE,GC_RS_EMPTY);
#undef RS
    }
    for(unsigned fault=0;fault<32;fault++){
        remembered_fixture(&a,0,0);uint64_t op=GC_RS_RECORD,expected=GC_RS_INVALID;
        switch(fault){case 0:op=5;break;case 1:a.s.start=0;break;case 2:a.s.limit=a.s.start;break;case 3:a.s.limit++;break;
        case 4:a.s.incarnation=0;break;case 5:a.s.current=2;break;case 6:a.s.words=0;break;case 7:a.s.words--;break;
        case 8:a.s.bits[0]=0;break;case 9:a.s.bits[0]++;break;case 10:a.s.bits[1]=a.s.bits[0];break;
        case 11:a.s.epoch|=0x8000;break;case 12:a.s.last_phase=0;break;case 13:a.s.current_count=258;break;
        case 14:a.s.pending=258;break;case 15:a.s.cursor=5;break;case 16:a.s.active_serial=1;break;
        case 17:a.s.active_offset=4096;break;case 18:a.s.previous_epoch=a.s.epoch;break;
        case 19:a.q.incarnation++;break;case 20:a.q.offset--;break;case 21:a.q.offset=a.s.limit;break;
        case 22:a.q.serial=1;break;case 23:a.q.phase^=0x1000;break;case 24:a.q.phase=0;break;
        case 25:op=GC_RS_TAKE;a.q.budget=0;break;case 26:op=GC_RS_TAKE;a.q.budget=257;break;
        case 27:op=GC_RS_FLIP;a.q.phase=0x21022;break;case 28:op=GC_RS_FLIP;a.q.phase=0x11021;break;
        case 29:op=GC_RS_FLIP;a.q.phase=0x20021;break;case 30:op=GC_RS_ACK;break;
        case 31:a.q.phase=0x22;break;}
        remembered_compare(&a,step,op,expected);
    }
    for(unsigned fault=0;fault<10;fault++) {
        remembered_fixture(&a,0,0);assert(gc_remembered_step_locked(&a.s,&a.q,GC_RS_RECORD)==GC_RS_OK);
        a.q=(RememberedJob){.incarnation=19,.phase=0x21021};assert(gc_remembered_step_locked(&a.s,&a.q,GC_RS_FLIP)==GC_RS_OK);
        a.q=(RememberedJob){.budget=256};uint64_t op=GC_RS_TAKE,expected=GC_RS_INVALID;
        if(fault>=1&&fault<=6){assert(gc_remembered_step_locked(&a.s,&a.q,GC_RS_TAKE)==GC_RS_OK);op=GC_RS_ACK;}
        switch(fault){case 0:a.s.last_serial=UINT64_MAX;expected=GC_RS_EXHAUSTED;break;
        case 1:a.q=(RememberedJob){.incarnation=19,.phase=0x30021};op=GC_RS_FLIP;expected=GC_RS_WAIT;break;
        case 2:a.q.keep=2;break;case 3:a.q.serial++;break;case 4:a.q.incarnation++;break;
        case 5:a.q.phase^=0x10000;break;case 6:a.q.offset+=8;break;
        case 7:a.s.previous_epoch^=0x1000;break;
        case 8:memset(a.bits[0],0,sizeof(a.bits[0]));break;
        case 9:a.bits[0][0]=0;a.bits[0][4]=2;break;}
        remembered_compare(&a,step,op,expected);
    }
    remembered_fixture(&a,0,0);assert(gc_remembered_step_locked(&a.s,&a.q,GC_RS_RECORD)==GC_RS_OK);
    a.q=(RememberedJob){.incarnation=19,.phase=0x21021};assert(gc_remembered_step_locked(&a.s,&a.q,GC_RS_FLIP)==GC_RS_OK);
    a.s.last_serial=UINT64_MAX-1;a.q=(RememberedJob){.budget=256};remembered_compare(&a,step,GC_RS_TAKE,GC_RS_OK);
    assert(a.q.serial==UINT64_MAX);a.q.keep=1;remembered_compare(&a,step,GC_RS_ACK,GC_RS_OK);
    a.q=(RememberedJob){.incarnation=19,.phase=0x30021};remembered_compare(&a,step,GC_RS_FLIP,GC_RS_OK);
    a.q=(RememberedJob){.budget=256};remembered_compare(&a,step,GC_RS_TAKE,GC_RS_EXHAUSTED);
    if(remembered_vectors){assert(fclose(remembered_vectors)==0);remembered_vectors=NULL;}
    printf("remembered slots %u C/candidate flip, late-entry, claim and retention cases PASS\n",remembered_cases);
}
#endif
