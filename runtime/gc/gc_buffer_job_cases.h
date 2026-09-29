#ifndef KLASSIC_GC_BUFFER_JOB_CASES_H
#define KLASSIC_GC_BUFFER_JOB_CASES_H
#include "gc_buffer_job.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*BufferJobStep)(MutatorContext *,BarrierJob *,uint64_t);
typedef struct {MutatorContext ctx;BarrierJob q;BarrierEntry entries[300],output[257];} BufferJobFixture;
static const uint64_t bj_pointers[]={offsetof(BufferJobFixture,ctx.satb.entries),offsetof(BufferJobFixture,ctx.remembered.entries),offsetof(BufferJobFixture,q.output)};
static unsigned bj_cases;static FILE *bj_vectors;
static BufferCursor *bj_buffer(BufferJobFixture *f){return f->q.selector?&f->ctx.remembered:&f->ctx.satb;}
static BufferConsumer *bj_control(BufferJobFixture *f){return f->q.selector?&f->ctx.remembered_consumer:&f->ctx.satb_consumer;}
static void bj_fixture(BufferJobFixture *f,uint64_t selector,uint64_t cap,uint64_t begin,uint64_t count,uint64_t budget,uint64_t state){
    memset(f,0,sizeof(*f));f->ctx.abi_version=GC_ABI_VERSION;f->ctx.thread_id=17;atomic_store(&f->ctx.state,state);
    f->q=(BarrierJob){.identity=17,.selector=selector,.output=(uintptr_t)f->output,.capacity=budget};
    BufferCursor *b=bj_buffer(f);*b=(BufferCursor){.entries=(uintptr_t)f->entries,.capacity=cap,.top=UINT64_MAX};
    atomic_store(&b->consumed,begin);atomic_store(&b->published,begin+count);bj_control(f)->last_serial=27;
    // Source owner may be inside its store barrier while the collector copies.
    atomic_store(&f->ctx.access_epoch,state%2?7:0);
    for(uint64_t i=0;i<count;i++)f->entries[(begin+i)%cap]=(BarrierEntry){.previous={i*16},.slot={.owner={i*16+16},.byte_offset=i*8,.page_incarnation=19,.record_epoch=(i%32)<<8|0x22,.metadata_lease=i+1}};
    memset(f->output,0xab,sizeof(f->output));
}
static uint64_t bj_normalize(BufferJobFixture *d,const BufferJobFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<3;i++){unsigned char *p=(unsigned char *)d+bj_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void bj_clone(BufferJobFixture *d,const BufferJobFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=bj_normalize(d,s);
    for(unsigned i=0;i<3;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+bj_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void bj_effect(const BufferJobFixture *before,const BufferJobFixture *after,uint64_t op,uint64_t result){
    BufferJobFixture e;memcpy(&e,before,sizeof(e));
    if(result==GC_BJ_OK){BufferCursor *b=bj_buffer(&e);BufferConsumer *c=bj_control(&e);
        if(op==GC_BJ_CLAIM){uint64_t begin=atomic_load(&b->consumed),n=atomic_load(&b->published)-begin;if(n>e.q.capacity)n=e.q.capacity;
            c->serial=(++c->last_serial)*2+e.q.selector;c->end=begin+n;c->cursor=begin;e.q.serial=c->serial;e.q.begin=begin;e.q.end=c->end;e.q.written=0;
        }else if(op==GC_BJ_COPY){uint64_t n=e.q.end-e.q.begin;for(uint64_t i=0;i<n;i++)e.output[i]=e.entries[(e.q.begin+i)%b->capacity];e.q.written=n;c->cursor=e.q.end;
        }else{atomic_store(&b->consumed,e.q.end);c->serial=c->end=c->cursor=0;}
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void bj_compare(BufferJobFixture *a,BufferJobStep step,uint64_t op,uint64_t expected){
    BufferJobFixture c,ba,bc,x,y;bj_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->ctx,&a->q,op),reference=gc_buffer_job_step(&c.ctx,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"buffer job case %u op %llu got %llu C %llu expected %llu\n",bj_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    bj_effect(&ba,a,op,actual);bj_effect(&bc,&c,op,reference);memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(bj_normalize(&x,a)==bj_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(bj_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,bj_normalize(&x,&c),bj_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,bj_vectors)==1);assert(fwrite(&x,sizeof(x),1,bj_vectors)==1);assert(fwrite(&y,sizeof(y),1,bj_vectors)==1);}bj_cases++;
}
static void gc_run_buffer_job_cases(BufferJobStep step){
    BufferJobFixture a;bj_cases=0;const char *path=getenv("KLASSIC_GC_BUFFER_JOB_VECTORS");
    if(path){bj_vectors=fopen(path,"wb");assert(bj_vectors);assert(fwrite("KLBJOB01",8,1,bj_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(BufferJobFixture,ctx),offsetof(BufferJobFixture,q),3};
        assert(fwrite(header,sizeof(header),1,bj_vectors)==1);assert(fwrite(bj_pointers,sizeof(bj_pointers),1,bj_vectors)==1);}
    const uint64_t caps[]={1,3,300},budgets[]={1,256};
    for(unsigned sel=0;sel<2;sel++)for(unsigned state=0;state<4;state++)for(unsigned c=0;c<3;c++)for(unsigned st=0;st<2;st++)for(unsigned count=0;count<2;count++)for(unsigned budget=0;budget<2;budget++){
        uint64_t cap=caps[c],begin=st?UINT64_MAX-cap:0;bj_fixture(&a,sel,cap,begin,count?cap:1,budgets[budget],state);
        bj_compare(&a,step,GC_BJ_CLAIM,GC_BJ_OK);bj_compare(&a,step,GC_BJ_ACK,GC_BJ_WAIT);
        bj_compare(&a,step,GC_BJ_COPY,GC_BJ_OK);assert(atomic_load(&bj_buffer(&a)->consumed)==begin&&bj_control(&a)->serial);
        bj_compare(&a,step,GC_BJ_COPY,GC_BJ_INVALID);bj_compare(&a,step,GC_BJ_ACK,GC_BJ_OK);bj_compare(&a,step,GC_BJ_ACK,GC_BJ_INVALID);
    }
    for(unsigned sel=0;sel<2;sel++)for(unsigned fault=0;fault<21;fault++){
        bj_fixture(&a,sel,3,0,2,2,0);BufferCursor *b=bj_buffer(&a);BufferConsumer *c=bj_control(&a);uint64_t op=0,result=GC_BJ_INVALID;
        switch(fault){case 0:op=3;break;case 1:a.ctx.abi_version++;break;case 2:a.q.identity=0;break;case 3:a.q.identity++;break;
        case 4:a.q.selector=2;break;case 5:atomic_store(&a.ctx.state,GC_DETACHED);break;case 6:b->entries=0;break;case 7:b->entries++;break;
        case 8:b->capacity=0;break;case 9:b->capacity=UINT64_C(1)<<32;break;case 10:b->entries=UINT64_MAX-7;break;
        case 11:atomic_store(&b->consumed,3);break;case 12:atomic_store(&b->published,4);break;case 13:c->serial=1;result=GC_BJ_WAIT;break;
        case 14:c->end=1;break;case 15:c->cursor=1;break;case 16:a.q.capacity=0;break;case 17:a.q.capacity=257;break;
        case 18:atomic_store(&b->published,0);result=GC_BJ_EMPTY;break;case 19:c->last_serial=UINT64_MAX;result=GC_BJ_EXHAUSTED;break;
        case 20:a.ctx.thread_id=0;break;}
        bj_compare(&a,step,op,result);
    }
    for(unsigned sel=0;sel<2;sel++)for(unsigned fault=0;fault<19;fault++){
        bj_fixture(&a,sel,3,0,2,2,0);assert(gc_buffer_job_step(&a.ctx,&a.q,GC_BJ_CLAIM)==0);
        BufferConsumer *c=bj_control(&a);uint64_t op=GC_BJ_COPY,result=GC_BJ_INVALID;
        switch(fault){case 0:a.q.serial=0;break;case 1:a.q.serial++;break;case 2:c->last_serial++;break;case 3:a.q.begin++;break;
        case 4:a.q.end++;break;case 5:a.q.end=c->end=0;break;case 6:a.q.end=c->end=3;break;case 7:c->cursor++;break;
        case 8:a.q.written=1;break;case 9:a.q.output=0;break;case 10:a.q.output++;break;case 11:a.q.capacity=1;break;
        case 12:a.q.capacity=257;break;case 13:a.q.output=UINT64_MAX-7;break;
        case 14:op=GC_BJ_ACK;c->cursor=a.q.end;result=GC_BJ_WAIT;break;
        case 15:op=GC_BJ_CLAIM;result=GC_BJ_WAIT;break;case 16:a.ctx.thread_id++;break;
        case 17:a.q.selector^=1;break;case 18:op=GC_BJ_ACK;a.q.written=2;result=GC_BJ_WAIT;break;}
        bj_compare(&a,step,op,result);
    }
    for(unsigned sel=0;sel<2;sel++){
        bj_fixture(&a,sel,3,0,2,1,0);bj_control(&a)->last_serial=UINT64_MAX/2-1;
        bj_compare(&a,step,GC_BJ_CLAIM,GC_BJ_OK);bj_compare(&a,step,GC_BJ_COPY,GC_BJ_OK);bj_compare(&a,step,GC_BJ_ACK,GC_BJ_OK);
        bj_compare(&a,step,GC_BJ_CLAIM,GC_BJ_EXHAUSTED);
        bj_fixture(&a,sel,300,0,300,256,0);assert(gc_buffer_job_step(&a.ctx,&a.q,0)==0);a.q.end=bj_control(&a)->end=257;bj_compare(&a,step,GC_BJ_COPY,GC_BJ_INVALID);
        bj_fixture(&a,sel,3,0,1,1,0);bj_compare(&a,step,GC_BJ_CLAIM,GC_BJ_OK);
        a.entries[1]=a.entries[0];atomic_store(&bj_buffer(&a)->published,2);
        bj_compare(&a,step,GC_BJ_COPY,GC_BJ_OK);BarrierJob stale=a.q;bj_compare(&a,step,GC_BJ_ACK,GC_BJ_OK);
        assert(atomic_load(&bj_buffer(&a)->consumed)==1);
        bj_compare(&a,step,GC_BJ_CLAIM,GC_BJ_OK);BarrierJob current=a.q;a.q=stale;bj_compare(&a,step,GC_BJ_ACK,GC_BJ_INVALID);a.q=current;
        bj_compare(&a,step,GC_BJ_COPY,GC_BJ_OK);bj_compare(&a,step,GC_BJ_ACK,GC_BJ_OK);
    }
    // Both buffers can hold claims at once. Selector tagging rejects an old
    // SATB request redirected to a remset control with the same claim counter.
    bj_fixture(&a,0,3,0,2,1,0);a.ctx.remembered=a.ctx.satb;a.ctx.remembered_consumer.last_serial=27;
    bj_compare(&a,step,GC_BJ_CLAIM,GC_BJ_OK);BarrierJob satb=a.q;a.q.selector=1;
    bj_compare(&a,step,GC_BJ_CLAIM,GC_BJ_OK);BarrierJob remembered=a.q;
    a.q=satb;a.q.selector=1;bj_compare(&a,step,GC_BJ_COPY,GC_BJ_INVALID);
    a.q=remembered;bj_compare(&a,step,GC_BJ_COPY,GC_BJ_OK);bj_compare(&a,step,GC_BJ_ACK,GC_BJ_OK);
    a.q=satb;bj_compare(&a,step,GC_BJ_COPY,GC_BJ_OK);bj_compare(&a,step,GC_BJ_ACK,GC_BJ_OK);
    if(bj_vectors){assert(!fclose(bj_vectors));bj_vectors=NULL;}
    printf("buffer jobs %u C/candidate SATB/remset claim, copy and handoff cases PASS\n",bj_cases);
}
#endif
