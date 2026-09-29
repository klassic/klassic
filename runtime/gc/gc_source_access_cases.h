#ifndef KLASSIC_GC_SOURCE_ACCESS_CASES_H
#define KLASSIC_GC_SOURCE_ACCESS_CASES_H
#include "gc_source_access.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*SourceAccess)(ForwardCell *,SourceAccessRequest *,uint64_t);
typedef struct {ForwardCell g;SourceAccessRequest q;MutatorContext ctx;} SourceFixture;
static const uint64_t source_pointers[]={offsetof(SourceFixture,q.context),offsetof(SourceFixture,q.held)};
static FILE *source_vectors;static unsigned source_cases;
static void source_fixture(SourceFixture *f) {
    memset(f,0,sizeof(*f));f->g.source_offset=32;f->g.incarnation=27;
    f->ctx.abi_version=GC_ABI_VERSION;f->ctx.thread_id=17;
    f->q=(SourceAccessRequest){(uintptr_t)&f->ctx,32,27,0,0};
}
static uint64_t source_normalize(SourceFixture *dst,const SourceFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<2;i++){
        unsigned char *p=(unsigned char *)dst+source_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void source_copy(SourceFixture *dst,const SourceFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=source_normalize(dst,src);
    for(unsigned i=0;i<2;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+source_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void source_compare(SourceFixture *a,SourceFixture *c,SourceAccess step,uint64_t op,uint64_t expected) {
    SourceFixture before_a,before,x,y;memcpy(&before_a,a,sizeof(*a));memcpy(&before,c,sizeof(*c));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_source_access_step(&c->g,&c->q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"source access case %u op %llu got %llu C %llu expected %llu\n",source_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected){assert(!memcmp(&before,c,sizeof(before)));assert(!memcmp(&before_a,a,sizeof(before_a)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(source_normalize(&x,a)==source_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(source_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={op,reference,source_normalize(&x,c),source_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,source_vectors)==1);assert(fwrite(&x,sizeof(x),1,source_vectors)==1);assert(fwrite(&y,sizeof(y),1,source_vectors)==1);}
    source_cases++;
}
static void gc_run_source_access_cases(SourceAccess step) {
    SourceFixture a,c;source_cases=0;const char *path=getenv("KLASSIC_GC_SOURCE_ACCESS_VECTORS");
    if(path){source_vectors=fopen(path,"wb");assert(source_vectors);assert(fwrite("KLSACC01",8,1,source_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(SourceFixture,g),offsetof(SourceFixture,q),2};assert(fwrite(header,sizeof(header),1,source_vectors)==1);assert(fwrite(source_pointers,sizeof(source_pointers),1,source_vectors)==1);}
#define SOURCE(op,result) source_compare(&a,&c,step,op,result)
    const uint64_t counts[]={0,1,2,63,UINT64_C(1)<<32,GC_SOURCE_CLOSED-2,GC_SOURCE_CLOSED-1};
    for(unsigned count=0;count<7;count++)for(unsigned closed=0;closed<2;closed++)for(unsigned retained=0;retained<2;retained++) {
        source_fixture(&a);atomic_store(&a.g.access,counts[count]|(closed?GC_SOURCE_CLOSED:0));atomic_store(&a.g.word,retained?3:0);source_copy(&c,&a);
        uint64_t expected=closed?GC_SA_WAIT:counts[count]==GC_SOURCE_CLOSED-1?GC_SA_EXHAUSTED:GC_SA_OK;SOURCE(0,expected);
        if(!expected){assert(a.q.held==(uintptr_t)&a.g&&atomic_load(&a.ctx.access_epoch)==27);SOURCE(0,GC_SA_INVALID);SOURCE(1,0);SOURCE(1,GC_SA_INVALID);assert(atomic_load(&a.g.access)==counts[count]);}
    }
    for(unsigned count=0;count<7;count++) {
        source_fixture(&a);a.q.context=0;a.q.ticket=UINT64_MAX>>2;atomic_store(&a.g.word,(a.q.ticket<<2)|1);atomic_store(&a.g.access,counts[count]);source_copy(&c,&a);
        SOURCE(3,GC_SA_WAIT);SOURCE(2,0);SOURCE(2,0);SOURCE(3,counts[count]?GC_SA_WAIT:GC_SA_OK);
    }
    // Reopen only a drained retained source. No published mapping can reopen.
    for(unsigned forwarding=0;forwarding<4;forwarding++)for(unsigned count=0;count<3;count++) {
        source_fixture(&a);a.q.context=0;atomic_store(&a.g.word,forwarding==2?(UINT64_C(64)<<16)|0x1512:forwarding==1?5:forwarding);
        atomic_store(&a.g.access,count==0?GC_SOURCE_CLOSED:count==1?GC_SOURCE_CLOSED+1:0);source_copy(&c,&a);
        SOURCE(4,forwarding==3&&count==0?GC_SA_OK:GC_SA_INVALID);
        if(forwarding==3&&count==0){a.q.context=(uintptr_t)&a.ctx;c.q.context=(uintptr_t)&c.ctx;SOURCE(0,0);SOURCE(1,0);}
    }
    // Release an already admitted guard after claim/selection. The context
    // marker and request only clear after the guarded count actually decreases.
    source_fixture(&a);source_copy(&c,&a);SOURCE(0,0);
    atomic_store(&a.g.word,5);atomic_store(&c.g.word,5);atomic_store(&a.g.access,GC_SOURCE_CLOSED+1);atomic_store(&c.g.access,GC_SOURCE_CLOSED+1);SOURCE(1,0);
    assert(atomic_load(&a.g.access)==GC_SOURCE_CLOSED&&!atomic_load(&a.ctx.access_epoch)&&!a.q.held);
    for(unsigned fault=0;fault<21;fault++) {
        source_fixture(&a);uint64_t op=0,expected=GC_SA_INVALID;
        switch(fault){case 0:op=5;break;case 1:a.g.source_offset=0;break;case 2:a.g.source_offset++;break;
        case 3:a.g.source_offset=UINT64_MAX;break;case 4:a.g.incarnation=0;break;case 5:a.q.source_offset+=16;break;
        case 6:a.q.incarnation++;break;case 7:a.q.context=0;break;case 8:a.q.context++;break;case 9:a.q.ticket=1;break;
        case 10:a.ctx.abi_version=0;break;case 11:a.ctx.thread_id=0;break;case 12:atomic_store(&a.ctx.state,GC_AT_SAFEPOINT);break;
        case 13:a.q.held=8;break;case 14:atomic_store(&a.ctx.access_epoch,27);break;
        case 15:atomic_store(&a.g.word,5);expected=GC_SA_WAIT;break;case 16:atomic_store(&a.g.word,(UINT64_C(64)<<16)|0x1512);expected=GC_SA_WAIT;break;
        default:assert(step(&a.g,&a.q,0)==0);op=1;
            switch(fault){case 17:a.q.held=8;break;case 18:atomic_store(&a.ctx.access_epoch,28);break;
            case 19:atomic_store(&a.g.access,GC_SOURCE_CLOSED);break;case 20:atomic_store(&a.g.access,0);break;}}
        source_copy(&c,&a);SOURCE(op,expected);
    }
    for(unsigned op=2;op<=3;op++)for(unsigned fault=0;fault<8;fault++) {
        source_fixture(&a);a.q.context=0;a.q.ticket=1;atomic_store(&a.g.word,5);
        switch(fault){case 0:a.q.context=(uintptr_t)&a.ctx;break;case 1:a.q.held=8;break;case 2:a.q.ticket=0;break;
        case 3:a.q.ticket=UINT64_MAX;break;case 4:a.q.ticket=2;break;case 5:atomic_store(&a.g.word,0);break;
        case 6:atomic_store(&a.g.word,3);break;case 7:atomic_store(&a.g.word,(UINT64_C(64)<<16)|0x1512);break;}
        source_copy(&c,&a);SOURCE(op,GC_SA_INVALID);
    }
    for(unsigned fault=0;fault<3;fault++) {
        source_fixture(&a);a.q.context=0;atomic_store(&a.g.word,3);atomic_store(&a.g.access,GC_SOURCE_CLOSED);
        if(fault==0)a.q.ticket=1;else if(fault==1)a.q.context=(uintptr_t)&a.ctx;else a.q.held=8;
        source_copy(&c,&a);SOURCE(4,GC_SA_INVALID);
    }
    const uint64_t incarnations[]={1,UINT64_C(1)<<63,UINT64_MAX};
    for(unsigned i=0;i<3;i++){source_fixture(&a);a.g.incarnation=a.q.incarnation=incarnations[i];source_copy(&c,&a);SOURCE(0,0);SOURCE(1,0);}
    if(source_vectors){assert(fclose(source_vectors)==0);source_vectors=NULL;}
    printf("source access %u C/candidate admission, drain and reopen cases PASS\n",source_cases);
#undef SOURCE
}
#endif
