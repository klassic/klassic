#ifndef KLASSIC_GC_RESOLVE_CASES_H
#define KLASSIC_GC_RESOLVE_CASES_H
#include "gc_resolve.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*Resolver)(const AllocationPage *,ResolveRequest *,uint64_t);
typedef struct {AllocationPage g;ResolveRequest q;MutatorContext ctx;ForwardTable table;ForwardCell cell;} ResolveFixture;
static const uint64_t resolve_pointers[]={offsetof(ResolveFixture,q.scope.context),offsetof(ResolveFixture,q.scope.held),offsetof(ResolveFixture,g.forward_table),offsetof(ResolveFixture,table.cells)};
static FILE *resolve_vectors;static unsigned resolve_cases;
static void resolve_fixture(ResolveFixture *f) {
    memset(f,0,sizeof(*f));f->cell.source_offset=48;f->cell.incarnation=27;
    f->ctx.abi_version=GC_ABI_VERSION;f->ctx.thread_id=17;
    f->table=(ForwardTable){(uintptr_t)&f->cell,1,1};
    f->g=(AllocationPage){.start=16,.limit=256,.incarnation=27,.state=GC_PAGE_CLOSED,.allocated_top=256,.forward_table=(uintptr_t)&f->table};
    f->q=(ResolveRequest){.reference=(UINT64_C(48)<<16)|0x1510,.scope={.context=(uintptr_t)&f->ctx}};
}
static uint64_t resolve_normalize(ResolveFixture *dst,const ResolveFixture *src) {
    uint64_t mask=0;for(unsigned i=0;i<4;i++){
        unsigned char *p=(unsigned char *)dst+resolve_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void resolve_copy(ResolveFixture *dst,const ResolveFixture *src) {
    memcpy(dst,src,sizeof(*dst));uint64_t mask=resolve_normalize(dst,src);
    for(unsigned i=0;i<4;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+resolve_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
static void resolve_compare(ResolveFixture *a,ResolveFixture *c,Resolver step,uint64_t op,uint64_t expected) {
    ResolveFixture before_a,before,x,y;memcpy(&before_a,a,sizeof(*a));memcpy(&before,c,sizeof(*c));
    uint64_t actual=step(&a->g,&a->q,op),reference=gc_resolve_access(&c->g,&c->q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"resolve case %u op %llu got %llu C %llu expected %llu\n",resolve_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    if(expected&&expected!=GC_RESOLVE_FORWARDED){assert(!memcmp(&before,c,sizeof(before)));assert(!memcmp(&before_a,a,sizeof(before_a)));}
    memcpy(&x,a,sizeof(x));memcpy(&y,c,sizeof(y));assert(resolve_normalize(&x,a)==resolve_normalize(&y,c));assert(!memcmp(&x,&y,sizeof(x)));
    if(resolve_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,c,sizeof(y));uint64_t record[]={op,reference,resolve_normalize(&x,c),resolve_normalize(&y,c)};
        assert(fwrite(record,sizeof(record),1,resolve_vectors)==1);assert(fwrite(&x,sizeof(x),1,resolve_vectors)==1);assert(fwrite(&y,sizeof(y),1,resolve_vectors)==1);}
    resolve_cases++;
}
static void gc_run_resolve_cases(Resolver step) {
    ResolveFixture a,c;resolve_cases=0;const char *path=getenv("KLASSIC_GC_RESOLVE_VECTORS");
    if(path){resolve_vectors=fopen(path,"wb");assert(resolve_vectors);assert(fwrite("KLRSLV01",8,1,resolve_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(ResolveFixture,g),offsetof(ResolveFixture,q),4};assert(fwrite(header,sizeof(header),1,resolve_vectors)==1);assert(fwrite(resolve_pointers,sizeof(resolve_pointers),1,resolve_vectors)==1);}
#define RESOLVE(op,result) resolve_compare(&a,&c,step,op,result)
    for(unsigned remembered=1;remembered<4;remembered++)for(unsigned young=0;young<2;young++)for(unsigned old=0;old<2;old++)for(unsigned remap=0;remap<4;remap++)
    for(unsigned state=0;state<6;state++)for(unsigned gate=0;gate<4;gate++) {
        resolve_fixture(&a);uint64_t color=(remembered<<4)|(1u<<(young+8))|(1u<<(old+10))|(1u<<(remap+12));
        a.q.reference=(UINT64_C(48)<<16)|color;
        uint64_t word=state==0?0:state==1?5:state==2?(UINT64_C(160)<<16)|color|2:state==3?3:state==4?1:a.q.reference|2;
        atomic_store(&a.cell.word,word);atomic_store(&a.cell.access,gate==0?0:gate==1?GC_SOURCE_CLOSED:gate==2?GC_SOURCE_CLOSED+1:GC_SOURCE_CLOSED-1);
        resolve_copy(&c,&a);
        uint64_t expected=state==1?GC_RESOLVE_WAIT:state==2?GC_RESOLVE_FORWARDED:state>=4?GC_RESOLVE_INVALID:gate==0?GC_RESOLVE_OK:gate==3?GC_RESOLVE_EXHAUSTED:GC_RESOLVE_WAIT;
        RESOLVE(0,expected);
        if(expected==GC_RESOLVE_FORWARDED){assert(a.q.destination==((UINT64_C(160)<<16)|color));assert(!a.q.scope.held&&!atomic_load(&a.ctx.access_epoch));}
        if(expected==GC_RESOLVE_OK){assert(a.q.scope.held==(uintptr_t)&a.cell);RESOLVE(0,GC_RESOLVE_INVALID);RESOLVE(1,0);RESOLVE(1,GC_RESOLVE_INVALID);}
    }
    // An evacuated run retains forwarding metadata but can never admit raw
    // source access, even if a malformed entry is empty/retained/claimed.
    for(unsigned page_state=GC_PAGE_EVACUATED;page_state<=GC_PAGE_UNCOMMITTED;page_state++)for(unsigned state=0;state<6;state++)for(unsigned gate=0;gate<4;gate++){
        resolve_fixture(&a);atomic_store(&a.g.state,page_state);
        uint64_t word=state==0?0:state==1?5:state==2?(UINT64_C(160)<<16)|0x1512:state==3?3:state==4?1:a.q.reference|2;
        atomic_store(&a.cell.word,word);atomic_store(&a.cell.access,gate==0?0:gate==1?GC_SOURCE_CLOSED:gate==2?GC_SOURCE_CLOSED+1:GC_SOURCE_CLOSED-1);
        resolve_copy(&c,&a);RESOLVE(0,state==2?GC_RESOLVE_FORWARDED:GC_RESOLVE_INVALID);
        assert(!a.q.scope.held&&!atomic_load(&a.ctx.access_epoch));
    }
    for(unsigned page_state=GC_PAGE_EVACUATED;page_state<=GC_PAGE_UNCOMMITTED;page_state++){
    resolve_fixture(&a);assert(step(&a.g,&a.q,0)==GC_RESOLVE_OK);
    atomic_store(&a.g.state,page_state);resolve_copy(&c,&a);RESOLVE(1,GC_RESOLVE_INVALID);
    }
    // Selection may win after entry: exit must still release the held count.
    resolve_fixture(&a);resolve_copy(&c,&a);RESOLVE(0,0);
    atomic_store(&a.cell.word,5);atomic_store(&c.cell.word,5);atomic_store(&a.cell.access,GC_SOURCE_CLOSED+1);atomic_store(&c.cell.access,GC_SOURCE_CLOSED+1);RESOLVE(1,0);
    for(unsigned fault=0;fault<35;fault++) {
        resolve_fixture(&a);uint64_t op=0;
        switch(fault){case 0:op=2;break;case 1:a.q.destination=8;break;case 2:a.q.scope.ticket=1;break;case 3:a.q.scope.context=0;break;
        case 4:a.q.scope.context++;break;case 5:a.ctx.abi_version=0;break;case 6:a.ctx.thread_id=0;break;case 7:atomic_store(&a.ctx.state,GC_BLOCKED_NATIVE);break;
        case 8:a.q.scope.source_offset=48;break;case 9:a.q.scope.incarnation=27;break;case 10:a.q.scope.held=8;break;case 11:atomic_store(&a.ctx.access_epoch,27);break;
        case 12:a.q.reference=0;break;case 13:a.q.reference|=1;break;case 14:a.q.reference|=UINT64_C(1)<<16;break;case 15:a.q.reference&=~UINT64_C(0x30);break;
        case 16:atomic_store(&a.g.state,GC_PAGE_READY);break;case 17:a.g.incarnation=0;break;case 18:a.g.incarnation++;break;case 19:a.g.start=0;break;
        case 20:a.g.start=256;break;case 21:a.g.limit=UINT64_MAX;break;case 22:a.g.limit++;break;case 23:atomic_store(&a.g.allocated_top,0);break;
        case 24:atomic_store(&a.g.allocated_top,272);break;case 25:atomic_store(&a.g.allocated_top,255);break;case 26:atomic_store(&a.g.allocated_top,48);break;
        case 27:a.g.start=32;break;case 28:a.g.forward_table=0;break;case 29:a.g.forward_table++;break;case 30:atomic_store(&a.table.used,0);break;
        default:assert(step(&a.g,&a.q,0)==0);op=1;
            if(fault==31)a.q.reference=(UINT64_C(64)<<16)|0x1510;
            if(fault==32)a.q.scope.source_offset=64;
            if(fault==33)a.q.scope.incarnation++;
            if(fault==34)a.q.scope.held=8;
        }
        resolve_copy(&c,&a);RESOLVE(op,GC_RESOLVE_INVALID);
    }
    // Bad reference colors are rejected even when a source mapping exists.
    for(unsigned bit=0;bit<16;bit++) {
        resolve_fixture(&a);a.q.reference^=UINT64_C(1)<<bit;resolve_copy(&c,&a);
        GcRef encoded;bool valid=gc_abi_encode_ref(48,(uint16_t)a.q.reference,&encoded);
        RESOLVE(0,valid?GC_RESOLVE_OK:GC_RESOLVE_INVALID);if(valid)RESOLVE(1,0);
    }
    if(resolve_vectors){assert(fclose(resolve_vectors)==0);resolve_vectors=NULL;}
    printf("resolve access %u C/candidate bounded forwarding and guard cases PASS\n",resolve_cases);
#undef RESOLVE
}
#endif
