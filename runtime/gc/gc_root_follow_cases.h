#ifndef KLASSIC_GC_ROOT_FOLLOW_CASES_H
#define KLASSIC_GC_ROOT_FOLLOW_CASES_H
#include "gc_root_follow.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef uint64_t (*RootFollower)(MetadataLeasePool *,RootFollow *,uint64_t);
typedef struct {
    MetadataLeasePool pool;RootFollow q;HeapPool heap;AllocationPage page;MetadataLeaseSlot slots[2];
    MutatorContext ctx;RootSegment segment;RootCell cell;BufferCursor journal;RootChange entries[3];
    ForwardTable table;ForwardCell forward;_Atomic uint64_t starts[128];
} FollowFixture;
static const uint64_t follow_pointers[]={
    offsetof(FollowFixture,pool.heap),offsetof(FollowFixture,pool.slots),offsetof(FollowFixture,q.context),
    offsetof(FollowFixture,q.root.segment),offsetof(FollowFixture,q.root.handle.cell),offsetof(FollowFixture,q.cleanup.page),
    offsetof(FollowFixture,heap.pages),offsetof(FollowFixture,page.starts),offsetof(FollowFixture,page.forward_table),
    offsetof(FollowFixture,slots[0].page),offsetof(FollowFixture,slots[1].page),offsetof(FollowFixture,segment.cells),
    offsetof(FollowFixture,segment.journal),offsetof(FollowFixture,journal.entries),offsetof(FollowFixture,table.cells)};
static unsigned follow_cases;static FILE *follow_vectors;
static void follow_fixture(FollowFixture *f,uint64_t phase,unsigned state){
    memset(f,0,sizeof(*f));f->pool=(MetadataLeasePool){(uintptr_t)&f->heap,(uintptr_t)f->slots,2,1,0};
    f->slots[0].next_free=2;
    // Deliberately unmapped: this operation must not read a managed address.
    f->heap=(HeapPool){.heap_base=UINT64_C(0x100000000),.pages=(uintptr_t)&f->page,.page_count=1};
    f->page=(AllocationPage){.heap_base=f->heap.heap_base,.start=GC_HEAP_PAGE_BYTES,.limit=2*GC_HEAP_PAGE_BYTES,
        .incarnation=27,.identity_base=1024,.state=GC_PAGE_CLOSED,.starts=(uintptr_t)f->starts,.starts_words=128,
        .allocated_top=GC_HEAP_PAGE_BYTES+48,.forward_table=(uintptr_t)&f->table};
    atomic_store(&f->starts[0],4);
    f->table=(ForwardTable){(uintptr_t)&f->forward,1,1};
    uint64_t source=((GC_HEAP_PAGE_BYTES+32)<<16)|UINT64_C(0x1510);
    uint64_t destination=((2*GC_HEAP_PAGE_BYTES+32)<<16)|UINT64_C(0x8630);
    f->forward=(ForwardCell){GC_HEAP_PAGE_BYTES+32,27,state==2?3:state==3?5:state==4?(destination|2):0,0};
    f->ctx=(MutatorContext){.abi_version=GC_ABI_VERSION,.thread_id=17,.adopted_phase=phase};
    f->segment=(RootSegment){.owner=17,.scope=101,.cells=(uintptr_t)&f->cell,.capacity=1,
        .state=GC_ROOT_SEGMENT_ACTIVE,.journal=(uintptr_t)&f->journal};
    f->journal=(BufferCursor){(uintptr_t)f->entries,3,0,0,0};
    f->q.context=(uintptr_t)&f->ctx;f->q.root.segment=(uintptr_t)&f->segment;
    assert(gc_root_step(&f->ctx,&f->q.root,GC_ROOT_RESERVE)==GC_ROOT_OK);
    atomic_store(&f->cell.value,state?source:0);
}
static uint64_t follow_normalize(FollowFixture *dst,const FollowFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(follow_pointers)/8;i++){
        unsigned char *p=(unsigned char *)dst+follow_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)src&&v-(uintptr_t)src<sizeof(*src)){v-=(uintptr_t)src;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}
    }return mask;
}
static void follow_clone(FollowFixture *dst,const FollowFixture *src){
    memcpy(dst,src,sizeof(*dst));uint64_t mask=follow_normalize(dst,src);
    for(unsigned i=0;i<sizeof(follow_pointers)/8;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)dst+follow_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)dst;memcpy(p,&v,8);
    }
}
// Independent effect oracle: lease generation consumption, rooted journal
// publication, or explicit cleanup. All other bytes must be preserved.
static void follow_effect(const FollowFixture *before,const FollowFixture *after,uint64_t result,unsigned effect){
    FollowFixture e;memcpy(&e,before,sizeof(e));
    if(effect==1)e.slots[0].generation++;
    if(effect==2){e.pool.active--;e.pool.free_head=1;e.page.metadata_leases--;
        e.slots[0].page=e.slots[0].incarnation=0;e.slots[0].next_free=before->pool.free_head;
        e.q.cleanup=(MetadataLeaseRequest){0};}
    if(result==GC_RF_ADVANCED){uint64_t next=atomic_load(&before->forward.word)&~UINT64_C(3);
        e.entries[e.journal.top%3]=(RootChange){atomic_load(&e.cell.value),next,e.segment.scope,atomic_load(&e.ctx.adopted_phase)};
        e.journal.top++;atomic_store(&e.journal.published,e.journal.top);atomic_store(&e.cell.value,next);
        atomic_fetch_add(&e.ctx.root_revision,1);}
    assert(!memcmp(&e,after,sizeof(e)));
}
static void follow_compare(FollowFixture *a,RootFollower step,uint64_t op,uint64_t expected,unsigned effect){
    FollowFixture c,ba,bc,x,y;follow_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->pool,&a->q,op),reference=gc_root_follow_locked(&c.pool,&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"root follow case %u got %llu C %llu expected %llu\n",follow_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    follow_effect(&ba,a,actual,effect);follow_effect(&bc,&c,reference,effect);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(follow_normalize(&x,a)==follow_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(follow_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,follow_normalize(&x,&c),follow_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,follow_vectors)==1);assert(fwrite(&x,sizeof(x),1,follow_vectors)==1);assert(fwrite(&y,sizeof(y),1,follow_vectors)==1);}
    follow_cases++;
}
static void gc_run_root_follow_cases(RootFollower step){
    FollowFixture a;follow_cases=0;const char *path=getenv("KLASSIC_GC_ROOT_FOLLOW_VECTORS");
    if(path){follow_vectors=fopen(path,"wb");assert(follow_vectors);assert(fwrite("KLRFOL01",8,1,follow_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(FollowFixture,pool),offsetof(FollowFixture,q),sizeof(follow_pointers)/8};
        assert(fwrite(header,sizeof(header),1,follow_vectors)==1);assert(fwrite(follow_pointers,sizeof(follow_pointers),1,follow_vectors)==1);}
    const uint64_t results[]={GC_RF_DONE,GC_RF_DONE,GC_RF_DONE,GC_RF_WAIT,GC_RF_ADVANCED};
    for(unsigned epoch=0;epoch<32;epoch++)for(unsigned phase=0;phase<8;phase++)for(unsigned state=0;state<5;state++){
        follow_fixture(&a,(UINT64_C(123)<<16)|(epoch<<8)|phase|(phase<<4),state);
        follow_compare(&a,step,GC_RF_STEP,results[state],state!=0);
        assert(!a.pool.active&&!a.page.metadata_leases&&!a.q.cleanup.token&&!a.q.root.value&&!atomic_load(&a.ctx.access_epoch));
    }
    for(unsigned pos=0;pos<9;pos++){
        follow_fixture(&a,0x22,4);a.journal.top=pos;atomic_store(&a.journal.published,pos);atomic_store(&a.journal.consumed,pos);
        follow_compare(&a,step,GC_RF_STEP,GC_RF_ADVANCED,1);
    }
    for(unsigned fault=0;fault<39;fault++){
        follow_fixture(&a,0x22,4);uint64_t result=GC_RF_INVALID;unsigned effect=0;
        switch(fault){case 0:a.q.context=0;break;case 1:a.q.context++;break;case 2:a.q.root.value=1;break;
        case 3:a.q.cleanup.page=8;result=GC_RF_CLEANUP_REQUIRED;break;case 4:a.q.cleanup.incarnation=1;result=GC_RF_CLEANUP_REQUIRED;break;
        case 5:a.q.cleanup.token=1;result=GC_RF_CLEANUP_REQUIRED;break;case 6:a.ctx.abi_version++;break;
        case 7:atomic_store(&a.ctx.state,GC_BLOCKED_NATIVE);break;case 8:atomic_store(&a.ctx.access_epoch,27);break;
        case 9:a.q.root.segment=0;break;case 10:a.q.root.handle.scope++;break;case 11:a.q.root.handle.reuse_generation++;break;
        case 12:a.cell.active=0;break;case 13:a.segment.owner++;break;case 14:atomic_store(&a.cell.value,1);break;
        case 15:atomic_store(&a.cell.value,atomic_load(&a.cell.value)|0x200);break;case 16:atomic_store(&a.starts[0],0);break;
        case 17:atomic_store(&a.page.state,GC_PAGE_READY);break;case 18:a.page.incarnation=0;break;
        case 19:a.pool.free_head=0;a.pool.active=2;result=GC_RF_METADATA_FULL;break;
        case 20:a.slots[0].generation=UINT32_MAX;result=GC_RF_EXHAUSTED;break;
        case 21:a.page.metadata_leases=UINT64_MAX;result=GC_RF_EXHAUSTED;break;
        case 22:a.page.forward_table=0;effect=1;break;case 23:a.page.forward_table++;effect=1;break;
        case 24:atomic_store(&a.table.used,0);effect=1;break;case 25:a.forward.incarnation++;effect=1;break;
        case 26:a.forward.source_offset+=16;effect=1;break;case 27:atomic_store(&a.forward.word,2);effect=1;break;
        case 28:atomic_store(&a.forward.word,atomic_load(&a.cell.value)|2);effect=1;break;
        case 29:a.segment.journal=0;effect=1;break;case 30:a.journal.capacity=0;effect=1;break;
        case 31:atomic_store(&a.journal.published,1);effect=1;break;case 32:atomic_store(&a.journal.consumed,1);effect=1;break;
        case 33:a.journal.entries++;effect=1;break;case 34:a.table.cells=0;effect=1;break;
        case 35:a.q.root.handle.cell++;break;case 36:atomic_store(&a.segment.state,GC_ROOT_SEGMENT_RETIRING);break;
        case 37:a.pool.heap=0;break;case 38:atomic_store(&a.forward.word,1);effect=1;break;}
        follow_compare(&a,step,GC_RF_STEP,result,effect);
    }
    follow_fixture(&a,0x22,4);a.journal.top=3;atomic_store(&a.journal.published,3);
    follow_compare(&a,step,GC_RF_STEP,GC_RF_FLUSH,1);
    // Draining preserves the old root; a fresh invocation consumes a fresh
    // token generation and recomputes forwarding instead of caching raw bits.
    atomic_store(&a.journal.consumed,3);follow_compare(&a,step,GC_RF_STEP,GC_RF_ADVANCED,1);
    follow_fixture(&a,0x22,4);a.journal.top=UINT64_MAX;atomic_store(&a.journal.published,UINT64_MAX);atomic_store(&a.journal.consumed,UINT64_MAX);
    follow_compare(&a,step,GC_RF_STEP,GC_RF_EXHAUSTED,1);
    follow_fixture(&a,0x22,1);follow_compare(&a,step,2,GC_RF_INVALID,0);
    follow_compare(&a,step,GC_RF_CLEANUP,GC_RF_CLEANUP_REQUIRED,0);
    a.q.cleanup=(MetadataLeaseRequest){(uintptr_t)&a.page,27,0};assert(gc_metadata_step_locked(&a.pool,&a.q.cleanup,GC_ML_ACQUIRE)==GC_ML_OK);
    follow_compare(&a,step,GC_RF_STEP,GC_RF_CLEANUP_REQUIRED,0);
    a.q.cleanup.token+=UINT64_C(1)<<32;follow_compare(&a,step,GC_RF_CLEANUP,GC_RF_CLEANUP_REQUIRED,0);
    a.q.cleanup.token-=UINT64_C(1)<<32;follow_compare(&a,step,GC_RF_CLEANUP,GC_RF_DONE,2);
    follow_fixture(&a,0x22,4);atomic_store(&a.ctx.root_revision,UINT64_MAX-1);
    follow_compare(&a,step,GC_RF_STEP,GC_RF_ADVANCED,1);
    follow_fixture(&a,0x22,4);atomic_store(&a.ctx.root_revision,UINT64_MAX);
    follow_compare(&a,step,GC_RF_STEP,GC_RF_EXHAUSTED,1);
    if(follow_vectors){assert(!fclose(follow_vectors));follow_vectors=NULL;}
    printf("root follow %u C/candidate rooted forwarding, retry and lease balance cases PASS\n",follow_cases);
}
#endif
