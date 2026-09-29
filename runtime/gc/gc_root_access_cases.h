#ifndef KLASSIC_GC_ROOT_ACCESS_CASES_H
#define KLASSIC_GC_ROOT_ACCESS_CASES_H
#include "gc_root_follow_cases.h"
#include "gc_root_access.h"
typedef uint64_t (*RootAccessor)(MetadataLeasePool *,RootAccess *);
typedef struct {FollowFixture f;RootAccess q;} AccessFixture;
static const uint64_t access_pointers[]={
    offsetof(AccessFixture,q.follow.context),offsetof(AccessFixture,q.follow.root.segment),
    offsetof(AccessFixture,q.follow.root.handle.cell),offsetof(AccessFixture,q.follow.cleanup.page),
    offsetof(AccessFixture,q.access.scope.context),offsetof(AccessFixture,q.access.scope.held)};
static unsigned access_cases;static FILE *access_vectors;
static uint64_t access_offset(unsigned i){return i<sizeof(follow_pointers)/8?follow_pointers[i]:access_pointers[i-sizeof(follow_pointers)/8];}
#define ACCESS_POINTERS ((sizeof(follow_pointers)+sizeof(access_pointers))/8)
static uint64_t access_normalize(AccessFixture *d,const AccessFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<ACCESS_POINTERS;i++){unsigned char *p=(unsigned char *)d+access_offset(i);uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void access_clone(AccessFixture *d,const AccessFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=access_normalize(d,s);
    for(unsigned i=0;i<ACCESS_POINTERS;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+access_offset(i);uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void access_fixture(AccessFixture *a,uint64_t phase,unsigned state){memset(a,0,sizeof(*a));follow_fixture(&a->f,phase,state);a->q.follow=a->f.q;}
static void access_compare(AccessFixture *a,RootAccessor step,uint64_t expected){
    AccessFixture c,before,x,y;access_clone(&c,a);memcpy(&before,&c,sizeof(c));
    uint64_t actual=step(&a->f.pool,&a->q),reference=gc_root_access_locked(&c.f.pool,&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"root access case %u got %llu C %llu expected %llu\n",access_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t amask=access_normalize(&x,a),cmask=access_normalize(&y,&c);assert(amask==cmask);assert(!memcmp(&x,&y,sizeof(x)));
    if(access_vectors){memcpy(&x,&before,sizeof(x));uint64_t record[]={0,reference,access_normalize(&x,&c),cmask};
        assert(fwrite(record,sizeof(record),1,access_vectors)==1);assert(fwrite(&x,sizeof(x),1,access_vectors)==1);assert(fwrite(&y,sizeof(y),1,access_vectors)==1);}
    access_cases++;
}
static void access_finish(AccessFixture *a){
    assert(a->q.follow.cleanup.token&&a->f.pool.active==1&&a->f.page.metadata_leases==1);
    assert(a->q.access.scope.held==(uintptr_t)&a->f.forward&&atomic_load(&a->f.ctx.access_epoch));
    assert(gc_resolve_access(&a->f.page,&a->q.access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
    assert(gc_metadata_step_locked(&a->f.pool,&a->q.follow.cleanup,GC_ML_RELEASE)==GC_ML_OK);
    assert(!a->f.pool.active&&!a->f.page.metadata_leases&&!atomic_load(&a->f.ctx.access_epoch));
}
/* Two forwarding edges with a full journal and a modeled native interval.
 * All heap bases stay unmapped: only native metadata/root storage is used. */
static void root_access_chain(RootAccessor step){
    for(unsigned round=0;round<32;round++){
        AccessFixture a;access_fixture(&a,0x10022|(round<<8),1);
        AllocationPage pages[3];ForwardCell cells[3];ForwardTable tables[3];
        _Atomic uint64_t starts[3][128]={0};
        a.f.heap.pages=(uintptr_t)pages;a.f.heap.page_count=3;a.f.journal.capacity=1;
        for(unsigned i=0;i<3;i++){
            uint64_t start=(i+1)*GC_HEAP_PAGE_BYTES,reference=((start+32)<<16)|UINT64_C(0x1510);
            pages[i]=(AllocationPage){.heap_base=a.f.heap.heap_base,.start=start,.limit=start+GC_HEAP_PAGE_BYTES,
                .incarnation=27+i,.identity_base=1024+i,.run_head=i,
                .state=GC_PAGE_CLOSED,.starts=(uintptr_t)starts[i],.starts_words=128,
                .allocated_top=start+48,.forward_table=(uintptr_t)&tables[i]};
            atomic_store(&starts[i][0],4);tables[i]=(ForwardTable){(uintptr_t)&cells[i],1,1};
            cells[i]=(ForwardCell){start+32,27+i,i<2?(reference+(GC_HEAP_PAGE_BYTES<<16))|2:0,0};
        }
        assert(step(&a.f.pool,&a.q)==GC_RF_ADVANCED);
        uint64_t middle=atomic_load(&a.f.cell.value),forward=atomic_load(&cells[1].word);
        assert((middle>>16)==pages[1].start+32&&!a.f.pool.active&&!atomic_load(&a.f.ctx.access_epoch));
        atomic_store(&cells[1].word,5);assert(step(&a.f.pool,&a.q)==GC_RF_WAIT);
        atomic_store(&cells[1].word,forward);assert(step(&a.f.pool,&a.q)==GC_RF_FLUSH);
        assert(atomic_load(&a.f.cell.value)==middle&&!a.f.pool.active&&!atomic_load(&a.f.ctx.access_epoch));
        atomic_store(&a.f.ctx.state,GC_BLOCKED_NATIVE);
        assert(step(&a.f.pool,&a.q)==GC_RF_INVALID&&atomic_load(&a.f.cell.value)==middle);
        atomic_fetch_add(&a.f.ctx.adopted_phase,0x10000);
        atomic_store(&a.f.ctx.state,GC_RUNNING);atomic_store(&a.f.journal.consumed,1);
        assert(step(&a.f.pool,&a.q)==GC_RF_ADVANCED);
        assert((atomic_load(&a.f.cell.value)>>16)==pages[2].start+32);
        assert(step(&a.f.pool,&a.q)==GC_RAC_READY);
        assert(a.q.follow.cleanup.page==(uintptr_t)&pages[2]&&a.q.follow.cleanup.incarnation==29);
        assert(a.q.access.reference==atomic_load(&a.f.cell.value)&&a.q.access.scope.held==(uintptr_t)&cells[2]);
        RootAccess owned=a.q;assert(step(&a.f.pool,&a.q)==GC_RF_INVALID&&!memcmp(&owned,&a.q,sizeof(owned)));
        assert(gc_resolve_access(&pages[2],&a.q.access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        assert(gc_metadata_step_locked(&a.f.pool,&a.q.follow.cleanup,GC_ML_RELEASE)==GC_ML_OK);
        assert(!a.f.pool.active&&!atomic_load(&a.f.ctx.access_epoch));
        for(unsigned i=0;i<3;i++)assert(!pages[i].metadata_leases&&!atomic_load(&cells[i].access));
    }
    puts("root access: 32 two-edge chains retain roots across busy/full/native retries before canonical guards PASS");
}
static void gc_run_root_access_cases(RootAccessor step){
    AccessFixture a;access_cases=0;const char *path=getenv("KLASSIC_GC_ROOT_ACCESS_VECTORS");
    if(path){access_vectors=fopen(path,"wb");assert(access_vectors);assert(fwrite("KLRACC01",8,1,access_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(AccessFixture,f.pool),offsetof(AccessFixture,q),ACCESS_POINTERS};
        assert(fwrite(header,sizeof(header),1,access_vectors)==1);
        for(unsigned i=0;i<ACCESS_POINTERS;i++){uint64_t offset=access_offset(i);assert(fwrite(&offset,8,1,access_vectors)==1);}}
    const uint64_t expected[]={GC_RF_DONE,GC_RAC_READY,GC_RAC_READY,GC_RF_WAIT,GC_RF_ADVANCED};
    for(unsigned epoch=0;epoch<32;epoch++)for(unsigned phase=0;phase<8;phase++)for(unsigned state=0;state<5;state++){
        access_fixture(&a,(UINT64_C(123)<<16)|(epoch<<8)|phase|(phase<<4),state);
        access_compare(&a,step,expected[state]);
        if(expected[state]==GC_RAC_READY)access_finish(&a);
        else{assert(!a.f.pool.active&&!a.f.page.metadata_leases&&!a.q.follow.cleanup.token&&!atomic_load(&a.f.ctx.access_epoch));
            ResolveRequest empty={0};assert(!memcmp(&a.q.access,&empty,sizeof(empty)));}
    }
    for(unsigned fault=0;fault<16;fault++){
        access_fixture(&a,0x10022,1);uint64_t result=GC_RF_INVALID;
        switch(fault){
        case 0:a.q.follow.context=0;break;case 1:a.q.follow.context++;break;
        case 2:a.q.follow.root.value=1;break;case 3:a.q.follow.root.handle.scope++;break;
        case 4:atomic_store(&a.f.ctx.state,GC_BLOCKED_NATIVE);break;
        case 5:a.q.access.reference=1;break;case 6:a.q.access.scope.held=8;break;
        case 7:a.q.follow.cleanup.token=1;result=GC_RF_CLEANUP_REQUIRED;break;
        case 8:a.f.pool.free_head=0;a.f.pool.active=2;result=GC_RF_METADATA_FULL;break;
        case 9:a.f.slots[0].generation=UINT32_MAX;result=GC_RF_EXHAUSTED;break;
        case 10:a.f.forward.incarnation++;break;
        case 11:atomic_store(&a.f.ctx.access_epoch,1);break;
        case 12:atomic_store(&a.f.forward.access,GC_SOURCE_CLOSED);result=GC_RF_WAIT;break;
        case 13:a.q.access.destination=8;break;
        case 14:a.q.access.scope.ticket=1;break;
        case 15:a.f.page.forward_table=0;break;
        }
        access_compare(&a,step,result);
    }
    access_fixture(&a,0x10022,4);a.f.journal.top=3;atomic_store(&a.f.journal.published,3);
    uint64_t original=atomic_load(&a.f.cell.value);
    access_compare(&a,step,GC_RF_FLUSH);assert(atomic_load(&a.f.cell.value)==original&&!a.f.pool.active);
    atomic_store(&a.f.journal.consumed,3);access_compare(&a,step,GC_RF_ADVANCED);
    assert(atomic_load(&a.f.cell.value)==(atomic_load(&a.f.forward.word)&~UINT64_C(3)));
    if(access_vectors){assert(fclose(access_vectors)==0);access_vectors=NULL;}
    printf("root access %u C/candidate bounded forwarding, guard and backpressure cases PASS\n",access_cases);
    root_access_chain(step);
}
static void gc_run_root_access_cleanup_cases(RootAccessCleanup candidate){
    unsigned count=0;AccessFixture *a=malloc(sizeof(*a)),*expected=malloc(sizeof(*expected));assert(a&&expected);
    for(unsigned impl=0;impl<2;impl++)for(unsigned phase=0;phase<2;phase++)for(unsigned fault=0;fault<14;fault++){
        access_fixture(a,phase?0x22:0,1);assert(gc_root_access_locked(&a->f.pool,&a->q)==GC_RAC_READY);
        RootAccessCleanup cleanup=impl?candidate:gc_root_access_cleanup;
        RootAccess *q=&a->q;MetadataLeasePool *pool=&a->f.pool;uint64_t op=GC_RAC_EXIT,status=GC_RF_INVALID;
        if(fault>=10){assert(gc_root_access_cleanup(NULL,q,GC_RAC_EXIT)==GC_RF_DONE);op=GC_RAC_RELEASE;}
        switch(fault){
        case 0:status=GC_RF_DONE;break;case 1:op=GC_RAC_RELEASE;break;
        case 2:q->follow.context=0;break;case 3:q->follow.cleanup.page=0;break;
        case 4:q->follow.cleanup.incarnation++;break;case 5:q->follow.cleanup.token=0;break;
        case 6:q->access.scope.context=0;break;case 7:op=2;break;case 8:q=NULL;break;
        case 9:q=(void *)((uintptr_t)&a->q+1);break;
        case 10:a->f.pool.active=0;status=GC_RF_CLEANUP_REQUIRED;break;
        case 11:atomic_store(&a->f.ctx.access_epoch,1);break;case 12:pool=NULL;break;
        case 13:q->access.reference=1;break;
        }
        memcpy(expected,a,sizeof(*a));
        if(!fault){expected->q.access=(ResolveRequest){0};atomic_store(&expected->f.ctx.access_epoch,0);atomic_store(&expected->f.forward.access,0);}
        assert(cleanup(pool,q,op)==status);assert(!memcmp(a,expected,sizeof(*a)));
        if(fault==10){
            // Restore the injected bad pool snapshot, retaining the exact
            // original lease/control rather than manufacturing a new token.
            a->f.pool.active=1;expected->f.pool.active=1;
        }
        if(!fault||fault==10){
            expected->q.follow.cleanup=(MetadataLeaseRequest){0};expected->f.pool.active=0;expected->f.pool.free_head=1;
            expected->f.page.metadata_leases=0;expected->f.slots[0].page=expected->f.slots[0].incarnation=0;expected->f.slots[0].next_free=2;
            assert(cleanup(pool,q,GC_RAC_RELEASE)==GC_RF_DONE);assert(!memcmp(a,expected,sizeof(*a)));
            assert(cleanup(pool,q,GC_RAC_RELEASE)==GC_RF_INVALID);assert(!memcmp(a,expected,sizeof(*a)));
            assert(gc_root_access_locked(pool,q)==GC_RAC_READY);
            assert(cleanup(NULL,q,GC_RAC_EXIT)==GC_RF_DONE&&cleanup(pool,q,GC_RAC_RELEASE)==GC_RF_DONE);
            assert(!a->f.pool.active&&!a->f.page.metadata_leases&&!atomic_load(&a->f.ctx.access_epoch));
        }
        count++;
    }
    free(expected);free(a);assert(count==56);
    puts("root access cleanup: 56 C/candidate ordered exit, lease retry, control reuse and rejection cases PASS");
}
#endif
