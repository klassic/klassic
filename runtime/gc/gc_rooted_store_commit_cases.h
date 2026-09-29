#ifndef KLASSIC_GC_ROOTED_STORE_COMMIT_CASES_H
#define KLASSIC_GC_ROOTED_STORE_COMMIT_CASES_H
#include "gc_rooted_store_commit.h"
#include "gc_barrier_cases.h"
typedef struct {StoreFixture field;RootedMarkSet marks;RootSegment segment;RootCell root;} RootStoreFixture;
static void root_store_fixture(RootStoreFixture *f,uint64_t phase,unsigned generation,uint64_t previous,bool nonnull){
    memset(f,0,sizeof(*f));store_fixture(&f->field,phase,generation,previous);
    uint64_t value=f->field.q.desired;f->field.q.desired=0;
    atomic_store(&f->field.ctx.private_producers,1);
    f->segment=(RootSegment){.owner=17,.scope=101,.cells=(uintptr_t)&f->root,.capacity=1,.used=1,.state=GC_ROOT_SEGMENT_ACTIVE};
    f->root=(RootCell){.value=nonnull?(value^0x30):0,.reuse_generation=1,.active=1};
    uint64_t required=0;for(unsigned g=0;g<2;g++){uint64_t p=(phase>>(4*g))&15;if(p>=1&&p<=3)required|=UINT64_C(1)<<g;}
    f->marks=(RootedMarkSet){.context=(uintptr_t)&f->field.ctx,
        .root={.segment=(uintptr_t)&f->segment,.handle={(uintptr_t)&f->root,1,101}},
        .phase=phase,.state=2,.required=required,.completed=required};
}
static void root_store_check(RootStoreFixture *f,RootedStoreCommitter step,uint64_t status){
    RootStoreFixture expected;memcpy(&expected,f,sizeof(expected));
    if(status==GC_STORE_OK){
        uint64_t mask=store_mask(&f->field),value=atomic_load(&f->root.value);
        uint64_t desired=value?(value&~UINT64_C(0xffff))|gc_abi_color((PhaseSnapshot){f->marks.phase}):0;
        StoreFixture *e=&expected.field;
        if(mask&1){e->satb[e->ctx.satb.top%3]=e->q.satb;e->ctx.satb.top++;atomic_store(&e->ctx.satb.published,e->ctx.satb.top);}
        if(mask&2){e->remembered[e->ctx.remembered.top%3]=e->q.remembered;e->ctx.remembered.top++;atomic_store(&e->ctx.remembered.published,e->ctx.remembered.top);}
        e->q.transferred=mask;atomic_store((_Atomic uint64_t *)(e->heap+48),desired);
    }
    uint64_t result=step(&f->marks,&f->field.q);
    if(result!=status){fprintf(stderr,"rooted store result %llu expected %llu\n",(unsigned long long)result,(unsigned long long)status);abort();}
    assert(!memcmp(f,&expected,sizeof(expected)));
}
static void gc_run_rooted_store_commit_cases(RootedStoreCommitter candidate){
    unsigned count=0;RootStoreFixture f;
    for(unsigned impl=0;impl<2;impl++){
        RootedStoreCommitter step=impl?candidate:gc_rooted_store_commit;
        for(unsigned phases=0;phases<64;phases++)for(unsigned g=0;g<2;g++)for(unsigned old=0;old<2;old++)for(unsigned next=0;next<2;next++)for(unsigned epoch=0;epoch<2;epoch++){
            uint64_t phase=(UINT64_C(17)<<16)|(epoch?0x1f00:0)|(phases&7)|((phases>>3)<<4);
            root_store_fixture(&f,phase,g,old?(UINT64_C(128)<<16)|0x1510:0,next);
            root_store_check(&f,step,GC_STORE_OK);count++;
        }
        for(unsigned fault=0;fault<27;fault++){
            root_store_fixture(&f,0x10022,GC_OLD,(UINT64_C(128)<<16)|0x1510,true);
            uint64_t status=GC_STORE_INVALID;
            switch(fault){
            case 0:f.marks.state=1;break;case 1:f.marks.root.value=1;break;
            case 2:f.field.q.desired=1;break;case 3:f.field.q.transferred=1;break;
            case 4:f.marks.phase+=0x10000;break;case 5:f.marks.required=1;break;
            case 6:f.marks.completed=1;break;case 7:f.marks.context=0;break;
            case 8:atomic_store(&f.field.ctx.private_producers,0);break;
            case 9:f.marks.root.segment=0;break;case 10:f.marks.root.segment++;break;
            case 11:f.segment.owner++;break;case 12:atomic_store(&f.segment.state,GC_ROOT_SEGMENT_RETIRING);break;
            case 13:f.marks.root.handle.scope++;break;case 14:atomic_store(&f.segment.used,2);break;
            case 15:f.marks.root.handle.cell+=8;break;case 16:f.marks.root.handle.reuse_generation++;break;
            case 17:f.root.active=0;break;case 18:atomic_store(&f.root.value,1);break;
            case 19:atomic_fetch_xor(&f.root.value,0x300);status=GC_STORE_REPREPARE;break;
            case 20:atomic_fetch_xor(&f.root.value,0x3000);status=GC_STORE_REPREPARE;break;
            case 21:f.field.ctx.satb.top=3;atomic_store(&f.field.ctx.satb.published,3);status=GC_STORE_FULL;break;
            case 22:f.field.ctx.remembered.top=3;atomic_store(&f.field.ctx.remembered.published,3);status=GC_STORE_FULL;break;
            case 23:atomic_store((_Atomic uint64_t *)(f.field.heap+48),0);status=GC_STORE_RETRY;break;
            case 24:f.field.ctx.satb.top=UINT64_MAX;atomic_store(&f.field.ctx.satb.published,UINT64_MAX);atomic_store(&f.field.ctx.satb.consumed,UINT64_MAX);status=GC_STORE_EXHAUSTED;break;
            case 25:f.field.q.byte_offset=16;break;case 26:f.field.q.satb.slot.metadata_lease=f.field.q.remembered.slot.metadata_lease;break;
            }
            root_store_check(&f,step,status);count++;
        }
        assert(step(NULL,&f.field.q)==GC_STORE_INVALID&&step(&f.marks,NULL)==GC_STORE_INVALID);
        assert(step((void *)((char *)&f.marks+1),&f.field.q)==GC_STORE_INVALID);
        assert(step(&f.marks,(void *)((char *)&f.field.q+1))==GC_STORE_INVALID);
    }
    assert(count==2102);
    puts("rooted store commit: 2102 C/candidate rooted colors, null stores, mark receipts and lease-transfer effects PASS");
}
#endif
