#ifndef KLASSIC_GC_ROOT_WALK_ITEM_CASES_H
#define KLASSIC_GC_ROOT_WALK_ITEM_CASES_H
#include "gc_root_walk_cases.h"
typedef uint64_t (*RootItemStep)(RootWalkItem *,uint64_t);
typedef struct {RootWalkItem q;RootWalk w;RootRegistration r;RootSegment s;RootRegistry g;RootCell cells[4];} RootItemFixture;
static const uint64_t item_pointers[]={offsetof(RootItemFixture,q.walk),offsetof(RootItemFixture,w.lease.registration),
    offsetof(RootItemFixture,w.registry),offsetof(RootItemFixture,r.segment),offsetof(RootItemFixture,s.registration),
    offsetof(RootItemFixture,s.cells),offsetof(RootItemFixture,g.inventory)};
static unsigned item_cases;static FILE *item_vectors;
static uint64_t item_normalize(RootItemFixture *dst,const RootItemFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(item_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+item_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void item_fixture(RootItemFixture *f,unsigned epoch){
    memset(f,0,sizeof(*f));uint64_t color=gc_abi_color((PhaseSnapshot){epoch<<8});
    f->q=(RootWalkItem){(uintptr_t)&f->w,7,99};
    f->w=(RootWalk){.registry=(uintptr_t)&f->g,.phase=0x22,.lease={(uintptr_t)&f->r,9,7,1,0},.limit=4,.mask=3,.completed_mask=3,.active=1};
    f->r=(RootRegistration){.segment=(uintptr_t)&f->s,.scope=9,.registered=1,.leases=1};
    f->s=(RootSegment){.owner=17,.scope=9,.cells=(uintptr_t)f->cells,.capacity=4,.used=4,.state=1,.registration=(uintptr_t)&f->r};
    f->g=(RootRegistry){.last_scope=9,.last_lease=7,.active=1,.leases=1,.inventory=(uintptr_t)&f->r,.walkers=1,.phase=0x22};
    for(unsigned i=0;i<4;i++){f->cells[i].active=1;f->cells[i].reuse_generation=5;atomic_store(&f->cells[i].value,((uint64_t)(i+1)<<20)|color);}
}
static void item_compare(RootItemFixture *a,RootItemStep step,uint64_t op,uint64_t status){
    RootItemFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=item_normalize(&c,a);
    for(unsigned i=0;i<sizeof(item_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))
        *(uint64_t *)((unsigned char *)&c+item_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(status==GC_RWI_ITEM){RootWalk *w=&expected.w;
        if(op==GC_RWI_CAPTURE){w->pending_reference=w->pending_observed=atomic_load(&expected.cells[w->cursor].value);w->pending_index=expected.q.index=w->cursor;w->pending=1;w->cursor++;}
        else w->pending=w->pending_reference=w->pending_observed=w->pending_index=0;}
    uint64_t actual=step(&a->q,op),reference=gc_root_walk_item(&c.q,op);
    if(actual!=status||reference!=status){fprintf(stderr,"root item case %u op %llu got %llu C %llu expected %llu\n",item_cases,(unsigned long long)op,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(item_normalize(&x,a)==item_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(item_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,item_normalize(&x,&c),item_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,item_vectors)==1);assert(fwrite(&x,sizeof(x),1,item_vectors)==1);assert(fwrite(&y,sizeof(y),1,item_vectors)==1);}
    item_cases++;
}
static void gc_run_root_walk_item_cases(RootItemStep step,RootWalker walk,RootWalkScanner scan){
    RootItemFixture f;item_cases=0;const char *path=getenv("KLASSIC_GC_ROOT_WALK_ITEM_VECTORS");
    if(path){item_vectors=fopen(path,"wb");assert(item_vectors);assert(fwrite("KLRWIT01",8,1,item_vectors)==1);
        uint64_t header[]={sizeof(f),offsetof(RootItemFixture,q),0,sizeof(item_pointers)/8};
        assert(fwrite(header,sizeof(header),1,item_vectors)==1);assert(fwrite(item_pointers,sizeof(item_pointers),1,item_vectors)==1);}
    for(unsigned epoch=0;epoch<32;epoch++)for(unsigned state=1;state<=2;state++)for(unsigned null=0;null<2;null++){
        item_fixture(&f,epoch);atomic_store(&f.s.state,state);
        for(unsigned i=0;i<4;i++){
            if(null)atomic_store(&f.cells[i].value,0);
            item_compare(&f,step,GC_RWI_CAPTURE,GC_RWI_ITEM);
            uint64_t observed=f.w.pending_observed;
            if(i){f.q.index--;item_compare(&f,step,GC_RWI_ACK,GC_RWI_INVALID);f.q.index++;}
            // Owner replacement cannot alter the captured snapshot or release
            // the scan lease. At the final cell only pending work blocks NEXT.
            atomic_store(&f.cells[i].value,walk_value(2,i));f.cells[i].active=0;
            RootItemFixture saved;memcpy(&saved,&f,sizeof(saved));
            assert(walk(&f.g,&f.w,GC_RW_NEXT)==GC_RW_WAIT);assert(!memcmp(&saved,&f,sizeof(f)));
            uint64_t out=123;RootWalkScan q={(uintptr_t)&f.w,f.q.serial,(uintptr_t)&out,1,0};
            assert(scan(&q)==GC_ROOT_INVALID&&out==123&&!q.written);assert(!memcmp(&saved,&f,sizeof(f)));
            item_compare(&f,step,GC_RWI_CAPTURE,GC_RWI_WAIT);
            assert(f.w.pending_observed==observed);
            // Symbolic same-object destination, as supplied by a protected
            // forwarding handoff. ACK clears both current and original holders.
            if(observed)f.w.pending_reference=(UINT64_C(0x10000)<<16)|(observed&UINT16_MAX);
            item_compare(&f,step,GC_RWI_ACK,GC_RWI_ITEM);
            item_compare(&f,step,GC_RWI_ACK,GC_RWI_INVALID);
        }
        item_compare(&f,step,GC_RWI_CAPTURE,GC_RWI_DONE);
        assert(walk(&f.g,&f.w,GC_RW_NEXT)==GC_RW_DONE&&!f.g.leases&&!f.g.walkers);
        item_compare(&f,step,GC_RWI_CAPTURE,GC_RWI_INVALID);
    }
    for(unsigned op=0;op<2;op++)for(unsigned fault=0;fault<27;fault++){
        item_fixture(&f,0);
        if(op){assert(gc_root_walk_item(&f.q,GC_RWI_CAPTURE)==GC_RWI_ITEM);}
        switch(fault){case 0:f.q.walk=0;break;case 1:f.q.walk++;break;case 2:f.q.serial=0;break;
        case 3:f.q.serial++;break;case 4:f.w.active=0;break;case 5:f.w.lease.kind=4;break;
        case 6:f.w.lease.registration=0;break;case 7:f.w.lease.registration++;break;case 8:f.w.cursor=5;break;
        case 9:f.w.pending=2;break;case 10:f.r.registered=0;break;case 11:f.r.scope++;break;
        case 12:f.r.segment=0;break;case 13:f.r.segment++;break;case 14:f.s.owner=0;break;
        case 15:f.s.scope=0;break;case 16:f.s.cells=0;break;case 17:f.s.cells++;break;
        case 18:f.s.capacity=0;break;case 19:f.s.capacity=UINT64_MAX;break;case 20:f.s.cells=UINT64_MAX-7;break;
        case 21:atomic_store(&f.s.state,0);break;case 22:atomic_store(&f.s.state,3);break;
        case 23:f.s.scope++;break;case 24:f.s.registration=0;break;case 25:atomic_store(&f.s.used,5);break;
        case 26:atomic_store(&f.s.used,3);break;}
        item_compare(&f,step,op,GC_RWI_INVALID);
    }
    for(unsigned dirty=0;dirty<3;dirty++){
        item_fixture(&f,0);if(dirty==0)f.w.pending_reference=16;if(dirty==1)f.w.pending_observed=16;if(dirty==2)f.w.pending_index=1;
        item_compare(&f,step,GC_RWI_CAPTURE,GC_RWI_INVALID);
    }
    item_fixture(&f,0);item_compare(&f,step,2,GC_RWI_INVALID);
    item_fixture(&f,0);assert(gc_root_walk_item(&f.q,0)==0);f.w.cursor=0;item_compare(&f,step,1,GC_RWI_INVALID);
    item_fixture(&f,0);assert(gc_root_walk_item(&f.q,0)==0);f.w.pending_index=1;item_compare(&f,step,1,GC_RWI_INVALID);
    item_fixture(&f,0);assert(gc_root_walk_item(&f.q,0)==0);f.q.index++;item_compare(&f,step,1,GC_RWI_INVALID);
    assert(item_cases==2749);if(item_vectors){assert(fclose(item_vectors)==0);item_vectors=NULL;}
    puts("root walk item 2749 C/candidate durable snapshot, pending exclusion and ACK cases PASS");
}
#endif
