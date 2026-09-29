#ifndef KLASSIC_GC_MARK_ROUTE_CASES_H
#define KLASSIC_GC_MARK_ROUTE_CASES_H
#include "gc_mark_prepare_cases.h"
#include "gc_mark_route.h"
typedef uint64_t (*MarkRouter)(GuardedMarkRoute *);
typedef struct {PrepareFixture f;GuardedMarkRoute q;} RouteFixture;
static uint64_t route_pointers[32];static unsigned route_pointer_count,route_cases;static FILE *route_vectors;
static void route_fixture(RouteFixture *f,unsigned kind,unsigned target,uint64_t phase){
    memset(f,0,sizeof(*f));prepare_fixture(&f->f,phase,target==2?GC_OLD:GC_YOUNG);
    f->q=(GuardedMarkRoute){.generation=kind,.prepare=f->f.q};
    if(!target)f->q.prepare=(GuardedMarkPrepare){.phase=phase};
}
static uint64_t route_normalize(RouteFixture *d,const RouteFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<route_pointer_count;i++){unsigned char *p=(unsigned char *)d+route_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}}return mask;}
static void route_clone(RouteFixture *d,const RouteFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=route_normalize(d,s);
    for(unsigned i=0;i<route_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+route_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void route_effect(const RouteFixture *before,const RouteFixture *after,uint64_t result){
    RouteFixture e;memcpy(&e,before,sizeof(e));if(result==GC_ROUTE_READY){
        GuardedMarkPrepare *p=&e.q.prepare;p->output=(MarkRequest){.record=((const ObjectHeader *)(before->f.field.heap+16))->mark_record,
            .reference=p->reference,.incarnation=p->lease.incarnation,.metadata_lease=p->lease.token};p->lease=(MetadataLeaseRequest){0};}
    assert(!memcmp(&e,after,sizeof(e)));
}
static void route_compare(RouteFixture *a,MarkRouter step,uint64_t expected){
    RouteFixture c,ba,bc,x,y;route_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->q),reference=gc_guarded_mark_route(&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"mark route case %u got %llu C %llu expected %llu\n",route_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    route_effect(&ba,a,actual);route_effect(&bc,&c,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(route_normalize(&x,a)==route_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(route_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,route_normalize(&x,&c),route_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,route_vectors)==1);assert(fwrite(&x,sizeof(x),1,route_vectors)==1);assert(fwrite(&y,sizeof(y),1,route_vectors)==1);}route_cases++;
}
static void gc_run_mark_route_cases(MarkRouter step){
    assert(prepare_pointer_count);route_cases=0;route_pointer_count=prepare_pointer_count;memcpy(route_pointers,prepare_pointers,route_pointer_count*8);
    const uint64_t extra[]={offsetof(RouteFixture,q.prepare.scope),offsetof(RouteFixture,q.prepare.page),offsetof(RouteFixture,q.prepare.lease.page),offsetof(RouteFixture,q.prepare.output.record)};
    memcpy(route_pointers+route_pointer_count,extra,sizeof(extra));route_pointer_count+=sizeof(extra)/8;assert(route_pointer_count<32);
    RouteFixture a;const char *path=getenv("KLASSIC_GC_MARK_ROUTE_VECTORS");
    if(path){route_vectors=fopen(path,"wb");assert(route_vectors);assert(fwrite("KLMRTE01",8,1,route_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(RouteFixture,q),offsetof(RouteFixture,q),route_pointer_count};assert(fwrite(header,sizeof(header),1,route_vectors)==1);assert(fwrite(route_pointers,route_pointer_count*8,1,route_vectors)==1);}
    for(unsigned ep=0;ep<32;ep++)for(unsigned selected=1;selected<=3;selected++)for(unsigned other=0;other<8;other++)for(unsigned kind=0;kind<2;kind++)for(unsigned target=0;target<3;target++){
        uint64_t phase=0x70000|(ep<<8)|(kind?other|(selected<<4):selected|(other<<4));
        route_fixture(&a,kind,target,phase);route_compare(&a,step,!target||(!kind&&target==2)?GC_ROUTE_SKIP:GC_ROUTE_READY);
    }
    for(unsigned phase=0;phase<8;phase++)if(phase==0||phase>3)for(unsigned kind=0;kind<2;kind++)for(unsigned target=0;target<3;target++){
        route_fixture(&a,kind,target,0x70000|(kind?2|(phase<<4):phase|0x20));route_compare(&a,step,GC_ROUTE_INVALID);
    }
    for(unsigned target=0;target<3;target++)for(unsigned fault=0;fault<17;fault++){
        route_fixture(&a,0,target,0x71122);GuardedMarkPrepare *p=&a.q.prepare;
        switch(fault){case 0:a.q.generation=2;break;case 1:p->phase=8;break;case 2:p->phase|=0x2000;break;
        case 3:p->output.record=8;break;case 4:p->output.reference=1;break;case 5:p->output.incarnation=1;break;
        case 6:p->output.metadata_lease=1;break;case 7:p->output.index=1;break;case 8:p->output.serial=1;break;case 9:p->output.cursor=1;break;
        case 10:p->lease.token=1;break;case 11:p->lease.incarnation++;break;case 12:p->lease.page=1;break;
        case 13:p->scope=1;break;case 14:p->page=1;break;case 15:p->reference=1;break;case 16:p->phase=0x71144;break;}
        route_compare(&a,step,GC_ROUTE_INVALID);
    }
    // Old targets must still authenticate the canonical guard, phase and lease
    // before being skipped. Skipping cannot hide stale/invalid metadata.
    for(unsigned fault=0;fault<6;fault++){
        route_fixture(&a,0,2,0x71122);
        switch(fault){case 0:atomic_store(&a.f.field.cell.word,2);break;case 1:a.f.field.scope.held=0;break;
        case 2:atomic_store(&a.f.field.ctx.adopted_phase,0);break;case 3:a.f.field.page.generation=2;break;
        case 4:a.f.field.record.object_id=0;break;case 5:a.q.prepare.reference+=UINT64_C(16)<<16;break;}
        route_compare(&a,step,GC_ROUTE_INVALID);
    }
    for(unsigned ep=0;ep<32;ep++)for(unsigned kind=0;kind<2;kind++)for(unsigned target=1;target<3;target++){
        route_fixture(&a,kind,target,0x70022|(ep<<8));
        ObjectHeader *h=(ObjectHeader *)(a.f.field.heap+16);
        atomic_store(&h->state,GC_OBJECT_INITIALIZING);
        route_compare(&a,step,GC_ROUTE_WAIT);
        // A ready release permits retry; the same input lease is still owned.
        atomic_store_explicit(&h->state,GC_OBJECT_PUBLISHED,memory_order_release);
        route_compare(&a,step,!kind&&target==2?GC_ROUTE_SKIP:GC_ROUTE_READY);
    }
    for(unsigned kind=0;kind<2;kind++)for(unsigned target=1;target<3;target++)for(unsigned fault=0;fault<9;fault++){
        route_fixture(&a,kind,target,0x70022);ObjectHeader *h=(ObjectHeader *)(a.f.field.heap+16);
        atomic_store(&h->state,GC_OBJECT_INITIALIZING);
        switch(fault){case 0:atomic_store(&h->state,0);break;case 1:atomic_store(&h->state,3);break;
        case 2:atomic_store(&h->state,UINT64_MAX);break;case 3:h->descriptor_id=0;break;case 4:h->mark_record=0;break;
        case 5:a.f.field.record.object_id=0;break;case 6:a.q.prepare.lease.incarnation++;break;
        case 7:atomic_store(&a.f.field.cell.access,0);break;case 8:h->payload_bytes=UINT64_MAX;break;}
        route_compare(&a,step,GC_ROUTE_INVALID);
    }
    assert(route_cases==4987);
    if(route_vectors){assert(!fclose(route_vectors));route_vectors=NULL;}
    printf("mark route %u C/candidate null, canonical generation and lease ownership cases PASS\n",route_cases);
}
#endif
