#ifndef KLASSIC_GC_BARRIER_HEAL_CASES_H
#define KLASSIC_GC_BARRIER_HEAL_CASES_H
#include "gc_barrier_cases.h"
typedef struct {StoreFixture field;GuardedHeal q;} HealFixture;
static uint64_t heal_pointers[32];static unsigned heal_pointer_count,heal_cases;static FILE *heal_vectors;
static void heal_fixture(HealFixture *f,uint64_t value,uint64_t phase){
    memset(f,0,sizeof(*f));store_fixture(&f->field,phase,GC_OLD,value);
    f->q=(GuardedHeal){.scope=(uintptr_t)&f->field.scope,.page=(uintptr_t)&f->field.page,.registry=(uintptr_t)&f->field.registry,
        .expected=value,.desired=(UINT64_C(160)<<16)|(gc_abi_color((PhaseSnapshot){phase})&~0x30u)|(value&0x30u),.phase=phase};
}
static uint64_t heal_normalize(HealFixture *f,const HealFixture *owner){
    uint64_t mask=0;for(unsigned i=0;i<heal_pointer_count;i++){
        unsigned char *p=(unsigned char *)f+heal_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)owner&&v-(uintptr_t)owner<sizeof(*owner)){v-=(uintptr_t)owner;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void heal_clone(HealFixture *d,const HealFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=heal_normalize(d,s);
    for(unsigned i=0;i<heal_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)d+heal_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);
    }
}
static void heal_check(const HealFixture *before,const HealFixture *after,uint64_t result){
    HealFixture e;memcpy(&e,before,sizeof(e));
    if(result==GC_HEAL_OK)atomic_store((_Atomic uint64_t *)(e.field.heap+48+e.q.byte_offset),e.q.desired);
    assert(!memcmp(&e,after,sizeof(e)));
}
static void heal_compare(HealFixture *a,GuardedHealer step,uint64_t expected){
    HealFixture c,ba,bc,x,y;heal_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->q),reference=gc_guarded_heal(&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"guarded heal case %u got %llu C %llu expected %llu\n",heal_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    heal_check(&ba,a,actual);heal_check(&bc,&c,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(heal_normalize(&x,a)==heal_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(heal_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,heal_normalize(&x,&c),heal_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,heal_vectors)==1);assert(fwrite(&x,sizeof(x),1,heal_vectors)==1);assert(fwrite(&y,sizeof(y),1,heal_vectors)==1);}
    heal_cases++;
}
static void gc_run_guarded_heal_cases(GuardedHealer step){
    HealFixture a;heal_cases=0;heal_pointer_count=0;
    for(unsigned i=0;i<sizeof(store_pointers)/sizeof(*store_pointers);i++)heal_pointers[heal_pointer_count++]=offsetof(HealFixture,field)+store_pointers[i];
    const uint64_t pointers[]={offsetof(HealFixture,q.scope),offsetof(HealFixture,q.page),offsetof(HealFixture,q.registry)};
    for(unsigned i=0;i<sizeof(pointers)/sizeof(*pointers);i++)heal_pointers[heal_pointer_count++]=pointers[i];
    const char *path=getenv("KLASSIC_GC_HEAL_VECTORS");
    if(path){heal_vectors=fopen(path,"wb");assert(heal_vectors);assert(fwrite("KLGHEL01",8,1,heal_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(HealFixture,q),offsetof(HealFixture,q),heal_pointer_count};
        assert(fwrite(header,sizeof(header),1,heal_vectors)==1);assert(fwrite(heal_pointers,8,heal_pointer_count,heal_vectors)==heal_pointer_count);}
    uint64_t value=(UINT64_C(128)<<16)|0x1510;
    for(unsigned epochs=0;epochs<32;epochs++)for(unsigned phase=0;phase<8;phase++)for(unsigned generation=0;generation<2;generation++)for(unsigned conflict=0;conflict<3;conflict++){
        heal_fixture(&a,(value&~UINT64_C(0x30))|((uint64_t)(epochs%3+1)<<4),(UINT64_C(123)<<16)|(epochs<<8)|phase|(phase<<4));a.field.page.generation=generation;
        if(conflict)atomic_store((_Atomic uint64_t *)(a.field.heap+48),conflict==1?0:(UINT64_C(192)<<16)|0x1510);
        heal_compare(&a,step,conflict?GC_HEAL_CHANGED:GC_HEAL_OK);
    }
    for(unsigned fault=0;fault<25;fault++){
        heal_fixture(&a,value,0x22);
        switch(fault){case 0:a.q.scope=0;break;case 1:a.q.page=0;break;case 2:a.q.registry=0;break;
        case 3:a.q.byte_offset=1;break;case 4:a.q.byte_offset=16;break;case 5:a.q.phase=8;break;case 6:a.q.phase=0xe000;break;
        case 7:a.q.expected=0;break;case 8:a.q.desired=0;break;case 9:a.q.expected=1;break;case 10:a.q.desired^=1;break;
        case 11:a.q.desired^=0x30;break;case 12:a.field.scope.context=0;break;case 13:a.field.scope.held=0;break;
        case 14:a.field.scope.ticket=1;break;case 15:atomic_store(&a.field.ctx.access_epoch,0);break;
        case 16:atomic_store(&a.field.ctx.state,GC_BLOCKED_NATIVE);break;case 17:atomic_fetch_add(&a.field.ctx.adopted_phase,0x10000);break;
        case 18:atomic_store(&a.field.cell.access,0);break;case 19:atomic_store(&a.field.cell.word,2);break;
        case 20:a.field.page.incarnation++;break;case 21:atomic_store(&a.field.page.allocated_top,48);break;
        case 22:a.field.bitmap=0;break;case 23:atomic_store(&((ObjectHeader *)(a.field.heap+16))->state,0);break;
        case 24:heal_fixture(&a,value,0x1022);a.q.desired=(UINT64_C(160)<<16)|gc_abi_color((PhaseSnapshot){0x1022});break;}
        heal_compare(&a,step,GC_HEAL_INVALID);
    }
    for(unsigned selected=0;selected<2;selected++){
        heal_fixture(&a,value,0x22);atomic_store(&a.field.cell.word,5);
        atomic_store(&a.field.cell.access,selected?GC_SOURCE_CLOSED|1:1);
        heal_compare(&a,step,GC_HEAL_OK);
    }
    if(heal_vectors){assert(!fclose(heal_vectors));heal_vectors=NULL;}
    printf("guarded heal %u C/candidate exact-observation, phase and preservation cases PASS\n",heal_cases);
}
#endif
