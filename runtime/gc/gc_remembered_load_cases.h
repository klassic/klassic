#ifndef KLASSIC_GC_REMEMBERED_LOAD_CASES_H
#define KLASSIC_GC_REMEMBERED_LOAD_CASES_H
#include "gc_barrier_load_cases.h"
#include "gc_remembered_inventory.h"
#include "gc_remembered_load.h"
typedef uint64_t (*RememberedLoader)(const RememberedLoad *);
typedef struct {LoadFixture f;RememberedInventory g;RememberedRegistration r;RememberedSet s;RememberedJob claim;RememberedLoad q;uint64_t bits[2][16];} RememberedLoadFixture;
static uint64_t rload_pointers[64];static unsigned rload_pointer_count,rload_cases;static FILE *rload_vectors;
static void rload_fixture(RememberedLoadFixture *f,uint64_t value,uint64_t phase){
    memset(f,0,sizeof(*f));load_fixture(&f->f,value,phase);
    uint64_t initial=((phase-0x20000)^0x1000)&~UINT64_C(15),start=((phase-0x10000)&~UINT64_C(15))|1;
    f->g=(RememberedInventory){.epoch=initial,.last_phase=initial};
    f->s=(RememberedSet){.start=f->f.field.page.start,.limit=f->f.field.page.limit,.incarnation=27,
        .bits={(uintptr_t)f->bits[0],(uintptr_t)f->bits[1]},.words=16,.epoch=initial,.last_phase=initial};
    f->r.set=(uintptr_t)&f->s;RememberedInventoryRequest q={(uintptr_t)&f->r,(uintptr_t)&f->claim,start};
    assert(gc_remembered_inventory_step_locked(&f->g,&q,GC_RI_ATTACH)==0);
    f->claim=(RememberedJob){.offset=48,.incarnation=27,.phase=initial,.budget=256};
    assert(gc_remembered_inventory_step_locked(&f->g,&q,GC_RI_RECORD)==0);
    assert(gc_remembered_inventory_step_locked(&f->g,&q,GC_RI_BEGIN)==0);f->claim.phase=start;
    assert(gc_remembered_inventory_step_locked(&f->g,&q,GC_RI_FLIP)==0);
    assert(gc_remembered_inventory_step_locked(&f->g,&q,GC_RI_TAKE)==0);
    f->q=(RememberedLoad){.registration=(uintptr_t)&f->r,.claim=(uintptr_t)&f->claim,.source_owner=(UINT64_C(48)<<16)|0x1510,.load=f->f.q};
}
static uint64_t rload_normalize(RememberedLoadFixture *d,const RememberedLoadFixture *s){uint64_t mask=0;
    for(unsigned i=0;i<rload_pointer_count;i++){unsigned char *p=(unsigned char *)d+rload_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)s&&v-(uintptr_t)s<sizeof(*s)){v-=(uintptr_t)s;mask|=UINT64_C(1)<<i;memcpy(p,&v,8);}}return mask;}
static void rload_clone(RememberedLoadFixture *d,const RememberedLoadFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=rload_normalize(d,s);
    for(unsigned i=0;i<rload_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){unsigned char *p=(unsigned char *)d+rload_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);}}
static void rload_compare(RememberedLoadFixture *a,RememberedLoader step,uint64_t expected){
    RememberedLoadFixture c,ba,bc,x,y;rload_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->q),reference=gc_remembered_load_into(&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"remembered load case %u got %llu C %llu expected %llu\n",rload_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    // Reuse the independent guarded-root effect oracle, then require all
    // inventory/claim/control bytes outside the load fixture to stay unchanged.
    load_check(&ba.f,&a->f,actual);load_check(&bc.f,&c.f,reference);
    assert(!memcmp((char *)&ba+sizeof(LoadFixture),(char *)a+sizeof(LoadFixture),sizeof(*a)-sizeof(LoadFixture)));
    assert(!memcmp((char *)&bc+sizeof(LoadFixture),(char *)&c+sizeof(LoadFixture),sizeof(c)-sizeof(LoadFixture)));
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(rload_normalize(&x,a)==rload_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(rload_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,rload_normalize(&x,&c),rload_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,rload_vectors)==1);assert(fwrite(&x,sizeof(x),1,rload_vectors)==1);assert(fwrite(&y,sizeof(y),1,rload_vectors)==1);}rload_cases++;
}
static void gc_run_remembered_load_cases(RememberedLoader step){
    assert(load_pointer_count);rload_pointer_count=load_pointer_count;memcpy(rload_pointers,load_pointers,load_pointer_count*8);
    const uint64_t extra[]={offsetof(RememberedLoadFixture,g.head),offsetof(RememberedLoadFixture,r.registry),offsetof(RememberedLoadFixture,r.set),offsetof(RememberedLoadFixture,r.previous),offsetof(RememberedLoadFixture,r.next),offsetof(RememberedLoadFixture,s.bits[0]),offsetof(RememberedLoadFixture,s.bits[1]),offsetof(RememberedLoadFixture,q.registration),offsetof(RememberedLoadFixture,q.claim),offsetof(RememberedLoadFixture,q.load.scope),offsetof(RememberedLoadFixture,q.load.page),offsetof(RememberedLoadFixture,q.load.registry),offsetof(RememberedLoadFixture,q.load.output.segment),offsetof(RememberedLoadFixture,q.load.output.handle.cell)};
    memcpy(rload_pointers+rload_pointer_count,extra,sizeof(extra));rload_pointer_count+=sizeof(extra)/8;assert(rload_pointer_count<64);
    RememberedLoadFixture a;rload_cases=0;const char *path=getenv("KLASSIC_GC_REMEMBERED_LOAD_VECTORS");
    if(path){rload_vectors=fopen(path,"wb");assert(rload_vectors);assert(fwrite("KLRMLD01",8,1,rload_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(RememberedLoadFixture,q),offsetof(RememberedLoadFixture,q),rload_pointer_count};assert(fwrite(header,sizeof(header),1,rload_vectors)==1);assert(fwrite(rload_pointers,rload_pointer_count*8,1,rload_vectors)==1);}
    for(unsigned ep=0;ep<32;ep++)for(unsigned y=1;y<=3;y++)for(unsigned old=0;old<8;old++)for(unsigned which=0;which<3;which++){
        uint64_t value=which?(((UINT64_C(128)+which*16)<<16)|0x1510):0;
        rload_fixture(&a,value,0xa0000|(ep<<8)|y|(old<<4));rload_compare(&a,step,GC_ROOT_OK);
        assert(a.g.claims==1&&a.claim.serial==a.s.active_serial);
    }
    for(unsigned fault=0;fault<28;fault++){
        rload_fixture(&a,(UINT64_C(128)<<16)|0x1510,0xa0022);
        switch(fault){case 0:a.q.registration=0;break;case 1:a.q.registration++;break;case 2:a.q.claim=0;break;case 3:a.q.claim++;break;
        case 4:a.q.source_owner=0;break;case 5:a.q.source_owner=1;break;case 6:a.r.registry=0;break;case 7:a.r.identity=0;break;
        case 8:a.r.set=0;break;case 9:a.claim.serial++;break;case 10:a.claim.offset+=8;break;case 11:a.claim.incarnation++;break;
        case 12:a.claim.phase++;break;case 13:a.r.epoch++;break;case 14:a.q.source_owner=(UINT64_C(16)<<16)|0x1510;break;
        case 15:a.q.load.byte_offset=UINT64_MAX;break;case 16:a.q.load.byte_offset=8;break;
        case 17:a.q.load.phase=0;break;case 18:a.q.load.phase^=0x1000;break;case 19:a.q.load.phase=(a.q.load.phase&~UINT64_C(15))|4;break;
        case 20:a.f.field.page.generation=GC_YOUNG;break;case 21:a.q.load.scope=0;break;
        case 22:a.q.load.output.value=1;break;case 23:a.q.load.page++;break;case 24:a.s.active_serial=0;break;
        case 25:a.r.registry++;break;case 26:a.s.active_offset++;break;case 27:a.q.source_owner=(UINT64_C(8192)<<16)|0x1510;break;}
        rload_compare(&a,step,GC_ROOT_INVALID);
    }
    rload_fixture(&a,0,0xa0022);a.f.journal.top=3;atomic_store(&a.f.journal.published,3);rload_compare(&a,step,GC_ROOT_FLUSH);
    if(rload_vectors){assert(fclose(rload_vectors)==0);rload_vectors=NULL;}
    printf("remembered load %u C/candidate claim, phase and rooted-handoff cases PASS\n",rload_cases);
}
#endif
