#ifndef KLASSIC_GC_BARRIER_LOAD_CASES_H
#define KLASSIC_GC_BARRIER_LOAD_CASES_H
#include "gc_barrier_cases.h"
typedef uint64_t (*GuardedLoader)(const GuardedLoad *);
typedef struct {StoreFixture field;GuardedLoad q;RootSegment segment;RootCell cells[2];BufferCursor journal;RootChange entries[3];} LoadFixture;
static uint64_t load_pointers[32];static unsigned load_pointer_count,load_cases;static FILE *load_vectors;
static void load_fixture(LoadFixture *f,uint64_t value,uint64_t phase){
    memset(f,0,sizeof(*f));store_fixture(&f->field,phase,GC_OLD,value);
    f->journal=(BufferCursor){(uintptr_t)f->entries,3,0,0,0};
    f->segment=(RootSegment){.owner=17,.scope=101,.cells=(uintptr_t)f->cells,.capacity=2,.state=GC_ROOT_SEGMENT_ACTIVE,.journal=(uintptr_t)&f->journal};
    f->q=(GuardedLoad){.scope=(uintptr_t)&f->field.scope,.page=(uintptr_t)&f->field.page,.registry=(uintptr_t)&f->field.registry,
        .phase=phase,.output={.segment=(uintptr_t)&f->segment}};
    atomic_store(&f->field.ctx.access_epoch,0);assert(gc_root_step(&f->field.ctx,&f->q.output,GC_ROOT_RESERVE)==GC_ROOT_OK);
    atomic_store(&f->field.ctx.access_epoch,27);
}
static uint64_t load_normalize(LoadFixture *f,const LoadFixture *owner){
    uint64_t mask=0;for(unsigned i=0;i<load_pointer_count;i++){
        unsigned char *p=(unsigned char *)f+load_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)owner&&v-(uintptr_t)owner<sizeof(*owner)){v-=(uintptr_t)owner;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void load_clone(LoadFixture *d,const LoadFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=load_normalize(d,s);
    for(unsigned i=0;i<load_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)d+load_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);
    }
}
static void load_check(const LoadFixture *before,const LoadFixture *after,uint64_t result){
    LoadFixture e;memcpy(&e,before,sizeof(e));
    if(result==GC_ROOT_OK){uint64_t next=atomic_load((const _Atomic uint64_t *)(before->field.heap+48+before->q.byte_offset));
        e.entries[e.journal.top%3]=(RootChange){atomic_load(&e.cells[0].value),next,e.segment.scope,atomic_load(&e.field.ctx.adopted_phase)};
        e.journal.top++;atomic_store(&e.journal.published,e.journal.top);atomic_store(&e.cells[0].value,next);
        atomic_fetch_add(&e.field.ctx.root_revision,1);
    }
    assert(!memcmp(&e,after,sizeof(e)));
}
static void load_compare(LoadFixture *a,GuardedLoader step,uint64_t expected){
    LoadFixture c,ba,bc,x,y;load_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->q),reference=gc_guarded_load_into(&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"guarded load case %u got %llu C %llu expected %llu\n",load_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    load_check(&ba,a,actual);load_check(&bc,&c,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(load_normalize(&x,a)==load_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(load_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,load_normalize(&x,&c),load_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,load_vectors)==1);assert(fwrite(&x,sizeof(x),1,load_vectors)==1);assert(fwrite(&y,sizeof(y),1,load_vectors)==1);}
    load_cases++;
}
static void gc_run_guarded_load_cases(GuardedLoader step){
    LoadFixture a;load_cases=0;load_pointer_count=0;
    for(unsigned i=0;i<sizeof(store_pointers)/sizeof(*store_pointers);i++)load_pointers[load_pointer_count++]=offsetof(LoadFixture,field)+store_pointers[i];
    const uint64_t pointers[]={offsetof(LoadFixture,q.scope),offsetof(LoadFixture,q.page),offsetof(LoadFixture,q.registry),
        offsetof(LoadFixture,q.output.segment),offsetof(LoadFixture,q.output.handle.cell),offsetof(LoadFixture,segment.cells),
        offsetof(LoadFixture,segment.journal),offsetof(LoadFixture,journal.entries)};
    for(unsigned i=0;i<sizeof(pointers)/sizeof(*pointers);i++)load_pointers[load_pointer_count++]=pointers[i];
    const char *path=getenv("KLASSIC_GC_LOAD_VECTORS");
    if(path){load_vectors=fopen(path,"wb");assert(load_vectors);assert(fwrite("KLGLOD01",8,1,load_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(LoadFixture,q),offsetof(LoadFixture,q),load_pointer_count};
        assert(fwrite(header,sizeof(header),1,load_vectors)==1);assert(fwrite(load_pointers,8,load_pointer_count,load_vectors)==load_pointer_count);}
    uint64_t value=(UINT64_C(128)<<16)|0x1510;
    for(unsigned epochs=0;epochs<32;epochs++)for(unsigned phase=0;phase<8;phase++)for(unsigned which=0;which<3;which++){
        // Mismatched reference/phase colors are deliberately rooted unresolved.
        uint64_t v=which==0?0:which==1?value:(UINT64_C(160)<<16)|0x8630;
        load_fixture(&a,v,(UINT64_C(123)<<16)|(epochs<<8)|phase|(phase<<4));
        atomic_store(&a.cells[0].value,value);load_compare(&a,step,GC_ROOT_OK);
    }
    for(unsigned pos=0;pos<9;pos++){
        load_fixture(&a,value,0x22);a.journal.top=pos;atomic_store(&a.journal.published,pos);atomic_store(&a.journal.consumed,pos);
        load_compare(&a,step,GC_ROOT_OK);
    }
    for(unsigned fault=0;fault<45;fault++){
        load_fixture(&a,value,0x22);
        switch(fault){case 0:a.q.output.value=1;break;case 1:a.q.scope=0;break;case 2:a.q.scope++;break;case 3:a.q.page=0;break;
        case 4:a.q.registry=0;break;case 5:a.q.byte_offset=1;break;case 6:a.q.phase=8;break;case 7:a.q.phase=0xe000;break;
        case 8:a.field.scope.context=0;break;case 9:a.field.scope.held=0;break;case 10:a.field.scope.ticket=1;break;
        case 11:atomic_store(&a.field.ctx.access_epoch,0);break;case 12:atomic_store(&a.field.ctx.state,GC_BLOCKED_NATIVE);break;
        case 13:atomic_store(&a.field.ctx.adopted_phase,0);break;case 14:atomic_store(&a.field.cell.access,0);break;
        case 15:atomic_store(&a.field.cell.word,2);break;case 16:a.field.page.incarnation++;break;
        case 17:atomic_store(&a.field.page.allocated_top,48);break;case 18:a.q.byte_offset=16;break;
        case 19:a.field.bitmap=0;break;case 20:atomic_store(&a.field.slots[1],0);break;
        case 21:atomic_store((_Atomic uint64_t *)(a.field.heap+48),1);break;case 22:a.q.output.segment=0;break;
        case 23:a.q.output.segment++;break;case 24:a.segment.owner++;break;case 25:a.segment.scope=0;break;
        case 26:a.segment.cells=0;break;case 27:a.segment.capacity=0;break;case 28:atomic_store(&a.segment.state,GC_ROOT_SEGMENT_RETIRING);break;
        case 29:atomic_store(&a.segment.used,3);break;case 30:a.q.output.handle.scope++;break;case 31:a.q.output.handle.cell++;break;
        case 32:a.q.output.handle.cell=(uintptr_t)&a.cells[1];break;case 33:a.cells[0].active=0;break;case 34:a.q.output.handle.reuse_generation++;break;
        case 35:a.segment.journal=0;break;case 36:a.segment.journal++;break;case 37:a.journal.entries=0;break;
        case 38:a.journal.entries++;break;case 39:a.journal.capacity=0;break;case 40:atomic_store(&a.journal.published,1);break;
        case 41:atomic_store(&a.journal.consumed,1);break;case 42:a.journal.entries=UINT64_MAX-7;break;
        case 43:a.journal.top=4;atomic_store(&a.journal.published,4);break;case 44:a.field.ctx.abi_version++;break;}
        load_compare(&a,step,GC_ROOT_INVALID);
    }
    load_fixture(&a,value,0x22);a.journal.top=3;atomic_store(&a.journal.published,3);load_compare(&a,step,GC_ROOT_FLUSH);
    load_fixture(&a,value,0x22);a.journal.top=UINT64_MAX;atomic_store(&a.journal.published,UINT64_MAX);atomic_store(&a.journal.consumed,UINT64_MAX);load_compare(&a,step,GC_ROOT_EXHAUSTED);
    for(unsigned closed=0;closed<2;closed++){
        load_fixture(&a,value,0x22);atomic_store(&a.field.cell.word,5);atomic_store(&a.field.cell.access,1|(closed?GC_SOURCE_CLOSED:0));load_compare(&a,step,GC_ROOT_OK);
    }
    load_fixture(&a,value,0x22);atomic_store(&a.field.ctx.root_revision,UINT64_MAX-1);
    load_compare(&a,step,GC_ROOT_OK);load_compare(&a,step,GC_ROOT_EXHAUSTED);
    if(load_vectors){assert(!fclose(load_vectors));load_vectors=NULL;}
    printf("guarded load %u C/candidate rooted transfer and failure preservation cases PASS\n",load_cases);
}
static void gc_run_observe_cases(GuardedObserver candidate){
    LoadFixture *f=malloc(sizeof(*f)),*before=malloc(sizeof(*before));assert(f&&before);
    const uint64_t values[]={0,(UINT64_C(128)<<16)|0x1510,(UINT64_C(160)<<16)|0x8630,(UINT64_C(192)<<16)|0x2520};
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++){
        GuardedObserver observe=impl?candidate:gc_guarded_observe_into;
        assert(observe(NULL)==GC_ROOT_INVALID);count++;
        assert(observe((void *)1)==GC_ROOT_INVALID);count++;
        for(unsigned value=0;value<4;value++)for(unsigned fault=0;fault<6;fault++){
            load_fixture(f,values[value],0x22);uint64_t status=GC_ROOT_INVALID;
            switch(fault){
            case 0:status=GC_ROOT_OK;break;
            case 1:f->q.output.value=1;break;
            case 2:f->journal.top=3;atomic_store(&f->journal.published,3);status=GC_ROOT_FLUSH;break;
            case 3:atomic_store(&f->field.ctx.root_revision,UINT64_MAX);status=GC_ROOT_EXHAUSTED;break;
            case 4:atomic_store((_Atomic uint64_t *)(f->field.heap+48),1);break;
            case 5:f->cells[0].active=0;break;
            }
            memcpy(before,f,sizeof(*f));assert(observe(&f->q)==status);
            assert(f->q.output.value==(status==GC_ROOT_OK?values[value]:before->q.output.value));
            // Account for the one additional successful output, then check the
            // independently specified ordinary rooted-publication mutations.
            f->q.output.value=before->q.output.value;load_check(before,f,status);count++;
        }
    }
    assert(count==52);free(before);free(f);
    puts("rooted observation: 52 C/candidate exact-bit publication and failure preservation cases PASS");
}
#endif
