#ifndef KLASSIC_GC_INITIALIZATION_CASES_H
#define KLASSIC_GC_INITIALIZATION_CASES_H
#include "gc_mark_prepare_cases.h"
#include "gc_initialization.h"
typedef uint64_t (*InitializationPublisher)(const GuardedMarkPrepare *);
static unsigned initialization_cases;static FILE *initialization_vectors;
static void initialization_fixture(PrepareFixture *f,uint64_t phase){
    prepare_fixture(f,phase,GC_YOUNG);f->field.page.owner=f->field.ctx.thread_id;
    atomic_store(&f->field.page.state,GC_PAGE_ALLOCATING);
    atomic_store(&((ObjectHeader *)(f->field.heap+16))->state,GC_OBJECT_INITIALIZING);
}
static void initialization_effect(const PrepareFixture *before,const PrepareFixture *after,uint64_t result){
    PrepareFixture expected;memcpy(&expected,before,sizeof(expected));
    if(result==GC_INIT_OK)atomic_store(&((ObjectHeader *)(expected.field.heap+16))->state,GC_OBJECT_PUBLISHED);
    assert(!memcmp(&expected,after,sizeof(expected)));
}
static void initialization_compare(PrepareFixture *a,InitializationPublisher step,uint64_t expected){
    PrepareFixture c,ba,bc,x,y;prepare_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->q),reference=gc_guarded_initialization_publish(&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"initialization case %u got %llu C %llu expected %llu\n",initialization_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    initialization_effect(&ba,a,actual);initialization_effect(&bc,&c,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(prepare_normalize(&x,a)==prepare_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(initialization_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));
        uint64_t record[]={0,reference,prepare_normalize(&x,&c),prepare_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,initialization_vectors)==1);
        assert(fwrite(&x,sizeof(x),1,initialization_vectors)==1);assert(fwrite(&y,sizeof(y),1,initialization_vectors)==1);}
    initialization_cases++;
}
static void gc_run_initialization_cases(InitializationPublisher step){
    assert(prepare_pointer_count);initialization_cases=0;
    const char *path=getenv("KLASSIC_GC_INITIALIZATION_VECTORS");PrepareFixture f;
    if(path){initialization_vectors=fopen(path,"wb");assert(initialization_vectors);
        assert(fwrite("KLINIT01",8,1,initialization_vectors)==1);
        uint64_t header[]={sizeof(f),offsetof(PrepareFixture,q),offsetof(PrepareFixture,q),prepare_pointer_count};
        assert(fwrite(header,sizeof(header),1,initialization_vectors)==1);
        assert(fwrite(prepare_pointers,8,prepare_pointer_count,initialization_vectors)==prepare_pointer_count);}
    for(unsigned epoch=0;epoch<32;epoch++)for(unsigned phase=0;phase<8;phase++){
        initialization_fixture(&f,0x70000|(epoch<<8)|phase|(phase<<4));
        initialization_compare(&f,step,GC_INIT_OK);
        initialization_compare(&f,step,GC_INIT_INVALID); // duplicate cannot republish
    }
    for(unsigned fault=0;fault<24;fault++){
        initialization_fixture(&f,0x71122);ObjectHeader *h=(ObjectHeader *)(f.field.heap+16);
        switch(fault){
        case 0:f.field.page.owner=0;break;case 1:f.field.page.owner++;break;
        case 2:f.field.page.generation=GC_OLD;break;case 3:atomic_store(&f.field.page.state,GC_PAGE_CLOSED);break;
        case 4:atomic_store(&h->state,0);break;case 5:atomic_store(&h->state,GC_OBJECT_PUBLISHED);break;
        case 6:atomic_store(&h->state,3);break;case 7:h->mark_record=0;break;
        case 8:f.field.record.object_id=0;break;case 9:h->descriptor_id=0;break;
        case 10:f.q.lease.token=0;break;case 11:f.q.lease.incarnation++;break;
        case 12:f.field.scope.held=0;break;case 13:atomic_store(&f.field.cell.access,0);break;
        case 14:atomic_store(&f.field.cell.access,GC_SOURCE_CLOSED|1);break;
        case 15:atomic_store(&f.field.cell.word,3);break;case 16:atomic_store(&f.field.cell.word,5);break;
        case 17:f.q.reference=1;break;case 18:f.q.phase=8;break;
        case 19:atomic_store(&f.field.ctx.adopted_phase,0);break;
        case 20:f.q.output.record=8;break;case 21:h->descriptor_id=UINT64_MAX;break;
        case 22:h->payload_bytes=UINT64_MAX;break;case 23:f.q.scope++;break;}
        initialization_compare(&f,step,GC_INIT_INVALID);
    }
    assert(initialization_cases==536);
    if(initialization_vectors){assert(fclose(initialization_vectors)==0);initialization_vectors=NULL;}
    puts("initialization publish 536 C/candidate readiness and failure preservation cases PASS");
}
#endif
