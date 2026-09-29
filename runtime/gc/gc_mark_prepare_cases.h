#ifndef KLASSIC_GC_MARK_PREPARE_CASES_H
#define KLASSIC_GC_MARK_PREPARE_CASES_H
#include "gc_barrier_cases.h"
#include "gc_mark_prepare.h"
typedef uint64_t (*MarkPreparer)(GuardedMarkPrepare *);
typedef struct {StoreFixture field;GuardedMarkPrepare q;} PrepareFixture;
static uint64_t prepare_pointers[24];static unsigned prepare_pointer_count,prepare_cases;static FILE *prepare_vectors;
static void prepare_fixture(PrepareFixture *f,uint64_t phase,unsigned generation){
    memset(f,0,sizeof(*f));store_fixture(&f->field,phase,generation,0);
    f->q=(GuardedMarkPrepare){.scope=(uintptr_t)&f->field.scope,.page=(uintptr_t)&f->field.page,.phase=phase,
        .reference=(UINT64_C(48)<<16)|0x1510,.lease={(uintptr_t)&f->field.page,27,(UINT64_C(1)<<32)|1}};
}
static uint64_t prepare_normalize(PrepareFixture *f,const PrepareFixture *owner){
    uint64_t mask=0;for(unsigned i=0;i<prepare_pointer_count;i++){
        unsigned char *p=(unsigned char *)f+prepare_pointers[i];uint64_t v;memcpy(&v,p,8);
        if(v>=(uintptr_t)owner&&v-(uintptr_t)owner<sizeof(*owner)){v-=(uintptr_t)owner;memcpy(p,&v,8);mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void prepare_clone(PrepareFixture *d,const PrepareFixture *s){memcpy(d,s,sizeof(*d));uint64_t mask=prepare_normalize(d,s);
    for(unsigned i=0;i<prepare_pointer_count;i++)if(mask&(UINT64_C(1)<<i)){
        unsigned char *p=(unsigned char *)d+prepare_pointers[i];uint64_t v;memcpy(&v,p,8);v+=(uintptr_t)d;memcpy(p,&v,8);
    }
}
static void prepare_effect(const PrepareFixture *before,const PrepareFixture *after,uint64_t result){
    PrepareFixture e;memcpy(&e,before,sizeof(e));
    if(result==GC_MARK_OK){e.q.output=(MarkRequest){.record=((const ObjectHeader *)(before->field.heap+16))->mark_record,
            .reference=e.q.reference,.incarnation=e.q.lease.incarnation,.metadata_lease=e.q.lease.token};
        e.q.lease=(MetadataLeaseRequest){0};}
    assert(!memcmp(&e,after,sizeof(e)));
}
static void prepare_compare(PrepareFixture *a,MarkPreparer step,uint64_t expected){
    PrepareFixture c,ba,bc,x,y;prepare_clone(&c,a);memcpy(&ba,a,sizeof(ba));memcpy(&bc,&c,sizeof(bc));
    uint64_t actual=step(&a->q),reference=gc_guarded_mark_prepare(&c.q);
    if(actual!=expected||reference!=expected){fprintf(stderr,"mark prepare case %u got %llu C %llu expected %llu\n",prepare_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    prepare_effect(&ba,a,actual);prepare_effect(&bc,&c,reference);
    memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));assert(prepare_normalize(&x,a)==prepare_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(prepare_vectors){memcpy(&x,&bc,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={0,reference,prepare_normalize(&x,&c),prepare_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,prepare_vectors)==1);assert(fwrite(&x,sizeof(x),1,prepare_vectors)==1);assert(fwrite(&y,sizeof(y),1,prepare_vectors)==1);}
    prepare_cases++;
}
static void gc_run_mark_prepare_cases(MarkPreparer step){
    PrepareFixture a;prepare_cases=prepare_pointer_count=0;
    for(unsigned i=0;i<sizeof(store_pointers)/8;i++)prepare_pointers[prepare_pointer_count++]=offsetof(PrepareFixture,field)+store_pointers[i];
    const uint64_t pointers[]={offsetof(PrepareFixture,q.scope),offsetof(PrepareFixture,q.page),offsetof(PrepareFixture,q.lease.page),offsetof(PrepareFixture,q.output.record)};
    for(unsigned i=0;i<sizeof(pointers)/8;i++)prepare_pointers[prepare_pointer_count++]=pointers[i];
    const char *path=getenv("KLASSIC_GC_MARK_PREPARE_VECTORS");
    if(path){prepare_vectors=fopen(path,"wb");assert(prepare_vectors);assert(fwrite("KLMPRP01",8,1,prepare_vectors)==1);
        uint64_t header[]={sizeof(a),offsetof(PrepareFixture,q),offsetof(PrepareFixture,q),prepare_pointer_count};
        assert(fwrite(header,sizeof(header),1,prepare_vectors)==1);assert(fwrite(prepare_pointers,8,prepare_pointer_count,prepare_vectors)==prepare_pointer_count);}
    for(unsigned epoch=0;epoch<32;epoch++)for(unsigned phase=0;phase<8;phase++)for(unsigned generation=0;generation<2;generation++){
        prepare_fixture(&a,(UINT64_C(123)<<16)|(epoch<<8)|phase|(phase<<4),generation);prepare_compare(&a,step,GC_MARK_OK);
    }
    for(unsigned word=0;word<3;word++)for(unsigned closed=0;closed<2;closed++){
        prepare_fixture(&a,0x22,0);atomic_store(&a.field.cell.word,word==0?0:word==1?3:5);
        atomic_store(&a.field.cell.access,1|(closed?GC_SOURCE_CLOSED:0));prepare_compare(&a,step,GC_MARK_OK);
    }
    const unsigned payloads[]={0,1,7,24,32};for(unsigned i=0;i<5;i++){
        prepare_fixture(&a,0x22,1);((ObjectHeader *)(a.field.heap+16))->payload_bytes=payloads[i];prepare_compare(&a,step,GC_MARK_OK);
    }
    for(unsigned young=0;young<2;young++)for(unsigned old=0;old<2;old++)for(unsigned remap=0;remap<4;remap++)for(unsigned remember=1;remember<4;remember++){
        prepare_fixture(&a,0x22,0);a.q.reference=(UINT64_C(48)<<16)|(UINT64_C(0x100)<<young)|(UINT64_C(0x400)<<old)|(UINT64_C(0x1000)<<remap)|(remember<<4);
        prepare_compare(&a,step,GC_MARK_OK);
    }
    prepare_compare(&a,step,GC_MARK_INVALID); // cannot overwrite a pending offer
    for(unsigned fault=0;fault<56;fault++){
        prepare_fixture(&a,0x22,0);ObjectHeader *h=(ObjectHeader *)(a.field.heap+16);
        switch(fault){case 0:a.q.scope=0;break;case 1:a.q.scope++;break;case 2:a.q.page=0;break;
        case 3:a.q.page++;a.q.lease.page++;break;case 4:a.q.phase=8;break;case 5:a.q.phase=0xe000;break;
        case 6:atomic_store(&a.field.ctx.adopted_phase,0);break;case 7:a.q.reference=0;break;case 8:a.q.reference=1;break;
        case 9:a.q.reference+=UINT64_C(16)<<16;break;case 10:a.q.reference|=0x200;break;
        case 11:a.q.lease.page+=8;break;case 12:a.q.lease.incarnation++;break;case 13:a.q.lease.token=0;break;
        case 14:a.q.lease.token=1;break;case 15:a.q.lease.token=UINT64_C(1)<<32;break;
        case 16:a.field.scope.held=0;break;case 17:a.field.scope.held++;break;case 18:a.field.scope.ticket=1;break;
        case 19:a.field.scope.incarnation=0;break;case 20:a.field.scope.context=0;break;case 21:a.field.scope.context++;break;
        case 22:a.field.ctx.abi_version++;break;case 23:a.field.ctx.thread_id=0;break;
        case 24:atomic_store(&a.field.ctx.state,GC_BLOCKED_NATIVE);break;case 25:atomic_store(&a.field.ctx.access_epoch,0);break;
        case 26:a.field.cell.source_offset++;break;case 27:a.field.cell.incarnation++;break;
        case 28:atomic_store(&a.field.cell.access,GC_SOURCE_CLOSED);break;case 29:atomic_store(&a.field.cell.word,2);break;
        case 30:atomic_store(&a.field.cell.word,1);break;case 31:atomic_store(&a.field.page.state,GC_PAGE_READY);break;
        case 32:a.field.page.incarnation++;break;case 33:a.field.page.generation=2;break;case 34:a.field.page.start=0;break;
        case 35:a.field.page.limit=UINT64_MAX;break;case 36:a.field.page.heap_base++;break;case 37:a.field.page.heap_base=UINT64_MAX-15;break;
        case 38:atomic_store(&a.field.page.allocated_top,16);break;case 39:a.field.page.start=48;break;case 40:h->payload_bytes=33;break;
        case 41:atomic_store(&h->state,GC_OBJECT_INITIALIZING);break;case 42:h->descriptor_id=0;break;
        case 43:h->descriptor_id=UINT64_C(1)<<32;break;case 44:h->mark_record=0;break;case 45:h->mark_record++;break;
        case 46:a.field.record.object_id=0;break;case 47:a.q.output.record=8;break;case 48:a.q.output.reference=8;break;
        case 49:a.q.output.incarnation=1;break;case 50:a.q.output.metadata_lease=1;break;case 51:a.q.output.index=1;break;
        case 52:a.q.output.serial=1;break;case 53:a.q.output.cursor=1;break;case 54:a.field.page.limit=a.field.page.start;break;
        case 55:atomic_store(&a.field.page.allocated_top,a.field.page.limit+16);break;}
        prepare_compare(&a,step,GC_MARK_INVALID);
    }
    if(prepare_vectors){assert(!fclose(prepare_vectors));prepare_vectors=NULL;}
    printf("mark prepare %u C/candidate guarded header and unique lease transfer cases PASS\n",prepare_cases);
}
#endif
