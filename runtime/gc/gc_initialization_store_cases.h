#ifndef KLASSIC_GC_INITIALIZATION_STORE_CASES_H
#define KLASSIC_GC_INITIALIZATION_STORE_CASES_H
#include "gc_initialization_cases.h"
typedef uint64_t (*InitializationStorer)(const GuardedMarkPrepare *,const InitializationField *);
typedef struct {PrepareFixture owner;InitializationField field;} InitStoreFixture;
static uint64_t init_store_pointers[32];static unsigned init_store_pointer_count,init_store_cases;
static uint64_t init_store_normalize(InitStoreFixture *dst,const InitStoreFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<init_store_pointer_count;i++){
        uint64_t *p=(void *)((unsigned char *)dst+init_store_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void init_store_fixture(InitStoreFixture *f,uint64_t phase){
    memset(f,0,sizeof(*f));initialization_fixture(&f->owner,phase);
    f->field=(InitializationField){(uintptr_t)&f->owner.field.registry,8,(UINT64_C(144)<<16)|gc_abi_color((PhaseSnapshot){phase}),1};
}
static void init_store_compare(InitStoreFixture *f,InitializationStorer step,uint64_t expected,FILE *vectors){
    InitStoreFixture c,before,after,x,y;memcpy(&c,f,sizeof(c));uint64_t mask=init_store_normalize(&c,f);
    for(unsigned i=0;i<init_store_pointer_count;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+init_store_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&after,&c,sizeof(c));
    if(expected==GC_INIT_OK)memcpy(after.owner.field.heap+48+c.field.byte_offset,&c.field.value,8);
    uint64_t actual=step(&f->owner.q,&f->field),reference=gc_guarded_initialization_store(&c.owner.q,&c.field);
    if(actual!=expected||reference!=expected){fprintf(stderr,"initialization store case %u got %llu C %llu expected %llu\n",init_store_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    assert(!memcmp(&c,&after,sizeof(c)));
    memcpy(&x,f,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(init_store_normalize(&x,f)==init_store_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));
        uint64_t record[]={0,reference,init_store_normalize(&x,&c),init_store_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,vectors)==1&&fwrite(&x,sizeof(x),1,vectors)==1&&fwrite(&y,sizeof(y),1,vectors)==1);}
    init_store_cases++;
}
static void gc_run_initialization_store_cases(InitializationStorer step){
    assert(prepare_pointer_count);init_store_pointer_count=prepare_pointer_count;
    memcpy(init_store_pointers,prepare_pointers,prepare_pointer_count*8);
    init_store_pointers[init_store_pointer_count++]=offsetof(InitStoreFixture,owner.field.descriptor.element_bitmap);
    init_store_pointers[init_store_pointer_count++]=offsetof(InitStoreFixture,field.registry);init_store_cases=0;
    FILE *vectors=NULL;const char *path=getenv("KLASSIC_GC_INITIALIZATION_STORE_VECTORS");
    if(path){vectors=fopen(path,"wb");assert(vectors);assert(fwrite("KLINST01",8,1,vectors)==1);
        uint64_t h[]={sizeof(InitStoreFixture),offsetof(InitStoreFixture,owner.q),offsetof(InitStoreFixture,field),init_store_pointer_count};
        assert(fwrite(h,sizeof(h),1,vectors)==1&&fwrite(init_store_pointers,init_store_pointer_count*8,1,vectors)==1);}
    InitStoreFixture f;
    for(unsigned epoch=0;epoch<32;epoch++)for(unsigned phase=0;phase<8;phase++){
        uint64_t snapshot=0x70000|(epoch<<8)|phase|(phase<<4);
        init_store_fixture(&f,snapshot);f.field.value=0;init_store_compare(&f,step,GC_INIT_OK,vectors);
        init_store_fixture(&f,snapshot);init_store_compare(&f,step,GC_INIT_OK,vectors);
        init_store_compare(&f,step,GC_INIT_INVALID,vectors); // no deletion-barrier bypass
        init_store_fixture(&f,snapshot);f.field.byte_offset=16;f.field.is_reference=0;
        init_store_compare(&f,step,GC_INIT_OK,vectors); // reference-shaped scalar is preserved
        f.field.value=UINT64_MAX;init_store_compare(&f,step,GC_INIT_OK,vectors);
    }
    for(unsigned fault=0;fault<28;fault++){
        init_store_fixture(&f,0x71122);StoreFixture *s=&f.owner.field;ObjectHeader *h=(void *)(s->heap+16);
        switch(fault){
        case 0:f.field.registry=0;break;case 1:f.field.registry++;break;
        case 2:f.field.byte_offset++;break;case 3:f.field.byte_offset=24;break;case 4:f.field.byte_offset=UINT64_MAX;break;
        case 5:f.field.is_reference=2;break;case 6:f.field.is_reference=0;break;case 7:f.field.value=1;break;
        case 8:f.field.value^=16;break;case 9:atomic_store((_Atomic uint64_t *)(s->heap+56),f.field.value);break;
        case 10:atomic_store(&h->state,GC_OBJECT_PUBLISHED);break;case 11:s->page.generation=GC_OLD;break;
        case 12:s->page.owner++;break;case 13:atomic_store(&s->page.state,GC_PAGE_CLOSED);break;
        case 14:atomic_store(&s->cell.word,3);break;case 15:atomic_store(&s->cell.access,GC_SOURCE_CLOSED|1);break;
        case 16:f.owner.q.lease.token=0;break;case 17:f.owner.q.phase=8;break;
        case 18:atomic_store(&s->ctx.adopted_phase,0);break;case 19:f.owner.q.output.record=8;break;
        case 20:atomic_store(&s->slots[1],0);break;case 21:s->descriptor.fixed_bytes=32;break;
        case 22:h->payload_bytes=15;break;case 23:f.field.byte_offset=16;break;
        case 24:atomic_store(&s->ctx.access_epoch,0);break;case 25:s->scope.held=0;break;
        case 26:h->mark_record=0;break;case 27:s->record.object_id=0;break;
        }
        init_store_compare(&f,step,GC_INIT_INVALID,vectors);
    }
    init_store_fixture(&f,0x71122);
    f.owner.field.descriptor.fixed_bytes=8;f.owner.field.descriptor.fixed_bitmap=0;f.owner.field.descriptor.fixed_bitmap_words=0;
    f.owner.field.descriptor.element_bytes=8;f.owner.field.descriptor.element_bitmap=(uintptr_t)&f.owner.field.bitmap;
    f.owner.field.descriptor.element_bitmap_words=1;f.owner.field.bitmap=1;
    f.field.byte_offset=0;f.field.is_reference=0;
    init_store_compare(&f,step,GC_INIT_OK,vectors);
    f.field.byte_offset=8;f.field.is_reference=1;
    init_store_compare(&f,step,GC_INIT_OK,vectors);
    init_store_fixture(&f,0x71122);
    assert(step(NULL,&f.field)==GC_INIT_INVALID&&step(&f.owner.q,NULL)==GC_INIT_INVALID);
    assert(step((void *)((char *)&f.owner.q+1),&f.field)==GC_INIT_INVALID);
    assert(step(&f.owner.q,(void *)((char *)&f.field+1))==GC_INIT_INVALID);
    assert(init_store_cases==1310);if(vectors)assert(!fclose(vectors));
    puts("initialization store 1310 C/candidate typed-slot, phase and overwrite rejection cases PASS");
}
#endif
