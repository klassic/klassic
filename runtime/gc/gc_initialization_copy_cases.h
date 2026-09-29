#ifndef KLASSIC_GC_INITIALIZATION_COPY_CASES_H
#define KLASSIC_GC_INITIALIZATION_COPY_CASES_H
#include "gc_initialization_cases.h"
typedef uint64_t (*InitializationCopier)(const GuardedMarkPrepare *,const InitializationBytes *);
typedef struct {PrepareFixture owner;InitializationBytes bytes;unsigned char source[257];} InitCopyFixture;
static uint64_t init_copy_pointers[32];static unsigned init_copy_pointer_count,init_copy_cases;
static uint64_t init_copy_normalize(InitCopyFixture *dst,const InitCopyFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<init_copy_pointer_count;i++){
        uint64_t *p=(void *)((unsigned char *)dst+init_copy_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void init_copy_fixture(InitCopyFixture *f){
    memset(f,0,sizeof(*f));initialization_fixture(&f->owner,0x71122);
    StoreFixture *s=&f->owner.field;ObjectHeader *h=(void *)(s->heap+16);
    h->payload_bytes=512;atomic_store(&s->page.allocated_top,560);
    s->descriptor.fixed_bytes=0;s->descriptor.element_bytes=1;s->descriptor.fixed_bitmap=0;s->descriptor.fixed_bitmap_words=0;
    memset(s->heap+48,0xa5,512);
    for(unsigned i=0;i<257;i++)f->source[i]=(unsigned char)(i*37+11);
    f->bytes=(InitializationBytes){(uintptr_t)&s->registry,7,(uintptr_t)(f->source+1),17};
}
static void init_copy_compare(InitCopyFixture *f,InitializationCopier step,uint64_t expected,FILE *vectors){
    InitCopyFixture c,before,after,x,y;memcpy(&c,f,sizeof(c));uint64_t mask=init_copy_normalize(&c,f);
    for(unsigned i=0;i<init_copy_pointer_count;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+init_copy_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&after,&c,sizeof(c));
    if(expected==GC_INIT_OK&&c.bytes.bytes)memcpy(after.owner.field.heap+48+c.bytes.byte_offset,(void *)(uintptr_t)c.bytes.source,c.bytes.bytes);
    uint64_t actual=step(&f->owner.q,&f->bytes),reference=gc_guarded_initialization_copy(&c.owner.q,&c.bytes);
    if(actual!=expected||reference!=expected){fprintf(stderr,"initialization copy case %u got %llu C %llu expected %llu\n",init_copy_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    assert(!memcmp(&c,&after,sizeof(c)));
    memcpy(&x,f,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(init_copy_normalize(&x,f)==init_copy_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));
        uint64_t record[]={0,reference,init_copy_normalize(&x,&c),init_copy_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,vectors)==1&&fwrite(&x,sizeof(x),1,vectors)==1&&fwrite(&y,sizeof(y),1,vectors)==1);}
    init_copy_cases++;
}
static void gc_run_initialization_copy_cases(InitializationCopier step){
    assert(prepare_pointer_count);init_copy_pointer_count=prepare_pointer_count;
    memcpy(init_copy_pointers,prepare_pointers,prepare_pointer_count*8);
    init_copy_pointers[init_copy_pointer_count++]=offsetof(InitCopyFixture,bytes.registry);
    init_copy_pointers[init_copy_pointer_count++]=offsetof(InitCopyFixture,bytes.source);init_copy_cases=0;
    FILE *vectors=NULL;const char *path=getenv("KLASSIC_GC_INITIALIZATION_COPY_VECTORS");
    if(path){vectors=fopen(path,"wb");assert(vectors);assert(fwrite("KLINCP01",8,1,vectors)==1);
        uint64_t h[]={sizeof(InitCopyFixture),offsetof(InitCopyFixture,owner.q),offsetof(InitCopyFixture,bytes),init_copy_pointer_count};
        assert(fwrite(h,sizeof(h),1,vectors)==1&&fwrite(init_copy_pointers,init_copy_pointer_count*8,1,vectors)==1);}
    InitCopyFixture f;const uint64_t offsets[]={0,1,255};
    for(unsigned i=0;i<3;i++)for(unsigned bytes=0;bytes<=256;bytes++){
        init_copy_fixture(&f);f.bytes.byte_offset=offsets[i];f.bytes.bytes=bytes;
        init_copy_compare(&f,step,GC_INIT_OK,vectors);
    }
    init_copy_fixture(&f);f.bytes.byte_offset=512;f.bytes.bytes=0;f.bytes.source=0;
    init_copy_compare(&f,step,GC_INIT_OK,vectors);
    for(unsigned fault=0;fault<24;fault++){
        init_copy_fixture(&f);StoreFixture *s=&f.owner.field;ObjectHeader *h=(void *)(s->heap+16);
        switch(fault){
        case 0:f.bytes.registry=0;break;case 1:f.bytes.registry++;break;case 2:f.bytes.bytes=257;break;
        case 3:f.bytes.byte_offset=513;break;case 4:f.bytes.byte_offset=500;break;case 5:f.bytes.source=0;break;
        case 6:f.bytes.source=UINT64_MAX-7;break;case 7:f.owner.q.phase=8;break;case 8:atomic_store(&h->state,GC_OBJECT_PUBLISHED);break;
        case 9:s->page.generation=GC_OLD;break;case 10:s->page.owner++;break;case 11:atomic_store(&s->cell.access,GC_SOURCE_CLOSED|1);break;
        case 12:s->descriptor.fixed_bitmap=(uintptr_t)&s->bitmap;break;case 13:s->descriptor.element_bitmap=(uintptr_t)&s->bitmap;break;
        case 14:s->descriptor.fixed_bitmap_words=1;break;case 15:s->descriptor.element_bitmap_words=1;break;
        case 16:h->descriptor_id=0;break;case 17:atomic_store(&s->slots[1],0);break;
        case 18:s->descriptor.fixed_bytes=513;break;case 19:s->descriptor.element_bytes=7;break;
        case 20:f.owner.q.lease.token=0;break;case 21:f.owner.q.scope=0;break;case 22:h->payload_bytes=513;break;case 23:h->mark_record=0;break;
        }
        init_copy_compare(&f,step,GC_INIT_INVALID,vectors);
    }
    init_copy_fixture(&f);
    assert(step(NULL,&f.bytes)==GC_INIT_INVALID&&step(&f.owner.q,NULL)==GC_INIT_INVALID);
    assert(step((void *)((char *)&f.owner.q+1),&f.bytes)==GC_INIT_INVALID);
    assert(step(&f.owner.q,(void *)((char *)&f.bytes+1))==GC_INIT_INVALID);
    assert(init_copy_cases==796);if(vectors)assert(!fclose(vectors));
    puts("initialization copy 796 C/candidate bounded-byte, scalar descriptor and failure preservation cases PASS");
}
#endif
