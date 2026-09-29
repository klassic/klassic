#ifndef KLASSIC_GC_SCALAR_CASES_H
#define KLASSIC_GC_SCALAR_CASES_H
#include "gc_barrier_cases.h"
typedef uint64_t (*ScalarAccessor)(GuardedScalar *,uint64_t);
typedef struct {StoreFixture field;GuardedScalar q;} ScalarFixture;
static uint64_t scalar_pointers[24];static unsigned scalar_pointer_count,scalar_cases;
static uint64_t scalar_normalize(ScalarFixture *dst,const ScalarFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<scalar_pointer_count;i++){
        uint64_t *p=(void *)((unsigned char *)dst+scalar_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}
    }return mask;
}
static void scalar_fixture(ScalarFixture *f,uint64_t phase,uint64_t generation,uint64_t value){
    memset(f,0,sizeof(*f));store_fixture(&f->field,phase,generation,0);
    atomic_store((_Atomic uint64_t *)(f->field.heap+64),value);
    f->q=(GuardedScalar){(uintptr_t)&f->field.scope,(uintptr_t)&f->field.page,(uintptr_t)&f->field.registry,16,phase,value^UINT64_C(0xdeadbeef)};
}
static void scalar_compare(ScalarFixture *f,ScalarAccessor step,uint64_t op,uint64_t expected,FILE *vectors){
    ScalarFixture c,before,after,x,y;memcpy(&c,f,sizeof(c));uint64_t mask=scalar_normalize(&c,f);
    for(unsigned i=0;i<scalar_pointer_count;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+scalar_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&after,&c,sizeof(c));
    if(expected==GC_SCALAR_OK){
        if(op==GC_SCALAR_LOAD)memcpy(&after.q.value,c.field.heap+48+c.q.byte_offset,8);
        else memcpy(after.field.heap+48+c.q.byte_offset,&c.q.value,8);
    }
    uint64_t actual=step(&f->q,op),reference=gc_guarded_scalar(&c.q,op);
    if(actual!=expected||reference!=expected){fprintf(stderr,"scalar case %u got %llu C %llu expected %llu\n",scalar_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)expected);abort();}
    assert(!memcmp(&c,&after,sizeof(c)));memcpy(&x,f,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(scalar_normalize(&x,f)==scalar_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t record[]={op,reference,scalar_normalize(&x,&c),scalar_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,vectors)==1&&fwrite(&x,sizeof(x),1,vectors)==1&&fwrite(&y,sizeof(y),1,vectors)==1);}
    scalar_cases++;
}
static void gc_run_scalar_cases(ScalarAccessor step){
    scalar_pointer_count=sizeof(store_pointers)/8;memcpy(scalar_pointers,store_pointers,sizeof(store_pointers));
    scalar_pointers[scalar_pointer_count++]=offsetof(ScalarFixture,field.descriptor.element_bitmap);
    scalar_pointers[scalar_pointer_count++]=offsetof(ScalarFixture,q.scope);
    scalar_pointers[scalar_pointer_count++]=offsetof(ScalarFixture,q.page);
    scalar_pointers[scalar_pointer_count++]=offsetof(ScalarFixture,q.registry);scalar_cases=0;
    FILE *vectors=NULL;const char *path=getenv("KLASSIC_GC_SCALAR_VECTORS");
    if(path){vectors=fopen(path,"wb");assert(vectors);assert(fwrite("KLSCAL01",8,1,vectors)==1);
        uint64_t h[]={sizeof(ScalarFixture),offsetof(ScalarFixture,q),offsetof(ScalarFixture,q),scalar_pointer_count};
        assert(fwrite(h,sizeof(h),1,vectors)==1&&fwrite(scalar_pointers,scalar_pointer_count*8,1,vectors)==1);}
    ScalarFixture f;const uint64_t values[]={0,UINT64_MAX,UINT64_C(0x301510),UINT64_C(0xffff000000000000)};
    for(uint64_t phase=0;phase<64;phase++)if(gc_abi_valid_phase((PhaseSnapshot){phase}))for(unsigned gen=0;gen<2;gen++)for(unsigned v=0;v<4;v++)for(unsigned op=0;op<2;op++){
        scalar_fixture(&f,phase,gen,values[v]);scalar_compare(&f,step,op,GC_SCALAR_OK,vectors);
    }
    for(unsigned op=0;op<2;op++){
        scalar_fixture(&f,0x22,GC_OLD,17);
        f.field.descriptor.fixed_bytes=8;f.field.descriptor.fixed_bitmap=0;f.field.descriptor.fixed_bitmap_words=0;
        f.field.descriptor.element_bytes=8;f.field.descriptor.element_bitmap=(uintptr_t)&f.field.bitmap;f.field.descriptor.element_bitmap_words=1;f.field.bitmap=1;
        f.q.byte_offset=0;scalar_compare(&f,step,op,GC_SCALAR_OK,vectors); // list length before pointers
        scalar_fixture(&f,0x22,GC_OLD,17);f.field.descriptor.fixed_bitmap=0;f.field.descriptor.fixed_bitmap_words=0;
        scalar_compare(&f,step,op,GC_SCALAR_OK,vectors);
        scalar_fixture(&f,0x22,GC_OLD,17);atomic_store(&f.field.cell.word,3);atomic_store(&f.field.cell.access,GC_SOURCE_CLOSED|1);
        scalar_compare(&f,step,op,GC_SCALAR_OK,vectors); // prior entrant completes before relocation drain
    }
    for(unsigned fault=0;fault<28;fault++)for(unsigned op=0;op<2;op++){
        scalar_fixture(&f,0x22,GC_OLD,17);StoreFixture *s=&f.field;ObjectHeader *h=(void *)(s->heap+16);
        switch(fault){
        case 0:f.q.scope=0;break;case 1:f.q.scope++;break;case 2:f.q.page=0;break;case 3:f.q.page++;break;
        case 4:f.q.registry=0;break;case 5:f.q.registry++;break;case 6:f.q.byte_offset=0;break;case 7:f.q.byte_offset=8;break;
        case 8:f.q.byte_offset=24;break;case 9:f.q.byte_offset++;break;case 10:f.q.phase=8;break;
        case 11:atomic_store(&s->ctx.adopted_phase,0);break;case 12:atomic_store(&s->ctx.access_epoch,0);break;
        case 13:atomic_store(&s->ctx.state,GC_BLOCKED_NATIVE);break;case 14:s->scope.held=0;break;
        case 15:atomic_store(&s->cell.access,0);break;case 16:atomic_store(&s->cell.word,2);break;
        case 17:atomic_store(&h->state,GC_OBJECT_INITIALIZING);break;case 18:h->payload_bytes=23;break;
        case 19:atomic_store(&s->slots[1],0);break;case 20:s->descriptor.id=2;break;case 21:s->bitmap=7;break;
        case 22:h->descriptor_id=0;break;case 23:s->scope.incarnation++;break;case 24:s->scope.ticket=1;break;
        case 25:s->page.incarnation++;break;case 26:s->descriptor.fixed_bytes=32;break;case 27:f.q.byte_offset=UINT64_MAX;break;
        }
        scalar_compare(&f,step,op,GC_SCALAR_INVALID,vectors);
    }
    scalar_fixture(&f,0x22,GC_OLD,17);scalar_compare(&f,step,2,GC_SCALAR_INVALID,vectors);
    assert(step(NULL,0)==GC_SCALAR_INVALID&&step((void *)((char *)&f.q+1),1)==GC_SCALAR_INVALID);
    assert(scalar_cases==575);if(vectors)assert(!fclose(vectors));
    puts("scalar access 575 C/candidate typed-slot, exact-bit and guard preservation cases PASS");
}
#endif
