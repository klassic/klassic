#ifndef KLASSIC_GC_ALLOC_START_CASES_H
#define KLASSIC_GC_ALLOC_START_CASES_H
#include "gc_alloc_into_cases.h"
#include "gc_alloc_start.h"
typedef uint64_t (*AllocateStart)(MutatorContext *,const AllocationStart *);
typedef struct { IntoFixture into;DescriptorRegistry registry;_Atomic uint64_t slots[8];AllocationShape shape;AllocationStart q; } StartFixture;
static unsigned start_cases;
static uint64_t start_pointers[40];static unsigned start_pointer_count;
static uint64_t start_normalize(StartFixture *dst,const StartFixture *src){
    uint64_t mask=0;
    for(unsigned i=0;i<start_pointer_count;i++){
        uint64_t *p=(void *)((unsigned char *)dst+start_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<=sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}
    }
    return mask;
}
static void start_fixture(StartFixture *f,uint64_t bytes){
    memset(f,0,sizeof(*f));into_fixture(&f->into,bytes);
    f->into.segment.used=0;f->into.cell.active=0;f->into.root.handle=(RootHandle){0};
    f->into.h.descriptor.id=3;
    f->registry=(DescriptorRegistry){(uintptr_t)f->slots,8};
    atomic_store(&f->slots[f->into.h.descriptor.id],(uintptr_t)&f->into.h.descriptor);
    f->shape=(AllocationShape){f->into.h.descriptor.id,bytes,(uintptr_t)&f->into.h.request};
    f->q=(AllocationStart){(uintptr_t)&f->registry,(uintptr_t)&f->shape,(uintptr_t)&f->into.root};
}
static void start_compare(StartFixture *f,AllocateStart step,uint64_t status,bool reserved,FILE *vectors){
    StartFixture c,x,y,before,expected;
    memcpy(&c,f,sizeof(c));uint64_t mask=start_normalize(&c,f);
    for(unsigned i=0;i<start_pointer_count;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+start_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));
    uint64_t actual=step(&f->into.h.ctx,&f->q),reference=gc_try_allocate_start(&c.into.h.ctx,&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"allocation start case %u got %llu C %llu expected %llu\n",start_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    start_cases++;
    if(status!=GC_ALLOC_OK){
        // Only resolved shape fields and a canceled reservation may change.
        memcpy(&expected,&before,sizeof(expected));
        expected.into.h.request.descriptor=c.into.h.request.descriptor;
        expected.into.h.request.elements=c.into.h.request.elements;
        if(reserved){
            expected.into.segment.used++;
            expected.into.cell.reuse_generation++;
            expected.into.root.handle=(RootHandle){(uintptr_t)&c.into.cell,expected.into.cell.reuse_generation,19};
        }
        assert(!memcmp(&c,&expected,sizeof(c)));
    }else{
        assert(c.into.cell.active&&atomic_load(&c.into.cell.value)==c.into.h.output.reference);
        assert(c.into.entries[0].next==c.into.h.output.reference&&atomic_load(&c.into.buffer.published)==1);
        assert(atomic_load(&c.into.segment.used)==1&&c.into.root.handle.reuse_generation==4);
        assert(atomic_load(&((ObjectHeader *)(c.into.h.heap+16))->state)==GC_OBJECT_INITIALIZING);
    }
    memcpy(&x,f,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(start_normalize(&x,f)==start_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));
        uint64_t record[]={0,status,start_normalize(&x,&c),start_normalize(&y,&c)};
        assert(fwrite(record,sizeof(record),1,vectors)==1&&fwrite(&x,sizeof(x),1,vectors)==1&&fwrite(&y,sizeof(y),1,vectors)==1);
    }
}
static void gc_run_alloc_start_cases(AllocateStart step){
    start_pointer_count=sizeof(into_pointers)/8;
    memcpy(start_pointers,into_pointers,sizeof(into_pointers));
    const uint64_t extra[]={offsetof(StartFixture,registry.slots),offsetof(StartFixture,slots)+3*8,
        offsetof(StartFixture,shape.allocation),offsetof(StartFixture,q.registry),offsetof(StartFixture,q.shape),offsetof(StartFixture,q.root)};
    memcpy(start_pointers+start_pointer_count,extra,sizeof(extra));start_pointer_count+=sizeof(extra)/8;
    StartFixture *f=malloc(sizeof(*f));assert(f);FILE *vectors=NULL;
    const char *path=getenv("KLASSIC_GC_ALLOC_START_VECTORS");
    if(path){vectors=fopen(path,"wb");assert(vectors);assert(fwrite("KLALST01",8,1,vectors)==1);
        uint64_t h[]={sizeof(*f),offsetof(StartFixture,into.h.ctx),offsetof(StartFixture,q),start_pointer_count};
        assert(fwrite(h,sizeof(h),1,vectors)==1&&fwrite(start_pointers,start_pointer_count*8,1,vectors)==1);}
    unsigned count=0;start_cases=0;
    for(uint64_t phase=0;phase<64;phase++)if(gc_abi_valid_phase((PhaseSnapshot){phase}))for(unsigned size=0;size<3;size++){
        start_fixture(f,size?size*17:0);atomic_store(&f->into.h.ctx.adopted_phase,phase);
        start_compare(f,step,GC_ALLOC_OK,true,vectors);count++;
    }
    for(unsigned fault=0;fault<24;fault++){
        start_fixture(f,32);uint64_t status=GC_ALLOC_INVALID;bool reserved=false;
        switch(fault){
        case 0:f->q.registry=0;break;case 1:f->q.registry++;break;
        case 2:f->q.shape=0;break;case 3:f->q.shape++;break;
        case 4:f->q.root=0;break;case 5:f->q.root++;break;
        case 6:f->shape.descriptor_id=0;break;case 7:f->shape.payload_bytes=31;break;
        case 8:f->shape.allocation=0;break;case 9:f->shape.allocation++;break;
        case 10:f->into.segment.used=1;status=GC_ALLOC_ROOT_FULL;break;
        case 11:f->into.cell.reuse_generation=UINT64_MAX;status=GC_ALLOC_ROOT_EXHAUSTED;break;
        case 12:atomic_store(&f->into.h.ctx.state,GC_BLOCKED_NATIVE);break;
        case 13:atomic_store(&f->into.h.ctx.access_epoch,1);break;
        case 14:f->into.segment.owner++;break;
        case 15:f->into.h.request.page=0;status=GC_ALLOC_REFILL;reserved=true;break;
        case 16:f->into.h.request.marks=0;status=GC_ALLOC_METADATA_REFILL;reserved=true;break;
        case 17:f->into.buffer.top=2;atomic_store(&f->into.buffer.published,2);status=GC_ALLOC_ROOT_FLUSH;reserved=true;break;
        case 18:atomic_store(&f->into.h.ctx.root_revision,UINT64_MAX);status=GC_ALLOC_ROOT_EXHAUSTED;reserved=true;break;
        case 19:f->into.h.request.output=0;reserved=true;break;
        case 20:f->into.h.page.owner++;reserved=true;break;
        case 21:f->into.segment.journal=0;reserved=true;break;
        case 22:f->into.h.marks.used=f->into.h.marks.capacity;status=GC_ALLOC_METADATA_REFILL;reserved=true;break;
        case 23:f->into.h.page.limit=64;f->into.h.ctx.tlab_end=(uintptr_t)f->into.h.heap+64;status=GC_ALLOC_REFILL;reserved=true;break;
        }
        start_compare(f,step,status,reserved,vectors);count++;
    }
    start_fixture(f,0);
    assert(step(NULL,&f->q)==GC_ALLOC_INVALID&&step(&f->into.h.ctx,NULL)==GC_ALLOC_INVALID);
    assert(step((void *)((char *)&f->into.h.ctx+1),&f->q)==GC_ALLOC_INVALID);
    assert(step(&f->into.h.ctx,(void *)((char *)&f->q+1))==GC_ALLOC_INVALID);
    if(vectors)assert(!fclose(vectors));free(f);
    printf("allocation start %u C/candidate shape, reservation and failure cleanup cases PASS\n",count);
}
#endif
