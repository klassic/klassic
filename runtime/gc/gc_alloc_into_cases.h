#ifndef KLASSIC_GC_ALLOC_INTO_CASES_H
#define KLASSIC_GC_ALLOC_INTO_CASES_H
#include "gc_heap_cases.h"
#include "gc_alloc_into.h"
typedef uint64_t (*AllocateInto)(MutatorContext *,const AllocationInto *);
typedef struct {HeapFixture h;RootSegment segment;RootCell cell;BufferCursor buffer;RootChange entries[2];RootRequest root;AllocationInto q;} IntoFixture;
#define IH(f) (offsetof(IntoFixture,h)+offsetof(HeapFixture,f))
static const uint64_t into_pointers[]={IH(ctx.tlab_start),IH(ctx.tlab_top),IH(ctx.tlab_end),IH(page.heap_base),IH(page.starts),
    IH(descriptor.fixed_bitmap),IH(descriptor.element_bitmap),IH(request.page),IH(request.descriptor),IH(request.output),
    IH(request.marks),IH(marks.records),IH(page.forward_table),IH(forwards.cells),IH(heap)+16+offsetof(ObjectHeader,mark_record),
    offsetof(IntoFixture,segment.cells),offsetof(IntoFixture,segment.journal),offsetof(IntoFixture,buffer.entries),
    offsetof(IntoFixture,root.segment),offsetof(IntoFixture,root.handle.cell),offsetof(IntoFixture,q.allocation),offsetof(IntoFixture,q.root)};
#undef IH
static unsigned into_cases;static FILE *into_vectors;
static uint64_t into_normalize(IntoFixture *dst,const IntoFixture *src){
    uint64_t mask=0;for(unsigned i=0;i<sizeof(into_pointers)/8;i++){
        uint64_t *p=(uint64_t *)((unsigned char *)dst+into_pointers[i]);
        if(*p>=(uintptr_t)src&&*p-(uintptr_t)src<=sizeof(*src)){*p-=(uintptr_t)src;mask|=UINT64_C(1)<<i;}}
    return mask;
}
static void into_fixture(IntoFixture *f,uint64_t payload){
    memset(f,0,sizeof(*f));heap_fixture_init(&f->h);f->h.descriptor.fixed_bytes=payload;
    f->segment=(RootSegment){.owner=7,.scope=19,.cells=(uintptr_t)&f->cell,.capacity=1,.used=1,
        .state=GC_ROOT_SEGMENT_ACTIVE,.journal=(uintptr_t)&f->buffer};
    f->cell=(RootCell){.reuse_generation=3,.active=1};
    f->buffer=(BufferCursor){.entries=(uintptr_t)f->entries,.capacity=2};
    f->root=(RootRequest){.segment=(uintptr_t)&f->segment,.handle={(uintptr_t)&f->cell,3,19},.value=UINT64_MAX};
    f->q=(AllocationInto){(uintptr_t)&f->h.request,(uintptr_t)&f->root};
}
static void into_compare(IntoFixture *a,AllocateInto step,uint64_t status){
    IntoFixture c,before,expected,x,y;memcpy(&c,a,sizeof(c));uint64_t mask=into_normalize(&c,a);
    for(unsigned i=0;i<sizeof(into_pointers)/8;i++)if(mask&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&c+into_pointers[i])+=(uintptr_t)&c;
    memcpy(&before,&c,sizeof(c));memcpy(&expected,&c,sizeof(c));
    if(status==GC_ALLOC_OK){
        /* The independently tested allocation leaf supplies the heap oracle;
         * root publication and its complete state delta are expected here. */
        IntoFixture model;memcpy(&model,&c,sizeof(model));uint64_t reloc=into_normalize(&model,&c);
        for(unsigned i=0;i<sizeof(into_pointers)/8;i++)if(reloc&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&model+into_pointers[i])+=(uintptr_t)&model;
        assert(gc_try_tlab_allocate(&model.h.ctx,&model.h.request)==GC_ALLOC_OK);
        uint64_t top=model.buffer.top,ref=model.h.output.reference;
        model.entries[top%2]=(RootChange){0,ref,19,atomic_load(&model.h.ctx.adopted_phase)};
        model.root.value=ref;model.buffer.top=top+1;atomic_store(&model.buffer.published,top+1);
        atomic_store(&model.cell.value,ref);atomic_fetch_add(&model.h.ctx.root_revision,1);
        memcpy(&expected,&model,sizeof(expected));reloc=into_normalize(&expected,&model);
        for(unsigned i=0;i<sizeof(into_pointers)/8;i++)if(reloc&(UINT64_C(1)<<i))*(uint64_t *)((unsigned char *)&expected+into_pointers[i])+=(uintptr_t)&c;
    }
    uint64_t actual=step(&a->h.ctx,&a->q),reference=gc_try_tlab_allocate_into(&c.h.ctx,&c.q);
    if(actual!=status||reference!=status){fprintf(stderr,"allocation into case %u got %llu C %llu expected %llu\n",into_cases,(unsigned long long)actual,(unsigned long long)reference,(unsigned long long)status);abort();}
    assert(!memcmp(&expected,&c,sizeof(c)));memcpy(&x,a,sizeof(x));memcpy(&y,&c,sizeof(y));
    assert(into_normalize(&x,a)==into_normalize(&y,&c));assert(!memcmp(&x,&y,sizeof(x)));
    if(into_vectors){memcpy(&x,&before,sizeof(x));memcpy(&y,&c,sizeof(y));uint64_t rec[]={0,reference,into_normalize(&x,&c),into_normalize(&y,&c)};
        assert(fwrite(rec,sizeof(rec),1,into_vectors)==1);assert(fwrite(&x,sizeof(x),1,into_vectors)==1);assert(fwrite(&y,sizeof(y),1,into_vectors)==1);}
    into_cases++;
}
static void gc_run_alloc_into_cases(AllocateInto step){
    IntoFixture *f=malloc(sizeof(*f));assert(f);into_cases=0;const char *path=getenv("KLASSIC_GC_ALLOC_INTO_VECTORS");
    if(path){into_vectors=fopen(path,"wb");assert(into_vectors);assert(fwrite("KLALIN01",8,1,into_vectors)==1);
        uint64_t h[]={sizeof(*f),offsetof(IntoFixture,h.ctx),offsetof(IntoFixture,q),sizeof(into_pointers)/8};
        assert(fwrite(h,sizeof(h),1,into_vectors)==1);assert(fwrite(into_pointers,sizeof(into_pointers),1,into_vectors)==1);}
    const uint64_t sizes[]={0,1,7,16,17,255,256,257,1025};
    for(uint64_t phase=0;phase<64;phase++)if(gc_abi_valid_phase((PhaseSnapshot){phase}))for(unsigned size=0;size<9;size++){
        into_fixture(f,sizes[size]);atomic_store(&f->h.ctx.adopted_phase,phase);
        into_compare(f,step,GC_ALLOC_OK);
        assert(atomic_load(&f->cell.value)==f->h.output.reference&&f->entries[0].next==f->h.output.reference);
        assert(atomic_load(&((ObjectHeader *)(f->h.heap+16))->state)==GC_OBJECT_INITIALIZING);
        into_compare(f,step,GC_ALLOC_INVALID);
    }
    for(unsigned fault=0;fault<40;fault++){
        into_fixture(f,32);uint64_t status=GC_ALLOC_INVALID;
        switch(fault){
        case 0:f->q.allocation=0;break;case 1:f->q.allocation++;break;case 2:f->q.root=0;break;case 3:f->q.root++;break;
        case 4:f->h.request.output=0;break;case 5:f->h.request.output++;break;case 6:f->root.segment=0;break;
        case 7:f->root.segment++;break;case 8:f->segment.owner++;break;case 9:f->root.handle.scope++;break;
        case 10:f->root.handle.reuse_generation++;break;case 11:f->cell.active=0;break;case 12:atomic_store(&f->cell.value,UINT64_C(0x301510));break;
        case 13:f->segment.journal=0;break;case 14:f->segment.journal++;break;case 15:f->buffer.entries=0;break;
        case 16:f->buffer.entries++;break;case 17:f->buffer.capacity=0;break;case 18:f->buffer.top=1;break;
        case 19:atomic_store(&f->buffer.consumed,1);break;
        case 20:f->buffer.top=2;atomic_store(&f->buffer.published,2);status=GC_ALLOC_ROOT_FLUSH;break;
        case 21:f->buffer.top=UINT64_MAX;atomic_store(&f->buffer.published,UINT64_MAX);atomic_store(&f->buffer.consumed,UINT64_MAX);status=GC_ALLOC_ROOT_EXHAUSTED;break;
        case 22:atomic_store(&f->h.ctx.root_revision,UINT64_MAX);status=GC_ALLOC_ROOT_EXHAUSTED;break;
        case 23:atomic_store(&f->h.ctx.access_epoch,1);break;case 24:atomic_store(&f->h.ctx.state,GC_BLOCKED_NATIVE);break;
        case 25:f->h.request.page=0;status=GC_ALLOC_REFILL;break;
        case 26:f->h.request.marks=0;status=GC_ALLOC_METADATA_REFILL;break;
        case 27:f->h.descriptor.element_bytes=1;f->h.request.elements=UINT64_MAX;status=GC_ALLOC_OVERFLOW;break;
        case 28:f->h.descriptor.id=0;break;case 29:f->h.ctx.abi_version++;break;case 30:f->h.page.owner++;break;
        case 31:f->h.marks.used=f->h.marks.capacity;status=GC_ALLOC_METADATA_REFILL;break;
        case 32:atomic_store(&f->segment.state,GC_ROOT_SEGMENT_RETIRING);break;
        case 33:f->segment.used=0;break;case 34:f->root.handle.cell++;break;
        case 35:f->buffer.top=3;atomic_store(&f->buffer.published,3);break;
        case 36:f->h.page.limit=64;f->h.ctx.tlab_end=(uintptr_t)f->h.heap+64;status=GC_ALLOC_REFILL;break;
        case 37:f->segment.cells=0;break;case 38:f->segment.capacity=0;break;case 39:f->h.request.descriptor=0;break;
        }
        into_compare(f,step,status);
    }
    into_fixture(f,32);f->buffer.top=2;atomic_store(&f->buffer.published,2);
    into_compare(f,step,GC_ALLOC_ROOT_FLUSH);
    atomic_store(&f->buffer.consumed,1);into_compare(f,step,GC_ALLOC_OK);
    assert(f->buffer.top==3&&f->entries[0].next==f->h.output.reference);
    assert(into_cases==618);
    if(into_vectors){assert(!fclose(into_vectors));into_vectors=NULL;}free(f);
    printf("allocation into %u C/candidate empty-root publication and pre-allocation failure cases PASS\n",into_cases);
}
#endif
