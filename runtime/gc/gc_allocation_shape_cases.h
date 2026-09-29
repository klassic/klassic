#ifndef KLASSIC_GC_ALLOCATION_SHAPE_CASES_H
#define KLASSIC_GC_ALLOCATION_SHAPE_CASES_H
#include "gc_allocation_shape.h"
#include "gc_builtin_descriptor_cases.h"
typedef uint64_t (*AllocationShaper)(const DescriptorRegistry *,const AllocationShape *);
static void gc_run_allocation_shape_cases(AllocationShaper shape){
    BuiltinDescriptors builtins;gc_builtin_descriptors_prepare(&builtins);
    TypeDescriptor fixed={.id=3,.alignment=8,.fixed_bytes=32};
    _Atomic uint64_t slots[8]={0};DescriptorRegistry registry={(uintptr_t)slots,8};
    const TypeDescriptor *descriptors[]={&builtins.raw,&builtins.record,&fixed,&builtins.list};
    for(unsigned i=0;i<4;i++)assert(gc_descriptor_register(&registry,descriptors[i])==GC_DESC_OK);
    const uint64_t sizes[]={0,1,7,8,9,16,24,32,33,64,2048,GC_MAX_OFFSET,GC_MAX_OFFSET+1};
    unsigned cases=0;
    for(unsigned kind=0;kind<4;kind++)for(unsigned j=0;j<sizeof(sizes)/sizeof(*sizes);j++){
        const TypeDescriptor *d=descriptors[kind];uint64_t bytes=sizes[j];
        bool valid=bytes<=GC_MAX_OFFSET&&bytes>=d->fixed_bytes&&
            (d->element_bytes?(bytes-d->fixed_bytes)%d->element_bytes==0:bytes==d->fixed_bytes);
        AllocationRequest request={11,22,33,44,55},expected=request;
        AllocationShape q={d->id,bytes,(uintptr_t)&request},before=q;
        if(valid){expected.descriptor=(uintptr_t)d;expected.elements=d->element_bytes?(bytes-d->fixed_bytes)/d->element_bytes:0;}
        assert(shape(&registry,&q)==(valid?GC_ALLOC_OK:GC_ALLOC_INVALID));
        assert(!memcmp(&request,&expected,sizeof(request))&&!memcmp(&q,&before,sizeof(q)));cases++;
    }
    for(unsigned fault=0;fault<10;fault++){
        AllocationRequest request={11,22,33,44,55},before=request;
        AllocationShape q={1,8,(uintptr_t)&request};DescriptorRegistry r=registry;
        switch(fault){
        case 0:q.descriptor_id=0;break;case 1:q.descriptor_id=5;break;
        case 2:q.descriptor_id=8;break;case 3:q.descriptor_id=UINT64_MAX;break;
        case 4:q.allocation=0;break;case 5:q.allocation++;break;
        case 6:r.slots=0;break;case 7:r.slots++;break;
        case 8:r.capacity=1;break;case 9:r.capacity=UINT64_MAX;break;
        }
        AllocationShape saved=q;
        assert(shape(&r,&q)==GC_ALLOC_INVALID);
        assert(!memcmp(&request,&before,sizeof(request))&&!memcmp(&q,&saved,sizeof(q)));cases++;
    }
    assert(cases==62);
    puts("allocation shape: 62 registry-ID and logical-payload cases preserve unrelated request fields PASS");
}
#endif
