#ifndef KLASSIC_GC_NATIVE_BUILTIN_PREPARE_CASES_H
#define KLASSIC_GC_NATIVE_BUILTIN_PREPARE_CASES_H
#include "gc_native_builtin_registry.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static inline void gc_run_native_builtin_prepare_cases(NativeBuiltinRegistryPreparer prepare,
        uint64_t (*budget)(HeapPool *,MetadataCharge *,uint64_t),const NativeMetadataOps *ops){
    const uint64_t capacities[]={5,64,1024};
    for(unsigned i=0;i<3;i++){
        uint64_t bytes=0;assert(gc_native_builtin_registry_size(capacities[i],ops->quantum,&bytes)==0);
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),.metadata_max=sizeof(AllocationPage)+bytes};
        MetadataCharge charge={.bytes=bytes};assert(budget(&heap,&charge,GC_BUDGET_RESERVE)==0);
        assert(prepare(&charge,capacities[i])==0);
        assert(charge.stage==1&&heap.metadata_pending==bytes);
        NativeBuiltinRegistry *s=(void *)(uintptr_t)charge.allocation;
        assert(s&&s->registry.slots==(uintptr_t)s->slots&&s->registry.capacity==capacities[i]);
        BuiltinDescriptors expected;gc_builtin_descriptors_prepare(&expected);
        expected.record.element_bitmap=expected.list.element_bitmap=(uintptr_t)&s->builtins.pointer_bit;
        assert(!memcmp(&s->builtins,&expected,sizeof(expected)));
        for(uint64_t k=0;k<capacities[i];k++){
            uint64_t value=k==1?(uintptr_t)&s->builtins.raw:k==2?(uintptr_t)&s->builtins.record:k==4?(uintptr_t)&s->builtins.list:0;
            assert(atomic_load(&s->slots[k])==value);
        }
        for(uint64_t k=sizeof(*s)+capacities[i]*8;k<bytes;k++)assert(!((unsigned char *)s)[k]);
        MetadataCharge before=charge;
        assert(prepare(&charge,capacities[i])==GC_NATIVE_METADATA_INVALID&&!memcmp(&charge,&before,sizeof(charge)));
        assert(budget(&heap,&charge,GC_BUDGET_COMMIT)==0);
        assert(gc_native_metadata_dispose(&charge,ops)==0);
        assert(budget(&heap,&charge,GC_BUDGET_RELEASE)==0);
        assert(!heap.metadata_pending&&heap.metadata_committed==sizeof(AllocationPage));
    }
    for(unsigned fault=0;fault<12;fault++){
        MetadataCharge charge={.bytes=ops->quantum,.owner=0x1000,.stage=1};uint64_t capacity=5;
        switch(fault){
        case 0:charge.stage=0;break;case 1:charge.stage=2;break;
        case 2:charge.allocation=0x2000;break;case 3:charge.owner=0;break;case 4:charge.owner++;break;
        case 5:charge.bytes=0;break;case 6:charge.bytes++;break;
        case 7:capacity=0;break;case 8:capacity=4;break;case 9:capacity=UINT64_C(0x100000001);break;
        case 10:capacity=ops->quantum/8;break;case 11:capacity=UINT64_MAX;break;
        }
        MetadataCharge before=charge;
        assert(prepare(&charge,capacity)==GC_NATIVE_METADATA_INVALID&&!memcmp(&charge,&before,sizeof(charge)));
    }
    MetadataCharge charge={.bytes=UINT64_MAX-(ops->quantum-1),.owner=0x1000,.stage=1},before=charge;
    assert(prepare(&charge,5)==GC_NATIVE_METADATA_OS_ERROR&&!memcmp(&charge,&before,sizeof(charge)));
    assert(prepare(NULL,5)==GC_NATIVE_METADATA_INVALID);
    assert(prepare((void *)((unsigned char *)&charge+1),5)==GC_NATIVE_METADATA_INVALID);
    assert(!memcmp(&charge,&before,sizeof(charge)));
    puts("native builtin registry: three mappings, immutable shapes, 14 rejected inputs and map failure PASS");
}
#endif
