#ifndef KLASSIC_GC_NATIVE_BUILTIN_REGISTRY_CASES_H
#define KLASSIC_GC_NATIVE_BUILTIN_REGISTRY_CASES_H
#include "gc_native_builtin_registry.h"
#include "gc_builtin_descriptor_cases.h"
static DescriptorRegister bootstrap_register;
static unsigned bootstrap_calls,bootstrap_failure;
static uint64_t bootstrap_register_candidate(const DescriptorRegistry *r,const TypeDescriptor *d){
    if(++bootstrap_calls==bootstrap_failure)return GC_DESC_INVALID;
    return bootstrap_register(r,d);
}
static void *bootstrap_map_failure(size_t bytes){(void)bytes;return NULL;}
static void gc_run_native_builtin_registry_cases(DescriptorRegister reg,BuiltinPreparer prepare,
        uint64_t (*budget)(HeapPool *,MetadataCharge *,uint64_t),const NativeMetadataOps *native){
    bootstrap_register=reg;
    uint64_t bytes=17;
    assert(gc_native_builtin_registry_size(4,native->quantum,&bytes)==GC_NATIVE_METADATA_INVALID&&bytes==17);
    assert(gc_native_builtin_registry_size(UINT64_MAX,native->quantum,&bytes)==GC_NATIVE_METADATA_INVALID&&bytes==17);
    assert(gc_native_builtin_registry_size(64,native->quantum,&bytes)==GC_NATIVE_METADATA_OK);
    for(unsigned failure=0;failure<=4;failure++){
        NativeMetadataOps ops=*native;if(failure==4)ops.map=bootstrap_map_failure;
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),.metadata_max=sizeof(AllocationPage)+bytes};
        MetadataCharge charge={.bytes=bytes};
        assert(budget(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        bootstrap_calls=0;bootstrap_failure=failure;
        MetadataCharge before=charge;
        MetadataCharge *misaligned=(void *)((unsigned char *)&charge+1);
        assert(gc_native_builtin_registry_prepare(misaligned,64,&ops,prepare,bootstrap_register_candidate)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&charge,&before,sizeof(charge))&&!bootstrap_calls);
        uint64_t result=gc_native_builtin_registry_prepare(&charge,64,&ops,prepare,bootstrap_register_candidate);
        assert(result==(failure==4?GC_NATIVE_METADATA_OS_ERROR:failure?GC_NATIVE_METADATA_INVALID:GC_NATIVE_METADATA_OK));
        assert(charge.stage==1&&heap.metadata_pending==bytes&&heap.metadata_committed==sizeof(AllocationPage));
        if(failure==4){assert(!charge.allocation&&!bootstrap_calls);}
        else{
            assert(charge.allocation);
            NativeBuiltinRegistry *s=(void *)(uintptr_t)charge.allocation;
            const uint64_t ids[]={1,2,4};
            const TypeDescriptor *shapes[]={&s->builtins.raw,&s->builtins.record,&s->builtins.list};
            for(unsigned i=0;i<3;i++)assert(atomic_load(&s->slots[ids[i]])==
                ((!failure||i+1<failure)?(uintptr_t)shapes[i]:0));
            if(!failure)assert(budget(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
            assert(gc_native_metadata_dispose(&charge,&ops)==GC_NATIVE_METADATA_OK);
        }
        assert(budget(&heap,&charge,failure?GC_BUDGET_ABORT:GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        assert(!heap.metadata_pending&&heap.metadata_committed==sizeof(AllocationPage));
    }
    puts("descriptor bootstrap failures: map and every registration failure clean private storage before budget rollback PASS");
}
#endif
