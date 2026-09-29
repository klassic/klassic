#ifndef KLASSIC_GC_NATIVE_METADATA_CASES_H
#define KLASSIC_GC_NATIVE_METADATA_CASES_H
#include "gc_native_metadata.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static NativeMetadataOps native_metadata_base;
static bool native_metadata_fail_map,native_metadata_fail_unmap;
static void *native_metadata_test_map(size_t bytes){
    return native_metadata_fail_map?NULL:native_metadata_base.map(bytes);
}
static bool native_metadata_test_unmap(void *p,size_t bytes){
    return !native_metadata_fail_unmap&&native_metadata_base.unmap(p,bytes);
}
static void gc_run_native_metadata_cases(uint64_t (*budget)(HeapPool *,MetadataCharge *,uint64_t),const NativeMetadataOps *candidate){
    NativeMetadataOps ops=candidate?*candidate:gc_native_metadata_ops();assert(ops.quantum);
    native_metadata_base=ops;
    ops.map=native_metadata_test_map;ops.unmap=native_metadata_test_unmap;
    uint64_t size=17;
    assert(gc_native_metadata_size(0,ops.quantum,&size)==GC_NATIVE_METADATA_INVALID&&size==17);
    assert(gc_native_metadata_size(UINT64_MAX,ops.quantum,&size)==GC_NATIVE_METADATA_INVALID&&size==17);
    assert(gc_native_metadata_size(1,3,&size)==GC_NATIVE_METADATA_INVALID&&size==17);
    assert(gc_native_metadata_size(ops.quantum+1,ops.quantum,&size)==GC_NATIVE_METADATA_OK&&size==2*ops.quantum);
    for(unsigned failure=0;failure<4;failure++){
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),
            .metadata_max=sizeof(AllocationPage)+size};
        MetadataCharge charge={.bytes=size},before=charge;
        assert(gc_native_metadata_prepare(&charge,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&charge,&before,sizeof(charge)));
        assert(budget(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        before=charge;
        MetadataCharge *misaligned=(void *)((unsigned char *)&charge+1);
        assert(gc_native_metadata_prepare(misaligned,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(gc_native_metadata_dispose(misaligned,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&charge,&before,sizeof(charge)));
        native_metadata_fail_map=failure==0;
        uint64_t status=gc_native_metadata_prepare(&charge,&ops);
        if(!failure){
            assert(status==GC_NATIVE_METADATA_OS_ERROR&&!memcmp(&charge,&before,sizeof(charge)));
            assert(heap.metadata_pending==size&&heap.metadata_committed==sizeof(AllocationPage));
            assert(budget(&heap,&charge,GC_BUDGET_ABORT)==GC_BUDGET_OK);
        }else{
            assert(status==GC_NATIVE_METADATA_OK);
            unsigned char *bytes=(void *)(uintptr_t)charge.allocation;
            for(uint64_t i=0;i<size;i++)assert(!bytes[i]);
            bytes[0]=0x51;bytes[size-1]=0xa7;
            before=charge;
            assert(gc_native_metadata_prepare(&charge,&ops)==GC_NATIVE_METADATA_INVALID);
            assert(!memcmp(&charge,&before,sizeof(charge)));
            if(failure>=2)assert(budget(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
            before=charge;HeapPool held=heap;
            native_metadata_fail_unmap=failure<3;
            status=gc_native_metadata_dispose(&charge,&ops);
            if(failure<3){
                assert(status==GC_NATIVE_METADATA_OS_ERROR&&!memcmp(&charge,&before,sizeof(charge)));
                assert(!memcmp(&heap,&held,sizeof(heap))&&bytes[0]==0x51&&bytes[size-1]==0xa7);
                assert(budget(&heap,&charge,failure==1?GC_BUDGET_ABORT:GC_BUDGET_RELEASE)==GC_BUDGET_INVALID);
                assert(!memcmp(&charge,&before,sizeof(charge))&&!memcmp(&heap,&held,sizeof(heap)));
                native_metadata_fail_unmap=false;
                assert(gc_native_metadata_dispose(&charge,&ops)==GC_NATIVE_METADATA_OK);
            }else assert(status==GC_NATIVE_METADATA_OK);
            assert(!charge.allocation);
            assert(budget(&heap,&charge,failure==1?GC_BUDGET_ABORT:GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        }
        assert(!heap.metadata_pending&&heap.metadata_committed==sizeof(AllocationPage));
        before=charge;
        assert(gc_native_metadata_dispose(&charge,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&charge,&before,sizeof(charge)));
    }
    native_metadata_fail_map=native_metadata_fail_unmap=false;
    puts("native metadata: map failure and pending/committed cleanup retries retain budgets and storage PASS");
}
#endif
