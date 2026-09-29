#ifndef KLASSIC_GC_NATIVE_ROOTS_CASES_H
#define KLASSIC_GC_NATIVE_ROOTS_CASES_H
#include "gc_native_roots.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static NativeMetadataOps root_backing_ops;
static RootStoragePreparer root_backing_prepare;
static bool root_backing_map_fail,root_backing_init_fail,root_backing_unmap_fail;
static unsigned root_backing_map_calls,root_backing_init_calls;
static void *root_backing_map(size_t bytes){
    root_backing_map_calls++;return root_backing_map_fail?NULL:root_backing_ops.map(bytes);
}
static bool root_backing_unmap(void *p,size_t bytes){
    return !root_backing_unmap_fail&&root_backing_ops.unmap(p,bytes);
}
static uint64_t root_backing_initialize(RootStorage *p,const RootStorageSpec *q){
    root_backing_init_calls++;
    uint64_t result=root_backing_prepare(p,q);
    return root_backing_init_fail?GC_ROOT_STORAGE_INVALID:result;
}
static void gc_run_native_roots_cases(RootStoragePreparer prepare,RootStorageDisposalCheck check,
        uint64_t (*budget)(HeapPool *,MetadataCharge *,uint64_t),const NativeMetadataOps *native){
    root_backing_ops=*native;root_backing_prepare=prepare;
    NativeMetadataOps ops={native->quantum,root_backing_map,root_backing_unmap};
    uint64_t bytes=0;assert(gc_native_roots_size(3,2,ops.quantum,&bytes)==GC_NATIVE_METADATA_OK);
    for(unsigned scenario=0;scenario<6;scenario++){
        root_backing_map_fail=scenario==0;root_backing_init_fail=scenario==1||scenario==2;
        root_backing_unmap_fail=scenario==2||scenario==4;
        root_backing_map_calls=root_backing_init_calls=0;
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),.metadata_max=sizeof(AllocationPage)+bytes};
        MetadataCharge charge={.bytes=bytes};
        assert(budget(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        MetadataCharge before=charge;
        MetadataCharge *misaligned=(void *)((unsigned char *)&charge+1);
        assert(gc_native_roots_prepare(misaligned,17,3,2,&ops,root_backing_initialize)==GC_NATIVE_METADATA_INVALID);
        assert(gc_native_roots_dispose(misaligned,&ops,check)==GC_NATIVE_METADATA_INVALID);
        assert(gc_native_roots_prepare(&charge,0,3,2,&ops,root_backing_initialize)==GC_NATIVE_METADATA_INVALID);
        assert(gc_native_roots_prepare(&charge,17,3,2,&ops,NULL)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&charge,&before,sizeof(charge))&&!root_backing_map_calls&&!root_backing_init_calls);
        uint64_t status=gc_native_roots_prepare(&charge,17,3,2,&ops,root_backing_initialize);
        assert(status==(scenario==0?GC_NATIVE_METADATA_OS_ERROR:root_backing_init_fail?GC_NATIVE_METADATA_INVALID:GC_NATIVE_METADATA_OK));
        assert(root_backing_map_calls==1&&root_backing_init_calls==(scenario!=0));
        assert(heap.metadata_pending==bytes&&heap.metadata_committed==sizeof(AllocationPage));
        if(scenario==0){assert(!memcmp(&charge,&before,sizeof(charge)));}
        else{
            RootStorage *storage=(void *)(uintptr_t)charge.allocation;
            assert(storage&&!storage->registration.registered&&!storage->segment.scope);
            assert(storage->registration.segment==(uintptr_t)&storage->segment);
            if(scenario>=4)assert(budget(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
            before=charge;HeapPool held=heap;RootStorage saved;memcpy(&saved,storage,sizeof(saved));
            status=gc_native_roots_dispose(&charge,&ops,check);
            if(root_backing_unmap_fail){
                assert(status==GC_NATIVE_METADATA_OS_ERROR&&!memcmp(&charge,&before,sizeof(charge)));
                assert(!memcmp(&heap,&held,sizeof(heap))&&!memcmp(storage,&saved,sizeof(saved)));
                assert(budget(&heap,&charge,scenario>=4?GC_BUDGET_RELEASE:GC_BUDGET_ABORT)==GC_BUDGET_INVALID);
                assert(!memcmp(&heap,&held,sizeof(heap))&&!memcmp(&charge,&before,sizeof(charge)));
                root_backing_unmap_fail=false;
                assert(gc_native_roots_dispose(&charge,&ops,check)==GC_NATIVE_METADATA_OK);
            }else assert(status==GC_NATIVE_METADATA_OK);
            assert(!charge.allocation);
        }
        assert(budget(&heap,&charge,scenario>=4?GC_BUDGET_RELEASE:GC_BUDGET_ABORT)==GC_BUDGET_OK);
        assert(!heap.metadata_pending&&heap.metadata_committed==sizeof(AllocationPage));
    }
    puts("root backing failures: six map/initializer/disposal scenarios retain storage and budget until cleanup succeeds PASS");
}
#endif
