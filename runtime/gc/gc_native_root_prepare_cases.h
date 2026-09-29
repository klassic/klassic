#ifndef KLASSIC_GC_NATIVE_ROOT_PREPARE_CASES_H
#define KLASSIC_GC_NATIVE_ROOT_PREPARE_CASES_H
#include "gc_native_roots.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_native_root_prepare_cases(NativeRootPreparer prepare,NativeRootDisposer dispose,
        uint64_t (*budget)(HeapPool *,MetadataCharge *,uint64_t),const NativeMetadataOps *ops){
    const uint64_t counts[]={1,3,256};
    for(unsigned i=0;i<3;i++){
        uint64_t bytes=0;assert(gc_native_roots_size(counts[i],7,ops->quantum,&bytes)==GC_NATIVE_METADATA_OK);
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),.metadata_max=sizeof(AllocationPage)+bytes};
        MetadataCharge charge={.bytes=bytes};assert(budget(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        RootStorageSpec spec={17,counts[i],7,bytes},before_spec=spec;
        assert(prepare(&charge,&spec)==GC_NATIVE_METADATA_OK);
        assert(!memcmp(&spec,&before_spec,sizeof(spec))&&charge.stage==1&&heap.metadata_pending==bytes);
        RootStorage *s=(void *)(uintptr_t)charge.allocation;
        assert(s&&s->registration.segment==(uintptr_t)&s->segment&&s->segment.owner==17);
        assert(s->segment.cells==(uintptr_t)(s+1)&&s->segment.capacity==counts[i]);
        assert(s->segment.journal==(uintptr_t)&s->journal&&s->journal.capacity==7);
        assert(s->journal.entries==(uintptr_t)(s+1)+counts[i]*sizeof(RootCell));
        for(uint64_t k=sizeof(*s);k<bytes;k++)assert(!((unsigned char *)s)[k]);
        MetadataCharge before=charge;
        assert(prepare(&charge,&spec)==GC_NATIVE_METADATA_INVALID&&!memcmp(&charge,&before,sizeof(charge)));
        for(unsigned fault=0;fault<9;fault++){
            MetadataCharge bad=charge;
            switch(fault){
            case 0:bad.stage=0;break;case 1:bad.stage=3;break;
            case 2:bad.owner=0;break;case 3:bad.owner++;break;
            case 4:bad.bytes=0;break;case 5:bad.bytes++;break;
            case 6:bad.allocation=0;break;case 7:bad.allocation++;break;
            case 8:atomic_store(&s->segment.state,1);break;
            }
            MetadataCharge saved=bad;
            assert(dispose(&bad)==GC_NATIVE_METADATA_INVALID&&!memcmp(&bad,&saved,sizeof(bad)));
            atomic_store(&s->segment.state,0);
        }
        assert(dispose(NULL)==GC_NATIVE_METADATA_INVALID);
        assert(dispose((void *)((unsigned char *)&charge+1))==GC_NATIVE_METADATA_INVALID);
        assert(budget(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
        assert(dispose(&charge)==GC_NATIVE_METADATA_OK);
        before=charge;
        assert(!charge.allocation&&dispose(&charge)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&charge,&before,sizeof(charge)));
        assert(budget(&heap,&charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        assert(!heap.metadata_pending&&heap.metadata_committed==sizeof(AllocationPage));
    }
    for(unsigned fault=0;fault<14;fault++){
        MetadataCharge charge={.bytes=ops->quantum,.owner=0x1000,.stage=1};RootStorageSpec spec={17,3,2,ops->quantum};
        switch(fault){
        case 0:charge.stage=0;break;case 1:charge.stage=2;break;case 2:charge.allocation=0x2000;break;
        case 3:charge.owner=0;break;case 4:charge.owner++;break;case 5:charge.bytes=0;spec.bytes=0;break;
        case 6:charge.bytes++;spec.bytes++;break;case 7:spec.bytes++;break;case 8:spec.owner=0;break;
        case 9:spec.cells=0;break;case 10:spec.entries=0;break;case 11:spec.cells=UINT64_C(0x100000000);break;
        case 12:spec.entries=UINT64_C(0x100000000);break;case 13:spec.cells=ops->quantum/32+1;break;
        }
        MetadataCharge before=charge;RootStorageSpec before_spec=spec;
        assert(prepare(&charge,&spec)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&charge,&before,sizeof(charge))&&!memcmp(&spec,&before_spec,sizeof(spec)));
    }
    MetadataCharge charge={.bytes=UINT64_MAX-(ops->quantum-1),.owner=0x1000,.stage=1},before=charge;
    RootStorageSpec spec={17,3,2,charge.bytes},before_spec=spec;
    assert(prepare(&charge,&spec)==GC_NATIVE_METADATA_OS_ERROR); // actual native mapping rejects an impossible extent
    assert(!memcmp(&charge,&before,sizeof(charge))&&!memcmp(&spec,&before_spec,sizeof(spec)));
    assert(prepare(NULL,&spec)==GC_NATIVE_METADATA_INVALID);
    assert(prepare(&charge,NULL)==GC_NATIVE_METADATA_INVALID);
    assert(prepare((void *)((unsigned char *)&charge+1),&spec)==GC_NATIVE_METADATA_INVALID);
    assert(prepare(&charge,(const void *)((const unsigned char *)&spec+1))==GC_NATIVE_METADATA_INVALID);
    assert(!memcmp(&charge,&before,sizeof(charge))&&!memcmp(&spec,&before_spec,sizeof(spec)));
    puts("native root preparation: native mapping plus shared initialization, 18 rejected inputs and real mapping failure PASS");
}
#endif
