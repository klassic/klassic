#ifndef KLASSIC_GC_NATIVE_STARTUP_CASES_H
#define KLASSIC_GC_NATIVE_STARTUP_CASES_H
#include "gc_native_startup.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static NativeStartupReserver startup_candidate;
static uint64_t gc_startup_case_reserve(HeapPool *h,MetadataCharge *r,MetadataCharge *d,
        uint64_t cells,uint64_t entries,uint64_t capacity,uint64_t quantum){
    NativeStartupRequest q={(uintptr_t)r,(uintptr_t)d,cells,entries,capacity,quantum},before=q;
    uint64_t status=startup_candidate(h,&q);
    assert(!memcmp(&q,&before,sizeof(q)));return status;
}
static void gc_run_native_startup_cases(uint64_t quantum,NativeStartupReserver reserve){
    startup_candidate=reserve;
    uint64_t rb=0,db=0;
    assert(gc_native_roots_size(3,2,quantum,&rb)==0);
    assert(gc_native_builtin_registry_size(64,quantum,&db)==0);
    for(unsigned fault=0;fault<16;fault++){
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),
            .metadata_pending=quantum,.metadata_max=sizeof(AllocationPage)+quantum+rb+db};
        MetadataCharge roots={0},descriptors={0};
        uint64_t cells=3,entries=2,capacity=64,q=quantum,expected=GC_BUDGET_INVALID;
        switch(fault){
        case 0:heap.metadata_max--;expected=GC_BUDGET_LIMIT;break;
        case 1:cells=0;break;case 2:entries=0;break;case 3:capacity=4;break;
        case 4:cells=UINT64_MAX;break;case 5:entries=UINT64_MAX;break;
        case 6:capacity=UINT64_MAX;break;case 7:q=3;break;
        case 8:q=UINT64_C(1)<<63;break;
        case 9:roots.bytes=8;break;case 10:roots.stage=1;break;
        case 11:descriptors.owner=(uintptr_t)&heap;break;
        case 12:descriptors.allocation=0x1000;break;
        case 13:heap.page_count=0;break;
        case 14:heap.metadata_pending=heap.metadata_max;break;
        case 15:heap.metadata_committed=0;break;
        }
        HeapPool before=heap;MetadataCharge r=roots,d=descriptors;
        assert(gc_startup_case_reserve(&heap,&roots,&descriptors,cells,entries,capacity,q)==expected);
        assert(!memcmp(&heap,&before,sizeof(heap))&&!memcmp(&roots,&r,sizeof(r))&&!memcmp(&descriptors,&d,sizeof(d)));
    }
    for(unsigned commit=0;commit<4;commit++){
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),
            .metadata_pending=quantum,.metadata_max=sizeof(AllocationPage)+quantum+rb+db};
        MetadataCharge roots={0},descriptors={0};HeapPool before=heap;
        assert(gc_startup_case_reserve(&heap,&roots,&roots,3,2,64,quantum)==GC_BUDGET_INVALID);
        assert(gc_startup_case_reserve(&heap,(void *)((unsigned char *)&roots+1),&descriptors,3,2,64,quantum)==GC_BUDGET_INVALID);
        assert(!memcmp(&heap,&before,sizeof(heap))&&!roots.bytes&&!descriptors.bytes);
        assert(gc_startup_case_reserve(&heap,&roots,&descriptors,3,2,64,quantum)==GC_BUDGET_OK);
        assert(roots.bytes==rb&&descriptors.bytes==db&&roots.owner==(uintptr_t)&heap&&descriptors.owner==(uintptr_t)&heap);
        assert(roots.stage==1&&descriptors.stage==1&&!roots.allocation&&!descriptors.allocation);
        assert(heap.metadata_pending==quantum+rb+db&&heap.metadata_committed==sizeof(AllocationPage));
        before=heap;MetadataCharge r=roots,d=descriptors;
        assert(gc_startup_case_reserve(&heap,&roots,&descriptors,3,2,64,quantum)==GC_BUDGET_INVALID);
        assert(!memcmp(&heap,&before,sizeof(heap))&&!memcmp(&roots,&r,sizeof(r))&&!memcmp(&descriptors,&d,sizeof(d)));
        MetadataCharge *charges[]={&roots,&descriptors};
        for(unsigned i=0;i<2;i++){
            if(commit&(1u<<i)){
                // Accounting-only model: no native mapping is dereferenced.
                charges[i]->allocation=0x1000;
                assert(gc_budget_step_locked(&heap,charges[i],GC_BUDGET_COMMIT)==GC_BUDGET_OK);
                charges[i]->allocation=0;
                assert(gc_budget_step_locked(&heap,charges[i],GC_BUDGET_RELEASE)==GC_BUDGET_OK);
            }else assert(gc_budget_step_locked(&heap,charges[i],GC_BUDGET_ABORT)==GC_BUDGET_OK);
        }
        assert(heap.metadata_pending==quantum&&heap.metadata_committed==sizeof(AllocationPage));
    }
    puts("native startup admission: paired root/descriptor budgets reject partial admission and clean independently PASS");
}
static void gc_run_startup_prepare_rejections(const NativeStartupRequest *q,uint64_t owner,
        const NativeStartupPreparationOps *ops,NativeStartupPreparer candidate){
    for(unsigned fault=0;fault<18;fault++){
        MetadataCharge roots=*(MetadataCharge *)(uintptr_t)q->roots;
        MetadataCharge descriptors=*(MetadataCharge *)(uintptr_t)q->descriptors;
        NativeStartupRequest request=*q;
        request.roots=(uintptr_t)&roots;request.descriptors=(uintptr_t)&descriptors;
        NativeStartupPreparationOps callbacks=*ops;NativeMetadataOps native=*ops->native;
        callbacks.native=&native;uint64_t id=owner;
        switch(fault){
        case 0:id=0;break;case 1:request.cells=0;break;case 2:request.entries=0;break;
        case 3:request.capacity=4;break;case 4:request.quantum*=2;break;
        case 5:roots.stage=0;break;case 6:descriptors.stage=2;break;
        case 7:roots.allocation=0x1000;break;case 8:descriptors.allocation=0x1000;break;
        case 9:descriptors.owner+=8;break;case 10:roots.bytes+=q->quantum;break;
        case 11:descriptors.bytes+=q->quantum;break;case 12:request.roots=request.descriptors;break;
        case 13:request.roots++;break;case 14:request.descriptors=0;break;
        case 15:callbacks.roots=NULL;callbacks.initialize_roots=NULL;break;
        case 16:callbacks.descriptors=NULL;callbacks.initialize_descriptors=NULL;break;
        case 17:native.map=NULL;break;
        }
        MetadataCharge r=roots,d=descriptors;NativeStartupRequest before=request;
        assert(gc_native_startup_prepare(&request,id,&callbacks)==GC_NATIVE_METADATA_INVALID);
        if(candidate&&fault<15)assert(candidate(&request,id)==GC_NATIVE_METADATA_INVALID);
        assert(!memcmp(&roots,&r,sizeof(r))&&!memcmp(&descriptors,&d,sizeof(d))&&!memcmp(&request,&before,sizeof(before)));
    }
    if(candidate){
        assert(candidate(NULL,owner)==GC_NATIVE_METADATA_INVALID);
        assert(candidate((const void *)((const unsigned char *)q+1),owner)==GC_NATIVE_METADATA_INVALID);
    }
    assert(gc_native_startup_prepare(NULL,owner,ops)==GC_NATIVE_METADATA_INVALID);
    assert(gc_native_startup_prepare((const void *)((const unsigned char *)q+1),owner,ops)==GC_NATIVE_METADATA_INVALID);
    assert(gc_native_startup_prepare(q,owner,NULL)==GC_NATIVE_METADATA_INVALID);
    puts("native startup preparation: 21 rejected configurations preserve private charges and request PASS");
}
#endif
