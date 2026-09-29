#ifndef KLASSIC_GC_NATIVE_STARTUP_COMMIT_CASES_H
#define KLASSIC_GC_NATIVE_STARTUP_COMMIT_CASES_H
#include "gc_native_startup.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void gc_run_startup_commit_cases(NativeStartupCommitter candidate){
    NativeStartupCommitter implementations[]={gc_native_startup_commit_locked,candidate};
    for(unsigned implementation=0;implementation<2;implementation++)for(unsigned fault=0;fault<35;fault++){
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),
            .metadata_pending=12288,.metadata_max=sizeof(AllocationPage)+12288};
        MetadataCharge roots={4096,(uintptr_t)&heap,0x10000,1},descriptors={4096,(uintptr_t)&heap,0x20000,1};
        NativeStartupRequest q={(uintptr_t)&roots,(uintptr_t)&descriptors,3,2,64,4096};
        HeapPool *h=&heap;const NativeStartupRequest *request=&q;
        switch(fault){
        case 0:break;case 1:roots.bytes=0;break;case 2:descriptors.bytes=0;break;
        case 3:roots.owner=0;break;case 4:descriptors.owner+=8;break;
        case 5:roots.stage=0;break;case 6:descriptors.stage=2;break;
        case 7:roots.allocation=0;break;case 8:descriptors.allocation=0;break;
        case 9:roots.allocation++;break;case 10:descriptors.allocation++;break;
        case 11:roots.allocation=UINT64_MAX-7;break;case 12:descriptors.allocation=UINT64_MAX-7;break;
        case 13:heap.page_count=0;break;case 14:heap.page_count=UINT64_MAX;break;
        case 15:heap.metadata_committed=0;break;case 16:heap.metadata_committed=heap.metadata_max+1;break;
        case 17:heap.metadata_pending=heap.metadata_max;break;
        case 18:heap.metadata_pending=4096;break; // each alone fits, pair does not
        case 19:roots.bytes=16384;break;case 20:descriptors.bytes=16384;break;
        case 21:q.roots=q.descriptors;break;case 22:q.roots=0;break;case 23:q.descriptors++;break;
        case 24:request=NULL;break;case 25:request=(const void *)((const unsigned char *)&q+1);break;
        case 26:h=NULL;break;case 27:h=(void *)((unsigned char *)&heap+1);break;
        case 28:heap.metadata_max=UINT64_MAX;heap.metadata_committed=UINT64_MAX-8192;heap.metadata_pending=8192;break;
        case 29:descriptors.allocation=roots.allocation;break;
        case 30:descriptors.allocation=roots.allocation+8;break;
        case 31:roots.allocation=descriptors.allocation+8;break;
        case 32:descriptors.allocation=roots.allocation+roots.bytes-8;break;
        case 33:descriptors.allocation=roots.allocation+roots.bytes;break;
        case 34:roots.allocation=descriptors.allocation+descriptors.bytes;break;
        }
        HeapPool expected=heap;MetadataCharge r=roots,d=descriptors;NativeStartupRequest before=q;
        bool success=fault==0||fault==28||fault==33||fault==34;
        if(success){expected.metadata_committed+=8192;expected.metadata_pending-=8192;r.stage=d.stage=2;}
        assert(implementations[implementation](h,request)==(success?GC_BUDGET_OK:GC_BUDGET_INVALID));
        assert(!memcmp(&heap,&expected,sizeof(heap))&&!memcmp(&roots,&r,sizeof(r))&&
            !memcmp(&descriptors,&d,sizeof(d))&&!memcmp(&q,&before,sizeof(q)));
        if(success){
            assert(implementations[implementation](h,request)==GC_BUDGET_INVALID);
            assert(!memcmp(&heap,&expected,sizeof(heap))&&!memcmp(&roots,&r,sizeof(r))&&!memcmp(&descriptors,&d,sizeof(d)));
            // No mapping is dereferenced by this accounting-only test.
            roots.allocation=descriptors.allocation=0;
            assert(gc_budget_step_locked(&heap,&roots,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
            assert(gc_budget_step_locked(&heap,&descriptors,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
            assert(heap.metadata_committed==expected.metadata_committed-8192&&heap.metadata_pending==expected.metadata_pending);
        }
    }
    puts("native startup commit: 35 C/candidate all-or-nothing budget and unchanged rejection cases PASS");
}
static void gc_run_startup_release_cases(NativeStartupReleaser candidate){
    unsigned count=0;
    for(unsigned impl=0;impl<2;impl++)for(unsigned stages=0;stages<4;stages++)for(unsigned fault=0;fault<20;fault++){
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage)+2048,.metadata_pending=1024,.metadata_max=UINT64_MAX};
        MetadataCharge roots={4096,(uintptr_t)&heap,0,1+(stages&1)},descriptors={8192,(uintptr_t)&heap,0,1+((stages>>1)&1)};
        MetadataCharge *charges[]={&roots,&descriptors};
        for(unsigned i=0;i<2;i++)if(charges[i]->stage==1)heap.metadata_pending+=charges[i]->bytes;else heap.metadata_committed+=charges[i]->bytes;
        NativeStartupRequest q={(uintptr_t)&roots,(uintptr_t)&descriptors,0,0,0,0};
        HeapPool *h=&heap;const NativeStartupRequest *request=&q;
        switch(fault){
        case 0:break;case 1:roots.allocation=0x10000;break;case 2:descriptors.allocation=0x20000;break;
        case 3:roots.stage=3;break;case 4:descriptors.stage=0;break;case 5:roots.bytes=0;break;
        case 6:descriptors.owner+=8;break;
        case 7:heap.metadata_pending=0;heap.metadata_committed=sizeof(AllocationPage);break;
        case 8:heap.metadata_pending=UINT64_MAX;break;case 9:heap.page_count=0;break;
        case 10:q.roots=q.descriptors;break;case 11:request=NULL;break;
        case 12:request=(const void *)((uintptr_t)&q+1);break;case 13:h=NULL;break;
        case 14:h=(void *)((uintptr_t)&heap+1);break;case 15:q.descriptors++;break;
        case 16:heap.metadata_committed=sizeof(AllocationPage)-1;break;case 17:heap.page_count=UINT64_MAX;break;
        case 18:roots.stage=descriptors.stage=1;roots.bytes=descriptors.bytes=4096;heap.metadata_pending=4096;break;
        case 19:roots.stage=descriptors.stage=2;roots.bytes=descriptors.bytes=4096;heap.metadata_committed=sizeof(AllocationPage)+4096;break;
        }
        HeapPool expected=heap;MetadataCharge r=roots,d=descriptors;NativeStartupRequest before=q;
        if(!fault){expected.metadata_pending=1024;expected.metadata_committed=sizeof(AllocationPage)+2048;r.stage=d.stage=3;}
        NativeStartupReleaser release=impl?candidate:gc_native_startup_release_locked;
        assert(release(h,request)==(fault?GC_BUDGET_INVALID:GC_BUDGET_OK));
        assert(!memcmp(&heap,&expected,sizeof(heap))&&!memcmp(&roots,&r,sizeof(r))&&!memcmp(&descriptors,&d,sizeof(d))&&!memcmp(&q,&before,sizeof(q)));
        if(!fault){assert(release(h,request)==GC_BUDGET_INVALID);assert(!memcmp(&heap,&expected,sizeof(heap))&&!memcmp(&roots,&r,sizeof(r))&&!memcmp(&descriptors,&d,sizeof(d)));}
        count++;
    }
    assert(count==160);
    puts("native startup release: 160 C/candidate mixed-budget cleanup and unchanged rejection cases PASS");
}
#endif
