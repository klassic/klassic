#ifndef KLASSIC_GC_NATIVE_PIN_REGISTRY_CASES_H
#define KLASSIC_GC_NATIVE_PIN_REGISTRY_CASES_H
#include "gc_native_pin_registry.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
static void *gc_pin_registry_fail_map(size_t bytes){(void)bytes;return NULL;}
static bool gc_pin_registry_fail_unmap(void *p,size_t bytes){(void)p;(void)bytes;return false;}
static void gc_run_native_pin_registry_cases(void){
    NativeMetadataOps ops=gc_native_metadata_ops();uint64_t bytes=17;
    assert(gc_native_pin_registry_size(0,ops.quantum,&bytes)==GC_NATIVE_METADATA_INVALID&&bytes==17);
    assert(gc_native_pin_registry_size(UINT64_MAX,ops.quantum,&bytes)==GC_NATIVE_METADATA_INVALID&&bytes==17);
    assert(gc_native_pin_registry_size(2,3,&bytes)==GC_NATIVE_METADATA_INVALID&&bytes==17);
    assert(gc_native_pin_registry_size(2,ops.quantum,&bytes)==GC_NATIVE_METADATA_OK);
    for(unsigned mode=0;mode<4;mode++){
        AllocationPage pages[2]={{.incarnation=7,.state=GC_PAGE_CLOSED},{.incarnation=8,.state=GC_PAGE_CLOSED,.run_head=1}};
        MetadataLeaseSlot slots[4]={{.next_free=2},{.next_free=3},{.next_free=4},{0}};
        HeapPool heap={.pages=(uintptr_t)pages,.page_count=2,.metadata_committed=sizeof(pages),.metadata_max=sizeof(pages)+bytes};
        MetadataLeasePool pool={(uintptr_t)&heap,(uintptr_t)slots,4,1,0};
        MetadataCharge charge={.bytes=bytes};
        assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        NativeMetadataOps failing=ops;failing.map=gc_pin_registry_fail_map;
        MetadataCharge before=charge;
        assert(gc_native_pin_registry_prepare(&charge,&pool,&failing)==GC_NATIVE_METADATA_OS_ERROR);
        assert(!memcmp(&before,&charge,sizeof(charge))&&heap.metadata_pending==bytes);
        if(!mode){assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_ABORT)==GC_BUDGET_OK);continue;}
        MetadataLeasePool wrong=pool;wrong.heap=0;
        assert(gc_native_pin_registry_prepare(&charge,&wrong,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(gc_native_pin_registry_prepare(&charge,&pool,&ops)==GC_NATIVE_METADATA_OK);
        assert(!gc_native_pin_registry_committed(&charge));
        if(mode==1){
            assert(gc_native_pin_registry_dispose(&charge,&ops)==GC_NATIVE_METADATA_OK);
            assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_ABORT)==GC_BUDGET_OK);continue;
        }
        assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
        PinPageRegistry *registry=gc_native_pin_registry_committed(&charge);assert(registry&&registry->capacity==2);
        MetadataLeaseRequest lease={(uintptr_t)&pages[0],7,0};
        assert(gc_metadata_step_locked(&pool,&lease,GC_ML_ACQUIRE)==GC_ML_OK);
        assert(gc_pin_registry_bind_locked(registry,&lease)==GC_RG_OK);
        assert(gc_native_pin_registry_dispose(&charge,&ops)==GC_NATIVE_METADATA_INVALID);
        RelocationPermit permit={0};RelocationGateRequest pin={.permit=(uintptr_t)&permit},selection={0};
        assert(gc_pin_registry_step_locked(registry,&lease,&pin,GC_PP_PIN)==GC_RG_OK);
        assert(gc_pin_registry_unbind_locked(registry,&lease)==GC_RG_WAIT);
        assert(gc_native_pin_registry_dispose(&charge,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(gc_pin_registry_step_locked(registry,&lease,&pin,GC_PP_UNPIN)==GC_RG_OK);
        assert(gc_pin_registry_step_locked(registry,&lease,&selection,GC_PP_SELECT)==GC_RG_OK);
        assert(gc_pin_registry_unbind_locked(registry,&lease)==GC_RG_WAIT);
        assert(gc_native_pin_registry_dispose(&charge,&ops)==GC_NATIVE_METADATA_INVALID);
        assert(gc_pin_registry_step_locked(registry,&lease,&selection,GC_PP_REOPEN)==GC_RG_OK);
        assert(gc_pin_registry_unbind_locked(registry,&lease)==GC_RG_OK);
        assert(gc_metadata_step_locked(&pool,&lease,GC_ML_RELEASE)==GC_ML_OK&&!pool.active);
        if(mode==2){
            failing=ops;failing.unmap=gc_pin_registry_fail_unmap;before=charge;HeapPool saved=heap;
            assert(gc_native_pin_registry_dispose(&charge,&failing)==GC_NATIVE_METADATA_OS_ERROR);
            assert(!memcmp(&before,&charge,sizeof(charge))&&!memcmp(&saved,&heap,sizeof(heap)));
        }
        assert(gc_native_pin_registry_dispose(&charge,&ops)==GC_NATIVE_METADATA_OK&&!charge.allocation);
        assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        assert(heap.metadata_committed==sizeof(pages)&&!heap.metadata_pending);
    }
    puts("native pin inventory: budgeted mapping, private abort, live-binding exclusion and unmap retry PASS");
}
#endif
