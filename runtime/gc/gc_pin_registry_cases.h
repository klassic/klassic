#ifndef KLASSIC_GC_PIN_REGISTRY_CASES_H
#define KLASSIC_GC_PIN_REGISTRY_CASES_H
#include "gc_pin_relocation.h"
#include "gc_source_access.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
typedef uint64_t (*PinRegistryOwner)(PinPageRegistry *,const MetadataLeaseRequest *);
typedef uint64_t (*PinRegistryStep)(PinPageRegistry *,const MetadataLeaseRequest *,RelocationGateRequest *,uint64_t);
typedef uint64_t (*PinSelectionCheck)(PinPageRegistry *,const MetadataLeaseRequest *,const RelocationGateRequest *);
typedef uint64_t (*PinRelocationClaimer)(PinPageRegistry *,const PinRelocationClaim *);
static void gc_run_pin_registry_cases(PinRegistryOwner bind,PinRegistryOwner unbind,
    PinRegistryStep step,PinSelectionCheck check,PinRelocationClaimer claim){
    AllocationPage pages[2]={0};MetadataLeaseSlot slots[4]={0};
    HeapPool heap={.pages=(uintptr_t)pages,.page_count=2};
    MetadataLeasePool pool={(uintptr_t)&heap,(uintptr_t)slots,4,1,0};
    PinPageRecord records[2]={0};PinPageRegistry registry={&pool,records,2};
    MetadataLeaseRequest leases[2];
    for(unsigned i=0;i<4;i++)slots[i].next_free=i==3?0:i+2;
    for(unsigned i=0;i<2;i++){
        pages[i]=(AllocationPage){.start=(i+1)*GC_HEAP_PAGE_BYTES,
            .limit=(i+2)*GC_HEAP_PAGE_BYTES,.incarnation=9+i,.run_head=i,
            .state=GC_PAGE_CLOSED,.allocated_top=(i+1)*GC_HEAP_PAGE_BYTES+64};
        leases[i]=(MetadataLeaseRequest){(uintptr_t)&pages[i],9+i,0};
        assert(gc_metadata_step_locked(&pool,&leases[i],GC_ML_ACQUIRE)==GC_ML_OK);
        assert(bind(&registry,&leases[i])==GC_RG_OK);
        assert(pages[i].metadata_leases==2);
    }
    assert(pool.active==4);
    /* Rejections authenticate both identities without consuming either lease. */
    assert(bind(NULL,&leases[0])==GC_RG_INVALID);
    assert(unbind(&registry,NULL)==GC_RG_INVALID);
    PinPageRegistry bad_registry=registry;bad_registry.capacity=1;
    assert(bind(&bad_registry,&leases[0])==GC_RG_INVALID);
    assert(unbind(&bad_registry,&leases[0])==GC_RG_INVALID);
    bad_registry=registry;bad_registry.capacity=UINT64_MAX;
    assert(bind(&bad_registry,&leases[0])==GC_RG_INVALID);
    MetadataLeaseRequest held=records[0].lifetime;
    records[0].lifetime.token=records[1].lifetime.token;
    assert(unbind(&registry,&leases[0])==GC_RG_INVALID);
    assert(records[0].lifetime.token==records[1].lifetime.token);
    records[0].lifetime=held;
    assert(pool.active==4&&pages[0].metadata_leases==2);

    assert(bind(&registry,&leases[0])==GC_RG_INVALID);
    RelocationPermit pin={0};RelocationGateRequest token={.permit=(uintptr_t)&pin},selection={0};
    assert(step(&registry,&leases[0],&token,GC_PP_PIN)==GC_RG_OK);
    PinPageRecord saved=records[0];
    assert(step(&registry,&leases[0],&selection,GC_PP_SELECT)==GC_RG_WAIT);
    assert(unbind(&registry,&leases[0])==GC_RG_WAIT);
    assert(!memcmp(&saved,&records[0],sizeof(saved)));
    assert(step(&registry,&leases[1],&token,GC_PP_UNPIN)==GC_RG_INVALID);
    MetadataLeaseRequest forged=leases[0];forged.page=leases[1].page;
    assert(step(&registry,&forged,&selection,GC_PP_SELECT)==GC_RG_INVALID);
    forged=leases[0];forged.incarnation++;
    assert(step(&registry,&forged,&selection,GC_PP_SELECT)==GC_RG_INVALID);
    RelocationGate movers={.epoch=1};RelocationPermit mover={0};
    ForwardCell source={.source_offset=GC_HEAP_PAGE_BYTES+32,.incarnation=9};
    MarkQueue queue={.epoch=3};MarkRecord mark={.object_id=4,.young_epoch=3};
    MarkedRelocation work={.claim={.permit=(uintptr_t)&mover,.source=(uintptr_t)&source,
        .source_offset=source.source_offset,.incarnation=9},.queue=(uintptr_t)&queue,
        .record=(uintptr_t)&mark,.epoch=3};
    /* Even a guessed current selection epoch cannot bypass an active pin. */
    RelocationGateRequest guess={.epoch=records[0].admission.epoch};
    assert(check(&registry,&leases[0],&guess)==GC_RG_WAIT);
    assert(check(&registry,&leases[0],NULL)==GC_RG_INVALID);
    PinRelocationClaim request={(uintptr_t)&leases[0],(uintptr_t)&guess,(uintptr_t)&movers,(uintptr_t)&work};
    assert(claim(&registry,NULL)==GC_RCL_INVALID);
    assert(claim(&registry,&request)==GC_RCL_CLOSED);
    assert(!movers.active&&!movers.last_serial&&!atomic_load(&source.word));
    RelocationGateRequest old_token=token;
    assert(step(&registry,&leases[0],&token,GC_PP_UNPIN)==GC_RG_OK);
    assert(step(&registry,&leases[0],&selection,GC_PP_SELECT)==GC_RG_OK);
    assert(check(&registry,&leases[0],&selection)==GC_RG_OK);
    RelocationGateRequest invalid_selection=selection;invalid_selection.epoch++;
    assert(check(&registry,&leases[0],&invalid_selection)==GC_RG_INVALID);
    invalid_selection=selection;invalid_selection.serial=1;
    assert(check(&registry,&leases[0],&invalid_selection)==GC_RG_INVALID);
    invalid_selection=selection;invalid_selection.permit=(uintptr_t)&pin;
    assert(check(&registry,&leases[0],&invalid_selection)==GC_RG_INVALID);
    invalid_selection=selection;
    assert(unbind(&registry,&leases[0])==GC_RG_WAIT);
    token=(RelocationGateRequest){.permit=(uintptr_t)&pin};
    assert(step(&registry,&leases[0],&token,GC_PP_PIN)==GC_RG_WAIT);
    /* A selected different page cannot authorize this source or generation. */
    RelocationGateRequest other={0};
    assert(step(&registry,&leases[1],&other,GC_PP_SELECT)==GC_RG_OK);
    request.lease=(uintptr_t)&leases[1];request.selection=(uintptr_t)&other;
    assert(claim(&registry,&request)==GC_RCL_INVALID);
    request.lease=(uintptr_t)&leases[0];request.selection=(uintptr_t)&selection;
    queue.generation=GC_OLD;
    assert(claim(&registry,&request)==GC_RCL_INVALID);
    queue.generation=GC_YOUNG;
    /* Page checks run before mover admission or source mutation. */
    for(unsigned fault=0;fault<7;fault++){
        AllocationPage original=pages[0];MarkedRelocation original_work=work;
        if(fault==0)atomic_store(&pages[0].state,GC_PAGE_ALLOCATING);
        if(fault==1)pages[0].generation=2;
        if(fault==2)work.claim.incarnation++;
        if(fault==3)pages[0].start=UINT64_MAX-16;
        if(fault==4)work.claim.source_offset=pages[0].start+16;
        if(fault==5)atomic_store(&pages[0].allocated_top,work.claim.source_offset);
        if(fault==6)pages[0].limit=work.claim.source_offset;
        MarkedRelocation rejected=work;
        assert(claim(&registry,&request)==GC_RCL_INVALID);
        assert(!memcmp(&work,&rejected,sizeof(work))&&!movers.active&&!movers.last_serial&&!atomic_load(&source.word));
        pages[0]=original;work=original_work;
    }
    mark.young_epoch=2;
    assert(claim(&registry,&request)==GC_RCL_NOT_SURVIVED);
    queue.epoch=4;
    assert(claim(&registry,&request)==GC_RCL_STALE);
    queue.epoch=3;mark.young_epoch=3;
    assert(!movers.active&&!movers.last_serial&&!atomic_load(&source.word));
    assert(claim(&registry,&request)==GC_RCL_OWNED);
    assert(movers.active==1&&work.claim.ticket&&atomic_load(&source.word)==((work.claim.ticket<<2)|1));
    SourceAccessRequest close={.source_offset=source.source_offset,.incarnation=9,.ticket=work.claim.ticket};
    assert(gc_source_access_step(&source,&close,GC_SA_SELECT)==GC_SA_OK);
    ForwardRequest retain={source.source_offset,9,work.claim.ticket,0};
    assert(gc_forward_step(&source,&retain,GC_FWD_RETAIN)==GC_FWD_OK);
    RelocationGateRequest release={.permit=(uintptr_t)&mover,.serial=work.claim.ticket,.epoch=work.claim.epoch};
    assert(gc_relocation_gate_locked(&movers,&release,GC_RG_RELEASE)==GC_RG_OK);
    atomic_store(&pages[0].state,GC_PAGE_EVACUATED);
    assert(check(&registry,&leases[0],&selection)==GC_RG_OK);
    assert(step(&registry,&leases[0],&selection,GC_PP_REOPEN)==GC_RG_OK);
    assert(check(&registry,&leases[0],&invalid_selection)==GC_RG_INVALID);
    atomic_store(&pages[0].state,GC_PAGE_CLOSED);
    assert(step(&registry,&leases[1],&other,GC_PP_REOPEN)==GC_RG_OK);
    assert(unbind(&registry,&leases[0])==GC_RG_OK);
    assert(pages[0].metadata_leases==1&&pool.active==3);
    MetadataLeaseRequest pressure={leases[1].page,leases[1].incarnation,0};
    assert(gc_metadata_step_locked(&pool,&pressure,GC_ML_ACQUIRE)==GC_ML_OK);
    saved=records[0];
    assert(bind(&registry,&leases[0])==GC_PR_FULL);
    assert(!memcmp(&saved,&records[0],sizeof(saved))&&pool.active==4);
    assert(gc_metadata_step_locked(&pool,&pressure,GC_ML_RELEASE)==GC_ML_OK);
    records[0].admission.epoch=UINT64_MAX;saved=records[0];
    assert(bind(&registry,&leases[0])==GC_RG_EXHAUSTED);
    assert(!memcmp(&saved,&records[0],sizeof(saved))&&pool.active==3);
    records[0].admission.epoch=2;
    atomic_store(&pages[0].state,GC_PAGE_EVACUATED);saved=records[0];
    assert(bind(&registry,&leases[0])==GC_RG_INVALID);
    assert(!memcmp(&saved,&records[0],sizeof(saved))&&pool.active==3);
    atomic_store(&pages[0].state,GC_PAGE_CLOSED);
    assert(bind(&registry,&leases[0])==GC_RG_OK);
    assert(step(&registry,&leases[0],&token,GC_PP_PIN)==GC_RG_OK);
    assert(step(&registry,&leases[0],&old_token,GC_PP_UNPIN)==GC_RG_INVALID);
    atomic_store(&pages[0].state,GC_PAGE_EVACUATED);
    PinPageRecord pinned_snapshot=records[0];
    assert(step(&registry,&leases[0],&selection,GC_PP_SELECT)==GC_RG_INVALID);
    assert(step(&registry,&leases[0],&token,GC_PP_PIN)==GC_RG_INVALID);
    assert(!memcmp(&pinned_snapshot,&records[0],sizeof(pinned_snapshot)));
    assert(step(&registry,&leases[0],&token,GC_PP_UNPIN)==GC_RG_OK);
    atomic_store(&pages[0].state,GC_PAGE_CLOSED);
    /* Caller lease disposal does not dispose the inventory's independent lease. */
    MetadataLeaseRequest old_lease=leases[0];
    assert(gc_metadata_step_locked(&pool,&leases[0],GC_ML_RELEASE)==GC_ML_OK);
    assert(pages[0].metadata_leases==1);
    assert(unbind(&registry,&old_lease)==GC_RG_INVALID);
    leases[0]=(MetadataLeaseRequest){(uintptr_t)&pages[0],9,0};
    assert(gc_metadata_step_locked(&pool,&leases[0],GC_ML_ACQUIRE)==GC_ML_OK);
    for(unsigned i=0;i<2;i++){
        assert(unbind(&registry,&leases[i])==GC_RG_OK);
        assert(gc_metadata_step_locked(&pool,&leases[i],GC_ML_RELEASE)==GC_ML_OK);
        assert(!pages[i].metadata_leases);
    }
    assert(!pool.active&&!movers.active);
    puts("pin relocation bridge: pinned exclusion, page bounds, mark epoch and actual source claim PASS");
    puts("pin registry admission: bound lease authentication, selected epoch and post-move cleanup PASS");
    puts("page pin registry: authenticated pages, independent lifetime, pressure, replay and marked relocation PASS");
}
#endif
