#ifndef KLASSIC_GC_PIN_SOURCE_RELEASE_CASES_H
#define KLASSIC_GC_PIN_SOURCE_RELEASE_CASES_H
#include "gc_pin_source_release.h"
#include "gc_native_metadata.h"
#include "gc_source_access.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
typedef uint64_t (*PinSourceDrainer)(PinPageRegistry *,const PinSourceRequest *);
typedef uint64_t (*PinSourceUncommitter)(PinPageRegistry *,const PinSourceRequest *,uint64_t);
static void gc_run_pin_source_release_cases(PinSourceDrainer drainer,PinSourceUncommitter uncommitter){
    NativeMetadataOps ops=gc_native_metadata_ops();
    for(unsigned generation=0;generation<2;generation++){
        void *mapping=ops.map(3*GC_HEAP_PAGE_BYTES);assert(mapping);
        AllocationPage pages[2]={0};MetadataLeaseSlot slots[4]={{.next_free=2},{.next_free=3},{.next_free=4},{0}};
        HeapPool heap={.heap_base=(uintptr_t)mapping,.pages=(uintptr_t)pages,.page_count=2,
            .hard_max=2*GC_HEAP_PAGE_BYTES,.soft_max=GC_HEAP_PAGE_BYTES,.committed=2*GC_HEAP_PAGE_BYTES};
        MetadataLeasePool pool={(uintptr_t)&heap,(uintptr_t)slots,4,1,0};
        PinPageRecord records[2]={0};PinPageRegistry registry={&pool,records,2};
        MetadataLeaseRequest leases[2];ForwardCell cells[2]={0};ForwardTable table={(uintptr_t)cells,2,2};
        for(unsigned i=0;i<2;i++){
            pages[i]=(AllocationPage){.heap_base=(uintptr_t)mapping,.start=(i+1)*GC_HEAP_PAGE_BYTES,
                .limit=(i+2)*GC_HEAP_PAGE_BYTES,.incarnation=7+i,.identity_base=10+i,
                .generation=generation,.run_head=i,.state=GC_PAGE_CLOSED,.allocated_top=(i+1)*GC_HEAP_PAGE_BYTES+96};
            leases[i]=(MetadataLeaseRequest){(uintptr_t)&pages[i],7+i,0};
            assert(gc_metadata_step_locked(&pool,&leases[i],GC_ML_ACQUIRE)==GC_ML_OK);
            assert(gc_pin_registry_bind_locked(&registry,&leases[i])==GC_RG_OK);
            cells[i]=(ForwardCell){.source_offset=GC_HEAP_PAGE_BYTES+32+48*i,.incarnation=7};
            *(uint64_t *)((char *)mapping+cells[i].source_offset)=11+i;
        }
        pages[0].forward_table=(uintptr_t)&table;
        RelocationPermit permit={0};RelocationGateRequest pin={.permit=(uintptr_t)&permit};
        assert(gc_pin_registry_step_locked(&registry,&leases[0],&pin,GC_PP_PIN)==GC_RG_OK);
        RelocationGateRequest selection={.epoch=records[0].admission.epoch};
        PinSourceDrain drain={.scan={.incarnation=7,.top=pages[0].allocated_top,.used=2,.budget=1}};
        PinSourceDrain saved=drain;AllocationPage before=pages[0];
        assert(drainer(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&drain})==GC_SD_WAIT);
        assert(!memcmp(&saved,&drain,sizeof(drain))&&!memcmp(&before,&pages[0],sizeof(before)));
        assert(gc_pin_registry_step_locked(&registry,&leases[0],&pin,GC_PP_UNPIN)==GC_RG_OK);
        selection=(RelocationGateRequest){0};
        assert(gc_pin_registry_step_locked(&registry,&leases[0],&selection,GC_PP_SELECT)==GC_RG_OK);
        for(unsigned i=0;i<2;i++){
            ForwardRequest claim={cells[i].source_offset,7,i+1,0};
            assert(gc_forward_step(&cells[i],&claim,GC_FWD_CLAIM)==GC_FWD_OK);
            SourceAccessRequest access={.source_offset=cells[i].source_offset,.incarnation=7,.ticket=i+1};
            assert(gc_source_access_step(&cells[i],&access,GC_SA_SELECT)==GC_SA_OK);
            assert(gc_source_access_step(&cells[i],&access,GC_SA_CHECK)==GC_SA_OK);
            uint64_t destination=cells[i].source_offset+GC_HEAP_PAGE_BYTES;
            *(uint64_t *)((char *)mapping+destination)=*(uint64_t *)((char *)mapping+cells[i].source_offset);
            claim.destination=(destination<<16)|0x1510;
            assert(gc_forward_step(&cells[i],&claim,GC_FWD_PUBLISH)==GC_FWD_OK);
        }
        assert(drainer(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&drain})==GC_SD_MORE&&drain.scan.cursor==1);
        PinPageRegistry copied_registry=registry;saved=drain;
        assert(drainer(&copied_registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&drain})==GC_SD_INVALID);
        assert(!memcmp(&saved,&drain,sizeof(drain)));
        assert(gc_pin_registry_step_locked(&registry,&leases[0],&selection,GC_PP_REOPEN)==GC_RG_OK);
        assert(gc_pin_registry_step_locked(&registry,&leases[0],&selection,GC_PP_SELECT)==GC_RG_OK);
        saved=drain;
        assert(drainer(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&drain})==GC_SD_INVALID);
        assert(!memcmp(&saved,&drain,sizeof(drain))&&pages[0].state==GC_PAGE_CLOSED);
        drain=(PinSourceDrain){.scan={.incarnation=7,.top=pages[0].allocated_top,.used=2,.budget=1}};
        assert(drainer(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&drain})==GC_SD_MORE);
        assert(drainer(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&drain})==GC_SD_DONE);
        assert(pages[0].state==GC_PAGE_EVACUATED);
        PinSourceUncommit uncommit={.work={.page_count=1,.incarnation=7}};
        PinSourceUncommit saved_uncommit=uncommit;
        RelocationGateRequest other={0};
        assert(gc_pin_registry_step_locked(&registry,&leases[1],&other,GC_PP_SELECT)==GC_RG_OK);
        assert(uncommitter(&registry,&(PinSourceRequest){(uintptr_t)&leases[1],(uintptr_t)&other,(uintptr_t)&uncommit},GC_SU_BEGIN)==GC_SU_INVALID);
        assert(!memcmp(&saved_uncommit,&uncommit,sizeof(uncommit)));
        assert(gc_pin_registry_step_locked(&registry,&leases[1],&other,GC_PP_REOPEN)==GC_RG_OK);
        assert(uncommitter(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&uncommit},GC_SU_BEGIN)==GC_SU_OK);
        saved_uncommit=uncommit;uint64_t committed=heap.committed;
        assert(uncommitter(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&uncommit},GC_SU_FINISH)==GC_SU_INVALID);
        assert(!memcmp(&saved_uncommit,&uncommit,sizeof(uncommit))&&heap.committed==committed);
        RelocationGateRequest stale=selection;stale.epoch++;
        assert(uncommitter(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&stale,(uintptr_t)&uncommit},GC_SU_FINISH)==GC_SU_INVALID);
        void *source=(char *)mapping+GC_HEAP_PAGE_BYTES;
#ifdef _WIN32
        assert(VirtualFree(source,GC_HEAP_PAGE_BYTES,MEM_DECOMMIT));
#else
        assert(mmap(source,GC_HEAP_PAGE_BYTES,PROT_NONE,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,-1,0)==source);
#endif
        uncommit.work.committed_mapping=0;
        assert(uncommitter(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&uncommit},GC_SU_FINISH)==GC_SU_OK);
        assert(heap.committed==GC_HEAP_PAGE_BYTES&&pages[0].state==GC_PAGE_UNCOMMITTED);
        assert(uncommitter(&registry,&(PinSourceRequest){(uintptr_t)&leases[0],(uintptr_t)&selection,(uintptr_t)&uncommit},GC_SU_FINISH)==GC_SU_INVALID);
        assert(heap.committed==GC_HEAP_PAGE_BYTES);
        for(unsigned i=0;i<2;i++)assert(*(uint64_t *)((char *)mapping+2*GC_HEAP_PAGE_BYTES+32+48*i)==11+i);
        assert(gc_pin_registry_step_locked(&registry,&leases[0],&selection,GC_PP_REOPEN)==GC_RG_OK);
        for(unsigned i=0;i<2;i++){
            assert(gc_pin_registry_unbind_locked(&registry,&leases[i])==GC_RG_OK);
            assert(gc_metadata_step_locked(&pool,&leases[i],GC_ML_RELEASE)==GC_ML_OK);
        }
        assert(!pool.active&&ops.unmap(mapping,3*GC_HEAP_PAGE_BYTES));
    }
    puts("pin source release: pin exclusion, selection-bound cursors, OS decommit and exact budget credit PASS");
}
#endif
