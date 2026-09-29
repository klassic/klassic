/* Exclusive one-object OS lifecycle. The only logical root is moved to the
 * canonical destination before retirement; scalar payloads have no heap edges,
 * both forwarding tables are audited, and there are no other actors/jobs or
 * native handles. One weak handle follows after payload uncommit; its metadata
 * lease must leave the source before metadata retirement. This supplies the
 * leaf's caller proof for this fixture,
 * not the ordinary runtime's concurrent retirement/epoch protocol. */
#ifndef KLASSIC_GC_SOURCE_RETIRE_PIPELINE_H
#define KLASSIC_GC_SOURCE_RETIRE_PIPELINE_H
#include "gc_source_retire_cases.h"
#include "gc_page_release_pipeline.h"
#include "gc_source_uncommit.h"
#include "gc_native_source_uncommit.h"
#include "gc_native_bitmap.h"
#include "gc_forward_incoming.h"
#include "gc_weak_follow.h"
#include "gc_weak_incoming.h"
/* Inject the valid post-acquire/pre-source-release cleanup state. No token is
 * fabricated or dropped: explicit disposal must leave this source lease owned. */
static uint64_t weak_retire_defer_release(MetadataLeasePool *pool,ReferenceLease *current,MetadataLeaseRequest *retired){
    AllocationPage *page=(void *)(uintptr_t)current->lease.page;
    ForwardLookup lookup={current->reference>>16,current->lease.incarnation,0};
    assert(gc_forward_lookup((void *)(uintptr_t)page->forward_table,&lookup)==GC_FL_OK);
    ForwardRequest read={lookup.source_offset,lookup.incarnation,0,0};
    assert(gc_forward_step((void *)(uintptr_t)lookup.cell,&read,GC_FWD_READ)==GC_FWD_OK);
    ReferenceLease destination={.reference=read.destination};
    assert(gc_reference_lease_locked(pool,&destination)==GC_ML_OK);
    *retired=current->lease;*current=destination;return GC_RF_CLEANUP_REQUIRED;
}
static bool gc_source_bitmap_unmap_failure(void *address,size_t bytes){(void)address;(void)bytes;return false;}
static unsigned gc_source_payload_os_calls;
static bool gc_source_payload_partial_failure(void *address,size_t bytes){
    gc_source_payload_os_calls++;
#ifndef _WIN32
    assert(mprotect(address,bytes,PROT_NONE)==0);
#else
    (void)address;(void)bytes;
#endif
    return false;
}
static void gc_run_source_retire_pipeline(SourceRetirer retire,PoolStep pool,BudgetStep budget,SourceUncommitter uncommit,WeakTableFollower follow_slot,WeakIncomingStepper scan_step,WeakTableStepper table_step,SourcePayloadUncommitter payload,NativePageDecommitter decommit){
    for(unsigned round=0;round<16;round++){
        AllocationPage pages[4]={0};MarkRecord records[2]={0};MarkArena arena={(uintptr_t)records,2,0};
        ForwardCell target_cell={0},replacement_cell={0};
        ForwardTable target_table={(uintptr_t)&target_cell,1,0},replacement_table={(uintptr_t)&replacement_cell,1,0};
        MetadataLeaseSlot slot[2]={{.next_free=2},{0}};size_t bytes=5*GC_HEAP_PAGE_BYTES;void *mapping=pool_vm_reserve(bytes);
        uint64_t baseline=sizeof(pages)+sizeof(records)+sizeof(slot)+sizeof(target_cell)+sizeof(replacement_cell)+sizeof(target_table)+sizeof(replacement_table);
        NativeMetadataOps native=gc_native_metadata_ops();NativeBitmapOwner bitmap={0};
        HeapPool heap={.heap_base=(uintptr_t)mapping,.pages=(uintptr_t)pages,.page_count=4,
            .hard_max=4*GC_HEAP_PAGE_BYTES,.soft_max=2*GC_HEAP_PAGE_BYTES,.metadata_committed=baseline,.metadata_max=baseline+8192+4*native.quantum};
        MetadataLeasePool leases={(uintptr_t)&heap,(uintptr_t)slot,2,1,0};
        PageLease source={.page_count=1+round%2},destination={.page_count=1,.generation=round%2,.relocation=1};
        assert(pool(&heap,&source,GC_POOL_RESERVE)==GC_POOL_OK&&source.head_index==0);
        assert(gc_native_bitmap_reserve_locked(&bitmap,&heap,&source,&native)==GC_BUDGET_OK);
        assert(gc_native_bitmap_prepare(&bitmap,&native)==GC_NATIVE_METADATA_OK);
        pool_vm_commit((char *)mapping+pages[0].start,source.page_count*GC_HEAP_PAGE_BYTES);
        source.committed_mapping=1;assert(gc_native_bitmap_publish_with_locked(&bitmap,pool)==GC_POOL_OK);
        MetadataCharge charge={.bytes=sizeof(ForwardTable)+sizeof(ForwardCell)};
        assert(budget(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        ForwardTable *table=calloc(1,charge.bytes);assert(table);table->cells=(uintptr_t)(table+1);table->capacity=1;
        charge.allocation=(uintptr_t)table;assert(budget(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);pages[0].forward_table=(uintptr_t)table;
        assert(pool(&heap,&destination,GC_POOL_RESERVE)==GC_POOL_OK);AllocationPage *target=&pages[destination.head_index];
        void *target_bitmap=calloc(1,1024);assert(target_bitmap);pool_vm_commit((char *)mapping+target->start,GC_HEAP_PAGE_BYTES);
        destination.starts=(uintptr_t)target_bitmap;destination.committed_mapping=1;assert(pool(&heap,&destination,GC_POOL_COMMIT)==GC_POOL_OK);
        target->forward_table=(uintptr_t)&target_table;
        MutatorContext ctx={.abi_version=GC_ABI_VERSION,.thread_id=17};TypeDescriptor desc={.id=1,.alignment=16,.fixed_bytes=17};
        AllocationResult value={0};AllocationRequest alloc={(uintptr_t)&pages[0],(uintptr_t)&desc,0,(uintptr_t)&value,(uintptr_t)&arena};
        assert(gc_tlab_page_locked(&ctx,&pages[0],0)==GC_ALLOC_OK&&gc_try_tlab_allocate(&ctx,&alloc)==GC_ALLOC_OK);
        uint64_t source_offset=value.reference>>16,identity=value.object_id;
        ObjectHeader *header=(ObjectHeader *)((char *)mapping+source_offset-32);memset(header+1,0x5a,17);atomic_store(&header->state,GC_OBJECT_PUBLISHED);header=NULL;
        assert(gc_tlab_page_locked(&ctx,&pages[0],1)==GC_ALLOC_OK);
        ReferenceLease held={.reference=value.reference};assert(gc_reference_lease_locked(&leases,&held)==GC_ML_OK);
        WeakLoadGate weak_gate={.pool=(uintptr_t)&leases};
        WeakEntry weak_entry={.slot={(uintptr_t)&weak_gate,1,held.reference,held.lease}};
        WeakTable weak_table={&weak_gate,&weak_entry,1,0,1,1};
        WeakHandle weak_handle={(uintptr_t)&weak_table,1,1};
        held=(ReferenceLease){0}; /* ownership transferred into the weak slot */
        ForwardCell *cell=(ForwardCell *)(uintptr_t)table->cells;
        ForwardRequest claim={.source_offset=source_offset,.incarnation=source.incarnation,.ticket=1};
        assert(gc_forward_step(cell,&claim,GC_FWD_CLAIM)==GC_FWD_OK);
        SourceAccessRequest selected={.source_offset=source_offset,.incarnation=source.incarnation,.ticket=1};
        assert(gc_source_access_step(cell,&selected,GC_SA_SELECT)==GC_SA_OK&&gc_source_access_step(cell,&selected,GC_SA_CHECK)==GC_SA_OK);
        RelocationCopy copy={(uintptr_t)mapping,source.incarnation,1,pages[0].limit,0,0,7,0,0};
        assert(gc_relocate_target_locked(target,&copy,GC_TARGET_PREPARE)==GC_TARGET_OK);
        uint64_t status;do{status=gc_relocate_copy(cell,&copy);assert(status==GC_COPY_MORE||status==GC_COPY_DONE);}while(status==GC_COPY_MORE);
        assert(gc_relocate_target_locked(target,&copy,GC_TARGET_INSTALL)==GC_TARGET_OK);
        assert(gc_relocate_publish(cell,&copy,0x1510)==GC_COPY_DONE);
        uint64_t live=(copy.destination<<16)|0x1510;value.reference=held.reference=0; // abandon both source references
        SourceDrain drain={source.incarnation,atomic_load(&pages[0].allocated_top),1,0,1,0};
        assert(gc_source_drain(&pages[0],&drain)==GC_SD_DONE);
        SourceUncommit cleanup={.page_count=source.page_count,.incarnation=source.incarnation};
        assert(uncommit(&heap,&cleanup,GC_SU_BEGIN)==GC_SU_OK);
        SourceUncommit before=cleanup,copied=cleanup;
        uint64_t committed=heap.committed,metadata=heap.metadata_committed;
        gc_source_payload_os_calls=0;
        assert(payload(&heap,&copied,gc_source_payload_partial_failure)==GC_NATIVE_METADATA_INVALID);
        for(unsigned fault=0;fault<7;fault++){
            cleanup=before;
            if(fault==0)cleanup.incarnation++;
            else if(fault==1)cleanup.owner=0;
            else if(fault==2)cleanup.stage=2;
            else if(fault==3)cleanup.committed_mapping=2;
            else if(fault==4)cleanup.page_count=0;
            else if(fault==5)cleanup.head_index=heap.page_count;
            else cleanup.page_count=heap.page_count+1;
            assert(payload(&heap,&cleanup,gc_source_payload_partial_failure)==GC_NATIVE_METADATA_INVALID);
        }
        cleanup=before;assert(!gc_source_payload_os_calls);
        if(source.page_count>1){
            pages[1].incarnation++;
            assert(payload(&heap,&cleanup,gc_source_payload_partial_failure)==GC_NATIVE_METADATA_INVALID);
            pages[1].incarnation--;assert(!gc_source_payload_os_calls);
        }
        assert(pages[0].metadata_leases==1); // weak handle still owns metadata
        assert(payload(&heap,&cleanup,gc_source_payload_partial_failure)==GC_NATIVE_METADATA_OS_ERROR);
        assert(gc_source_payload_os_calls==1&&!memcmp(&cleanup,&before,sizeof(cleanup)));
        assert(heap.committed==committed&&heap.metadata_committed==metadata);
        assert(uncommit(&heap,&cleanup,GC_SU_FINISH)==GC_SU_INVALID);
        assert(payload(&heap,&cleanup,decommit)==GC_NATIVE_METADATA_OK);
        assert(heap.committed==committed&&pages[0].metadata_leases==1);
        assert(payload(&heap,&cleanup,decommit)==GC_NATIVE_METADATA_INVALID);
        assert(uncommit(&heap,&cleanup,GC_SU_FINISH)==GC_SU_OK&&heap.committed==GC_HEAP_PAGE_BYTES);
        for(unsigned i=0;i<2;i++){
            ForwardTable *t=i?&target_table:table;AllocationPage *p=i?target:&pages[0];
            ForwardIncoming incoming={p->incarnation,atomic_load(&t->used),pages[0].start,pages[0].limit,0,1,0,0};
            assert(gc_forward_incoming(t,&incoming)==GC_FI_DONE);
        }
        weak_gate.closed=1;WeakIncoming weak_incoming={.target_start=pages[0].start,.target_limit=pages[0].limit};
        assert(scan_step(&weak_table,&weak_incoming,0)==GC_WI_MORE);
        assert(scan_step(&weak_table,&weak_incoming,1)==GC_WI_FOUND);
        assert(weak_incoming.handle.serial==weak_handle.serial&&weak_incoming.observed==weak_entry.slot.reference);
        SourceRetire request={.page_count=source.page_count,.incarnation=source.incarnation};
        assert(retire(&heap,&request,GC_SR_BEGIN)==GC_SR_WAIT);
        WeakFollowRequest weak_follow={.index=weak_handle.index,.serial=weak_handle.serial};
        if(round&1){
            ReferenceLease pressure={.reference=live};assert(gc_reference_lease_locked(&leases,&pressure)==GC_ML_OK);
            WeakSlot before=weak_entry.slot;
            assert(follow_slot(&weak_table,&weak_follow,0)==GC_RF_METADATA_FULL);
            assert(!memcmp(&before,&weak_entry.slot,sizeof(before))&&!weak_follow.retired.token);
            assert(gc_metadata_step_locked(&leases,&pressure.lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        bool weak_released=round%4==2;
        if(weak_released){
            assert(gc_weak_table_follow_locked(&weak_table,weak_handle,&weak_follow.retired,weak_retire_defer_release)==GC_RF_CLEANUP_REQUIRED);
            assert(weak_entry.slot.reference==live&&weak_follow.retired.token&&pages[0].metadata_leases==1&&target->metadata_leases==1);
            assert(follow_slot(&weak_table,&weak_follow,0)==GC_RF_CLEANUP_REQUIRED);
            assert(table_step(&weak_table,&(WeakTableRequest){.handle=weak_handle},GC_WT_RELEASE)==GC_WEAK_RELEASED);
            assert(!target->metadata_leases&&pages[0].metadata_leases==1);
            assert(scan_step(&weak_table,&weak_incoming,1)==GC_WI_MORE);
            assert(scan_step(&weak_table,&weak_incoming,1)==GC_WI_DONE);
            /* Slot absence does not dispose an independently pending lease. */
            assert(retire(&heap,&request,GC_SR_BEGIN)==GC_SR_WAIT);
            assert(gc_metadata_step_locked(&leases,&weak_follow.retired,GC_ML_RELEASE)==GC_ML_OK);
            assert(follow_slot(&weak_table,&weak_follow,0)==GC_RF_INVALID);
        }else{
            assert(follow_slot(&weak_table,&weak_follow,0)==GC_RF_ADVANCED);
            assert(weak_entry.slot.reference==live&&!weak_follow.retired.token&&!pages[0].metadata_leases&&target->metadata_leases==1);
            assert(follow_slot(&weak_table,&weak_follow,0)==GC_RF_DONE);
        }
        if(!weak_released)assert(scan_step(&weak_table,&weak_incoming,1)==GC_WI_MORE);
        assert(scan_step(&weak_table,&weak_incoming,1)==GC_WI_DONE);
        assert(retire(&heap,&request,GC_SR_BEGIN)==GC_SR_MORE);
        MetadataLeaseRequest denied={(uintptr_t)&pages[0],source.incarnation,0};assert(gc_metadata_step_locked(&leases,&denied,GC_ML_ACQUIRE)==GC_ML_INVALID);
        assert(retire(&heap,&request,GC_SR_STEP)==GC_SR_WAIT);
        SourceRetire copied_retire=request;
        assert(gc_native_bitmap_retire(&bitmap,&copied_retire,&native)==GC_NATIVE_METADATA_INVALID);
        uint64_t bitmap_mapping=bitmap.mapping,bitmap_committed=heap.metadata_committed;
        NativeMetadataOps fail_unmap=native;fail_unmap.unmap=gc_source_bitmap_unmap_failure;
        assert(gc_native_bitmap_retire(&bitmap,&request,&fail_unmap)==GC_NATIVE_METADATA_OS_ERROR);
        assert(bitmap.mapping==bitmap_mapping&&request.starts==bitmap_mapping&&heap.metadata_committed==bitmap_committed);
        assert(gc_native_bitmap_release_locked(&bitmap)==GC_BUDGET_INVALID);
        assert(retire(&heap,&request,GC_SR_STEP)==GC_SR_WAIT);
        assert(gc_native_bitmap_retire(&bitmap,&request,&native)==GC_NATIVE_METADATA_OK);
        assert(gc_native_bitmap_release_locked(&bitmap)==GC_BUDGET_OK);
        assert(gc_native_bitmap_retire(&bitmap,&request,&native)==GC_NATIVE_METADATA_INVALID);
        assert(retire(&heap,&request,GC_SR_STEP)==GC_SR_WAIT);
        free((void *)(uintptr_t)request.forward_table);request.forward_table=0;table=NULL;cell=NULL;
        charge.allocation=0;assert(budget(&heap,&charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        do{status=retire(&heap,&request,GC_SR_STEP);assert(status==GC_SR_MORE||status==GC_SR_DONE);}while(status==GC_SR_MORE);
        assert(heap.committed==GC_HEAP_PAGE_BYTES&&heap.metadata_committed==baseline+1024);
        assert(retire(&heap,&request,GC_SR_STEP)==GC_SR_INVALID);
        PageLease reused={.page_count=source.page_count};assert(pool(&heap,&reused,GC_POOL_RESERVE)==GC_POOL_OK);
        assert(reused.head_index==0&&reused.incarnation>source.incarnation);
        void *new_bitmap=calloc(1,reused.page_count*1024);assert(new_bitmap);
        pool_vm_commit((char *)mapping+pages[0].start,reused.page_count*GC_HEAP_PAGE_BYTES);
        reused.starts=(uintptr_t)new_bitmap;reused.committed_mapping=1;assert(pool(&heap,&reused,GC_POOL_COMMIT)==GC_POOL_OK);
        pages[0].forward_table=(uintptr_t)&replacement_table;
        assert(gc_tlab_page_locked(&ctx,&pages[0],0)==GC_ALLOC_OK&&gc_try_tlab_allocate(&ctx,&alloc)==GC_ALLOC_OK);
        assert((value.reference>>16)==source_offset&&value.object_id!=identity&&arena.used==2);
        header=(ObjectHeader *)((char *)mapping+source_offset-32);memset(header+1,0xa5,17);atomic_store(&header->state,GC_OBJECT_PUBLISHED);header=NULL;
        assert(gc_tlab_page_locked(&ctx,&pages[0],1)==GC_ALLOC_OK);
        ResolveRequest resolved={.reference=live,.scope={.context=(uintptr_t)&ctx}};
        assert(gc_resolve_access(target,&resolved,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        header=(ObjectHeader *)((char *)mapping+(live>>16)-32);
        assert(header->mark_record==(uintptr_t)&records[0]&&records[0].object_id==identity);
        for(unsigned i=0;i<17;i++)assert(((unsigned char *)(header+1))[i]==0x5a);
        assert(gc_resolve_access(target,&resolved,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        if(!weak_released){
            assert(weak_entry.slot.reference==live&&weak_entry.slot.lease.page==(uintptr_t)target);
            assert(table_step(&weak_table,&(WeakTableRequest){.handle=weak_handle},GC_WT_RELEASE)==GC_WEAK_RELEASED);
        }
        assert(!leases.active&&!target->metadata_leases);
        assert(follow_slot(&weak_table,&weak_follow,0)==GC_RF_INVALID);
        free(new_bitmap);free(target_bitmap);pool_vm_release(mapping,bytes);
    }
    puts("source retire OS: 16 copied objects and weak handles survive payload uncommit, metadata retirement and same-range recommit PASS");
    puts("native source payload: request-bound OS retry preserves weak metadata and defers budget credit PASS");
    puts("native source bitmap: actual mapped retirement, retained failed-unmap ownership and rounded budget return PASS");
}
#endif
