/* Retained remembered claim -> guarded root -> Young mark queue. Explicit
 * fixture-owned Running context and native metadata survive until teardown.
 * Relocations are adversarial barrier schedules, not a full cycle scheduler,
 * promotion policy, dead-owner retirement or virtual-address reuse proof. */
#ifndef KLASSIC_GC_REMEMBERED_PIPELINE_H
#define KLASSIC_GC_REMEMBERED_PIPELINE_H
#include "gc_remembered_load_cases.h"
#include "gc_promotion_remember.h"
#include "gc_mark_move_pipeline.h"
#include "gc_mark_route_cases.h"
#include "gc_remembered_transfer_cases.h"
#include "gc_remembered_owner_cases.h"
#include "gc_remembered_lease_cases.h"
static RelocationCopy remembered_install(const MarkMoveOps *o,void *mapping,AllocationPage *source,ForwardCell *cell,AllocationPage *destination,int claimed){
    ForwardRequest claim={cell->source_offset,cell->incarnation,1,0};
    if(!claimed)assert(o->forward(cell,&claim,GC_FWD_CLAIM)==GC_FWD_OK);
    SourceAccessRequest select={0,cell->source_offset,cell->incarnation,0,1};
    assert(o->access(cell,&select,GC_SA_SELECT)==GC_SA_OK);
    assert(o->access(cell,&select,GC_SA_CHECK)==GC_SA_OK);
    RelocationCopy copy={(uintptr_t)mapping,cell->incarnation,1,source->limit,0,0,3,0,0};
    assert(o->target(destination,&copy,GC_TARGET_PREPARE)==GC_TARGET_OK);
    uint64_t status;do{status=o->copy(cell,&copy);}while(status==GC_COPY_MORE);
    assert(status==GC_COPY_DONE);
    assert(o->target(destination,&copy,GC_TARGET_INSTALL)==GC_TARGET_OK);
    return copy;
}
static void remembered_move(const MarkMoveOps *o,void *mapping,AllocationPage *source,ForwardCell *cell,AllocationPage *destination,int claimed){
    RelocationCopy copy=remembered_install(o,mapping,source,cell,destination,claimed);
    assert(o->publish(cell,&copy,0x1510)==GC_COPY_DONE);
    pool_vm_decommit((char *)mapping+source->start,GC_HEAP_PAGE_BYTES);
}
static void remembered_skip_finish(const MarkMoveOps *o,RememberedInventoryStep inventory,RememberedInventory *g,RememberedInventoryRequest *q,
        RememberedSet *set,MarkQueue *queue,MetadataLeasePool *leases,uint64_t keeper,ChainRoots *roots,RootRequest *output,unsigned round){
    assert(!atomic_load(&roots->ctx.access_epoch)&&leases->active==1);
    assert(inventory(g,q,GC_RI_CHECK)==GC_RS_WAIT&&g->claims==1);
    assert(o->mark(queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
    RememberedJob *claim=(RememberedJob *)(uintptr_t)q->job;
    if(round%2){
        // Model an interposed current record after classification. A no-Young
        // result must not clear a record produced while this claim was held.
        RememberedJob record={.offset=claim->offset,.incarnation=claim->incarnation,.phase=0x31102};
        RememberedInventoryRequest update=*q;update.job=(uintptr_t)&record;
        assert(inventory(g,&update,GC_RI_RECORD)==GC_RS_OK&&set->current_count==1);
    }
    claim->keep=0;assert(inventory(g,q,GC_RI_ACK)==GC_RS_OK);
    assert(inventory(g,q,GC_RI_CHECK)==GC_RS_EMPTY&&set->current_count==round%2);
    move_release(o,leases,keeper);assert(!leases->active);
    assert(o->root(&roots->ctx,output,GC_ROOT_RELEASE)==GC_ROOT_OK);
    assert(o->root(&roots->ctx,&roots->request,GC_ROOT_RELEASE)==GC_ROOT_OK);
}
/* A second prepared fixture epoch revisits the carried slot. This exercises
 * inventory retention and actual rooted marking, not global cycle retirement. */
static void remembered_revisit(const MarkMoveOps *o,RememberedInventoryStep inventory,RememberedLoader load,MarkRouter route,RememberedOwnerFinder find,
        RememberedInventory *g,RememberedInventoryRequest *q,RememberedRegistration *source,RememberedRegistration *destination,
        ChainRoots *roots,RootRequest *output,MetadataLeasePool *leases,DescriptorRegistry *registry,MarkQueue *queue){
    uint64_t start=((atomic_load(&roots->ctx.adopted_phase)+0x10000)^0x1100)&~UINT64_C(15);start|=GC_MARK_START;
    q->phase=start;assert(inventory(g,q,GC_RI_BEGIN)==GC_RS_OK&&g->unflipped==2);
    RememberedRegistration *nodes[]={source,destination};
    for(unsigned i=0;i<2;i++){
        RememberedSet *s=(RememberedSet *)(uintptr_t)nodes[i]->set;
        RememberedJob flip={.incarnation=s->incarnation,.phase=start,.budget=256};RememberedInventoryRequest request={(uintptr_t)nodes[i],(uintptr_t)&flip,0};
        assert(inventory(g,&request,GC_RI_FLIP)==GC_RS_OK);
    }
    assert(g->pending==1&&!g->claims&&inventory(g,q,GC_RI_CHECK)==GC_RS_WAIT);
    RememberedJob *claim=(RememberedJob *)(uintptr_t)q->job;*claim=(RememberedJob){.budget=256};
    assert(inventory(g,q,GC_RI_TAKE)==GC_RS_OK&&g->claims==1);
    uint64_t phase=(start+0x10000+1);atomic_store(&roots->ctx.adopted_phase,phase);queue->epoch++;
    roots->request.value=0;assert(o->root(&roots->ctx,&roots->request,GC_ROOT_READ_RAW)==GC_ROOT_OK);
    assert(claim->offset==(roots->request.value>>16)+8);
    ReferenceLease owner={.reference=roots->request.value};assert(o->acquire(leases,&owner)==GC_ML_OK);
    AllocationPage *page=(AllocationPage *)(uintptr_t)owner.lease.page;
    RememberedOwner located={.registration=q->registration,.claim=q->job,.page=(uintptr_t)page};
    assert(find(&located)==GC_RO_OK&&located.owner==(roots->request.value>>16)&&located.byte_offset==8);
    ResolveRequest guard={.reference=roots->request.value,.scope={.context=(uintptr_t)&roots->ctx}};
    assert(o->resolve(page,&guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
    RootRequest reserved=*output;reserved.value=0;
    RememberedLoad read={.registration=q->registration,.claim=q->job,.source_owner=roots->request.value,
        .load={.scope=(uintptr_t)&guard.scope,.page=(uintptr_t)page,.registry=(uintptr_t)registry,.byte_offset=located.byte_offset,.phase=phase,.output=reserved}};
    assert(load(&read)==GC_ROOT_OK);
    assert(o->resolve(page,&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);move_release(o,leases,owner.lease.token);
    // The heap slot still contains the old Young source reference. Resolve it
    // again from the output root; the old physical mapping is protected.
    RootFollow follow={.context=(uintptr_t)&roots->ctx,.root=reserved};
    assert(o->follow(leases,&follow,GC_RF_STEP)==GC_RF_ADVANCED);assert(o->follow(leases,&follow,GC_RF_STEP)==GC_RF_DONE);
    output->value=0;assert(o->root(&roots->ctx,output,GC_ROOT_READ_RAW)==GC_ROOT_OK);
    ReferenceLease child={.reference=output->value};assert(o->acquire(leases,&child)==GC_ML_OK);
    page=(AllocationPage *)(uintptr_t)child.lease.page;
    guard=(ResolveRequest){.reference=output->value,.scope={.context=(uintptr_t)&roots->ctx}};
    assert(o->resolve(page,&guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
    GuardedMarkRoute offer={.generation=GC_YOUNG,.prepare={.scope=(uintptr_t)&guard.scope,.page=(uintptr_t)page,.phase=phase,
        .reference=output->value,.lease=child.lease}};child.lease=(MetadataLeaseRequest){0};
    assert(route(&offer)==GC_ROUTE_READY);assert(o->resolve(page,&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
    assert(inventory(g,q,GC_RI_CHECK)==GC_RS_WAIT&&o->mark(queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
    assert(o->mark(queue,&offer.prepare.output,GC_MARK_OFFER)==GC_MARK_OK);
    claim->keep=1;assert(inventory(g,q,GC_RI_ACK)==GC_RS_OK&&inventory(g,q,GC_RI_CHECK)==GC_RS_EMPTY);
    assert(o->mark(queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);
    MarkRequest work={0};assert(o->mark(queue,&work,GC_MARK_TAKE)==GC_MARK_OK);
    uint64_t out=0,next=0,written=0,base=((HeapPool *)(uintptr_t)leases->heap)->heap_base;
    assert(move_scan(o,leases,&roots->ctx,registry,&work,base,&out,1,&next,&written)==GC_SCAN_DONE&&!written);
    work.cursor=next;uint64_t token=work.metadata_lease;assert(o->mark(queue,&work,GC_MARK_FINISH)==GC_MARK_OK);move_release(o,leases,token);
    assert(!leases->active&&o->mark(queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
}
static void gc_run_remembered_pipeline(const MarkMoveOps *o,RememberedInventoryStep inventory,RememberedLoader load,MarkRouter route,RememberedTransferStep transfer,RememberedOwnerFinder find,RememberedAcquirer acquire,ResolveRootDrain drain,PromotionRememberStep promote){
    for(unsigned round=0;round<64;round++)for(unsigned variant=0;variant<3;variant++){
        void *mapping=pool_vm_reserve(6*GC_HEAP_PAGE_BYTES);
        AllocationPage pages[5]={0};ForwardCell cells[5]={0};ForwardTable tables[5]={0};
        _Atomic uint64_t starts[5][128]={0};MarkRecord records[2]={0};MarkArena arena={(uintptr_t)records,2,0};
        uint64_t bits[2][256]={0},destination_bits[2][256]={0};RememberedSet set={0},destination_set={0};RememberedRegistration registration={0},destination_registration={0};
        RememberedSet *active_set=&set;
        RememberedInventory rem={.epoch=0x10000,.last_phase=0x10000};RememberedJob claim={0};
        uint64_t native=sizeof(pages)+sizeof(tables)+sizeof(cells)+sizeof(records)+sizeof(bits)+sizeof(destination_bits)+sizeof(set)+sizeof(destination_set)+sizeof(registration)+sizeof(destination_registration)+sizeof(rem);
        HeapPool heap={.heap_base=(uintptr_t)mapping,.pages=(uintptr_t)pages,.page_count=5,.hard_max=5*GC_HEAP_PAGE_BYTES,
            .soft_max=2*GC_HEAP_PAGE_BYTES,.relocation_reserve=3*GC_HEAP_PAGE_BYTES,.metadata_committed=native,.metadata_max=native+sizeof(starts)};
        for(unsigned i=0;i<5;i++){
            tables[i]=(ForwardTable){(uintptr_t)&cells[i],1,0};
            PageLease page={.page_count=1,.relocation=i>=2,.generation=(i==2||i==4||(i==3&&variant==1))?GC_OLD:GC_YOUNG};
            assert(o->pool(&heap,&page,GC_POOL_RESERVE)==GC_POOL_OK&&page.head_index==i);
            pool_vm_commit((char *)mapping+pages[i].start,GC_HEAP_PAGE_BYTES);page.starts=(uintptr_t)starts[i];page.committed_mapping=1;
            assert(o->pool(&heap,&page,GC_POOL_COMMIT)==GC_POOL_OK);pages[i].forward_table=(uintptr_t)&tables[i];
        }
        MetadataLeaseSlot slots[8]={0};for(unsigned i=0;i<7;i++)slots[i].next_free=i+2;
        MetadataLeasePool leases={(uintptr_t)&heap,(uintptr_t)slots,8,1,0};
        ChainRoots roots;chain_roots_init(&roots);atomic_store(&roots.ctx.adopted_phase,0x10000);
        assert(o->root(&roots.ctx,&roots.request,GC_ROOT_RESERVE)==GC_ROOT_OK);
        RootRequest output={.segment=(uintptr_t)&roots.segment};assert(o->root(&roots.ctx,&output,GC_ROOT_RESERVE)==GC_ROOT_OK);
        uint64_t bitmap=2;TypeDescriptor parent={.id=1,.alignment=16,.fixed_bytes=24,.fixed_bitmap=(uintptr_t)&bitmap,.fixed_bitmap_words=1};
        TypeDescriptor child={.id=2,.alignment=16,.fixed_bytes=7};_Atomic uint64_t descriptors[3]={0};DescriptorRegistry registry={(uintptr_t)descriptors,3};
        assert(o->descriptor(&registry,&parent)==GC_DESC_OK&&o->descriptor(&registry,&child)==GC_DESC_OK);
        AllocationResult objects[2];
        for(unsigned i=0;i<2;i++){
            assert(o->bind(&roots.ctx,&pages[i],0)==GC_ALLOC_OK);
            AllocationRequest allocation={(uintptr_t)&pages[i],(uintptr_t)(i?&child:&parent),0,(uintptr_t)&objects[i],(uintptr_t)&arena};
            assert(o->allocate(&roots.ctx,&allocation)==GC_ALLOC_OK);assert(o->bind(&roots.ctx,&pages[i],1)==GC_ALLOC_OK);
        }
        ObjectHeader *h=(ObjectHeader *)((char *)mapping+(objects[1].reference>>16)-32);memcpy(h+1,"payload",7);atomic_store(&h->state,GC_OBJECT_PUBLISHED);
        h=(ObjectHeader *)((char *)mapping+(objects[0].reference>>16)-32);atomic_store((_Atomic uint64_t *)(h+1)+1,variant==2?0:objects[1].reference);atomic_store(&h->state,GC_OBJECT_PUBLISHED);
        roots.request.value=objects[0].reference;assert(o->root(&roots.ctx,&roots.request,GC_ROOT_STORE)==GC_ROOT_OK);
        // This fixture proves one Young survival and uses a fixed threshold.
        // Install privately, then register every reference slot (including null)
        // before forwarding publication can expose the promoted Old owner.
        assert(o->age(&records[0],round+1,1)==GC_AGE_PROMOTE);
        RelocationCopy promotion=remembered_install(o,mapping,&pages[0],&cells[0],&pages[2],0);
        assert(pages[2].generation==GC_OLD&&records[0].age==1&&records[0].age_epoch==round+1);
        set=(RememberedSet){.start=pages[2].start,.limit=pages[2].limit,.incarnation=pages[2].incarnation,
            .bits={(uintptr_t)bits[0],(uintptr_t)bits[1]},.words=256,.epoch=rem.epoch,.last_phase=rem.epoch};
        registration.set=(uintptr_t)&set;
        RememberedInventoryRequest rq={(uintptr_t)&registration,(uintptr_t)&claim,0x21101};
        assert(inventory(&rem,&rq,GC_RI_ATTACH)==GC_RS_OK);
        destination_set=(RememberedSet){.start=pages[4].start,.limit=pages[4].limit,.incarnation=pages[4].incarnation,
            .bits={(uintptr_t)destination_bits[0],(uintptr_t)destination_bits[1]},.words=256,.epoch=rem.epoch,.last_phase=rem.epoch};
        destination_registration.set=(uintptr_t)&destination_set;
        RememberedJob destination_job={.incarnation=destination_set.incarnation,.budget=256};
        RememberedInventoryRequest destination_request={(uintptr_t)&destination_registration,(uintptr_t)&destination_job,0};
        assert(inventory(&rem,&destination_request,GC_RI_ATTACH)==GC_RS_OK);
        claim=(RememberedJob){.offset=promotion.destination+8,.incarnation=set.incarnation,.phase=rem.epoch,.budget=256};
        PromotionRemember handoff={(uintptr_t)&registry,(uintptr_t)&registration,promotion.destination,parent.id,parent.fixed_bytes,rem.epoch,0};
        unsigned slices=0;uint64_t promotion_status;
        do{promotion_status=promote(&rem,&handoff);slices++;}while(promotion_status==GC_PR_MORE);
        assert(promotion_status==GC_PR_DONE&&slices==3&&handoff.cursor==parent.fixed_bytes);
        assert(set.current_count==1&&atomic_load(&cells[0].word)==((promotion.ticket<<2)|1));
        assert(o->publish(&cells[0],&promotion,0x1510)==GC_COPY_DONE);
        pool_vm_decommit((char *)mapping+pages[0].start,GC_HEAP_PAGE_BYTES);
        RootFollow owner={.context=(uintptr_t)&roots.ctx,.root=roots.request};owner.root.value=0;
        assert(o->follow(&leases,&owner,GC_RF_STEP)==GC_RF_ADVANCED);assert(o->follow(&leases,&owner,GC_RF_STEP)==GC_RF_DONE);
        roots.request.value=0;assert(o->root(&roots.ctx,&roots.request,GC_ROOT_READ_RAW)==GC_ROOT_OK);
        uint64_t source_owner=roots.request.value;
        ObjectHeader *promoted=(ObjectHeader *)((char *)mapping+promotion.destination-32);
        assert(promoted->mark_record==(uintptr_t)&records[0]&&records[0].age==1&&records[0].age_epoch==round+1);
        assert(inventory(&rem,&rq,GC_RI_BEGIN)==GC_RS_OK);claim.phase=rem.epoch;
        assert(inventory(&rem,&rq,GC_RI_FLIP)==GC_RS_OK);destination_job.phase=rem.epoch;
        assert(inventory(&rem,&destination_request,GC_RI_FLIP)==GC_RS_OK);
        assert(inventory(&rem,&rq,GC_RI_TAKE)==GC_RS_OK);
        atomic_store(&roots.ctx.adopted_phase,0x31102);
        ReferenceLease keeper={.reference=source_owner};assert(o->acquire(&leases,&keeper)==GC_ML_OK);
        RememberedOwner located={.registration=rq.registration,.claim=(uintptr_t)&claim,.page=(uintptr_t)&pages[2]};
        assert(find(&located)==GC_RO_OK&&located.owner==(source_owner>>16)&&located.byte_offset==8&&located.cell==(uintptr_t)&cells[2]);
        // Reuse the color of this already-held owner; the lookup itself does
        // not manufacture a GcRef or establish strong object liveness.
        source_owner=(located.owner<<16)|(source_owner&0xffff);

        MarkTask task={0};MarkQueue queue={.tasks=(uintptr_t)&task,.capacity=1,.free_head=1,.epoch=round+1,.generation=GC_YOUNG};
        assert(inventory(&rem,&rq,GC_RI_CHECK)==GC_RS_WAIT&&rem.claims==1);
        assert(o->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY&&!records[1].young_epoch);
        if(round%2==0)while(roots.buffer.top<7)assert(o->root(&roots.ctx,&roots.request,GC_ROOT_STORE)==GC_ROOT_OK);
        ReferenceLease receiver={.reference=source_owner};assert(o->acquire(&leases,&receiver)==GC_ML_OK);
        AllocationPage *page=(AllocationPage *)(uintptr_t)receiver.lease.page;
        ResolveRequest guard={.reference=source_owner,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(o->resolve(page,&guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        RememberedLoad request={.registration=(uintptr_t)&registration,.claim=(uintptr_t)&claim,.source_owner=source_owner,
            .load={.scope=(uintptr_t)&guard.scope,.page=(uintptr_t)page,.registry=(uintptr_t)&registry,.byte_offset=located.byte_offset,.phase=0x31102,.output=output}};
        if(round%2==0){
            assert(load(&request)==GC_ROOT_FLUSH&&!atomic_load(&roots.cells[1].value)&&rem.claims==1);
            assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_ACTIVE_ACCESS);
            assert(o->resolve(page,&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);move_release(o,&leases,receiver.lease.token);
            assert(drain(&roots.drain)==GC_ROOT_OK&&roots.drain.written==7);
            // All pre-mark journal entries describe the fixture's parent;
            // none is a Young referent requiring a Young mark handoff.
            for(unsigned i=0;i<7;i++){
                uint64_t a=roots.changes[i].previous,b=roots.changes[i].next;
                assert(!a||a==objects[0].reference||a==source_owner);assert(!b||b==objects[0].reference||b==source_owner);
            }
            assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
            remembered_move(o,mapping,&pages[2],&cells[2],&pages[4],0);
            RememberedLease retained={.registration=rq.registration,.claim=rq.job};
            assert(acquire(&leases,&retained)==GC_ML_OK&&retained.lease.page==(uintptr_t)&pages[2]);
            RememberedOwner protected_source={.registration=rq.registration,.claim=(uintptr_t)&claim,.page=retained.lease.page};
            assert(find(&protected_source)==GC_RO_OK&&protected_source.owner==located.owner&&protected_source.byte_offset==8);
            assert(o->metadata(&leases,&retained.lease,GC_ML_RELEASE)==GC_ML_OK);

            assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
            owner.root=roots.request;owner.root.value=0;assert(o->follow(&leases,&owner,GC_RF_STEP)==GC_RF_ADVANCED);assert(o->follow(&leases,&owner,GC_RF_STEP)==GC_RF_DONE);
            roots.request.value=0;assert(o->root(&roots.ctx,&roots.request,GC_ROOT_READ_RAW)==GC_ROOT_OK);
            ReferenceLease next_keeper={.reference=roots.request.value};assert(o->acquire(&leases,&next_keeper)==GC_ML_OK);
            RememberedTransfer moved={.source=(uintptr_t)&registration,.claim=(uintptr_t)&claim,.destination=(uintptr_t)&destination_registration,
                .offset=(roots.request.value>>16)+protected_source.byte_offset,.incarnation=pages[4].incarnation};
            assert(transfer(&rem,&moved)==GC_RS_OK);
            assert(!set.current_count&&!set.pending&&!set.active_serial&&destination_set.pending==1);
            assert(rem.claims==0&&rem.pending==1&&inventory(&rem,&rq,GC_RI_CHECK)==GC_RS_WAIT);
            assert(inventory(&rem,&rq,GC_RI_ACK)==GC_RS_INVALID); // old claim was consumed
            uint64_t before_leases=leases.active;assert(acquire(&leases,&retained)==GC_ML_INVALID&&!retained.lease.token&&leases.active==before_leases);
            move_release(o,&leases,keeper.lease.token);keeper=next_keeper;next_keeper.lease=(MetadataLeaseRequest){0};
            rq.registration=(uintptr_t)&destination_registration;claim=(RememberedJob){.budget=256};
            assert(inventory(&rem,&rq,GC_RI_TAKE)==GC_RS_OK&&claim.offset==moved.offset&&rem.claims==1);
            active_set=&destination_set;request.registration=rq.registration;
            located=(RememberedOwner){.registration=rq.registration,.claim=(uintptr_t)&claim,.page=(uintptr_t)&pages[4]};
            assert(find(&located)==GC_RO_OK&&located.owner==(roots.request.value>>16)&&located.byte_offset==8);
            request.source_owner=(located.owner<<16)|(roots.request.value&0xffff);request.load.byte_offset=located.byte_offset;
            receiver=(ReferenceLease){.reference=roots.request.value};assert(o->acquire(&leases,&receiver)==GC_ML_OK);
            page=(AllocationPage *)(uintptr_t)receiver.lease.page;guard.reference=roots.request.value;
            assert(o->resolve(page,&guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);request.load.page=(uintptr_t)page;
        }
        assert(load(&request)==GC_ROOT_OK&&rem.claims==1&&!request.load.output.value);
        assert(atomic_load(&roots.cells[1].value)==(variant==2?0:objects[1].reference));
        assert(o->resolve(page,&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);move_release(o,&leases,receiver.lease.token);
        assert(inventory(&rem,&rq,GC_RI_CHECK)==GC_RS_WAIT&&o->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
        if(variant==2){
            assert(o->root(&roots.ctx,&output,GC_ROOT_READ_RAW)==GC_ROOT_OK&&!output.value);
            GuardedMarkRoute empty={.generation=GC_YOUNG,.prepare={.phase=0x31102,.reference=output.value}};
            assert(route(&empty)==GC_ROUTE_SKIP&&!empty.prepare.lease.token&&!empty.prepare.output.metadata_lease);
            remembered_skip_finish(o,inventory,&rem,&rq,active_set,&queue,&leases,keeper.lease.token,&roots,&output,round);
            assert(!records[1].young_epoch&&!records[1].old_epoch);
            pool_vm_release(mapping,6*GC_HEAP_PAGE_BYTES);continue;
        }
        ForwardRequest child_claim={cells[1].source_offset,cells[1].incarnation,1,0};assert(o->forward(&cells[1],&child_claim,GC_FWD_CLAIM)==GC_FWD_OK);
        RootFollow referent={.context=(uintptr_t)&roots.ctx,.root=output};
        assert(o->follow(&leases,&referent,GC_RF_STEP)==GC_RF_WAIT&&!referent.cleanup.token&&leases.active==1);
        assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
        remembered_move(o,mapping,&pages[1],&cells[1],&pages[3],1);
        assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
        assert(o->follow(&leases,&referent,GC_RF_STEP)==GC_RF_ADVANCED);assert(o->follow(&leases,&referent,GC_RF_STEP)==GC_RF_DONE);
        assert(o->root(&roots.ctx,&output,GC_ROOT_READ_RAW)==GC_ROOT_OK);
        ReferenceLease child_lease={.reference=output.value};assert(o->acquire(&leases,&child_lease)==GC_ML_OK);
        ResolveRequest child_guard={.reference=output.value,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(o->resolve(&pages[3],&child_guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        h=(ObjectHeader *)((char *)mapping+(output.value>>16)-32);
        assert(h->mark_record==(uintptr_t)&records[1]&&records[1].object_id==objects[1].object_id&&!memcmp(h+1,"payload",7));
        GuardedMarkRoute routing={.generation=GC_YOUNG,.prepare={.scope=(uintptr_t)&child_guard.scope,.page=(uintptr_t)&pages[3],.phase=0x31102,
            .reference=output.value,.lease=child_lease.lease}};child_lease.lease=(MetadataLeaseRequest){0};
        uint64_t routed=route(&routing);
        assert(routed==(variant==1?GC_ROUTE_SKIP:GC_ROUTE_READY));
        assert(o->resolve(&pages[3],&child_guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        if(variant==1){
            assert(routing.prepare.lease.token&&!routing.prepare.output.metadata_lease);
            assert(!records[1].young_epoch&&!records[1].old_epoch);
            move_release(o,&leases,routing.prepare.lease.token);
            remembered_skip_finish(o,inventory,&rem,&rq,active_set,&queue,&leases,keeper.lease.token,&roots,&output,round);
            pool_vm_release(mapping,6*GC_HEAP_PAGE_BYTES);continue;
        }
        assert(!routing.prepare.lease.token);GuardedMarkPrepare *pending=&routing.prepare;
        assert(inventory(&rem,&rq,GC_RI_CHECK)==GC_RS_WAIT&&o->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
        assert(o->mark(&queue,&pending->output,GC_MARK_OFFER)==GC_MARK_OK&&!pending->output.metadata_lease);
        assert(o->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY&&records[1].young_epoch==round+1&&!records[1].old_epoch);
        claim.keep=1;assert(inventory(&rem,&rq,GC_RI_ACK)==GC_RS_OK);
        assert(inventory(&rem,&rq,GC_RI_CHECK)==GC_RS_EMPTY&&active_set->current_count==1);
        assert(o->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);move_release(o,&leases,keeper.lease.token);
        MarkRequest work={0};assert(o->mark(&queue,&work,GC_MARK_TAKE)==GC_MARK_OK);
        uint64_t out=0,next=0,written=0;assert(move_scan(o,&leases,&roots.ctx,&registry,&work,(uintptr_t)mapping,&out,1,&next,&written)==GC_SCAN_DONE&&!written);
        work.cursor=next;uint64_t token=work.metadata_lease;assert(o->mark(&queue,&work,GC_MARK_FINISH)==GC_MARK_OK);move_release(o,&leases,token);
        assert(o->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY&&!leases.active&&!atomic_load(&roots.ctx.access_epoch));
        assert(!records[0].young_epoch&&!records[0].old_epoch);
        remembered_revisit(o,inventory,load,route,find,&rem,&rq,&registration,&destination_registration,&roots,&output,&leases,&registry,&queue);
        assert(records[1].young_epoch==round+2&&!records[1].old_epoch);
        // All fixture journal references denote this retained Old owner or
        // the Young child just marked again. Validate that closed set before
        // consuming entries to make room for root cleanup.
        assert(drain(&roots.drain)==GC_ROOT_OK);
        for(unsigned i=0;i<roots.drain.written;i++){
            uint64_t values[]={roots.changes[i].previous,roots.changes[i].next};
            for(unsigned j=0;j<2;j++)assert(!values[j]||values[j]==objects[0].reference||values[j]==source_owner||
                values[j]==roots.request.value||values[j]==objects[1].reference||values[j]==output.value);
        }
        assert(o->root(&roots.ctx,&output,GC_ROOT_RELEASE)==GC_ROOT_OK);assert(o->root(&roots.ctx,&roots.request,GC_ROOT_RELEASE)==GC_ROOT_OK);
        // Retained current slot and original metadata are fixture-owned until
        // global teardown. Do not pretend this is a legal nonempty DETACH.
        pool_vm_release(mapping,6*GC_HEAP_PAGE_BYTES);
    }
    puts("promotion handoff: 192 age-selected Old moves register reference and null slots before forwarding publication PASS");
    puts("remembered transfer: 64 later flips reread retained canonical slots and mark moved Young references PASS");
    puts("remembered lease: 96 protected-source acquisitions and consumed-claim rejections PASS");
    puts("remembered owner: 192 claimed interior slots and 96 protected source lookups preserve relative offsets PASS");
    puts("remembered transfer: 96 protected owner moves rehome claims into canonical page inventories PASS");
    puts("mark route: 64 null and 64 promoted Old claims skip Young marking without losing current records PASS");
    puts("remembered handoff: 64 Old slots, rooted Young moves and gap-free queue transfers PASS");
}
#endif
