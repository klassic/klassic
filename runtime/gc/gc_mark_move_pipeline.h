/* A claimed array moves between bounded scans. Real pool/TLAB/descriptor/root,
 * forwarding, metadata and queue leaves run here; the fixture owns OS mappings,
 * all phase-held objects and native controls through teardown. It supplies an
 * explicit Running worker, not production registration or cycle scheduling. */
#ifndef KLASSIC_GC_MARK_MOVE_PIPELINE_H
#define KLASSIC_GC_MARK_MOVE_PIPELINE_H
#include "gc_resolve_chain.h"
#include "gc_mark_prepare_cases.h"
#include "gc_mark_retarget_cases.h"
#include "gc_mark_scan_cases.h"
#include "gc_mark_spill.h"
#include "gc_survivor_age.h"
#include "gc_promotion_remember.h"
typedef uint64_t (*MoveMarkSpill)(MarkQueue *,MarkRequest *,uint64_t);
typedef struct {
    PoolStep pool;TlabPage bind;TlabAllocator allocate;DescriptorRegister descriptor;
    ReferenceAcquirer acquire;MetadataStep metadata;Resolver resolve;ResolveRootStep root;
    RootFollower follow;MarkPreparer prepare;MarkRetargeter retarget;MarkStep mark;MoveMarkSpill spill;
    MarkScanner scan;ForwardStep forward;SourceAccess access;RelocateTarget target;RelocateCopy copy;RelocatePublish publish;SurvivorAge age;MarkedSurvivorAge marked_age;
} MarkMoveOps;
static void move_release(const MarkMoveOps *ops,MetadataLeasePool *pool,uint64_t token){
    MetadataLeaseRequest lease={.token=token};assert(ops->metadata(pool,&lease,GC_ML_RESOLVE)==GC_ML_OK);
    assert(ops->metadata(pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
}
static MarkRequest move_prepare(const MarkMoveOps *ops,MetadataLeasePool *pool,MutatorContext *ctx,uint64_t reference){
    ReferenceLease lease={.reference=reference};assert(ops->acquire(pool,&lease)==GC_ML_OK);
    AllocationPage *page=(AllocationPage *)(uintptr_t)lease.lease.page;
    ResolveRequest guard={.reference=reference,.scope={.context=(uintptr_t)ctx}};
    assert(ops->resolve(page,&guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
    GuardedMarkPrepare pending={.scope=(uintptr_t)&guard.scope,.page=(uintptr_t)page,.phase=atomic_load(&ctx->adopted_phase),.reference=reference,.lease=lease.lease};
    lease.lease=(MetadataLeaseRequest){0};assert(ops->prepare(&pending)==GC_MARK_OK);
    assert(ops->resolve(page,&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
    return pending.output; // ownership moves to caller; no raw guard/address
}
static uint64_t move_scan(const MarkMoveOps *ops,MetadataLeasePool *pool,MutatorContext *ctx,DescriptorRegistry *registry,
        MarkRequest *job,uint64_t base,uint64_t *out,uint64_t budget,uint64_t *next,uint64_t *written){
    MetadataLeaseRequest lease={.token=job->metadata_lease};assert(ops->metadata(pool,&lease,GC_ML_RESOLVE)==GC_ML_OK);
    AllocationPage *page=(AllocationPage *)(uintptr_t)lease.page;
    ResolveRequest guard={.reference=job->reference,.scope={.context=(uintptr_t)ctx}};
    assert(ops->resolve(page,&guard,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
    MarkScan scan={(uintptr_t)job,base,(uintptr_t)registry,(uintptr_t)out,budget,0,0,0,0};
    uint64_t status=ops->scan(&scan);assert((status==GC_SCAN_MORE||status==GC_SCAN_DONE)&&scan.examined<=budget);
    assert(ops->resolve(page,&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
    *next=scan.next_cursor;*written=scan.written;return status;
}
static void move_children(const MarkMoveOps *ops,MetadataLeasePool *pool,MutatorContext *ctx,MarkQueue *queue,
        const uint64_t *children,uint64_t count){
    for(uint64_t i=0;i<count;i++)if(children[i]){
        MarkRequest offer=move_prepare(ops,pool,ctx,children[i]);
        uint64_t status=ops->spill(queue,&offer,GC_MARK_OFFER_OR_SPILL);
        assert(status==GC_MARK_OK||status==GC_MARK_DUPLICATE);
        if(status==GC_MARK_DUPLICATE)move_release(ops,pool,offer.metadata_lease);
    }
}
static void gc_run_mark_move_pipeline(const MarkMoveOps *ops,
        uint64_t (*inventory)(RememberedInventory *,RememberedInventoryRequest *,uint64_t),PromotionRememberStep remember){
    for(unsigned round=0;round<80;round++){
        bool deferred=round>=64,promoting=deferred||round%4==3;
        const uint64_t length=513;void *mapping=pool_vm_reserve(4*GC_HEAP_PAGE_BYTES);
        AllocationPage pages[3]={0};ForwardCell cells[5]={0};
        ForwardTable tables[3]={{(uintptr_t)&cells[0],1,0},{(uintptr_t)&cells[1],3,0},{(uintptr_t)&cells[4],1,0}};
        _Atomic uint64_t starts[3][128]={0};MarkRecord records[4]={0};MarkArena arena={(uintptr_t)records,4,0};
        MetadataLeaseSlot slots[8]={0};for(unsigned i=0;i<8;i++)slots[i].next_free=i+1<8?i+2:0;
        uint64_t remembered_bits[2][256]={0};RememberedSet remembered={0};RememberedRegistration registration={0};
        RememberedInventory rem={.epoch=0x10000,.last_phase=0x10000};
        uint64_t native=sizeof(pages)+sizeof(tables)+sizeof(cells)+sizeof(records)+sizeof(slots)+
            sizeof(remembered_bits)+sizeof(remembered)+sizeof(registration)+sizeof(rem);
        HeapPool heap={.heap_base=(uintptr_t)mapping,.pages=(uintptr_t)pages,.page_count=3,.hard_max=3*GC_HEAP_PAGE_BYTES,
            .soft_max=2*GC_HEAP_PAGE_BYTES,.relocation_reserve=GC_HEAP_PAGE_BYTES,.metadata_committed=native,.metadata_max=native+sizeof(starts)};
        MetadataLeasePool pool={(uintptr_t)&heap,(uintptr_t)slots,8,1,0};
        for(unsigned i=0;i<3;i++){
            PageLease lease={.page_count=1,.relocation=i==2,.generation=i==2&&promoting?GC_OLD:GC_YOUNG};assert(ops->pool(&heap,&lease,GC_POOL_RESERVE)==GC_POOL_OK&&lease.head_index==i);
            pool_vm_commit((char *)mapping+pages[i].start,GC_HEAP_PAGE_BYTES);lease.starts=(uintptr_t)starts[i];lease.committed_mapping=1;
            assert(ops->pool(&heap,&lease,GC_POOL_COMMIT)==GC_POOL_OK);pages[i].forward_table=(uintptr_t)&tables[i];
        }
        ChainRoots roots;chain_roots_init(&roots);
        if(deferred){
            atomic_store(&roots.ctx.adopted_phase,0x10020); // Old ConcurrentMark
            rem.epoch=rem.last_phase=0x10020;
        }
        uint64_t bitmap=1;TypeDescriptor array={.id=1,.alignment=16,.element_bytes=8,.element_bitmap=(uintptr_t)&bitmap,.element_bitmap_words=1};
        TypeDescriptor scalar={.id=2,.alignment=16,.fixed_bytes=7};_Atomic uint64_t descriptor_slots[3]={0};
        DescriptorRegistry registry={(uintptr_t)descriptor_slots,3};
        assert(ops->descriptor(&registry,&array)==GC_DESC_OK&&ops->descriptor(&registry,&scalar)==GC_DESC_OK);
        AllocationResult objects[4]={0};
        for(unsigned i=0;i<4;i++){
            AllocationPage *page=&pages[i?1:0];if(i<2)assert(ops->bind(&roots.ctx,page,0)==GC_ALLOC_OK);
            AllocationRequest request={(uintptr_t)page,(uintptr_t)(i?&scalar:&array),i?0:length,(uintptr_t)&objects[i],(uintptr_t)&arena};
            assert(ops->allocate(&roots.ctx,&request)==GC_ALLOC_OK);if(i==0||i==3)assert(ops->bind(&roots.ctx,page,1)==GC_ALLOC_OK);
            ObjectHeader *h=(ObjectHeader *)((char *)mapping+(objects[i].reference>>16)-32);
            if(i)memcpy(h+1,"payload",7);
            if(i)atomic_store(&h->state,GC_OBJECT_PUBLISHED);
        }
        _Atomic uint64_t *fields=(_Atomic uint64_t *)((char *)mapping+(objects[0].reference>>16));
        for(uint64_t i=0;i<length;i++)atomic_store(&fields[i],i%4?objects[i%4].reference:0);
        atomic_store(&((ObjectHeader *)((char *)mapping+(objects[0].reference>>16)-32))->state,GC_OBJECT_PUBLISHED);
        assert(ops->root(&roots.ctx,&roots.request,GC_ROOT_RESERVE)==GC_ROOT_OK);
        roots.request.value=objects[0].reference;assert(ops->root(&roots.ctx,&roots.request,GC_ROOT_STORE)==GC_ROOT_OK);
        MarkTask task={0};MarkQueue queue={.tasks=(uintptr_t)&task,.capacity=1,.free_head=1,.epoch=round+1,.generation=deferred?GC_OLD:round%2};
        MarkRequest job={0},blocker={0};uint64_t out[64],next,written,visited=0;
        if(deferred){
            // An unrelated in-flight scalar occupies the only ordinary slot.
            blocker=move_prepare(ops,&pool,&roots.ctx,objects[1].reference);
            assert(ops->mark(&queue,&blocker,GC_MARK_OFFER)==GC_MARK_OK);
            assert(ops->mark(&queue,&blocker,GC_MARK_TAKE)==GC_MARK_OK);
            job=move_prepare(ops,&pool,&roots.ctx,objects[0].reference);
            MarkRequest before=job;MarkQueue queue_before=queue;MarkRecord record_before=records[0];
            assert(ops->mark(&queue,&job,GC_MARK_OFFER)==GC_MARK_FULL);
            assert(!memcmp(&job,&before,sizeof(job))&&!memcmp(&queue,&queue_before,sizeof(queue))&&!memcmp(&records[0],&record_before,sizeof(record_before)));
            assert(!records[0].old_epoch&&ops->spill(&queue,&job,GC_MARK_OFFER_OR_SPILL)==GC_MARK_OK);
            assert(!job.metadata_lease&&queue.overflow_count==1&&records[0].old_epoch==queue.epoch);
            assert(records[0].old_spill.reference==objects[0].reference&&records[0].old_spill.metadata_lease==before.metadata_lease);
        }else{
            job=move_prepare(ops,&pool,&roots.ctx,objects[0].reference);
            assert(ops->mark(&queue,&job,GC_MARK_OFFER)==GC_MARK_OK);assert(ops->mark(&queue,&job,GC_MARK_TAKE)==GC_MARK_OK);
            assert(move_scan(ops,&pool,&roots.ctx,&registry,&job,(uintptr_t)mapping,out,3,&next,&written)==GC_SCAN_MORE);
            assert(next==24&&written==3);for(uint64_t i=0;i<written;i++)assert(out[i]==(i%4?objects[i%4].reference:0));
            move_children(ops,&pool,&roots.ctx,&queue,out,written);visited+=written;job.cursor=next;
            if(round%2){assert(ops->mark(&queue,&job,GC_MARK_YIELD)==GC_MARK_OK);assert(ops->mark(&queue,&job,GC_MARK_TAKE)==GC_MARK_OK);}
        }
        MarkRequest stale=job;
        ForwardRequest claim={cells[0].source_offset,cells[0].incarnation,1,0};assert(ops->forward(&cells[0],&claim,GC_FWD_CLAIM)==GC_FWD_OK);
        SourceAccessRequest select={0,cells[0].source_offset,cells[0].incarnation,0,1};assert(ops->access(&cells[0],&select,GC_SA_SELECT)==GC_SA_OK);
        assert(!records[0].age&&!records[0].age_epoch);
        // The retained root proves this selected Young object is live. Old
        // marking alone must not age it. Threshold two keeps this move Young.
        if(!queue.generation)assert(ops->age(&records[0],queue.epoch,2)==GC_AGE_YOUNG);
        // A separate fixture-proven Young survival selects promotion while the
        // existing Old claim still owns its cursor and outstanding children.
        if(promoting)assert(ops->age(&records[0],1000+round,1)==GC_AGE_PROMOTE);
        RelocationCopy copy={(uintptr_t)mapping,cells[0].incarnation,1,pages[0].limit,0,0,256,0,0};
        assert(ops->target(&pages[2],&copy,GC_TARGET_PREPARE)==GC_TARGET_OK);
        uint64_t copied;do{copied=ops->copy(&cells[0],&copy);}while(copied==GC_COPY_MORE);
        assert(copied==GC_COPY_DONE&&copy.cursor==length*8);
        assert(ops->target(&pages[2],&copy,GC_TARGET_INSTALL)==GC_TARGET_OK);
        if(promoting){
            remembered=(RememberedSet){.start=pages[2].start,.limit=pages[2].limit,.incarnation=pages[2].incarnation,
                .bits={(uintptr_t)remembered_bits[0],(uintptr_t)remembered_bits[1]},.words=256,.epoch=rem.epoch,.last_phase=rem.epoch};
            registration.set=(uintptr_t)&remembered;RememberedJob record={0};
            RememberedInventoryRequest attach={(uintptr_t)&registration,(uintptr_t)&record,0};
            assert(inventory(&rem,&attach,GC_RI_ATTACH)==GC_RS_OK);
            PromotionRemember pending={(uintptr_t)&registry,(uintptr_t)&registration,copy.destination,array.id,length*8,rem.epoch,0};
            unsigned slices=0;uint64_t status;
            do{
                MarkQueue before=queue;MarkTask task_before=task;MarkRequest job_before=job;
                status=remember(&rem,&pending);assert(status==GC_PR_MORE||status==GC_PR_DONE);slices++;
                assert(!memcmp(&queue,&before,sizeof(queue))&&!memcmp(&task,&task_before,sizeof(task))&&!memcmp(&job,&job_before,sizeof(job)));
                assert(ops->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);
            }while(status==GC_PR_MORE);
            assert(slices==length&&remembered.current_count==length&&pending.cursor==length*8);
            assert(pages[2].generation==GC_OLD&&records[0].old_epoch==queue.epoch&&job.cursor==visited*8&&queue.in_flight==1);
            assert(atomic_load(&cells[0].word)==((copy.ticket<<2)|1));
            for(uint64_t i=0;i<length;i++){
                uint64_t bit=(copy.destination+8*i-remembered.start)/8;
                assert(remembered_bits[0][bit/64]&(UINT64_C(1)<<(bit%64)));
            }
        }
        assert(ops->publish(&cells[0],&copy,0x8630)==GC_COPY_DONE);
        pool_vm_decommit((char *)mapping+pages[0].start,GC_HEAP_PAGE_BYTES);
        assert(pages[0].metadata_leases==1&&queue.in_flight==1);
        RootFollow follow={.context=(uintptr_t)&roots.ctx,.root=roots.request};follow.root.value=0;
        assert(ops->follow(&pool,&follow,GC_RF_STEP)==GC_RF_ADVANCED);
        assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
        assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
        roots.request.value=0;assert(ops->root(&roots.ctx,&roots.request,GC_ROOT_READ_RAW)==GC_ROOT_OK);
        if(deferred){
            // Forwarding is already published and the old payload inaccessible;
            // the spilled task still protects its source metadata until retarget.
            assert(ops->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY&&queue.overflow_count==1);
            assert(move_scan(ops,&pool,&roots.ctx,&registry,&blocker,(uintptr_t)mapping,out,1,&next,&written)==GC_SCAN_DONE&&!written);
            blocker.cursor=next;assert(ops->mark(&queue,&blocker,GC_MARK_FINISH)==GC_MARK_OK);
            move_release(ops,&pool,blocker.metadata_lease);blocker=(MarkRequest){0};
            assert(ops->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);
            assert(ops->spill(&queue,NULL,GC_MARK_REFILL)==GC_MARK_OK);
            assert(ops->mark(&queue,&job,GC_MARK_TAKE)==GC_MARK_OK&&!job.cursor);
            assert(job.reference==objects[0].reference&&job.record==(uintptr_t)&records[0]);
            stale=job;
        }
        MarkRetarget handoff={.job=(uintptr_t)&job,.destination=move_prepare(ops,&pool,&roots.ctx,roots.request.value)};
        assert(ops->retarget(&queue,&handoff)==GC_MARK_OK&&job.cursor==visited*8&&task.cursor==visited*8&&queue.in_flight==1);
        assert(job.reference==roots.request.value&&job.record==(uintptr_t)&records[0]&&!handoff.destination.metadata_lease);
        if(!queue.generation){
            MarkRecord before=records[0];
            assert(ops->age((void *)(uintptr_t)job.record,queue.epoch,2)==GC_AGE_YOUNG);
            assert(records[0].age==1&&records[0].age_epoch==queue.epoch&&!memcmp(&before,&records[0],sizeof(before)));
        }else if(promoting){
            assert(records[0].age==1&&records[0].age_epoch==1000+round&&records[0].old_epoch==queue.epoch);
            assert(pages[2].generation==GC_OLD&&remembered.current_count==length);
        }else assert(!records[0].age&&!records[0].age_epoch);
        assert(handoff.retired.metadata_lease==stale.metadata_lease&&pages[0].metadata_leases==1&&pages[2].metadata_leases==1);
        assert(ops->mark(&queue,&stale,GC_MARK_YIELD)==GC_MARK_INVALID&&ops->mark(&queue,&stale,GC_MARK_FINISH)==GC_MARK_INVALID);
        move_release(ops,&pool,handoff.retired.metadata_lease);handoff.retired=(MarkRequest){0};assert(!pages[0].metadata_leases);
        assert(ops->root(&roots.ctx,&roots.request,GC_ROOT_RELEASE)==GC_ROOT_OK);
        uint64_t status;
        do{
            uint64_t before=job.cursor;
            status=move_scan(ops,&pool,&roots.ctx,&registry,&job,(uintptr_t)mapping,out,round%3==0?1:round%3==1?7:64,&next,&written);
            assert(before==visited*8&&next==(visited+written)*8);
            for(uint64_t i=0;i<written;i++){uint64_t index=visited+i;assert(out[i]==(index%4?objects[index%4].reference:0));}
            move_children(ops,&pool,&roots.ctx,&queue,out,written);visited+=written;job.cursor=next;
            if(status==GC_SCAN_MORE){assert(ops->mark(&queue,&job,GC_MARK_YIELD)==GC_MARK_OK);assert(ops->mark(&queue,&job,GC_MARK_TAKE)==GC_MARK_OK);}
        }while(status==GC_SCAN_MORE);
        assert(visited==length&&queue.overflow_count==(deferred?2:3));
        assert(ops->mark(&queue,&job,GC_MARK_FINISH)==GC_MARK_OK);move_release(ops,&pool,job.metadata_lease);job=(MarkRequest){0};
        for(unsigned i=0;i<(deferred?2u:3u);i++){
            assert(ops->spill(&queue,NULL,GC_MARK_REFILL)==GC_MARK_OK);assert(ops->mark(&queue,&job,GC_MARK_TAKE)==GC_MARK_OK);
            assert(move_scan(ops,&pool,&roots.ctx,&registry,&job,(uintptr_t)mapping,out,1,&next,&written)==GC_SCAN_DONE&&!written);
            job.cursor=next;assert(ops->mark(&queue,&job,GC_MARK_FINISH)==GC_MARK_OK);
            move_release(ops,&pool,job.metadata_lease);job=(MarkRequest){0};
        }
        assert(ops->mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY&&!pool.active&&!atomic_load(&roots.ctx.access_epoch));
        for(unsigned i=0;i<4;i++){
            assert((queue.generation?records[i].old_epoch:records[i].young_epoch)==round+1);
            assert((queue.generation?records[i].young_epoch:records[i].old_epoch)==0&&records[i].object_id==objects[i].object_id);
        }
        for(unsigned i=0;i<3;i++)assert(!pages[i].metadata_leases);
        pool_vm_release(mapping,4*GC_HEAP_PAGE_BYTES);
    }
    puts("promotion Old work admission: 16 full-queue parents retain source leases through spill, promotion and post-protection retarget PASS");
    puts("promotion during Old mark: 16 partial-array claims retain cursors, overflow children and all 513 remembered slots across protected-source promotion PASS");
    puts("survivor age relocation: shared records retain one Young survival across moved-job retries without Old-mark aging PASS");
    puts("mark retarget: 64 protected-source partial-array moves, overflow children and stale-claim rounds PASS");
}
#endif
