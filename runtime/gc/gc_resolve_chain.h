/* Real protected source pages and prepared-root handoff between bounded hops.
 * This fixture owns all metadata until final cleanup; it does not claim a
 * production phase scheduler or virtual-address retirement implementation. */
#ifndef KLASSIC_GC_RESOLVE_CHAIN_H
#define KLASSIC_GC_RESOLVE_CHAIN_H
#include "gc_forward_compress.h"
#include "gc_forward_incoming.h"
#include "gc_resolve_cases.h"
#include "gc_relocate_target_cases.h"
#include "gc_roots.h"
#include "gc_root_follow_cases.h"
#include "gc_reference_lease_cases.h"
#include "gc_forward_cases.h"
#include "gc_source_access_cases.h"
#include "gc_context.h"
#include "gc_pending_mark.h"
#include "gc_root_journal.h"
#include "gc_pages_pipeline.h"
typedef uint64_t (*ResolveRootStep)(MutatorContext *,RootRequest *,uint64_t);
typedef uint64_t (*ResolveRootDrain)(RootDrain *);
typedef struct {
    MutatorContext ctx;RootSegment segment;RootRequest request;BufferCursor buffer;
    RootCell cells[2];RootChange entries[7],changes[7];RootDrain drain;
} ChainRoots;
static void chain_roots_init(ChainRoots *r) {
    memset(r,0,sizeof(*r));r->ctx=(MutatorContext){.abi_version=GC_ABI_VERSION,.thread_id=17};
    r->segment=(RootSegment){.owner=17,.scope=101,.cells=(uintptr_t)r->cells,.capacity=2,
        .state=GC_ROOT_SEGMENT_ACTIVE,.journal=(uintptr_t)&r->buffer};
    r->request.segment=(uintptr_t)&r->segment;
    r->buffer.entries=(uintptr_t)r->entries;r->buffer.capacity=7;
    r->drain=(RootDrain){(uintptr_t)&r->buffer,(uintptr_t)r->changes,7,0};
}
static void chain_move(void *mapping,ForwardCell *source,AllocationPage *target,ForwardStep forward,SourceAccess access,uint64_t color,RelocateCopy copy,RelocatePublish publish,RelocateTarget prepare) {
    ForwardRequest request={source->source_offset,source->incarnation,1,0};assert(forward(source,&request,GC_FWD_CLAIM)==GC_FWD_OK);
    SourceAccessRequest select={0,source->source_offset,source->incarnation,0,1};
    assert(access(source,&select,GC_SA_SELECT)==GC_SA_OK);assert(access(source,&select,GC_SA_CHECK)==GC_SA_OK);
    RelocationCopy q={(uintptr_t)mapping,source->incarnation,1,source->source_offset-32+GC_HEAP_PAGE_BYTES,
        0,0,3,0,0};
    assert(prepare(target,&q,GC_TARGET_PREPARE)==GC_TARGET_OK);
    assert(prepare(target,&q,GC_TARGET_INSTALL)==GC_TARGET_INVALID);
    uint64_t status;do{status=copy(source,&q);}while(status==GC_COPY_MORE);
    assert(status==GC_COPY_DONE&&q.stage==2&&q.cursor==7);
    assert(prepare(target,&q,GC_TARGET_INSTALL)==GC_TARGET_OK);
    assert(publish(source,&q,color)==GC_COPY_DONE);
    // Any subsequent header access to the old physical copy faults here.
    pool_vm_decommit((char *)mapping+source->source_offset-sizeof(ObjectHeader),GC_HEAP_PAGE_BYTES);
}
typedef struct {
    void *mapping;ForwardCell *source;AllocationPage *target;
    ForwardStep forward;SourceAccess access;uint64_t color;
    RelocateCopy copy;RelocatePublish publish;RelocateTarget prepare;
} ChainMoveTask;
#ifdef _WIN32
static DWORD WINAPI chain_move_actor(void *argument){
#else
static void *chain_move_actor(void *argument){
#endif
    ChainMoveTask *t=argument;
    chain_move(t->mapping,t->source,t->target,t->forward,t->access,t->color,t->copy,t->publish,t->prepare);
    return 0;
}
/* The consumer deliberately pauses without a guard but with counted journal
 * work and a metadata token. A distinct OS actor then copies/publishes and
 * protects that source before the consumer attempts canonical access. */
static void chain_move_join(ChainMoveTask *task){
#ifdef _WIN32
    HANDLE thread=CreateThread(NULL,0,chain_move_actor,task,0,NULL);assert(thread);
    assert(WaitForSingleObject(thread,30000)==WAIT_OBJECT_0);assert(CloseHandle(thread));
#else
    pthread_t thread;assert(!pthread_create(&thread,NULL,chain_move_actor,task));
    assert(!pthread_join(thread,NULL));
#endif
}
/* A registered, copied journal item retains the old source while its owner
 * clears the root. A supplied mover protects the intermediate copy after
 * follow DONE; otherwise both source payload pages are already inaccessible.
 * This idle-phase handoff has no mark obligations; it is not cycle closure. */
static void chain_pending_journal(void *mapping,AllocationPage *pages,ForwardCell *cells,
        MetadataLeasePool *pool,MutatorContext *ctx,MarkRecord *record,uint64_t original,
        uint64_t canonical,PendingFollower follow,PendingAccessor enter,Resolver resolve,MetadataStep metadata,ChainMoveTask *mover){
    assert(!atomic_load(&ctx->adopted_phase));
    RootRegistry registry={0};RootCell cell={0};RootChange entries[2]={0};
    BufferCursor journal={.entries=(uintptr_t)entries,.capacity=2};
    RootSegment segment={.owner=ctx->thread_id,.cells=(uintptr_t)&cell,.capacity=1,.journal=(uintptr_t)&journal};
    RootRegistration registration={.segment=(uintptr_t)&segment};
    RootLease cleanup={0};RootLifeRequest life={.registration=(uintptr_t)&registration,
        .context=(uintptr_t)ctx,.lease=(uintptr_t)&cleanup};
    assert(gc_root_life_locked(&registry,&life,GC_RL_ATTACH)==GC_RL_OK);
    RootRequest root={.segment=(uintptr_t)&segment};
    assert(gc_root_step(ctx,&root,GC_ROOT_RESERVE)==GC_ROOT_OK);
    root.value=original;assert(gc_root_step(ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
    RootLease claim={0};RootChange item={0};
    RootJournalRequest work={.registration=(uintptr_t)&registration,.lease=(uintptr_t)&claim,
        .scope=segment.scope,.output=(uintptr_t)&item,.capacity=1};
    for(unsigned batch=0;batch<2;batch++){
        assert(gc_root_journal_step(&registry,&work,GC_RJ_CLAIM)==GC_RJ_OK);
        assert(gc_root_journal_step(&registry,&work,GC_RJ_COPY)==GC_RJ_OK&&work.written==1&&!item.phase);
        if(!batch){
            root.value=0;assert(gc_root_step(ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
            assert(!atomic_load(&cell.value)&&registry.leases==1&&registration.journal_serial==work.serial);
            assert(gc_root_step(ctx,&root,GC_ROOT_STORE)==GC_ROOT_FLUSH);
        }
        uint64_t reference=batch?item.previous:item.next;
        assert(reference==original&&(batch?!item.next:!item.previous));
        ReferenceLease current={.reference=reference};MetadataLeaseRequest retired={0};
        ResolveRequest guard={.scope={.context=(uintptr_t)ctx}};
        if(mover&&!batch){
            assert(follow(pool,&current,&retired)==GC_RF_DONE&&current.reference==original);
            ReferenceLease before=current;
            assert(current.lease.page==(uintptr_t)&pages[1]&&pool->active==1&&!atomic_load(&ctx->access_epoch));
            assert(gc_context_transition_locked(ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
            chain_move_join(mover);
            assert(gc_context_transition_locked(ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
            assert(enter(pool,&current,&guard)==GC_RF_ADVANCED);
            ResolveRequest empty={.scope={.context=(uintptr_t)ctx}};
            assert(!memcmp(&guard,&empty,sizeof(guard))&&!memcmp(&current,&before,sizeof(current)));
            assert(pages[1].metadata_leases==1&&registry.leases==1&&!atomic_load(&ctx->access_epoch));
        }
        /* Occupy one slot: source acquisition succeeds, but destination cannot.
         * The exact source token and journal claim must survive this failure. */
        MetadataLeaseRequest blocker={(uintptr_t)&pages[2],pages[2].incarnation,0};
        assert(metadata(pool,&blocker,GC_ML_ACQUIRE)==GC_ML_OK);
        assert(follow(pool,&current,&retired)==GC_RF_METADATA_FULL);
        assert(current.reference==original&&current.lease.page==(uintptr_t)&pages[mover?1:0]&&pool->active==2);
        assert(atomic_load(&journal.consumed)==batch&&registry.leases==1);
        assert(metadata(pool,&blocker,GC_ML_RELEASE)==GC_ML_OK);
        for(unsigned hop=mover?1:0;hop<2;hop++){
            assert(follow(pool,&current,&retired)==GC_RF_ADVANCED);
            assert(current.lease.page==(uintptr_t)&pages[hop+1]&&!pages[hop].metadata_leases);
            assert(!retired.token&&pool->active==1&&registry.leases==1);
        }
        assert(follow(pool,&current,&retired)==GC_RF_DONE&&current.reference==canonical);
        assert(enter(pool,&current,&guard)==GC_RAC_READY);
        ObjectHeader *header=(void *)((char *)mapping+(current.reference>>16)-sizeof(ObjectHeader));
        assert(header->mark_record==(uintptr_t)record&&record->object_id==42&&header->payload_bytes==7);
        assert(!memcmp(header+1,"payload",7));
        assert(resolve(&pages[2],&guard,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        assert(metadata(pool,&current.lease,GC_ML_RELEASE)==GC_ML_OK);
        assert(!pool->active&&atomic_load(&journal.consumed)==batch);
        assert(gc_root_journal_step(&registry,&work,GC_RJ_ACK)==GC_RJ_OK);
    }
    assert(!registry.leases&&atomic_load(&journal.consumed)==2);
    assert(gc_root_life_locked(&registry,&life,GC_RL_RETIRE)==GC_RL_OK);
    assert(gc_root_life_locked(&registry,&life,GC_RL_CLAIM)==GC_RL_OK&&!life.mask);
    uint64_t value=1;RootScanJob scan={.scan={(uintptr_t)&segment,0,(uintptr_t)&value,1,0},
        .lease=(uintptr_t)&cleanup,.serial=life.serial};
    assert(gc_root_scan_leased(&scan)==GC_ROOT_OK&&!value);
    assert(gc_root_life_locked(&registry,&life,GC_RL_ACK)==GC_RL_OK);
    assert(gc_root_life_locked(&registry,&life,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&life)==GC_RL_OK);
    assert(gc_root_life_locked(&registry,&life,GC_RL_FINISH)==GC_RL_OK);
    assert(!registry.inventory&&!registry.retiring&&!registry.active&&!ctx->root_segments&&!ctx->retiring_roots);
    assert(atomic_load(&cells[0].word)&&atomic_load(&cells[1].word));
}
/* Both historical payload pages are protected before this marking-phase
 * consumer starts. A counted journal claim owns the old reference through
 * metadata pressure, both generation offers, scalar scanning and cleanup.
 * The other fixture root remains live; this is not sole-root cycle closure. */
static void chain_pending_mark(void *mapping,AllocationPage *pages,MetadataLeasePool *pool,
        MarkRecord *record,uint64_t original,uint64_t canonical,PendingMarker marker,
        Resolver resolve,MetadataStep metadata){
    MutatorContext ctx={.abi_version=GC_ABI_VERSION,.thread_id=29,.adopted_phase=0x10022};
    RootRegistry registry={.phase=0x10022};RootCell cell={0};RootChange entries[2]={0};
    BufferCursor journal={.entries=(uintptr_t)entries,.capacity=2};
    RootSegment segment={.owner=29,.cells=(uintptr_t)&cell,.capacity=1,.journal=(uintptr_t)&journal};
    RootRegistration registration={.segment=(uintptr_t)&segment};RootLease cleanup={0};
    RootLifeRequest life={.registration=(uintptr_t)&registration,.context=(uintptr_t)&ctx,.lease=(uintptr_t)&cleanup};
    assert(gc_root_life_locked(&registry,&life,GC_RL_ATTACH)==GC_RL_OK);
    RootRequest root={.segment=(uintptr_t)&segment};
    assert(gc_root_step(&ctx,&root,GC_ROOT_RESERVE)==GC_ROOT_OK);
    root.value=original;assert(gc_root_step(&ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
    root.value=0;assert(gc_root_step(&ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
    assert(!atomic_load(&cell.value)&&gc_root_step(&ctx,&root,GC_ROOT_STORE)==GC_ROOT_FLUSH);
    MarkTask tasks[2]={0};MarkQueue queues[2]={0};
    for(unsigned g=0;g<2;g++)queues[g]=(MarkQueue){.tasks=(uintptr_t)&tasks[g],.capacity=1,.free_head=1,
        .epoch=(g?record->old_epoch:record->young_epoch)+1,.generation=g};
    for(unsigned batch=0;batch<2;batch++){
        RootLease claim={0};RootChange item={0};
        RootJournalRequest journal_work={.registration=(uintptr_t)&registration,.lease=(uintptr_t)&claim,
            .scope=segment.scope,.output=(uintptr_t)&item,.capacity=1};
        assert(gc_root_journal_step(&registry,&journal_work,GC_RJ_CLAIM)==GC_RJ_OK);
        assert(gc_root_journal_step(&registry,&journal_work,GC_RJ_COPY)==GC_RJ_OK&&journal_work.written==1);
        assert(item.phase==registry.phase&&(batch?item.previous:item.next)==original);
        for(unsigned g=0;g<2;g++){
            PendingMark q={.context=(uintptr_t)&ctx,.current={.reference=original},.queue=(uintptr_t)&queues[g],
                .generation=g,.epoch=queues[g].epoch,.phase=item.phase};
            MetadataLeaseRequest blocker={(uintptr_t)&pages[2],pages[2].incarnation,0};
            assert(metadata(pool,&blocker,GC_ML_ACQUIRE)==GC_ML_OK);
            assert(marker(pool,&q,GC_RM_ENTER)==GC_RF_METADATA_FULL);
            assert(!q.stage&&q.current.reference==original&&q.current.lease.page==(uintptr_t)&pages[0]);
            assert(pool->active==2&&registry.leases==1&&atomic_load(&journal.consumed)==batch);
            assert(metadata(pool,&blocker,GC_ML_RELEASE)==GC_ML_OK);
            unsigned hops=0;
            for(;;){uint64_t status=marker(pool,&q,GC_RM_ENTER);
                if(status==GC_RAC_READY)break;
                assert(status==GC_RF_ADVANCED&&++hops<=2);
                assert(registry.leases==1&&atomic_load(&journal.consumed)==batch);
            }
            assert(hops==2&&q.current.reference==canonical&&q.current.lease.page==(uintptr_t)&pages[2]);
            assert(marker(pool,&q,GC_RM_ROUTE)==GC_RM_EXIT_REQUIRED);
            bool skip=g==GC_YOUNG&&pages[2].generation==GC_OLD;
            assert(q.route_result==(skip?GC_ROUTE_SKIP:GC_ROUTE_READY));
            assert(marker(pool,&q,GC_RM_EXIT)==(skip?GC_RM_RELEASE_REQUIRED:GC_RM_OFFER_REQUIRED));
            if(!skip){
                uint64_t status=marker(pool,&q,GC_RM_OFFER);
                assert(status==(batch?GC_RM_RELEASE_REQUIRED:GC_RM_COMPLETE));
                assert(q.offer_result==(batch?GC_MARK_DUPLICATE:GC_MARK_OK));
            }
            if(q.stage==4)assert(marker(pool,&q,GC_RM_RELEASE)==GC_RM_COMPLETE);
            assert(q.stage==5&&!q.current.lease.token&&!q.retired.token&&!atomic_load(&ctx.access_epoch));
            if(queues[g].queued){
                MarkRequest work={0};assert(gc_mark_step_locked(&queues[g],&work,GC_MARK_TAKE)==GC_MARK_OK);
                assert(work.reference==canonical&&work.record==(uintptr_t)record&&queues[g].in_flight==1);
                MetadataLeaseRequest lease={.token=work.metadata_lease};
                assert(metadata(pool,&lease,GC_ML_RESOLVE)==GC_ML_OK&&lease.page==(uintptr_t)&pages[2]);
                ResolveRequest access={.reference=work.reference,.scope={.context=(uintptr_t)&ctx}};
                assert(resolve(&pages[2],&access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
                ObjectHeader *header=(void *)((char *)mapping+(work.reference>>16)-sizeof(ObjectHeader));
                assert(header->mark_record==work.record&&header->payload_bytes==7&&!memcmp(header+1,"payload",7));
                assert(resolve(&pages[2],&access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
                assert(gc_mark_step_locked(&queues[g],&work,GC_MARK_FINISH)==GC_MARK_OK);
                assert(metadata(pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
            }
            assert(!pool->active&&!queues[g].queued&&!queues[g].in_flight&&!queues[g].overflow_count);
            assert(skip||(g?record->old_epoch:record->young_epoch)==queues[g].epoch);
            assert(registry.leases==1&&atomic_load(&journal.consumed)==batch);
        }
        assert(gc_root_journal_step(&registry,&journal_work,GC_RJ_ACK)==GC_RJ_OK);
    }
    assert(gc_root_life_locked(&registry,&life,GC_RL_RETIRE)==GC_RL_OK);
    assert(gc_root_life_locked(&registry,&life,GC_RL_CLAIM)==GC_RL_OK&&life.mask==3);
    uint64_t value=1;RootScanJob scan={.scan={(uintptr_t)&segment,0,(uintptr_t)&value,1,0},
        .lease=(uintptr_t)&cleanup,.serial=life.serial};
    assert(gc_root_scan_leased(&scan)==GC_ROOT_OK&&!value);
    assert(gc_root_life_locked(&registry,&life,GC_RL_ACK)==GC_RL_OK);
    assert(gc_root_life_locked(&registry,&life,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&life)==GC_RL_OK);
    assert(gc_root_life_locked(&registry,&life,GC_RL_FINISH)==GC_RL_OK);
    assert(!registry.inventory&&!registry.leases&&!ctx.root_segments&&!ctx.retiring_roots&&!pool->active);
}
static void gc_run_resolve_chains(RootFollower follow,Resolver resolve,ReferenceAcquirer acquire,MetadataStep metadata,ForwardStep forward,SourceAccess access,ResolveRootStep root,ResolveRootDrain drain,RelocateCopy copy,RelocatePublish publish,RelocateTarget prepare,PoolStep pool_step,ForwardCompressor compress,
        uint64_t (*incoming)(const ForwardTable *,ForwardIncoming *),PendingFollower pending_follow,PendingAccessor pending_access,PendingMarker pending_mark) {
    for(unsigned round=0;round<64;round++) {
        void *mapping=pool_vm_reserve(4*GC_HEAP_PAGE_BYTES);
        AllocationPage pages[3]={0};ForwardCell cells[3]={0};ForwardTable tables[3]={0};_Atomic uint64_t starts[3][128]={0};
        HeapPool heap={.heap_base=(uintptr_t)mapping,.pages=(uintptr_t)pages,.page_count=3,
            .hard_max=3*GC_HEAP_PAGE_BYTES,.soft_max=GC_HEAP_PAGE_BYTES,.relocation_reserve=2*GC_HEAP_PAGE_BYTES,
            .metadata_committed=sizeof(pages)+sizeof(tables)+sizeof(cells),
            .metadata_max=sizeof(pages)+sizeof(tables)+sizeof(cells)+sizeof(starts)};
        MetadataLeaseSlot slots[2]={{.next_free=2},{0}};
        MetadataLeasePool pool={(uintptr_t)&heap,(uintptr_t)slots,2,1,0};
        MarkRecord record={.object_id=42,.old_epoch=round+1};
        ChainRoots *roots=malloc(sizeof(*roots));assert(roots);chain_roots_init(roots);
        uint64_t colors[3]={UINT64_C(0x1510),UINT64_C(0x2620),UINT64_C(0x4930)};
        for(unsigned i=0;i<3;i++) {
            PageLease lease={.page_count=1,.generation=(round+i)%2,.relocation=i!=0};
            assert(pool_step(&heap,&lease,GC_POOL_RESERVE)==GC_POOL_OK&&lease.head_index==i);
            pool_vm_commit((char *)mapping+pages[i].start,GC_HEAP_PAGE_BYTES);
            lease.starts=(uintptr_t)starts[i];lease.committed_mapping=1;
            assert(pool_step(&heap,&lease,GC_POOL_COMMIT)==GC_POOL_OK);
            tables[i]=(ForwardTable){(uintptr_t)&cells[i],1,0};pages[i].forward_table=(uintptr_t)&tables[i];
        }
        // Only the source is pre-existing. Destination geometry, incarnation
        // and bitmap now come from the pool; copy installation populates them.
        cells[0]=(ForwardCell){pages[0].start+32,pages[0].incarnation,0,0};
        atomic_store(&tables[0].used,1);atomic_store(&starts[0][0],4);
        atomic_store(&pages[0].allocated_top,pages[0].start+48);atomic_store(&pages[0].state,GC_PAGE_CLOSED);

        ObjectHeader *original=(ObjectHeader *)((char *)mapping+GC_HEAP_PAGE_BYTES);
        original->payload_bytes=7;original->descriptor_id=1;original->mark_record=(uintptr_t)&record;atomic_store(&original->state,GC_OBJECT_PUBLISHED);
        memcpy(original+1,"payload",7);
        assert(root(&roots->ctx,&roots->request,GC_ROOT_RESERVE)==GC_ROOT_OK);
        roots->request.value=(cells[0].source_offset<<16)|colors[0];assert(root(&roots->ctx,&roots->request,GC_ROOT_STORE)==GC_ROOT_OK);
        RootFollow follower={.context=(uintptr_t)&roots->ctx,.root=roots->request};follower.root.value=0;
        chain_move(mapping,&cells[0],&pages[1],forward,access,colors[1],copy,publish,prepare);
        if(round%4==0) {
            // A prepared cell alone does not guarantee journal room. Failed
            // adoption discards the private result before polling/retrying.
            for(unsigned i=0;i<6;i++)assert(root(&roots->ctx,&roots->request,GC_ROOT_STORE)==GC_ROOT_OK);
            uint64_t source=atomic_load(&roots->cells[0].value);
            assert(follow(&pool,&follower,GC_RF_STEP)==GC_RF_FLUSH);
            assert(atomic_load(&roots->cells[0].value)==source&&!follower.root.value&&!follower.cleanup.token&&!pool.active);
            assert(gc_context_transition_locked(&roots->ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
            assert(gc_context_transition_locked(&roots->ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
            // The fixture holds every object/table through cleanup. Drained
            // changes stay in its native in-flight storage for this lifetime.
            assert(drain(&roots->drain)==GC_ROOT_OK&&roots->drain.written==7);
        }
        roots->request.value=0;
        for(unsigned hop=0;hop<3;hop++) {
            // The intermediate copy can move before or after root adoption.
            if(hop==0&&round%2==0){
                ChainMoveTask mover={mapping,&cells[1],&pages[2],forward,access,colors[2],copy,publish,prepare};
                chain_pending_journal(mapping,pages,cells,&pool,&roots->ctx,&record,
                    (cells[1].source_offset<<16)|colors[1],((pages[2].start+32)<<16)|colors[2],
                    pending_follow,pending_access,resolve,metadata,&mover);
            }
            assert(follow(&pool,&follower,GC_RF_STEP)==(hop<2?GC_RF_ADVANCED:GC_RF_DONE));
            assert(!pool.active&&!follower.cleanup.token&&!follower.root.value&&!atomic_load(&roots->ctx.access_epoch));
            if(hop<2) {
                assert(atomic_load(&roots->cells[0].value)==((cells[hop+1].source_offset<<16)|colors[hop+1]));
                if(hop==0&&round%2!=0)chain_move(mapping,&cells[1],&pages[2],forward,access,colors[2],copy,publish,prepare);
                // The root, not a private destination, carries the value across
                // a transition between bounded forwarding invocations.
                assert(gc_context_transition_locked(&roots->ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
                assert(gc_context_transition_locked(&roots->ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
            }else{
                assert(root(&roots->ctx,&roots->request,GC_ROOT_READ_RAW)==GC_ROOT_OK);
                uint64_t reference=roots->request.value;
                ReferenceLease lease={.reference=reference};assert(acquire(&pool,&lease)==GC_ML_OK);
                ResolveRequest q={.reference=reference,.scope={.context=(uintptr_t)&roots->ctx}};
                assert(resolve((AllocationPage *)(uintptr_t)lease.lease.page,&q,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
                assert(gc_context_transition_locked(&roots->ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_ACTIVE_ACCESS);
                ObjectHeader *header=(ObjectHeader *)((char *)mapping+(reference>>16)-sizeof(ObjectHeader));
                assert(atomic_load_explicit(&header->state,memory_order_acquire)==GC_OBJECT_PUBLISHED);
                assert(header->payload_bytes==7&&header->mark_record==(uintptr_t)&record&&record.object_id==42&&record.old_epoch==round+1);
                assert(!memcmp(header+1,"payload",7));
                assert(resolve((AllocationPage *)(uintptr_t)lease.lease.page,&q,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
                assert(metadata(&pool,&lease.lease,GC_ML_RELEASE)==GC_ML_OK);
            }
            assert(!pool.active);
        }
        chain_pending_journal(mapping,pages,cells,&pool,&roots->ctx,&record,
            (cells[0].source_offset<<16)|colors[0],roots->request.value,pending_follow,pending_access,resolve,metadata,NULL);
        chain_pending_mark(mapping,pages,&pool,&record,(cells[0].source_offset<<16)|colors[0],
            roots->request.value,pending_mark,resolve,metadata);
        // The root followed both moves and its guarded header was checked
        // against the original stable record. Both old payloads are protected;
        // retain source/destination leases and exclusive intermediate metadata.
        ReferenceLease source_lease={.reference=(cells[0].source_offset<<16)|colors[0]};
        ReferenceLease destination_lease={.reference=roots->request.value};
        assert(acquire(&pool,&source_lease)==GC_ML_OK&&acquire(&pool,&destination_lease)==GC_ML_OK);
        RelocationGate frozen={.epoch=1,.closed=1}; // no copy/publication owner survives chain_move
        ForwardIncoming audit={pages[0].incarnation,1,pages[1].start,pages[1].limit,0,1,0,0};
        assert(incoming(&tables[0],&audit)==GC_FI_FOUND&&audit.source_offset==cells[0].source_offset);
        ForwardCompress shorten={(uintptr_t)&cells[0],cells[0].source_offset,cells[0].incarnation,
            (cells[1].source_offset<<16)|colors[1],roots->request.value,frozen.epoch};
        assert(compress(&frozen,&shorten)==GC_FC_OK);
        assert(compress(&frozen,&shorten)==GC_FC_RETRY); // stale exact-edge observation
        audit.cursor=audit.source_offset=0;
        assert(incoming(&tables[0],&audit)==GC_FI_DONE&&audit.cursor==1);
        ResolveRequest direct={.reference=source_lease.reference,.scope={.context=(uintptr_t)&roots->ctx}};
        assert(resolve(&pages[0],&direct,GC_RESOLVE_ENTER)==GC_RESOLVE_FORWARDED&&direct.destination==roots->request.value);
        assert(!direct.scope.held&&!atomic_load(&roots->ctx.access_epoch));
        assert(metadata(&pool,&source_lease.lease,GC_ML_RELEASE)==GC_ML_OK);
        assert(metadata(&pool,&destination_lease.lease,GC_ML_RELEASE)==GC_ML_OK);
        // The root journal can still contain intermediate references. A cleared
        // incoming table edge therefore does not authorize metadata/range reuse.
        assert(pages[1].forward_table==(uintptr_t)&tables[1]&&tables[1].used==1);
        assert(root(&roots->ctx,&roots->request,GC_ROOT_RELEASE)==GC_ROOT_OK);
        for(unsigned i=0;i<3;i++)assert(!pages[i].metadata_leases);
        free(roots);pool_vm_release(mapping,4*GC_HEAP_PAGE_BYTES);
    }
    puts("pending mark chain: 64 protected two-hop graphs, metadata pressure, both generation queues and duplicate cleanup before journal ACK PASS");
    puts("pending journal move race: 32 OS movers publish and protect after follow DONE, then consumers retry canonical access with retained claims PASS");
    puts("pending journal chain: 64 protected-source graphs retain copied items through root clear, metadata pressure and two-hop guarded handoff PASS");
    puts("forward compression chain: 64 protected two-hop sources shorten to rooted canonical identities and clear intermediate incoming edges PASS");
    puts("root follow: 64 protected-source two-hop roots and 16 journal-full retries PASS");
    puts("relocation target: 64 pool-backed two-hop destination installation rounds PASS");
    puts("resolve handoff: 16 full-journal, discard-before-poll and rooted-retry rounds PASS");
    puts("resolve chains: 64 protected-source, two-hop, rooted-handoff and guarded-destination rounds PASS");
}
#endif
