/* OS-backed guarded field -> root handoff. Roots/metadata remain owned until
 * fixture teardown; phase scheduling and color retirement are separate work. */
#ifndef KLASSIC_GC_BARRIER_LOAD_PIPELINE_H
#define KLASSIC_GC_BARRIER_LOAD_PIPELINE_H
#include "gc_barrier_load_cases.h"
#include "gc_resolve_chain.h"
#include "gc_mark_prepare_cases.h"
#include "gc_mark_scan_cases.h"
#include "gc_mark_cases.h"
#include "gc_root_copy.h"
#include "gc_rooted_observation.h"
#include "gc_rooted_mark.h"
#include "gc_rooted_mark_set.h"
#include "gc_rooted_mark_publish.h"
#include "gc_rooted_load.h"
#include "gc_rooted_load_admit.h"
#include "gc_rooted_store.h"
#include "gc_worker.h"
/* A second OS owner performs a logical store while RootedLoad retains its
 * receiver guard. The fixture holds strong roots and real metadata leases;
 * rendezvous spins below are deliberate test preemption, never GC polls. */
typedef struct {
    MutatorContext context;
    SourceAccessRequest scope;
    GuardedStore request;
    GuardedStorer store;
    SourceAccess access;
    ForwardCell *cell;
    _Atomic uint64_t ready,go,done;
    uint64_t result;
} RootedLoadStoreActor;
#ifdef _WIN32
static DWORD WINAPI gc_rooted_load_store_actor(void *argument)
#else
static void *gc_rooted_load_store_actor(void *argument)
#endif
{
    RootedLoadStoreActor *a=argument;uint64_t status;
    do{status=a->access(a->cell,&a->scope,GC_SA_ENTER);if(status==GC_SA_WAIT)metadata_yield();}while(status==GC_SA_WAIT);
    assert(status==GC_SA_OK);
    atomic_store_explicit(&a->ready,1,memory_order_release);
    while(!atomic_load_explicit(&a->go,memory_order_acquire)){}
    a->result=a->store(&a->request);
    do{status=a->access(a->cell,&a->scope,GC_SA_EXIT);}while(status==GC_SA_WAIT);
    assert(status==GC_SA_OK);
    atomic_store_explicit(&a->done,1,memory_order_release);return 0;
}
static void gc_test_rooted_mark_prepare(MetadataLeasePool *pool,RootedMark *q,RootedMarker mark_owner){
    assert(mark_owner(pool,q,GC_RM_ENTER)==GC_RAC_READY);
    assert(mark_owner(NULL,q,GC_RM_ROUTE)==GC_RM_EXIT_REQUIRED);
    assert(q->route_result==GC_ROUTE_READY);
    RootedMark pending=*q;
    assert(mark_owner(NULL,q,GC_RM_ROUTE)==GC_RM_EXIT_REQUIRED);
    assert(!memcmp(q,&pending,sizeof(*q)));
    q->access.access.scope.ticket=1;pending=*q;
    assert(mark_owner(NULL,q,GC_RM_EXIT)==GC_RM_EXIT_REQUIRED);
    assert(!memcmp(q,&pending,sizeof(*q)));
    q->access.access.scope.ticket=0;
    assert(mark_owner(NULL,q,GC_RM_EXIT)==GC_RM_OFFER_REQUIRED);
    assert(!q->access.access.scope.held&&q->route.prepare.output.metadata_lease==q->access.follow.cleanup.token);
}
static uint64_t gc_test_mark_set_duplicate_or_wait(MetadataLeasePool *pool,RootedMarkSet *q,RootedMarkSetter step){
    assert(step(pool,q,GC_RM_ENTER)==GC_RAC_READY);
    assert(step(NULL,q,GC_RM_ROUTE)==GC_RM_EXIT_REQUIRED);
    uint64_t result=step(NULL,q,GC_RM_EXIT);
    if(result==GC_RM_OFFER_REQUIRED)result=step(NULL,q,GC_RM_OFFER);
    assert(result==GC_RM_RELEASE_REQUIRED);
    return step(pool,q,GC_RM_RELEASE);
}
static uint64_t gc_test_rooted_load_drive(MetadataLeasePool *pool,RootedLoad *q,RootedLoader load_owner){
    for(unsigned steps=0;steps<96;steps++){
        uint64_t action=gc_rooted_load_next(q);
        if(action!=GC_RLOAD_HEAP&&action!=GC_RLOAD_ACCESS&&action!=GC_RLOAD_QUEUE)return action;
        MutatorContext *ctx=(void *)(uintptr_t)q->observation.receiver.follow.context;
        assert(!!atomic_load(&ctx->access_epoch)==(action==GC_RLOAD_ACCESS));
        RootedLoad saved=*q;
        uint64_t wrong=action==GC_RLOAD_ACCESS?GC_RLOAD_QUEUE:GC_RLOAD_ACCESS;
        assert(load_owner(NULL,q,wrong)==GC_RLOAD_INVALID);
        assert(!memcmp(q,&saved,sizeof(*q)));
        uint64_t result=load_owner(action==GC_RLOAD_HEAP?pool:NULL,q,action);
        assert(result!=GC_RLOAD_INVALID&&result!=GC_RLOAD_ERROR&&result!=GC_RLOAD_EXHAUSTED&&result!=GC_RLOAD_METADATA_FULL);
        if(result==GC_RLOAD_WAIT||result==GC_RLOAD_FLUSH)return result;
    }
    assert(!"rooted load owner exceeded finite fixture steps");return GC_RLOAD_INVALID;
}
static void gc_run_guarded_load_pipeline(MarkPreparer prepare,MarkStep mark,MarkScanner scan,RootFollower follow,GuardedLoader load,GuardedStorer store,PoolStep pool,TlabPage bind,TlabAllocator allocate,
        DescriptorRegister register_descriptor,ReferenceAcquirer acquire,MetadataStep metadata,Resolver resolve,
        ResolveRootStep root,ResolveRootDrain drain,ForwardStep forward,SourceAccess access,RelocateTarget target,RelocateCopy copy,RelocatePublish publish,
        uint64_t (*root_access)(MetadataLeasePool *,RootAccess *),uint64_t (*scalar)(GuardedScalar *,uint64_t),RootCopier root_copy,GuardedLogicalCas logical_cas,GuardedIdentity identity,GuardedObserver observe,RootAccessCleanup cleanup,RootedObserver observation_step,uint64_t (*spill)(MarkQueue *,MarkRequest *,uint64_t),RootedMarker mark_owner,RootedMarker spill_owner,RootedMarkSetter set_owner,RootedMarkPublisher publish_root,GuardedHealer heal,RootedLoader load_owner,uint64_t (*worker_step)(RootRegistry *,WorkerRequest *,uint64_t),RootedLoadAdmitter admit_load,RootedLoader value_owner,RootedStorer store_owner){
    for(unsigned round=0;round<64;round++){
        void *mapping=pool_vm_reserve(5*GC_HEAP_PAGE_BYTES);
        AllocationPage pages[4]={0};ForwardCell cells[6]={0};ForwardTable tables[4]={{(uintptr_t)&cells[0],1,0},{(uintptr_t)&cells[1],2,0},{(uintptr_t)&cells[3],1,0},{(uintptr_t)&cells[4],2,0}};
        _Atomic uint64_t starts[4][128]={0};MarkRecord records[3]={0};MarkArena arena={(uintptr_t)records,3,0};
        uint64_t native=sizeof(pages)+sizeof(tables)+sizeof(cells)+sizeof(records);
        HeapPool heap={.heap_base=(uintptr_t)mapping,.pages=(uintptr_t)pages,.page_count=4,.hard_max=4*GC_HEAP_PAGE_BYTES,
            .soft_max=2*GC_HEAP_PAGE_BYTES,.relocation_reserve=2*GC_HEAP_PAGE_BYTES,.metadata_committed=native,.metadata_max=native+sizeof(starts)};
        for(unsigned i=0;i<4;i++){
            PageLease q={.page_count=1,.relocation=i>=2};assert(pool(&heap,&q,GC_POOL_RESERVE)==GC_POOL_OK&&q.head_index==i);
            pool_vm_commit((char *)mapping+pages[i].start,GC_HEAP_PAGE_BYTES);q.starts=(uintptr_t)starts[i];q.committed_mapping=1;
            assert(pool(&heap,&q,GC_POOL_COMMIT)==GC_POOL_OK);pages[i].forward_table=(uintptr_t)&tables[i];
        }
        MetadataLeaseSlot slots[4]={{.next_free=2},{.next_free=3},{.next_free=4},{0}};
        MetadataLeasePool leases={(uintptr_t)&heap,(uintptr_t)slots,4,1,0};
        ChainRoots roots;chain_roots_init(&roots);RootCell root_cells[3]={0};roots.segment.cells=(uintptr_t)root_cells;roots.segment.capacity=3;
        assert(root(&roots.ctx,&roots.request,GC_ROOT_RESERVE)==GC_ROOT_OK);
        RootRequest output={.segment=(uintptr_t)&roots.segment};assert(root(&roots.ctx,&output,GC_ROOT_RESERVE)==GC_ROOT_OK);
        RootRequest keep_b={.segment=(uintptr_t)&roots.segment};assert(root(&roots.ctx,&keep_b,GC_ROOT_RESERVE)==GC_ROOT_OK);
        uint64_t bitmap=1;TypeDescriptor parent={.id=1,.alignment=16,.fixed_bytes=16,.fixed_bitmap=(uintptr_t)&bitmap,.fixed_bitmap_words=1};
        TypeDescriptor child={.id=2,.alignment=16,.fixed_bytes=7};_Atomic uint64_t descriptors[3]={0};DescriptorRegistry registry={(uintptr_t)descriptors,3};
        assert(register_descriptor(&registry,&parent)==GC_DESC_OK&&register_descriptor(&registry,&child)==GC_DESC_OK);
        AllocationResult objects[3];
        assert(bind(&roots.ctx,&pages[0],0)==GC_ALLOC_OK);
        AllocationRequest allocation={(uintptr_t)&pages[0],(uintptr_t)&parent,0,(uintptr_t)&objects[0],(uintptr_t)&arena};
        assert(allocate(&roots.ctx,&allocation)==GC_ALLOC_OK);assert(bind(&roots.ctx,&pages[0],1)==GC_ALLOC_OK);
        assert(bind(&roots.ctx,&pages[1],0)==GC_ALLOC_OK);allocation.page=(uintptr_t)&pages[1];allocation.descriptor=(uintptr_t)&child;
        for(unsigned i=1;i<3;i++){allocation.output=(uintptr_t)&objects[i];assert(allocate(&roots.ctx,&allocation)==GC_ALLOC_OK);
            ObjectHeader *h=(ObjectHeader *)((char *)mapping+(objects[i].reference>>16)-32);memcpy(h+1,i==1?"child-A":"child-B",7);atomic_store(&h->state,GC_OBJECT_PUBLISHED);}
        assert(bind(&roots.ctx,&pages[1],1)==GC_ALLOC_OK);
        ObjectHeader *parent_header=(ObjectHeader *)((char *)mapping+(objects[0].reference>>16)-32);
        uint64_t scalar_initial=UINT64_C(0x301510)|((uint64_t)(round+1)<<32);
        atomic_store((_Atomic uint64_t *)(parent_header+1)+1,scalar_initial);
        atomic_store((_Atomic uint64_t *)(parent_header+1),objects[1].reference);atomic_store(&parent_header->state,GC_OBJECT_PUBLISHED);
        keep_b.value=objects[2].reference;assert(root(&roots.ctx,&keep_b,GC_ROOT_STORE)==GC_ROOT_OK);
        roots.request.value=objects[0].reference;assert(root(&roots.ctx,&roots.request,GC_ROOT_STORE)==GC_ROOT_OK);
        if(round%2==0)for(unsigned i=0;i<5;i++)assert(root(&roots.ctx,&roots.request,GC_ROOT_STORE)==GC_ROOT_OK);
        ReferenceLease lease={.reference=objects[0].reference};assert(acquire(&leases,&lease)==GC_ML_OK);
        ResolveRequest guarded={.reference=objects[0].reference,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(resolve(&pages[0],&guarded,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        GuardedLoad request={.scope=(uintptr_t)&guarded.scope,.page=(uintptr_t)&pages[0],.registry=(uintptr_t)&registry,.output=output};
        if(round%2==0){
            assert(observe(&request)==GC_ROOT_FLUSH&&!atomic_load(&root_cells[1].value)&&!request.output.value);
            assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_ACTIVE_ACCESS);
            assert(resolve(&pages[0],&guarded,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);assert(metadata(&leases,&lease.lease,GC_ML_RELEASE)==GC_ML_OK);
            assert(drain(&roots.drain)==GC_ROOT_OK&&roots.drain.written==7);
            assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
            assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
            // Another logical store occurs before retry. Reacquire the receiver
            // from its root, then reread the slot instead of adopting stale A.
            roots.request.value=0;assert(root(&roots.ctx,&roots.request,GC_ROOT_READ_RAW)==GC_ROOT_OK);
            keep_b.value=0;assert(root(&roots.ctx,&keep_b,GC_ROOT_READ_RAW)==GC_ROOT_OK);
            lease=(ReferenceLease){.reference=roots.request.value};assert(acquire(&leases,&lease)==GC_ML_OK);
            guarded.reference=roots.request.value;assert(resolve(&pages[0],&guarded,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            GuardedStore update={.scope=(uintptr_t)&guarded.scope,.page=(uintptr_t)&pages[0],.registry=(uintptr_t)&registry,
                .expected=objects[1].reference,.desired=keep_b.value};
            assert(store(&update)==GC_STORE_OK&&!update.transferred); // Young owner, idle phase
        }
        assert(observe(&request)==GC_ROOT_OK);
        uint64_t exact_observation=request.output.value;
        uint64_t chosen=round%2==0?2:1;
        assert(atomic_load(&root_cells[1].value)==objects[chosen].reference&&exact_observation==objects[chosen].reference);
        assert(roots.entries[(roots.buffer.top-1)%7].next==objects[chosen].reference);
        assert(guarded.scope.held&&atomic_load(&roots.ctx.access_epoch)==pages[0].incarnation);
        ForwardRequest claim={cells[0].source_offset,cells[0].incarnation,1,0};assert(forward(&cells[0],&claim,GC_FWD_CLAIM)==GC_FWD_OK);
        SourceAccessRequest select={0,cells[0].source_offset,cells[0].incarnation,0,1};assert(access(&cells[0],&select,GC_SA_SELECT)==GC_SA_OK);
        RelocationCopy job={(uintptr_t)mapping,cells[0].incarnation,1,pages[0].limit,0,0,256,0,0};assert(target(&pages[2],&job,GC_TARGET_PREPARE)==GC_TARGET_OK);
        assert(copy(&cells[0],&job)==GC_COPY_WAIT);
        assert(resolve(&pages[0],&guarded,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        assert(copy(&cells[0],&job)==GC_COPY_DONE);assert(target(&pages[2],&job,GC_TARGET_INSTALL)==GC_TARGET_OK);
        assert(publish(&cells[0],&job,0x1510)==GC_COPY_DONE);
        pool_vm_decommit((char *)mapping+pages[0].start,GC_HEAP_PAGE_BYTES);
        assert(metadata(&leases,&lease.lease,GC_ML_RELEASE)==GC_ML_OK);
        assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
        assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
        // The original receiver page is protected. Follow its registered root
        // before touching any field, including a scalar with reference-shaped bits.
        RootAccess receiver={.follow={.context=(uintptr_t)&roots.ctx,.root=roots.request}};
        receiver.follow.root.value=0;
        if(round%2==0){
            while(roots.buffer.top-atomic_load(&roots.buffer.consumed)<roots.buffer.capacity){
                roots.request.value=objects[0].reference;
                assert(root(&roots.ctx,&roots.request,GC_ROOT_STORE)==GC_ROOT_OK);
            }
            RootAccess unchanged=receiver;
            assert(root_access(&leases,&receiver)==GC_RF_FLUSH&&!memcmp(&receiver,&unchanged,sizeof(receiver)));
            assert(atomic_load(&root_cells[0].value)==objects[0].reference&&!leases.active&&!atomic_load(&roots.ctx.access_epoch));
            assert(drain(&roots.drain)==GC_ROOT_OK&&roots.drain.written==7);
        }
        assert(root_access(&leases,&receiver)==GC_RF_ADVANCED&&!leases.active&&!receiver.follow.cleanup.token);
        assert(atomic_load(&root_cells[0].value)==((cells[3].source_offset<<16)|UINT64_C(0x1510)));
        assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
        assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
        assert(root_access(&leases,&receiver)==GC_RAC_READY);
        assert(receiver.follow.cleanup.page==(uintptr_t)&pages[2]&&receiver.access.scope.held==(uintptr_t)&cells[3]);
        GuardedScalar value={(uintptr_t)&receiver.access.scope,receiver.follow.cleanup.page,(uintptr_t)&registry,8,0,0};
        assert(scalar(&value,GC_SCALAR_LOAD)==GC_SCALAR_OK&&value.value==scalar_initial);
        value.value=UINT64_MAX-round;
        assert(scalar(&value,GC_SCALAR_STORE)==GC_SCALAR_OK);
        value.value=0;assert(scalar(&value,GC_SCALAR_LOAD)==GC_SCALAR_OK&&value.value==UINT64_MAX-round);
        ObjectHeader *canonical=(void *)((char *)mapping+(receiver.access.reference>>16)-sizeof(ObjectHeader));
        assert(canonical->mark_record==(uintptr_t)&records[0]&&records[0].object_id==objects[0].object_id);
        assert(atomic_load((_Atomic uint64_t *)(canonical+1))==objects[chosen].reference);
        value.byte_offset=0;GuardedScalar untouched=value;
        assert(scalar(&value,GC_SCALAR_STORE)==GC_SCALAR_INVALID&&!memcmp(&value,&untouched,sizeof(value)));
        assert(resolve(&pages[2],&receiver.access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        assert(metadata(&leases,&receiver.follow.cleanup,GC_ML_RELEASE)==GC_ML_OK&&!leases.active&&!atomic_load(&roots.ctx.access_epoch));
        assert(drain(&roots.drain)==GC_ROOT_OK);
        // Keep the source and destination handles, not a sampled reference,
        // across journal pressure and relocation. Scratch values are deliberately
        // stale; root_copy must read the source cell again on every attempt.
        RootRequest copy_source=output,copy_destination=keep_b;
        copy_source.value=UINT64_MAX;copy_destination.value=111;
        while(roots.buffer.top-atomic_load(&roots.buffer.consumed)<roots.buffer.capacity)
            assert(root_copy(&roots.ctx,&copy_source,&copy_source)==GC_ROOT_OK);
        RootRequest saved_source=copy_source,saved_destination=copy_destination;
        RootCell saved_cells[3];memcpy(saved_cells,root_cells,sizeof(root_cells));
        BufferCursor saved_buffer=roots.buffer;RootChange saved_entries[7];memcpy(saved_entries,roots.entries,sizeof(saved_entries));
        uint64_t saved_revision=atomic_load(&roots.ctx.root_revision);
        assert(root_copy(&roots.ctx,&copy_source,&copy_destination)==GC_ROOT_FLUSH);
        assert(!memcmp(root_cells,saved_cells,sizeof(root_cells))&&!memcmp(&roots.buffer,&saved_buffer,sizeof(saved_buffer)));
        assert(!memcmp(roots.entries,saved_entries,sizeof(saved_entries))&&atomic_load(&roots.ctx.root_revision)==saved_revision);
        RootFollow follower={.context=(uintptr_t)&roots.ctx,.root=output};
        for(unsigned i=1;i<3;i++){
            ForwardRequest child_claim={cells[i].source_offset,cells[i].incarnation,1,0};
            assert(forward(&cells[i],&child_claim,GC_FWD_CLAIM)==GC_FWD_OK);
            if(i==chosen){
                assert(follow(&leases,&follower,GC_RF_STEP)==GC_RF_WAIT&&!leases.active&&!follower.cleanup.token);
                assert(atomic_load(&root_cells[1].value)==objects[chosen].reference);
                assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
                assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
            }
            SourceAccessRequest child_select={0,cells[i].source_offset,cells[i].incarnation,0,1};
            assert(access(&cells[i],&child_select,GC_SA_SELECT)==GC_SA_OK);
            RelocationCopy child_job={(uintptr_t)mapping,cells[i].incarnation,1,pages[1].limit,0,0,256,0,0};
            assert(target(&pages[3],&child_job,GC_TARGET_PREPARE)==GC_TARGET_OK);
            assert(copy(&cells[i],&child_job)==GC_COPY_DONE);
            assert(target(&pages[3],&child_job,GC_TARGET_INSTALL)==GC_TARGET_OK);
            assert(publish(&cells[i],&child_job,0x8630)==GC_COPY_DONE);
        }
        pool_vm_decommit((char *)mapping+pages[1].start,GC_HEAP_PAGE_BYTES);
        // Neither failed copy nor root following may dereference protected
        // source payload. Journal holders still retain the forwarding metadata.
        assert(root_copy(&roots.ctx,&copy_source,&copy_destination)==GC_ROOT_FLUSH);
        assert(follow(&leases,&follower,GC_RF_STEP)==GC_RF_FLUSH&&!leases.active&&!follower.cleanup.token);
        assert(!memcmp(root_cells,saved_cells,sizeof(root_cells))&&!memcmp(&roots.buffer,&saved_buffer,sizeof(saved_buffer)));
        assert(!memcmp(roots.entries,saved_entries,sizeof(saved_entries))&&atomic_load(&roots.ctx.root_revision)==saved_revision);
        assert(drain(&roots.drain)==GC_ROOT_OK&&roots.drain.written==7);
        assert(follow(&leases,&follower,GC_RF_STEP)==GC_RF_ADVANCED&&!leases.active&&!follower.cleanup.token);
        uint64_t canonical_child=(cells[chosen+3].source_offset<<16)|UINT64_C(0x8630);
        assert(root_copy(&roots.ctx,&copy_source,&copy_destination)==GC_ROOT_OK);
        assert(atomic_load(&root_cells[2].value)==canonical_child);
        assert(!memcmp(&copy_source,&saved_source,sizeof(copy_source))&&!memcmp(&copy_destination,&saved_destination,sizeof(copy_destination)));
        RootChange overwritten=roots.entries[(roots.buffer.top-1)%roots.buffer.capacity];
        assert(overwritten.previous==objects[2].reference&&overwritten.next==canonical_child);
        // Resolve the overwritten B through its retained journal reference,
        // even when the destination root now names A and B's source is unreadable.
        ReferenceLease prior_lease={.reference=overwritten.previous};
        assert(acquire(&leases,&prior_lease)==GC_ML_OK);
        ForwardRequest prior={overwritten.previous>>16,prior_lease.lease.incarnation,0,0};
        assert(forward(&cells[2],&prior,GC_FWD_READ)==GC_FWD_OK);
        ResolveRequest prior_access={.reference=prior.destination,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(resolve(&pages[3],&prior_access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        ObjectHeader *prior_header=(void *)((char *)mapping+(prior.destination>>16)-sizeof(ObjectHeader));
        assert(prior_header->mark_record==(uintptr_t)&records[2]&&records[2].object_id==objects[2].object_id);
        assert(!memcmp(prior_header+1,"child-B",7));
        assert(resolve(&pages[3],&prior_access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        assert(metadata(&leases,&prior_lease.lease,GC_ML_RELEASE)==GC_ML_OK&&!leases.active);
        assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
        assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
        assert(follow(&leases,&follower,GC_RF_STEP)==GC_RF_DONE);
        assert(root(&roots.ctx,&output,GC_ROOT_READ_RAW)==GC_ROOT_OK);
        assert(output.value==((cells[chosen+3].source_offset<<16)|UINT64_C(0x8630)));
        ReferenceLease child_lease={.reference=output.value};assert(acquire(&leases,&child_lease)==GC_ML_OK);
        ResolveRequest child_access={.reference=output.value,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(resolve(&pages[3],&child_access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        ObjectHeader *h=(ObjectHeader *)((char *)mapping+(output.value>>16)-32);
        assert(h->mark_record==(uintptr_t)&records[chosen]&&records[chosen].object_id==objects[chosen].object_id);
        assert(!memcmp(h+1,chosen==1?"child-A":"child-B",7));
        // The rooted observation came from the receiver before relocation;
        // its exact field bits still name an inaccessible source page. Obtain
        // identity from its canonical, guarded copy rather than that old payload.
        const MarkRecord *observed_record=identity((uintptr_t)&child_access.scope,(uintptr_t)&pages[3],atomic_load(&roots.ctx.adopted_phase));
        assert(observed_record==&records[chosen]);
        assert(resolve(&pages[3],&child_access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        RootAccess expected_access={.follow={.context=(uintptr_t)&roots.ctx,.root=keep_b}};
        expected_access.follow.root.value=0;
        assert(root_access(&leases,&expected_access)==GC_RAC_READY);
        const MarkRecord *expected_record=identity((uintptr_t)&expected_access.access.scope,expected_access.follow.cleanup.page,atomic_load(&roots.ctx.adopted_phase));
        assert(expected_record==observed_record);
        assert(expected_access.access.reference!=objects[chosen].reference);
        assert((expected_access.access.reference>>16)!=(objects[chosen].reference>>16));
        assert((uint16_t)expected_access.access.reference!=(uint16_t)objects[chosen].reference);
        assert(resolve(&pages[3],&expected_access.access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        assert(metadata(&leases,&expected_access.follow.cleanup,GC_ML_RELEASE)==GC_ML_OK);
        // Both referents remain strongly rooted, and the original child lease
        // protects their shared record while acquiring the receiver's guard.
        // Force pressure precisely at the fresh-observation step after RETRY.
        // Both identity roots and the record lease survive servicing that pressure.
        while(roots.buffer.top-atomic_load(&roots.buffer.consumed)<roots.buffer.capacity)
            assert(root_copy(&roots.ctx,&keep_b,&keep_b)==GC_ROOT_OK);
        RootAccess cas_receiver={.follow={.context=(uintptr_t)&roots.ctx,.root=roots.request}};
        cas_receiver.follow.root.value=0;
        assert(root_access(&leases,&cas_receiver)==GC_RAC_READY);
        GuardedStore exchange={.scope=(uintptr_t)&cas_receiver.access.scope,.page=cas_receiver.follow.cleanup.page,
            .registry=(uintptr_t)&registry,.expected=exact_observation,.desired=0,
            .phase=atomic_load(&roots.ctx.adopted_phase)};
        assert(logical_cas(&exchange,expected_record,observed_record)==GC_STORE_OK&&!exchange.transferred);
        ObjectHeader *receiver_header=(void *)((char *)mapping+(cas_receiver.access.reference>>16)-sizeof(ObjectHeader));
        assert(!atomic_load((_Atomic uint64_t *)(receiver_header+1)));
        GuardedStore completed=exchange;
        assert(logical_cas(&exchange,expected_record,observed_record)==GC_STORE_RETRY);
        assert(!memcmp(&exchange,&completed,sizeof(exchange))&&!atomic_load((_Atomic uint64_t *)(receiver_header+1)));
        assert(atomic_load(&root_cells[1].value)==canonical_child&&atomic_load(&root_cells[2].value)==canonical_child);
        GuardedLoad retry_load={.scope=(uintptr_t)&cas_receiver.access.scope,
            .page=cas_receiver.follow.cleanup.page,.registry=(uintptr_t)&registry,.output=output,
            .phase=atomic_load(&roots.ctx.adopted_phase)};
        retry_load.output.value=0;
        uint64_t before_revision=atomic_load(&roots.ctx.root_revision);
        assert(load(&retry_load)==GC_ROOT_FLUSH);
        assert(atomic_load(&root_cells[1].value)==canonical_child&&atomic_load(&roots.ctx.root_revision)==before_revision);
        assert(cleanup(NULL,&cas_receiver,GC_RAC_EXIT)==GC_RF_DONE);
        assert(cleanup(&leases,&cas_receiver,GC_RAC_RELEASE)==GC_RF_DONE&&leases.active==1);
        assert(!atomic_load(&roots.ctx.access_epoch));
        assert(drain(&roots.drain)==GC_ROOT_OK&&roots.drain.written==7);
        assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_OK);
        assert(gc_context_transition_locked(&roots.ctx,GC_BLOCKED_NATIVE,GC_RUNNING)==GC_CONTEXT_OK);
        cas_receiver=(RootAccess){.follow={.context=(uintptr_t)&roots.ctx,.root=roots.request}};
        cas_receiver.follow.root.value=0;
        assert(root_access(&leases,&cas_receiver)==GC_RAC_READY);
        retry_load.scope=(uintptr_t)&cas_receiver.access.scope;retry_load.page=cas_receiver.follow.cleanup.page;
        retry_load.phase=atomic_load(&roots.ctx.adopted_phase);
        assert(load(&retry_load)==GC_ROOT_OK&&!atomic_load(&root_cells[1].value));
        assert(roots.entries[(roots.buffer.top-1)%7].previous==canonical_child&&
            roots.entries[(roots.buffer.top-1)%7].next==0);
        // A newly rooted null has no identity record. Keeping the previous
        // observation's record would incorrectly permit this logical comparison.
        exchange=(GuardedStore){.scope=retry_load.scope,.page=retry_load.page,
            .registry=(uintptr_t)&registry,.phase=retry_load.phase};
        completed=exchange;
        assert(logical_cas(&exchange,expected_record,NULL)==GC_STORE_MISMATCH);
        assert(!memcmp(&exchange,&completed,sizeof(exchange)));
        // Null compared with null succeeds using the same exact observation.
        assert(logical_cas(&exchange,NULL,NULL)==GC_STORE_OK&&!exchange.transferred);
        assert(!atomic_load(&root_cells[1].value)&&atomic_load(&root_cells[2].value)==canonical_child);
        assert(cleanup(NULL,&cas_receiver,GC_RAC_EXIT)==GC_RF_DONE);
        assert(cleanup(&leases,&cas_receiver,GC_RAC_RELEASE)==GC_RF_DONE&&leases.active==1);
        assert(root_copy(&roots.ctx,&keep_b,&output)==GC_ROOT_OK);
        assert(root(&roots.ctx,&output,GC_ROOT_READ_RAW)==GC_ROOT_OK&&output.value==canonical_child);
        child_access=(ResolveRequest){.reference=output.value,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(resolve(&pages[3],&child_access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        GuardedMarkPrepare pending={.scope=(uintptr_t)&child_access.scope,.page=(uintptr_t)&pages[3],
            .reference=output.value,.lease=child_lease.lease};child_lease.lease=(MetadataLeaseRequest){0};
        assert(prepare(&pending)==GC_MARK_OK&&!pending.lease.token&&pending.output.record==(uintptr_t)&records[chosen]);
        assert(child_access.scope.held&&atomic_load(&roots.ctx.access_epoch));
        assert(resolve(&pages[3],&child_access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        MarkTask task={0};MarkQueue queue={.tasks=(uintptr_t)&task,.capacity=1,.free_head=1,.epoch=round+1,.generation=round%2};
        assert(mark(&queue,&pending.output,GC_MARK_OFFER)==GC_MARK_OK&&!pending.output.metadata_lease&&leases.active==1);
        assert((queue.generation?records[chosen].old_epoch:records[chosen].young_epoch)==round+1);
        assert((queue.generation?records[chosen].young_epoch:records[chosen].old_epoch)==0);
        // A duplicate offer has its own real lease. Failure to transfer keeps
        // that obligation local even while the first task owns another token.
        child_lease=(ReferenceLease){.reference=output.value};assert(acquire(&leases,&child_lease)==GC_ML_OK);
        child_access=(ResolveRequest){.reference=output.value,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(resolve(&pages[3],&child_access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        pending=(GuardedMarkPrepare){.scope=(uintptr_t)&child_access.scope,.page=(uintptr_t)&pages[3],
            .reference=output.value,.lease=child_lease.lease};child_lease.lease=(MetadataLeaseRequest){0};
        assert(prepare(&pending)==GC_MARK_OK);
        assert(resolve(&pages[3],&child_access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        assert(mark(&queue,&pending.output,GC_MARK_OFFER)==GC_MARK_DUPLICATE&&pending.output.metadata_lease&&leases.active==2);
        MetadataLeaseRequest release={.token=pending.output.metadata_lease};
        assert(metadata(&leases,&release,GC_ML_RESOLVE)==GC_ML_OK&&release.page==(uintptr_t)&pages[3]);
        assert(metadata(&leases,&release,GC_ML_RELEASE)==GC_ML_OK);pending.output=(MarkRequest){0};
        MarkRequest work={0};assert(mark(&queue,&work,GC_MARK_TAKE)==GC_MARK_OK);
        release=(MetadataLeaseRequest){.token=work.metadata_lease};assert(metadata(&leases,&release,GC_ML_RESOLVE)==GC_ML_OK);
        child_access=(ResolveRequest){.reference=work.reference,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(resolve((AllocationPage *)(uintptr_t)release.page,&child_access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        uint64_t child_out=0;MarkScan slice={(uintptr_t)&work,(uintptr_t)mapping,(uintptr_t)&registry,(uintptr_t)&child_out,1,0,0,0,0};
        assert(scan(&slice)==GC_SCAN_DONE&&!slice.written);
        assert(resolve((AllocationPage *)(uintptr_t)release.page,&child_access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        work.cursor=slice.next_cursor;assert(mark(&queue,&work,GC_MARK_FINISH)==GC_MARK_OK);
        assert(metadata(&leases,&release,GC_ML_RELEASE)==GC_ML_OK&&!leases.active);
        work=(MarkRequest){0};assert(mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
        // Exercise the owning protocol against the relocated receiver, whose
        // original payload is inaccessible. Journal pressure must be returned
        // only after EXIT and metadata release; RESET causes a fresh read.
        RootedObservation observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=roots.request},
            .registry=(uintptr_t)&registry,.output=output};
        observation.receiver.follow.root.value=0;observation.output.value=0;
        RootRequest saved_output=observation.output;
        observation.output=observation.receiver.follow.root;
        RootedObservation rejected_observation=observation;
        assert(observation_step(&leases,&observation,GC_RO_ENTER)==GC_RF_INVALID);
        assert(!memcmp(&observation,&rejected_observation,sizeof(observation))&&!leases.active);
        observation.output=saved_output;
        assert(drain(&roots.drain)==GC_ROOT_OK);
        while(roots.buffer.top-atomic_load(&roots.buffer.consumed)<roots.buffer.capacity)
            assert(root_copy(&roots.ctx,&keep_b,&keep_b)==GC_ROOT_OK);
        // The earlier forwarding fixture deliberately uses stale colors.
        // This idle-phase logical store uses the current color of that rooted
        // canonical child, as required by the existing store precondition.
        uint64_t stored_child=(canonical_child&~UINT64_C(0xffff))|
            gc_abi_color((PhaseSnapshot){atomic_load(&roots.ctx.adopted_phase)});
        for(unsigned attempt=0;attempt<2;attempt++){
            uint64_t revision=atomic_load(&roots.ctx.root_revision);
            assert(observation_step(&leases,&observation,GC_RO_ENTER)==GC_RAC_READY);
            assert(observation_step(NULL,&observation,GC_RO_OBSERVE)==GC_RO_EXIT_REQUIRED);
            RootedObservation pending_observation=observation;
            assert(observation_step(NULL,&observation,GC_RO_OBSERVE)==GC_RO_EXIT_REQUIRED);
            assert(!memcmp(&observation,&pending_observation,sizeof(observation)));
            assert(atomic_load(&roots.ctx.root_revision)==revision+attempt);
            assert(observation_step(&leases,&observation,GC_RO_RELEASE)==GC_RO_EXIT_REQUIRED);
            // Invalid cleanup control must retain the exact pending read and
            // guard. Restore the injected fault, then retry EXIT without a poll.
            observation.receiver.access.scope.ticket=1;
            pending_observation=observation;
            assert(observation_step(NULL,&observation,GC_RO_EXIT)==GC_RO_EXIT_REQUIRED);
            assert(!memcmp(&observation,&pending_observation,sizeof(observation))&&atomic_load(&roots.ctx.access_epoch));
            observation.receiver.access.scope.ticket=0;
            assert(observation_step(NULL,&observation,GC_RO_EXIT)==GC_RO_RELEASE_REQUIRED);
            assert(!atomic_load(&roots.ctx.access_epoch)&&leases.active==1);
            uint64_t free_head=leases.free_head;leases.free_head=leases.capacity+1;
            pending_observation=observation;
            assert(observation_step(&leases,&observation,GC_RO_RELEASE)==GC_RF_CLEANUP_REQUIRED);
            assert(!memcmp(&observation,&pending_observation,sizeof(observation))&&leases.active==1);
            leases.free_head=free_head;
            assert(observation_step(&leases,&observation,GC_RO_RELEASE)==GC_RO_COMPLETE);
            assert(!leases.active&&observation.result==(attempt?GC_ROOT_OK:GC_ROOT_FLUSH));
            assert(observation.output.value==(attempt?stored_child:0));
            pending_observation=observation;
            assert(observation_step(&leases,&observation,GC_RO_ENTER)==GC_RO_COMPLETE);
            assert(!memcmp(&observation,&pending_observation,sizeof(observation)));
            assert(observation_step(&leases,&observation,GC_RO_RESET)==GC_RF_DONE);
            assert(drain(&roots.drain)==GC_ROOT_OK);
            if(!attempt){
                // Change the field after failed observation and before retry.
                // The controller must not retain its previous sampled null.
                RootAccess writer={.follow={.context=(uintptr_t)&roots.ctx,.root=roots.request}};
                writer.follow.root.value=0;
                assert(root_access(&leases,&writer)==GC_RAC_READY);
                GuardedStore change={.scope=(uintptr_t)&writer.access.scope,.page=writer.follow.cleanup.page,
                    .registry=(uintptr_t)&registry,.desired=stored_child,
                    .phase=atomic_load(&roots.ctx.adopted_phase)};
                assert(store(&change)==GC_STORE_OK&&!change.transferred);
                assert(cleanup(NULL,&writer,GC_RAC_EXIT)==GC_RF_DONE);
                assert(cleanup(&leases,&writer,GC_RAC_RELEASE)==GC_RF_DONE&&!leases.active);
            }
        }
        // Route the actual observation output through both generation choices.
        // This exclusive fixture holds its roots/journals and pending producer
        // until every queued task is drained; no termination is inferred here.
        atomic_store(&roots.ctx.adopted_phase,0x22);
        task=(MarkTask){0};queue=(MarkQueue){.tasks=(uintptr_t)&task,.capacity=1,.free_head=1,
            .epoch=round+2,.generation=round%2};
        RootedMark parent_mark={.access.follow={.context=(uintptr_t)&roots.ctx,.root=roots.request},
            .queue=(uintptr_t)&queue,.generation=queue.generation,.epoch=queue.epoch,.phase=0x22};
        parent_mark.access.follow.root.value=0;
        gc_test_rooted_mark_prepare(&leases,&parent_mark,mark_owner);
        queue.last_serial=UINT64_MAX;
        assert(mark_owner(NULL,&parent_mark,GC_RM_OFFER)==GC_RM_EXHAUSTED);
        assert(parent_mark.route.prepare.output.metadata_lease&&leases.active==1);
        queue.last_serial=0;
        assert(mark_owner(NULL,&parent_mark,GC_RM_OFFER)==GC_RM_COMPLETE);
        assert(!parent_mark.access.follow.cleanup.token&&!parent_mark.route.prepare.output.metadata_lease&&leases.active==1);
        RootedMark loaded_mark={.access.follow={.context=(uintptr_t)&roots.ctx,.root=observation.output},
            .queue=(uintptr_t)&queue,.generation=queue.generation,.epoch=queue.epoch,.phase=0x22};
        loaded_mark.access.follow.root.value=0;
        gc_test_rooted_mark_prepare(&leases,&loaded_mark,mark_owner);
        assert(loaded_mark.route.prepare.output.record==(uintptr_t)&records[chosen]);
        assert(loaded_mark.route.prepare.output.reference==stored_child&&!atomic_load(&roots.ctx.access_epoch));
        assert(mark_owner(NULL,&loaded_mark,GC_RM_OFFER)==GC_RM_FULL&&leases.active==2);
        RootedMark pending_mark=loaded_mark;
        assert(mark_owner(NULL,&loaded_mark,GC_RM_OFFER)==GC_RM_FULL);
        assert(!memcmp(&loaded_mark,&pending_mark,sizeof(loaded_mark)));
        // Admit the pending child into counted overflow BEFORE advancing the
        // parent's scan. A merely local pending child is not a completed offer.
        assert(spill_owner(NULL,&loaded_mark,GC_RM_OFFER)==GC_RM_COMPLETE);
        assert(queue.overflow_count==1&&leases.active==2&&!loaded_mark.access.follow.cleanup.token);
        // Scan the blocking parent after its child has been offered.
        work=(MarkRequest){0};assert(mark(&queue,&work,GC_MARK_TAKE)==GC_MARK_OK);
        release=(MetadataLeaseRequest){.token=work.metadata_lease};assert(metadata(&leases,&release,GC_ML_RESOLVE)==GC_ML_OK);
        ResolveRequest parent_scan={.reference=work.reference,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(resolve((AllocationPage *)(uintptr_t)release.page,&parent_scan,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        uint64_t scanned_child=0;
        slice=(MarkScan){(uintptr_t)&work,(uintptr_t)mapping,(uintptr_t)&registry,(uintptr_t)&scanned_child,1,0,0,0,0};
        uint64_t scan_status=scan(&slice);
        assert((scan_status==GC_SCAN_MORE||scan_status==GC_SCAN_DONE)&&slice.written==1&&scanned_child==stored_child);
        if(scan_status==GC_SCAN_MORE){work.cursor=slice.next_cursor;slice.written=0;
            assert(scan(&slice)==GC_SCAN_DONE&&!slice.written);}
        assert(resolve((AllocationPage *)(uintptr_t)release.page,&parent_scan,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        work.cursor=slice.next_cursor;assert(mark(&queue,&work,GC_MARK_FINISH)==GC_MARK_OK);
        assert(metadata(&leases,&release,GC_ML_RELEASE)==GC_ML_OK&&leases.active==1);
        assert(spill(&queue,NULL,GC_MARK_REFILL)==GC_MARK_OK&&queue.queued==1&&!queue.overflow_count);
        assert(loaded_mark.stage==5&&!loaded_mark.access.follow.cleanup.token&&loaded_mark.offer_result==GC_MARK_OK);
        assert((queue.generation?records[chosen].old_epoch:records[chosen].young_epoch)==queue.epoch);
        assert(mark_owner(&leases,&loaded_mark,GC_RM_RESET)==GC_RF_DONE);
        gc_test_rooted_mark_prepare(&leases,&loaded_mark,mark_owner);
        assert(mark_owner(NULL,&loaded_mark,GC_RM_OFFER)==GC_RM_RELEASE_REQUIRED);
        assert(loaded_mark.offer_result==GC_MARK_DUPLICATE&&leases.active==2);
        uint64_t free_head=leases.free_head;leases.free_head=leases.capacity+1;pending_mark=loaded_mark;
        assert(mark_owner(&leases,&loaded_mark,GC_RM_RELEASE)==GC_RF_CLEANUP_REQUIRED);
        assert(!memcmp(&loaded_mark,&pending_mark,sizeof(loaded_mark))&&leases.active==2);
        leases.free_head=free_head;
        assert(mark_owner(&leases,&loaded_mark,GC_RM_RELEASE)==GC_RM_COMPLETE&&leases.active==1);
        assert(mark_owner(&leases,&loaded_mark,GC_RM_RESET)==GC_RF_DONE);
        gc_test_rooted_mark_prepare(&leases,&loaded_mark,mark_owner);
        queue.epoch++;pending_mark=loaded_mark;
        assert(mark_owner(NULL,&loaded_mark,GC_RM_OFFER)==GC_RM_STALE);
        assert(!memcmp(&loaded_mark,&pending_mark,sizeof(loaded_mark))&&leases.active==2);
        assert(mark_owner(&leases,&loaded_mark,GC_RM_CANCEL)==GC_RM_CANCELLED&&leases.active==1);
        queue.epoch--;
        work=(MarkRequest){0};assert(mark(&queue,&work,GC_MARK_TAKE)==GC_MARK_OK);
        release=(MetadataLeaseRequest){.token=work.metadata_lease};assert(metadata(&leases,&release,GC_ML_RESOLVE)==GC_ML_OK);
        child_access=(ResolveRequest){.reference=work.reference,.scope={.context=(uintptr_t)&roots.ctx}};
        assert(resolve((AllocationPage *)(uintptr_t)release.page,&child_access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
        slice=(MarkScan){(uintptr_t)&work,(uintptr_t)mapping,(uintptr_t)&registry,(uintptr_t)&child_out,1,0,0,0,0};
        assert(scan(&slice)==GC_SCAN_DONE&&!slice.written);
        assert(resolve((AllocationPage *)(uintptr_t)release.page,&child_access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
        work.cursor=slice.next_cursor;assert(mark(&queue,&work,GC_MARK_FINISH)==GC_MARK_OK);
        assert(metadata(&leases,&release,GC_ML_RELEASE)==GC_ML_OK&&!leases.active);
        assert(mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
        // Fault/eligibility fixtures: skip canonical Old in a Young pass,
        // retain INITIALIZING work for retry, and reject a stale captured phase.
        // No cycle completion is claimed for any of these cleaned results.
        for(unsigned rejected=0;rejected<3;rejected++){
            assert(mark_owner(&leases,&loaded_mark,GC_RM_RESET)==GC_RF_DONE);
            uint64_t saved_generation=pages[3].generation;
            ObjectHeader *loaded_header=(void *)((char *)mapping+(stored_child>>16)-sizeof(ObjectHeader));
            uint64_t saved_state=atomic_load(&loaded_header->state);
            queue.generation=rejected?round%2:GC_YOUNG;
            loaded_mark.generation=queue.generation;loaded_mark.phase=rejected==2?0x33:0x22;
            if(!rejected)pages[3].generation=GC_OLD;
            if(rejected==1)atomic_store(&loaded_header->state,GC_OBJECT_INITIALIZING);
            assert(mark_owner(&leases,&loaded_mark,GC_RM_ENTER)==GC_RAC_READY);
            assert(mark_owner(NULL,&loaded_mark,GC_RM_ROUTE)==GC_RM_EXIT_REQUIRED);
            assert(loaded_mark.route_result==(rejected==0?GC_ROUTE_SKIP:rejected==1?GC_ROUTE_WAIT:GC_ROUTE_INVALID));
            assert(mark_owner(NULL,&loaded_mark,GC_RM_EXIT)==GC_RM_RELEASE_REQUIRED);
            assert(mark_owner(&leases,&loaded_mark,GC_RM_RELEASE)==GC_RM_COMPLETE&&!leases.active);
            assert(mark(&queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
            pages[3].generation=saved_generation;atomic_store(&loaded_header->state,saved_state);
        }
        assert(mark_owner(&leases,&loaded_mark,GC_RM_RESET)==GC_RF_DONE);
        loaded_mark.phase=0x22;
        output.value=0;assert(root(&roots.ctx,&output,GC_ROOT_STORE)==GC_ROOT_OK);
        assert(mark_owner(&leases,&loaded_mark,GC_RM_ENTER)==GC_RM_COMPLETE);
        assert(loaded_mark.route_result==GC_ROUTE_SKIP&&!leases.active);
        // The same observed output must satisfy BOTH concurrent generation
        // marks. Preserve Old in-flight work across a snapshot restart and the
        // complete Young handoff/scan; cancellation cannot revoke queue work.
        assert(drain(&roots.drain)==GC_ROOT_OK);
        assert(observation_step(&leases,&observation,GC_RO_ENTER)==GC_RAC_READY);
        assert(observation_step(NULL,&observation,GC_RO_OBSERVE)==GC_RO_EXIT_REQUIRED);
        assert(observation_step(NULL,&observation,GC_RO_EXIT)==GC_RO_RELEASE_REQUIRED);
        assert(observation_step(&leases,&observation,GC_RO_RELEASE)==GC_RO_COMPLETE);
        assert(observation.result==GC_ROOT_OK&&observation.output.value==stored_child&&!leases.active);
        MarkTask paired_tasks[2]={{0}};MarkQueue paired_queues[2];
        for(unsigned generation=0;generation<2;generation++)paired_queues[generation]=(MarkQueue){
            .tasks=(uintptr_t)&paired_tasks[generation],.capacity=1,.free_head=1,.epoch=round+3,.generation=generation};
        RootedMarkSet marks={.context=(uintptr_t)&roots.ctx,.root=observation.output,
            .queues={(uintptr_t)&paired_queues[0],(uintptr_t)&paired_queues[1]},
            .epochs={round+3,round+3},.phase=0x22};marks.root.value=0;
        assert(set_owner(&leases,&marks,GC_MS_INIT)==GC_MS_NEXT);
        assert(marks.required==3&&marks.current==GC_OLD&&!marks.completed);
        for(unsigned pass=0;pass<3;pass++){
            assert(set_owner(&leases,&marks,GC_RM_ENTER)==GC_RAC_READY);
            assert(set_owner(NULL,&marks,GC_RM_ROUTE)==GC_RM_EXIT_REQUIRED);
            assert(set_owner(NULL,&marks,GC_RM_EXIT)==GC_RM_OFFER_REQUIRED);
            uint64_t offered=set_owner(NULL,&marks,GC_RM_OFFER);
            if(pass==1){
                assert(offered==GC_RM_RELEASE_REQUIRED&&marks.current==GC_OLD);
                offered=set_owner(&leases,&marks,GC_RM_RELEASE);
            }
            assert(offered==(pass==2?GC_MS_DONE:GC_MS_NEXT));
            if(!pass){
                assert(marks.completed==2&&marks.current==GC_YOUNG&&leases.active==1);
                work=(MarkRequest){0};assert(mark(&paired_queues[1],&work,GC_MARK_TAKE)==GC_MARK_OK);
                // A new adopted snapshot invalidates the partial receipt. It
                // leaves the Old task and root protected for a fresh pass.
                atomic_store(&roots.ctx.adopted_phase,0x10022);
                RootedMarkSet stale=marks;
                assert(set_owner(&leases,&marks,GC_RM_ENTER)==GC_MS_RESTART);
                assert(!memcmp(&marks,&stale,sizeof(marks))&&paired_queues[1].in_flight==1);
                assert(set_owner(&leases,&marks,GC_MS_CANCEL)==GC_MS_CANCELLED&&leases.active==1);
                assert(set_owner(&leases,&marks,GC_MS_RESET)==GC_RF_DONE);
                marks.phase=0x10022;
                assert(set_owner(&leases,&marks,GC_MS_INIT)==GC_MS_NEXT);
            }
        }
        assert(marks.completed==3&&marks.state==2&&leases.active==2);
        assert(records[chosen].young_epoch==round+3&&records[chosen].old_epoch==round+3);
        assert(paired_queues[1].in_flight==1&&paired_queues[0].queued==1);
        RootedMarkSet complete_marks=marks;
        assert(set_owner(&leases,&marks,GC_RM_ENTER)==GC_MS_DONE);
        assert(!memcmp(&marks,&complete_marks,sizeof(marks)));
        for(unsigned generation=0;generation<2;generation++){
            MarkRequest paired_work={0};
            if(!generation)assert(mark(&paired_queues[0],&paired_work,GC_MARK_TAKE)==GC_MARK_OK);
            else paired_work=work; // Old claim stayed in flight throughout Young.
            release=(MetadataLeaseRequest){.token=paired_work.metadata_lease};
            assert(metadata(&leases,&release,GC_ML_RESOLVE)==GC_ML_OK);
            child_access=(ResolveRequest){.reference=paired_work.reference,.scope={.context=(uintptr_t)&roots.ctx}};
            assert(resolve((AllocationPage *)(uintptr_t)release.page,&child_access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            slice=(MarkScan){(uintptr_t)&paired_work,(uintptr_t)mapping,(uintptr_t)&registry,(uintptr_t)&child_out,1,0,0,0,0};
            assert(scan(&slice)==GC_SCAN_DONE&&!slice.written);
            assert(resolve((AllocationPage *)(uintptr_t)release.page,&child_access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
            paired_work.cursor=slice.next_cursor;assert(mark(&paired_queues[generation],&paired_work,GC_MARK_FINISH)==GC_MARK_OK);
            assert(metadata(&leases,&release,GC_ML_RELEASE)==GC_ML_OK&&leases.active==1-generation);
            if(!generation)assert(paired_queues[1].in_flight==1&&paired_tasks[1].metadata_lease==work.metadata_lease);
        }
        // A completed receipt also expires when another snapshot is adopted.
        atomic_store(&roots.ctx.adopted_phase,0x20022);
        assert(set_owner(&leases,&marks,GC_RM_ENTER)==GC_MS_RESTART);
        assert(set_owner(&leases,&marks,GC_MS_CANCEL)==GC_MS_CANCELLED);
        assert(set_owner(&leases,&marks,GC_MS_RESET)==GC_RF_DONE);
        for(unsigned mask=0;mask<4;mask++){
            uint64_t phase=((uint64_t)(mask+3)<<16)|(mask&1?2:0)|(mask&2?0x20:0);
            atomic_store(&roots.ctx.adopted_phase,phase);marks.phase=phase;
            uint64_t status=set_owner(&leases,&marks,GC_MS_INIT);
            assert(status==(mask?GC_MS_NEXT:GC_MS_DONE)&&marks.required==mask);
            unsigned passes=0;
            while(status==GC_MS_NEXT){
                assert(passes++<2);
                status=gc_test_mark_set_duplicate_or_wait(&leases,&marks,set_owner);
            }
            assert(status==GC_MS_DONE&&marks.completed==mask&&!leases.active);
            assert(atomic_load(&root_cells[1].value)==stored_child);
            assert(set_owner(&leases,&marks,GC_MS_RESET)==GC_RF_DONE);
        }
        marks.phase=0x70022;atomic_store(&roots.ctx.adopted_phase,marks.phase);
        assert(set_owner(&leases,&marks,GC_MS_INIT)==GC_MS_NEXT);
        ObjectHeader *waiting_header=(void *)((char *)mapping+(stored_child>>16)-sizeof(ObjectHeader));
        atomic_store(&waiting_header->state,GC_OBJECT_INITIALIZING); // exclusive fault fixture
        assert(gc_test_mark_set_duplicate_or_wait(&leases,&marks,set_owner)==GC_MS_WAIT);
        assert(!marks.completed&&marks.current==GC_OLD&&marks.jobs[GC_OLD].stage==0&&!leases.active);
        atomic_store(&waiting_header->state,GC_OBJECT_PUBLISHED);
        assert(gc_test_mark_set_duplicate_or_wait(&leases,&marks,set_owner)==GC_MS_NEXT);
        assert(gc_test_mark_set_duplicate_or_wait(&leases,&marks,set_owner)==GC_MS_DONE);
        assert(marks.completed==3&&!leases.active);
        // keep_b names the SAME marked child with deliberately stale colors.
        // Refresh the physical root bits, then publish through a canonical guard.
        assert(root_copy(&roots.ctx,&keep_b,&output)==GC_ROOT_OK);
        RootAccess publication={.follow={.context=(uintptr_t)&roots.ctx,.root=marks.root}};
        assert(root_access(&leases,&publication)==GC_RAC_READY);
        uint64_t before_color=atomic_load(&root_cells[1].value);
        uint64_t after_color=(before_color&~UINT64_C(0xffff))|
            (gc_abi_color((PhaseSnapshot){marks.phase})&~UINT64_C(0x30))|(before_color&0x30);
        assert(before_color!=after_color);
        RootChange color_entries[7];memcpy(color_entries,roots.entries,sizeof(color_entries));
        BufferCursor color_journal=roots.buffer;uint64_t color_revision=atomic_load(&roots.ctx.root_revision);
        for(unsigned fault=0;fault<9;fault++){
            RootedMarkSet bad_marks=marks;RootAccess bad_access=publication;
            if(fault==0)bad_marks.state=1;
            else if(fault==1)bad_marks.completed^=1;
            else if(fault==2)bad_marks.phase+=0x10000;
            else if(fault==3)bad_marks.required=bad_marks.completed=0;
            else if(fault==4)bad_marks.context=0;
            else if(fault==5)bad_marks.root.handle.cell=keep_b.handle.cell;
            else if(fault==6)bad_marks.root.handle.scope++;
            else if(fault==7)bad_access.follow.cleanup.incarnation++;
            else bad_access.follow.cleanup.token=0;
            assert(publish_root(&bad_marks,&bad_access)==GC_HEAL_INVALID);
            assert(atomic_load(&root_cells[1].value)==before_color);
        }
        RootAccess kept_access=publication;RootedMarkSet kept_marks=marks;
        assert(publish_root(&marks,&publication)==GC_HEAL_OK);
        assert(atomic_load(&root_cells[1].value)==after_color&&((after_color^before_color)&0x30)==0);
        assert(publish_root(&marks,&publication)==GC_HEAL_OK);
        assert(!memcmp(&publication,&kept_access,sizeof(publication))&&!memcmp(&marks,&kept_marks,sizeof(marks)));
        assert(!memcmp(&roots.buffer,&color_journal,sizeof(color_journal))&&!memcmp(roots.entries,color_entries,sizeof(color_entries)));
        assert(atomic_load(&roots.ctx.root_revision)==color_revision);
        assert(cleanup(NULL,&publication,GC_RAC_EXIT)==GC_RF_DONE);
        assert(cleanup(&leases,&publication,GC_RAC_RELEASE)==GC_RF_DONE&&!leases.active);
        // Run the selected C/generated controller and its embedded
        // leaves. It owns ordering and cleanup; no raw result is returned.
        // Both old queues are drained. This exclusive fixture admits a new
        // queue epoch so the controller must transfer real offers, not only
        // handle duplicates. This is not a complete cycle/color-reuse test.
        for(unsigned generation=0;generation<2;generation++){
            assert(mark(&paired_queues[generation],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
            paired_queues[generation].epoch++;marks.epochs[generation]++;
        }
        RootedLoad owned={.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=observation.receiver.follow.root},
            .registry=(uintptr_t)&registry,.output=marks.root},
            .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase}};
        HandshakeState admission_h={.members=1,.last_identity=roots.ctx.thread_id};
        ThreadRegistration admission_p={.context=(uintptr_t)&roots.ctx,.identity=roots.ctx.thread_id,
            .registered=1,.registry=(uintptr_t)&admission_h};admission_h.head=(uintptr_t)&admission_p;
        RootRegistry admission_roots={.phase=marks.phase};
        WorkerRequest admission_worker={(uintptr_t)&admission_h,(uintptr_t)&admission_p};
        RootedLoadAdmission admission={(uintptr_t)&admission_roots,(uintptr_t)&admission_h,(uintptr_t)&admission_p,
            {marks.queues[0],marks.queues[1]}};
        // Capture replaces stale input snapshots only after all checks pass.
        owned.marks.phase=UINT64_MAX;owned.marks.queues[0]=1;owned.marks.epochs[1]=0;
        RootedLoad uncaptured=owned;
        assert(gc_handshake_step_locked(&admission_h,NULL,GC_HS_BEGIN)==GC_HS_OK);
        assert(admit_load(&admission,&owned)==GC_LA_WAIT&&!memcmp(&owned,&uncaptured,sizeof(owned)));
        assert(worker_step(&admission_roots,&admission_worker,GC_WK_POLL)==GC_WK_PARK);
        assert(gc_handshake_step_locked(&admission_h,NULL,GC_HS_END)==GC_HS_OK);
        assert(worker_step(&admission_roots,&admission_worker,GC_WK_POLL)==GC_WK_OK);
        RootedLoadAdmission bad_admission=admission;bad_admission.queues[1]=admission.queues[0];
        assert(admit_load(&bad_admission,&owned)==GC_LA_INVALID&&!memcmp(&owned,&uncaptured,sizeof(owned)));
        atomic_store(&roots.ctx.private_producers,UINT64_MAX);
        assert(admit_load(&admission,&owned)==GC_LA_EXHAUSTED&&!memcmp(&owned,&uncaptured,sizeof(owned)));
        atomic_store(&roots.ctx.private_producers,0);
        assert(admit_load(&admission,&owned)==GC_LA_OK&&owned.admitted&&atomic_load(&roots.ctx.private_producers)==1);
        assert(owned.marks.phase==marks.phase&&owned.marks.queues[0]==marks.queues[0]&&owned.marks.epochs[1]==marks.epochs[1]);
        RootedLoad captured_load=owned;
        assert(admit_load(&admission,&owned)==GC_LA_OK&&!memcmp(&owned,&captured_load,sizeof(owned))&&atomic_load(&roots.ctx.private_producers)==1);
        assert(drain(&roots.drain)==GC_ROOT_OK);
        while(roots.buffer.top-atomic_load(&roots.buffer.consumed)<roots.buffer.capacity)
            assert(root_copy(&roots.ctx,&keep_b,&keep_b)==GC_ROOT_OK);
        uint64_t flush_revision=atomic_load(&roots.ctx.root_revision);
        assert(load_owner(&leases,&owned,GC_RLOAD_HEAP)==GC_RLOAD_ACCESS);
        assert(load_owner(NULL,&owned,GC_RLOAD_ACCESS)==GC_RLOAD_ACCESS);
        assert(owned.observation.result==GC_ROOT_FLUSH);
        owned.observation.receiver.access.scope.ticket=1;
        assert(load_owner(NULL,&owned,GC_RLOAD_ACCESS)==GC_RLOAD_ACCESS);
        assert(leases.active==1&&atomic_load(&roots.ctx.access_epoch));
        owned.observation.receiver.access.scope.ticket=0;
        assert(load_owner(NULL,&owned,GC_RLOAD_ACCESS)==GC_RLOAD_HEAP);
        uint64_t saved_free_head=leases.free_head;leases.free_head=leases.capacity+1;
        assert(load_owner(&leases,&owned,GC_RLOAD_HEAP)==GC_RLOAD_HEAP);
        assert(leases.active==1&&!atomic_load(&roots.ctx.access_epoch));
        leases.free_head=saved_free_head;
        assert(load_owner(&leases,&owned,GC_RLOAD_HEAP)==GC_RLOAD_FLUSH);
        assert(!leases.active&&!owned.observation.output.value&&owned.observation.stage==0);
        assert(atomic_load(&roots.ctx.root_revision)==flush_revision);
        assert(drain(&roots.drain)==GC_ROOT_OK);
        assert(gc_test_rooted_load_drive(&leases,&owned,load_owner)==GC_RLOAD_DONE);
        assert(leases.active==2&&owned.observation.output.value==stored_child&&owned.marks.completed==3);
        for(unsigned generation=0;generation<2;generation++){
            assert(!owned.marks.jobs[generation].access.follow.cleanup.token);
            MarkRequest owned_work={0};assert(mark(&paired_queues[generation],&owned_work,GC_MARK_TAKE)==GC_MARK_OK);
            release=(MetadataLeaseRequest){.token=owned_work.metadata_lease};
            assert(metadata(&leases,&release,GC_ML_RESOLVE)==GC_ML_OK);
            child_access=(ResolveRequest){.reference=owned_work.reference,.scope={.context=(uintptr_t)&roots.ctx}};
            assert(resolve((void *)(uintptr_t)release.page,&child_access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            slice=(MarkScan){(uintptr_t)&owned_work,(uintptr_t)mapping,(uintptr_t)&registry,(uintptr_t)&child_out,1,0,0,0,0};
            assert(scan(&slice)==GC_SCAN_DONE&&!slice.written);
            assert(resolve((void *)(uintptr_t)release.page,&child_access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
            owned_work.cursor=slice.next_cursor;
            assert(mark(&paired_queues[generation],&owned_work,GC_MARK_FINISH)==GC_MARK_OK);
            assert(metadata(&leases,&release,GC_ML_RELEASE)==GC_ML_OK);
        }
        assert(!leases.active);
        assert(!atomic_load(&roots.ctx.private_producers));
        RootedLoad counted={.observation=owned.observation,
            .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase},
            .stage=GC_RLOAD_MARK_INIT};
        RootedLoad counted_before=counted;
        atomic_store(&roots.ctx.private_producers,UINT64_MAX);
        assert(load_owner(&leases,&counted,GC_RLOAD_HEAP)==GC_RLOAD_EXHAUSTED);
        assert(!memcmp(&counted,&counted_before,sizeof(counted))&&atomic_load(&roots.ctx.private_producers)==UINT64_MAX);
        atomic_store(&roots.ctx.private_producers,0);
        RootedLoad second=counted;
        assert(load_owner(&leases,&counted,GC_RLOAD_HEAP)==GC_RLOAD_HEAP);
        assert(load_owner(&leases,&second,GC_RLOAD_HEAP)==GC_RLOAD_HEAP);
        assert(atomic_load(&roots.ctx.private_producers)==2&&!atomic_load(&roots.ctx.access_epoch));
        HandshakeState held={.members=1,.last_identity=roots.ctx.thread_id};
        ThreadRegistration held_owner={.context=(uintptr_t)&roots.ctx,.identity=roots.ctx.thread_id,
            .registered=1,.registry=(uintptr_t)&held};held.head=(uintptr_t)&held_owner;
        WorkerRequest held_worker={(uintptr_t)&held,(uintptr_t)&held_owner};
        RootRegistry held_roots={.phase=marks.phase};
        assert(worker_step(&held_roots,&held_worker,GC_WK_ENTER_NATIVE)==GC_WK_RETRY);
        assert(gc_context_transition_locked(&roots.ctx,GC_RUNNING,GC_BLOCKED_NATIVE)==GC_CONTEXT_PRIVATE_WORK);
        assert(gc_handshake_step_locked(&held,NULL,GC_HS_BEGIN)==GC_HS_OK);
        assert(worker_step(&held_roots,&held_worker,GC_WK_POLL)==GC_WK_RETRY&&!held.acknowledged);
        assert(load_owner(NULL,&second,GC_RLOAD_CANCEL)==GC_RLOAD_HEAP);
        assert(load_owner(&leases,&second,GC_RLOAD_HEAP)==GC_RLOAD_CANCELLED);
        assert(atomic_load(&roots.ctx.private_producers)==1);
        assert(worker_step(&held_roots,&held_worker,GC_WK_POLL)==GC_WK_RETRY&&!held.acknowledged);
        assert(load_owner(NULL,&counted,GC_RLOAD_CANCEL)==GC_RLOAD_HEAP);
        assert(load_owner(&leases,&counted,GC_RLOAD_HEAP)==GC_RLOAD_CANCELLED);
        assert(!atomic_load(&roots.ctx.private_producers));
        assert(worker_step(&held_roots,&held_worker,GC_WK_POLL)==GC_WK_PARK&&held.acknowledged==1);
        assert(gc_handshake_step_locked(&held,NULL,GC_HS_END)==GC_HS_OK);
        assert(worker_step(&held_roots,&held_worker,GC_WK_POLL)==GC_WK_OK);
        RootedLoad completed_load=owned;
        assert(load_owner(NULL,&owned,GC_RLOAD_HEAP)==GC_RLOAD_DONE);
        assert(!memcmp(&owned,&completed_load,sizeof(owned)));
        // A phase change after observation must mark the protected value again
        // without rereading the receiver or adding another root journal entry.
        RootedLoad restarted={.observation=owned.observation,
            .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase},
            .stage=GC_RLOAD_MARK_INIT};
        uint64_t restart_revision=atomic_load(&roots.ctx.root_revision);
        atomic_store(&roots.ctx.adopted_phase,marks.phase+0x10000);
        assert(load_owner(&leases,&restarted,GC_RLOAD_HEAP)==GC_RLOAD_RESTART);
        restarted.marks.phase=atomic_load(&roots.ctx.adopted_phase);
        assert(load_owner(&leases,&restarted,GC_RLOAD_RESUME)==GC_RLOAD_HEAP);
        assert(gc_test_rooted_load_drive(&leases,&restarted,load_owner)==GC_RLOAD_DONE);
        assert(restarted.observation.output.value==stored_child&&atomic_load(&roots.ctx.root_revision)==restart_revision);
        marks.phase=restarted.marks.phase;
        RootedLoad pending_restart={.observation=owned.observation,
            .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase},
            .stage=GC_RLOAD_MARK_INIT};
        assert(load_owner(&leases,&pending_restart,GC_RLOAD_HEAP)==GC_RLOAD_HEAP);
        assert(load_owner(&leases,&pending_restart,GC_RLOAD_HEAP)==GC_RLOAD_ACCESS);
        assert(load_owner(NULL,&pending_restart,GC_RLOAD_ACCESS)==GC_RLOAD_ACCESS);
        assert(load_owner(NULL,&pending_restart,GC_RLOAD_ACCESS)==GC_RLOAD_QUEUE);
        assert(leases.active==1&&!atomic_load(&roots.ctx.access_epoch));
        atomic_store(&roots.ctx.adopted_phase,marks.phase+0x10000);
        assert(load_owner(NULL,&pending_restart,GC_RLOAD_QUEUE)==GC_RLOAD_HEAP);
        assert(load_owner(&leases,&pending_restart,GC_RLOAD_HEAP)==GC_RLOAD_RESTART);
        assert(!leases.active&&pending_restart.marks.state==0);
        pending_restart.marks.phase=atomic_load(&roots.ctx.adopted_phase);
        assert(load_owner(&leases,&pending_restart,GC_RLOAD_RESUME)==GC_RLOAD_HEAP);
        assert(gc_test_rooted_load_drive(&leases,&pending_restart,load_owner)==GC_RLOAD_DONE);
        assert(atomic_load(&roots.ctx.root_revision)==restart_revision);
        marks.phase=pending_restart.marks.phase;
        // Every reachable step boundary can cancel, including an active
        // receiver/result guard and a pending mark offer. Published queue work
        // is never revoked; these offers are duplicates in this fixture.
        for(unsigned boundary=0;boundary<26;boundary++){
            RootedLoad cancelled={.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=observation.receiver.follow.root},
                .registry=(uintptr_t)&registry,.output=marks.root},
                .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase}};
            assert(drain(&roots.drain)==GC_ROOT_OK);
            for(unsigned step=0;step<boundary;step++){
                uint64_t action=gc_rooted_load_next(&cancelled);
                if(action==GC_RLOAD_DONE)break;
                assert(action==GC_RLOAD_HEAP||action==GC_RLOAD_ACCESS||action==GC_RLOAD_QUEUE);
                (void)load_owner(action==GC_RLOAD_HEAP?&leases:NULL,&cancelled,action);
            }
            if(gc_rooted_load_next(&cancelled)==GC_RLOAD_DONE)continue;
            (void)load_owner(NULL,&cancelled,GC_RLOAD_CANCEL);
            assert(gc_test_rooted_load_drive(&leases,&cancelled,load_owner)==GC_RLOAD_CANCELLED);
            assert(!leases.active&&!atomic_load(&roots.ctx.access_epoch));
            // The owner is clean before acknowledging a new phase. These
            // registered fixture controls use the actual worker/handshake
            // protocol, not a direct store to adopted_phase.
            bool captured=cancelled.observation.stage>=3&&cancelled.observation.result==GC_ROOT_OK;
            uint64_t rearm_revision=atomic_load(&roots.ctx.root_revision);
            HandshakeState pause={.members=1,.last_identity=roots.ctx.thread_id};
            ThreadRegistration participant={.context=(uintptr_t)&roots.ctx,.identity=roots.ctx.thread_id,
                .registered=1,.registry=(uintptr_t)&pause};pause.head=(uintptr_t)&participant;
            RootRegistry adoption={.phase=marks.phase+0x10000};
            WorkerRequest worker={(uintptr_t)&pause,(uintptr_t)&participant};
            assert(gc_handshake_step_locked(&pause,NULL,GC_HS_BEGIN)==GC_HS_OK);
            assert(worker_step(&adoption,&worker,GC_WK_POLL)==GC_WK_PARK);
            RootedLoad parked=cancelled;
            assert(load_owner(&leases,&cancelled,GC_RLOAD_REARM)==GC_RLOAD_INVALID);
            assert(!memcmp(&parked,&cancelled,sizeof(parked)));
            assert(gc_handshake_step_locked(&pause,NULL,GC_HS_END)==GC_HS_OK);
            assert(worker_step(&adoption,&worker,GC_WK_POLL)==GC_WK_OK);
            uint64_t rearmed=load_owner(&leases,&cancelled,GC_RLOAD_REARM);
            assert(rearmed==(captured?GC_RLOAD_RESTART:GC_RLOAD_HEAP));
            if(!captured)assert(gc_test_rooted_load_drive(&leases,&cancelled,load_owner)==GC_RLOAD_RESTART);
            RootedLoadAdmission readmission={(uintptr_t)&adoption,(uintptr_t)&pause,(uintptr_t)&participant,
                {marks.queues[0],marks.queues[1]}};
            assert(admit_load(&readmission,&cancelled)==GC_LA_OK&&cancelled.marks.phase==adoption.phase);
            assert(atomic_load(&roots.ctx.private_producers)==1);
            assert(load_owner(&leases,&cancelled,GC_RLOAD_RESUME)==GC_RLOAD_HEAP);
            assert(gc_test_rooted_load_drive(&leases,&cancelled,load_owner)==GC_RLOAD_DONE);
            assert(cancelled.observation.output.value==stored_child&&!leases.active);
            assert(atomic_load(&roots.ctx.root_revision)==rearm_revision+!captured);
            marks.phase=adoption.phase;
        }
        color_revision=atomic_load(&roots.ctx.root_revision);
        after_color=atomic_load(&root_cells[1].value);
        // Reacquire the moved receiver only after the marked result guard is
        // gone. The observation token remains exact; never reread the field to
        // choose which logical referent the load returns.
        RootedLoad healing_load={.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=observation.receiver.follow.root},
            .registry=(uintptr_t)&registry,.output=marks.root},
            .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase}};
        RootAccess *healing=&healing_load.observation.receiver;
        RootRequest heal_owner=healing->follow.root;
        assert(root(&roots.ctx,&heal_owner,GC_ROOT_READ_RAW)==GC_ROOT_OK);
        ReferenceLease heal_entries[2]={{.reference=heal_owner.value},{.reference=heal_owner.value}};
        for(unsigned i=0;i<2;i++)assert(acquire(&leases,&heal_entries[i])==GC_ML_OK);
        BarrierEntry heal_satb[1],heal_remembered[1];
        assert(drain(&roots.drain)==GC_ROOT_OK);
        for(unsigned steps=0;healing_load.stage!=GC_RLOAD_HEAL;steps++){
            assert(steps<96);
            uint64_t action=gc_rooted_load_next(&healing_load);
            assert(action==GC_RLOAD_HEAP||action==GC_RLOAD_ACCESS||action==GC_RLOAD_QUEUE);
            assert(load_owner(action==GC_RLOAD_HEAP?&leases:NULL,&healing_load,action)!=GC_RLOAD_ERROR);
        }
        assert(atomic_load(&roots.ctx.private_producers)==1);
        color_revision=atomic_load(&roots.ctx.root_revision);
        GuardedHeal repair={.scope=(uintptr_t)&healing->access.scope,.page=healing->follow.cleanup.page,
            .registry=observation.registry,.byte_offset=observation.byte_offset,.phase=marks.phase,
            .expected=observation.output.value,
            .desired=(after_color&~UINT64_C(0x30))|(observation.output.value&0x30)};
        assert(heal(&repair)==GC_HEAL_OK);
        // A real logical store wins over a delayed self-heal. Its SATB work
        // remains owned by this fixture through teardown.
        GuardedStore clear={.scope=repair.scope,.page=repair.page,.registry=repair.registry,
            .byte_offset=repair.byte_offset,.phase=repair.phase,.expected=repair.desired,.desired=0};
        clear.satb=(BarrierEntry){{repair.desired},{{heal_owner.value},repair.byte_offset,healing->access.scope.incarnation,marks.phase,heal_entries[0].lease.token}};
        clear.remembered=clear.satb;clear.remembered.slot.metadata_lease=heal_entries[1].lease.token;
        RootedLoadStoreActor actor={.context={.abi_version=GC_ABI_VERSION,.thread_id=roots.ctx.thread_id+1,.adopted_phase=marks.phase},
            .scope={.source_offset=healing->access.scope.source_offset,.incarnation=healing->access.scope.incarnation},
            .request=clear,.store=store,.access=access,.cell=(void *)(uintptr_t)healing->access.scope.held};
        actor.scope.context=(uintptr_t)&actor.context;actor.request.scope=(uintptr_t)&actor.scope;
        actor.context.satb=(BufferCursor){(uintptr_t)heal_satb,1,0,0,0};
        actor.context.remembered=(BufferCursor){(uintptr_t)heal_remembered,1,0,0,0};
        MetadataThread writer;
#ifdef _WIN32
        writer=CreateThread(NULL,0,gc_rooted_load_store_actor,&actor,0,NULL);assert(writer);
#else
        assert(pthread_create(&writer,NULL,gc_rooted_load_store_actor,&actor)==0);
#endif
        while(!atomic_load_explicit(&actor.ready,memory_order_acquire)){}
        atomic_store_explicit(&actor.go,1,memory_order_release);
        if(!(round&1))while(!atomic_load_explicit(&actor.done,memory_order_acquire)){}
        assert(load_owner(NULL,&healing_load,GC_RLOAD_ACCESS)==GC_RLOAD_ACCESS);
        assert(healing_load.last==GC_HEAL_OK||healing_load.last==GC_HEAL_CHANGED);
        if(!(round&1))assert(healing_load.last==GC_HEAL_CHANGED);
        while(!atomic_load_explicit(&actor.done,memory_order_acquire)){}
        assert(heal(&repair)==GC_HEAL_CHANGED);
        _Atomic uint64_t *healed_slot=(void *)((char *)mapping+healing->access.scope.source_offset+repair.byte_offset);
        assert(!atomic_load(healed_slot));
        assert(atomic_load(&root_cells[1].value)==after_color);
        assert(atomic_load(&roots.ctx.root_revision)==color_revision);
        assert(gc_test_rooted_load_drive(&leases,&healing_load,load_owner)==GC_RLOAD_DONE);
        assert(leases.active==2&&!atomic_load(&roots.ctx.private_producers));
#ifdef _WIN32
        assert(WaitForSingleObject(writer,30000)==WAIT_OBJECT_0);assert(CloseHandle(writer));
#else
        assert(pthread_join(writer,NULL)==0);
#endif
        assert(actor.result==GC_STORE_OK&&(actor.request.transferred&1));
        assert(!actor.scope.held&&!atomic_load(&actor.context.access_epoch));
        assert(actor.context.satb.top==1&&heal_satb[0].previous.bits==repair.desired);
        // Exclusive teardown consumes the buffered entries before releasing
        // their real metadata leases; this is not a termination certificate.
        atomic_store(&actor.context.satb.consumed,actor.context.satb.top);
        atomic_store(&actor.context.remembered.consumed,actor.context.remembered.top);
        for(unsigned i=0;i<2;i++){
            MetadataLeaseRequest entry_release=heal_entries[i].lease;
            assert(metadata(&leases,&entry_release,GC_ML_RELEASE)==GC_ML_OK);
        }
        assert(!leases.active);
        assert(drain(&roots.drain)==GC_ROOT_OK);
        RootedLoad null_load={.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=observation.receiver.follow.root},
            .registry=(uintptr_t)&registry,.output=marks.root},
            .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase}};
        assert(gc_test_rooted_load_drive(&leases,&null_load,load_owner)==GC_RLOAD_DONE);
        assert(!null_load.observation.output.value&&!atomic_load(&root_cells[1].value)&&!leases.active);
        RootedLoad null_receiver={.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=marks.root},
            .registry=(uintptr_t)&registry,.output=observation.receiver.follow.root}};
        assert(gc_test_rooted_load_drive(&leases,&null_receiver,load_owner)==GC_RLOAD_NULL_RECEIVER);
        assert(!leases.active&&!atomic_load(&roots.ctx.access_epoch));
        // Prepare the existing retained child without reading any receiver
        // field. The original receiver's field is now null. Fresh queue epochs
        // require real mark offers; the fixture exclusively owns this reset.
        RootRequest desired=keep_b;desired.value=0;
        for(unsigned generation=0;generation<2;generation++){
            assert(mark(&paired_queues[generation],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
            paired_queues[generation].epoch++;marks.epochs[generation]++;
        }
        RootedLoad prepared={.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=desired},.output=desired},
            .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase}};
        for(unsigned fault=0;fault<6;fault++){
            RootedLoad bad=prepared;
            if(fault==0)bad.observation.registry=(uintptr_t)&registry;
            else if(fault==1)bad.observation.byte_offset=8;
            else if(fault==2)bad.observation.output.handle.cell=output.handle.cell;
            else if(fault==3)bad.observation.output.value=stored_child;
            else if(fault==4)bad.observation.stage=1;
            else bad.stage=GC_RLOAD_HEAL_ENTER;
            RootedLoad unchanged=bad;
            assert(value_owner(&leases,&bad,GC_RLOAD_HEAP)==GC_RLOAD_INVALID&&!memcmp(&bad,&unchanged,sizeof(bad)));
            assert(!atomic_load(&roots.ctx.private_producers));
        }
        admission_roots.phase=marks.phase;
        // Exclusive fixture admission owns a separate outer-store obligation
        // before value preparation and keeps it through physical publication.
        atomic_store(&roots.ctx.private_producers,1);
        assert(admit_load(&admission,&prepared)==GC_LA_OK);
        assert(gc_test_rooted_load_drive(&leases,&prepared,value_owner)==GC_RLOAD_DONE);
        assert(prepared.marks.completed==3&&!prepared.observation.output.value&&atomic_load(&roots.ctx.private_producers)==1);
        for(unsigned generation=0;generation<2;generation++){
            MarkRequest work={0};assert(mark(&paired_queues[generation],&work,GC_MARK_TAKE)==GC_MARK_OK);
            MetadataLeaseRequest held={.token=work.metadata_lease};assert(metadata(&leases,&held,GC_ML_RESOLVE)==GC_ML_OK);
            ResolveRequest scope={.reference=work.reference,.scope={.context=(uintptr_t)&roots.ctx}};
            assert(resolve((void *)(uintptr_t)held.page,&scope,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            MarkScan scan_value={(uintptr_t)&work,(uintptr_t)mapping,(uintptr_t)&registry,(uintptr_t)&child_out,1,0,0,0,0};
            assert(scan(&scan_value)==GC_SCAN_DONE&&!scan_value.written);
            assert(resolve((void *)(uintptr_t)held.page,&scope,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
            work.cursor=scan_value.next_cursor;assert(mark(&paired_queues[generation],&work,GC_MARK_FINISH)==GC_MARK_OK);
            assert(metadata(&leases,&held,GC_ML_RELEASE)==GC_ML_OK);
        }
        assert(!leases.active);
        BarrierEntry value_satb[2]={0},value_remembered[2]={0};
        roots.ctx.satb=(BufferCursor){.entries=(uintptr_t)value_satb,.capacity=2};
        roots.ctx.remembered=(BufferCursor){.entries=(uintptr_t)value_remembered,.capacity=2};
        // The previous preparation was a standalone value-operation check.
        // The complete store now acquires and owns its own admission interval.
        atomic_store(&roots.ctx.private_producers,0);
        for(unsigned attempt=0;attempt<2;attempt++){
            RootedStore pending={.value.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=desired},.output=desired},
                .observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=observation.receiver.follow.root},
                    .registry=(uintptr_t)&registry,.output=output}};
            assert(admit_load(&admission,&pending.value)==GC_LA_OK);
            unsigned steps=0;
            while(gc_rooted_store_next(&pending)!=GC_RLOAD_DONE){
                uint64_t action=gc_rooted_store_next(&pending);
                assert(action==GC_RLOAD_HEAP||action==GC_RLOAD_ACCESS||action==GC_RLOAD_QUEUE);
                assert(++steps<100);
                uint64_t r=store_owner(action==GC_RLOAD_HEAP?&leases:NULL,&pending,action);
                assert(r==GC_RLOAD_HEAP||r==GC_RLOAD_ACCESS||r==GC_RLOAD_QUEUE||r==GC_RLOAD_DONE);
            }
            GuardedStore write=pending.attempt.commit;
            AllocationPage *receiver_page=(void *)(uintptr_t)write.page;
            uint64_t mask=(attempt?1:0)|(receiver_page->generation==GC_OLD?2:0);
            assert(pending.result==GC_STORE_OK&&!write.desired&&write.transferred==mask);
            assert(!atomic_load(&roots.ctx.private_producers));
            RootAccess check={.follow={.context=(uintptr_t)&roots.ctx,.root=observation.receiver.follow.root}};
            assert(root_access(&leases,&check)==GC_RAC_READY);
            uint64_t stored_value=atomic_load((_Atomic uint64_t *)((char *)mapping+check.access.scope.source_offset));
            assert(stored_value&&((stored_value&UINT16_MAX)==gc_abi_color((PhaseSnapshot){write.phase})));
            assert(cleanup(NULL,&check,GC_RAC_EXIT)==GC_RF_DONE&&cleanup(&leases,&check,GC_RAC_RELEASE)==GC_RF_DONE);
            for(unsigned i=0;i<2;i++)if(mask&(UINT64_C(1)<<i)){
                // This buffer job owns the transferred token. The completed
                // attempt has already released every untransferred lease.
                BarrierEntry entry={0};BarrierJob job={.identity=roots.ctx.thread_id,.selector=i,
                    .output=(uintptr_t)&entry,.capacity=1};
                assert(gc_buffer_job_step(&roots.ctx,&job,GC_BJ_CLAIM)==GC_BJ_OK);
                assert(gc_buffer_job_step(&roots.ctx,&job,GC_BJ_COPY)==GC_BJ_OK&&job.written==1);
                assert(entry.slot.metadata_lease==(i?write.remembered.slot.metadata_lease:write.satb.slot.metadata_lease));
                assert(entry.previous.bits==write.expected&&entry.slot.owner.bits==write.satb.slot.owner.bits);
                assert(gc_buffer_job_step(&roots.ctx,&job,GC_BJ_ACK)==GC_BJ_OK);
                MetadataLeaseRequest held={.token=entry.slot.metadata_lease};
                assert(metadata(&leases,&held,GC_ML_RESOLVE)==GC_ML_OK);
                assert(metadata(&leases,&held,GC_ML_RELEASE)==GC_ML_OK);
            }
            assert(!leases.active);
        }
        assert(!atomic_load(&roots.ctx.private_producers));
        for(unsigned boundary=0;boundary<24;boundary++){
            RootedLoad value={.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=desired},.output=desired},
                .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase}};
            uint64_t revision=atomic_load(&roots.ctx.root_revision);
            for(unsigned step=0;step<boundary&&gc_rooted_load_next(&value)!=GC_RLOAD_DONE;step++){
                uint64_t action=gc_rooted_load_next(&value);
                assert(value_owner(action==GC_RLOAD_HEAP?&leases:NULL,&value,action)!=GC_RLOAD_ERROR);
            }
            if(gc_rooted_load_next(&value)!=GC_RLOAD_DONE){
                (void)value_owner(NULL,&value,GC_RLOAD_CANCEL);
                assert(gc_test_rooted_load_drive(&leases,&value,value_owner)==GC_RLOAD_CANCELLED);
                assert(!leases.active&&!atomic_load(&roots.ctx.private_producers));
                uint64_t rearm=value_owner(&leases,&value,GC_RLOAD_REARM);
                assert(rearm==GC_RLOAD_HEAP||rearm==GC_RLOAD_RESTART);
                assert(admit_load(&admission,&value)==GC_LA_OK);
                if(rearm==GC_RLOAD_RESTART)assert(value_owner(&leases,&value,GC_RLOAD_RESUME)==GC_RLOAD_HEAP);
                assert(gc_test_rooted_load_drive(&leases,&value,value_owner)==GC_RLOAD_DONE);
            }
            assert(!leases.active&&!atomic_load(&roots.ctx.access_epoch)&&!atomic_load(&roots.ctx.private_producers));
            assert(atomic_load(&roots.ctx.root_revision)==revision);
        }
        RootedLoad null_value={.observation={.receiver.follow={.context=(uintptr_t)&roots.ctx,.root=marks.root},.output=marks.root},
            .marks={.queues={marks.queues[0],marks.queues[1]},.epochs={marks.epochs[0],marks.epochs[1]},.phase=marks.phase}};
        assert(gc_test_rooted_load_drive(&leases,&null_value,value_owner)==GC_RLOAD_DONE);
        assert(!leases.active&&!atomic_load(&roots.ctx.private_producers));
        assert(root(&roots.ctx,&output,GC_ROOT_RELEASE)==GC_ROOT_OK);assert(root(&roots.ctx,&roots.request,GC_ROOT_RELEASE)==GC_ROOT_OK);
        assert(root(&roots.ctx,&keep_b,GC_ROOT_RELEASE)==GC_ROOT_OK);
        pool_vm_release(mapping,5*GC_HEAP_PAGE_BYTES);
    }
    puts("rooted store pipeline: 64 admitted value preparations drive 128 physical commits with real buffer lease handoff PASS");
    puts("rooted value: 64 dual-mark and null preparations plus 1536 cancellation/rearm boundaries preserve owned roots without field reads PASS");
    puts("rooted load capture: 64 registered snapshot admissions and post-handshake readmissions PASS");
    puts("rooted load admission: 64 counted owners block ACK/native entry until both release, without overflow PASS");
    puts("rooted load rearm: 64 worker phase handoffs retain observations and reject parked resume PASS");
    puts("rooted load owner: 64 marked/null results, snapshot restarts and cancellation boundaries PASS");
    puts("rooted load completion: 64 two-OS-owner self-heal/null-store races preserve results and SATB PASS");
    puts("rooted mark publication: 64 guarded repairs preserve identity, remembered bits, journals and logical revisions PASS");
    puts("rooted mark set: 64 dual-generation observations retain Old work across snapshot restart and Young completion PASS");
    puts("rooted mark owner: 64 observed referents survive queue pressure, duplicate cleanup and stale-epoch cancellation PASS");
    puts("rooted observation owner: 64 moved receivers, clean journal-pressure retries and exactly-once rooted reads PASS");
    puts("logical CAS retry: 64 full-journal fresh observations release guards, preserve identities and compare null correctly PASS");
    puts("logical CAS relocation: 64 protected-source observations match relocated rooted identities and reject stale retries PASS");
    puts("root copy relocation: 64 full-journal retries reread moved roots and retain overwritten identity PASS");
    puts("scalar root access: 64 protected-source moves, journal-pressure retries and canonical field updates PASS");
    puts("mark prepare: 64 moved output roots, queue duplicates and guarded completion rounds PASS");
    puts("root follow: 64 guarded-field output roots, busy retries and protected referent moves PASS");
    puts("guarded load: 64 pool-backed output roots, journal-full rereads and protected receiver moves PASS");
}
#endif
