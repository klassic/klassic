#ifndef KLASSIC_GC_NATIVE_GRAPH_CASES_H
#define KLASSIC_GC_NATIVE_GRAPH_CASES_H
#include "gc_native_root_mark.h"
#include "gc_native_cycle_cases.h"
#include "gc_native_allocation_cases.h"
#include "gc_mark_scan_commit.h"
#include "gc_native_mark_scan.h"
#include "gc_native_scan_incoming.h"
typedef struct {
    NativeAllocationStarter allocate;CycleMarkStarter start;PendingMarker pending;
    uint64_t (*scan)(MarkScan *);MarkScanCommitter commit;
} NativeGraphOps;
/* Counted parent claim or root-journal lease remains held through this handoff. */
static void gc_ng_offer(NativeRuntimeOwner *r,uint64_t reference,const NativeGraphOps *ops){
    NativeWorkStorage *work=gc_native_runtime_work(r);MarkQueue *queue=&work->queues[0];
    PendingMark p={.context=(uintptr_t)&r->context,.current={.reference=reference},
        .queue=(uintptr_t)queue,.generation=0,.epoch=queue->epoch,.phase=r->roots.phase};
    uint64_t status=ops->pending(&work->leases,&p,GC_RM_ENTER);
    if(reference){
        assert(status==GC_RAC_READY);
        assert(ops->pending(&work->leases,&p,GC_RM_ROUTE)==GC_RM_EXIT_REQUIRED);
        assert(p.route_result==GC_ROUTE_READY);
        assert(ops->pending(&work->leases,&p,GC_RM_EXIT)==GC_RM_OFFER_REQUIRED);
        status=ops->pending(&work->leases,&p,GC_RM_OFFER);
        if(status==GC_RM_RELEASE_REQUIRED){
            assert(p.offer_result==GC_MARK_DUPLICATE);
            status=ops->pending(&work->leases,&p,GC_RM_RELEASE);
        }
    }
    assert(status==GC_RM_COMPLETE&&!p.current.lease.token&&!p.retired.token);
}
static void gc_ng_roots(NativeRuntimeOwner *r,const NativeGraphOps *ops,bool cancel){
    RootAudit *audit=&r->cycle[0].marking_audit;
    assert(gc_root_audit_step_locked(&r->roots,audit,GC_RA_BEGIN)==GC_RA_ITEM);
    for(;;){
        NativeRootMark mark={0};
        uint64_t status=gc_native_root_mark_begin_locked(r,0,&r->context,&mark);
        if(status==GC_RWI_DONE)break;
        assert(status==GC_RWI_ITEM);
        assert(gc_native_root_mark_ack_locked(&mark)==GC_RWI_WAIT);
        NativeRootMark copy=mark;assert(gc_native_root_mark_step(&copy,GC_RM_ENTER,ops->pending)==GC_RF_INVALID);
        status=gc_native_root_mark_step(&mark,GC_RM_ENTER,ops->pending);
        if(mark.pending.current.reference){
            assert(status==GC_RAC_READY);
            assert(gc_native_root_mark_step(&mark,GC_RM_ROUTE,ops->pending)==GC_RM_EXIT_REQUIRED);
            assert(gc_native_root_mark_ack_locked(&mark)==GC_RWI_WAIT);
            assert(gc_native_root_mark_step(&mark,GC_RM_EXIT,ops->pending)==GC_RM_OFFER_REQUIRED);
            if(cancel){
                assert(gc_native_root_mark_step(&mark,GC_RM_CANCEL,ops->pending)==GC_RM_CANCELLED);
                assert(gc_native_root_mark_ack_locked(&mark)==GC_RWI_WAIT&&audit->walk.pending);
                assert(gc_native_root_mark_step(&mark,GC_RM_RESET,ops->pending)==GC_RF_DONE);
                assert(gc_native_root_mark_step(&mark,GC_RM_ENTER,ops->pending)==GC_RAC_READY);
                assert(gc_native_root_mark_step(&mark,GC_RM_ROUTE,ops->pending)==GC_RM_EXIT_REQUIRED);
                assert(gc_native_root_mark_step(&mark,GC_RM_EXIT,ops->pending)==GC_RM_OFFER_REQUIRED);
                cancel=false;
            }
            status=gc_native_root_mark_step(&mark,GC_RM_OFFER,ops->pending);
            if(status==GC_RM_RELEASE_REQUIRED)status=gc_native_root_mark_step(&mark,GC_RM_RELEASE,ops->pending);
        }
        assert(status==GC_RM_COMPLETE);
        assert(gc_native_root_mark_ack_locked(&mark)==GC_RWI_ITEM&&!audit->walk.pending);
        assert(gc_native_root_mark_ack_locked(&mark)==GC_RWI_INVALID);
    }
    audit->walk.completed_mask=audit->walk.mask;
    assert(gc_root_audit_step_locked(&r->roots,audit,GC_RA_NEXT)==GC_RA_DONE);
}
static void gc_ng_queue_spill_probe(NativeRuntimeOwner *r){
    NativeWorkStorage *work=gc_native_runtime_work(r);MarkQueue *queue=&work->queues[0];
    assert(queue->overflow_count>=2);
    uint64_t active=work->leases.active;
    uint64_t offset=gc_mark_spill_cell(queue,queue->overflow_head)->reference>>16;
    NativeQueueIncoming found={0};
    assert(gc_native_queue_incoming_begin_locked(r,offset,offset+16,&found)==GC_NSI_MORE);
    uint64_t status;do{status=gc_native_queue_incoming_next_locked(&found);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_FOUND&&found.pin.token&&work->leases.active==active+1);
    assert(gc_native_queue_incoming_cancel_locked(&found)==GC_NSI_DONE&&work->leases.active==active);
    NativeQueueIncoming absent={0};
    assert(gc_native_queue_incoming_begin_locked(r,3*GC_HEAP_PAGE_BYTES,4*GC_HEAP_PAGE_BYTES,&absent)==GC_NSI_MORE);
    while(!absent.pin.token)assert(gc_native_queue_incoming_next_locked(&absent)==GC_NSI_MORE);
    MetadataLeaseRequest held[64]={{0}};unsigned count=0;uint64_t token=absent.pin.token;
    while(work->leases.free_head){
        held[count]=(MetadataLeaseRequest){absent.pin.page,absent.pin.incarnation,0};
        assert(gc_metadata_step_locked(&work->leases,&held[count++],GC_ML_ACQUIRE)==GC_ML_OK);
    }
    assert(gc_native_queue_incoming_next_locked(&absent)==GC_NSI_WAIT&&absent.pin.token==token);
    for(unsigned i=0;i<count;i++)assert(gc_metadata_step_locked(&work->leases,&held[i],GC_ML_RELEASE)==GC_ML_OK);
    do{status=gc_native_queue_incoming_next_locked(&absent);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_DONE&&!absent.pin.token&&work->leases.active==active);
}
static void gc_ng_drain(NativeRuntimeOwner *r,NativeAllocationRun *run,NativeRunRetireCheck *blocked,const NativeGraphOps *ops){
    NativeWorkStorage *work=gc_native_runtime_work(r);MarkQueue *queue=&work->queues[0];
    unsigned completed=0,slices=0;bool spilled=false;
    {
        uint64_t serial=r->mark_scan_serial;r->mark_scan_serial=UINT64_MAX-1;
        NativeMarkScan rejected={0};MarkQueue before=*queue;
        assert(gc_native_mark_scan_begin_locked(r,0,&r->context,3,&rejected)==GC_MARK_EXHAUSTED);
        assert(!rejected.self&&!memcmp(&before,queue,sizeof(before))&&!r->mark_scan_count);
        r->mark_scan_serial=serial;
    }
    for(;;){
        NativeMarkScan scan={0};
        uint64_t status=gc_native_mark_scan_begin_locked(r,0,&r->context,3,&scan);
        if(status==GC_MARK_EMPTY)break;
        assert(status==GC_MARK_OK);
        assert(r->mark_scans==(uintptr_t)&scan&&r->mark_scan_count==1);
        NativeScanIncoming incoming={0};uint64_t target=scan.job.reference>>16;
        assert(gc_native_scan_incoming_begin_locked(r,target,target+16,&incoming)==GC_NSI_MORE);
        assert(gc_native_scan_incoming_next_locked(&incoming)==GC_NSI_FOUND);
        assert(gc_native_mark_scan_finish_locked(&scan)==GC_MARK_INVALID);
        NativeMarkScan copy=scan;
        assert(gc_native_mark_scan_enter_locked(&copy)==GC_RF_INVALID);
        if(!slices){
            assert(gc_native_allocation_retire_check_begin_locked(run,blocked)==GC_NSI_MORE);
            assert(gc_native_allocation_retire_check_next_locked(blocked)==GC_NSI_FOUND);
            /* Exhaust actual lease slots, then resume the same counted claim. */
            MetadataLeaseRequest owner={.token=scan.job.metadata_lease},held[64]={{0}};
            assert(gc_metadata_step_locked(&work->leases,&owner,GC_ML_RESOLVE)==GC_ML_OK);
            unsigned count=0;
            while(work->leases.free_head){
                held[count]=(MetadataLeaseRequest){owner.page,owner.incarnation,0};
                assert(gc_metadata_step_locked(&work->leases,&held[count++],GC_ML_ACQUIRE)==GC_ML_OK);
            }
            assert(gc_native_mark_scan_enter_locked(&scan)==GC_RF_METADATA_FULL&&scan.stage==1&&queue->in_flight==1);
            for(unsigned i=0;i<count;i++)assert(gc_metadata_step_locked(&work->leases,&held[i],GC_ML_RELEASE)==GC_ML_OK);
            assert(gc_native_mark_scan_enter_locked(&scan)==GC_RAC_READY);
            assert(gc_native_mark_scan_exit(&scan)==GC_RF_DONE); /* abandon before SCAN */
            assert(gc_native_mark_scan_release_locked(&scan)==GC_RF_DONE&&scan.stage==1);
        }
        assert(gc_native_mark_scan_enter_locked(&scan)==GC_RAC_READY);
        assert(gc_native_scan_incoming_next_locked(&incoming)==GC_NSI_WAIT);
        status=gc_native_mark_scan_read(&scan,ops->scan);assert(status==GC_SCAN_DONE||status==GC_SCAN_MORE);
        assert(scan.scan.examined<=3&&scan.scan.written<=3);
        assert(gc_native_mark_scan_exit(&scan)==GC_RF_DONE);
        assert(gc_native_mark_scan_release_locked(&scan)==GC_RF_DONE);
        NativeScanIncoming child_incoming={0};
        if(!slices){
            uint64_t child=scan.children[0]>>16;
            assert(child&&child!=target);
            assert(gc_native_scan_incoming_begin_locked(r,child,child+16,&child_incoming)==GC_NSI_MORE);
            /* No child lease exists yet: this finding is the captured buffer. */
            assert(!scan.child.context&&gc_native_scan_incoming_next_locked(&child_incoming)==GC_NSI_FOUND);
        }
        /* A native interval cannot discard the claim or captured children. */
        gc_allocation_fixture_native(r,true);gc_allocation_fixture_native(r,false);
        if(scan.scan.written)assert(gc_native_mark_scan_commit_locked(&scan,ops->commit)==GC_MARK_BUSY);
        while(scan.child_index<scan.scan.written){
            assert(gc_native_mark_scan_child_ack(&scan)==GC_MARK_BUSY);
            uint64_t offered=gc_native_mark_scan_child(&scan,GC_RM_ENTER,ops->pending);
            if(scan.child.current.reference){
                assert(offered==GC_RAC_READY);
                assert(gc_native_mark_scan_child(&scan,GC_RM_ROUTE,ops->pending)==GC_RM_EXIT_REQUIRED);
                assert(gc_native_mark_scan_child(&scan,GC_RM_EXIT,ops->pending)==GC_RM_OFFER_REQUIRED);
                if(!slices&&!scan.child_index){
                    assert(gc_native_mark_scan_child(&scan,GC_RM_CANCEL,ops->pending)==GC_RM_CANCELLED);
                    assert(gc_native_mark_scan_child_ack(&scan)==GC_MARK_BUSY);
                    assert(gc_native_mark_scan_commit_locked(&scan,ops->commit)==GC_MARK_BUSY);
                    assert(gc_native_mark_scan_child(&scan,GC_RM_RESET,ops->pending)==GC_RF_DONE);
                    assert(gc_native_mark_scan_child(&scan,GC_RM_ENTER,ops->pending)==GC_RAC_READY);
                    assert(gc_native_mark_scan_child(&scan,GC_RM_ROUTE,ops->pending)==GC_RM_EXIT_REQUIRED);
                    assert(gc_native_mark_scan_child(&scan,GC_RM_EXIT,ops->pending)==GC_RM_OFFER_REQUIRED);
                }
                offered=gc_native_mark_scan_child(&scan,GC_RM_OFFER,ops->pending);
                if(offered==GC_RM_RELEASE_REQUIRED)offered=gc_native_mark_scan_child(&scan,GC_RM_RELEASE,ops->pending);
            }
            assert(offered==GC_RM_COMPLETE);
            assert(gc_native_mark_scan_child_ack(&scan)==GC_MARK_OK);
            if(!slices&&scan.child_index==1){
                assert(gc_native_scan_incoming_next_locked(&child_incoming)==GC_NSI_MORE);
                assert(gc_native_scan_incoming_next_locked(&child_incoming)==GC_NSI_DONE);
                /* The child now belongs to the queue, which has its own audit. */
                assert(gc_mark_step_locked(queue,NULL,GC_MARK_CHECK)==GC_MARK_BUSY);
            }
        }
        NativeMarkScan second={0};bool two=!slices;
        if(two){
            gc_ng_queue_spill_probe(r);
            assert(gc_native_mark_scan_begin_locked(r,0,&r->context,3,&second)==GC_MARK_OK);
            assert(r->mark_scan_count==2&&second.next==(uintptr_t)&scan&&scan.previous==(uintptr_t)&second);
            assert(gc_native_scan_incoming_next_locked(&incoming)==GC_NSI_RESTART);
            incoming=(NativeScanIncoming){0};
            assert(gc_native_scan_incoming_begin_locked(r,target,target+16,&incoming)==GC_NSI_MORE);
            assert(gc_native_scan_incoming_next_locked(&incoming)==GC_NSI_MORE); /* scalar child */
        }
        spilled|=queue->overflow_count!=0;slices++;
        assert(gc_native_mark_scan_commit_locked(&scan,ops->commit)==GC_MARK_OK);
        if(status==GC_SCAN_DONE){
            assert(scan.stage==6&&scan.job.metadata_lease);
            assert(gc_native_mark_scan_release_locked(&scan)==GC_RF_DONE);completed++;
        }
        assert(scan.stage==7);
        assert(gc_native_mark_scan_commit_locked(&scan,ops->commit)==GC_MARK_INVALID);
        assert(gc_native_scan_incoming_next_locked(&incoming)==GC_NSI_MORE);
        assert(gc_native_scan_incoming_next_locked(&incoming)==GC_NSI_DONE);
        assert(gc_native_mark_scan_finish_locked(&scan)==GC_MARK_OK);
        assert(gc_native_scan_incoming_next_locked(&incoming)==GC_NSI_RESTART);
        assert(gc_native_mark_scan_finish_locked(&scan)==GC_MARK_INVALID);
        if(two){
            /* Remove the older control first; the younger claim stays live. */
            assert(r->mark_scans==(uintptr_t)&second&&r->mark_scan_count==1&&!second.next&&!second.previous);
            assert(gc_native_mark_scan_enter_locked(&second)==GC_RAC_READY);
            assert(gc_native_mark_scan_read(&second,ops->scan)==GC_SCAN_DONE&&!second.scan.written);
            assert(gc_native_mark_scan_exit(&second)==GC_RF_DONE);
            assert(gc_native_mark_scan_release_locked(&second)==GC_RF_DONE);
            assert(gc_native_mark_scan_commit_locked(&second,ops->commit)==GC_MARK_OK);
            assert(gc_native_mark_scan_release_locked(&second)==GC_RF_DONE);
            NativeQueueIncoming refill={0};uint64_t offset=gc_mark_spill_cell(queue,queue->overflow_head)->reference>>16;
            assert(gc_native_queue_incoming_begin_locked(r,offset,offset+16,&refill)==GC_NSI_MORE);
            while(!refill.pin.token)assert(gc_native_queue_incoming_next_locked(&refill)==GC_NSI_MORE);
            uint64_t serial=queue->last_serial,active=work->leases.active;
            assert(gc_mark_spill_locked(queue,NULL,GC_MARK_REFILL)==GC_MARK_OK&&queue->last_serial==serial);
            assert(gc_native_queue_incoming_next_locked(&refill)==GC_NSI_RESTART&&refill.pin.token);
            assert(gc_native_queue_incoming_cancel_locked(&refill)==GC_NSI_DONE&&work->leases.active==active-1);
            assert(gc_native_mark_scan_finish_locked(&second)==GC_MARK_OK);completed++;slices++;
        }
        assert(!r->mark_scans&&!r->mark_scan_count);
    }
    assert(completed==14&&slices==25&&spilled&&!work->leases.active);
    assert(gc_mark_step_locked(queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
}
static void gc_ng_root_probe(NativeRuntimeOwner *r,RootRequest *leaf){
    uint64_t offset=leaf->value>>16;NativeRootIncoming audit={0};
    assert(gc_native_root_incoming_begin_locked(r,offset,offset+16,&audit)==GC_NSI_MORE);
    assert(gc_native_root_incoming_next_locked(&audit)==GC_NSI_FOUND);
    NativeRootIncoming copy=audit;
    assert(gc_native_root_incoming_next_locked(&copy)==GC_NSI_INVALID);
    assert(r->roots.leases==1&&r->roots.walkers==1);
    assert(gc_native_root_incoming_cancel_locked(&audit)==GC_NSI_DONE&&!r->roots.leases&&!r->roots.walkers);
    audit=(NativeRootIncoming){0};
    assert(gc_native_root_incoming_begin_locked(r,offset,offset+16,&audit)==GC_NSI_MORE);
    assert(gc_native_root_incoming_next_locked(&audit)==GC_NSI_FOUND);
    /* The parent still strongly holds this leaf. Root deletion only removes
     * its direct root; the old value remains a journal obligation. */
    assert(gc_root_step(&r->context,leaf,GC_ROOT_RELEASE)==GC_ROOT_OK);*leaf=(RootRequest){0};
    uint64_t status;do{status=gc_native_root_incoming_next_locked(&audit);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_WAIT&&audit.lease.kind==1);
    gc_allocation_fixture_drain(r);
    RootStorage *storage=(void *)(uintptr_t)r->root_charge.allocation;
    RootLease reader={0};RootLifeRequest hold={.registration=(uintptr_t)&storage->registration,
        .scope=storage->segment.scope,.lease=(uintptr_t)&reader};
    assert(gc_root_life_locked(&r->roots,&hold,GC_RL_ACQUIRE)==GC_RL_OK);
    assert(gc_native_root_incoming_next_locked(&audit)==GC_NSI_MORE);
    assert(gc_native_root_incoming_next_locked(&audit)==GC_NSI_WAIT&&!audit.lease.kind);
    assert(gc_root_life_locked(&r->roots,&hold,GC_RL_RELEASE)==GC_RL_OK);
    assert(gc_native_root_incoming_next_locked(&audit)==GC_NSI_DONE);
    assert(gc_native_root_incoming_cancel_locked(&audit)==GC_NSI_DONE);
}
static void gc_ng_retire_roots(NativeRuntimeOwner *r){
    RootStorage *storage=(void *)(uintptr_t)r->root_charge.allocation;
    RootLease deletion={0};RootLifeRequest q={.registration=(uintptr_t)&storage->registration,
        .scope=storage->segment.scope,.context=(uintptr_t)&r->context,.lease=(uintptr_t)&deletion};
    assert(gc_root_life_locked(&r->roots,&q,GC_RL_RETIRE)==GC_RL_OK);
    NativeRootIncoming audit={0};
    assert(gc_native_root_incoming_begin_locked(r,GC_HEAP_PAGE_BYTES,2*GC_HEAP_PAGE_BYTES,&audit)==GC_NSI_MORE);
    assert(gc_root_life_locked(&r->roots,&q,GC_RL_CLAIM)==GC_RL_OK&&q.mask==0);
    uint64_t values[16]={0};RootScanJob scan={.scan={.segment=(uintptr_t)&storage->segment,
        .output=(uintptr_t)values,.capacity=q.end-q.begin},.lease=(uintptr_t)&deletion,.serial=q.serial};
    assert(gc_root_scan_leased(&scan)==GC_ROOT_OK);
    for(unsigned i=0;i<q.end-q.begin;i++)assert(!values[i]);
    uint64_t status;do{status=gc_native_root_incoming_next_locked(&audit);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_WAIT&&audit.lease.kind); /* deletion job not yet ACKed */
    assert(gc_root_life_locked(&r->roots,&q,GC_RL_ACK)==GC_RL_OK);
    assert(gc_root_life_locked(&r->roots,&q,GC_RL_CLEAN)==GC_RL_WAIT); /* observer still pins */
    do{status=gc_native_root_incoming_next_locked(&audit);}while(status==GC_NSI_MORE);
    assert(status==GC_NSI_DONE&&!audit.lease.kind);
    assert(gc_root_life_locked(&r->roots,&q,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&q)==GC_RL_OK);
    assert(gc_root_life_locked(&r->roots,&q,GC_RL_FINISH)==GC_RL_OK);
}
static void gc_run_native_graph_cases(const NativeGraphOps *ops){
    NativeRuntimeOwner r={0};NativeAllocationRun run={0};NativeHeapOps os=gc_native_heap_ops();
    NativeRuntimeConfig config={{4*GC_HEAP_PAGE_BYTES,2*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES,64*os.metadata.quantum},16,64,64,{8,64,2}};
    assert(gc_native_runtime_prepare(&r,&config,&os)==GC_NATIVE_METADATA_OK);
    assert(gc_native_runtime_activate(&r)==GC_NATIVE_METADATA_OK);
    uint64_t baseline=r.backing.heap.metadata_committed;
    assert(gc_native_allocation_reserve_locked(&run,&r,1,16,&os.metadata)==GC_POOL_OK);
    gc_allocation_fixture_native(&r,true);
    assert(gc_native_allocation_prepare(&run,&os.metadata)==GC_POOL_OK);
    assert(gc_native_allocation_publish_locked(&run)==GC_POOL_OK);
    gc_allocation_fixture_native(&r,false);
    assert(gc_native_allocation_bind_locked(&run)==GC_POOL_OK);
    RootRequest roots[14]={{0}};AllocationResult objects[14]={{0}};
    for(unsigned i=0;i<14;i++){
        assert(gc_native_allocation_start(&run,i==13?GC_DESC_POINTER_RECORD:GC_DESC_RAW_BYTES,
            i==13?35*8:17,&roots[i],&objects[i],ops->allocate)==GC_ALLOC_OK);
        if(i<13)gc_allocation_fixture_publish(&run,&roots[i],17);
    }
    NativeWorkStorage *work=gc_native_runtime_work(&r);
    NativeBuiltinRegistry *registry=(void *)(uintptr_t)r.descriptor_charge.allocation;
    ReferenceLease parent={.reference=roots[13].value};
    assert(gc_reference_lease_locked(&work->leases,&parent)==GC_ML_OK);
    ResolveRequest access={.reference=parent.reference,.scope={.context=(uintptr_t)&r.context}};
    assert(gc_resolve_access((void *)(uintptr_t)parent.lease.page,&access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
    GuardedMarkPrepare init={.scope=(uintptr_t)&access.scope,.page=parent.lease.page,
        .phase=r.roots.phase,.reference=parent.reference,.lease=parent.lease};
    for(unsigned i=0;i<35;i++){
        InitializationField field={(uintptr_t)&registry->registry,i*8,i%15==14?0:roots[i%14].value,1};
        assert(gc_guarded_initialization_store(&init,&field)==GC_INIT_OK);
    }
    assert(gc_guarded_initialization_publish(&init)==GC_INIT_OK);
    assert(gc_resolve_access((void *)(uintptr_t)parent.lease.page,&access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
    assert(gc_metadata_step_locked(&work->leases,&parent.lease,GC_ML_RELEASE)==GC_ML_OK);
    parent=(ReferenceLease){0};init=(GuardedMarkPrepare){0};
    assert(gc_native_allocation_close_locked(&run)==GC_POOL_OK);
    gc_ng_root_probe(&r,&roots[0]);
    for(unsigned i=1;i<13;i++)assert(gc_root_step(&r.context,&roots[i],GC_ROOT_RELEASE)==GC_ROOT_OK);
    memset(objects,0,sizeof(objects));memset(roots,0,13*sizeof(*roots));
    gc_allocation_fixture_drain(&r);
    CycleTicket ticket={0};assert(gc_native_cycle_request_locked(&r,&ticket)==GC_CY_OK);
    gc_nc_start(&r,0,ops->start);gc_ng_roots(&r,ops,true);
    /* The root is acknowledged, but its queued graph still prevents mark end. */
    assert(gc_native_cycle_mark_locked(&r,0,GC_MP_BEGIN)==GC_MP_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_mark_locked(&r,0,GC_MP_DECIDE)==GC_MP_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_mark_locked(&r,0,GC_MP_RELEASE)==GC_MP_RETRY);gc_nc_poll(&r,false);
    NativeRunRetireCheck blocked={0};gc_ng_drain(&r,&run,&blocked,ops);
    for(unsigned i=0;i<14;i++){
        MarkRecord *record=(MarkRecord *)(uintptr_t)run.marks.records+i;
        assert(record->young_epoch==work->queues[0].epoch&&!record->old_epoch);
    }
    assert(gc_root_step(&r.context,&roots[13],GC_ROOT_RELEASE)==GC_ROOT_OK);memset(roots,0,sizeof(roots));
    RootStorage *storage=(void *)(uintptr_t)r.root_charge.allocation;
    RootLease journal_lease={0};RootChange change={0};
    RootJournalRequest journal={.registration=(uintptr_t)&storage->registration,.scope=storage->segment.scope,
        .lease=(uintptr_t)&journal_lease,.output=(uintptr_t)&change,.capacity=1};
    assert(gc_root_journal_step(&r.roots,&journal,GC_RJ_CLAIM)==GC_RJ_OK);
    assert(gc_root_journal_step(&r.roots,&journal,GC_RJ_COPY)==GC_RJ_OK&&journal.written==1);
    assert(change.previous&&!change.next&&change.phase==r.roots.phase);
    gc_ng_offer(&r,change.previous,ops);gc_ng_offer(&r,change.next,ops);
    assert(gc_root_journal_step(&r.roots,&journal,GC_RJ_ACK)==GC_RJ_OK);change=(RootChange){0};
    assert(!work->leases.active&&gc_mark_step_locked(&work->queues[0],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
    gc_ng_roots(&r,ops,false);
    assert(gc_native_cycle_mark_locked(&r,0,GC_MP_BEGIN)==GC_MP_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_mark_locked(&r,0,GC_MP_DECIDE)==GC_MP_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_mark_locked(&r,0,GC_MP_RELEASE)==GC_MP_SELECT);gc_nc_poll(&r,false);
    /* Retain the marked run during selection. After dropping the only external
     * root and draining its SATB journal, every graph edge is inside this one
     * dead run. No other runs, actors, handles, jobs or incoming edges exist. */
    assert(gc_native_cycle_move_locked(&r,0,GC_RP_BEGIN)==GC_RP_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_move_locked(&r,0,GC_RP_ACTIVATE)==GC_RP_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_move_locked(&r,0,GC_RP_RELEASE)==GC_RP_RELOCATING);gc_nc_poll(&r,false);
    assert(gc_relocation_gate_locked(&r.cycle[0].gate,NULL,GC_RG_CLOSE)==GC_RG_OK);
    assert(gc_native_cycle_retire_locked(&r,0,GC_RR_BEGIN)==GC_RP_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_retire_locked(&r,0,GC_RR_RELEASE)==GC_RP_RETIRING);gc_nc_poll(&r,false);
    gc_ng_retire_roots(&r);
    assert(gc_native_allocation_dead_seal_locked(&run)==GC_SR_DONE);
    SourceUncommit dead={.head_index=run.lease.head_index,.page_count=1,.incarnation=run.lease.incarnation};
    assert(gc_source_uncommit_locked(&r.backing.heap,&dead,GC_SU_BEGIN)==GC_SU_OK);
    gc_allocation_fixture_native(&r,true);
    assert(gc_native_source_uncommit_payload(&r.backing.heap,&dead,NULL)==GC_NATIVE_METADATA_OK);
    assert(gc_source_uncommit_locked(&r.backing.heap,&dead,GC_SU_FINISH)==GC_SU_OK);
    assert(gc_native_allocation_retire_check_next_locked(&blocked)==GC_NSI_RESTART);
    gc_allocation_fixture_retire_reject(&run,&blocked,GC_SR_WAIT);
    assert(gc_native_allocation_retire_check_cancel_locked(&blocked)==GC_NSI_DONE);
    assert(gc_allocation_fixture_retire_begin(&run)==GC_SR_MORE);
    uint64_t status;do{status=gc_native_allocation_retire_import_locked(&run);}while(status==GC_SR_MORE);
    assert(status==GC_SR_DONE&&gc_native_allocation_retire_cleanup(&run,&os.metadata)==GC_POOL_OK);
    assert(gc_native_allocation_retire_step_locked(&run)==GC_SR_DONE);
    assert(gc_native_records_begin_locked(&run)==GC_SR_DONE);
    assert(gc_native_records_dispose(&run,&os.metadata)==GC_POOL_OK);
    assert(gc_native_records_finish_locked(&run)==GC_POOL_OK);
    gc_allocation_fixture_native(&r,false);
    assert(gc_root_retire_audit_step_locked(&r.roots,&r.cycle[0].retirement_audit,GC_RA_BEGIN)==GC_RA_DONE);
    assert(gc_native_cycle_complete_locked(&r,0,GC_NC_BEGIN)==GC_NC_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_complete_locked(&r,0,GC_NC_DECIDE)==GC_NC_ADVANCED);gc_nc_poll(&r,true);
    assert(gc_native_cycle_complete_locked(&r,0,GC_NC_RELEASE)==GC_NC_COMPLETE);gc_nc_poll(&r,false);
    assert(gc_cycle_step_locked(&r.cycles,&ticket,GC_CY_POLL)==GC_CY_OK);
    assert(!r.allocations&&!r.backing.heap.committed&&r.backing.heap.metadata_committed==baseline);
    assert(gc_native_runtime_dispose(&r,&os)==GC_NATIVE_METADATA_OK);
    puts("native graph cycle: rooted cyclic graph, bounded scans, counted spill and journal handoff through retirement PASS");
    puts("native mark scan owner: lease exhaustion, guarded restart and child cancellation preserve claims through bounded commit PASS");
    puts("native scan inventory: captured children prevent incoming absence, terminal unlink invalidates audit cursors PASS");
    puts("native source retirement gate: bound scan receipt required, stale or mismatched audits preserve page and budget ownership PASS");
    puts("native queue incoming: leased spill cursors survive pressure and invalidate on serial-preserving refill PASS");
    puts("native root incoming: live cells, deletion journals and reader snapshots block absence until handoff, retiring scopes stay pinned PASS");
}
#endif
