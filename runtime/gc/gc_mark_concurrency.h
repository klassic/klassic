#include "gc_heap_audit.h"
#include "gc_heap_audit_slice.h"
#include "gc_object_walk.h"
#include "gc_heap_walk.h"
#include "gc_heap_incoming.h"
/* Integration fixture: actual heap objects, bounded generated scans, mark
 * queues, native leases, roots and SATB. A controlled Old-owner move retains
 * remembered claims while its source mapping becomes inaccessible. General
 * moving-chain dispatch, cycle scheduling and production lowering remain open. */
#ifndef KLASSIC_GC_MARK_CONCURRENCY_H
#define KLASSIC_GC_MARK_CONCURRENCY_H
#include "gc_metadata_concurrency.h"
#include "gc_mark_cases.h"
#include "gc_mark_spill.h"
#include "gc_budget_cases.h"
#include "gc_reference_lease_cases.h"
#include "gc_resolve_cases.h"
#include "gc_source_access_cases.h"
#include "gc_barrier_cases.h"
#include "gc_mark_prepare_cases.h"
#include "gc_mark_route_cases.h"
#include "gc_worker_cases.h"
#include "gc_root_lifetime.h"
#include "gc_root_access.h"
#include "gc_root_journal_cases.h"
#include "gc_root_audit_cases.h"
#include "gc_mark_end_cases.h"
#include "gc_mark_phase_cases.h"
#include "gc_mark_move_pipeline.h"
#include "gc_remembered_owner_cases.h"
#include "gc_remembered_inventory_cases.h"
#include "gc_remembered_walk_cases.h"
#include "gc_remembered_lease_cases.h"
#include "gc_remembered_load_cases.h"
#include "gc_remembered_transfer_cases.h"
typedef uint64_t (*GraphMarkSpill)(MarkQueue *,MarkRequest *,uint64_t);
#include "gc_mark_scan_cases.h"
#include "gc_source_drain_cases.h"
#include "gc_source_uncommit_cases.h"
#include "gc_source_retire.h"
#include "gc_forward_incoming_cases.h"
#include "gc_relocation_job.h"
#include "gc_relocation_complete.h"
#include "gc_relocation_claim.h"
#include "gc_alloc_into.h"
#include "gc_relocation_gate_cases.h"
#include "gc_forward_compress.h"
#include "gc_cycle_mark_start.h"
#include "gc_relocation_phase.h"
#include "gc_rooted_load_admit.h"
typedef uint64_t (*GraphRootStep)(MutatorContext *,RootRequest *,uint64_t);
typedef uint64_t (*GraphRootScan)(RootScan *);
typedef uint64_t (*GraphRootDrain)(RootDrain *);
typedef struct {
    WorkerStep worker;
    uint64_t (*handshake)(HandshakeState *,ThreadRegistration *,uint64_t);
    uint64_t (*life)(RootRegistry *,RootLifeRequest *,uint64_t);
    uint64_t (*clear)(RootLifeRequest *);
    uint64_t (*leased)(RootScanJob *);
    RootJournalStep journal;
    BufferJobStep buffer_job;
    RootAuditor audit;
    MarkEndChecker end;
    MarkPhaseStepper phase;
    uint64_t (*walk_scan)(RootWalkScan *);
    uint64_t (*walk_heal)(RootWalkHeal *);
    uint64_t (*walk_item)(RootWalkItem *,uint64_t);
    RelocationGateStep relocation_gate;
    uint64_t (*relocation_complete)(RelocationGate *,RelocationCompletion *);
    uint64_t (*relocation_claim)(RelocationGate *,RelocationClaim *);
    uint64_t (*cycle)(CycleLedger *,CycleTicket *,uint64_t);
    CycleMarkStarter cycle_start;
    GuardedIdentity identity;GuardedBlockSize block_size;
    MarkedRelocationClaimer marked_claim;
    RelocationPhaseStep relocation_phase,relocation_retire;
    RootAuditor retire_audit;
    MarkEndChecker retire_inventory;
    SourceRetirer source_retire;
} GraphLifecycle;
typedef struct {
    MarkTask tasks[16];MarkRecord records[8];ForwardTable forwards;ForwardCell cells[8];MetadataLeaseSlot leases[32];
    ForwardTable moved_forwards,array_forwards;ForwardCell moved_cells[3],array_cells[4];uint64_t remembered_bits[2][256],empty_bits[2][256];
    MarkRecord reused_records[2];ForwardTable reused_tables[2];ForwardCell reused_cells[2];
} GraphStorage;
// Source tables have independent lifetimes from the live graph and records.
typedef struct { ForwardTable table;ForwardCell cell; } GraphSourceStorage;
typedef struct {const MarkMoveOps *move;RememberedInventoryStep inventory;RememberedOwnerFinder find;RememberedLoader load;RememberedWalker walk;RememberedAcquirer acquire;RememberedTransferStep transfer;uint64_t (*buffer)(MutatorContext *,const BarrierEntry *,uint64_t);uint64_t (*root_access)(MetadataLeasePool *,RootAccess *);PromotionRememberStep promote;RootedLoader rooted_load;RootedLoadAdmitter admit_load;PendingFollower pending_follow;PendingAccessor pending_access;} GraphRememberedOps;
typedef struct {
    MetadataRace m;GraphLifecycle lifecycle;RootRegistry root_registry;HandshakeState handshake;
    MarkPhaseControl phase_control;
    CycleLedger cycles;CycleTicket active_cycle;uint64_t cycle_requesters[2];
    RelocationGate relocation;
    struct {RelocationPermit permit;RelocationGateRequest request;ForwardCell *source;RelocationJob job;RelocationCompletion completion;} moves[8];
    unsigned moves_used;
    RememberedInventory remembered;
    GraphRememberedOps rem_ops;RememberedSet rem_set;RememberedRegistration rem_registration;
    RememberedSet empty_set;RememberedRegistration empty_registration;
    AllocationPage *old_page;uint64_t old_owner,moved_owner;
    _Atomic uint64_t owner_remaps,rem_transferred,mark_retargets,array_parked,array_resume,scan_metadata_wait,array_metadata_wait,destination_wait,scan_busy_wait;
    MetadataLeaseRequest destination_held[32];unsigned destination_held_count;bool destination_injected;
    MetadataLeaseRequest scan_held[32];unsigned scan_held_count;bool scan_pressure_injected;
    MarkQueue queue;MarkTask *tasks;MarkRecord *records;MetadataLeaseSlot *leases;
    AllocationResult objects[8];DescriptorRegistry registry;TypeDescriptor descriptors[3];
    _Atomic uint64_t descriptor_slots[4];uint64_t bitmap;
    MarkRouter route;MarkStep mark;GraphMarkSpill spill;MarkScanner scan;GuardedHealer heal;ReferenceAcquirer reference_lease;Resolver resolve;AllocationPage *page;
    uint64_t spill_offers,refills,full_claims,root_repairs,root_conflicts,audit_hops;
    _Atomic uint64_t old_skips,worker_repairs,worker_conflicts;
    _Atomic uint64_t parked,start,stop,slices,slots,native_gate,marking_ready;
    _Atomic uint64_t pending_ready,children_parked,children_resume,pending_segment,offer_metadata_wait,offer_busy_wait;
    _Atomic uint64_t rem_ready,rem_claimed,rem_resume,rem_loaded,rem_continue,rem_done,rem_flushes;
    _Atomic uint64_t rem_output_segment;
    _Atomic uint64_t offer_advanced,offer_advance_resume;
    _Atomic uint64_t journal_steps,journal_resume,journal_job,journal_reference,journal_token,journal_destination_wait;
    _Atomic uint64_t scan_steps,scan_resume,scan_cursor,scan_job;
    MetadataLeaseRequest journal_held[32];unsigned journal_held_count;bool journal_pressure_injected;
    _Atomic uint64_t rem_walk_parked,rem_walk_resume;
    _Atomic uint64_t rem_metadata_wait,rem_access_metadata_wait,rem_access_advanced,rem_access_resume,rem_access_flushes;
    /* Predicate and notification share the registry mutex: release before
     * wait cannot lose a wakeup, and spurious wakeups recheck the predicate. */
    uint64_t metadata_resume;
    _Atomic uint64_t audit_source_wait,audit_destination_wait,audit_token;
    MetadataLeaseRequest audit_held[32];unsigned audit_held_count;
    bool audit_pressure,audit_destination_injected;
#ifdef _WIN32
    CONDITION_VARIABLE metadata_available;
#else
    pthread_cond_t metadata_available;
#endif
} MarkGraph;
// Target admission/publication shares the heap lock with reference leasing;
// the coordinator exclusively owns append rights through copy completion.
static uint64_t graph_target(MarkGraph *g,AllocationPage *page,RelocationCopy *copy,uint64_t op){
    metadata_lock(&g->m);uint64_t status=g->rem_ops.move->target(page,copy,op);metadata_unlock(&g->m);return status;
}
static uint64_t graph_move_claim(MarkGraph *g,ForwardCell *source,ForwardRequest *q){
    assert(g->moves_used<8);unsigned i=g->moves_used++;
    g->moves[i].source=source;
    RelocationClaim claim={.permit=(uintptr_t)&g->moves[i].permit,.source=(uintptr_t)source,
        .source_offset=q->source_offset,.incarnation=q->incarnation};
    metadata_lock(&g->m);uint64_t status=g->lifecycle.relocation_claim(&g->relocation,&claim);metadata_unlock(&g->m);
    if(status==GC_RCL_OWNED){
        q->ticket=claim.ticket;g->moves[i].request=(RelocationGateRequest){claim.permit,claim.ticket,claim.epoch};
        return GC_FWD_OK;
    }
    return status==GC_RCL_BUSY?GC_FWD_BUSY:status==GC_RCL_RETAINED?GC_FWD_RETAINED:
        status==GC_RCL_FORWARDED?GC_FWD_PUBLISHED:GC_FWD_INVALID;
}
static uint64_t graph_move_publish(MarkGraph *g,ForwardCell *source,RelocationCopy *q,uint64_t color){
    unsigned i=0;while(i<g->moves_used&&(g->moves[i].source!=source||!g->moves[i].permit.serial))i++;
    assert(i<g->moves_used);RelocationJob *job=&g->moves[i].job;
    RelocationCompletion *completion=&g->moves[i].completion;
    metadata_lock(&g->m);
    if(!completion->owner){
        *job=(RelocationJob){.permit=(uintptr_t)&g->moves[i].permit,.source=(uintptr_t)source,
            .ticket=q->ticket,.destination=(q->destination<<16)|color};
        *completion=(RelocationCompletion){.job=(uintptr_t)job,.copy=(uintptr_t)q};
    }
    assert(completion->copy==(uintptr_t)q);
    uint64_t status=GC_RC_MORE;
    for(unsigned step=0;step<4&&status==GC_RC_MORE;step++){
        status=g->lifecycle.relocation_complete(&g->relocation,completion);
        if(status==GC_RC_MORE&&completion->stage<=2)
            assert(g->lifecycle.relocation_gate(&g->relocation,&g->moves[i].request,GC_RG_RELEASE)==GC_RG_INVALID);
    }
    metadata_unlock(&g->m);
    return status==GC_RC_DONE?GC_COPY_DONE:status==GC_RC_WAIT?GC_COPY_WAIT:GC_COPY_INVALID;
}
static uint64_t graph_source_drain(MarkGraph *g,SourceDrainer drain,AllocationPage *page,SourceDrain *q){
    metadata_lock(&g->m);uint64_t frozen=g->lifecycle.relocation_gate(&g->relocation,NULL,GC_RG_CLOSE);
    assert(frozen==GC_RG_OK||frozen==GC_RG_WAIT);
    if(frozen==GC_RG_WAIT){metadata_unlock(&g->m);return GC_SD_WAIT;}
    uint64_t status=drain(page,q);
    if(status==GC_SD_DONE)assert(g->lifecycle.relocation_gate(&g->relocation,NULL,GC_RG_REOPEN)==GC_RG_OK);
    metadata_unlock(&g->m);return status;
}
static void graph_uncommit_payload(MarkGraph *g,SourceUncommitter uncommit,AllocationPage *page){
    SourceUncommit q={.head_index=page->run_head,.page_count=(page->limit-page->start)/GC_HEAP_PAGE_BYTES,.incarnation=page->incarnation};
    metadata_lock(&g->m);uint64_t before=g->m.f.h.committed,metadata=g->m.f.h.metadata_committed;
    assert(uncommit(&g->m.f.h,&q,GC_SU_BEGIN)==GC_SU_OK);
    assert(q.stage==1&&q.committed_mapping&&page->owner==(uintptr_t)&q);
    assert(uncommit(&g->m.f.h,&q,GC_SU_FINISH)==GC_SU_INVALID); // cleanup has not succeeded
    assert(g->m.f.h.committed==before);metadata_unlock(&g->m);
    pool_vm_decommit((void *)(uintptr_t)(page->heap_base+page->start),q.page_count*GC_HEAP_PAGE_BYTES);
    q.committed_mapping=0;metadata_lock(&g->m);
    assert(uncommit(&g->m.f.h,&q,GC_SU_FINISH)==GC_SU_OK);
    assert(g->m.f.h.committed==before-q.page_count*GC_HEAP_PAGE_BYTES&&g->m.f.h.metadata_committed==metadata);
    assert(!page->owner&&atomic_load(&page->state)==GC_PAGE_UNCOMMITTED);
    uint64_t after=g->m.f.h.committed;assert(uncommit(&g->m.f.h,&q,GC_SU_FINISH)==GC_SU_INVALID);
    assert(g->m.f.h.committed==after);metadata_unlock(&g->m);
}
// Actual OS owners register and poll between finite scans/offers. Pending
// children are committed while their parent claim remains in-flight; only
// then can the owner acknowledge the next coordinator-selected snapshot.
static uint64_t graph_worker_call(MarkGraph *g,ThreadRegistration *p,uint64_t op){
    WorkerRequest request={(uintptr_t)&g->handshake,(uintptr_t)p};
    metadata_lock(&g->m);uint64_t result=g->lifecycle.worker(&g->root_registry,&request,op);
    if(op==GC_WK_LEAVE_NATIVE&&result==GC_WK_OK){
        MutatorContext *ctx=(MutatorContext *)(uintptr_t)p->context;
        assert(atomic_load(&ctx->adopted_phase)==g->root_registry.phase);
    }
    metadata_unlock(&g->m);return result;
}
static void graph_poll(MarkGraph *g,ThreadRegistration *p){
    uint64_t status;while((status=graph_worker_call(g,p,GC_WK_POLL))!=GC_WK_OK){assert(status==GC_WK_PARK);metadata_yield();}
}
// Called without a guard or partial token. The registered claim/root remains
// published throughout the native interval; the coordinator releases tokens
// and updates the predicate under the same mutex as this condition wait.
static void graph_metadata_wait(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,_Atomic uint64_t *waits){
    assert(!atomic_load(&ctx->access_epoch));
    uint64_t previous_phase=atomic_load(&ctx->adopted_phase),entered;
    while((entered=graph_worker_call(g,participant,GC_WK_ENTER_NATIVE))!=GC_WK_OK){
        assert(entered==GC_WK_RETRY);graph_poll(g,participant);
    }
    metadata_lock(&g->m);atomic_fetch_add_explicit(waits,1,memory_order_release);
    while(!g->metadata_resume){
#ifdef _WIN32
        assert(SleepConditionVariableCS(&g->metadata_available,&g->m.lock,INFINITE));
#else
        assert(pthread_cond_wait(&g->metadata_available,&g->m.lock)==0);
#endif
    }
    metadata_unlock(&g->m);
    uint64_t left;while((left=graph_worker_call(g,participant,GC_WK_LEAVE_NATIVE))!=GC_WK_OK){assert(left==GC_WK_RETRY);metadata_yield();}
    // A late waiter may already have adopted the rendezvous phase before
    // entering native. Successful LEAVE checks the current phase under lock;
    // entering/exiting without another phase change is also valid.
    assert(atomic_load(&ctx->adopted_phase)>=previous_phase);
}
// Returns with registry/queue lock held and every registered producer either
// acknowledged or native-blocked. No roots, journals or heap are scanned here.
static void graph_end_check_enter(MarkGraph *g,ThreadRegistration *owner){
    metadata_lock(&g->m);
    assert(g->lifecycle.phase(&g->root_registry,&g->phase_control,GC_MP_BEGIN)==GC_MP_ADVANCED);
    metadata_unlock(&g->m);
    assert(graph_worker_call(g,owner,GC_WK_POLL)==GC_WK_PARK);
    for(;;){metadata_lock(&g->m);
        if(g->handshake.acknowledged==g->handshake.expected)return;
        metadata_unlock(&g->m);metadata_yield();}
}
// Integration end predicate over actual membership with the registry lock
// held and a completed rendezvous. Only per-owner metadata is visited here.
static bool graph_inventory_end(MarkGraph *g,const RootAudit *completed) {
    MarkEndRequest request={(uintptr_t)&g->handshake,(uintptr_t)completed,g->handshake.generation,(uintptr_t)&g->remembered};
    uint64_t result=g->lifecycle.end(&g->root_registry,&request,0);
    assert(result==GC_ME_READY||result==GC_ME_WAIT||result==GC_ME_STALE);
    return result==GC_ME_READY;
}
// Caller owns the common registry lock and keeps the rendezvous active.
// This inventory predicate does not replace heap/gate/forwarding proofs.
static uint64_t graph_retire_inventory_check(MarkGraph *g,RootAudit *audit){
    MarkEndRequest q={(uintptr_t)&g->handshake,(uintptr_t)audit,g->handshake.generation,(uintptr_t)&g->remembered};
    return g->lifecycle.retire_inventory(&g->root_registry,&q,0);
}
static void graph_end_check_leave(MarkGraph *g,ThreadRegistration *owner,uint64_t expected){
    assert(g->lifecycle.phase(&g->root_registry,&g->phase_control,GC_MP_DECIDE)==GC_MP_ADVANCED);
    metadata_unlock(&g->m);
    assert(graph_worker_call(g,owner,GC_WK_POLL)==GC_WK_PARK);
    for(;;){metadata_lock(&g->m);
        uint64_t result=g->lifecycle.phase(&g->root_registry,&g->phase_control,GC_MP_RELEASE);
        if(result!=GC_MP_WAIT){assert(result==expected);metadata_unlock(&g->m);break;}
        metadata_unlock(&g->m);metadata_yield();}
    graph_poll(g,owner);
    assert(atomic_load(&g->m.ctx.adopted_phase)==g->root_registry.phase);
}

typedef struct {AllocationPage *page;ResolveRequest request;} GraphAccess;
enum { GRAPH_ACCESS_READY=0,GRAPH_ACCESS_BUSY=1,GRAPH_ACCESS_FULL=2 };
/* Coordinator/control producers keep this fixture chain strongly reachable and its
 * native metadata alive throughout this no-poll call. Canonical metadata
 * replaces the input token before guarded access. The controlled chain is
 * finite. Destination FULL releases the temporary lease and discards the
 * private destination; the caller must retry from its original strong producer.
 * No private destination crosses a poll/native transition. */
static uint64_t graph_try_enter(MarkGraph *g,MutatorContext *ctx,MetadataLeaseRequest *lease,uint64_t reference,bool cooperative,GraphAccess *out) {
    for(;;){
        ReferenceLease cursor={.reference=reference,.lease=*lease};
        GraphAccess scope={.page=(AllocationPage *)(uintptr_t)lease->page,.request={.scope={.context=(uintptr_t)ctx}}};
        assert(scope.page->incarnation==lease->incarnation);
        uint64_t status;
        do {
            metadata_lock(&g->m);
            status=g->rem_ops.pending_access(&g->m.leases,&cursor,&scope.request);
            metadata_unlock(&g->m);
        }while(status==GC_RF_WAIT&&!cooperative);
        if(status==GC_RF_WAIT){
            assert(!atomic_load(&ctx->access_epoch));metadata_lock(&g->m);
            assert(g->m.metadata(&g->m.leases,lease,GC_ML_RELEASE)==GC_ML_OK);
            metadata_unlock(&g->m);return GRAPH_ACCESS_BUSY;
        }
        if(status==GC_RAC_READY){*out=scope;return GRAPH_ACCESS_READY;}
        assert(status==GC_RF_ADVANCED&&!atomic_load(&ctx->access_epoch));
        assert(!scope.request.reference&&!scope.request.destination&&!scope.request.scope.held);
        MetadataLeaseRequest retired={0};metadata_lock(&g->m);
        uint64_t acquired=g->rem_ops.pending_follow(&g->m.leases,&cursor,&retired);
        if(acquired==GC_RF_METADATA_FULL){
            assert(cursor.reference==reference&&!memcmp(&cursor.lease,lease,sizeof(*lease))&&!retired.token);
            assert(g->m.metadata(&g->m.leases,lease,GC_ML_RELEASE)==GC_ML_OK);
            metadata_unlock(&g->m);return GRAPH_ACCESS_FULL;
        }
        assert(acquired==GC_RF_ADVANCED&&!retired.token);
        reference=cursor.reference;*lease=cursor.lease;metadata_unlock(&g->m);
        if(scope.page->generation==GC_OLD)atomic_fetch_add_explicit(&g->owner_remaps,1,memory_order_relaxed);
    }
}
static GraphAccess graph_enter(MarkGraph *g,MutatorContext *ctx,MetadataLeaseRequest *lease,uint64_t reference){
    GraphAccess scope={0};assert(graph_try_enter(g,ctx,lease,reference,false,&scope)==GRAPH_ACCESS_READY);return scope;
}
static void graph_exit(MarkGraph *g,GraphAccess *scope) {
    uint64_t status;
    do {status=g->resolve(scope->page,&scope->request,GC_RESOLVE_EXIT);}while(status==GC_RESOLVE_WAIT);
    assert(status==GC_RESOLVE_OK);
}
/* The authenticated queue claim retains its original reference/token while
 * this exclusive consumer owns a separate resumable access cursor. A cursor
 * edge transfers matching metadata protection before polling; it does not
 * mutate the queue's receipt. Only a canonical guarded MarkPrepare permits
 * retargeting. No private forwarding result crosses a poll or native wait. */
static GraphAccess graph_scan_enter(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,MarkRequest *job,MetadataLeaseRequest *access){
    assert(!access->token);
    ReferenceLease cursor={.reference=job->reference};
    for(;;){
        if(!cursor.lease.token){
            ReferenceLease fresh={.reference=cursor.reference};metadata_lock(&g->m);
            // Inject at the actual acquisition, under its lock. Exhausting the
            // pool at an earlier rendezvous is insufficient: another worker may
            // return a temporary token before this claimant attempts acquisition.
            if(job->record==(uintptr_t)&g->records[0]&&job->cursor==64&&!g->scan_pressure_injected){
                while(g->m.leases.free_head){
                    assert(g->scan_held_count<32);
                    MetadataLeaseRequest *held=&g->scan_held[g->scan_held_count++];
                    *held=(MetadataLeaseRequest){(uintptr_t)g->page,g->page->incarnation,0};
                    assert(g->m.metadata(&g->m.leases,held,GC_ML_ACQUIRE)==GC_ML_OK);
                }
                assert(g->scan_held_count&&g->m.leases.active==32&&!g->metadata_resume);
                g->scan_pressure_injected=true;
            }
            uint64_t acquired=g->reference_lease(&g->m.leases,&fresh);metadata_unlock(&g->m);
            if(acquired==GC_ML_FULL){
                assert(!fresh.lease.token&&!atomic_load(&ctx->access_epoch));
                MarkRequest retained=*job;
                bool partial_array=job->record==(uintptr_t)&g->records[0]&&job->cursor==64;
                graph_metadata_wait(g,ctx,participant,partial_array?&g->array_metadata_wait:&g->scan_metadata_wait);
                assert(!memcmp(&retained,job,sizeof(retained)));continue;
            }
            assert(acquired==GC_ML_OK);cursor.lease=fresh.lease;
        }
        GraphAccess guard={.page=(AllocationPage *)(uintptr_t)cursor.lease.page,
            .request={.scope={.context=(uintptr_t)ctx}}};
        /* Authentication and guard entry share the heap lock with metadata
         * retirement. Forwarding results stay private to the common helper. */
        metadata_lock(&g->m);
        uint64_t resolved=g->rem_ops.pending_access(&g->m.leases,&cursor,&guard.request);
        metadata_unlock(&g->m);
        if(resolved==GC_RF_WAIT){
            assert(!atomic_load(&ctx->access_epoch));MarkRequest retained=*job;
            atomic_fetch_add_explicit(&g->scan_busy_wait,1,memory_order_release);
            graph_poll(g,participant);metadata_yield();
            assert(!memcmp(&retained,job,sizeof(retained)));continue;
        }
        if(resolved==GC_RF_ADVANCED){
            assert(!atomic_load(&ctx->access_epoch));MarkRequest retained=*job;
            assert(!guard.request.reference&&!guard.request.destination&&!guard.request.scope.held);
            MetadataLeaseRequest retired={0};metadata_lock(&g->m);
            // Force real destination pressure only with a one-task queue: no
            // other marker can hold a payload slice and start offers concurrently.
            // The coordinator later releases these separately owned obligations.
            if(g->queue.capacity==1&&!g->destination_injected){
                while(g->m.leases.free_head){
                    assert(g->destination_held_count<32);
                    MetadataLeaseRequest *held=&g->destination_held[g->destination_held_count++];
                    *held=(MetadataLeaseRequest){cursor.lease.page,cursor.lease.incarnation,0};
                    assert(g->m.metadata(&g->m.leases,held,GC_ML_ACQUIRE)==GC_ML_OK);
                }
                assert(g->destination_held_count&&g->m.leases.active==32);
                g->destination_injected=true;g->metadata_resume=0;
            }
            ReferenceLease retained_cursor=cursor;
            uint64_t acquired=g->rem_ops.pending_follow(&g->m.leases,&cursor,&retired);
            if(acquired==GC_RF_METADATA_FULL){
                assert(!retired.token&&cursor.lease.token&&!memcmp(&cursor,&retained_cursor,sizeof(cursor)));metadata_unlock(&g->m);
                guard=(GraphAccess){0};
                graph_metadata_wait(g,ctx,participant,&g->destination_wait);
                assert(!memcmp(&retained,job,sizeof(retained)));continue;
            }
            assert(acquired==GC_RF_ADVANCED&&!retired.token);metadata_unlock(&g->m);
            if(guard.page->generation==GC_OLD)atomic_fetch_add_explicit(&g->owner_remaps,1,memory_order_relaxed);
            guard=(GraphAccess){0};
            if(job->record==(uintptr_t)&g->records[0]&&job->cursor==64){
                atomic_store(&g->scan_cursor,(uintptr_t)&cursor);atomic_store(&g->scan_job,(uintptr_t)job);
                uint64_t step=atomic_fetch_add_explicit(&g->scan_steps,1,memory_order_release)+1;
                while(atomic_load_explicit(&g->scan_resume,memory_order_acquire)<step){graph_poll(g,participant);metadata_yield();}
            }
            graph_poll(g,participant);assert(!memcmp(&retained,job,sizeof(retained)));continue;
        }
        assert(resolved==GC_RAC_READY);
        if(guard.request.reference==job->reference&&guard.page->incarnation==job->incarnation){
            *access=cursor.lease;cursor=(ReferenceLease){0};return guard;
        }
        GuardedMarkPrepare prepared={.scope=(uintptr_t)&guard.request.scope,.page=(uintptr_t)guard.page,
            .reference=guard.request.reference,.phase=atomic_load(&ctx->adopted_phase),.lease=cursor.lease};
        cursor.lease=(MetadataLeaseRequest){0};
        assert(g->rem_ops.move->prepare(&prepared)==GC_MARK_OK);graph_exit(g,&guard);
        MarkRequest stale=*job;MarkRetarget handoff={.job=(uintptr_t)job,.destination=prepared.output};
        metadata_lock(&g->m);
        assert(g->rem_ops.move->retarget(&g->queue,&handoff)==GC_MARK_OK);
        assert(job->cursor==stale.cursor&&job->record==stale.record&&job->serial!=stale.serial);
        assert(g->mark(&g->queue,&stale,GC_MARK_YIELD)==GC_MARK_INVALID);
        assert(g->mark(&g->queue,&stale,GC_MARK_FINISH)==GC_MARK_INVALID);
        MetadataLeaseRequest retired={.token=handoff.retired.metadata_lease};
        assert(g->m.metadata(&g->m.leases,&retired,GC_ML_RESOLVE)==GC_ML_OK);
        assert(g->m.metadata(&g->m.leases,&retired,GC_ML_RELEASE)==GC_ML_OK);
        metadata_unlock(&g->m);atomic_fetch_add_explicit(&g->mark_retargets,1,memory_order_relaxed);
        cursor.reference=job->reference;
    }
}
static uint64_t graph_offer_guarded(MarkGraph *g,MutatorContext *ctx,GraphAccess *guard,MetadataLeaseRequest lease){
    GuardedMarkRoute routed={.generation=g->queue.generation,
        .prepare={.scope=(uintptr_t)&guard->request.scope,.page=(uintptr_t)guard->page,
            .reference=guard->request.reference,.phase=atomic_load(&ctx->adopted_phase),.lease=lease}};
    lease=(MetadataLeaseRequest){0};
    uint64_t routed_status=g->route(&routed);
    assert(routed_status==GC_ROUTE_READY||routed_status==GC_ROUTE_SKIP||routed_status==GC_ROUTE_WAIT);
    graph_exit(g,guard);
    metadata_lock(&g->m);
    if(routed_status==GC_ROUTE_WAIT){
        assert(routed.prepare.lease.token&&!routed.prepare.output.metadata_lease);
        assert(g->m.metadata(&g->m.leases,&routed.prepare.lease,GC_ML_RELEASE)==GC_ML_OK);
        metadata_unlock(&g->m);return GRAPH_ACCESS_BUSY;
    }
    if(routed_status==GC_ROUTE_SKIP){
        assert(g->queue.generation==GC_YOUNG&&guard->page->generation==GC_OLD);
        assert(routed.prepare.lease.token&&!routed.prepare.output.metadata_lease);
        assert(g->m.metadata(&g->m.leases,&routed.prepare.lease,GC_ML_RELEASE)==GC_ML_OK);
        atomic_fetch_add_explicit(&g->old_skips,1,memory_order_relaxed);
        metadata_unlock(&g->m);return GRAPH_ACCESS_READY;
    }
    assert(!routed.prepare.lease.token&&routed.prepare.output.metadata_lease);
    MarkRequest *q=&routed.prepare.output;
    uint64_t before=g->queue.overflow_count;
    uint64_t status=g->spill(&g->queue,q,GC_MARK_OFFER_OR_SPILL);
    if(g->queue.overflow_count>before){g->spill_offers++;if(g->queue.in_flight==g->queue.capacity)g->full_claims++;}
    assert(status==GC_MARK_OK||status==GC_MARK_DUPLICATE);
    if(status==GC_MARK_DUPLICATE){
        lease=(MetadataLeaseRequest){.token=q->metadata_lease};
        assert(g->m.metadata(&g->m.leases,&lease,GC_ML_RESOLVE)==GC_ML_OK);
        assert(g->m.metadata(&g->m.leases,&lease,GC_ML_RELEASE)==GC_ML_OK);*q=(MarkRequest){0};
    }
    metadata_unlock(&g->m);return GRAPH_ACCESS_READY;
}
static uint64_t graph_offer_try(MarkGraph *g,MutatorContext *ctx,uint64_t reference,bool cooperative,uint64_t *canonical) {
    *canonical=0;if(!reference)return GRAPH_ACCESS_READY;
    metadata_lock(&g->m);
    ReferenceLease acquired={.reference=reference};
    uint64_t acquired_status=g->reference_lease(&g->m.leases,&acquired);
    if(acquired_status==GC_ML_FULL){metadata_unlock(&g->m);return GRAPH_ACCESS_FULL;}
    assert(acquired_status==GC_ML_OK);
    MetadataLeaseRequest lease=acquired.lease;acquired.lease=(MetadataLeaseRequest){0};
    metadata_unlock(&g->m); // WAIT must not retain the heap/queue lock.
    GraphAccess guard={0};uint64_t accessed=graph_try_enter(g,ctx,&lease,reference,cooperative,&guard);
    if(accessed!=GRAPH_ACCESS_READY){assert(!lease.token&&!guard.page);return accessed;}
    *canonical=guard.request.reference;
    uint64_t offered=graph_offer_guarded(g,ctx,&guard,lease);
    if(offered!=GRAPH_ACCESS_READY)*canonical=0;return offered;
}
// Coordinator/control callers currently require immediately available capacity.
static uint64_t graph_offer(MarkGraph *g,MutatorContext *ctx,uint64_t reference){
    uint64_t canonical=0;assert(graph_offer_try(g,ctx,reference,false,&canonical)==GRAPH_ACCESS_READY);return canonical;
}
static void graph_offer_retry(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,uint64_t status){
    assert(!atomic_load(&ctx->access_epoch));
    if(status==GRAPH_ACCESS_BUSY){
        atomic_fetch_add_explicit(&g->offer_busy_wait,1,memory_order_release);
        graph_poll(g,participant);metadata_yield();
    }else{
        assert(status==GRAPH_ACCESS_FULL);
        graph_metadata_wait(g,ctx,participant,&g->offer_metadata_wait);
    }
}
// The copied job remains a counted kind-4 producer until both references
// have reached the queue. Its owner may advance its private COPY (never the
// immutable ring entry); an owned metadata token protects each durable cursor.
// Every forwarding edge transfers the token and updates COPY before polling.
// The source ring retains the original strong reference throughout. This
// fixture retains all forwarding controls until cleanup; it does not establish
// general source retirement or independent Young/Old cycle coordination.
static void graph_offer_cursor(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,RootJournalRequest *job,RootWalk *walk,uint64_t *reference,bool mark){
    assert((job!=NULL)!=(walk!=NULL));
    if(walk)assert(walk->active&&walk->lease.kind==1&&walk->pending==1&&reference==&walk->pending_reference);
    if(!*reference)return;
    MetadataLeaseRequest lease={0};
    for(;;){
        if(!lease.token){
            ReferenceLease fresh={.reference=*reference};metadata_lock(&g->m);
            uint64_t acquired=g->reference_lease(&g->m.leases,&fresh);metadata_unlock(&g->m);
            if(acquired==GC_ML_FULL){
                if(walk)graph_metadata_wait(g,ctx,participant,&g->audit_source_wait);
                else graph_offer_retry(g,ctx,participant,GRAPH_ACCESS_FULL);
                continue;
            }
            assert(acquired==GC_ML_OK);lease=fresh.lease;
        }
        ReferenceLease cursor={.reference=*reference,.lease=lease};
        GraphAccess guard={.page=(AllocationPage *)(uintptr_t)lease.page,
            .request={.scope={.context=(uintptr_t)ctx}}};
        metadata_lock(&g->m);
        uint64_t status=g->rem_ops.pending_access(&g->m.leases,&cursor,&guard.request);
        metadata_unlock(&g->m);
        if(status==GC_RAC_READY){
            if(!mark){
                assert(walk&&((g->root_registry.phase>>(4*g->queue.generation))&15)==GC_RETIRE);
                graph_exit(g,&guard);metadata_lock(&g->m);
                assert(g->m.metadata(&g->m.leases,&lease,GC_ML_RELEASE)==GC_ML_OK);
                metadata_unlock(&g->m);return;
            }
            MetadataLeaseRequest transferred=lease;lease=(MetadataLeaseRequest){0};
            uint64_t offered=graph_offer_guarded(g,ctx,&guard,transferred);
            if(offered==GRAPH_ACCESS_READY)return;
            graph_offer_retry(g,ctx,participant,offered);continue;
        }
        assert(!atomic_load(&ctx->access_epoch));
        if(status==GC_RF_WAIT){graph_offer_retry(g,ctx,participant,GRAPH_ACCESS_BUSY);continue;}
        assert(status==GC_RF_ADVANCED&&!guard.request.reference&&!guard.request.destination&&!guard.request.scope.held);
        MetadataLeaseRequest retired={0};metadata_lock(&g->m);
        RootSegment *pending=(RootSegment *)(uintptr_t)atomic_load_explicit(&g->pending_segment,memory_order_acquire);
        bool controlled=job&&pending&&job->registration==pending->registration&&!job->begin;
        // Inject at the exact second-hop acquisition: another worker may
        // return tokens after the preceding phase rendezvous.
        if(controlled&&atomic_load(&g->journal_steps)==1&&!g->journal_pressure_injected){
            while(g->m.leases.free_head){
                assert(g->journal_held_count<32);
                MetadataLeaseRequest *held=&g->journal_held[g->journal_held_count++];
                *held=(MetadataLeaseRequest){lease.page,lease.incarnation,0};
                assert(g->m.metadata(&g->m.leases,held,GC_ML_ACQUIRE)==GC_ML_OK);
            }
            assert(g->journal_held_count&&g->m.leases.active==32);
            g->metadata_resume=0;g->journal_pressure_injected=true;
        }
        // The audit worker injects destination pressure at acquisition while
        // holding its source token. Coordinator-side prefill alone would race
        // with other workers returning tokens between rendezvous and retry.
        if(walk&&g->audit_pressure&&!g->audit_destination_injected){
            while(g->m.leases.free_head){
                assert(g->audit_held_count<32);
                MetadataLeaseRequest *held=&g->audit_held[g->audit_held_count++];
                *held=(MetadataLeaseRequest){lease.page,lease.incarnation,0};
                assert(g->m.metadata(&g->m.leases,held,GC_ML_ACQUIRE)==GC_ML_OK);
            }
            assert(g->audit_held_count&&g->m.leases.active==32);
            g->metadata_resume=0;g->audit_destination_injected=true;
        }
        uint64_t acquired=g->rem_ops.pending_follow(&g->m.leases,&cursor,&retired);
        if(acquired==GC_RF_METADATA_FULL){
            assert(cursor.reference==*reference&&!memcmp(&cursor.lease,&lease,sizeof(lease))&&!retired.token);
            metadata_unlock(&g->m);
            // Keep the COPY's current token/reference. A private forwarding
            // result never survives a native wait; retry the current step.
            cursor=(ReferenceLease){0};guard=(GraphAccess){0};
            if(walk)atomic_store_explicit(&g->audit_token,lease.token,memory_order_release);
            graph_metadata_wait(g,ctx,participant,walk?&g->audit_destination_wait:controlled?&g->journal_destination_wait:&g->offer_metadata_wait);continue;
        }
        assert(acquired==GC_RF_ADVANCED&&!retired.token);
        lease=cursor.lease;*reference=cursor.reference;
        metadata_unlock(&g->m);
        if(guard.page->generation==GC_OLD)atomic_fetch_add_explicit(&g->owner_remaps,1,memory_order_relaxed);
        guard=(GraphAccess){0};cursor=(ReferenceLease){0};
        if(controlled){
            atomic_store(&g->journal_job,(uintptr_t)job);
            atomic_store(&g->journal_reference,(uintptr_t)reference);
            atomic_store(&g->journal_token,lease.token);
            uint64_t step=atomic_fetch_add_explicit(&g->journal_steps,1,memory_order_release)+1;
            while(step<=2&&atomic_load_explicit(&g->journal_resume,memory_order_acquire)<step){graph_poll(g,participant);metadata_yield();}
        }
        if(walk){assert(walk->pending==1&&walk->pending_reference==*reference);g->audit_hops++;}
        graph_poll(g,participant);
    }
}
static void graph_offer_leased(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,RootJournalRequest *job,uint64_t *reference){
    graph_offer_cursor(g,ctx,participant,job,NULL,reference,true);
}
static bool graph_worker_journal_one(MarkGraph *,MutatorContext *,ThreadRegistration *,RootRegistration *);
// Each root-access call follows at most one edge. ADVANCED stores that edge
// in the registered root before any poll; READY transfers guard and lease
// directly into queue routing without resolving the chain again.
static void graph_offer_root(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,RootRegistration *registration,RootRequest *root){
    assert(!root->value);
    RootAccess access={.follow={.context=(uintptr_t)ctx,.root=*root}};
    for(;;){
        metadata_lock(&g->m);uint64_t status=g->rem_ops.root_access(&g->m.leases,&access);metadata_unlock(&g->m);
        if(status==GC_RAC_READY){
            GraphAccess guard={.page=(AllocationPage *)(uintptr_t)access.follow.cleanup.page,.request=access.access};
            MetadataLeaseRequest lease=access.follow.cleanup;
            access=(RootAccess){0};uint64_t offered=graph_offer_guarded(g,ctx,&guard,lease);
            if(offered==GRAPH_ACCESS_READY)return;
            access=(RootAccess){.follow={.context=(uintptr_t)ctx,.root=*root}};
            graph_offer_retry(g,ctx,participant,offered);continue;
        }
        assert(!access.follow.cleanup.token&&!atomic_load(&ctx->access_epoch));
        if(status==GC_RF_DONE)return;
        if(status==GC_RF_ADVANCED){
            uint64_t step=atomic_fetch_add_explicit(&g->offer_advanced,1,memory_order_acq_rel)+1;
            while(step<=2&&atomic_load_explicit(&g->offer_advance_resume,memory_order_acquire)<step){graph_poll(g,participant);metadata_yield();}
            graph_poll(g,participant);
        }else if(status==GC_RF_FLUSH){
            assert(graph_worker_journal_one(g,ctx,participant,registration));graph_poll(g,participant);
        }else{
            assert(status==GC_RF_WAIT||status==GC_RF_METADATA_FULL);
            graph_offer_retry(g,ctx,participant,status==GC_RF_WAIT?GRAPH_ACCESS_BUSY:GRAPH_ACCESS_FULL);
        }
    }
}
static uint64_t graph_queue_status(MarkGraph *g) {
    metadata_lock(&g->m);uint64_t status=g->mark(&g->queue,NULL,GC_MARK_CHECK);metadata_unlock(&g->m);return status;
}
static void graph_journal_claim(MarkGraph *g,RootRegistration *r,RootLease *lease,RootJournalRequest *q,RootChange *out){
    *q=(RootJournalRequest){.registration=(uintptr_t)r,.lease=(uintptr_t)lease,.scope=r->scope,.output=(uintptr_t)out,.capacity=1};
    metadata_lock(&g->m);assert(g->lifecycle.journal(&g->root_registry,q,GC_RJ_CLAIM)==GC_RJ_OK);metadata_unlock(&g->m);
    assert(g->lifecycle.journal(&g->root_registry,q,GC_RJ_COPY)==GC_RJ_OK&&q->written==1);
}
static void graph_journal_handoff(MarkGraph *g,RootJournalRequest *q,RootChange *out){
    uint64_t phase=(out->phase>>(g->queue.generation*4))&15;
    if(phase>=1&&phase<=3){graph_offer(g,&g->m.ctx,out->previous);graph_offer(g,&g->m.ctx,out->next);}
    metadata_lock(&g->m);assert(g->lifecycle.journal(&g->root_registry,q,GC_RJ_ACK)==GC_RJ_OK);metadata_unlock(&g->m);
}
// Entire inventory traversal and journal handoff happen with mutators running.
// Registry operations are finite; scans/offers/copies never hold its lock.
typedef struct {uint64_t start,limit;} GraphAuditRange;
static uint64_t graph_audit_roots_mode(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,RootAudit *audit,bool retire,const GraphAuditRange *ranges,unsigned range_count){
    assert(range_count<=2&&(!range_count||(retire&&ranges)));
    for(unsigned i=0;i<range_count;i++)assert(ranges[i].start&&ranges[i].start<ranges[i].limit&&!(ranges[i].start&15)&&!(ranges[i].limit&15));
    RootAuditor step=retire?g->lifecycle.retire_audit:g->lifecycle.audit;
    uint64_t findings=0;
    metadata_lock(&g->m);uint64_t status=step(&g->root_registry,audit,GC_RA_BEGIN);metadata_unlock(&g->m);
    while(status==GC_RA_ITEM){
        RootWalk *walk=&audit->walk;
        RootRegistration *registration=(RootRegistration *)(uintptr_t)walk->lease.registration;
        RootSegment *segment=(RootSegment *)(uintptr_t)registration->segment;
        RootWalkItem item={(uintptr_t)walk,walk->lease.serial,0};
        for(;;){
            uint64_t result=g->lifecycle.walk_item(&item,GC_RWI_CAPTURE);
            if(result==GC_RWI_DONE)break;assert(result==GC_RWI_ITEM&&walk->pending==1);
            for(unsigned i=0;i<range_count;i++){
                uint64_t offset=walk->pending_observed>>16;
                if(offset>=ranges[i].start&&offset<ranges[i].limit){findings++;break;}
            }
            graph_offer_cursor(g,ctx,participant,NULL,walk,&walk->pending_reference,!retire);
            if(walk->pending_observed){
                RootWalkHeal heal={(uintptr_t)walk,item.serial,walk->pending_index,walk->pending_observed,
                    (walk->pending_reference&~UINT64_C(0xffff))|(walk->pending_observed&UINT64_C(0xffff))};
                uint64_t repaired=g->lifecycle.walk_heal(&heal);
                assert(repaired==GC_RWH_OK||repaired==GC_RWH_CHANGED);
                if(repaired==GC_RWH_OK&&heal.expected!=heal.desired)g->root_repairs++;
                if(repaired==GC_RWH_CHANGED)g->root_conflicts++;
            }
            assert(g->lifecycle.walk_item(&item,GC_RWI_ACK)==GC_RWI_ITEM&&!walk->pending);
            graph_poll(g,participant);
        }
        if(segment->journal){RootLease lease={0};RootChange entry;
            RootJournalRequest q={.registration=(uintptr_t)registration,.lease=(uintptr_t)&lease,.scope=registration->scope,.output=(uintptr_t)&entry,.capacity=1};
            for(;;){metadata_lock(&g->m);uint64_t result=g->lifecycle.journal(&g->root_registry,&q,GC_RJ_CLAIM);metadata_unlock(&g->m);
                if(result==GC_RJ_EMPTY)break;assert(result==GC_RJ_OK);
                assert(g->lifecycle.journal(&g->root_registry,&q,GC_RJ_COPY)==GC_RJ_OK&&q.written==1);
                uint64_t phase=(entry.phase>>(g->queue.generation*4))&15;
                if(phase>=1&&phase<=3){graph_offer_leased(g,ctx,participant,&q,&entry.previous);graph_offer_leased(g,ctx,participant,&q,&entry.next);}
                metadata_lock(&g->m);assert(g->lifecycle.journal(&g->root_registry,&q,GC_RJ_ACK)==GC_RJ_OK);metadata_unlock(&g->m);}}
        walk->completed_mask=walk->mask;
        metadata_lock(&g->m);status=step(&g->root_registry,audit,GC_RA_NEXT);metadata_unlock(&g->m);
    }
    assert(status==GC_RA_DONE);return findings;
}
static void graph_audit_roots(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,RootAudit *audit){
    (void)graph_audit_roots_mode(g,ctx,participant,audit,false,NULL,0);
}
// A separate OS owner performs the root inventory audit. It has no logical
// roots of its own: the walk's counted pending snapshot is its durable work.
// Native controls belong to the joining coordinator until this thread exits.
typedef struct {MarkGraph *graph;RootAudit *audit;MutatorContext context;ThreadRegistration participant;} GraphAuditWorker;
#ifdef _WIN32
static DWORD WINAPI graph_audit_worker(void *argument)
#else
static void *graph_audit_worker(void *argument)
#endif
{
    GraphAuditWorker *w=argument;MarkGraph *g=w->graph;
    w->context.abi_version=GC_ABI_VERSION;w->participant.context=(uintptr_t)&w->context;
    uint64_t attached;while((attached=graph_worker_call(g,&w->participant,GC_WK_ATTACH))!=GC_WK_OK){assert(attached==GC_WK_RETRY);metadata_yield();}
    graph_audit_roots(g,&w->context,&w->participant,w->audit);
    graph_poll(g,&w->participant);
    metadata_lock(&g->m);
    assert(!g->handshake.active&&!w->context.root_segments&&!w->context.retiring_roots&&!atomic_load(&w->context.access_epoch));
    assert(gc_context_transition_locked(&w->context,GC_RUNNING,GC_DETACHING)==GC_CONTEXT_OK);
    assert(g->lifecycle.handshake(&g->handshake,&w->participant,GC_HS_DETACH)==GC_HS_OK);
    metadata_unlock(&g->m);return 0;
}
// One remembered claim is a producer even while every mark queue is empty.
// The fixture retains this Old owner and all native metadata until teardown;
// the predecessor lookup does not turn a metadata lease into strong liveness.
// Drain at most one journal entry. The ring remains immutable and retained by
// its claim through both offers and ACK. Private COPY cursors can advance with
// metadata ownership and poll while this same producer remains in flight.
static bool graph_worker_journal_one(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant,RootRegistration *registration){
    RootLease lease={0};RootChange change;
    RootJournalRequest q={.registration=(uintptr_t)registration,.lease=(uintptr_t)&lease,
        .scope=registration->scope,.output=(uintptr_t)&change,.capacity=1};
    metadata_lock(&g->m);uint64_t status=g->lifecycle.journal(&g->root_registry,&q,GC_RJ_CLAIM);metadata_unlock(&g->m);
    if(status==GC_RJ_EMPTY)return false;
    assert(status==GC_RJ_OK);
    assert(g->lifecycle.journal(&g->root_registry,&q,GC_RJ_COPY)==GC_RJ_OK&&q.written==1);
    uint64_t phase=(change.phase>>(g->queue.generation*4))&15;
    if(phase>=GC_MARK_START&&phase<=GC_MARK_END_CHECK){graph_offer_leased(g,ctx,participant,&q,&change.previous);graph_offer_leased(g,ctx,participant,&q,&change.next);}
    metadata_lock(&g->m);assert(g->lifecycle.journal(&g->root_registry,&q,GC_RJ_ACK)==GC_RJ_OK);metadata_unlock(&g->m);
    return true;
}
// Retire repair consumes an audited location, never its diagnostic GcRef.
// The cycle keeps the audited receiver live and its page lease stays held;
// publish that receiver into an owned root before any resolving/repair step.
static unsigned graph_repair_finding(MarkGraph *g,ThreadRegistration *participant,HeapAudit *audit,
        RootRegistration *registration,RootRequest *receiver,RootRequest *output,unsigned logical){
    assert(audit->stage==3&&audit->slice.observed&&audit->walk.lease.token);
    assert(!atomic_load(&g->m.ctx.access_epoch)&&g->relocation.closed);
    receiver->value=audit->slice.reference;
    uint64_t stored;
    while((stored=g->rem_ops.move->root(&g->m.ctx,receiver,GC_ROOT_STORE))==GC_ROOT_FLUSH)
        assert(graph_worker_journal_one(g,&g->m.ctx,participant,registration));
    assert(stored==GC_ROOT_OK);receiver->value=0;
    RootedLoad load={.observation={.receiver.follow={.context=(uintptr_t)&g->m.ctx,.root=*receiver},
        .registry=(uintptr_t)&g->registry,.byte_offset=audit->slice.cursor,.output=*output}};
    RootedLoadAdmission admission={(uintptr_t)&g->root_registry,(uintptr_t)&g->handshake,(uintptr_t)participant,{0,0}};
    metadata_lock(&g->m);assert(g->rem_ops.admit_load(&admission,&load)==GC_LA_OK);metadata_unlock(&g->m);
    assert(!load.marks.queues[0]&&!load.marks.queues[1]); // both marks inactive here
    unsigned steps=0,flushes=0;
    for(;;){
        assert(++steps<128);uint64_t action=gc_rooted_load_next(&load);
        if(action==GC_RLOAD_DONE)break;
        assert(action==GC_RLOAD_HEAP||action==GC_RLOAD_ACCESS);
        if(action==GC_RLOAD_HEAP)metadata_lock(&g->m);
        uint64_t status=g->rem_ops.rooted_load(action==GC_RLOAD_HEAP?&g->m.leases:NULL,&load,action);
        if(action==GC_RLOAD_HEAP)metadata_unlock(&g->m);
        if(status==GC_RLOAD_FLUSH){
            assert(!atomic_load(&g->m.ctx.access_epoch));flushes++;
            assert(graph_worker_journal_one(g,&g->m.ctx,participant,registration));
        }else assert(status==GC_RLOAD_HEAP||status==GC_RLOAD_ACCESS||status==GC_RLOAD_DONE);
    }
    assert(!load.admitted&&!atomic_load(&g->m.ctx.private_producers)&&!atomic_load(&g->m.ctx.access_epoch));
    assert(load.last==GC_HEAL_OK); // no logical writer in this frozen repair pass
    RootRequest result=*output;assert(g->rem_ops.move->root(&g->m.ctx,&result,GC_ROOT_READ_RAW)==GC_ROOT_OK);
    assert(result.value&&(result.value>>16)!=(g->objects[logical].reference>>16));
    ReferenceLease lease={.reference=result.value};
    metadata_lock(&g->m);assert(g->reference_lease(&g->m.leases,&lease)==GC_ML_OK);metadata_unlock(&g->m);
    GraphAccess guard=graph_enter(g,&g->m.ctx,&lease.lease,result.value);
    assert(g->lifecycle.identity((uintptr_t)&guard.request.scope,(uintptr_t)guard.page,audit->phase)==&g->records[logical]);
    graph_exit(g,&guard);assert(metadata_call(&g->m,&lease.lease,GC_ML_RELEASE)==GC_ML_OK);
    RootRequest *clear[]={receiver,output};
    for(unsigned i=0;i<2;i++){
        clear[i]->value=0;
        while((stored=g->rem_ops.move->root(&g->m.ctx,clear[i],GC_ROOT_STORE))==GC_ROOT_FLUSH)
            assert(graph_worker_journal_one(g,&g->m.ctx,participant,registration));
        assert(stored==GC_ROOT_OK);
    }
    while(graph_worker_journal_one(g,&g->m.ctx,participant,registration)){}
    assert(g->m.leases.active==1&&g->relocation.closed); // only the audit page lease
    return flushes;
}
static bool graph_remembered_work(MarkGraph *g,MutatorContext *ctx,ThreadRegistration *participant){
    if(!atomic_load_explicit(&g->rem_ready,memory_order_acquire))return false;
    RememberedJob claim={.budget=256};
    RememberedInventoryRequest rq={.job=(uintptr_t)&claim};RememberedWalk walk={0};
    metadata_lock(&g->m);
    if(!g->remembered.pending&&!g->remembered.unflipped){
        metadata_unlock(&g->m);return false;
    }
    uint64_t walked=g->rem_ops.walk(&g->remembered,&walk,GC_RMW_BEGIN);metadata_unlock(&g->m);
    while(walked==GC_RMW_ITEM){
        rq.registration=walk.registration;metadata_lock(&g->m);
        RememberedRegistration *node=(RememberedRegistration *)(uintptr_t)walk.registration;
        RememberedSet *set=(RememberedSet *)(uintptr_t)node->set;
        if(node->epoch!=g->remembered.epoch){
            claim=(RememberedJob){.incarnation=set->incarnation,.phase=g->remembered.epoch,.budget=256};
            assert(g->rem_ops.inventory(&g->remembered,&rq,GC_RI_FLIP)==GC_RS_OK);
        }
        if(node==&g->empty_registration){
            metadata_unlock(&g->m);atomic_store_explicit(&g->rem_walk_parked,1,memory_order_release);
            while(!atomic_load_explicit(&g->rem_walk_resume,memory_order_acquire)){graph_poll(g,participant);metadata_yield();}
            graph_poll(g,participant);metadata_lock(&g->m);
        }
        claim=(RememberedJob){.budget=256};
        uint64_t status=g->rem_ops.inventory(&g->remembered,&rq,GC_RI_TAKE);
        if(status==GC_RS_OK){
            // The slot claim now retains this registration independently.
            assert(g->rem_ops.walk(&g->remembered,&walk,GC_RMW_CANCEL)==GC_RMW_DONE);
            metadata_unlock(&g->m);break;
        }
        assert(status==GC_RS_WAIT||status==GC_RS_EMPTY||status==GC_RS_MORE);
        if(status!=GC_RS_MORE)walked=g->rem_ops.walk(&g->remembered,&walk,GC_RMW_NEXT);
        metadata_unlock(&g->m);graph_poll(g,participant);
    }
    if(walked!=GC_RMW_ITEM){assert(walked==GC_RMW_DONE);return false;}
    bool first=atomic_fetch_add_explicit(&g->rem_claimed,1,memory_order_acq_rel)==0;
    while(first&&!atomic_load_explicit(&g->rem_resume,memory_order_acquire)){graph_poll(g,participant);metadata_yield();}
    graph_poll(g,participant);
    const MarkMoveOps *o=g->rem_ops.move;
    RootCell cells[2]={0};RootCell *cell=&cells[0];RootChange changes[2];BufferCursor journal={.entries=(uintptr_t)changes,.capacity=g->queue.epoch%4==1?1:2};
    RootSegment segment={.owner=ctx->thread_id,.cells=(uintptr_t)cells,.capacity=2,.journal=(uintptr_t)&journal};
    RootRegistration registration={.segment=(uintptr_t)&segment};RootLease root_lease={0};
    RootLifeRequest life={.registration=(uintptr_t)&registration,.lease=(uintptr_t)&root_lease,.context=(uintptr_t)ctx};
    metadata_lock(&g->m);assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_ATTACH)==GC_RL_OK);metadata_unlock(&g->m);
    RootRequest output={.segment=(uintptr_t)&segment};assert(o->root(ctx,&output,GC_ROOT_RESERVE)==GC_ROOT_OK);
    // Fill the one-entry journal before the guarded load. A rejected load
    // must leave its output root null and retain the remembered claim.
    if(journal.capacity==1)assert(o->root(ctx,&output,GC_ROOT_STORE)==GC_ROOT_OK);
    RootRequest owner_root={.segment=(uintptr_t)&segment};assert(o->root(ctx,&owner_root,GC_ROOT_RESERVE)==GC_ROOT_OK);
    uint64_t source_owner=rq.registration==(uintptr_t)&g->rem_registration?g->old_owner:g->moved_owner;
    RememberedOwner located={0};bool owner_published=false;
    uint64_t loaded_phase;
    for(;;){
        if(!owner_published){
            RememberedLease owner={.registration=rq.registration,.claim=rq.job};metadata_lock(&g->m);
            uint64_t acquired=g->rem_ops.acquire(&g->m.leases,&owner);metadata_unlock(&g->m);
            if(acquired==GC_ML_FULL){
                assert(!owner.lease.page&&!owner.lease.incarnation&&!owner.lease.token);
                assert(!atomic_load(&ctx->access_epoch)&&!atomic_load(&cell->value));
                graph_metadata_wait(g,ctx,participant,&g->rem_metadata_wait);continue;
            }
            assert(acquired==GC_ML_OK);
            assert(source_owner);
            located=(RememberedOwner){.registration=rq.registration,.claim=rq.job,.page=owner.lease.page};
            assert(g->rem_ops.find(&located)==GC_RO_OK&&located.owner==(source_owner>>16)&&!located.byte_offset);
            // Publish the strong owner before releasing the source metadata token.
            // Both roots share a registered segment; journal pressure is serviced
            // without a guard, while the remembered claim remains in flight.
            owner_root.value=source_owner;
            for(;;){
                uint64_t stored=o->root(ctx,&owner_root,GC_ROOT_STORE);if(stored==GC_ROOT_OK)break;
                assert(stored==GC_ROOT_FLUSH);
                atomic_fetch_add_explicit(&g->rem_access_flushes,1,memory_order_relaxed);
                assert(graph_worker_journal_one(g,ctx,participant,&registration));graph_poll(g,participant);
            }
            owner_root.value=0;owner_published=true;
            assert(metadata_call(&g->m,&owner.lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        RootAccess rooted={.follow={.context=(uintptr_t)ctx,.root=owner_root}};
        for(;;){
            metadata_lock(&g->m);uint64_t accessed=g->rem_ops.root_access(&g->m.leases,&rooted);metadata_unlock(&g->m);
            if(accessed==GC_RAC_READY)break;
            assert(!rooted.follow.cleanup.token&&!atomic_load(&ctx->access_epoch));
            if(accessed==GC_RF_ADVANCED){
                bool first_hop=atomic_fetch_add_explicit(&g->rem_access_advanced,1,memory_order_acq_rel)==0;
                while(first_hop&&!atomic_load_explicit(&g->rem_access_resume,memory_order_acquire)){graph_poll(g,participant);metadata_yield();}
            }else if(accessed==GC_RF_FLUSH){
                atomic_fetch_add_explicit(&g->rem_access_flushes,1,memory_order_relaxed);
                assert(graph_worker_journal_one(g,ctx,participant,&registration));
            }else if(accessed==GC_RF_METADATA_FULL){
                RootRequest retained=owner_root;
                assert(o->root(ctx,&retained,GC_ROOT_READ_RAW)==GC_ROOT_OK&&retained.value);
                graph_metadata_wait(g,ctx,participant,&g->rem_access_metadata_wait);
                RootRequest resumed=owner_root;
                assert(o->root(ctx,&resumed,GC_ROOT_READ_RAW)==GC_ROOT_OK&&resumed.value==retained.value);
            }else assert(accessed==GC_RF_WAIT);
            graph_poll(g,participant);metadata_yield();
        }
        GraphAccess guard={.page=(AllocationPage *)(uintptr_t)rooted.follow.cleanup.page,.request=rooted.access};
        assert(guard.request.reference==g->moved_owner);
        RememberedLoad load={.registration=rq.registration,.claim=rq.job,.source_owner=source_owner,
            .load={.scope=(uintptr_t)&guard.request.scope,.page=(uintptr_t)guard.page,.registry=(uintptr_t)&g->registry,
                .byte_offset=located.byte_offset,.phase=atomic_load(&ctx->adopted_phase),.output=output}};
        uint64_t loaded=g->rem_ops.load(&load);graph_exit(g,&guard);
        assert(metadata_call(&g->m,&rooted.follow.cleanup,GC_ML_RELEASE)==GC_ML_OK);
        if(loaded==GC_ROOT_OK){loaded_phase=load.load.phase;break;}
        assert(loaded==GC_ROOT_FLUSH&&!atomic_load(&ctx->access_epoch)&&!atomic_load(&cell->value));
        atomic_fetch_add_explicit(&g->rem_flushes,1,memory_order_relaxed);
        assert(graph_worker_journal_one(g,ctx,participant,&registration));graph_poll(g,participant);
    }
    // The root, its published journal and the remembered claim survive this
    // real phase rendezvous. No resolved address or metadata guard crosses it.
    assert(!atomic_load(&ctx->access_epoch));
    if(first)atomic_store_explicit(&g->rem_output_segment,(uintptr_t)&segment,memory_order_release);
    atomic_fetch_add_explicit(&g->rem_loaded,1,memory_order_release);
    while(first&&!atomic_load_explicit(&g->rem_continue,memory_order_acquire)){graph_poll(g,participant);metadata_yield();}
    graph_poll(g,participant);if(first)assert(atomic_load(&ctx->adopted_phase)>loaded_phase);
    assert(o->root(ctx,&output,GC_ROOT_READ_RAW)==GC_ROOT_OK&&output.value==g->objects[7].reference);
    output.value=0; // discard the private read before a safepoint-capable offer
    graph_offer_root(g,ctx,participant,&registration,&output); // rooted handoff before claim ACK
    for(;;){
        uint64_t released=o->root(ctx,&output,GC_ROOT_RELEASE);if(released==GC_ROOT_OK)break;
        assert(released==GC_ROOT_FLUSH);
        atomic_fetch_add_explicit(&g->rem_flushes,1,memory_order_relaxed);
        assert(graph_worker_journal_one(g,ctx,participant,&registration));graph_poll(g,participant);
    }
    for(;;){
        uint64_t released=o->root(ctx,&owner_root,GC_ROOT_RELEASE);if(released==GC_ROOT_OK)break;
        assert(released==GC_ROOT_FLUSH);
        atomic_fetch_add_explicit(&g->rem_access_flushes,1,memory_order_relaxed);
        assert(graph_worker_journal_one(g,ctx,participant,&registration));graph_poll(g,participant);
    }
    while(graph_worker_journal_one(g,ctx,participant,&registration))graph_poll(g,participant);
    metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_RETIRE)==GC_RL_OK);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_CLAIM)==GC_RL_OK);metadata_unlock(&g->m);
    uint64_t values[2]={UINT64_MAX,UINT64_MAX};RootScanJob scan={.scan={(uintptr_t)&segment,life.begin,(uintptr_t)values,2,0},.lease=(uintptr_t)&root_lease,.serial=life.serial};
    assert(g->lifecycle.leased(&scan)==GC_ROOT_OK&&scan.scan.written==2&&!values[0]&&!values[1]);
    metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_ACK)==GC_RL_OK);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_CLEAN)==GC_RL_OK);metadata_unlock(&g->m);
    assert(g->lifecycle.clear(&life)==GC_RL_OK);metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_FINISH)==GC_RL_OK);
    if(rq.registration==(uintptr_t)&g->rem_registration){
        // The guarded load validated the same canonical descriptor field and
        // its child is handed off. Move pending/current bits before consuming
        // this source claim; the late source record must force a destination
        // claim, even if the Young mark queue has already drained.
        RememberedTransfer transfer={.source=rq.registration,.destination=(uintptr_t)&g->empty_registration,
            .claim=rq.job,.offset=g->moved_owner>>16,.incarnation=g->empty_set.incarnation};
        assert(g->rem_ops.transfer(&g->remembered,&transfer)==GC_RS_OK);
        assert(!g->rem_set.pending&&!g->rem_set.current_count&&!g->rem_set.active_serial);
        assert(g->empty_set.pending==1&&g->empty_set.current_count==1);
        assert(g->remembered.pending==1&&!g->remembered.claims);
        assert(g->rem_ops.inventory(&g->remembered,&rq,GC_RI_CHECK)==GC_RS_WAIT);
        assert(g->rem_ops.inventory(&g->remembered,&rq,GC_RI_ACK)==GC_RS_INVALID);
        atomic_fetch_add_explicit(&g->rem_transferred,1,memory_order_release);
    }else{
        assert(rq.registration==(uintptr_t)&g->empty_registration);
        claim.keep=1;assert(g->rem_ops.inventory(&g->remembered,&rq,GC_RI_ACK)==GC_RS_OK);
    }
    metadata_unlock(&g->m);atomic_fetch_add_explicit(&g->rem_done,1,memory_order_release);return true;
}
#ifdef _WIN32
static DWORD WINAPI graph_marker(void *argument)
#else
static void *graph_marker(void *argument)
#endif
{
    MarkGraph *g=argument;
    MutatorContext ctx={.abi_version=GC_ABI_VERSION};ThreadRegistration participant={.context=(uintptr_t)&ctx};
    uint64_t attached;while((attached=graph_worker_call(g,&participant,GC_WK_ATTACH))!=GC_WK_OK){assert(attached==GC_WK_RETRY);metadata_yield();}
    // Fixed bounded pending storage is registered before this owner can scan.
    // All eight root handles remain reserved until worker retirement.
    RootCell pending_cells[8]={0};RootChange pending_changes[8];
    BufferCursor pending_journal={.entries=(uintptr_t)pending_changes,.capacity=8};
    RootSegment pending_segment={.owner=ctx.thread_id,.cells=(uintptr_t)pending_cells,.capacity=8,.journal=(uintptr_t)&pending_journal};
    RootRegistration pending_registration={.segment=(uintptr_t)&pending_segment};RootLease pending_lease={0};
    RootLifeRequest pending_life={.registration=(uintptr_t)&pending_registration,.lease=(uintptr_t)&pending_lease,.context=(uintptr_t)&ctx};
    metadata_lock(&g->m);assert(g->lifecycle.life(&g->root_registry,&pending_life,GC_RL_ATTACH)==GC_RL_OK);metadata_unlock(&g->m);
    RootRequest pending[8]={0};
    for(unsigned i=0;i<8;i++){pending[i].segment=(uintptr_t)&pending_segment;assert(g->rem_ops.move->root(&ctx,&pending[i],GC_ROOT_RESERVE)==GC_ROOT_OK);}
    CycleTicket requests[2]={{.kind=g->queue.generation},{.kind=g->queue.generation}};
    metadata_lock(&g->m);
    assert(g->lifecycle.cycle(&g->cycles,&requests[0],GC_CY_REQUEST)==GC_CY_OK);
    assert(requests[0].cycle==1&&!g->cycles.started[g->queue.generation]);
    g->cycle_requesters[0]++;
    metadata_unlock(&g->m);
    atomic_fetch_add_explicit(&g->pending_ready,1,memory_order_release);
    while(!atomic_load_explicit(&g->marking_ready,memory_order_acquire)){graph_poll(g,&participant);metadata_yield();}
    metadata_lock(&g->m);
    assert(g->lifecycle.cycle(&g->cycles,&requests[1],GC_CY_REQUEST)==GC_CY_OK);
    assert(requests[1].cycle==2&&g->cycles.started[g->queue.generation]==1);
    assert(g->lifecycle.cycle(&g->cycles,&requests[0],GC_CY_POLL)==GC_CY_WAIT);
    g->cycle_requesters[1]++;
    metadata_unlock(&g->m);
    while(!atomic_load_explicit(&g->stop,memory_order_acquire)) {
        graph_poll(g,&participant);
        if(graph_remembered_work(g,&ctx,&participant))continue;
        MarkRequest q={0};metadata_lock(&g->m);
        uint64_t status=g->spill(&g->queue,NULL,GC_MARK_REFILL);
        assert(status==GC_MARK_OK||status==GC_MARK_EMPTY||status==GC_MARK_FULL);if(status==GC_MARK_OK)g->refills++;
        status=g->mark(&g->queue,&q,GC_MARK_TAKE);metadata_unlock(&g->m);
        assert(status==GC_MARK_OK||status==GC_MARK_EMPTY);
        if(status==GC_MARK_EMPTY){
            uint64_t entered=graph_worker_call(g,&participant,GC_WK_ENTER_NATIVE);
            if(entered==GC_WK_RETRY)continue;
            assert(entered==GC_WK_OK);
            // This idle owner is genuinely native-blocked during the forced
            // rendezvous. It cannot touch roots, buffers or heap until LEAVE.
            while(!atomic_load_explicit(&g->native_gate,memory_order_acquire))metadata_yield();
            uint64_t left;while((left=graph_worker_call(g,&participant,GC_WK_LEAVE_NATIVE))!=GC_WK_OK){assert(left==GC_WK_RETRY);metadata_yield();}
            continue;
        }
        if(q.record==(uintptr_t)&g->records[0]&&!q.cursor) {
            atomic_store_explicit(&g->parked,1,memory_order_release);
            while(!atomic_load_explicit(&g->start,memory_order_acquire)){graph_poll(g,&participant);metadata_yield();}
        }
        uint64_t children[8],locations[8],observations[8];MarkScan scan={(uintptr_t)&q,g->m.f.h.heap_base,(uintptr_t)&g->registry,(uintptr_t)children,8,0,0,0,(uintptr_t)locations};
        MetadataLeaseRequest access_lease={0};
        GraphAccess guard=graph_scan_enter(g,&ctx,&participant,&q,&access_lease);
        uint64_t before=q.cursor;status=g->scan(&scan);
        graph_exit(g,&guard);
        assert(status==GC_SCAN_DONE||status==GC_SCAN_MORE);assert(scan.examined<=8&&scan.written<=8&&q.cursor==before);
        // No poll/flush/allocation between the scan and these bounded stores.
        // The empty journal reserves enough room for the entire result slice.
        assert(pending_journal.top==atomic_load(&pending_journal.consumed));
        uint64_t pending_mask=0;
        for(uint64_t i=0;i<scan.written;i++){
            assert(!atomic_load(&pending_cells[i].value));
            if(children[i]){
                pending_mask|=UINT64_C(1)<<i;pending[i].value=children[i];
                assert(g->rem_ops.move->root(&ctx,&pending[i],GC_ROOT_STORE)==GC_ROOT_OK);
                pending[i].value=0;
            }
            // Exact comparison bits only: never dereference this snapshot.
            // Pending roots protect the logical children; this fixture retains
            // all forwarding controls until teardown (no address reuse).
            observations[i]=children[i];
            children[i]=0;
        }
        assert(metadata_call(&g->m,&access_lease,GC_ML_RELEASE)==GC_ML_OK);
        atomic_fetch_add_explicit(&g->slices,1,memory_order_relaxed);atomic_fetch_add_explicit(&g->slots,scan.examined,memory_order_relaxed);
        if(q.record==(uintptr_t)&g->records[0]&&!before){
            atomic_store_explicit(&g->pending_segment,(uintptr_t)&pending_segment,memory_order_release);
            atomic_store_explicit(&g->children_parked,1,memory_order_release);
            while(!atomic_load_explicit(&g->children_resume,memory_order_acquire)){graph_poll(g,&participant);metadata_yield();}
        }
        for(uint64_t i=0;i<scan.written;i++)if(pending_mask&(UINT64_C(1)<<i)){
            graph_offer_root(g,&ctx,&participant,&pending_registration,&pending[i]);
        }
        // Commit the publication journals, then clear and commit deletions.
        // The parent claim/cursor is unchanged until every child handoff ends.
        while(graph_worker_journal_one(g,&ctx,&participant,&pending_registration))graph_poll(g,&participant);
        if(pending_mask){
            // Reacquire after every child offer/journal poll. No payload address
            // from the original scan survives. Only offsets and CAS bits do.
            access_lease=(MetadataLeaseRequest){0};
            guard=graph_scan_enter(g,&ctx,&participant,&q,&access_lease);
            uint64_t phase=atomic_load(&ctx.adopted_phase);
            for(uint64_t i=0;i<scan.written;i++)if(pending_mask&(UINT64_C(1)<<i)){
                // These native cells are reserved and uniquely owned until the
                // clear below. A direct atomic snapshot cannot poll or release
                // a root; unlike the public root-load it does not resolve it.
                uint64_t desired=atomic_load_explicit(&pending_cells[i].value,memory_order_acquire);
                assert(desired&&pending_cells[i].active);
                GuardedHeal repair={.scope=(uintptr_t)&guard.request.scope,.page=(uintptr_t)guard.page,
                    .registry=(uintptr_t)&g->registry,.byte_offset=locations[i],.expected=observations[i],
                    .desired=(desired&~UINT64_C(0xffff))|(gc_abi_color((PhaseSnapshot){phase})&~0x30u)|(observations[i]&0x30u),
                    .phase=phase};
                uint64_t repaired=g->heal(&repair);
                assert(repaired==GC_HEAL_OK||repaired==GC_HEAL_CHANGED);
                if(repaired==GC_HEAL_OK)atomic_fetch_add_explicit(&g->worker_repairs,1,memory_order_relaxed);
                else atomic_fetch_add_explicit(&g->worker_conflicts,1,memory_order_relaxed);
            }
            graph_exit(g,&guard);assert(metadata_call(&g->m,&access_lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        for(uint64_t i=0;i<scan.written;i++)if(pending_mask&(UINT64_C(1)<<i))assert(g->rem_ops.move->root(&ctx,&pending[i],GC_ROOT_STORE)==GC_ROOT_OK);
        while(graph_worker_journal_one(g,&ctx,&participant,&pending_registration))graph_poll(g,&participant);
        q.cursor=scan.next_cursor;
        if(q.record==(uintptr_t)&g->records[0]&&q.cursor==64){
            atomic_store_explicit(&g->array_parked,1,memory_order_release);
            while(!atomic_load_explicit(&g->array_resume,memory_order_acquire)){graph_poll(g,&participant);metadata_yield();}
        }
        metadata_lock(&g->m);
        assert(g->mark(&g->queue,&q,status==GC_SCAN_MORE?GC_MARK_YIELD:GC_MARK_FINISH)==GC_MARK_OK);
        if(status==GC_SCAN_DONE){MetadataLeaseRequest lease={.token=q.metadata_lease};assert(g->m.metadata(&g->m.leases,&lease,GC_ML_RESOLVE)==GC_ML_OK);assert(lease.incarnation==q.incarnation);assert(g->m.metadata(&g->m.leases,&lease,GC_ML_RELEASE)==GC_ML_OK);}
        metadata_unlock(&g->m);
    }
    for(unsigned i=0;i<8;i++)assert(g->rem_ops.move->root(&ctx,&pending[i],GC_ROOT_RELEASE)==GC_ROOT_OK);
    while(graph_worker_journal_one(g,&ctx,&participant,&pending_registration))graph_poll(g,&participant);
    metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&pending_life,GC_RL_RETIRE)==GC_RL_OK);
    assert(g->lifecycle.life(&g->root_registry,&pending_life,GC_RL_CLAIM)==GC_RL_OK);metadata_unlock(&g->m);
    uint64_t retired_values[8];RootScanJob retired_scan={.scan={(uintptr_t)&pending_segment,pending_life.begin,(uintptr_t)retired_values,8,0},
        .lease=(uintptr_t)&pending_lease,.serial=pending_life.serial};
    assert(g->lifecycle.leased(&retired_scan)==GC_ROOT_OK&&retired_scan.scan.written==8);
    for(unsigned i=0;i<8;i++)assert(!retired_values[i]);
    metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&pending_life,GC_RL_ACK)==GC_RL_OK);
    assert(g->lifecycle.life(&g->root_registry,&pending_life,GC_RL_CLEAN)==GC_RL_OK);metadata_unlock(&g->m);
    assert(g->lifecycle.clear(&pending_life)==GC_RL_OK);metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&pending_life,GC_RL_FINISH)==GC_RL_OK);
    // Select after the real root/heap audit is not full-cycle completion.
    for(unsigned i=0;i<2;i++)assert(g->lifecycle.cycle(&g->cycles,&requests[i],GC_CY_POLL)==GC_CY_WAIT);
    assert(gc_context_transition_locked(&ctx,GC_RUNNING,GC_DETACHING)==GC_CONTEXT_OK);
    assert(g->lifecycle.handshake(&g->handshake,&participant,GC_HS_DETACH)==GC_HS_OK);
    metadata_unlock(&g->m);
    return 0;
}
/* Construction occurs in the initial idle phase. The registered observer
 * borrows real segment/journal leases, never reads initializing payloads, and
 * deliberately withholds journal ACK while the allocating owner hits FULL. */
typedef struct {
    MarkGraph *graph;RootRegistration *registration;
    _Atomic uint64_t requested,claimed,allow_ack,processed,ready;
    uint64_t snapshots;
} GraphAllocationObserver;
#ifdef _WIN32
static DWORD WINAPI graph_allocation_observer(void *argument)
#else
static void *graph_allocation_observer(void *argument)
#endif
{
    GraphAllocationObserver *w=argument;MarkGraph *g=w->graph;
    MutatorContext ctx={.abi_version=GC_ABI_VERSION};ThreadRegistration participant={.context=(uintptr_t)&ctx};
    assert(graph_worker_call(g,&participant,GC_WK_ATTACH)==GC_WK_OK);
    for(uint64_t sequence=1;sequence<=17;sequence++){
        RootRegistration *r=w->registration;RootSegment *s=(RootSegment *)(uintptr_t)r->segment;
        RootLease lease={0};RootLifeRequest life={.registration=(uintptr_t)r,.lease=(uintptr_t)&lease,.scope=r->scope};
        metadata_lock(&g->m);assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_ACQUIRE)==GC_RL_OK);metadata_unlock(&g->m);
        uint64_t values[9]={0};RootScanJob scan={0};
        for(;;){
            scan=(RootScanJob){.scan={(uintptr_t)s,0,(uintptr_t)values,9,0},.lease=(uintptr_t)&lease,.serial=life.serial};
            assert(g->lifecycle.leased(&scan)==GC_ROOT_OK);w->snapshots++;
            /* An acquired nonnull cell orders its AllocationResult writes.
             * Initializing payloads remain unreadable and are never touched. */
            for(unsigned i=0;i<scan.scan.written;i++)if(values[i])assert(i<8&&values[i]==g->objects[i].reference);
            atomic_store_explicit(&w->ready,sequence,memory_order_release);
            if(atomic_load_explicit(&w->requested,memory_order_acquire)>=sequence)break;
            graph_poll(g,&participant);metadata_yield();
        }
        // The request follows the producer's completed publication/deletion.
        scan=(RootScanJob){.scan={(uintptr_t)s,0,(uintptr_t)values,9,0},.lease=(uintptr_t)&lease,.serial=life.serial};
        assert(g->lifecycle.leased(&scan)==GC_ROOT_OK);
        unsigned index=sequence<=8?(unsigned)sequence-1:(unsigned)sequence-9;
        assert(scan.scan.written>index&&values[index]==(sequence<=8?g->objects[index].reference:0));
        metadata_lock(&g->m);assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_RELEASE)==GC_RL_OK);metadata_unlock(&g->m);
        RootLease journal_lease={0};RootChange change={0};
        RootJournalRequest journal={.registration=(uintptr_t)r,.scope=r->scope,.lease=(uintptr_t)&journal_lease,
            .output=(uintptr_t)&change,.capacity=1};
        metadata_lock(&g->m);assert(g->lifecycle.journal(&g->root_registry,&journal,GC_RJ_CLAIM)==GC_RJ_OK);metadata_unlock(&g->m);
        assert(g->lifecycle.journal(&g->root_registry,&journal,GC_RJ_COPY)==GC_RJ_OK&&journal.written==1);
        assert(change.phase==0&&change.scope==r->scope);
        assert(change.previous==(sequence>8&&index<8?g->objects[index].reference:0));
        assert(change.next==(sequence<=8?g->objects[index].reference:0));
        atomic_store_explicit(&w->claimed,sequence,memory_order_release);
        while(atomic_load_explicit(&w->allow_ack,memory_order_acquire)<sequence){graph_poll(g,&participant);metadata_yield();}
        metadata_lock(&g->m);
        /* No active mark exists in phase zero. This is an idle-phase journal
         * handoff, not permission to drop entries during concurrent marking. */
        assert(g->root_registry.phase==0&&atomic_load(&ctx.adopted_phase)==0&&!g->handshake.active);
        assert(g->lifecycle.journal(&g->root_registry,&journal,GC_RJ_ACK)==GC_RJ_OK);
        metadata_unlock(&g->m);atomic_store_explicit(&w->processed,sequence,memory_order_release);
    }
    metadata_lock(&g->m);
    assert(gc_context_transition_locked(&ctx,GC_RUNNING,GC_DETACHING)==GC_CONTEXT_OK);
    assert(g->lifecycle.handshake(&g->handshake,&participant,GC_HS_DETACH)==GC_HS_OK);
    metadata_unlock(&g->m);return 0;
}
static void graph_allocation_observe(GraphAllocationObserver *w,uint64_t sequence){
    atomic_store_explicit(&w->requested,sequence,memory_order_release);
    while(atomic_load_explicit(&w->claimed,memory_order_acquire)<sequence)metadata_yield();
}
static void graph_allocation_ack(GraphAllocationObserver *w,uint64_t sequence){
    atomic_store_explicit(&w->allow_ack,sequence,memory_order_release);
    while(atomic_load_explicit(&w->processed,memory_order_acquire)<sequence)metadata_yield();
}
// The joined-worker coordinator excludes allocation/table replacement and
// compression across this complete walk, in addition to closed move admission.
// One page lease survives each bounded incoming-table scan; no payload is read,
// including for the OS-decommitted source heads. This is not general concurrent
// retirement: the cycle still owns every logical/native control lifetime.
static uint64_t graph_forward_range_audit(MarkGraph *g,HeapWalker next,ForwardIncomingScanner incoming,
    uint64_t epoch,uint64_t phase,GraphAuditRange range){
    GcHeapWalk walk={(uintptr_t)&g->m.leases,(uintptr_t)&g->m.f.h,g->m.f.h.pages,g->m.f.h.page_count,0,{0}};
    uint64_t findings=0,heads=0,keys=0;unsigned steps=0;
    for(;;){
        metadata_lock(&g->m);
        assert(g->root_registry.phase==phase&&g->relocation.epoch==epoch);
        assert(g->lifecycle.relocation_gate(&g->relocation,NULL,GC_RG_CHECK)==GC_RG_OK);
        uint64_t status=next(&g->m.leases,&walk);
        if(status!=GC_HW_PAGE){
            metadata_unlock(&g->m);if(status==GC_HW_DONE)break;
            assert(status==GC_HW_MORE);continue;
        }
        AllocationPage *page=(AllocationPage *)(uintptr_t)walk.lease.page;
        assert(page->forward_table&&walk.lease.incarnation==page->incarnation);
        const ForwardTable *table=(const ForwardTable *)(uintptr_t)page->forward_table;
        ForwardIncoming scan={page->incarnation,atomic_load_explicit(&table->used,memory_order_acquire),
            range.start,range.limit,0,1,0,0};
        keys+=scan.used;heads++;metadata_unlock(&g->m);
        for(;;){
            assert(++steps<256);status=incoming(table,&scan);
            assert(scan.examined<=1);
            if(status==GC_FI_DONE)break;
            assert(status==GC_FI_MORE||status==GC_FI_FOUND);
            if(status==GC_FI_FOUND){
                assert(scan.source_offset&&scan.cursor<scan.used);findings++;
                // Consume the diagnostic location, retaining its table lease.
                scan.source_offset=0;scan.cursor++;
            }
            metadata_yield();
        }
        assert(scan.cursor==scan.used&&!scan.source_offset);
        metadata_lock(&g->m);
        assert(g->root_registry.phase==phase&&g->relocation.epoch==epoch);
        assert(g->lifecycle.relocation_gate(&g->relocation,NULL,GC_RG_CHECK)==GC_RG_OK);
        assert(g->m.metadata(&g->m.leases,&walk.lease,GC_ML_RELEASE)==GC_ML_OK);
        metadata_unlock(&g->m);
    }
    assert(walk.cursor==g->m.f.h.page_count&&!walk.lease.token&&heads==5&&keys>=8);
    return findings;
}
// Fresh whole-pool payload proof for one complete source run. The same frozen
// fixture ownership as the forwarding pass excludes old-reference introduction.
static void graph_heap_range_audit(MarkGraph *g,HeapAuditor step,uint64_t epoch,uint64_t phase,GraphAuditRange range){
    HeapAudit audit={.context=(uintptr_t)&g->m.ctx,.registry=(uintptr_t)&g->registry,
        .gate=(uintptr_t)&g->relocation,.epoch=epoch,.phase=phase,
        .target_start=range.start,.target_limit=range.limit,
        .walk={(uintptr_t)&g->m.leases,(uintptr_t)&g->m.f.h,g->m.f.h.pages,g->m.f.h.page_count,0,{0}}};
    unsigned steps=0;
    for(;;){
        assert(++steps<65536);
        uint64_t op=audit.stage==2?GC_HA_SCAN:GC_HA_META;
        if(op==GC_HA_META)metadata_lock(&g->m);
        uint64_t status=step(&g->m.leases,&audit,op);
        if(op==GC_HA_META)metadata_unlock(&g->m);
        if(status==GC_HA_EXIT_REQUIRED)continue; // immediate guard cleanup
        assert(!atomic_load(&g->m.ctx.access_epoch));
        if(status==GC_HA_DONE)break;
        assert(status==GC_HA_MORE||status==GC_HA_SCAN_READY);
        metadata_yield();
    }
    assert(!audit.findings&&audit.walk.cursor==g->m.f.h.page_count&&!audit.walk.lease.token);
    metadata_lock(&g->m);assert(step(&g->m.leases,&audit,GC_HA_CANCEL)==GC_HA_CANCELLED);metadata_unlock(&g->m);
}
/* The range was fully retired before reuse. Allocate through a registered
 * root into its new incarnation; all replacement metadata was charged in the
 * graph's native package before any managed allocation. This owner has no
 * competing mutator here and retains the new page/record through teardown. */
static void graph_reuse_allocate(MarkGraph *g,AllocationPage *page,MarkRecord *record,
        ForwardTable *table,ForwardCell *cell,uint64_t (*allocate_into)(MutatorContext *,const AllocationInto *)){
    const MarkMoveOps *ops=g->rem_ops.move;
    *table=(ForwardTable){(uintptr_t)cell,1,0};page->forward_table=(uintptr_t)table;
    RootCell root_cell={0};RootChange changes[2]={0};
    BufferCursor journal={.entries=(uintptr_t)changes,.capacity=2};
    RootSegment segment={.owner=g->m.ctx.thread_id,.cells=(uintptr_t)&root_cell,.capacity=1,.journal=(uintptr_t)&journal};
    RootRegistration registration={.segment=(uintptr_t)&segment};RootLease lease={0};
    RootLifeRequest life={.registration=(uintptr_t)&registration,.context=(uintptr_t)&g->m.ctx,.lease=(uintptr_t)&lease};
    metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_ATTACH)==GC_RL_OK);
    metadata_unlock(&g->m);
    RootRequest root={.segment=(uintptr_t)&segment};
    assert(ops->root(&g->m.ctx,&root,GC_ROOT_RESERVE)==GC_ROOT_OK);
    MarkArena arena={(uintptr_t)record,1,0};AllocationResult value={0};
    AllocationRequest request={(uintptr_t)page,(uintptr_t)&g->descriptors[2],0,(uintptr_t)&value,(uintptr_t)&arena};
    AllocationInto into={(uintptr_t)&request,(uintptr_t)&root};
    assert(allocate_into(&g->m.ctx,&into)==GC_ALLOC_OK);
    assert((value.reference>>16)==page->start+sizeof(ObjectHeader));
    assert(atomic_load(&root_cell.value)==value.reference&&record->object_id==value.object_id);
    assert(value.object_id>g->objects[6].object_id&&arena.used==1&&atomic_load(&table->used)==1);
    ObjectHeader *header=(void *)(uintptr_t)(page->heap_base+page->start);
    assert(header->mark_record==(uintptr_t)record&&header->payload_bytes==7&&atomic_load(&header->state)==GC_OBJECT_INITIALIZING);
    memcpy(header+1,"reused!",7);atomic_store_explicit(&header->state,GC_OBJECT_PUBLISHED,memory_order_release);header=NULL;
    metadata_lock(&g->m);
    assert(ops->bind(&g->m.ctx,page,1)==GC_ALLOC_OK);
    ReferenceLease current={.reference=value.reference};
    assert(g->reference_lease(&g->m.leases,&current)==GC_ML_OK&&current.lease.incarnation==page->incarnation);
    metadata_unlock(&g->m);
    GraphAccess access=graph_enter(g,&g->m.ctx,&current.lease,current.reference);
    header=(void *)(uintptr_t)(page->heap_base+(access.request.reference>>16)-sizeof(ObjectHeader));
    assert(g->lifecycle.identity((uintptr_t)&access.request.scope,(uintptr_t)page,atomic_load(&g->m.ctx.adopted_phase))==record);
    assert(header->mark_record!=(uintptr_t)&g->records[6]&&!memcmp(header+1,"reused!",7));header=NULL;
    graph_exit(g,&access);metadata_lock(&g->m);
    assert(g->m.metadata(&g->m.leases,&current.lease,GC_ML_RELEASE)==GC_ML_OK);metadata_unlock(&g->m);
    assert(ops->root(&g->m.ctx,&root,GC_ROOT_RELEASE)==GC_ROOT_OK);
    /* Retire is not a marking phase here. Consume both publication/deletion
     * receipts while the registered scope and fixture-owned object remain live. */
    for(unsigned i=0;i<2;i++){
        RootLease claim={0};RootJournalRequest job={0};RootChange change={0};
        graph_journal_claim(g,&registration,&claim,&job,&change);
        assert((i?change.previous:change.next)==value.reference);
        graph_journal_handoff(g,&job,&change);
    }
    assert(atomic_load(&journal.consumed)==2);
    metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_RETIRE)==GC_RL_OK);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_CLAIM)==GC_RL_OK);metadata_unlock(&g->m);
    uint64_t empty=1;RootScanJob scan={.scan={(uintptr_t)&segment,life.begin,(uintptr_t)&empty,1,0},
        .lease=(uintptr_t)&lease,.serial=life.serial};
    assert(g->lifecycle.leased(&scan)==GC_ROOT_OK&&scan.scan.written==1&&!empty);
    metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_ACK)==GC_RL_OK);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_CLEAN)==GC_RL_OK);metadata_unlock(&g->m);
    assert(g->lifecycle.clear(&life)==GC_RL_OK);metadata_lock(&g->m);
    assert(g->lifecycle.life(&g->root_registry,&life,GC_RL_FINISH)==GC_RL_OK);
    assert(!g->m.leases.active&&g->root_registry.active==1&&!g->root_registry.leases&&!g->m.ctx.retiring_roots);
    metadata_unlock(&g->m);
}
static void gc_run_mark_graph(const GraphLifecycle *lifecycle,const GraphRememberedOps *remembered,MarkRouter route,MarkStep mark,GraphMarkSpill spill,MarkScanner scan,MetadataStep metadata,BudgetStep budget,ReferenceAcquirer reference_lease,Resolver resolve,
    GuardedStorer store,GuardedHealer heal,PoolStep pool,TlabPage bind,
    uint64_t (*allocate_into)(MutatorContext *,const AllocationInto *),
    DescriptorRegister register_descriptor,GraphRootStep roots,SourceDrainer drain,SourceUncommitter uncommit,ForwardIncomingScanner incoming,ForwardCompressor compress,HeapIncomingScanner heap_incoming,HeapWalker walk_heap,ObjectWalker walk_object,HeapAuditSlicer audit_slice,HeapAuditor audit_owner) {
    for(unsigned round=0;round<64;round++) {
        if(getenv("KLASSIC_GC_GRAPH_TRACE"))fprintf(stderr,"mark graph round %u start\n",round);
        MarkGraph g={.lifecycle=*lifecycle,.rem_ops=*remembered,.route=route,.mark=mark,.spill=spill,.scan=scan,.heal=heal,.reference_lease=reference_lease,.resolve=resolve};g.m.metadata=metadata;g.relocation.epoch=1;
#ifdef _WIN32
        InitializeCriticalSection(&g.m.lock);
        InitializeConditionVariable(&g.metadata_available);
#else
        assert(pthread_mutex_init(&g.m.lock,NULL)==0);
        assert(pthread_cond_init(&g.metadata_available,NULL)==0);
#endif
        pool_fixture(&g.m.f,0);size_t bytes=9*GC_HEAP_PAGE_BYTES;void *mapping=pool_vm_reserve(bytes);g.m.f.h.heap_base=(uintptr_t)mapping;
        // Reserve an earlier range temporarily so the live run is not always
        // page zero. Abort the unused reservation before preparing the run.
        PageLease prefix={.page_count=round%5};
        if(prefix.page_count)assert(pool(&g.m.f.h,&prefix,GC_POOL_RESERVE)==0);
        uint64_t array_length=round%4==0?GC_HEAP_PAGE_BYTES/8:1024;
        uint64_t run_bytes=(round%4==0?2:1)*GC_HEAP_PAGE_BYTES;
        g.m.f.l.page_count=run_bytes/GC_HEAP_PAGE_BYTES;
        assert(pool(&g.m.f.h,&g.m.f.l,GC_POOL_RESERVE)==0);
        if(prefix.page_count)assert(pool(&g.m.f.h,&prefix,GC_POOL_ABORT)==0);
        g.page=&g.m.f.pages[g.m.f.l.head_index];
        pool_vm_commit((char *)mapping+g.page->start,run_bytes);
        void *starts=calloc(1,run_bytes/128);assert(starts);g.m.f.l.starts=(uintptr_t)starts;g.m.f.l.committed_mapping=1;
        assert(pool(&g.m.f.h,&g.m.f.l,GC_POOL_COMMIT)==0);
        PageLease old_run={.page_count=1,.relocation=1,.generation=GC_OLD};
        assert(pool(&g.m.f.h,&old_run,GC_POOL_RESERVE)==GC_POOL_OK);g.old_page=&g.m.f.pages[old_run.head_index];
        pool_vm_commit((char *)mapping+g.old_page->start,GC_HEAP_PAGE_BYTES);
        void *old_starts=calloc(1,GC_HEAP_PAGE_BYTES/128);assert(old_starts);
        old_run.starts=(uintptr_t)old_starts;old_run.committed_mapping=1;
        assert(pool(&g.m.f.h,&old_run,GC_POOL_COMMIT)==GC_POOL_OK);
        PageLease empty_run={.page_count=1,.relocation=1,.generation=GC_OLD};
        assert(pool(&g.m.f.h,&empty_run,GC_POOL_RESERVE)==GC_POOL_OK);
        AllocationPage *empty_page=&g.m.f.pages[empty_run.head_index];
        pool_vm_commit((char *)mapping+empty_page->start,GC_HEAP_PAGE_BYTES);
        void *empty_starts=calloc(1,GC_HEAP_PAGE_BYTES/128);assert(empty_starts);
        empty_run.starts=(uintptr_t)empty_starts;empty_run.committed_mapping=1;
        assert(pool(&g.m.f.h,&empty_run,GC_POOL_COMMIT)==GC_POOL_OK);
        PageLease middle_run={.page_count=1,.relocation=1,.generation=GC_OLD};
        assert(pool(&g.m.f.h,&middle_run,GC_POOL_RESERVE)==GC_POOL_OK);
        AllocationPage *middle_page=&g.m.f.pages[middle_run.head_index];
        pool_vm_commit((char *)mapping+middle_page->start,GC_HEAP_PAGE_BYTES);
        void *middle_starts=calloc(1,GC_HEAP_PAGE_BYTES/128);assert(middle_starts);
        middle_run.starts=(uintptr_t)middle_starts;middle_run.committed_mapping=1;
        assert(pool(&g.m.f.h,&middle_run,GC_POOL_COMMIT)==GC_POOL_OK);
        PageLease array_run={.page_count=run_bytes/GC_HEAP_PAGE_BYTES,.relocation=1};
        assert(pool(&g.m.f.h,&array_run,GC_POOL_RESERVE)==GC_POOL_OK);
        AllocationPage *array_page=&g.m.f.pages[array_run.head_index];
        pool_vm_commit((char *)mapping+array_page->start,run_bytes);
        void *array_starts=calloc(1,run_bytes/128);assert(array_starts);
        array_run.starts=(uintptr_t)array_starts;array_run.committed_mapping=1;
        assert(pool(&g.m.f.h,&array_run,GC_POOL_COMMIT)==GC_POOL_OK);
        g.m.ctx=(MutatorContext){.abi_version=GC_ABI_VERSION};ThreadRegistration owner={.context=(uintptr_t)&g.m.ctx};
        assert(graph_worker_call(&g,&owner,GC_WK_ATTACH)==GC_WK_OK);assert(bind(&g.m.ctx,g.page,0)==0);
        // Charge all task, per-object stamp/spill and lease storage BEFORE any
        // object allocation or publication. One byte short must leave the
        // empty TLAB, bitmap, object results and accounting unchanged.
        uint64_t base_metadata=g.m.f.h.metadata_committed;
        MetadataCharge charge={.bytes=sizeof(GraphStorage)};
        g.m.f.h.metadata_max=base_metadata+sizeof(GraphStorage)-1;
        HeapPool before_heap=g.m.f.h;MutatorContext before_ctx;memcpy(&before_ctx,&g.m.ctx,sizeof(before_ctx));
        assert(budget(&g.m.f.h,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_LIMIT);
        assert(!memcmp(&before_heap,&g.m.f.h,sizeof(before_heap))&&!memcmp(&before_ctx,&g.m.ctx,sizeof(before_ctx))&&!charge.stage);
        for(unsigned i=0;i<8;i++)assert(!g.objects[i].reference&&!g.objects[i].object_id);
        for(unsigned i=0;i<run_bytes/128/8;i++)assert(!((uint64_t *)starts)[i]);
        g.m.f.h.metadata_max++; // exact fit, including both spill cells
        assert(budget(&g.m.f.h,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        // Inject failure before native allocation; abort restores full capacity.
        assert(budget(&g.m.f.h,&charge,GC_BUDGET_ABORT)==GC_BUDGET_OK);
        assert(!g.m.f.h.metadata_pending&&g.m.f.h.metadata_committed==base_metadata);
        charge=(MetadataCharge){.bytes=sizeof(GraphStorage)};
        assert(budget(&g.m.f.h,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        GraphStorage *storage=calloc(1,sizeof(*storage));assert(storage);charge.allocation=(uintptr_t)storage;
        assert(budget(&g.m.f.h,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
        assert(g.m.f.h.metadata_committed==g.m.f.h.metadata_max&&!g.m.f.h.metadata_pending);
        GraphSourceStorage *sources[2]={0};MetadataCharge source_charges[2]={0};
        g.m.f.h.metadata_max+=2*sizeof(GraphSourceStorage);
        for(unsigned i=0;i<2;i++){
            source_charges[i].bytes=sizeof(GraphSourceStorage);
            assert(budget(&g.m.f.h,&source_charges[i],GC_BUDGET_RESERVE)==GC_BUDGET_OK);
            sources[i]=calloc(1,sizeof(*sources[i]));assert(sources[i]);
            source_charges[i].allocation=(uintptr_t)sources[i];
            assert(budget(&g.m.f.h,&source_charges[i],GC_BUDGET_COMMIT)==GC_BUDGET_OK);
        }
        assert(g.m.f.h.metadata_committed==g.m.f.h.metadata_max&&!g.m.f.h.metadata_pending);
        uint64_t retired_bitmap_bytes=0,recommitted_bitmap_bytes=0;
        g.tasks=storage->tasks;g.records=storage->records;g.leases=storage->leases;
        g.registry=(DescriptorRegistry){(uintptr_t)g.descriptor_slots,4};g.bitmap=1;
        g.descriptors[0]=(TypeDescriptor){.id=1,.alignment=8,.element_bytes=8,.element_bitmap=(uintptr_t)&g.bitmap,.element_bitmap_words=1};
        g.descriptors[1]=(TypeDescriptor){.id=2,.alignment=8,.fixed_bytes=8,.fixed_bitmap=(uintptr_t)&g.bitmap,.fixed_bitmap_words=1};
        g.descriptors[2]=(TypeDescriptor){.id=3,.alignment=8,.fixed_bytes=7};
        for(unsigned i=0;i<3;i++)assert(register_descriptor(&g.registry,&g.descriptors[i])==0);
        MarkArena arena={(uintptr_t)g.records,8,0};
        storage->forwards=(ForwardTable){(uintptr_t)storage->cells,8,0};
        g.page->forward_table=(uintptr_t)&storage->forwards;
        RootCell construction_cells[9]={0};RootChange construction_change={0};
        BufferCursor construction_journal={.entries=(uintptr_t)&construction_change,.capacity=1};
        RootSegment construction_segment={.owner=g.m.ctx.thread_id,.cells=(uintptr_t)construction_cells,
            .capacity=9,.journal=(uintptr_t)&construction_journal};
        RootRegistration construction_registration={.segment=(uintptr_t)&construction_segment};RootLease construction_lease={0};
        RootLifeRequest construction_life={.registration=(uintptr_t)&construction_registration,.lease=(uintptr_t)&construction_lease,.context=(uintptr_t)&g.m.ctx};
        metadata_lock(&g.m);assert(lifecycle->life(&g.root_registry,&construction_life,GC_RL_ATTACH)==GC_RL_OK);metadata_unlock(&g.m);
        RootRequest construction_roots[9]={0};GraphAllocationObserver observer={.graph=&g,.registration=&construction_registration};
        MetadataThread observer_thread;
#ifdef _WIN32
        observer_thread=CreateThread(NULL,0,graph_allocation_observer,&observer,0,NULL);assert(observer_thread);
#else
        assert(pthread_create(&observer_thread,NULL,graph_allocation_observer,&observer)==0);
#endif
        for(unsigned i=0;i<8;i++){
            unsigned shape=i==0?0:(i==2||i==5||i==7?2:1);
            AllocationRequest q={(uintptr_t)g.page,(uintptr_t)&g.descriptors[shape],i==0?array_length:0,(uintptr_t)&g.objects[i],(uintptr_t)&arena};
            construction_roots[i].segment=(uintptr_t)&construction_segment;
            assert(roots(&g.m.ctx,&construction_roots[i],GC_ROOT_RESERVE)==GC_ROOT_OK);
            AllocationInto into={(uintptr_t)&q,(uintptr_t)&construction_roots[i]};
            if(i){
                uint64_t unchanged_top=g.m.ctx.tlab_top,unchanged_revision=atomic_load(&g.m.ctx.root_revision);
                assert(allocate_into(&g.m.ctx,&into)==GC_ALLOC_ROOT_FLUSH);
                assert(g.m.ctx.tlab_top==unchanged_top&&arena.used==i&&!g.objects[i].reference&&!atomic_load(&construction_cells[i].value));
                assert(atomic_load(&g.m.ctx.root_revision)==unchanged_revision&&atomic_load(&construction_journal.consumed)==i-1);
                graph_allocation_ack(&observer,i);
            }
            while(atomic_load_explicit(&observer.ready,memory_order_acquire)<i+1)metadata_yield();
            assert(allocate_into(&g.m.ctx,&into)==GC_ALLOC_OK);
            assert(arena.used==i+1&&g.records[i].object_id==g.objects[i].object_id);
            graph_allocation_observe(&observer,i+1);
        }
        uint64_t top_before=g.m.ctx.tlab_top;
        AllocationResult denied={UINT64_MAX,UINT64_MAX};
        AllocationRequest denied_request={(uintptr_t)g.page,(uintptr_t)&g.descriptors[2],0,(uintptr_t)&denied,(uintptr_t)&arena};
        graph_allocation_ack(&observer,8);
        construction_roots[8].segment=(uintptr_t)&construction_segment;
        assert(roots(&g.m.ctx,&construction_roots[8],GC_ROOT_RESERVE)==GC_ROOT_OK);
        AllocationInto denied_into={(uintptr_t)&denied_request,(uintptr_t)&construction_roots[8]};
        assert(allocate_into(&g.m.ctx,&denied_into)==GC_ALLOC_METADATA_REFILL);
        assert(!atomic_load(&construction_cells[8].value)&&construction_journal.top==8);
        assert(g.m.ctx.tlab_top==top_before&&arena.used==8&&denied.reference==UINT64_MAX&&denied.object_id==UINT64_MAX);
        _Atomic uint64_t *array=(_Atomic uint64_t *)((char *)mapping+(g.objects[0].reference>>16));
        for(unsigned i=0;i<array_length;i++)atomic_store(&array[i],i%63?0:g.objects[1].reference);
        atomic_store(&array[1],g.objects[2].reference); // this object's only edge
        for(unsigned i=1;i<5;i++)if(i!=2){_Atomic uint64_t *field=(_Atomic uint64_t *)((char *)mapping+(g.objects[i].reference>>16));atomic_store(field,g.objects[i==1?3:i==3?4:3].reference);}
        atomic_store((_Atomic uint64_t *)((char *)mapping+(g.objects[6].reference>>16)),g.objects[7].reference);
        const unsigned char survivor_bytes[7]={0x73,0x19,0xc5,0,0xff,0x42,0x8e};
        memcpy((char *)mapping+(g.objects[7].reference>>16),survivor_bytes,sizeof(survivor_bytes));
        // In Old rounds the array remains INITIALIZING until its registered
        // root and copied publication journal encounter the mark router.
        for(unsigned i=round%2?1:0;i<8;i++){ObjectHeader *h=(ObjectHeader *)((char *)mapping+(g.objects[i].reference>>16)-sizeof(ObjectHeader));atomic_store_explicit(&h->state,GC_OBJECT_PUBLISHED,memory_order_release);}
        for(unsigned i=0;i<9;i++){
            while(atomic_load_explicit(&observer.ready,memory_order_acquire)<i+9)metadata_yield();
            assert(roots(&g.m.ctx,&construction_roots[i],GC_ROOT_RELEASE)==GC_ROOT_OK);
            graph_allocation_observe(&observer,i+9);graph_allocation_ack(&observer,i+9);
        }
#ifdef _WIN32
        assert(WaitForSingleObject(observer_thread,INFINITE)==WAIT_OBJECT_0);assert(CloseHandle(observer_thread));
#else
        assert(pthread_join(observer_thread,NULL)==0);
#endif
        metadata_lock(&g.m);
        assert(observer.snapshots>=17);
        assert(g.handshake.members==1&&!g.root_registry.leases&&construction_journal.top==17&&atomic_load(&construction_journal.consumed)==17);
        assert(lifecycle->life(&g.root_registry,&construction_life,GC_RL_RETIRE)==GC_RL_OK);
        assert(lifecycle->life(&g.root_registry,&construction_life,GC_RL_CLAIM)==GC_RL_OK);metadata_unlock(&g.m);
        uint64_t retired[9];RootScanJob construction_scan={.scan={(uintptr_t)&construction_segment,construction_life.begin,(uintptr_t)retired,9,0},
            .lease=(uintptr_t)&construction_lease,.serial=construction_life.serial};
        assert(lifecycle->leased(&construction_scan)==GC_ROOT_OK&&construction_scan.scan.written==9);
        for(unsigned i=0;i<9;i++)assert(!retired[i]);
        metadata_lock(&g.m);
        assert(lifecycle->life(&g.root_registry,&construction_life,GC_RL_ACK)==GC_RL_OK);
        assert(lifecycle->life(&g.root_registry,&construction_life,GC_RL_CLEAN)==GC_RL_OK);metadata_unlock(&g.m);
        assert(lifecycle->clear(&construction_life)==GC_RL_OK);metadata_lock(&g.m);
        assert(lifecycle->life(&g.root_registry,&construction_life,GC_RL_FINISH)==GC_RL_OK);
        assert(!g.root_registry.active&&!g.root_registry.retiring&&!g.m.ctx.root_segments&&!g.m.ctx.retiring_roots);metadata_unlock(&g.m);
        assert(bind(&g.m.ctx,g.page,1)==0);
        // Establish one fixture-held Old owner. Its Young child is absent
        // from roots, SATB and the ordinary graph; only this remset may find it.
        const MarkMoveOps *o=remembered->move;
        sources[0]->table=(ForwardTable){(uintptr_t)&sources[0]->cell,1,0};
        g.old_page->forward_table=(uintptr_t)&sources[0]->table;
        storage->moved_forwards=(ForwardTable){(uintptr_t)storage->moved_cells,round%8==0?3:1,0};
        empty_page->forward_table=(uintptr_t)&storage->moved_forwards;
        sources[1]->table=(ForwardTable){(uintptr_t)&sources[1]->cell,1,0};
        middle_page->forward_table=(uintptr_t)&sources[1]->table;
        storage->array_forwards=(ForwardTable){(uintptr_t)storage->array_cells,4,0};
        array_page->forward_table=(uintptr_t)&storage->array_forwards;
        ForwardCell *source=&storage->cells[6];ForwardRequest claim={source->source_offset,source->incarnation,0,0};
        assert(graph_move_claim(&g,source,&claim)==GC_FWD_OK);
        SourceAccessRequest select={0,source->source_offset,source->incarnation,0,claim.ticket};
        assert(o->access(source,&select,GC_SA_SELECT)==GC_SA_OK&&o->access(source,&select,GC_SA_CHECK)==GC_SA_OK);
        RelocationCopy copy={(uintptr_t)mapping,source->incarnation,claim.ticket,g.page->limit,0,0,8,0,0};
        assert(graph_target(&g,g.old_page,&copy,GC_TARGET_PREPARE)==GC_TARGET_OK);
        assert(o->copy(source,&copy)==GC_COPY_DONE&&graph_target(&g,g.old_page,&copy,GC_TARGET_INSTALL)==GC_TARGET_OK);
        // Promotion bookkeeping is explicit fixture setup, before publication.
        g.remembered.epoch=g.remembered.last_phase=0x10000;
        g.rem_set=(RememberedSet){.start=g.old_page->start,.limit=g.old_page->limit,.incarnation=g.old_page->incarnation,
            .bits={(uintptr_t)storage->remembered_bits[0],(uintptr_t)storage->remembered_bits[1]},.words=256,.epoch=0x10000,.last_phase=0x10000};
        g.rem_registration.set=(uintptr_t)&g.rem_set;
        RememberedJob rem_job={.offset=copy.destination,.incarnation=g.old_page->incarnation,.phase=0x10000,.budget=256};
        RememberedInventoryRequest rem_request={(uintptr_t)&g.rem_registration,(uintptr_t)&rem_job,0};
        // Capture the source slot in its original epoch. Fixture preparation
        // retains its native controls until a real buffer lease is acquired.
        BarrierEntry delayed_record={.slot={.owner={(rem_job.offset<<16)|(g.objects[6].reference&UINT16_MAX)},.byte_offset=0,
            .page_incarnation=g.rem_set.incarnation,.record_epoch=g.remembered.epoch}},delayed_storage;

        assert(remembered->inventory(&g.remembered,&rem_request,GC_RI_ATTACH)==GC_RS_OK);
        assert(remembered->inventory(&g.remembered,&rem_request,GC_RI_RECORD)==GC_RS_OK);
        g.empty_set=(RememberedSet){.start=empty_page->start,.limit=empty_page->limit,.incarnation=empty_page->incarnation,
            .bits={(uintptr_t)storage->empty_bits[0],(uintptr_t)storage->empty_bits[1]},.words=256,.epoch=0x10000,.last_phase=0x10000};
        g.empty_registration.set=(uintptr_t)&g.empty_set;
        RememberedJob empty_job={.incarnation=empty_page->incarnation,.budget=256};
        RememberedInventoryRequest empty_request={(uintptr_t)&g.empty_registration,(uintptr_t)&empty_job,0};
        assert(remembered->inventory(&g.remembered,&empty_request,GC_RI_ATTACH)==GC_RS_OK);
        assert(graph_move_publish(&g,source,&copy,g.objects[6].reference&UINT16_MAX)==GC_COPY_DONE);
        g.old_owner=(copy.destination<<16)|(g.objects[6].reference&UINT16_MAX);
        g.queue=(MarkQueue){.tasks=(uintptr_t)g.tasks,.capacity=round%3==0?1:round%3==1?2:16,.free_head=1,.epoch=round+1,.generation=round%2};
        for(unsigned i=0;i<g.queue.capacity;i++)g.tasks[i].next=i+1<g.queue.capacity?i+2:0;
        g.m.leases=(MetadataLeasePool){(uintptr_t)&g.m.f.h,(uintptr_t)g.leases,32,1,0};for(unsigned i=0;i<32;i++)g.leases[i].next_free=i+1<32?i+2:0;
        RootAudit audit={.queue=(uintptr_t)&g.queue,.context=(uintptr_t)&g.m.ctx};
        g.phase_control=(MarkPhaseControl){.handshake=(uintptr_t)&g.handshake,.audit=(uintptr_t)&audit,.generation=g.queue.generation,.remembered=(uintptr_t)&g.remembered};
        MetadataThread workers[2];
#ifdef _WIN32
        for(unsigned i=0;i<2;i++){workers[i]=CreateThread(NULL,0,graph_marker,&g,0,NULL);assert(workers[i]);}
#else
        for(unsigned i=0;i<2;i++)assert(pthread_create(&workers[i],NULL,graph_marker,&g)==0);
#endif
        for(;;){metadata_lock(&g.m);bool ready=g.handshake.members==3&&atomic_load_explicit(&g.pending_ready,memory_order_acquire)==2;metadata_unlock(&g.m);if(ready)break;metadata_yield();}
        uint64_t store_phase=((uint64_t)(round+2)<<16)|(g.queue.generation?0x10:0x1001);
        metadata_lock(&g.m);
        if(!g.queue.generation){
            rem_request.phase=store_phase;assert(remembered->inventory(&g.remembered,&rem_request,GC_RI_BEGIN)==GC_RS_OK);
            rem_job.phase=store_phase;assert(remembered->inventory(&g.remembered,&rem_request,GC_RI_FLIP)==GC_RS_OK);
            // The empty head remains unflipped for the registered worker.
        }
        g.root_registry.phase=store_phase;
        assert(g.cycle_requesters[0]==2&&g.cycles.requested[g.queue.generation]==1);
        g.active_cycle.kind=g.queue.generation;
        CycleMarkStart admission={(uintptr_t)&g.root_registry,(uintptr_t)&g.phase_control};
        assert(lifecycle->cycle_start(&g.cycles,&g.active_cycle,&admission)==GC_CY_OK);
        assert(g.active_cycle.cycle==1&&g.cycles.started[g.queue.generation]==1);
        metadata_unlock(&g.m);
        assert(graph_worker_call(&g,&owner,GC_WK_POLL)==GC_WK_PARK);
        for(;;){metadata_lock(&g.m);uint64_t result=lifecycle->phase(&g.root_registry,&g.phase_control,GC_MP_ACTIVATE);metadata_unlock(&g.m);
            if(result==GC_MP_ADVANCED)break;assert(result==GC_MP_WAIT);metadata_yield();}
        assert(graph_worker_call(&g,&owner,GC_WK_POLL)==GC_WK_PARK);
        for(;;){metadata_lock(&g.m);uint64_t result=lifecycle->phase(&g.root_registry,&g.phase_control,GC_MP_RELEASE);metadata_unlock(&g.m);
            if(result==GC_MP_MARKING)break;assert(result==GC_MP_WAIT);metadata_yield();}
        store_phase=g.root_registry.phase;graph_poll(&g,&owner);
        RootCell cell={0};RootChange changes[2];BufferCursor journal={.entries=(uintptr_t)changes,.capacity=2};
        RootSegment segment={.owner=g.m.ctx.thread_id,.cells=(uintptr_t)&cell,.capacity=1,.journal=(uintptr_t)&journal};
        RootRegistration registration={.segment=(uintptr_t)&segment};RootLease root_lease={0};
        RootLifeRequest life={.registration=(uintptr_t)&registration,.lease=(uintptr_t)&root_lease,.context=(uintptr_t)&g.m.ctx};
        metadata_lock(&g.m);assert(lifecycle->life(&g.root_registry,&life,GC_RL_ATTACH)==GC_RL_OK);metadata_unlock(&g.m);
        RootRequest root={.segment=(uintptr_t)&segment};assert(roots(&g.m.ctx,&root,GC_ROOT_RESERVE)==0);root.value=g.queue.generation?g.objects[0].reference:g.old_owner;assert(roots(&g.m.ctx,&root,GC_ROOT_STORE)==0);
        metadata_lock(&g.m);assert(lifecycle->life(&g.root_registry,&life,GC_RL_ACQUIRE)==GC_RL_OK);metadata_unlock(&g.m);
        uint64_t root_value=0;RootScanJob rs={.scan={(uintptr_t)&segment,0,(uintptr_t)&root_value,1,0},.lease=(uintptr_t)&root_lease,.serial=life.serial};
        assert(lifecycle->leased(&rs)==0&&rs.scan.written==1);
        if(g.queue.generation){
            uint64_t canonical=UINT64_MAX;
            assert(graph_offer_try(&g,&g.m.ctx,root_value,false,&canonical)==GRAPH_ACCESS_BUSY);
            assert(!canonical&&!atomic_load(&g.m.ctx.access_epoch)&&!g.m.leases.active);
            assert(graph_queue_status(&g)==GC_MARK_EMPTY&&!g.records[0].old_epoch);
        }else graph_offer(&g,&g.m.ctx,root_value);
        metadata_lock(&g.m);assert(lifecycle->life(&g.root_registry,&life,GC_RL_RELEASE)==GC_RL_OK);metadata_unlock(&g.m);
        RootChange change;RootLease journal_lease={0};RootJournalRequest journal_job={0};
        graph_journal_claim(&g,&registration,&journal_lease,&journal_job,&change);
        if(g.queue.generation){
            RootJournalRequest receipt=journal_job;RootChange snapshot=change;
            uint64_t canonical=UINT64_MAX,consumed=atomic_load(&journal.consumed);
            assert(change.next==root_value&&!change.previous);
            assert(graph_offer_try(&g,&g.m.ctx,change.next,false,&canonical)==GRAPH_ACCESS_BUSY);
            assert(!canonical&&!g.m.leases.active&&!atomic_load(&g.m.ctx.access_epoch));
            graph_end_check_enter(&g,&owner);
            assert(g.mark(&g.queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
            assert(!g.records[0].young_epoch&&!g.records[0].old_epoch);
            assert(atomic_load(&cell.value)==root_value&&journal.top>consumed);
            assert(!memcmp(&journal_job,&receipt,sizeof(receipt))&&!memcmp(&change,&snapshot,sizeof(snapshot)));
            assert(atomic_load(&journal.consumed)==consumed&&!graph_inventory_end(&g,&audit));
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            store_phase=g.root_registry.phase;
            // All child fields were initialized before this release. The
            // owner's root and unacknowledged journal remain strong throughout.
            ObjectHeader *h=(ObjectHeader *)((char *)mapping+(root_value>>16)-sizeof(ObjectHeader));
            assert(atomic_load(&h->state)==GC_OBJECT_INITIALIZING);
            atomic_store_explicit(&h->state,GC_OBJECT_PUBLISHED,memory_order_release);
            graph_offer(&g,&g.m.ctx,root_value);
        }
        graph_journal_handoff(&g,&journal_job,&change);
        if(!g.queue.generation){
            // An actual registered Old root, scanned and journaled, must not
            // enqueue its owner or prematurely discover its unique Young edge.
            assert(graph_queue_status(&g)==GC_MARK_EMPTY&&!g.m.leases.active);
            assert(!g.records[6].young_epoch&&!g.records[7].young_epoch);
            assert(atomic_load(&g.old_skips)==2);
            root.value=g.objects[0].reference;
            assert(roots(&g.m.ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
            graph_journal_claim(&g,&registration,&journal_lease,&journal_job,&change);
            assert(change.previous==g.old_owner&&change.next==g.objects[0].reference);
            graph_journal_handoff(&g,&journal_job,&change);
            assert(atomic_load(&g.old_skips)==3&&!g.records[7].young_epoch);
        }
        uint64_t root_audit_revision=atomic_load_explicit(&g.m.ctx.root_revision,memory_order_acquire);
        graph_audit_roots(&g,&g.m.ctx,&owner,&audit);
        atomic_store_explicit(&g.marking_ready,1,memory_order_release);
        while(!atomic_load_explicit(&g.parked,memory_order_acquire))metadata_yield();
        assert(graph_queue_status(&g)==GC_MARK_BUSY); // queue empty, root job claimed
        for(;;){metadata_lock(&g.m);bool all=g.handshake.members==3&&g.handshake.native==1;metadata_unlock(&g.m);if(all)break;metadata_yield();}
        // An actual registry rendezvous occurs while a worker retains its
        // in-flight parent. No header pointer or unoffered child crosses it.
        metadata_lock(&g.m);g.root_registry.phase+=0x10000;store_phase=g.root_registry.phase;
        assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);metadata_unlock(&g.m);
        assert(graph_worker_call(&g,&owner,GC_WK_POLL)==GC_WK_PARK);
        for(;;){metadata_lock(&g.m);uint64_t result=lifecycle->handshake(&g.handshake,NULL,GC_HS_END);metadata_unlock(&g.m);
            if(result==GC_HS_OK)break;assert(result==GC_HS_INCOMPLETE);metadata_yield();}
        metadata_lock(&g.m);assert(g.handshake.native==1&&!g.handshake.active);metadata_unlock(&g.m);
        atomic_store_explicit(&g.native_gate,1,memory_order_release);
        graph_poll(&g,&owner);assert(atomic_load(&g.m.ctx.adopted_phase)==store_phase);

        ReferenceLease owner_lease={.reference=g.objects[0].reference};metadata_lock(&g.m);assert(reference_lease(&g.m.leases,&owner_lease)==GC_ML_OK);metadata_unlock(&g.m);
        MetadataLeaseRequest lease=owner_lease.lease;
        ReferenceLease access_lease={.reference=g.objects[0].reference};
        metadata_lock(&g.m);assert(reference_lease(&g.m.leases,&access_lease)==GC_ML_OK);metadata_unlock(&g.m);
        GraphAccess guard=graph_enter(&g,&g.m.ctx,&access_lease.lease,g.objects[0].reference);
        bool replace_scanned=round%8>=4;
        uint64_t satb_count=replace_scanned?2:1;
        BarrierEntry satb_entries[2];
        g.m.ctx.satb=(BufferCursor){.entries=(uintptr_t)satb_entries,.capacity=satb_count};
        GuardedStore request={.scope=(uintptr_t)&guard.request.scope,.page=(uintptr_t)g.page,.registry=(uintptr_t)&g.registry,
            .byte_offset=8,.expected=g.objects[2].reference,.desired=0,.phase=store_phase,
            .satb={.previous={g.objects[2].reference},.slot={.owner={g.objects[0].reference},.byte_offset=8,
                .page_incarnation=lease.incarnation,.record_epoch=store_phase,.metadata_lease=lease.token}}};
        assert(store(&request)==GC_STORE_OK&&request.transferred==1);
        graph_exit(&g,&guard);assert(metadata_call(&g.m,&access_lease.lease,GC_ML_RELEASE)==GC_ML_OK);
        assert(atomic_load(&array[1])==0);atomic_store_explicit(&g.start,1,memory_order_release);
        while(!atomic_load_explicit(&g.children_parked,memory_order_acquire))metadata_yield();
        if(replace_scanned){
            // A real logical deletion after the scanner's observation. Its
            // original referent remains rooted by the worker and logged here.
            // The later heal must not resurrect it, even after two child moves.
            ReferenceLease logged={.reference=g.objects[0].reference};
            access_lease=(ReferenceLease){.reference=g.objects[0].reference};
            metadata_lock(&g.m);
            assert(reference_lease(&g.m.leases,&logged)==GC_ML_OK);
            assert(reference_lease(&g.m.leases,&access_lease)==GC_ML_OK);
            metadata_unlock(&g.m);
            guard=graph_enter(&g,&g.m.ctx,&access_lease.lease,g.objects[0].reference);
            GuardedStore deletion={.scope=(uintptr_t)&guard.request.scope,.page=(uintptr_t)g.page,.registry=(uintptr_t)&g.registry,
                .expected=g.objects[1].reference,.phase=atomic_load(&g.m.ctx.adopted_phase),
                .satb={.previous={g.objects[1].reference},.slot={.owner={g.objects[0].reference},
                    .page_incarnation=logged.lease.incarnation,.record_epoch=atomic_load(&g.m.ctx.adopted_phase),.metadata_lease=logged.lease.token}}};
            assert(store(&deletion)==GC_STORE_OK&&deletion.transferred==1);
            graph_exit(&g,&guard);assert(metadata_call(&g.m,&access_lease.lease,GC_ML_RELEASE)==GC_ML_OK);
            assert(!atomic_load(&array[0]));
        }
        graph_end_check_enter(&g,&owner);
        assert(g.root_registry.active==3&&g.queue.in_flight&&!graph_inventory_end(&g,&audit));
        // Inspect a fixed eight-cell test slice, not a collector root traversal.
        RootSegment *pending_probe=(RootSegment *)(uintptr_t)atomic_load_explicit(&g.pending_segment,memory_order_acquire);
        RootCell *pending_values=(RootCell *)(uintptr_t)pending_probe->cells;
        BufferCursor *pending_buffer=(BufferCursor *)(uintptr_t)pending_probe->journal;
        assert(atomic_load(&pending_probe->used)==8&&atomic_load(&pending_values[0].value)==g.objects[1].reference);
        for(unsigned i=1;i<8;i++)assert(!atomic_load(&pending_values[i].value));
        assert(atomic_load(&pending_buffer->published)-atomic_load(&pending_buffer->consumed)==1);
        MetadataLeaseRequest pending_held[32]={0};unsigned pending_held_count=0;
        while(g.m.leases.free_head){
            assert(pending_held_count<32);
            pending_held[pending_held_count]=(MetadataLeaseRequest){(uintptr_t)g.page,g.page->incarnation,0};
            assert(metadata(&g.m.leases,&pending_held[pending_held_count++],GC_ML_ACQUIRE)==GC_ML_OK);
        }
        assert(g.m.leases.active==32);g.metadata_resume=0;
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        atomic_store_explicit(&g.children_resume,1,memory_order_release);
        while(!atomic_load_explicit(&g.offer_metadata_wait,memory_order_acquire))metadata_yield();
        graph_end_check_enter(&g,&owner);
        assert(g.handshake.native>=1&&g.m.leases.active==32&&g.queue.in_flight);
        assert(atomic_load(&pending_values[0].value)==g.objects[1].reference&&!graph_inventory_end(&g,&audit));
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        // The child has not yet reached the queue. Its pending root keeps it
        // strong while copying finishes and publication remains deliberately Busy.
        ForwardCell *child_source=&storage->cells[1];
        ForwardRequest child_claim={child_source->source_offset,child_source->incarnation,0,0};
        assert(graph_move_claim(&g,child_source,&child_claim)==GC_FWD_OK);
        SourceAccessRequest child_select={0,child_source->source_offset,child_source->incarnation,0,child_claim.ticket};
        assert(o->access(child_source,&child_select,GC_SA_SELECT)==GC_SA_OK);
        assert(o->access(child_source,&child_select,GC_SA_CHECK)==GC_SA_OK);
        RelocationCopy child_copy={(uintptr_t)mapping,child_source->incarnation,child_claim.ticket,g.page->limit,0,0,8,0,0};
        assert(graph_target(&g,array_page,&child_copy,GC_TARGET_PREPARE)==GC_TARGET_OK);
        assert(o->copy(child_source,&child_copy)==GC_COPY_DONE);
        assert(graph_target(&g,array_page,&child_copy,GC_TARGET_INSTALL)==GC_TARGET_OK);
        metadata_lock(&g.m);
        for(unsigned i=0;i<pending_held_count;i++)assert(metadata(&g.m.leases,&pending_held[i],GC_ML_RELEASE)==GC_ML_OK);
        g.metadata_resume=1;
#ifdef _WIN32
        WakeAllConditionVariable(&g.metadata_available);
#else
        assert(pthread_cond_broadcast(&g.metadata_available)==0);
#endif
        metadata_unlock(&g.m);
        while(!atomic_load_explicit(&g.offer_busy_wait,memory_order_acquire))metadata_yield();
        if(getenv("KLASSIC_GC_GRAPH_TRACE"))fprintf(stderr,"mark graph round %u offer busy\n",round);
        graph_end_check_enter(&g,&owner);
        assert(g.queue.in_flight&&!g.records[1].young_epoch&&!g.records[1].old_epoch);
        assert((atomic_load(&child_source->word)&3)==1&&atomic_load(&child_source->access)==GC_SOURCE_CLOSED);
        assert(atomic_load(&pending_values[0].value)==g.objects[1].reference&&!graph_inventory_end(&g,&audit));
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        assert(graph_move_publish(&g,child_source,&child_copy,g.objects[1].reference&UINT16_MAX)==GC_COPY_DONE);
        while(!atomic_load_explicit(&g.offer_advanced,memory_order_acquire))metadata_yield();
        graph_end_check_enter(&g,&owner);
        uint64_t canonical_child=(child_copy.destination<<16)|(g.objects[1].reference&UINT16_MAX);
        assert(atomic_load(&pending_values[0].value)==canonical_child);
        assert(!g.records[1].young_epoch&&!g.records[1].old_epoch&&g.queue.in_flight);
        assert(!array_page->metadata_leases&&!graph_inventory_end(&g,&audit));
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        // Move the just-adopted child again while its worker is outside every
        // access guard. The shared target run appends a second child copy;
        // the array is appended later, with its own forward-table entry.
        ForwardCell *intermediate_child=&storage->array_cells[0];
        ForwardRequest child_again_claim={intermediate_child->source_offset,intermediate_child->incarnation,0,0};
        assert(graph_move_claim(&g,intermediate_child,&child_again_claim)==GC_FWD_OK);
        SourceAccessRequest child_again_select={0,intermediate_child->source_offset,intermediate_child->incarnation,0,child_again_claim.ticket};
        assert(o->access(intermediate_child,&child_again_select,GC_SA_SELECT)==GC_SA_OK);
        assert(o->access(intermediate_child,&child_again_select,GC_SA_CHECK)==GC_SA_OK);
        RelocationCopy child_again={(uintptr_t)mapping,intermediate_child->incarnation,child_again_claim.ticket,array_page->limit,0,0,8,0,0};
        assert(graph_target(&g,array_page,&child_again,GC_TARGET_PREPARE)==GC_TARGET_OK);
        assert(o->copy(intermediate_child,&child_again)==GC_COPY_DONE);
        assert(graph_target(&g,array_page,&child_again,GC_TARGET_INSTALL)==GC_TARGET_OK);
        assert(graph_move_publish(&g,intermediate_child,&child_again,canonical_child&UINT16_MAX)==GC_COPY_DONE);
        atomic_store_explicit(&g.offer_advance_resume,1,memory_order_release);
        while(atomic_load_explicit(&g.offer_advanced,memory_order_acquire)<2)metadata_yield();
        graph_end_check_enter(&g,&owner);
        canonical_child=(child_again.destination<<16)|(canonical_child&UINT16_MAX);
        assert(atomic_load(&pending_values[0].value)==canonical_child);
        assert(!g.records[1].young_epoch&&!g.records[1].old_epoch&&g.queue.in_flight);
        assert(!array_page->metadata_leases&&!graph_inventory_end(&g,&audit));
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        atomic_store_explicit(&g.offer_advance_resume,2,memory_order_release);
        for(uint64_t step=1;step<=2;step++){
            while(atomic_load_explicit(&g.journal_steps,memory_order_acquire)<step)metadata_yield();
            graph_end_check_enter(&g,&owner);
            RootJournalRequest *copied=(RootJournalRequest *)(uintptr_t)atomic_load(&g.journal_job);
            RootLease *producer=(RootLease *)(uintptr_t)copied->lease;
            uint64_t *cursor=(uint64_t *)(uintptr_t)atomic_load(&g.journal_reference);
            uint64_t expected=((step==1?child_copy.destination:child_again.destination)<<16)|(canonical_child&UINT16_MAX);
            assert(*cursor==expected&&producer->kind==4&&producer->cursor==copied->end);
            assert(g.root_registry.leases&&g.queue.in_flight&&!graph_inventory_end(&g,&audit));
            MetadataLeaseRequest protection={.token=atomic_load(&g.journal_token)};
            assert(metadata(&g.m.leases,&protection,GC_ML_RESOLVE)==GC_ML_OK);
            assert(protection.page==(uintptr_t)array_page&&protection.incarnation==array_page->incarnation);
            // COPY advancement never consumes or overwrites the producer ring.
            RootChange *original=(RootChange *)(uintptr_t)pending_buffer->entries;
            assert(!atomic_load(&pending_buffer->consumed)&&original[0].next==g.objects[1].reference);
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            atomic_store_explicit(&g.journal_resume,step,memory_order_release);
            if(step==1){
                while(!atomic_load_explicit(&g.journal_destination_wait,memory_order_acquire))metadata_yield();
                graph_end_check_enter(&g,&owner);
                assert(*cursor==expected&&producer->kind==4&&g.handshake.native>=1);
                assert(atomic_load(&g.journal_steps)==1&&g.journal_pressure_injected);
                assert(g.m.leases.active>g.journal_held_count&&!graph_inventory_end(&g,&audit));
                assert(metadata(&g.m.leases,&protection,GC_ML_RESOLVE)==GC_ML_OK);
                assert(protection.token==atomic_load(&g.journal_token));
                graph_end_check_leave(&g,&owner,GC_MP_RETRY);
                metadata_lock(&g.m);
                for(unsigned i=0;i<g.journal_held_count;i++)assert(metadata(&g.m.leases,&g.journal_held[i],GC_ML_RELEASE)==GC_ML_OK);
                g.metadata_resume=1;
#ifdef _WIN32
                WakeAllConditionVariable(&g.metadata_available);
#else
                assert(pthread_cond_broadcast(&g.metadata_available)==0);
#endif
                metadata_unlock(&g.m);
            }
        }
        while(!atomic_load_explicit(&g.array_parked,memory_order_acquire))metadata_yield();
        if(getenv("KLASSIC_GC_GRAPH_TRACE"))fprintf(stderr,"mark graph round %u array parked\n",round);
        graph_end_check_enter(&g,&owner);
        assert(g.queue.in_flight&&!graph_inventory_end(&g,&audit));
        g.metadata_resume=0; // the array claimant injects pressure at acquisition
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        // The worker must repair the twice-moved child or preserve the real
        // intervening deletion before yielding its partial array claim. A stale
        // coordinator CAS must preserve either exact result.
        ReferenceLease repair_lease={.reference=g.objects[0].reference};
        metadata_lock(&g.m);assert(reference_lease(&g.m.leases,&repair_lease)==GC_ML_OK);metadata_unlock(&g.m);
        GraphAccess repair_guard=graph_enter(&g,&g.m.ctx,&repair_lease.lease,g.objects[0].reference);
        GuardedHeal repair={.scope=(uintptr_t)&repair_guard.request.scope,.page=(uintptr_t)repair_guard.page,
            .registry=(uintptr_t)&g.registry,.expected=g.objects[1].reference,
            .desired=(canonical_child&~UINT64_C(0xffff))|(gc_abi_color((PhaseSnapshot){atomic_load(&g.m.ctx.adopted_phase)})&~0x30u)|(g.objects[1].reference&0x30u),
            .phase=atomic_load(&g.m.ctx.adopted_phase)};
        uint64_t worker_value=atomic_load(&array[0]);
        assert(atomic_load(&g.worker_conflicts)==replace_scanned);
        if(replace_scanned)assert(!worker_value);
        else {
            assert(atomic_load(&g.worker_repairs)&&worker_value>>16==canonical_child>>16);
            assert((worker_value&0x30)==(g.objects[1].reference&0x30));
        }
        assert(heal(&repair)==GC_HEAL_CHANGED&&atomic_load(&array[0])==worker_value);
        graph_exit(&g,&repair_guard);assert(metadata_call(&g.m,&repair_lease.lease,GC_ML_RELEASE)==GC_ML_OK);
        // Move the claimed array after exactly eight examined slots. Its queue
        // obligation still owns the old reference, token and nonzero cursor.
        ForwardCell *array_source=&storage->cells[0];
        ForwardRequest array_claim={array_source->source_offset,array_source->incarnation,0,0};
        assert(graph_move_claim(&g,array_source,&array_claim)==GC_FWD_OK);
        SourceAccessRequest array_select={0,array_source->source_offset,array_source->incarnation,0,array_claim.ticket};
        assert(o->access(array_source,&array_select,GC_SA_SELECT)==GC_SA_OK);
        assert(o->access(array_source,&array_select,GC_SA_CHECK)==GC_SA_OK);
        RelocationCopy array_copy={(uintptr_t)mapping,array_source->incarnation,array_claim.ticket,g.page->limit,0,0,64,0,0};
        assert(graph_target(&g,array_page,&array_copy,GC_TARGET_PREPARE)==GC_TARGET_OK);
        uint64_t array_copied;do{array_copied=o->copy(array_source,&array_copy);}while(array_copied==GC_COPY_MORE);
        assert(array_copied==GC_COPY_DONE&&array_copy.cursor==array_length*8);
        assert(atomic_load((_Atomic uint64_t *)((uintptr_t)mapping+array_copy.destination))==worker_value);
        assert(graph_target(&g,array_page,&array_copy,GC_TARGET_INSTALL)==GC_TARGET_OK);
        // Delay forwarding publication until the consumer has polled on BUSY.
        atomic_store_explicit(&g.array_resume,1,memory_order_release);
        while(!atomic_load_explicit(&g.array_metadata_wait,memory_order_acquire))metadata_yield();
        if(getenv("KLASSIC_GC_GRAPH_TRACE"))fprintf(stderr,"mark graph round %u scan full\n",round);
        graph_end_check_enter(&g,&owner);
        assert(g.handshake.native>=1&&g.queue.in_flight&&g.scan_pressure_injected);
        assert(g.scan_held_count&&g.m.leases.active>=g.scan_held_count);
        assert(!graph_inventory_end(&g,&audit));
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        metadata_lock(&g.m);
        for(unsigned i=0;i<g.scan_held_count;i++)assert(metadata(&g.m.leases,&g.scan_held[i],GC_ML_RELEASE)==GC_ML_OK);
        assert(g.m.leases.free_head);g.metadata_resume=1;
#ifdef _WIN32
        WakeAllConditionVariable(&g.metadata_available);
#else
        assert(pthread_cond_broadcast(&g.metadata_available)==0);
#endif
        metadata_unlock(&g.m);
        while(!atomic_load_explicit(&g.scan_busy_wait,memory_order_acquire))metadata_yield();
        if(getenv("KLASSIC_GC_GRAPH_TRACE"))fprintf(stderr,"mark graph round %u scan busy\n",round);
        graph_end_check_enter(&g,&owner);
        assert(g.queue.in_flight&&!graph_inventory_end(&g,&audit));
        assert((atomic_load(&array_source->word)&3)==1&&atomic_load(&array_source->access)==GC_SOURCE_CLOSED);
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        assert(graph_move_publish(&g,array_source,&array_copy,g.objects[0].reference&UINT16_MAX)==GC_COPY_DONE);
        // Large arrays exclusively occupy the first source page. Other graph
        // objects share the tail page, which remains mapped for their scans.
        if(run_bytes==2*GC_HEAP_PAGE_BYTES)pool_vm_decommit((char *)mapping+g.page->start,GC_HEAP_PAGE_BYTES);
        if(g.queue.capacity==1){
            while(!atomic_load_explicit(&g.destination_wait,memory_order_acquire))metadata_yield();
            graph_end_check_enter(&g,&owner);
            // Both the original queue token and the separate resumable
            // access cursor remain protected during destination pressure.
            assert(g.handshake.native>=1&&g.queue.in_flight==1&&g.m.leases.active==32);
            assert(g.destination_injected&&g.destination_held_count&&!graph_inventory_end(&g,&audit));
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            metadata_lock(&g.m);
            for(unsigned i=0;i<g.destination_held_count;i++)assert(metadata(&g.m.leases,&g.destination_held[i],GC_ML_RELEASE)==GC_ML_OK);
            g.metadata_resume=1;
#ifdef _WIN32
            WakeAllConditionVariable(&g.metadata_available);
#else
            assert(pthread_cond_broadcast(&g.metadata_available)==0);
#endif
            metadata_unlock(&g.m);
        }
        uint64_t scan_hops=run_bytes==GC_HEAP_PAGE_BYTES?2:1;
        uint64_t array_destination=array_copy.destination;
        for(uint64_t step=1;step<=scan_hops;step++){
            while(atomic_load_explicit(&g.scan_steps,memory_order_acquire)<step)metadata_yield();
            graph_end_check_enter(&g,&owner);
            ReferenceLease *cursor=(ReferenceLease *)(uintptr_t)atomic_load(&g.scan_cursor);
            MarkRequest *claim=(MarkRequest *)(uintptr_t)atomic_load(&g.scan_job);
            assert(cursor->reference==((array_destination<<16)|(g.objects[0].reference&UINT16_MAX)));
            assert(claim->reference==g.objects[0].reference&&claim->incarnation==g.page->incarnation&&claim->cursor==64);
            MarkTask *task=&g.tasks[claim->index-1];
            assert(task->state==2&&task->reference==claim->reference&&task->serial==claim->serial);
            assert(task->metadata_lease==claim->metadata_lease&&cursor->lease.token!=claim->metadata_lease);
            MetadataLeaseRequest held={.token=cursor->lease.token};
            assert(metadata(&g.m.leases,&held,GC_ML_RESOLVE)==GC_ML_OK&&held.page==(uintptr_t)array_page);
            held.token=claim->metadata_lease;
            assert(metadata(&g.m.leases,&held,GC_ML_RESOLVE)==GC_ML_OK&&held.page==(uintptr_t)g.page);
            assert(!atomic_load(&g.mark_retargets)&&g.queue.in_flight&&!graph_inventory_end(&g,&audit));
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            if(step<scan_hops){
                ForwardCell *intermediate_array=&storage->array_cells[2];
                ForwardRequest claim_again={intermediate_array->source_offset,intermediate_array->incarnation,0,0};
                assert(graph_move_claim(&g,intermediate_array,&claim_again)==GC_FWD_OK);
                SourceAccessRequest select_again={0,intermediate_array->source_offset,intermediate_array->incarnation,0,claim_again.ticket};
                assert(o->access(intermediate_array,&select_again,GC_SA_SELECT)==GC_SA_OK);
                assert(o->access(intermediate_array,&select_again,GC_SA_CHECK)==GC_SA_OK);
                RelocationCopy copy_again={(uintptr_t)mapping,intermediate_array->incarnation,claim_again.ticket,array_page->limit,0,0,64,0,0};
                assert(graph_target(&g,array_page,&copy_again,GC_TARGET_PREPARE)==GC_TARGET_OK);
                uint64_t copied;do{copied=o->copy(intermediate_array,&copy_again);}while(copied==GC_COPY_MORE);
                assert(copied==GC_COPY_DONE&&graph_target(&g,array_page,&copy_again,GC_TARGET_INSTALL)==GC_TARGET_OK);
                assert(atomic_load((_Atomic uint64_t *)((uintptr_t)mapping+copy_again.destination))==worker_value);
                assert(graph_move_publish(&g,intermediate_array,&copy_again,g.objects[0].reference&UINT16_MAX)==GC_COPY_DONE);
                array_destination=copy_again.destination;
            }
            atomic_store_explicit(&g.scan_resume,step,memory_order_release);
        }
        while(graph_queue_status(&g)!=GC_MARK_EMPTY)metadata_yield();
        assert(atomic_load(&g.scan_steps)==scan_hops);
        assert(atomic_load(&g.destination_wait)==(g.queue.capacity==1));
        assert(atomic_load(&g.array_metadata_wait)==1);
        assert(atomic_load(&g.mark_retargets)==1&&!array_page->metadata_leases);
        metadata_lock(&g.m);assert((g.queue.generation?g.records[2].old_epoch:g.records[2].young_epoch)==0);assert(g.page->metadata_leases==satb_count);metadata_unlock(&g.m);
        // Published but unconsumed SATB still prevents global completion.
        assert(g.m.ctx.satb.top==satb_count&&atomic_load(&g.m.ctx.satb.published)==satb_count&&!atomic_load(&g.m.ctx.satb.consumed));
        // The registered root still names the protected original array.
        // Audit while mutators run, then require exact physical repair without
        // a logical root revision or extra journal publication.
        assert((atomic_load(&cell.value)>>16)==(g.objects[0].reference>>16));
        uint64_t before_heal_revision=atomic_load(&g.m.ctx.root_revision);
        uint64_t before_heal_publication=atomic_load(&journal.published);
        // Stop the scanner first at source lease acquisition, then at the
        // exact destination acquisition. Both waits retain the captured root.
        MetadataLeaseRequest audit_source_held[32]={0};unsigned audit_source_count=0;
        metadata_lock(&g.m);
        while(g.m.leases.free_head){
            MetadataLeaseRequest *held=&audit_source_held[audit_source_count++];
            *held=(MetadataLeaseRequest){(uintptr_t)g.page,g.page->incarnation,0};
            assert(metadata(&g.m.leases,held,GC_ML_ACQUIRE)==GC_ML_OK);
        }
        assert(audit_source_count&&g.m.leases.active==32);g.metadata_resume=0;g.audit_pressure=true;
        metadata_unlock(&g.m);
        bool delete_audited_root=round%4>=2;
        GraphAuditWorker auditor={.graph=&g,.audit=&audit};MetadataThread audit_thread;
#ifdef _WIN32
        audit_thread=CreateThread(NULL,0,graph_audit_worker,&auditor,0,NULL);assert(audit_thread);
#else
        assert(pthread_create(&audit_thread,NULL,graph_audit_worker,&auditor)==0);
#endif
        for(unsigned stage=0;stage<2;stage++){
            _Atomic uint64_t *waits=stage?&g.audit_destination_wait:&g.audit_source_wait;
            while(!atomic_load_explicit(waits,memory_order_acquire))metadata_yield();
            if(stage&&delete_audited_root){
                // The actual owner deletes the root while the foreign scanner
                // retains its snapshot and current source token in native wait.
                root.value=0;assert(roots(&g.m.ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
                assert(!atomic_load(&cell.value));
            }
            graph_end_check_enter(&g,&owner);
            assert(g.handshake.members==4&&g.handshake.native>=1&&g.m.leases.active==32);
            assert(atomic_load(&auditor.context.state)==GC_BLOCKED_NATIVE&&!atomic_load(&auditor.context.access_epoch));
            RootWalk held=audit.walk;
            assert(held.active==1&&held.lease.kind==1&&held.pending==1&&held.cursor==1&&held.limit==1);
            assert(held.lease.registration==(uintptr_t)&registration&&held.pending_index==0);
            assert(held.pending_observed==g.objects[0].reference&&held.pending_reference==held.pending_observed);
            assert(g.root_registry.walkers==1&&g.root_registry.leases>=1&&!graph_inventory_end(&g,&audit));
            if(stage){
                MetadataLeaseRequest cursor={.token=atomic_load(&g.audit_token)};
                assert(cursor.token&&metadata(&g.m.leases,&cursor,GC_ML_RESOLVE)==GC_ML_OK);
                assert(cursor.page==(uintptr_t)g.page&&cursor.incarnation==g.page->incarnation);
            }else assert(!atomic_load(&g.audit_token));
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            // The native waiter cannot mutate its snapshot until notified.
            metadata_lock(&g.m);assert(!memcmp(&held,&audit.walk,sizeof(held)));
            MetadataLeaseRequest *held_leases=stage?g.audit_held:audit_source_held;
            unsigned count=stage?g.audit_held_count:audit_source_count;
            assert(count);
            for(unsigned i=0;i<count;i++)assert(metadata(&g.m.leases,&held_leases[i],GC_ML_RELEASE)==GC_ML_OK);
            g.metadata_resume=1;
#ifdef _WIN32
            WakeAllConditionVariable(&g.metadata_available);
#else
            assert(pthread_cond_broadcast(&g.metadata_available)==0);
#endif
            metadata_unlock(&g.m);
        }
#ifdef _WIN32
        assert(WaitForSingleObject(audit_thread,30000)==WAIT_OBJECT_0);assert(CloseHandle(audit_thread));
#else
        assert(pthread_join(audit_thread,NULL)==0);
#endif
        metadata_lock(&g.m);
        assert(g.handshake.members==3&&!auditor.participant.registered&&!audit.walk.pending);
        assert(!g.root_registry.walkers&&!g.root_registry.leases&&g.audit_destination_injected);
        assert(atomic_load(&g.audit_source_wait)==1&&atomic_load(&g.audit_destination_wait)==1);
        g.audit_pressure=false;metadata_unlock(&g.m);
        if(delete_audited_root){
            assert(!atomic_load(&cell.value)&&g.root_conflicts==1);
            assert(atomic_load(&g.m.ctx.root_revision)==before_heal_revision+1);
            assert(atomic_load(&journal.published)==before_heal_publication+1);
            assert(atomic_load(&journal.consumed)==atomic_load(&journal.published));
            // Only this explicit owner publication may restore the root. The
            // fixture still protects the original source reference/metadata.
            root.value=g.objects[0].reference;assert(roots(&g.m.ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
            before_heal_revision+=2;before_heal_publication+=2;
            graph_audit_roots(&g,&g.m.ctx,&owner,&audit);
        }else assert(!g.root_conflicts);
        assert((atomic_load(&cell.value)>>16)==array_destination);
        assert(g.audit_hops>=scan_hops);
        assert((atomic_load(&cell.value)&UINT16_MAX)==(g.objects[0].reference&UINT16_MAX));
        assert(atomic_load(&g.m.ctx.root_revision)==before_heal_revision);
        assert(atomic_load(&journal.published)==before_heal_publication);
        // The old empty-journal audit becomes stale after another root write.
        // The new referent is still protected by the unconsumed SATB entry.
        // Even null stores must invalidate an audit (odd rounds delete root).
        root.value=round%2?0:g.objects[2].reference;
        assert(roots(&g.m.ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
        graph_end_check_enter(&g,&owner);
        assert(g.mark(&g.queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
        assert(atomic_load(&g.m.ctx.root_revision)!=root_audit_revision);
        assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_CHECK)==GC_RA_DONE);
        assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_OWNER)==GC_RA_STALE);
        assert(!graph_inventory_end(&g,&audit));
        assert(atomic_load(&journal.published)!=atomic_load(&journal.consumed));
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        // Resume first, then consume the bounded journal and commit both sides
        // to the work queue. The audit revision is sampled BEFORE that work.
        root_audit_revision=atomic_load_explicit(&g.m.ctx.root_revision,memory_order_acquire);
        graph_journal_claim(&g,&registration,&journal_lease,&journal_job,&change);
        graph_end_check_enter(&g,&owner);
        assert(g.mark(&g.queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
        assert(atomic_load(&g.m.ctx.root_revision)==root_audit_revision);
        assert(g.root_registry.leases==1&&registration.leases==1&&journal_lease.kind==4);
        assert(atomic_load(&journal.consumed)==journal_job.begin);
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        graph_journal_handoff(&g,&journal_job,&change);
        graph_audit_roots(&g,&g.m.ctx,&owner,&audit);
        // A newly attached empty segment has not changed root_revision, but
        // it changes the inventory boundary and must invalidate the audit.
        RootCell extra_cell={0};RootChange extra_change;
        BufferCursor extra_journal={.entries=(uintptr_t)&extra_change,.capacity=1};
        RootSegment extra_segment={.owner=g.m.ctx.thread_id,.cells=(uintptr_t)&extra_cell,.capacity=1,.journal=(uintptr_t)&extra_journal};
        RootRegistration extra_registration={.segment=(uintptr_t)&extra_segment};RootLease extra_lease={0};
        RootLifeRequest extra_life={.registration=(uintptr_t)&extra_registration,.lease=(uintptr_t)&extra_lease,.context=(uintptr_t)&g.m.ctx};
        metadata_lock(&g.m);assert(lifecycle->life(&g.root_registry,&extra_life,GC_RL_ATTACH)==GC_RL_OK);metadata_unlock(&g.m);
        graph_end_check_enter(&g,&owner);
        assert(atomic_load(&g.m.ctx.root_revision)==root_audit_revision);
        assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_CHECK)==GC_RA_STALE);
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);graph_audit_roots(&g,&g.m.ctx,&owner,&audit);
        metadata_lock(&g.m);
        assert(lifecycle->life(&g.root_registry,&extra_life,GC_RL_RETIRE)==GC_RL_OK);
        assert(lifecycle->life(&g.root_registry,&extra_life,GC_RL_CLEAN)==GC_RL_OK);metadata_unlock(&g.m);
        assert(lifecycle->clear(&extra_life)==GC_RL_OK);
        metadata_lock(&g.m);assert(lifecycle->life(&g.root_registry,&extra_life,GC_RL_FINISH)==GC_RL_OK);metadata_unlock(&g.m);
        BarrierEntry out[2];BarrierJob buffer_job={.identity=g.m.ctx.thread_id,.output=(uintptr_t)out,.capacity=2};
        metadata_lock(&g.m);assert(lifecycle->buffer_job(&g.m.ctx,&buffer_job,GC_BJ_CLAIM)==GC_BJ_OK);metadata_unlock(&g.m);
        assert(lifecycle->buffer_job(&g.m.ctx,&buffer_job,GC_BJ_COPY)==GC_BJ_OK&&buffer_job.written==satb_count);
        while(graph_queue_status(&g)!=GC_MARK_EMPTY)metadata_yield();
        graph_end_check_enter(&g,&owner);
        assert(g.mark(&g.queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
        assert(g.m.ctx.satb_consumer.serial==buffer_job.serial&&!atomic_load(&g.m.ctx.satb.consumed));
        assert(atomic_load(&g.m.ctx.root_revision)==root_audit_revision&&!g.root_registry.leases);
        assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_OWNER)==GC_RA_DONE);
        assert(!graph_inventory_end(&g,&audit));
        graph_end_check_leave(&g,&owner,GC_MP_RETRY);
        assert(out[0].previous.bits==g.objects[2].reference&&out[0].slot.byte_offset==8);
        if(replace_scanned)assert(out[1].previous.bits==g.objects[1].reference&&!out[1].slot.byte_offset);
        for(uint64_t i=0;i<buffer_job.written;i++){
            graph_offer(&g,&g.m.ctx,out[i].previous.bits);
            lease=(MetadataLeaseRequest){.token=out[i].slot.metadata_lease};assert(metadata_call(&g.m,&lease,GC_ML_RESOLVE)==0);assert(metadata_call(&g.m,&lease,GC_ML_RELEASE)==0);
        }
        metadata_lock(&g.m);assert(lifecycle->buffer_job(&g.m.ctx,&buffer_job,GC_BJ_ACK)==GC_BJ_OK);metadata_unlock(&g.m);
        while(graph_queue_status(&g)!=GC_MARK_EMPTY)metadata_yield();
        if(!g.queue.generation){
            // First reject pending work, then reject a claimed slot with no
            // mark tasks. Both decisions use the real registered-worker phase.
            graph_end_check_enter(&g,&owner);
            assert(!g.records[7].young_epoch&&g.remembered.pending==1&&!g.remembered.claims);
            assert(!graph_inventory_end(&g,&audit));graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            atomic_store_explicit(&g.rem_ready,1,memory_order_release);
            while(!atomic_load_explicit(&g.rem_walk_parked,memory_order_acquire))metadata_yield();
            graph_end_check_enter(&g,&owner);
            assert(g.remembered.walkers&&!g.remembered.unflipped&&!g.remembered.claims);
            assert(!g.empty_set.pending&&!g.empty_set.current_count&&!g.empty_set.active_serial);
            assert(remembered->inventory(&g.remembered,&empty_request,GC_RI_DETACH)==GC_RS_WAIT);
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            atomic_store_explicit(&g.rem_walk_resume,1,memory_order_release);
            while(!atomic_load_explicit(&g.rem_claimed,memory_order_acquire))metadata_yield();
            for(;;){metadata_lock(&g.m);bool finished=!g.remembered.walkers;metadata_unlock(&g.m);if(finished)break;metadata_yield();}
            graph_end_check_enter(&g,&owner);
            assert(g.mark(&g.queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
            assert(!g.records[7].young_epoch&&!g.remembered.pending&&g.remembered.claims==1&&!g.remembered.unflipped);
            assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_CHECK)==GC_RA_DONE);
            assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_OWNER)==GC_RA_DONE);
            // Replay a record captured before this flip AFTER the slot was
            // taken. It must survive the first ACK and require another pass.
            RememberedJob late={.offset=g.rem_set.active_offset,.incarnation=g.rem_set.incarnation,
                .phase=g.remembered.previous_epoch};
            RememberedInventoryRequest late_request={(uintptr_t)&g.rem_registration,(uintptr_t)&late,0};
            assert(remembered->inventory(&g.remembered,&late_request,GC_RI_RECORD)==GC_RS_OK);
            assert(g.remembered.pending==1&&g.remembered.claims==1&&g.rem_set.current_count==1);
            // Exhaust the real pool with independently owned tokens, without
            // changing capacity/accounting by hand. Only this owner releases
            // these obligations, after the consumer has blocked on FULL.
            MetadataLeaseRequest held_metadata[32]={0};assert(!g.m.leases.active);g.metadata_resume=0;
            for(unsigned i=0;i<32;i++){
                held_metadata[i]=(MetadataLeaseRequest){(uintptr_t)g.old_page,g.old_page->incarnation,0};
                assert(metadata(&g.m.leases,&held_metadata[i],GC_ML_ACQUIRE)==GC_ML_OK);
            }
            assert(g.m.leases.active==32&&!g.m.leases.free_head);
            assert(!graph_inventory_end(&g,&audit));graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            g.m.ctx.remembered=(BufferCursor){.entries=(uintptr_t)&delayed_storage,.capacity=1};
            delayed_record.slot.metadata_lease=held_metadata[0].token;
            assert(remembered->buffer(&g.m.ctx,&delayed_record,GC_BB_APPEND_REMEMBERED)==GC_BB_OK);
            held_metadata[0]=(MetadataLeaseRequest){0}; // obligation belongs to the ring
            assert(remembered->buffer(&g.m.ctx,NULL,GC_BB_PUBLISH_REMEMBERED)==GC_BB_OK);
            atomic_store_explicit(&g.rem_resume,1,memory_order_release);
            while(!atomic_load_explicit(&g.rem_metadata_wait,memory_order_acquire))metadata_yield();
            graph_end_check_enter(&g,&owner);
            assert(g.handshake.native>=1&&g.remembered.claims==1&&g.remembered.pending==1);
            assert(g.m.leases.active==32&&g.old_page->metadata_leases==32);
            assert(g.mark(&g.queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY&&!g.records[7].young_epoch);
            assert(!graph_inventory_end(&g,&audit));graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            // The consumer is genuinely blocked with its unique claim and no
            // access guard. Metadata leases do not prohibit relocation. Move
            // the Old owner and revoke the source mapping before waking it.
            ForwardCell *moving=&sources[0]->cell;
            ForwardRequest move_claim={moving->source_offset,moving->incarnation,0,0};
            assert(graph_move_claim(&g,moving,&move_claim)==GC_FWD_OK);
            SourceAccessRequest moving_scope={0,moving->source_offset,moving->incarnation,0,move_claim.ticket};
            assert(o->access(moving,&moving_scope,GC_SA_SELECT)==GC_SA_OK);
            assert(o->access(moving,&moving_scope,GC_SA_CHECK)==GC_SA_OK);
            RelocationCopy moved={(uintptr_t)mapping,moving->incarnation,move_claim.ticket,g.old_page->limit,0,0,1,0,0};
            assert(graph_target(&g,middle_page,&moved,GC_TARGET_PREPARE)==GC_TARGET_OK);
            uint64_t copied;do{copied=o->copy(moving,&moved);}while(copied==GC_COPY_MORE);
            assert(copied==GC_COPY_DONE);
            assert(graph_target(&g,middle_page,&moved,GC_TARGET_INSTALL)==GC_TARGET_OK);
            SourceDrain source_ready={g.old_page->incarnation,atomic_load(&g.old_page->allocated_top),atomic_load(&sources[0]->table.used),0,1,0};
            assert(graph_source_drain(&g,drain,g.old_page,&source_ready)==GC_SD_WAIT&&!source_ready.cursor&&!source_ready.examined);
            assert(graph_move_publish(&g,moving,&moved,g.old_owner&UINT16_MAX)==GC_COPY_DONE);
            assert(graph_source_drain(&g,drain,g.old_page,&source_ready)==GC_SD_DONE&&source_ready.cursor==1&&source_ready.examined==1);
            g.moved_owner=(moved.destination<<16)|(g.old_owner&UINT16_MAX);
            graph_uncommit_payload(&g,uncommit,g.old_page);
            assert(atomic_load(&g.old_page->state)==GC_PAGE_UNCOMMITTED);
            RelocationCopy rejected_first={.heap_base=(uintptr_t)mapping};
            assert(graph_target(&g,g.old_page,&rejected_first,GC_TARGET_PREPARE)==GC_TARGET_INVALID);
            assert(!rejected_first.destination);
            metadata_lock(&g.m);
            assert(g.remembered.claims==1&&g.old_page->metadata_leases==32);
            for(unsigned i=1;i<32;i++)assert(metadata(&g.m.leases,&held_metadata[i],GC_ML_RELEASE)==GC_ML_OK);
            assert(g.m.leases.active==1&&g.old_page->metadata_leases==1);
            metadata_unlock(&g.m);
            // Publish and delete the ORIGINAL reference after its source is
            // inaccessible. Real journal obligations protect both offers;
            // routing must inspect the canonical Old header, then skip it.
            RootRequest saved=root;saved.value=0;
            assert(roots(&g.m.ctx,&saved,GC_ROOT_READ_RAW)==GC_ROOT_OK);
            root.value=g.old_owner;assert(roots(&g.m.ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
            graph_journal_claim(&g,&registration,&journal_lease,&journal_job,&change);
            assert(change.next==g.old_owner);graph_journal_handoff(&g,&journal_job,&change);
            root.value=saved.value;assert(roots(&g.m.ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
            graph_journal_claim(&g,&registration,&journal_lease,&journal_job,&change);
            assert(change.previous==g.old_owner);graph_journal_handoff(&g,&journal_job,&change);
            root_audit_revision=atomic_load_explicit(&g.m.ctx.root_revision,memory_order_acquire);
            assert(atomic_load(&g.old_skips)==5&&atomic_load(&g.owner_remaps)==2);
            assert(graph_queue_status(&g)==GC_MARK_EMPTY&&!g.records[7].young_epoch);
            metadata_lock(&g.m);
            assert(g.m.leases.active==1);
            g.metadata_resume=1;
#ifdef _WIN32
            WakeAllConditionVariable(&g.metadata_available);
#else
            assert(pthread_cond_broadcast(&g.metadata_available)==0);
#endif
            metadata_unlock(&g.m);
            // Force a real phase rendezvous after the rooted forwarding hop,
            // before the worker may acquire the canonical payload guard.
            while(!atomic_load_explicit(&g.rem_access_advanced,memory_order_acquire))metadata_yield();
            graph_end_check_enter(&g,&owner);
            assert(g.remembered.claims==1&&g.root_registry.active==4&&g.m.leases.active==1);
            assert(!g.records[7].young_epoch&&!graph_inventory_end(&g,&audit));
            // Exhaust the pool again, this time AFTER owner publication and
            // its first rooted hop. The buffered source keeps one token.
            for(unsigned i=1;i<32;i++){
                held_metadata[i]=(MetadataLeaseRequest){(uintptr_t)middle_page,middle_page->incarnation,0};
                assert(metadata(&g.m.leases,&held_metadata[i],GC_ML_ACQUIRE)==GC_ML_OK);
            }
            assert(g.m.leases.active==32&&middle_page->metadata_leases==31);
            g.metadata_resume=0;
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            atomic_store_explicit(&g.rem_access_resume,1,memory_order_release);
            while(!atomic_load_explicit(&g.rem_access_metadata_wait,memory_order_acquire))metadata_yield();
            graph_end_check_enter(&g,&owner);
            assert(g.handshake.native>=1&&g.remembered.claims==1&&g.root_registry.active==4);
            assert(g.m.leases.active==32&&!g.records[7].young_epoch&&!graph_inventory_end(&g,&audit));
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            // The worker now holds the intermediate reference in a live root,
            // without a guard. Move again before admitting its next access.
            moving=&sources[1]->cell;
            move_claim=(ForwardRequest){moving->source_offset,moving->incarnation,0,0};
            assert(graph_move_claim(&g,moving,&move_claim)==GC_FWD_OK);
            moving_scope=(SourceAccessRequest){0,moving->source_offset,moving->incarnation,0,move_claim.ticket};
            assert(o->access(moving,&moving_scope,GC_SA_SELECT)==GC_SA_OK);
            assert(o->access(moving,&moving_scope,GC_SA_CHECK)==GC_SA_OK);
            moved=(RelocationCopy){(uintptr_t)mapping,moving->incarnation,move_claim.ticket,middle_page->limit,0,0,1,0,0};
            assert(graph_target(&g,empty_page,&moved,GC_TARGET_PREPARE)==GC_TARGET_OK);
            do{copied=o->copy(moving,&moved);}while(copied==GC_COPY_MORE);
            assert(copied==GC_COPY_DONE&&graph_target(&g,empty_page,&moved,GC_TARGET_INSTALL)==GC_TARGET_OK);
            source_ready=(SourceDrain){middle_page->incarnation,atomic_load(&middle_page->allocated_top),atomic_load(&sources[1]->table.used),0,1,0};
            assert(graph_source_drain(&g,drain,middle_page,&source_ready)==GC_SD_WAIT&&!source_ready.cursor&&!source_ready.examined);
            assert(graph_move_publish(&g,moving,&moved,g.old_owner&UINT16_MAX)==GC_COPY_DONE);
            assert(graph_source_drain(&g,drain,middle_page,&source_ready)==GC_SD_DONE&&source_ready.cursor==1&&source_ready.examined==1);
            g.moved_owner=(moved.destination<<16)|(g.old_owner&UINT16_MAX);
            graph_uncommit_payload(&g,uncommit,middle_page);
            assert(atomic_load(&middle_page->state)==GC_PAGE_UNCOMMITTED);
            RelocationCopy rejected_second={.heap_base=(uintptr_t)mapping};
            assert(graph_target(&g,middle_page,&rejected_second,GC_TARGET_PREPARE)==GC_TARGET_INVALID);
            assert(!rejected_second.destination);
            metadata_lock(&g.m);
            for(unsigned i=1;i<32;i++)assert(metadata(&g.m.leases,&held_metadata[i],GC_ML_RELEASE)==GC_ML_OK);
            assert(g.m.leases.active==1&&!middle_page->metadata_leases);
            g.metadata_resume=1;
#ifdef _WIN32
            WakeAllConditionVariable(&g.metadata_available);
#else
            assert(pthread_cond_broadcast(&g.metadata_available)==0);
#endif
            metadata_unlock(&g.m);
            while(!atomic_load_explicit(&g.rem_loaded,memory_order_acquire))metadata_yield();
            graph_end_check_enter(&g,&owner);
            assert(g.mark(&g.queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY&&!g.records[7].young_epoch);
            assert(g.remembered.claims==1&&g.root_registry.active==4&&g.m.leases.active==1);
            assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_CHECK)==GC_RA_STALE);
            assert(!graph_inventory_end(&g,&audit));
            // The loaded Young child is rooted but has not reached the queue.
            // No other marker owns a token here: the only existing lease is
            // the buffered Old source record. Exhaust before releasing its gate.
            uint64_t offer_waits=atomic_load(&g.offer_metadata_wait);
            for(unsigned i=1;i<32;i++){
                held_metadata[i]=(MetadataLeaseRequest){(uintptr_t)g.page,g.page->incarnation,0};
                assert(metadata(&g.m.leases,&held_metadata[i],GC_ML_ACQUIRE)==GC_ML_OK);
            }
            assert(g.m.leases.active==32);g.metadata_resume=0;
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            atomic_store_explicit(&g.rem_continue,1,memory_order_release);
            while(atomic_load_explicit(&g.offer_metadata_wait,memory_order_acquire)==offer_waits)metadata_yield();
            graph_end_check_enter(&g,&owner);
            RootSegment *loaded_segment=(RootSegment *)(uintptr_t)atomic_load_explicit(&g.rem_output_segment,memory_order_acquire);
            RootCell *loaded_cells=(RootCell *)(uintptr_t)loaded_segment->cells;
            assert(atomic_load(&loaded_cells[0].value)==g.objects[7].reference);
            assert(g.handshake.native>=1&&g.remembered.claims==1&&!atomic_load(&g.rem_done));
            assert(g.m.leases.active==32&&!g.records[7].young_epoch&&!graph_inventory_end(&g,&audit));
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            metadata_lock(&g.m);
            for(unsigned i=1;i<32;i++)assert(metadata(&g.m.leases,&held_metadata[i],GC_ML_RELEASE)==GC_ML_OK);
            g.metadata_resume=1;
#ifdef _WIN32
            WakeAllConditionVariable(&g.metadata_available);
#else
            assert(pthread_cond_broadcast(&g.metadata_available)==0);
#endif
            metadata_unlock(&g.m);
            while(atomic_load_explicit(&g.rem_done,memory_order_acquire)!=2)metadata_yield();
            for(;;){metadata_lock(&g.m);bool finished=!g.remembered.walkers;metadata_unlock(&g.m);if(finished)break;metadata_yield();}
            graph_audit_roots(&g,&g.m.ctx,&owner,&audit);
            while(graph_queue_status(&g)!=GC_MARK_EMPTY)metadata_yield();
            graph_end_check_enter(&g,&owner);
            assert(!g.remembered.pending&&!g.remembered.claims&&!g.remembered.walkers);
            assert(!g.rem_set.current_count&&!g.rem_set.pending);
            assert(g.m.leases.active==1&&g.old_page->metadata_leases==1);
            RememberedInventoryRequest retained_source=rem_request;retained_source.phase=(uintptr_t)g.old_page;
            assert(remembered->inventory(&g.remembered,&retained_source,GC_RI_DETACH_PAGE)==GC_RS_WAIT);
            assert(atomic_load(&g.m.ctx.remembered.published)==1&&!atomic_load(&g.m.ctx.remembered.consumed));
            // Empty source membership is insufficient: the actual registered
            // producer's buffered old slot still rejects global completion.
            assert(!graph_inventory_end(&g,&audit));
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            BarrierEntry delayed_output;
            BarrierJob delayed_job={.identity=g.m.ctx.thread_id,.selector=1,.output=(uintptr_t)&delayed_output,.capacity=1};
            metadata_lock(&g.m);
            assert(lifecycle->buffer_job(&g.m.ctx,&delayed_job,GC_BJ_CLAIM)==GC_BJ_OK);
            metadata_unlock(&g.m);
            assert(lifecycle->buffer_job(&g.m.ctx,&delayed_job,GC_BJ_COPY)==GC_BJ_OK&&delayed_job.written==1);
            graph_end_check_enter(&g,&owner);
            assert(g.m.ctx.remembered_consumer.serial==delayed_job.serial&&!graph_inventory_end(&g,&audit));
            assert(remembered->inventory(&g.remembered,&retained_source,GC_RI_DETACH_PAGE)==GC_RS_WAIT);
            graph_end_check_leave(&g,&owner,GC_MP_RETRY);
            RememberedJob replay={.offset=(delayed_output.slot.owner.bits>>16)+delayed_output.slot.byte_offset,
                .incarnation=delayed_output.slot.page_incarnation,.phase=delayed_output.slot.record_epoch};
            RememberedInventoryRequest replay_request={(uintptr_t)&g.rem_registration,(uintptr_t)&replay,0};
            metadata_lock(&g.m);
            assert(remembered->inventory(&g.remembered,&replay_request,GC_RI_RECORD)==GC_RS_OK);
            MetadataLeaseRequest delayed_lease={.token=delayed_output.slot.metadata_lease};
            assert(metadata(&g.m.leases,&delayed_lease,GC_ML_RESOLVE)==GC_ML_OK);
            assert(delayed_lease.page==(uintptr_t)g.old_page);
            assert(metadata(&g.m.leases,&delayed_lease,GC_ML_RELEASE)==GC_ML_OK);
            assert(lifecycle->buffer_job(&g.m.ctx,&delayed_job,GC_BJ_ACK)==GC_BJ_OK);
            assert(g.rem_set.pending==1&&g.remembered.pending==1&&!g.m.leases.active);
            assert(remembered->inventory(&g.remembered,&retained_source,GC_RI_DETACH_PAGE)==GC_RS_WAIT);
            metadata_unlock(&g.m);
            while(atomic_load_explicit(&g.rem_done,memory_order_acquire)!=4)metadata_yield();
            for(;;){metadata_lock(&g.m);bool finished=!g.remembered.walkers;metadata_unlock(&g.m);if(finished)break;metadata_yield();}
            graph_audit_roots(&g,&g.m.ctx,&owner,&audit);
            while(graph_queue_status(&g)!=GC_MARK_EMPTY)metadata_yield();
            metadata_lock(&g.m);
            assert(g.records[7].young_epoch==g.queue.epoch&&!g.remembered.pending&&!g.remembered.claims&&!g.rem_set.current_count&&g.empty_set.current_count==1);
            assert(atomic_load(&g.rem_claimed)==4&&atomic_load(&g.rem_loaded)==4&&g.rem_set.last_serial==2&&g.empty_set.last_serial==2);
            assert(atomic_load(&g.rem_transferred)==2);
            assert(atomic_load(&g.rem_metadata_wait)==1&&atomic_load(&g.rem_access_metadata_wait)==1);
            assert(atomic_load(&g.owner_remaps)==(g.queue.epoch%4==1?13:14));
            assert(atomic_load(&g.rem_access_advanced)==4);
            assert(atomic_load(&g.rem_access_flushes)==(g.queue.epoch%4==1?12:6));
            assert(!empty_page->metadata_leases&&!middle_page->metadata_leases);
            assert(atomic_load(&g.rem_flushes)==(g.queue.epoch%4==1?8:6));
            metadata_unlock(&g.m);
        }
        graph_end_check_enter(&g,&owner);
        assert(g.mark(&g.queue,NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
        assert(atomic_load(&g.m.ctx.root_revision)==root_audit_revision);
        assert(!g.root_registry.leases&&!registration.journal_serial);
        assert(!g.m.ctx.satb_consumer.serial&&!g.m.ctx.remembered_consumer.serial);
        assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_CHECK)==GC_RA_DONE);
        assert(lifecycle->audit(&g.root_registry,&audit,GC_RA_OWNER)==GC_RA_DONE);
        assert(atomic_load(&g.m.ctx.satb.published)==atomic_load(&g.m.ctx.satb.consumed));
        assert(graph_inventory_end(&g,&audit));
        atomic_store_explicit(&g.stop,1,memory_order_release);
        graph_end_check_leave(&g,&owner,GC_MP_SELECT);
#ifdef _WIN32
        for(unsigned i=0;i<2;i++){assert(WaitForSingleObject(workers[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(workers[i]));}
#else
        for(unsigned i=0;i<2;i++)assert(pthread_join(workers[i],NULL)==0);
#endif
        assert(g.cycle_requesters[0]==2&&g.cycle_requesters[1]==2);
        assert(g.cycles.requested[g.queue.generation]==2&&g.cycles.started[g.queue.generation]==1&&!g.cycles.completed[g.queue.generation]);
        assert(!g.cycles.requested[1-g.queue.generation]&&!g.cycles.started[1-g.queue.generation]&&!g.cycles.completed[1-g.queue.generation]);
        assert(lifecycle->cycle(&g.cycles,&g.active_cycle,GC_CY_POLL)==GC_CY_WAIT);
        for(unsigned i=0;i<8;i++){assert((g.queue.generation?g.records[i].old_epoch:g.records[i].young_epoch)==(i<5||(i==7&&!g.queue.generation)?g.queue.epoch:0));assert((g.queue.generation?g.records[i].young_epoch:g.records[i].old_epoch)==0);}
        assert(atomic_load(&g.slots)==array_length+3&&atomic_load(&g.slices)==array_length/8+4+!g.queue.generation);
        assert(!g.m.leases.active&&!g.page->metadata_leases&&!g.old_page->metadata_leases);
        assert(atomic_load(&g.old_skips)==(g.queue.generation?0:21));
        assert(g.root_repairs>0);
        assert(!g.relocation.active&&!g.relocation.head&&g.relocation.last_serial==g.moves_used);
        for(unsigned i=0;i<g.moves_used;i++)assert(!g.moves[i].permit.serial&&!g.moves[i].permit.job&&g.moves[i].job.state==3&&g.moves[i].completion.stage==4);
        assert(g.spill_offers==g.refills&&!g.queue.overflow_count&&!g.queue.overflow_head&&!g.queue.overflow_tail);
        if(g.queue.capacity==1)assert(g.spill_offers>=3&&g.full_claims>=3);
        MarkTask zero={0};for(unsigned i=0;i<8;i++){assert(!memcmp(&g.records[i].young_spill,&zero,sizeof(zero)));assert(!memcmp(&g.records[i].old_spill,&zero,sizeof(zero)));}
        assert(g.handshake.members==1&&g.handshake.last_identity==5&&!g.handshake.active);
        assert(!g.remembered.walkers);
        metadata_lock(&g.m);
        assert(lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_CLOSE)==GC_RG_OK);
        uint64_t heap_audit_epoch=g.relocation.epoch;
        metadata_unlock(&g.m);
        uint64_t heap_audit_phase=atomic_load(&g.m.ctx.adopted_phase);
        // Enumerate the real pool, including large-run tails and protected
        // payload sources. Joined workers and this exclusive owner keep page
        // topology stable. Leases do not make payloads safe to dereference.
        GcHeapWalk heap_walk={(uintptr_t)&g.m.leases,(uintptr_t)&g.m.f.h,g.m.f.h.pages,g.m.f.h.page_count,0,{0}};
        unsigned seen_source=0,seen_old=0,seen_uncommitted=0,seen_objects=0;
        unsigned canonical_objects=0,forwarded_objects=0,seen_records=0,field_slices=0,heap_findings=0;
        unsigned aged_records=0;uint64_t young_live_bytes=0;
        struct {RelocationPermit permit;MarkedRelocation claim;AllocationPage *page;} selections[2]={0};
        uint64_t audit_start=g.queue.generation?g.page->start:g.old_page->start;
        uint64_t audit_limit=g.queue.generation?g.page->limit:g.old_page->limit;
        for(uint64_t index=0;index<g.m.f.h.page_count;index++){
            metadata_lock(&g.m);uint64_t walked=walk_heap(&g.m.leases,&heap_walk);
            assert(heap_walk.cursor==index+1&&(walked==GC_HW_PAGE||walked==GC_HW_MORE));
            if(walked==GC_HW_PAGE){
                AllocationPage *head=(AllocationPage *)(uintptr_t)heap_walk.lease.page;
                assert(head==(AllocationPage *)(uintptr_t)g.m.f.h.pages+index&&head->metadata_leases==1);
                seen_source+=head==g.page;seen_old+=head==g.old_page;
                seen_uncommitted+=atomic_load(&head->state)==GC_PAGE_UNCOMMITTED;
                ForwardTable *table=(ForwardTable *)(uintptr_t)head->forward_table;
                ObjectWalk objects={(uintptr_t)&heap_walk.lease,(uintptr_t)head,head->incarnation,
                    head->forward_table,atomic_load(&table->used),0,0};
                if(head==g.page)assert(objects.used==8);
                if(head==g.old_page)assert(objects.used==1);
                metadata_unlock(&g.m);
                // A page lease survives across bounded metadata-lock slices.
                // Protected source payloads remain completely untouched.
                for(uint64_t key=0;key<objects.used;key++){
                    metadata_lock(&g.m);assert(walk_object(&g.m.leases,&objects)==GC_OW_ITEM);
                    assert(head->metadata_leases==1&&g.m.leases.active==1);metadata_unlock(&g.m);
                    assert(objects.cursor==key+1&&objects.source_offset);
                    if(head==g.page)assert(objects.source_offset==(g.objects[key].reference>>16));
                    if(head==g.old_page)assert(objects.source_offset==(g.old_owner>>16));
                    // The exclusive post-join fixture retains all eight logical
                    // object lifetimes and every physical copy until teardown.
                    // This is stronger than a metadata lease alone. Production
                    // use needs the cycle's equivalent liveness/phase ownership.
                    HeapAuditSlice payload={.page=(uintptr_t)head,.context=(uintptr_t)&g.m.ctx,
                        .reference=(objects.source_offset<<16)|gc_abi_color((PhaseSnapshot){heap_audit_phase}),
                        .registry=(uintptr_t)&g.registry,.phase=heap_audit_phase,.target_start=audit_start,.target_limit=audit_limit};
                    bool identified=false;
                    for(;;){
                        assert(atomic_load(&g.m.ctx.adopted_phase)==heap_audit_phase);
                        uint64_t scanned=audit_slice(&payload);
                        while(scanned==GC_HAS_EXIT_REQUIRED){
                            assert(payload.stage==1&&payload.access.scope.held);
                            scanned=audit_slice(&payload); // immediate cleanup, no poll
                        }
                        assert(!payload.stage&&!payload.access.scope.held&&!atomic_load(&g.m.ctx.access_epoch));
                        if(scanned==GC_HAS_FORWARDED){
                            assert(!identified&&!payload.observed&&!payload.access.destination);
                            forwarded_objects++;break;
                        }
                        assert(scanned==GC_HAS_DONE||scanned==GC_HAS_MORE||scanned==GC_HAS_FOUND);
                        if(!identified){
                            ResolveRequest identity={.reference=payload.reference,.scope={.context=payload.context}};
                            assert(resolve(head,&identity,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
                            const MarkRecord *record=lifecycle->identity((uintptr_t)&identity.scope,(uintptr_t)head,heap_audit_phase);
                            assert((uintptr_t)record>=(uintptr_t)g.records&&
                                (uintptr_t)record-(uintptr_t)g.records<8*sizeof(MarkRecord)&&
                                ((uintptr_t)record-(uintptr_t)g.records)%sizeof(MarkRecord)==0);
                            unsigned logical=(unsigned)(((uintptr_t)record-(uintptr_t)g.records)/sizeof(MarkRecord));
                            assert(!(seen_records&(1u<<logical))&&g.records[logical].object_id==g.objects[logical].object_id);
                            seen_records|=1u<<logical;canonical_objects++;identified=true;
                            uint64_t canonical_cell=identity.scope.held;
                            uint64_t block=lifecycle->block_size((uintptr_t)&identity.scope,(uintptr_t)head,heap_audit_phase);
                            assert(block>=48&&!(block&15));
                            assert(resolve(head,&identity,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
                            assert(!atomic_load(&g.m.ctx.access_epoch));
                            assert(!lifecycle->block_size((uintptr_t)&identity.scope,(uintptr_t)head,heap_audit_phase));
                            // Native records remain cycle-owned after guard exit.
                            // Stamp/age inspection uses the same queue lock as
                            // marking; no managed address crosses this boundary.
                            metadata_lock(&g.m);
                            assert(((g.root_registry.phase>>(4*g.queue.generation))&15)==GC_SELECT);
                            if(g.queue.generation==GC_YOUNG&&head->generation==GC_YOUNG){
                                uint64_t threshold=round%4+1;
                                uint64_t selected=g.rem_ops.move->marked_age(&g.queue,&g.records[logical],threshold);
                                if(g.records[logical].young_epoch==g.queue.epoch){
                                    assert(selected==(threshold==1?GC_AGE_PROMOTE:GC_AGE_YOUNG));
                                    assert(g.records[logical].age==1&&g.records[logical].age_epoch==g.queue.epoch);
                                    assert(!(aged_records&(1u<<logical)));aged_records|=1u<<logical;
                                    if(logical==7||logical==3){
                                        unsigned selected=logical==7?0:1;
                                        selections[selected].page=head;
                                        selections[selected].claim=(MarkedRelocation){.claim={.permit=(uintptr_t)&selections[selected].permit,
                                            .source=canonical_cell,.source_offset=objects.source_offset,.incarnation=head->incarnation},
                                            .queue=(uintptr_t)&g.queue,.record=(uintptr_t)record,.epoch=g.queue.epoch};
                                    }
                                    assert(young_live_bytes<=UINT64_MAX-block);young_live_bytes+=block;
                                    MarkRecord saved=g.records[logical];
                                    assert(g.rem_ops.move->marked_age(&g.queue,&g.records[logical],threshold)==selected);
                                    assert(!memcmp(&saved,&g.records[logical],sizeof(saved)));
                                }else assert(selected==GC_AGE_NOT_SURVIVED&&!g.records[logical].age&&!g.records[logical].age_epoch);
                            }else assert(!g.records[logical].age&&!g.records[logical].age_epoch);
                            metadata_unlock(&g.m);
                        }
                        if(scanned==GC_HAS_FOUND){
                            assert(payload.observed&&(payload.observed>>16)>=audit_start&&(payload.observed>>16)<audit_limit);
                            // Record this failed audit slot and consume it for
                            // enumeration only. No repair or retirement occurs.
                            heap_findings++;payload.cursor+=8;payload.observed=0;
                        }
                        field_slices++;
                        if(scanned==GC_HAS_DONE)break;
                        metadata_yield(); // no raw address or guard survives
                    }
                    objects.source_offset=0;seen_objects++;
                }
                metadata_lock(&g.m);assert(walk_object(&g.m.leases,&objects)==GC_OW_DONE);
                assert(metadata(&g.m.leases,&heap_walk.lease,GC_ML_RELEASE)==GC_ML_OK);
                assert(walk_object(&g.m.leases,&objects)==GC_OW_INVALID);
            }
            assert(!g.m.leases.active);metadata_unlock(&g.m);
        }
        metadata_lock(&g.m);assert(walk_heap(&g.m.leases,&heap_walk)==GC_HW_DONE);metadata_unlock(&g.m);
        assert(seen_source==1&&seen_objects==8+g.moves_used);
        assert(canonical_objects==8&&seen_records==255&&forwarded_objects==g.moves_used);
        assert(aged_records==(g.queue.generation?0:0x9f));
        assert(young_live_bytes==(g.queue.generation?0:array_length*8+272));
        assert(field_slices>=array_length);
        if(g.queue.generation)assert(heap_findings>=2);else assert(!heap_findings);
        // Selection has finished; these first-use fixture colors have no older
        // remap cycle to retire. All joined-worker objects/native controls stay
        // cycle-owned. The sole coordinator owns relocation in this fixture.
        // Publish the selected remap bit and adopt both boundaries before any
        // selected-survivor copying. This is not an Idle/FINISH certificate.
        // Keep real registered roots from before relocation through Retire.
        // These are already-marked survivors; Select publication adds no liveness.
        RootCell retire_cells[2]={0};RootChange retire_changes[2];
        BufferCursor retire_journal={.entries=(uintptr_t)retire_changes,.capacity=2};
        RootSegment retire_segment={.owner=g.m.ctx.thread_id,.cells=(uintptr_t)retire_cells,
            .capacity=2,.journal=(uintptr_t)&retire_journal};
        RootRegistration retire_registration={.segment=(uintptr_t)&retire_segment};RootLease retire_lease={0};
        RootLifeRequest retire_life={.registration=(uintptr_t)&retire_registration,.lease=(uintptr_t)&retire_lease,.context=(uintptr_t)&g.m.ctx};
        RootRequest retire_roots[2]={{.segment=(uintptr_t)&retire_segment},{.segment=(uintptr_t)&retire_segment}};
        if(round%8==0){
            metadata_lock(&g.m);assert(lifecycle->life(&g.root_registry,&retire_life,GC_RL_ATTACH)==GC_RL_OK);metadata_unlock(&g.m);
            for(unsigned i=0;i<2;i++){
                assert(roots(&g.m.ctx,&retire_roots[i],GC_ROOT_RESERVE)==GC_ROOT_OK);
                retire_roots[i].value=g.objects[i?7:3].reference;
                assert(roots(&g.m.ctx,&retire_roots[i],GC_ROOT_STORE)==GC_ROOT_OK);
            }
            while(graph_worker_journal_one(&g,&g.m.ctx,&owner,&retire_registration)){}
        }
        RelocationPhaseControl relocation_start={.handshake=(uintptr_t)&g.handshake,
            .gate=(uintptr_t)&g.relocation,.generation=g.queue.generation};
        WorkerRequest relocation_owner={(uintptr_t)&g.handshake,(uintptr_t)&owner};
        metadata_lock(&g.m);
        assert(g.handshake.members==1&&!g.handshake.native&&!g.handshake.active);
        assert(lifecycle->relocation_phase(&g.root_registry,&relocation_start,GC_RP_BEGIN)==GC_RP_ADVANCED);
        assert(lifecycle->relocation_phase(&g.root_registry,&relocation_start,GC_RP_ACTIVATE)==GC_RP_WAIT);
        assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_PARK);
        assert(lifecycle->relocation_phase(&g.root_registry,&relocation_start,GC_RP_ACTIVATE)==GC_RP_ADVANCED);
        assert(lifecycle->relocation_phase(&g.root_registry,&relocation_start,GC_RP_RELEASE)==GC_RP_WAIT);
        assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_PARK);
        assert(lifecycle->relocation_phase(&g.root_registry,&relocation_start,GC_RP_RELEASE)==GC_RP_RELOCATING);
        assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_OK);
        assert(atomic_load(&g.m.ctx.adopted_phase)==g.root_registry.phase);
        assert(((g.root_registry.phase>>(4*g.queue.generation))&15)==GC_CONCURRENT_RELOCATE);
        assert(!g.cycles.completed[g.queue.generation]);
        assert(lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_CLOSE)==GC_RG_OK);
        heap_audit_epoch=g.relocation.epoch;heap_audit_phase=g.root_registry.phase;
        metadata_unlock(&g.m);
        if(!g.queue.generation)for(unsigned selection=0;selection<(round%8==0?2u:1u);selection++){
            unsigned logical=selection?3:7;
            uint64_t payload_bytes=selection?8:sizeof(survivor_bytes);
            RelocationPermit *selected_permit=&selections[selection].permit;
            MarkedRelocation *selected_claim=&selections[selection].claim;
            AllocationPage *selected_page=selections[selection].page;
            assert(selected_page&&selected_claim->record==(uintptr_t)&g.records[logical]);
            metadata_lock(&g.m);
            RelocationGate saved_gate=g.relocation;MarkedRelocation saved_claim=*selected_claim;
            assert(lifecycle->marked_claim(&g.relocation,selected_claim)==GC_RCL_CLOSED);
            assert(!memcmp(&g.relocation,&saved_gate,sizeof(saved_gate))&&!memcmp(selected_claim,&saved_claim,sizeof(saved_claim)));
            if(round%4==0){ // threshold one selected this survivor for promotion
                ReferenceLease retained={.reference=(selected_claim->claim.source_offset<<16)|gc_abi_color((PhaseSnapshot){heap_audit_phase})};
                assert(reference_lease(&g.m.leases,&retained)==GC_ML_OK);
                assert(lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_REOPEN)==GC_RG_OK);
                assert(lifecycle->marked_claim(&g.relocation,selected_claim)==GC_RCL_OWNED);
                ForwardCell *candidate=(void *)(uintptr_t)selected_claim->claim.source;
                SourceAccessRequest close={0,candidate->source_offset,candidate->incarnation,0,selected_claim->claim.ticket};
                assert(g.rem_ops.move->access(candidate,&close,GC_SA_SELECT)==GC_SA_OK);
                assert(g.rem_ops.move->access(candidate,&close,GC_SA_CHECK)==GC_SA_OK);
                RelocationCopy attempt={(uintptr_t)mapping,candidate->incarnation,selected_claim->claim.ticket,selected_page->limit,0,0,8,0,0};
                if(round%8==0){
                    assert(empty_page->generation==GC_OLD&&selected_page->generation==GC_YOUNG);
                    assert(g.rem_ops.move->target(empty_page,&attempt,GC_TARGET_PREPARE)==GC_TARGET_OK);
                    metadata_unlock(&g.m);
                    // The drained source, cycle-held object and exclusive target
                    // append rights survive this bounded copy outside the lock.
                    attempt.budget=1;
                    for(unsigned byte=0;byte<payload_bytes;byte++){
                        assert(g.rem_ops.move->copy(candidate,&attempt)==(byte+1==payload_bytes?GC_COPY_DONE:GC_COPY_MORE));
                        assert(attempt.cursor==byte+1&&atomic_load(&candidate->word)==((attempt.ticket<<2)|1));
                        metadata_yield();
                    }
                    assert(attempt.stage==2&&attempt.cursor==payload_bytes);
                    metadata_lock(&g.m);
                    assert(g.rem_ops.move->target(empty_page,&attempt,GC_TARGET_INSTALL)==GC_TARGET_OK);
                    PromotionRemember promotion={(uintptr_t)&g.registry,(uintptr_t)&g.empty_registration,
                        attempt.destination,selection?2:3,payload_bytes,g.remembered.epoch,0};
                    uint64_t slots_before=g.empty_set.current_count;
                    assert(remembered->promote(&g.remembered,&promotion)==GC_PR_DONE);
                    assert(g.empty_set.current_count==slots_before+selection);
                    if(selection){
                        uint64_t bit=(attempt.destination-g.empty_set.start)/8;
                        uint64_t mask=UINT64_C(1)<<(bit%64);
                        assert(storage->empty_bits[g.empty_set.current][bit/64]&mask);
                        assert(!(storage->empty_bits[g.empty_set.current^1][bit/64]&mask));
                        // Repeating a finished descriptor walk cannot duplicate
                        // the new slot or erase the existing retained owner.
                        assert(remembered->promote(&g.remembered,&promotion)==GC_PR_DONE);
                        assert(g.empty_set.current_count==slots_before+1);
                    }
                    // These Young rounds have no active Old mark. Overlapping
                    // Old marking requires an additional mark handoff.
                    assert(((heap_audit_phase>>4)&15)==GC_IDLE);
                    assert(atomic_load(&candidate->word)==((attempt.ticket<<2)|1));
                    RelocationJob job={.permit=(uintptr_t)selected_permit,.source=(uintptr_t)candidate,
                        .ticket=attempt.ticket,.destination=(attempt.destination<<16)|gc_abi_color((PhaseSnapshot){heap_audit_phase})};
                    RelocationCompletion completion={.job=(uintptr_t)&job,.copy=(uintptr_t)&attempt};
                    for(unsigned stage=0;stage<4;stage++){
                        assert(lifecycle->relocation_complete(&g.relocation,&completion)==(stage==3?GC_RC_DONE:GC_RC_MORE));
                    }
                    assert(completion.stage==4&&!selected_permit->serial);
                    ReferenceLease destination={.reference=job.destination};
                    assert(reference_lease(&g.m.leases,&destination)==GC_ML_OK);
                    metadata_unlock(&g.m);
                    // Both leases are acquired before the private forwarding
                    // result exists. Resolve and inspect without a poll or lock.
                    ResolveRequest original={.reference=retained.reference,.scope={.context=(uintptr_t)&g.m.ctx}};
                    assert(resolve(selected_page,&original,GC_RESOLVE_ENTER)==GC_RESOLVE_FORWARDED);
                    assert(original.destination==job.destination&&!original.scope.held);
                    ResolveRequest canonical={.reference=original.destination,.scope={.context=(uintptr_t)&g.m.ctx}};
                    original.destination=0;
                    assert(resolve(empty_page,&canonical,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
                    assert(lifecycle->identity((uintptr_t)&canonical.scope,(uintptr_t)empty_page,heap_audit_phase)==&g.records[logical]);
                    assert(g.records[logical].object_id==g.objects[logical].object_id);
                    assert(lifecycle->block_size((uintptr_t)&canonical.scope,(uintptr_t)empty_page,heap_audit_phase)==48);
                    if(!selection)assert(!memcmp((char *)mapping+(canonical.reference>>16),survivor_bytes,sizeof(survivor_bytes)));
                    assert(resolve(empty_page,&canonical,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
                    assert(!atomic_load(&g.m.ctx.access_epoch));
                    if(selection){
                        // The installed Old object's reference slot still names
                        // its live Young child. Scan through generated guarded
                        // access, then consume the observation without healing.
                        HeapAuditSlice edge={.page=(uintptr_t)empty_page,.context=(uintptr_t)&g.m.ctx,
                            .reference=job.destination,.registry=(uintptr_t)&g.registry,.phase=heap_audit_phase,
                            .target_start=g.page->start,.target_limit=g.page->limit};
                        assert(audit_slice(&edge)==GC_HAS_FOUND);
                        assert(!edge.stage&&!edge.access.scope.held&&!atomic_load(&g.m.ctx.access_epoch));
                        assert(edge.observed==g.objects[4].reference&&edge.cursor==0);
                        assert(g.records[4].young_epoch==g.queue.epoch&&g.page->generation==GC_YOUNG);
                        edge.cursor+=8;edge.observed=0;
                        assert(audit_slice(&edge)==GC_HAS_DONE&&!edge.observed&&!edge.stage);
                    }
                    metadata_lock(&g.m);
                    assert(metadata(&g.m.leases,&destination.lease,GC_ML_RELEASE)==GC_ML_OK);
                }else{
                    RelocationCopy clean_attempt=attempt;
                    // The Old target's forwarding metadata is full. No private
                    // target exists, so retention needs no copy cleanup.
                    assert(g.rem_ops.move->target(empty_page,&attempt,GC_TARGET_PREPARE)==GC_TARGET_METADATA_REFILL);
                    assert(!memcmp(&attempt,&clean_attempt,sizeof(attempt)));
                    ForwardRequest keep={candidate->source_offset,candidate->incarnation,selected_claim->claim.ticket,0};
                    assert(g.rem_ops.move->forward(candidate,&keep,GC_FWD_RETAIN)==GC_FWD_OK);
                    close.ticket=0;assert(g.rem_ops.move->access(candidate,&close,GC_SA_REOPEN)==GC_SA_OK);
                    RelocationGateRequest release={selected_claim->claim.permit,selected_claim->claim.ticket,selected_claim->claim.epoch};
                    assert(lifecycle->relocation_gate(&g.relocation,&release,GC_RG_RELEASE)==GC_RG_OK);
                }
                assert(lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_CLOSE)==GC_RG_OK);
                assert(metadata(&g.m.leases,&retained.lease,GC_ML_RELEASE)==GC_ML_OK);
                assert(!g.m.leases.active&&!g.relocation.active&&!selected_permit->serial);
                heap_audit_epoch=g.relocation.epoch;
            }
            metadata_unlock(&g.m);
        }
        // All selected placements are published or retained. Keep admission
        // closed while every actor adopts Retire; audits still own the separate
        // proof that no source/metadata/color can be reclaimed prematurely.
        RelocationPhaseControl retire_phase={.handshake=(uintptr_t)&g.handshake,
            .gate=(uintptr_t)&g.relocation,.generation=g.queue.generation};
        metadata_lock(&g.m);
        assert(!g.relocation.active&&g.relocation.closed&&!g.m.leases.active);
        assert(lifecycle->relocation_retire(&g.root_registry,&retire_phase,GC_RR_BEGIN)==GC_RP_ADVANCED);
        assert(lifecycle->relocation_retire(&g.root_registry,&retire_phase,GC_RR_RELEASE)==GC_RP_WAIT);
        assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_PARK);
        assert(lifecycle->relocation_retire(&g.root_registry,&retire_phase,GC_RR_RELEASE)==GC_RP_RETIRING);
        assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_OK);
        assert(atomic_load(&g.m.ctx.adopted_phase)==g.root_registry.phase);
        assert(((g.root_registry.phase>>(4*g.queue.generation))&15)==GC_RETIRE);
        assert(g.relocation.closed&&g.relocation.epoch==heap_audit_epoch);
        assert(!g.cycles.completed[g.queue.generation]);
        heap_audit_phase=g.root_registry.phase;
        metadata_unlock(&g.m);
        // Independently drive the reusable owner over the same frozen heap.
        // META owns the lock; SCAN/CONSUME never execute under it. The preceding
        // explicit traversal supplies identity coverage and expected findings.
        HeapAudit owned_audit={.context=(uintptr_t)&g.m.ctx,.registry=(uintptr_t)&g.registry,
            .gate=(uintptr_t)&g.relocation,.epoch=heap_audit_epoch,.phase=heap_audit_phase,
            .target_start=audit_start,.target_limit=audit_limit,
            .walk={(uintptr_t)&g.m.leases,(uintptr_t)&g.m.f.h,g.m.f.h.pages,g.m.f.h.page_count,0,{0}}};
        unsigned audit_steps=0;
        for(;;){
            assert(++audit_steps<array_length*4+512);
            uint64_t op=owned_audit.stage==2?GC_HA_SCAN:owned_audit.stage==3?GC_HA_CONSUME:GC_HA_META;
            if(op==GC_HA_META)metadata_lock(&g.m);
            uint64_t status=audit_owner(&g.m.leases,&owned_audit,op);
            if(op==GC_HA_META)metadata_unlock(&g.m);
            if(status==GC_HA_EXIT_REQUIRED){assert(op==GC_HA_SCAN&&owned_audit.slice.access.scope.held);continue;}
            assert(!atomic_load(&g.m.ctx.access_epoch));
            if(status==GC_HA_DONE||status==GC_HA_DIRTY){
                assert(status==(heap_findings?GC_HA_DIRTY:GC_HA_DONE));break;
            }
            assert(status==GC_HA_MORE||status==GC_HA_SCAN_READY||status==GC_HA_FOUND);
            metadata_yield();
        }
        assert(owned_audit.findings==heap_findings&&owned_audit.stage==4&&!g.m.leases.active);
        assert(owned_audit.walk.cursor==g.m.f.h.page_count&&!owned_audit.walk.lease.token);
        metadata_lock(&g.m);assert(audit_owner(&g.m.leases,&owned_audit,GC_HA_CANCEL)==GC_HA_CANCELLED);metadata_unlock(&g.m);
        assert(!owned_audit.stage&&!owned_audit.findings&&!g.m.leases.active);
        if(round%8==0){
            // Revisit actual incoming edges to the two newly promoted sources.
            // Their old user granules contain no remaining canonical object;
            // this narrow range proof does not authorize freeing their page.
            RootCell repair_cells[2]={0};RootChange repair_changes[2];
            BufferCursor repair_journal={.entries=(uintptr_t)repair_changes,.capacity=2};
            RootSegment repair_segment={.owner=g.m.ctx.thread_id,.cells=(uintptr_t)repair_cells,
                .capacity=2,.journal=(uintptr_t)&repair_journal};
            RootRegistration repair_registration={.segment=(uintptr_t)&repair_segment};RootLease repair_lease={0};
            RootLifeRequest repair_life={.registration=(uintptr_t)&repair_registration,.lease=(uintptr_t)&repair_lease,.context=(uintptr_t)&g.m.ctx};
            metadata_lock(&g.m);assert(lifecycle->life(&g.root_registry,&repair_life,GC_RL_ATTACH)==GC_RL_OK);metadata_unlock(&g.m);
            RootRequest repair_roots[2]={{.segment=(uintptr_t)&repair_segment},{.segment=(uintptr_t)&repair_segment}};
            for(unsigned i=0;i<2;i++)assert(roots(&g.m.ctx,&repair_roots[i],GC_ROOT_RESERVE)==GC_ROOT_OK);
            unsigned repairs=0,repair_flushes=0;
            const unsigned promoted[]={3,7};
            for(unsigned i=0;i<2;i++)for(unsigned pass=0;pass<2;pass++){
                unsigned logical=promoted[i];uint64_t start=g.objects[logical].reference>>16;
                HeapAudit repair={.context=(uintptr_t)&g.m.ctx,.registry=(uintptr_t)&g.registry,
                    .gate=(uintptr_t)&g.relocation,.epoch=heap_audit_epoch,.phase=heap_audit_phase,
                    .target_start=start,.target_limit=start+16,
                    .walk={(uintptr_t)&g.m.leases,(uintptr_t)&g.m.f.h,g.m.f.h.pages,g.m.f.h.page_count,0,{0}}};
                unsigned steps=0;
                for(;;){
                    assert(++steps<array_length*4+512);
                    uint64_t op=repair.stage==2?GC_HA_SCAN:repair.stage==3?GC_HA_CONSUME:GC_HA_META;
                    if(op==GC_HA_META)metadata_lock(&g.m);
                    uint64_t status=audit_owner(&g.m.leases,&repair,op);
                    if(op==GC_HA_META)metadata_unlock(&g.m);
                    if(status==GC_HA_EXIT_REQUIRED)continue; // exact guard cleanup, no poll
                    assert(!atomic_load(&g.m.ctx.access_epoch));
                    if(status==GC_HA_FOUND){
                        assert(!pass);
                        repair_flushes+=graph_repair_finding(&g,&owner,&repair,&repair_registration,&repair_roots[0],&repair_roots[1],logical);
                        repairs++;
                    }else if(status==GC_HA_DONE||status==GC_HA_DIRTY){
                        // A repairing pass still reports what it observed. Only
                        // the fresh, independent second pass may report clean.
                        assert(status==(pass?GC_HA_DONE:GC_HA_DIRTY));
                        assert(repair.findings==(pass?0u:logical==3?2u:1u));break;
                    }else assert(status==GC_HA_MORE||status==GC_HA_SCAN_READY);
                    metadata_yield();
                }
                metadata_lock(&g.m);assert(audit_owner(&g.m.leases,&repair,GC_HA_CANCEL)==GC_HA_CANCELLED);metadata_unlock(&g.m);
                assert(!g.m.leases.active);
            }
            assert(repairs==3&&repair_flushes>=3&&!atomic_load(&g.m.ctx.private_producers));
            for(unsigned i=0;i<2;i++)assert(roots(&g.m.ctx,&repair_roots[i],GC_ROOT_RELEASE)==GC_ROOT_OK);
            while(graph_worker_journal_one(&g,&g.m.ctx,&owner,&repair_registration)){}
            metadata_lock(&g.m);
            assert(lifecycle->life(&g.root_registry,&repair_life,GC_RL_RETIRE)==GC_RL_OK);
            assert(lifecycle->life(&g.root_registry,&repair_life,GC_RL_CLAIM)==GC_RL_OK);metadata_unlock(&g.m);
            uint64_t empty_roots[2]={1,1};
            RootScanJob repair_scan={.scan={(uintptr_t)&repair_segment,repair_life.begin,(uintptr_t)empty_roots,2,0},
                .lease=(uintptr_t)&repair_lease,.serial=repair_life.serial};
            assert(lifecycle->leased(&repair_scan)==GC_ROOT_OK&&repair_scan.scan.written==2&&!empty_roots[0]&&!empty_roots[1]);
            metadata_lock(&g.m);
            assert(lifecycle->life(&g.root_registry,&repair_life,GC_RL_ACK)==GC_RL_OK);
            assert(lifecycle->life(&g.root_registry,&repair_life,GC_RL_CLEAN)==GC_RL_OK);metadata_unlock(&g.m);
            assert(lifecycle->clear(&repair_life)==GC_RL_OK);metadata_lock(&g.m);
            assert(lifecycle->life(&g.root_registry,&repair_life,GC_RL_FINISH)==GC_RL_OK);metadata_unlock(&g.m);
            assert(g.root_registry.active==2&&!g.root_registry.retiring&&!g.root_registry.leases&&g.relocation.closed);
            RootAudit retired_roots={.queue=(uintptr_t)&g.queue,.context=(uintptr_t)&g.m.ctx};
            const GraphAuditRange promoted_ranges[]={
                {g.objects[3].reference>>16,(g.objects[3].reference>>16)+16},
                {g.objects[7].reference>>16,(g.objects[7].reference>>16)+16}};
            uint64_t before_repairs=g.root_repairs;
            assert(graph_audit_roots_mode(&g,&g.m.ctx,&owner,&retired_roots,true,promoted_ranges,2)==2);
            assert(g.root_repairs==before_repairs+2&&!g.m.leases.active);
            // A real logical root store after scanning invalidates this owner's
            // certificate, even when the value is unchanged. Its journal must
            // be consumed before a new pass can certify inventory completion.
            assert(roots(&g.m.ctx,&retire_roots[0],GC_ROOT_READ_RAW)==GC_ROOT_OK);
            assert(roots(&g.m.ctx,&retire_roots[0],GC_ROOT_STORE)==GC_ROOT_OK);
            metadata_lock(&g.m);
            assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
            assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_PARK);
            assert(lifecycle->retire_audit(&g.root_registry,&retired_roots,GC_RA_CHECK)==GC_RA_DONE);
            assert(graph_retire_inventory_check(&g,&retired_roots)==GC_ME_STALE);
            assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_END)==GC_HS_OK);
            assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_OK);metadata_unlock(&g.m);
            while(graph_worker_journal_one(&g,&g.m.ctx,&owner,&retire_registration)){}
            assert(!graph_audit_roots_mode(&g,&g.m.ctx,&owner,&retired_roots,true,promoted_ranges,2));
            // A conservative transport entry is independent of the now-clean
            // root inventory. Its old source and token remain owned until the
            // registered consumer processes COPY and acknowledges the ring.
            // This fixture keeps all logical objects alive; both marks are off.
            assert((heap_audit_phase&255)==GC_RETIRE);
            ReferenceLease buffered_source={.reference=g.objects[3].reference};
            metadata_lock(&g.m);assert(reference_lease(&g.m.leases,&buffered_source)==GC_ML_OK);metadata_unlock(&g.m);
            BarrierEntry retiring_entry={.previous={g.objects[3].reference},
                .slot={.page_incarnation=buffered_source.lease.incarnation,.record_epoch=heap_audit_phase,
                    .metadata_lease=buffered_source.lease.token}};
            assert(remembered->buffer(&g.m.ctx,&retiring_entry,GC_BB_APPEND_SATB)==GC_BB_OK);
            BarrierEntry copied_retiring_entry={0};
            BarrierJob retiring_job={.identity=g.m.ctx.thread_id,.output=(uintptr_t)&copied_retiring_entry,.capacity=1};
            for(unsigned stage=0;stage<2;stage++){
                metadata_lock(&g.m);
                assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
                if(!stage){
                    assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_FLUSH_SATB);
                    metadata_unlock(&g.m);
                    assert(remembered->buffer(&g.m.ctx,NULL,GC_BB_PUBLISH_SATB)==GC_BB_OK);
                    metadata_lock(&g.m);
                }
                assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_PARK);
                assert(lifecycle->retire_audit(&g.root_registry,&retired_roots,GC_RA_CHECK)==GC_RA_DONE);
                assert(graph_retire_inventory_check(&g,&retired_roots)==GC_ME_WAIT);
                assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_END)==GC_HS_OK);
                assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_OK);
                if(!stage)assert(lifecycle->buffer_job(&g.m.ctx,&retiring_job,GC_BJ_CLAIM)==GC_BJ_OK);
                metadata_unlock(&g.m);
                if(!stage){
                    assert(lifecycle->buffer_job(&g.m.ctx,&retiring_job,GC_BJ_COPY)==GC_BJ_OK&&retiring_job.written==1);
                    assert(!memcmp(&retiring_entry,&copied_retiring_entry,sizeof(retiring_entry)));
                }
            }
            // Consume the already-live reference under its canonical guard,
            // transfer source metadata to the destination, then retire the job.
            MetadataLeaseRequest copied_lease={.token=copied_retiring_entry.slot.metadata_lease};
            metadata_lock(&g.m);assert(metadata(&g.m.leases,&copied_lease,GC_ML_RESOLVE)==GC_ML_OK);metadata_unlock(&g.m);
            GraphAccess copied_guard=graph_enter(&g,&g.m.ctx,&copied_lease,copied_retiring_entry.previous.bits);
            assert(lifecycle->identity((uintptr_t)&copied_guard.request.scope,(uintptr_t)copied_guard.page,heap_audit_phase)==&g.records[3]);
            graph_exit(&g,&copied_guard);metadata_lock(&g.m);
            assert(metadata(&g.m.leases,&copied_lease,GC_ML_RELEASE)==GC_ML_OK);
            assert(lifecycle->buffer_job(&g.m.ctx,&retiring_job,GC_BJ_ACK)==GC_BJ_OK);
            assert(!g.m.leases.active);
            metadata_unlock(&g.m);
            metadata_lock(&g.m);
            assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
            assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_PARK);
            assert(g.handshake.members==1&&g.handshake.acknowledged==1);
            assert(lifecycle->retire_audit(&g.root_registry,&retired_roots,GC_RA_CHECK)==GC_RA_DONE);
            assert(graph_retire_inventory_check(&g,&retired_roots)==GC_ME_READY);
            assert(g.relocation.closed&&!g.cycles.completed[g.queue.generation]);
            assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_END)==GC_HS_OK);
            assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_OK);metadata_unlock(&g.m);
            for(unsigned i=0;i<2;i++)assert(roots(&g.m.ctx,&retire_roots[i],GC_ROOT_RELEASE)==GC_ROOT_OK);
            while(graph_worker_journal_one(&g,&g.m.ctx,&owner,&retire_registration)){}
            metadata_lock(&g.m);
            assert(lifecycle->life(&g.root_registry,&retire_life,GC_RL_RETIRE)==GC_RL_OK);
            assert(lifecycle->life(&g.root_registry,&retire_life,GC_RL_CLAIM)==GC_RL_OK);metadata_unlock(&g.m);
            RootScanJob retire_scan={.scan={(uintptr_t)&retire_segment,retire_life.begin,(uintptr_t)empty_roots,2,0},
                .lease=(uintptr_t)&retire_lease,.serial=retire_life.serial};
            assert(lifecycle->leased(&retire_scan)==GC_ROOT_OK&&retire_scan.scan.written==2&&!empty_roots[0]&&!empty_roots[1]);
            metadata_lock(&g.m);
            assert(lifecycle->life(&g.root_registry,&retire_life,GC_RL_ACK)==GC_RL_OK);
            assert(lifecycle->life(&g.root_registry,&retire_life,GC_RL_CLEAN)==GC_RL_OK);metadata_unlock(&g.m);
            assert(lifecycle->clear(&retire_life)==GC_RL_OK);metadata_lock(&g.m);
            assert(lifecycle->life(&g.root_registry,&retire_life,GC_RL_FINISH)==GC_RL_OK);metadata_unlock(&g.m);
            assert(g.root_registry.active==1&&!g.root_registry.retiring&&!g.root_registry.leases);
        }
        // Both generations install the initial Old owner. Only Young rounds
        // move it twice more and decommit those two source payloads.
        assert(seen_old==1);
        assert(seen_uncommitted==(g.queue.generation?0u:2u));
        if(!g.queue.generation){
            // Workers are joined and all captured source records consumed.
            // Only the source membership is now empty; the destination's
            // retained edge still forbids dropping its registration.
            RememberedInventoryRequest detach_source=rem_request;detach_source.phase=(uintptr_t)g.old_page;
            assert(remembered->inventory(&g.remembered,&detach_source,GC_RI_DETACH_PAGE)==GC_RS_OK);
            // No metadata lease or remembered membership remains on this source,
            // but the older forwarding table still points into its virtual run.
            // All workers/publication jobs are finished; table keys stay frozen.
            metadata_lock(&g.m);assert(g.relocation.epoch==heap_audit_epoch&&
                lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_CHECK)==GC_RG_OK);
            uint64_t frozen_epoch=heap_audit_epoch;
            RelocationPermit blocked_move={0};RelocationGateRequest blocked_request={(uintptr_t)&blocked_move,0,0};
            assert(lifecycle->relocation_gate(&g.relocation,&blocked_request,GC_RG_ACQUIRE)==GC_RG_WAIT);
            metadata_unlock(&g.m);
            ForwardIncoming audit_incoming={g.page->incarnation,atomic_load(&storage->forwards.used),g.old_page->start,g.old_page->limit,0,2,0,0};
            uint64_t incoming_status;
            do{incoming_status=incoming(&storage->forwards,&audit_incoming);assert(incoming_status==GC_FI_MORE||incoming_status==GC_FI_FOUND);}while(incoming_status==GC_FI_MORE);
            assert(audit_incoming.source_offset==(g.objects[6].reference>>16));
            assert(!g.old_page->metadata_leases&&atomic_load(&g.old_page->state)==GC_PAGE_UNCOMMITTED);
            audit_incoming=(ForwardIncoming){g.old_page->incarnation,atomic_load(&sources[0]->table.used),middle_page->start,middle_page->limit,0,1,0,0};
            assert(incoming(&sources[0]->table,&audit_incoming)==GC_FI_FOUND);
            assert(audit_incoming.source_offset==(g.old_owner>>16)&&!middle_page->metadata_leases);
            // Preserve the full observed chain while proving its final header.
            // The fixture's live graph and private page storage retain the
            // object; the four counted leases keep all forwarding metadata live.
            uint64_t chain[]={g.objects[6].reference,g.old_owner,
                (sources[1]->cell.source_offset<<16)|(g.old_owner&UINT16_MAX),g.moved_owner};
            ReferenceLease held_chain[4]={0};
            metadata_lock(&g.m);
            for(unsigned i=0;i<4;i++){
                held_chain[i].reference=chain[i];assert(reference_lease(&g.m.leases,&held_chain[i])==GC_ML_OK);
            }
            metadata_unlock(&g.m);
            for(unsigned i=0;i<4;i++){
                ResolveRequest observed={.reference=chain[i],.scope={.context=(uintptr_t)&g.m.ctx}};
                AllocationPage *page=(AllocationPage *)(uintptr_t)held_chain[i].lease.page;
                if(i<3)assert(resolve(page,&observed,GC_RESOLVE_ENTER)==GC_RESOLVE_FORWARDED&&observed.destination==chain[i+1]);
                else{
                    assert(resolve(page,&observed,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
                    ObjectHeader *header=(ObjectHeader *)((char *)mapping+(chain[i]>>16)-sizeof(ObjectHeader));
                    assert(header->mark_record==(uintptr_t)&g.records[6]&&g.records[6].object_id==g.objects[6].object_id);
                    // Inspect the real moved Old owner while its canonical
                    // payload guard and all source-chain metadata leases live.
                    // Workers are joined; this is not a racing heap snapshot.
                    uint64_t child=atomic_load_explicit((_Atomic uint64_t *)(header+1),memory_order_acquire);
                    assert(child&&header->payload_bytes==8);
                    HeapIncoming heap_audit={(uintptr_t)&observed.scope,(uintptr_t)page,(uintptr_t)&g.registry,
                        atomic_load(&g.m.ctx.adopted_phase),child>>16,(child>>16)+16,0,0};
                    assert(heap_incoming(&heap_audit)==GC_HI_FOUND&&heap_audit.cursor==0&&heap_audit.observed==child);
                    heap_audit.observed=0;
                    assert(heap_incoming(&heap_audit)==GC_HI_FOUND&&heap_audit.cursor==0&&heap_audit.observed==child);
                    for(unsigned retired=1;retired<3;retired++){
                        AllocationPage *source=(AllocationPage *)(uintptr_t)held_chain[retired].lease.page;
                        heap_audit=(HeapIncoming){(uintptr_t)&observed.scope,(uintptr_t)page,(uintptr_t)&g.registry,
                            atomic_load(&g.m.ctx.adopted_phase),source->start,source->limit,0,0};
                        assert(heap_incoming(&heap_audit)==GC_HI_DONE&&heap_audit.cursor==8&&!heap_audit.observed);
                    }
                    assert(resolve(page,&observed,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
                }
                assert(!atomic_load(&g.m.ctx.access_epoch));
            }
            const GraphAuditRange source_ranges[]={{g.old_page->start,g.old_page->limit},{middle_page->start,middle_page->limit}};
            // A table-local success cannot exclude references in a different
            // page. Inspect every retained table before and after compression.
            for(unsigned i=0;i<2;i++)assert(graph_forward_range_audit(&g,walk_heap,incoming,frozen_epoch,heap_audit_phase,source_ranges[i])==1);
            assert(g.m.leases.active==4);
            metadata_lock(&g.m);
            ForwardCompress shorten_old={(uintptr_t)&sources[0]->cell,sources[0]->cell.source_offset,
                sources[0]->cell.incarnation,chain[2],chain[3],frozen_epoch};
            ForwardCompress shorten_first={(uintptr_t)&storage->cells[6],storage->cells[6].source_offset,
                storage->cells[6].incarnation,chain[1],chain[3],frozen_epoch};
            assert(compress(&g.relocation,&shorten_old)==GC_FC_OK);
            assert(compress(&g.relocation,&shorten_first)==GC_FC_OK);
            assert(compress(&g.relocation,&shorten_first)==GC_FC_RETRY);
            metadata_unlock(&g.m);
            // Restart both audits after modifying edges. No publication or
            // compression occurs until both complete under the frozen epoch.
            audit_incoming=(ForwardIncoming){g.page->incarnation,atomic_load(&storage->forwards.used),g.old_page->start,g.old_page->limit,0,2,0,0};
            do{incoming_status=incoming(&storage->forwards,&audit_incoming);assert(incoming_status==GC_FI_MORE||incoming_status==GC_FI_DONE);}while(incoming_status==GC_FI_MORE);
            audit_incoming=(ForwardIncoming){g.old_page->incarnation,atomic_load(&sources[0]->table.used),middle_page->start,middle_page->limit,0,1,0,0};
            assert(incoming(&sources[0]->table,&audit_incoming)==GC_FI_DONE);
            for(unsigned i=0;i<2;i++){
                ResolveRequest direct={.reference=chain[i],.scope={.context=(uintptr_t)&g.m.ctx}};
                assert(resolve((AllocationPage *)(uintptr_t)held_chain[i].lease.page,&direct,GC_RESOLVE_ENTER)==GC_RESOLVE_FORWARDED&&direct.destination==chain[3]);
            }
            metadata_lock(&g.m);
            for(unsigned i=0;i<4;i++)assert(metadata(&g.m.leases,&held_chain[i].lease,GC_ML_RELEASE)==GC_ML_OK);
            assert(g.old_page->forward_table==(uintptr_t)&sources[0]->table&&middle_page->forward_table==(uintptr_t)&sources[1]->table);
            metadata_unlock(&g.m);
            metadata_lock(&g.m);assert(g.relocation.epoch==frozen_epoch&&lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_CHECK)==GC_RG_OK);
            metadata_unlock(&g.m); // Retire keeps move admission closed.
            for(unsigned i=0;i<2;i++){
                assert(!graph_forward_range_audit(&g,walk_heap,incoming,frozen_epoch,heap_audit_phase,source_ranges[i]));
                graph_heap_range_audit(&g,audit_owner,frozen_epoch,heap_audit_phase,source_ranges[i]);
                assert(!g.m.leases.active);
            }
            RootAudit source_root_audit={.queue=(uintptr_t)&g.queue,.context=(uintptr_t)&g.m.ctx};
            assert(!graph_audit_roots_mode(&g,&g.m.ctx,&owner,&source_root_audit,true,source_ranges,2));
            metadata_lock(&g.m);
            assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
            assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_PARK);
            assert(graph_retire_inventory_check(&g,&source_root_audit)==GC_ME_READY);
            assert(g.relocation.epoch==frozen_epoch&&g.root_registry.phase==heap_audit_phase);
            assert(lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_CHECK)==GC_RG_OK);
            assert(!g.m.leases.active&&!g.old_page->metadata_leases&&!middle_page->metadata_leases);
            // Workers are joined and this coordinator is the only remaining
            // actor. There is no other-generation queue, weak or native handle
            // in this fixture. The full-range audits above cover published
            // holders; discard the coordinator's diagnostic references and
            // terminal relocation receipts before detaching source metadata.
            assert(g.handshake.members==1&&!g.rem_registration.registry);
            for(unsigned i=0;i<g.moves_used;i++){
                assert(!g.moves[i].permit.serial&&!g.moves[i].permit.job&&
                    g.moves[i].job.state==3&&g.moves[i].completion.stage==4);
            }
            memset(g.moves,0,sizeof(g.moves));
            memset(chain,0,sizeof(chain));memset(held_chain,0,sizeof(held_chain));
            memset(&shorten_old,0,sizeof(shorten_old));memset(&shorten_first,0,sizeof(shorten_first));
            g.old_owner=0;old_run.starts=middle_run.starts=0;
            AllocationPage *retired_pages[]={g.old_page,middle_page};
            SourceRetire retire[2]={0};
            uint64_t committed_before=g.m.f.h.committed,metadata_before=g.m.f.h.metadata_committed;
            for(unsigned i=0;i<2;i++){
                assert(sources[i]->cell.source_offset==retired_pages[i]->start+sizeof(ObjectHeader));
                retire[i].head_index=retired_pages[i]->run_head;
                retire[i].page_count=(retired_pages[i]->limit-retired_pages[i]->start)/GC_HEAP_PAGE_BYTES;
                retire[i].incarnation=retired_pages[i]->incarnation;
                assert(lifecycle->source_retire(&g.m.f.h,&retire[i],GC_SR_BEGIN)==GC_SR_MORE);
                assert(retire[i].forward_table==(uintptr_t)&sources[i]->table);
                assert(retire[i].starts==(uintptr_t)(i?middle_starts:old_starts));
                assert(lifecycle->source_retire(&g.m.f.h,&retire[i],GC_SR_STEP)==GC_SR_WAIT);
            }
            assert(!g.cycles.completed[g.queue.generation]);
            assert(lifecycle->handshake(&g.handshake,NULL,GC_HS_END)==GC_HS_OK);
            assert(lifecycle->worker(&g.root_registry,&relocation_owner,GC_WK_POLL)==GC_WK_OK);
            metadata_unlock(&g.m);

            // Admission stays closed throughout outside-lock native cleanup.
            // BEGIN has removed both tables from metadata lookup. Only after
            // freeing each backing allocation may STEP publish the range Free.
            for(unsigned i=0;i<2;i++){
                free((void *)(uintptr_t)retire[i].starts);retire[i].starts=0;
                if(i)middle_starts=NULL;else old_starts=NULL;
                free(sources[i]);sources[i]=NULL;retire[i].forward_table=0;
                source_charges[i].allocation=0;
                metadata_lock(&g.m);
                assert(budget(&g.m.f.h,&source_charges[i],GC_BUDGET_RELEASE)==GC_BUDGET_OK);
                assert(lifecycle->source_retire(&g.m.f.h,&retire[i],GC_SR_STEP)==GC_SR_DONE);
                assert(atomic_load(&retired_pages[i]->state)==GC_PAGE_FREE);
                retired_bitmap_bytes+=retire[i].page_count*(GC_HEAP_PAGE_BYTES/128);
                assert(g.relocation.epoch==frozen_epoch&&g.root_registry.phase==heap_audit_phase);
                assert(lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_CHECK)==GC_RG_OK);
                metadata_unlock(&g.m);
            }
            assert(g.m.f.h.committed==committed_before);
            assert(g.m.f.h.metadata_committed==metadata_before-retired_bitmap_bytes-2*sizeof(GraphSourceStorage));
            // Reuse only these two fully audited ranges, not the still-live
            // source run or the whole cycle. Fresh incarnations invalidate old
            // page receipts; empty bitmaps forbid resurrecting stale objects.
            for(unsigned i=0;i<2;i++){
                AllocationPage *page=retired_pages[i];
                uint64_t old_incarnation=retire[i].incarnation,old_identity=page->identity_base;
                uint64_t old_start=page->start;
                PageLease reused={.page_count=retire[i].page_count,.generation=GC_YOUNG,.relocation=1};
                metadata_lock(&g.m);
                assert(pool(&g.m.f.h,&reused,GC_POOL_RESERVE)==GC_POOL_OK);
                assert(reused.head_index==retire[i].head_index&&page->start==old_start);
                assert(reused.incarnation>old_incarnation&&page->identity_base>old_identity&&!page->forward_table);
                metadata_unlock(&g.m);
                size_t bitmap_bytes=reused.page_count*(GC_HEAP_PAGE_BYTES/128);
                void *bitmap=calloc(1,bitmap_bytes);assert(bitmap);
                pool_vm_commit((char *)mapping+page->start,reused.page_count*GC_HEAP_PAGE_BYTES);
                reused.starts=(uintptr_t)bitmap;reused.committed_mapping=1;
                metadata_lock(&g.m);
                assert(pool(&g.m.f.h,&reused,GC_POOL_COMMIT)==GC_POOL_OK);
                assert(bind(&g.m.ctx,page,0)==GC_ALLOC_OK);
                MetadataLeaseRequest stale={(uintptr_t)page,old_incarnation,0};
                assert(metadata(&g.m.leases,&stale,GC_ML_ACQUIRE)==GC_ML_INVALID&&!stale.token);
                ReferenceLease stale_object={.reference=((old_start+32)<<16)|gc_abi_color((PhaseSnapshot){heap_audit_phase})};
                assert(reference_lease(&g.m.leases,&stale_object)==GC_ML_INVALID&&!stale_object.lease.token);
                AllocationPage unchanged=*page;
                assert(lifecycle->source_retire(&g.m.f.h,&retire[i],GC_SR_STEP)==GC_SR_INVALID);
                assert(!memcmp(page,&unchanged,sizeof(unchanged)));
                MetadataLeaseRequest current={(uintptr_t)page,reused.incarnation,0};
                assert(metadata(&g.m.leases,&current,GC_ML_ACQUIRE)==GC_ML_OK);
                assert(metadata(&g.m.leases,&current,GC_ML_RELEASE)==GC_ML_OK);
                assert(!g.m.leases.active&&g.relocation.closed&&!g.cycles.completed[g.queue.generation]);
                metadata_unlock(&g.m);
                graph_reuse_allocate(&g,page,&storage->reused_records[i],&storage->reused_tables[i],&storage->reused_cells[i],allocate_into);
                metadata_lock(&g.m);
                assert(metadata(&g.m.leases,&stale,GC_ML_ACQUIRE)==GC_ML_INVALID&&!stale.token);
                metadata_unlock(&g.m);
                if(i)middle_starts=bitmap;else old_starts=bitmap;
                recommitted_bitmap_bytes+=bitmap_bytes;
            }
            assert(g.m.f.h.committed==committed_before+2*GC_HEAP_PAGE_BYTES);
            assert(g.m.f.h.metadata_committed==metadata_before-retired_bitmap_bytes-2*sizeof(GraphSourceStorage)+recommitted_bitmap_bytes);
            // The original live source now forwards directly to the surviving
            // destination, whose identity and payload remain accessible.
            ReferenceLease canonical={.reference=g.moved_owner};
            metadata_lock(&g.m);assert(reference_lease(&g.m.leases,&canonical)==GC_ML_OK);metadata_unlock(&g.m);
            ResolveRequest live={.reference=g.moved_owner,.scope={.context=(uintptr_t)&g.m.ctx}};
            assert(resolve(empty_page,&live,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            ObjectHeader *header=(ObjectHeader *)((char *)mapping+(g.moved_owner>>16)-sizeof(ObjectHeader));
            assert(header->mark_record==(uintptr_t)&g.records[6]&&header->payload_bytes==8);
            assert(g.records[6].object_id==g.objects[6].object_id&&atomic_load((_Atomic uint64_t *)(header+1)));
            assert(resolve(empty_page,&live,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
            metadata_lock(&g.m);assert(metadata(&g.m.leases,&canonical.lease,GC_ML_RELEASE)==GC_ML_OK);metadata_unlock(&g.m);
            assert(remembered->inventory(&g.remembered,&empty_request,GC_RI_DETACH)==GC_RS_WAIT);
        }else{
            metadata_lock(&g.m);assert(g.relocation.epoch==heap_audit_epoch&&
                lifecycle->relocation_gate(&g.relocation,NULL,GC_RG_CHECK)==GC_RG_OK);
            metadata_unlock(&g.m);
            assert(remembered->inventory(&g.remembered,&empty_request,GC_RI_DETACH)==GC_RS_OK);
        }
        assert(g.relocation.closed&&!g.relocation.active&&!g.cycles.completed[g.queue.generation]);
        assert(roots(&g.m.ctx,&root,GC_ROOT_RELEASE)==GC_ROOT_OK);
        graph_journal_claim(&g,&registration,&journal_lease,&journal_job,&change);
        assert(lifecycle->life(&g.root_registry,&life,GC_RL_RETIRE)==GC_RL_OK);
        assert(lifecycle->life(&g.root_registry,&life,GC_RL_CLAIM)==GC_RL_OK);
        rs=(RootScanJob){.scan={(uintptr_t)&segment,life.begin,(uintptr_t)&root_value,1,0},.lease=(uintptr_t)&root_lease,.serial=life.serial};
        assert(lifecycle->leased(&rs)==GC_ROOT_OK&&rs.scan.written==1&&!root_value);
        assert(lifecycle->life(&g.root_registry,&life,GC_RL_ACK)==GC_RL_OK);
        assert(lifecycle->life(&g.root_registry,&life,GC_RL_CLEAN)==GC_RL_WAIT);
        graph_journal_handoff(&g,&journal_job,&change);assert(graph_queue_status(&g)==GC_MARK_EMPTY);
        assert(lifecycle->life(&g.root_registry,&life,GC_RL_CLEAN)==GC_RL_OK);assert(lifecycle->clear(&life)==GC_RL_OK);
        assert(lifecycle->life(&g.root_registry,&life,GC_RL_FINISH)==GC_RL_OK);
        assert(!g.root_registry.active&&!g.root_registry.retiring&&!g.root_registry.leases&&!g.root_registry.inventory);
        assert(gc_context_transition_locked(&g.m.ctx,GC_RUNNING,GC_DETACHING)==GC_CONTEXT_OK);
        assert(lifecycle->handshake(&g.handshake,&owner,GC_HS_DETACH)==GC_HS_OK&&!g.handshake.members&&!g.handshake.head);
#ifdef _WIN32
        DeleteCriticalSection(&g.m.lock);
#else
        assert(pthread_cond_destroy(&g.metadata_available)==0);
        assert(pthread_mutex_destroy(&g.m.lock)==0);
#endif
        // All workers joined, jobs drained and records retired by fixture
        // teardown; only now may the native storage and its charge be returned.
        for(unsigned i=0;i<2;i++)if(sources[i]){
            free(sources[i]);source_charges[i].allocation=0;
            assert(budget(&g.m.f.h,&source_charges[i],GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        }
        free(storage);charge.allocation=0;
        assert(budget(&g.m.f.h,&charge,GC_BUDGET_RELEASE)==GC_BUDGET_OK);
        assert(g.m.f.h.metadata_committed==base_metadata-retired_bitmap_bytes+recommitted_bitmap_bytes&&!g.m.f.h.metadata_pending);
        free(array_starts);free(middle_starts);free(empty_starts);free(old_starts);free(starts);pool_vm_release(mapping,bytes);
    }
    puts("heap audit owner graph: 64 cooperative whole-pool walks agree on findings and release every lease PASS");
    puts("heap payload audit: 64 whole-pool scans visit eight canonical identities, skip protected sources and release every field guard PASS");
    puts("object walk graph: 64 leased whole-pool key walks cover every allocation and installed copy without touching protected payloads PASS");
    puts("heap walk graph: 64 real pools enumerate moved and protected source heads with balanced metadata leases PASS");
    puts("heap incoming graph: 32 protected-source moved owners retain child findings and exclude two old source ranges PASS");
    puts("source replacement graph: 64 rooted allocations reuse retired object addresses with new identities and balanced publication/deletion journals PASS");
    puts("source reuse graph: 64 audited ranges recommit at the same addresses with new identities, reject stale incarnation and retirement receipts, and retain live destinations PASS");
    puts("source retire graph: 64 audited source tables and bitmaps freed before teardown with balanced charges and surviving canonical identities PASS");
    puts("source range audits: 64 whole-pool forwarding findings vanish after compression; 64 heap and 32 root/inventory re-audits pass PASS");
    puts("forward compression graph: 32 joined-worker audits shorten two retained edges, preserve canonical identity and clear intermediate incoming references PASS");
    puts("mark initialization graph: 32 registered roots and copied journals retain unfinished objects across mark-end retry PASS");
    puts("allocation graph: 512 rooted allocations, 448 pre-allocation journal waits and 1088 leased observer handoffs PASS");
    puts("remembered offer pressure: 32 rooted children retain slot claims across full-pool native waits PASS");
    puts("rooted offer steps: 64 two-edge child moves permit phase rendezvous between adoptions before queue routing PASS");
    puts("journal offer steps: 64 two-edge copies retain claims and metadata through phase rendezvous and full-pool waits PASS");
    puts("scan cursor steps: 64 claimed arrays poll before retarget, 48 move twice with original receipts retained PASS");
    puts("guarded heal graph: 32 worker repairs and 32 intervening deletions survive stale CAS and array copy PASS");
    puts("mark busy offers: 64 rooted child moves admit phase rendezvous before publication and queue handoff PASS");
    puts("mark rooted offers: 64 full pools retain pending child roots across native waits and queue handoff PASS");
    puts("mark pending roots: 64 scanned child batches survive phase rendezvous before queue handoff and root retirement PASS");
    puts("mark busy forwarding: 64 pending publications admit phase rendezvous with unchanged scan claims PASS");
    puts("mark destination pressure: 22 full pools discard private forwarding results and resume original claims PASS");
    puts("mark worker pressure: 64 full pools retain queued claims across native waits before moved-array scans PASS");
    puts("mark worker retarget: 64 moved partial-array claims retain cursors and reject stale receipts, 16 protected source pages PASS");
    puts("rooted pressure: 32 intermediate owner roots survive metadata exhaustion, native waits and second moves PASS");
    puts("relocation job graph: 64 moving rounds reject premature permit release and finish every publication PASS");
    puts("relocation gate graph: 64 pending publications block source sealing and 32 incoming audits freeze admission PASS");
    puts("forward incoming graph: 32 lease-free source chains retain incoming forwarding dependencies PASS");
    puts("source uncommit graph: 64 OS decommits credit payload once while retaining leased metadata and virtual ranges PASS");
    puts("source drain graph: 64 sealed source runs reject target append after payload protection with live metadata leases PASS");
    puts("rooted access: 32 real-worker two-edge owners survive journal pressure, phase rendezvous and source decommits PASS");
    puts("remembered detach: 32 buffered and copied source leases block page-bound unlink PASS");
    puts("remembered retirement: 32 buffered source replays after transfer retain metadata and reject mark end PASS");
    puts("remembered transfer: 32 real-worker moved claims retain late records and rescan destination slots PASS");
    puts("remembered movement: 32 blocked claims reload canonical Old fields after source decommit PASS");
    puts("mark routing: 32 registered Old roots and journal deletions skip Young marking before remembered handoff PASS");
    puts("remembered workers: 32 unique Old-to-Young edges survive pending and claimed mark-end retries PASS");
    puts("remembered workers: 32 rooted phase resumptions and 16 load/release journal backpressure rounds PASS");
    puts("remembered dispatch: 32 real-worker page walks skip empty Old runs and transfer membership to slot claims PASS");
    puts("remembered dispatch: 32 late previous-epoch replays survive ACK and force a second worker pass PASS");
    puts("remembered pressure: 32 full metadata pools retain claims across blocked phase adoption and retry PASS");
    puts("guarded store: 64 actual graph edge deletions, SATB survival and lease transfers PASS");
    puts("mark phase: 64 real-worker retry and Select transitions with phase adoption PASS");
    puts("mark start: 64 prepared-epoch adoption and activation boundaries before graph traversal PASS");
    puts("mark end: 64 shared C/emitted complete-inventory graph end checks PASS");
    puts("thread inventory: 64 complete registered-owner end checks and pending-work rejections PASS");
    puts("worker rendezvous: 64 registered two-worker graphs, live-claim ACK, native adoption and root retirement rounds PASS");
    puts("root publication: 64 stopped-owner stale audit rejections and resumed journal handoffs PASS");
    puts("root journal: 64 counted pending handoffs across rendezvous and retirement rounds PASS");
    puts("buffer jobs: 64 pending SATB copies across registered graph end checks PASS");
    puts("selected relocation: 40 closed-gate candidates, 8 retained sources and 16 promotions preserve identity and payload PASS");
    puts("retire inventory graph: 8 changed inventories and 16 registered jobs prevent READY before 8 completed rendezvous pass PASS");
    puts("retire context holders: 8 unpublished buffers delay adoption and 16 published or copied jobs delay owner certification PASS");
    puts("retire root repair: 16 pre-move roots healed, 8 changed-owner certificates rejected and 8 fresh range audits pass PASS");
    puts("retire heap repair: 24 rooted field repairs survive journal pressure and 16 fresh range audits find no stale edges PASS");
    puts("relocation retire graph: 64 drained heaps adopt Retire before audits and keep admission closed and tickets unfinished PASS");
    puts("relocation phase graph: 64 selected heaps adopt two boundaries before survivor copies and keep cycle tickets unfinished PASS");
    puts("selected promotion: 8 reference-bearing Old copies retain Young child slots before publication PASS");
    puts("graph survivor selection: 64 canonical Select walks age only marked Young survivors once PASS");
    puts("cycle graph admission: 64 two-worker coalesced starts, next requests and unfinished Select tickets PASS");
    puts("root audit: 64 registered inventory scans, stale owner rejection and renewed end checks PASS");
    puts("mark prepare: 64 two-worker graph, spill and duplicate lease handoff rounds PASS");
    puts("mark graph reference leases: 64 varying run-head and multi-page array rounds PASS");
    puts("mark graph records: 64 allocator-owned record, full-arena and header-lookup rounds PASS");
    puts("mark graph metadata: 64 pre-publication limit, allocation rollback and retirement rounds PASS");
    puts("mark graph overflow: 64 bounded-capacity rounds (1/2/16 slots), deferred jobs drained PASS");
    puts("root audit deletion: 32 owner null stores survive delayed repair before explicit republish PASS");
    puts("root audit pressure: 64 registered scanners retain snapshots across 128 native metadata waits and phase retries PASS");
    puts("root walk item graph: 64 leased snapshots survive forwarding-edge polls before root repair PASS");
    puts("root walk heal graph: 64 moving-array rounds repair registered roots before mark end PASS");
    puts("mark graph: 64 two-worker root/SATB deletion, cyclic graph and bounded-array rounds PASS");
    puts("mark graph: 64 metadata lookup and guarded header/scan rounds PASS");
}
#endif
