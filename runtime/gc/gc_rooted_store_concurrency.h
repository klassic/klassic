#ifndef KLASSIC_GC_ROOTED_STORE_CONCURRENCY_H
#define KLASSIC_GC_ROOTED_STORE_CONCURRENCY_H
#include "gc_rooted_store_dispatch_cases.h"
#include "gc_barrier_poll.h"
#include "gc_remembered.h"
#include "gc_root_journal.h"
#include "gc_mark_spill.h"
#include "gc_pending_mark.h"
#ifdef _WIN32
typedef HANDLE StoreRaceThread;
static void store_race_yield(void){SwitchToThread();}
#else
#include <sched.h>
typedef pthread_t StoreRaceThread;
static void store_race_yield(void){sched_yield();}
#endif
/* Shared-slot contention with fixed live roots and a fixed mark epoch.
 * A second schedule advances snapshot sequence through a real handshake,
 * without retiring mark queues or reusing colors. Neither closes a cycle. */
typedef struct StoreRace StoreRace;
typedef struct {
    StoreRace *race;unsigned index,completed,loads,retries,rearmed;
    MutatorContext ctx;RootCell cells[3];RootSegment segment;RootRegistration registration;
    BufferCursor journal;RootChange changes[1024];BarrierEntry entries[2][256];
    ThreadRegistration participant;RootedLoadAdmission admission;
    RootedStoreRuntime runtime;RootedStore store;
} StoreRaceOwner;
struct StoreRace {
    FollowFixture heap;MetadataLeaseSlot leases[4096];AllocationPage pages[2];
    _Atomic uint64_t target_starts[128];ForwardCell target_cell;ForwardTable target_table;MarkRecord target_record;
    StoreNativeMutex locks[4];RootRegistry roots;HandshakeState handshake;
    MarkRecord record;MarkQueue queues[2];MarkTask tasks[2];
    uint64_t bitmap;TypeDescriptor descriptor;_Atomic uint64_t descriptors[2];DescriptorRegistry registry;
    RootedStoreDispatcher dispatch;RootedLoadDispatcher load_dispatch;BarrierPoller poll;PendingMarker pending_mark;StoreRaceOwner owners[8];
    MutatorContext consumer;RememberedSet remembered;uint64_t rem_bits[2][256];
    unsigned count,handshake_mode,pressure_mode,cross_page;_Atomic unsigned ready,first_done,first_failed,requested,parked,resumed,pressure,journal_pressure,finished;
    uint64_t consumed_entries[2],scanned_jobs,concurrent_jobs,scanned_generation[2],journal_entries,concurrent_journal_entries;uint64_t reference,target_reference;void *mapping;
};
static _Thread_local MutatorContext *store_race_context;
static _Thread_local uintptr_t store_race_held;
static void store_race_lock(uintptr_t address){
    StoreNativeMutex *m=(void *)address;
    /* A native coordinator has no mutator context; OS owners always bind TLS. */
    assert(!store_race_held&&(!store_race_context||!atomic_load(&store_race_context->access_epoch)));
#ifdef _WIN32
    EnterCriticalSection(&m->mutex);
#else
    assert(!pthread_mutex_lock(&m->mutex));
#endif
    assert(!m->held);m->held=1;m->acquired++;store_race_held=address;
}
static void store_race_unlock(uintptr_t address){
    StoreNativeMutex *m=(void *)address;assert(store_race_held==address&&m->held);
    m->held=0;m->released++;store_race_held=0;
#ifdef _WIN32
    LeaveCriticalSection(&m->mutex);
#else
    assert(!pthread_mutex_unlock(&m->mutex));
#endif
}
/* Copied buffer work retains its owner lease until all generation handoffs
 * finish. The referent acquires independent leases; owner and referent need
 * not share a page or generation. These fixtures do not move either object. */
static void store_race_release(StoreRace *r,MetadataLeaseRequest *lease){
    store_race_lock((uintptr_t)&r->locks[0]);
    assert(gc_metadata_step_locked(&r->heap.pool,lease,GC_ML_RELEASE)==GC_ML_OK);
    store_race_unlock((uintptr_t)&r->locks[0]);
}
static void store_race_enter(StoreRace *r,AllocationPage *page,ResolveRequest *access){
    uint64_t status;
    do{status=gc_resolve_access(page,access,GC_RESOLVE_ENTER);
        if(status==GC_RESOLVE_WAIT){assert(!atomic_load(&r->consumer.access_epoch));store_race_yield();}
    }while(status==GC_RESOLVE_WAIT);
    assert(status==GC_RESOLVE_OK);
}
static void store_race_exit(StoreRace *r,AllocationPage *page,ResolveRequest *access){
    uint64_t status;do{status=gc_resolve_access(page,access,GC_RESOLVE_EXIT);}while(status==GC_RESOLVE_WAIT);
    assert(status==GC_RESOLVE_OK&&!atomic_load(&r->consumer.access_epoch));
}
/* The producer retains a root/journal/parent or SATB obligation until both
 * generation offers finish. All objects in this graph remain canonical. */
static void store_race_mark_reference(StoreRace *r,uint64_t reference,uint64_t phase,unsigned mask){
    assert((phase&UINT16_MAX)==(atomic_load(&r->consumer.adopted_phase)&UINT16_MAX));
    /* This schedule retains both queue epochs across sequence-only handshakes. */
    for(unsigned g=0;g<2;g++)if(mask&(1u<<g)){
        PendingMark q={.context=(uintptr_t)&r->consumer,.current={.reference=reference},
            .queue=(uintptr_t)&r->queues[g],.generation=g,.epoch=r->queues[g].epoch,
            .phase=atomic_load(&r->consumer.adopted_phase)};
        while(q.stage!=5){
            uint64_t op,status;uintptr_t lock=0;
            switch(q.stage){
            case 0:op=GC_RM_ENTER;lock=(uintptr_t)&r->locks[0];break;
            case 1:op=GC_RM_ROUTE;break;
            case 2:op=GC_RM_EXIT;break;
            case 3:op=GC_RM_OFFER;lock=(uintptr_t)&r->locks[2+g];break;
            case 4:op=GC_RM_RELEASE;lock=(uintptr_t)&r->locks[0];break;
            default:abort();
            }
            if(lock)store_race_lock(lock);
            status=r->pending_mark(&r->heap.pool,&q,op);
            if(lock)store_race_unlock(lock);
            assert(status==GC_RF_ADVANCED||status==GC_RF_WAIT||status==GC_RAC_READY||
                status==GC_RM_EXIT_REQUIRED||status==GC_RM_OFFER_REQUIRED||
                status==GC_RM_RELEASE_REQUIRED||status==GC_RM_COMPLETE);
            if(status==GC_RF_WAIT){assert(!atomic_load(&r->consumer.access_epoch));store_race_yield();}
        }
        assert(q.route_result==GC_ROUTE_SKIP||
            (q.route_result==GC_ROUTE_READY&&(q.offer_result==GC_MARK_OK||q.offer_result==GC_MARK_DUPLICATE)));
        assert(!q.current.lease.page&&!q.current.lease.incarnation&&!q.current.lease.token&&
            !q.retired.page&&!q.retired.incarnation&&!q.retired.token&&!atomic_load(&r->consumer.access_epoch));
    }
}

/* Cancellation must discharge private resources without offering or consuming
 * the caller's journal obligation. Exercise before launching OS mutators. */
static void store_race_pending_cancel(StoreRace *r){
    StoreRaceOwner *owner=&r->owners[0];
    uint64_t consumed=atomic_load(&owner->journal.consumed),published=atomic_load(&owner->journal.published);
    for(unsigned mode=0;mode<3;mode++){
        PendingMark q={.context=(uintptr_t)&r->consumer,.current={.reference=r->reference},
            .queue=(uintptr_t)&r->queues[GC_OLD],.generation=GC_OLD,
            .epoch=r->queues[GC_OLD].epoch,.phase=atomic_load(&r->consumer.adopted_phase)};
        if(mode){
            store_race_lock((uintptr_t)&r->locks[0]);
            assert(r->pending_mark(&r->heap.pool,&q,GC_RM_ENTER)==GC_RAC_READY);
            store_race_unlock((uintptr_t)&r->locks[0]);
            uint64_t token=q.current.lease.token;assert(token&&r->heap.pool.active==1);
            assert(r->pending_mark(&r->heap.pool,&q,GC_RM_CANCEL)==GC_RAC_READY);
            assert(q.current.lease.token==token&&atomic_load(&r->consumer.access_epoch));
            assert(r->pending_mark(&r->heap.pool,&q,GC_RM_ROUTE)==GC_RM_EXIT_REQUIRED);
            assert(r->pending_mark(&r->heap.pool,&q,GC_RM_CANCEL)==GC_RM_EXIT_REQUIRED);
            assert(r->pending_mark(&r->heap.pool,&q,GC_RM_EXIT)==GC_RM_OFFER_REQUIRED);
            if(mode==2){
                store_race_lock((uintptr_t)&r->locks[3]);
                r->queues[GC_OLD].epoch++;
                assert(r->pending_mark(&r->heap.pool,&q,GC_RM_OFFER)==GC_RM_STALE);
                r->queues[GC_OLD].epoch--;
                store_race_unlock((uintptr_t)&r->locks[3]);
                assert(q.stage==3&&q.current.lease.token==token);
            }
        }
        store_race_lock((uintptr_t)&r->locks[0]);
        assert(r->pending_mark(&r->heap.pool,&q,GC_RM_CANCEL)==GC_RM_CANCELLED);
        store_race_unlock((uintptr_t)&r->locks[0]);
        assert(!r->heap.pool.active&&!q.current.lease.token&&!q.retired.token&&!atomic_load(&r->consumer.access_epoch));
        assert(!r->queues[GC_OLD].queued&&!r->queues[GC_OLD].overflow_count);
        assert(atomic_load(&owner->journal.consumed)==consumed&&atomic_load(&owner->journal.published)==published);
        assert(r->pending_mark(&r->heap.pool,&q,GC_RM_RESET)==GC_RF_DONE&&q.current.reference==r->reference);
    }
}
static bool store_race_consume(StoreRace *r,StoreRaceOwner *o,unsigned selector){
    BarrierEntry entry={0};BarrierJob job={.identity=o->ctx.thread_id,.selector=selector,.output=(uintptr_t)&entry,.capacity=1};
    store_race_lock((uintptr_t)&r->locks[1]);
    uint64_t status=gc_buffer_job_step(&o->ctx,&job,GC_BJ_CLAIM);
    store_race_unlock((uintptr_t)&r->locks[1]);
    if(status==GC_BJ_EMPTY)return false;
    assert(status==GC_BJ_OK);
    assert(gc_buffer_job_step(&o->ctx,&job,GC_BJ_COPY)==GC_BJ_OK&&job.written==1);
    assert((entry.slot.owner.bits>>16)==(r->reference>>16)&&!entry.slot.byte_offset&&entry.slot.record_epoch==0x10022);
    MetadataLeaseRequest lease={.token=entry.slot.metadata_lease};
    store_race_lock((uintptr_t)&r->locks[0]);
    assert(gc_metadata_step_locked(&r->heap.pool,&lease,GC_ML_RESOLVE)==GC_ML_OK);
    store_race_unlock((uintptr_t)&r->locks[0]);
    assert(lease.page==(uintptr_t)&r->pages[0]&&lease.incarnation==entry.slot.page_incarnation);
    if(!selector){
        assert(entry.previous.bits&&(entry.previous.bits>>16)==(r->target_reference>>16));
        /* The copied SATB job stays counted, and owns its original slot lease,
         * until independent referent handoffs have completed in both queues. */
        store_race_mark_reference(r,entry.previous.bits,entry.slot.record_epoch,3);
        MetadataLeaseRequest retained={.token=lease.token};
        store_race_lock((uintptr_t)&r->locks[0]);
        assert(gc_metadata_step_locked(&r->heap.pool,&retained,GC_ML_RESOLVE)==GC_ML_OK);
        store_race_unlock((uintptr_t)&r->locks[0]);
        assert(retained.page==lease.page&&retained.incarnation==lease.incarnation);
        assert(job.written==1);
    }else{
        ResolveRequest access={.reference=entry.slot.owner.bits,.scope={.context=(uintptr_t)&r->consumer}};
        store_race_enter(r,&r->pages[0],&access);
        MutatorContext *ctx;_Atomic uint64_t *slot;
        assert(gc_guarded_slot((uintptr_t)&access.scope,lease.page,(uintptr_t)&r->registry,entry.slot.byte_offset,entry.slot.record_epoch,&ctx,&slot));
        assert(ctx==&r->consumer);
        assert((uintptr_t)slot-r->heap.heap.heap_base==(entry.slot.owner.bits>>16)+entry.slot.byte_offset);
        store_race_exit(r,&r->pages[0],&access);
        RememberedJob record={.offset=(entry.slot.owner.bits>>16)+entry.slot.byte_offset,.incarnation=lease.incarnation,.phase=entry.slot.record_epoch};
        store_race_lock((uintptr_t)&r->locks[1]);
        assert(gc_remembered_step_locked(&r->remembered,&record,GC_RS_RECORD)==GC_RS_OK);
        store_race_unlock((uintptr_t)&r->locks[1]);
    }
    store_race_release(r,&lease);
    store_race_lock((uintptr_t)&r->locks[1]);
    assert(gc_buffer_job_step(&o->ctx,&job,GC_BJ_ACK)==GC_BJ_OK);
    store_race_unlock((uintptr_t)&r->locks[1]);
    r->consumed_entries[selector]++;if(atomic_load(&r->finished)<r->count)r->concurrent_jobs++;
    return true;
}
static bool store_race_scan(StoreRace *r,unsigned g){
    MarkRequest work={0};store_race_lock((uintptr_t)&r->locks[2+g]);
    uint64_t refill=gc_mark_spill_locked(&r->queues[g],NULL,GC_MARK_REFILL);
    assert(refill==GC_MARK_OK||refill==GC_MARK_EMPTY||refill==GC_MARK_FULL);
    uint64_t status=gc_mark_step_locked(&r->queues[g],&work,GC_MARK_TAKE);
    store_race_unlock((uintptr_t)&r->locks[2+g]);
    if(status==GC_MARK_EMPTY)return false;assert(status==GC_MARK_OK);
    MetadataLeaseRequest lease={.token=work.metadata_lease};
    store_race_lock((uintptr_t)&r->locks[0]);
    assert(gc_metadata_step_locked(&r->heap.pool,&lease,GC_ML_RESOLVE)==GC_ML_OK);
    store_race_unlock((uintptr_t)&r->locks[0]);
    AllocationPage *page=(void *)(uintptr_t)lease.page;
    ResolveRequest access={.reference=work.reference,.scope={.context=(uintptr_t)&r->consumer}};
    store_race_enter(r,page,&access);
    uint64_t child=0;MarkScan scan={(uintptr_t)&work,r->heap.heap.heap_base,(uintptr_t)&r->registry,(uintptr_t)&child,1,0,0,0,0};
    assert(gc_mark_scan_slice(&scan)==GC_SCAN_DONE);
    if(scan.written)assert(!child||(child>>16)==(r->target_reference>>16));
    store_race_exit(r,page,&access);
    if(child)store_race_mark_reference(r,child,atomic_load(&r->consumer.adopted_phase),1u<<g);
    store_race_lock((uintptr_t)&r->locks[2+g]);
    work.cursor=scan.next_cursor;assert(gc_mark_step_locked(&r->queues[g],&work,GC_MARK_FINISH)==GC_MARK_OK);
    store_race_unlock((uintptr_t)&r->locks[2+g]);
    store_race_release(r,&lease);r->scanned_jobs++;r->scanned_generation[g]++;return true;
}
/* Joined mutators leave their registered scopes live until all root and
 * journal obligations are handed to the retained marking epochs. */
static void store_race_drain_marks(StoreRace *r){
    for(unsigned g=0;g<2;g++)while(store_race_scan(r,g)){}
}
static uint64_t store_race_life(StoreRace *r,RootLifeRequest *q,uint64_t op){
    store_race_lock((uintptr_t)&r->locks[1]);
    uint64_t result=gc_root_life_locked(&r->roots,q,op);
    store_race_unlock((uintptr_t)&r->locks[1]);return result;
}
/* One bounded registered-journal batch. COPY and referent processing run
 * outside the registry mutex. The lease retains ring entries until both
 * generation handoffs and all temporary lease releases finish. */
static bool store_race_journal(StoreRace *r,StoreRaceOwner *o,RootLifeRequest *retiring){
    RootLease lease={0};RootChange changes[2];
    RootJournalRequest journal={.registration=(uintptr_t)&o->registration,.lease=(uintptr_t)&lease,
        .scope=o->segment.scope,.output=(uintptr_t)changes,.capacity=2};
    store_race_lock((uintptr_t)&r->locks[1]);
    uint64_t status=gc_root_journal_step(&r->roots,&journal,GC_RJ_CLAIM);
    store_race_unlock((uintptr_t)&r->locks[1]);
    if(status==GC_RJ_EMPTY)return false;
    assert(status==GC_RJ_OK&&gc_root_journal_step(&r->roots,&journal,GC_RJ_COPY)==GC_RJ_OK);
    assert(atomic_load(&o->journal.consumed)==journal.begin);
    if(retiring)assert(store_race_life(r,retiring,GC_RL_CLEAN)==GC_RL_WAIT);
    for(uint64_t j=0;j<journal.written;j++){
        store_race_mark_reference(r,changes[j].previous,changes[j].phase,3);store_race_drain_marks(r);
        store_race_mark_reference(r,changes[j].next,changes[j].phase,3);store_race_drain_marks(r);
    }
    assert(atomic_load(&o->journal.consumed)==journal.begin);
    store_race_lock((uintptr_t)&r->locks[1]);
    assert(gc_root_journal_step(&r->roots,&journal,GC_RJ_ACK)==GC_RJ_OK);
    store_race_unlock((uintptr_t)&r->locks[1]);
    r->journal_entries+=journal.written;
    if(atomic_load(&r->finished)<r->count)r->concurrent_journal_entries+=journal.written;
    return true;
}
static void store_race_retire_roots(StoreRace *r,StoreRaceOwner *o){
    RootLease lease={0};RootLifeRequest q={.registration=(uintptr_t)&o->registration,
        .context=(uintptr_t)&o->ctx,.scope=o->segment.scope,.lease=(uintptr_t)&lease};
    assert(store_race_life(r,&q,GC_RL_RETIRE)==GC_RL_OK);
    assert(store_race_life(r,&q,GC_RL_CLAIM)==GC_RL_OK&&q.mask==3);
    uint64_t values[3]={0};RootScanJob scan={.scan={(uintptr_t)&o->segment,q.begin,(uintptr_t)values,3,0},
        .lease=(uintptr_t)&lease,.serial=q.serial};
    assert(gc_root_scan_leased(&scan)==GC_ROOT_OK&&scan.scan.written==3);
    for(unsigned j=0;j<3;j++){
        store_race_drain_marks(r);store_race_mark_reference(r,values[j],q.phase,(unsigned)q.mask);
    }
    store_race_drain_marks(r);
    assert(store_race_life(r,&q,GC_RL_ACK)==GC_RL_OK);
    while(store_race_journal(r,o,&q)){}
    assert(store_race_life(r,&q,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&q)==GC_RL_OK);
    assert(store_race_life(r,&q,GC_RL_FINISH)==GC_RL_OK);
    assert(!o->ctx.root_segments&&!o->ctx.retiring_roots);
    for(unsigned j=0;j<3;j++)assert(!o->cells[j].active&&!atomic_load(&o->cells[j].value));
    atomic_store(&o->ctx.state,GC_DETACHING);
    store_race_lock((uintptr_t)&r->locks[1]);
    assert(gc_handshake_step_locked(&r->handshake,&o->participant,GC_HS_DETACH)==GC_HS_OK);
    store_race_unlock((uintptr_t)&r->locks[1]);
}
#ifdef _WIN32
static DWORD WINAPI store_race_consumer(void *argument){
#else
static void *store_race_consumer(void *argument){
#endif
    StoreRace *r=argument;store_race_context=&r->consumer;
    /* Make FULL unavoidable before the consumer starts freeing ring space. */
    while(!atomic_load(&r->pressure)&&!atomic_load(&r->journal_pressure))store_race_yield();
    for(;;){
        bool done=atomic_load(&r->finished)==r->count,worked=false;
        for(unsigned i=0;i<r->count;i++)for(unsigned selector=0;selector<2;selector++)worked=store_race_consume(r,&r->owners[i],selector)||worked;
        for(unsigned g=0;g<2;g++)worked=store_race_scan(r,g)||worked;
        if(atomic_load(&r->journal_pressure))for(unsigned i=0;i<r->count;i++)worked=store_race_journal(r,&r->owners[i],NULL)||worked;
        if(done&&!worked)break;
        if(!worked)store_race_yield();
    }
    assert(!atomic_load(&r->consumer.access_epoch));return 0;
}
/* Owner cleanup must finish before ACK. Only phase sequence changes here:
 * the old mark queue and its leases deliberately remain live across END. */
static void store_race_handshake(StoreRaceOwner *o){
    StoreRace *r=o->race;WorkerRequest worker={(uintptr_t)&r->handshake,(uintptr_t)&o->participant};
    while(!atomic_load(&r->requested))store_race_yield();
    store_race_lock(o->runtime.registry_lock);
    assert(gc_worker_step_locked(&r->roots,&worker,GC_WK_POLL)==GC_WK_RETRY);
    assert(atomic_load(&o->ctx.state)==GC_RUNNING&&atomic_load(&o->ctx.private_producers));
    store_race_unlock(o->runtime.registry_lock);
    /* Admit the physical attempt, then cancel with its guard and leases live.
     * No wait or lock acquisition occurs until cleanup has exited the guard. */
    unsigned ticks=0;
    while(o->store.stage!=GC_RS_ATTEMPT||o->store.attempt.stage!=1){
        assert(++ticks<10000);
        uint64_t status=r->dispatch(&o->runtime,&o->store,GC_RSD_STEP);
        assert(status==GC_RLOAD_HEAP||status==GC_RLOAD_ACCESS||status==GC_RLOAD_WAIT);
        if(status==GC_RLOAD_WAIT){assert(!atomic_load(&o->ctx.access_epoch));store_race_yield();}
    }
    assert(atomic_load(&o->ctx.access_epoch)&&o->store.attempt.receiver.follow.cleanup.token);
    uint64_t parked;
    do{
        assert(++ticks<10000);parked=r->poll(&o->runtime,&o->store,1);
        assert(parked==GC_RLOAD_HEAP||parked==GC_RLOAD_ACCESS||parked==GC_BP_PARK);
    }while(parked!=GC_BP_PARK);
    assert(gc_rooted_store_next(&o->store)==GC_RLOAD_CANCELLED);
    assert(!atomic_load(&o->ctx.access_epoch)&&!atomic_load(&o->ctx.private_producers));
    assert(atomic_load(&o->ctx.adopted_phase)==UINT64_C(0x20022));
    RootedStore before=o->store;
    assert(r->dispatch(&o->runtime,&o->store,GC_RSD_REARM)==GC_RLOAD_INVALID);
    assert(!memcmp(&before,&o->store,sizeof(before)));
    atomic_fetch_add(&r->parked,1);
    while(!atomic_load(&r->resumed))store_race_yield();
    assert(r->poll(&o->runtime,&o->store,1)==GC_BP_READY);
    assert(!o->store.admitted&&!o->store.value.admitted&&!o->store.value.marks.phase);
    assert(r->dispatch(&o->runtime,&o->store,GC_RSD_STEP)==GC_RLOAD_HEAP);
    assert(o->store.value.admitted&&o->store.value.marks.phase==UINT64_C(0x20022));
    o->rearmed++;
}
static void store_race_wait_flush(StoreRaceOwner *o){
    StoreRace *r=o->race;assert(r->pressure_mode&&!atomic_load(&o->ctx.access_epoch));
    bool journal=o->journal.top-atomic_load(&o->journal.consumed)==o->journal.capacity;
    if(journal)atomic_fetch_add(&r->journal_pressure,1);
    else atomic_fetch_add(&r->pressure,1);
    while(o->journal.top-atomic_load(&o->journal.consumed)==o->journal.capacity||
          o->ctx.satb.top-atomic_load(&o->ctx.satb.consumed)==o->ctx.satb.capacity||
          o->ctx.remembered.top-atomic_load(&o->ctx.remembered.consumed)==o->ctx.remembered.capacity)store_race_yield();
}
#ifdef _WIN32
static DWORD WINAPI store_race_run(void *argument){
#else
static void *store_race_run(void *argument){
#endif
    StoreRaceOwner *o=argument;StoreRace *r=o->race;store_race_context=&o->ctx;
    RootRequest receiver={.segment=(uintptr_t)&o->segment,.handle={(uintptr_t)&o->cells[0],1,o->segment.scope}};
    RootRequest value=receiver,observed=receiver;value.handle.cell=(uintptr_t)&o->cells[1];observed.handle.cell=(uintptr_t)&o->cells[2];
    for(unsigned iteration=0;iteration<16;iteration++){
        o->store=(RootedStore){.value.observation={.receiver.follow={.context=(uintptr_t)&o->ctx,.root=value},.output=value},
            .observation={.receiver.follow={.context=(uintptr_t)&o->ctx,.root=receiver},.registry=(uintptr_t)&r->registry,.output=observed}};
        bool gated=false,retry_gated=false;unsigned ticks=0;
        while(gc_rooted_store_next(&o->store)!=GC_RLOAD_DONE){
            /* Everyone captures the same first value. Wait only AFTER leaving
             * the observation guard; owner zero then clears the slot. All
             * other first attempts must lose their stale expected-value CAS. */
            if(!iteration&&!gated&&o->store.stage==GC_RS_INIT){
                assert(!atomic_load(&o->ctx.access_epoch));gated=true;atomic_fetch_add(&r->ready,1);
                if(r->handshake_mode)store_race_handshake(o);
                else{
                    while(atomic_load(&r->ready)!=r->count)store_race_yield();
                    if(o->index)while(!atomic_load(&r->first_done))store_race_yield();
                }
            }
            if(!r->handshake_mode&&!iteration&&o->index&&!retry_gated&&o->store.stage==GC_RS_RETRY){
                assert(!atomic_load(&o->ctx.access_epoch));retry_gated=true;atomic_fetch_add(&r->first_failed,1);
                while(atomic_load(&r->first_failed)!=r->count-1)store_race_yield();
            }
            assert(++ticks<10000);
            uint64_t result=r->dispatch(&o->runtime,&o->store,GC_RSD_STEP);
            if(result==GC_RLOAD_FLUSH&&r->pressure_mode){
                store_race_wait_flush(o);
                continue;
            }
            if(result==GC_RLOAD_WAIT){assert(!atomic_load(&o->ctx.access_epoch));store_race_yield();continue;}
            if(result!=GC_RLOAD_HEAP&&result!=GC_RLOAD_ACCESS&&result!=GC_RLOAD_QUEUE&&result!=GC_RLOAD_DONE)fprintf(stderr,"store race n=%u owner=%u iteration=%u tick=%u result=%llu stage=%llu last=%llu child=%llu\n",r->count,o->index,iteration,ticks,(unsigned long long)result,(unsigned long long)o->store.stage,(unsigned long long)o->store.last,(unsigned long long)o->store.value.stage);
            assert(result==GC_RLOAD_HEAP||result==GC_RLOAD_ACCESS||result==GC_RLOAD_QUEUE||result==GC_RLOAD_DONE);
            if(o->store.stage==GC_RS_RETRY)o->retries++;
        }
        assert(!atomic_load(&o->ctx.access_epoch)&&!atomic_load(&o->ctx.private_producers)&&!store_race_held);
        RootedLoad load={.observation={.receiver.follow={.context=(uintptr_t)&o->ctx,.root=receiver},.registry=(uintptr_t)&r->registry,.output=observed}};
        unsigned reads=0;
        while(gc_rooted_load_next(&load)!=GC_RLOAD_DONE){
            assert(++reads<10000);
            uint64_t status=r->load_dispatch(&o->runtime,&load,GC_RLD_STEP);
            if(status==GC_RLOAD_FLUSH&&r->pressure_mode){store_race_wait_flush(o);continue;}
            assert(status==GC_RLOAD_HEAP||status==GC_RLOAD_ACCESS||status==GC_RLOAD_QUEUE||status==GC_RLOAD_DONE||status==GC_RLOAD_WAIT);
            if(status==GC_RLOAD_WAIT){assert(!atomic_load(&o->ctx.access_epoch));store_race_yield();}
        }
        uint64_t value_bits=atomic_load(&o->cells[2].value);
        assert(!value_bits||(value_bits>>16)==(r->target_reference>>16));
        assert(!atomic_load(&o->ctx.access_epoch)&&!atomic_load(&o->ctx.private_producers));o->loads++;
        o->completed++;
        if(!iteration&&!o->index)atomic_store(&r->first_done,1);
    }
    atomic_fetch_add(&r->finished,1);return 0;
}
static void gc_run_rooted_store_concurrency(RootedStoreDispatcher candidate,RootedLoadDispatcher load_candidate,BarrierPoller poll_candidate,PendingMarker mark_candidate){
    const unsigned counts[]={1,2,8};
    for(unsigned impl=0;impl<2;impl++)for(unsigned c=0;c<3;c++)for(unsigned rendezvous=0;rendezvous<4;rendezvous++){
        StoreRace *r=calloc(1,sizeof(*r));assert(r);r->count=counts[c];r->handshake_mode=rendezvous==1;r->pressure_mode=rendezvous>=2;r->cross_page=rendezvous==3;r->dispatch=impl?candidate:gc_rooted_store_dispatch;r->load_dispatch=impl?load_candidate:gc_rooted_load_dispatch;r->poll=impl?poll_candidate:gc_barrier_poll;r->pending_mark=impl?mark_candidate:gc_pending_mark;
        follow_fixture(&r->heap,0x10022,1);FollowFixture *f=&r->heap;
        r->mapping=pool_vm_reserve((2+r->cross_page)*GC_HEAP_PAGE_BYTES);pool_vm_commit((char *)r->mapping+GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES);
        f->heap.heap_base=f->page.heap_base=(uintptr_t)r->mapping;f->page.generation=GC_OLD;
        r->pages[0]=f->page;f->heap.pages=(uintptr_t)r->pages;f->heap.page_count=1+r->cross_page;
        f->pool.slots=(uintptr_t)r->leases;f->pool.capacity=4096;
        for(unsigned i=0;i<4095;i++)r->leases[i].next_free=i+2;
        r->reference=atomic_load(&f->cell.value);r->target_reference=r->reference;r->record.object_id=1;
        if(r->cross_page){
            pool_vm_commit((char *)r->mapping+2*GC_HEAP_PAGE_BYTES,GC_HEAP_PAGE_BYTES);
            r->pages[1]=(AllocationPage){.heap_base=f->heap.heap_base,.start=2*GC_HEAP_PAGE_BYTES,.limit=3*GC_HEAP_PAGE_BYTES,
                .incarnation=28,.identity_base=2048,.generation=GC_YOUNG,.state=GC_PAGE_CLOSED,.starts=(uintptr_t)r->target_starts,
                .starts_words=128,.allocated_top=2*GC_HEAP_PAGE_BYTES+48,.run_head=1,.forward_table=(uintptr_t)&r->target_table};
            atomic_store(&r->target_starts[0],4);r->target_table=(ForwardTable){(uintptr_t)&r->target_cell,1,1};
            r->target_cell=(ForwardCell){2*GC_HEAP_PAGE_BYTES+32,28,0,0};r->target_record.object_id=2;
            r->target_reference=((2*GC_HEAP_PAGE_BYTES+32)<<16)|UINT64_C(0x1510);
            ObjectHeader *target=(void *)((char *)r->mapping+2*GC_HEAP_PAGE_BYTES);
            *target=(ObjectHeader){.payload_bytes=8,.descriptor_id=1,.mark_record=(uintptr_t)&r->target_record,.state=GC_OBJECT_PUBLISHED};
            atomic_store((_Atomic uint64_t *)(target+1),0);
        }
        ObjectHeader *h=(void *)((char *)r->mapping+f->page.start);
        *h=(ObjectHeader){.payload_bytes=8,.descriptor_id=1,.mark_record=(uintptr_t)&r->record,.state=GC_OBJECT_PUBLISHED};
        _Atomic uint64_t *field=(void *)((char *)h+sizeof(*h));atomic_store(field,r->target_reference);
        r->bitmap=1;r->descriptor=(TypeDescriptor){.id=1,.alignment=8,.fixed_bytes=8,.fixed_bitmap=(uintptr_t)&r->bitmap,.fixed_bitmap_words=1};
        atomic_store(&r->descriptors[1],(uintptr_t)&r->descriptor);r->registry=(DescriptorRegistry){(uintptr_t)r->descriptors,2};
        r->roots.phase=0x10022;
        r->consumer=(MutatorContext){.abi_version=GC_ABI_VERSION,.thread_id=100,.adopted_phase=r->roots.phase};
        r->remembered=(RememberedSet){.start=f->page.start,.limit=f->page.limit,.incarnation=f->page.incarnation,
            .bits={(uintptr_t)r->rem_bits[0],(uintptr_t)r->rem_bits[1]},.words=256,.epoch=r->roots.phase,.last_phase=r->roots.phase};
        for(unsigned g=0;g<2;g++)r->queues[g]=(MarkQueue){.tasks=(uintptr_t)&r->tasks[g],.capacity=1,.free_head=1,.epoch=1,.generation=g};
        for(unsigned i=0;i<4;i++){
#ifdef _WIN32
            InitializeCriticalSection(&r->locks[i].mutex);
#else
            assert(!pthread_mutex_init(&r->locks[i].mutex,NULL));
#endif
        }
        StoreRaceThread threads[8];
        for(unsigned i=0;i<r->count;i++){
            StoreRaceOwner *o=&r->owners[i];o->race=r;o->index=i;
            o->ctx=(MutatorContext){.abi_version=GC_ABI_VERSION,.adopted_phase=r->roots.phase};
            o->participant.context=(uintptr_t)&o->ctx;
            assert(gc_handshake_step_locked(&r->handshake,&o->participant,GC_HS_REGISTER)==GC_HS_OK);
            o->segment=(RootSegment){.owner=o->ctx.thread_id,.cells=(uintptr_t)o->cells,.capacity=3,.journal=(uintptr_t)&o->journal};
            o->journal=(BufferCursor){.entries=(uintptr_t)o->changes,.capacity=r->pressure_mode?8:1024};
            o->registration.segment=(uintptr_t)&o->segment;
            RootLifeRequest attach={.registration=(uintptr_t)&o->registration,.context=(uintptr_t)&o->ctx};
            store_race_lock((uintptr_t)&r->locks[1]);
            assert(gc_root_life_locked(&r->roots,&attach,GC_RL_ATTACH)==GC_RL_OK);
            store_race_unlock((uintptr_t)&r->locks[1]);
            for(unsigned j=0;j<3;j++){
                RootRequest root={.segment=(uintptr_t)&o->segment};
                assert(gc_root_step(&o->ctx,&root,GC_ROOT_RESERVE)==GC_ROOT_OK);
                root.value=j==0?r->reference:j==1&&i?r->target_reference:0;
                assert(gc_root_step(&o->ctx,&root,GC_ROOT_STORE)==GC_ROOT_OK);
            }
            assert(o->ctx.root_segments==(uintptr_t)&o->registration);
            o->ctx.satb=(BufferCursor){.entries=(uintptr_t)o->entries[0],.capacity=r->pressure_mode?1:256};
            o->ctx.remembered=(BufferCursor){.entries=(uintptr_t)o->entries[1],.capacity=r->pressure_mode?1:256};
            o->admission=(RootedLoadAdmission){(uintptr_t)&r->roots,(uintptr_t)&r->handshake,(uintptr_t)&o->participant,{(uintptr_t)&r->queues[0],(uintptr_t)&r->queues[1]}};
            o->runtime=(RootedStoreRuntime){(uintptr_t)&f->pool,(uintptr_t)&o->admission,(uintptr_t)&r->locks[0],(uintptr_t)&r->locks[1],
                {(uintptr_t)&r->locks[2],(uintptr_t)&r->locks[3]},(uintptr_t)store_race_lock,(uintptr_t)store_race_unlock};
        }
        store_race_pending_cancel(r);
        StoreRaceThread consumer;
        if(r->pressure_mode){
#ifdef _WIN32
            consumer=CreateThread(NULL,0,store_race_consumer,r,0,NULL);assert(consumer);
#else
            assert(!pthread_create(&consumer,NULL,store_race_consumer,r));
#endif
        }
        for(unsigned i=0;i<r->count;i++){
#ifdef _WIN32
            threads[i]=CreateThread(NULL,0,store_race_run,&r->owners[i],0,NULL);assert(threads[i]);
#else
            assert(!pthread_create(&threads[i],NULL,store_race_run,&r->owners[i]));
#endif
        }
        if(r->handshake_mode){
            while(atomic_load(&r->ready)!=r->count)store_race_yield();
            store_race_lock((uintptr_t)&r->locks[1]);
            r->roots.phase=UINT64_C(0x20022);
            assert(gc_handshake_step_locked(&r->handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
            assert(!r->handshake.acknowledged);
            store_race_unlock((uintptr_t)&r->locks[1]);
            atomic_store(&r->requested,1);
            while(atomic_load(&r->parked)!=r->count)store_race_yield();
            assert(atomic_load(field)==r->reference);
            store_race_lock((uintptr_t)&r->locks[0]);
            assert(f->pool.active==(r->count>1?1u:0u)&&r->pages[0].metadata_leases==f->pool.active);
            store_race_unlock((uintptr_t)&r->locks[0]);
            store_race_lock((uintptr_t)&r->locks[3]);
            assert(r->queues[GC_OLD].queued==(r->count>1?1u:0u)&&!r->queues[GC_OLD].in_flight);
            store_race_unlock((uintptr_t)&r->locks[3]);
            store_race_lock((uintptr_t)&r->locks[1]);
            assert(r->handshake.acknowledged==r->count);
            for(unsigned i=0;i<r->count;i++)assert(!atomic_load(&r->owners[i].ctx.access_epoch)&&!atomic_load(&r->owners[i].ctx.private_producers));
            assert(gc_handshake_step_locked(&r->handshake,NULL,GC_HS_END)==GC_HS_OK);
            store_race_unlock((uintptr_t)&r->locks[1]);
            atomic_store(&r->resumed,1);
        }
        for(unsigned i=0;i<r->count;i++){
#ifdef _WIN32
            assert(WaitForSingleObject(threads[i],30000)==WAIT_OBJECT_0);assert(CloseHandle(threads[i]));
#else
            assert(!pthread_join(threads[i],NULL));
#endif
        }
        if(r->pressure_mode){
#ifdef _WIN32
            assert(WaitForSingleObject(consumer,30000)==WAIT_OBJECT_0);assert(CloseHandle(consumer));
#else
            assert(!pthread_join(consumer,NULL));
#endif
            assert(atomic_load(&r->pressure)&&r->concurrent_jobs&&r->consumed_entries[0]&&r->consumed_entries[1]&&r->scanned_jobs);
            assert(atomic_load(&r->journal_pressure)&&r->concurrent_journal_entries);
            for(unsigned i=0;i<r->count;i++){
                assert(r->owners[i].journal.top>r->owners[i].journal.capacity);
                assert(atomic_load(&r->owners[i].journal.consumed)==atomic_load(&r->owners[i].journal.published));
            }
            assert(!r->roots.leases&&!r->queues[0].overflow_count&&!r->queues[1].overflow_count);
            if(r->cross_page)assert(r->scanned_generation[0]&&r->scanned_generation[1]);
            assert(!r->queues[0].queued&&!r->queues[0].in_flight&&!r->queues[1].queued&&!r->queues[1].in_flight);
            assert(r->remembered.current_count==1&&r->rem_bits[0][0]==16);
            for(unsigned i=0;i<r->count;i++)assert(atomic_load(&r->owners[i].ctx.satb.consumed)==atomic_load(&r->owners[i].ctx.satb.published)&&
                atomic_load(&r->owners[i].ctx.remembered.consumed)==atomic_load(&r->owners[i].ctx.remembered.published));
        }
        assert(atomic_load(field)==0||(atomic_load(field)>>16)==(r->target_reference>>16));
        /* All referents remain strongly rooted in this fixed graph. Consume
         * complete jobs and release each transferred token before teardown. */
        for(unsigned i=0;i<r->count;i++){
            StoreRaceOwner *o=&r->owners[i];assert(o->completed==16&&o->loads==16);if(i&&!r->handshake_mode)assert(o->retries);assert(o->rearmed==r->handshake_mode);
            for(unsigned selector=0;selector<2;selector++){
                BufferCursor *b=selector?&o->ctx.remembered:&o->ctx.satb;
                while(atomic_load(&b->consumed)<atomic_load(&b->published)){
                    BarrierEntry entry={0};BarrierJob job={.identity=o->ctx.thread_id,.selector=selector,.output=(uintptr_t)&entry,.capacity=1};
                    assert(gc_buffer_job_step(&o->ctx,&job,GC_BJ_CLAIM)==GC_BJ_OK);
                    assert(gc_buffer_job_step(&o->ctx,&job,GC_BJ_COPY)==GC_BJ_OK&&job.written==1);
                    assert((entry.slot.owner.bits>>16)==(r->reference>>16)&&!entry.slot.byte_offset);
                    MetadataLeaseRequest lease={.token=entry.slot.metadata_lease};
                    assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_RESOLVE)==GC_ML_OK);
                    assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
                    assert(gc_buffer_job_step(&o->ctx,&job,GC_BJ_ACK)==GC_BJ_OK);
                }
            }
        }
        for(unsigned g=0;g<2;g++)if(r->queues[g].queued){
            MarkRequest work={0};assert(gc_mark_step_locked(&r->queues[g],&work,GC_MARK_TAKE)==GC_MARK_OK);
            MetadataLeaseRequest lease={.token=work.metadata_lease};assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_RESOLVE)==GC_ML_OK);
            ResolveRequest access={.reference=work.reference,.scope={.context=(uintptr_t)&r->owners[0].ctx}};
            assert(gc_resolve_access(&r->pages[0],&access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            uint64_t child=0;MarkScan scan={(uintptr_t)&work,(uintptr_t)r->mapping,(uintptr_t)&r->registry,(uintptr_t)&child,1,0,0,0,0};
            assert(gc_mark_scan_slice(&scan)==GC_SCAN_DONE);if(scan.written)assert(!child||(child>>16)==(r->reference>>16));
            assert(gc_resolve_access(&r->pages[0],&access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);work.cursor=scan.next_cursor;
            assert(gc_mark_step_locked(&r->queues[g],&work,GC_MARK_FINISH)==GC_MARK_OK);
            assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_RELEASE)==GC_ML_OK);
        }
        {
            StoreRaceOwner *o=&r->owners[0];store_race_context=&o->ctx;
            RootRequest receiver={.segment=(uintptr_t)&o->segment,.handle={(uintptr_t)&o->cells[0],1,o->segment.scope}},out=receiver;
            out.handle.cell=(uintptr_t)&o->cells[2];
            RootedLoad load={.observation={.receiver.follow={.context=(uintptr_t)&o->ctx,.root=receiver},.registry=(uintptr_t)&r->registry,.output=out}};
            RootedStoreRuntime bad=o->runtime;bad.admission=0;RootedLoad saved=load;
            assert(r->load_dispatch(&bad,&load,GC_RLD_STEP)==GC_RLOAD_INVALID&&!memcmp(&saved,&load,sizeof(load)));
            unsigned ticks=0;
            while(load.stage!=GC_RLOAD_MARK_INIT){assert(++ticks<100);uint64_t result=r->load_dispatch(&o->runtime,&load,GC_RLD_STEP);assert(result==GC_RLOAD_HEAP||result==GC_RLOAD_ACCESS);}
            uint64_t captured=atomic_load(&o->cells[2].value);
            (void)r->load_dispatch(&o->runtime,&load,GC_RLD_CANCEL);
            while(gc_rooted_load_next(&load)!=GC_RLOAD_CANCELLED){assert(++ticks<150);(void)r->load_dispatch(&o->runtime,&load,GC_RLD_STEP);}
            saved=load;atomic_store(&o->ctx.state,GC_AT_SAFEPOINT);
            assert(r->load_dispatch(&o->runtime,&load,GC_RLD_REARM)==GC_RLOAD_INVALID&&!memcmp(&saved,&load,sizeof(load)));
            atomic_store(&o->ctx.state,GC_RUNNING);
            ResolveRequest access={.reference=r->reference,.scope={.context=(uintptr_t)&o->ctx}};
            assert(gc_resolve_access(&r->pages[0],&access,GC_RESOLVE_ENTER)==GC_RESOLVE_OK);
            uint64_t changed=captured?0:r->target_reference;atomic_store(field,changed);
            assert(gc_resolve_access(&r->pages[0],&access,GC_RESOLVE_EXIT)==GC_RESOLVE_OK);
            assert(r->poll(&o->runtime,&load,0)==GC_BP_READY&&load.stage==GC_RLOAD_RECAPTURE);
            assert(r->load_dispatch(&o->runtime,&load,GC_RLD_STEP)==GC_RLOAD_HEAP&&load.admitted&&load.stage==GC_RLOAD_MARK_INIT);
            while(gc_rooted_load_next(&load)!=GC_RLOAD_DONE){assert(++ticks<250);uint64_t result=r->load_dispatch(&o->runtime,&load,GC_RLD_STEP);assert(result==GC_RLOAD_HEAP||result==GC_RLOAD_ACCESS||result==GC_RLOAD_QUEUE||result==GC_RLOAD_DONE);}
            assert((atomic_load(&o->cells[2].value)>>16)==(captured>>16)&&atomic_load(field)==changed);
            assert(!atomic_load(&o->ctx.private_producers)&&!atomic_load(&o->ctx.access_epoch));
            RootedLoad terminal=load;assert(r->poll(&o->runtime,&load,0)==GC_RLOAD_DONE&&!memcmp(&terminal,&load,sizeof(load)));
            assert(r->poll(&o->runtime,&load,2)==GC_RLOAD_INVALID&&!memcmp(&terminal,&load,sizeof(load)));
            store_race_context=NULL;
        }
        assert(r->roots.active==r->count);
        for(unsigned i=0;i<r->count;i++)store_race_retire_roots(r,&r->owners[i]);
        assert(!r->roots.active&&!r->roots.retiring&&!r->roots.leases&&!r->roots.inventory&&!r->handshake.members);
        assert(!f->pool.active&&!r->pages[0].metadata_leases&&!r->pages[1].metadata_leases);
        for(unsigned i=0;i<4;i++){
            assert(!r->locks[i].held&&r->locks[i].acquired==r->locks[i].released);
#ifdef _WIN32
            DeleteCriticalSection(&r->locks[i].mutex);
#else
            assert(!pthread_mutex_destroy(&r->locks[i].mutex));
#endif
        }
        pool_vm_release(r->mapping,(2+r->cross_page)*GC_HEAP_PAGE_BYTES);free(r);
    }
    puts("root journal pressure: 1/2/8 live mutators, wrapped eight-entry rings and counted concurrent mark handoff PASS");
    puts("rooted store registry: 1/2/8 owners register roots, drain leased journals, retire scopes and detach PASS");
    puts("barrier poll: guarded cancellation, registry rendezvous and load/store rearm through native dispatch PASS");
    puts("rooted load dispatch: native locks, automatic admission, concurrent loads and captured-result rearm PASS");
    puts("rooted store cross-page SATB: independent owner/referent leases and Young/Old queue handoff PASS");
    puts("rooted store pressure: 1/2/8 mutators, one-entry rings, concurrent SATB/remset worker and retry progress PASS");
    puts("rooted store handshake: 1/2/8 OS owners cancel guarded attempts before ACK, resume and readmit PASS");
    puts("rooted store concurrency: 1/2/8 OS mutators, shared-slot CAS retry and transferred lease conservation PASS");
}
#endif
