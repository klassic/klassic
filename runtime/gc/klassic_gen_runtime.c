/* Rooted C mutator adapter for the shared generational collector contracts. */
#include "klassic_gen_runtime_internal.h"
#include "klassic_gen_policy.h"
#include "klassic_gen_weak_internal.h"
#include "gc_root_journal.h"
#include "gc_root_walk.h"

static KgRuntime kg_runtime;
static _Thread_local KgMutator *kg_mutator;
/* Process-lifetime admission pin protects the interval BEFORE acquiring a
 * runtime mutex that shutdown may eventually destroy. Closed stays permanent
 * after disposal; rejected callers never read destroyed runtime controls. */
static _Atomic uint64_t kg_admission_gate=1;
static bool kg_admission_acquire(void){
    uint64_t value=atomic_load_explicit(&kg_admission_gate,memory_order_acquire);
    for(;;){
        if(value&1)return false;
        if(value>UINT64_MAX-2)kg_fail("attach reader count overflow",0);
        if(atomic_compare_exchange_weak_explicit(&kg_admission_gate,&value,value+2,memory_order_acq_rel,memory_order_acquire))return true;
    }
}
static void kg_admission_release(void){
    if(atomic_fetch_sub_explicit(&kg_admission_gate,2,memory_order_acq_rel)<2)kg_fail("attach reader count underflow",0);
}
KgMutator *kg_current_mutator(KgRuntime *r){return kg_mutator&&kg_mutator->runtime==r?kg_mutator:NULL;}
MutatorContext *kg_current_context(KgRuntime *r){KgMutator *m=kg_current_mutator(r);return m?m->context:NULL;}
static MutatorContext *kg_context(KgRuntime *r){MutatorContext *ctx=kg_current_context(r);if(!ctx)kg_fail("unattached mutator",0);return ctx;}
bool kg_current_initializing(KgRuntime *r){KgMutator *m=kg_current_mutator(r);return m&&m->initializing;}
MutatorContext *kg_context_acquire_after_locked(KgRuntime *r,uint64_t after){
    for(KgMutator *m=r->mutators;m;m=m->next)if(m->participant->registered&&(!after||m->context->thread_id<after)){
        if(m->pins==UINT64_MAX)kg_fail("mutator native pin overflow",0);m->pins++;return m->context;
    }
    return NULL;
}
void kg_context_release_locked(KgRuntime *r,MutatorContext *ctx){
    for(KgMutator *m=r->mutators;m;m=m->next)if(m->context==ctx){if(!m->pins)kg_fail("mutator native pin underflow",0);m->pins--;kg_notify(r);return;}
    kg_fail("unknown mutator native pin",0);
}
uint64_t kg_root_revision_locked(KgRuntime *r){return r->root_revision;}
uint64_t kg_root_publications_locked(KgRuntime *r){return r->root_publications;}
void kg_root_revision_sync_locked(KgRuntime *r){
    KgMutator *m=kg_current_mutator(r);if(!m)return;
    uint64_t revision=atomic_load_explicit(&m->context->root_revision,memory_order_acquire);
    if(revision!=m->root_revision_seen){
        if(revision<m->root_revision_seen||r->root_revision==UINT64_MAX)kg_fail("global root revision overflow",0);
        r->root_revision++;m->root_revision_seen=revision;
    }
    if(m->bootstrap){r->current=m->current;r->root_blocks=m->root_blocks;r->initializing=m->initializing;}
}

_Noreturn void kg_fail(const char *operation,uint64_t status){
    fprintf(stderr,"klassic generational GC: %s failed (%llu)\n",operation,(unsigned long long)status);
    abort();
}
static void kg_require_owner(KgRuntime *r){
    KgMutator *m=kg_current_mutator(r);
    if(!m||m->state!=KG_MUTATOR_REGISTERED||!r->initialized||r->closing)kg_fail("runtime or mutator is not running",0);
#ifdef _WIN32
    if(m->owner!=GetCurrentThreadId())kg_fail("foreign mutator thread",0);
#else
    if(!pthread_equal(m->owner,pthread_self()))kg_fail("foreign mutator thread",0);
#endif
}
void kg_configure(uint64_t maximum,uint64_t minimum,uint64_t soft,uint64_t flags){
    KgRuntime *r=&kg_runtime;
    if(r->initialized||r->native.stage)kg_fail("configuration after initialization",0);
    if(flags&~UINT64_C(7))kg_fail("unsupported GC configuration flags",flags);
    r->hard_max=maximum;r->min_heap=minimum;r->soft_max=soft;r->flags=flags;
}
void kg_configure_uncommit_delay(uint64_t nanoseconds){
    KgRuntime *r=&kg_runtime;
    if(r->initialized||r->native.stage)kg_fail("uncommit delay after initialization",0);
    r->uncommit_delay_ns=nanoseconds;
}
void kg_init(void){
    KgRuntime *r=&kg_runtime;
    if(r->initialized){kg_require_owner(r);return;}
    if(r->native.stage)kg_fail("runtime reinitialization",r->native.stage);
    r->os=gc_native_heap_ops();
#ifdef _WIN32
    InitializeSRWLock(&r->lock);InitializeConditionVariable(&r->changed);r->owner=GetCurrentThreadId();
#else
    if(pthread_mutex_init(&r->lock,NULL)||pthread_cond_init(&r->changed,NULL))kg_fail("OS synchronization initialization",1);
    r->owner=pthread_self();
#endif
    if(!r->hard_max)r->hard_max=UINT64_C(64)*1024*1024;
    if(r->hard_max<4*GC_HEAP_PAGE_BYTES||r->hard_max&(GC_HEAP_PAGE_BYTES-1))kg_fail("heap maximum alignment/size",r->hard_max);
    if(r->min_heap>r->hard_max)kg_fail("minimum heap exceeds hard maximum",r->min_heap);
    if(!r->soft_max)r->soft_max=r->hard_max-r->hard_max/4;
    if(r->soft_max>r->hard_max)kg_fail("soft heap maximum",r->soft_max);
    r->root_capacity=128;r->journal_capacity=256;
    NativeRuntimeConfig config={
        .heap={r->hard_max,r->soft_max,r->hard_max/8,UINT64_C(256)*1024*1024},
        .root_cells=r->root_capacity,.root_changes=r->journal_capacity,.descriptor_capacity=64,
        .work={256,1,1024}};
    uint64_t s=gc_native_runtime_prepare(&r->native,&config,&r->os);
    if(s)kg_fail("native heap/bootstrap",s);
    r->primary=(KgMutator){.runtime=r,.context=&r->native.context,.participant=&r->native.participant,
        .worker=&r->native.worker,.owner=r->owner,.bootstrap=1,.state=KG_MUTATOR_REGISTERED};
    kg_mutator=&r->primary;r->mutators=&r->primary;r->mutator_count=1;r->mutator_generation=1;r->root_sync=kg_root_revision_sync_locked;
    kg_lease_initialize(r,&r->native.context,1024,NULL);
    kg_weak_initialize_native(r,&r->native.context);
    uint64_t policy_failures=0;
    while((s=kg_policy_init(r))==GC_NATIVE_METADATA_OS_ERROR)
        kg_os_retry(&policy_failures,"minimum heap commitment OS retry limit");
    if(s)kg_fail("heap backing policy bootstrap",s);
    if(!r->payload_commit)r->payload_commit=kg_policy_commit;
    if(!r->payload_decommit)r->payload_decommit=kg_policy_decommit;
    if(!r->idle_decommit)r->idle_decommit=gc_native_heap_decommit_range;
    s=gc_native_runtime_activate(&r->native);if(s)kg_fail("native activation",s);
    r->initial_roots=(KgRootBlock){.charge=&r->native.root_charge,
        .roots=(void *)(uintptr_t)r->native.root_charge.allocation,.initial=1};
    r->primary.initial_roots=&r->initial_roots;r->primary.root_blocks=&r->initial_roots;
    r->root_blocks=&r->initial_roots;r->initialized=1;
    kg_driver_start(r);
    r->primary.store_observed=kg_root_new();
    atomic_store_explicit(&kg_admission_gate,0,memory_order_release);
    if(r->flags&2)fprintf(stderr,"klassic generational GC: heap=%llu soft=%llu minimum=%llu committed=%llu uncommit_delay_ns=%llu\n",(unsigned long long)r->hard_max,(unsigned long long)r->soft_max,(unsigned long long)r->min_heap,(unsigned long long)kg_policy_committed_locked(r),(unsigned long long)r->uncommit_delay_ns);
}
uint64_t kg_committed_bytes(void){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_lock(r);
    uint64_t bytes=kg_policy_committed_locked(r);kg_unlock(r);return bytes;
}
void kg_verify(void){KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_driver_verify(r);}
uint64_t kg_verification_count(void){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_lock(r);
    uint64_t count=kg_policy_verifications_locked(r);kg_unlock(r);return count;
}
void kg_close_tlab_locked(KgRuntime *r){
    if(!kg_current_mutator(r)->current){
        if(kg_context(r)->tlab_start||kg_context(r)->tlab_top||kg_context(r)->tlab_end)kg_fail("unowned TLAB",0);
        return;
    }
    uint64_t s=gc_native_allocation_close_context_locked(&kg_current_mutator(r)->current->native,kg_context(r));
    if(s)kg_fail("TLAB close",s);
    kg_current_mutator(r)->current->closed_phase=r->native.roots.phase>>16;
    kg_current_mutator(r)->current=NULL;
}
static void kg_boundary(KgRuntime *r,uint64_t operation){
    for(;;){
        kg_lock(r);
        if(operation==GC_WK_POLL&&!r->native.handshake.active&&
           atomic_load_explicit(&kg_context(r)->state,memory_order_acquire)==GC_RUNNING&&
           atomic_load_explicit(&kg_context(r)->adopted_phase,memory_order_acquire)==r->native.roots.phase){
            kg_unlock(r);return;
        }
        uint64_t action=operation;
        /* Native admission cannot bypass an already active rendezvous. The
         * real owner first publishes/ACKs/resumes through its ordinary poll,
         * then retries its intended Native transition. */
        if(operation==GC_WK_ENTER_NATIVE&&(r->native.handshake.active||
           atomic_load_explicit(&kg_context(r)->state,memory_order_acquire)==GC_AT_SAFEPOINT))action=GC_WK_POLL;
        uint64_t s=gc_worker_step_locked(&r->native.roots,kg_current_mutator(r)->worker,action);
        if(s==GC_WK_CLOSE_TLAB){kg_close_tlab_locked(r);kg_unlock(r);continue;}
        if(s==GC_WK_FLUSH_SATB||s==GC_WK_FLUSH_REMEMBERED){
            uint64_t q=gc_barrier_buffer_step(kg_context(r),NULL,s==GC_WK_FLUSH_SATB?2:3);
            kg_unlock(r);if(q!=GC_BB_OK)kg_fail("barrier publication",q);continue;
        }
        kg_unlock(r);
        if(s==GC_WK_OK){if(action!=operation)continue;return;}
        if(s!=GC_WK_PARK&&s!=GC_WK_RETRY)kg_fail("native/safepoint boundary",s);
        kg_driver_step(r);kg_yield();
    }
}
/* Keep adoption and an operation requiring the current snapshot in the same
 * common-lock interval; a newly begun rendezvous is retried before mutation. */
static void kg_lock_current(KgRuntime *r){
    for(;;){
        kg_boundary(r,GC_WK_POLL);kg_lock(r);
        if(!r->native.handshake.active&&
           atomic_load_explicit(&kg_context(r)->state,memory_order_acquire)==GC_RUNNING&&
           atomic_load_explicit(&kg_context(r)->adopted_phase,memory_order_acquire)==r->native.roots.phase)return;
        kg_unlock(r);
    }
}
void kg_root_publication_begin(KgRuntime *r){
    kg_lock_current(r);KgMutator *m=kg_current_mutator(r);
    if(r->root_publications==UINT64_MAX||m->publications==UINT64_MAX)kg_fail("root publication count overflow",0);
    r->root_publications++;m->publications++;kg_unlock(r);
}
void kg_root_publication_end(KgRuntime *r){
    KgMutator *m=kg_current_mutator(r);
    if(!m||atomic_load_explicit(&m->context->access_epoch,memory_order_acquire)||
       atomic_load_explicit(&m->context->private_producers,memory_order_acquire))kg_fail("unfinished root publisher",0);
    kg_lock(r);if(!r->root_publications||!m->publications)kg_fail("root publication count underflow",0);
    kg_root_revision_sync_locked(r);r->root_publications--;m->publications--;kg_unlock(r);
}
void kg_enter_native(KgRuntime *r){kg_boundary(r,GC_WK_ENTER_NATIVE);}
void kg_leave_native(KgRuntime *r){kg_boundary(r,GC_WK_LEAVE_NATIVE);}
void kg_native_enter(void){KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_enter_native(r);}
void kg_native_exit(void){KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_leave_native(r);}
void kg_poll(void){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    /* The sole busy-owned driver transports the global journal inventory.
     * Polling peers must not create competing root walks while it proves
     * incoming absence. Explicit flush/retirement paths still drain as needed. */
    kg_boundary(r,GC_WK_POLL);kg_driver_step(r);
}
void kg_drain_journals(KgRuntime *r){
    RootWalk walk={0};kg_lock(r);
    uint64_t status=gc_root_walk_locked(&r->native.roots,&walk,GC_RW_BEGIN);kg_unlock(r);
    while(status==GC_RW_ITEM){
        RootRegistration *reg=(void *)(uintptr_t)walk.lease.registration;
        for(;;){
            RootLease lease={0};RootChange changes[16];
            RootJournalRequest q={.registration=(uintptr_t)reg,.scope=walk.lease.scope,
                .lease=(uintptr_t)&lease,.output=(uintptr_t)changes,.capacity=16};
            kg_lock(r);uint64_t s=gc_root_journal_step(&r->native.roots,&q,GC_RJ_CLAIM);kg_unlock(r);
            if(s==GC_RJ_EMPTY||s==GC_RJ_WAIT)break;
            if(s!=GC_RJ_OK)kg_fail("root journal claim",s);
            s=gc_root_journal_step(NULL,&q,GC_RJ_COPY);if(s)kg_fail("root journal copy",s);
            for(uint64_t i=0;i<q.written;i++){
                kg_driver_reference(r,changes[i].previous,changes[i].phase);
                kg_driver_reference(r,changes[i].next,changes[i].phase);
            }
            kg_lock(r);s=gc_root_journal_step(&r->native.roots,&q,GC_RJ_ACK);kg_unlock(r);
            if(s)kg_fail("root journal acknowledgement",s);
            break; /* one leased batch per segment; remaining work stays published */
        }
        /* This walk discovers transport work only. It grants no root marking
         * or termination credit; each copied journal has its own real lease. */
        kg_lock(r);walk.mask=walk.completed_mask=0;walk.cursor=walk.limit;
        status=gc_root_walk_locked(&r->native.roots,&walk,GC_RW_NEXT);kg_unlock(r);
    }
    if(status!=GC_RW_DONE)kg_fail("root journal discovery",status);
}
RootRequest kg_root_request_locked(KgRuntime *r,const RootCell *cell){
    uint64_t address=(uintptr_t)cell;
    for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next){
        RootSegment *s=&b->roots->segment;
        if(address>=s->cells&&address-s->cells<s->capacity*sizeof(RootCell)&&
           !((address-s->cells)%sizeof(RootCell))){
            if(!cell->active||atomic_load_explicit(&s->state,memory_order_acquire)!=GC_ROOT_SEGMENT_ACTIVE)
                kg_fail("inactive root",0);
            return (RootRequest){.segment=(uintptr_t)s,.handle={address,cell->reuse_generation,s->scope}};
        }
    }
    kg_fail("foreign root storage",address);
}
/* Retire an entirely unused segment through the real deletion scanner, journal
 * handoff, reader exclusion and finite clear protocol. New scope on republish
 * prevents stale handles from becoming valid when its cells are reused. */
static void kg_retire_roots(KgRuntime *r,KgRootBlock *b){
    kg_boundary(r,GC_WK_POLL);
    RootLease lease={0};RootLifeRequest q={.registration=(uintptr_t)&b->roots->registration,
        .scope=b->roots->segment.scope,.context=(uintptr_t)kg_context(r),.lease=(uintptr_t)&lease};
    kg_lock_current(r);uint64_t s=gc_root_life_locked(&r->native.roots,&q,GC_RL_RETIRE);kg_unlock(r);
    if(s)kg_fail("root retirement",s);
    for(;;){
        kg_lock(r);s=gc_root_life_locked(&r->native.roots,&q,GC_RL_CLAIM);kg_unlock(r);
        if(s==GC_RL_WAIT)break;
        if(s)kg_fail("root deletion claim",s);
        uint64_t values[256];RootScanJob scan={.scan={.segment=(uintptr_t)&b->roots->segment,
            .cursor=q.begin,.capacity=q.end-q.begin,.output=(uintptr_t)values},.lease=(uintptr_t)&lease,.serial=q.serial};
        s=gc_root_scan_leased(&scan);if(s!=GC_ROOT_OK&&s!=2)kg_fail("root deletion scan",s);
        for(uint64_t i=0;i<scan.scan.written;i++)kg_driver_reference(r,values[i],q.phase);
        kg_lock(r);s=gc_root_life_locked(&r->native.roots,&q,GC_RL_ACK);kg_unlock(r);
        if(s)kg_fail("root deletion acknowledgement",s);
    }
    for(;;){
        /* Ordinary transport consumes one finite batch. This retired segment
         * has stopped producing, so exhaust its captured finite prefix before
         * CLEAN removes it from discovery. FINISH cannot drain CLEANING data. */
        /* A different owner can start a rendezvous while a reader prevents
         * CLEAN. This actual owner must ACK/adopt instead of spinning Running
         * behind that reader; no raw guard/private producer crosses this poll. */
        kg_boundary(r,GC_WK_POLL);kg_drain_journals(r);kg_lock(r);
        BufferCursor *journal=&b->roots->journal;
        bool drained=atomic_load_explicit(&journal->consumed,memory_order_acquire)==
            atomic_load_explicit(&journal->published,memory_order_acquire);
        s=drained?gc_root_life_locked(&r->native.roots,&q,GC_RL_CLEAN):GC_RL_WAIT;kg_unlock(r);
        if(s==GC_RL_OK)break;
        if(s!=GC_RL_WAIT)kg_fail("root cleanup claim",s);
        kg_driver_step(r);kg_yield();
    }
    do{s=gc_root_clear_chunk(&q);}while(s==GC_RL_WAIT);
    if(s)kg_fail("root cleanup",s);
    kg_lock(r);s=gc_root_life_locked(&r->native.roots,&q,GC_RL_FINISH);kg_unlock(r);
    if(s)kg_fail("root retirement finish",s);
}
/* Retry native OS effects while their original receipts and charges remain
 * owned. The 32-attempt fail-stop policy never publishes a failed operation or
 * returns its budget; permanent failure terminates with the live receipt held. */
void kg_metadata_prepare_retry(KgRuntime *r,MetadataCharge *charge){
    uint64_t failures=0,status;
    while((status=gc_native_metadata_prepare(charge,&r->os.metadata))==GC_NATIVE_METADATA_OS_ERROR)
        kg_os_retry(&failures,"metadata mapping OS retry limit");
    if(status)kg_fail("metadata mapping receipt",status);
}
uint64_t kg_metadata_retire_step_native(KgRuntime *r,MetadataCharge *source,KgMetadataRetire *job,const MutatorContext *actor){
    if(!job||!actor||actor->abi_version!=GC_ABI_VERSION||!actor->thread_id||
       atomic_load_explicit(&actor->state,memory_order_acquire)!=GC_BLOCKED_NATIVE||
       atomic_load_explicit(&actor->access_epoch,memory_order_acquire))return GC_NATIVE_METADATA_INVALID;
    if(!job->charge.stage){
        if(!source||source->stage!=2||!source->allocation)return GC_NATIVE_METADATA_INVALID;
        job->charge=*source;*source=(MetadataCharge){0};
    }
    if(job->charge.stage==3)return !source||!source->owner?GC_NATIVE_METADATA_OK:GC_NATIVE_METADATA_INVALID;
    if(job->charge.stage!=2)return GC_NATIVE_METADATA_INVALID;
    uint64_t status=gc_native_metadata_dispose(&job->charge,&r->os.metadata);
    if(status==GC_NATIVE_METADATA_OS_ERROR){kg_os_retry(&job->failures,"metadata retirement OS retry limit");return status;}
    if(status)return status;
    kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,&job->charge,GC_BUDGET_RELEASE);kg_unlock(r);
    return status?GC_NATIVE_METADATA_INVALID:GC_NATIVE_METADATA_OK;
}
#include "klassic_gen_leases.h"

void kg_prepare_run_native_retry(KgRuntime *r,KgRun *run,const MutatorContext *actor){
    uint64_t failures=0,status;
    for(;;){
        kg_policy_before_prepare(r,run,actor);
        status=gc_native_allocation_prepare_native_with(&run->native,&r->os.metadata,actor,r->payload_commit);
        kg_policy_after_prepare(r,run);
        if(status!=GC_NATIVE_RUN_OS_ERROR)break;
        kg_os_retry(&failures,"allocation preparation OS retry limit");
    }
    if(status)kg_fail("allocation preparation receipt",status);
}
void kg_cancel_run_native_retry(KgRuntime *r,KgRun *run,const MutatorContext *actor){
    if(!gc_native_allocation_actor_native(&run->native,actor))kg_fail("private run cleanup actor",0);
    uint64_t borrowed=kg_driver_cleanup_enter(r,run),status;
    kg_lock(r);status=gc_native_allocation_cancel_locked(&run->native);kg_unlock(r);
    if(status)kg_fail("private run cancellation",status);
    uint64_t failures=0;
    for(;;){
        kg_policy_os_enter(r,actor);
        status=gc_native_allocation_cleanup_native_with(&run->native,&r->os.metadata,actor,r->payload_decommit);
        kg_policy_os_leave(r);
        if(status!=GC_NATIVE_RUN_OS_ERROR)break;
        kg_os_retry(&failures,"allocation cleanup OS retry limit");
    }
    if(status)kg_fail("private run cleanup receipt",status);
    kg_lock(r);status=gc_native_allocation_finish_locked(&run->native);kg_unlock(r);
    if(status)kg_fail("private run cleanup finish",status);
    kg_release_run_control_native(r,run,actor);kg_driver_cleanup_leave(r,borrowed);
}
/* Unique budget receipts are moved, never duplicated/replayed. The control
 * mapping outlives all canonical inventories and keeps its own charge. */
static void *kg_control_map(KgRuntime *r,uint64_t bytes,MetadataCharge *receipt){
    MetadataCharge q={0};if(gc_native_metadata_size(bytes,r->os.metadata.quantum,&q.bytes))kg_fail("metadata control size",bytes);
    kg_enter_native(r);kg_lock(r);
    uint64_t s=gc_budget_step_locked(&r->native.backing.heap,&q,GC_BUDGET_RESERVE);kg_unlock(r);
    if(s){kg_leave_native(r);kg_fail("metadata control budget",s);}
    kg_metadata_prepare_retry(r,&q);
    kg_lock(r);s=gc_budget_step_locked(&r->native.backing.heap,&q,GC_BUDGET_COMMIT);kg_unlock(r);
    if(s)kg_fail("metadata control commit",s);
    *receipt=q;q=(MetadataCharge){0};
    kg_leave_native(r);return (void *)(uintptr_t)receipt->allocation;
}
static void kg_control_unmap(KgRuntime *r,MetadataCharge *control){
    KgMetadataRetire job={0};kg_enter_native(r);uint64_t status;
    do{status=kg_metadata_retire_step_native(r,job.charge.stage?NULL:control,&job,kg_context(r));}while(status==GC_NATIVE_METADATA_OS_ERROR);
    kg_leave_native(r);if(status)kg_fail("metadata control retirement",status);
}
#include "klassic_gen_pin_impl.h"
#include "klassic_gen_thread_impl.h"
static KgRootBlock *kg_grow_roots(KgRuntime *r){
    MetadataCharge receipt={0};KgRootBlock *b=kg_control_map(r,sizeof(*b),&receipt);
    b->control=receipt;receipt=(MetadataCharge){0};b->charge=&b->storage;
    uint64_t s=gc_native_roots_size(r->root_capacity,r->journal_capacity,r->os.metadata.quantum,&b->storage.bytes);
    if(s)kg_fail("root storage size",s);
    kg_enter_native(r);kg_lock(r);
    s=gc_budget_step_locked(&r->native.backing.heap,&b->storage,GC_BUDGET_RESERVE);kg_unlock(r);
    if(s)kg_fail("root storage admission",s);
    uint64_t map_failures=0;
    while((s=gc_native_roots_prepare(&b->storage,kg_context(r)->thread_id,r->root_capacity,r->journal_capacity,&r->os.metadata,gc_root_storage_prepare))==GC_NATIVE_METADATA_OS_ERROR)
        kg_os_retry(&map_failures,"root storage mapping OS retry limit");
    if(s)kg_fail("root storage mapping",s);
    kg_lock(r);s=gc_budget_step_locked(&r->native.backing.heap,&b->storage,GC_BUDGET_COMMIT);kg_unlock(r);
    if(s)kg_fail("root storage commit",s);
    b->roots=(void *)(uintptr_t)b->storage.allocation;
    kg_leave_native(r);kg_boundary(r,GC_WK_POLL);
    kg_lock_current(r);s=gc_native_roots_publish_locked(&r->native.roots,kg_context(r),&b->storage);
    if(!s){b->next=kg_current_mutator(r)->root_blocks;kg_current_mutator(r)->root_blocks=b;}kg_unlock(r);
    if(s)kg_fail("root storage publication",s);
    return b;
}
RootCell *kg_root_new(void){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    for(;;){
        kg_boundary(r,GC_WK_POLL);
        kg_lock(r);KgRootBlock *reuse=NULL;
        for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next){
            RootRequest q={.segment=(uintptr_t)&b->roots->segment};
            uint64_t s=gc_root_step(kg_context(r),&q,GC_ROOT_RESERVE);
            if(s==GC_ROOT_OK){b->live++;kg_unlock(r);return (void *)(uintptr_t)q.handle.cell;}
            if(s!=GC_ROOT_FULL)kg_fail("root reservation",s);
            if(!b->live)reuse=b;
        }
        kg_unlock(r);
        if(reuse){
            kg_retire_roots(r,reuse);
            /* FINISH excluded every reader and drained all journal holders. */
            reuse->roots->journal.top=0;
            atomic_store(&reuse->roots->journal.published,0);atomic_store(&reuse->roots->journal.consumed,0);
            kg_lock_current(r);
            uint64_t s=gc_native_roots_publish_locked(&r->native.roots,kg_context(r),reuse->charge);kg_unlock(r);
            if(s)kg_fail("root storage reuse",s);
        }else kg_grow_roots(r);
    }
}
static void kg_mutate_root(RootCell *out,const RootCell *source,uint64_t operation){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    if(source==out){
        for(;;){
            kg_boundary(r,GC_WK_POLL);kg_lock(r);
            RootFollow follow={.context=(uintptr_t)kg_context(r),.root=kg_root_request_locked(r,out)};
            uint64_t status=gc_root_follow_locked(kg_leases(r),&follow,GC_RF_STEP);kg_unlock(r);
            if(status==GC_RF_DONE)return;
            if(status!=GC_RF_ADVANCED&&status!=GC_RF_WAIT&&status!=GC_RF_FLUSH&&status!=GC_RF_METADATA_FULL)
                kg_fail("self root canonical follow",status);
            if(status==GC_RF_METADATA_FULL)kg_lease_pressure(r);
            kg_poll();
        }
    }
    for(;;){
        kg_boundary(r,GC_WK_POLL);kg_lock(r);
        RootRequest q=kg_root_request_locked(r,out);
        if(source){
            RootRequest in=kg_root_request_locked(r,source);
            RootFollow follow={.context=(uintptr_t)kg_context(r),.root=in};
            uint64_t canonical=gc_root_follow_locked(kg_leases(r),&follow,GC_RF_STEP);
            if(canonical!=GC_RF_DONE){
                kg_unlock(r);
                if(canonical!=GC_RF_ADVANCED&&canonical!=GC_RF_WAIT&&canonical!=GC_RF_FLUSH&&canonical!=GC_RF_METADATA_FULL)
                    kg_fail("root assignment canonical follow",canonical);
                if(canonical==GC_RF_METADATA_FULL)kg_lease_pressure(r);
                kg_poll();continue;
            }
            /* Follow and copy share the mover/heap lock. At Retire every
             * obsolete source already forwards, so assignment cannot put an
             * old-range reference behind a concurrent absence audit cursor. */
            uint64_t read=gc_root_step(kg_context(r),&in,GC_ROOT_READ_RAW);
            if(read)kg_fail("root source read",read);q.value=in.value;
        }
        uint64_t s=gc_root_step(kg_context(r),&q,operation);
        if(s==GC_ROOT_OK&&operation==GC_ROOT_RELEASE){
            for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next)if(q.segment==(uintptr_t)&b->roots->segment){if(!b->live)kg_fail("root live count",0);b->live--;break;}
        }
        kg_unlock(r);
        if(s==GC_ROOT_OK)return;
        if(s!=GC_ROOT_FLUSH)kg_fail("root publication",s);
        kg_drain_journals(r);kg_driver_step(r);
    }
}
void kg_root_copy(RootCell *out,const RootCell *source){if(!source)kg_fail("null root source",0);kg_mutate_root(out,source,GC_ROOT_STORE);}
void kg_root_clear(RootCell *root){kg_mutate_root(root,NULL,GC_ROOT_STORE);}
void kg_root_release(RootCell *root){kg_mutate_root(root,NULL,GC_ROOT_RELEASE);}
KgRun *kg_reserve_run(KgRuntime *r,uint64_t pages,uint64_t generation){
    MetadataCharge receipt={0};KgRun *run=kg_control_map(r,sizeof(*run),&receipt);
    run->control=receipt;receipt=(MetadataCharge){0};
    uint64_t capacity=(pages*GC_HEAP_PAGE_BYTES)/(sizeof(ObjectHeader)+16)+1;
    for(unsigned attempts=0;;attempts++){
        kg_enter_native(r);kg_lock(r);
        uint64_t s=gc_native_allocation_reserve_generation_locked(&run->native,&r->native,pages,capacity,generation,&r->os.metadata);kg_unlock(r);
        if(s==GC_POOL_OK){
            kg_prepare_run_native_retry(r,run,kg_context(r));
            kg_lock(r);s=gc_native_allocation_publish_locked(&run->native);kg_unlock(r);
            kg_leave_native(r);if(s)kg_fail("run publication",s);return run;
        }
        kg_leave_native(r);
        if(s==GC_POOL_INVALID)kg_fail("run reservation",s);
        if(attempts==32)kg_fail("out of managed heap/metadata",s);
        kg_driver_pressure(r,s==GC_POOL_METADATA_LIMIT?KG_PRESSURE_METADATA:KG_PRESSURE_HEAP);kg_poll();
    }
}
void kg_release_run_control(KgRuntime *r,KgRun *run){
    if(run->native.stage!=7)kg_fail("run control still owned",run->native.stage);
    kg_control_unmap(r,&run->control);
}
/* Collector OS thread variant: never transitions, reads or borrows the
 * registered mutator's private context. Caller owns actor in BlockedNative. */
KgRun *kg_try_reserve_run_native(KgRuntime *r,uint64_t pages,uint64_t generation,
        const MutatorContext *actor,uint64_t *status){
    if(!status||!actor||actor->abi_version!=GC_ABI_VERSION||!actor->thread_id||
       atomic_load_explicit(&actor->state,memory_order_acquire)!=GC_BLOCKED_NATIVE||
       atomic_load_explicit(&actor->access_epoch,memory_order_acquire)||!pages||
       pages>r->native.backing.heap.page_count||generation>GC_OLD)kg_fail("collector target actor",0);
    MetadataCharge receipt={0};
    if(gc_native_metadata_size(sizeof(KgRun),r->os.metadata.quantum,&receipt.bytes))kg_fail("run control size",0);
    kg_lock(r);*status=gc_budget_step_locked(&r->native.backing.heap,&receipt,GC_BUDGET_RESERVE);kg_unlock(r);
    if(*status){*status=*status==GC_BUDGET_LIMIT?GC_POOL_METADATA_LIMIT:GC_POOL_INVALID;return NULL;}
    kg_metadata_prepare_retry(r,&receipt);
    kg_lock(r);*status=gc_budget_step_locked(&r->native.backing.heap,&receipt,GC_BUDGET_COMMIT);kg_unlock(r);
    if(*status)kg_fail("collector control commit",*status);
    KgRun *run=(void *)(uintptr_t)receipt.allocation;
    run->control=receipt;receipt=(MetadataCharge){0};
    uint64_t capacity=pages*GC_HEAP_PAGE_BYTES/(sizeof(ObjectHeader)+16)+1;
    kg_lock(r);*status=gc_native_allocation_reserve_target_locked(&run->native,&r->native,pages,capacity,generation,&r->os.metadata);kg_unlock(r);
    if(*status){
        kg_release_run_control_native(r,run,actor);
        return NULL;
    }
    kg_prepare_run_native_retry(r,run,actor);
    kg_lock(r);*status=gc_native_allocation_publish_locked(&run->native);kg_unlock(r);
    if(*status)kg_fail("collector target publication",*status);
    return run;
}
void kg_release_run_control_native(KgRuntime *r,KgRun *run,const MutatorContext *actor){
    if((run->native.self&&(!gc_native_allocation_actor_native(&run->native,actor)||run->native.stage!=7))||
       (!run->native.self&&run->native.stage))kg_fail("collector control still owned",run->native.stage);
    KgMetadataRetire job={0};uint64_t status;
    do{status=kg_metadata_retire_step_native(r,job.charge.stage?NULL:&run->control,&job,actor);}while(status==GC_NATIVE_METADATA_OS_ERROR);
    if(status)kg_fail("collector control retirement",status);
}
static void kg_initialization_ready(KgRuntime *r,RootCell *root,MetadataLeaseRequest *reserved){
    PendingMark access={.context=(uintptr_t)kg_context(r),
        .current={.reference=atomic_load_explicit(&root->value,memory_order_acquire),.lease=*reserved}};
    *reserved=(MetadataLeaseRequest){0};
    for(;;){
        kg_lock(r);uint64_t status=gc_pending_mark_enter(kg_leases(r),&access);kg_unlock(r);
        if(status==GC_RAC_READY)break;
        if(status!=GC_RF_ADVANCED&&status!=GC_RF_WAIT)kg_fail("reserved initialization access",status);
        /* TLAB remains owned until readiness. The exact lease was acquired
         * before allocation, so no metadata growth or native entry is needed. */
        kg_driver_step(r);kg_yield();
    }
    GuardedMarkPrepare q={.scope=(uintptr_t)&access.access.scope,.page=access.current.lease.page,
        .phase=atomic_load_explicit(&kg_context(r)->adopted_phase,memory_order_acquire),
        .reference=access.current.reference,.lease=access.current.lease};
    uint64_t status=gc_guarded_initialization_publish(&q);
    uint64_t exited;do{exited=gc_pending_mark_cleanup(NULL,&access,GC_RAC_EXIT);}while(exited==GC_RESOLVE_WAIT);
    if(exited!=GC_RF_DONE)kg_fail("initialization source guard exit",exited);
    kg_lock(r);uint64_t clean=gc_pending_mark_cleanup(kg_leases(r),&access,GC_RAC_RELEASE);kg_unlock(r);
    if(clean!=GC_RF_DONE)kg_fail("initialization metadata release",clean);
    if(status)kg_fail("initialization publication",status);
}

void kg_alloc_into(uint64_t bytes,uint64_t kind,RootCell *out){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    if(kind!=1&&kind!=2)kg_fail("allocation descriptor",kind);
    if(bytes>r->hard_max-sizeof(ObjectHeader)-16)kg_fail("oversized object",bytes);
    kg_root_clear(out);
    if(r->flags&1)kg_driver_pressure(r,KG_PRESSURE_STRESS);
    for(;;){
        kg_poll();MetadataLeaseRequest initialization={0};kg_lock(r);
        RootRequest root=kg_root_request_locked(r,out);AllocationResult result={0};
        AllocationRequest request={.page=kg_current_mutator(r)->current?(uintptr_t)gc_native_allocation_page(&kg_current_mutator(r)->current->native):0,
            .marks=kg_current_mutator(r)->current?(uintptr_t)&kg_current_mutator(r)->current->native.marks:0,.output=(uintptr_t)&result};
        AllocationShape shape={kind,bytes,(uintptr_t)&request};
        uint64_t s=gc_allocation_shape(kg_descriptors(r),&shape);
        if(!s){
            if(!kg_current_mutator(r)->current)s=GC_ALLOC_REFILL;
            else{
                AllocationPage *page=gc_native_allocation_page(&kg_current_mutator(r)->current->native);
                initialization=(MetadataLeaseRequest){.page=(uintptr_t)page,.incarnation=page->incarnation};
                uint64_t token=gc_metadata_step_locked(kg_leases(r),&initialization,GC_ML_ACQUIRE);
                if(token==GC_ML_FULL)s=128; /* facade metadata pressure, before allocation */
                else if(token)kg_fail("initialization lease admission",token);
                else{AllocationInto into={(uintptr_t)&request,(uintptr_t)&root};s=gc_try_tlab_allocate_into(kg_context(r),&into);}
            }
        }
        if(s==GC_ALLOC_OK)kg_current_mutator(r)->initializing=1;
        else if(initialization.token){uint64_t released=gc_metadata_step_locked(kg_leases(r),&initialization,GC_ML_RELEASE);if(released)kg_fail("unused initialization lease",released);}
        kg_unlock(r);
        if(s==128){kg_lease_pressure(r);continue;}
        if(s==GC_ALLOC_OK){kg_initialization_ready(r,out,&initialization);kg_lock(r);kg_current_mutator(r)->initializing=0;kg_unlock(r);return;}
        if(s==GC_ALLOC_ROOT_FLUSH){kg_drain_journals(r);continue;}
        if(s!=GC_ALLOC_REFILL&&s!=GC_ALLOC_METADATA_REFILL)kg_fail("rooted allocation",s);
        kg_lock(r);kg_close_tlab_locked(r);kg_unlock(r);
        uint64_t total=sizeof(ObjectHeader)+((bytes+15)&~UINT64_C(15));
        uint64_t pages=(total+GC_HEAP_PAGE_BYTES-1)/GC_HEAP_PAGE_BYTES;if(!pages)pages=1;
        KgRun *run=kg_reserve_run(r,pages,GC_YOUNG);
        kg_boundary(r,GC_WK_POLL);kg_lock(r);s=gc_native_allocation_bind_context_locked(&run->native,kg_context(r));
        if(!s)kg_current_mutator(r)->current=run;kg_unlock(r);if(s)kg_fail("TLAB bind",s);
    }
}
static uint64_t kg_scalar(const RootCell *base,uint64_t offset,uint64_t bits,uint64_t operation){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_poll();
    RootedScalar q={.registry=(uintptr_t)kg_descriptors(r),.byte_offset=offset,.value=bits,.operation=operation};
    q.receiver.follow.context=(uintptr_t)kg_context(r);
    kg_lock(r);q.receiver.follow.root=kg_root_request_locked(r,base);kg_unlock(r);
    for(;;){
        kg_lock(r);uint64_t s=gc_rooted_scalar(kg_leases(r),&q,GC_RSC_ENTER);kg_unlock(r);
        if(s==GC_RAC_READY){
            s=gc_rooted_scalar(NULL,&q,GC_RSC_ACCESS);
            if(s!=GC_RSC_EXIT_REQUIRED)kg_fail("scalar access",s);
            do{s=gc_rooted_scalar(NULL,&q,GC_RSC_EXIT);}while(s==GC_RSC_EXIT_REQUIRED);
            if(s!=GC_RSC_RELEASE_REQUIRED)kg_fail("scalar guard exit",s);
            kg_lock(r);s=gc_rooted_scalar(kg_leases(r),&q,GC_RSC_RELEASE);kg_unlock(r);
            if(s!=GC_RSC_COMPLETE||q.result!=GC_SCALAR_OK)kg_fail("scalar completion",q.result);
            return q.value;
        }
        if(s!=GC_RF_ADVANCED&&s!=GC_RF_WAIT&&s!=GC_RF_FLUSH&&s!=GC_RF_METADATA_FULL)kg_fail("scalar receiver",s);
        if(s==GC_RF_METADATA_FULL)kg_lease_pressure(r);
        kg_poll();
    }
}
void kg_store_i64(const RootCell *base,uint64_t offset,uint64_t bits){(void)kg_scalar(base,offset,bits,GC_SCALAR_STORE);}
uint64_t kg_load_i64(const RootCell *base,uint64_t offset){return kg_scalar(base,offset,0,GC_SCALAR_LOAD);}
static void kg_dispatch_lock(uintptr_t p){kg_lock((KgRuntime *)p);}
static void kg_dispatch_unlock(uintptr_t p){kg_unlock((KgRuntime *)p);}
static RootedStoreRuntime kg_dispatch(KgRuntime *r,RootedLoadAdmission *a){
    NativeWorkStorage *w=gc_native_runtime_work(&r->native);
    *a=(RootedLoadAdmission){(uintptr_t)&r->native.roots,(uintptr_t)&r->native.handshake,(uintptr_t)kg_current_mutator(r)->participant,
        {(uintptr_t)&w->queues[0],(uintptr_t)&w->queues[1]}};
    return (RootedStoreRuntime){(uintptr_t)kg_leases(r),(uintptr_t)a,(uintptr_t)r,(uintptr_t)r,
        {(uintptr_t)r,(uintptr_t)r},(uintptr_t)kg_dispatch_lock,(uintptr_t)kg_dispatch_unlock};
}
static bool kg_retry_event(uint64_t s){return s==GC_RLOAD_WAIT||s==GC_RLOAD_FLUSH||s==GC_RLOAD_METADATA_FULL||s==GC_RLOAD_EXHAUSTED;}
void kg_load_ref_into(const RootCell *base,uint64_t offset,RootCell *out){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);if(base==out)kg_fail("load root aliases receiver",0);
    kg_root_clear(out);kg_poll();
    RootedLoad q={.observation={.registry=(uintptr_t)kg_descriptors(r),.byte_offset=offset}};
    kg_lock(r);q.observation.receiver.follow.context=(uintptr_t)kg_context(r);
    q.observation.receiver.follow.root=kg_root_request_locked(r,base);q.observation.output=kg_root_request_locked(r,out);kg_unlock(r);
    RootedLoadAdmission a;RootedStoreRuntime d=kg_dispatch(r,&a);
    bool cancelling=false;uint64_t reason=0;kg_root_publication_begin(r);
    for(;;){
        uint64_t s=gc_rooted_load_dispatch(&d,&q,GC_RLD_STEP);
        if(s==GC_RLOAD_DONE){kg_root_publication_end(r);return;}
        if(s==GC_RLOAD_CANCELLED){if(reason==GC_RLOAD_EXHAUSTED)kg_fail("reference load resources exhausted",reason);kg_root_publication_end(r);if(reason==GC_RLOAD_METADATA_FULL)kg_lease_pressure(r);kg_poll();kg_root_publication_begin(r);s=gc_rooted_load_dispatch(&d,&q,GC_RLD_REARM);cancelling=false;}
        if(kg_retry_event(s)&&!cancelling){reason=s;(void)gc_rooted_load_dispatch(&d,&q,GC_RLD_CANCEL);cancelling=true;}
        else if(s==GC_RLOAD_INVALID||s==GC_RLOAD_ERROR||s==GC_RLOAD_NULL_RECEIVER)kg_fail("reference load",s);
    }
}
void kg_store_ref(const RootCell *base,uint64_t offset,const RootCell *value){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);kg_root_clear(kg_current_mutator(r)->store_observed);kg_poll();
    RootedStore q={.observation={.registry=(uintptr_t)kg_descriptors(r),.byte_offset=offset}};
    kg_lock(r);
    q.value.observation.receiver.follow.context=(uintptr_t)kg_context(r);
    q.value.observation.receiver.follow.root=kg_root_request_locked(r,value);
    q.value.observation.output=kg_root_request_locked(r,value);
    q.observation.receiver.follow.context=(uintptr_t)kg_context(r);
    q.observation.receiver.follow.root=kg_root_request_locked(r,base);
    q.observation.output=kg_root_request_locked(r,kg_current_mutator(r)->store_observed);kg_unlock(r);
    RootedLoadAdmission a;RootedStoreRuntime d=kg_dispatch(r,&a);bool cancelling=false;uint64_t reason=0;kg_root_publication_begin(r);
    for(;;){
        uint64_t s=gc_rooted_store_dispatch(&d,&q,GC_RSD_STEP);
        if(s==GC_RLOAD_DONE){kg_root_publication_end(r);kg_root_clear(kg_current_mutator(r)->store_observed);return;}
        if(s==GC_RLOAD_CANCELLED){if(reason==GC_RLOAD_EXHAUSTED)kg_fail("reference store resources exhausted",reason);kg_root_publication_end(r);if(reason==GC_RLOAD_METADATA_FULL)kg_lease_pressure(r);kg_poll();kg_root_publication_begin(r);s=gc_rooted_store_dispatch(&d,&q,GC_RSD_REARM);cancelling=false;}
        if(kg_retry_event(s)&&!cancelling){reason=s;(void)gc_rooted_store_dispatch(&d,&q,GC_RSD_CANCEL);cancelling=true;}
        else if(s==GC_RLOAD_INVALID||s==GC_RLOAD_ERROR||s==GC_RLOAD_NULL_RECEIVER)kg_fail("reference store",s);
    }
}
#include "klassic_gen_weak_impl.h"
#include "klassic_gen_bulk_impl.h"
#include "klassic_gen_atomic_ref_impl.h"
#include "klassic_gen_mutator.h"

void kg_shutdown(void){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    if(!kg_current_mutator(r)->bootstrap)kg_fail("global shutdown requires bootstrap owner",0);
    if(kg_managed_callback_active())kg_fail("shutdown inside managed callback",0);
    kg_lock(r);kg_pin_owner_can_detach_locked(r,kg_context(r));kg_unlock(r);
    atomic_fetch_or_explicit(&kg_admission_gate,1,memory_order_acq_rel);
    kg_lock(r);r->admission_closed=1;kg_threads_shutdown_request_locked(r);kg_unlock(r);kg_enter_native(r);
#ifdef KG_RUNTIME_ADMISSION_CLOSED_HOOK
    KG_RUNTIME_ADMISSION_CLOSED_HOOK(r);
#endif
    for(;;){kg_lock(r);bool alone=r->mutator_count==1&&!r->attaching&&!r->detaching;kg_unlock(r);if(alone&&atomic_load_explicit(&kg_admission_gate,memory_order_acquire)==1)break;kg_driver_step(r);kg_yield();}
    kg_threads_shutdown_native(r);kg_leave_native(r);
    kg_policy_begin_shutdown(r);
    kg_root_release(kg_current_mutator(r)->store_observed);kg_current_mutator(r)->store_observed=NULL;
    for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next)if(b->live)kg_fail("live roots at shutdown",b->live);
    kg_lock(r);kg_close_tlab_locked(r);kg_unlock(r);
    kg_drain_journals(r);
    /* Retire registrations first; driver_stop performs actual final source and
     * logical-record retirement after every public/native root is gone. */
    for(KgRootBlock *b=kg_current_mutator(r)->root_blocks;b;b=b->next)kg_retire_roots(r,b);
    kg_weak_drop_all(r);kg_driver_stop(r);
    KgRootBlock *b=kg_current_mutator(r)->root_blocks;
    while(b){
        KgRootBlock *next=b->next;
        if(!b->initial){
            kg_enter_native(r);uint64_t s,failures=0;
            while((s=gc_native_roots_dispose(&b->storage,&r->os.metadata,gc_root_storage_can_dispose))==GC_NATIVE_METADATA_OS_ERROR)
                kg_os_retry(&failures,"root storage disposal OS retry limit");
            if(s)kg_fail("root storage unmap",s);
            kg_lock(r);s=gc_budget_step_locked(&r->native.backing.heap,&b->storage,GC_BUDGET_RELEASE);kg_unlock(r);
            kg_leave_native(r);if(s)kg_fail("root storage release",s);
            kg_control_unmap(r,&b->control);
        }
        b=next;
    }
    kg_current_mutator(r)->root_blocks=NULL;r->closing=1;
    kg_enter_native(r);uint64_t s,failures=0;
    while((s=kg_policy_dispose_native(r))==GC_NATIVE_METADATA_OS_ERROR)
        kg_os_retry(&failures,"retained heap backing disposal OS retry limit");
    if(s)kg_fail("retained heap backing disposal",s);
    kg_pin_dispose_native(r,kg_context(r));kg_weak_dispose_native(r,kg_context(r));kg_lease_dispose_native(r,kg_context(r));
    failures=0;
    /* Bootstrap disposal is itself resumable: successful prefix unmaps stay
     * recorded in the private stage-4 owner until the final OS release. */
    while((s=gc_native_runtime_dispose(&r->native,&r->os))==GC_NATIVE_METADATA_OS_ERROR)
        kg_os_retry(&failures,"native runtime disposal OS retry limit");
    if(s)kg_fail("native runtime disposal",s);
    r->initialized=0;r->primary.state=KG_MUTATOR_DETACHED;kg_mutator=NULL;
#ifndef _WIN32
    if(pthread_cond_destroy(&r->changed)||pthread_mutex_destroy(&r->lock))kg_fail("OS synchronization disposal",1);
#endif
}
