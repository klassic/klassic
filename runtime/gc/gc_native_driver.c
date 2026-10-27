/* OS collector driver for the canonical C mutator adapter.
 * The sole collector actor is registered by the runtime-owned r->driver
 * control and its unique reserved identity, independently of mutator handshake
 * membership. Busy ownership serializes that actor and every phase change;
 * raw access_epoch must be zero at either end of an OS-worker/owner-assist
 * handoff. Its pending work remains registered in real root walks, journal
 * leases, NativeMarkScan controls, queues and relocation gates. This is the
 * single-collector protocol, not a general exemption for unregistered actors.
 */
#include "klassic_gen_runtime_internal.h"
#include "klassic_gen_weak_internal.h"
#include "klassic_gen_remembered.h"
#include "klassic_gen_policy.h"
#include "gc_buffer_job.h"
#include "gc_relocation_claim.h"
#include "gc_relocation_complete.h"
#include "gc_survivor_age.h"
#include "gc_native_allocation_retire.h"
#include "gc_native_records.h"
#include "gc_native_source_uncommit.h"
#include "gc_heap_incoming.h"
#include "gc_forward_incoming.h"
#include <errno.h>
#include <time.h>
#ifdef _WIN32
typedef HANDLE KgCollectorThread;
#else
typedef pthread_t KgCollectorThread;
#endif
typedef struct KgDriver {
    MetadataCharge charge;
    KgRuntime *runtime;
    KgCollectorThread thread;
    MutatorContext actor;
    uint64_t busy,stop,requested[2],state,generation,serial,next_serial,young_cycles;
    uint64_t background_old,old_root_started,old_serial,selection_cutoff,cleanup_waiters;
    uint64_t old_frozen,old_scan_quanta,old_pin_done,young_old_normal_remaining,young_old_normal_tail,young_old_stage,young_old_index,young_old_node,young_old_tail;
    uint64_t rebind_stage,rebind_index,rebind_node,rebind_tail,rebind_revision,rebind_normal_tail,rebind_normal_remaining;
    KgRun *rebind_source;
    KgRun *snapshot,*cursor,*target[2];
    uint64_t object_index,root_started,heap_started;
    RootWalk heal_walk;
    PendingMark object;
    uint64_t payload,descriptor,record,live,target_generation,copy_stage;
    RelocationPermit permit;
    RelocationClaim claim;
    SourceAccessRequest source;
    RelocationCopy copy;
    RelocationJob job;
    RelocationCompletion completion;
    NativeRunRetireCheck retirement;
    uint64_t retirement_root_revision,transport_identity;
    SourceUncommit uncommit;
    uint64_t source_os_failures,record_os_failures;
    KgMetadataRetire control_retire;
    KgRun *record_next;
    KgRun *old_young_cursor;
    uint64_t old_young_started,old_young_index,old_young_visits;
    KgRun *pin_cursor;
    uint64_t pin_started,pin_index,pin_visits;
    uint64_t field_index,heap_index;
    KgRun *heap_cursor;
    uint64_t closure_stage,closure_index,closure_field,closure_gates[2];
    KgRun *closure_cursor;
    ForwardIncoming closure_forward;
} KgDriver;
static _Thread_local KgDriver *kg_owned_driver;
static void kgd_scan(KgDriver *,unsigned);
static bool kgd_copy(KgDriver *);
static void kgd_native(KgDriver *,bool);
static void kgd_lease_pressure(KgDriver *d){
    KgRuntime *r=d->runtime;kgd_native(d,true);
    uint64_t status=kg_lease_grow_native(r,&d->actor);kgd_native(d,false);
    if(status==KG_LEASE_LIMIT){
        kg_lock(r);if(kg_leases(r)->free_head)status=KG_LEASE_OK;
        else if(kg_lease_growing_locked(r))status=KG_LEASE_WAIT;kg_unlock(r);
        if(status==KG_LEASE_LIMIT)kg_fail("collector metadata lease backing budget or index limit",status);
    }
    if(status!=KG_LEASE_OK&&status!=KG_LEASE_WAIT)kg_fail("collector metadata lease growth",status);
    if(status==KG_LEASE_WAIT)kg_yield();
}
static void kgd_remembered_visit(KgRuntime *,const RememberedRegistration *,const RememberedJob *,
    const RememberedOwner *,const MetadataLeaseRequest *,KgRememberedResult *,void *);
static void kgd_check(const char *operation,uint64_t got,uint64_t expected){if(got!=expected)kg_fail(operation,got);}
static void kgd_adopt(KgDriver *d){
    KgRuntime *r=d->runtime;
    if(atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire))kg_fail("collector raw guard across quantum",0);
    atomic_store_explicit(&d->actor.adopted_phase,r->native.roots.phase,memory_order_release);
    atomic_store_explicit(&d->actor.state,GC_RUNNING,memory_order_release);
}
static void kgd_exit(PendingMark *p){
    uint64_t status;do{status=gc_pending_mark_cleanup(NULL,p,GC_RAC_EXIT);}while(status==GC_RESOLVE_WAIT);
    if(status!=GC_RF_DONE)kg_fail("collector source guard exit",status);
}
static void kgd_release(KgDriver *d,PendingMark *p){
    KgRuntime *r=d->runtime;kg_lock(r);
    uint64_t s=gc_pending_mark_cleanup(kg_leases(r),p,GC_RAC_RELEASE);kg_unlock(r);
    kgd_check("collector metadata release",s,GC_RF_DONE);
}
static uint64_t kgd_enter(KgDriver *d,PendingMark *p){
    KgRuntime *r=d->runtime;
    for(;;){
        kg_lock(r);uint64_t s=gc_pending_mark_enter(kg_leases(r),p);kg_unlock(r);
        if(s==GC_RAC_READY||s==GC_RF_DONE)return s;
        if(s==GC_RF_METADATA_FULL)kgd_lease_pressure(d);
        else if(s!=GC_RF_ADVANCED&&s!=GC_RF_WAIT)kg_fail("collector canonical access",s);
        kg_yield();
    }
}
/* The external journal/queue/root-walk owner remains counted until completion. */
static void kgd_offer(KgDriver *d,PendingMark *p){
    KgRuntime *r=d->runtime;
    /* NativeMarkScan.child ENTER can already complete a null child. Its
     * terminal receipt must be consumed as-is, rather than RESET/replayed. */
    if(p->stage==5&&(p->route_result==GC_ROUTE_SKIP||
       (p->route_result==GC_ROUTE_READY&&(p->offer_result==GC_MARK_OK||p->offer_result==GC_MARK_DUPLICATE))))return;
    for(;;){
        uint64_t op=p->stage==0?GC_RM_ENTER:p->stage==1?GC_RM_ROUTE:
            p->stage==2?GC_RM_EXIT:p->stage==3?GC_RM_OFFER:p->stage==4?GC_RM_RELEASE:GC_RM_RESET;
        uint64_t s;
        if(op==GC_RM_ROUTE||op==GC_RM_EXIT)s=gc_pending_mark(NULL,p,op);
        else{kg_lock(r);s=gc_pending_mark(kg_leases(r),p,op);kg_unlock(r);}
        if(s==GC_RM_COMPLETE){
            if(p->route_result==GC_ROUTE_SKIP||
               (p->route_result==GC_ROUTE_READY&&(p->offer_result==GC_MARK_OK||p->offer_result==GC_MARK_DUPLICATE)))return;
            kg_lock(r);s=gc_pending_mark(kg_leases(r),p,GC_RM_RESET);kg_unlock(r);
            kgd_check("initializing mark retry",s,GC_RF_DONE);kg_yield();continue;
        }
        if(s==GC_RF_METADATA_FULL){kgd_lease_pressure(d);continue;}
        if(s==GC_RM_FULL){kgd_scan(d,(unsigned)p->generation);continue;}
        if(s==GC_RF_WAIT){kg_yield();continue;}
        if(s==GC_RF_ADVANCED||s==GC_RAC_READY||s==GC_RM_EXIT_REQUIRED||s==GC_RM_OFFER_REQUIRED||s==GC_RM_RELEASE_REQUIRED)continue;
        kg_fail("collector mark handoff",s);
    }
}
static void kgd_reference(KgDriver *d,uint64_t reference,uint64_t captured){
    if(!reference)return;
    /* A journal can retain the exact Young object whose copy this owner is
     * between slices on. Complete that same admitted job before trying to
     * follow it, so a mutator assist never waits on its own forwarding claim. */
    while(d->copy_stage&&(reference>>16)==d->claim.source_offset)(void)kgd_copy(d);
    KgRuntime *r=d->runtime;
    for(unsigned gen=0;gen<2;gen++){
        kg_lock(r);uint64_t phase=r->native.roots.phase,lane=(phase>>(gen*4))&15;
        if(lane<GC_MARK_START||lane>GC_MARK_END_CHECK){
            uint64_t old=(captured>>(gen*4))&15;
            kg_unlock(r);
            if(old>=GC_MARK_START&&old<=GC_MARK_END_CHECK)kg_fail("lost captured marking obligation",captured);
            continue;
        }
        NativeWorkStorage *w=gc_native_runtime_work(&r->native);
        PendingMark pending={.context=(uintptr_t)&d->actor,.current={.reference=reference},
            .queue=(uintptr_t)&w->queues[gen],.generation=gen,.epoch=w->queues[gen].epoch,.phase=phase};
        kg_unlock(r);kgd_offer(d,&pending);
    }
}
static void kgd_satb(KgDriver *d,MutatorContext *ctx){
    KgRuntime *r=d->runtime;
    for(;;){
        BarrierEntry entries[16];BarrierJob job={.identity=ctx->thread_id,.selector=0,.output=(uintptr_t)entries,.capacity=16};
        kg_lock(r);uint64_t s=gc_buffer_job_step(ctx,&job,GC_BJ_CLAIM);kg_unlock(r);
        if(s==GC_BJ_EMPTY||s==GC_BJ_WAIT)return;
        kgd_check("SATB claim",s,GC_BJ_OK);
        kgd_check("SATB copy",gc_buffer_job_step(ctx,&job,GC_BJ_COPY),GC_BJ_OK);
        for(uint64_t i=0;i<job.written;i++){
            kgd_reference(d,entries[i].previous.bits,entries[i].slot.record_epoch);
            if(entries[i].slot.metadata_lease){
                MetadataLeaseRequest lease={.token=entries[i].slot.metadata_lease};
                kg_lock(r);s=gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_RESOLVE);
                if(!s)s=gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_RELEASE);kg_unlock(r);
                kgd_check("SATB lease release",s,GC_ML_OK);
            }
        }
        kg_lock(r);s=gc_buffer_job_step(ctx,&job,GC_BJ_ACK);kg_unlock(r);kgd_check("SATB acknowledgement",s,GC_BJ_OK);
        return; /* One actual bounded batch; other owners cannot be starved. */
    }
}
static void kgd_transport(KgDriver *d){
    KgRuntime *r=d->runtime;kg_drain_journals(r);
    /* A native context pin retains registration and both ring packages across
     * all actual CLAIM/COPY/process/ACK work. Only scalar identity survives a
     * quantum; a detach/reused address can never become a saved next pointer.
     * Visit one registered owner per quantum in decreasing identity order. */
    kg_lock(r);MutatorContext *ctx=kg_context_acquire_after_locked(r,d->transport_identity);
    if(!ctx){d->transport_identity=0;kg_unlock(r);return;}
    d->transport_identity=ctx->thread_id;kg_unlock(r);
    kgd_satb(d,ctx);kg_remembered_drain_context(r,ctx);
    kg_lock(r);kg_context_release_locked(r,ctx);kg_unlock(r);
}
static void kgd_scan(KgDriver *d,unsigned gen){
    KgRuntime *r=d->runtime;kg_lock(r);
    if(gen==GC_OLD&&d->old_frozen){kg_unlock(r);return;}
    uint64_t lane=(r->native.roots.phase>>(gen*4))&15;
    if(lane<GC_MARK_START||lane>GC_MARK_END_CHECK){kg_unlock(r);return;}
    NativeMarkScan scan={0};uint64_t s=gc_native_mark_scan_begin_locked(&r->native,gen,&d->actor,16,&scan);kg_unlock(r);
    if(s==GC_MARK_EMPTY||s==GC_MARK_BUSY)return;
    kgd_check("collector mark claim",s,GC_MARK_OK);
    for(;;){
        kg_lock(r);s=gc_native_mark_scan_enter_locked(&scan);kg_unlock(r);
        if(s==GC_RAC_READY)break;
        if(s==GC_RF_METADATA_FULL){kgd_lease_pressure(d);continue;}
        if(s!=GC_RF_ADVANCED&&s!=GC_RF_WAIT)kg_fail("collector mark receiver",s);
        kg_yield();
    }
    s=gc_native_mark_scan_read(&scan,gc_mark_scan_slice);
    if(s!=GC_SCAN_DONE&&s!=GC_SCAN_MORE)kg_fail("collector descriptor scan",s);
    while(gc_native_mark_scan_exit(&scan)!=GC_RF_DONE){}
    kg_lock(r);s=gc_native_mark_scan_release_locked(&scan);kg_unlock(r);kgd_check("scan source release",s,GC_RF_DONE);
    while(scan.child_index<scan.scan.written){
        kg_lock(r);s=gc_native_mark_scan_child(&scan,GC_RM_ENTER,gc_pending_mark);kg_unlock(r);
        if(!scan.child.context)kg_fail("unbound mark child",s);
        kgd_offer(d,&scan.child);
        kgd_reference(d,scan.child.current.reference,r->native.roots.phase);
        kgd_check("mark child acknowledgement",gc_native_mark_scan_child_ack(&scan),GC_MARK_OK);
    }
    kg_lock(r);s=gc_native_mark_scan_commit_locked(&scan,gc_mark_scan_commit_locked);
    if(s==GC_MARK_OK&&scan.stage==6)s=gc_native_mark_scan_release_locked(&scan);
    if(s==GC_RF_DONE||s==GC_MARK_OK)s=gc_native_mark_scan_finish_locked(&scan);kg_unlock(r);
    kgd_check("collector scan commit",s,GC_MARK_OK);
}
static bool kgd_roots(KgDriver *d,bool retire){
    KgRuntime *r=d->runtime;unsigned gen=(unsigned)d->generation;
    RootAudit *audit=retire?&r->native.cycle[gen].retirement_audit:&r->native.cycle[gen].marking_audit;
    uint64_t (*step)(RootRegistry*,RootAudit*,uint64_t)=retire?gc_root_retire_audit_step_locked:gc_root_audit_step_locked;
    if(!d->root_started){
        kg_lock(r);uint64_t s=step(&r->native.roots,audit,GC_RA_BEGIN);kg_unlock(r);
        if(s==GC_RA_DONE){d->root_started=2;return true;}
        kgd_check("collector root audit begin",s,GC_RA_ITEM);d->root_started=1;
    }
    if(d->root_started==2)return true;
    RootWalk *walk=&audit->walk;uint64_t s;
    if(!retire){
        NativeRootMark mark={0};kg_lock(r);
        s=gc_native_root_mark_begin_locked(&r->native,gen,&d->actor,&mark);kg_unlock(r);
        if(s==GC_RWI_ITEM){
            kgd_offer(d,&mark.pending);walk->pending_reference=mark.pending.current.reference;
            kg_lock(r);s=gc_native_root_mark_ack_locked(&mark);kg_unlock(r);
            kgd_check("collector root mark acknowledgement",s,GC_RWI_ITEM);return false;
        }
    }else{
        RootWalkItem item={(uintptr_t)walk,walk->lease.serial,0};
        kg_lock(r);s=gc_root_walk_item(&item,GC_RWI_CAPTURE);kg_unlock(r);
        if(s==GC_RWI_ITEM){
            PendingMark value={.context=(uintptr_t)&d->actor,.current={.reference=walk->pending_reference}};
            s=kgd_enter(d,&value);
            if(s==GC_RAC_READY){kgd_exit(&value);
                uint64_t desired=(value.current.reference&~UINT64_C(0xffff))|(walk->pending_observed&0xffff);
                if(walk->pending_observed){RootWalkHeal heal={(uintptr_t)walk,walk->lease.serial,item.index,walk->pending_observed,desired};
                    kg_lock(r);s=gc_root_walk_heal(&heal);kg_unlock(r);if(s!=GC_RWH_OK&&s!=GC_RWH_CHANGED)kg_fail("retirement root heal",s);}
                kgd_release(d,&value);
            }
            kg_lock(r);s=gc_root_walk_item(&item,GC_RWI_ACK);kg_unlock(r);kgd_check("retirement root acknowledgement",s,GC_RWI_ITEM);return false;
        }
    }
    kgd_check("collector root segment complete",s,GC_RWI_DONE);
    kgd_transport(d);kg_lock(r);walk->completed_mask=walk->mask;
    s=step(&r->native.roots,audit,GC_RA_NEXT);kg_unlock(r);
    if(s==GC_RA_DONE){d->root_started=2;return true;}
    if(s!=GC_RA_WAIT)kgd_check("collector root audit next",s,GC_RA_ITEM);
    return false;
}
static KgRun *kgd_run_for_page_locked(KgRuntime *r,uint64_t page){
    for(NativeAllocationRun *n=(void *)(uintptr_t)r->native.allocations;n;n=(void *)(uintptr_t)n->next)
        if((uintptr_t)gc_native_allocation_page(n)==page)return (KgRun *)n;
    kg_fail("collector physical run ownership",page);
}
static void kgd_shape(KgDriver *d,PendingMark *p,uint64_t *payload,uint64_t *descriptor,uint64_t *record){
    KgRuntime *r=d->runtime;MutatorContext *ctx;ObjectHeader *header;
    if(!gc_guarded_object((uintptr_t)&p->access.scope,p->current.lease.page,r->native.roots.phase,&ctx,&header))kg_fail("collector published object",0);
    *payload=header->payload_bytes;*descriptor=header->descriptor_id;*record=header->mark_record;
}
static void kgd_native(KgDriver *d,bool enter){
    if(atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire))kg_fail("collector native guard",0);
    atomic_store_explicit(&d->actor.state,enter?GC_BLOCKED_NATIVE:GC_RUNNING,memory_order_release);
}
static KgRun *kgd_target(KgDriver *d,uint64_t pages,unsigned gen){
    KgRuntime *r=d->runtime;uint64_t status;
    kgd_native(d,true);KgRun *run=kg_try_reserve_run_native(r,pages,gen,&d->actor,&status);
    if(!run)kg_fail("relocation to-space exhausted",status);
    if(gen==GC_OLD){status=kg_remembered_attach(r,run,&d->actor);kgd_check("relocation remembered backing",status,GC_BUDGET_OK);}
    kgd_native(d,false);return run;
}

#include "klassic_gen_queue_frontier.h"

static bool kgd_copy(KgDriver *d){
    KgRuntime *r=d->runtime;NativeRuntimeOwner *n=&r->native;unsigned gen=(unsigned)d->generation;
    while(d->cursor&&d->cursor->selected_serial!=d->serial){
        d->cursor=(void *)(uintptr_t)d->cursor->native.next;d->object_index=0;
    }
    if(!d->cursor)return true;
    KgRun *source=d->cursor;ForwardTable *table=(void *)(uintptr_t)source->native.forwarding.allocation;
    ForwardCell *cells=(void *)(uintptr_t)table->cells;
    if(!source->selection.self){kg_lock(r);uint64_t status=gc_native_selected_source_begin_locked(&source->native,gen,&source->selection);kg_unlock(r);
        kgd_check("selected source proof admission",status,GC_SD_DONE);}
    if(d->object_index==atomic_load_explicit(&table->used,memory_order_acquire)){
        if(!kgd_old_rebind(d,source))return false;
        if(!kg_weak_rebind_source(r,source)){
            kg_lock(r);bool full=!kg_leases(r)->free_head;kg_unlock(r);
            if(full)kgd_lease_pressure(d);return false;
        }
        kg_lock(r);uint64_t status=kg_pin_source_seal_locked(r,source,16);
        if(status==GC_SD_WAIT&&d->old_frozen&&
           d->rebind_revision!=gc_native_runtime_work(n)->queues[GC_OLD].last_serial)d->rebind_stage=0;
        kg_unlock(r);
        if(status==GC_SD_WAIT||status==GC_SD_MORE)return false;
        kgd_check("completed mixed source payload seal",status,GC_SD_DONE);
        d->cursor=(void *)(uintptr_t)source->native.next;d->object_index=0;return false;
    }
    ForwardCell *cell=&cells[d->object_index];RelocationGate *gate=&n->cycle[gen].gate;
    if(!d->copy_stage){
        d->object=(PendingMark){.context=(uintptr_t)&d->actor,
            .current={.reference=(cell->source_offset<<16)|gc_abi_color((PhaseSnapshot){n->roots.phase})}};
        kgd_check("relocation source access",kgd_enter(d,&d->object),GC_RAC_READY);
        kgd_shape(d,&d->object,&d->payload,&d->descriptor,&d->record);kgd_exit(&d->object);
        kg_lock(r);MarkRecord *record=(void *)(uintptr_t)d->record;
        uint64_t epoch=gc_native_runtime_work(n)->queues[gen].epoch;
        d->live=(gen?record->old_epoch:record->young_epoch)==epoch;
        d->target_generation=gen;
        if(d->live&&!gen){
            uint64_t age=gc_marked_survivor_age_locked(&gc_native_runtime_work(n)->queues[0],record,2);
            if(age!=GC_AGE_YOUNG&&age!=GC_AGE_PROMOTE)kg_fail("survivor age selection",age);
            d->target_generation=age==GC_AGE_PROMOTE?GC_OLD:GC_YOUNG;
        }
        d->claim=(RelocationClaim){.permit=(uintptr_t)&d->permit,.source=(uintptr_t)cell,
            .source_offset=cell->source_offset,.incarnation=cell->incarnation};
        uint64_t status=gc_native_selected_source_claim_locked(&source->selection,&d->object.current.lease,d->record,&d->claim);kg_unlock(r);
        kgd_check("relocation admitted claim",status,GC_RCL_OWNED);
        d->source=(SourceAccessRequest){.source_offset=cell->source_offset,.incarnation=cell->incarnation,.ticket=d->claim.ticket};
        d->copy_stage=1;
    }
    if(d->copy_stage==1){
        uint64_t status=gc_source_access_step(cell,&d->source,GC_SA_SELECT);
        if(status==GC_SA_WAIT)return false;kgd_check("relocation source close",status,GC_SA_OK);
        status=gc_source_access_step(cell,&d->source,GC_SA_CHECK);
        if(status==GC_SA_WAIT)return false;kgd_check("relocation source drain",status,GC_SA_OK);
        if(!d->live){
            ForwardRequest retain={cell->source_offset,cell->incarnation,d->claim.ticket,0};
            kgd_check("dead object source seal",gc_forward_step(cell,&retain,GC_FWD_RETAIN),GC_FWD_OK);
            RelocationGateRequest permit={(uintptr_t)&d->permit,d->claim.ticket,d->claim.epoch};
            kg_lock(r);status=gc_relocation_gate_locked(gate,&permit,GC_RG_RELEASE);kg_unlock(r);
            kgd_check("dead source permit release",status,GC_RG_OK);
            kg_lock(r);status=gc_native_selected_source_commit_locked(&source->selection,&d->object.current.lease,&d->claim);kg_unlock(r);
            kgd_check("selected dead slot proof",status,GC_SD_DONE);kgd_release(d,&d->object);
            d->object=(PendingMark){0};d->copy_stage=0;d->object_index++;return false;
        }
        d->copy=(RelocationCopy){.heap_base=n->backing.heap.heap_base,.source_incarnation=cell->incarnation,
            .ticket=d->claim.ticket,.source_limit=gc_native_allocation_page(&source->native)->limit,.budget=256};
        unsigned target_gen=(unsigned)d->target_generation;
        KgRun *target=d->target[target_gen];
        uint64_t total=sizeof(ObjectHeader)+((d->payload+15)&~UINT64_C(15));if(!d->payload)total+=16;
        if(target){
            AllocationPage *page=gc_native_allocation_page(&target->native);
            ForwardTable *forward=(void *)(uintptr_t)page->forward_table;
            kg_lock(r);uint64_t top=atomic_load_explicit(&page->allocated_top,memory_order_acquire);
            bool fits=total<=page->limit-top&&atomic_load_explicit(&forward->used,memory_order_acquire)<forward->capacity;kg_unlock(r);
            if(!fits)target=NULL;
        }
        if(!target){uint64_t pages=(total+GC_HEAP_PAGE_BYTES-1)/GC_HEAP_PAGE_BYTES;
            target=kgd_target(d,pages,target_gen);d->target[target_gen]=target;}
        kg_lock(r);status=gc_relocate_target_locked(gc_native_allocation_page(&target->native),&d->copy,GC_TARGET_PREPARE);kg_unlock(r);
        kgd_check("relocation target preparation",status,GC_TARGET_OK);d->copy_stage=2;
    }
    if(d->copy_stage==2){
        uint64_t status=gc_relocate_copy(cell,&d->copy);
        if(status==GC_COPY_MORE||status==GC_COPY_WAIT)return false;
        kgd_check("relocation payload copy",status,GC_COPY_DONE);
        KgRun *target=d->target[d->target_generation];
        kg_lock(r);NativeAllocationRun *record_owner=(void *)(uintptr_t)((uint64_t *)(uintptr_t)source->native.imports)[d->object_index];
        if(!record_owner)record_owner=&source->native;
        status=gc_native_allocation_install_copy_locked(&target->native,record_owner,&d->copy);
        if(status==GC_TARGET_OK)target->closed_phase=n->roots.phase>>16;kg_unlock(r);
        kgd_check("relocation target installation",status,GC_TARGET_OK);
        if(d->target_generation==GC_OLD){
            kg_remembered_promote(r,target,d->copy.destination,d->descriptor,d->payload,n->remembered.epoch);
            kgd_reference(d,(d->copy.destination<<16)|gc_abi_color((PhaseSnapshot){n->roots.phase}),n->roots.phase);
        }
        d->job=(RelocationJob){.permit=(uintptr_t)&d->permit,.source=(uintptr_t)cell,.ticket=d->claim.ticket,
            .destination=(d->copy.destination<<16)|gc_abi_color((PhaseSnapshot){n->roots.phase})};
        d->completion=(RelocationCompletion){.job=(uintptr_t)&d->job,.copy=(uintptr_t)&d->copy};d->copy_stage=3;
    }
    kg_lock(r);uint64_t status=gc_relocation_complete_locked(gate,&d->completion);kg_unlock(r);
    if(status==GC_RC_MORE||status==GC_RC_WAIT)return false;
    kgd_check("relocation forwarding completion",status,GC_RC_DONE);
    kg_lock(r);status=gc_native_selected_source_commit_locked(&source->selection,&d->object.current.lease,&d->claim);kg_unlock(r);
    kgd_check("selected live slot proof",status,GC_SD_DONE);
    kgd_release(d,&d->object);d->object=(PendingMark){0};d->copy_stage=0;d->object_index++;return false;
}
/* Canonical receiver field read -> canonical child -> exact-observation heal.
 * Every managed address is confined to a source guard. Independent leases keep
 * both references alive between the finite observation and CAS scopes. */
static uint64_t kgd_field(KgDriver *d,uint64_t owner,uint64_t offset,uint64_t *canonical_owner){
    KgRuntime *r=d->runtime;PendingMark base={.context=(uintptr_t)&d->actor,.current={.reference=owner}};
    kgd_check("collector field receiver",kgd_enter(d,&base),GC_RAC_READY);
    uint64_t payload,descriptor,record;kgd_shape(d,&base,&payload,&descriptor,&record);(void)record;
    DescriptorScan scan={descriptor,payload,offset,1,(uintptr_t)&offset,1,0,0};
    uint64_t status=gc_descriptor_scan(kg_descriptors(r),&scan);if(status==GC_SCAN_INVALID||!scan.written)kg_fail("collector reference slot",offset);
    _Atomic uint64_t *slot=(void *)(uintptr_t)(r->native.backing.heap.heap_base+(base.current.reference>>16)+offset);
    uint64_t observed=atomic_load_explicit(slot,memory_order_acquire);slot=NULL;kgd_exit(&base);
    PendingMark child={.context=(uintptr_t)&d->actor,.current={.reference=observed}};
    status=kgd_enter(d,&child);uint64_t value=0;
    if(status==GC_RAC_READY){value=child.current.reference;kgd_exit(&child);}
    kgd_check("collector field receiver reacquire",kgd_enter(d,&base),GC_RAC_READY);
    if(observed&&value){
        uint64_t desired=(value&~UINT64_C(0xffff))|
            ((gc_abi_color((PhaseSnapshot){r->native.roots.phase})&~UINT64_C(0x30))|(observed&0x30));
        GuardedHeal heal={(uintptr_t)&base.access.scope,base.current.lease.page,(uintptr_t)kg_descriptors(r),offset,
            observed,desired,r->native.roots.phase};
        status=gc_guarded_heal(&heal);if(status!=GC_HEAL_OK&&status!=GC_HEAL_CHANGED)kg_fail("collector exact field heal",status);
    }
    *canonical_owner=base.current.reference;kgd_exit(&base);kgd_release(d,&base);
    if(observed)kgd_release(d,&child);
    return value;
}
static void kgd_remembered_visit(KgRuntime *r,const RememberedRegistration *registration,const RememberedJob *job,
        const RememberedOwner *owner,const MetadataLeaseRequest *lease,KgRememberedResult *result,void *opaque){
    (void)registration;(void)job;KgDriver *d=opaque;
    ForwardCell *source_cell=(void *)(uintptr_t)owner->cell;
    if(atomic_load_explicit(&source_cell->word,memory_order_acquire)==3&&
       atomic_load_explicit(&source_cell->access,memory_order_acquire)==GC_SOURCE_CLOSED){
        /* Only this selected Old cycle owns a retained, permanently closed
         * dead-source cell. Its immutable payload is still committed and the
         * caller's metadata lease protects the source and imported arena. */
        kg_lock(r);KgRun *source_run=kgd_run_for_page_locked(r,lease->page);
        NativeSelectedSource *proof=&source_run->selection;
        if(source_run->selected_serial!=d->serial||source_run->selected_generation!=GC_OLD||
           source_run->selected_epoch!=gc_native_runtime_work(&r->native)->queues[GC_OLD].epoch||
           proof->self!=(uintptr_t)proof||proof->run!=&source_run->native||proof->active||
           proof->cycle!=source_run->selected_cycle||proof->epoch!=source_run->selected_epoch||
           proof->cursor!=proof->used||proof->seal_cursor!=proof->used)
            kg_fail("unproved remembered owner death",0);
        ObjectHeader *header=(void *)(uintptr_t)(r->native.backing.heap.heap_base+owner->owner-sizeof(ObjectHeader));
        result->dead_record=(void *)(uintptr_t)header->mark_record;result->dead_epoch=source_run->selected_epoch;
        kg_unlock(r);return;
    }
    /* A bitmap owner is a physical scan candidate, not a mark-good root.
     * Syntactically valid opposite mark bits deliberately grant no good-color
     * receipt. Its claimed page lease retains allocation/source metadata; the
     * canonical guard validates the actual header and reference descriptor.
     * Only the child's subsequent guarded record/queue handoff grants marking. */
    uint64_t canonical_owner=0;
    uint64_t value=kgd_field(d,(owner->owner<<16)|(gc_abi_color((PhaseSnapshot){r->native.roots.phase})^
        GC_YOUNG_MARK_MASK^GC_OLD_MARK_MASK),owner->byte_offset,&canonical_owner);
    kg_lock(r);
    uint64_t page=0;
    if(value){uint64_t index=(value>>16)/GC_HEAP_PAGE_BYTES-1;
        AllocationPage *p=(void *)(uintptr_t)r->native.backing.heap.pages;
        page=(uintptr_t)&p[p[index].run_head];}
    bool young=value&&((AllocationPage *)(uintptr_t)page)->generation==GC_YOUNG;
    uint64_t base_index=(canonical_owner>>16)/GC_HEAP_PAGE_BYTES-1;
    AllocationPage *pages=(void *)(uintptr_t)r->native.backing.heap.pages;
    KgRun *target=kgd_run_for_page_locked(r,(uintptr_t)&pages[pages[base_index].run_head]);
    uint64_t phase=r->native.roots.phase,lane=phase&15;
    PendingMark child={.context=(uintptr_t)&d->actor,.current={.reference=value},.generation=GC_YOUNG,
        .queue=(uintptr_t)&gc_native_runtime_work(&r->native)->queues[0],
        .epoch=gc_native_runtime_work(&r->native)->queues[0].epoch,.phase=phase};kg_unlock(r);
    /* A conservative Old bitmap is a source of Young work only. Offering an
     * Old field to the Old queue here would root unreachable Old cycles. */
    if(young&&lane>=GC_MARK_START&&lane<=GC_MARK_END_CHECK)kgd_offer(d,&child);
    if((canonical_owner>>16)!=owner->owner){result->target=target;result->target_offset=(canonical_owner>>16)+owner->byte_offset;}
    else result->keep=young;
}
static bool kgd_heap_heal(KgDriver *d){
    KgRuntime *r=d->runtime;
    if(!d->heap_started){kg_lock(r);d->heap_cursor=(void *)(uintptr_t)r->native.allocations;kg_unlock(r);
        d->heap_index=d->field_index=0;d->heap_started=1;}
    KgRun *run=d->heap_cursor;
    if(!run)return true;
    uint64_t stage=atomic_load_explicit(&run->native.stage,memory_order_acquire);
    if((stage!=3&&stage!=4&&stage!=8)||run->selected_serial==d->serial||!run->native.forwarding.allocation){
        d->heap_cursor=(void *)(uintptr_t)run->native.next;d->heap_index=d->field_index=0;return false;}
    ForwardTable *table=(void *)(uintptr_t)run->native.forwarding.allocation;
    uint64_t used=atomic_load_explicit(&table->used,memory_order_acquire);
    if(d->heap_index==used){d->heap_cursor=(void *)(uintptr_t)run->native.next;d->heap_index=d->field_index=0;return false;}
    ForwardCell *cell=(ForwardCell *)(uintptr_t)table->cells+d->heap_index;
    if(atomic_load_explicit(&cell->word,memory_order_acquire)){d->heap_index++;d->field_index=0;return false;}
    uint64_t reference=(cell->source_offset<<16)|gc_abi_color((PhaseSnapshot){r->native.roots.phase});
    PendingMark probe={.context=(uintptr_t)&d->actor,.current={.reference=reference}};
    kgd_check("heap repair receiver",kgd_enter(d,&probe),GC_RAC_READY);
    MutatorContext *actor;ObjectHeader *header;uint64_t object_state;
    if(!gc_guarded_object_state((uintptr_t)&probe.access.scope,probe.current.lease.page,
        r->native.roots.phase,true,&actor,&header,&object_state))kg_fail("heap repair object state",0);
    if(object_state==GC_OBJECT_INITIALIZING){
        header=NULL;
        /* This adapter publishes readiness while every reference slot is
         * still zero. Post-publication stores canonicalize their inputs, so
         * a private initializing suffix cannot introduce an obsolete range.
         * The subsequent real HeapIncoming closure verifies the range too. */
        kgd_exit(&probe);kgd_release(d,&probe);d->heap_index++;d->field_index=0;return false;
    }
    uint64_t payload=header->payload_bytes,descriptor=header->descriptor_id,offset=0;header=NULL;
    DescriptorScan scan={descriptor,payload,d->field_index,1,(uintptr_t)&offset,1,0,0};
    uint64_t s=gc_descriptor_scan(kg_descriptors(r),&scan);
    if(s==GC_SCAN_INVALID)kg_fail("heap repair descriptor",s);
    kgd_exit(&probe);reference=probe.current.reference;kgd_release(d,&probe);
    if(scan.written){uint64_t receiver;(void)kgd_field(d,reference,offset,&receiver);d->field_index=scan.next_offset;}
    else{d->heap_index++;d->field_index=0;}
    return false;
}
/* The same closed mover gates freeze publication and compression throughout
 * this real forwarding/heap absence audit. New mutator allocations publish
 * zero references, and every later field/root store canonicalizes first; they
 * cannot reintroduce this obsolete range behind a completed cursor. */
static bool kgd_closure(KgDriver *d,KgRun *source){
    KgRuntime *r=d->runtime;AllocationPage *retiring=gc_native_allocation_page(&source->native);
    kg_lock(r);
    for(unsigned g=0;g<2;g++){
        RelocationGate *gate=&r->native.cycle[g].gate;
        if(gc_relocation_gate_locked(gate,NULL,GC_RG_CHECK)!=GC_RG_OK)kg_fail("source closure mover gate",g);
        if(d->closure_stage&&d->closure_gates[g]!=gate->epoch)kg_fail("source closure gate changed",g);
        d->closure_gates[g]=gate->epoch;
    }
    if(!d->closure_stage){d->closure_stage=1;d->closure_cursor=(void *)(uintptr_t)r->native.allocations;}
    KgRun *run=d->closure_cursor;
    if(d->closure_stage==1){
        if(!run){d->closure_stage=2;d->closure_cursor=(void *)(uintptr_t)r->native.allocations;kg_unlock(r);return false;}
        uint64_t stage=atomic_load_explicit(&run->native.stage,memory_order_acquire);
        if((stage!=3&&stage!=4&&stage!=8)||!run->native.forwarding.allocation){d->closure_cursor=(void *)(uintptr_t)run->native.next;kg_unlock(r);return false;}
        ForwardTable *table=(void *)(uintptr_t)run->native.forwarding.allocation;
        uint64_t used=atomic_load_explicit(&table->used,memory_order_acquire);
        if(!d->closure_forward.incarnation||d->closure_forward.used!=used)
            d->closure_forward=(ForwardIncoming){.incarnation=run->native.lease.incarnation,.used=used,
                .target_start=retiring->start,.target_limit=retiring->limit,.budget=16};
        uint64_t status=gc_forward_incoming(table,&d->closure_forward);
        if(status==GC_FI_DONE){d->closure_cursor=(void *)(uintptr_t)run->native.next;d->closure_forward=(ForwardIncoming){0};}
        kg_unlock(r);
        if(status!=GC_FI_DONE&&status!=GC_FI_MORE)kg_fail("incoming forwarding closure",status);
        return false;
    }
    if(!run){kg_unlock(r);return true;}
    uint64_t stage=atomic_load_explicit(&run->native.stage,memory_order_acquire);
    if((stage!=3&&stage!=4&&stage!=8)||run->selected_serial==d->serial||!run->native.forwarding.allocation){
        d->closure_cursor=(void *)(uintptr_t)run->native.next;d->closure_index=d->closure_field=0;kg_unlock(r);return false;}
    ForwardTable *table=(void *)(uintptr_t)run->native.forwarding.allocation;
    if(d->closure_index==atomic_load_explicit(&table->used,memory_order_acquire)){
        d->closure_cursor=(void *)(uintptr_t)run->native.next;d->closure_index=d->closure_field=0;kg_unlock(r);return false;}
    ForwardCell *cell=(ForwardCell *)(uintptr_t)table->cells+d->closure_index;
    if(atomic_load_explicit(&cell->word,memory_order_acquire)){d->closure_index++;d->closure_field=0;kg_unlock(r);return false;}
    uint64_t key=(cell->source_offset<<16)|(gc_abi_color((PhaseSnapshot){r->native.roots.phase})^GC_YOUNG_MARK_MASK^GC_OLD_MARK_MASK);
    kg_unlock(r);
    PendingMark probe={.context=(uintptr_t)&d->actor,.current={.reference=key}};
    kgd_check("heap closure receiver",kgd_enter(d,&probe),GC_RAC_READY);
    MutatorContext *actor;ObjectHeader *header;uint64_t state;
    if(!gc_guarded_object_state((uintptr_t)&probe.access.scope,probe.current.lease.page,r->native.roots.phase,true,&actor,&header,&state))
        kg_fail("heap closure object state",0);
    header=NULL;
    if(state==GC_OBJECT_INITIALIZING){
        /* This adapter release-publishes a zeroed allocation before its first
         * field store. An initializing post-snapshot object has no old refs. */
        kgd_exit(&probe);kgd_release(d,&probe);d->closure_index++;d->closure_field=0;return false;
    }
    HeapIncoming incoming={.scope=(uintptr_t)&probe.access.scope,.page=probe.current.lease.page,
        .registry=(uintptr_t)kg_descriptors(r),.phase=r->native.roots.phase,
        .target_start=retiring->start,.target_limit=retiring->limit,.cursor=d->closure_field};
    uint64_t status=gc_heap_incoming(&incoming),canonical=probe.current.reference;
    kgd_exit(&probe);
    if(status==GC_HI_FOUND){
        /* Reread the actual field under its new canonical guard, exact-CAS
         * repair it, then recheck this SAME absence cursor on the next step. */
        uint64_t receiver;(void)kgd_field(d,canonical,incoming.cursor,&receiver);d->closure_field=incoming.cursor;
    }else if(status==GC_HI_MORE)d->closure_field=incoming.cursor;
    else if(status==GC_HI_DONE){d->closure_index++;d->closure_field=0;}
    else kg_fail("heap incoming closure",status);
    kgd_release(d,&probe);return false;
}
static bool kgd_retire_run(KgDriver *d){
    KgRuntime *r=d->runtime;
    while(d->cursor&&d->cursor->selected_serial!=d->serial)d->cursor=(void *)(uintptr_t)d->cursor->native.next;
    if(!d->cursor)return true;
    KgRun *run=d->cursor;NativeAllocationRun *source=&run->native;uint64_t s;
    if(source->stage==8){
        kg_lock(r);bool weak_absent=kg_weak_incoming_source_locked(r,run);kg_unlock(r);
        if(!weak_absent)return false;
        kg_lock(r);bool pin_selected=kg_pin_source_selected_locked(r,run);kg_unlock(r);
        if(!pin_selected)kg_fail("source lost pin selection",0);
        if(!d->uncommit.stage){
            if(!kgd_closure(d,run))return false;
            if(run->remembered){
                kg_remembered_migrate_run(r,run,kgd_remembered_visit,d);
                kgd_native(d,true);s=kg_pin_remembered_detach(r,run,&d->actor);kgd_native(d,false);
                if(s==GC_RS_WAIT)return false;kgd_check("source remembered retirement",s,GC_RS_OK);
            }
            d->uncommit=(SourceUncommit){.head_index=source->lease.head_index,.page_count=source->lease.page_count,.incarnation=source->lease.incarnation};
            kg_lock(r);s=gc_source_uncommit_locked(&r->native.backing.heap,&d->uncommit,GC_SU_BEGIN);kg_unlock(r);
            if(s==GC_SR_WAIT){d->uncommit=(SourceUncommit){0};return false;}kgd_check("source uncommit admission",s,GC_SU_OK);
        }
        if(d->uncommit.stage==1){
            kgd_native(d,true);s=kg_policy_source_uncommit(r,&d->uncommit,&d->actor);
            if(s==GC_NATIVE_METADATA_OS_ERROR){kg_os_retry(&d->source_os_failures,"source payload OS retry limit");kgd_native(d,false);return false;}
            kgd_native(d,false);kgd_check("source OS uncommit",s,GC_NATIVE_METADATA_OK);d->source_os_failures=0;
            kg_lock(r);s=gc_source_uncommit_locked(&r->native.backing.heap,&d->uncommit,GC_SU_FINISH);kg_unlock(r);
            kgd_check("source uncommit receipt",s,GC_SU_OK);
        }
        if(!d->retirement.self){kg_lock(r);
            d->retirement_root_revision=kg_root_revision_locked(r);
            s=gc_native_allocation_retire_check_begin_locked(source,&d->retirement);
            if(s==GC_NSI_MORE&&d->old_frozen)s=gc_native_queue_incoming_prefer_fifo_locked(&d->retirement.queues);
            kg_unlock(r);
            kgd_check("source incoming admission",s,GC_NSI_MORE);return false;}
        kg_lock(r);
        /* Every owner's logical root publication participates in this actual
         * runtime revision. Outside-lock rooted controllers register before
         * ACCESS and keep their publication admitted through cleanup/revision
         * transfer. Final consume uses the SAME lock as their admission, so a
         * guarded STORE cannot hide between its cell write and revision update.
         * Physical source-away CAS healing does not change logical revision. */
        if(d->retirement_root_revision!=kg_root_revision_locked(r)){
            kgd_check("root publication audit restart",gc_native_allocation_retire_check_cancel_locked(&d->retirement),GC_NSI_DONE);
            d->retirement=(NativeRunRetireCheck){0};kg_unlock(r);return false;
        }
        s=gc_native_allocation_retire_check_next_locked(&d->retirement);
        if(s==GC_NSI_RESTART){
            kgd_check("source incoming audit cancel",gc_native_allocation_retire_check_cancel_locked(&d->retirement),GC_NSI_DONE);
            d->retirement=(NativeRunRetireCheck){0};kg_unlock(r);return false;
        }
        if(s==GC_NSI_DONE){
            if(kg_root_publications_locked(r)){kg_unlock(r);return false;}
            if(!kg_pin_source_retire_locked(r,run)){kg_unlock(r);return false;}
            s=gc_native_allocation_retire_begin_locked(source,&d->retirement);kg_unlock(r);
            kgd_check("source retirement admission",s,GC_SR_MORE);return false;
        }
        kg_unlock(r);
        if(s==GC_NSI_FOUND){
            if(d->retirement.roots.observed){
                if(d->root_started==2)d->root_started=0;
                (void)kgd_roots(d,true);
            }else kg_fail("unexpected queued/scanned source after closure",s);
            return false;
        }
        if(s==GC_NSI_MORE||s==GC_NSI_WAIT)return false;
        kg_fail("source incoming audit",s);
    }
    if(source->stage==9){
        if(source->imports){
            kg_lock(r);s=gc_native_allocation_retire_import_locked(source);kg_unlock(r);
            if(s==GC_SR_MORE)return false;kgd_check("source imported record handoff",s,GC_SR_DONE);
        }
        if(source->forwarding.allocation||source->bitmap.mapping){
            kgd_native(d,true);s=gc_native_allocation_retire_cleanup_native(source,&r->os.metadata,&d->actor);
            if(s==GC_NATIVE_RUN_OS_ERROR){kg_os_retry(&d->source_os_failures,"source metadata OS retry limit");kgd_native(d,false);return false;}
            kgd_native(d,false);kgd_check("source metadata OS cleanup",s,GC_POOL_OK);d->source_os_failures=0;
        }
        kg_lock(r);s=gc_native_allocation_retire_step_locked(source);kg_unlock(r);
        if(s==GC_SR_MORE)return false;kgd_check("physical source retirement",s,GC_SR_DONE);
    }
    kgd_check("physically retired source stage",source->stage,10);
    d->cursor=(void *)(uintptr_t)source->next;d->retirement=(NativeRunRetireCheck){0};d->uncommit=(SourceUncommit){0};
    d->closure_stage=d->closure_index=d->closure_field=0;d->closure_cursor=NULL;d->closure_forward=(ForwardIncoming){0};return false;
}
static bool kgd_control_retire(KgDriver *d,MetadataCharge *source){
    KgRuntime *r=d->runtime;kgd_native(d,true);
    uint64_t status=kg_metadata_retire_step_native(r,source,&d->control_retire,&d->actor);kgd_native(d,false);
    if(status==GC_NATIVE_METADATA_OS_ERROR)return false;
    kgd_check("retired run control backing",status,GC_NATIVE_METADATA_OK);
    d->control_retire=(KgMetadataRetire){0};return true;
}
static bool kgd_records(KgDriver *d){
    KgRuntime *r=d->runtime;
    if(d->control_retire.charge.stage){
        if(!kgd_control_retire(d,NULL))return false;
        return d->cursor==NULL;
    }
    if(!d->cursor){kg_lock(r);d->cursor=(void *)(uintptr_t)r->native.allocations;kg_unlock(r);if(!d->cursor)return true;}
    KgRun *run=d->cursor;uint64_t s;
    if(run->native.stage==10){
        kg_lock(r);d->record_next=(void *)(uintptr_t)run->native.next;s=gc_native_records_begin_locked(&run->native);kg_unlock(r);
        if(s==GC_SR_WAIT){d->cursor=d->record_next;return d->cursor==NULL;}
        kgd_check("logical record seal",s,GC_SR_DONE);
    }
    if(run->native.stage==11){
        if(run->native.mark_records.allocation){
            kgd_native(d,true);s=gc_native_records_dispose_native(&run->native,&r->os.metadata,&d->actor);
            if(s==GC_NATIVE_RUN_OS_ERROR){kg_os_retry(&d->record_os_failures,"logical record OS retry limit");kgd_native(d,false);return false;}
            kgd_native(d,false);kgd_check("logical record OS cleanup",s,GC_POOL_OK);d->record_os_failures=0;
        }
        kg_lock(r);s=gc_native_records_finish_locked(&run->native);kg_unlock(r);kgd_check("logical record retirement",s,GC_POOL_OK);
        d->cursor=d->record_next;
        if(!kgd_control_retire(d,&run->control))return false;
    }else d->cursor=(void *)(uintptr_t)run->native.next;
    return d->cursor==NULL;
}
/* Old tracing must retain the children of every physically surviving Young
 * object, including snapshot-floating Young garbage. Such a Young receiver can
 * outlive this Major's selection even though it is absent from the Old root
 * snapshot. Merely ignoring its fields by an Old mark mismatch would not prove
 * incoming absence and could leave an unresolvable retained-dead child.
 *
 * Enumerate AFTER the concurrent Young move has fully retired its sources and
 * BEFORE foreground Old MarkEnd. No Young mover runs in this interval, and
 * private cancellation excludes this busy-owned inventory reader. Each actual
 * candidate is handed to the real Old queue through guard/record resolution;
 * these temporary bad-color keys never grant mark credit by themselves. Only
 * Young pages form this conservative boundary. Once the receiver promotes, an
 * unreachable Old cycle is reclaimable by a later actual Major. */
static bool kgd_old_young(KgDriver *d){
    KgRuntime *r=d->runtime;kg_lock(r);
    if(!d->old_young_started){d->old_young_cursor=(void *)(uintptr_t)r->native.allocations;d->old_young_index=0;d->old_young_started=1;}
    KgRun *run=d->old_young_cursor;
    if(!run){kg_unlock(r);return true;}
    uint64_t stage=atomic_load_explicit(&run->native.stage,memory_order_acquire);
    if((stage!=3&&stage!=4&&stage!=8)||gc_native_allocation_page(&run->native)->generation!=GC_YOUNG||!run->native.forwarding.allocation){
        d->old_young_cursor=(void *)(uintptr_t)run->native.next;d->old_young_index=0;kg_unlock(r);return false;
    }
    ForwardTable *table=(void *)(uintptr_t)run->native.forwarding.allocation;
    if(d->old_young_index>=atomic_load_explicit(&table->used,memory_order_acquire)){
        d->old_young_cursor=(void *)(uintptr_t)run->native.next;d->old_young_index=0;kg_unlock(r);return false;
    }
    ForwardCell *cell=(ForwardCell *)(uintptr_t)table->cells+d->old_young_index++;
    if(atomic_load_explicit(&cell->word,memory_order_acquire)){kg_unlock(r);return false;}
    uint64_t phase=r->native.roots.phase;
    PendingMark owner={.context=(uintptr_t)&d->actor,
        .current={.reference=(cell->source_offset<<16)|(gc_abi_color((PhaseSnapshot){phase})^GC_YOUNG_MARK_MASK^GC_OLD_MARK_MASK)},
        .generation=GC_OLD,.queue=(uintptr_t)&gc_native_runtime_work(&r->native)->queues[GC_OLD],
        .epoch=gc_native_runtime_work(&r->native)->queues[GC_OLD].epoch,.phase=phase};kg_unlock(r);
    kgd_offer(d,&owner);d->old_young_visits++;return false;
}
/* Admission is idle-only, so active pinned pages can only disappear while
 * this walk runs. Treat every object on such a page as a conservative root:
 * even an unreachable neighbouring record must have canonical surviving
 * children when the page stays physically present through this collection.
 * Page tables are immutable after the pin's actual CLOSED-page admission. */
static bool kgd_pin_roots(KgDriver *d){
    KgRuntime *r=d->runtime;kg_lock(r);
    if(!r->pin_inventory){kg_unlock(r);return true;}
    if(!d->pin_started){d->pin_cursor=(void *)(uintptr_t)r->native.allocations;d->pin_index=0;d->pin_started=1;}
    KgRun *run=d->pin_cursor;if(!run){kg_unlock(r);return true;}
    uint64_t stage=atomic_load_explicit(&run->native.stage,memory_order_acquire);
    if(stage!=8||!kg_pin_page_active_locked(r,run)||!run->native.forwarding.allocation){
        d->pin_cursor=(void *)(uintptr_t)run->native.next;d->pin_index=0;kg_unlock(r);return false;
    }
    ForwardTable *table=(void *)(uintptr_t)run->native.forwarding.allocation;
    if(d->pin_index>=atomic_load_explicit(&table->used,memory_order_acquire)){
        d->pin_cursor=(void *)(uintptr_t)run->native.next;d->pin_index=0;kg_unlock(r);return false;
    }
    ForwardCell *cell=(ForwardCell *)(uintptr_t)table->cells+d->pin_index++;
    if(atomic_load_explicit(&cell->word,memory_order_acquire)){kg_unlock(r);return false;}
    uint64_t phase=r->native.roots.phase,gen=d->generation;
    PendingMark owner={.context=(uintptr_t)&d->actor,
        .current={.reference=(cell->source_offset<<16)|(gc_abi_color((PhaseSnapshot){phase})^GC_YOUNG_MARK_MASK^GC_OLD_MARK_MASK)},
        .generation=gen,.queue=(uintptr_t)&gc_native_runtime_work(&r->native)->queues[gen],
        .epoch=gc_native_runtime_work(&r->native)->queues[gen].epoch,.phase=phase};kg_unlock(r);
    kgd_offer(d,&owner);d->pin_visits++;return false;
}
static void kgd_begin_locked(KgDriver *d,unsigned gen){
    CycleTicket ticket={.kind=gen};
    kgd_check("cycle request",gc_native_cycle_request_locked(&d->runtime->native,&ticket),GC_CY_OK);
    kgd_check("cycle start",gc_native_cycle_begin_locked(&d->runtime->native,gen,gc_cycle_mark_start_locked),GC_CY_OK);
    if(gen==GC_OLD){d->old_young_started=d->old_young_index=0;d->old_young_cursor=NULL;}
    d->requested[gen]=0;
}
/* A Major traces both generations. Every captured child is offered to both
 * active queues; neither a Young root nor a Young->Old edge is skipped. Old
 * marking remains live while the Young lane relocates and retires its sources. */
static bool kgd_background(KgDriver *d){
    KgRuntime *r=d->runtime;NativeRuntimeOwner *n=&r->native;uint64_t s;
    kg_lock(r);
    if(kg_weak_phase_frozen_locked(r)){kg_unlock(r);return false;}
    if(!d->background_old&&d->generation==GC_YOUNG&&d->requested[GC_OLD]&&!n->handshake.active){
        kgd_begin_locked(d,GC_OLD);d->old_serial=++d->next_serial;d->background_old=1;d->old_root_started=0;d->old_pin_done=0;kg_unlock(r);return true;
    }
    kg_unlock(r);
    if(d->background_old==1||d->background_old==2){
        kg_lock(r);s=gc_native_cycle_mark_locked(n,GC_OLD,d->background_old==1?GC_MP_ACTIVATE:GC_MP_RELEASE);kg_unlock(r);
        if(s==GC_MP_WAIT)return true;
        if(d->background_old==1){kgd_check("Old mark activation",s,GC_MP_ADVANCED);d->background_old=2;}
        else{kgd_check("Old mark adoption",s,GC_MP_MARKING);d->background_old=3;}
        return true;
    }
    kg_lock(r);bool can_scan=!n->handshake.active;kg_unlock(r);
    if(d->background_old==3&&!d->copy_stage&&can_scan){
        uint64_t gen=d->generation,roots=d->root_started;d->generation=GC_OLD;d->root_started=d->old_root_started;
        (void)kgd_roots(d,false);d->old_root_started=d->root_started;d->generation=gen;d->root_started=roots;
        /* Root discovery is finite; after it finishes preserve the Old FIFO
         * through Young discovery/copy/rebinding, while producers may append. */
        if(d->old_root_started==2)d->old_frozen=1;
        if(!d->old_frozen)kgd_scan(d,GC_OLD);
    }
    return false;
}
static void kgd_step(KgDriver *d){
    KgRuntime *r=d->runtime;NativeRuntimeOwner *n=&r->native;uint64_t s;
#ifdef KG_DRIVER_DEBUG
    static _Thread_local uint64_t previous=UINT64_MAX,ticks;
    uint64_t signature=(n->roots.phase<<8)|d->state;
    if(signature!=previous||!(++ticks%10000)){
        fprintf(stderr,"driver state=%llu gen=%llu phase=%llx bg=%llu root=%llu audit=%llu cursor=%llu/%llu hs=%llu ack=%llu/%llu ctx=%u current=%p copy=%llu leases=%llu walkers=%llu\n",
            (unsigned long long)d->state,(unsigned long long)d->generation,(unsigned long long)n->roots.phase,
            (unsigned long long)d->background_old,(unsigned long long)d->root_started,
            (unsigned long long)n->cycle[d->generation].marking_audit.state,
            (unsigned long long)n->cycle[d->generation].marking_audit.walk.cursor,
            (unsigned long long)n->cycle[d->generation].marking_audit.walk.limit,
            (unsigned long long)n->handshake.active,(unsigned long long)n->handshake.acknowledged,
            (unsigned long long)n->handshake.expected,atomic_load(&n->context.state),(void *)d->cursor,
            (unsigned long long)d->copy_stage,(unsigned long long)n->roots.leases,(unsigned long long)n->roots.walkers);
        previous=signature;
    }
#endif
    /* A rendezvous performs metadata transitions only. Newly published work
     * causes the shared DECIDE predicate to release/retry; graph/journal/root
     * processing resumes outside the stopped interval. */
    kg_lock(r);bool rendezvous=n->handshake.active!=0;kg_unlock(r);
    if(!rendezvous)kgd_transport(d);
    if(!d->state){
        if(d->background_old==3){d->generation=GC_OLD;d->serial=d->old_serial;d->root_started=d->old_root_started;d->pin_started=d->old_pin_done;d->pin_cursor=NULL;d->old_frozen=0;d->old_scan_quanta=0;
            d->young_old_stage=0;d->rebind_source=NULL;d->rebind_stage=0;
            d->background_old=0;d->state=3;return;}
        kg_lock(r);
        if(d->cleanup_waiters){kg_unlock(r);return;}
        if(n->backing.heap.pressure&&!d->stop){d->requested[0]=1;if(d->young_cycles>=2)d->requested[1]=1;}
        if(!d->requested[0]&&!d->requested[1]){kg_unlock(r);return;}
        /* A pending Major starts with a Young tracing lane as well. */
        d->generation=GC_YOUNG;d->serial=++d->next_serial;kgd_begin_locked(d,GC_YOUNG);
        d->state=1;d->root_started=d->heap_started=d->pin_started=0;d->target[0]=d->target[1]=NULL;kg_unlock(r);
        kg_remembered_begin(r,n->remembered.epoch);return;
    }
    /* An active Old mark is a suspended snapshot, not a prerequisite drain
     * for a later Young request. No raw cursor from the Old conservative
     * inventory may survive Young physical retirement. */
    kg_lock(r);
    if(d->state==3&&d->generation==GC_OLD&&d->requested[GC_YOUNG]&&
       d->root_started==2&&d->old_pin_done&&d->old_young_started&&!d->old_young_cursor&&
       d->old_scan_quanta>=32&&!n->handshake.active&&
       gc_mark_step_locked(&gc_native_runtime_work(n)->queues[GC_OLD],NULL,GC_MARK_CHECK)==GC_MARK_BUSY){
        d->background_old=3;d->old_serial=d->serial;d->old_root_started=d->root_started;
        d->old_frozen=1; /* Both inventory passes are complete: no native cursor survives. */
        d->generation=GC_YOUNG;d->serial=++d->next_serial;kgd_begin_locked(d,GC_YOUNG);
        d->state=1;d->root_started=d->heap_started=d->pin_started=0;
        d->young_old_stage=0;d->rebind_source=NULL;d->rebind_stage=0;
        d->target[0]=d->target[1]=NULL;kg_unlock(r);
        kg_remembered_begin(r,n->remembered.epoch);return;
    }
    kg_unlock(r);
    if(kgd_background(d))return;
    unsigned gen=(unsigned)d->generation;
    if(d->state==17){
        /* The completed handshake is released. Verification belongs to this
         * busy-owned phase actor and runs outside any stopped interval. */
        s=(r->flags&4)?kg_verify_runtime(r,&d->actor):KG_VERIFY_OK;
        if(s==KG_VERIFY_WAIT){
            kg_lock(r);bool full=!kg_leases(r)->free_head;kg_unlock(r);
            if(full)kgd_lease_pressure(d);else{kgd_scan(d,GC_YOUNG);kgd_scan(d,GC_OLD);}return;
        }
        kgd_check("cycle verification",s,KG_VERIFY_OK);
        if(gen==GC_YOUNG)d->young_cycles++;
        if(r->flags&2)fprintf(stderr,"klassic GC: %s cycle=%llu completed\n",gen?"Old":"Young",(unsigned long long)n->cycle[gen].ticket.cycle);
        d->state=0;d->snapshot=d->cursor=NULL;d->root_started=d->heap_started=0;d->target[0]=d->target[1]=NULL;return;
    }
    if(d->state==1||d->state==2){
        kg_lock(r);s=gc_native_cycle_mark_locked(n,gen,d->state==1?GC_MP_ACTIVATE:GC_MP_RELEASE);kg_unlock(r);
        if(s==GC_MP_WAIT)return;
        if(d->state==1){kgd_check("mark activation",s,GC_MP_ADVANCED);d->state=2;}
        else{kgd_check("mark adoption",s,GC_MP_MARKING);d->state=3;}
        return;
    }
    if(d->state==3){
        bool roots=kgd_roots(d,false),pins=kgd_pin_roots(d);
        bool young_boundary=gen!=GC_OLD?kgd_young_old(d):kgd_old_young(d);
        if(gen==GC_OLD){
            if(pins)d->old_pin_done=1;
            kg_lock(r);bool queued=gc_mark_step_locked(&gc_native_runtime_work(n)->queues[GC_OLD],NULL,GC_MARK_CHECK)==GC_MARK_BUSY;
            bool slice=!(d->requested[GC_YOUNG]&&d->old_scan_quanta>=32);kg_unlock(r);
            /* Thirty-two actual receiver slices, never delay on an empty
             * queue. Finish finite inventories without draining the remaining
             * graph, then let the requested Young own its independent lane. */
            if(queued&&slice){kgd_scan(d,gen);if(d->old_scan_quanta<32)d->old_scan_quanta++;}
        }else kgd_scan(d,gen);
        if(gen==GC_YOUNG)kg_remembered_scan(r,kgd_remembered_visit,d);
        kg_lock(r);NativeWorkStorage *work=gc_native_runtime_work(n);
        bool empty=gc_mark_step_locked(&work->queues[gen],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY;
        if(d->background_old==3)empty=empty&&d->old_root_started==2&&d->young_old_stage==3;
        if(roots&&pins&&young_boundary&&empty&&!n->handshake.active){s=kg_weak_mark_locked(r,gen,GC_MP_BEGIN);if(s!=GC_MP_WAIT){kgd_check("mark end rendezvous",s,GC_MP_ADVANCED);d->state=4;}}
        kg_unlock(r);return;
    }
    if(d->state==4||d->state==5){
        kg_lock(r);
        if(d->state==4)d->selection_cutoff=n->roots.phase>>16;
        s=kg_weak_mark_locked(r,gen,d->state==4?GC_MP_DECIDE:GC_MP_RELEASE);
        if(s==GC_MP_WAIT){kg_unlock(r);return;}
        if(d->state==4){kgd_check("mark end decision",s,GC_MP_ADVANCED);d->state=5;}
        else if(s==GC_MP_RETRY){d->state=3;d->root_started=d->pin_started=0;d->young_old_stage=0;if(gen==GC_OLD){d->old_young_started=0;d->old_pin_done=0;}}
        else{kgd_check("mark termination",s,GC_MP_SELECT);
            d->snapshot=d->cursor=(void *)(uintptr_t)n->allocations;d->state=6;}
        kg_unlock(r);return;
    }
    if(d->state==6){
        s=kg_weak_scan_step(r,&d->actor);
        if(s==GC_WP_FULL){kgd_lease_pressure(d);return;}
        if(s!=GC_WS_DONE){if(s==GC_WP_EXHAUSTED)kg_fail("weak processor lease exhausted",s);return;}
        if(d->cursor){kg_lock(r);KgRun *run=d->cursor;d->cursor=(void *)(uintptr_t)run->native.next;
            /* Native preparation can span MarkEnd and later resume in Select.
             * Only runs actually closed by the captured deciding snapshot may
             * enter this selection; a new post-mark allocation is not dead. */
            if(run->native.stage==8&&run->closed_phase&&run->closed_phase<=d->selection_cutoff&&
               gc_native_allocation_page(&run->native)->generation==gen){
                uint64_t pin=kg_pin_select_locked(r,run);
                if(pin==KG_PIN_FULL){d->cursor=run;kg_unlock(r);kgd_lease_pressure(d);return;}
                if(pin==KG_PIN_SKIP){kg_unlock(r);return;}
                if(pin!=KG_PIN_SELECTED)kg_fail("source pin selection",pin);
                run->selected_serial=d->serial;run->selected_generation=gen;
                run->selected_epoch=gc_native_runtime_work(n)->queues[gen].epoch;
                run->selected_cycle=n->cycle[gen].ticket.cycle;
            }
            kg_unlock(r);return;
        }
        kg_lock(r);s=kg_weak_move_begin_locked(r,gen);kg_unlock(r);
        if(s==GC_RP_WAIT)return;kgd_check("relocation start",s,GC_RP_ADVANCED);d->state=7;return;
    }
    if(d->state==7||d->state==8){
        kg_lock(r);s=gc_native_cycle_move_locked(n,gen,d->state==7?GC_RP_ACTIVATE:GC_RP_RELEASE);kg_unlock(r);
        if(s==GC_RP_WAIT)return;
        if(d->state==7){kgd_check("relocation activation",s,GC_RP_ADVANCED);d->state=8;}
        else{kgd_check("relocation adoption",s,GC_RP_RELOCATING);d->cursor=d->snapshot;d->object_index=0;d->state=9;}
        return;
    }
    if(d->state==9){
        if(!kgd_copy(d))return;
        kg_lock(r);s=gc_relocation_gate_locked(&n->cycle[gen].gate,NULL,GC_RG_CLOSE);
        if(s==GC_RG_OK){s=gc_native_cycle_retire_locked(n,gen,GC_RR_BEGIN);
            if(s==GC_RP_ADVANCED)kg_weak_retire_begin_locked(r,gen);}
        kg_unlock(r);
        if(s==GC_RP_WAIT)return;kgd_check("relocation source handoff",s,GC_RP_ADVANCED);d->state=10;return;
    }
    if(d->state==10){
        kg_lock(r);s=gc_native_cycle_retire_locked(n,gen,GC_RR_RELEASE);kg_unlock(r);
        if(s==GC_RP_WAIT)return;kgd_check("retirement adoption",s,GC_RP_RETIRING);
        d->root_started=d->heap_started=0;d->state=11;return;
    }
    if(d->state==11){
        bool roots=kgd_roots(d,true),heap=kgd_heap_heal(d);
        if(roots&&heap){d->cursor=d->snapshot;d->state=12;}return;
    }
    if(d->state==12){if(kgd_retire_run(d)){d->cursor=NULL;d->state=13;}return;}
    if(d->state==13){if(kgd_records(d)){d->root_started=0;d->state=14;}return;}
    if(d->state==14){
        if(!kgd_roots(d,true))return;
        kg_lock(r);s=gc_native_cycle_complete_locked(n,gen,GC_NC_BEGIN);kg_unlock(r);
        if(s==GC_NC_WAIT)return;kgd_check("cycle final rendezvous",s,GC_NC_ADVANCED);d->state=15;return;
    }
    kg_lock(r);s=gc_native_cycle_complete_locked(n,gen,d->state==15?GC_NC_DECIDE:GC_NC_RELEASE);kg_unlock(r);
    if(s==GC_NC_WAIT)return;
    if(s==GC_NC_REAUDIT){d->root_started=0;d->state=14;return;}
    if(d->state==15){kgd_check("cycle final decision",s,GC_NC_ADVANCED);d->state=16;return;}
    kgd_check("cycle full completion",s,GC_NC_COMPLETE);
    kg_lock(r);kg_weak_cycle_finish_locked(r,gen);kg_unlock(r);
    d->state=17;
}
/* A registered mutator, the shutdown owner, or the OS worker holds driver
 * lifetime here. Shutdown closes admission, waits for every peer/attach/detach,
 * then drains busy/cleanup owners and joins the worker before unpublishing and
 * disposing d. The common mutex closes the worker stale-stop decision window. */
static bool kgd_acquire_for(KgDriver *d,bool graph){
    KgRuntime *r=d->runtime;kg_lock(r);
    /* Only this busy owner can advance phase. Reference transport must not
     * begin graph work in an already active rendezvous; metadata assistance
     * remains available to finish/release that rendezvous first. */
    if(d->busy||d->stop==2||(graph&&r->native.handshake.active)){kg_unlock(r);return false;}
    d->busy=1;kgd_adopt(d);kg_unlock(r);kg_owned_driver=d;return true;
}
static bool kgd_acquire(KgDriver *d){return kgd_acquire_for(d,false);}
static void kgd_leave(KgDriver *d){
    KgRuntime *r=d->runtime;
    if(atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire))kg_fail("collector guard at handoff",0);
    kg_owned_driver=NULL;kg_lock(r);atomic_store_explicit(&d->actor.state,GC_BLOCKED_NATIVE,memory_order_release);
    d->busy=0;kg_notify(r);kg_unlock(r);
}
void kg_driver_step(KgRuntime *r){
    KgDriver *d=r->driver;if(!d||kg_owned_driver==d)return;
    /* NativeBufferIncoming consumes a real per-owner quiescent handoff. The
     * actual mutator OS thread closes its own TLAB and publishes Native for
     * one finite audit quantum; a coordinator never changes its state. */
    MutatorContext *ctx=kg_current_context(r);
    kg_lock(r);bool native=!d->busy&&ctx&&d->state==12&&!kg_current_initializing(r)&&
        atomic_load_explicit(&ctx->state,memory_order_acquire)==GC_RUNNING&&
        !atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)&&
        !atomic_load_explicit(&ctx->private_producers,memory_order_acquire);kg_unlock(r);
    if(native){
        kg_enter_native(r);
        while(!kgd_acquire(d)){
            kg_lock(r);bool terminal=r->driver!=d||d->stop==2;kg_unlock(r);
            if(terminal){kg_leave_native(r);return;}kg_yield();
        }
        kgd_step(d);kgd_leave(d);kg_leave_native(r);return;
    }
    if(kgd_acquire(d)){kgd_step(d);kgd_leave(d);}
}
uint64_t kg_driver_cleanup_enter(KgRuntime *r,KgRun *run){
    KgDriver *d=r->driver;if(!d)return 2;
    if(kg_owned_driver==d){
        /* A fresh preparation failure is never a saved selected-source cursor.
         * Reject cancellation of any still-borrowed inventory control. */
        if((d->state>=6&&d->state<=12&&(d->cursor==run||d->snapshot==run))||
           (d->state==3&&d->generation==GC_OLD&&d->old_young_started&&d->old_young_cursor==run)||
           (d->state==3&&d->pin_started&&d->pin_cursor==run)||
           (d->state==11&&d->heap_started&&d->heap_cursor==run)||
           (d->state==12&&d->closure_stage&&d->closure_cursor==run))kg_fail("private cleanup still has driver readers",0);
        return 1;
    }
    kg_lock(r);if(d->stop==2){kg_unlock(r);kg_fail("cleanup after collector terminal stop",0);}
    d->cleanup_waiters++;kg_unlock(r);
    for(;;){
        kg_lock(r);
        if(d->stop==2){d->cleanup_waiters--;kg_unlock(r);kg_fail("cleanup admission after collector stop",0);}
        if(!d->busy&&!d->state&&!d->background_old){
            d->cleanup_waiters--;d->busy=1;kgd_adopt(d);
            atomic_store_explicit(&d->actor.state,GC_BLOCKED_NATIVE,memory_order_release);
            kg_unlock(r);kg_owned_driver=d;return 0;
        }
        kg_unlock(r);kg_driver_step(r);kg_yield();
    }
}
void kg_driver_cleanup_leave(KgRuntime *r,uint64_t borrowed){
    if(!borrowed)kgd_leave((KgDriver *)r->driver);
}
const MutatorContext *kg_driver_cleanup_actor(KgRuntime *r){
    KgDriver *d=r->driver;
    if(!d||kg_owned_driver!=d||!d->busy||
       atomic_load_explicit(&d->actor.access_epoch,memory_order_acquire)||
       atomic_load_explicit(&d->actor.state,memory_order_acquire)!=GC_BLOCKED_NATIVE)
        kg_fail("cleanup without actual collector Native ownership",0);
    return &d->actor;
}
void kg_driver_verify(KgRuntime *r){
    KgDriver *d=r->driver;
    if(!d||kg_owned_driver)kg_fail("recursive or unavailable verifier",0);
    /* Keep the actual caller Native, without raw values/guards. At an idle
     * ownership boundary the collector actor can verify a stable phase and
     * inventory while the OS worker is excluded by its real busy ownership. */
    kg_enter_native(r);
    uint64_t ownership=kg_driver_cleanup_enter(r,NULL);
    for(;;){
        kgd_native(d,false);uint64_t status=kg_verify_runtime(r,&d->actor);
        if(status==KG_VERIFY_WAIT){
            /* All verifier leases/guards have closed. Advance real shared
             * work, then finish any started cycle before retrying the audit. */
            kg_lock(r);bool full=!kg_leases(r)->free_head;kg_unlock(r);
            if(full)kgd_lease_pressure(d);else kgd_step(d);
            kgd_native(d,true);kg_driver_cleanup_leave(r,ownership);
            ownership=kg_driver_cleanup_enter(r,NULL);continue;
        }
        kgd_native(d,true);kgd_check("requested heap verification",status,KG_VERIFY_OK);
        kg_driver_cleanup_leave(r,ownership);kg_leave_native(r);return;
    }
}
void kg_driver_reference(KgRuntime *r,uint64_t reference,uint64_t phase){
    if(!reference)return;
    KgDriver *d=r->driver;if(!d)kg_fail("reference before collector initialization",0);
    if(kg_owned_driver==d){kgd_reference(d,reference,phase);return;}
    bool native=false;
    if(!kgd_acquire_for(d,true)){
        /* The journal/deletion owner retains this exact reference until its
         * actual ACK. A contended OS mutator must not spin in Running while
         * the sole phase owner needs that mutator's handshake acknowledgement.
         * Finish private producers before using this optional Native handoff. */
        MutatorContext *ctx=kg_current_context(r);
        kg_lock(r);bool can_native=ctx&&!kg_current_initializing(r)&&
            atomic_load_explicit(&ctx->state,memory_order_acquire)==GC_RUNNING&&
            !atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)&&
            !atomic_load_explicit(&ctx->private_producers,memory_order_acquire);kg_unlock(r);
        if(can_native){kg_enter_native(r);native=true;}
        while(!kgd_acquire_for(d,true)){
            kg_lock(r);bool terminal=r->driver!=d||d->stop==2;kg_unlock(r);
            if(terminal)kg_fail("reference admission after collector stop",0);
            kg_driver_step(r);kg_yield();
        }
    }
    kgd_reference(d,reference,phase);kgd_leave(d);
    if(native)kg_leave_native(r);
}
#ifdef _WIN32
static DWORD WINAPI kgd_worker(LPVOID input){
#else
static void *kgd_worker(void *input){
#endif
    KgDriver *d=input;KgRuntime *r=d->runtime;
    for(;;){
        kg_lock(r);
        if(!d->busy&&!d->stop&&!d->state&&!d->background_old&&!d->requested[0]&&!d->requested[1]&&!r->native.backing.heap.pressure){
#ifdef _WIN32
            if(!SleepConditionVariableSRW(&r->changed,&r->lock,10,0)&&GetLastError()!=ERROR_TIMEOUT)
                kg_fail("collector idle condition",GetLastError());
#else
            struct timespec deadline;if(timespec_get(&deadline,TIME_UTC)!=TIME_UTC)kg_fail("collector clock",0);
            deadline.tv_nsec+=10000000;if(deadline.tv_nsec>=1000000000){deadline.tv_sec++;deadline.tv_nsec-=1000000000;}
            int error=pthread_cond_timedwait(&r->changed,&r->lock,&deadline);
            if(error&&error!=ETIMEDOUT)kg_fail("collector idle condition",(uint64_t)error);
#endif
        }
        bool stop=d->stop==2;kg_unlock(r);if(stop)break;
#ifdef KG_DRIVER_AFTER_STOP_CHECK_HOOK
        KG_DRIVER_AFTER_STOP_CHECK_HOOK(d);
#endif
        if(kgd_acquire(d)){
            /* OS maintenance never runs inside a stopped handshake. The
             * acquired busy owner also excludes terminal-stop admission. */
            kg_lock(r);bool idle=!d->state&&!d->background_old&&!r->native.handshake.active;kg_unlock(r);
            if(idle&&r->uncommit_delay_ns){kgd_native(d,true);kg_policy_poll(r,&d->actor,kg_policy_clock());kgd_native(d,false);}
            kgd_step(d);kgd_leave(d);
        }
#ifdef KG_DRIVER_AFTER_WORKER_STEP_HOOK
        KG_DRIVER_AFTER_WORKER_STEP_HOOK(d);
#endif
        kg_yield();
    }
#ifdef _WIN32
    return 0;
#else
    return NULL;
#endif
}
void kg_driver_start(KgRuntime *r){
    if(r->driver)kg_fail("duplicate collector driver",0);
    MetadataCharge charge={0};
    kgd_check("collector control size",gc_native_metadata_size(sizeof(KgDriver),r->os.metadata.quantum,&charge.bytes),GC_NATIVE_METADATA_OK);
    kg_enter_native(r);kg_lock(r);
    uint64_t status=gc_budget_step_locked(&r->native.backing.heap,&charge,GC_BUDGET_RESERVE);kg_unlock(r);
    kgd_check("collector control admission",status,GC_BUDGET_OK);
    kg_metadata_prepare_retry(r,&charge);
    kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,&charge,GC_BUDGET_COMMIT);kg_unlock(r);
    kgd_check("collector control commitment",status,GC_BUDGET_OK);
    KgDriver *d=(void *)(uintptr_t)charge.allocation;d->charge=charge;charge=(MetadataCharge){0};d->runtime=r;
    kg_lock(r);if(r->native.handshake.last_identity==UINT64_MAX)kg_fail("collector identity exhaustion",0);
    d->actor.abi_version=GC_ABI_VERSION;d->actor.thread_id=++r->native.handshake.last_identity;
    atomic_init(&d->actor.state,GC_BLOCKED_NATIVE);atomic_init(&d->actor.adopted_phase,r->native.roots.phase);
    atomic_init(&d->actor.access_epoch,0);r->driver=d;kg_unlock(r);
#ifdef _WIN32
    d->thread=CreateThread(NULL,0,kgd_worker,d,0,NULL);if(!d->thread)kg_fail("collector OS thread",GetLastError());
#else
    int error=pthread_create(&d->thread,NULL,kgd_worker,d);if(error)kg_fail("collector OS thread",(uint64_t)error);
#endif
    kg_leave_native(r);
}
static void kgd_wait(KgRuntime *r,uint64_t young,uint64_t old){
    for(;;){
        kg_lock(r);bool done=r->native.cycles.completed[0]>=young&&r->native.cycles.completed[1]>=old;kg_unlock(r);
        if(done)return;
        kg_poll();kg_yield();
    }
}
void kg_driver_pressure(KgRuntime *r,uint64_t reason){
    KgDriver *d=r->driver;if(!d)kg_fail("pressure before collector initialization",reason);
    if(kg_owned_driver==d)kg_fail("recursive to-space exhaustion",reason);
    kg_lock(r);uint64_t young=r->native.cycles.completed[0]+1,old=0;
    d->requested[0]=1;
    if(reason!=KG_PRESSURE_STRESS||r->native.cycles.completed[0]%4==3){d->requested[1]=1;old=r->native.cycles.completed[1]+1;}
    kg_unlock(r);kgd_wait(r,young,old);
}
void kg_driver_stop(KgRuntime *r){
    KgDriver *d=r->driver;if(!d)return;
    MutatorContext *ctx=kg_current_context(r);
    kg_lock(r);
    bool sole_owner=r->admission_closed&&!r->attaching&&!r->detaching&&r->mutator_count==1&&
        !kg_root_publications_locked(r)&&ctx==&r->native.context&&
        r->native.handshake.members==1&&r->native.participant.registered;
    kg_unlock(r);
    if(!sole_owner)kg_fail("collector shutdown still has foreign mutators",0);
    /* No application roots or TLABs remain. A real joint tracing cycle removes
     * unreachable cross-generation cycles; its source and logical record
     * retirement must finish before worker join or metadata disposal. */
    kg_lock(r);d->stop=1;uint64_t young=r->native.cycles.completed[0]+1,old=r->native.cycles.completed[1]+1;
    d->requested[0]=d->requested[1]=1;kg_unlock(r);kgd_wait(r,young,old);
    /* Concurrent allocation/promotion may leave a finite floating survivor
     * outside a previous selection. Run actual rootless Major/Young cycles to
     * their fixed point; never bypass source or record retirement. */
    for(;;){
        kg_lock(r);bool empty=r->native.allocations==0;
        young=r->native.cycles.completed[0]+1;old=r->native.cycles.completed[1]+1;
        if(!empty)d->requested[0]=d->requested[1]=1;kg_unlock(r);
        if(empty)break;kgd_wait(r,young,old);
    }
#ifdef KG_DRIVER_BEFORE_TERMINAL_HOOK
    KG_DRIVER_BEFORE_TERMINAL_HOOK(d);
#endif
    for(;;){kg_lock(r);bool quiet=!d->busy&&!d->state&&!d->background_old&&!d->cleanup_waiters;
        if(quiet){if(r->native.handshake.active)kg_fail("collector idle with active handshake",0);d->stop=2;}
        kg_unlock(r);if(quiet)break;kg_poll();kg_yield();}
#ifdef KG_DRIVER_TERMINAL_HOOK
    KG_DRIVER_TERMINAL_HOOK(d);
#endif
    kg_enter_native(r);
#ifdef _WIN32
    if(WaitForSingleObject(d->thread,INFINITE)!=WAIT_OBJECT_0||!CloseHandle(d->thread))kg_fail("collector OS join",GetLastError());
#else
    int error=pthread_join(d->thread,NULL);if(error)kg_fail("collector OS join",(uint64_t)error);
#endif
    if(r->native.allocations)kg_fail("collector retained runs at shutdown",r->native.allocations);
    /* Admission is closed under the same lock as both acquisition paths.
     * Already busy actors and cleanup waiters drained before stop2; the OS
     * worker is joined before publishing NULL or retiring its control. */
    KgMetadataRetire retirement={0};kg_lock(r);r->driver=NULL;kg_unlock(r);uint64_t status;
    do{status=kg_metadata_retire_step_native(r,retirement.charge.stage?NULL:&d->charge,&retirement,ctx);}while(status==GC_NATIVE_METADATA_OS_ERROR);
    kgd_check("collector control retirement",status,GC_NATIVE_METADATA_OK);kg_leave_native(r);
}
