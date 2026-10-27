/* Actual task capacity two with independently budgeted logical-record spills.
 * Override the private STARTUP configuration, never live queue/phase counters. */
#include <klassic_gen_runtime_internal.h>
static uint64_t queue_fixture_prepare(NativeRuntimeOwner *owner,const NativeRuntimeConfig *input,const NativeHeapOps *ops){
    NativeRuntimeConfig config=*input;config.work.tasks=2;
    return gc_native_runtime_prepare(owner,&config,ops);
}
#define gc_native_runtime_prepare queue_fixture_prepare
#include <klassic_gen_runtime.c>
#undef gc_native_runtime_prepare
#include <gc_native_driver.c>
#include <klassic_gen_remembered.c>
#include <klassic_gen_policy.c>
#include <klassic_gen_verify.c>
#include <assert.h>

static NativeAllocationRun *record_arena(KgRuntime *r,uint64_t record){
    for(NativeAllocationRun *n=(void *)(uintptr_t)r->native.allocations;n;n=(void *)(uintptr_t)n->next){
        if(n->mark_records.stage==2&&n->marks.records&&record>=n->marks.records&&
           record-n->marks.records<n->marks.used*sizeof(MarkRecord)&&
           !((record-n->marks.records)%sizeof(MarkRecord)))return n;
    }
    return NULL;
}
static NativeAllocationRun *physical_run(KgRuntime *r,const MetadataLeaseRequest *lease){
    AllocationPage *page=(void *)(uintptr_t)lease->page;
    for(NativeAllocationRun *n=(void *)(uintptr_t)r->native.allocations;n;n=(void *)(uintptr_t)n->next)
        if(n->lease.head_index==page->run_head&&n->lease.incarnation==lease->incarnation)return n;
    return NULL;
}
static uint64_t audit_spill(KgRuntime *r,unsigned generation){
    MarkQueue *queue=&gc_native_runtime_work(&r->native)->queues[generation];
    assert(queue->capacity==2);uint64_t count=0,record=queue->overflow_head,last=0;
    while(record){
        NativeAllocationRun *arena=record_arena(r,record);assert(arena);
        MarkRecord *logical=(void *)(uintptr_t)record;assert(logical->object_id);
        MarkTask *task=gc_mark_spill_cell(queue,record);
        assert(task->state==1&&task->record==record&&task->serial&&task->serial<=queue->last_serial&&
            task->reference&&task->incarnation&&task->metadata_lease&&!task->cursor);
        assert((generation?logical->old_epoch:logical->young_epoch)==queue->epoch);
        MetadataLeaseRequest lease={.token=task->metadata_lease};
        assert(gc_metadata_step_locked(kg_leases(r),&lease,GC_ML_RESOLVE)==GC_ML_OK&&lease.incarnation==task->incarnation);
        NativeAllocationRun *physical=physical_run(r,&lease);assert(physical);
        AllocationPage *page=(void *)(uintptr_t)lease.page;assert(page->metadata_leases);
        assert((task->reference>>16)>=page->start+sizeof(ObjectHeader)&&(task->reference>>16)<page->limit);
        ForwardTable *table=(void *)(uintptr_t)physical->forwarding.allocation;
        ForwardLookup lookup={task->reference>>16,task->incarnation,0};assert(gc_forward_lookup(table,&lookup)==GC_FL_OK);
        uint64_t cell=(lookup.cell-table->cells)/sizeof(ForwardCell);
        assert(cell<atomic_load_explicit(&table->used,memory_order_acquire));
        uint64_t imported=((uint64_t *)(uintptr_t)physical->imports)[cell];
        assert(arena==(imported?(void *)(uintptr_t)imported:physical));
        if(imported)assert(arena->import_users);
        last=record;record=task->next;assert(++count<=queue->overflow_count);
    }
    assert(count==queue->overflow_count&&last==queue->overflow_tail);return count;
}
static void quantum(KgRuntime *r,KgDriver *driver){
    kg_lock(r);kgd_adopt(driver);kg_unlock(r);kgd_step(driver);
    assert(!atomic_load_explicit(&driver->actor.access_epoch,memory_order_acquire));
}
#include "klassic_gen_queue_frontier_test.h"
#include "klassic_gen_queue_incoming_test.h"
int main(int argc,char **argv){
    if(argc>1&&(!strcmp(argv[1],"partial")||!strcmp(argv[1],"full")||!strcmp(argv[1],"partial-young"))){partial_and_late(!strcmp(argv[1],"full"),!strcmp(argv[1],"partial-young"));return 0;}
    bool spanning=argc>1&&!strcmp(argv[1],"spanning"),pin_mode=argc>1&&!strcmp(argv[1],"pin"),reuse_mode=argc>1&&!strcmp(argv[1],"reuse"),fair_mode=argc>1&&!strcmp(argv[1],"fair");
    enum { EACH=600,TOTAL=2*EACH };
    kg_configure(UINT64_C(32)*1024*1024,0,UINT64_C(16)*1024*1024,0);kg_init();
    KgRuntime *r=&kg_runtime;RootCell *roots[TOTAL],*alias=kg_root_new(),*reuse_root=kg_root_new(),*edge=kg_root_new(),*fresh_child=kg_root_new(),*promotion_edge=kg_root_new();
    kg_alloc_into(8,2,edge);
    KgPinnedBytes *pin=NULL;uint64_t *pinned=NULL;
    if(pin_mode){RootCell *root=kg_root_new();kg_alloc_into(384*1024,1,root);kg_store_i64(root,0,9922);
        pin=kg_pin_bytes(root);kg_root_release(root);kg_native_enter();pinned=kg_pinned_bytes_address(pin);
        assert(kg_pinned_bytes_size(pin)==384*1024&&*pinned==9922);kg_native_exit();}
    for(unsigned i=0;i<EACH;i++){
        roots[i]=kg_root_new();kg_alloc_into(8,1,roots[i]);kg_store_i64(roots[i],0,10000+i);
    }
    kg_root_copy(alias,roots[0]);
    for(unsigned i=0;i<2;i++)kg_driver_pressure(r,KG_PRESSURE_METADATA);
    kg_lock(r);ReferenceLease aged={.reference=atomic_load_explicit(&roots[0]->value,memory_order_acquire)};
    assert(gc_reference_lease_locked(kg_leases(r),&aged)==GC_ML_OK);
    assert(((AllocationPage *)(uintptr_t)aged.lease.page)->generation==GC_OLD);
    assert(gc_metadata_step_locked(kg_leases(r),&aged.lease,GC_ML_RELEASE)==GC_ML_OK);kg_unlock(r);
    for(unsigned i=EACH;i<TOTAL;i++){
        roots[i]=kg_root_new();kg_alloc_into(spanning&&i==EACH?384*1024:8,1,roots[i]);kg_store_i64(roots[i],0,10000+i);
        if(spanning&&i==EACH)kg_store_i64(roots[i],384*1024-8,992244);
    }
    kg_lock(r);ReferenceLease original={.reference=atomic_load_explicit(&roots[EACH]->value,memory_order_acquire)};
    assert(gc_reference_lease_locked(kg_leases(r),&original)==GC_ML_OK);
    AllocationPage *original_page=(void *)(uintptr_t)original.lease.page;uint64_t original_incarnation=original.lease.incarnation;
    assert(gc_metadata_step_locked(kg_leases(r),&original.lease,GC_ML_RELEASE)==GC_ML_OK);kg_unlock(r);
    kg_enter_native(r);uint64_t ownership=kg_driver_cleanup_enter(r,NULL);assert(!ownership);KgDriver *driver=r->driver;
    kg_lock(r);driver->requested[0]=driver->requested[1]=1;kg_unlock(r);uint64_t turns=0;
    while(driver->state!=3||driver->background_old!=3){assert(++turns<UINT64_C(3000000));quantum(r,driver);}
    /* Real per-generation root snapshots admit work without scanning it yet.
     * The logical record arenas were funded before actual object publication. */
    while(driver->root_started!=2){assert(++turns<UINT64_C(3000000));(void)kgd_roots(driver,false);}
    while(driver->old_root_started!=2){
        uint64_t generation=driver->generation,started=driver->root_started;
        driver->generation=GC_OLD;driver->root_started=driver->old_root_started;
        (void)kgd_roots(driver,false);driver->old_root_started=driver->root_started;
        driver->generation=generation;driver->root_started=started;assert(++turns<UINT64_C(3000000));
    }
    kg_lock(r);uint64_t young_spilled=audit_spill(r,GC_YOUNG),old_spilled=audit_spill(r,GC_OLD);
    assert(young_spilled>=EACH-2&&old_spilled>TOTAL-4&&old_spilled>1024);
    assert(kg_leases(r)->active>1024&&kg_leases(r)->capacity>1024);kg_unlock(r);
    incoming_differential_and_races(r,driver);
    uint64_t old_ticket=r->native.cycle[GC_OLD].ticket.cycle,young_before=r->native.cycles.completed[GC_YOUNG],actual_old_slices=0;
    bool source_reused=false;uint64_t reuse_cookie=818181;
    bool overlapping_copy=false,nested_started=false,nested_audited=false;uint64_t sole_record=0;
    uint64_t audits=0,copy_quanta=0,max_copy_spill=0,blocked_young=0;
    uint64_t first_copy_stage=0,first_copy_spill=0,blocked_discovery=0,blocked_roots=0,blocked_other=0;
    while(driver->state||driver->background_old||driver->requested[0]||driver->requested[1]){
        if(fair_mode){kg_lock(r);if(r->native.cycles.completed[GC_OLD]<old_ticket)driver->requested[GC_YOUNG]=1;kg_unlock(r);}
        uint64_t before_generation=driver->generation,before_count=driver->old_scan_quanta,before_state=driver->state;
        assert(++turns<UINT64_C(3000000));quantum(r,driver);
        if(before_generation==GC_OLD&&before_state==3&&driver->old_scan_quanta>before_count)actual_old_slices++;
        if(driver->generation==GC_YOUNG&&!driver->state&&driver->background_old==3){
            if(!nested_started){
                /* Finish the first Young while its Old snapshot remains live,
                 * then delete every application root to the 600 Young objects
                 * during actual foreground Old ConcurrentMark. */
                quantum(r,driver);assert(driver->generation==GC_OLD&&driver->state==3);
                /* Finish the actual conservative inventory without consuming
                 * its captured graph. It stays complete over later Young
                 * movement; the scheduler still owes real Old scan slices. */
                while(!kgd_old_young(driver)){assert(++turns<3000000);}
                assert(driver->old_young_started&&!driver->old_young_cursor);
                PendingMark probe={.context=(uintptr_t)&driver->actor,
                    .current={.reference=atomic_load_explicit(&roots[EACH]->value,memory_order_acquire)}};
                assert(kgd_enter(driver,&probe)==GC_RAC_READY);
                uint64_t bytes,descriptor;kgd_shape(driver,&probe,&bytes,&descriptor,&sole_record);
                kgd_exit(&probe);kgd_release(driver,&probe);
                kgd_native(driver,true);kg_leave_native(r);
                for(unsigned i=EACH;i<TOTAL;i++){assert(kg_load_i64(roots[i],0)==10000+i);kg_root_clear(roots[i]);}
                /* The inventory is already complete. This is the sole new
                 * Old->Young edge; its actual store barrier/remset, rather
                 * than a restarted physical census, must protect the child. */
                kg_alloc_into(8,1,fresh_child);kg_store_i64(fresh_child,0,777888);
                if(reuse_mode){
                    kg_lock(r);ReferenceLease fresh={.reference=atomic_load_explicit(&fresh_child->value,memory_order_acquire)};
                    assert(gc_reference_lease_locked(kg_leases(r),&fresh)==GC_ML_OK);
                    source_reused=fresh.lease.page==(uintptr_t)original_page;
                    if(source_reused)assert(fresh.lease.incarnation!=original_incarnation);
                    assert(gc_metadata_step_locked(kg_leases(r),&fresh.lease,GC_ML_RELEASE)==GC_ML_OK);kg_unlock(r);
                    if(source_reused){kg_root_copy(reuse_root,fresh_child);reuse_cookie=777888;}
                }
                kg_store_ref(edge,0,fresh_child);
                if(fair_mode){kg_alloc_into(8,2,promotion_edge);kg_store_ref(promotion_edge,0,fresh_child);}
                kg_root_clear(fresh_child);
                if(reuse_mode){
                    bool reused=source_reused;
                    for(unsigned i=0;i<64&&!reused;i++){
                        kg_alloc_into(GC_HEAP_PAGE_BYTES-sizeof(ObjectHeader),1,reuse_root);kg_store_i64(reuse_root,0,818181);
                        kg_lock(r);ReferenceLease fresh={.reference=atomic_load_explicit(&reuse_root->value,memory_order_acquire)};
                        assert(gc_reference_lease_locked(kg_leases(r),&fresh)==GC_ML_OK);
                        reused=fresh.lease.page==(uintptr_t)original_page;
                        if(reused)assert(fresh.lease.incarnation!=original_incarnation);
                        assert(gc_metadata_step_locked(kg_leases(r),&fresh.lease,GC_ML_RELEASE)==GC_ML_OK);kg_unlock(r);
                    }
                    assert(reused);fprintf(stderr,"original Young source page actually reused with new incarnation while original Old task/arena survives\n");
                }
                kg_enter_native(r);kg_lock(r);driver->requested[GC_YOUNG]=1;kg_unlock(r);nested_started=true;
            }else if(!nested_audited){
                /* The only strong owner is the unchanged-epoch Old task. Its
                 * record/cursor survives promotion and original-page retire. */
                kg_lock(r);MarkQueue *old=&gc_native_runtime_work(&r->native)->queues[GC_OLD];
                assert(audit_spill(r,GC_OLD)>1000);MarkTask saved={0};
                MarkTask *tasks=(void *)(uintptr_t)old->tasks;
                for(uint64_t i=0;i<old->capacity;i++)if(tasks[i].state==1&&tasks[i].record==sole_record)saved=tasks[i];
                for(uint64_t node=old->overflow_head;node;){MarkTask *t=gc_mark_spill_cell(old,node);if(node==sole_record)saved=*t;node=t->next;}
                assert(saved.record==sole_record&&saved.metadata_lease);kg_unlock(r);
                PendingMark probe={.context=(uintptr_t)&driver->actor,.current={.reference=saved.reference}};
                assert(kgd_enter(driver,&probe)==GC_RAC_READY);
                uint64_t bytes,descriptor,actual;kgd_shape(driver,&probe,&bytes,&descriptor,&actual);
                assert(actual==sole_record&&bytes==(spanning?384*1024:8)&&descriptor==1);
                if(spanning)assert(atomic_load_explicit((_Atomic uint64_t *)(uintptr_t)(r->native.backing.heap.heap_base+(probe.current.reference>>16)+bytes-8),memory_order_acquire)==992244);
                assert(atomic_load_explicit((_Atomic uint64_t *)(uintptr_t)(r->native.backing.heap.heap_base+(probe.current.reference>>16)),memory_order_acquire)==10000+EACH);
                kgd_exit(&probe);kgd_release(driver,&probe);nested_audited=true;
                if(fair_mode){
                    kgd_native(driver,true);kg_leave_native(r);
                    kg_alloc_into(8,1,fresh_child);kg_store_i64(fresh_child,0,909090);
                    kg_store_ref(promotion_edge,0,fresh_child);kg_root_clear(fresh_child);kg_enter_native(r);
                }
            }
        }
        kg_lock(r);
        uint64_t remaining=gc_native_runtime_work(&r->native)->queues[GC_OLD].overflow_count;
        if(driver->generation==GC_YOUNG&&driver->state==3&&
           gc_mark_step_locked(&gc_native_runtime_work(&r->native)->queues[0],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY&&
           gc_mark_step_locked(&gc_native_runtime_work(&r->native)->queues[1],NULL,GC_MARK_CHECK)==GC_MARK_BUSY){blocked_young++;
            if(driver->young_old_stage!=3)blocked_discovery++;
            else if(driver->root_started!=2)blocked_roots++;else blocked_other++;
        }
        if(driver->generation==GC_YOUNG&&driver->copy_stage){
            if(!copy_quanta){first_copy_stage=driver->state;first_copy_spill=remaining;}
            copy_quanta++;if(remaining>max_copy_spill)max_copy_spill=remaining;
            if(remaining)overlapping_copy=true;
        }
        if(remaining&&!(turns%37)){(void)audit_spill(r,GC_OLD);audits++;}
        kg_unlock(r);
    }
    fprintf(stderr,"observed young_spill=%llu old_spill=%llu audits=%llu copy_quanta=%llu first_copy_stage=%llu first_copy_old_spill=%llu max_copy_old_spill=%llu blocked_young_empty_old_busy=%llu overlap=%u discovery=%llu roots=%llu other=%llu\n",
        (unsigned long long)young_spilled,(unsigned long long)old_spilled,(unsigned long long)audits,
        (unsigned long long)copy_quanta,(unsigned long long)first_copy_stage,(unsigned long long)first_copy_spill,
        (unsigned long long)max_copy_spill,(unsigned long long)blocked_young,(unsigned)overlapping_copy,
        (unsigned long long)blocked_discovery,(unsigned long long)blocked_roots,(unsigned long long)blocked_other);
    assert(audits&&copy_quanta&&nested_started&&nested_audited);
    if(fair_mode){assert(r->native.cycles.completed[GC_OLD]>=old_ticket&&r->native.cycles.completed[GC_YOUNG]>young_before+2&&actual_old_slices>=32);
        fprintf(stderr,"continuous Young requests: completed Young %llu -> %llu, actual Old receiver slices %llu, completed Major %llu PASS\n",
            (unsigned long long)young_before,(unsigned long long)r->native.cycles.completed[GC_YOUNG],
            (unsigned long long)actual_old_slices,(unsigned long long)r->native.cycles.completed[GC_OLD]);}
    kg_lock(r);uint64_t retained=0;
    if(pin_mode){KgPinnedBytes *h=kgp_validate_locked(r,pin);MetadataLeaseRequest held=h->pin.root.follow.cleanup;
        assert(held.token&&gc_metadata_step_locked(kg_leases(r),&held,GC_ML_RESOLVE)==GC_ML_OK);
        PinPageRecord *record=gc_pin_record_locked(r->pin_registry,&held);assert(record&&record->lifetime.token&&record->admission.active==1);
        retained=2;}
    assert(!audit_spill(r,0)&&!audit_spill(r,1)&&kg_leases(r)->active==retained);
    assert(gc_mark_step_locked(&gc_native_runtime_work(&r->native)->queues[0],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);
    assert(gc_mark_step_locked(&gc_native_runtime_work(&r->native)->queues[1],NULL,GC_MARK_CHECK)==GC_MARK_EMPTY);kg_unlock(r);
    kgd_native(driver,true);kg_driver_cleanup_leave(r,ownership);kg_leave_native(r);
    for(unsigned i=0;i<EACH;i++)assert(kg_load_i64(roots[i],0)==10000+i);
    kg_store_i64(alias,0,222222);assert(kg_load_i64(roots[0],0)==222222);
    if(pin_mode){kg_native_enter();assert(kg_pinned_bytes_address(pin)==pinned&&*pinned==9922);kg_native_exit();kg_unpin_bytes(pin);
        kg_driver_pressure(r,KG_PRESSURE_METADATA);kg_lock(r);assert(!kg_leases(r)->active);kg_unlock(r);
        assert(kg_load_i64(roots[0],0)==222222);}
    kg_load_ref_into(edge,0,fresh_child);assert(kg_load_i64(fresh_child,0)==777888);
    if(fair_mode){kg_load_ref_into(promotion_edge,0,fresh_child);assert(kg_load_i64(fresh_child,0)==909090);}
    kg_root_release(promotion_edge);kg_root_release(fresh_child);kg_root_release(edge);
    if(reuse_mode)assert(kg_load_i64(reuse_root,0)==reuse_cookie);
    kg_root_release(reuse_root);kg_root_release(alias);for(unsigned i=0;i<TOTAL;i++)kg_root_release(roots[i]);kg_shutdown();
    assert(!r->native.allocations&&!r->native.backing.heap.metadata_committed&&!r->native.backing.heap.metadata_pending);
    fprintf(stderr,"actual1200values/sole Old snapshot across nested Young/promotion/alias mutation/source and record retirement/final metadata budgets0 verified\n");
    assert(overlapping_copy); /* Keep the desired independent-Young obligation failing. */
    printf("C task capacity2: Young spill%llu Old spill%llu, native arena leases, actual overlapping copy and final budgets0 PASS\n",
        (unsigned long long)young_spilled,(unsigned long long)old_spilled);return 0;
}
