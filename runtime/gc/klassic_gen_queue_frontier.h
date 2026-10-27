/* Driver-private bounded Old frontier ownership across a Young cycle.
 * The sole busy collector freezes Old TAKE/REFILL after its real root audit.
 * Existing normal slots and native MarkRecord spill cells then keep their
 * actual metadata tokens and intrusive links until Young completes. Producers
 * may append: current-phase roots/barriers offer Young independently, and
 * pre-adoption producers remain actual handshake/termination obligations.
 * No pointer into a moving managed object or guard survives a quantum.
 */
#ifndef KLASSIC_GEN_QUEUE_FRONTIER_H
#define KLASSIC_GEN_QUEUE_FRONTIER_H
static MarkQueue *kgd_old_queue_locked(KgDriver *d){
    MarkQueue *q=&gc_native_runtime_work(&d->runtime->native)->queues[GC_OLD];
    uint64_t status=gc_mark_step_locked(q,NULL,GC_MARK_CHECK);
    if((status!=GC_MARK_BUSY&&status!=GC_MARK_EMPTY)||q->generation!=GC_OLD||q->in_flight||
       q->epoch!=d->runtime->native.cycle[GC_OLD].marking.epoch)
        kg_fail("Old frozen queue identity",status);
    return q;
}
static uint64_t kgd_old_prefix_next(MarkTask *task,uint64_t index,uint64_t tail,uint64_t *remaining){
    if(task->state!=1||!*remaining||(*remaining==1)!=(index==tail)||(!task->next&&index!=tail))
        kg_fail("Old frozen queue prefix",index);
    (*remaining)--;return index==tail?0:task->next;
}
static MarkTask *kgd_old_task(MarkQueue *q,uint64_t stage,uint64_t index,uint64_t node){
    if(stage==1&&(!index||index>q->capacity))kg_fail("Old frontier slot identity",index);
    return stage==1?&((MarkTask *)(uintptr_t)q->tasks)[index-1]:gc_mark_spill_cell(q,node);
}
static bool kgd_young_old(KgDriver *d){
    if(d->background_old!=3)return true;
    if(!d->old_frozen)return false;
    KgRuntime *r=d->runtime;kg_lock(r);MarkQueue *q=kgd_old_queue_locked(d);
    if(!d->young_old_stage){
        if(q->in_flight)kg_fail("Old frontier frozen with claimed task",q->in_flight);
        d->young_old_stage=1;d->young_old_index=q->head;d->young_old_normal_tail=q->tail;d->young_old_normal_remaining=q->queued;
        d->young_old_node=q->overflow_head;d->young_old_tail=q->overflow_tail;
    }
    if(d->young_old_stage==1&&!d->young_old_index){if(d->young_old_normal_remaining)kg_fail("Old discovery prefix incomplete",0);d->young_old_stage=2;}
    if(d->young_old_stage==2&&!d->young_old_node)d->young_old_stage=3;
    if(d->young_old_stage==3){kg_unlock(r);return true;}
    MarkTask *task=kgd_old_task(q,d->young_old_stage,d->young_old_index,d->young_old_node);
    uint64_t reference=task->state==1?task->reference:0;
    if(task->state>1)kg_fail("Old frontier claimed during freeze",task->state);
    if(d->young_old_stage==1)d->young_old_index=kgd_old_prefix_next(task,d->young_old_index,d->young_old_normal_tail,&d->young_old_normal_remaining);
    else d->young_old_node=d->young_old_node==d->young_old_tail?0:task->next;
    uint64_t phase=r->native.roots.phase;
    PendingMark p={.context=(uintptr_t)&d->actor,.current={.reference=reference},
        .generation=GC_YOUNG,.queue=(uintptr_t)&gc_native_runtime_work(&r->native)->queues[GC_YOUNG],
        .epoch=gc_native_runtime_work(&r->native)->queues[GC_YOUNG].epoch,.phase=phase};
    kg_unlock(r);if(reference)kgd_offer(d,&p);return false;
}
/* One actual queued task per quantum. All cells in this source have already
 * completed forwarding or dead-only RETAIN. A queued Old receiver was offered
 * to Young before MarkEnd, so it must have a surviving canonical destination.
 * The old task token remains owned until a fresh guarded follow and record
 * identity check supplies the replacement. Queue slot/serial are re-resolved
 * after OS pressure; the new lease is either transferred once or released.
 *
 * Late pre-close producers can enqueue after the captured tail. They are a
 * finite admitted prefix: closed source guards reject new preparation. Seal
 * still requires actual source leases0 (apart from authenticated pin registry).
 * A changed real queue serial after a WAIT restarts this bounded pass; no
 * artificial credit or early source uncommit is used.
 */
static bool kgd_old_rebind(KgDriver *d,KgRun *source){
    if(!d->old_frozen)return true;
    KgRuntime *r=d->runtime;kg_lock(r);MarkQueue *q=kgd_old_queue_locked(d);
    if(d->rebind_source!=source){d->rebind_source=source;d->rebind_stage=0;}
    if(!d->rebind_stage){
        if(q->in_flight)kg_fail("Old rebind with claimed task",q->in_flight);
        d->rebind_stage=1;d->rebind_index=q->head;d->rebind_normal_tail=q->tail;d->rebind_normal_remaining=q->queued;
        d->rebind_node=q->overflow_head;d->rebind_tail=q->overflow_tail;
        d->rebind_revision=q->last_serial;
    }
    if(d->rebind_stage==1&&!d->rebind_index){if(d->rebind_normal_remaining)kg_fail("Old rebind prefix incomplete",0);d->rebind_stage=2;}
    if(d->rebind_stage==2&&!d->rebind_node)d->rebind_stage=3;
    if(d->rebind_stage==3){kg_unlock(r);return true;}
    uint64_t stage=d->rebind_stage,index=d->rebind_index,node=d->rebind_node;
    MarkTask *task=kgd_old_task(q,stage,index,node);MarkTask captured=*task;
    if(stage==1)d->rebind_index=kgd_old_prefix_next(&captured,index,d->rebind_normal_tail,&d->rebind_normal_remaining);
    else d->rebind_node=node==d->rebind_tail?0:captured.next;
    if(captured.state>1)kg_fail("Old rebind claimed during freeze",captured.state);
    MetadataLeaseRequest old={.token=captured.metadata_lease};
    if(!captured.state)kg_fail("Old linked frontier free task",index);
    kgd_check("Old queued token resolve",gc_metadata_step_locked(kg_leases(r),&old,GC_ML_RESOLVE),GC_ML_OK);
    if(old.incarnation!=captured.incarnation)kg_fail("Old queued incarnation",captured.incarnation);
    if(old.page!=(uintptr_t)gc_native_allocation_page(&source->native)){kg_unlock(r);return false;}
    kg_unlock(r);
    PendingMark p={.context=(uintptr_t)&d->actor,.current={.reference=captured.reference}};
    kgd_check("Old queued canonical receiver",kgd_enter(d,&p),GC_RAC_READY);
    uint64_t payload,descriptor,record;kgd_shape(d,&p,&payload,&descriptor,&record);
    kgd_exit(&p);
    kg_lock(r);q=&gc_native_runtime_work(&r->native)->queues[GC_OLD];task=kgd_old_task(q,stage,index,node);
    if(record!=captured.record||task->state!=1||task->serial!=captured.serial||
       task->record!=captured.record||task->reference!=captured.reference||
       task->metadata_lease!=captured.metadata_lease||task->incarnation!=captured.incarnation||
       p.current.lease.page==old.page){
        kg_unlock(r);kgd_release(d,&p);kg_fail("Old queued canonical ownership",captured.serial);
    }
    if(q->last_serial==UINT64_MAX){kg_unlock(r);kgd_release(d,&p);kg_fail("Old queued serial exhausted",0);}
    task->reference=p.current.reference;task->incarnation=p.current.lease.incarnation;
    task->metadata_lease=p.current.lease.token;task->serial=++q->last_serial;
    d->rebind_revision++; /* Exclude our own mutation from append detection. */
    p.current.lease=(MetadataLeaseRequest){0};
    kgd_check("Old queued source token release",gc_metadata_step_locked(kg_leases(r),&old,GC_ML_RELEASE),GC_ML_OK);
    kg_unlock(r);return false;
}
#endif
