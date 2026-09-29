/* Both runtime mark queues: queued, in-flight and spilled references.
 * BEGIN/NEXT/CANCEL hold registry + both generation + heap locks, outside raw
 * guards. NEXT checks one ordinary slot or one spill cell; a spill handoff may
 * additionally validate/pin its successor. No payload scan, allocation or wait.
 * The cycle excludes introduction of target references behind the cursor until
 * DONE is consumed. Epoch/serial/storage changes force restart; ordinary job
 * removal may proceed. REFILL clears a spill cell without rotating serial, so
 * a leased spill cursor additionally authenticates its captured task serial.
 * The metadata lease retains the logical record after dequeue/unmap of source
 * payload. CANCEL releases it even after invalidation, before resetting owner.
 * An audit is not a strong root and never dereferences managed payload.
 * DONE covers queue references only. In-flight scan children, roots, buffers,
 * native/weak handles and incoming forwarding remain separate proof domains.
 */
#ifndef KLASSIC_GC_NATIVE_QUEUE_INCOMING_H
#define KLASSIC_GC_NATIVE_QUEUE_INCOMING_H
#include "gc_native_scan_incoming.h"
typedef struct {
    uint64_t self,phase,epoch[2],serial[2],tasks[2],capacity[2],overflow[2];
    uint64_t generation,cursor,spill,spill_serial,stage,target_start,target_limit,observed;
    NativeRuntimeOwner *runtime;
    MetadataLeaseRequest pin;
} NativeQueueIncoming;
static inline bool gc_native_queue_incoming_valid(const NativeQueueIncoming *q){
    return q&&!((uintptr_t)q&7)&&q->self==(uintptr_t)q&&gc_native_cycle_valid(q->runtime,0)&&
        q->stage>=1&&q->stage<=4&&q->generation<=2&&gc_native_scan_incoming_range(q->target_start,q->target_limit);
}
static inline uint64_t gc_native_queue_incoming_begin_locked(NativeRuntimeOwner *r,uint64_t start,uint64_t limit,NativeQueueIncoming *q){
    if(!q||((uintptr_t)q&7)||q->self||!gc_native_cycle_valid(r,0)||!gc_native_scan_incoming_range(start,limit))return GC_NSI_INVALID;
    NativeWorkStorage *work=gc_native_runtime_work(r);
    for(unsigned i=0;i<2;i++)if(gc_mark_step_locked(&work->queues[i],NULL,GC_MARK_CHECK)==GC_MARK_INVALID)return GC_NSI_INVALID;
    *q=(NativeQueueIncoming){.self=(uintptr_t)q,.phase=r->roots.phase,.runtime=r,.stage=1,.target_start=start,.target_limit=limit};
    for(unsigned i=0;i<2;i++){
        MarkQueue *queue=&work->queues[i];q->epoch[i]=queue->epoch;q->serial[i]=queue->last_serial;
        q->tasks[i]=queue->tasks;q->capacity[i]=queue->capacity;
        q->overflow[i]=queue->overflow_count;
    }
    return GC_NSI_MORE;
}
static inline uint64_t gc_native_queue_incoming_cancel_locked(NativeQueueIncoming *q){
    if(!gc_native_queue_incoming_valid(q))return GC_NSI_INVALID;
    if(q->pin.token&&gc_metadata_step_locked(&gc_native_runtime_work(q->runtime)->leases,&q->pin,GC_ML_RELEASE)!=GC_ML_OK)
        return GC_NSI_INVALID;
    q->spill=q->spill_serial=q->observed=0;q->stage=4;return GC_NSI_DONE;
}
/* The queued task's own token protects its record while the common locks are
 * held. A distinct token then protects our cursor across subsequent calls. */
static inline uint64_t gc_native_queue_incoming_pin(NativeQueueIncoming *q,uint64_t record,MetadataLeaseRequest *pin,uint64_t *serial){
    if(!record||(record&7)||q->generation>1)return GC_NSI_INVALID;
    NativeWorkStorage *work=gc_native_runtime_work(q->runtime);MarkQueue *queue=&work->queues[q->generation];
    MarkTask *task=gc_mark_spill_cell(queue,record);
    if(task->state!=1||task->record!=record||!task->serial||task->serial>queue->last_serial||!task->metadata_lease)return GC_NSI_RESTART;
    MetadataLeaseRequest lease={.token=task->metadata_lease};
    if(gc_metadata_step_locked(&work->leases,&lease,GC_ML_RESOLVE)!=GC_ML_OK||lease.incarnation!=task->incarnation)return GC_NSI_INVALID;
    lease.token=0;uint64_t status=gc_metadata_step_locked(&work->leases,&lease,GC_ML_ACQUIRE);
    if(status!=GC_ML_OK)return status==GC_ML_FULL||status==GC_ML_EXHAUSTED?GC_NSI_WAIT:GC_NSI_INVALID;
    *pin=lease;*serial=task->serial;return GC_NSI_MORE;
}
static inline uint64_t gc_native_queue_incoming_next_locked(NativeQueueIncoming *q){
    if(!gc_native_queue_incoming_valid(q)||q->stage==4)return GC_NSI_INVALID;
    NativeWorkStorage *work=gc_native_runtime_work(q->runtime);
    if(q->phase!=q->runtime->roots.phase)return GC_NSI_RESTART;
    for(unsigned i=0;i<2;i++){
        MarkQueue *queue=&work->queues[i];
        if(queue->epoch!=q->epoch[i]||queue->last_serial!=q->serial[i]||
           queue->tasks!=q->tasks[i]||queue->capacity!=q->capacity[i]||queue->overflow_count!=q->overflow[i])return GC_NSI_RESTART;
        if(gc_mark_step_locked(queue,NULL,GC_MARK_CHECK)==GC_MARK_INVALID)return GC_NSI_INVALID;
    }
    if(q->stage==3){
        if(q->generation!=2||q->pin.token)return GC_NSI_INVALID;
        q->observed=0;return GC_NSI_DONE;
    }
    if(q->generation>1)return GC_NSI_INVALID;
    MarkQueue *queue=&work->queues[q->generation];MarkTask *task;
    if(q->stage==1){
        if(q->pin.token||q->cursor>queue->capacity)return GC_NSI_INVALID;
        if(q->cursor==queue->capacity){q->stage=2;return GC_NSI_MORE;}
        task=(MarkTask *)(uintptr_t)queue->tasks+q->cursor;
        if(task->state>2)return GC_NSI_INVALID;
        if(!task->state){q->cursor++;return GC_NSI_MORE;}
    }else{
        if(!q->spill){
            if(q->pin.token)return GC_NSI_INVALID;
            if(!queue->overflow_head){
                q->generation++;q->cursor=0;q->stage=q->generation==2?3:1;return GC_NSI_MORE;
            }
            uint64_t status=gc_native_queue_incoming_pin(q,queue->overflow_head,&q->pin,&q->spill_serial);
            if(status==GC_NSI_MORE)q->spill=queue->overflow_head;
            return status;
        }
        MetadataLeaseRequest checked=q->pin;
        if(gc_metadata_step_locked(&work->leases,&checked,GC_ML_RESOLVE)!=GC_ML_OK||
           checked.page!=q->pin.page||checked.incarnation!=q->pin.incarnation)return GC_NSI_INVALID;
        task=gc_mark_spill_cell(queue,q->spill);
        if(task->state!=1||task->record!=q->spill||task->serial!=q->spill_serial)return GC_NSI_RESTART;
    }
    if(!task->serial||task->serial>queue->last_serial||!task->record||(task->record&7)||
       !task->reference||!gc_store_ref_valid(task->reference)||!task->metadata_lease||!task->incarnation)return GC_NSI_INVALID;
    uint64_t offset=task->reference>>16;
    if(offset>=q->target_start&&offset<q->target_limit){q->observed=task->reference;return GC_NSI_FOUND;}
    q->observed=0;
    if(q->stage==1){q->cursor++;return GC_NSI_MORE;}
    uint64_t next=task->next,serial=0;MetadataLeaseRequest pin={0};
    if(next){
        if(next==q->spill)return GC_NSI_INVALID;
        uint64_t status=gc_native_queue_incoming_pin(q,next,&pin,&serial);
        if(status!=GC_NSI_MORE)return status;
    }
    if(gc_metadata_step_locked(&work->leases,&q->pin,GC_ML_RELEASE)!=GC_ML_OK){
        if(pin.token)(void)gc_metadata_step_locked(&work->leases,&pin,GC_ML_RELEASE);
        return GC_NSI_INVALID;
    }
    q->spill=next;q->spill_serial=serial;q->pin=pin;
    if(!next){q->generation++;q->cursor=0;q->stage=q->generation==2?3:1;}
    return GC_NSI_MORE;
}
#endif
