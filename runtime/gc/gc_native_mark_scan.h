/* Resumable, bounded runtime mark claim. The stable native control and its
 * child buffer are provisioned/accounted by the worker owner before admission.
 * BEGIN links the controller into runtime source-retirement discovery; terminal
 * FINISH unlinks it under the same registry lock before storage can be reused.
 * Worker registration, lifetime, locks, notifications and polling are external.
 * Incoming inspection requires exclusive worker ownership of this control;
 * registry locking alone does not serialize fields mutated outside that lock.
 * BEGIN/COMMIT: registry+generation lock; ENTER/RELEASE: heap lock;
 * SCAN/EXIT: outside locks under the guard. Child operations use GC_RM locks.
 * Each call is finite. Never poll or acquire another lock with a live guard.
 *
 * The original claim retains its physical metadata until YIELD/FINISH. A
 * separate canonical receiver lease follows movement one edge per ENTER and
 * protects each scan, including when original payload is already uncommitted.
 * A scan view uses the same logical record/cursor but the canonical receiver.
 * The queue claim is advanced only after every captured child is handed off.
 */
#ifndef KLASSIC_GC_NATIVE_MARK_SCAN_H
#define KLASSIC_GC_NATIVE_MARK_SCAN_H
#include "gc_native_cycle.h"
#include "gc_pending_mark.h"
#include "gc_mark_scan_commit.h"
typedef struct {
    uint64_t self,generation,epoch,stage,budget,status,child_index;
    uint64_t previous,next,identity;
    NativeRuntimeOwner *runtime;
    MutatorContext *worker;
    MarkRequest job,view;
    PendingMark receiver,child;
    MarkScan scan;
    uint64_t children[GC_DESCRIPTOR_SCAN_MAX_SLOTS];
} NativeMarkScan;
/* 1 claim, 2 guarded, 3 scan attempted/guarded, 4 receiver cleanup,
 * 5 child handoff, 6 completed claim cleanup, 7 yielded/finished, 8 unlinked. */
static inline bool gc_native_mark_scan_valid(const NativeMarkScan *q){
    return q&&!((uintptr_t)q&7)&&q->self==(uintptr_t)q&&q->stage>=1&&q->stage<=7&&
        gc_native_cycle_valid(q->runtime,q->generation);
}
static inline uint64_t gc_native_mark_scan_begin_locked(NativeRuntimeOwner *r,unsigned gen,
        MutatorContext *worker,uint64_t budget,NativeMarkScan *q){
    if(!q||((uintptr_t)q&7)||q->self||!gc_native_cycle_valid(r,gen)||!worker||((uintptr_t)worker&7)||
       !budget||budget>GC_DESCRIPTOR_SCAN_MAX_SLOTS||worker->abi_version!=GC_ABI_VERSION||!worker->thread_id||
       atomic_load_explicit(&worker->state,memory_order_acquire)!=GC_RUNNING||
       atomic_load_explicit(&worker->access_epoch,memory_order_acquire)||
       atomic_load_explicit(&worker->adopted_phase,memory_order_acquire)!=r->roots.phase)return GC_MARK_INVALID;
    uint64_t phase=(r->roots.phase>>(gen*4))&15;
    if(phase<GC_MARK_START||phase>GC_MARK_END_CHECK)return GC_MARK_INVALID;
    if(!!r->mark_scans!=!!r->mark_scan_count||r->mark_scan_count>r->mark_scan_serial)return GC_MARK_INVALID;
    /* Reserve one future unlink for every live control, plus this attach/unlink. */
    if(r->mark_scan_count>UINT64_MAX-2||r->mark_scan_serial>UINT64_MAX-r->mark_scan_count-2)return GC_MARK_EXHAUSTED;
    NativeMarkScan *head=(void *)(uintptr_t)r->mark_scans;
    if(head&&(!gc_native_mark_scan_valid(head)||head->runtime!=r||head->previous||
        !head->identity||head->identity>r->mark_scan_serial))return GC_MARK_INVALID;
    MarkQueue *queue=&gc_native_runtime_work(r)->queues[gen];
    uint64_t status=gc_mark_spill_locked(queue,NULL,GC_MARK_REFILL);
    if(status!=GC_MARK_OK&&status!=GC_MARK_EMPTY&&status!=GC_MARK_FULL)return status;
    MarkRequest job={0};status=gc_mark_step_locked(queue,&job,GC_MARK_TAKE);
    if(status!=GC_MARK_OK)return status;
    *q=(NativeMarkScan){.self=(uintptr_t)q,.generation=gen,.epoch=queue->epoch,.stage=1,
        .budget=budget,.runtime=r,.worker=worker,.job=job,
        .next=r->mark_scans,.identity=++r->mark_scan_serial,
        .receiver={.context=(uintptr_t)worker,.current={.reference=job.reference}}};
    if(head)head->previous=(uintptr_t)q;
    r->mark_scans=(uintptr_t)q;r->mark_scan_count++;
    return GC_MARK_OK;
}
/* Registry lock. Terminal controls stay discoverable until this operation;
 * registry audits detect unlink/reuse by serial before borrowing a cursor. */
static inline uint64_t gc_native_mark_scan_finish_locked(NativeMarkScan *q){
    if(!gc_native_mark_scan_valid(q)||q->stage!=7)return GC_MARK_INVALID;
    NativeRuntimeOwner *r=q->runtime;
    if(!r->mark_scan_count||r->mark_scan_serial==UINT64_MAX||!q->identity||q->identity>r->mark_scan_serial||
       ((q->previous|q->next)&7))return GC_MARK_INVALID;
    NativeMarkScan *prev=(void *)(uintptr_t)q->previous,*next=(void *)(uintptr_t)q->next;
    if(prev?(!gc_native_mark_scan_valid(prev)||prev->runtime!=r||prev->next!=(uintptr_t)q||prev->identity<=q->identity):
        r->mark_scans!=(uintptr_t)q)return GC_MARK_INVALID;
    if(next&&(!gc_native_mark_scan_valid(next)||next->runtime!=r||next->previous!=(uintptr_t)q||
        !next->identity||next->identity>=q->identity))return GC_MARK_INVALID;
    if(prev)prev->next=q->next;else r->mark_scans=q->next;
    if(next)next->previous=q->previous;
    r->mark_scan_count--;r->mark_scan_serial++;q->previous=q->next=0;q->stage=8;
    return GC_MARK_OK;
}
static inline uint64_t gc_native_mark_scan_enter_locked(NativeMarkScan *q){
    if(!gc_native_mark_scan_valid(q)||q->stage!=1)return GC_RF_INVALID;
    uint64_t status=gc_pending_mark_enter(&gc_native_runtime_work(q->runtime)->leases,&q->receiver);
    if(status==GC_RAC_READY)q->stage=2;
    return status;
}
static inline uint64_t gc_native_mark_scan_read(NativeMarkScan *q,uint64_t (*scan)(MarkScan *)){
    if(!gc_native_mark_scan_valid(q)||q->stage!=2||!scan)return GC_SCAN_INVALID;
    NativeBuiltinRegistry *registry=(void *)(uintptr_t)q->runtime->descriptor_charge.allocation;
    q->view=q->job;q->view.reference=q->receiver.current.reference;
    q->view.incarnation=q->receiver.current.lease.incarnation;
    q->view.metadata_lease=q->receiver.current.lease.token;
    q->scan=(MarkScan){.job=(uintptr_t)&q->view,.heap_base=q->runtime->backing.heap.heap_base,
        .registry=(uintptr_t)&registry->registry,.output=(uintptr_t)q->children,.capacity=q->budget};
    q->status=scan(&q->scan);q->stage=3;return q->status;
}
static inline uint64_t gc_native_mark_scan_exit(NativeMarkScan *q){
    if(!gc_native_mark_scan_valid(q)||(q->stage!=2&&q->stage!=3))return GC_RF_INVALID;
    uint64_t status=gc_pending_mark_cleanup(NULL,&q->receiver,GC_RAC_EXIT);
    if(status==GC_RF_DONE){
        if(q->stage==2)q->status=GC_SCAN_INVALID; /* abandon before reading */
        q->stage=4;
    }
    return status;
}
static inline uint64_t gc_native_mark_scan_release_locked(NativeMarkScan *q){
    if(!gc_native_mark_scan_valid(q)||(q->stage!=4&&q->stage!=6))return GC_RF_INVALID;
    MetadataLeasePool *pool=&gc_native_runtime_work(q->runtime)->leases;
    if(q->stage==6){
        MetadataLeaseRequest lease={.token=q->job.metadata_lease};
        if(gc_metadata_step_locked(pool,&lease,GC_ML_RESOLVE)!=GC_ML_OK||
           gc_metadata_step_locked(pool,&lease,GC_ML_RELEASE)!=GC_ML_OK)return GC_RF_CLEANUP_REQUIRED;
        q->job=(MarkRequest){0};q->stage=7;return GC_RF_DONE;
    }
    uint64_t status=gc_pending_mark_cleanup(pool,&q->receiver,GC_RAC_RELEASE);
    if(status!=GC_RF_DONE)return status;
    q->view=(MarkRequest){0};
    if(q->status>GC_SCAN_MORE){
        q->receiver=(PendingMark){.context=(uintptr_t)q->worker,.current={.reference=q->job.reference}};
        q->scan=(MarkScan){0};q->stage=1;
    }else{
        q->receiver=(PendingMark){0};q->scan.job=(uintptr_t)&q->job;q->stage=5;
    }
    return GC_RF_DONE;
}
/* Child cancellation/reset never advances child_index or the parent cursor.
 * A changed other-generation phase can be adopted and retried with RESET. */
static inline uint64_t gc_native_mark_scan_child(NativeMarkScan *q,uint64_t op,PendingMarker mark){
    if(!gc_native_mark_scan_valid(q)||q->stage!=5||q->child_index>=q->scan.written||!mark)return GC_RF_INVALID;
    NativeWorkStorage *work=gc_native_runtime_work(q->runtime);PendingMark *child=&q->child;
    if(!child->context){
        if(op!=GC_RM_ENTER)return GC_RF_INVALID;
        uint64_t phase=q->runtime->roots.phase,lane=(phase>>(q->generation*4))&15;
        if(work->queues[q->generation].epoch!=q->epoch||lane<GC_MARK_START||lane>GC_MARK_END_CHECK||
           atomic_load_explicit(&q->worker->adopted_phase,memory_order_acquire)!=phase)return GC_RM_STALE;
        *child=(PendingMark){.context=(uintptr_t)q->worker,.current={.reference=q->children[q->child_index]},
            .queue=(uintptr_t)&work->queues[q->generation],.generation=q->generation,.epoch=q->epoch,.phase=phase};
    }
    if(op==GC_RM_RESET){
        uint64_t phase=q->runtime->roots.phase,lane=(phase>>(q->generation*4))&15;
        if(work->queues[q->generation].epoch!=q->epoch||lane<GC_MARK_START||lane>GC_MARK_END_CHECK||
           atomic_load_explicit(&q->worker->adopted_phase,memory_order_acquire)!=phase)return GC_RM_STALE;
        uint64_t status=mark(&work->leases,child,op);
        if(status==GC_RF_DONE)child->phase=phase;
        return status;
    }
    return mark(&work->leases,child,op);
}
static inline uint64_t gc_native_mark_scan_child_ack(NativeMarkScan *q){
    if(!gc_native_mark_scan_valid(q)||q->stage!=5||q->child_index>=q->scan.written)return GC_MARK_INVALID;
    PendingMark *p=&q->child;
    if(p->stage!=5||(p->route_result!=GC_ROUTE_SKIP&&!(p->route_result==GC_ROUTE_READY&&
        (p->offer_result==GC_MARK_OK||p->offer_result==GC_MARK_DUPLICATE)))||
       p->current.lease.token||p->retired.token||p->access.scope.held)return GC_MARK_BUSY;
    q->children[q->child_index++]=0;q->child=(PendingMark){0};return GC_MARK_OK;
}
static inline uint64_t gc_native_mark_scan_commit_locked(NativeMarkScan *q,MarkScanCommitter commit){
    if(!gc_native_mark_scan_valid(q)||q->stage!=5||!commit)return GC_MARK_INVALID;
    if(q->child_index!=q->scan.written||q->child.context)return GC_MARK_BUSY;
    MarkQueue *queue=&gc_native_runtime_work(q->runtime)->queues[q->generation];
    if(queue->epoch!=q->epoch)return GC_MARK_INVALID;
    uint64_t status=commit(queue,&q->scan,q->status);
    if(status==GC_MARK_OK)q->stage=q->status==GC_SCAN_DONE?6:7;
    return status;
}
#endif
