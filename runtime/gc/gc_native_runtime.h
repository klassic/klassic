/* Bootstrap owner for one heap and its initial mutator/root/descriptor packages.
 * Controls/config/ops are stable and disjoint. Prepare is private, before any
 * worker or caller can observe the runtime, so registry/heap serialization is
 * exclusive ownership rather than mutex callbacks. Activation is likewise
 * private and performs no OS calls. After activation ordinary users require
 * the shared registry/heap locks and boundary protocol.
 * Disposal requires external quiescence, no page allocations/jobs, and completed
 * root retirement. It is not a collector or a way to bypass retirement. Failed
 * OS cleanup retains every surviving resource and budget for retry.
 * Pin/remset inventories, worker launch and default compiler wiring remain
 * separate obligations before full concurrent operation.
 */
#ifndef KLASSIC_GC_NATIVE_RUNTIME_H
#define KLASSIC_GC_NATIVE_RUNTIME_H
#include "gc_native_heap.h"
#include "gc_native_startup.h"
#include "gc_worker.h"
#include "gc_detach.h"
#include "gc_native_work.h"
#include "gc_native_cycle_state.h"
#define GC_NATIVE_RUNTIME_LIMIT 3u
/* 0 empty, 1 preparation attempted, 2 private/native, 3 activated,
 * 4 disposing (no longer usable), 5 disposed. Self-binding forbids copies. */
typedef struct {
    uint64_t self,stage;
    NativeHeapOwner backing;
    MetadataCharge root_charge,descriptor_charge,work_charge;
    NativeStartupRequest startup;
    MutatorContext context;
    RootRegistry roots;
    HandshakeState handshake;
    ThreadRegistration participant;
    WorkerRequest worker;
    uint64_t allocations; /* Intrusive inventory of stable NativeAllocationRun controls. */
    uint64_t mark_scans,mark_scan_count,mark_scan_serial; /* Live NativeMarkScan controls. */
    CycleLedger cycles;
    RememberedInventory remembered;
    NativeCycleState cycle[2];
} NativeRuntimeOwner;
typedef struct {
    NativeHeapConfig heap;
    uint64_t root_cells,root_changes,descriptor_capacity;
    NativeWorkConfig work;
} NativeRuntimeConfig;
static inline bool gc_native_runtime_valid(const NativeRuntimeOwner *owner){
    return owner&&!((uintptr_t)owner&7)&&owner->self==(uintptr_t)owner;
}
static inline NativeWorkStorage *gc_native_runtime_work(NativeRuntimeOwner *owner){
    return gc_native_runtime_valid(owner)&&(owner->stage==2||owner->stage==3)&&owner->work_charge.stage==2?
        (void *)(uintptr_t)owner->work_charge.allocation:NULL;
}
static inline uint64_t gc_native_runtime_prepare(NativeRuntimeOwner *owner,
        const NativeRuntimeConfig *config,const NativeHeapOps *ops){
    if(!owner||((uintptr_t)owner&7)||!config||!gc_native_heap_ops_valid(ops))return GC_NATIVE_METADATA_INVALID;
    const unsigned char *raw=(const void *)owner;
    for(size_t i=0;i<sizeof(*owner);i++)if(raw[i])return GC_NATIVE_METADATA_INVALID;
    uint64_t bytes=0,work_bytes=0;
    if(gc_native_roots_size(config->root_cells,config->root_changes,ops->metadata.quantum,&bytes)||
       gc_native_builtin_registry_size(config->descriptor_capacity,ops->metadata.quantum,&bytes)||
       gc_native_work_size(&config->work,ops->metadata.quantum,&work_bytes))return GC_NATIVE_METADATA_INVALID;
    owner->self=(uintptr_t)owner;owner->stage=1;
    uint64_t status=gc_native_heap_prepare(&owner->backing,&config->heap,ops);
    if(status)return status;
    owner->context.abi_version=GC_ABI_VERSION;owner->roots.phase=UINT64_C(1)<<16;
    owner->participant.context=(uintptr_t)&owner->context;
    owner->worker=(WorkerRequest){(uintptr_t)&owner->handshake,(uintptr_t)&owner->participant};
    if(gc_worker_step_locked(&owner->roots,&owner->worker,GC_WK_ATTACH)!=GC_WK_OK)return GC_NATIVE_METADATA_INVALID;
    if(gc_worker_step_locked(&owner->roots,&owner->worker,GC_WK_ENTER_NATIVE)!=GC_WK_OK)return GC_NATIVE_METADATA_INVALID;
    owner->startup=(NativeStartupRequest){(uintptr_t)&owner->root_charge,(uintptr_t)&owner->descriptor_charge,
        config->root_cells,config->root_changes,config->descriptor_capacity,ops->metadata.quantum};
    status=gc_native_startup_request_locked(&owner->backing.heap,&owner->startup);
    if(status)return status==GC_BUDGET_LIMIT?GC_NATIVE_RUNTIME_LIMIT:GC_NATIVE_METADATA_INVALID;
    owner->work_charge.bytes=work_bytes;
    status=gc_budget_step_locked(&owner->backing.heap,&owner->work_charge,GC_BUDGET_RESERVE);
    if(status)return status==GC_BUDGET_LIMIT?GC_NATIVE_RUNTIME_LIMIT:GC_NATIVE_METADATA_INVALID;
    NativeStartupPreparationOps prepare={.native=&ops->metadata,.initialize_roots=gc_root_storage_prepare,
        .initialize_descriptors=gc_builtin_descriptors_prepare,.register_descriptor=gc_descriptor_register};
    status=gc_native_startup_prepare(&owner->startup,owner->context.thread_id,&prepare);
    if(status)return status;
    if(gc_native_startup_commit_locked(&owner->backing.heap,&owner->startup)!=GC_BUDGET_OK)return GC_NATIVE_METADATA_INVALID;
    status=gc_native_work_prepare(&owner->work_charge,&owner->backing.heap,&config->work,&ops->metadata);
    if(status)return status;
    if(gc_budget_step_locked(&owner->backing.heap,&owner->work_charge,GC_BUDGET_COMMIT)!=GC_BUDGET_OK||
       gc_native_work_bind(&owner->context,&owner->work_charge)!=GC_NATIVE_METADATA_OK)return GC_NATIVE_METADATA_INVALID;
    NativeWorkStorage *work=(void *)(uintptr_t)owner->work_charge.allocation;
    owner->remembered=(RememberedInventory){.epoch=owner->roots.phase,.last_phase=owner->roots.phase};
    for(unsigned i=0;i<2;i++){
        NativeCycleState *cycle=&owner->cycle[i];cycle->ticket.kind=i;
        cycle->marking_audit.queue=cycle->retirement_audit.queue=(uintptr_t)&work->queues[i];
        cycle->marking=(MarkPhaseControl){.handshake=(uintptr_t)&owner->handshake,
            .audit=(uintptr_t)&cycle->marking_audit,.generation=i,.remembered=(uintptr_t)&owner->remembered};
        cycle->gate=(RelocationGate){.epoch=1,.closed=1};
        cycle->moving=cycle->retiring=(RelocationPhaseControl){.handshake=(uintptr_t)&owner->handshake,
            .gate=(uintptr_t)&cycle->gate,.generation=i};
    }
    owner->stage=2;return GC_NATIVE_METADATA_OK;
}
/* Private activation only. Once this succeeds, caller owns root retirement
 * and all concurrent consumers before disposal can be attempted. */
static inline uint64_t gc_native_runtime_activate(NativeRuntimeOwner *owner){
    if(!gc_native_runtime_valid(owner)||owner->stage!=2)return GC_NATIVE_METADATA_INVALID;
    if(atomic_load_explicit(&owner->context.state,memory_order_acquire)==GC_BLOCKED_NATIVE&&
       gc_worker_step_locked(&owner->roots,&owner->worker,GC_WK_LEAVE_NATIVE)!=GC_WK_OK)return GC_NATIVE_METADATA_INVALID;
    if(gc_native_roots_publish_locked(&owner->roots,&owner->context,&owner->root_charge)!=GC_RL_OK)return GC_NATIVE_METADATA_INVALID;
    owner->stage=3;return GC_NATIVE_METADATA_OK;
}
static inline uint64_t gc_native_runtime_dispose(NativeRuntimeOwner *owner,const NativeHeapOps *ops){
    if(!gc_native_runtime_valid(owner)||owner->stage<1||owner->stage>4||!gc_native_heap_ops_valid(ops))return GC_NATIVE_METADATA_INVALID;
    if(owner->stage!=4){
        if(owner->mark_scans||owner->mark_scan_count)return GC_NATIVE_METADATA_INVALID;
        for(unsigned i=0;i<2;i++)if(owner->cycles.requested[i]!=owner->cycles.completed[i]||
            owner->cycles.started[i]!=owner->cycles.completed[i]||owner->cycle[i].gate.active||owner->cycle[i].gate.head)
            return GC_NATIVE_METADATA_INVALID;
        if(owner->remembered.head||owner->remembered.members||owner->remembered.unflipped||
           owner->remembered.pending||owner->remembered.claims||owner->remembered.walkers)return GC_NATIVE_METADATA_INVALID;
        if(owner->allocations||owner->context.root_segments||owner->context.retiring_roots||owner->context.tlab_start||
           owner->context.tlab_top||owner->context.tlab_end||owner->roots.active||owner->roots.retiring||
           owner->roots.leases||owner->roots.inventory||owner->roots.walkers||
           owner->backing.heap.committed||owner->backing.heap.pending||owner->handshake.active)
            return GC_NATIVE_METADATA_INVALID;
        if(owner->handshake.members!=(owner->participant.registered?1u:0u)||
           atomic_load_explicit(&owner->context.access_epoch,memory_order_acquire)||
           atomic_load_explicit(&owner->context.private_producers,memory_order_acquire)||
           owner->context.satb_consumer.serial||owner->context.satb_consumer.end||owner->context.satb_consumer.cursor||
           owner->context.remembered_consumer.serial||owner->context.remembered_consumer.end||owner->context.remembered_consumer.cursor)
            return GC_NATIVE_METADATA_INVALID;
        BufferCursor *buffers[]={&owner->context.satb,&owner->context.remembered};
        for(unsigned i=0;i<2;i++)
            if(buffers[i]->top!=atomic_load_explicit(&buffers[i]->published,memory_order_acquire)||
               buffers[i]->top!=atomic_load_explicit(&buffers[i]->consumed,memory_order_acquire))return GC_NATIVE_METADATA_INVALID;
        if(owner->work_charge.stage==2&&!gc_native_work_quiescent((void *)(uintptr_t)owner->work_charge.allocation))
            return GC_NATIVE_METADATA_INVALID;
        uint64_t committed=owner->backing.stage==2?owner->backing.table_bytes:0,pending=0;
        MetadataCharge *owned[]={&owner->root_charge,&owner->descriptor_charge,&owner->work_charge};
        for(unsigned i=0;i<3;i++){
            uint64_t *sum=owned[i]->stage==1?&pending:&committed;
            if(owned[i]->stage==1||owned[i]->stage==2){
                if(owned[i]->bytes>UINT64_MAX-*sum)return GC_NATIVE_METADATA_INVALID;
                *sum+=owned[i]->bytes;
            }
        }
        if(owner->backing.heap.metadata_committed!=committed||owner->backing.heap.metadata_pending!=pending)
            return GC_NATIVE_METADATA_INVALID;
        if(owner->participant.registered&&atomic_load_explicit(&owner->context.state,memory_order_acquire)==GC_RUNNING&&
           gc_worker_step_locked(&owner->roots,&owner->worker,GC_WK_ENTER_NATIVE)!=GC_WK_OK)return GC_NATIVE_METADATA_INVALID;
        if(owner->participant.registered&&atomic_load_explicit(&owner->context.state,memory_order_acquire)!=GC_BLOCKED_NATIVE)
            return GC_NATIVE_METADATA_INVALID;
        owner->stage=4;
    }
    MetadataCharge *charges[]={&owner->root_charge,&owner->descriptor_charge,&owner->work_charge};
    for(unsigned i=0;i<3;i++)if(charges[i]->allocation){
        uint64_t status=i?gc_native_metadata_dispose(charges[i],&ops->metadata):
            gc_native_roots_dispose(charges[i],&ops->metadata,gc_root_storage_can_dispose);
        if(status)return status;
        if(i==2)owner->context.satb.entries=owner->context.satb.capacity=
            owner->context.remembered.entries=owner->context.remembered.capacity=0;
    }
    for(unsigned i=0;i<3;i++)if(charges[i]->stage==1||charges[i]->stage==2){
        uint64_t op=charges[i]->stage==1?GC_BUDGET_ABORT:GC_BUDGET_RELEASE;
        if(gc_budget_step_locked(&owner->backing.heap,charges[i],op)!=GC_BUDGET_OK)return GC_NATIVE_METADATA_INVALID;
    }
    if(owner->participant.registered){
        if(atomic_load_explicit(&owner->context.state,memory_order_acquire)==GC_BLOCKED_NATIVE&&
           gc_worker_step_locked(&owner->roots,&owner->worker,GC_WK_LEAVE_NATIVE)!=GC_WK_OK)return GC_NATIVE_METADATA_INVALID;
        /* Owner-only transition after all roots/backing are gone. Registry is
         * still private/quiescent and no new participant can join. */
        atomic_store_explicit(&owner->context.state,GC_DETACHING,memory_order_release);
        DetachRequest detach={(uintptr_t)&owner->roots,(uintptr_t)&owner->handshake,(uintptr_t)&owner->participant};
        if(gc_detach_step_locked(&detach)!=GC_DT_DONE)return GC_NATIVE_METADATA_INVALID;
    }
    if(owner->backing.stage&&owner->backing.stage!=3){
        uint64_t status=gc_native_heap_dispose(&owner->backing,ops);
        if(status)return status;
    }
    owner->stage=5;return GC_NATIVE_METADATA_OK;
}
#endif
