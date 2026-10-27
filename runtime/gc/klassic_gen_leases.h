/* Stable native lease slot backing. Implementation include, exactly once in
 * runtime.c. Reservation owns VM; chunk receipts separately own real committed
 * metadata bytes, including OS padding and per-chunk native control. The
 * reservation ceiling derives from metadata_max and the 32-bit token index;
 * actual controls/padding can make the budget limit occur before that ceiling.
 * No active slot is copied, moved or zeroed by later growth.
 *
 * FULL alone is retryable. LIMIT means the same-lock observation has no free
 * token and the current budget/index ceiling refuses a new prefix; it is not a
 * proof that a different owner can never release a token in the future. Native
 * helpers retain all old tokens and return LIMIT for an explicit retry. The
 * public adapter applies the runtime's hard resource refusal policy after a
 * fresh locked check of both returned tokens and actual in-flight growth.
 */
#ifndef KLASSIC_GEN_LEASES_H
#define KLASSIC_GEN_LEASES_H
#include "gc_metadata_grow.h"
typedef struct KgLeaseChunk {
    struct KgLeaseChunk *next;
    MetadataCharge control,backing;
    uint64_t offset;
} KgLeaseChunk;
typedef struct {
    MetadataCharge control;
    KgLeaseOps ops;
    uint64_t reservation,reservation_bytes,committed_bytes,maximum,growing,closing;
    KgLeaseChunk initial,*chunks;
} KgLeaseBacking;
bool kg_lease_growing_locked(KgRuntime *r){
    KgLeaseBacking *owner=r->lease_backing;return owner&&owner->growing;
}
static bool kgl_native(const MutatorContext *actor){
    return actor&&actor->abi_version==GC_ABI_VERSION&&actor->thread_id&&
        atomic_load_explicit(&actor->state,memory_order_acquire)==GC_BLOCKED_NATIVE&&
        !atomic_load_explicit(&actor->access_epoch,memory_order_acquire);
}
static bool kgl_commit(void *address,size_t bytes){
#ifdef _WIN32
    return VirtualAlloc(address,bytes,MEM_COMMIT,PAGE_READWRITE)==address;
#else
    return !mprotect(address,bytes,PROT_READ|PROT_WRITE);
#endif
}
static uint64_t kgl_control(KgRuntime *r,uint64_t size,MetadataCharge *charge){
    if(gc_native_metadata_size(size,r->os.metadata.quantum,&charge->bytes))return KG_LEASE_INVALID;
    kg_lock(r);uint64_t status=gc_budget_step_locked(&r->native.backing.heap,charge,GC_BUDGET_RESERVE);kg_unlock(r);
    if(status)return status==GC_BUDGET_LIMIT?KG_LEASE_LIMIT:KG_LEASE_INVALID;
    kg_metadata_prepare_retry(r,charge);
    kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,charge,GC_BUDGET_COMMIT);kg_unlock(r);
    return status?KG_LEASE_INVALID:KG_LEASE_OK;
}
static void kgl_retire_control(KgRuntime *r,MetadataCharge *source,const MutatorContext *actor){
    KgMetadataRetire job={0};uint64_t status;
    do{status=kg_metadata_retire_step_native(r,job.charge.stage?NULL:source,&job,actor);}while(status==GC_NATIVE_METADATA_OS_ERROR);
    if(status)kg_fail("lease backing control retirement",status);
}
void kg_lease_initialize(KgRuntime *r,const MutatorContext *actor,uint64_t initial_capacity,const KgLeaseOps *configured){
    if(!kgl_native(actor)||r->native.stage!=2||r->driver||r->lease_backing)kg_fail("initial lease backing ownership",0);
    MetadataLeasePool *pool=kg_leases(r);HeapPool *heap=&r->native.backing.heap;
    if(pool->active||!pool->capacity||pool->free_head!=1||!initial_capacity||initial_capacity>UINT32_MAX)kg_fail("initial lease tokens already issued",0);
    MetadataLeaseSlot *old=(void *)(uintptr_t)pool->slots;
    /* This switch is permitted ONLY before the first lease generation. The
     * old work package remains mapped/charged until its ordinary final unmap. */
    for(uint64_t i=0;i<pool->capacity;i++)
        if(old[i].page||old[i].incarnation||old[i].generation||old[i].next_free!=(i+1<pool->capacity?i+2:0))
            kg_fail("initial lease slots already used",i);
    MetadataCharge control={0};uint64_t status=kgl_control(r,sizeof(KgLeaseBacking),&control);
    if(status)kg_fail("initial lease control budget",status);
    KgLeaseBacking *owner=(void *)(uintptr_t)control.allocation;owner->control=control;
    owner->ops=configured?*configured:(KgLeaseOps){gc_native_heap_reserve,kgl_commit,gc_native_heap_decommit_range,gc_native_metadata_unmap};
    if(!owner->ops.reserve||!owner->ops.commit||!owner->ops.decommit||!owner->ops.release)kg_fail("lease native operations",0);
    owner->maximum=heap->metadata_max/sizeof(MetadataLeaseSlot);
    if(owner->maximum>UINT32_MAX)owner->maximum=UINT32_MAX;
    uint64_t initial_bytes;
    if(owner->maximum<initial_capacity||gc_native_metadata_size(owner->maximum*sizeof(MetadataLeaseSlot),r->os.metadata.quantum,&owner->reservation_bytes)||
       gc_native_metadata_size(initial_capacity*sizeof(MetadataLeaseSlot),r->os.metadata.quantum,&initial_bytes))kg_fail("lease reservation size",0);
    r->lease_backing=owner;uint64_t failures=0;
    while(!(owner->reservation=(uintptr_t)owner->ops.reserve((size_t)owner->reservation_bytes)))kg_os_retry(&failures,"lease reservation OS retry limit");
    if(owner->reservation&(r->os.metadata.quantum-1))kg_fail("lease reservation alignment",owner->reservation);
    owner->initial.backing.bytes=initial_bytes;
    kg_lock(r);status=gc_budget_step_locked(heap,&owner->initial.backing,GC_BUDGET_RESERVE);kg_unlock(r);
    if(status)kg_fail("initial lease prefix budget",status);failures=0;
    while(!owner->ops.commit((void *)(uintptr_t)owner->reservation,(size_t)initial_bytes))kg_os_retry(&failures,"initial lease prefix OS retry limit");
    memset((void *)(uintptr_t)owner->reservation,0,(size_t)initial_bytes);
    MetadataLeaseSlot *fresh=(void *)(uintptr_t)owner->reservation;
    for(uint64_t i=0;i<initial_capacity;i++)fresh[i].next_free=i+1<initial_capacity?i+2:0;
    owner->initial.backing.allocation=owner->reservation;
    kg_lock(r);status=gc_budget_step_locked(heap,&owner->initial.backing,GC_BUDGET_COMMIT);
    if(status)kg_fail("initial lease prefix publication",status);
    owner->committed_bytes=initial_bytes;owner->chunks=&owner->initial;pool->slots=owner->reservation;pool->capacity=initial_capacity;pool->free_head=1;kg_unlock(r);
}
uint64_t kg_lease_grow_native(KgRuntime *r,const MutatorContext *actor){
    if(!kgl_native(actor)||!r->lease_backing)return KG_LEASE_INVALID;
    KgLeaseBacking *owner=r->lease_backing;MetadataLeasePool *pool=kg_leases(r);
    kg_lock(r);
    if(owner->closing||pool->slots!=owner->reservation){kg_unlock(r);return KG_LEASE_INVALID;}
    if(pool->free_head){kg_unlock(r);return KG_LEASE_OK;}
    if(owner->growing){kg_unlock(r);return KG_LEASE_WAIT;}
    if(pool->capacity>=owner->maximum){kg_unlock(r);return KG_LEASE_LIMIT;}
    uint64_t capacity=pool->capacity,amount=owner->maximum-capacity;if(amount>256)amount=256;
    uint64_t target=capacity+amount,bytes;
    if(gc_native_metadata_size(target*sizeof(MetadataLeaseSlot),r->os.metadata.quantum,&bytes)||bytes>owner->reservation_bytes){kg_unlock(r);return KG_LEASE_INVALID;}
    if(bytes<=owner->committed_bytes){
        /* Already prepared physical padding is not zeroed again: it can
         * contain earlier admitted, active slots in the same OS quantum. */
        uint64_t status=gc_metadata_grow_locked(pool,target);kg_unlock(r);return status?KG_LEASE_INVALID:KG_LEASE_OK;
    }
    owner->growing=1;uint64_t offset=owner->committed_bytes;kg_unlock(r);
    MetadataCharge control={0};uint64_t status=kgl_control(r,sizeof(KgLeaseChunk),&control);
    if(status){
        kg_lock(r);owner->growing=0;
        if(status==KG_LEASE_LIMIT&&pool->free_head)status=KG_LEASE_OK;
        kg_notify(r);kg_unlock(r);return status;
    }
    KgLeaseChunk *chunk=(void *)(uintptr_t)control.allocation;chunk->control=control;chunk->offset=offset;chunk->backing.bytes=bytes-offset;
    kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,&chunk->backing,GC_BUDGET_RESERVE);kg_unlock(r);
    if(status){
        kgl_retire_control(r,&chunk->control,actor);kg_lock(r);owner->growing=0;
        uint64_t result=status==GC_BUDGET_LIMIT?(pool->free_head?KG_LEASE_OK:KG_LEASE_LIMIT):KG_LEASE_INVALID;
        kg_notify(r);kg_unlock(r);return result;
    }
    void *address=(void *)(uintptr_t)(owner->reservation+offset);uint64_t failures=0;
    while(!owner->ops.commit(address,(size_t)chunk->backing.bytes))kg_os_retry(&failures,"lease tail commit OS retry limit");
    memset(address,0,(size_t)chunk->backing.bytes);chunk->backing.allocation=(uintptr_t)address;
    kg_lock(r);
    if(pool->capacity!=capacity||pool->slots!=owner->reservation)kg_fail("concurrent lease capacity mutation",0);
    status=gc_budget_step_locked(&r->native.backing.heap,&chunk->backing,GC_BUDGET_COMMIT);
    if(status)kg_fail("lease tail budget publication",status);
    chunk->next=owner->chunks;owner->chunks=chunk;owner->committed_bytes=bytes;
    /* The core links to the ACTUAL current free head. Existing tokens may
     * have been released/reacquired while this new physical tail was native. */
    status=gc_metadata_grow_locked(pool,target);owner->growing=0;kg_notify(r);kg_unlock(r);
    return status?KG_LEASE_INVALID:KG_LEASE_OK;
}
void kg_lease_pressure(KgRuntime *r){
    MutatorContext *actor=kg_current_context(r);
    if(!actor||atomic_load_explicit(&actor->access_epoch,memory_order_acquire)||
       atomic_load_explicit(&actor->private_producers,memory_order_acquire))kg_fail("lease pressure still owns managed work",0);
    kg_enter_native(r);uint64_t status=kg_lease_grow_native(r,actor);kg_leave_native(r);
    if(status==KG_LEASE_LIMIT){
        kg_lock(r);KgLeaseBacking *owner=r->lease_backing;
        if(kg_leases(r)->free_head)status=KG_LEASE_OK;
        else if(owner->growing)status=KG_LEASE_WAIT;
        kg_unlock(r);
        if(status==KG_LEASE_LIMIT)kg_fail("metadata lease backing budget or index limit",status);
    }
    if(status!=KG_LEASE_OK&&status!=KG_LEASE_WAIT)kg_fail("metadata lease growth",status);
    if(status==KG_LEASE_WAIT){kg_driver_step(r);kg_yield();}
}
void kg_lease_dispose_native(KgRuntime *r,const MutatorContext *actor){
    KgLeaseBacking *owner=r->lease_backing;if(!owner)return;
    if(!kgl_native(actor)||r->driver||r->native.allocations||kg_leases(r)->active||owner->growing)kg_fail("live lease backing disposal",0);
    owner->closing=1;MetadataLeasePool *pool=kg_leases(r);
    /* Final shutdown only, after all users and every token have gone. Never
     * restore the unused bootstrap table with reset generations for new use. */
    kg_lock(r);pool->slots=pool->capacity=pool->free_head=0;kg_unlock(r);
    while(owner->chunks){
        KgLeaseChunk *chunk=owner->chunks;uint64_t failures=0,status;
        /* Tail discard may fail after protection; unlike a control-unmap
         * failure it need not leave readable data. No pool user remains, and
         * the exact reservation/range/charge stay owned until real success. */
        while(!owner->ops.decommit((void *)(uintptr_t)chunk->backing.allocation,(size_t)chunk->backing.bytes))
            kg_os_retry(&failures,"lease tail decommit OS retry limit");
        chunk->backing.allocation=0;
        kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,&chunk->backing,GC_BUDGET_RELEASE);kg_unlock(r);
        if(status)kg_fail("lease tail budget release",status);
        KgLeaseChunk *next=chunk->next;bool initial=chunk==&owner->initial;owner->chunks=next;
        if(!initial)kgl_retire_control(r,&chunk->control,actor);
    }
    uint64_t failures=0;
    while(!owner->ops.release((void *)(uintptr_t)owner->reservation,(size_t)owner->reservation_bytes))
        kg_os_retry(&failures,"lease reservation release OS retry limit");
    owner->reservation=0;r->lease_backing=NULL;kgl_retire_control(r,&owner->control,actor);
}
#endif
