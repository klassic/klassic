/* Stable virtual weak-entry prefix. A logical slot is published only after
 * its physical prefix and unique budget receipt are prepared in Native.
 * Existing slots/indices/serials never move or get reinitialized by growth. */
#ifndef KLASSIC_GEN_WEAK_BACKING_H
#define KLASSIC_GEN_WEAK_BACKING_H
#include "klassic_gen_weak_internal.h"
typedef struct KgWeakChunk {
    struct KgWeakChunk *next;
    MetadataCharge control,backing;
} KgWeakChunk;
typedef struct {
    KgRuntime *runtime;
    MetadataCharge control;
    KgLeaseOps ops;
    WeakLoadGate gate;
    WeakTable table;
    WeakMarkControl mark;
    WeakScan scan;
    WeakIncoming incoming;
    KgRun *rebind_source,*incoming_source;
    uint64_t incarnation,reservation,reservation_bytes,committed_bytes,maximum;
    uint64_t growing,closing,retiring,rebind_index,rebind_capacity,rebind_serial;
    KgWeakChunk initial,*chunks;
} KgWeakInventory;
static KgWeakInventory *kg_weak_inventory;
static _Atomic uint64_t kg_weak_runtime_serial;
static KgWeakInventory *kgw_owner(KgRuntime *r){
    KgWeakInventory *w=kg_weak_inventory;
    if(!w||w->runtime!=r||w->table.gate!=&w->gate||w->gate.pool!=(uintptr_t)kg_leases(r)||
       !gc_weak_table_valid(&w->table))kg_fail("weak inventory ownership",0);
    return w;
}
static void kgw_publish_prefix(KgWeakInventory *w,uint64_t capacity){
    WeakTable *t=&w->table;
    if(w->gate.closed||w->mark.scan||w->mark.processor||capacity<=t->capacity||capacity-t->capacity>256||
       capacity>w->maximum||capacity*sizeof(WeakEntry)>w->committed_bytes)kg_fail("weak prefix admission",capacity);
    for(uint64_t i=t->capacity;i<capacity;i++){
        WeakEntry zero={0};if(memcmp(&t->entries[i],&zero,sizeof(zero)))kg_fail("weak unpublished entry reused",i);
        t->entries[i].next_free=i+1<capacity?i+2:t->free_head;
    }
    t->free_head=t->capacity+1;t->capacity=capacity;
}
void kg_weak_initialize_native(KgRuntime *r,const MutatorContext *actor){
    if(!kgl_native(actor)||r->driver||r->native.stage!=2||kg_weak_inventory)kg_fail("weak bootstrap ownership",0);
    MetadataCharge control={0};uint64_t status=kgl_control(r,sizeof(KgWeakInventory),&control);
    if(status)kg_fail("weak initial control budget",status);
    KgWeakInventory *w=(void *)(uintptr_t)control.allocation;memset(w,0,sizeof(*w));w->control=control;w->runtime=r;
    w->ops=(KgLeaseOps){gc_native_heap_reserve,kgl_commit,gc_native_heap_decommit_range,gc_native_metadata_unmap};
    uint64_t id=atomic_load_explicit(&kg_weak_runtime_serial,memory_order_relaxed);
    for(;;){if(id==UINT64_MAX)kg_fail("weak runtime incarnation exhausted",id);
        if(atomic_compare_exchange_weak_explicit(&kg_weak_runtime_serial,&id,id+1,memory_order_relaxed,memory_order_relaxed))break;}
    w->incarnation=id+1;w->maximum=r->native.backing.heap.metadata_max/sizeof(WeakEntry);
    if(w->maximum>UINT32_MAX)w->maximum=UINT32_MAX;
    uint64_t bytes;
    if(w->maximum<128||gc_native_metadata_size(w->maximum*sizeof(WeakEntry),r->os.metadata.quantum,&w->reservation_bytes)||
       gc_native_metadata_size(128*sizeof(WeakEntry),r->os.metadata.quantum,&bytes))kg_fail("weak prefix geometry",0);
    uint64_t failures=0;
    while(!(w->reservation=(uintptr_t)w->ops.reserve((size_t)w->reservation_bytes)))kg_os_retry(&failures,"weak reservation OS retry limit");
    if(w->reservation&(r->os.metadata.quantum-1))kg_fail("weak reservation alignment",w->reservation);
    MetadataCharge *backing=&w->initial.backing;backing->bytes=bytes;
    kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,backing,GC_BUDGET_RESERVE);kg_unlock(r);
    if(status)kg_fail("weak initial prefix budget",status);failures=0;
    while(!w->ops.commit((void *)(uintptr_t)w->reservation,(size_t)bytes))kg_os_retry(&failures,"weak initial prefix OS retry limit");
    memset((void *)(uintptr_t)w->reservation,0,(size_t)bytes);backing->allocation=w->reservation;
    kg_lock(r);status=gc_budget_step_locked(&r->native.backing.heap,backing,GC_BUDGET_COMMIT);
    if(status)kg_fail("weak initial prefix commit",status);
    w->committed_bytes=bytes;w->chunks=&w->initial;
    w->gate=(WeakLoadGate){(uintptr_t)kg_leases(r),0,(uintptr_t)&r->native.roots,(uintptr_t)&r->native.handshake};
    w->table=(WeakTable){&w->gate,(void *)(uintptr_t)w->reservation,128,1,0,0};
    for(uint64_t i=0;i<128;i++)w->table.entries[i].next_free=i+1<128?i+2:0;
    w->mark.gate=&w->gate;kg_weak_inventory=w;kg_unlock(r);
}
static uint64_t kgw_grow_native(KgRuntime *r,const MutatorContext *actor){
    if(!kgl_native(actor))return KG_LEASE_INVALID;
    kg_lock(r);KgWeakInventory *w=kgw_owner(r);
    if(w->closing){kg_unlock(r);return KG_LEASE_INVALID;}
    if(w->table.free_head){kg_unlock(r);return KG_LEASE_OK;}
    if(w->growing||w->gate.closed){kg_unlock(r);return KG_LEASE_WAIT;}
    if(w->table.capacity==w->maximum){kg_unlock(r);return KG_LEASE_LIMIT;}
    w->growing=1;uint64_t capacity=w->table.capacity+256;
    if(capacity>w->maximum)capacity=w->maximum;
    uint64_t physical;
    if(gc_native_metadata_size(capacity*sizeof(WeakEntry),r->os.metadata.quantum,&physical))kg_fail("weak growth geometry",capacity);
    uint64_t offset=w->committed_bytes;kg_unlock(r);
    if(physical>offset){
        MetadataCharge control={0};uint64_t status=kgl_control(r,sizeof(KgWeakChunk),&control);
        if(status==KG_LEASE_LIMIT){kg_lock(r);w->growing=0;bool free=w->table.free_head!=0;kg_unlock(r);return free?KG_LEASE_OK:KG_LEASE_LIMIT;}
        if(status)kg_fail("weak chunk control",status);
        KgWeakChunk *chunk=(void *)(uintptr_t)control.allocation;memset(chunk,0,sizeof(*chunk));chunk->control=control;
        chunk->backing.bytes=physical-offset;kg_lock(r);
        status=gc_budget_step_locked(&r->native.backing.heap,&chunk->backing,GC_BUDGET_RESERVE);kg_unlock(r);
        if(status){
            if(status!=GC_BUDGET_LIMIT)kg_fail("weak tail reservation",status);
            kgl_retire_control(r,&chunk->control,actor);kg_lock(r);w->growing=0;bool free=w->table.free_head!=0;kg_unlock(r);
            return free?KG_LEASE_OK:KG_LEASE_LIMIT;
        }
        uint64_t failures=0;
        while(!w->ops.commit((void *)(uintptr_t)(w->reservation+offset),(size_t)chunk->backing.bytes))kg_os_retry(&failures,"weak prefix commit OS retry limit");
        memset((void *)(uintptr_t)(w->reservation+offset),0,(size_t)chunk->backing.bytes);
        chunk->backing.allocation=w->reservation+offset;kg_lock(r);
        status=gc_budget_step_locked(&r->native.backing.heap,&chunk->backing,GC_BUDGET_COMMIT);
        if(status)kg_fail("weak prefix commit ownership",status);
        chunk->next=w->chunks;w->chunks=chunk;w->committed_bytes=physical;kg_unlock(r);
    }
    kg_lock(r);uint64_t result=KG_LEASE_WAIT;
    if(!w->gate.closed){kgw_publish_prefix(w,capacity);result=KG_LEASE_OK;}
    w->growing=0;kg_unlock(r);return result;
}
static void kgw_pressure(KgRuntime *r){
    kg_enter_native(r);uint64_t status=kgw_grow_native(r,kg_context(r));kg_leave_native(r);
    if(status==KG_LEASE_LIMIT){kg_lock(r);KgWeakInventory *w=kgw_owner(r);
        if(w->table.free_head)status=KG_LEASE_OK;else if(w->growing||w->gate.closed)status=KG_LEASE_WAIT;kg_unlock(r);}
    if(status==KG_LEASE_LIMIT)kg_fail("weak entry backing budget or index limit",status);
    if(status!=KG_LEASE_OK&&status!=KG_LEASE_WAIT)kg_fail("weak entry growth",status);
}
void kg_weak_dispose_native(KgRuntime *r,const MutatorContext *actor){
    if(!kgl_native(actor)||r->driver||r->native.allocations)kg_fail("weak disposal before collector retirement",0);
    kg_lock(r);KgWeakInventory *w=kgw_owner(r);
    if(w->table.active||w->growing||w->mark.processor||w->mark.scan||w->mark.stage||w->retiring)kg_fail("weak live disposal",w->table.active);
    w->closing=1;kg_unlock(r);
    for(KgWeakChunk *chunk=w->chunks;chunk;){
        KgWeakChunk *next=chunk->next;uint64_t failures=0;
        while(!w->ops.decommit((void *)(uintptr_t)chunk->backing.allocation,(size_t)chunk->backing.bytes))kg_os_retry(&failures,"weak prefix decommit OS retry limit");
        chunk->backing.allocation=0; /* Exact range decommitted; reservation remains owned. */
        kg_lock(r);uint64_t status=gc_budget_step_locked(&r->native.backing.heap,&chunk->backing,GC_BUDGET_RELEASE);kg_unlock(r);
        if(status)kg_fail("weak prefix budget return",status);
        if(chunk!=&w->initial)kgl_retire_control(r,&chunk->control,actor);
        chunk=next;
    }
    uint64_t failures=0;
    while(!w->ops.release((void *)(uintptr_t)w->reservation,(size_t)w->reservation_bytes))kg_os_retry(&failures,"weak reservation release OS retry limit");
    kg_lock(r);kg_weak_inventory=NULL;kg_unlock(r);kgl_retire_control(r,&w->control,actor);
}
#endif
