/* Runtime-owned bounded weak table. All calls hold the common weak/phase/root
 * and heap locks; controls and ops are trusted, live, disjoint native storage.
 * Entries are prepared and budgeted before publication. No allocation, polling,
 * managed payload access or table-wide scan occurs in these operations.
 * Table identity/storage lives for the runtime's entire lifetime and cannot be
 * reinitialized while any handle can exist. Handles carry indices, never slot
 * addresses. A freed index receives a globally increasing table serial.
 * The table is NOT a strong root inventory. Metadata leases protect source
 * metadata only. Dead clearing/termination and scan claims are separate work;
 * future scans must hold this lock or retain an explicit lifetime claim.
 */
#ifndef KLASSIC_GC_WEAK_TABLE_H
#define KLASSIC_GC_WEAK_TABLE_H
#include "gc_weak_release.h"
#include "gc_reference_lease.h"
#define GC_WEAK_FULL 6u
typedef struct {
    uint64_t (*root)(MutatorContext *,RootRequest *,uint64_t);
    uint64_t (*acquire)(MetadataLeasePool *,ReferenceLease *);
    WeakLoader load; WeakReleaser release;
} WeakTableOps;
static inline bool gc_weak_table_valid(const WeakTable *t){
    return t&&!((uintptr_t)t&7)&&t->gate&&!((uintptr_t)t->gate&7)&&
        t->entries&&!((uintptr_t)t->entries&7)&&t->capacity&&t->capacity<=UINT32_MAX&&
        (uintptr_t)t->entries<=UINT64_MAX-t->capacity*sizeof(WeakEntry)&&
        t->active<=t->capacity&&t->active<=t->last_serial&&t->free_head<=t->capacity&&
        (!!t->free_head==(t->active<t->capacity))&&t->gate->closed<=1&&
        t->gate->pool&&!(t->gate->pool&7);
}
/* Returned pointer is borrowed only until the caller drops the common lock. */
static inline WeakEntry *gc_weak_table_entry_locked(WeakTable *t,WeakHandle h){
    if(!gc_weak_table_valid(t)||h.table!=(uintptr_t)t||!h.index||h.index>t->capacity||
        !h.serial||h.serial>t->last_serial)return NULL;
    WeakEntry *e=&t->entries[h.index-1];
    return e->slot.gate==(uintptr_t)t->gate&&e->slot.serial==h.serial&&!e->next_free?e:NULL;
}
static inline uint64_t gc_weak_table_new_locked(WeakTable *t,RootedLoadAdmission *a,
        MutatorContext *ctx,const RootRequest *source,WeakHandle *out,const WeakTableOps *ops){
    if(!gc_weak_table_valid(t)||!a||((uintptr_t)a&7)||!ctx||((uintptr_t)ctx&7)||
        !source||((uintptr_t)source&7)||!out||((uintptr_t)out&7)||out->table||out->index||out->serial||
        !ops||!ops->root||!ops->acquire)return GC_WEAK_INVALID;
    if(t->gate->closed)return GC_WEAK_WAIT;
    if(a->registry!=t->gate->registry||a->handshake!=t->gate->handshake||
        !a->registry||(a->registry&7)||!a->handshake||(a->handshake&7)||!a->participant||(a->participant&7))return GC_WEAK_INVALID;
    RootRegistry *registry=(void *)(uintptr_t)a->registry;
    HandshakeState *h=(void *)(uintptr_t)a->handshake;
    ThreadRegistration *p=(void *)(uintptr_t)a->participant;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||p->context!=(uintptr_t)ctx||p->registry!=a->handshake||
        p->registered!=1||p->identity!=ctx->thread_id||!h->members||!h->head||h->native>h->members||h->active>1||
        p->identity>h->last_identity||p->acknowledged_generation>h->generation||
        (h->active?(!h->generation||h->expected!=h->members-h->native||h->acknowledged>h->expected):(h->expected||h->acknowledged))||
        !gc_abi_valid_phase((PhaseSnapshot){registry->phase}))return GC_WEAK_INVALID;
    if(h->active||p->acknowledged_generation!=h->generation||atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=registry->phase)return GC_WEAK_WAIT;
    if(!source->segment||(source->segment&7))return GC_WEAK_INVALID;
    RootSegment *segment=(void *)(uintptr_t)source->segment;
    if(!segment->registration||(segment->registration&7))return GC_WEAK_INVALID;
    RootRegistration *registration=(void *)(uintptr_t)segment->registration;
    if(registration->segment!=source->segment||registration->scope!=segment->scope||
        registration->context!=(uintptr_t)ctx||registration->registered!=1)return GC_WEAK_INVALID;
    RootRequest input=*source;
    if(ops->root(ctx,&input,GC_ROOT_READ_RAW)!=GC_ROOT_OK)return GC_WEAK_INVALID;
    if(t->last_serial==UINT64_MAX)return GC_WEAK_EXHAUSTED;
    if(!t->free_head)return GC_WEAK_FULL;
    uint64_t index=t->free_head;WeakEntry *e=&t->entries[index-1];
    if(e->slot.gate||e->slot.serial||e->slot.reference||e->slot.lease.page||e->slot.lease.incarnation||e->slot.lease.token||
        e->next_free>t->capacity||e->next_free==index|| (!!e->next_free!=(t->active+1<t->capacity)))return GC_WEAK_INVALID;
    ReferenceLease held={.reference=input.value};
    if(input.value){
        uint64_t status=ops->acquire((void *)(uintptr_t)t->gate->pool,&held);
        if(status!=GC_ML_OK)return status==GC_ML_FULL?GC_WEAK_FULL:status==GC_ML_EXHAUSTED?GC_WEAK_EXHAUSTED:GC_WEAK_INVALID;
    }
    uint64_t serial=t->last_serial+1;
    t->free_head=e->next_free;t->active++;t->last_serial=serial;
    *e=(WeakEntry){.slot={(uintptr_t)t->gate,serial,input.value,held.lease}};
    *out=(WeakHandle){(uintptr_t)t,index,serial};return GC_WEAK_LOADED;
}
static inline uint64_t gc_weak_table_load_locked(WeakTable *t,WeakHandle h,
        RootedLoadAdmission *a,MutatorContext *ctx,RootRequest *out,const WeakTableOps *ops){
    WeakEntry *e=gc_weak_table_entry_locked(t,h);
    if(!e||!ops||!ops->load)return GC_WEAK_INVALID;
    WeakLoadRequest q={(uintptr_t)a,(uintptr_t)ctx,(uintptr_t)&e->slot,h.serial,(uintptr_t)out};
    return ops->load(t->gate,&q,0);
}
static inline uint64_t gc_weak_table_release_locked(WeakTable *t,WeakHandle h,const WeakTableOps *ops){
    WeakEntry *e=gc_weak_table_entry_locked(t,h);
    if(!e||!t->active||!ops||!ops->release)return GC_WEAK_INVALID;
    uint64_t status=ops->release(t->gate,&e->slot,h.serial);
    if(status!=GC_WEAK_RELEASED)return status;
    e->next_free=t->free_head;t->free_head=h.index;t->active--;return GC_WEAK_RELEASED;
}
/* Fixed ABI: LOAD publishes into the registered output root; RELEASE ignores
 * admission/context/output_root and invalidates the serial before returning the
 * index to the table free list. The request is borrowed and never modified. */
enum { GC_WT_LOAD=0,GC_WT_RELEASE=1 };
typedef uint64_t (*WeakTableStepper)(WeakTable *,WeakTableRequest *,uint64_t);
static inline uint64_t gc_weak_table_step_locked(WeakTable *table,WeakTableRequest *q,uint64_t op){
    if(op>1||!q||((uintptr_t)q&7))return GC_WEAK_INVALID;
    const WeakTableOps ops={.load=gc_weak_load_into_locked,.release=gc_weak_release_locked};
    if(op==GC_WT_RELEASE)return gc_weak_table_release_locked(table,q->handle,&ops);
    return gc_weak_table_load_locked(table,q->handle,(void *)(uintptr_t)q->admission,
        (void *)(uintptr_t)q->context,(void *)(uintptr_t)q->output_root,&ops);
}
/* The source root remains owned throughout this finite operation. The request
 * is borrowed; output_handle starts zero and receives metadata ownership only
 * after all validation and acquisition succeed. No source-root write occurs. */
typedef uint64_t (*WeakTableCreator)(WeakTable *,WeakCreateRequest *,uint64_t);
static inline uint64_t gc_weak_create_step_locked(WeakTable *table,WeakCreateRequest *q,uint64_t op){
    if(op||!q||((uintptr_t)q&7))return GC_WEAK_INVALID;
    const WeakTableOps ops={.root=gc_root_step,.acquire=gc_reference_lease_locked};
    return gc_weak_table_new_locked(table,(void *)(uintptr_t)q->admission,(void *)(uintptr_t)q->context,
        (void *)(uintptr_t)q->source_root,(void *)(uintptr_t)q->output_handle,&ops);
}
#endif
