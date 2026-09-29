/* Incoming root-domain audit, registry lock held for each bounded call.
 * One reader lease pins the current ACTIVE/RETIRING segment. NEXT reads one
 * atomic root cell; FOUND leaves the cursor in place. These observations are
 * diagnostic bits, not strong roots and never authorize heap dereference.
 * This is an absence observer, NOT a marking walk: it neither sets mark masks
 * nor ACKs another walk/deletion/journal job's handoff obligations.
 *
 * Before advancing a segment require its journal and deletion claim drained.
 * Final DONE additionally waits for all root reader/walker/job leases, covering
 * outstanding snapshots held by other consumers. New scopes or phase changes
 * force restart. The cycle must exclude new target references behind the
 * cursor until DONE is consumed; atomic cell loads alone are not a snapshot.
 * Cancellation releases the cursor even after invalidation. No whole-root
 * traversal, payload access, waiting, journal consumption or allocation here.
 */
#ifndef KLASSIC_GC_NATIVE_ROOT_INCOMING_H
#define KLASSIC_GC_NATIVE_ROOT_INCOMING_H
#include "gc_native_scan_incoming.h"
typedef struct {
    uint64_t self,phase,last_scope,stage,target_start,target_limit,cursor,limit,observed;
    NativeRuntimeOwner *runtime;
    RootLease lease;
} NativeRootIncoming;
/* Native observer admission for both active and retiring segments. A normal
 * owner ACQUIRE admits active roots only; this observer participates in the
 * existing kind-1 reader counts, so CLEAN cannot unlink its current node. */
static inline uint64_t gc_native_root_incoming_pin(RootRegistry *g,uint64_t address,RootLease *lease,uint64_t *limit){
    if(!address||(address&7))return GC_NSI_INVALID;
    RootRegistration *r=(void *)(uintptr_t)address;
    if(r->registered!=1||!r->scope||!r->segment||(r->segment&7)||r->leases>g->leases)return GC_NSI_INVALID;
    RootSegment *s=(void *)(uintptr_t)r->segment;
    uint64_t state=atomic_load_explicit(&s->state,memory_order_acquire),used=atomic_load_explicit(&s->used,memory_order_acquire);
    if(!gc_root_segment_valid(s)||state<GC_ROOT_SEGMENT_ACTIVE||state>GC_ROOT_SEGMENT_RETIRING||
       s->registration!=address||s->scope!=r->scope||used>s->capacity)return GC_NSI_INVALID;
    if(g->last_lease==UINT64_MAX||g->leases==UINT64_MAX||r->leases==UINT64_MAX)return GC_NSI_WAIT;
    *lease=(RootLease){address,r->scope,++g->last_lease,1,0};g->leases++;r->leases++;*limit=used;
    return GC_NSI_MORE;
}
static inline uint64_t gc_native_root_incoming_release(RootRegistry *g,RootLease *lease){
    RootLifeRequest release={.registration=lease->registration,.scope=lease->scope,.lease=(uintptr_t)lease,.serial=lease->serial};
    return gc_root_life_locked(g,&release,GC_RL_RELEASE)==GC_RL_OK?GC_NSI_DONE:GC_NSI_INVALID;
}
static inline uint64_t gc_native_root_incoming_begin_locked(NativeRuntimeOwner *r,uint64_t start,uint64_t limit,NativeRootIncoming *q){
    if(!q||((uintptr_t)q&7)||q->self||!gc_native_cycle_valid(r,0)||!gc_native_scan_incoming_range(start,limit))return GC_NSI_INVALID;
    RootRegistry *g=&r->roots;RootLease lease={0};uint64_t used=0;
    if(g->inventory){
        if(g->walkers==UINT64_MAX)return GC_NSI_WAIT;
        uint64_t status=gc_native_root_incoming_pin(g,g->inventory,&lease,&used);
        if(status!=GC_NSI_MORE)return status;
        g->walkers++;
    }
    *q=(NativeRootIncoming){.self=(uintptr_t)q,.phase=g->phase,.last_scope=g->last_scope,.stage=g->inventory?1:2,
        .target_start=start,.target_limit=limit,.runtime=r,.lease=lease,.limit=used};
    return GC_NSI_MORE;
}
static inline bool gc_native_root_incoming_valid(const NativeRootIncoming *q){
    return q&&!((uintptr_t)q&7)&&q->self==(uintptr_t)q&&gc_native_cycle_valid(q->runtime,0)&&
        q->stage>=1&&q->stage<=3&&gc_native_scan_incoming_range(q->target_start,q->target_limit);
}
static inline uint64_t gc_native_root_incoming_cancel_locked(NativeRootIncoming *q){
    if(!gc_native_root_incoming_valid(q))return GC_NSI_INVALID;
    RootRegistry *g=&q->runtime->roots;
    if(q->lease.kind){
        if(!g->walkers||gc_native_root_incoming_release(g,&q->lease)!=GC_NSI_DONE)return GC_NSI_INVALID;
        g->walkers--;
    }
    q->stage=3;q->observed=0;return GC_NSI_DONE;
}
static inline uint64_t gc_native_root_incoming_next_locked(NativeRootIncoming *q){
    if(!gc_native_root_incoming_valid(q)||q->stage==3)return GC_NSI_INVALID;
    RootRegistry *g=&q->runtime->roots;
    if(q->phase!=g->phase||q->last_scope!=g->last_scope)return GC_NSI_RESTART;
    if(q->stage==2){
        if(q->lease.kind)return GC_NSI_INVALID;
        if(g->leases||g->walkers)return GC_NSI_WAIT;
        q->observed=0;return GC_NSI_DONE;
    }
    if(q->lease.kind!=1||!q->lease.registration||!q->lease.serial||!g->walkers||q->cursor>q->limit)return GC_NSI_INVALID;
    RootRegistration *r=(void *)(uintptr_t)q->lease.registration;
    if(r->registered!=1||r->scope!=q->lease.scope||!r->leases||r->leases>g->leases||!r->segment||(r->segment&7))return GC_NSI_INVALID;
    RootSegment *s=(void *)(uintptr_t)r->segment;
    uint64_t state=atomic_load_explicit(&s->state,memory_order_acquire),used=atomic_load_explicit(&s->used,memory_order_acquire);
    if(!gc_root_segment_valid(s)||state<1||state>2||s->registration!=(uintptr_t)r||s->scope!=r->scope||used>s->capacity||q->limit>used)return GC_NSI_INVALID;
    if(q->cursor<q->limit){
        uint64_t value=atomic_load_explicit(&((RootCell *)(uintptr_t)s->cells)[q->cursor].value,memory_order_acquire);
        if(!gc_store_ref_valid(value))return GC_NSI_INVALID;
        if(value&&(value>>16)>=q->target_start&&(value>>16)<q->target_limit){q->observed=value;return GC_NSI_FOUND;}
        q->cursor++;q->observed=0;return GC_NSI_MORE;
    }
    if(r->claim_serial||r->journal_serial||r->journal_end)return GC_NSI_WAIT;
    if(!s->journal||(s->journal&7))return GC_NSI_INVALID;
    BufferCursor *journal=(void *)(uintptr_t)s->journal;
    uint64_t consumed=atomic_load_explicit(&journal->consumed,memory_order_acquire);
    uint64_t published=atomic_load_explicit(&journal->published,memory_order_acquire);
    if(consumed>published)return GC_NSI_INVALID;
    if(consumed!=published)return GC_NSI_WAIT;
    RootLease next={0};uint64_t limit=0;
    if(r->inventory_next){
        if(r->inventory_next&7)return GC_NSI_INVALID;
        RootRegistration *node=(void *)(uintptr_t)r->inventory_next;
        if(node->inventory_previous!=(uintptr_t)r||node->scope>=r->scope)return GC_NSI_INVALID;
        uint64_t status=gc_native_root_incoming_pin(g,r->inventory_next,&next,&limit);
        if(status!=GC_NSI_MORE)return status;
    }
    if(gc_native_root_incoming_release(g,&q->lease)!=GC_NSI_DONE){
        if(next.kind)(void)gc_native_root_incoming_release(g,&next);
        return GC_NSI_INVALID;
    }
    q->lease=next;q->limit=limit;q->cursor=q->observed=0;
    if(!next.kind){g->walkers--;q->stage=2;}
    return GC_NSI_MORE;
}
#endif
