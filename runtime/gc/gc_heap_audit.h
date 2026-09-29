/* Cooperative page/key/payload audit owner. All native storage is unique,
 * nonmoving and live. Initialize stage/findings/object/slice and walk.cursor /
 * walk.lease to zero; capture walk's pool/heap/table/count and closed gate epoch.
 * Captured context/registry/phase/range stay immutable until CANCEL completes.
 *
 * META and CANCEL run under the heap/metadata/relocation lock. SCAN and CONSUME
 * run outside it. Each call performs one bounded metadata or payload step.
 * SCAN_READY permits SCAN; MORE requests another META. FOUND retains diagnostic
 * bits in slice.observed. CONSUME records a finding and advances past that slot
 * for enumeration only, never repair. A completed nonempty audit returns DIRTY,
 * never DONE. EXIT_REQUIRED must be retried with SCAN immediately, before any
 * poll, blocking, metadata operation or cancellation.
 *
 * Every error retains owned leases/scratch. CANCEL releases the exact page lease
 * and resets traversal only after cleanup succeeds; it bypasses stale gate and
 * phase checks so interrupted audits remain disposable. CANCELLED is not DONE.
 * FULL/WAIT may be retried after the owner handles pressure. RESTART requires
 * cancellation and recapture; no old cursor is silently adopted into new state.
 *
 * The cycle still owns strong object lifetimes, descriptor storage, exclusion of
 * allocation/topology/range reuse and introduction of old-range references.
 * Closing relocation alone does not provide those conditions. Root, buffer,
 * job, handle and forwarding audits are separate. DONE proves only this captured
 * heap traversal, not a concurrent snapshot or a retirement certificate.
 */
#ifndef KLASSIC_GC_HEAP_AUDIT_H
#define KLASSIC_GC_HEAP_AUDIT_H
#include "gc_heap_walk.h"
#include "gc_object_walk.h"
#include "gc_heap_audit_slice.h"
#include "gc_relocation_gate.h"
enum { GC_HA_META=0,GC_HA_SCAN=1,GC_HA_CONSUME=2,GC_HA_CANCEL=3 };
enum { GC_HA_DONE=0,GC_HA_MORE=1,GC_HA_SCAN_READY=2,GC_HA_FOUND=3,GC_HA_INVALID=4,
    GC_HA_WAIT=5,GC_HA_EXHAUSTED=6,GC_HA_RESTART=7,GC_HA_EXIT_REQUIRED=8,
    GC_HA_DIRTY=9,GC_HA_CLEANUP_REQUIRED=10,GC_HA_CANCELLED=11,GC_HA_FULL=12 };
typedef uint64_t (*HeapAuditor)(MetadataLeasePool *,HeapAudit *,uint64_t);
static inline uint64_t gc_heap_audit_step(MetadataLeasePool *g,HeapAudit *q,uint64_t op){
    if(!q||((uintptr_t)q&7)||op>GC_HA_CANCEL||q->stage>4)return GC_HA_INVALID;
    if(op==GC_HA_SCAN){
        if(q->stage!=2)return GC_HA_INVALID;
        // Authenticate the prepared slice's binding before any new guard.
        // Existing guards must still be allowed to finish their exact EXIT.
        if(!q->slice.stage&&!q->slice.access.scope.held&&
           (!q->walk.lease.page||!q->walk.lease.incarnation||!q->walk.lease.token||
            q->object.lease!=(uintptr_t)&q->walk.lease||q->object.page!=q->walk.lease.page||
            q->object.incarnation!=q->walk.lease.incarnation||!q->object.source_offset||
            q->slice.page!=q->walk.lease.page||(q->slice.reference>>16)!=q->object.source_offset||
            q->slice.context!=q->context||q->slice.registry!=q->registry||q->slice.phase!=q->phase||
            q->slice.target_start!=q->target_start||q->slice.target_limit!=q->target_limit))return GC_HA_INVALID;
        uint64_t result=gc_heap_audit_slice(&q->slice);
        if(q->slice.stage||q->slice.access.scope.held)return GC_HA_EXIT_REQUIRED;
        if(result==GC_HAS_DONE||result==GC_HAS_FORWARDED){
            q->object.source_offset=0;q->slice=(HeapAuditSlice){0};q->stage=1;return GC_HA_MORE;
        }
        if(result==GC_HAS_MORE)return GC_HA_SCAN_READY;
        if(result==GC_HAS_FOUND){q->stage=3;return GC_HA_FOUND;}
        return result==GC_HAS_WAIT?GC_HA_WAIT:result==GC_HAS_EXHAUSTED?GC_HA_EXHAUSTED:GC_HA_INVALID;
    }
    if(q->slice.stage||q->slice.access.scope.held)return GC_HA_EXIT_REQUIRED;
    if(op==GC_HA_CONSUME){
        if(q->stage!=3||!q->slice.observed||q->slice.cursor>UINT64_MAX-8)return GC_HA_INVALID;
        if(q->findings==UINT64_MAX)return GC_HA_EXHAUSTED;
        q->findings++;q->slice.cursor+=8;q->slice.observed=0;q->stage=2;return GC_HA_SCAN_READY;
    }
    if(!g||((uintptr_t)g&7)||q->walk.pool!=(uintptr_t)g||!q->context||(q->context&7))return GC_HA_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_HA_INVALID;
    if(op==GC_HA_CANCEL){
        if(q->walk.lease.page||q->walk.lease.incarnation||q->walk.lease.token){
            if(gc_metadata_step_locked(g,&q->walk.lease,GC_ML_RELEASE)!=GC_ML_OK)return GC_HA_CLEANUP_REQUIRED;
        }
        q->walk.cursor=0;q->object=(ObjectWalk){0};q->slice=(HeapAuditSlice){0};q->stage=q->findings=0;
        return GC_HA_CANCELLED;
    }
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
       !q->registry||(q->registry&7)||!q->gate||(q->gate&7)||!q->epoch||!gc_abi_valid_phase((PhaseSnapshot){q->phase})||
       !q->target_start||q->target_start>=q->target_limit||q->target_limit>GC_MAX_OFFSET||((q->target_start|q->target_limit)&15))return GC_HA_INVALID;
    RelocationGate *gate=(void *)(uintptr_t)q->gate;
    if(gate->epoch!=q->epoch||atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=q->phase)return GC_HA_RESTART;
    uint64_t status=gc_relocation_gate_locked(gate,NULL,GC_RG_CHECK);
    if(status!=GC_RG_OK)return status==GC_RG_WAIT?GC_HA_WAIT:GC_HA_INVALID;
    if(q->stage==4)return q->findings?GC_HA_DIRTY:GC_HA_DONE;
    if(q->stage==3)return GC_HA_FOUND;
    if(q->stage==2)return GC_HA_SCAN_READY;
    if(!q->stage){
        status=gc_heap_walk_locked(g,&q->walk);
        if(status==GC_HW_DONE){q->stage=4;return q->findings?GC_HA_DIRTY:GC_HA_DONE;}
        if(status!=GC_HW_PAGE)return status==GC_HW_MORE?GC_HA_MORE:status==GC_HW_FULL?GC_HA_FULL:
            status==GC_HW_EXHAUSTED?GC_HA_EXHAUSTED:status==GC_HW_WAIT?GC_HA_WAIT:GC_HA_INVALID;
        AllocationPage *page=(void *)(uintptr_t)q->walk.lease.page;
        if(!page->forward_table||(page->forward_table&7))return GC_HA_CLEANUP_REQUIRED;
        ForwardTable *table=(void *)(uintptr_t)page->forward_table;
        q->object=(ObjectWalk){(uintptr_t)&q->walk.lease,q->walk.lease.page,q->walk.lease.incarnation,
            page->forward_table,atomic_load_explicit(&table->used,memory_order_acquire),0,0};
        q->stage=1;return GC_HA_MORE;
    }
    status=gc_object_walk_locked(g,&q->object);
    if(status==GC_OW_WAIT)return GC_HA_RESTART;
    if(status==GC_OW_INVALID)return GC_HA_INVALID;
    if(status==GC_OW_DONE){
        if(gc_metadata_step_locked(g,&q->walk.lease,GC_ML_RELEASE)!=GC_ML_OK)return GC_HA_CLEANUP_REQUIRED;
        q->object=(ObjectWalk){0};q->slice=(HeapAuditSlice){0};q->stage=0;return GC_HA_MORE;
    }
    q->slice=(HeapAuditSlice){.page=q->walk.lease.page,.context=q->context,
        .reference=(q->object.source_offset<<16)|gc_abi_color((PhaseSnapshot){q->phase}),
        .registry=q->registry,.phase=q->phase,.target_start=q->target_start,.target_limit=q->target_limit};
    q->stage=2;return GC_HA_SCAN_READY;
}
#endif
