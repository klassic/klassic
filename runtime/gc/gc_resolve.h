/* One bounded nonnull reference-resolution/access step. The caller owns a
 * strong source reference and the run-head metadata lease throughout this
 * request, including guard exit. Neither this leaf nor its callees poll, wait,
 * allocate, acquire locks, form a heap address, or inspect managed memory.
 *
 * ENTER returns OK with a source guard, FORWARDED with a private destination,
 * or WAIT with no guard. FORWARDED consumes one edge only: before polling the
 * caller must adopt destination into a prepared root through a no-safepoint
 * transfer (or discard it and retry from the original root). Reacquire the
 * destination's metadata before the next step. Never carry an unrooted private
 * destination across a poll. WAIT is handled outside every guard/data lock.
 *
 * EXIT must complete before metadata release; WAIT retains the live scope and
 * must be retried immediately without polling/blocking. Each successful EXIT
 * resets the scope key for reuse. Unique requests cannot be copied/replayed.
 * Source/color retirement and phase adoption are separate obligations. This
 * primitive is not the complete public, safepoint-capable load/store barrier.
 */
#ifndef KLASSIC_GC_RESOLVE_H
#define KLASSIC_GC_RESOLVE_H
#include "gc_forward_table.h"
#include "gc_forward.h"
#include "gc_source_access.h"
enum { GC_RESOLVE_ENTER=0, GC_RESOLVE_EXIT=1 };
enum { GC_RESOLVE_OK=0, GC_RESOLVE_INVALID=1, GC_RESOLVE_WAIT=2, GC_RESOLVE_FORWARDED=3, GC_RESOLVE_EXHAUSTED=4 };
static inline uint64_t gc_resolve_access(const AllocationPage *g,ResolveRequest *q,uint64_t op) {
    if(op>GC_RESOLVE_EXIT||q->destination||q->scope.ticket||!q->scope.context||(q->scope.context&7))return GC_RESOLVE_INVALID;
    MutatorContext *ctx=(MutatorContext *)(uintptr_t)q->scope.context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING)return GC_RESOLVE_INVALID;
    if(!op&&(q->scope.source_offset||q->scope.incarnation||q->scope.held||atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)))return GC_RESOLVE_INVALID;
    uint64_t offset=q->reference>>16;GcRef encoded;
    if(!gc_abi_encode_ref(offset,(uint16_t)q->reference,&encoded))return GC_RESOLVE_INVALID;
    uint64_t state=atomic_load_explicit(&g->state,memory_order_acquire),top=atomic_load_explicit(&g->allocated_top,memory_order_acquire);
    if((state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED&&state!=GC_PAGE_EVACUATED&&state!=GC_PAGE_UNCOMMITTED)||!g->incarnation||!g->start||g->start>=g->limit||
        g->limit>GC_MAX_OFFSET||((g->start|g->limit|top)&15)||top<g->start||top>g->limit||
        offset<g->start+sizeof(ObjectHeader)||offset>=top||!g->forward_table||(g->forward_table&7))return GC_RESOLVE_INVALID;
    if(op==GC_RESOLVE_EXIT&&(state==GC_PAGE_EVACUATED||state==GC_PAGE_UNCOMMITTED))return GC_RESOLVE_INVALID;
    ForwardLookup lookup={offset,g->incarnation,0};
    if(gc_forward_lookup((const ForwardTable *)(uintptr_t)g->forward_table,&lookup)!=GC_FL_OK)return GC_RESOLVE_INVALID;
    ForwardCell *cell=(ForwardCell *)(uintptr_t)lookup.cell;
    SourceAccessRequest scope=q->scope;
    if(op==GC_RESOLVE_EXIT){
        if(scope.source_offset!=offset||scope.incarnation!=g->incarnation||scope.held!=lookup.cell)return GC_RESOLVE_INVALID;
    }else{
        ForwardRequest forward={offset,g->incarnation,0,0};
        uint64_t status=gc_forward_step(cell,&forward,GC_FWD_READ);
        if(status==GC_FWD_OK){q->destination=forward.destination;return GC_RESOLVE_FORWARDED;}
        if((state==GC_PAGE_EVACUATED||state==GC_PAGE_UNCOMMITTED))return GC_RESOLVE_INVALID;
        if(status==GC_FWD_BUSY)return GC_RESOLVE_WAIT;
        if(status!=GC_FWD_EMPTY&&status!=GC_FWD_RETAINED)return GC_RESOLVE_INVALID;
        scope.source_offset=offset;scope.incarnation=g->incarnation;
    }
    uint64_t status=gc_source_access_step(cell,&scope,op);
    if(status!=GC_SA_OK)return status==GC_SA_EXHAUSTED?GC_RESOLVE_EXHAUSTED:status;
    if(op==GC_RESOLVE_EXIT)scope.source_offset=scope.incarnation=0;
    q->scope=scope;return GC_RESOLVE_OK;
}
#endif
