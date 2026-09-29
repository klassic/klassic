/* Resumable rooted pin owner. The runtime supplies a RESERVED, independent root in
 * registered nonmoving storage. PREPARE transfers that root to this control
 * only on success; it copies the caller root through the publication journal.
 * The storage owner must keep the private root segment registered and live
 * until FINISH completes, independently of the original caller's scope.
 * Controls are unique, stable and private; self-binding rejects control copies.
 * The registry must cover every selection/retirement path for this heap.
 *
 * ENTER runs under the existing heap lock. GUARDED requires EXIT immediately
 * after unlocking, without polling. EXIT publishes a canonical payload address
 * only after dropping the ordinary source guard; the page permit, private root
 * and metadata lease remain held through blocking/native callbacks. Failed pin
 * admission also drops its guard before CLEANUP under the lock. Forwarding is
 * followed one rooted edge per ENTER, permitting polls between edges.
 * CANCEL runs under the heap lock, invalidates the address and drops pin/lease.
 * FINISH runs outside the lock, in Running state; journal pressure retains the
 * private root for retry. The caller stops using its borrowed address BEFORE
 * CANCEL, including when cancellation returns a retryable cleanup result.
 * EMPTY observes a null private root; it still requires CANCEL/FINISH. DONE
 * alone means this control has released its root and completed cleanup.
 * These are owner steps, not a public safepoint/assist loop or an FFI unwinder.
 */
#ifndef KLASSIC_GC_ROOT_PIN_H
#define KLASSIC_GC_ROOT_PIN_H
#include "gc_pin_registry.h"
#include "gc_root_access.h"
#include "gc_root_copy.h"
enum { GC_RPIN_MORE=0,GC_RPIN_READY=1,GC_RPIN_WAIT=2,GC_RPIN_FLUSH=3,
    GC_RPIN_INVALID=4,GC_RPIN_FULL=5,GC_RPIN_EXHAUSTED=6,GC_RPIN_CLEANUP=7,
    GC_RPIN_DONE=8,GC_RPIN_GUARDED=9,GC_RPIN_EMPTY=10 };
static inline bool gc_root_pin_valid(const RootPin *q){
    return q&&!((uintptr_t)q&7)&&q->self==(uintptr_t)q&&q->registry&&!(q->registry&7);
}
static inline uint64_t gc_root_pin_root_status(uint64_t status){
    return status==GC_ROOT_OK?GC_RPIN_MORE:status==GC_ROOT_FLUSH?GC_RPIN_FLUSH:
        status==GC_ROOT_FULL?GC_RPIN_FULL:status==GC_ROOT_EXHAUSTED?GC_RPIN_EXHAUSTED:GC_RPIN_INVALID;
}
static inline uint64_t gc_root_pin_prepare(PinPageRegistry *r,MutatorContext *ctx,
    const RootRequest *source,const RootRequest *reserved,RootPin *q){
    if(!r||((uintptr_t)r&7)||!r->pool||((uintptr_t)r->pool&7)||
       !ctx||((uintptr_t)ctx&7)||!source||((uintptr_t)source&7)||!reserved||((uintptr_t)reserved&7)||
       !q||((uintptr_t)q&7)||source->handle.cell==reserved->handle.cell)return GC_RPIN_INVALID;
    const unsigned char *bytes=(const void *)q;
    for(uint64_t i=0;i<sizeof(*q);i++)if(bytes[i])return GC_RPIN_INVALID;
    RootRequest empty=*reserved;empty.value=0;
    uint64_t status=gc_root_step(ctx,&empty,GC_ROOT_PREPARE_EMPTY);
    if(status!=GC_ROOT_OK)return gc_root_pin_root_status(status);
    status=gc_root_copy(ctx,source,&empty);
    if(status!=GC_ROOT_OK)return gc_root_pin_root_status(status);
    q->self=(uintptr_t)q;q->registry=(uintptr_t)r;q->stage=1;
    q->root.follow.context=(uintptr_t)ctx;q->root.follow.root=empty;
    q->token.permit=(uintptr_t)&q->permit;return GC_RPIN_MORE;
}
static inline uint64_t gc_root_pin_enter_locked(RootPin *q){
    if(!gc_root_pin_valid(q)||q->stage!=1)return GC_RPIN_INVALID;
    PinPageRegistry *r=(void *)(uintptr_t)q->registry;
    if(q->root.follow.cleanup.token)return GC_RPIN_CLEANUP;
    uint64_t status=gc_root_access_locked(r->pool,&q->root);
    if(status!=GC_RAC_READY)return status==GC_RF_ADVANCED?GC_RPIN_MORE:
        status==GC_RF_DONE?GC_RPIN_EMPTY:status==GC_RF_WAIT?GC_RPIN_WAIT:
        status==GC_RF_FLUSH?GC_RPIN_FLUSH:status==GC_RF_METADATA_FULL?GC_RPIN_FULL:
        status==GC_RF_EXHAUSTED?GC_RPIN_EXHAUSTED:
        status==GC_RF_CLEANUP_REQUIRED?GC_RPIN_CLEANUP:GC_RPIN_INVALID;
    q->reference=q->root.access.reference;
    status=gc_pin_registry_step_locked(r,&q->root.follow.cleanup,&q->token,GC_PP_PIN);
    q->pending=status==GC_RG_OK?GC_RPIN_READY:status==GC_RG_WAIT?GC_RPIN_WAIT:
        status==GC_RG_EXHAUSTED?GC_RPIN_EXHAUSTED:GC_RPIN_INVALID;
    q->stage=2;return GC_RPIN_GUARDED;
}
static inline uint64_t gc_root_pin_exit(RootPin *q){
    if(!gc_root_pin_valid(q)||q->stage!=2)return GC_RPIN_INVALID;
    uint64_t status=gc_root_access_cleanup(NULL,&q->root,GC_RAC_EXIT);
    if(status!=GC_RESOLVE_OK)return status==GC_RESOLVE_WAIT?GC_RPIN_GUARDED:GC_RPIN_INVALID;
    if(q->pending!=GC_RPIN_READY){q->stage=1;return GC_RPIN_CLEANUP;}
    const AllocationPage *page=(void *)(uintptr_t)q->root.follow.cleanup.page;
    q->address=page->heap_base+(q->reference>>16);q->stage=3;return GC_RPIN_READY;
}
static inline uint64_t gc_root_pin_cleanup_locked(RootPin *q){
    if(!gc_root_pin_valid(q)||q->stage!=1)return GC_RPIN_INVALID;
    PinPageRegistry *r=(void *)(uintptr_t)q->registry;
    if(!q->root.follow.cleanup.token)return GC_RPIN_MORE;
    if(gc_root_access_cleanup(r->pool,&q->root,GC_RAC_RELEASE)!=GC_RF_DONE)return GC_RPIN_CLEANUP;
    uint64_t result=q->pending;q->pending=0;q->reference=0;return result;
}
static inline uint64_t gc_root_pin_cancel_locked(RootPin *q){
    if(!gc_root_pin_valid(q)||(q->stage!=1&&q->stage!=3&&q->stage!=4))return GC_RPIN_INVALID;
    PinPageRegistry *r=(void *)(uintptr_t)q->registry;
    if(q->stage==3){
        if(gc_pin_registry_step_locked(r,&q->root.follow.cleanup,&q->token,GC_PP_UNPIN)!=GC_RG_OK)
            return GC_RPIN_CLEANUP;
        q->address=0;
    }
    q->stage=4;
    if(q->root.follow.cleanup.token&&
       gc_metadata_step_locked(r->pool,&q->root.follow.cleanup,GC_ML_RELEASE)!=GC_ML_OK)return GC_RPIN_CLEANUP;
    q->stage=5;return GC_RPIN_MORE;
}
static inline uint64_t gc_root_pin_finish(RootPin *q){
    if(!gc_root_pin_valid(q)||q->stage!=5)return GC_RPIN_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)q->root.follow.context;
    RootRequest root=q->root.follow.root;
    if(gc_root_step(ctx,&root,GC_ROOT_READ_RAW)!=GC_ROOT_OK)return GC_RPIN_INVALID;
    uint64_t status=root.value?gc_root_step(ctx,&root,GC_ROOT_RELEASE):gc_root_cancel_empty(ctx,&root);
    if(status!=GC_ROOT_OK)return gc_root_pin_root_status(status);
    q->root=(RootAccess){0};q->reference=q->pending=0;q->stage=6;return GC_RPIN_DONE;
}
static inline uint64_t gc_root_pin_prepare_entry(RootPin *q,const RootPinStart *start){
    if(!start||((uintptr_t)start&7))return GC_RPIN_INVALID;
    return gc_root_pin_prepare((void *)(uintptr_t)start->registry,(void *)(uintptr_t)start->context,
        (void *)(uintptr_t)start->source,(void *)(uintptr_t)start->reserved,q);
}
#endif
