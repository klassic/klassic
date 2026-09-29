/* Owner-side interruption of one rooted barrier, one bounded cleanup action
 * per call followed (only when clean) by a registry poll and optional rearm.
 * kind=0 load, kind=1 store. Bindings/controls/roots retain their dispatch
 * lifetimes; the caller uniquely owns the operation and its context.
 *
 * Start when a safepoint is requested; repeat while cleanup/park/assistance is
 * pending. READY means Running with a cancelled operation rearmed but NOT yet
 * readmitted: resume its normal dispatcher. A completed operation returns its
 * original DONE/ERROR/NULL status after poll, never executes again. This helper
 * does not initiate a handshake, wait, flush, close a TLAB, release roots or
 * retire queued work. Other nested private producers can keep POLL waiting.
 * ACCESS requires immediate finite cleanup, never a wait. Invalid bindings can
 * be discovered after cleanup; INVALID is not transactional rollback of it. */
#ifndef KLASSIC_GC_BARRIER_POLL_H
#define KLASSIC_GC_BARRIER_POLL_H
#include "gc_rooted_load_dispatch.h"
enum { GC_BP_READY=80,GC_BP_PARK=81,GC_BP_RETRY=82,GC_BP_FLUSH_SATB=83,
    GC_BP_FLUSH_REMEMBERED=84,GC_BP_CLOSE_TLAB=85,GC_BP_EXHAUSTED=86 };
typedef uint64_t (*BarrierPoller)(const RootedStoreRuntime *,void *,uint64_t);
static inline bool gc_barrier_poll_terminal(uint64_t result){
    return result==GC_RLOAD_DONE||result==GC_RLOAD_CANCELLED||result==GC_RLOAD_ERROR||result==GC_RLOAD_NULL_RECEIVER;
}
static inline uint64_t gc_barrier_poll(const RootedStoreRuntime *d,void *request,uint64_t kind){
    if(!d||((uintptr_t)d&7)||kind>1)return GC_RLOAD_INVALID;
    uint64_t action=kind?gc_rooted_store_next(request):gc_rooted_load_next(request);
    if(action==GC_RLOAD_INVALID)return action;
    if(!gc_barrier_poll_terminal(action)){
        uint64_t status=kind?gc_rooted_store_dispatch(d,request,GC_RSD_CANCEL):gc_rooted_load_dispatch(d,request,GC_RLD_CANCEL);
        if(status==GC_RLOAD_INVALID)return status;
        action=kind?gc_rooted_store_dispatch(d,request,GC_RSD_STEP):gc_rooted_load_dispatch(d,request,GC_RLD_STEP);
        if(!gc_barrier_poll_terminal(action))return action;
    }
    uint64_t context=((RootedLoad *)request)->observation.receiver.follow.context;
    if(!context||(context&7)||!d->admission||(d->admission&7)||!d->registry_lock||!d->lock||!d->unlock)return GC_RLOAD_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RLOAD_INVALID;
    const RootedLoadAdmission *admission=(void *)(uintptr_t)d->admission;
    if(!admission->registry||(admission->registry&7)||!admission->participant||(admission->participant&7))return GC_RLOAD_INVALID;
    WorkerRequest worker={admission->handshake,admission->participant};
    GcNativeLock lock=(GcNativeLock)(uintptr_t)d->lock,unlock=(GcNativeLock)(uintptr_t)d->unlock;
    lock(d->registry_lock);
    uint64_t status=((ThreadRegistration *)(uintptr_t)admission->participant)->context==context?
        gc_worker_step_locked((void *)(uintptr_t)admission->registry,&worker,GC_WK_POLL):GC_WK_INVALID;
    unlock(d->registry_lock);
    if(status==GC_WK_OK){
        if(action!=GC_RLOAD_CANCELLED)return action;
        status=kind?gc_rooted_store_dispatch(d,request,GC_RSD_REARM):gc_rooted_load_dispatch(d,request,GC_RLD_REARM);
        return status==GC_RLOAD_HEAP||status==GC_RLOAD_RESTART?GC_BP_READY:status;
    }
    switch(status){
        case GC_WK_PARK:return GC_BP_PARK;case GC_WK_RETRY:return GC_BP_RETRY;
        case GC_WK_FLUSH_SATB:return GC_BP_FLUSH_SATB;case GC_WK_FLUSH_REMEMBERED:return GC_BP_FLUSH_REMEMBERED;
        case GC_WK_CLOSE_TLAB:return GC_BP_CLOSE_TLAB;case GC_WK_EXHAUSTED:return GC_BP_EXHAUSTED;
        default:return GC_RLOAD_INVALID;
    }
}
#endif
