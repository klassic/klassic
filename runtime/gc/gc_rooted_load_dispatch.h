/* Native-lock slow-load entry. Shares the 64-byte binding layout with the
 * store dispatcher; admission points to this owner's RootedLoadAdmission.
 * Same lock/callback lifetime and no-managed-callback contract as store.
 * One bounded action per call: ACCESS and cancellation never acquire a lock.
 * Fresh loads and RESTART capture under the common registry lock. A captured
 * result survives cancellation/rearm; recapture must not reread its field.
 * REARM only accepts cancelled requests after Running resumes. The outer
 * runtime supplies waits/assistance and owns roots through completion. */
#ifndef KLASSIC_GC_ROOTED_LOAD_DISPATCH_H
#define KLASSIC_GC_ROOTED_LOAD_DISPATCH_H
#include "gc_rooted_store_dispatch.h"
enum { GC_RLD_STEP=0,GC_RLD_CANCEL=1,GC_RLD_REARM=2 };
typedef uint64_t (*RootedLoadDispatcher)(const RootedStoreRuntime *,RootedLoad *,uint64_t);
static inline uint64_t gc_rooted_load_dispatch(const RootedStoreRuntime *d,RootedLoad *q,uint64_t op){
    if(!d||((uintptr_t)d&7)||op>GC_RLD_REARM)return GC_RLOAD_INVALID;
    uint64_t action=gc_rooted_load_next(q);
    if(action==GC_RLOAD_INVALID)return action;
    if(op==GC_RLD_CANCEL)return gc_rooted_load(NULL,q,GC_RLOAD_CANCEL);
    if(op==GC_RLD_REARM&&q->stage!=GC_RLOAD_ABORTED)return gc_rooted_load(NULL,q,GC_RLOAD_REARM);
    if(op==GC_RLD_STEP&&(action==GC_RLOAD_DONE||action==GC_RLOAD_CANCELLED||action==GC_RLOAD_ERROR||action==GC_RLOAD_NULL_RECEIVER||action==GC_RLOAD_ACCESS))return gc_rooted_load(NULL,q,action);
    uint64_t context=q->observation.receiver.follow.context;
    if(!context||(context&7)||!d->lock||!d->unlock)return GC_RLOAD_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RLOAD_INVALID;
    bool admit=op==GC_RLD_STEP&&!q->cancel&&(action==GC_RLOAD_RESTART||(!q->stage&&!q->admitted));
    uint64_t selected;
    if(admit){if(!d->admission||(d->admission&7))return GC_RLOAD_INVALID;selected=d->registry_lock;}
    else if(op==GC_RLD_REARM||action==GC_RLOAD_HEAP){if(!d->heap_pool||(d->heap_pool&7))return GC_RLOAD_INVALID;selected=d->heap_lock;}
    else if(action==GC_RLOAD_QUEUE){if(q->marks.current>1)return GC_RLOAD_INVALID;selected=d->queue_locks[q->marks.current];}
    else return GC_RLOAD_INVALID;
    if(!selected)return GC_RLOAD_INVALID;
    GcNativeLock lock=(GcNativeLock)(uintptr_t)d->lock,unlock=(GcNativeLock)(uintptr_t)d->unlock;
    lock(selected);uint64_t result;
    if(admit){
        result=gc_rooted_load_admit_locked((void *)(uintptr_t)d->admission,q);
        if(result==GC_LA_OK)result=action==GC_RLOAD_RESTART?gc_rooted_load(NULL,q,GC_RLOAD_RESUME):GC_RLOAD_HEAP;
        else result=result==GC_LA_WAIT?GC_RLOAD_WAIT:result==GC_LA_EXHAUSTED?GC_RLOAD_EXHAUSTED:GC_RLOAD_INVALID;
    }else result=gc_rooted_load(action==GC_RLOAD_QUEUE?NULL:(void *)(uintptr_t)d->heap_pool,q,op==GC_RLD_REARM?GC_RLOAD_REARM:action);
    unlock(selected);return result;
}
#endif
