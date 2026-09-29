/* Slow rooted-store OS dispatch: one admission or controller action per call.
 * Runtime bindings and native locks outlive the uniquely owned store request.
 * Heap, registry and each generation queue lock must be the SAME locks used
 * by the collector; they are never nested here. Lock callbacks return only on
 * success, otherwise the native adapter terminates with a diagnostic. They may
 * not poll, execute managed callbacks, adopt a phase or change thread state.
 * Unlock must also be safe when the bounded leaf just established an access
 * guard. An ACCESS action never calls either native lock callback.
 *
 * STEP automatically admits a fresh/rearmed value or recaptures a RESTART
 * under the registry lock. Other actions use exactly the advertised lock
 * domain, with no blocking lock acquisition while access_epoch is nonzero.
 * CANCEL is lock-free intent; keep stepping to terminal cleanup. REARM uses
 * the heap lock only for cancelled controls. Successful stores stay complete.
 * WAIT/FLUSH/exhaustion retain existing obligations; the outer runtime still
 * supplies consumption/assist and cancels before polling. This is not an OS
 * wait loop, collection scheduler, or a fast-path barrier.
 */
#ifndef KLASSIC_GC_ROOTED_STORE_DISPATCH_H
#define KLASSIC_GC_ROOTED_STORE_DISPATCH_H
#include "gc_rooted_store.h"
#include "gc_rooted_load_admit.h"
enum { GC_RSD_STEP=0,GC_RSD_CANCEL=1,GC_RSD_REARM=2 };
typedef void (*GcNativeLock)(uintptr_t);
typedef uint64_t (*RootedStoreDispatcher)(const RootedStoreRuntime *,RootedStore *,uint64_t);
static inline uint64_t gc_rooted_store_dispatch(const RootedStoreRuntime *d,RootedStore *q,uint64_t op){
    if(!d||((uintptr_t)d&7)||op>GC_RSD_REARM)return GC_RLOAD_INVALID;
    uint64_t action=gc_rooted_store_next(q);
    if(action==GC_RLOAD_INVALID)return action;
    if(op==GC_RSD_CANCEL)return gc_rooted_store(NULL,q,GC_RLOAD_CANCEL);
    if(op==GC_RSD_REARM&&q->stage!=GC_RS_ABORTED)return gc_rooted_store(NULL,q,GC_RLOAD_REARM);
    if(op==GC_RSD_STEP&&(action==GC_RLOAD_DONE||action==GC_RLOAD_CANCELLED||action==GC_RLOAD_ERROR||action==GC_RLOAD_NULL_RECEIVER))return gc_rooted_store(NULL,q,action);
    if(op==GC_RSD_STEP&&action==GC_RLOAD_ACCESS)return gc_rooted_store(NULL,q,action);
    uint64_t context=q->value.observation.receiver.follow.context;
    if(!context||(context&7)||!d->lock||!d->unlock)return GC_RLOAD_INVALID;
    MutatorContext *ctx=(void *)(uintptr_t)context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
        atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RLOAD_INVALID;
    bool admit=op==GC_RSD_STEP&&!q->stage&&!q->cancel&&
        (action==GC_RLOAD_RESTART||(!q->admitted&&!q->value.stage&&!q->value.admitted));
    uint64_t selected;
    if(admit){
        if(!d->admission||(d->admission&7))return GC_RLOAD_INVALID;
        selected=d->registry_lock;
    }else if(op==GC_RSD_REARM||action==GC_RLOAD_HEAP){
        if(!d->heap_pool||(d->heap_pool&7))return GC_RLOAD_INVALID;
        selected=d->heap_lock;
    }else if(action==GC_RLOAD_QUEUE){
        if(q->value.marks.current>1)return GC_RLOAD_INVALID;
        selected=d->queue_locks[q->value.marks.current];
    }else return GC_RLOAD_INVALID;
    if(!selected)return GC_RLOAD_INVALID;
    GcNativeLock lock=(GcNativeLock)(uintptr_t)d->lock,unlock=(GcNativeLock)(uintptr_t)d->unlock;
    lock(selected);
    uint64_t r;
    if(admit){
        r=gc_rooted_load_admit_locked((void *)(uintptr_t)d->admission,&q->value);
        if(r==GC_LA_OK)r=action==GC_RLOAD_RESTART?gc_rooted_store(NULL,q,GC_RLOAD_RESUME):GC_RLOAD_HEAP;
        else r=r==GC_LA_WAIT?GC_RLOAD_WAIT:r==GC_LA_EXHAUSTED?GC_RLOAD_EXHAUSTED:GC_RLOAD_INVALID;
    }else r=gc_rooted_store(action==GC_RLOAD_QUEUE?NULL:(void *)(uintptr_t)d->heap_pool,q,op==GC_RSD_REARM?GC_RLOAD_REARM:action);
    unlock(selected);return r;
}
#endif
