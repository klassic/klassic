/* Owner-side shutdown progress with the real worker rendezvous protocol.
 * Call outside all locks and relinquish borrowed payload addresses first.
 * Every invocation performs at most one close action and two worker actions;
 * there is no spin, wait, buffer drain or journal scan inside this function.
 * PARK/RETRY and FLUSH/CLOSE_TLAB results require the scheduler's corresponding
 * service outside locks, then retry this same owner. Never treat these as DONE.
 * Controls, participant, context and runtime bindings remain fixed and live.
 * Callbacks may be C or direct emitted leaves with the same native ABI.
 */
#ifndef KLASSIC_GC_NATIVE_PIN_PROGRESS_H
#define KLASSIC_GC_NATIVE_PIN_PROGRESS_H
#include "gc_native_pin_close.h"
#include "gc_worker.h"
enum { GC_RPIN_WORKER_PARK=17,GC_RPIN_WORKER_RETRY=18,
    GC_RPIN_WORKER_FLUSH_SATB=20,GC_RPIN_WORKER_FLUSH_REMEMBERED=21,
    GC_RPIN_WORKER_CLOSE_TLAB=22,GC_RPIN_WORKER_EXHAUSTED=23 };
typedef struct {
    uint64_t (*close)(const NativePinRuntime *,NativePin *);
    uint64_t (*worker)(RootRegistry *,WorkerRequest *,uint64_t);
} NativePinProgressOps;
static inline uint64_t gc_native_pin_worker_result(uint64_t result){
    if(result==GC_WK_OK)return GC_RPIN_MORE;
    if(result==GC_WK_INVALID||result>GC_WK_EXHAUSTED)return GC_RPIN_INVALID;
    return 16+result;
}
static inline uint64_t gc_native_pin_worker_call(const NativePinRuntime *r,
        WorkerRequest *worker,uint64_t op,const NativePinProgressOps *ops){
    r->lock(r->registry_lock);
    uint64_t result=ops->worker(r->registry,worker,op);
    r->unlock(r->registry_lock);return result;
}
static inline uint64_t gc_native_pin_progress(const NativePinRuntime *r,NativePin *q,
        WorkerRequest *worker,const NativePinProgressOps *provided){
    const NativePinProgressOps defaults={gc_native_pin_close,gc_worker_step_locked};
    const NativePinProgressOps *ops=provided?provided:&defaults;
    if(!ops->close||!ops->worker||!gc_native_pin_runtime_valid(r,q))return GC_RPIN_INVALID;
    if(q->stage==6)return ops->close(r,q);
    if(!q->charge||(q->charge&7)||((MetadataCharge *)(uintptr_t)q->charge)->owner!=(uintptr_t)r->heap||
       !q->context||(q->context&7)||!worker||((uintptr_t)worker&7)||
       !worker->handshake||(worker->handshake&7)||!worker->participant||(worker->participant&7))
        return GC_RPIN_INVALID;
    const ThreadRegistration *participant=(void *)(uintptr_t)worker->participant;
    if(participant->context!=q->context||participant->registry!=worker->handshake||participant->registered!=1)
        return GC_RPIN_INVALID;
    /* Guard EXIT is the sole action allowed before polling or taking a mutex. */
    if(q->stage==3&&q->pin.stage==2)return ops->close(r,q);
    const MutatorContext *ctx=(void *)(uintptr_t)q->context;
    uint64_t state=atomic_load_explicit(&ctx->state,memory_order_acquire);
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
       atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||state>GC_BLOCKED_NATIVE)
        return GC_RPIN_INVALID;
    if(state!=GC_BLOCKED_NATIVE){
        uint64_t result=gc_native_pin_worker_call(r,worker,GC_WK_POLL,ops);
        if(result!=GC_WK_OK)return gc_native_pin_worker_result(result);
    }
    uint64_t result=ops->close(r,q);
    if(result!=GC_RPIN_RESUME&&result!=GC_RPIN_NATIVE)return result;
    return gc_native_pin_worker_result(gc_native_pin_worker_call(r,worker,
        result==GC_RPIN_RESUME?GC_WK_LEAVE_NATIVE:GC_WK_ENTER_NATIVE,ops));
}
static inline uint64_t gc_native_pin_progress_entry(const NativePinRuntime *r,NativePin *q,WorkerRequest *worker){
    return gc_native_pin_progress(r,q,worker,NULL);
}
#endif
