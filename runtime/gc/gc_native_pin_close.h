/* Bounded package shutdown outside all locks. The caller relinquishes every
 * borrowed payload address before the first call and never resumes pin use.
 * Each call performs at most one owner action. RESUME/NATIVE request the worker
 * boundary protocol, not a bare context-state store. WAIT/FLUSH/CLEANUP retain
 * ownership; the scheduler must service retirement/journals/readers or OS retry.
 * A guarded RootPin EXIT is performed before any lock or boundary request.
 * Runtime, context, charge and lock storage remain live through terminal DONE.
 * No nested heap/registry locks, callback polling or managed reentry is allowed.
 */
#ifndef KLASSIC_GC_NATIVE_PIN_CLOSE_H
#define KLASSIC_GC_NATIVE_PIN_CLOSE_H
#include "gc_native_pin.h"
#include "gc_pin_dispatch.h"
#define GC_RPIN_NATIVE 12u
static inline bool gc_native_pin_runtime_valid(const NativePinRuntime *r,const NativePin *q){
    if(!r||((uintptr_t)r&7)||!q||((uintptr_t)q&7)||q->self!=(uintptr_t)q||
       q->stage<1||q->stage>6||q->registry!=(uintptr_t)r->registry||
       !r->pins||((uintptr_t)r->pins&7)||!r->registry||((uintptr_t)r->registry&7)||
       !r->registry_lock||!r->lock||!r->unlock||!r->heap||((uintptr_t)r->heap&7)||
       !r->native||((uintptr_t)r->native&7)||!r->pins->heap_lock||!r->pins->lock||!r->pins->unlock)
        return false;
    return true;
}
static inline uint64_t gc_native_pin_close(const NativePinRuntime *r,NativePin *q){
    if(!gc_native_pin_runtime_valid(r,q))return GC_RPIN_INVALID;
    if(q->stage==6)return GC_RPIN_DONE;
    if(!q->charge||(q->charge&7)||((MetadataCharge *)(uintptr_t)q->charge)->owner!=(uintptr_t)r->heap)
        return GC_RPIN_INVALID;
    if(q->stage==3&&q->pin.stage!=6){
        uint64_t result=gc_root_pin_dispatch(r->pins,&q->pin,GC_PIN_CANCEL);
        return result==GC_RPIN_DONE?GC_RPIN_MORE:result;
    }
    if(!q->context||(q->context&7))return GC_RPIN_INVALID;
    const MutatorContext *ctx=(void *)(uintptr_t)q->context;
    uint64_t state=atomic_load_explicit(&ctx->state,memory_order_acquire);
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
       atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||
       (state!=GC_RUNNING&&state!=GC_BLOCKED_NATIVE))return GC_RPIN_INVALID;
    if(q->stage<=3){
        if(state==GC_BLOCKED_NATIVE)return GC_RPIN_RESUME;
        r->lock(r->registry_lock);
        uint64_t result=gc_native_pin_retire_locked(q);
        r->unlock(r->registry_lock);return result;
    }
    if(state==GC_RUNNING)return GC_RPIN_NATIVE;
    if(q->stage==4){
        uint64_t result=gc_native_pin_dispose(q,r->native);
        return result==GC_RPIN_DONE?GC_RPIN_MORE:result;
    }
    r->pins->lock(r->pins->heap_lock);
    uint64_t result=gc_native_pin_release_locked(q,r->heap);
    r->pins->unlock(r->pins->heap_lock);return result;
}
#endif
