/* Native-lock pin owner driver after private-root adoption. Bind the one heap
 * mutex used by this page registry; callbacks only acquire/release that mutex
 * and never call managed code. Runtime/control storage remains stable.
 * Call outside every data/registry lock; Running owners must already have
 * completed the worker phase-admission protocol.
 * STEP does one bounded action. GUARDED means retry immediately: no polling,
 * allocation, wait or lock acquisition is permitted until EXIT succeeds.
 * WAIT/FLUSH/FULL/EXHAUSTED are reached without a guard and can be serviced by
 * the outer runtime while the private root remains owned. RESUME requests the
 * normal worker native-return/phase-adoption protocol before further root work.
 * CANCEL is sticky, including through an in-progress guard EXIT; it never
 * returns READY. The caller relinquishes its borrowed address before CANCEL.
 * DONE means pin/lease/root cleanup, not native package retirement or unmap.
 * INVALID is a contract failure, not permission to abandon retained resources.
 */
#ifndef KLASSIC_GC_PIN_DISPATCH_H
#define KLASSIC_GC_PIN_DISPATCH_H
#include "gc_root_pin.h"
enum { GC_PIN_STEP=0,GC_PIN_CANCEL=1 };
#define GC_RPIN_RESUME 11u
static inline uint64_t gc_root_pin_dispatch(const PinRuntime *runtime,RootPin *q,uint64_t op){
    if(!runtime||((uintptr_t)runtime&7)||!gc_root_pin_valid(q)||op>GC_PIN_CANCEL||
       q->registry!=(uintptr_t)runtime->pages||!runtime->heap_lock||!runtime->lock||!runtime->unlock||
       q->stage<1||q->stage>6||q->cancel>1)return GC_RPIN_INVALID;
    if(op==GC_PIN_CANCEL)q->cancel=1;
    if(q->stage==6)return GC_RPIN_DONE;
    if(q->stage==2){
        uint64_t result=gc_root_pin_exit(q);
        return q->cancel&&result==GC_RPIN_READY?GC_RPIN_MORE:result;
    }
    MutatorContext *ctx=(void *)(uintptr_t)q->root.follow.context;
    if(!ctx||((uintptr_t)ctx&7)||ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
       atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RPIN_INVALID;
    uint64_t state=atomic_load_explicit(&ctx->state,memory_order_acquire);
    if(state!=GC_RUNNING&&state!=GC_BLOCKED_NATIVE)return GC_RPIN_INVALID;
    if(q->stage==3&&!q->cancel)return GC_RPIN_READY;
    if(q->stage==5){
        if(state==GC_BLOCKED_NATIVE)return GC_RPIN_RESUME;
        return gc_root_pin_finish(q);
    }
    if(!q->cancel&&q->stage==1&&state==GC_BLOCKED_NATIVE)return GC_RPIN_RESUME;
    if(state!=GC_RUNNING&&!(q->cancel&&state==GC_BLOCKED_NATIVE))return GC_RPIN_INVALID;
    runtime->lock(runtime->heap_lock);
    uint64_t result;
    if(q->cancel||q->stage==4)result=gc_root_pin_cancel_locked(q);
    else if(q->stage==1)result=q->root.follow.cleanup.token?
        gc_root_pin_cleanup_locked(q):gc_root_pin_enter_locked(q);
    else result=GC_RPIN_INVALID;
    runtime->unlock(runtime->heap_lock);return result;
}
#endif
