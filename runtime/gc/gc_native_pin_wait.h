/* Arm one exclusive owner's sticky u32 event under the SAME registry lock
 * used by root FINISH and its notifier. After WAIT, unlock then park(event,0).
 * FINISH must set event=1 and wake under that lock. Publication before park is
 * not lost: the OS compares against zero and declines to sleep. Recheck after
 * every wake/EINTR/EAGAIN; an event alone is never a disposal certificate.
 * Event storage is disjoint from owner/runtime/backing and outlives all users.
 * No shared waiter event,
 * fresh attachment, OS wait, polling or unmap is allowed under this lock. */
#ifndef KLASSIC_GC_NATIVE_PIN_WAIT_H
#define KLASSIC_GC_NATIVE_PIN_WAIT_H
#include "gc_native_pin_close.h"
static inline uint64_t gc_native_pin_wait_arm_locked(const NativePinRuntime *r,NativePin *q,_Atomic uint32_t *event){
    if(!gc_native_pin_runtime_valid(r,q)||!gc_native_pin_valid(q)||q->stage!=4||
       !event||((uintptr_t)event&3)||((MetadataCharge *)(uintptr_t)q->charge)->owner!=(uintptr_t)r->heap)
        return GC_RPIN_INVALID;
    const MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||
       atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_BLOCKED_NATIVE||
       atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RPIN_INVALID;
    if(gc_root_storage_can_dispose((void *)(uintptr_t)q->storage)==GC_ROOT_STORAGE_OK)return GC_RPIN_MORE;
    atomic_store_explicit(event,0,memory_order_release);return GC_RPIN_WAIT;
}
#endif
