/* Finite shared context publication step. The caller owns ctx and holds the
 * coordinator's registry lock whenever the context is registered. This does
 * not poll, adopt a phase, hand off buffers, change participant counts or grant
 * permission to access a raw heap address. Those remain caller obligations. */
#ifndef KLASSIC_GC_CONTEXT_H
#define KLASSIC_GC_CONTEXT_H
#include "gc_abi.h"

#define GC_CONTEXT_OK 0u
#define GC_CONTEXT_WRONG_ABI 1u
#define GC_CONTEXT_WRONG_STATE 2u
#define GC_CONTEXT_ACTIVE_ACCESS 3u
#define GC_CONTEXT_INVALID_EDGE 4u
#define GC_CONTEXT_PRIVATE_WORK 5u

/* Failure leaves the entire context untouched. State is the final release
 * publication of the owner's preceding private metadata writes. Expected and
 * desired are full-width integers so generated ABIs have no narrow-argument
 * extension ambiguity. Only the state field itself is a 32-bit atomic word. */
static inline uint64_t gc_context_transition_locked(
    MutatorContext *ctx, uint64_t expected, uint64_t desired
) {
    if (ctx->abi_version != GC_ABI_VERSION) return GC_CONTEXT_WRONG_ABI;
    if (atomic_load_explicit(&ctx->state, memory_order_acquire) != expected)
        return GC_CONTEXT_WRONG_STATE;
    if (atomic_load_explicit(&ctx->access_epoch, memory_order_acquire))
        return GC_CONTEXT_ACTIVE_ACCESS;
    if (atomic_load_explicit(&ctx->private_producers, memory_order_acquire))
        return GC_CONTEXT_PRIVATE_WORK;
    bool permitted = (expected == GC_RUNNING && desired <= GC_DETACHING) ||
        (expected == GC_AT_SAFEPOINT && desired <= GC_AT_SAFEPOINT) ||
        (expected == GC_BLOCKED_NATIVE && desired == GC_RUNNING) ||
        (expected == GC_DETACHING && desired == GC_DETACHED);
    if (!permitted) return GC_CONTEXT_INVALID_EDGE;
    atomic_store_explicit(&ctx->state, (uint32_t)desired, memory_order_release);
    return GC_CONTEXT_OK;
}
#endif
