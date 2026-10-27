/* Freestanding caller of the actual emitted public ABI. Compiled into a
 * relocation-free function by the native conformance harness; no mock table. */
#include "gc_native_thread_abi.h"

static uint64_t invoke_until_ready(GcNativeThreadCall *q) {
    uint64_t result = 2;
    for (unsigned i = 0; i < 1000000 && result == 2; ++i)
        result = gc_native_thread_invoke(q);
    return result;
}

uint64_t gc_native_thread_abi_probe(const GcNativeApiBinding *primary,
                                    uint64_t (*collect)(void)) {
    if (gc_native_thread_api_v1(NULL) || gc_native_thread_invoke(NULL) != 3
        || gc_native_thread_api_from_binding(NULL)) return 24;
    if (gc_native_thread_api_v1((const GcNativeThreadApi *)(uintptr_t)1)
        || gc_native_thread_invoke((GcNativeThreadCall *)(uintptr_t)1) != 3
        || gc_native_thread_api_from_binding((const GcNativeApiBinding *)(uintptr_t)1))
        return 26;
    const GcNativeThreadApi *api = gc_native_thread_api_from_binding(primary);
    if (!api) return 10;
    GcNativeThreadApi invalid = *api;
    invalid.self_ = (uint64_t)(uintptr_t)&invalid;
    invalid.call = 0;
    GcNativeThreadCall invalid_call = {
        (uint64_t)(uintptr_t)&invalid, 0, 0, 0, 0, 1, 1
    };
    if (gc_native_thread_invoke(&invalid_call) != 3
        || invalid_call.context || invalid_call.binding) return 25;
    invalid_call.api = 0;
    invalid_call.context = invalid_call.binding = 1;
    if (gc_native_thread_invoke(&invalid_call) != 3
        || invalid_call.context || invalid_call.binding) return 27;
    GcNativeThreadToken token = {{0}};
    GcNativeThreadCall q = {
        (uint64_t)(uintptr_t)api, (uint64_t)(uintptr_t)&token,
        GC_NATIVE_THREAD_ATTACH, 32, 16, 0, 0
    };
    if (invoke_until_ready(&q)) return 11;
    uint64_t context = q.context, binding = q.binding;
    if (!context || !binding) return 12;
    q.operation = GC_NATIVE_THREAD_CURRENT;
    if (gc_native_thread_invoke(&q) || q.context != context || q.binding != binding)
        return 13;
    const GcNativeApiBinding *owner = (const void *)(uintptr_t)binding;
    if (owner->version != 1 || owner->bytes != sizeof(*owner)
        || owner->self_ != binding || owner->context != context)
        return 14;
    uint64_t (*boundary)(GcNativeApiCall *) =
        (uint64_t (*)(GcNativeApiCall *))(uintptr_t)owner->call;
    GcNativeApiCall native = {binding, 0};
    uint64_t result = 2;
    for (unsigned i = 0; i < 1000000 && result == 2; ++i)
        result = boundary(&native);
    if (result) return 15;
    if (gc_native_thread_invoke(&q) || q.context != context || q.binding != binding)
        return 16;
    /* The attached owner remains Native while an actual managed callback
     * drives collection on the primary owner. The stack token and borrowed
     * native table must survive that entire physical collection. */
    if (!collect || collect()) return 22;
    if (gc_native_thread_invoke(&q) || q.context != context || q.binding != binding)
        return 23;
    native.operation = 1;
    result = 2;
    for (unsigned i = 0; i < 1000000 && result == 2; ++i)
        result = boundary(&native);
    if (result) return 17;
    q.operation = GC_NATIVE_THREAD_DETACH;
    if (invoke_until_ready(&q) || q.context || q.binding) return 18;
    q.operation = GC_NATIVE_THREAD_CURRENT;
    if (gc_native_thread_invoke(&q) || q.context || q.binding) return 19;
    q.operation = GC_NATIVE_THREAD_SHUTDOWN_TLS;
    if (gc_native_thread_invoke(&q)) return 20;
    q.operation = GC_NATIVE_THREAD_DETACH;
    if (gc_native_thread_invoke(&q) || q.context || q.binding) return 21;
    return 0;
}
