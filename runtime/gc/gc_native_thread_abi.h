#ifndef KLASSIC_GC_NATIVE_THREAD_ABI_H
#define KLASSIC_GC_NATIVE_THREAD_ABI_H
#include "gc_native_api.h"
/* Direct runtime ABI, obtained by NativeApiCall operation 4. The existing
 * NativeApiBinding v1 remains seven words; operation 3 remains invalid.
 * All storage is ordinary aligned native memory, never a movable heap object.
 * The runtime table is borrowed for its runtime's lifetime and is read-only. */
typedef struct {
    uint64_t version, bytes, self_, runtime, call, token_bytes, call_bytes;
} GcNativeThreadApi;
/* Exclusive zero-initialized storage. Keep this exact address and OS thread
 * through terminal cleanup. Never inspect/copy/modify the private words. */
typedef struct { uint64_t opaque[5]; } GcNativeThreadToken;
typedef struct {
    uint64_t api, token, operation, cells, entries, context, binding;
} GcNativeThreadCall;
enum {
    GC_NATIVE_THREAD_ATTACH = 0,
    GC_NATIVE_THREAD_STEP = 1,
    GC_NATIVE_THREAD_DETACH = 2,
    GC_NATIVE_THREAD_CURRENT = 3,
    GC_NATIVE_THREAD_SHUTDOWN_TLS = 4
};
/* Return: 0 success, 2 pending/busy, 3 invalid/foreign association, 6 OS failure.
 * Attach/Step may be repeated; cells/entries (1..UINT32_MAX) configure only a
 * fresh token. A token remains owned after pending/failure once allocation has
 * succeeded. Detach requires Running (leave Native first), is resumable, and
 * zeroes the token only after roots, TLS and actual context release complete.
 * Empty-token Detach succeeds. Current/Shutdown ignore token. Context/binding
 * are output only; keep their association/token alive while borrowing them.
 * During Detaching/Detached cleanup Current can return context with binding == 0.
 * No raw movable heap address may cross these calls. Shutdown is explicit and
 * refuses while any association or lookup retains the runtime's TLS key. */
/* Linux fresh Attach can clear a known runtime association inherited from a
 * different physical OS thread, while retaining its key/list reader across
 * the OS call. It never releases the creator's association. Unknown/foreign
 * GS is refused unchanged. An embedding launcher must establish fresh child
 * TLS itself if the creator can dispose its association before child Attach;
 * the direct runtime's owned Linux trampoline clears child GS before entry. */
_Static_assert(sizeof(GcNativeApiBinding) == 56, "native binding v1 layout");
_Static_assert(sizeof(GcNativeApiCall) == 16, "native call v1 layout");
_Static_assert(sizeof(GcNativeThreadApi) == 56, "thread API layout");
_Static_assert(sizeof(GcNativeThreadToken) == 40, "thread token layout");
_Static_assert(sizeof(GcNativeThreadCall) == 56, "thread call layout");
_Static_assert(offsetof(GcNativeThreadCall, context) == 40, "thread context output");
_Static_assert(offsetof(GcNativeThreadCall, binding) == 48, "thread binding output");
static inline int gc_native_thread_api_v1(const GcNativeThreadApi *api) {
    return api && !((uintptr_t)api & 7)
        && api->version == 1 && api->bytes == sizeof(*api)
        && api->self_ == (uint64_t)(uintptr_t)api && api->runtime && api->call
        && api->token_bytes == sizeof(GcNativeThreadToken)
        && api->call_bytes == sizeof(GcNativeThreadCall);
}
/* Helpers reject null, misaligned, and malformed v1 fields. Non-null aligned
 * pointers must still designate readable/writable native storage; the runtime
 * entry authenticates the live binding, token and physical owner. */
/* The binding must be a live borrowed view on its actual owner. */
static inline const GcNativeThreadApi *gc_native_thread_api_from_binding(
        const GcNativeApiBinding *binding) {
    if (!binding || ((uintptr_t)binding & 7) || binding->version != 1
        || binding->bytes != sizeof(*binding)
        || binding->self_ != (uint64_t)(uintptr_t)binding || !binding->call)
        return NULL;
    GcNativeApiCall q = {(uint64_t)(uintptr_t)binding, 4};
    uint64_t (*entry)(GcNativeApiCall *) =
        (uint64_t (*)(GcNativeApiCall *))(uintptr_t)binding->call;
    uint64_t result = entry(&q);
    if (result <= 6 || (result & 7)) return NULL;
    const GcNativeThreadApi *api = (const void *)(uintptr_t)result;
    return gc_native_thread_api_v1(api) ? api : NULL;
}
static inline uint64_t gc_native_thread_invoke(GcNativeThreadCall *q) {
    if (!q || ((uintptr_t)q & 7)) return 3;
    const GcNativeThreadApi *api = (const void *)(uintptr_t)q->api;
    q->context = q->binding = 0;
    if (!gc_native_thread_api_v1(api)) return 3;
    uint64_t (*entry)(GcNativeThreadCall *) =
        (uint64_t (*)(GcNativeThreadCall *))(uintptr_t)api->call;
    return entry(q);
}
#endif
