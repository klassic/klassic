/* Borrowed adapter binding published by the real attached production owner.
 * Bootstrap(ctx*) returns its native binding, or NULL for an invalid input.
 * Binding and context are creator-affine and remain attached through all uses.
 * Enter/exit/poll allow no unpinned raw managed address. Reload managed values
 * from their real strong roots after returning Running. No heap-wide borrow
 * is taken by these operations. Pin API is the actual same-heap function table.
 */
#ifndef KLASSIC_GC_NATIVE_API_H
#define KLASSIC_GC_NATIVE_API_H
#include "gc_scoped_pin.h"
typedef struct {
    uint64_t version,bytes,self_,context,identity,pins,call;
} GcNativeApiBinding;
typedef struct {uint64_t binding,operation;} GcNativeApiCall;
typedef uint64_t (GC_SCOPED_PIN_NATIVE_ABI *GcNativeApiBootstrap)(void *);
enum {GC_NATIVE_ENTER=0,GC_NATIVE_EXIT=1,GC_NATIVE_POLL=2};
_Static_assert(sizeof(GcNativeApiBinding)==56,"native binding ABI");
_Static_assert(sizeof(GcNativeApiCall)==16,"native call ABI");
static inline int gc_native_api_valid(const GcNativeApiBinding *api){
    return api&&api->version==1&&api->bytes==sizeof(*api)&&
        api->self_==(uintptr_t)api&&api->context&&api->identity&&api->pins&&api->call;
}
static inline uint64_t gc_native_api_call(const GcNativeApiBinding *api,uint64_t operation){
    if(!gc_native_api_valid(api))return GC_SCOPED_PIN_INVALID;
    GcNativeApiCall call={(uintptr_t)api,operation};
    return ((GcScopedPinFunction)(uintptr_t)api->call)(&call);
}
static inline const GcScopedPinApi *gc_native_api_pins(const GcNativeApiBinding *api){
    return gc_native_api_valid(api)?(const GcScopedPinApi *)(uintptr_t)api->pins:NULL;
}
#endif
