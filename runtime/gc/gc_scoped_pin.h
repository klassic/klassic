/* Native unary API for the direct collector's real scoped-pin implementation.
 * Contexts and source RootCells belong to the calling attached mutator. This
 * header does not implement a second collector or mutate any GC control state.
 * Token/scope storage is aligned, initially zero, unique and address-stable
 * through terminal cleanup. Do not copy it or write runtime-owned fields.
 * New: 2 owns the root; 4 owns nothing and requires retaining the source root.
 * Step/release: 2 or 6 can retain resources. Keep storage and retry cleanup.
 * Callback returns (including application errors) always pass through actual
 * Native exit and unpin. A foreign throw/longjmp must not cross runtime frames.
 */
#ifndef KLASSIC_GC_SCOPED_PIN_H
#define KLASSIC_GC_SCOPED_PIN_H
#include <stddef.h>
#include <stdint.h>
#ifndef GC_SCOPED_PIN_NATIVE_ABI
#define GC_SCOPED_PIN_NATIVE_ABI
#endif
enum {
    GC_SCOPED_PIN_OK=0, GC_SCOPED_PIN_PENDING=2, GC_SCOPED_PIN_INVALID=3,
    GC_SCOPED_PIN_UNOWNED_RETRY=4, GC_SCOPED_PIN_RESOURCE=6,
    GC_SCOPED_PIN_NEW=0, GC_SCOPED_PIN_STEP=1, GC_SCOPED_PIN_RELEASE=2,
    GC_SCOPED_PIN_NATIVE_ENTER=3, GC_SCOPED_PIN_NATIVE_EXIT=4,
    GC_SCOPED_PIN_CALLBACK_ENTER=5, GC_SCOPED_PIN_CALLBACK_RETURN=6,
    GC_SCOPED_PIN_VIEW=7, GC_SCOPED_PIN_END_RUNNING_USE=8
};
typedef struct {uint64_t opaque[18];} GcScopedPinToken;
typedef struct {
    uint64_t context,source,token,operation,address,identity;
} GcScopedPinCall;
typedef struct {
    uint64_t version,token_bytes,call_bytes,scope_bytes,call,scope;
} GcScopedPinApi;
typedef struct {
    GcScopedPinCall call;
    uint64_t api,callback,user,result,stage,error,last_status;
    GcScopedPinToken token;
} GcScopedPinScope;
typedef uint64_t (GC_SCOPED_PIN_NATIVE_ABI *GcScopedPinFunction)(void *);
_Static_assert(sizeof(GcScopedPinToken)==144,"pin token ABI");
_Static_assert(sizeof(GcScopedPinCall)==48,"pin call ABI");
_Static_assert(sizeof(GcScopedPinScope)==248,"pin scope ABI");
_Static_assert(offsetof(GcScopedPinScope,token)==104,"pin scope token ABI");
static inline int gc_scoped_pin_api_valid(const GcScopedPinApi *api){
    return api&&api->version==1&&api->token_bytes==sizeof(GcScopedPinToken)&&
        api->call_bytes==sizeof(GcScopedPinCall)&&api->scope_bytes==sizeof(GcScopedPinScope)&&
        api->call&&api->scope;
}
static inline uint64_t gc_scoped_pin_call(const GcScopedPinApi *api,GcScopedPinCall *call){
    if(!gc_scoped_pin_api_valid(api))return GC_SCOPED_PIN_INVALID;
    return ((GcScopedPinFunction)(uintptr_t)api->call)(call);
}
static inline uint64_t gc_scoped_pin_scope_step(const GcScopedPinApi *api,GcScopedPinScope *scope){
    if(!gc_scoped_pin_api_valid(api))return GC_SCOPED_PIN_INVALID;
    return ((GcScopedPinFunction)(uintptr_t)api->scope)(scope);
}
/* Only owned stages retain cleanup obligations. A preparation failure can
 * finish cleanup and return its original error at terminal stage 5; it must
 * not be confused with an OS failure that still owns stage-4 storage. */
static inline int gc_scoped_pin_scope_pending(const GcScopedPinScope *scope,uint64_t status){
    return scope&&(status==GC_SCOPED_PIN_PENDING||status==GC_SCOPED_PIN_RESOURCE)&&
        (scope->stage==1||scope->stage==2||scope->stage==4);
}
/* A callback can enter Running, allocate/poll through its attached context,
 * and then return to Native. Scope cleanup also handles a body returning in
 * Running on an error path, and drains nested uses owned by this same scope. */
static inline uint64_t gc_scoped_pin_callback(GcScopedPinScope *scope,uint64_t operation){
    if(!scope)return GC_SCOPED_PIN_INVALID;
    GcScopedPinCall call={.context=scope->call.context,
        .token=(uint64_t)(uintptr_t)&scope->token,.operation=operation};
    return gc_scoped_pin_call((const GcScopedPinApi *)(uintptr_t)scope->api,&call);
}
#endif
