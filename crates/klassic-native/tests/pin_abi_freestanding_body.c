/* Freestanding native callback, compiled for AAPCS64 or Win64 and embedded
 * without native libraries or external-symbol substitution in direct images. */
#define inline __inline__ __attribute__((__always_inline__))
#include "gc_native_api.h"
#undef inline
typedef struct {uint64_t managed,fail_unmap,calls,mode,retries,cleanup_yields,bootstrap,binding;} User;
uint64_t pin_body(void *arg){
    GcScopedPinScope *s=arg;User *u=(void *)(uintptr_t)s->user;
    if(!gc_scoped_pin_api_valid((void *)(uintptr_t)s->api)||s->stage!=3||++u->calls!=1)return 601;
    uint64_t address=s->call.address;
    if(!address||*(const uint64_t *)(uintptr_t)address!=77)return 602;
    GcNativeApiBootstrap bootstrap=(GcNativeApiBootstrap)(uintptr_t)u->bootstrap;
    if(bootstrap(NULL)||bootstrap((void *)(uintptr_t)(s->call.context+1)))return 613;
    const GcNativeApiBinding *binding=(void *)(uintptr_t)bootstrap((void *)(uintptr_t)s->call.context);
    if(!gc_native_api_valid(binding)||(uintptr_t)binding!=u->binding||gc_native_api_pins(binding)!=(void *)(uintptr_t)s->api)return 612;
    if(gc_native_api_call(binding,GC_NATIVE_ENTER)!=GC_SCOPED_PIN_INVALID)return 614;
    if(gc_native_api_call(binding,GC_NATIVE_EXIT))return 603;
    if(gc_native_api_call(binding,GC_NATIVE_EXIT)!=GC_SCOPED_PIN_INVALID||gc_native_api_call(binding,99)!=GC_SCOPED_PIN_INVALID)return 615;
    GcNativeApiBinding copied=*binding;copied.self_=(uintptr_t)&copied;
    if(gc_native_api_call(&copied,GC_NATIVE_POLL)!=GC_SCOPED_PIN_INVALID)return 616;
    if(((GcScopedPinFunction)(uintptr_t)binding->call)(NULL)!=GC_SCOPED_PIN_INVALID||gc_native_api_call(binding,GC_NATIVE_POLL))return 617;
    if(((GcScopedPinFunction)(uintptr_t)u->managed)(s))return 604;
    if(s->call.address!=address||*(const uint64_t *)(uintptr_t)address!=77)return 605;
    if(u->mode==1){
        if(gc_scoped_pin_callback(s,GC_SCOPED_PIN_NATIVE_ENTER))return 606;
        if(gc_scoped_pin_callback(s,GC_SCOPED_PIN_CALLBACK_ENTER))return 607;
        if(((GcScopedPinFunction)(uintptr_t)u->managed)(s))return 608;
        return 77; /* Running with two uses: automatic scope unwinds both. */
    }
    if(u->mode==2&&((GcScopedPinFunction)(uintptr_t)u->fail_unmap)(s))return 610;
    if(u->mode==3&&gc_scoped_pin_scope_step((void *)(uintptr_t)s->api,s)!=GC_SCOPED_PIN_INVALID)return 611;
    if(gc_native_api_call(binding,GC_NATIVE_ENTER))return 609;
    if(gc_native_api_call(binding,GC_NATIVE_POLL)!=GC_SCOPED_PIN_INVALID)return 618;
    return 77;
}
