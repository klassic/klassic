#ifndef KLASSIC_PIN_ABI_TEST_BODY_H
#define KLASSIC_PIN_ABI_TEST_BODY_H
#include "gc_native_api.h"
typedef struct {uint64_t managed,fail_unmap,calls,mode,retries,cleanup_yields,bootstrap,binding;} User;
static uint64_t GC_SCOPED_PIN_NATIVE_ABI body(void *arg){
    GcScopedPinScope *s=arg;User *u=(void *)(uintptr_t)s->user;
    assert(gc_scoped_pin_api_valid((void *)(uintptr_t)s->api));
    assert(s->stage==3&&s->call.address&&s->call.identity&&++u->calls==1);
    uint64_t address=s->call.address,identity=s->call.identity;
    assert(*(const uint64_t *)(uintptr_t)address==77);
    GcNativeApiBootstrap bootstrap=(GcNativeApiBootstrap)(uintptr_t)u->bootstrap;
    assert(bootstrap(NULL)==0);
    assert(bootstrap((void *)(uintptr_t)(s->call.context+1))==0);
    const GcNativeApiBinding *binding=(void *)(uintptr_t)bootstrap((void *)(uintptr_t)s->call.context);
    assert(gc_native_api_valid(binding)&&binding==(void *)(uintptr_t)u->binding);
    assert(gc_native_api_pins(binding)==(void *)(uintptr_t)s->api);
    assert(gc_native_api_call(binding,GC_NATIVE_ENTER)==GC_SCOPED_PIN_INVALID);
    assert(gc_native_api_call(binding,GC_NATIVE_EXIT)==0);
    assert(gc_native_api_call(binding,GC_NATIVE_EXIT)==GC_SCOPED_PIN_INVALID);
    assert(gc_native_api_call(binding,99)==GC_SCOPED_PIN_INVALID);
    GcScopedPinFunction native_call=(GcScopedPinFunction)(uintptr_t)binding->call;
    assert(native_call(NULL)==GC_SCOPED_PIN_INVALID);
    GcNativeApiBinding copied_binding=*binding;copied_binding.self_=(uintptr_t)&copied_binding;
    assert(gc_native_api_call(&copied_binding,GC_NATIVE_POLL)==GC_SCOPED_PIN_INVALID);
    assert(gc_native_api_call(binding,GC_NATIVE_POLL)==0);
    const GcScopedPinApi *api=(void *)(uintptr_t)s->api;
    assert(gc_scoped_pin_call(api,NULL)==GC_SCOPED_PIN_INVALID);
    assert(gc_scoped_pin_scope_step(api,NULL)==GC_SCOPED_PIN_INVALID);
    GcScopedPinToken copied;memcpy(&copied,&s->token,sizeof(copied));
    GcScopedPinCall invalid={.context=s->call.context,.token=(uintptr_t)&copied,.operation=GC_SCOPED_PIN_RELEASE};
    assert(gc_scoped_pin_call(api,&invalid)==GC_SCOPED_PIN_INVALID);
    assert(invalid.address==0&&invalid.identity==0);
    memset(&copied,0,sizeof(copied));invalid.operation=GC_SCOPED_PIN_NEW;
    assert(gc_scoped_pin_call(api,&invalid)==GC_SCOPED_PIN_INVALID);
    for(size_t i=0;i<18;i++)assert(copied.opaque[i]==0);
    invalid.token=(uintptr_t)&s->token;invalid.operation=GC_SCOPED_PIN_NATIVE_EXIT;
    assert(gc_scoped_pin_call(api,&invalid)==GC_SCOPED_PIN_INVALID); /* Already Running. */
    assert(((GcScopedPinFunction)(uintptr_t)u->managed)(s)==0);
    assert(s->call.address==address&&s->call.identity==identity);
    assert(*(const uint64_t *)(uintptr_t)address==77);
    if(u->mode==1){
        assert(gc_scoped_pin_callback(s,GC_SCOPED_PIN_NATIVE_ENTER)==0);
        assert(gc_scoped_pin_callback(s,GC_SCOPED_PIN_CALLBACK_ENTER)==0);
        assert(((GcScopedPinFunction)(uintptr_t)u->managed)(s)==0);
        assert(*(const uint64_t *)(uintptr_t)address==77);
        return 77; /* Error return while Running, with two owned uses. */
    }
    if(u->mode==2)assert(((GcScopedPinFunction)(uintptr_t)u->fail_unmap)(s)==0);
    if(u->mode==3)assert(gc_scoped_pin_scope_step((void *)(uintptr_t)s->api,s)==GC_SCOPED_PIN_INVALID);
    assert(gc_native_api_call(binding,GC_NATIVE_ENTER)==0);
    assert(gc_native_api_call(binding,GC_NATIVE_POLL)==GC_SCOPED_PIN_INVALID);
    return 77; /* Application error is preserved while mandatory cleanup runs. */
}
#endif
