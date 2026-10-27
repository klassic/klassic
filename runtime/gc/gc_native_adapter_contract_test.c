/* Adapter-table contracts only: this fixture supplies no collector or owner. */
#if THREAD_HEADER_FIRST
#include "gc_native_thread_abi.h"
#include "gc_native_api.h"
#else
#include "gc_native_api.h"
#include "gc_native_thread_abi.h"
#endif
#include "gc_scoped_pin.h"
#include <assert.h>
#include <string.h>

static unsigned calls;
static GcNativeThreadApi table;
static uint64_t binding_call(GcNativeApiCall *q) {
    calls++;
    return q->operation==4?(uint64_t)(uintptr_t)&table:3;
}
static uint64_t thread_call(GcNativeThreadCall *q) {
    assert(!q->context && !q->binding);
    calls++;
    return 6;
}
int main(void) {
    GcNativeApiBinding binding = {1,sizeof(binding),0,17,19,21,(uintptr_t)binding_call};
    binding.self_=(uintptr_t)&binding;
    table=(GcNativeThreadApi){1,sizeof(table),(uintptr_t)&table,1,(uintptr_t)thread_call,
        sizeof(GcNativeThreadToken),sizeof(GcNativeThreadCall)};
    assert(gc_native_api_valid(&binding));
    assert(gc_native_thread_api_from_binding(&binding)==&table && calls==1);
    assert(!gc_native_thread_api_from_binding(NULL));
    assert(!gc_native_thread_api_from_binding((void *)((uintptr_t)&binding+1)));
    binding.version=2;
    assert(!gc_native_thread_api_from_binding(&binding) && calls==1);
    binding.version=1;
    table.token_bytes++;
    assert(!gc_native_thread_api_from_binding(&binding) && calls==2);
    GcNativeThreadCall q = {.api=(uintptr_t)&table,.context=77,.binding=88};
    assert(gc_native_thread_invoke(&q)==3 && !q.context && !q.binding && calls==2);
    table.token_bytes--;
    assert(gc_native_thread_invoke(&q)==6 && calls==3);
    assert(gc_native_thread_invoke(NULL)==3);
    assert(gc_native_thread_invoke((void *)((uintptr_t)&q+1))==3 && calls==3);
    table.call=0;
    assert(gc_native_thread_invoke(&q)==3 && calls==3);
    GcScopedPinScope scope;
    memset(&scope,0,sizeof(scope));
    for (unsigned stage=0;stage<=5;stage++) {
        scope.stage=stage;
        int pending=stage==1||stage==2||stage==4;
        assert(gc_scoped_pin_scope_pending(&scope,2)==pending);
        assert(gc_scoped_pin_scope_pending(&scope,6)==pending);
        assert(!gc_scoped_pin_scope_pending(&scope,0));
        assert(!gc_scoped_pin_scope_pending(&scope,3));
    }
    assert(!gc_scoped_pin_scope_pending(NULL,2));
}
