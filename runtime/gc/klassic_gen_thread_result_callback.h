/* Implementation include after the scalar callback owner in runtime.c. */
#ifndef KLASSIC_GEN_THREAD_RESULT_CALLBACK_H
#define KLASSIC_GEN_THREAD_RESULT_CALLBACK_H
#include "klassic_gen_thread_result.h"
static void kgtr_check_binding(KgRuntime *r,RootRequest *binding,const char *operation){
    kg_lock(r);uint64_t status=gc_root_step(kg_context(r),binding,GC_ROOT_READ_RAW);binding->value=0;kg_unlock(r);
    if(status)kg_fail(operation,status);
}
void kg_managed_callback_into(KgManagedResultCallback callback,const RootCell *argument,RootCell *out,void *userdata){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);if(!callback)kg_fail("null managed result callback",0);
    bool native=kgt_state(r)==GC_BLOCKED_NATIVE;if(native)kg_leave_native(r);
    kg_lock(r);RootRequest output=kg_root_request_locked(r,out);kg_unlock(r);
    RootCell *borrowed=kg_root_new(),*temporary=kg_root_new();if(argument)kg_root_copy(borrowed,argument);
    kg_lock(r);RootRequest input=kg_root_request_locked(r,borrowed),result=kg_root_request_locked(r,temporary);kg_unlock(r);
    if(kg_callback_depth==UINT64_MAX)kg_fail("managed result callback depth exhausted",0);kg_callback_depth++;
    callback(borrowed,temporary,userdata);
    if(kgt_state(r)==GC_BLOCKED_NATIVE)kg_leave_native(r);
    kgtr_check_binding(r,&input,"managed result callback argument lifetime");
    kgtr_check_binding(r,&result,"managed result callback reservation lifetime");
    kgtr_check_binding(r,&output,"managed result callback caller output lifetime");
    kg_root_copy(out,temporary);kg_root_release(temporary);kg_root_release(borrowed);
    kg_callback_depth--;if(native)kg_enter_native(r);
}
#endif
