/* Separate translation unit: public callback and launch ABI only. */
#include "klassic_gen_thread.h"
#include "klassic_gen_thread_result.h"
#include <assert.h>
#include <stdio.h>
static uint64_t allocating_callback(const RootCell *argument,void *unused){
    (void)unused;RootCell *temporary=kg_root_new();
    for(unsigned i=0;i<24;i++){kg_alloc_into(8,1,temporary);kg_store_i64(temporary,0,i);assert(kg_load_i64(temporary,0)==i);}
    kg_root_release(temporary);uint64_t result=kg_load_i64(argument,0);kg_native_enter();return result;
}
static void allocating_result(const RootCell *argument,RootCell *out,void *unused){
    (void)unused;assert(kg_load_i64(argument,0)==41);kg_alloc_into(8,1,out);kg_store_i64(out,0,77);kg_native_enter();
}
int main(void){
    kg_configure(8*1024*1024,0,4*1024*1024,7);kg_init();RootCell *argument=kg_root_new();
    kg_alloc_into(8,1,argument);kg_store_i64(argument,0,41);
    kg_native_enter();assert(kg_managed_callback(allocating_callback,argument,NULL)==41);kg_native_exit();
    RootCell*out=kg_root_new();KgMutatorThread *managed=kg_thread_start_managed(allocating_result,argument,NULL);assert(managed);
    KgMutatorThread *thread=kg_thread_start(allocating_callback,argument,NULL);assert(thread);kg_root_release(argument);
    assert(kg_thread_join(thread)==41);kg_native_enter();kg_thread_join_into(managed,out);kg_native_exit();
    assert(kg_load_i64(out,0)==77);kg_root_release(out);assert(kg_verification_count()>0);kg_shutdown();
    puts("public launch ABI: separate C translation units, actual stress collection and Native-return callbacks PASS");return 0;
}
