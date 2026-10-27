/* Separate public ABI translation unit, actual collection and root aliases. */
#include "klassic_gen_atomic_ref.h"
#include <assert.h>
#include <stdio.h>
int main(void){
    kg_configure(16*1024*1024,1024*1024,8*1024*1024,7);kg_init();
    RootCell *receiver=kg_root_new(),*a=kg_root_new(),*b=kg_root_new(),*out=kg_root_new();
    kg_alloc_into(8,1,a);kg_store_i64(a,0,17);kg_alloc_into(8,1,b);kg_store_i64(b,0,23);kg_alloc_into(8,2,receiver);
    assert(kg_compare_exchange_ref_into(receiver,0,out,a,out));
    assert(kg_compare_exchange_ref_into(receiver,0,a,b,out)&&kg_load_i64(out,0)==17);
    kg_exchange_ref_into(receiver,0,out,out);assert(kg_load_i64(out,0)==23);
    kg_root_clear(a);kg_root_clear(b);
    for(unsigned i=0;i<12;i++)kg_alloc_into(8,1,a);
    kg_load_ref_into(receiver,0,b);assert(kg_load_i64(b,0)==17&&kg_load_i64(out,0)==23);
    kg_root_release(out);kg_root_release(b);kg_root_release(a);kg_root_release(receiver);assert(kg_verification_count()>0);kg_shutdown();
    puts("public atomic reference ABI: separate runtime TUs, actual stress movement and sole-edge rooted old result PASS");return 0;
}
