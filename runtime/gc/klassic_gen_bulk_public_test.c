/* Public bulk ABI only: actual separately compiled runtime. */
#include "klassic_gen_bulk.h"
#include <assert.h>
#include <stdio.h>
int main(void){
    kg_configure(16*1024*1024,1024*1024,8*1024*1024,7);kg_init();
    RootCell *source=kg_root_new(),*alias=kg_root_new(),*destination=kg_root_new(),*leaf=kg_root_new();
    kg_alloc_into(8*8,2,source);
    for(unsigned i=0;i<8;i++){kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,i+1);kg_store_ref(source,i*8,leaf);}
    kg_root_copy(alias,source);kg_copy_refs(alias,16,source,0,6);
    kg_alloc_into(8*8,2,destination);kg_copy_refs(destination,0,alias,0,8);
    kg_root_release(alias);kg_root_release(source);kg_root_clear(leaf);
    for(unsigned i=0;i<12;i++){kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,i);}
    unsigned expected[]={1,2,1,2,3,4,5,6};
    for(unsigned i=0;i<8;i++){kg_load_ref_into(destination,i*8,leaf);assert(kg_load_i64(leaf,0)==expected[i]);}
    assert(kg_verification_count()>0);kg_root_release(leaf);kg_root_release(destination);kg_shutdown();
    puts("public bulk ABI: separate runtime TUs, actual collection and sole-edge output retention PASS");return 0;
}
