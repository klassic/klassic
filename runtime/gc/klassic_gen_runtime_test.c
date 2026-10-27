/* Actual application-facing C ABI test. Link the real collector driver;
 * no fixture phase edits, forged root/mark receipts or raw managed pointers. */
#include "klassic_gen_runtime.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

int main(int argc,char **argv){
    uint64_t flags=argc>1&&!strcmp(argv[1],"stress")?1:0;
    kg_configure(UINT64_C(8)*1024*1024,0,UINT64_C(4)*1024*1024,flags);
    kg_init();
    RootCell *number=kg_root_new(),*record=kg_root_new(),*alias=kg_root_new();
    RootCell *observed=kg_root_new(),*scratch=kg_root_new(),*null=kg_root_new();
    RootCell *cycle_a=kg_root_new(),*cycle_b=kg_root_new();
    kg_alloc_into(8,1,number);kg_store_i64(number,0,UINT64_C(0xfeed0123456789ab));
    kg_root_copy(alias,number);kg_root_copy(alias,alias);
    kg_alloc_into(16,2,record);kg_store_ref(record,0,number);kg_store_ref(record,8,number);
    kg_load_ref_into(record,0,observed);
    assert(kg_load_i64(observed,0)==UINT64_C(0xfeed0123456789ab));
    /* The overwritten reference is retained by another field and alias, while
     * the deletion and reinsertion exercise both actual store-barrier paths. */
    kg_store_ref(record,0,null);kg_load_ref_into(record,0,observed);
    kg_root_copy(observed,number);kg_store_ref(record,0,observed);
    kg_root_clear(number);kg_root_clear(observed);
    kg_alloc_into(8,2,cycle_a);kg_store_ref(cycle_a,0,cycle_a);
    unsigned rounds=flags?64:6000;
    for(unsigned i=0;i<rounds;i++){
        kg_alloc_into(128,1,scratch);kg_store_i64(scratch,0,i);
        if(!(i%17)){
            kg_load_ref_into(record,(i&1)*8,observed);
            assert(kg_load_i64(observed,0)==UINT64_C(0xfeed0123456789ab));
            assert(kg_load_i64(alias,0)==UINT64_C(0xfeed0123456789ab));
            kg_root_clear(observed);
        }
        if(!(i%31)){kg_native_enter();kg_native_exit();}
        kg_root_clear(scratch);
    }
    /* Mutation through an old alias must affect every reference after moves:
     * comparing constant payloads alone would miss duplicate live copies. */
    kg_store_i64(alias,0,UINT64_C(0x12345678abcdef01));
    kg_load_ref_into(record,0,observed);
    assert(kg_load_i64(observed,0)==UINT64_C(0x12345678abcdef01));
    kg_load_ref_into(record,8,observed);
    assert(kg_load_i64(observed,0)==UINT64_C(0x12345678abcdef01));
    kg_root_clear(observed);
    /* The long-lived first record has survived collections; join a fresh
     * record to it, then drop both roots. Shutdown must trace and reclaim an
     * unreachable cross-generation cycle, rather than keep its bitmap alive. */
    kg_alloc_into(8,2,cycle_b);kg_store_ref(cycle_a,0,cycle_b);kg_store_ref(cycle_b,0,cycle_a);
    kg_root_release(cycle_b);kg_root_release(cycle_a);
    for(unsigned round=0;round<12;round++){
        RootCell *roots[500];
        for(unsigned i=0;i<500;i++){roots[i]=kg_root_new();kg_root_copy(roots[i],alias);}
        for(unsigned i=500;i>0;i--)kg_root_release(roots[i-1]);
        kg_poll();
    }
    /* Multi-page and zero-payload shapes use the same rooted allocation API. */
    kg_alloc_into(3*UINT64_C(128)*1024,1,scratch);
    kg_store_i64(scratch,3*UINT64_C(128)*1024-8,771);
    assert(kg_load_i64(scratch,3*UINT64_C(128)*1024-8)==771);
    kg_alloc_into(0,1,scratch);kg_root_clear(scratch);
    kg_load_ref_into(record,0,observed);
    assert(kg_load_i64(observed,0)==UINT64_C(0x12345678abcdef01));
    kg_root_release(null);kg_root_release(scratch);kg_root_release(observed);
    kg_root_release(alias);kg_root_release(record);kg_root_release(number);
    kg_shutdown();
    puts(flags?"canonical C runtime stress: roots, aliases, barriers, growth, large/empty allocations and shutdown PASS":
        "canonical C runtime: roots, aliases, barriers, allocation pressure, growth, large/empty allocations and shutdown PASS");
    return 0;
}
