/* Public API only: real OS mutators share one cyclic moving graph. */
#include "klassic_gen_runtime.h"
#include <pthread.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {RootCell *source;unsigned identity,count;} ShareJob;
static void *share_owner(void *argument){
    ShareJob *job=argument;kg_thread_attach();
    RootCell *graph=kg_root_new(),*leaf=kg_root_new(),*scratch=kg_root_new(),*read=kg_root_new();
    kg_root_import(graph,job->source);
    kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,1000+job->identity);
    for(unsigned i=0;i<32;i++){
        kg_alloc_into(64,1,scratch);kg_store_i64(scratch,0,UINT64_C(0x10000000)+job->identity*1000+i);
        kg_store_ref(graph,job->identity*8,scratch);
        kg_load_ref_into(graph,job->identity*8,read);
        assert(kg_load_i64(read,0)==UINT64_C(0x10000000)+job->identity*1000+i);
        kg_store_ref(graph,job->identity*8,leaf);
        kg_load_ref_into(graph,job->identity*8,read);assert(kg_load_i64(read,0)==1000+job->identity);
        kg_load_ref_into(graph,(job->count+1)*8,read);assert(kg_load_i64(read,0)==771);
        kg_root_clear(scratch);kg_root_clear(read);
        if(!(i%7)){kg_native_enter();kg_native_exit();}
    }
    kg_root_release(read);kg_root_release(scratch);kg_root_release(leaf);kg_root_release(graph);
    kg_thread_detach();return NULL;
}
int main(int argc,char **argv){
    unsigned count=argc>1?(unsigned)strtoul(argv[1],NULL,10):2;assert(count==2||count==8);
    kg_configure(UINT64_C(16)*1024*1024,UINT64_C(2)*1024*1024,UINT64_C(8)*1024*1024,5);kg_init();
    RootCell *graph=kg_root_new(),*seed=kg_root_new(),*read=kg_root_new(),*cycle=kg_root_new();
    kg_alloc_into(8,1,seed);kg_store_i64(seed,0,771);kg_alloc_into((count+2)*8,2,graph);
    for(unsigned i=0;i<count;i++)kg_store_ref(graph,i*8,seed);
    kg_store_ref(graph,count*8,graph);kg_store_ref(graph,(count+1)*8,seed);
    ShareJob jobs[8];pthread_t threads[8];kg_native_enter();
    for(unsigned i=0;i<count;i++){jobs[i]=(ShareJob){graph,i,count};assert(!pthread_create(&threads[i],NULL,share_owner,&jobs[i]));}
    for(unsigned i=0;i<count;i++)assert(!pthread_join(threads[i],NULL));
    kg_native_exit();
    for(unsigned i=0;i<count;i++){kg_load_ref_into(graph,i*8,read);assert(kg_load_i64(read,0)==1000+i);}
    kg_load_ref_into(graph,count*8,cycle);kg_load_ref_into(cycle,(count+1)*8,read);assert(kg_load_i64(read,0)==771);
    kg_verify();kg_root_release(cycle);kg_root_release(read);kg_root_release(seed);kg_root_release(graph);kg_shutdown();
    printf("canonical C shared graph: %u actual owners, foreign-root import, cycles, moving aliases and detach PASS\n",count);return 0;
}
