/* Reuse the actual scalar-launch OS fixture, no alternate collector. */
#define main klassic_scalar_launch_fixture_main
#include "klassic_gen_thread_test.c"
#undef main
static _Atomic unsigned result_entered,result_release;
static void managed_result(const RootCell *argument,RootCell *out,void *userdata){
    assert(argument_value(argument)==41);
    if(userdata){kg_native_enter();atomic_store(&result_entered,1);while(!atomic_load(&result_release))kg_yield();kg_native_exit();}
    RootCell *leaf=kg_root_new();kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,99);
    kg_alloc_into(16,2,out);kg_store_ref(out,0,leaf);kg_store_ref(out,8,out);kg_root_release(leaf);kg_native_enter();
}
static void verify_result(const RootCell *out){
    RootCell *leaf=kg_root_new(),*cycle=kg_root_new();kg_load_ref_into(out,0,leaf);assert(kg_load_i64(leaf,0)==99);
    kg_load_ref_into(out,8,cycle);kg_load_ref_into(cycle,0,leaf);assert(kg_load_i64(leaf,0)==99);kg_root_release(cycle);kg_root_release(leaf);
}
static void callback_result(void){
    RootCell *argument=launch_argument(),*out=kg_root_new();kg_native_enter();kg_managed_callback_into(managed_result,argument,out,NULL);
    assert(atomic_load(&kg_current_context(&kg_runtime)->state)==GC_BLOCKED_NATIVE);kg_native_exit();kg_root_release(argument);
    for(unsigned i=0;i<8;i++){kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);verify_result(out);}
    kg_root_release(out);complete_shutdown();puts("managed callback result: reserved output, Native return and cyclic result survive real movement PASS");
}
static void joined_result(void){
    KgRuntime*r=&kg_runtime;RootCell *argument=launch_argument(),*out=kg_root_new();
    KgMutatorThread *thread=kg_thread_start_managed(managed_result,argument,NULL);assert(thread);kg_root_release(argument);kg_native_enter();
    for(;;){kg_lock(r);KgThreadJob*job=kgt_find_locked(r,thread);bool ready=job->result_ready;
        if(ready){assert(job->result_root&&!job->result_acked);kgt_result_binding_locked(r,job);}kg_unlock(r);if(ready)break;kg_yield();}
    kg_native_exit();for(unsigned i=0;i<8;i++)kg_driver_pressure(r,KG_PRESSURE_HEAP);
    kg_native_enter();kg_thread_join_into(thread,out);assert(atomic_load(&kg_current_context(r)->state)==GC_BLOCKED_NATIVE);kg_native_exit();
    for(unsigned i=0;i<8;i++){kg_driver_pressure(r,KG_PRESSURE_HEAP);verify_result(out);}
    kg_lock(r);assert(r->mutator_count==1&&!kg_thread_jobs);kg_unlock(r);
    kg_root_release(out);complete_shutdown();puts("managed launch result: actual child root retained before import ACK, real detach/join and rooted parent survival PASS");
}
static void concurrent_results(unsigned count){
    KgRuntime*r=&kg_runtime;RootCell*argument=launch_argument();RootCell*out[8];KgMutatorThread*threads[8];
    for(unsigned i=0;i<count;i++){out[i]=kg_root_new();threads[i]=kg_thread_start_managed(managed_result,argument,NULL);assert(threads[i]);}
    kg_root_release(argument);kg_native_enter();
    for(;;){kg_lock(r);bool ready=true;for(unsigned i=0;i<count;i++)ready=ready&&kgt_find_locked(r,threads[i])->result_ready;kg_unlock(r);if(ready)break;kg_yield();}
    kg_native_exit();for(unsigned i=0;i<8;i++)kg_driver_pressure(r,KG_PRESSURE_HEAP);
    for(unsigned i=0;i<count;i++){kg_native_enter();kg_thread_join_into(threads[i],out[i]);kg_native_exit();verify_result(out[i]);}
    for(unsigned i=0;i<8;i++)kg_driver_pressure(r,KG_PRESSURE_HEAP);
    for(unsigned i=0;i<count;i++){verify_result(out[i]);kg_root_release(out[i]);}complete_shutdown();
    printf("managed launch result: %u actual child owners retain cyclic outputs through movement and authenticated Native joins PASS\n",count);
}
static void *release_result_on_shutdown(void *unused){(void)unused;while(!atomic_load(&shutdown_closed))kg_yield();atomic_store(&result_release,1);return NULL;}
static void shutdown_result(bool future){
    KgRuntime*r=&kg_runtime;RootCell *argument=launch_argument();KgMutatorThread*thread=kg_thread_start_managed(managed_result,argument,future?(void *)1:NULL);
    assert(thread);kg_root_release(argument);kg_native_enter();
    for(;;){kg_lock(r);KgThreadJob*job=kgt_find_locked(r,thread);bool ready=future?atomic_load(&result_entered)!=0:job->result_ready!=0;
        if(ready)assert(r->mutator_count==2&&!job->result_acked);kg_unlock(r);if(ready)break;kg_yield();}
    kg_native_exit();pthread_t release;if(future)assert(!pthread_create(&release,NULL,release_result_on_shutdown,NULL));
    complete_shutdown();if(future)assert(!pthread_join(release,NULL));
    puts(future?"managed result shutdown: discard published before future READY and membership wait, actual root release/detach/kerneljoin PASS":
        "managed result shutdown: unjoined READY root discarded with actual owner release and kerneljoin before disposal PASS");
}
typedef struct {KgMutatorThread *thread;_Atomic unsigned acquired;} ResultJoiner;
static void *result_joiner(void *argument){
    ResultJoiner *job=argument;kg_thread_attach();RootCell*out=kg_root_new();atomic_store(&job->acquired,1);
    kg_thread_join_into(job->thread,out);verify_result(out);kg_root_release(out);kg_thread_detach();return NULL;
}
static void join_shutdown_race(void){
    KgRuntime*r=&kg_runtime;RootCell*argument=launch_argument();ResultJoiner joined={.thread=kg_thread_start_managed(managed_result,argument,(void *)1)};
    assert(joined.thread);kg_root_release(argument);kg_native_enter();while(!atomic_load(&result_entered))kg_yield();
    pthread_t joiner,release;assert(!pthread_create(&joiner,NULL,result_joiner,&joined));
    for(;;){kg_lock(r);bool claimed=kgt_find_locked(r,joined.thread)->joiner!=0;kg_unlock(r);if(claimed)break;kg_yield();}
    assert(!pthread_create(&release,NULL,release_result_on_shutdown,NULL));kg_native_exit();complete_shutdown();
    assert(!pthread_join(joiner,NULL)&&!pthread_join(release,NULL));puts("managed result race: actual join claim excludes shutdown discard, rooted import ACK precedes real detach PASS");
}
static void borrowed_result_release(const RootCell *argument,RootCell *result,void *unused){(void)argument;(void)unused;kg_root_release(result);}
static void rejected_result(const char *mode){
    RootCell*argument=launch_argument(),*out=kg_root_new();
    if(!strcmp(mode,"borrowed"))kg_managed_callback_into(borrowed_result_release,argument,out,NULL);
    else if(!strcmp(mode,"scalar")){KgMutatorThread*thread=kg_thread_start(simple_callback,argument,NULL);kg_thread_join_into(thread,out);}
    else if(!strcmp(mode,"binding")){
        KgMutatorThread*thread=kg_thread_start_managed(managed_result,argument,NULL);kg_native_enter();
        for(;;){kg_lock(&kg_runtime);KgThreadJob*job=kgt_find_locked(&kg_runtime,thread);bool ready=job->result_ready;
            if(ready)job->result_binding.handle.scope++;kg_unlock(&kg_runtime);if(ready)break;kg_yield();}
        kg_thread_join_into(thread,out);
    }else{KgMutatorThread*thread=kg_thread_start_managed(managed_result,argument,NULL);(void)kg_thread_join(thread);}
    abort();
}
int main(int argc,char**argv){
    kg_configure(8*1024*1024,0,4*1024*1024,4);kg_init();const char*mode=argc>1?argv[1]:"joined";
    if(!strcmp(mode,"joined"))joined_result();else if(!strcmp(mode,"callback"))callback_result();
    else if(!strcmp(mode,"2")||!strcmp(mode,"8"))concurrent_results((unsigned)strtoul(mode,NULL,10));
    else if(!strcmp(mode,"ready"))shutdown_result(false);else if(!strcmp(mode,"future"))shutdown_result(true);
    else if(!strcmp(mode,"race"))join_shutdown_race();else rejected_result(mode);return 0;
}
