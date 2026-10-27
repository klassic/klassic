/* Public OS launch/import/callback/kernel-join lifetime, actual shared runtime. */
static void thread_pre_attach(void *);
static void thread_post_detach(void *);
static void thread_admission_closed(void *);
#define KG_THREAD_PRE_ATTACH_HOOK(job) thread_pre_attach(job)
#define KG_THREAD_POST_DETACH_HOOK(job) thread_post_detach(job)
#define KG_RUNTIME_ADMISSION_CLOSED_HOOK(r) thread_admission_closed(r)
#include "klassic_gen_runtime.c"
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
static _Atomic unsigned block_attach,attach_entered,release_attach,block_tail,tail_entered,release_tail,shutdown_closed,join_entered;
static void thread_pre_attach(void *job){
    (void)job;if(!atomic_exchange(&block_attach,0))return;
    atomic_store(&attach_entered,1);while(!atomic_load(&release_attach))kg_yield();
}
static void thread_post_detach(void *job){
    (void)job;if(!atomic_exchange(&block_tail,0))return;
    assert(!kg_current_context(&kg_runtime));atomic_store(&tail_entered,1);
    while(!atomic_load(&release_tail))kg_yield();
}
static void thread_admission_closed(void *runtime){(void)runtime;atomic_store(&shutdown_closed,1);}
static void thread_barrier(pthread_barrier_t *b){int s=pthread_barrier_wait(b);assert(!s||s==PTHREAD_BARRIER_SERIAL_THREAD);}
static void complete_shutdown(void){kg_shutdown();assert(!kg_thread_jobs&&!kg_runtime.native.backing.heap.metadata_committed&&!kg_runtime.native.backing.heap.metadata_pending);}
static RootCell *launch_argument(void){
    RootCell *record=kg_root_new(),*leaf=kg_root_new();kg_alloc_into(8,1,leaf);kg_store_i64(leaf,0,41);
    kg_alloc_into(16,2,record);kg_store_ref(record,0,leaf);kg_store_ref(record,8,record);kg_root_release(leaf);return record;
}
static uint64_t argument_value(const RootCell *argument){
    RootCell *leaf=kg_root_new();kg_load_ref_into(argument,0,leaf);uint64_t value=kg_load_i64(leaf,0);kg_root_release(leaf);return value;
}
static uint64_t nested_callback(const RootCell *argument,void *unused){
    (void)unused;assert(kg_callback_depth==2);uint64_t value=argument_value(argument);kg_native_enter();return value+1;
}
static uint64_t outer_callback(const RootCell *argument,void *userdata){
    assert(kg_callback_depth==1);kg_root_clear(userdata);
    for(unsigned i=0;i<4;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    kg_native_enter();uint64_t result=kg_managed_callback(nested_callback,argument,NULL);
    assert(atomic_load(&kg_current_context(&kg_runtime)->state)==GC_BLOCKED_NATIVE);
    return result+1; /* Deliberate Native return: the owner restores the caller. */
}
static void callback_lifetime(void){
    RootCell *argument=launch_argument();kg_native_enter();uint64_t result=kg_managed_callback(outer_callback,argument,argument);
    assert(result==43&&!kg_callback_depth&&atomic_load(&kg_current_context(&kg_runtime)->state)==GC_BLOCKED_NATIVE);
    kg_native_exit();kg_lock(&kg_runtime);assert(!atomic_load(&argument->value));kg_unlock(&kg_runtime);
    kg_root_release(argument);complete_shutdown();puts("public callback: independent root, nested Native return, movement and caller-state restoration PASS");
}
typedef struct {pthread_barrier_t *ready,*release;unsigned identity;} LaunchJob;
static uint64_t launched_callback(const RootCell *argument,void *userdata){
    LaunchJob *job=userdata;assert(kg_callback_depth==1);kg_native_enter();thread_barrier(job->ready);thread_barrier(job->release);kg_native_exit();
    uint64_t value=argument_value(argument);assert(value==41);
    RootCell *temp=kg_root_new();kg_alloc_into(8,1,temp);kg_store_i64(temp,0,99);assert(kg_load_i64(temp,0)==99);kg_root_release(temp);
    kg_native_enter();return value+job->identity;
}
static void launch_owners(unsigned count){
    RootCell *argument=launch_argument();pthread_barrier_t ready,release;KgMutatorThread *threads[8];LaunchJob jobs[8];
    assert(!pthread_barrier_init(&ready,NULL,count+1)&&!pthread_barrier_init(&release,NULL,count+1));
    for(unsigned i=0;i<count;i++){jobs[i]=(LaunchJob){&ready,&release,i};threads[i]=kg_thread_start(launched_callback,argument,&jobs[i]);assert(threads[i]);}
    kg_root_release(argument);kg_native_enter();thread_barrier(&ready);kg_native_exit();
    for(unsigned i=0;i<8;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    kg_native_enter();thread_barrier(&release);
    for(unsigned i=0;i<count;i++){assert(kg_thread_join(threads[i])==41+i);assert(atomic_load(&kg_current_context(&kg_runtime)->state)==GC_BLOCKED_NATIVE);}
    kg_native_exit();assert(!pthread_barrier_destroy(&ready)&&!pthread_barrier_destroy(&release));complete_shutdown();
    printf("public launch: %u actual child imports survive parent-root release, Young/Old moves, Native callbacks and kernel joins PASS\n",count);
}
static uint64_t simple_callback(const RootCell *argument,void *unused){(void)unused;return argument_value(argument);}
static bool (*real_thread_create)(KgThreadJob *),(*real_thread_join)(KgThreadJob *);
static unsigned fail_create,fail_join,fail_control_unmap;
static NativeMetadataOps real_thread_metadata;
static uintptr_t control_unmap_target;
static bool create_failure(KgThreadJob *job){
    assert(atomic_load(&kg_current_context(&kg_runtime)->state)==GC_BLOCKED_NATIVE&&job->control.stage==2&&job->startup_root&&job->startup_root->active);
    if(fail_create){fail_create--;return false;}return real_thread_create(job);
}
static bool join_failure(KgThreadJob *job){
    assert(atomic_load(&kg_current_context(&kg_runtime)->state)==GC_BLOCKED_NATIVE&&job->control.stage==2);
    atomic_store(&join_entered,1);if(fail_join){fail_join--;return false;}return real_thread_join(job);
}
static bool thread_control_unmap(void *address,size_t bytes){
    if((uintptr_t)address==control_unmap_target&&fail_control_unmap){fail_control_unmap--;return false;}
    return real_thread_metadata.unmap(address,bytes);
}
static void launch_failures(void){
    RootCell *argument=launch_argument();real_thread_create=kg_thread_create_native;real_thread_join=kg_thread_join_native;
    kg_thread_create_native=create_failure;kg_thread_join_native=join_failure;fail_create=fail_join=1;
    KgMutatorThread *thread=kg_thread_start(simple_callback,argument,NULL);assert(thread&&!fail_create);
    kg_lock(&kg_runtime);control_unmap_target=kgt_find_locked(&kg_runtime,thread)->control.allocation;kg_unlock(&kg_runtime);
    real_thread_metadata=kg_runtime.os.metadata;kg_runtime.os.metadata.unmap=thread_control_unmap;fail_control_unmap=1;
    assert(kg_thread_join(thread)==41&&!fail_join&&!fail_control_unmap);kg_root_release(argument);complete_shutdown();
    puts("public launch: actual Native creation/join/control-unmap failures retain roots and receipts through retry PASS");
}
static void *release_os_tail(void *unused){
    (void)unused;while(!atomic_load(&join_entered))kg_yield();assert(atomic_load(&shutdown_closed)&&atomic_load(&tail_entered));
    atomic_store(&release_tail,1);return NULL;
}
static void detached_os_tail(void){
    RootCell *argument=launch_argument();atomic_store(&block_tail,1);
    KgMutatorThread *thread=kg_thread_start(simple_callback,argument,NULL);assert(thread);kg_root_release(argument);
    kg_native_enter();while(!atomic_load(&tail_entered))kg_yield();kg_native_exit();
    kg_lock(&kg_runtime);assert(kg_runtime.mutator_count==1&&kg_thread_jobs&&!kg_thread_jobs->done);kg_unlock(&kg_runtime);
    real_thread_join=kg_thread_join_native;kg_thread_join_native=join_failure;
    pthread_t release;assert(!pthread_create(&release,NULL,release_os_tail,NULL));complete_shutdown();assert(!pthread_join(release,NULL));
    puts("public launch: actual detached OS tail stays owned until shutdown kernel join before mutex disposal PASS");
}
static _Atomic unsigned rejected_child_calls;
static uint64_t rejected_child(const RootCell *argument,void *unused){(void)argument;(void)unused;atomic_fetch_add(&rejected_child_calls,1);return 0;}
static uint64_t racing_parent(const RootCell *argument,void *unused){
    (void)unused;atomic_store(&block_attach,1);assert(!kg_thread_start(rejected_child,argument,NULL));return 77;
}
static void *close_release(void *unused){(void)unused;while(!atomic_load(&shutdown_closed))kg_yield();atomic_store(&release_attach,1);return NULL;}
static void close_launch_race(void){
    RootCell *argument=launch_argument();KgMutatorThread *parent=kg_thread_start(racing_parent,argument,NULL);assert(parent);kg_root_release(argument);
    kg_native_enter();while(!atomic_load(&attach_entered))kg_yield();kg_native_exit();
    pthread_t release;assert(!pthread_create(&release,NULL,close_release,NULL));complete_shutdown();assert(!pthread_join(release,NULL));
    assert(!atomic_load(&rejected_child_calls));puts("public launch: actual shutdown closes pre-attach child, imports reject, both kernel joins precede runtime disposal PASS");
}
static uintptr_t thread_reuse_address;
typedef struct {LaunchJob child;_Atomic uintptr_t handle;} DetachedParent;
static uint64_t spawning_callback(const RootCell *argument,void *userdata){
    DetachedParent *parent=userdata;KgMutatorThread *child=kg_thread_start(launched_callback,argument,&parent->child);
    assert(child);atomic_store(&parent->handle,(uintptr_t)child);return 7;
}
static void launching_parent_detaches(void){
    pthread_barrier_t ready,release;assert(!pthread_barrier_init(&ready,NULL,2)&&!pthread_barrier_init(&release,NULL,2));
    DetachedParent data={.child={&ready,&release,0}};RootCell *argument=launch_argument();
    KgMutatorThread *parent=kg_thread_start(spawning_callback,argument,&data);assert(parent);kg_root_release(argument);
    /* Real kernel join proves source ctx/rings/root mappings already retired. */
    assert(kg_thread_join(parent)==7);kg_native_enter();thread_barrier(&ready);kg_native_exit();
    kg_lock(&kg_runtime);assert(kg_runtime.mutator_count==2);kg_unlock(&kg_runtime);
    for(unsigned i=0;i<8;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    kg_native_enter();thread_barrier(&release);assert(kg_thread_join((KgMutatorThread *)atomic_load(&data.handle))==41);kg_native_exit();
    assert(!pthread_barrier_destroy(&ready)&&!pthread_barrier_destroy(&release));complete_shutdown();
    puts("public launch: source parent's real ctx and root mappings retire before child argument use and movement PASS");
}
static size_t thread_reuse_bytes;
static unsigned thread_reuse_mapped;
static void *thread_reuse_map(size_t bytes){
    if(!thread_reuse_mapped&&bytes==thread_reuse_bytes){
        void *address=mmap((void *)thread_reuse_address,bytes,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE,-1,0);
        assert(address!=MAP_FAILED&&(uintptr_t)address==thread_reuse_address);thread_reuse_mapped=1;return address;
    }
    return real_thread_metadata.map(bytes);
}
static void thread_identity(bool stale){
    RootCell *argument=launch_argument();KgMutatorThread *old=kg_thread_start(simple_callback,argument,NULL);assert(old);
    kg_lock(&kg_runtime);KgThreadJob *control=kgt_find_locked(&kg_runtime,old);
    thread_reuse_address=control->control.allocation;thread_reuse_bytes=(size_t)control->control.bytes;kg_unlock(&kg_runtime);
    assert(kg_thread_join(old)==41);real_thread_metadata=kg_runtime.os.metadata;kg_runtime.os.metadata.map=thread_reuse_map;
    KgMutatorThread *fresh=kg_thread_start(simple_callback,argument,NULL);kg_runtime.os.metadata.map=real_thread_metadata.map;
    kg_lock(&kg_runtime);assert(fresh&&old!=fresh&&thread_reuse_mapped&&kgt_find_locked(&kg_runtime,fresh)->control.allocation==thread_reuse_address);kg_unlock(&kg_runtime);
    if(stale){fputs("actual joined OS control address reused with distinct launch serial\n",stderr);(void)kg_thread_join(old);abort();}
    assert(kg_thread_join(fresh)==41);kg_root_release(argument);complete_shutdown();
    puts("public launch: actual joined control address reused, new serial joins once and all backing returns PASS");
}
static uint64_t forbidden_detach(const RootCell *argument,void *unused){(void)argument;(void)unused;kg_thread_detach();abort();}
static uint64_t forbidden_shutdown(const RootCell *argument,void *unused){(void)argument;(void)unused;kg_shutdown();abort();}
static uint64_t forbidden_root_release(const RootCell *argument,void *unused){(void)unused;kg_root_release((RootCell *)argument);return 0;}
static _Atomic uintptr_t self_handle;
static uint64_t forbidden_self_join(const RootCell *argument,void *unused){
    (void)argument;(void)unused;kg_native_enter();while(!atomic_load(&self_handle))kg_yield();kg_native_exit();
    (void)kg_thread_join((KgMutatorThread *)atomic_load(&self_handle));abort();
}
static bool permanent_create_failure(KgThreadJob *job){
    assert(job->control.stage==2&&job->startup_root&&job->startup_root->active&&
        atomic_load(&kg_current_context(&kg_runtime)->state)==GC_BLOCKED_NATIVE);return false;
}
static void rejected_lifetime(const char *mode){
    RootCell *argument=launch_argument();
    if(!strcmp(mode,"detach"))(void)kg_thread_start(forbidden_detach,argument,NULL);
    else if(!strcmp(mode,"shutdown"))(void)kg_managed_callback(forbidden_shutdown,argument,NULL);
    else if(!strcmp(mode,"borrowed"))(void)kg_managed_callback(forbidden_root_release,argument,NULL);
    else if(!strcmp(mode,"create-permanent")){kg_thread_create_native=permanent_create_failure;(void)kg_thread_start(simple_callback,argument,NULL);}
    else if(!strcmp(mode,"self")){
        KgMutatorThread *thread=kg_thread_start(forbidden_self_join,argument,NULL);atomic_store(&self_handle,(uintptr_t)thread);
        kg_native_enter();for(;;)kg_yield();
    }
    kg_native_enter();for(;;)kg_yield();
}
int main(int argc,char **argv){
    kg_configure(8*1024*1024,0,4*1024*1024,4);kg_init();const char *mode=argc>1?argv[1]:"callback";
    if(!strcmp(mode,"2")||!strcmp(mode,"8"))launch_owners((unsigned)strtoul(mode,NULL,10));
    else if(!strcmp(mode,"failures"))launch_failures();
    else if(!strcmp(mode,"tail"))detached_os_tail();
    else if(!strcmp(mode,"close"))close_launch_race();
    else if(!strcmp(mode,"parent-detach"))launching_parent_detaches();
    else if(!strcmp(mode,"identity")||!strcmp(mode,"reuse"))thread_identity(!strcmp(mode,"reuse"));
    else if(!strcmp(mode,"detach")||!strcmp(mode,"shutdown")||!strcmp(mode,"borrowed")||!strcmp(mode,"self")||!strcmp(mode,"create-permanent"))rejected_lifetime(mode);
    else callback_lifetime();return 0;
}
