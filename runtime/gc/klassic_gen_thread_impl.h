/* Implementation include, once in runtime.c. No raw managed reference crosses
 * an OS wait: startup owns a registered parent RootCell until the real child
 * imports it, and callbacks own their own borrowed frame root. The native job
 * remains owned through an actual kernel join, even after HS detach. */
#ifndef KLASSIC_GEN_THREAD_IMPL_H
#define KLASSIC_GEN_THREAD_IMPL_H
#include "klassic_gen_thread.h"
#include "klassic_gen_thread_result.h"
typedef struct KgThreadJob {
    MetadataCharge control;
    struct KgThreadJob *previous,*next;
    KgRuntime *runtime;
    KgManagedCallback callback;
    KgManagedResultCallback result_callback;
    void *userdata;
    RootCell *startup_root;
    uint64_t identity,child_identity,started,rejected,published,done,result,joiner;
    uint64_t managed,result_ready,result_acked,result_discard;
    RootCell *result_root;
    RootRequest result_binding;
#ifdef _WIN32
    HANDLE thread;
#else
    pthread_t thread;
#endif
} KgThreadJob;
static KgThreadJob *kg_thread_jobs;
static uint64_t kg_thread_serial;
static uint64_t kg_result_shutdown_requested;
static _Thread_local uint64_t kg_callback_depth;
bool kg_managed_callback_active(void){return kg_callback_depth!=0;}
static uint64_t kgt_state(KgRuntime *r){
    MutatorContext *ctx=kg_context(r);uint64_t state=atomic_load_explicit(&ctx->state,memory_order_acquire);
    if((state!=GC_RUNNING&&state!=GC_BLOCKED_NATIVE)||atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||
       atomic_load_explicit(&ctx->private_producers,memory_order_acquire))kg_fail("callback or launch boundary still has managed work",state);
    return state;
}
uint64_t kg_managed_callback(KgManagedCallback callback,const RootCell *argument,void *userdata){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);if(!callback)kg_fail("null managed callback",0);
    bool native=kgt_state(r)==GC_BLOCKED_NATIVE;if(native)kg_leave_native(r);
    RootCell *borrowed=kg_root_new();if(argument)kg_root_copy(borrowed,argument);
    kg_lock(r);RootRequest binding=kg_root_request_locked(r,borrowed);kg_unlock(r);
    if(kg_callback_depth==UINT64_MAX)kg_fail("managed callback depth exhausted",0);kg_callback_depth++;
    uint64_t result=callback(borrowed,userdata);
    if(kgt_state(r)==GC_BLOCKED_NATIVE)kg_leave_native(r);
    kg_lock(r);uint64_t status=gc_root_step(kg_context(r),&binding,GC_ROOT_READ_RAW);binding.value=0;kg_unlock(r);
    if(status)kg_fail("managed callback borrowed root lifetime",status);
    kg_root_release(borrowed);kg_callback_depth--;if(native)kg_enter_native(r);return result;
}
#include "klassic_gen_thread_result_callback.h"
static void kgt_wait_locked(KgRuntime *r){
#ifdef _WIN32
    if(!SleepConditionVariableSRW(&r->changed,&r->lock,INFINITE,0))kg_fail("thread acknowledgement wait",GetLastError());
#else
    int status=pthread_cond_wait(&r->changed,&r->lock);if(status)kg_fail("thread acknowledgement wait",(uint64_t)status);
#endif
}
static void kgt_body(KgThreadJob *job){
    KgRuntime *r=job->runtime;
#ifdef KG_THREAD_PRE_ATTACH_HOOK
    KG_THREAD_PRE_ATTACH_HOOK(job);
#endif
    if(!kg_try_thread_attach()){
        kg_lock(r);job->rejected=job->done=1;kg_notify(r);kg_unlock(r);return;
    }
    RootCell *argument=kg_root_new();kg_root_import(argument,job->startup_root);
    kg_lock(r);job->child_identity=kg_context(r)->thread_id;job->started=1;kg_notify(r);kg_unlock(r);
    uint64_t result=0;RootCell *managed_result=NULL;
    if(job->managed){managed_result=kg_root_new();kg_managed_callback_into(job->result_callback,argument,managed_result,job->userdata);}
    else result=kg_managed_callback(job->callback,argument,job->userdata);
    kg_root_release(argument);
    if(job->managed){
        kg_lock(r);job->result_binding=kg_root_request_locked(r,managed_result);kg_unlock(r);kg_enter_native(r);
        kg_lock(r);job->result_root=managed_result;job->result_ready=1;
        if(kg_result_shutdown_requested&&!job->joiner){job->result_discard=job->result_acked=1;}
        kg_notify(r);while(!job->result_acked)kgt_wait_locked(r);
        job->result_root=NULL;job->result_binding=(RootRequest){0};kg_unlock(r);
        kg_leave_native(r);kg_root_release(managed_result);
    }
    kg_thread_detach();
#ifdef KG_THREAD_POST_DETACH_HOOK
    KG_THREAD_POST_DETACH_HOOK(job);
#endif
    /* The job belongs to the runtime until kernel join. Shutdown retains its
     * mutex/control even though this OS thread has now left HS membership. */
    kg_lock(r);job->result=result;job->done=1;kg_notify(r);kg_unlock(r);
}
#ifdef _WIN32
static DWORD WINAPI kgt_entry(LPVOID argument){kgt_body(argument);return 0;}
#else
static void *kgt_entry(void *argument){kgt_body(argument);return NULL;}
#endif
static bool kgt_os_create(KgThreadJob *job){
#ifdef _WIN32
    job->thread=CreateThread(NULL,0,kgt_entry,job,0,NULL);return job->thread!=NULL;
#else
    return !pthread_create(&job->thread,NULL,kgt_entry,job);
#endif
}
static bool kgt_os_join(KgThreadJob *job){
#ifdef _WIN32
    if(WaitForSingleObject(job->thread,INFINITE)!=WAIT_OBJECT_0||!CloseHandle(job->thread))return false;
    job->thread=NULL;return true;
#else
    if(pthread_join(job->thread,NULL))return false;memset(&job->thread,0,sizeof(job->thread));return true;
#endif
}
/* False leaves no created child, or leaves the joinable OS handle owned and
 * usable respectively. Post-effect false is outside these native contracts. */
static bool (*kg_thread_create_native)(KgThreadJob *)=kgt_os_create;
static bool (*kg_thread_join_native)(KgThreadJob *)=kgt_os_join;
static void kgt_join_native_retry(KgThreadJob *job){
    uint64_t failures=0;while(!kg_thread_join_native(job))kg_os_retry(&failures,"OS mutator join retry limit");
}
static void kgt_remove_locked(KgThreadJob *job){
    if(job->previous)job->previous->next=job->next;else kg_thread_jobs=job->next;
    if(job->next)job->next->previous=job->previous;
}
static void kgt_dispose_native(KgRuntime *r,KgThreadJob *job){
    KgMetadataRetire retire={0};uint64_t status;
    do{status=kg_metadata_retire_step_native(r,retire.charge.stage?NULL:&job->control,&retire,kg_context(r));}while(status==GC_NATIVE_METADATA_OS_ERROR);
    if(status)kg_fail("joined mutator control retirement",status);
}
static KgMutatorThread *kgt_start(KgManagedCallback callback,KgManagedResultCallback managed,const RootCell *argument,void *userdata){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);if(!callback&&!managed)kg_fail("null mutator callback",0);
    bool native=kgt_state(r)==GC_BLOCKED_NATIVE;if(native)kg_leave_native(r);
    kg_lock(r);bool closed=r->admission_closed;kg_unlock(r);if(closed){if(native)kg_enter_native(r);return NULL;}
    RootCell *startup=kg_root_new();if(argument)kg_root_copy(startup,argument);
    MetadataCharge control={0};KgThreadJob *job=kg_control_map(r,sizeof(*job),&control);
    job->control=control;job->runtime=r;job->callback=callback;job->result_callback=managed;job->managed=managed!=NULL;job->userdata=userdata;job->startup_root=startup;
    kg_lock(r);if(kg_thread_serial==UINT64_MAX)kg_fail("thread handle identity exhausted",0);
    job->identity=++kg_thread_serial;job->next=kg_thread_jobs;if(job->next)job->next->previous=job;kg_thread_jobs=job;kg_unlock(r);
    kg_enter_native(r);uint64_t failures=0;
    while(!kg_thread_create_native(job))kg_os_retry(&failures,"OS mutator creation retry limit");
    kg_lock(r);while(!job->started&&!job->rejected)kgt_wait_locked(r);bool rejected=job->rejected;kg_unlock(r);
    if(rejected)kgt_join_native_retry(job);
    kg_leave_native(r);kg_root_release(startup);kg_lock(r);job->startup_root=NULL;job->published=1;
    uint64_t identity=job->identity;if(rejected)kgt_remove_locked(job);kg_notify(r);kg_unlock(r);
    if(rejected){kg_enter_native(r);kgt_dispose_native(r,job);kg_leave_native(r);}
    if(native)kg_enter_native(r);return rejected?NULL:(KgMutatorThread *)(uintptr_t)identity;
}
KgMutatorThread *kg_thread_start(KgManagedCallback callback,const RootCell *argument,void *userdata){return kgt_start(callback,NULL,argument,userdata);}
KgMutatorThread *kg_thread_start_managed(KgManagedResultCallback callback,const RootCell *argument,void *userdata){return kgt_start(NULL,callback,argument,userdata);}
static KgThreadJob *kgt_find_locked(KgRuntime *r,const KgMutatorThread *thread){
    for(KgThreadJob *job=kg_thread_jobs;job;job=job->next)if(job->identity==(uintptr_t)thread){
        if(job->runtime!=r||!job->published||job->startup_root)kg_fail("mutator launch publication",0);return job;
    }
    kg_fail("unknown or joined mutator handle",0);
}
uint64_t kg_thread_join(KgMutatorThread *thread){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);bool native=kgt_state(r)==GC_BLOCKED_NATIVE;
    kg_lock(r);KgThreadJob *job=kgt_find_locked(r,thread);uint64_t identity=kg_context(r)->thread_id;
    if(job->managed)kg_fail("managed result requires rooted join",0);
    if(job->child_identity==identity)kg_fail("mutator cannot join itself",0);
    if(job->joiner)kg_fail("mutator join already owned",0);job->joiner=identity;kg_unlock(r);
    if(!native)kg_enter_native(r);kgt_join_native_retry(job);
    kg_lock(r);if(!job->done)kg_fail("kernel-joined mutator missing completion",0);uint64_t result=job->result;kgt_remove_locked(job);kg_unlock(r);
    kgt_dispose_native(r,job);if(!native)kg_leave_native(r);return result;
}
static void kgt_result_binding_locked(KgRuntime *r,KgThreadJob *job){
    if(!job->managed||!job->result_ready||job->result_acked||job->result_discard||!job->result_root||
       job->result_binding.handle.cell!=(uintptr_t)job->result_root||job->result_binding.value)
        kg_fail("managed result ownership receipt",0);
    RootRequest *binding=&job->result_binding;
    for(KgMutator *m=r->mutators;m;m=m->next)if(m->context->thread_id==job->child_identity){
        if(m->state!=KG_MUTATOR_REGISTERED||!m->participant->registered||atomic_load(&m->context->state)!=GC_BLOCKED_NATIVE||
           atomic_load(&m->context->access_epoch)||atomic_load(&m->context->private_producers))kg_fail("managed result publisher quiescence",0);
        for(KgRootBlock *b=m->root_blocks;b;b=b->next)if(binding->segment==(uintptr_t)&b->roots->segment){
            RootSegment *segment=&b->roots->segment;RootRegistration *registration=&b->roots->registration;
            uint64_t address=(uintptr_t)job->result_root,used=atomic_load(&segment->used);
            if(!gc_root_segment_valid(segment)||segment->owner!=job->child_identity||segment->scope!=binding->handle.scope||
               atomic_load(&segment->state)!=GC_ROOT_SEGMENT_ACTIVE||registration->registered!=1||
               registration->segment!=(uintptr_t)segment||segment->registration!=(uintptr_t)registration||used>segment->capacity||
               registration->context!=(uintptr_t)m->context||registration->scope!=segment->scope||
               address<segment->cells||address-segment->cells>=used*sizeof(RootCell)||(address-segment->cells)%sizeof(RootCell)||
               !job->result_root->active||job->result_root->reuse_generation!=binding->handle.reuse_generation)
                kg_fail("managed result root binding",0);
            return;
        }
        kg_fail("managed result root storage lifetime",0);
    }
    kg_fail("managed result publisher registration",0);
}
void kg_thread_join_into(KgMutatorThread *thread,RootCell *out){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);bool native=kgt_state(r)==GC_BLOCKED_NATIVE;
    kg_lock(r);(void)kg_root_request_locked(r,out);KgThreadJob *job=kgt_find_locked(r,thread);uint64_t identity=kg_context(r)->thread_id;
    if(!job->managed)kg_fail("scalar result requires scalar join",0);
    if(job->child_identity==identity)kg_fail("mutator cannot join itself",0);
    if(job->joiner)kg_fail("mutator join already owned",0);
    if(job->result_discard||kg_result_shutdown_requested)kg_fail("managed result discarded by shutdown",0);
    job->joiner=identity;kg_unlock(r);if(!native)kg_enter_native(r);
    kg_lock(r);while(!job->result_ready)kgt_wait_locked(r);kgt_result_binding_locked(r,job);
    RootCell *source=job->result_root;kg_unlock(r);kg_leave_native(r);
    /* Import acquires the actual foreign RootLease, journals an owned output,
     * and follows it canonically before returning. The publisher cannot free
     * its context/storage until the authenticated ACK below. */
    kg_root_import(out,source);source=NULL;
    kg_lock(r);kgt_result_binding_locked(r,job);job->result_acked=1;kg_notify(r);kg_unlock(r);kg_enter_native(r);
    kgt_join_native_retry(job);kg_lock(r);
    if(!job->done||job->result_root||job->result_binding.segment)kg_fail("joined managed result cleanup",0);
    kgt_remove_locked(job);kg_unlock(r);kgt_dispose_native(r,job);
    if(!native)kg_leave_native(r);
}
void kg_threads_shutdown_request_locked(KgRuntime *r){
    if(!r->admission_closed)kg_fail("managed result discard before shutdown close",0);kg_result_shutdown_requested=1;
    for(KgThreadJob *job=kg_thread_jobs;job;job=job->next)if(job->managed&&job->result_ready&&!job->joiner){
        job->result_discard=job->result_acked=1;
    }
    kg_notify(r);
}
void kg_threads_shutdown_native(KgRuntime *r){
    if(kgt_state(r)!=GC_BLOCKED_NATIVE||!r->admission_closed||r->mutator_count!=1)kg_fail("mutator launch shutdown boundary",0);
    for(;;){kg_lock(r);KgThreadJob *job=kg_thread_jobs;
        if(!job){kg_unlock(r);return;}
        if(!job->published||job->startup_root||job->joiner)kg_fail("mutator launch still borrowed at shutdown",0);
        job->joiner=kg_context(r)->thread_id;kg_unlock(r);kgt_join_native_retry(job);
        kg_lock(r);if(!job->done)kg_fail("shutdown joined mutator missing completion",0);kgt_remove_locked(job);kg_unlock(r);kgt_dispose_native(r,job);
    }
}
#endif
