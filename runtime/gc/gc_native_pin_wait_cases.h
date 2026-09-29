#ifndef KLASSIC_GC_NATIVE_PIN_WAIT_CASES_H
#define KLASSIC_GC_NATIVE_PIN_WAIT_CASES_H
#include "gc_native_pin_wait.h"
#include "gc_root_finish_notify.h"
#include "gc_native_pin_progress.h"
#include <pthread.h>
#include <sched.h>
#include <linux/futex.h>
#include <sys/syscall.h>
#include <errno.h>
#include <assert.h>
#include <stdio.h>
typedef uint64_t (*RootFinishNotifier)(RootRegistry *,RootLifeRequest *,RootFinishNotification *);
typedef uint64_t (*NativePinWaitArm)(const NativePinRuntime *,NativePin *,_Atomic uint32_t *);
typedef int64_t (*PinEventNotify)(_Atomic uint32_t *);
typedef int64_t (*PinEventPark)(_Atomic uint32_t *,uint64_t);
static inline int64_t gc_pin_event_notify(_Atomic uint32_t *event){
    atomic_store_explicit(event,1,memory_order_release);
    return syscall(SYS_futex,event,FUTEX_WAKE_PRIVATE,1,NULL,NULL,0)<0?-errno:0;
}
static inline int64_t gc_pin_event_park(_Atomic uint32_t *event,uint64_t expected){
    return syscall(SYS_futex,event,FUTEX_WAIT_PRIVATE,(uint32_t)expected,NULL,NULL,0)<0?-errno:0;
}
static inline int64_t gc_pin_notify_fail(_Atomic uint32_t *event){(void)event;return -EIO;}
static void gc_pin_wait_lock(uint64_t p){assert(!pthread_mutex_lock((void *)(uintptr_t)p));}
static void gc_pin_wait_unlock(uint64_t p){assert(!pthread_mutex_unlock((void *)(uintptr_t)p));}
typedef struct {
    RootRegistry roots;MutatorContext ctx;pthread_mutex_t mutex;
    RootLease reader,cleaner;RootLifeRequest reading,cleaning;
    struct { _Atomic uint32_t word;uint32_t sentinel; } event;
    _Atomic unsigned go,notified;PinEventNotify notify;
    RootFinishNotifier finish;RootFinishNotification notice;
} NativePinWaitFixture;
static void *gc_pin_wait_collector(void *arg){
    NativePinWaitFixture *f=arg;
    while(!atomic_load_explicit(&f->go,memory_order_acquire))sched_yield();
    for(unsigned i=0;i<32;i++)sched_yield();
    assert(!pthread_mutex_lock(&f->mutex));
    assert(gc_root_life_locked(&f->roots,&f->reading,GC_RL_RELEASE)==GC_RL_OK);
    assert(gc_root_life_locked(&f->roots,&f->cleaning,GC_RL_CLEAN)==GC_RL_OK);
    assert(!pthread_mutex_unlock(&f->mutex));
    assert(gc_root_clear_chunk(&f->cleaning)==GC_RL_OK);
    assert(!pthread_mutex_lock(&f->mutex));
    f->notice.context+=8;
    assert(f->finish(&f->roots,&f->cleaning,&f->notice)==GC_RL_INVALID&&!f->notice.stage);
    f->notice.context-=8;
    uint64_t status=f->finish(&f->roots,&f->cleaning,&f->notice);
    if(status==GC_RL_NOTIFY_RETRY){
        assert(f->notice.stage==1&&f->notice.last_error==(uint64_t)-EIO);
        f->notice.notify=(uintptr_t)f->notify;
        assert(f->finish(NULL,NULL,&f->notice)==GC_RL_OK);
    }else assert(status==GC_RL_OK);
    assert(f->notice.stage==2&&!f->notice.last_error);
    assert(f->finish(NULL,NULL,&f->notice)==GC_RL_OK);
    assert(!pthread_mutex_unlock(&f->mutex));
    atomic_store_explicit(&f->notified,1,memory_order_release);return NULL;
}
static void gc_root_finish_notify_unmapped(RootFinishNotifier finish){
    NativeMetadataOps os=gc_native_metadata_ops();RootStorage *storage=os.map(os.quantum);assert(storage);
    RootStorageSpec spec={17,1,1,os.quantum};assert(gc_root_storage_prepare(storage,&spec)==GC_ROOT_STORAGE_OK);
    RootRegistry roots={.phase=0x10000};MutatorContext ctx={.abi_version=GC_ABI_VERSION,.thread_id=17,.adopted_phase=0x10000};
    RootLifeRequest life={.registration=(uintptr_t)&storage->registration,.context=(uintptr_t)&ctx};
    assert(gc_root_life_locked(&roots,&life,GC_RL_ATTACH)==GC_RL_OK);
    assert(gc_root_life_locked(&roots,&life,GC_RL_RETIRE)==GC_RL_OK);
    RootLease cleaner={0};life.lease=(uintptr_t)&cleaner;
    assert(gc_root_life_locked(&roots,&life,GC_RL_CLEAN)==GC_RL_OK);
    assert(gc_root_clear_chunk(&life)==GC_RL_OK);
    _Atomic uint32_t event=0;
    RootFinishNotification notice={.context=(uintptr_t)&ctx,.event=(uintptr_t)&event,.notify=(uintptr_t)gc_pin_notify_fail};
    assert(finish(&roots,&life,&notice)==GC_RL_NOTIFY_RETRY&&notice.stage==1);
    assert(gc_root_storage_can_dispose(storage)==GC_ROOT_STORAGE_OK);
    assert(os.unmap(storage,os.quantum));
    notice.notify=(uintptr_t)gc_pin_event_notify;
    assert(finish(NULL,NULL,&notice)==GC_RL_OK&&notice.stage==2&&atomic_load(&event)==1&&!notice.last_error);
    atomic_store(&event,0);
    assert(finish(NULL,NULL,&notice)==GC_RL_OK&&!atomic_load(&event));
    puts("root finish notification: retry after actual unmap avoids backing and duplicate wake PASS");
}
static void gc_run_native_pin_wait_cases(NativePinWaitArm arm,PinEventNotify notify,PinEventPark park,RootFinishNotifier finish){
    gc_root_finish_notify_unmapped(finish);
    NativeMetadataOps os=gc_native_metadata_ops();
    for(unsigned mode=0;mode<3;mode++)for(unsigned attempt=0;attempt<32;attempt++){
        NativePinWaitFixture f={.roots={.phase=0x10000},.ctx={.abi_version=GC_ABI_VERSION},
            .event={.word=1,.sentinel=0xa55a1234},.notify=notify,.finish=finish};
        f.notice=(RootFinishNotification){.context=(uintptr_t)&f.ctx,.event=(uintptr_t)&f.event.word,
            .notify=(uintptr_t)(attempt%2?notify:gc_pin_notify_fail)};
        assert(!pthread_mutex_init(&f.mutex,NULL));
        pthread_mutex_t heap_mutex;assert(!pthread_mutex_init(&heap_mutex,NULL));
        HandshakeState hs={.last_identity=16};ThreadRegistration participant={.context=(uintptr_t)&f.ctx};
        WorkerRequest worker={(uintptr_t)&hs,(uintptr_t)&participant};
        assert(gc_worker_step_locked(&f.roots,&worker,GC_WK_ATTACH)==GC_WK_OK);
        HeapPool heap={.page_count=1,.metadata_committed=sizeof(AllocationPage),
            .metadata_max=sizeof(AllocationPage)+os.quantum};
        MetadataCharge charge={.bytes=os.quantum};
        assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_RESERVE)==GC_BUDGET_OK);
        assert(gc_worker_step_locked(&f.roots,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        assert(gc_native_roots_prepare(&charge,17,1,1,&os,gc_root_storage_prepare)==GC_NATIVE_METADATA_OK);
        assert(gc_budget_step_locked(&heap,&charge,GC_BUDGET_COMMIT)==GC_BUDGET_OK);
        assert(gc_worker_step_locked(&f.roots,&worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
        PinRuntime pins={NULL,(uintptr_t)&heap_mutex,gc_pin_wait_lock,gc_pin_wait_unlock};
        NativePinRuntime runtime={&pins,&f.roots,(uintptr_t)&f.mutex,gc_pin_wait_lock,gc_pin_wait_unlock,&heap,&os};
        NativePin pin={0};NativePinStart start={(uintptr_t)&f.roots,(uintptr_t)&f.ctx,(uintptr_t)&charge};
        assert(gc_native_pin_publish_entry_locked(&pin,&start)==GC_RPIN_MORE);
        RootStorage *storage=(void *)(uintptr_t)pin.storage;
        f.reading=(RootLifeRequest){.registration=(uintptr_t)&storage->registration,
            .scope=storage->segment.scope,.lease=(uintptr_t)&f.reader};
        assert(gc_root_life_locked(&f.roots,&f.reading,GC_RL_ACQUIRE)==GC_RL_OK);
        f.cleaning=(RootLifeRequest){.registration=f.reading.registration,.scope=f.reading.scope,.lease=(uintptr_t)&f.cleaner};
        assert(gc_native_pin_retire_locked(&pin)==GC_RPIN_MORE);
        assert(arm(&runtime,&pin,&f.event.word)==GC_RPIN_INVALID&&atomic_load(&f.event.word)==1);
        assert(gc_worker_step_locked(&f.roots,&worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
        pthread_t collector;assert(!pthread_create(&collector,NULL,gc_pin_wait_collector,&f));
        if(mode==0){atomic_store_explicit(&f.go,1,memory_order_release);
            while(!atomic_load_explicit(&f.notified,memory_order_acquire))sched_yield();}
        unsigned waits=0;
        for(;;){
            assert(!pthread_mutex_lock(&f.mutex));
            uint64_t result=arm(&runtime,&pin,&f.event.word);
            assert(!pthread_mutex_unlock(&f.mutex));
            assert(f.event.sentinel==0xa55a1234);
            if(result==GC_RPIN_MORE)break;
            assert(result==GC_RPIN_WAIT&&++waits<1000);
            atomic_store_explicit(&f.go,1,memory_order_release);
            if(mode==1)while(!atomic_load_explicit(&f.notified,memory_order_acquire))sched_yield();
            int64_t result_os=park(&f.event.word,0);
            assert(result_os==0||result_os==-EAGAIN||result_os==-EINTR);
            if(mode==1)assert(result_os==-EAGAIN);
        }
        assert(mode==0?waits==0:waits>0);
        assert(gc_native_pin_dispose(&pin,&os)==GC_RPIN_DONE);
        assert(!pthread_mutex_lock(&heap_mutex));
        assert(gc_native_pin_release_locked(&pin,&heap)==GC_RPIN_DONE);
        assert(!pthread_mutex_unlock(&heap_mutex));
        assert(!pthread_join(collector,NULL));
        assert(heap.metadata_committed==sizeof(AllocationPage)&&!heap.metadata_pending);
        assert(!pthread_mutex_destroy(&f.mutex)&&!pthread_mutex_destroy(&heap_mutex));
    }
    puts("root finish notification: 96 owner-bound completions, 48 failed-wake retries and terminal replay PASS");
    puts("native pin wait: 96 completion-before-arm, notify-before-park and concurrent-park lifecycles PASS");
}
#endif
