#ifndef KLASSIC_GC_PIN_DISPATCH_CASES_H
#define KLASSIC_GC_PIN_DISPATCH_CASES_H
#include "gc_pin_dispatch.h"
#include "gc_root_pin_cases.h"
#include "gc_test_threads.h"
typedef struct { GcTestMutex mutex;MutatorContext *context;unsigned held,locks,unlocks; } PinDispatchLock;
static void gc_pin_dispatch_lock(uint64_t value){
    PinDispatchLock *lock=(void *)(uintptr_t)value;
    assert(!atomic_load(&lock->context->access_epoch)&&!lock->held);
    assert(!gc_test_mutex_lock(&lock->mutex));lock->held=1;lock->locks++;
}
static void gc_pin_dispatch_unlock(uint64_t value){
    PinDispatchLock *lock=(void *)(uintptr_t)value;
    assert(lock->held);lock->held=0;lock->unlocks++;assert(!gc_test_mutex_unlock(&lock->mutex));
}
typedef uint64_t (*PinDispatcher)(const PinRuntime *,RootPin *,uint64_t);
static void gc_run_pin_dispatch_cases(PinDispatcher dispatch){
    for(unsigned mode=0;mode<6;mode++){
        RootPinFixture f;gc_root_pin_fixture(&f);
        PinDispatchLock lock={.context=&f.ctx};assert(!gc_test_mutex_init(&lock.mutex,NULL));
        PinRuntime runtime={&f.registry,(uintptr_t)&lock,gc_pin_dispatch_lock,gc_pin_dispatch_unlock};
        assert(gc_root_pin_prepare(&f.registry,&f.ctx,&f.input,&f.output,&f.pin)==GC_RPIN_MORE);
        if(mode==0){
            atomic_store(&f.forwards[0].word,5);
            assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_WAIT);
            assert(!atomic_load(&f.ctx.access_epoch)&&!lock.held);
            atomic_store(&f.forwards[0].word,0);
        }
        if(mode==1){
            atomic_store(&f.ctx.state,GC_BLOCKED_NATIVE);
            assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_RESUME&&!lock.locks);
            atomic_store(&f.ctx.state,GC_RUNNING);
        }
        if(mode==5){
            RelocationGateRequest selection={0};MetadataLeaseRequest lease=f.records[0].lifetime;
            assert(gc_pin_registry_step_locked(&f.registry,&lease,&selection,GC_PP_SELECT)==GC_RG_OK);
            assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_GUARDED);
            assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_CLEANUP);
            assert(!atomic_load(&f.ctx.access_epoch)&&!lock.held);
            assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_WAIT);
            assert(gc_pin_registry_step_locked(&f.registry,&lease,&selection,GC_PP_REOPEN)==GC_RG_OK);
        }
        assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_GUARDED);
        unsigned acquired=lock.locks;
        assert(dispatch(&runtime,&f.pin,mode==2?GC_PIN_CANCEL:GC_PIN_STEP)==
            (mode==2?GC_RPIN_MORE:GC_RPIN_READY));
        assert(lock.locks==acquired&&!atomic_load(&f.ctx.access_epoch));
        if(mode==0){
            atomic_store(&f.ctx.state,GC_DETACHING);
            assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_INVALID);
            atomic_store(&f.ctx.state,GC_RUNNING);
        }
        if(mode==3)atomic_store(&f.ctx.state,GC_BLOCKED_NATIVE);
        if(mode==4){f.journal.top=8;atomic_store(&f.journal.published,8);}
        PinRuntime wrong=runtime;wrong.pages=NULL;
        assert(dispatch(&wrong,&f.pin,GC_PIN_CANCEL)==GC_RPIN_INVALID);
        assert(dispatch(&runtime,&f.pin,GC_PIN_CANCEL)==GC_RPIN_MORE);
        assert(f.pin.cancel&&!f.pin.address&&!lock.held);
        if(mode==3){
            assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_RESUME);
            atomic_store(&f.ctx.state,GC_RUNNING);
        }
        if(mode==4){
            assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_FLUSH);
            assert(f.cells[1].active&&!atomic_load(&f.ctx.access_epoch));
            atomic_store(&f.journal.consumed,8);
        }
        assert(dispatch(&runtime,&f.pin,GC_PIN_STEP)==GC_RPIN_DONE);
        assert(dispatch(&runtime,&f.pin,GC_PIN_CANCEL)==GC_RPIN_DONE);
        assert(lock.locks==lock.unlocks&&!lock.held&&f.pool.active==2);
        for(unsigned i=0;i<2;i++){
            MetadataLeaseRequest lease=f.records[i].lifetime;
            assert(gc_pin_registry_unbind_locked(&f.registry,&lease)==GC_RG_OK);
        }
        assert(!gc_test_mutex_destroy(&lock.mutex));
    }
    puts("pin dispatch: native mutex ownership, guard-only exit, sticky cancellation and resume/flush retries PASS");
}
#endif
