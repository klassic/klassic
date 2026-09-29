#ifndef KLASSIC_GC_BARRIER_ASSIST_CASES_H
#define KLASSIC_GC_BARRIER_ASSIST_CASES_H
#include "gc_barrier_assist.h"
#include "gc_rooted_store_dispatch_cases.h"
typedef struct {FollowFixture f;BarrierEntry entries[2];RootedStoreRuntime runtime;} AssistFixture;
static void assist_fixture(AssistFixture *a){
    memset(a,0,sizeof(*a));follow_fixture(&a->f,0x10022,1);FollowFixture *f=&a->f;
    f->page.generation=GC_YOUNG;f->page.owner=f->ctx.thread_id;atomic_store(&f->page.state,GC_PAGE_ALLOCATING);
    f->ctx.tlab_start=f->heap.heap_base+f->page.start;f->ctx.tlab_top=f->heap.heap_base+atomic_load(&f->page.allocated_top);f->ctx.tlab_end=f->heap.heap_base+f->page.limit;
    for(unsigned i=0;i<2;i++){
        MetadataLeaseRequest lease={(uintptr_t)&f->page,f->page.incarnation,0};assert(gc_metadata_step_locked(&f->pool,&lease,GC_ML_ACQUIRE)==GC_ML_OK);
        a->entries[i]=(BarrierEntry){.previous={atomic_load(&f->cell.value)},.slot={.owner={atomic_load(&f->cell.value)},.page_incarnation=f->page.incarnation,.record_epoch=0x10022,.metadata_lease=lease.token}};
    }
    f->ctx.satb=(BufferCursor){(uintptr_t)&a->entries[0],1,1,0,0};f->ctx.remembered=(BufferCursor){(uintptr_t)&a->entries[1],1,1,0,0};
    a->runtime=store_native_runtime(&f->pool,NULL);store_native_context=&f->ctx;
}
static void gc_run_barrier_assist_cases(BarrierAssister candidate,BarrierPoller poll_candidate){
    memset(store_native_locks,0,sizeof(store_native_locks));store_native_held=0;
    for(unsigned i=0;i<4;i++){
#ifdef _WIN32
        InitializeCriticalSection(&store_native_locks[i].mutex);
#else
        assert(!pthread_mutex_init(&store_native_locks[i].mutex,NULL));
#endif
    }
    for(unsigned impl=0;impl<2;impl++){
        BarrierAssister assist=impl?candidate:gc_barrier_assist;BarrierPoller poll=impl?poll_candidate:gc_barrier_poll;
        for(unsigned action=83;action<=85;action++)for(unsigned fault=0;fault<16;fault++){
            AssistFixture a;assist_fixture(&a);FollowFixture *f=&a.f;const RootedStoreRuntime *d=&a.runtime;MutatorContext *ctx=&f->ctx;
            BufferCursor *buffer=action==83?&ctx->satb:&ctx->remembered;uint64_t op=action;
            if(fault==0)ctx->abi_version++;else if(fault==1)atomic_store(&ctx->state,GC_AT_SAFEPOINT);
            else if(fault==2)atomic_store(&ctx->access_epoch,27);else if(fault==3)ctx->thread_id=0;else if(fault==4)op=82;
            else if(action==85){
                switch(fault){case 5:a.runtime.heap_pool=0;break;case 6:a.runtime.heap_lock=0;break;case 7:a.runtime.lock=0;break;
                    case 8:a.runtime.unlock=0;break;case 9:f->pool.heap=0;break;case 10:f->heap.pages=0;break;case 11:f->page.owner++;break;
                    case 12:ctx->tlab_start++;break;case 13:f->heap.page_count=0;break;case 14:f->page.run_head=1;break;case 15:f->page.generation=GC_OLD;break;}
            }else{
                switch(fault){case 5:buffer->capacity=0;break;case 6:atomic_store(&buffer->published,2);break;case 7:atomic_store(&buffer->consumed,2);break;
                    case 8:buffer->top=2;break;case 9:buffer->entries=0;break;case 10:buffer->entries++;break;case 11:buffer->entries=UINT64_MAX-7;break;
                    case 12:d=(void *)((char *)d+1);break;case 13:ctx=(void *)((char *)ctx+1);break;case 14:op=86;break;case 15:d=NULL;break;}
            }
            AssistFixture before=a;assert(assist(d,ctx,op)==GC_RLOAD_INVALID&&!memcmp(&before,&a,sizeof(a))&&!store_native_held);
        }
        for(unsigned kind=0;kind<2;kind++){
            AssistFixture a;assist_fixture(&a);FollowFixture *f=&a.f;
            RootRegistry roots={.phase=0x10022};HandshakeState hs={.members=1,.last_identity=17};
            ThreadRegistration participant={.context=(uintptr_t)&f->ctx,.identity=17,.registered=1,.registry=(uintptr_t)&hs};hs.head=(uintptr_t)&participant;
            RootedLoadAdmission admission={(uintptr_t)&roots,(uintptr_t)&hs,(uintptr_t)&participant,{0,0}};a.runtime.admission=(uintptr_t)&admission;
            RootedLoad load={.observation.receiver.follow.context=(uintptr_t)&f->ctx,.stage=GC_RLOAD_COMPLETE};
            RootedStore store={.value.observation.receiver.follow.context=(uintptr_t)&f->ctx,.stage=GC_RS_COMPLETE};void *request=kind?(void *)&store:(void *)&load;
            assert(gc_handshake_step_locked(&hs,NULL,GC_HS_BEGIN)==GC_HS_OK);
            const unsigned actions[]={GC_BP_CLOSE_TLAB,GC_BP_FLUSH_SATB,GC_BP_FLUSH_REMEMBERED};
            for(unsigned i=0;i<3;i++){
                assert(poll(&a.runtime,request,kind)==actions[i]&&!hs.acknowledged);
                unsigned locks=store_native_locks[0].acquired;
                assert(assist(&a.runtime,&f->ctx,actions[i])==GC_BP_RETRY);
                assert(store_native_locks[0].acquired==locks+(i==0));
                assert(f->pool.active==2&&f->page.metadata_leases==2&&!atomic_load(&f->ctx.satb.consumed)&&!atomic_load(&f->ctx.remembered.consumed));
            }
            assert(!f->ctx.tlab_start&&!f->ctx.tlab_top&&!f->ctx.tlab_end&&!f->page.owner&&atomic_load(&f->page.state)==GC_PAGE_CLOSED);
            assert(atomic_load(&f->ctx.satb.published)==1&&atomic_load(&f->ctx.remembered.published)==1);
            assert(poll(&a.runtime,request,kind)==GC_BP_PARK&&hs.acknowledged==1);
            assert(gc_handshake_step_locked(&hs,NULL,GC_HS_END)==GC_HS_OK);
            assert(poll(&a.runtime,request,kind)==GC_RLOAD_DONE);
            for(unsigned selector=0;selector<2;selector++){
                BarrierEntry out={0};BarrierJob job={.identity=17,.selector=selector,.output=(uintptr_t)&out,.capacity=1};
                store_native_lock(a.runtime.registry_lock);assert(gc_buffer_job_step(&f->ctx,&job,GC_BJ_CLAIM)==GC_BJ_OK);store_native_unlock(a.runtime.registry_lock);
                assert(gc_buffer_job_step(&f->ctx,&job,GC_BJ_COPY)==GC_BJ_OK&&!memcmp(&out,&a.entries[selector],sizeof(out)));
                MetadataLeaseRequest held={.token=out.slot.metadata_lease};store_native_lock(a.runtime.heap_lock);
                assert(gc_metadata_step_locked(&f->pool,&held,GC_ML_RESOLVE)==GC_ML_OK);assert(gc_metadata_step_locked(&f->pool,&held,GC_ML_RELEASE)==GC_ML_OK);store_native_unlock(a.runtime.heap_lock);
                store_native_lock(a.runtime.registry_lock);assert(gc_buffer_job_step(&f->ctx,&job,GC_BJ_ACK)==GC_BJ_OK);store_native_unlock(a.runtime.registry_lock);
            }
            assert(!f->pool.active&&!f->page.metadata_leases);
        }
    }
    for(unsigned i=0;i<4;i++){
        assert(!store_native_locks[i].held&&store_native_locks[i].acquired==store_native_locks[i].released);
#ifdef _WIN32
        DeleteCriticalSection(&store_native_locks[i].mutex);
#else
        assert(!pthread_mutex_destroy(&store_native_locks[i].mutex));
#endif
    }
    puts("barrier assist: TLAB close and complete buffer publication precede ACK without consuming leases PASS");
}
#endif
