#ifndef KLASSIC_GC_NATIVE_BUFFER_QUIESCENCE_CASES_H
#define KLASSIC_GC_NATIVE_BUFFER_QUIESCENCE_CASES_H
#include "gc_native_buffer_quiescence.h"
#include "gc_buffer_job_cases.h"
#include "gc_test_threads.h"
typedef struct {
    MutatorContext ctx;
    HandshakeState handshake;
    ThreadRegistration registration;
    BarrierEntry entries[2][256],output[256];
    BarrierJob job;
    BufferJobStep step;
} NativeBufferQuiescenceFixture;
static void *gc_nbq_copy(void *arg){
    NativeBufferQuiescenceFixture *f=arg;
    assert(f->step(&f->ctx,&f->job,GC_BJ_COPY)==GC_BJ_OK);
    return NULL;
}
static uint64_t gc_nbq_check(NativeBufferQuiescenceFixture *f){
    return gc_native_buffer_quiescence_locked(&f->handshake,&f->registration);
}
static void gc_run_native_buffer_quiescence_cases(BufferJobStep step){
    for(unsigned selector=0;selector<2;selector++){
        NativeBufferQuiescenceFixture f={.ctx={.abi_version=GC_ABI_VERSION},.step=step};
        f.ctx.satb=(BufferCursor){.entries=(uintptr_t)f.entries[0],.capacity=256};
        f.ctx.remembered=(BufferCursor){.entries=(uintptr_t)f.entries[1],.capacity=256};
        f.registration.context=(uintptr_t)&f.ctx;
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_REGISTER)==GC_HS_OK);
        assert(gc_nbq_check(&f)==GC_NBQ_WAIT); /* Empty Running is not an owner handoff. */
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_ENTER_NATIVE)==GC_HS_OK);
        assert(gc_nbq_check(&f)==GC_NBQ_DONE);
        /* Defensive rejection of inconsistent stopped-owner state. */
        atomic_store(&f.ctx.private_producers,1);assert(gc_nbq_check(&f)==GC_NBQ_WAIT);
        atomic_store(&f.ctx.private_producers,0);
        atomic_store(&f.ctx.access_epoch,1);assert(gc_nbq_check(&f)==GC_NBQ_WAIT);
        atomic_store(&f.ctx.access_epoch,0);
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_LEAVE_NATIVE)==GC_HS_OK);
        BarrierEntry entry={0}; /* No invented metadata lease or managed reference. */
        for(unsigned i=0;i<256;i++)assert(gc_barrier_buffer_step(&f.ctx,&entry,selector)==GC_BB_OK);
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_ENTER_NATIVE)==GC_HS_OK);
        BufferCursor *b=selector?&f.ctx.remembered:&f.ctx.satb;
        assert(!atomic_load(&b->published)&&!atomic_load(&b->consumed));
        assert(gc_nbq_check(&f)==GC_NBQ_WAIT); /* Private prefix is still pending. */
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_LEAVE_NATIVE)==GC_HS_OK);
        assert(gc_barrier_buffer_step(&f.ctx,NULL,selector+2)==GC_BB_OK);
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_ENTER_NATIVE)==GC_HS_OK);
        assert(gc_nbq_check(&f)==GC_NBQ_WAIT);
        f.job=(BarrierJob){.identity=f.ctx.thread_id,.selector=selector,.output=(uintptr_t)f.output,.capacity=256};
        assert(step(&f.ctx,&f.job,GC_BJ_CLAIM)==GC_BJ_OK);
        GcTestThread copier;assert(!gc_test_thread_create(&copier,NULL,gc_nbq_copy,&f));
        /* COPY owns cursor outside the registry lock. The checker only reads
         * serial (unchanged until ACK), so these concurrent calls are valid. */
        for(unsigned i=0;i<4096;i++)assert(gc_nbq_check(&f)==GC_NBQ_WAIT);
        assert(!gc_test_thread_join(copier,NULL));
        assert(gc_nbq_check(&f)==GC_NBQ_WAIT);
        assert(f.job.written==256&&!atomic_load(&b->consumed));
        /* All entries are null and token-free; complete processing is trivial. */
        assert(step(&f.ctx,&f.job,GC_BJ_ACK)==GC_BJ_OK);
        assert(gc_nbq_check(&f)==GC_NBQ_DONE);
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_LEAVE_NATIVE)==GC_HS_OK);
        assert(gc_nbq_check(&f)==GC_NBQ_WAIT);
        assert(gc_handshake_step_locked(&f.handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_ACK)==GC_HS_OK);
        assert(gc_nbq_check(&f)==GC_NBQ_DONE);
        assert(gc_handshake_step_locked(&f.handshake,NULL,GC_HS_END)==GC_HS_OK);
        assert(gc_nbq_check(&f)==GC_NBQ_WAIT); /* An ended handshake grants no receipt. */
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_RESUME)==GC_HS_OK);
        assert(gc_handshake_step_locked(&f.handshake,&f.registration,GC_HS_ENTER_NATIVE)==GC_HS_OK);
        uint64_t identity=f.registration.identity;f.registration.identity++;
        assert(gc_nbq_check(&f)==GC_NBQ_INVALID);f.registration.identity=identity;
        uint64_t published=atomic_load(&b->published);atomic_store(&b->published,b->top+1);
        assert(gc_nbq_check(&f)==GC_NBQ_INVALID);atomic_store(&b->published,published);
        BufferConsumer *c=selector?&f.ctx.remembered_consumer:&f.ctx.satb_consumer;
        c->cursor=1;assert(gc_nbq_check(&f)==GC_NBQ_INVALID);c->cursor=0;
        assert(gc_nbq_check(&f)==GC_NBQ_DONE);
    }
    puts("native buffer quiescence: private prefixes and concurrent consumer copies require owner handoff and ACK PASS");
}
#endif
