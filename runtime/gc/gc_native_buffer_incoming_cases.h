#ifndef KLASSIC_GC_NATIVE_BUFFER_INCOMING_CASES_H
#define KLASSIC_GC_NATIVE_BUFFER_INCOMING_CASES_H
#include "gc_native_allocation_cases.h"
#include <stdlib.h>
typedef struct {
    MutatorContext context;
    ThreadRegistration registration;
    WorkerRequest worker;
    BarrierEntry entries[2][2];
} NativeBufferActor;
static void gc_nbi_attach(NativeRuntimeOwner *r,NativeBufferActor *a){
    *a=(NativeBufferActor){.context={.abi_version=GC_ABI_VERSION}};
    a->context.satb=(BufferCursor){.entries=(uintptr_t)a->entries[0],.capacity=2};
    a->context.remembered=(BufferCursor){.entries=(uintptr_t)a->entries[1],.capacity=2};
    a->registration.context=(uintptr_t)&a->context;
    a->worker=(WorkerRequest){(uintptr_t)&r->handshake,(uintptr_t)&a->registration};
    assert(gc_worker_step_locked(&r->roots,&a->worker,GC_WK_ATTACH)==GC_WK_OK);
    assert(gc_worker_step_locked(&r->roots,&a->worker,GC_WK_ENTER_NATIVE)==GC_WK_OK);
}
static void gc_nbi_detach(NativeRuntimeOwner *r,NativeBufferActor *a){
    assert(gc_worker_step_locked(&r->roots,&a->worker,GC_WK_LEAVE_NATIVE)==GC_WK_OK);
    assert(gc_context_transition_locked(&a->context,GC_RUNNING,GC_DETACHING)==GC_CONTEXT_OK);
    assert(gc_handshake_step_locked(&r->handshake,&a->registration,GC_HS_DETACH)==GC_HS_OK);
}
static void gc_run_native_buffer_incoming_cases(void){
    NativeHeapOps os=gc_native_heap_ops();NativeRuntimeOwner r={0};
    gc_allocation_fixture_start(&r,&os,2,8);
    NativeBufferActor *first=malloc(sizeof(*first)),*second=malloc(sizeof(*second));assert(first&&second);
    gc_nbi_attach(&r,first);gc_nbi_attach(&r,second);
    NativeBufferIncoming q={0};
    assert(gc_native_buffer_incoming_begin_locked(&r,16,32,&q)==GC_NSI_MORE);
    NativeBufferIncoming copied=q;assert(gc_native_buffer_incoming_next_locked(&copied)==GC_NSI_INVALID);
    assert(gc_native_buffer_incoming_next_locked(&q)==GC_NSI_MORE&&q.remaining==2);
    assert(q.cursor==(uintptr_t)&first->registration);
    gc_nbi_detach(&r,first);free(first);first=NULL;
    /* q.cursor now names freed storage. Snapshot invalidation must precede any
     * borrow; ASan checks the failure order, not merely the returned status. */
    assert(gc_native_buffer_incoming_next_locked(&q)==GC_NSI_RESTART);
    q=(NativeBufferIncoming){0};
    assert(gc_native_buffer_incoming_begin_locked(&r,16,32,&q)==GC_NSI_MORE);
    assert(gc_native_buffer_incoming_next_locked(&q)==GC_NSI_MORE&&q.remaining==1);
    uint64_t cursor=q.cursor;
    assert(gc_native_buffer_incoming_next_locked(&q)==GC_NSI_WAIT&&q.cursor==cursor);
    gc_allocation_fixture_native(&r,true);
    assert(gc_native_buffer_incoming_next_locked(&q)==GC_NSI_MORE&&!q.remaining);
    assert(gc_native_buffer_incoming_next_locked(&q)==GC_NSI_DONE);
    NativeBufferIncoming before_reuse={0};
    assert(gc_native_buffer_incoming_begin_locked(&r,16,32,&before_reuse)==GC_NSI_MORE);
    uint64_t members=r.handshake.members,identity=r.handshake.last_identity;
    gc_nbi_detach(&r,second);gc_nbi_attach(&r,second); /* Exact same address and member count. */
    assert(r.handshake.members==members&&r.handshake.last_identity>identity);
    assert(gc_native_buffer_incoming_next_locked(&q)==GC_NSI_RESTART); /* Even completed audits expire. */
    assert(gc_native_buffer_incoming_next_locked(&before_reuse)==GC_NSI_RESTART);
    q=(NativeBufferIncoming){0};assert(gc_native_buffer_incoming_begin_locked(&r,16,32,&q)==GC_NSI_MORE);
    assert(gc_handshake_step_locked(&r.handshake,NULL,GC_HS_BEGIN)==GC_HS_OK);
    assert(gc_native_buffer_incoming_next_locked(&q)==GC_NSI_RESTART);
    assert(gc_handshake_step_locked(&r.handshake,NULL,GC_HS_END)==GC_HS_OK);
    gc_nbi_detach(&r,second);free(second);
    gc_allocation_fixture_native(&r,false);
    gc_allocation_fixture_roots_finish(&r);
    assert(gc_native_runtime_dispose(&r,&os)==GC_NATIVE_METADATA_OK);
    puts("native buffer inventory: bounded owner handoffs reject detached cursors and same-address registration reuse PASS");
}
#endif
