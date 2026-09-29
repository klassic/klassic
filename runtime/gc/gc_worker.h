/* Owner-side worker rendezvous under the SAME registry lock as roots and the
 * handshake. Native controls are live/disjoint; only the OS owner calls this
 * for its participant. The coordinator publishes roots->phase and BEGIN under
 * that lock, and keeps the observable snapshot fixed until END. A fresh owner inherits it on ATTACH;
 * a Running owner adopts it with ACK; a native-blocked owner adopts on LEAVE.
 * Snapshot sequence must advance when bits change, and never wraps.
 *
 * CLOSE_TLAB/FLUSH_* require bounded work outside this lock, then retry. PARK
 * means ACK completed and no managed work may resume until POLL succeeds.
 * OK reports completion of the requested operation (ENTER leaves it native).
 * RETRY from ENTER means still Running: POLL to acknowledge before waiting.
 * RETRY from ATTACH/LEAVE preserves the unattached/native-blocked state.
 * No guard, root scan, buffer drain, allocation or wait occurs here. Root
 * mutations already publish their journal before returning. Published buffers,
 * root journals, queued/in-flight jobs and private producer work remain separate
 * phase holders: nonzero context.private_producers makes the handshake return
 * RETRY, including ACK/native entry. This is not a termination certificate.
 */
#ifndef KLASSIC_GC_WORKER_H
#define KLASSIC_GC_WORKER_H
#include "gc_handshake.h"
enum { GC_WK_POLL=0,GC_WK_ENTER_NATIVE=1,GC_WK_LEAVE_NATIVE=2,GC_WK_ATTACH=3 };
enum { GC_WK_OK=0,GC_WK_PARK=1,GC_WK_RETRY=2,GC_WK_INVALID=3,GC_WK_FLUSH_SATB=4,
    GC_WK_FLUSH_REMEMBERED=5,GC_WK_CLOSE_TLAB=6,GC_WK_EXHAUSTED=7 };
static inline uint64_t gc_worker_step_locked(RootRegistry *roots,WorkerRequest *q,uint64_t op){
    if(op>GC_WK_ATTACH||!q->handshake||(q->handshake&7)||!q->participant||(q->participant&7)||
        !gc_abi_valid_phase((PhaseSnapshot){roots->phase}))return GC_WK_INVALID;
    HandshakeState *h=(HandshakeState *)(uintptr_t)q->handshake;
    ThreadRegistration *p=(ThreadRegistration *)(uintptr_t)q->participant;
    if(!p->context||(p->context&7))return GC_WK_INVALID;
    MutatorContext *ctx=(MutatorContext *)(uintptr_t)p->context;
    uint64_t previous=atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire);
    if(ctx->abi_version!=GC_ABI_VERSION||atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||
        !gc_abi_valid_phase((PhaseSnapshot){previous}))return GC_WK_INVALID;
    if(op==GC_WK_ATTACH){
        if(previous||ctx->root_segments||ctx->retiring_roots||ctx->tlab_start||ctx->tlab_top||ctx->tlab_end)return GC_WK_INVALID;
    }else if(previous!=roots->phase){
        if((roots->phase>>16)<=(previous>>16))return GC_WK_INVALID;
        // A coordinator may publish a newer snapshot after the owner's poll
        // but before native entry. Stay Running and acknowledge it first.
        if(op==GC_WK_ENTER_NATIVE)return h->active==1?GC_WK_RETRY:GC_WK_INVALID;
        if(op==GC_WK_POLL&&!h->active)return GC_WK_INVALID;
    }
    if(ctx->tlab_start||ctx->tlab_top||ctx->tlab_end)return GC_WK_CLOSE_TLAB;
    BufferCursor *buffers[]={&ctx->satb,&ctx->remembered};
    for(unsigned i=0;i<2;i++){
        BufferCursor *b=buffers[i];uint64_t published=atomic_load_explicit(&b->published,memory_order_acquire);
        uint64_t consumed=atomic_load_explicit(&b->consumed,memory_order_acquire);
        if(published>b->top||consumed>published)return GC_WK_INVALID;
        if(published!=b->top)return i?GC_WK_FLUSH_REMEMBERED:GC_WK_FLUSH_SATB;
    }
    uint64_t operation=op==GC_WK_ATTACH?GC_HS_REGISTER:op==GC_WK_ENTER_NATIVE?GC_HS_ENTER_NATIVE:
        op==GC_WK_LEAVE_NATIVE?GC_HS_LEAVE_NATIVE:h->active?GC_HS_ACK:GC_HS_RESUME;
    uint64_t result=gc_handshake_step_locked(h,p,operation);
    if(result==GC_HS_WAIT)return GC_WK_RETRY;
    if(result==GC_HS_EXHAUSTED)return GC_WK_EXHAUSTED;
    if(result==GC_HS_DUPLICATE)return previous==roots->phase?GC_WK_PARK:GC_WK_INVALID;
    if(result!=GC_HS_OK)return GC_WK_INVALID;
    if(op==GC_WK_ATTACH||op==GC_WK_LEAVE_NATIVE||operation==GC_HS_ACK)
        atomic_store_explicit(&ctx->adopted_phase,roots->phase,memory_order_release);
    return operation==GC_HS_ACK?GC_WK_PARK:GC_WK_OK;
}
#endif
