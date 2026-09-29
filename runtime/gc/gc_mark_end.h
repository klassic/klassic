/* Read-only mark termination check under the common thread/root/queue lock.
 * Caller holds that lock through the subsequent phase transition, before END.
 * A READY result is valid only in the requested active, completed rendezvous.
 * All root owners must be registered; every producer acknowledges only after
 * publishing private work or retaining it in the counted queues/leases/buffers.
 * New external jobs must participate in that accounting before integration.
 * Native entries cannot touch managed state or return while the rendezvous is
 * active. The list and every context remain borrowed only inside this call.
 * O(registered threads) metadata checks; no root/heap scan, drain or allocation.
 * This does not end the handshake or choose the next collection phase. */
#ifndef KLASSIC_GC_MARK_END_H
#define KLASSIC_GC_MARK_END_H
#include "gc_handshake.h"
#include "gc_root_audit.h"
#include "gc_mark.h"
#include "gc_remembered_inventory.h"
#define GC_ME_READY 0u
#define GC_ME_INVALID 1u
#define GC_ME_WAIT 2u
#define GC_ME_STALE 3u
static inline uint64_t gc_inventory_check_mode_locked(RootRegistry *g,MarkEndRequest *q,uint64_t op,int retire) {
    if(op||!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||!q->handshake||(q->handshake&7)||!q->audit||(q->audit&7))return GC_ME_INVALID;
    HandshakeState *h=(HandshakeState *)(uintptr_t)q->handshake;
    if(h->active>1||h->native>h->members||!!h->head!=!!h->members||h->acknowledged>h->expected||
       (h->active?(!h->generation||h->expected!=h->members-h->native):(h->expected||h->acknowledged)))return GC_ME_INVALID;
    if(!q->generation)return GC_ME_INVALID;
    if(q->generation!=h->generation)return GC_ME_STALE;
    if(!h->active||h->acknowledged!=h->expected)return GC_ME_WAIT;
    if(g->retiring||g->leases||g->walkers)return GC_ME_WAIT;
    RootAudit audit=*(RootAudit *)(uintptr_t)q->audit;
    uint64_t result=retire?gc_root_retire_audit_step_locked(g,&audit,GC_RA_CHECK):gc_root_audit_step_locked(g,&audit,GC_RA_CHECK);
    if(result!=GC_RA_DONE)return result==GC_RA_STALE?GC_ME_STALE:result==GC_RA_WAIT?GC_ME_WAIT:GC_ME_INVALID;
    MarkQueue *queue=(MarkQueue *)(uintptr_t)audit.queue;
    result=gc_mark_step_locked(queue,NULL,GC_MARK_CHECK);
    if(result!=GC_MARK_EMPTY)return result==GC_MARK_BUSY?GC_ME_WAIT:GC_ME_INVALID;
    RememberedInventory *remembered=(RememberedInventory *)(uintptr_t)q->remembered;
    if(!gc_remembered_inventory_valid(remembered))return GC_ME_INVALID;
    if(retire||queue->generation==GC_YOUNG) {
        if(remembered->last_phase>g->phase||((remembered->epoch^g->phase)&0x1000))return GC_ME_STALE;
        if(remembered->unflipped||remembered->pending||remembered->claims||remembered->walkers)return GC_ME_WAIT;
    }
    uint64_t previous=0,count=0,native=0;
    for(uint64_t ptr=h->head;ptr;) {
        if((ptr&7)||count>=h->members)return GC_ME_INVALID;
        ThreadRegistration *p=(ThreadRegistration *)(uintptr_t)ptr;
        if(p->registry!=(uintptr_t)h||p->registered!=1||p->previous!=previous||!p->identity||p->identity>h->last_identity||
           (previous&&p->identity>=((ThreadRegistration *)(uintptr_t)previous)->identity)||
           !p->context||(p->context&7)||p->acknowledged_generation>h->generation)return GC_ME_INVALID;
        MutatorContext *ctx=(MutatorContext *)(uintptr_t)p->context;
        if(ctx->thread_id!=p->identity)return GC_ME_INVALID;
        uint64_t state=atomic_load_explicit(&ctx->state,memory_order_acquire);
        if(state==GC_BLOCKED_NATIVE)native++;
        else if((state!=GC_AT_SAFEPOINT&&state!=GC_DETACHING)||p->acknowledged_generation!=h->generation||
                atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=g->phase)return GC_ME_INVALID;
        if(atomic_load_explicit(&ctx->private_producers,memory_order_acquire))return GC_ME_WAIT;
        audit.context=p->context;
        result=retire?gc_root_retire_audit_step_locked(g,&audit,GC_RA_OWNER):gc_root_audit_step_locked(g,&audit,GC_RA_OWNER);
        if(result!=GC_RA_DONE)return result==GC_RA_STALE?GC_ME_STALE:result==GC_RA_WAIT?GC_ME_WAIT:GC_ME_INVALID;
        if(!retire){
        if(ctx->satb_consumer.serial||ctx->satb_consumer.end||ctx->remembered_consumer.serial||ctx->remembered_consumer.end)return GC_ME_WAIT;
        BufferCursor *buffers[]={&ctx->satb,&ctx->remembered};
        for(unsigned i=0;i<2;i++) {
            uint64_t published=atomic_load_explicit(&buffers[i]->published,memory_order_acquire);
            uint64_t consumed=atomic_load_explicit(&buffers[i]->consumed,memory_order_acquire);
            if(consumed>published||published>buffers[i]->top)return GC_ME_INVALID;
            if(consumed!=published||published!=buffers[i]->top)return GC_ME_WAIT;
        }
        }
        previous=ptr;ptr=p->next;count++;
    }
    return count==h->members&&native==h->native?GC_ME_READY:GC_ME_INVALID;
}
static inline uint64_t gc_mark_end_check_locked(RootRegistry *g,MarkEndRequest *q,uint64_t op){
    return gc_inventory_check_mode_locked(g,q,op,0);
}
/* Same request layout and result codes, but consumes a Retire root audit.
 * Read-only and bound to one completed handshake: visits every registered
 * managed/native owner, the selected queue and both-generation remset work.
 * READY only certifies this inventory. The other queue, relocation gate,
 * heap-range absence, external jobs, weak/native handles, forwarding and source
 * leases/reuse remain separate obligations. It never changes phase or FINISHes.
 * Hold the registry lock and producers stopped through the combined decision. */
static inline uint64_t gc_retire_inventory_check_locked(RootRegistry *g,MarkEndRequest *q,uint64_t op){
    return gc_inventory_check_mode_locked(g,q,op,1);
}
#endif
