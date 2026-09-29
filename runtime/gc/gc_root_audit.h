/* Concurrent root inventory audit. Every step holds the root/thread registry
 * lock, but scans/journal COPY/mark handoff run outside it through the embedded
 * RootWalk and registered journal jobs. One unique audit owner uses this record.
 * NEXT requires processed root cells AND an empty, unclaimed journal. A unique
 * serial samples each owner's identity/revision before its first segment scan.
 * CHECK/OWNER are bounded metadata checks only: callers must hold all producers
 * stopped in the same handshake while evaluating the overall end predicate.
 * The selected MarkQueue remains live through the audit; its generation and
 * monotonic cycle epoch are changed only under this registry lock. Per-owner
 * certificates are separate for Young/Old. One current audit per generation
 * avoids conservatively invalidating an older audit with another same-gen pass.
 * The marking entry covers roots only. The Retire entry also gates context
 * buffers/consumers/private producers. Neither certifies queues, external jobs
 * or full retirement. */
#ifndef KLASSIC_GC_ROOT_AUDIT_H
#define KLASSIC_GC_ROOT_AUDIT_H
#include "gc_root_walk.h"
#define GC_RA_BEGIN 0u
#define GC_RA_NEXT 1u
#define GC_RA_CHECK 2u
#define GC_RA_OWNER 3u
#define GC_RA_ITEM 0u
#define GC_RA_INVALID 1u
#define GC_RA_DONE 2u
#define GC_RA_EXHAUSTED 3u
#define GC_RA_WAIT 4u
#define GC_RA_STALE 5u
static inline uint64_t gc_root_audit_mode_locked(RootRegistry *g,RootAudit *a,uint64_t op,int retire){
    if(op>GC_RA_OWNER)return GC_RA_INVALID;
    if(!a->queue||(a->queue&7))return GC_RA_INVALID;
    MarkQueue *queue=(MarkQueue *)(uintptr_t)a->queue;
    RootWalk *w=&a->walk;
    if(op>=GC_RA_CHECK){
        if(a->state!=2||!a->serial||a->serial>g->last_lease||!a->epoch||a->generation>1||w->active||w->lease.kind||
            w->registry!=(uintptr_t)g||w->phase!=a->phase||w->mask!=(UINT64_C(1)<<a->generation)||
            !gc_abi_valid_phase((PhaseSnapshot){g->phase}))return GC_RA_INVALID;
        uint64_t phase=(g->phase>>(a->generation*4))&15;
        if(a->last_scope!=g->last_scope||queue->epoch!=a->epoch||queue->generation!=a->generation||
            (retire?(phase!=7||a->phase!=g->phase):(phase<1||phase>3||
            ((a->phase^g->phase)&(UINT64_C(1)<<(8+a->generation))))))return GC_RA_STALE;
        if(op==GC_RA_CHECK)return GC_RA_DONE;
        if(!a->context||(a->context&7))return GC_RA_INVALID;
        MutatorContext *ctx=(MutatorContext *)(uintptr_t)a->context;
        uint64_t state=atomic_load_explicit(&ctx->state,memory_order_acquire);
        if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||state<GC_AT_SAFEPOINT||state>GC_DETACHING||
            atomic_load_explicit(&ctx->access_epoch,memory_order_acquire))return GC_RA_INVALID;
        if(retire){
            // Producers stay stopped through all OWNER checks and the caller's
            // retirement decision. Even an owner with no roots can retain work.
            if(atomic_load_explicit(&ctx->private_producers,memory_order_acquire)||
                ctx->satb_consumer.serial||ctx->satb_consumer.end||ctx->satb_consumer.cursor||
                ctx->remembered_consumer.serial||ctx->remembered_consumer.end||ctx->remembered_consumer.cursor)return GC_RA_WAIT;
            BufferCursor *buffers[]={&ctx->satb,&ctx->remembered};
            for(unsigned i=0;i<2;i++){
                uint64_t published=atomic_load_explicit(&buffers[i]->published,memory_order_acquire);
                uint64_t consumed=atomic_load_explicit(&buffers[i]->consumed,memory_order_acquire);
                if(consumed>published||published>buffers[i]->top)return GC_RA_INVALID;
                if(consumed!=published||published!=buffers[i]->top)return GC_RA_WAIT;
            }
        }
        if(ctx->retiring_roots)return GC_RA_WAIT;
        if(!ctx->root_segments)return GC_RA_DONE;
        return ctx->root_audit_serial[a->generation]==a->serial&&ctx->root_audit_identity[a->generation]==ctx->thread_id&&
            ctx->root_audit_revision[a->generation]==atomic_load_explicit(&ctx->root_revision,memory_order_acquire)?GC_RA_DONE:GC_RA_STALE;
    }
    RootRegistration *next;
    if(op==GC_RA_BEGIN){
        if((a->state!=0&&a->state!=2)||w->active||w->lease.kind||!gc_abi_valid_phase((PhaseSnapshot){g->phase}))return GC_RA_INVALID;
        if(!queue->epoch||queue->generation>1)return GC_RA_INVALID;
        uint64_t phase=(g->phase>>(queue->generation*4))&15;
        if(retire?phase!=7:(phase<1||phase>3))return GC_RA_INVALID;
        next=(RootRegistration *)(uintptr_t)g->inventory;
        if(g->last_lease>=UINT64_MAX-(next!=NULL))return GC_RA_EXHAUSTED;
    }else{
        if(a->state!=1||!a->serial||a->serial>g->last_lease||!a->epoch||a->generation>1||w->phase!=a->phase||w->registry!=(uintptr_t)g||
            w->active!=1||w->lease.kind!=1||w->mask!=(UINT64_C(1)<<a->generation)||!w->lease.registration||(w->lease.registration&7))return GC_RA_INVALID;
        RootRegistration *r=(RootRegistration *)(uintptr_t)w->lease.registration;
        if(r->registered!=1||r->scope!=w->lease.scope||!r->segment||(r->segment&7))return GC_RA_INVALID;
        RootSegment *s=(RootSegment *)(uintptr_t)r->segment;
        if(s->journal){
            if(s->journal&7)return GC_RA_INVALID;
            BufferCursor *b=(BufferCursor *)(uintptr_t)s->journal;
            uint64_t consumed=atomic_load_explicit(&b->consumed,memory_order_acquire);
            uint64_t published=atomic_load_explicit(&b->published,memory_order_acquire);
            if(consumed>published)return GC_RA_INVALID;
            if(consumed!=published)return GC_RA_WAIT;
        }
        if(r->journal_serial)return GC_RA_WAIT;
        next=(RootRegistration *)(uintptr_t)r->inventory_next;
    }
    // Validate the next owner before the walk commits any lease transfer. The
    // registry lock pins next here; the acquired walk lease pins it afterward.
    MutatorContext *ctx=NULL;
    if(next){
        if(((uintptr_t)next&7)||!next->context||(next->context&7)||!next->segment||(next->segment&7))return GC_RA_INVALID;
        RootSegment *s=(RootSegment *)(uintptr_t)next->segment;ctx=(MutatorContext *)(uintptr_t)next->context;
        if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||ctx->thread_id!=s->owner)return GC_RA_INVALID;
    }
    uint64_t saved=g->last_lease,serial=a->serial;
    if(op==GC_RA_BEGIN)serial=++g->last_lease;
    uint64_t result=gc_root_walk_locked(g,w,op);
    if(result!=GC_RW_ITEM&&result!=GC_RW_DONE){g->last_lease=saved;return result;}
    if(op==GC_RA_BEGIN){a->serial=serial;a->last_scope=g->last_scope;a->phase=g->phase;
        a->epoch=queue->epoch;a->generation=queue->generation;w->mask=UINT64_C(1)<<a->generation;}
    a->state=result==GC_RW_DONE?2:1;
    if(ctx&&ctx->root_audit_serial[a->generation]!=a->serial){
        ctx->root_audit_serial[a->generation]=a->serial;ctx->root_audit_identity[a->generation]=ctx->thread_id;
        ctx->root_audit_revision[a->generation]=atomic_load_explicit(&ctx->root_revision,memory_order_acquire);
    }
    return result;
}
static inline uint64_t gc_root_audit_step_locked(RootRegistry *g,RootAudit *a,uint64_t op){
    return gc_root_audit_mode_locked(g,a,op,0);
}
/* Retire inventory certificate: unlike marking, any snapshot change invalidates
 * the pass. Callers separately prove range absence, all-owner quiescence and
 * drained external jobs. OWNER additionally requires both context buffers and
 * their consumer controls empty and no private producer. No reclaim, color reuse
 * or cycle completion is implied.
 * Do not switch entry points during a pass; the selected queue owns its epoch. */
static inline uint64_t gc_root_retire_audit_step_locked(RootRegistry *g,RootAudit *a,uint64_t op){
    return gc_root_audit_mode_locked(g,a,op,1);
}
#endif
