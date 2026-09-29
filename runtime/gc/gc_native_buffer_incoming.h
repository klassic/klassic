/* Bounded absence audit over the runtime's registered SATB/remset owners.
 * Each call holds the registry lock; NEXT inspects at most one context and
 * does not retain a context lease. Before dereferencing a saved cursor it
 * checks last_identity and member count: registration strictly increases the
 * former, and any detach without registration decreases the latter. Thus
 * removal/reuse cannot leave a stale cursor looking valid, even when a new
 * entry replaces an old one at exactly the same address. Neither may wrap.
 *
 * The cycle must exclude introduction of target references behind the cursor
 * from BEGIN until DONE is consumed. A successful per-context quiescence check
 * is consumed immediately as evidence of absence at that owner's handoff;
 * resumed owners may do unrelated work, but must not recreate target refs.
 * This is not a certificate that all owners remain stopped or buffers empty.
 * The caller implements that exclusion protocol; this observer does not.
 *
 * Waiting for a Running owner or active consumer occurs outside locks. No
 * global stopped-phase traversal, buffer drain, managed access, allocation or
 * polling occurs here. All consumers must use registered CLAIM/COPY/ACK.
 * DONE covers the registered buffer domain only, not other retirement proofs.
 */
#ifndef KLASSIC_GC_NATIVE_BUFFER_INCOMING_H
#define KLASSIC_GC_NATIVE_BUFFER_INCOMING_H
#include "gc_native_buffer_quiescence.h"
#include "gc_native_scan_incoming.h"
typedef struct {
    uint64_t self,phase,generation,last_identity,members,head,cursor,remaining;
    uint64_t previous,previous_identity,stage,target_start,target_limit;
    NativeRuntimeOwner *runtime;
} NativeBufferIncoming;
static inline uint64_t gc_native_buffer_incoming_begin_locked(NativeRuntimeOwner *r,
        uint64_t start,uint64_t limit,NativeBufferIncoming *q){
    if(!q||((uintptr_t)q&7)||q->self||!gc_native_cycle_valid(r,0)||
       !gc_native_scan_incoming_range(start,limit))return GC_NSI_INVALID;
    HandshakeState *h=&r->handshake;
    if(!!h->head!=!!h->members||h->members>h->last_identity||h->native>h->members||h->active>1)
        return GC_NSI_INVALID;
    *q=(NativeBufferIncoming){.self=(uintptr_t)q,.phase=r->roots.phase,.generation=h->generation,
        .last_identity=h->last_identity,.members=h->members,.head=h->head,.cursor=h->head,
        .remaining=h->members,.stage=1,.target_start=start,.target_limit=limit,.runtime=r};
    return GC_NSI_MORE;
}
static inline uint64_t gc_native_buffer_incoming_next_locked(NativeBufferIncoming *q){
    if(!q||((uintptr_t)q&7)||q->self!=(uintptr_t)q||(q->stage!=1&&q->stage!=2)||
       !gc_native_cycle_valid(q->runtime,0)||!gc_native_scan_incoming_range(q->target_start,q->target_limit))return GC_NSI_INVALID;
    HandshakeState *h=&q->runtime->handshake;
    /* No saved thread pointer may be borrowed before these checks. */
    if(q->phase!=q->runtime->roots.phase||q->generation!=h->generation||
       q->last_identity!=h->last_identity||q->members!=h->members||q->head!=h->head)return GC_NSI_RESTART;
    if(!!q->cursor!=!!q->remaining||q->remaining>q->members)return GC_NSI_INVALID;
    if(!q->cursor){q->stage=2;return GC_NSI_DONE;}
    if(q->stage!=1||(q->cursor&7))return GC_NSI_INVALID;
    ThreadRegistration *p=(void *)(uintptr_t)q->cursor;
    if(p->registry!=(uintptr_t)h||p->registered!=1||p->previous!=q->previous||
       !p->identity||p->identity>q->last_identity||
       (q->previous&&p->identity>=q->previous_identity)||
       (!q->previous&&q->cursor!=q->head)||(p->next&7)||!!p->next!=(q->remaining>1))return GC_NSI_INVALID;
    uint64_t status=gc_native_buffer_quiescence_locked(h,p);
    if(status!=GC_NBQ_DONE)return status==GC_NBQ_WAIT?GC_NSI_WAIT:GC_NSI_INVALID;
    q->previous=q->cursor;q->previous_identity=p->identity;
    q->cursor=p->next;q->remaining--;
    return GC_NSI_MORE;
}
#endif
