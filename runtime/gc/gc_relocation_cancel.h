/* Resumable private relocation cancellation, heap lock held. Unique stable
 * controls and immutable inputs remain live until DONE; source strong liveness,
 * metadata, mappings and exclusive target append/cleanup remain caller owned.
 * Enter only after SELECT/CHECK drained every source actor, before INSTALL.
 * Each call clears at most 256 bytes or performs one finite metadata transition.
 * Stages: 0 discard, 1 retain, 2 reopen source, 3 release permit, 4 terminal.
 * MORE allows rescheduling while ownership is retained. INVALID changes nothing
 * and never releases admission. No allocation, OS call, polling or waiting here.
 * Once reopened, readers may enter before the final permit release.
 */
#ifndef KLASSIC_GC_RELOCATION_CANCEL_H
#define KLASSIC_GC_RELOCATION_CANCEL_H
#include "gc_relocate_discard.h"
#include "gc_relocation_gate.h"
#include "gc_source_access.h"
enum { GC_RCAN_DONE=0,GC_RCAN_INVALID=1,GC_RCAN_MORE=2 };
static inline uint64_t gc_relocation_cancel_locked(RelocationGate *g,RelocationCancellation *q){
    if(q->stage>3||(q->owner?q->owner!=(uintptr_t)q:q->stage!=0)||
        !q->target||(q->target&7)||!q->discard||(q->discard&7))return GC_RCAN_INVALID;
    RelocationDiscard *d=(RelocationDiscard *)(uintptr_t)q->discard;
    if(!d->source||(d->source&7)||!d->copy||(d->copy&7)||
        (!q->owner&&(d->owner||d->cursor||d->bytes)))return GC_RCAN_INVALID;
    ForwardCell *source=(ForwardCell *)(uintptr_t)d->source;
    RelocationCopy *copy=(RelocationCopy *)(uintptr_t)d->copy;
    if(!q->serial||q->serial>(UINT64_MAX>>2)||copy->ticket!=q->serial||
        !source->incarnation||copy->source_incarnation!=source->incarnation||
        source->source_offset<48||(source->source_offset&15)||source->source_offset>GC_MAX_OFFSET)return GC_RCAN_INVALID;
    RelocationGateRequest permit={q->permit,q->serial,q->epoch};
    if(gc_relocation_gate_locked(g,&permit,GC_RG_INSPECT)!=GC_RG_OK)return GC_RCAN_INVALID;
    if(!q->stage){
        uint64_t status=gc_relocate_discard_locked((AllocationPage *)(uintptr_t)q->target,d);
        if(status==GC_DISCARD_INVALID)return GC_RCAN_INVALID;
        q->owner=(uintptr_t)q;
        if(status==GC_DISCARD_DONE)q->stage=1;
        return GC_RCAN_MORE;
    }
    if(d->owner!=(uintptr_t)d||d->cursor!=d->bytes)return GC_RCAN_INVALID;
    if(q->stage==1){
        ForwardRequest retain={source->source_offset,source->incarnation,q->serial,0};
        if(gc_forward_step(source,&retain,GC_FWD_RETAIN)!=GC_FWD_OK)return GC_RCAN_INVALID;
        q->stage=2;return GC_RCAN_MORE;
    }
    if(q->stage==2){
        SourceAccessRequest reopen={0,source->source_offset,source->incarnation,0,0};
        if(gc_source_access_step(source,&reopen,GC_SA_REOPEN)!=GC_SA_OK)return GC_RCAN_INVALID;
        q->stage=3;return GC_RCAN_MORE;
    }
    if(atomic_load_explicit(&source->word,memory_order_acquire)!=3||
        (atomic_load_explicit(&source->access,memory_order_acquire)&GC_SOURCE_CLOSED))return GC_RCAN_INVALID;
    if(gc_relocation_gate_locked(g,&permit,GC_RG_RELEASE)!=GC_RG_OK)return GC_RCAN_INVALID;
    q->stage=4;return GC_RCAN_DONE;
}
#endif
