/* Transfer one unique claimed slot after its Old owner moves. SAME registry/
 * phase lock as the inventory operations; no heap access or guard is held.
 * The caller has rooted/followed the owner, validated the identical descriptor
 * field in a distinct canonical Old page, and retained both metadata lifetimes.
 * New buffer resolution uses the canonical destination. An already staged
 * source entry can still arrive while its source metadata/inventory is retained;
 * its reinserted work must be claimed/remapped again before source retirement.
 * This leaf checks inventory/claim geometry, not that arbitrary objects match.
 *
 * Destination must have adopted the same epoch; WAIT/INVALID preserve all
 * bytes and leave the source claim owned. On OK, destination previous gains
 * pending work BEFORE source obligations disappear in the locked transaction.
 * Any source current bit moves too, and a late reinserted source previous bit
 * coalesces with the new destination pending bit. Existing destination bits
 * and a destination worker's active claim are preserved. A later scan of the
 * destination decides keep/Young marking, not this transfer.
 *
 * Claim input is a consumed receipt on OK (its serial no longer authenticates
 * source ownership). The caller must reacquire a destination claim; do not ACK
 * the old receipt. Source bits for OTHER slots survive. Source metadata cannot
 * be retired merely because this slot transferred; all roots/buffers/jobs and
 * address/color retirement obligations still apply. Native controls/maps are
 * disjoint/live and ownership follows the inventory registration contract.
 */
#ifndef KLASSIC_GC_REMEMBERED_TRANSFER_H
#define KLASSIC_GC_REMEMBERED_TRANSFER_H
#include "gc_remembered_inventory.h"
static inline uint64_t gc_remembered_transfer_locked(RememberedInventory *g,const RememberedTransfer *q){
    if(!q||((uintptr_t)q&7))return GC_RS_INVALID;
    RememberedInventoryRequest check={.registration=q->source,.job=q->claim};
    if(gc_remembered_inventory_step_locked(g,&check,GC_RI_VALIDATE)!=GC_RS_OK)return GC_RS_INVALID;
    check.registration=q->destination;
    if(gc_remembered_inventory_step_locked(g,&check,GC_RI_VALIDATE)!=GC_RS_OK)return GC_RS_INVALID;
    RememberedSet *s=(RememberedSet *)(uintptr_t)((RememberedRegistration *)(uintptr_t)q->source)->set;
    RememberedSet *d=(RememberedSet *)(uintptr_t)((RememberedRegistration *)(uintptr_t)q->destination)->set;
    RememberedJob *claim=(RememberedJob *)(uintptr_t)q->claim;
    if(s==d||(s->start<d->limit&&d->start<s->limit)||claim->incarnation!=s->incarnation||
       !claim->serial||claim->serial!=s->active_serial||claim->offset!=s->active_offset||claim->phase!=s->epoch||claim->keep||
       q->incarnation!=d->incarnation||q->offset<d->start||q->offset>=d->limit||(q->offset&7)||
       s->last_phase>g->last_phase||d->last_phase>g->last_phase)return GC_RS_INVALID;
    if(s->epoch!=g->epoch||d->epoch!=g->epoch)return GC_RS_WAIT;
    uint64_t si=(claim->offset-s->start)/8,di=(q->offset-d->start)/8;
    uint64_t sm=UINT64_C(1)<<(si%64),dm=UINT64_C(1)<<(di%64);si/=64;di/=64;
    uint64_t *sc=(uint64_t *)(uintptr_t)s->bits[s->current]+si,*sp=(uint64_t *)(uintptr_t)s->bits[s->current^1]+si;
    uint64_t *dc=(uint64_t *)(uintptr_t)d->bits[d->current]+di,*dp=(uint64_t *)(uintptr_t)d->bits[d->current^1]+di;
    uint64_t c=!!(*sc&sm),p=!!(*sp&sm),a=!!(*dc&dm),b=!!(*dp&dm),before=!!s->pending+!!d->pending;
    if(s->current_count<c||s->pending<p||d->current_count<a||d->pending<b||
       (c&&!a&&d->current_count==(d->limit-d->start)/8)||(!b&&d->pending==(d->limit-d->start)/8)||
       g->pending<before||g->claims<1+!!d->active_serial||
       g->pending-before+!!(s->pending-p)+1>g->members)return GC_RS_INVALID;
    *dc|=c?dm:0;*dp|=dm;d->current_count+=c&&!a;d->pending+=!b;
    *sc&=~sm;*sp&=~sm;s->current_count-=c;s->pending-=p;
    s->active_serial=s->active_offset=0;g->claims--;
    g->pending=g->pending-before+!!s->pending+1;
    if(s->last_phase>d->last_phase)d->last_phase=s->last_phase;
    return GC_RS_OK;
}
#endif
