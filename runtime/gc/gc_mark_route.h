/* Generation-filtered mark preparation for strongly held work (root, SATB,
 * remembered claim or parent scan). The caller retains that producer's
 * termination obligation through routing and queue handoff. reference is the
 * protected value; null is represented with no guard/page/lease. Nonnull input
 * has been rooted/followed to the canonical receiver and acquired its guard
 * and metadata lease. No raw value may cross a poll/flush unrooted.
 *
 * Young routing skips canonical Old objects; Old routing traces both
 * generations so Young intermediaries cannot hide reachable Old children.
 * The selected generation must be in a marking phase. OFFER is still required
 * on the corresponding epoch's queue. This does not coordinate promotion or
 * transfer a moved remembered slot to another inventory.
 *
 * WAIT authenticates an INITIALIZING header without reading payload or moving
 * its lease. The caller must EXIT the guard before polling, retain its counted
 * producer/root/journal obligation, and retry after ready publication. Never ACK
 * initializing work merely because it is not ready.
 * READY transfers lease -> pending output. SKIP preserves input including its
 * lease, which the caller must release AFTER guard EXIT; null owns no lease.
 * All failures preserve input. No ACK, lock acquisition, wait or poll occurs.
 * A skipped claim can be ACKed only after this result's producer-specific
 * cleanup; concurrent current remset records must not be cleared by ACK.
 */
#ifndef KLASSIC_GC_MARK_ROUTE_H
#define KLASSIC_GC_MARK_ROUTE_H
#include "gc_mark_prepare.h"
#define GC_ROUTE_READY 0u
#define GC_ROUTE_INVALID 1u
#define GC_ROUTE_SKIP 2u
#define GC_ROUTE_WAIT 3u
static inline uint64_t gc_guarded_mark_route(GuardedMarkRoute *q){
    if(!q||((uintptr_t)q&7)||q->generation>GC_OLD)return GC_ROUTE_INVALID;
    GuardedMarkPrepare *p=&q->prepare;
    if(!gc_abi_valid_phase((PhaseSnapshot){p->phase}))return GC_ROUTE_INVALID;
    uint64_t phase=(p->phase>>(4*q->generation))&15;
    if(phase<GC_MARK_START||phase>GC_MARK_END_CHECK)return GC_ROUTE_INVALID;
    if(!p->reference){
        if(p->scope||p->page||p->lease.page||p->lease.incarnation||p->lease.token||
           p->output.record||p->output.reference||p->output.incarnation||p->output.metadata_lease||
           p->output.index||p->output.serial||p->output.cursor)return GC_ROUTE_INVALID;
        return GC_ROUTE_SKIP;
    }
    MarkRecord *record;uint64_t state;
    if(!gc_mark_prepare_record_state(p,&record,true,&state))return GC_ROUTE_INVALID;
    if(state==GC_OBJECT_INITIALIZING)return GC_ROUTE_WAIT;
    if(q->generation==GC_YOUNG&&((AllocationPage *)(uintptr_t)p->page)->generation==GC_OLD)return GC_ROUTE_SKIP;
    gc_mark_prepare_transfer(p,record);return GC_ROUTE_READY;
}
#endif
