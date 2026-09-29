/* Shorten an already published forwarding edge under the scheduler/heap lock.
 * All publication jobs are drained and the relocation gate stays closed across
 * resolution and this update. Serialize compression with incoming-reference
 * audits: after changing an edge, restart that audit before using DONE. The caller
 * has followed observed to destination for the SAME logical object, validated
 * its canonical header under a guard/root, and retains every involved metadata
 * lifetime (including intermediate tables). Native controls are live/disjoint.
 * This primitive cannot infer object identity from arbitrary supplied addresses.
 *
 * The old source payload may already be inaccessible; only its retained cell is
 * read. CAS changes exactly the observed published edge, never a claim, payload
 * or identity. Existing readers of the old edge remain safe under their leases.
 * Rejections and stale observations preserve all state. Gate reopening requires
 * a fresh resolution/audit epoch. Removing one incoming edge is NOT a source,
 * root, buffer, job, handle or color-retirement certificate; retain that metadata
 * until the complete owner audit proves reuse safe. No allocation or waiting.
 */
#ifndef KLASSIC_GC_FORWARD_COMPRESS_H
#define KLASSIC_GC_FORWARD_COMPRESS_H
#include "gc_forward.h"
#include "gc_relocation_gate.h"
enum { GC_FC_OK=0,GC_FC_INVALID=1,GC_FC_WAIT=2,GC_FC_RETRY=3 };
typedef uint64_t (*ForwardCompressor)(RelocationGate *,const ForwardCompress *);
static inline uint64_t gc_forward_compress_locked(RelocationGate *g,const ForwardCompress *q){
    if(!g||((uintptr_t)g&7)||!q||((uintptr_t)q&7)||!q->cell||(q->cell&7)||!q->epoch||
       !q->source_offset||(q->source_offset&15)||q->source_offset>GC_MAX_OFFSET||!q->incarnation)return GC_FC_INVALID;
    uint64_t status=gc_relocation_gate_locked(g,NULL,GC_RG_CHECK);
    if(status!=GC_RG_OK)return status==GC_RG_WAIT?GC_FC_WAIT:GC_FC_INVALID;
    if(q->epoch!=g->epoch)return GC_FC_RETRY;
    ForwardCell *cell=(ForwardCell *)(uintptr_t)q->cell;
    if(cell->source_offset!=q->source_offset||cell->incarnation!=q->incarnation||
       atomic_load_explicit(&cell->access,memory_order_acquire)!=GC_SOURCE_CLOSED||
       !gc_forward_destination(q->observed,q->source_offset)||
       !gc_forward_destination(q->destination,q->source_offset))return GC_FC_INVALID;
    uint64_t expected=q->observed|2;
    return atomic_compare_exchange_strong_explicit(&cell->word,&expected,q->destination|2,
        memory_order_acq_rel,memory_order_acquire)?GC_FC_OK:GC_FC_RETRY;
}
#endif
