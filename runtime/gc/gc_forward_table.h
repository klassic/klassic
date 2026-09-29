/* Metadata-only lookup; no managed address is formed or dereferenced.
 * Caller holds the run's metadata lease (or its exclusive preparation right)
 * through the returned cell's final use, and separately keeps the object live.
 * The table/backing are immutable after installation, except the owner appends
 * zero cells in strictly increasing source order and release-publishes used.
 * Never replace/resize/reset a table with live readers. A full table closes its
 * TLAB; preparation of another run is outside this leaf. This lookup does not
 * resolve stale colors, authorize range reuse, or admit source access itself.
 */
#ifndef KLASSIC_GC_FORWARD_TABLE_H
#define KLASSIC_GC_FORWARD_TABLE_H
#include "gc_abi.h"
enum { GC_FL_OK=0, GC_FL_INVALID=1, GC_FL_MISSING=2 };
static inline uint64_t gc_forward_lookup(const ForwardTable *g,ForwardLookup *q) {
    if(q->cell||!q->source_offset||q->source_offset>GC_MAX_OFFSET||(q->source_offset&15)||!q->incarnation||
        !g->cells||(g->cells&7)||!g->capacity||g->capacity>UINT32_MAX||
        g->cells>UINT64_MAX-g->capacity*sizeof(ForwardCell))return GC_FL_INVALID;
    uint64_t lo=0,hi=atomic_load_explicit(&g->used,memory_order_acquire);
    if(hi>g->capacity)return GC_FL_INVALID;
    const ForwardCell *cells=(const ForwardCell *)(uintptr_t)g->cells;
    /* At most 32 probes, including equality, even at UINT32_MAX capacity. */
    while(lo<hi){
        uint64_t mid=lo+((hi-lo)>>1);const ForwardCell *cell=&cells[mid];
        if(cell->source_offset<q->source_offset)lo=mid+1;
        else if(cell->source_offset>q->source_offset)hi=mid;
        else {
            if(cell->incarnation!=q->incarnation)return GC_FL_INVALID;
            q->cell=(uintptr_t)cell;return GC_FL_OK;
        }
    }
    return GC_FL_MISSING;
}
#endif
