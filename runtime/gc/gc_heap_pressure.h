/* Forecast mutator pressure using a coherent heap accounting snapshot under
 * its lock (or an immutable private copy). The soft target may trigger GC but
 * is not an allocation prohibition. The hard target retains the same reserve
 * as mutator page admission: min(soft_max, hard_max-relocation_reserve).
 * Committed plus pending bytes at/above that target mean zero headroom, not
 * unsigned underflow. Relocation may legally have used the reserved capacity.
 * Metadata has its separate budget; no cached pressure flag is trusted here.
 * No heap pages or recorded sample context are dereferenced. This is still a
 * single-mutator forecast with a caller-supplied horizon, not a full scheduler. */
#ifndef KLASSIC_GC_HEAP_PRESSURE_H
#define KLASSIC_GC_HEAP_PRESSURE_H
#include "gc_allocation_pressure.h"
typedef uint64_t (*HeapAllocationPressure)(const HeapPool *,const AllocationSample *,uint64_t);
static inline uint64_t gc_heap_allocation_pressure(const HeapPool *h,const AllocationSample *sample,uint64_t horizon){
    if(!h||((uintptr_t)h&7)||!h->hard_max||h->soft_max>h->hard_max||
        h->relocation_reserve>h->hard_max||h->committed>h->hard_max||
        h->pending>h->hard_max-h->committed)return GC_PRESSURE_INVALID;
    uint64_t limit=h->hard_max-h->relocation_reserve;
    if(h->soft_max<limit)limit=h->soft_max;
    uint64_t used=h->committed+h->pending;
    return gc_allocation_pressure(sample,used>=limit?0:limit-used,horizon);
}
#endif
