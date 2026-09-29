/* Linear forecast from one successful allocation sample. Caller supplies a
 * fresh immutable snapshot, usable headroom AFTER reserves, and a positive
 * forecast horizon. This is not a bound on future allocation or aggregate
 * multi-mutator demand. It neither selects generations/workers nor runs a GC.
 * Compare delta_bytes*horizon >= headroom*elapsed exactly, without rounding,
 * division, floating point or overflowing either 128-bit product. No context
 * pointer is dereferenced. INVALID must not be interpreted as quiet demand. */
#ifndef KLASSIC_GC_ALLOCATION_PRESSURE_H
#define KLASSIC_GC_ALLOCATION_PRESSURE_H
#include "gc_abi.h"
#define GC_PRESSURE_QUIET 0u
#define GC_PRESSURE_REQUEST 1u
#define GC_PRESSURE_INVALID 2u
typedef uint64_t (*AllocationPressure)(const AllocationSample *,uint64_t,uint64_t);
typedef struct { uint64_t high,low; } GcAllocationProduct;
/* Base-2^32 limbs keep intermediates in uint64_t; no compiler-specific integer type. */
static inline GcAllocationProduct gc_allocation_product(uint64_t a,uint64_t b){
    uint64_t a0=(uint32_t)a,a1=a>>32,b0=(uint32_t)b,b1=b>>32;
    uint64_t w0=a0*b0,t=a1*b0+(w0>>32),w1=(uint32_t)t,w2=t>>32;
    w1+=a0*b1;
    return (GcAllocationProduct){a1*b1+w2+(w1>>32),(w1<<32)|(uint32_t)w0};
}
static inline uint64_t gc_allocation_pressure(const AllocationSample *q,uint64_t headroom,uint64_t horizon){
    if(!q||((uintptr_t)q&7)||!q->context||(q->context&7)||!q->identity||!q->elapsed_ns||!horizon||
        q->total_bytes==UINT64_MAX||q->delta_bytes>q->total_bytes||q->elapsed_ns>q->timestamp_ns)
        return GC_PRESSURE_INVALID;
    GcAllocationProduct demand=gc_allocation_product(q->delta_bytes,horizon);
    GcAllocationProduct available=gc_allocation_product(headroom,q->elapsed_ns);
    return demand.high>available.high||(demand.high==available.high&&demand.low>=available.low)?
        GC_PRESSURE_REQUEST:GC_PRESSURE_QUIET;
}
#endif
