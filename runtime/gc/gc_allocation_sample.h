/* Finite observation of one retained native context. Its immutable identity
 * and storage must survive the call; the unique observer owns a disjoint query.
 * A caller-supplied monotonic timestamp is taken near the observation, outside
 * locks/guards. This is a rate-estimation window, not exact event timing or a
 * pause measurement. It grants no managed-memory or context-retirement rights.
 * No clock call, allocation, poll, lock, traversal or division occurs here.
 * Empty query establishes a baseline. OK provides an exact byte delta and a
 * positive timestamp delta. WAIT/INVALID/SATURATED preserve all query bytes.
 * SATURATED is unavailable data, never an observed zero allocation rate. */
#ifndef KLASSIC_GC_ALLOCATION_SAMPLE_H
#define KLASSIC_GC_ALLOCATION_SAMPLE_H
#include "gc_abi.h"
#define GC_SAMPLE_OK 0u
#define GC_SAMPLE_BASELINE 1u
#define GC_SAMPLE_WAIT 2u
#define GC_SAMPLE_INVALID 3u
#define GC_SAMPLE_SATURATED 4u
typedef uint64_t (*AllocationSampler)(const MutatorContext *,AllocationSample *,uint64_t);
static inline uint64_t gc_allocation_sample(const MutatorContext *ctx,AllocationSample *q,uint64_t now){
    if(!ctx||((uintptr_t)ctx&7)||!q||((uintptr_t)q&7)||ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id)
        return GC_SAMPLE_INVALID;
    if(!q->context){
        if(q->identity||q->timestamp_ns||q->total_bytes||q->elapsed_ns||q->delta_bytes)return GC_SAMPLE_INVALID;
    }else{
        if(q->context!=(uintptr_t)ctx||q->identity!=ctx->thread_id||q->total_bytes==UINT64_MAX||now<q->timestamp_ns)
            return GC_SAMPLE_INVALID;
        if(now==q->timestamp_ns)return GC_SAMPLE_WAIT;
    }
    uint64_t total=atomic_load_explicit(&ctx->allocated_bytes,memory_order_acquire);
    if(total==UINT64_MAX)return GC_SAMPLE_SATURATED;
    if(!q->context){
        *q=(AllocationSample){(uintptr_t)ctx,ctx->thread_id,now,total,0,0};return GC_SAMPLE_BASELINE;
    }
    if(total<q->total_bytes)return GC_SAMPLE_INVALID;
    q->elapsed_ns=now-q->timestamp_ns;q->delta_bytes=total-q->total_bytes;
    q->timestamp_ns=now;q->total_bytes=total;return GC_SAMPLE_OK;
}
#endif
