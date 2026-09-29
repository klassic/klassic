/* Native SPSC transport for already prepared BarrierEntries. A single context
 * owner appends/publishes; one collector consumer drains each buffer. All
 * supplied native storage is live, disjoint and nonmoving. Success transfers
 * the entry's reference/metadata-lease obligations, never acquires them. Full
 * or invalid returns retain input ownership. Actual barriers must acquire the
 * required protection BEFORE calling append and must keep it through retry.
 * No operation polls, blocks, allocates or resolves a managed address. These
 * leaves may run with a heap access guard, but callers may not wait with it.
 */
#ifndef KLASSIC_GC_BUFFERS_H
#define KLASSIC_GC_BUFFERS_H
#include "gc_abi.h"
#define GC_BB_APPEND_SATB 0u
#define GC_BB_APPEND_REMEMBERED 1u
#define GC_BB_PUBLISH_SATB 2u
#define GC_BB_PUBLISH_REMEMBERED 3u
#define GC_BB_OK 0u
#define GC_BB_INVALID 1u
#define GC_BB_FULL 2u
#define GC_BB_EXHAUSTED 3u
static inline bool gc_barrier_buffer_valid(const BufferCursor *b) {
    return b->entries && !(b->entries&7) && b->capacity && b->capacity<=UINT32_MAX &&
        b->entries<=UINT64_MAX-b->capacity*sizeof(BarrierEntry);
}
static inline uint64_t gc_barrier_buffer_step(MutatorContext *ctx,const BarrierEntry *entry,uint64_t op) {
    if(op>3 || ctx->abi_version!=GC_ABI_VERSION || !ctx->thread_id) return GC_BB_INVALID;
    uint64_t state=atomic_load_explicit(&ctx->state,memory_order_acquire);
    if(op<2 ? state!=GC_RUNNING : (state!=GC_RUNNING && state!=GC_AT_SAFEPOINT && state!=GC_DETACHING)) return GC_BB_INVALID;
    BufferCursor *b=(op&1)?&ctx->remembered:&ctx->satb;
    if(!gc_barrier_buffer_valid(b)) return GC_BB_INVALID;
    uint64_t top=b->top,published=atomic_load_explicit(&b->published,memory_order_acquire);
    uint64_t consumed=atomic_load_explicit(&b->consumed,memory_order_acquire);
    if(published>top || consumed>published || top-consumed>b->capacity) return GC_BB_INVALID;
    if(op>=2) {atomic_store_explicit(&b->published,top,memory_order_release);return GC_BB_OK;}
    if(!entry || ((uintptr_t)entry&7)) return GC_BB_INVALID;
    if(top==UINT64_MAX) return GC_BB_EXHAUSTED;
    if(top-consumed==b->capacity) return GC_BB_FULL;
    ((BarrierEntry *)(uintptr_t)b->entries)[top%b->capacity]=*entry;
    b->top=top+1;
    return GC_BB_OK;
}
/* Output is protected worker storage, including the transferred lease tokens.
 * Consume copies <=256 entries and publishes progress AFTER the entire copy.
 * It never reads producer-private top. The consumer must not retain a source
 * entry address; copied references remain in-flight work after the ring empties.
 * Scheduling/registry borrows must separately exclude polls of a freed context.
 */
static inline uint64_t gc_barrier_buffer_drain(BarrierDrain *q) {
    if(!q->buffer || (q->buffer&7) || !q->output || (q->output&7) || !q->capacity ||
        q->capacity>256 || q->output>UINT64_MAX-q->capacity*sizeof(BarrierEntry)) return GC_BB_INVALID;
    BufferCursor *b=(BufferCursor *)(uintptr_t)q->buffer;
    if(!gc_barrier_buffer_valid(b)) return GC_BB_INVALID;
    uint64_t consumed=atomic_load_explicit(&b->consumed,memory_order_acquire);
    uint64_t published=atomic_load_explicit(&b->published,memory_order_acquire);
    if(consumed>published || published-consumed>b->capacity) return GC_BB_INVALID;
    uint64_t n=published-consumed;if(n>q->capacity)n=q->capacity;
    BarrierEntry *entries=(BarrierEntry *)(uintptr_t)b->entries,*out=(BarrierEntry *)(uintptr_t)q->output;
    for(uint64_t i=0;i<n;i++)out[i]=entries[(consumed+i)%b->capacity];
    q->written=n;
    atomic_store_explicit(&b->consumed,consumed+n,memory_order_release);
    return consumed+n==published?GC_BB_OK:2;
}
#endif
