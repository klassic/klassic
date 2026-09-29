/* Finite allocation into a pre-reserved empty root. No poll/lock/OS allocation.
 * The owner exclusively controls the context, TLAB, root and journal producer;
 * their live/disjoint native storage remains stable throughout this call.
 * Concurrent journal consumption only frees capacity; scanners cannot change an
 * empty root's logical value. All fallible checks precede allocation. Success
 * publishes the journal before the root and revision, in the same no-safepoint
 * interval. The object remains INITIALIZING: initialization/ready publication
 * and allocation slow paths still have their independent obligations.
 */
#ifndef KLASSIC_GC_ALLOC_INTO_H
#define KLASSIC_GC_ALLOC_INTO_H
#include "gc_heap.h"
#include "gc_roots.h"
#define GC_ALLOC_ROOT_FLUSH 5u
#define GC_ALLOC_ROOT_EXHAUSTED 6u
static inline uint64_t gc_try_tlab_allocate_into(MutatorContext *ctx,const AllocationInto *q){
    if(!q->allocation||(q->allocation&7)||!q->root||(q->root&7))return GC_ALLOC_INVALID;
    const AllocationRequest *r=(const AllocationRequest *)(uintptr_t)q->allocation;
    RootRequest *root=(RootRequest *)(uintptr_t)q->root;
    if(!r->output||(r->output&7))return GC_ALLOC_INVALID;
    uint64_t status=gc_root_step(ctx,root,GC_ROOT_PREPARE_EMPTY);
    if(status!=GC_ROOT_OK)return status==GC_ROOT_FLUSH?GC_ALLOC_ROOT_FLUSH:
        status==GC_ROOT_EXHAUSTED?GC_ALLOC_ROOT_EXHAUSTED:GC_ALLOC_INVALID;
    status=gc_try_tlab_allocate(ctx,r);
    if(status!=GC_ALLOC_OK)return status;
    /* Preflight plus exclusive owner/no poll means these fields cannot change.
     * Do not add another fallible operation after the allocation commits. */
    RootSegment *s=(RootSegment *)(uintptr_t)root->segment;
    BufferCursor *b=(BufferCursor *)(uintptr_t)s->journal;
    RootCell *cell=(RootCell *)(uintptr_t)root->handle.cell;
    uint64_t top=b->top,next=((AllocationResult *)(uintptr_t)r->output)->reference;
    uint64_t revision=atomic_load_explicit(&ctx->root_revision,memory_order_acquire);
    RootChange *entry=(RootChange *)(uintptr_t)b->entries+top%b->capacity;
    *entry=(RootChange){0,next,s->scope,atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)};
    root->value=next;b->top=top+1;
    atomic_store_explicit(&b->published,top+1,memory_order_release);
    atomic_store_explicit(&cell->value,next,memory_order_release);
    atomic_store_explicit(&ctx->root_revision,revision+1,memory_order_release);
    return GC_ALLOC_OK;
}
#endif
