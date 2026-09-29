/* Compiler allocation entry for a fresh output handle. Native inputs are live,
 * aligned and disjoint; the owner exclusively controls context/TLAB/root/journal.
 * No poll, lock, OS call or managed payload initialization occurs here.
 * Resolve logical shape, reserve an empty root, then allocate into that root.
 * Failure after reservation cancels the empty cell without shrinking its scan
 * range. A retry needs a fresh slot; ROOT_FULL requests segment growth.
 * The request's descriptor/elements may change even on later failure. Success
 * leaves a rooted INITIALIZING object: the compiler must initialize and publish
 * before exposing it. Callers retain any previous handle before reusing q.root.
 */
#ifndef KLASSIC_GC_ALLOC_START_H
#define KLASSIC_GC_ALLOC_START_H
#include "gc_alloc_into.h"
#include "gc_allocation_shape.h"
#define GC_ALLOC_ROOT_FULL 7u
static inline uint64_t gc_try_allocate_start(MutatorContext *ctx,const AllocationStart *q){
    if(!ctx||((uintptr_t)ctx&7)||!q||((uintptr_t)q&7)||!q->root||(q->root&7))return GC_ALLOC_INVALID;
    uint64_t status=gc_allocation_shape((void *)(uintptr_t)q->registry,(void *)(uintptr_t)q->shape);
    if(status!=GC_ALLOC_OK)return status;
    RootRequest *root=(void *)(uintptr_t)q->root;
    status=gc_root_step(ctx,root,GC_ROOT_RESERVE);
    if(status!=GC_ROOT_OK)return status==GC_ROOT_FULL?GC_ALLOC_ROOT_FULL:
        status==GC_ROOT_EXHAUSTED?GC_ALLOC_ROOT_EXHAUSTED:GC_ALLOC_INVALID;
    const AllocationShape *shape=(void *)(uintptr_t)q->shape;
    AllocationInto into={shape->allocation,q->root};
    status=gc_try_tlab_allocate_into(ctx,&into);
    if(status!=GC_ALLOC_OK){
        /* The successful reserve plus exclusive no-poll ownership proves this
         * cell is still empty. Cancellation cannot fail under this contract. */
        RootCell *cell=(void *)(uintptr_t)root->handle.cell;
        cell->active=0;
    }
    return status;
}
#endif
