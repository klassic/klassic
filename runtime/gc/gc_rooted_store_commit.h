/* Finite commit from a retained, prepared desired root under a receiver guard.
 * The outer store owns phase/private-producer admission continuously through
 * preparation and commit, preserves the desired root's logical referent, and
 * retains receiver/observed roots, descriptors and distinct entry leases. The
 * RootedMarkSet receipt alone cannot prove an unchanged logical referent or
 * admission ownership. Prepared reference bits may forward again; only encoded
 * root bits are copied here, never dereferenced or kept across a poll.
 *
 * q.desired starts/stays zero: no unrooted desired snapshot escapes. Required
 * mark/remap colors must still match the receipt's adopted phase. REPREPARE
 * changes nothing and requires leaving the guard before repairing the root.
 * Remembered color is composed only for this actual logging/CAS attempt.
 * All GuardedStore statuses and lease-transfer semantics are preserved; a
 * losing physical CAS may have published conservative entries. Only the
 * transferred mask is copied back. Release only untransferred entry leases.
 * No polling, locking, allocation, root journal update or guard release here.
 */
#ifndef KLASSIC_GC_ROOTED_STORE_COMMIT_H
#define KLASSIC_GC_ROOTED_STORE_COMMIT_H
#include "gc_rooted_mark_set.h"
#include "gc_barrier.h"
#define GC_STORE_REPREPARE 6u
typedef uint64_t (*RootedStoreCommitter)(const RootedMarkSet *,GuardedStore *);
static inline uint64_t gc_rooted_store_commit(const RootedMarkSet *marks,GuardedStore *q){
    if(!marks||((uintptr_t)marks&7)||!q||((uintptr_t)q&7)||marks->state!=2||marks->root.value||
        q->desired||q->transferred||marks->phase!=q->phase)return GC_STORE_INVALID;
    uint64_t required=0;
    for(unsigned g=0;g<2;g++){uint64_t phase=(marks->phase>>(4*g))&15;if(phase>=GC_MARK_START&&phase<=GC_MARK_END_CHECK)required|=UINT64_C(1)<<g;}
    if(marks->required!=required||marks->completed!=required)return GC_STORE_INVALID;
    MutatorContext *ctx;ObjectHeader *header;
    if(!gc_guarded_object(q->scope,q->page,q->phase,&ctx,&header)||(uintptr_t)ctx!=marks->context||
        !atomic_load_explicit(&ctx->private_producers,memory_order_acquire))return GC_STORE_INVALID;
    (void)header;
    const RootRequest *root=&marks->root;
    if(!root->segment||(root->segment&7))return GC_STORE_INVALID;
    RootSegment *segment=(void *)(uintptr_t)root->segment;
    if(!gc_root_segment_valid(segment)||segment->owner!=ctx->thread_id||
        atomic_load_explicit(&segment->state,memory_order_acquire)!=GC_ROOT_SEGMENT_ACTIVE||segment->scope!=root->handle.scope)return GC_STORE_INVALID;
    uint64_t used=atomic_load_explicit(&segment->used,memory_order_acquire);
    if(used>segment->capacity||root->handle.cell<segment->cells||root->handle.cell-segment->cells>=used*sizeof(RootCell)||
        (root->handle.cell-segment->cells)%sizeof(RootCell)||!root->handle.reuse_generation)return GC_STORE_INVALID;
    RootCell *cell=(void *)(uintptr_t)root->handle.cell;
    if(!cell->active||cell->reuse_generation!=root->handle.reuse_generation)return GC_STORE_INVALID;
    uint64_t value=atomic_load_explicit(&cell->value,memory_order_acquire);
    if(!gc_store_ref_valid(value))return GC_STORE_INVALID;
    uint64_t color=gc_abi_color((PhaseSnapshot){marks->phase});
    if(value&&(value&UINT64_C(0xffcf))!=(color&~UINT64_C(0x30)))return GC_STORE_REPREPARE;
    GuardedStore commit=*q;commit.desired=value?(value&~UINT64_C(0xffff))|color:0;
    uint64_t result=gc_guarded_store(&commit);q->transferred=commit.transferred;return result;
}
#endif
