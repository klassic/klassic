/* Publish current mark/remap colors into the same retained owner root.
 * The owner preserves the root's LOGICAL referent from mark-set admission
 * through this call; canonical forwarding and physical recoloring are allowed.
 * A matching handle alone cannot prove that an intervening logical replacement
 * was marked. The caller also retains cycle admission/producer accounting and
 * the root, descriptor/record and page metadata lifetimes.
 *
 * access is a canonical RootAccess READY result for that root. Run outside
 * locks without polling; every return retains the guard and its metadata lease.
 * Only an exact CAS changes the root. Preserve remembered bits because physical
 * repair certifies no remset record. Root journals and logical revisions do not
 * change. CHANGED requires another guarded attempt, never an unconditional store.
 * Null is handled by the outer owner without a guard. No raw value escapes.
 */
#ifndef KLASSIC_GC_ROOTED_MARK_PUBLISH_H
#define KLASSIC_GC_ROOTED_MARK_PUBLISH_H
#include "gc_rooted_mark_set.h"
#include "gc_barrier.h"
typedef uint64_t (*RootedMarkPublisher)(const RootedMarkSet *,const RootAccess *);
static inline uint64_t gc_rooted_mark_publish(const RootedMarkSet *marks,const RootAccess *access){
    if(!marks||((uintptr_t)marks&7)||!access||((uintptr_t)access&7)||marks->state!=2||
        marks->context!=access->follow.context||marks->root.value||access->follow.root.value||
        marks->root.segment!=access->follow.root.segment||
        marks->root.handle.cell!=access->follow.root.handle.cell||
        marks->root.handle.reuse_generation!=access->follow.root.handle.reuse_generation||
        marks->root.handle.scope!=access->follow.root.handle.scope)return GC_HEAL_INVALID;
    uint64_t required=0;
    for(unsigned generation=0;generation<2;generation++){
        uint64_t phase=(marks->phase>>(generation*4))&15;
        if(phase>=GC_MARK_START&&phase<=GC_MARK_END_CHECK)required|=UINT64_C(1)<<generation;
    }
    if(marks->required!=required||marks->completed!=required||
        access->follow.cleanup.incarnation!=access->access.scope.incarnation||
        !(access->follow.cleanup.token&UINT32_MAX)||!(access->follow.cleanup.token>>32))return GC_HEAL_INVALID;
    MutatorContext *ctx;ObjectHeader *header;
    if(!gc_guarded_object((uintptr_t)&access->access.scope,access->follow.cleanup.page,marks->phase,&ctx,&header)||
        (uintptr_t)ctx!=marks->context)return GC_HEAL_INVALID;
    (void)header;
    const RootRequest *root=&marks->root;
    if(!root->segment||(root->segment&7))return GC_HEAL_INVALID;
    RootSegment *segment=(void *)(uintptr_t)root->segment;
    if(!gc_root_segment_valid(segment)||segment->owner!=ctx->thread_id||
        atomic_load_explicit(&segment->state,memory_order_acquire)!=GC_ROOT_SEGMENT_ACTIVE||
        segment->scope!=root->handle.scope)return GC_HEAL_INVALID;
    uint64_t used=atomic_load_explicit(&segment->used,memory_order_acquire);
    if(used>segment->capacity||root->handle.cell<segment->cells||
        root->handle.cell-segment->cells>=used*sizeof(RootCell)||
        (root->handle.cell-segment->cells)%sizeof(RootCell)||!root->handle.reuse_generation)return GC_HEAL_INVALID;
    RootCell *cell=(void *)(uintptr_t)root->handle.cell;
    if(!cell->active||cell->reuse_generation!=root->handle.reuse_generation)return GC_HEAL_INVALID;
    uint64_t observed=atomic_load_explicit(&cell->value,memory_order_acquire);
    if(!observed||!gc_store_ref_valid(observed)||(observed>>16)!=access->access.scope.source_offset)return GC_HEAL_INVALID;
    uint64_t desired=(observed&~UINT64_C(0xffff))|
        (gc_abi_color((PhaseSnapshot){marks->phase})&~UINT64_C(0x30))|(observed&0x30);
    return atomic_compare_exchange_strong_explicit(&cell->value,&observed,desired,memory_order_acq_rel,memory_order_acquire)?GC_HEAL_OK:GC_HEAL_CHANGED;
}
#endif
