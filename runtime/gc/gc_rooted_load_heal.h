/* Exact self-heal of the original field after a counted load marks and repairs
 * its retained output root. observation.receiver is a fresh canonical guard;
 * output.value is the original rooted comparison token. Logical referents and
 * all source/forwarding metadata stay retained by the owner/cycle contract.
 * The result may forward again: this leaf only copies encoded rooted bits and
 * never dereferences the result. Future payload access still uses RootAccess.
 * No reference snapshot is carried from the previous result guard across polls.
 *
 * Every return keeps the receiver guard/lease. RETRY asks the outer owner to
 * exit/release and repair the result root again. CHANGED is successful load
 * completion: a competing field update wins, while the original result root
 * remains the return value. No journal, SATB/remset record or revision changes.
 */
#ifndef KLASSIC_GC_ROOTED_LOAD_HEAL_H
#define KLASSIC_GC_ROOTED_LOAD_HEAL_H
#include "gc_rooted_mark_publish.h"
#define GC_LOAD_HEAL_RETRY 3u
typedef uint64_t (*RootedLoadHealer)(const RootedLoad *);
static inline uint64_t gc_rooted_load_heal(const RootedLoad *q){
    if(!q||((uintptr_t)q&7)||q->admitted!=1||q->cancel||q->fault||q->observation.stage!=4||
        q->observation.result!=GC_ROOT_OK||!q->observation.output.value||!gc_store_ref_valid(q->observation.output.value))return GC_HEAL_INVALID;
    const RootedMarkSet *marks=&q->marks;const RootAccess *access=&q->observation.receiver;
    const RootRequest *root=&marks->root,*observed_root=&q->observation.output;
    if(marks->state!=2||marks->context!=access->follow.context||root->value||access->follow.root.value||
        root->segment!=observed_root->segment||root->handle.cell!=observed_root->handle.cell||
        root->handle.reuse_generation!=observed_root->handle.reuse_generation||root->handle.scope!=observed_root->handle.scope)return GC_HEAL_INVALID;
    uint64_t required=0;
    for(unsigned g=0;g<2;g++){uint64_t phase=(marks->phase>>(4*g))&15;if(phase>=GC_MARK_START&&phase<=GC_MARK_END_CHECK)required|=UINT64_C(1)<<g;}
    if(marks->required!=required||marks->completed!=required||
        access->follow.cleanup.incarnation!=access->access.scope.incarnation||
        !(access->follow.cleanup.token&UINT32_MAX)||!(access->follow.cleanup.token>>32))return GC_HEAL_INVALID;
    MutatorContext *ctx;ObjectHeader *header;
    if(!gc_guarded_object((uintptr_t)&access->access.scope,access->follow.cleanup.page,marks->phase,&ctx,&header)||
        (uintptr_t)ctx!=marks->context||!atomic_load_explicit(&ctx->private_producers,memory_order_acquire))return GC_HEAL_INVALID;
    (void)header;
    if(!root->segment||(root->segment&7))return GC_HEAL_INVALID;
    RootSegment *segment=(void *)(uintptr_t)root->segment;
    if(!gc_root_segment_valid(segment)||segment->owner!=ctx->thread_id||
        atomic_load_explicit(&segment->state,memory_order_acquire)!=GC_ROOT_SEGMENT_ACTIVE||segment->scope!=root->handle.scope)return GC_HEAL_INVALID;
    uint64_t used=atomic_load_explicit(&segment->used,memory_order_acquire);
    if(used>segment->capacity||root->handle.cell<segment->cells||root->handle.cell-segment->cells>=used*sizeof(RootCell)||
        (root->handle.cell-segment->cells)%sizeof(RootCell)||!root->handle.reuse_generation)return GC_HEAL_INVALID;
    RootCell *cell=(void *)(uintptr_t)root->handle.cell;
    if(!cell->active||cell->reuse_generation!=root->handle.reuse_generation)return GC_HEAL_INVALID;
    uint64_t value=atomic_load_explicit(&cell->value,memory_order_acquire);
    if(!value||!gc_store_ref_valid(value))return GC_HEAL_INVALID;
    uint64_t color=gc_abi_color((PhaseSnapshot){marks->phase})&~UINT64_C(0x30);
    if((value&UINT64_C(0xffcf))!=color)return GC_LOAD_HEAL_RETRY;
    GuardedHeal heal={.scope=(uintptr_t)&access->access.scope,.page=access->follow.cleanup.page,
        .registry=q->observation.registry,.byte_offset=q->observation.byte_offset,.phase=marks->phase,
        .expected=q->observation.output.value,.desired=(value&~UINT64_C(0x30))|(q->observation.output.value&0x30)};
    return gc_guarded_heal(&heal);
}
#endif
