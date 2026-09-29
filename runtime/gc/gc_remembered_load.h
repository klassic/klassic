/* Claim -> rooted unresolved referent, outside the registry lock. The unique
 * claim owner retains registration/set/job storage and source metadata; no ACK
 * or concurrent job use is allowed during this call. Registration epoch and
 * active claim fields cannot change until this owner ACKs; unrelated mutable
 * inventory counters/list links are deliberately not read here.
 *
 * source_owner is the original object containing the claimed slot, validated
 * from object-start/descriptor metadata before resolving the receiver. Caller
 * roots that owner and follows its forwarding to the canonical Old receiver
 * supplying load.scope/page. byte_offset stays relative to the SAME logical
 * owner across relocation. This leaf validates the claim/slot relation and
 * guarded descriptor access, but does not prove arbitrary owner retargeting.
 *
 * On success the field value lives in load.output's reserved root, including
 * null; no raw field address/value is returned. Claim and guard remain held.
 * EXIT before any journal flush, poll or wait; on FLUSH retain the claim and
 * retry the canonical field after making room. Keep the claim through rooted
 * referent resolution and Young mark handoff. Only that later handoff may ACK.
 */
#ifndef KLASSIC_GC_REMEMBERED_LOAD_H
#define KLASSIC_GC_REMEMBERED_LOAD_H
#include "gc_barrier.h"
static inline uint64_t gc_remembered_load_into(const RememberedLoad *q) {
    if(!q||((uintptr_t)q&7)||!q->registration||(q->registration&7)||!q->claim||(q->claim&7)||
       !q->source_owner||!gc_store_ref_valid(q->source_owner)||!q->load.page||(q->load.page&7))return GC_ROOT_INVALID;
    RememberedRegistration *r=(RememberedRegistration *)(uintptr_t)q->registration;
    RememberedJob *job=(RememberedJob *)(uintptr_t)q->claim;
    if(!r->registry||(r->registry&7)||!r->identity||!r->set||(r->set&7))return GC_ROOT_INVALID;
    RememberedSet *s=(RememberedSet *)(uintptr_t)r->set;
    if(!job->serial||job->serial!=s->active_serial||job->offset!=s->active_offset||
       job->incarnation!=s->incarnation||job->phase!=s->epoch||r->epoch!=s->epoch)return GC_ROOT_INVALID;
    uint64_t owner=q->source_owner>>16,offset=q->load.byte_offset;
    if(owner<s->start||owner>=s->limit||offset>=s->limit-owner||(offset&7)||owner+offset!=job->offset)return GC_ROOT_INVALID;
    if(!gc_abi_valid_phase((PhaseSnapshot){q->load.phase})||q->load.phase<job->phase||
       ((q->load.phase^job->phase)&0x1000)||(q->load.phase&15)<GC_MARK_START||
       (q->load.phase&15)>GC_MARK_END_CHECK||((AllocationPage *)(uintptr_t)q->load.page)->generation!=GC_OLD)return GC_ROOT_INVALID;
    return gc_guarded_load_into(&q->load);
}
#endif
