/* One finite resolve -> descriptor candidate -> exit step outside data locks.
 * Caller retains a strong receiver (or the cycle's equivalent exclusive object
 * lifetime), its page metadata lease and immutable registry across all calls.
 * Request/control storage is nonmoving, unique and disjoint. Captured reference,
 * page, phase, range and cursor describe one object audit; changing any requires
 * a restart after cleanup. No function here polls, allocates or waits.
 *
 * DONE/MORE/FOUND preserve the incoming scanner's meaning. FORWARDED discards
 * the private destination and does NOT certify completion of this object; the
 * owner must visit its canonical key or restart its whole-heap audit. WAIT and
 * EXHAUSTED acquired no guard. INVALID never discards a newly acquired guard.
 * EXIT_REQUIRED retains stage 1 and the exact scope: immediately call again,
 * without polling, blocking or releasing metadata. Only after a successful EXIT
 * are cursor/observed committed. Completed steps clear scratch; idle rejection preserves preexisting scratch.
 * FOUND's observed bits are diagnostic, not rooted. Consume/clear before retry.
 * Whole-heap enumeration, phase/admission, liveness and retirement stay with the
 * owning cycle. This is not a snapshot or a page-reclamation certificate.
 */
#ifndef KLASSIC_GC_HEAP_AUDIT_SLICE_H
#define KLASSIC_GC_HEAP_AUDIT_SLICE_H
#include "gc_heap_incoming.h"
#include "gc_resolve.h"
enum { GC_HAS_DONE=0,GC_HAS_MORE=1,GC_HAS_INVALID=2,GC_HAS_FOUND=3,
    GC_HAS_FORWARDED=4,GC_HAS_WAIT=5,GC_HAS_EXHAUSTED=6,GC_HAS_EXIT_REQUIRED=7 };
typedef uint64_t (*HeapAuditSlicer)(HeapAuditSlice *);
static inline uint64_t gc_heap_audit_slice(HeapAuditSlice *q){
    if(!q||((uintptr_t)q&7)||q->stage>1)return GC_HAS_INVALID;
    if(!q->stage){
        ResolveRequest *a=&q->access;
        if(!q->page||(q->page&7)||q->observed||q->pending_cursor||q->pending_observed||q->pending_result||
           a->reference||a->destination||a->scope.context||a->scope.source_offset||a->scope.incarnation||a->scope.held||a->scope.ticket)return GC_HAS_INVALID;
        *a=(ResolveRequest){.reference=q->reference,.scope={.context=q->context}};
        uint64_t result=gc_resolve_access((const void *)(uintptr_t)q->page,a,GC_RESOLVE_ENTER);
        if(result!=GC_RESOLVE_OK){
            *a=(ResolveRequest){0};
            return result==GC_RESOLVE_FORWARDED?GC_HAS_FORWARDED:result==GC_RESOLVE_WAIT?GC_HAS_WAIT:
                result==GC_RESOLVE_EXHAUSTED?GC_HAS_EXHAUSTED:GC_HAS_INVALID;
        }
        q->stage=1;
        HeapIncoming scan={(uintptr_t)&a->scope,q->page,q->registry,q->phase,q->target_start,q->target_limit,q->cursor,0};
        q->pending_result=gc_heap_incoming(&scan);
        q->pending_cursor=scan.cursor;q->pending_observed=scan.observed;
    }
    if(!q->page||(q->page&7)||gc_resolve_access((const void *)(uintptr_t)q->page,&q->access,GC_RESOLVE_EXIT)!=GC_RESOLVE_OK)
        return GC_HAS_EXIT_REQUIRED;
    uint64_t result=q->pending_result;
    if(result==GC_HAS_DONE||result==GC_HAS_MORE||result==GC_HAS_FOUND){
        q->cursor=q->pending_cursor;q->observed=q->pending_observed;
    }else result=GC_HAS_INVALID;
    q->access=(ResolveRequest){0};q->stage=q->pending_cursor=q->pending_observed=q->pending_result=0;
    return result;
}
#endif
