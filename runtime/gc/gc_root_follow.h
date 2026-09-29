/* One rooted forwarding edge under the heap lock. The unique context owner
 * retains its RootHandle/segment and the heap/lease storage. All controls are
 * native/live/disjoint. No heap address is formed, no raw guard is entered,
 * and no allocation/poll/wait occurs. Unlock before waiting, flushing or poll.
 *
 * STEP rereads the root, acquires its source run metadata, reads forwarding,
 * and journals/stores a destination into the SAME root before releasing the
 * source lease. ADVANCED permits a poll: no private destination escapes.
 * FLUSH discards that destination; retry rereads the still-protected root.
 * DONE observes null/empty/retained forwarding, not immobility or marking;
 * a later heap access must still resolve and enter its source guard.
 * A chain is never traversed synchronously here. Each invocation is bounded.
 *
 * Acquisition consumes a token generation even when a later step fails; these
 * counters never roll back. All normally returning paths balance holders.
 * If an internal release invariant fails, cleanup retains the exact obligation
 * and CLEANUP_REQUIRED prevents reuse until CLEANUP succeeds. The caller must
 * not drop this request or reuse its lease token. Under the heap-lock/disjoint
 * ownership contract release cannot fail after successful acquire.
 * Source/color retirement and phase/mark integration remain separate duties.
 */
#ifndef KLASSIC_GC_ROOT_FOLLOW_H
#define KLASSIC_GC_ROOT_FOLLOW_H
#include "gc_roots.h"
#include "gc_reference_lease.h"
#include "gc_forward_table.h"
#include "gc_forward.h"
enum { GC_RF_STEP=0, GC_RF_CLEANUP=1 };
enum { GC_RF_DONE=0, GC_RF_INVALID=1, GC_RF_WAIT=2, GC_RF_ADVANCED=3, GC_RF_FLUSH=4,
    GC_RF_METADATA_FULL=5, GC_RF_EXHAUSTED=6, GC_RF_CLEANUP_REQUIRED=7 };
static inline uint64_t gc_root_follow_locked(MetadataLeasePool *g,RootFollow *q,uint64_t op){
    if(op>GC_RF_CLEANUP)return GC_RF_INVALID;
    if(op==GC_RF_CLEANUP)return gc_metadata_step_locked(g,&q->cleanup,GC_ML_RELEASE)==GC_ML_OK?GC_RF_DONE:GC_RF_CLEANUP_REQUIRED;
    if(q->cleanup.page||q->cleanup.incarnation||q->cleanup.token)return GC_RF_CLEANUP_REQUIRED;
    if(!q->context||(q->context&7)||q->root.value)return GC_RF_INVALID;
    MutatorContext *ctx=(MutatorContext *)(uintptr_t)q->context;RootRequest root=q->root;
    if(gc_root_step(ctx,&root,GC_ROOT_READ_RAW)!=GC_ROOT_OK)return GC_RF_INVALID;
    if(!root.value)return GC_RF_DONE;
    ReferenceLease source={.reference=root.value};uint64_t status=gc_reference_lease_locked(g,&source);
    if(status!=GC_ML_OK)return status==GC_ML_FULL?GC_RF_METADATA_FULL:status==GC_ML_EXHAUSTED?GC_RF_EXHAUSTED:GC_RF_INVALID;
    q->cleanup=source.lease;
    AllocationPage *page=(AllocationPage *)(uintptr_t)source.lease.page;
    uint64_t result=GC_RF_INVALID;
    if(page->forward_table&&!(page->forward_table&7)){
        ForwardLookup lookup={root.value>>16,source.lease.incarnation,0};
        if(gc_forward_lookup((ForwardTable *)(uintptr_t)page->forward_table,&lookup)==GC_FL_OK){
            ForwardRequest forward={lookup.source_offset,lookup.incarnation,0,0};
            status=gc_forward_step((ForwardCell *)(uintptr_t)lookup.cell,&forward,GC_FWD_READ);
            if(status==GC_FWD_BUSY)result=GC_RF_WAIT;
            else if(status==GC_FWD_EMPTY||status==GC_FWD_RETAINED)result=GC_RF_DONE;
            else if(status==GC_FWD_OK){
                root.value=forward.destination;status=gc_root_step(ctx,&root,GC_ROOT_STORE);
                result=status==GC_ROOT_OK?GC_RF_ADVANCED:status==GC_ROOT_FLUSH?GC_RF_FLUSH:status==GC_ROOT_EXHAUSTED?GC_RF_EXHAUSTED:GC_RF_INVALID;
            }
        }
    }
    if(gc_metadata_step_locked(g,&q->cleanup,GC_ML_RELEASE)!=GC_ML_OK)return GC_RF_CLEANUP_REQUIRED;
    return result;
}
#endif
