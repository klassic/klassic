/* Commit one successful scan receipt under the mark queue lock, WITHOUT a raw
 * guard. Caller retains the exclusive claim and immutable receipt, and has
 * protected/offered EVERY returned child before calling. The receipt must be
 * from that claim's most recent scan; validation cannot prove this provenance
 * or DONE against payload size without reacquiring a managed access guard.
 * Native controls are live and disjoint. Failure preserves all controls;
 * success consumes s->job, and YIELD transfers protection back to the queue
 * while DONE returns protection in the completed MarkRequest for release.
 * This does not certify whole-queue or whole-cycle completion. */
#ifndef KLASSIC_GC_MARK_SCAN_COMMIT_H
#define KLASSIC_GC_MARK_SCAN_COMMIT_H
#include "gc_mark.h"
#include "gc_mark_scan.h"
typedef uint64_t (*MarkScanCommitter)(MarkQueue *,MarkScan *,uint64_t);
static inline uint64_t gc_mark_scan_commit_locked(MarkQueue *g,MarkScan *s,uint64_t status){
    if(!g||((uintptr_t)g&7)||!s||((uintptr_t)s&7)||status>GC_SCAN_MORE||
        !s->job||(s->job&7)||!s->capacity||s->capacity>GC_DESCRIPTOR_SCAN_MAX_SLOTS||
        s->examined>s->capacity||s->written>s->examined)return GC_MARK_INVALID;
    MarkRequest *q=(MarkRequest *)(uintptr_t)s->job;
    uint64_t cursor=q->cursor;
    if(!q->index||!q->serial||s->next_cursor<cursor||
        (status==GC_SCAN_MORE&&(s->next_cursor==cursor||(s->next_cursor&7))))return GC_MARK_INVALID;
    q->cursor=s->next_cursor;
    uint64_t result=gc_mark_step_locked(g,q,status==GC_SCAN_MORE?GC_MARK_YIELD:GC_MARK_FINISH);
    if(result!=GC_MARK_OK)q->cursor=cursor;
    else s->job=0;
    return result;
}
#endif
