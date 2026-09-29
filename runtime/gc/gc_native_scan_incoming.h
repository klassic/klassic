/* Incoming references in the runtime's registered NativeMarkScan inventory.
 * Hold the registry lock for BEGIN/NEXT and through consuming DONE, or recheck
 * NEXT after reacquiring it. NEXT also requires exclusive access to the current
 * control, obtained by worker ownership handoff BEFORE inspection. Do not read
 * a concurrently executing owner or wait for it under registry/heap locks.
 * This is an explicit synchronization precondition, not a lock-free snapshot.
 *
 * Before BEGIN, the cycle excludes introduction of target references behind
 * the cursor until DONE is consumed. Canonicalization away and ACK/removal are
 * allowed. Any attach/unlink changes the serial and forces a full restart.
 * Control storage may die after unlink: validate serial BEFORE borrowing it.
 * One NEXT checks at most one control (12 references + <=256 child slots).
 * No heap access, root traversal, allocation, waiting or repair occurs here.
 * FOUND stays at the same control; a finding cannot be skipped. It reports
 * diagnostic bits, not an independently rooted value. Guarded work returns
 * WAIT. DONE covers these controls only, not queues, roots, buffers, handles,
 * incoming forwarding, leases or complete source/color retirement proofs.
 */
#ifndef KLASSIC_GC_NATIVE_SCAN_INCOMING_H
#define KLASSIC_GC_NATIVE_SCAN_INCOMING_H
#include "gc_native_mark_scan.h"
enum { GC_NSI_DONE=0,GC_NSI_MORE=1,GC_NSI_FOUND=2,GC_NSI_WAIT=3,GC_NSI_RESTART=4,GC_NSI_INVALID=5 };
typedef struct {
    uint64_t self,serial,cursor,remaining,previous,stage,target_start,target_limit,observed;
    NativeRuntimeOwner *runtime;
} NativeScanIncoming;
static inline bool gc_native_scan_incoming_range(uint64_t start,uint64_t limit){
    return start&&start<limit&&limit<=GC_MAX_OFFSET&&!((start|limit)&15);
}
static inline uint64_t gc_native_scan_incoming_begin_locked(NativeRuntimeOwner *r,
        uint64_t start,uint64_t limit,NativeScanIncoming *q){
    if(!q||((uintptr_t)q&7)||q->self||!gc_native_cycle_valid(r,0)||
       !gc_native_scan_incoming_range(start,limit)||!!r->mark_scans!=!!r->mark_scan_count||
       r->mark_scan_count>r->mark_scan_serial)return GC_NSI_INVALID;
    *q=(NativeScanIncoming){.self=(uintptr_t)q,.serial=r->mark_scan_serial,.cursor=r->mark_scans,
        .remaining=r->mark_scan_count,.stage=1,.target_start=start,.target_limit=limit,.runtime=r};
    return GC_NSI_MORE;
}
static inline uint64_t gc_native_scan_incoming_next_locked(NativeScanIncoming *q){
    if(!q||((uintptr_t)q&7)||q->self!=(uintptr_t)q||(q->stage!=1&&q->stage!=2)||
       !gc_native_cycle_valid(q->runtime,0)||!gc_native_scan_incoming_range(q->target_start,q->target_limit))return GC_NSI_INVALID;
    NativeRuntimeOwner *r=q->runtime;
    if(q->serial!=r->mark_scan_serial)return GC_NSI_RESTART;
    if(!!q->cursor!=!!q->remaining||q->remaining>r->mark_scan_count)return GC_NSI_INVALID;
    if(!q->cursor){q->stage=2;q->observed=0;return GC_NSI_DONE;}
    if(q->stage!=1||(q->cursor&7))return GC_NSI_INVALID;
    NativeMarkScan *s=(void *)(uintptr_t)q->cursor;
    if(!gc_native_mark_scan_valid(s)||s->runtime!=r||s->previous!=q->previous||!s->identity||
       s->identity>q->serial||(!q->previous&&r->mark_scans!=q->cursor)||
       s->scan.written>s->budget||s->budget>GC_DESCRIPTOR_SCAN_MAX_SLOTS||
       s->child_index>s->scan.written||((s->next&7))||!!s->next!=(q->remaining>1))return GC_NSI_INVALID;
    if(s->stage==2||s->stage==3||s->receiver.access.scope.held||s->child.access.scope.held)return GC_NSI_WAIT;
    const uint64_t values[]={s->job.reference,s->view.reference,
        s->receiver.current.reference,s->receiver.access.reference,s->receiver.access.destination,
        s->receiver.route.prepare.reference,s->receiver.route.prepare.output.reference,
        s->child.current.reference,s->child.access.reference,s->child.access.destination,
        s->child.route.prepare.reference,s->child.route.prepare.output.reference};
    for(unsigned i=0;i<sizeof(values)/sizeof(*values);i++){
        uint64_t value=values[i];if(!gc_store_ref_valid(value))return GC_NSI_INVALID;
        if(value&&(value>>16)>=q->target_start&&(value>>16)<q->target_limit){q->observed=value;return GC_NSI_FOUND;}
    }
    /* Only a successful scan has published managed references to this buffer.
     * Guarded/failed reads cannot be certified by treating offsets as GcRefs. */
    if(s->stage>=4&&s->status<=GC_SCAN_MORE)for(uint64_t i=s->child_index;i<s->scan.written;i++){
        uint64_t value=s->children[i];if(!gc_store_ref_valid(value))return GC_NSI_INVALID;
        if(value&&(value>>16)>=q->target_start&&(value>>16)<q->target_limit){q->observed=value;return GC_NSI_FOUND;}
    }
    q->previous=q->cursor;q->cursor=s->next;q->remaining--;q->observed=0;
    return GC_NSI_MORE;
}
#endif
