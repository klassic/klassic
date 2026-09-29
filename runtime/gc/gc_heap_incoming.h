/* One bounded heap-reference audit slice under an existing canonical guard.
 * The owner retains the strong receiver, source metadata and immutable registry;
 * request/control storage is live and disjoint. No raw address escapes.
 * Cursor belongs to this exact logical object, descriptor, phase and range.
 * Restart after changing any of them. FOUND retains the matching cursor; clear
 * observed before retry, and root/re-resolve before attempting any repair.
 * observed is only diagnostic bits, NOT a strong reference or dereferenceable
 * address. No marking, healing or lease transfer is performed here.
 *
 * DONE covers this object's sampled slots only. Safe retirement also requires
 * complete heap enumeration and a protocol excluding introduction of references
 * into the retired range, plus roots/buffers/jobs/handles/forwarding audits.
 * Concurrent ordinary writes are atomic but do not create a snapshot. A source
 * guard alone does not establish those global conditions or permit reuse.
 */
#ifndef KLASSIC_GC_HEAP_INCOMING_H
#define KLASSIC_GC_HEAP_INCOMING_H
#include "gc_barrier.h"
enum { GC_HI_DONE=0, GC_HI_MORE=1, GC_HI_INVALID=2, GC_HI_FOUND=3 };
typedef uint64_t (*HeapIncomingScanner)(HeapIncoming *);
static inline uint64_t gc_heap_incoming(HeapIncoming *q){
    if(!q||((uintptr_t)q&7)||!q->registry||(q->registry&7)||q->observed||
       !q->target_start||q->target_start>=q->target_limit||
       q->target_limit>GC_MAX_OFFSET||((q->target_start|q->target_limit)&15))return GC_HI_INVALID;
    MutatorContext *ctx;ObjectHeader *header;
    if(!gc_guarded_object(q->scope,q->page,q->phase,&ctx,&header))return GC_HI_INVALID;
    uint64_t offset=0;
    DescriptorScan scan={header->descriptor_id,header->payload_bytes,q->cursor,1,(uintptr_t)&offset,1,0,0};
    uint64_t result=gc_descriptor_scan((const DescriptorRegistry *)(uintptr_t)q->registry,&scan);
    if(result==GC_SCAN_INVALID)return GC_HI_INVALID;
    if(scan.written){
        uint64_t value=atomic_load_explicit((_Atomic uint64_t *)((unsigned char *)(header+1)+offset),memory_order_acquire);
        if(!gc_store_ref_valid(value))return GC_HI_INVALID;
        if(value&&(value>>16)>=q->target_start&&(value>>16)<q->target_limit){
            q->cursor=offset;q->observed=value;return GC_HI_FOUND;
        }
    }
    q->cursor=scan.next_offset;return result==GC_SCAN_DONE?GC_HI_DONE:GC_HI_MORE;
}
#endif
