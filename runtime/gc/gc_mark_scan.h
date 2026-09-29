/* Caller holds an exclusive mark claim and a finite raw access guard for its
 * canonical receiver. Source/page/record/descriptor lifetimes are protected.
 * Native controls/output are live, disjoint and separate from heap payloads.
 * This leaf does NOT resolve forwarding or acquire those protections. Returned
 * references stay in protected worker storage until offered; only then may
 * the caller advance the job cursor. Optional locations is a disjoint native
 * capacity-element buffer receiving byte offsets parallel to output (including
 * null references). Unwritten entries stay untouched; zero disables it. These
 * scalar offsets survive polls, but a receiver must be resolved again before
 * using them for repair. No managed address survives this call. */
#ifndef KLASSIC_GC_MARK_SCAN_H
#define KLASSIC_GC_MARK_SCAN_H
#include "gc_descriptors.h"
static inline uint64_t gc_mark_scan_slice(MarkScan *s) {
    if(!s->job||(s->job&7)||!s->registry||(s->registry&7)||(s->heap_base&15))return GC_SCAN_INVALID;
    if(s->locations) {
        if((s->locations&7)||!s->capacity||s->capacity>GC_DESCRIPTOR_SCAN_MAX_SLOTS||
            s->locations>UINT64_MAX-s->capacity*8||s->output>UINT64_MAX-s->capacity*8||
            (s->locations<s->output+s->capacity*8&&s->output<s->locations+s->capacity*8))return GC_SCAN_INVALID;
    }
    MarkRequest *j=(MarkRequest *)(uintptr_t)s->job;
    GcRef encoded;uint64_t offset=j->reference>>16;
    if(!j->index||!j->serial||!j->record||(j->record&7)||!j->incarnation||!j->metadata_lease||
        !gc_abi_encode_ref(offset,(uint16_t)j->reference,&encoded)||offset<sizeof(ObjectHeader)||
        s->heap_base>UINT64_MAX-offset)return GC_SCAN_INVALID;
    uintptr_t user=s->heap_base+offset;
    ObjectHeader *h=(ObjectHeader *)(user-sizeof(ObjectHeader));
    if(atomic_load_explicit(&h->state,memory_order_acquire)!=GC_OBJECT_PUBLISHED)return GC_SCAN_INVALID;
    MarkRecord *record=(MarkRecord *)(uintptr_t)j->record;
    if(!record->object_id||h->mark_record!=j->record||user>UINT64_MAX-h->payload_bytes)return GC_SCAN_INVALID;
    DescriptorScan scan={h->descriptor_id,h->payload_bytes,j->cursor,s->capacity,s->output,s->capacity,0,0};
    uint64_t result=gc_descriptor_scan((DescriptorRegistry *)(uintptr_t)s->registry,&scan);
    if(result==GC_SCAN_INVALID)return result;
    uint64_t *out=(uint64_t *)(uintptr_t)s->output;
    uint64_t *locations=(uint64_t *)(uintptr_t)s->locations;
    for(uint64_t i=0;i<scan.written;i++){
        uint64_t offset=out[i];if(locations)locations[i]=offset;
        out[i]=atomic_load_explicit((_Atomic uint64_t *)(user+offset),memory_order_acquire);
    }
    s->next_cursor=scan.next_offset;s->written=scan.written;s->examined=scan.examined;
    return result;
}
#endif
