/* Resolve an ID and logical payload size without accessing the managed heap.
 * Registry/descriptors are immutable and retained by the owner. Query, request
 * and metadata are live, aligned, disjoint native storage. Success changes only
 * request.descriptor/elements; page/output/mark-arena remain owner inputs.
 * Failure changes nothing. This neither allocates nor reserves/publishes a root.
 */
#ifndef KLASSIC_GC_ALLOCATION_SHAPE_H
#define KLASSIC_GC_ALLOCATION_SHAPE_H
#include "gc_descriptors.h"
#include "gc_heap.h"
static inline uint64_t gc_allocation_shape(const DescriptorRegistry *registry,const AllocationShape *q){
    if(!registry||((uintptr_t)registry&7)||!q||((uintptr_t)q&7)||
        !gc_descriptor_registry_valid(registry)||!q->descriptor_id||q->descriptor_id>=registry->capacity||
        !q->allocation||(q->allocation&7)||q->payload_bytes>GC_MAX_OFFSET)return GC_ALLOC_INVALID;
    uint64_t address=atomic_load_explicit((_Atomic uint64_t *)(uintptr_t)registry->slots+q->descriptor_id,memory_order_acquire);
    if(!address||(address&7))return GC_ALLOC_INVALID;
    const TypeDescriptor *d=(void *)(uintptr_t)address;
    if(d->id!=q->descriptor_id||q->payload_bytes<d->fixed_bytes||d->element_bytes>GC_MAX_OFFSET)return GC_ALLOC_INVALID;
    uint64_t remaining=q->payload_bytes-d->fixed_bytes,elements=0;
    if(d->element_bytes){if(remaining%d->element_bytes)return GC_ALLOC_INVALID;elements=remaining/d->element_bytes;}
    else if(remaining)return GC_ALLOC_INVALID;
    AllocationRequest *r=(void *)(uintptr_t)q->allocation;
    r->descriptor=address;r->elements=elements;
    return GC_ALLOC_OK;
}
#endif
