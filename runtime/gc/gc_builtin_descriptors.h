/* Core allocation shapes for the generational compiler ABI. These IDs are
 * reserved within a registry; remaining compiler-defined shapes use other IDs.
 * Prepare only fresh, unpublished storage at its final address. The owner keeps
 * the entire package immutable/alive through registry and heap retirement.
 * IDs matching legacy tags do NOT make references or object headers compatible.
 */
#ifndef KLASSIC_GC_BUILTIN_DESCRIPTORS_H
#define KLASSIC_GC_BUILTIN_DESCRIPTORS_H
#include "gc_descriptors.h"
#define GC_DESC_RAW_BYTES 1u
#define GC_DESC_POINTER_RECORD 2u
#define GC_DESC_POINTER_LIST 4u
typedef struct {
    uint64_t pointer_bit;
    TypeDescriptor raw,record,list;
} BuiltinDescriptors;
_Static_assert(sizeof(BuiltinDescriptors)==200&&offsetof(BuiltinDescriptors,raw)==8&&
    offsetof(BuiltinDescriptors,record)==72&&offsetof(BuiltinDescriptors,list)==136,"builtin descriptor package ABI");
static inline void gc_builtin_descriptors_prepare(BuiltinDescriptors *d){
    d->pointer_bit=1;
    d->raw=(TypeDescriptor){.id=GC_DESC_RAW_BYTES,.alignment=16,.element_bytes=1};
    d->record=(TypeDescriptor){.id=GC_DESC_POINTER_RECORD,.alignment=8,.element_bytes=8,
        .element_bitmap=(uintptr_t)&d->pointer_bit,.element_bitmap_words=1};
    d->list=(TypeDescriptor){.id=GC_DESC_POINTER_LIST,.alignment=8,.fixed_bytes=8,.element_bytes=8,
        .element_bitmap=(uintptr_t)&d->pointer_bit,.element_bitmap_words=1};
}
#endif
