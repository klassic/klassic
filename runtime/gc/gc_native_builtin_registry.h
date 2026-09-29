/* Private native descriptor-registry bootstrap. RESERVE the rounded charge
 * under the heap lock, then prepare outside locks/guards in native state.
 * Only successful preparation followed by budget COMMIT permits publication.
 * Partial registration remains private: dispose backing, then ABORT its charge.
 * Failed cleanup retains the mapping/charge for retry. Once published, the
 * owner must retire every heap/job user before disposal and budget RELEASE.
 */
#ifndef KLASSIC_GC_NATIVE_BUILTIN_REGISTRY_H
#define KLASSIC_GC_NATIVE_BUILTIN_REGISTRY_H
#include "gc_native_metadata.h"
#include "gc_builtin_descriptors.h"
typedef uint64_t (*NativeBuiltinRegistryPreparer)(MetadataCharge *,uint64_t);
typedef struct {
    DescriptorRegistry registry;
    BuiltinDescriptors builtins;
    _Atomic uint64_t slots[];
} NativeBuiltinRegistry;
_Static_assert(sizeof(NativeBuiltinRegistry)==216&&offsetof(NativeBuiltinRegistry,builtins)==16,"native builtin registry ABI");
static inline uint64_t gc_native_builtin_registry_size(uint64_t capacity,uint64_t quantum,uint64_t *bytes){
    if(capacity<5||capacity>UINT64_C(0x100000000))return GC_NATIVE_METADATA_INVALID;
    return gc_native_metadata_size(sizeof(NativeBuiltinRegistry)+capacity*8,quantum,bytes);
}
static inline uint64_t gc_native_builtin_registry_prepare(MetadataCharge *charge,uint64_t capacity,
        const NativeMetadataOps *ops,void (*prepare)(BuiltinDescriptors *),
        uint64_t (*reg)(const DescriptorRegistry *,const TypeDescriptor *)){
    uint64_t bytes=0;
    if(!charge||((uintptr_t)charge&7)||!ops||!prepare||!reg||
        gc_native_builtin_registry_size(capacity,ops->quantum,&bytes)!=GC_NATIVE_METADATA_OK||charge->bytes<bytes)
        return GC_NATIVE_METADATA_INVALID;
    uint64_t status=gc_native_metadata_prepare(charge,ops);
    if(status!=GC_NATIVE_METADATA_OK)return status;
    NativeBuiltinRegistry *storage=(void *)(uintptr_t)charge->allocation;
    storage->registry=(DescriptorRegistry){(uintptr_t)storage->slots,capacity};
    for(uint64_t i=0;i<capacity;i++)atomic_init(&storage->slots[i],0);
    prepare(&storage->builtins);
    const TypeDescriptor *shapes[]={&storage->builtins.raw,&storage->builtins.record,&storage->builtins.list};
    for(unsigned i=0;i<3;i++)if(reg(&storage->registry,shapes[i])!=GC_DESC_OK)return GC_NATIVE_METADATA_INVALID;
    return GC_NATIVE_METADATA_OK;
}
#endif
