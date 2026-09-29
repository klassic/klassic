/* Native metadata backing adapter. The caller reserves a MetadataCharge under
 * the heap lock, leaves that lock (and any source guard), then prepares storage
 * here. Commit the charge before publishing any consumer. On failure, ABORT
 * only after cleanup succeeds. Disposal requires independent quiescence proof;
 * it never decides whether slots, jobs or roots still reference the mapping.
 * OS calls may block. Registered callers must obey the native-boundary protocol.
 */
#ifndef KLASSIC_GC_NATIVE_METADATA_H
#define KLASSIC_GC_NATIVE_METADATA_H
#include "gc_budget.h"
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#endif
#define GC_NATIVE_METADATA_OK 0u
#define GC_NATIVE_METADATA_INVALID 1u
#define GC_NATIVE_METADATA_OS_ERROR 2u
/* map returns quantum-aligned, zeroed, writable storage or NULL; unmap returns true on success.
 * Failed unmap MUST leave the mapping owned and usable for retry. */
static inline void *gc_native_metadata_map(size_t bytes){
#ifdef _WIN32
    return VirtualAlloc(NULL,bytes,MEM_RESERVE|MEM_COMMIT,PAGE_READWRITE);
#else
    void *p=mmap(NULL,bytes,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
    return p==MAP_FAILED?NULL:p;
#endif
}
static inline bool gc_native_metadata_unmap(void *p,size_t bytes){
#ifdef _WIN32
    (void)bytes;return VirtualFree(p,0,MEM_RELEASE)!=0;
#else
    return munmap(p,bytes)==0;
#endif
}
static inline NativeMetadataOps gc_native_metadata_ops(void){
#ifdef _WIN32
    SYSTEM_INFO info;GetSystemInfo(&info);uint64_t quantum=info.dwPageSize;
#else
    long size=sysconf(_SC_PAGESIZE);uint64_t quantum=size>0?(uint64_t)size:0;
#endif
    return (NativeMetadataOps){quantum,gc_native_metadata_map,gc_native_metadata_unmap};
}
/* Round before RESERVE so padding consumes metadata budget too. */
static inline uint64_t gc_native_metadata_size(uint64_t requested,uint64_t quantum,uint64_t *bytes){
    if(!bytes||!requested||quantum<8||(quantum&(quantum-1))||requested>UINT64_MAX-(quantum-1))
        return GC_NATIVE_METADATA_INVALID;
    *bytes=(requested+quantum-1)&~(quantum-1);return GC_NATIVE_METADATA_OK;
}
static inline bool gc_native_metadata_charge_valid(const MetadataCharge *q,const NativeMetadataOps *ops){
    return q&&!((uintptr_t)q&7)&&ops&&ops->map&&ops->unmap&&ops->quantum>=8&&!(ops->quantum&(ops->quantum-1))&&
        q->owner&&!(q->owner&7)&&q->bytes&&!(q->bytes&(ops->quantum-1));
}
static inline uint64_t gc_native_metadata_prepare(MetadataCharge *q,const NativeMetadataOps *ops){
    if(!gc_native_metadata_charge_valid(q,ops)||q->stage!=1||q->allocation)return GC_NATIVE_METADATA_INVALID;
    void *p=ops->map((size_t)q->bytes);
    if(!p)return GC_NATIVE_METADATA_OS_ERROR;
    q->allocation=(uintptr_t)p;return GC_NATIVE_METADATA_OK;
}
static inline uint64_t gc_native_metadata_dispose(MetadataCharge *q,const NativeMetadataOps *ops){
    if(!gc_native_metadata_charge_valid(q,ops)||(q->stage!=1&&q->stage!=2)||!q->allocation||
        (q->allocation&(ops->quantum-1)))return GC_NATIVE_METADATA_INVALID;
    if(!ops->unmap((void *)(uintptr_t)q->allocation,(size_t)q->bytes))return GC_NATIVE_METADATA_OS_ERROR;
    q->allocation=0;return GC_NATIVE_METADATA_OK;
}
#endif
