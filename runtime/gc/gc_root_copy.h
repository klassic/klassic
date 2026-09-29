/* Same-owner root assignment. Both controls borrow live owner roots and remain
 * unchanged, including when source==destination. Their value members are not
 * inputs: reread the source cell on EVERY attempt. FLUSH leaves both roots and
 * the journal unchanged; the caller keeps both handles live while servicing
 * pressure/polling, then retries rather than retaining copied raw bits.
 * No poll, allocation, forwarding resolution or raw managed access here.
 * Context and request storage must be live and disjoint from root/journal
 * storage. Source and destination requests may alias each other. This handles
 * null/self assignment through the normal pre-mutation journal contract. */
#ifndef KLASSIC_GC_ROOT_COPY_H
#define KLASSIC_GC_ROOT_COPY_H
#include "gc_roots.h"
typedef uint64_t (*RootCopier)(MutatorContext *,const RootRequest *,const RootRequest *);
static inline uint64_t gc_root_copy(MutatorContext *ctx,const RootRequest *source,const RootRequest *destination){
    if(!ctx||((uintptr_t)ctx&7)||!source||((uintptr_t)source&7)||
        !destination||((uintptr_t)destination&7))return GC_ROOT_INVALID;
    RootRequest read=*source;
    uint64_t status=gc_root_step(ctx,&read,GC_ROOT_READ_RAW);
    if(status!=GC_ROOT_OK)return status;
    RootRequest write=*destination;write.value=read.value;
    return gc_root_step(ctx,&write,GC_ROOT_STORE);
}
#endif
