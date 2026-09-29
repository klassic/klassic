/* Finite readiness publication for an exclusively initialized Young allocation.
 * Reuses the guarded-mark envelope, but transfers neither lease nor output.
 * The caller owns the allocating page and initialization right, has completed
 * every field write, and retains registered strong roots for parent and children
 * plus unacknowledged publication work until normal marking handoff. Payload
 * references must obey the current barrier/phase contract; this leaf does not
 * initialize fields, mark children, discharge producers or permit early escape.
 * Native controls/metadata remain live and disjoint. No poll, lock or allocation.
 * Success release-publishes readiness; failure changes nothing. The guard and
 * metadata lease remain caller-owned on every return, and must be exited before
 * any wait. Completion after TLAB closure needs a separate ownership protocol.
 */
#ifndef KLASSIC_GC_INITIALIZATION_H
#define KLASSIC_GC_INITIALIZATION_H
#include "gc_mark_prepare.h"
#include "gc_allocation_shape.h"
#include <string.h>
#define GC_INIT_OK 0u
#define GC_INIT_INVALID 1u
static inline ObjectHeader *gc_initialization_owned_header(const GuardedMarkPrepare *q){
    if(!q||((uintptr_t)q&7))return NULL;
    MarkRecord *record;uint64_t state;
    if(!gc_mark_prepare_record_state(q,&record,true,&state)||state!=GC_OBJECT_INITIALIZING)return NULL;
    const SourceAccessRequest *s=(const SourceAccessRequest *)(uintptr_t)q->scope;
    const MutatorContext *ctx=(const MutatorContext *)(uintptr_t)s->context;
    const AllocationPage *p=(const AllocationPage *)(uintptr_t)q->page;
    const ForwardCell *cell=(const ForwardCell *)(uintptr_t)s->held;
    if(p->generation!=GC_YOUNG||p->owner!=ctx->thread_id||
        atomic_load_explicit(&p->state,memory_order_acquire)!=GC_PAGE_ALLOCATING||
        atomic_load_explicit(&cell->word,memory_order_acquire)||
        (atomic_load_explicit(&cell->access,memory_order_acquire)&GC_SOURCE_CLOSED))return NULL;
    ObjectHeader *h=(ObjectHeader *)(uintptr_t)(p->heap_base+s->source_offset-sizeof(ObjectHeader));
    return h;
}
static inline uint64_t gc_guarded_initialization_publish(const GuardedMarkPrepare *q){
    ObjectHeader *h=gc_initialization_owned_header(q);if(!h)return GC_INIT_INVALID;
    atomic_store_explicit(&h->state,GC_OBJECT_PUBLISHED,memory_order_release);
    return GC_INIT_OK;
}
/* One word only. Scalar/reference kind is checked against the descriptor.
 * Reference inputs are already canonical/store-good and strongly rooted by the
 * caller through publication/mark handoff. Only null reference slots may be
 * initialized: overwrites require the normal deletion barrier, not this leaf.
 * Scalars may be rewritten while privately owned. No readiness publication or
 * lease transfer occurs. Failure preserves all state. Byte/bulk writes need
 * their own bounded interface; this entry never writes beyond one word. */
static inline uint64_t gc_guarded_initialization_store(const GuardedMarkPrepare *q,const InitializationField *f){
    if(!f||((uintptr_t)f&7)||!f->registry||(f->registry&7)||(f->byte_offset&7)||f->is_reference>1)return GC_INIT_INVALID;
    ObjectHeader *h=gc_initialization_owned_header(q);
    if(!h||f->byte_offset>h->payload_bytes||h->payload_bytes-f->byte_offset<8)return GC_INIT_INVALID;
    uint64_t selected=UINT64_MAX;
    DescriptorScan scan={h->descriptor_id,h->payload_bytes,f->byte_offset,1,(uintptr_t)&selected,1,0,0};
    if(gc_descriptor_scan((void *)(uintptr_t)f->registry,&scan)==GC_SCAN_INVALID||((scan.written==1&&selected==f->byte_offset)!=(bool)f->is_reference))return GC_INIT_INVALID;
    _Atomic uint64_t *slot=(void *)((unsigned char *)(h+1)+f->byte_offset);
    if(f->is_reference&&(!gc_store_ref_valid(f->value)||
        (f->value&&(uint16_t)f->value!=gc_abi_color((PhaseSnapshot){q->phase}))||
        atomic_load_explicit(slot,memory_order_acquire)))return GC_INIT_INVALID;
    atomic_store_explicit(slot,f->value,memory_order_release);
    return GC_INIT_OK;
}
/* At most 256 bytes into an entirely scalar descriptor. Source is immutable,
 * live native storage, disjoint from the destination and runtime controls.
 * It is not permission to carry a managed address across a poll. Caller keeps
 * receiver rooted and reacquires a guard for every chunk. No progress cursor or
 * raw destination escapes; publication remains a separate operation. Empty
 * chunks need no source pointer. Reject before writing on every failure. */
static inline uint64_t gc_guarded_initialization_copy(const GuardedMarkPrepare *q,const InitializationBytes *f){
    if(!f||((uintptr_t)f&7)||f->bytes>256||(f->bytes&&!f->source)||f->source>UINT64_MAX-f->bytes)return GC_INIT_INVALID;
    ObjectHeader *h=gc_initialization_owned_header(q);
    if(!h||f->byte_offset>h->payload_bytes||f->bytes>h->payload_bytes-f->byte_offset)return GC_INIT_INVALID;
    AllocationRequest allocation={0};AllocationShape shape={h->descriptor_id,h->payload_bytes,(uintptr_t)&allocation};
    if(gc_allocation_shape((void *)(uintptr_t)f->registry,&shape)!=GC_ALLOC_OK)return GC_INIT_INVALID;
    const TypeDescriptor *d=(void *)(uintptr_t)allocation.descriptor;
    if(d->fixed_bitmap||d->element_bitmap||d->fixed_bitmap_words||d->element_bitmap_words)return GC_INIT_INVALID;
    if(f->bytes)memcpy((unsigned char *)(h+1)+f->byte_offset,(void *)(uintptr_t)f->source,(size_t)f->bytes);
    return GC_INIT_OK;
}
#endif
