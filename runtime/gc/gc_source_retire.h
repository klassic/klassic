/* Bounded heap-lock transaction AFTER complete source retirement proofs.
 * The owning cycle excludes every future reference into this entire range:
 * roots/heap/both queues/buffers/jobs/handles/weak/incoming forwarding, admission
 * and color lifetime. Zero metadata leases alone is NOT that proof. Payload is
 * already OS-uncommitted; allocator geometry/tails are immutable while owned.
 * BEGIN binds the unique stable request address, excludes new metadata leases,
 * and transfers bitmap/table cleanup ownership. Free backing outside the lock;
 * clear each output only after cleanup succeeds. Table backing has a separate
 * MetadataCharge, released by its owner. Bitmap charge stays until DONE.
 * STEP frees at most one tail and then (if last) the head. Freed tails may be
 * reserved again before the head finishes: the proof covers the WHOLE range
 * before BEGIN, and no later STEP inspects an already-freed prefix. No payload
 * credit is issued twice. All other resources/controls remain live until DONE.
 * No copying/replay/rewind of requests, heap access, OS calls or waits here.
 */
#ifndef KLASSIC_GC_SOURCE_RETIRE_H
#define KLASSIC_GC_SOURCE_RETIRE_H
#include "gc_abi.h"
enum { GC_SR_BEGIN=0,GC_SR_STEP=1 };
enum { GC_SR_DONE=0,GC_SR_INVALID=1,GC_SR_MORE=2,GC_SR_WAIT=3 };
typedef uint64_t (*SourceRetirer)(HeapPool *,SourceRetire *,uint64_t);
static inline uint64_t gc_source_retire_locked(HeapPool *h,SourceRetire *q,uint64_t op){
    if(op>1||!h||((uintptr_t)h&7)||!q||((uintptr_t)q&7)||!h->pages||(h->pages&7)||
        !h->page_count||h->page_count>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
        h->pages>UINT64_MAX-h->page_count*sizeof(AllocationPage)||(h->heap_base&15)||
        h->heap_base>UINT64_MAX-((h->page_count+1)<<GC_HEAP_PAGE_SHIFT)||
        !h->hard_max||h->hard_max>(h->page_count<<GC_HEAP_PAGE_SHIFT)||h->soft_max>h->hard_max||
        h->relocation_reserve>h->hard_max||h->committed>h->hard_max||h->pending>h->hard_max-h->committed||
        h->metadata_committed<h->page_count*sizeof(AllocationPage)||h->metadata_committed>h->metadata_max||
        h->metadata_pending>h->metadata_max-h->metadata_committed||!q->page_count||q->page_count>h->page_count||
        q->head_index>h->page_count-q->page_count||!q->incarnation||q->stage!=op||
        q->owner!=(op?(uintptr_t)h:0))return GC_SR_INVALID;
    uint64_t bitmap=q->page_count<<(GC_HEAP_PAGE_SHIFT-7);
    if(h->metadata_committed-h->page_count*sizeof(AllocationPage)<bitmap)return GC_SR_INVALID;
    AllocationPage *pages=(AllocationPage *)(uintptr_t)h->pages,*p=&pages[q->head_index];
    if(p->heap_base!=h->heap_base||p->run_head!=q->head_index||p->start!=((q->head_index+1)<<GC_HEAP_PAGE_SHIFT)||
        p->limit!=((q->head_index+q->page_count+1)<<GC_HEAP_PAGE_SHIFT)||p->incarnation!=q->incarnation||
        !p->identity_base||p->generation>GC_OLD||p->owner!=(op?(uintptr_t)q:0)||
        atomic_load_explicit(&p->state,memory_order_acquire)!=(op?GC_PAGE_RELEASING:GC_PAGE_UNCOMMITTED))return GC_SR_INVALID;
    if(p->metadata_leases)return GC_SR_WAIT;
    if(!op){
        if(q->cursor||q->starts||q->forward_table||!p->starts||(p->starts&7)||p->starts>UINT64_MAX-bitmap||
            p->starts_words!=bitmap/8||!p->forward_table||(p->forward_table&7))return GC_SR_INVALID;
        q->owner=(uintptr_t)h;q->stage=1;q->cursor=1;q->starts=p->starts;q->forward_table=p->forward_table;
        p->owner=(uintptr_t)q;p->starts=p->starts_words=p->forward_table=0;
        atomic_store_explicit(&p->state,GC_PAGE_RELEASING,memory_order_release);return GC_SR_MORE;
    }
    if(!q->cursor||q->cursor>q->page_count||p->starts||p->starts_words||p->forward_table)return GC_SR_INVALID;
    if(q->starts||q->forward_table)return GC_SR_WAIT;
    if(q->cursor<q->page_count){
        uint64_t index=q->head_index+q->cursor;AllocationPage *tail=&pages[index];
        if(tail->heap_base!=h->heap_base||tail->run_head!=q->head_index||tail->start!=((index+1)<<GC_HEAP_PAGE_SHIFT)||
            tail->limit!=p->limit||tail->incarnation!=q->incarnation||tail->identity_base!=p->identity_base||
            tail->generation!=p->generation||tail->owner||tail->metadata_leases||tail->starts||tail->starts_words||tail->forward_table||
            atomic_load_explicit(&tail->state,memory_order_acquire)!=GC_PAGE_RUN_TAIL)return GC_SR_INVALID;
        atomic_store_explicit(&tail->state,GC_PAGE_FREE,memory_order_release);q->cursor++;
        if(q->cursor<q->page_count)return GC_SR_MORE;
    }
    h->metadata_committed-=bitmap;p->owner=0;
    atomic_store_explicit(&p->state,GC_PAGE_FREE,memory_order_release);q->stage=2;return GC_SR_DONE;
}
#endif
