/* Heap-lock protected acquisition for a strongly held nonnull current or
 * forwarded-source reference. The page table and start bitmaps remain live
 * under that lock; page geometry cannot change while allocation owns a run.
 * Only the owning mutator changes starts/allocated_top, using release stores.
 * A metadata lease does not keep an object alive or authorize raw heap access.
 * This helper never dereferences the heap or resolves forwarding. The caller
 * must obey color/source retirement; a GcRef contains no incarnation and this
 * check cannot distinguish stale bits after forbidden premature address reuse.
 * All rejection paths preserve the pool, output and bitmap byte-for-byte.
 */
#ifndef KLASSIC_GC_REFERENCE_LEASE_H
#define KLASSIC_GC_REFERENCE_LEASE_H
#include "gc_metadata.h"
static inline uint64_t gc_reference_lease_locked(MetadataLeasePool *g,ReferenceLease *q) {
    if(q->lease.page||q->lease.incarnation||q->lease.token||!g->heap||(g->heap&7))return GC_ML_INVALID;
    uint64_t offset=q->reference>>16;GcRef encoded;
    if(!gc_abi_encode_ref(offset,(uint16_t)q->reference,&encoded))return GC_ML_INVALID;
    HeapPool *h=(HeapPool *)(uintptr_t)g->heap;
    if(!h->pages||(h->pages&7)||!h->page_count||h->page_count>=(GC_MAX_OFFSET>>GC_HEAP_PAGE_SHIFT)||
        h->pages>UINT64_MAX-h->page_count*sizeof(AllocationPage)||(h->heap_base&15)||
        h->heap_base>UINT64_MAX-((h->page_count+1)<<GC_HEAP_PAGE_SHIFT))return GC_ML_INVALID;
    uint64_t index=offset>>GC_HEAP_PAGE_SHIFT;
    if(!index||index>h->page_count)return GC_ML_INVALID;
    index--;
    AllocationPage *pages=(AllocationPage *)(uintptr_t)h->pages,*p=&pages[index];
    uint64_t head=p->run_head;
    if(head>index)return GC_ML_INVALID;
    AllocationPage *r=&pages[head];
    uint64_t state=atomic_load_explicit(&r->state,memory_order_acquire);
    if((state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED&&state!=GC_PAGE_EVACUATED&&state!=GC_PAGE_UNCOMMITTED)||r->run_head!=head||r->heap_base!=h->heap_base||
        r->start!=((head+1)<<GC_HEAP_PAGE_SHIFT)||r->limit<=r->start||
        r->limit>((h->page_count+1)<<GC_HEAP_PAGE_SHIFT)||(r->limit&(GC_HEAP_PAGE_BYTES-1))||
        !r->incarnation||r->generation>GC_OLD||!r->identity_base||
        p->heap_base!=h->heap_base||p->start!=((index+1)<<GC_HEAP_PAGE_SHIFT)||
        p->limit!=r->limit||p->incarnation!=r->incarnation||p->identity_base!=r->identity_base||p->generation!=r->generation||
        (index!=head&&atomic_load_explicit(&p->state,memory_order_acquire)!=GC_PAGE_RUN_TAIL))return GC_ML_INVALID;
    uint64_t top=atomic_load_explicit(&r->allocated_top,memory_order_acquire);
    if(top<r->start||top>r->limit||(top&15)||offset<r->start+sizeof(ObjectHeader)||offset>=top||
        !r->starts||(r->starts&7)||r->starts_words<((r->limit-r->start)>>10)||
        r->starts_words>UINT64_MAX/8||r->starts>UINT64_MAX-r->starts_words*8)return GC_ML_INVALID;
    uint64_t granule=(offset-r->start)>>4;
    _Atomic uint64_t *starts=(_Atomic uint64_t *)(uintptr_t)r->starts;
    if(!(atomic_load_explicit(&starts[granule>>6],memory_order_acquire)&(UINT64_C(1)<<(granule&63))))return GC_ML_INVALID;
    MetadataLeaseRequest lease={(uintptr_t)r,r->incarnation,0};
    uint64_t result=gc_metadata_step_locked(g,&lease,GC_ML_ACQUIRE);
    if(result==GC_ML_OK)q->lease=lease;
    return result;
}
#endif
