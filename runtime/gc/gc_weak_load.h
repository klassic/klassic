/* No-safepoint weak -> registered empty root publication. Caller holds the
 * common weak/phase/root AND heap locks (or one common lock). All native controls
 * are stable/disjoint; the context owner exclusively owns output_root. A live
 * weak slot owns its source metadata lease, but is NOT a strong mark producer.
 * Its serial cannot repeat. The admission's registry/handshake pair belongs to
 * this runtime. Slot/table storage cannot retire during this operation.
 *
 * Close the shared gate BEFORE the final strong-root/journal/queue audit, under
 * the same lock; earlier successful loads have published their keep-alive work.
 * Leave it closed throughout weak processing and clear every dead entry before
 * reopening. WAIT callers retain their handle and retry outside locks. This leaf
 * does not close/reopen the gate, determine liveness, scan weak entries, release
 * their leases, or certify mark termination. Those are the weak owner's duties.
 * No managed address or unrooted result escapes. Cleared/null entries leave the
 * already empty output root unchanged. FLUSH/EXHAUSTED have no publication.
 */
#ifndef KLASSIC_GC_WEAK_LOAD_H
#define KLASSIC_GC_WEAK_LOAD_H
#include "gc_roots.h"
#include "gc_metadata.h"
enum { GC_WEAK_LOADED=0,GC_WEAK_EMPTY=1,GC_WEAK_WAIT=2,GC_WEAK_INVALID=3,GC_WEAK_FLUSH=4,GC_WEAK_EXHAUSTED=5 };
static inline uint64_t gc_weak_load_into_locked(WeakLoadGate *gate,WeakLoadRequest *q,uint64_t op){
    if(op||!gate||((uintptr_t)gate&7)||!q||((uintptr_t)q&7)||gate->closed>1||
        !gate->pool||(gate->pool&7)||!q->admission||(q->admission&7)||!q->context||(q->context&7)||
        !q->slot||(q->slot&7)||!q->serial||!q->output_root||(q->output_root&7))return GC_WEAK_INVALID;
    if(gate->closed)return GC_WEAK_WAIT;
    RootedLoadAdmission *a=(void *)(uintptr_t)q->admission;
    if(!a->registry||(a->registry&7)||!a->handshake||(a->handshake&7)||!a->participant||(a->participant&7))return GC_WEAK_INVALID;
    if(a->registry!=gate->registry||a->handshake!=gate->handshake)return GC_WEAK_INVALID;
    RootRegistry *registry=(void *)(uintptr_t)a->registry;
    HandshakeState *h=(void *)(uintptr_t)a->handshake;
    ThreadRegistration *p=(void *)(uintptr_t)a->participant;
    MutatorContext *ctx=(void *)(uintptr_t)q->context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)||p->context!=q->context||p->registry!=a->handshake||
        p->registered!=1||p->identity!=ctx->thread_id||!h->members||!h->head||h->native>h->members||h->active>1||
        p->identity>h->last_identity||p->acknowledged_generation>h->generation||
        (h->active?(!h->generation||h->expected!=h->members-h->native||h->acknowledged>h->expected):(h->expected||h->acknowledged))||
        !gc_abi_valid_phase((PhaseSnapshot){registry->phase}))return GC_WEAK_INVALID;
    if(h->active||p->acknowledged_generation!=h->generation||atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=registry->phase)return GC_WEAK_WAIT;
    WeakSlot *slot=(void *)(uintptr_t)q->slot;
    if(slot->gate!=(uintptr_t)gate||slot->serial!=q->serial)return GC_WEAK_INVALID;
    RootRequest *output=(void *)(uintptr_t)q->output_root;
    if(output->value||!output->segment||(output->segment&7))return GC_WEAK_INVALID;
    RootSegment *segment=(void *)(uintptr_t)output->segment;
    if(!segment->registration||(segment->registration&7))return GC_WEAK_INVALID;
    RootRegistration *registration=(void *)(uintptr_t)segment->registration;
    if(registration->segment!=output->segment||registration->scope!=segment->scope||registration->context!=q->context||registration->registered!=1)return GC_WEAK_INVALID;
    RootRequest prepared=*output;
    uint64_t result=gc_root_step(ctx,&prepared,GC_ROOT_PREPARE_EMPTY);
    if(result!=GC_ROOT_OK)return result==GC_ROOT_FLUSH?GC_WEAK_FLUSH:result==GC_ROOT_EXHAUSTED?GC_WEAK_EXHAUSTED:GC_WEAK_INVALID;
    if(!slot->reference)return slot->lease.page||slot->lease.incarnation||slot->lease.token?GC_WEAK_INVALID:GC_WEAK_EMPTY;
    MetadataLeaseRequest lease=slot->lease;
    if(gc_metadata_step_locked((void *)(uintptr_t)gate->pool,&lease,GC_ML_RESOLVE)!=GC_ML_OK||
        lease.page!=slot->lease.page||lease.incarnation!=slot->lease.incarnation)return GC_WEAK_INVALID;
    uint64_t offset=slot->reference>>16;GcRef encoded;
    if(!gc_abi_encode_ref(offset,(uint16_t)slot->reference,&encoded))return GC_WEAK_INVALID;
    AllocationPage *page=(void *)(uintptr_t)lease.page;
    uint64_t top=atomic_load_explicit(&page->allocated_top,memory_order_acquire);
    if(page->start<GC_HEAP_PAGE_BYTES||page->limit<=page->start||page->limit>GC_MAX_OFFSET||
        ((page->start|page->limit)&(GC_HEAP_PAGE_BYTES-1))||top<page->start||top>page->limit||(top&15)||
        offset<page->start+sizeof(ObjectHeader)||offset>=top||!page->starts||(page->starts&7)||
        page->starts_words<((page->limit-page->start)>>10)||page->starts_words>UINT64_MAX/8||
        page->starts>UINT64_MAX-page->starts_words*8)return GC_WEAK_INVALID;
    uint64_t granule=(offset-page->start)>>4;
    if(!(atomic_load_explicit(&((_Atomic uint64_t *)(uintptr_t)page->starts)[granule>>6],memory_order_acquire)&(UINT64_C(1)<<(granule&63))))return GC_WEAK_INVALID;
    prepared.value=slot->reference;result=gc_root_step(ctx,&prepared,GC_ROOT_STORE);
    if(result!=GC_ROOT_OK)return result==GC_ROOT_FLUSH?GC_WEAK_FLUSH:result==GC_ROOT_EXHAUSTED?GC_WEAK_EXHAUSTED:GC_WEAK_INVALID;
    *output=prepared;return GC_WEAK_LOADED;
}
typedef uint64_t (*WeakLoader)(WeakLoadGate *,WeakLoadRequest *,uint64_t);
#endif
