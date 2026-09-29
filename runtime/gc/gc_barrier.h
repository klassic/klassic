/* One bounded physical store attempt inside an already acquired canonical
 * source guard. Caller retains receiver/new/expected strong references, page
 * metadata, descriptor registry and each prepared entry lease. Desired is
 * already canonical, marked as required and store-good for adopted phase.
 * Entries use distinct owner-metadata lease tokens; their previous referents
 * remain protected by phase/buffer lifetime until consumer processing finishes.
 * Native request/scope/ring storage is live and disjoint; each ring has one
 * context producer and one consumer. Page geometry/generation is stable.
 * No pointer/guard may cross a poll, flush, allocation or worker wait here.
 *
 * This is NOT the public logical-reference CAS: exact expected bits detect any
 * intervening store/heal. A mismatch asks the outer barrier to release its
 * guard and re-resolve/reprepare from protected roots. It returns no observed
 * unrooted value. All checks precede entry publication; then logging precedes
 * acq_rel CAS. Losing that CAS leaves conservative entries in the buffers.
 * transferred reports lease ownership handed to SATB(1)/remembered(2), even on
 * RETRY. Only untransferred input leases may be released by the caller.
 * FULL/EXHAUSTED/INVALID preserve all state. Full must be handled after EXIT.
 * Phase adoption/termination must include this producer and its buffer work;
 * publishing an entry alone is not a mark-termination certificate.
 */
#ifndef KLASSIC_GC_BARRIER_H
#define KLASSIC_GC_BARRIER_H
#include "gc_buffers.h"
#include "gc_descriptors.h"
#include "gc_roots.h"
enum { GC_STORE_OK=0, GC_STORE_INVALID=1, GC_STORE_FULL=2, GC_STORE_EXHAUSTED=3, GC_STORE_RETRY=4, GC_STORE_MISMATCH=5 };
static inline bool gc_store_ref_valid(uint64_t v){GcRef checked;return !v||gc_abi_encode_ref(v>>16,(uint16_t)v,&checked);}
/* Internal shared object validation. Addresses remain local to the finite
 * leaf under its existing scope; neither escapes through ABI. */
static inline bool gc_guarded_object_state(uint64_t scope,uint64_t page,uint64_t phase,bool allow_initializing,
        MutatorContext **context,ObjectHeader **header,uint64_t *object_state){
    if(!scope||(scope&7)||!page||(page&7)||
        !gc_abi_valid_phase((PhaseSnapshot){phase}))return false;
    SourceAccessRequest *s=(SourceAccessRequest *)(uintptr_t)scope;
    if(!s->context||(s->context&7)||!s->held||(s->held&7)||s->ticket||!s->incarnation)return false;
    MutatorContext *ctx=(MutatorContext *)(uintptr_t)s->context;
    if(ctx->abi_version!=GC_ABI_VERSION||!ctx->thread_id||atomic_load_explicit(&ctx->state,memory_order_acquire)!=GC_RUNNING||
        atomic_load_explicit(&ctx->access_epoch,memory_order_acquire)!=s->incarnation||
        atomic_load_explicit(&ctx->adopted_phase,memory_order_acquire)!=phase)return false;
    ForwardCell *cell=(ForwardCell *)(uintptr_t)s->held;
    if(cell->source_offset!=s->source_offset||cell->incarnation!=s->incarnation||
        !(atomic_load_explicit(&cell->access,memory_order_acquire)&~GC_SOURCE_CLOSED))return false;
    // A claim/selection may start while this prior entrant still holds its guard.
    // A drained/published source cannot legitimately have this live scope.
    uint64_t word=atomic_load_explicit(&cell->word,memory_order_acquire);
    if(word&&word!=3&&((word&3)!=1||!(word>>2)))return false;
    AllocationPage *p=(AllocationPage *)(uintptr_t)page;
    uint64_t state=atomic_load_explicit(&p->state,memory_order_acquire),top=atomic_load_explicit(&p->allocated_top,memory_order_acquire);
    uint64_t offset=s->source_offset;
    if((state!=GC_PAGE_ALLOCATING&&state!=GC_PAGE_CLOSED)||p->incarnation!=s->incarnation||p->generation>GC_OLD||
        !p->start||p->start>=p->limit||p->limit>GC_MAX_OFFSET||((p->heap_base|p->start|p->limit|top|offset)&15)||
        p->heap_base>UINT64_MAX-p->limit||top<p->start||top>p->limit||offset<p->start+32||offset>=top)return false;
    ObjectHeader *h=(ObjectHeader *)(uintptr_t)(p->heap_base+offset-32);
    uint64_t ready=atomic_load_explicit(&h->state,memory_order_acquire);
    if((ready!=GC_OBJECT_PUBLISHED&&(!allow_initializing||ready!=GC_OBJECT_INITIALIZING))||h->payload_bytes>top-offset)return false;
    *context=ctx;*header=h;*object_state=ready;return true;
}
static inline bool gc_guarded_object(uint64_t scope,uint64_t page,uint64_t phase,
        MutatorContext **context,ObjectHeader **header){
    uint64_t state;return gc_guarded_object_state(scope,page,phase,false,context,header,&state);
}
/* Read identity only under a live canonical object guard. The returned native
 * record remains borrowed: caller retains the referent's strong root and a
 * metadata lease before leaving the guard and using it for logical comparison.
 * Zero denotes invalid access/metadata, never the identity of a managed object.
 * Null references are handled by the outer rooted barrier without entering here.
 * No heap address escapes, mutation, allocation, polling or lock acquisition.
 * Native scope/page/record storage must be live for this finite operation. */
typedef const MarkRecord *(*GuardedIdentity)(uint64_t,uint64_t,uint64_t);
static inline const MarkRecord *gc_guarded_identity(uint64_t scope,uint64_t page,uint64_t phase){
    MutatorContext *ctx;ObjectHeader *header;
    if(!gc_guarded_object(scope,page,phase,&ctx,&header)||
        !header->mark_record||(header->mark_record&7))return NULL;
    const MarkRecord *record=(const void *)(uintptr_t)header->mark_record;
    return record->object_id?record:NULL;
}
/* Published canonical allocation size, including header and minimum 16-byte
 * payload storage. Zero means invalid access, never a valid allocation size.
 * Only a scalar escapes; no heap pointer, polling, mutation or allocation.
 * The caller retains its scope and lease on every return. This is physical
 * block size, not logical payload bytes, liveness or a reclaimability proof. */
typedef uint64_t (*GuardedBlockSize)(uint64_t,uint64_t,uint64_t);
static inline uint64_t gc_guarded_block_size(uint64_t scope,uint64_t page,uint64_t phase){
    MutatorContext *ctx;ObjectHeader *header;
    if(!gc_guarded_object(scope,page,phase,&ctx,&header))return 0;
    /* Guard validation bounds payload by the aligned page top below MAX_OFFSET. */
    uint64_t rounded=(header->payload_bytes+15)&~UINT64_C(15);
    return sizeof(ObjectHeader)+(rounded?rounded:16);
}
static inline bool gc_guarded_slot_kind(uint64_t scope,uint64_t page,uint64_t registry,uint64_t byte_offset,uint64_t phase,bool reference,
        MutatorContext **context,_Atomic uint64_t **address){
    ObjectHeader *h;
    if(!registry||(registry&7)||(byte_offset&7)||!gc_guarded_object(scope,page,phase,context,&h)||
        byte_offset>h->payload_bytes||h->payload_bytes-byte_offset<8)return false;
    uint64_t selected=UINT64_MAX;
    DescriptorScan scan={h->descriptor_id,h->payload_bytes,byte_offset,1,(uintptr_t)&selected,1,0,0};
    if(gc_descriptor_scan((DescriptorRegistry *)(uintptr_t)registry,&scan)==GC_SCAN_INVALID||((scan.written==1&&selected==byte_offset)!=reference))return false;
    *address=(_Atomic uint64_t *)((unsigned char *)(h+1)+byte_offset);return true;
}
static inline bool gc_guarded_slot(uint64_t scope,uint64_t page,uint64_t registry,uint64_t byte_offset,uint64_t phase,
        MutatorContext **context,_Atomic uint64_t **address){
    return gc_guarded_slot_kind(scope,page,registry,byte_offset,phase,true,context,address);
}
#define GC_SCALAR_LOAD 0u
#define GC_SCALAR_STORE 1u
#define GC_SCALAR_OK 0u
#define GC_SCALAR_INVALID 1u
/* Scalar values never acquire GC reference semantics, including pointer-shaped
 * bits. Caller retains a canonical guard and metadata/descriptor lifetimes for
 * this entire finite access. Published objects only. No poll/lock/allocation.
 * LOAD changes only q.value; STORE changes only the payload word. INVALID is
 * nonmutating. Source guard prevents relocation copy from racing a scalar store;
 * acquire/release atomics also order concurrent scalar readers/writers. */
static inline uint64_t gc_guarded_scalar(GuardedScalar *q,uint64_t op){
    if(!q||((uintptr_t)q&7)||op>GC_SCALAR_STORE)return GC_SCALAR_INVALID;
    MutatorContext *ctx;_Atomic uint64_t *slot;
    if(!gc_guarded_slot_kind(q->scope,q->page,q->registry,q->byte_offset,q->phase,false,&ctx,&slot))return GC_SCALAR_INVALID;
    if(op==GC_SCALAR_LOAD)q->value=atomic_load_explicit(slot,memory_order_acquire);
    else atomic_store_explicit(slot,q->value,memory_order_release);
    return GC_SCALAR_OK;
}
static inline uint64_t gc_guarded_store_impl(GuardedStore *q,const MarkRecord *expected_record,const MarkRecord *observed_record,bool logical){
    if(logical&&(!q||((uintptr_t)q&7)))return GC_STORE_INVALID;
    if(!q->scope||(q->scope&7)||!q->page||(q->page&7)||!q->registry||(q->registry&7)||q->transferred||
        (q->byte_offset&7)||!gc_store_ref_valid(q->expected)||!gc_store_ref_valid(q->desired))return GC_STORE_INVALID;
    PhaseSnapshot phase={q->phase};
    if(!gc_abi_valid_phase(phase)||(q->desired&&(uint16_t)q->desired!=gc_abi_color(phase)))return GC_STORE_INVALID;
    MutatorContext *ctx;_Atomic uint64_t *slot;
    if(!gc_guarded_slot(q->scope,q->page,q->registry,q->byte_offset,q->phase,&ctx,&slot))return GC_STORE_INVALID;
    if(logical){
        const MarkRecord *records[]={expected_record,observed_record};
        for(unsigned i=0;i<2;i++)if(records[i]&&(((uintptr_t)records[i]&7)||!records[i]->object_id))return GC_STORE_INVALID;
        if(!!q->expected!=!!observed_record)return GC_STORE_INVALID;
        if(atomic_load_explicit(slot,logical?memory_order_seq_cst:memory_order_acquire)!=q->expected)return GC_STORE_RETRY;
        if(expected_record!=observed_record)return GC_STORE_MISMATCH;
    }
    SourceAccessRequest *s=(SourceAccessRequest *)(uintptr_t)q->scope;
    AllocationPage *p=(AllocationPage *)(uintptr_t)q->page;
    uint64_t offset=s->source_offset;
    uint64_t young=q->phase&15,old=(q->phase>>4)&15;
    uint64_t mask=(q->expected&&((young>=GC_MARK_START&&young<=GC_MARK_END_CHECK)||(old>=GC_MARK_START&&old<=GC_MARK_END_CHECK)))?1:0;
    if(p->generation==GC_OLD)mask|=2; // every Old slot, including null -> null
    if(mask==3&&q->satb.slot.metadata_lease==q->remembered.slot.metadata_lease)return GC_STORE_INVALID;
    BufferCursor *buffers[2]={&ctx->satb,&ctx->remembered};
    const BarrierEntry *entries[2]={&q->satb,&q->remembered};
    for(unsigned i=0;i<2;i++)if(mask&(UINT64_C(1)<<i)){
        const BarrierEntry *e=entries[i];BufferCursor *b=buffers[i];
        if(e->previous.bits!=q->expected||!e->slot.owner.bits||!gc_store_ref_valid(e->slot.owner.bits)||
            (e->slot.owner.bits>>16)!=offset||e->slot.byte_offset!=q->byte_offset||e->slot.page_incarnation!=p->incarnation||
            e->slot.record_epoch!=q->phase||!e->slot.metadata_lease||!gc_barrier_buffer_valid(b))return GC_STORE_INVALID;
        uint64_t published=atomic_load_explicit(&b->published,memory_order_acquire),consumed=atomic_load_explicit(&b->consumed,memory_order_acquire);
        if(published>b->top||consumed>published||b->top-consumed>b->capacity)return GC_STORE_INVALID;
        if(b->top==UINT64_MAX)return GC_STORE_EXHAUSTED;
        if(b->top-consumed==b->capacity)return GC_STORE_FULL;
    }
    uint64_t expected=q->expected;
    if(atomic_load_explicit(slot,logical?memory_order_seq_cst:memory_order_acquire)!=expected)return GC_STORE_RETRY;
    for(unsigned i=0;i<2;i++)if(mask&(UINT64_C(1)<<i)){
        BufferCursor *b=buffers[i];((BarrierEntry *)(uintptr_t)b->entries)[b->top%b->capacity]=*entries[i];
        b->top++;atomic_store_explicit(&b->published,b->top,memory_order_release);
    }
    q->transferred=mask;
    return atomic_compare_exchange_strong_explicit(slot,&expected,q->desired,logical?memory_order_seq_cst:memory_order_acq_rel,logical?memory_order_seq_cst:memory_order_acquire)?GC_STORE_OK:GC_STORE_RETRY;
}
static inline uint64_t gc_guarded_store(GuardedStore *q){
    return gc_guarded_store_impl(q,NULL,NULL,false);
}
/* Prepared logical CAS commit, not the complete public rooted CAS barrier.
 * Caller has resolved the logical expected reference and the rooted observation
 * into their unique nonmoving MarkRecords, retains both strong references and
 * record lifetimes, and proves observed_record belongs to q->expected's exact
 * observed bits. Null uses a null record. Records cannot be reused while these
 * protections exist; relocated copies share a record. Compare record identity,
 * NEVER debug ObjectId values or colored/reference offsets alone.
 * MISMATCH linearizes at a SeqCst read and logs nothing. Successful CAS and
 * failed physical observations are also SeqCst. This deliberately provides
 * the strongest language ordering; GC journal publication is separate. Equal
 * identities proceed through the ordinary pre-barrier/physical CAS, preserving
 * SATB/remset lease-transfer semantics even on RETRY. No observed value escapes.
 * RETRY requires a new rooted observation/identity preparation by the caller. */
typedef uint64_t (*GuardedLogicalCas)(GuardedStore *,const MarkRecord *,const MarkRecord *);
static inline uint64_t gc_guarded_compare_exchange(GuardedStore *q,const MarkRecord *expected_record,const MarkRecord *observed_record){
    return gc_guarded_store_impl(q,expected_record,observed_record,true);
}
/* Acquire unresolved slot bits into an already reserved RootHandle before
 * releasing the source guard. Root journal publication protects both overwritten
 * output and new value. Outer barriers subsequently resolve/mark/heal the rooted
 * result; a good color alone is never inferred here. Failure preserves request,
 * roots/journal and heap, returns no private value, and keeps the source scope.
 * On FLUSH the caller EXITs, flushes outside its guard and rereads the field.
 * No input/output root cell may alias native controls or the managed heap. */
static inline uint64_t gc_guarded_load_impl(const GuardedLoad *q,uint64_t *observed){
    MutatorContext *ctx;_Atomic uint64_t *slot;
    if(q->output.value||!gc_guarded_slot(q->scope,q->page,q->registry,q->byte_offset,q->phase,&ctx,&slot))return GC_ROOT_INVALID;
    RootRequest out=q->output;out.value=atomic_load_explicit(slot,memory_order_acquire);
    if(!gc_store_ref_valid(out.value))return GC_ROOT_INVALID;
    uint64_t status=gc_root_store_guarded(ctx,&out);
    if(status==GC_ROOT_OK&&observed)*observed=out.value;
    return status;
}
static inline uint64_t gc_guarded_load_into(const GuardedLoad *q){
    return gc_guarded_load_impl(q,NULL);
}
/* Rooted exact observation for CAS preparation. q.output.value must start at
 * zero; only successful root publication replaces it with the observed bits.
 * Failure leaves the entire request unchanged. The bits are a comparison token,
 * not a canonical reference or a managed address; retain the output root while
 * resolving its identity. Forwarding may update that root without changing the
 * token. A retry clears this scratch value and obtains a NEW rooted observation.
 * The output handle must not alias receiver/expected/desired roots whose values
 * the caller needs preserved. This leaf neither resolves nor marks the referent. */
typedef uint64_t (*GuardedObserver)(GuardedLoad *);
static inline uint64_t gc_guarded_observe_into(GuardedLoad *q){
    if(!q||((uintptr_t)q&7))return GC_ROOT_INVALID;
    return gc_guarded_load_impl(q,&q->output.value);
}
/* Finite collector repair, not a logical mutator store. Caller proves expected
 * and desired denote the SAME nonnull object, retains its strong protection
 * and all resolution metadata, and marks it as needed before this call.
 * Receiver guard and adopted phase remain valid throughout. No poll, lock,
 * allocation, barrier log or raw pointer escapes. Exact-bit CAS preserves any
 * intervening logical store; CHANGED returns no unrooted observed reference.
 * The outer load still returns its protected original referent, not a reread.
 * Request/scope/native controls are live and disjoint from the payload.
 * Remembered bits must stay as observed: physical repair cannot certify a
 * remset record. Only mark/remap colors are updated to the adopted phase.
 * Selection may close admission while this prior guard remains: copy must
 * still wait for its EXIT. This leaf is not a forwarding resolver or retirement
 * certificate. Null is handled by the outer load, not healed to an object. */
enum { GC_HEAL_OK=0, GC_HEAL_INVALID=1, GC_HEAL_CHANGED=2 };
static inline uint64_t gc_guarded_heal(const GuardedHeal *q){
    if(!q->expected||!q->desired||!gc_store_ref_valid(q->expected)||!gc_store_ref_valid(q->desired)||
        !gc_abi_valid_phase((PhaseSnapshot){q->phase})||
        (uint16_t)q->desired!=((gc_abi_color((PhaseSnapshot){q->phase})&~0x30u)|(q->expected&0x30u)))return GC_HEAL_INVALID;
    MutatorContext *ctx;_Atomic uint64_t *slot;
    if(!gc_guarded_slot(q->scope,q->page,q->registry,q->byte_offset,q->phase,&ctx,&slot))return GC_HEAL_INVALID;
    uint64_t observed=q->expected;
    return atomic_compare_exchange_strong_explicit(slot,&observed,q->desired,memory_order_acq_rel,memory_order_acquire)?GC_HEAL_OK:GC_HEAL_CHANGED;
}
#endif
