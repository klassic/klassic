/* Canonical guarded header -> protected pending mark offer. The caller owns a
 * validated metadata lease, strong reference/worker entry, and live native
 * MarkRecord storage. Move that lease into q.lease BEFORE calling; all native
 * controls and output are disjoint from each other and the managed heap.
 * On OK the unique lease obligation moves to q.output and q.lease is cleared.
 * Rejection preserves the entire request and guard. No mark stamp is changed.
 *
 * The source guard remains held on return. EXIT before acquiring the queue/
 * heap lock or polling/waiting. The pending output remains protected in-flight
 * producer work until OFFER transfers it; DUPLICATE/rejection retains ownership
 * and requires release or retry by its holder. Never release a transferred
 * input lease twice. The record is nonmoving/shared between physical copies;
 * this leaf returns no managed address. It does not establish phase eligibility,
 * termination, source/color retirement or a complete public load barrier.
 */
#ifndef KLASSIC_GC_MARK_PREPARE_H
#define KLASSIC_GC_MARK_PREPARE_H
#include "gc_barrier.h"
#include "gc_mark.h"
static inline bool gc_mark_prepare_record_state(const GuardedMarkPrepare *q,MarkRecord **result,bool allow_initializing,uint64_t *state){
    const MarkRequest *out=&q->output;
    if(out->record||out->reference||out->incarnation||out->metadata_lease||out->index||out->serial||out->cursor||
        !q->reference||!gc_store_ref_valid(q->reference)||q->lease.page!=q->page||
        !(q->lease.token&UINT32_MAX)||!(q->lease.token>>32))return false;
    MutatorContext *ctx;ObjectHeader *h;
    if(!gc_guarded_object_state(q->scope,q->page,q->phase,allow_initializing,&ctx,&h,state))return false;
    SourceAccessRequest *scope=(SourceAccessRequest *)(uintptr_t)q->scope;
    if((q->reference>>16)!=scope->source_offset||q->lease.incarnation!=scope->incarnation||
        !h->descriptor_id||h->descriptor_id>UINT32_MAX||!h->mark_record||(h->mark_record&7))return false;
    MarkRecord *record=(MarkRecord *)(uintptr_t)h->mark_record;
    if(!record->object_id)return false;
    *result=record;return true;
}
static inline bool gc_mark_prepare_record(const GuardedMarkPrepare *q,MarkRecord **result){
    uint64_t state;return gc_mark_prepare_record_state(q,result,false,&state);
}
static inline void gc_mark_prepare_transfer(GuardedMarkPrepare *q,MarkRecord *record){
    q->output=(MarkRequest){.record=(uintptr_t)record,.reference=q->reference,.incarnation=q->lease.incarnation,.metadata_lease=q->lease.token};
    q->lease=(MetadataLeaseRequest){0};
}
static inline uint64_t gc_guarded_mark_prepare(GuardedMarkPrepare *q){
    MarkRecord *record;if(!gc_mark_prepare_record(q,&record))return GC_MARK_INVALID;
    gc_mark_prepare_transfer(q,record);return GC_MARK_OK;
}
#endif
