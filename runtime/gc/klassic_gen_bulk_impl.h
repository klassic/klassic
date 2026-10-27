/* Include once, after the public rooted load/store dispatcher in runtime.c. */
#ifndef KLASSIC_GEN_BULK_IMPL_H
#define KLASSIC_GEN_BULK_IMPL_H
#include "klassic_gen_bulk.h"
/* Copy immutable scalars only while holding actual canonical SourceAccess.
 * The original/imported MarkRecord stays live under this source metadata lease;
 * no record or managed address survives cleanup. Identity survives relocation,
 * unlike a comparison of two encoded references read across separate polls. */
static uint64_t kgb_identity(KgRuntime *r,RootCell *root,uint64_t offset,uint64_t bytes){
    RootAccess q={.follow={.context=(uintptr_t)kg_context(r)}};
    kg_lock(r);q.follow.root=kg_root_request_locked(r,root);kg_unlock(r);
    for(;;){
        kg_lock(r);uint64_t status=gc_root_access_locked(kg_leases(r),&q);kg_unlock(r);
        if(status==GC_RAC_READY){
            MutatorContext *ctx;ObjectHeader *header;uint64_t state;
            uint64_t phase=atomic_load_explicit(&kg_context(r)->adopted_phase,memory_order_acquire);
            bool valid=gc_guarded_object_state((uintptr_t)&q.access.scope,q.follow.cleanup.page,phase,false,&ctx,&header,&state);
            const MarkRecord *record=valid?gc_guarded_identity((uintptr_t)&q.access.scope,q.follow.cleanup.page,phase):NULL;
            uint64_t identity=record?record->object_id:0;
            bool kind=valid&&header->descriptor_id==2;
            bool range=kind&&offset<=header->payload_bytes&&bytes<=header->payload_bytes-offset;
            /* EXIT's single CAS can legitimately race another owner guard.
             * Retry only WAIT immediately; never poll, lock, or grow here. */
            do{status=gc_root_access_cleanup(NULL,&q,GC_RAC_EXIT);}while(status==GC_RESOLVE_WAIT);
            if(status!=GC_RESOLVE_OK)kg_fail("bulk source guard exit",status);
            kg_lock(r);status=gc_root_access_cleanup(kg_leases(r),&q,GC_RAC_RELEASE);kg_unlock(r);
            if(status!=GC_RF_DONE)kg_fail("bulk metadata release",status);
            if(!identity)kg_fail("bulk receiver identity",0);
            if(!kind)kg_fail("bulk requires REF_RECORD",0);
            if(!range)kg_fail("bulk reference range",0);
            return identity;
        }
        if(status==GC_RF_DONE)kg_fail("bulk null receiver",0);
        if(status==GC_RF_METADATA_FULL)kg_lease_pressure(r);
        else if(status!=GC_RF_ADVANCED&&status!=GC_RF_WAIT&&status!=GC_RF_FLUSH)kg_fail("bulk receiver access",status);
        kg_poll();
    }
}
void kg_copy_refs(const RootCell *destination,uint64_t destination_offset,
                  const RootCell *source,uint64_t source_offset,uint64_t count){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    if(atomic_load_explicit(&kg_context(r)->state,memory_order_acquire)!=GC_RUNNING)kg_fail("bulk requires Running mutator",0);
    if((destination_offset|source_offset)&7)kg_fail("bulk reference alignment",0);
    if(count>UINT64_MAX/8)kg_fail("bulk reference count overflow",count);
    uint64_t bytes=count*8;
    RootCell *src=kg_root_new();kg_root_copy(src,source);
    RootCell *dst=kg_root_new();kg_root_copy(dst,destination);
    uint64_t source_id=kgb_identity(r,src,source_offset,bytes);
    uint64_t destination_id=kgb_identity(r,dst,destination_offset,bytes);
    bool reverse=source_id==destination_id&&destination_offset>source_offset&&destination_offset-source_offset<bytes;
#ifdef KG_BULK_VALIDATED_HOOK
    KG_BULK_VALIDATED_HOOK(r,src,dst,reverse);
#endif
    if(count&&(source_id!=destination_id||source_offset!=destination_offset)){
        RootCell *value=kg_root_new();
        for(uint64_t copied=0;copied<count;copied++){
            uint64_t index=reverse?count-1-copied:copied;
            /* Each call reauthenticates the actual owned receiver root and
             * obtains fresh canonical source/slot metadata after possible
             * moves. Only immutable identities/range arithmetic span polls. */
            kg_load_ref_into(src,source_offset+index*8,value);
            kg_store_ref(dst,destination_offset+index*8,value);
#ifdef KG_BULK_COPIED_HOOK
            KG_BULK_COPIED_HOOK(r,copied+1);
#endif
        }
        kg_root_release(value);
    }
    kg_root_release(dst);kg_root_release(src);
}
#endif
