/* Include once after the rooted facade dispatcher. No new ABI/core fields. */
#ifndef KLASSIC_GEN_ATOMIC_REF_IMPL_H
#define KLASSIC_GEN_ATOMIC_REF_IMPL_H
#include "klassic_gen_atomic_ref.h"
#include <stddef.h>
typedef struct {RootAccess access;const MarkRecord *record;bool prepared;} KgaIdentity;
typedef struct {
    RootedStore store;
    KgaIdentity expected,observed;
    RootCell *expected_root,*observed_root;
    uintptr_t self;
    bool exchange;
} KgaOperation;
enum { KGA_ID_READY=100 };
static uint64_t kga_cleanup(MetadataLeasePool *pool,RootAccess *q,uint64_t op){
    uint64_t status=gc_root_access_cleanup(pool,q,op);
    if(op==GC_RAC_EXIT&&status!=GC_RESOLVE_OK&&status!=GC_RESOLVE_WAIT)kg_fail("atomic source guard exit",status);
    if(op==GC_RAC_RELEASE&&status!=GC_RF_DONE)kg_fail("atomic metadata release",status);
    return status;
}
/* Both the logical root and the exact canonical page lease protect this native
 * identity. EXIT releases the guard only; RELEASE happens after commit/cleanup
 * and before any poll/Native transition, including all cancellation retries. */
static uint64_t kga_identity(KgRuntime *r,KgaIdentity *identity,RootCell *root){
    if(identity->prepared)return KGA_ID_READY;
    if(!identity->access.follow.context){
        kg_lock(r);identity->access.follow.context=(uintptr_t)kg_context(r);
        identity->access.follow.root=kg_root_request_locked(r,root);kg_unlock(r);
    }
    kg_lock(r);uint64_t status=gc_root_access_locked(kg_leases(r),&identity->access);kg_unlock(r);
    if(status==GC_RF_DONE){identity->prepared=true;return KGA_ID_READY;}
    if(status!=GC_RAC_READY)return status;
    identity->record=gc_guarded_identity((uintptr_t)&identity->access.access.scope,identity->access.follow.cleanup.page,
        atomic_load_explicit(&kg_context(r)->adopted_phase,memory_order_acquire));
    do{status=kga_cleanup(NULL,&identity->access,GC_RAC_EXIT);}while(status==GC_RESOLVE_WAIT);
    if(status!=GC_RESOLVE_OK||!identity->record)kg_fail("atomic guarded logical identity",status);
    identity->prepared=true;return KGA_ID_READY;
}
static void kga_release_identity(KgRuntime *r,KgaIdentity *identity){
    if(identity->access.follow.cleanup.token){
        kg_lock(r);uint64_t status=kga_cleanup(kg_leases(r),&identity->access,GC_RAC_RELEASE);kg_unlock(r);
        if(status!=GC_RF_DONE)kg_fail("atomic identity lease release",status);
    }
    *identity=(KgaIdentity){0};
}
/* Same desired-root/mark receipt checks as gc_rooted_store_commit, with the
 * actual logical SeqCst commit substituted. Prepared identity leases and strong
 * roots belong to the enclosing operation; no fabricated records or addresses.
 * Only the transferred buffer mask escapes this guarded finite operation. */
static uint64_t kga_commit(const RootedMarkSet *marks,GuardedStore *q){
    KgaOperation *operation=(void *)((unsigned char *)marks - offsetof(KgaOperation,store.value.marks));
    if(operation->self!=(uintptr_t)operation||!operation->observed.prepared||(!operation->exchange&&!operation->expected.prepared))return GC_STORE_INVALID;
    if(marks->state!=2||marks->root.value||q->desired||q->transferred||marks->phase!=q->phase)return GC_STORE_INVALID;
    uint64_t required=0;
    for(unsigned g=0;g<2;g++){uint64_t phase=(marks->phase>>(4*g))&15;if(phase>=GC_MARK_START&&phase<=GC_MARK_END_CHECK)required|=UINT64_C(1)<<g;}
    if(marks->required!=required||marks->completed!=required)return GC_STORE_INVALID;
    MutatorContext *ctx;ObjectHeader *header;
    if(!gc_guarded_object(q->scope,q->page,q->phase,&ctx,&header)||(uintptr_t)ctx!=marks->context||
       !atomic_load_explicit(&ctx->private_producers,memory_order_acquire))return GC_STORE_INVALID;
    (void)header;
    const RootRequest *root=&marks->root;
    if(!root->segment||(root->segment&7))return GC_STORE_INVALID;
    RootSegment *segment=(void *)(uintptr_t)root->segment;
    if(!gc_root_segment_valid(segment)||segment->owner!=ctx->thread_id||
       atomic_load_explicit(&segment->state,memory_order_acquire)!=GC_ROOT_SEGMENT_ACTIVE||segment->scope!=root->handle.scope)return GC_STORE_INVALID;
    uint64_t used=atomic_load_explicit(&segment->used,memory_order_acquire);
    if(used>segment->capacity||root->handle.cell<segment->cells||root->handle.cell-segment->cells>=used*sizeof(RootCell)||
       (root->handle.cell-segment->cells)%sizeof(RootCell)||!root->handle.reuse_generation)return GC_STORE_INVALID;
    RootCell *cell=(void *)(uintptr_t)root->handle.cell;
    if(!cell->active||cell->reuse_generation!=root->handle.reuse_generation)return GC_STORE_INVALID;
    uint64_t value=atomic_load_explicit(&cell->value,memory_order_acquire);
    if(!gc_store_ref_valid(value))return GC_STORE_INVALID;
    uint64_t color=gc_abi_color((PhaseSnapshot){marks->phase});
    if(value&&(value&UINT64_C(0xffcf))!=(color&~UINT64_C(0x30)))return GC_STORE_REPREPARE;
    GuardedStore commit=*q;commit.desired=value?(value&~UINT64_C(0xffff))|color:0;
    const MarkRecord *observed=operation->observed.record;
    uint64_t result=gc_guarded_compare_exchange(&commit,operation->exchange?observed:operation->expected.record,observed);
    q->transferred=commit.transferred;
#ifdef KG_ATOMIC_COMMITTED_HOOK
    KG_ATOMIC_COMMITTED_HOOK(operation,q,result);
#endif
    return result;
}
static uint64_t kga_attempt(MetadataLeasePool *pool,RootedStoreAttempt *q,uint64_t op){
    const RootedStoreAttemptOps ops={gc_root_access_locked,gc_metadata_step_locked,kga_commit,kga_cleanup};
    return gc_rooted_store_attempt_step(pool,q,op,&ops);
}
/* The facade uses one common mutex for registry/heap/both generation queues.
 * ACCESS is always outside it. Admission and recapture preserve the exact
 * existing controller's private-producer and queue/phase ownership protocol. */
static uint64_t kga_store_step(KgRuntime *r,RootedStore *q,bool cancel){
    const RootedStoreOps ops={gc_rooted_value,gc_rooted_observation,kga_attempt,kga_cleanup};
    if(cancel)return gc_rooted_store_step(NULL,q,GC_RLOAD_CANCEL,&ops);
    uint64_t action=gc_rooted_store_next(q);
    if(action==GC_RLOAD_INVALID)return action;
    if(action==GC_RLOAD_ACCESS||action==GC_RLOAD_DONE||action==GC_RLOAD_CANCELLED||action==GC_RLOAD_ERROR||action==GC_RLOAD_NULL_RECEIVER)
        return gc_rooted_store_step(NULL,q,action,&ops);
    bool admit=!q->cancel&&!q->stage&&(action==GC_RLOAD_RESTART||(!q->admitted&&!q->value.stage&&!q->value.admitted));
    kg_lock(r);uint64_t status;
    if(admit){RootedLoadAdmission a;(void)kg_dispatch(r,&a);status=gc_rooted_load_admit_locked(&a,&q->value);
        if(status==GC_LA_OK)status=action==GC_RLOAD_RESTART?gc_rooted_store_step(NULL,q,GC_RLOAD_RESUME,&ops):GC_RLOAD_HEAP;
        else status=status==GC_LA_WAIT?GC_RLOAD_WAIT:status==GC_LA_EXHAUSTED?GC_RLOAD_EXHAUSTED:GC_RLOAD_INVALID;
    }else status=gc_rooted_store_step(kg_leases(r),q,action,&ops);
    kg_unlock(r);return status;
}
static uint64_t kga_value_step(KgRuntime *r,RootedLoad *q,bool cancel){
    if(cancel)return gc_rooted_value(NULL,q,GC_RLOAD_CANCEL);
    uint64_t action=gc_rooted_load_next(q);
    if(action==GC_RLOAD_INVALID)return action;
    if(action==GC_RLOAD_ACCESS||action==GC_RLOAD_DONE||action==GC_RLOAD_CANCELLED||action==GC_RLOAD_ERROR||action==GC_RLOAD_NULL_RECEIVER)
        return gc_rooted_value(NULL,q,action);
    bool admit=!q->cancel&&(action==GC_RLOAD_RESTART||(!q->stage&&!q->admitted));
    kg_lock(r);uint64_t status;
    if(admit){RootedLoadAdmission a;(void)kg_dispatch(r,&a);status=gc_rooted_load_admit_locked(&a,q);
        if(status==GC_LA_OK)status=action==GC_RLOAD_RESTART?gc_rooted_value(NULL,q,GC_RLOAD_RESUME):GC_RLOAD_HEAP;
        else status=status==GC_LA_WAIT?GC_RLOAD_WAIT:status==GC_LA_EXHAUSTED?GC_RLOAD_EXHAUSTED:GC_RLOAD_INVALID;
    }else status=gc_rooted_value(kg_leases(r),q,action);
    kg_unlock(r);return status;
}
static void kga_publish_observed(KgRuntime *r,RootCell *observed,RootCell *out){
    for(;;){
        kg_poll();RootedLoad q={0};kg_lock(r);
        q.observation.receiver.follow.context=(uintptr_t)kg_context(r);
        q.observation.receiver.follow.root=q.observation.output=kg_root_request_locked(r,observed);kg_unlock(r);
        kg_root_publication_begin(r);bool cancel=false;uint64_t reason=0;
        for(;;){uint64_t status=kga_value_step(r,&q,false);
            if(status==GC_RLOAD_DONE){kg_root_publication_end(r);kg_root_copy(out,observed);return;}
            if(status==GC_RLOAD_CANCELLED){kg_root_publication_end(r);if(reason==GC_RLOAD_EXHAUSTED)kg_fail("atomic result resources exhausted",reason);if(reason==GC_RLOAD_METADATA_FULL)kg_lease_pressure(r);break;}
            if(kg_retry_event(status)&&!cancel){reason=status;(void)kga_value_step(r,&q,true);cancel=true;}
            else if(status==GC_RLOAD_INVALID||status==GC_RLOAD_ERROR||status==GC_RLOAD_NULL_RECEIVER)kg_fail("atomic result load barrier",status);
        }
    }
}
static bool kga_reference(const RootCell *receiver,uint64_t offset,const RootCell *expected,const RootCell *desired,RootCell *out,bool exchange){
    KgRuntime *r=&kg_runtime;kg_require_owner(r);
    if(atomic_load_explicit(&kg_context(r)->state,memory_order_acquire)!=GC_RUNNING)kg_fail("atomic requires Running mutator",0);
    RootCell *base=kg_root_new();kg_root_copy(base,receiver);
    RootCell *compare=kg_root_new();if(!exchange)kg_root_copy(compare,expected);
    RootCell *value=kg_root_new();kg_root_copy(value,desired);
    RootCell *observed=kg_root_new();kg_root_clear(out);bool result=false;
    for(;;){
        kg_poll();KgaOperation operation={.expected_root=compare,.observed_root=observed,.exchange=exchange};operation.self=(uintptr_t)&operation;
        RootedStore *q=&operation.store;kg_lock(r);
        q->value.observation.receiver.follow.context=q->observation.receiver.follow.context=(uintptr_t)kg_context(r);
        q->value.observation.receiver.follow.root=q->value.observation.output=kg_root_request_locked(r,value);
        q->observation.receiver.follow.root=kg_root_request_locked(r,base);q->observation.output=kg_root_request_locked(r,observed);
        q->observation.registry=(uintptr_t)kg_descriptors(r);q->observation.byte_offset=offset;kg_unlock(r);
        kg_root_publication_begin(r);bool cancelling=false;uint64_t reason=0;
        for(;;){
            uint64_t status=GC_RLOAD_HEAP;
            if(!cancelling&&q->stage==GC_RS_RETRY){kga_release_identity(r,&operation.observed);kga_release_identity(r,&operation.expected);}
            if(!cancelling&&q->stage==GC_RS_INIT){
#ifdef KG_ATOMIC_BEFORE_IDENTITIES_HOOK
                KG_ATOMIC_BEFORE_IDENTITIES_HOOK(r,&operation);
#endif
                status=exchange?KGA_ID_READY:kga_identity(r,&operation.expected,compare);
                if(status==KGA_ID_READY)status=kga_identity(r,&operation.observed,observed);
                if(status==GC_RF_METADATA_FULL)status=GC_RLOAD_METADATA_FULL;
                else if(status==GC_RF_WAIT)status=GC_RLOAD_WAIT;
                else if(status==GC_RF_FLUSH)status=GC_RLOAD_FLUSH;
                else if(status==GC_RF_EXHAUSTED)status=GC_RLOAD_EXHAUSTED;
                else if(status==GC_RF_ADVANCED)continue;
                else if(status!=KGA_ID_READY)kg_fail("atomic identity preparation",status);
                if(status!=KGA_ID_READY){reason=status;(void)kga_store_step(r,q,true);cancelling=true;}
#ifdef KG_ATOMIC_IDENTITIES_HOOK
                if(!cancelling)KG_ATOMIC_IDENTITIES_HOOK(r,&operation);
#endif
            }
            status=kga_store_step(r,q,false);
            bool mismatch=status==GC_RLOAD_ERROR&&q->result==GC_STORE_MISMATCH&&q->attempt.stage==5;
            if(status==GC_RLOAD_DONE||mismatch||status==GC_RLOAD_CANCELLED){
                kga_release_identity(r,&operation.observed);kga_release_identity(r,&operation.expected);kg_root_publication_end(r);
                if(status==GC_RLOAD_CANCELLED){if(reason==GC_RLOAD_EXHAUSTED)kg_fail("atomic resources exhausted",reason);
                    if(reason==GC_RLOAD_METADATA_FULL)kg_lease_pressure_min(r,5);break;}
                result=!mismatch;goto finished;
            }
            if(kg_retry_event(status)&&!cancelling){reason=status;(void)kga_store_step(r,q,true);cancelling=true;}
            else if(status==GC_RLOAD_INVALID||status==GC_RLOAD_ERROR||status==GC_RLOAD_NULL_RECEIVER)kg_fail("atomic reference operation",status);
        }
    }
finished:
    kga_publish_observed(r,observed,out);
    kg_root_release(observed);kg_root_release(value);kg_root_release(compare);kg_root_release(base);return result;
}
bool kg_compare_exchange_ref_into(const RootCell *receiver,uint64_t offset,const RootCell *expected,const RootCell *desired,RootCell *out){
    return kga_reference(receiver,offset,expected,desired,out,false);
}
void kg_exchange_ref_into(const RootCell *receiver,uint64_t offset,const RootCell *desired,RootCell *out){
    (void)kga_reference(receiver,offset,NULL,desired,out,true);
}
#endif
