/* Actual public logical CAS, full barriers and collector movement. */
#define _DEFAULT_SOURCE 1
#include <stdint.h>
static void atomic_identities(void *,void *);
static void atomic_before_identities(void *,void *);
static void atomic_committed(void *,void *,uint64_t);
#define KG_ATOMIC_BEFORE_IDENTITIES_HOOK(r,q) atomic_before_identities(r,q)
#define KG_ATOMIC_IDENTITIES_HOOK(r,q) atomic_identities(r,q)
#define KG_ATOMIC_COMMITTED_HOOK(o,q,s) atomic_committed(o,q,s)
#include "klassic_gen_runtime.c"
#ifndef KLASSIC_GEN_ATOMIC_REF_IMPL_H
#include "klassic_gen_atomic_ref_impl.h"
#endif
#include "gc_native_driver.c"
#include "klassic_gen_remembered.c"
#include "klassic_gen_policy.c"
#include "klassic_gen_verify.c"
#include <assert.h>
static _Thread_local unsigned move_at_commit,identity_calls,commit_calls,physical_retries,contend_at_commit;
static _Atomic unsigned race_ready,race_go,race_done;
static _Thread_local ForwardCell *moving_cell;
static _Thread_local RootCell *exhaust_root;
static _Thread_local MetadataLeaseRequest held_tokens[8192];
static _Thread_local uint64_t held_count,before_capacity;
static void atomic_before_identities(void *runtime,void *operation){
    (void)operation;if(!exhaust_root)return;KgRuntime *r=runtime;kg_lock(r);before_capacity=kg_leases(r)->capacity;
    ReferenceLease source={.reference=atomic_load(&exhaust_root->value)};assert(!gc_reference_lease_locked(kg_leases(r),&source));
    held_tokens[held_count++]=source.lease;
    for(;;){MetadataLeaseRequest token={.page=source.lease.page,.incarnation=source.lease.incarnation};uint64_t status=gc_metadata_step_locked(kg_leases(r),&token,GC_ML_ACQUIRE);
        if(status==GC_ML_FULL)break;assert(!status&&held_count<8192);held_tokens[held_count++]=token;}
    assert(!kg_leases(r)->free_head);kg_unlock(r);exhaust_root=NULL;
}
static void quantum(KgRuntime *r,KgDriver *d){kg_lock(r);kgd_adopt(d);kg_unlock(r);kgd_step(d);assert(!atomic_load(&d->actor.access_epoch));}
static void atomic_identities(void *runtime,void *operation){
    (void)operation;identity_calls++;
    if(contend_at_commit){contend_at_commit=0;atomic_store(&race_go,1);while(!atomic_load(&race_done))kg_yield();}
    if(move_at_commit){
        move_at_commit=0;KgRuntime *r=runtime;KgDriver *d=r->driver;unsigned turns=0;
        while((atomic_load(&moving_cell->word)&3)!=2){assert(++turns<10000&&d->state==9);quantum(r,d);}
    }
}
static void atomic_committed(void *operation,void *request,uint64_t status){
    (void)operation;GuardedStore *q=request;commit_calls++;physical_retries+=status==GC_STORE_RETRY;
    if(status==GC_STORE_MISMATCH)assert(!q->transferred);
}
static void finish(void){kg_shutdown();assert(!kg_runtime.native.backing.heap.metadata_committed&&!kg_runtime.native.backing.heap.metadata_pending);}
static RootCell *leaf(uint64_t value){RootCell *out=kg_root_new();kg_alloc_into(8,1,out);kg_store_i64(out,0,value);return out;}
static void basic(void){
    RootCell *receiver=kg_root_new(),*a=leaf(17),*b=leaf(23),*empty=kg_root_new(),*observed=kg_root_new(),*check=kg_root_new();
    kg_alloc_into(8,2,receiver);
    assert(kg_compare_exchange_ref_into(receiver,0,empty,a,observed)&&!atomic_load(&observed->value));
    assert(!kg_compare_exchange_ref_into(receiver,0,b,empty,observed)&&kg_load_i64(observed,0)==17);
    assert(kg_compare_exchange_ref_into(receiver,0,a,b,observed)&&kg_load_i64(observed,0)==17);
    assert(kg_compare_exchange_ref_into(receiver,0,b,b,observed)&&kg_load_i64(observed,0)==23);
    kg_exchange_ref_into(receiver,0,a,observed);assert(kg_load_i64(observed,0)==23);
    kg_load_ref_into(receiver,0,check);assert(kg_load_i64(check,0)==17);
    kg_exchange_ref_into(receiver,0,empty,observed);assert(kg_load_i64(observed,0)==17);
    RootCell *alias=kg_root_new();kg_root_copy(alias,receiver);
    assert(kg_compare_exchange_ref_into(receiver,0,empty,b,receiver)&&!atomic_load(&receiver->value));
    kg_load_ref_into(alias,0,check);assert(kg_load_i64(check,0)==23);kg_root_release(alias);
    kg_root_release(check);kg_root_release(observed);kg_root_release(empty);kg_root_release(b);kg_root_release(a);kg_root_release(receiver);
    finish();puts("logical CAS/exchange: actual success, mismatch, same-value, null, aliased output and rooted old result PASS");
}
static void metadata_full(void){
    RootCell *receiver=kg_root_new(),*a=leaf(17),*b=leaf(23),*out=kg_root_new();kg_alloc_into(8,2,receiver);kg_store_ref(receiver,0,a);
    exhaust_root=receiver;assert(kg_compare_exchange_ref_into(receiver,0,a,b,out));assert(!exhaust_root&&held_count);
    assert(kg_load_i64(out,0)==17);kg_lock(&kg_runtime);assert(kg_leases(&kg_runtime)->capacity>before_capacity);
    for(uint64_t i=0;i<held_count;i++){MetadataLeaseRequest token={.token=held_tokens[i].token};
        assert(!gc_metadata_step_locked(kg_leases(&kg_runtime),&token,GC_ML_RESOLVE)&&token.page==held_tokens[i].page&&token.incarnation==held_tokens[i].incarnation);
        assert(!gc_metadata_step_locked(kg_leases(&kg_runtime),&held_tokens[i],GC_ML_RELEASE));}kg_unlock(&kg_runtime);
    kg_root_release(out);kg_root_release(b);kg_root_release(a);kg_root_release(receiver);finish();
    puts("logical CAS metadata FULL: actual admitted identity failure, complete cancellation, Native stable growth and all old tokens returned PASS");
}
static uint64_t slot_color(KgRuntime *r,RootCell *receiver){
    RootAccess q={.follow={.context=(uintptr_t)kg_context(r)}};kg_lock(r);q.follow.root=kg_root_request_locked(r,receiver);
    assert(gc_root_access_locked(kg_leases(r),&q)==GC_RAC_READY);kg_unlock(r);
    MutatorContext *ctx;_Atomic uint64_t *slot;assert(gc_guarded_slot((uintptr_t)&q.access.scope,q.follow.cleanup.page,(uintptr_t)kg_descriptors(r),0,
        atomic_load(&kg_context(r)->adopted_phase),&ctx,&slot));uint64_t color=atomic_load(slot)&UINT64_C(0xffff);
    uint64_t status;do{status=gc_root_access_cleanup(NULL,&q,GC_RAC_EXIT);}while(status==GC_RESOLVE_WAIT);assert(status==GC_RESOLVE_OK);
    kg_lock(r);assert(gc_root_access_cleanup(kg_leases(r),&q,GC_RAC_RELEASE)==GC_RF_DONE);kg_unlock(r);return color;
}
static void phase_case(bool moving){
    KgRuntime *r=&kg_runtime;RootCell *receiver=kg_root_new(),*a=leaf(17),*b=leaf(23),*out=kg_root_new();kg_alloc_into(8,2,receiver);kg_store_ref(receiver,0,a);
    kg_enter_native(r);uint64_t owned=kg_driver_cleanup_enter(r,NULL);assert(!owned);KgDriver *d=r->driver;kgd_native(d,false);
    kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);unsigned turns=0;
    while(d->state!=(moving?9u:3u)||d->copy_stage){assert(++turns<300000);quantum(r,d);}
    assert(!r->native.handshake.active);kg_leave_native(r);
    ReferenceLease source={0};uint64_t original=0;
    if(moving){
        kg_lock(r);original=atomic_load(&receiver->value)>>16;source.reference=atomic_load(&receiver->value);
        assert(!gc_reference_lease_locked(kg_leases(r),&source));AllocationPage *page=(void *)(uintptr_t)source.lease.page;
        ForwardLookup lookup={original,page->incarnation,0};assert(gc_forward_lookup((void *)(uintptr_t)page->forward_table,&lookup)==GC_FL_OK);
        moving_cell=(void *)(uintptr_t)lookup.cell;assert(!atomic_load(&moving_cell->word));kg_unlock(r);move_at_commit=1;
    }else{
        kga_publish_observed(r,a,a);assert(slot_color(r,receiver)!=(atomic_load(&a->value)&UINT64_C(0xffff)));
    }
    assert(kg_compare_exchange_ref_into(receiver,0,a,b,out));assert(kg_load_i64(out,0)==17);
    kg_load_ref_into(receiver,0,out);assert(kg_load_i64(out,0)==23);
    if(moving){assert(!move_at_commit);kg_root_copy(receiver,receiver);assert((atomic_load(&receiver->value)>>16)!=original);
        kg_lock(r);assert(!gc_metadata_step_locked(kg_leases(r),&source.lease,GC_ML_RELEASE));kg_unlock(r);}
    kg_enter_native(r);while(d->state||d->background_old||d->requested[0]||d->requested[1]){assert(++turns<300000);quantum(r,d);}
    kgd_native(d,true);kg_driver_cleanup_leave(r,owned);kg_leave_native(r);
    kg_root_release(out);kg_root_release(b);kg_root_release(a);kg_root_release(receiver);finish();
    puts(moving?"logical CAS move: actual receiver forwarding after identity preparation, fresh canonical slot and source retirement PASS":
        "logical CAS colors: real MarkStart and desired-root marking make expected/slot colors unequal, identical actual record matches PASS");
}
typedef struct {RootCell *receiver,*desired;} CompetingStore;
static void *competing_store(void *argument){
    CompetingStore *job=argument;kg_thread_attach();RootCell *receiver=kg_root_new(),*desired=kg_root_new(),*old=kg_root_new();
    kg_root_import(receiver,job->receiver);kg_root_import(desired,job->desired);kg_native_enter();atomic_store(&race_ready,1);
    while(!atomic_load(&race_go))kg_yield();kg_native_exit();kg_exchange_ref_into(receiver,0,desired,old);
    atomic_store(&race_done,1);kg_root_release(old);kg_root_release(desired);kg_root_release(receiver);kg_thread_detach();return NULL;
}
static void competing(bool heal){
    KgRuntime *r=&kg_runtime;RootCell *receiver=kg_root_new(),*a=leaf(17),*b=leaf(23),*c=leaf(31),*out=kg_root_new();
    kg_alloc_into(8,2,receiver);kg_store_ref(receiver,0,a);CompetingStore job={receiver,heal?a:c};pthread_t other;
    assert(!pthread_create(&other,NULL,competing_store,&job));kg_enter_native(r);while(!atomic_load(&race_ready))kg_yield();
    uint64_t owned=kg_driver_cleanup_enter(r,NULL);assert(!owned);KgDriver *d=r->driver;kgd_native(d,false);unsigned turns=0;
    if(heal){kg_lock(r);d->requested[GC_YOUNG]=1;kg_unlock(r);while(d->state!=3){assert(++turns<300000);quantum(r,d);}}
    kg_leave_native(r);if(heal){kga_publish_observed(r,a,a);assert(slot_color(r,receiver)!=(atomic_load(&a->value)&UINT64_C(0xffff)));}
    contend_at_commit=1;bool success=kg_compare_exchange_ref_into(receiver,0,a,b,out);assert(success==heal&&physical_retries>=1&&identity_calls>=2);
    assert(kg_load_i64(out,0)==(heal?17u:31u));kg_load_ref_into(receiver,0,out);assert(kg_load_i64(out,0)==(heal?23u:31u));
    /* The competing owner may be retiring its real root journal. Do not hold
     * the phase actor while joining or waiting for its claim/ACK handoff. */
    kg_enter_native(r);kgd_native(d,true);kg_driver_cleanup_leave(r,owned);assert(!pthread_join(other,NULL));kg_leave_native(r);
    for(unsigned i=0;i<4;i++)kg_driver_pressure(r,KG_PRESSURE_HEAP);
    kg_root_release(out);kg_root_release(c);kg_root_release(b);kg_root_release(a);kg_root_release(receiver);finish();
    puts(heal?"logical CAS heal race: actual other-owner same-object recolor forces exact CAS retry, fresh identity succeeds PASS":
        "logical CAS mutation race: actual other-owner exchange forces physical retry, fresh mismatch read and rooted current value PASS");
}
typedef struct {RootCell *shared;pthread_barrier_t *start;unsigned rounds,index;bool exchange;uint64_t final_token;} AtomicOwner;
static void barrier_native(pthread_barrier_t *barrier){kg_native_enter();int s=pthread_barrier_wait(barrier);assert(!s||s==PTHREAD_BARRIER_SERIAL_THREAD);kg_native_exit();}
static void *counter_owner(void *argument){
    AtomicOwner *job=argument;kg_thread_attach();RootCell *shared=kg_root_new(),*current=kg_root_new(),*desired=kg_root_new();kg_root_import(shared,job->shared);
    if(job->exchange){kg_alloc_into(8,1,current);kg_store_i64(current,0,job->index+1);}else kg_load_ref_into(shared,0,current);
    barrier_native(job->start);
    for(unsigned i=0;i<job->rounds;i++){
        if(job->exchange)kg_exchange_ref_into(shared,0,current,current);
        else for(;;){uint64_t before=kg_load_i64(current,0);kg_alloc_into(8,1,desired);kg_store_i64(desired,0,before+1);
            if(kg_compare_exchange_ref_into(shared,0,current,desired,current))break;}
        if(!(i&3))kg_poll();
    }
    if(job->exchange)job->final_token=kg_load_i64(current,0);
    kg_root_release(desired);kg_root_release(current);kg_root_release(shared);kg_thread_detach();return NULL;
}
static void concurrent(unsigned count,bool exchange){
    RootCell *shared=kg_root_new(),*initial=leaf(0);kg_alloc_into(8,2,shared);kg_store_ref(shared,0,initial);kg_root_release(initial);
    pthread_barrier_t start;assert(!pthread_barrier_init(&start,NULL,count+1));pthread_t threads[8];AtomicOwner jobs[8];
    for(unsigned i=0;i<count;i++){jobs[i]=(AtomicOwner){shared,&start,exchange?128:32,i,exchange,UINT64_MAX};assert(!pthread_create(&threads[i],NULL,counter_owner,&jobs[i]));}
    barrier_native(&start);for(unsigned i=0;i<8;i++)kg_driver_pressure(&kg_runtime,KG_PRESSURE_HEAP);
    kg_native_enter();for(unsigned i=0;i<count;i++)assert(!pthread_join(threads[i],NULL));kg_native_exit();
    RootCell *out=kg_root_new();kg_load_ref_into(shared,0,out);uint64_t value=kg_load_i64(out,0);
    if(exchange){assert(value<=count);uint64_t seen=UINT64_C(1)<<value;
        for(unsigned i=0;i<count;i++){assert(jobs[i].final_token<=count);uint64_t bit=UINT64_C(1)<<jobs[i].final_token;assert(!(seen&bit));seen|=bit;}
        assert(seen==(UINT64_C(1)<<(count+1))-1);
    }else assert(value==count*32);
    kg_root_release(out);kg_root_release(shared);assert(!pthread_barrier_destroy(&start));finish();
    printf("atomic reference %s: %u actual owners, exact %s with real Young/Old movement and detach PASS\n",exchange?"exchange":"CAS",count,exchange?"token conservation":"counter increments");
}
int main(int argc,char **argv){
    assert(argc==2);kg_configure(32*1024*1024,1024*1024,16*1024*1024,4);kg_init();
    if(!strcmp(argv[1],"basic"))basic();else if(!strcmp(argv[1],"colors"))phase_case(false);else if(!strcmp(argv[1],"moves"))phase_case(true);
    else if(!strcmp(argv[1],"mutation"))competing(false);else if(!strcmp(argv[1],"heal"))competing(true);
    else if(!strcmp(argv[1],"2")||!strcmp(argv[1],"8"))concurrent((unsigned)atoi(argv[1]),false);
    else if(!strcmp(argv[1],"exchange2")||!strcmp(argv[1],"exchange8"))concurrent((unsigned)atoi(argv[1]+8),true);
    else if(!strcmp(argv[1],"full"))metadata_full();
    else{
        RootCell *receiver=kg_root_new(),*desired=leaf(17),*empty=kg_root_new(),*out=kg_root_new();kg_alloc_into(8,2,receiver);
        if(!strcmp(argv[1],"native")){kg_native_enter();kg_exchange_ref_into(receiver,0,desired,out);}
        else if(!strcmp(argv[1],"alignment"))kg_exchange_ref_into(receiver,1,desired,out);
        else if(!strcmp(argv[1],"range"))kg_compare_exchange_ref_into(receiver,8,empty,desired,out);
        else if(!strcmp(argv[1],"type"))kg_exchange_ref_into(desired,0,empty,out);
        else if(!strcmp(argv[1],"null"))kg_exchange_ref_into(empty,0,desired,out);
        abort();
    }return 0;
}
